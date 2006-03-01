/* LP_Problem class implementation: non-inline functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>
#include "LP_Problem.defs.hh"
#include "globals.types.hh"
#include "globals.defs.hh"
#include "Row.defs.hh"
#include "Matrix.defs.hh"
#include "Linear_Row.defs.hh"
#include "Linear_System.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Generator.defs.hh"
#include "Scalar_Products.defs.hh"
#include <stdexcept>
#include <sstream>
#include <map>
#include <deque>
#include <set>
#include <algorithm>

#ifndef PPL_NOISY_SIMPLEX
#define PPL_NOISY_SIMPLEX 0
#endif

#ifdef PPL_NOISY_SIMPLEX
#include <iostream>
#endif

#ifndef PPL_SIMPLEX_USE_STEEPEST_EDGE_FLOATING_POINT
#define PPL_SIMPLEX_USE_STEEPEST_EDGE_FLOATING_POINT 0
#endif

namespace PPL = Parma_Polyhedra_Library;

#if PPL_NOISY_SIMPLEX
namespace {

unsigned long num_iterations = 0;

} // namespace
#endif // PPL_NOISY_SIMPLEX

bool
PPL::LP_Problem::is_in_base(const dimension_type var_index,
			    dimension_type& row_index) const {
  for (row_index = base.size(); row_index-- > 0; )
    if (base[row_index] == var_index)
      return true;
  return false;
}

void
PPL::
LP_Problem::unsplit(dimension_type var_index,
		    std::vector<dimension_type>&
		    unfeasible_tableau_rows) {
  // We are surely removing a non artificial column.
  is_artificial.pop_back();

  const dimension_type tableau_nrows = tableau.num_rows();
  const dimension_type column = mapping[var_index].second;

  for (dimension_type i = 0; i < tableau_nrows; ++i) {
    // In the following case the negative side of the splitted Variable is
    // in base: this means that the constraint will be nonfeasible.
    if (base[i] == mapping[var_index].second) {
      // CHECKME: I don't know if is possible that the positive and the
      // negative part of a splitted Variable can be together in base: it
      // seems that this case is not possible. The algorithm i have written
      // requires that condition.
      // This code is run only for testing purposes.
      for (dimension_type j = 0; j < tableau_nrows; ++j) {
 	dimension_type test;
	// Avoid a compiler warning.
	test = 0;
	assert(!is_in_base(mapping[var_index].first, test));
      }
      // We set base[i] to zero to keep track that that the constraint is not
      // feasible by `last_generator'.
      base[i] = 0;
      unfeasible_tableau_rows.push_back(i);
    }
  }

  const dimension_type tableau_cols = tableau.num_columns();
  // Remove the column.
  if (column != tableau_cols - 1) {
    std::vector<dimension_type> cycle;
    for (dimension_type j = tableau_cols - 1; j >= column; --j)
      cycle.push_back(j);
    cycle.push_back(0);
    tableau.permute_columns(cycle);
  }
  tableau.remove_trailing_columns(1);

  // var_index is no longer splitted.
  mapping[var_index].second = 0;

  // Adjust data structured, `shifting' the proper columns to the left by 1.
  const dimension_type base_size = base.size();
  for (dimension_type i = base_size; i-- > 0; )
    if (base[i] > column)
      --base[i];
  const dimension_type mapping_size = mapping.size();
  for (dimension_type i = mapping_size; i-- > 0; ) {
    if (mapping[i].first > column)
      --mapping[i].first;
    if (mapping[i].second > column)
      --mapping[i].second;
  }
}

bool
PPL::LP_Problem::is_satisfied(const Constraint& ineq) const {
  assert(ineq.is_inequality());
  // Scalar_Products::sign() requires the second argument to be at least
  // as large as the first one.
  int sp_sign = last_generator.size() <= ineq.size()
    ? Scalar_Products::sign(last_generator, ineq)
    : Scalar_Products::sign(ineq, last_generator);
  return sp_sign >= 0;
}

bool
PPL::LP_Problem::parse_constraints(const Constraint_System& cs,
				   dimension_type& tableau_num_rows,
				   dimension_type& num_slack_variables,
				   std::deque<bool>& is_tableau_constraint,
				   std::deque<bool>& nonnegative_variable,
				   std::vector<dimension_type>&
				   unfeasible_tableau_rows,
				   std::deque<bool>& satisfied_ineqs,
				   const bool bootstrap) {
  satisfied_ineqs.clear();
  satisfied_ineqs.insert(satisfied_ineqs.end(), pending_input_cs.num_rows(),
			 false);

  const dimension_type cs_num_rows = cs.num_rows();
  const dimension_type cs_num_cols = cs.num_columns();
  // Step 1:
  // determine variables that are constrained to be nonnegative,
  // detect (non-negativity or tautology) constraints that will not
  // be part of the tableau and count the number of slack variables.

  // Counters determining the dimensions of the tableau:
  // initialized here, they will be updated while examining `cs'.
  tableau_num_rows = cs_num_rows;
  dimension_type tableau_num_cols = 2*(cs_num_cols - 1);
  num_slack_variables = 0;

  // On exit, `is_tableau_constraint[i]' will be true if and only if
  // `cs[i]' is neither a tautology (e.g., 1 >= 0) nor a non-negativity
  // constraint (e.g., X >= 0).
  is_tableau_constraint = std::deque<bool> (cs_num_rows, true);

  // On exit, `nonnegative_variable[j]' will be true if and only if
  // Variable(j) is bound to be nonnegative in `cs'.
  nonnegative_variable = std::deque<bool> (cs_num_cols - 1, false);

  // Check for already known infos about space dimensions and store them in
  // `nonnegative_variable'.
  const dimension_type mapping_size = mapping.size();
  for (dimension_type i = std::min(mapping_size, cs_num_cols); i-- > 1; )
    if(mapping[i].second == 0) {
      nonnegative_variable[i-1] = true;
      --tableau_num_cols;
    }

  // Process each row of the `cs' matrix.
  for (dimension_type i = cs_num_rows; i-- > 0; ) {
    const Linear_Row& cs_i = cs[i];
    bool found_a_nonzero_coeff = false;
    bool found_many_nonzero_coeffs = false;
    dimension_type nonzero_coeff_column_index = 0;
    for (dimension_type j = cs_num_cols; j-- > 1; ) {
      if (cs_i[j] != 0)
	if (found_a_nonzero_coeff) {
	  found_many_nonzero_coeffs = true;
	  if (cs_i.is_ray_or_point_or_inequality())
	    ++num_slack_variables;
	  break;
	}
	else {
	  nonzero_coeff_column_index = j;
	  found_a_nonzero_coeff = true;
	}
    }
    // If more than one coefficient is nonzero,
    // continue with next constraint.
    if (found_many_nonzero_coeffs) {
      // Check for satisfiabilty of the inequality. This can be done if we
      // have a feasible point of *this: `bootstap' assures that condition.
      if (!bootstrap && cs[i].is_inequality() && is_satisfied(cs[i]))
	satisfied_ineqs[i] = true;
      continue;
    }

    if (!found_a_nonzero_coeff) {
      // All coefficients are 0.
      // The constraint is either trivially true or trivially false.
      if (cs_i.is_ray_or_point_or_inequality()) {
	if (cs_i[0] < 0)
	  // A constraint such as -1 >= 0 is trivially false.
	  return false;
      }
      else
	// The constraint is an equality.
	if (cs_i[0] != 0)
	  // A constraint such as 1 == 0 is trivially false.
	  return false;
      // Here the constraint is trivially true.
      is_tableau_constraint[i] = false;
      --tableau_num_rows;
      continue;
    }
    else {
      // Here we have only one nonzero coefficient.
      /*

      We have the following methods:
      A) Do split the variable and do add the constraint in the tableau.
      B) Don't split the variable and do add the constraint in the tableau.
      C) Don't split the variable and don't add the constraint in the tableau.

      Let the constraint be (a*v + b relsym 0).
      These are the 12 possible combinations we can have:
                a |  b | relsym | method
      ----------------------------------
      1)       >0 | >0 |   >=   |   A
      2)       >0 | >0 |   ==   |   A
      3)       <0 | <0 |   >=   |   A
      4)       >0 | =0 |   ==   |   B
      5)       >0 | <0 |   ==   |   B
      Note:    <0 | >0 |   ==   | impossible by strong normalization
      Note:    <0 | =0 |   ==   | impossible by strong normalization
      Note:    <0 | <0 |   ==   | impossible by strong normalization
      6)       >0 | <0 |   >=   |   B
      7)       >0 | =0 |   >=   |   C
      8)       <0 | >0 |   >=   |   A
      9)       <0 | =0 |   >=   |   A

      The next lines will apply the correct method to each case.
      */

      // The variable index is not equal to the column index.
      const dimension_type nonzero_var_index = nonzero_coeff_column_index - 1;

      const int sgn_a = sgn(cs_i[nonzero_coeff_column_index]);
      const int sgn_b = sgn(cs_i[0]);
      // Cases 1-3: apply method A.
      if (sgn_a == sgn_b) {
	if (cs_i.is_ray_or_point_or_inequality())
	  ++num_slack_variables;
      }
      // Cases 4-5: apply method B.
      else if (cs_i.is_line_or_equality()) {
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  --tableau_num_cols;
	}
      }
      // Case 6: apply method B.
      else if (sgn_b < 0) {
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  --tableau_num_cols;
	}
	++num_slack_variables;
      }
      // Case 7: apply method C.
      else if (sgn_a > 0) {
	// This is the most important case in the incrementality solving:
	// unsplit two Variables.
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  --tableau_num_cols;
	  if (nonzero_coeff_column_index < mapping_size)
	    unsplit(nonzero_coeff_column_index, unfeasible_tableau_rows);
	  is_tableau_constraint[i] = false;
	}
	else
	  is_tableau_constraint[i] = false;
	--tableau_num_rows;
      }
      // Cases 8-9: apply method A.
      else
	++num_slack_variables;
    }
  }
  return true;
}


bool
PPL::LP_Problem::process_pending_constraints() {
  const dimension_type num_original_rows = tableau.num_rows();
  const dimension_type new_c_sd = pending_input_cs.space_dimension();
  const dimension_type input_cs_sd = input_cs.space_dimension();
  const dimension_type pending_cs_num_rows = pending_input_cs.num_rows();
  const dimension_type pending_cs_num_cols = pending_input_cs.num_columns();
  dimension_type new_rows, new_slacks, new_var_columns = 0;
  std::deque<bool> is_tableau_constraint;
  std::deque<bool> nonnegative_variable;
  std::vector<dimension_type> unfeasible_tableau_rows;
  std::deque<bool> satisfied_ineqs;
  // Check the new constraints to adjust the data structures.
  // If `false' is returned, the pending constraints are trivially
  // unfeasible. Call `parse_constraints' with `bootstrap' parameter set
  // to false: *this now has have a feasible point.
  if (!parse_constraints(pending_input_cs, new_rows,
			 new_slacks, is_tableau_constraint,
			 nonnegative_variable, unfeasible_tableau_rows,
			 satisfied_ineqs, false)) {
    // Insert the pending constraints in `input_cs'.
    for (dimension_type i = pending_cs_num_rows; i-- > 0; )
      input_cs.insert(pending_input_cs[i]);

    // The pending constraints are no more pending.
    pending_input_cs.clear();
    status = UNSATISFIABLE;
    return false;
  };

  const dimension_type first_free_tableau_index = tableau.num_columns()-1;
  if (new_c_sd > input_cs_sd) {
    const dimension_type space_diff = new_c_sd - input_cs_sd;
    for (dimension_type i = 0, j = 0; i < space_diff; ++i, ++j) {
      // Set `mapping' properly to store that every variable is split.
      // In the folliwing case the value of the orginal Variable can be
      // negative.
      if (!nonnegative_variable[input_cs_sd+i]) {
	mapping.push_back(std::make_pair(first_free_tableau_index+j,
					 first_free_tableau_index+1+j));
	++j;
	// These are not artificial variables.
	is_artificial.push_back(false);
	is_artificial.push_back(false);
	new_var_columns += 2;
      }
      // The variable is nonnegative.
      else {
	mapping.push_back(std::make_pair(first_free_tableau_index+j, 0));
	// This is not an artificial variable.
	is_artificial.push_back(false);
	++new_var_columns;
      }
    }
  }

  // Resize the tableau and adding the necessary columns for artificial and
  // slack variables.
  dimension_type num_satisfied_ineqs = 0;
  num_satisfied_ineqs = std::count(satisfied_ineqs.begin(),
				   satisfied_ineqs.end(), true);
  const dimension_type unfeasible_tableau_rows_size =
    unfeasible_tableau_rows.size();
  const dimension_type artificial_cols = new_rows +
    unfeasible_tableau_rows_size - num_satisfied_ineqs;
  const dimension_type new_total_columns = new_var_columns + new_slacks +
    artificial_cols;
  if (new_rows > 0)
    tableau.add_zero_rows(new_rows, Row::Flags());
  if (new_total_columns > 0)
    tableau.add_zero_columns(new_total_columns);
  dimension_type tableau_num_rows = tableau.num_rows();

  // The following vector will be useful know if a constraint is feasible
  // and doesn't require an additional artificial variable.
  std::deque<bool> worked_out_row (tableau_num_rows, false);
  dimension_type tableau_num_columns = tableau.num_columns();

  // Sync the `base' vector size to the new tableau: fill with zeros to encode
  // that these rows are not OK and must be adjusted.
  base.insert(base.end(), new_rows, 0);
  const dimension_type base_size = base.size();

  // These indexes will be used to insert slack and artificial variables.
  dimension_type slack_index = tableau_num_columns - artificial_cols - 1;
  dimension_type artificial_index = slack_index;

  // Proceed with the insertion of the constraints.
  for (dimension_type k = tableau_num_rows, i = pending_cs_num_rows; i-- > 0; )
    if (is_tableau_constraint[i]) {
      // Copy the original constraint in the tableau.
      Row& tableau_k = tableau[--k];
      const Linear_Row& cs_i = pending_input_cs[i];
      for (dimension_type j = pending_cs_num_cols; j-- > 0; ) {
	tableau_k[mapping[j].first] = cs_i[j];
	// Split if needed.
	if (mapping[j].second != 0)
	  tableau_k[mapping[j].second] = -cs_i[j];
      }
      // Add the slack variable, if needed.
      if (cs_i.is_ray_or_point_or_inequality()) {
	tableau_k[--slack_index] = -1;
	is_artificial.push_back(false);
	// If the constraint is already satisfied, we will not use artificial
	// variables to compute a feasible base: this to speed up
	// the algorithm.
	if (satisfied_ineqs[i]) {
 	  base[k] = slack_index;
 	  worked_out_row[k] = true;
	}
      }
      for (dimension_type j = base_size; j-- > 0; )
	if (k != j && tableau_k[base[j]] != 0 && base[j] != 0)
	  linear_combine(tableau_k, tableau[j], base[j]);
    }

  // We negate the row if tableau[i][0] <= 0 to get the inhomogeneous term > 0.
  // This simplifies the insertion of the artificial variables: the value of
  // each artificial variable will be 1.
  for (dimension_type i = tableau_num_rows; i-- > 0 ; ) {
    Row& tableau_i = tableau[i];
    if (tableau_i[0] > 0)
      for (dimension_type j = tableau_num_columns; j-- > 0; )
	neg_assign(tableau_i[j]);
  }

  // Sync `is_artificial'.
  if (artificial_cols > 0)
    is_artificial.insert(is_artificial.end(), artificial_cols, true);

  // Set the working cost function with the right size.
  working_cost = Row(tableau_num_columns, Row::Flags());

  // Insert artificial variables for the nonfeasible constraints.
  for (dimension_type i = 0; i < unfeasible_tableau_rows_size; ++i) {
    tableau[unfeasible_tableau_rows[i]][artificial_index] = 1;
    working_cost[artificial_index] = -1;
    base[unfeasible_tableau_rows[i]] = artificial_index;
    ++artificial_index;
  }

  // Modify the tableau and the new cost function by adding
  // the artificial variables (which enter the base). Note that if an
  // inequality was satisfied by `last_generator', this will be not procesed.
  // This information in encoded in `worked_out_row'.
  // As for the cost function, all the artificial variables should have
  // coefficient -1.
  for (dimension_type i = num_original_rows; i < tableau_num_rows; ++i) {
    if (worked_out_row[i])
      continue;
    tableau[i][artificial_index] = 1;
    working_cost[artificial_index] = -1;
    base[i] = artificial_index;
    ++artificial_index;
  }

  // Set the extra-coefficient of the cost functions to record its sign.
  // This is done to keep track of the possible sign's inversion.
  const dimension_type last_obj_index = working_cost.size() - 1;
  working_cost[last_obj_index] = 1;

  // Express the problem in terms of the variables in base.
  for (dimension_type i = tableau_num_rows; i-- > 0; )
    if(working_cost[base[i]] != 0)
      linear_combine(working_cost, tableau[i], base[i]);

  // Now we are ready to solve the first phase.
  bool first_phase_succesful = compute_simplex();

#if PPL_NOISY_SIMPLEX
  std::cout << "LP_Problem::solve: 1st phase ended at iteration "
 	    << num_iterations << "." << std::endl;
#endif

  // Insert the pending constraints in `input_cs'.
  for (dimension_type i = pending_cs_num_rows; i-- > 0; )
    input_cs.insert(pending_input_cs[i]);

  // The pending constraints are no more pending.
  pending_input_cs.clear();

  if (!first_phase_succesful || working_cost[0] != 0) {
    // The feasible region is empty.
    status = UNSATISFIABLE;
    return false;
  }

  // Prepare *this for a possible second phase.
  erase_artificials();
  compute_generator();
  status = SATISFIABLE;
  assert(OK());
  return true;
}
#if PPL_SIMPLEX_USE_STEEPEST_EDGE_FLOATING_POINT
PPL::dimension_type
PPL::LP_Problem::steepest_edge() const {
  const dimension_type tableau_num_rows = tableau.num_rows();
  assert(tableau_num_rows == base.size());
  double challenger_num = 0;
  double challenger_den = 0;
  double challenger_value = 0;
  double current_value = 0;
  dimension_type entering_index = 0;
  const int cost_sign = sgn(working_cost[working_cost.size() - 1]);
  for (dimension_type j = tableau.num_columns() - 1; j-- > 1; ) {
    const Coefficient& cost_j = working_cost[j];
    if (sgn(cost_j) == cost_sign) {
      // We can't compute the (exact) square root of abs(\Delta x_j).
      // The workaround is to compute the square of `cost[j]'.
      challenger_num = fabs(raw_value(cost_j).get_d());
      // Due to our integer implementation, the `1' term in the denominator
      // of the original formula has to be replaced by `squared_lcm_basis'.
      challenger_den = 1;
      for (dimension_type i = tableau_num_rows; i-- > 0; ) {
	const Coefficient& tableau_ij = tableau[i][j];
	if (tableau_ij != 0) {
	  mpq_class real_coeff(raw_value(tableau[i][j]).get_d(),
			       raw_value(tableau[i][base[i]]).get_d());
	  double float_tableau_value = real_coeff.get_d();
	  challenger_den += float_tableau_value * float_tableau_value;
	}
      }
      // Initialization during the first loop.
      if (entering_index == 0) {
	current_value = challenger_num / sqrt(challenger_den);
	entering_index = j;
 	continue;
      }
      challenger_value = challenger_num / sqrt(challenger_den);
      // Update the values, if the challeger wins.
      if (challenger_value > current_value) {
	std::swap(challenger_value, current_value);
	entering_index = j;
      }
    }
  }
  return entering_index;
}

#else
PPL::dimension_type
PPL::LP_Problem::steepest_edge() const {
  const dimension_type tableau_num_rows = tableau.num_rows();
  assert(tableau_num_rows == base.size());
  // The square of the lcm of all the coefficients of variables in base.
  TEMP_INTEGER(squared_lcm_basis);
  // The normalization factor for each coefficient in the tableau.
  std::vector<Coefficient> norm_factor(tableau_num_rows);
  {
    // Compute the lcm of all the coefficients of variables in base.
    TEMP_INTEGER(lcm_basis);
    lcm_basis = 1;
    for (dimension_type i = tableau_num_rows; i-- > 0; )
      lcm_assign(lcm_basis, tableau[i][base[i]]);
    // Compute normalization factors.
    for (dimension_type i = tableau_num_rows; i-- > 0; )
      exact_div_assign(norm_factor[i], lcm_basis, tableau[i][base[i]]);
    // Compute the square of `lcm_basis', exploiting the fact that
    // `lcm_basis' will no longer be needed.
    lcm_basis *= lcm_basis;
    std::swap(squared_lcm_basis, lcm_basis);
  }

  // Defined here to avoid repeated (de-)allocations.
  TEMP_INTEGER(challenger_num);
  TEMP_INTEGER(scalar_value);
  TEMP_INTEGER(challenger_den);
  TEMP_INTEGER(challenger_value);
  TEMP_INTEGER(current_value);

  TEMP_INTEGER(current_num);
  TEMP_INTEGER(current_den);
  dimension_type entering_index = 0;
  const int cost_sign = sgn(working_cost[working_cost.size() - 1]);
  for (dimension_type j = tableau.num_columns() - 1; j-- > 1; ) {
    const Coefficient& cost_j = working_cost[j];
    if (sgn(cost_j) == cost_sign) {
      // We can't compute the (exact) square root of abs(\Delta x_j).
      // The workaround is to compute the square of `cost[j]'.
      challenger_num = cost_j * cost_j;
      // Due to our integer implementation, the `1' term in the denominator
      // of the original formula has to be replaced by `squared_lcm_basis'.
      challenger_den = squared_lcm_basis;
      for (dimension_type i = tableau_num_rows; i-- > 0; ) {
	const Coefficient& tableau_ij = tableau[i][j];
	// FIXME: the test seems to speed up the GMP computation.
	if (tableau_ij != 0) {
	  scalar_value = tableau_ij * norm_factor[i];
	  add_mul_assign(challenger_den, scalar_value, scalar_value);
	}
      }
      // Initialization during the first loop.
      if (entering_index == 0) {
	std::swap(current_num, challenger_num);
	std::swap(current_den, challenger_den);
	entering_index = j;
 	continue;
      }
      challenger_value = challenger_num * current_den;
      current_value = current_num * challenger_den;
      // Update the values, if the challeger wins.
      if (challenger_value > current_value) {
	std::swap(current_num, challenger_num);
	std::swap(current_den, challenger_den);
	entering_index = j;
      }
    }
  }
  return entering_index;
}
#endif

// See pag. 47 of Papadimitriou.

PPL::dimension_type
PPL::LP_Problem::get_entering_var_index() const {
  // The variable entering the base is the first one whose coefficient
  // in the cost function has the same sign the cost function itself.
  // If no such variable exists, then we met the optimality condition
  // (and return 0 to the caller).

  // Get the "sign" of the cost function.
  const dimension_type cost_sign_index = working_cost.size() - 1;
  const int cost_sign = sgn(working_cost[cost_sign_index]);
  assert(cost_sign != 0);
  for (dimension_type i = 1; i < cost_sign_index; ++i)
    if (sgn(working_cost[i]) == cost_sign)
      return i;
  // No variable has to enter the base:
  // the cost function was optimized.
  return 0;
}

void
PPL::LP_Problem::linear_combine(Row& x,
				const Row& y,
				const dimension_type k) {
  assert(x.size() == y.size());
  assert(y[k] != 0 && x[k] != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  TEMP_INTEGER(normalized_x_k);
  TEMP_INTEGER(normalized_y_k);
  normalize2(x[k], y[k], normalized_x_k, normalized_y_k);
  for (dimension_type i = x.size(); i-- > 0; )
    if (i != k) {
      Coefficient& x_i = x[i];
      x_i *= normalized_y_k;
#if 1
      // FIXME: the test seems to speed up the GMP computation.
      const Coefficient& y_i = y[i];
      if (y_i != 0)
	sub_mul_assign(x_i, y_i, normalized_x_k);
#else
      sub_mul_assign(x_i, y[i], normalized_x_k);
#endif
    }
  x[k] = 0;
  x.normalize();
}

// See pag 42-43 of Papadimitriou.

void
PPL::LP_Problem::pivot(const dimension_type entering_var_index,
		       const dimension_type exiting_base_index) {
  const Row& tableau_out = tableau[exiting_base_index];
  // Linearly combine the constraints.
  for (dimension_type i = tableau.num_rows(); i-- > 0; ) {
    Row& tableau_i = tableau[i];
    if (i != exiting_base_index && tableau_i[entering_var_index] != 0)
      linear_combine(tableau_i, tableau_out, entering_var_index);
  }
  // Linearly combine the cost function.
  if (working_cost[entering_var_index] != 0)
    linear_combine(working_cost, tableau_out, entering_var_index);
  // Adjust the base.
  base[exiting_base_index] = entering_var_index;
}

// See pag. 47 + 50 of Papadimitriou.

PPL::dimension_type
PPL::LP_Problem
::get_exiting_base_index(const dimension_type entering_var_index) const  {
  // The variable exiting the base should be associated to a tableau
  // constraint such that the ratio
  // tableau[i][entering_var_index] / tableau[i][base[i]]
  // is strictly positive and minimal.

  // Find the first tableau constraint `c' having a positive value for
  //   tableau[i][entering_var_index] / tableau[i][base[i]]
  const dimension_type tableau_num_rows = tableau.num_rows();
  dimension_type exiting_base_index = tableau_num_rows;
  for (dimension_type i = 0; i < tableau_num_rows; ++i) {
    const Row& t_i = tableau[i];
    const int num_sign = sgn(t_i[entering_var_index]);
    if (num_sign != 0 && num_sign == sgn(t_i[base[i]])) {
      exiting_base_index = i;
      break;
    }
  }
  // Check for unboundedness.
  if (exiting_base_index == tableau_num_rows)
    return tableau_num_rows;

  // Reaching this point means that a variable will definitely exit the base.
  TEMP_INTEGER(lcm);
  TEMP_INTEGER(current_min);
  TEMP_INTEGER(challenger);
  for (dimension_type i = exiting_base_index + 1; i < tableau_num_rows; ++i) {
    const Row& t_i = tableau[i];
    const Coefficient& t_ie = t_i[entering_var_index];
    const Coefficient& t_ib = t_i[base[i]];
    const int t_ie_sign = sgn(t_ie);
    if (t_ie_sign != 0 && t_ie_sign == sgn(t_ib)) {
      const Row& t_e = tableau[exiting_base_index];
      const Coefficient& t_ee = t_e[entering_var_index];
      lcm_assign(lcm, t_ee, t_ie);
      exact_div_assign(current_min, lcm, t_ee);
      current_min *= t_e[0];
      current_min = abs(current_min);
      exact_div_assign(challenger, lcm, t_ie);
      challenger *= t_i[0];
      challenger = abs(challenger);
      current_min -= challenger;
      const int sign = sgn(current_min);
      if (sign > 0
	  || (sign == 0 && base[i] < base[exiting_base_index]))
	exiting_base_index = i;
    }
  }
  return exiting_base_index;
}

// See pag 49 of Papadimitriou.

#if PPL_SIMPLEX_USE_STEEPEST_EDGE_FLOATING_POINT
bool
PPL::LP_Problem::compute_simplex() {
  const dimension_type allowed_non_increasing_loops = 200;
  dimension_type non_increased_times = 0;
  bool call_textbook = false;
  Coefficient cost_sgn_coeff = working_cost[working_cost.size()-1];
  Coefficient current_num = working_cost[0]*sgn(cost_sgn_coeff);
  Coefficient current_den = abs(cost_sgn_coeff);
  assert(tableau.num_columns() == working_cost.size());
  const dimension_type tableau_num_rows = tableau.num_rows();
  while (true) {
    // Choose the index of the variable entering the base, if any.
    const dimension_type entering_var_index = call_textbook ?
      get_entering_var_index() : steepest_edge();

    // If no entering index was computed, the problem is solved.
    if (entering_var_index == 0)
      return true;

    // Choose the index of the row exiting the base.
    const dimension_type exiting_base_index
      = get_exiting_base_index(entering_var_index);
    // If no exiting index was computed, the problem is unbounded.
    if (exiting_base_index == tableau_num_rows)
      return false;

    // We have not reached the optimality or unbounded condition:
    // compute the new base and the corresponding vertex of the
    // feasible region.
    pivot(entering_var_index, exiting_base_index);

    // Now begins the objective function's value check to choose between
    // the  `textbook' and the float `steepest-edge` technique.
    cost_sgn_coeff = working_cost[working_cost.size()-1];
    Coefficient challenger = working_cost[0] * sgn(cost_sgn_coeff);
    Coefficient current = current_num * abs(cost_sgn_coeff);
    //  If the following condition fails, probably there's a bug.
    assert(challenger >= current);
      // If the value of the objective function doesn't improve,
      // keep track of that.
    if (challenger == current) {
      ++non_increased_times;
      // In the following case we will proceed using the `textbook'
      // technique, until the objective function is not improved.
      if (non_increased_times > allowed_non_increasing_loops)
	call_textbook = true;
    }
    // The objective function has an improvement, reset `non_increased_times'.
    else {
      non_increased_times = 0;
      if (call_textbook)
	call_textbook = false;
      current_num = working_cost[0]*sgn(working_cost[working_cost.size()-1]);
      current_den = abs(working_cost[working_cost.size()-1]);
    }
#if PPL_NOISY_SIMPLEX
    ++num_iterations;
    if (num_iterations % 200 == 0)
      std::cout << "Primal Simplex: iteration "
		<< num_iterations << "." << std::endl;
#endif
  }
}

#else
bool
PPL::LP_Problem::compute_simplex() {
  assert(tableau.num_columns() == working_cost.size());
  const dimension_type tableau_num_rows = tableau.num_rows();
  while (true) {
    // Choose the index of the variable entering the base, if any.
    const dimension_type entering_var_index = steepest_edge();
    // If no entering index was computed, the problem is solved.
    if (entering_var_index == 0)
      return true;

    // Choose the index of the row exiting the base.
    const dimension_type exiting_base_index
      = get_exiting_base_index(entering_var_index);
    // If no exiting index was computed, the problem is unbounded.
    if (exiting_base_index == tableau_num_rows)
      return false;

    // We have not reached the optimality or unbounded condition:
    // compute the new base and the corresponding vertex of the
    // feasible region.
    pivot(entering_var_index, exiting_base_index);
#if PPL_NOISY_SIMPLEX
    ++num_iterations;
    if (num_iterations % 200 == 0)
      std::cout << "Primal Simplex: iteration "
                << num_iterations << "." << std::endl;
#endif
  }
}
#endif

// See pag 28  Papadimitriou.
void
PPL::LP_Problem::prepare_first_phase() {
  // We negate the row if tableau[i][0] <= 0 to get the inhomogeneous term > 0.
  // This simplifies the insertion of the artificial variables: the value of
  // each artificial variable will be 1.
  const dimension_type tableau_old_n_cols = tableau.num_columns();
  for (dimension_type i = tableau.num_rows(); i-- > 0 ; ) {
    Row& tableau_i = tableau[i];
    if (tableau_i[0] > 0)
      for (dimension_type j = tableau_old_n_cols; j-- > 0; )
	neg_assign(tableau_i[j]);
  }

  // Add the columns for all the artificial variables, plus an additional
  // column for the sign of the cost function.
  tableau.add_zero_columns(tableau.num_rows() + 1);

  for (dimension_type i = tableau.num_columns()-1 ; i-- > 0; )
    is_artificial.push_back(false);

  // Set the working cost function with the right size.
  working_cost = Row(tableau.num_columns(), Row::Flags());

  // Modify the tableau and the new cost function by adding
  // the artificial variables (which enter the base).
  // As for the cost function, all the artificial variables should have
  // coefficient -1.
  for (dimension_type i = 0; i < tableau.num_rows(); ++i) {
    const dimension_type j = tableau_old_n_cols + i;
    tableau[i][j] = 1;
    working_cost[j] = -1;
    base[i] = j;
    is_artificial[j] = true;
  }

  // Set the extra-coefficient of the cost functions to record its sign.
  // This is done to keep track of the possible sign's inversion.
  const dimension_type last_obj_index = working_cost.size() - 1;
  working_cost[last_obj_index] = 1;

  // Express the problem in terms of the variables in base.
  for (dimension_type i = tableau.num_rows(); i-- > 0; )
    linear_combine(working_cost, tableau[i], base[i]);
}

//See pag 55-56 Papadimitriou.
void
PPL::LP_Problem::erase_artificials() {
  const dimension_type tableau_last_index = tableau.num_columns() - 1;
  dimension_type tableau_n_rows = tableau.num_rows();
  const dimension_type is_artificial_size = is_artificial.size();
  // Step 1: try to remove from the base all the remaining slack variables.
  for (dimension_type i = 0; i < tableau_n_rows; ++i)
    if (is_artificial[base[i]]) {
      // Search for a non-zero element to enter the base.
      Row& tableau_i = tableau[i];
      bool redundant = true;
      for (dimension_type j = is_artificial_size; j-- > 1; )
	if (!is_artificial[j] && tableau_i[j] != 0) {
	  pivot(j, i);
	  redundant = false;
	  break;
	}
      if (redundant) {
	// No original variable entered the base:
	// the constraint is redundant and should be deleted.
	--tableau_n_rows;
	if (i < tableau_n_rows) {
	  // Replace the redundant row with the last one,
	  // taking care of adjusting the iteration index.
	  tableau_i.swap(tableau[tableau_n_rows]);
	  base[i] = base[tableau_n_rows];
	  --i;
	}
	tableau.erase_to_end(tableau_n_rows);
	base.pop_back();
      }
    }

  // Step 2: Adjust data structures so as to enter phase 2 of the simplex.

  // Compute the dimensions of the new tableau.
  dimension_type num_artificials = 0;
  for (dimension_type i = is_artificial.size(); i-- > 1; )
    if (is_artificial[i]) {
      ++num_artificials;
      if (i != is_artificial.size()-1) {
	// WARNING: this case, at the moment is not possible. The
	// following code (still rough and not working) will be useful
	// when `incrementality' will be modified.
	assert(false);
	tableau.swap_columns(i, tableau.num_columns()-1);
	is_artificial[i] = is_artificial[is_artificial.size()-1];
      }
      tableau.remove_trailing_columns(1);
      is_artificial.pop_back();
    }

  // Zero the last column of the tableau.
  for (dimension_type i = tableau_n_rows; i-- > 0; )
    tableau[i][tableau.num_columns()-1] = 0;

  // ... then properly set the element in the (new) last column,
  // encoding the kind of optimization; ...
  working_cost[tableau.num_columns()-1] = working_cost[tableau_last_index];
  // ... and finally remove redundant columns.
  const dimension_type working_cost_new_size = working_cost.size() -
    num_artificials;
  working_cost.shrink(working_cost_new_size);
}

// See pag 55 of Papadimitriou.

PPL::LP_Problem_Status
PPL::LP_Problem::compute_tableau() {
  dimension_type tableau_num_rows = 0;
  dimension_type num_slack_variables = 0;
  std::deque<bool> is_tableau_constraint;
  std::deque<bool> nonnegative_variable;
  std::deque<bool> satisfied_ineqs;
  // Totally useless in this case because the tableau now is empty.
  std::vector<dimension_type> unfeasible_tableau_rows;
  // Call `parse_constraints' with `bootstrap' parameter set to true:
  // *this now doesn't have a feasible point.
  if (!parse_constraints(input_cs, tableau_num_rows, num_slack_variables,
			 is_tableau_constraint, nonnegative_variable,
			 unfeasible_tableau_rows, satisfied_ineqs, true))
    return UNFEASIBLE_LP_PROBLEM;
  Linear_System& cs = input_cs;
  const dimension_type cs_num_rows = cs.num_rows();
  const dimension_type cs_num_cols = cs.num_columns();

  dimension_type tableau_num_cols = 2*cs_num_cols - 1;
  // The slack variables will be columns in the tableau.
  dimension_type num_nonnegative = std::count(nonnegative_variable.begin(),
					      nonnegative_variable.end(), true);

  tableau_num_cols += num_slack_variables - num_nonnegative;
  mapping.push_back(std::make_pair(0, 0));
  for (dimension_type i = 1; i < input_cs.num_columns(); ++i)
    mapping.push_back(std::make_pair(i, 0));

  // Now we can update `mapping'.
  for (dimension_type i = 0, j = nonnegative_variable.size(),
	 nnv_size = j; i < nnv_size; ++i)
    if (!nonnegative_variable[i]) {
      mapping[i+1].second = j+1;
      ++j;
    }

  // Step 2:
  // set the dimensions for the tableau.
  if (tableau_num_rows > 0)
    tableau.add_zero_rows_and_columns(tableau_num_rows,
				      tableau_num_cols,
				      Row::Flags());

  // Phase 3:
  // insert all the (possibly transformed) constraints that are not
  // nonnegativity constraints. The transformation includes both
  // the variable splitting (for variables that are unconstrained
  // in sign) and the addition of slack variables (for inequalities
  // in the original problem).

  for (dimension_type k = tableau_num_rows, slack_index = tableau_num_cols,
	 i = cs_num_rows; i-- > 0; )
    if (is_tableau_constraint[i]) {
      // Copy the original constraint in the tableau.
      Row& tableau_k = tableau[--k];
      const Linear_Row& cs_i = cs[i];
      for (dimension_type j = cs_num_cols; j-- > 0; )
	tableau_k[j] = cs_i[j];
      // Add the slack variable, if needed.
      if (cs_i.is_ray_or_point_or_inequality())
	tableau_k[--slack_index] = -1;
    }

  // Split the variables in the tableau.
  for (dimension_type i = mapping.size(); i-- > 1; ) {
    if (mapping[i].second != 0) {
      const dimension_type original_var = mapping[i].first;
      const dimension_type split_var = mapping[i].second;
      for (dimension_type j = tableau_num_rows; j-- > 0; ) {
	Row& tableau_j = tableau[j];
	tableau_j[split_var] = -tableau_j[original_var];
      }
    }
  }

  // If there is no constraint in the tableau, then the feasible region
  // is only delimited by non-negativity constraints. Therefore,
  // the problem is unbounded as soon as the cost function has
  // a variable with a positive coefficient.
  if (tableau_num_rows == 0)
    for (dimension_type i = tableau_num_cols; i-- > 1; )
      if (input_obj_function[i] > 0){
	status = UNBOUNDED;
	return UNBOUNDED_LP_PROBLEM;
      }
  // The problem is neither trivially unfeasible nor trivially unbounded.
  // The tableau was successfull computed and the caller has to figure
  // out which case applies.
  status = OPTIMIZED;
  return OPTIMIZED_LP_PROBLEM;
}

void
PPL::LP_Problem::compute_generator() const {
  // We will store in num[] and in den[] the numerators and
  // the denominators of every variable of the original problem.
  const dimension_type original_space_dim = input_cs.space_dimension();
  std::vector<Coefficient> num(original_space_dim);
  std::vector<Coefficient> den(original_space_dim);
  dimension_type row = 0;

  // We start to compute num[] and den[].
  for (dimension_type i = original_space_dim; i-- > 0; ) {
    Coefficient& num_i = num[i];
    Coefficient& den_i = den[i];
    // Get the value of the variable from the tableau
    // (if it is not a basic variable, the value is 0).
    const dimension_type original_var = mapping[i+1].first;
    if (is_in_base(original_var, row)) {
      const Row& t_row = tableau[row];
      if (t_row[original_var] > 0) {
	num_i= -t_row[0];
	den_i= t_row[original_var];
      }
      else {
	num_i= t_row[0];
	den_i= -t_row[original_var];
      }
    }
    else {
      num_i = 0;
      den_i = 1;
    }
    // Check whether the variable was split.
    const dimension_type split_var = mapping[i+1].second;
    if (split_var != 0) {
      // The variable was split: get the value for the negative component,
      // having index mapping[i+1].second .
      // Like before, we he have to check if the variable is in base.
      if (is_in_base(split_var, row)) {
	const Row& t_row = tableau[row];
	TEMP_INTEGER(split_num);
	TEMP_INTEGER(split_den);
	if (t_row[split_var] > 0) {
	  split_num = -t_row[0];
	  split_den = t_row[split_var];
	}
	else {
	  split_num = t_row[0];
 	  split_den = -t_row[split_var];
	}
	// We compute the lcm to compute subsequently the difference
	// between the 2 variables.
	TEMP_INTEGER(lcm);
	lcm_assign(lcm, den_i, split_den);
	exact_div_assign(den_i, lcm, den_i);
	exact_div_assign(split_den, lcm, split_den);
	num_i *= den_i;
	sub_mul_assign(num_i, split_num, split_den);
	if (num_i == 0)
	  den_i = 1;
	else
	  den_i = lcm;
      }
      // Note: if the negative component was not in base, then
      // it has value zero and there is nothing left to do.
    }
  }

  // Compute the lcm of all denominators.
  TEMP_INTEGER(lcm);
  lcm = den[0];
  for (dimension_type i = 1; i < original_space_dim; ++i)
    lcm_assign(lcm, den[i]);
  // Use the denominators to store the numerators' multipliers
  // and then compute the normalized numerators.
  for (dimension_type i = original_space_dim; i-- > 0; ) {
    exact_div_assign(den[i], lcm, den[i]);
    num[i] *= den[i];
  }

  // Finally, build the generator.
  Linear_Expression expr;
  for (dimension_type i = original_space_dim; i-- > 0; )
    expr += num[i] * Variable(i);

  LP_Problem& x = const_cast<LP_Problem&>(*this);
  x.last_generator = point(expr, lcm);
}

void
PPL::LP_Problem::second_phase() {
  // Second_phase requires that *this is satisfiable.
  assert(status == SATISFIABLE || status == UNBOUNDED || status == OPTIMIZED);
  // In the following cases the problem is already solved.
  if (status == UNBOUNDED || status == OPTIMIZED)
    return;

  // Negate the cost function if we are minimizing.
  Row new_cost = input_obj_function;
  if (opt_mode == MINIMIZATION)
    for (dimension_type i = new_cost.size(); i-- > 0; )
      neg_assign(new_cost[i]);

  // Substitute properly the cost function in the `costs' Matrix.
  const dimension_type cost_zero_size = working_cost.size();
  Row tmp_cost = Row(new_cost, cost_zero_size, cost_zero_size);
  tmp_cost.swap(working_cost);
  working_cost[cost_zero_size-1] = 1;

  // Split the variables the cost function.
  for (dimension_type i = mapping.size(); i-- > 1; ) {
    if (mapping[i].second != 0) {
      const dimension_type original_var = mapping[i].first;
      const dimension_type split_var = mapping[i].second;
      working_cost[split_var] = -working_cost[original_var];
    }
  }

  // Here the first phase problem succeeded with optimum value zero.
  // Express the old cost function in terms of the computed base.
  for (dimension_type i = tableau.num_rows(); i-- > 0; ) {
    const dimension_type base_i = base[i];
    if (working_cost[base_i] != 0)
      linear_combine(working_cost, tableau[i], base_i);
  }
  // Solve the second phase problem.
  bool second_phase_successful = compute_simplex();
  compute_generator();

#if PPL_NOISY_SIMPLEX
  std::cout << "LP_Problem::solve: 2nd phase ended at iteration "
	    << num_iterations << "." << std::endl;
#endif
  status = second_phase_successful ? OPTIMIZED : UNBOUNDED;
  assert(OK());
}

void
PPL::LP_Problem::evaluate_objective_function(const Generator& evaluating_point,
					     Coefficient& ext_n,
					     Coefficient& ext_d) const {
  const dimension_type ep_space_dim = evaluating_point.space_dimension();
  if (space_dimension() < ep_space_dim)
    throw std::invalid_argument("PPL::LP_Problem::"
				"evaluate_objective_function(p, n, d):\n"
				"*this and p are dimension incompatible.");
  if (!evaluating_point.is_point())
    throw std::invalid_argument("PPL::LP_Problem::"
				"evaluate_objective_function(p, n, d):\n"
				"p is not a point.");

  // Compute the smallest space dimension  between `input_obj_function'
  // and `evaluating_point'.
  const dimension_type space_dim
    = std::min(ep_space_dim, input_obj_function.space_dimension());
  // Compute the optimal value of the cost function.
  ext_n = input_obj_function.inhomogeneous_term();
  for (dimension_type i = space_dim; i-- > 0; )
    ext_n += evaluating_point.coefficient(Variable(i))
      * input_obj_function.coefficient(Variable(i));
  // Numerator and denominator should be coprime.
  normalize2(ext_n, evaluating_point.divisor(), ext_n, ext_d);
}

// FIXME: assert(OK()) before returning.
bool
PPL::LP_Problem::is_satisfiable() const {
  // Check `status' to filter out trivial cases.
  switch (status) {
  case UNSATISFIABLE:
    assert(OK());
    return false;
  case SATISFIABLE:
    // Intentionally fall through.
  case UNBOUNDED:
    // Intentionally fall through.
  case OPTIMIZED:
    assert(OK());
    return true;
  case PARTIALLY_SATISFIABLE:
    {
      LP_Problem& x = const_cast<LP_Problem&>(*this);
      // Apply incrementality to the pending Constraint_System.
      x.process_pending_constraints();
      if (status == UNSATISFIABLE)
	return false;
      else
	return true;
    }
    break;
  case UNSOLVED:
    break;
  }

  LP_Problem& x = const_cast<LP_Problem&>(*this);
  const dimension_type space_dim = x.space_dimension();
  // Reset internal objects.
  x.tableau.clear();
  x.mapping.clear();
  x.is_artificial.clear();

  // Compute the initial tableau.
  LP_Problem_Status s_status = x.compute_tableau();
  // Check for trivial cases.
  switch (s_status) {
  case UNFEASIBLE_LP_PROBLEM:
    assert(OK());
    return false;
  case UNBOUNDED_LP_PROBLEM:
    // A feasible point has to be returned: the origin.
    // Ensure the right space dimension is obtained.
    x.last_generator = point(0*Variable(space_dim-1));
    assert(OK());
    return true;
  case OPTIMIZED_LP_PROBLEM:
    // Check for the special case of an empty tableau,
    // in which case an optimizing solution is the origin.
    if (x.tableau.num_rows() == 0) {
      // Ensure the right space dimension is obtained.
      x.last_generator = point(0*Variable(space_dim-1));
      assert(OK());
      return true;
    }
    break;
  }

#if PPL_NOISY_SIMPLEX
  num_iterations = 0;
#endif

  // Actually solve the LP problem.
  x.base = std::vector<dimension_type> (x.tableau.num_rows());

  // Adds the necessary slack variables to get the 1st phase problem.
  x.prepare_first_phase();
  // Solve the first phase of the primal simplex algorithm.
  bool first_phase_successful = x.compute_simplex();

#if PPL_NOISY_SIMPLEX
  std::cout << "LP_Problem::solve: 1st phase ended at iteration "
	    << num_iterations << "." << std::endl;
#endif
  // If the first phase problem was not solved or if we found an optimum
  // value different from zero, then the origianl problem is unfeasible.
  if (!first_phase_successful || x.working_cost[0] != 0){
    x.status = UNSATISFIABLE;
    return false;
  }
  // The first phase has found a feasible solution. If only a satisfiability
  // check was requested, we can return that feasible solution.
  // Store the last succesfully computed generator.
  compute_generator();
  x.status = SATISFIABLE;
  // Erase the slack variables.
  x.erase_artificials();
  assert(OK());
  return true;
}

bool
PPL::LP_Problem::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif
  const dimension_type input_sd = input_cs.space_dimension();
  const dimension_type pending_input_sd = pending_input_cs.space_dimension();
  // Constraint system should contain no strict inequalities.
  if (input_cs.has_strict_inequalities()) {
#ifndef NDEBUG
    cerr << "The feasible region of the LP_Problem is defined by "
	 << "a constraint system containing strict inequalities."
	 << endl;
    ascii_dump(cerr);
#endif
    return false;
  }

  // Constraint system and objective function should be dimension compatible.
  const dimension_type space_dim = std::max(input_sd, pending_input_sd);
  if (space_dim < input_obj_function.space_dimension()) {
#ifndef NDEBUG
    cerr << "The LP_Problem and the objective function have "
	 << "incompatible space dimensions ("
	 << space_dim << " < " << input_obj_function.space_dimension() << ")."
	 << endl;
    ascii_dump(cerr);
#endif
    return false;
  }

  if (status != UNSOLVED && status != UNSATISFIABLE)  {
    // Here `last_generator' has to be meaningful.
    // Check for dimension compatibility and actual feasibility.
    if (input_sd != last_generator.space_dimension()) {
#ifndef NDEBUG
      cerr << "The LP_Problem and the cached feasible point have "
 	   << "incompatible space dimensions ("
 	   << input_sd << " != " << last_generator.space_dimension() << ")."
 	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
    if (!input_cs.satisfies_all_constraints(last_generator)) {
#ifndef NDEBUG
      cerr << "The cached feasible point does not belong to "
 	   << "the feasible region of the LP_Problem."
 	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }

    const dimension_type tableau_nrows = tableau.num_rows();
    const dimension_type tableau_ncols = tableau.num_columns();

    // The number of rows in the tableau and base should be equal.
    if (tableau_nrows != base.size()) {
#ifndef NDEBUG
      cerr << "tableau and base have incompatible sizes" << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
    // The number of columns in the tableau and in `is_artificial.size()+1'
    // should be equal.
    if (tableau_ncols != is_artificial.size()+1) {
#ifndef NDEBUG
      cerr << "tableau and `is_artificial' have incompatible sizes" << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
    // The size  of `input_cs' and `mapping' should be equal.
    if (input_cs.num_columns() != mapping.size()) {
#ifndef NDEBUG
      cerr << "`input_cs' and `mapping' have incompatible sizes" << endl;
      ascii_dump(cerr);
#endif
      return false;
    }

    // The number of columns in the tableau and working_cost should be equal.
    if (tableau_ncols != working_cost.size()) {
#ifndef NDEBUG
      cerr << "tableau and working_cost have incompatible sizes" << endl;
      ascii_dump(cerr);
#endif
      return false;
    }

    // The vector base should contain indices of tableau's columns.
    for (dimension_type i = base.size(); i-- > 0; )
      if (base[i] > tableau_ncols) {
#ifndef NDEBUG
	cerr << "base contains an invalid column index" << endl;
	ascii_dump(cerr);
#endif
	return false;
      }
    // The vector base should contain indices of tableau's columns.
    for (dimension_type i = base.size(); i-- > 0; )
      if (tableau[i][base[i]] == 0) {
#ifndef NDEBUG
	cerr << "tableau[base] contains a zero!" << endl;
	ascii_dump(cerr);
#endif
	return false;
      }

    // The last column of the tableau must contain only zeroes.
    for (dimension_type i = tableau_nrows; i-- > 0; )
      if (tableau[i][tableau_ncols-1] != 0) {
#ifndef NDEBUG
	cerr << "the last column of the tableau must contain only"
	  "zeroes"<< endl;
	ascii_dump(cerr);
#endif
	return false;
      }

    // FIXME: still to be completed...
  }

  // All checks passed.
  return true;
}

void
PPL::LP_Problem::ascii_dump(std::ostream& s) const {
  using namespace IO_Operators;

  s << "input_cs\n";
  input_cs.ascii_dump(s);
  s << "pending_input_cs\n";
  pending_input_cs.ascii_dump(s);
  s << "\ninput_obj_function\n";
  input_obj_function.ascii_dump(s);
  s << "\nopt_mode " << (opt_mode == MAXIMIZATION ? "MAX" : "MIN") << "\n";

  s << "\nstatus: ";
  switch (status) {
  case UNSATISFIABLE:
    s << "UNSAT";
    break;
  case SATISFIABLE:
    s << "SATIS";
    break;
  case UNBOUNDED:
    s << "UNBOU";
    break;
  case OPTIMIZED:
    s << "OPTIM";
    break;
  case PARTIALLY_SATISFIABLE:
    s << "P_SAT";
    break;
  case UNSOLVED:
    s << "UNSOL";
    break;
  }
  s << "\n";

  s << "\ntableau\n";
  tableau.ascii_dump(s);
  s << "\nworking_cost(" << working_cost.size()<< ")\n";
  working_cost.ascii_dump(s);

  const dimension_type base_size = base.size();
  s << "\nbase (" << base_size << ")\n";
  for (dimension_type i = 0; i != base_size; ++i)
    s << base[i] << ' ';

  // FIXME: no ascii_dump() for Generator?
  // last_generator.ascii_dump(s);
  s << "\nlast_generator\n";
  s << last_generator << "\n";
  s << std::endl;
  const dimension_type mapping_size = mapping.size();
  s << "\nmapping(" << mapping_size << ")\n";
  for (dimension_type i = 1; i < mapping_size; ++i)
    s << "\n"<< i << "->" << mapping[i].first << "->" << mapping[i].second << ' ';
  const dimension_type is_artificial_size = is_artificial.size();
  s << "\nis_artificial(" << is_artificial_size << ")\n";
  for (dimension_type i = 1; i < is_artificial_size; ++i) {
    s << "\n"<< i << "->";
    is_artificial[i] ?  s << "true" : s << "false";
  }
}
