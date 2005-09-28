/* Constraint_System class implementation: primal_simplex().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include <stdexcept>
#include <sstream>
#include <map>
#include <deque>

namespace {

using namespace Parma_Polyhedra_Library;

//! Linearly combines \p x with \p y so that <CODE>*this[k]</CODE> is 0.
/*!
  \param x
  The Row that will be combined with \p y object.

  \param y
  The Row that will be combined with \p x object.

  \param k
  The position of \p *this that have to be \f$0\f$.

  Computes a linear combination of \p x and \p y having
  the element of index \p k equal to \f$0\f$. Then it assigns
  the resulting Linear_Row to \p x and normalizes it.
*/
void
linear_combine(Row& x, const Row& y, const dimension_type k) {
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
      sub_mul_assign(x_i, y[i], normalized_x_k);
    }
  x[k] = 0;
  x.normalize();
} 

//! Inserts a row in a matrix.
/*!
  \param matrix
  The Matrix in which will be inserted the Row;

  \param row
  The Row that will be inserted with in the \p x Matrix;

  This has the same effects of Linear_System.insert()
  The Matrix grows if the Row has more columns than the Matrix ones.
*/
void
insert_row_in_matrix(Matrix& matrix, const Row& row) {
  dimension_type row_size = row.size();
  dimension_type matrix_num_columns = matrix.num_columns();
  
  if (matrix_num_columns < row_size)
    matrix.add_zero_columns(row_size - matrix_num_columns);
  
  matrix.add_zero_rows(1, Row::Flags());
  dimension_type matrix_num_rows = matrix.num_rows();
  
  for (dimension_type i = 0; i < row_size; ++i) 
    matrix[matrix_num_rows - 1][i] = row[i];
}

//! Append \p k elements to \p r, each with value \p n.
void
add_elements_to_row(Row& r,
		    const dimension_type k,
		    Coefficient_traits::const_reference n) {
  dimension_type r_size = r.size();
  dimension_type s_size = r_size + k;
  Row s(r, s_size, s_size);
  for (dimension_type i = r_size; i < s_size; ++i) 
    s[i] = n;  
  r.swap(s);
}

//! \brief
//! Creates a copy of a column of a matrix.
/*!
  \param tableau
  The matrix that contains the column to be extracted.

  \param index
  The index of the column to be extracted.

  \param column
  A vector where will be stored the column.
*/
inline void 
copy_column(const Matrix& tableau,
	    const dimension_type index,
	    std::vector<Coefficient>& column) {
  const dimension_type tableau_num_rows = tableau.num_rows();
  // Used to avoid memory reallocation during column.push_back().
  column.reserve(tableau_num_rows);
  for (dimension_type i = 0; i < tableau_num_rows; ++i)
    column.push_back(tableau[i][index]);
}

// See pag 42-43 of Papadimitriou. 

//! \brief
//! Swaps two variables in base during the simplex algorithm,
//! performing the needed linear combinations.
/*!
  \param tableau
  The constraints of the LP problem.

  \param cost_function
  The cost function of the LP problem.

  \param inside_var
  The index of the variable entering the base.

  \param outside_row
  The index of the variable exiting the base.
*/
void
swap_base(Matrix& tableau,
	  Row& cost_function,
	  const dimension_type inside_var,
	  const dimension_type outside_row) {
  // Linearly combine the constraints.
  for (dimension_type i = tableau.num_rows(); i-- > 0; )
    if (i != outside_row && tableau[i][inside_var] != 0)
      linear_combine(tableau[i], tableau[outside_row], inside_var);

  // Linearly combine the cost function.
  if (cost_function[inside_var] != 0)
    linear_combine(cost_function, tableau[outside_row], inside_var);
}


// See pag. 47 of Papadimitriou.

//! \brief
//!  Checks for the optimality condition of the LP problem.
//!  If this condition is not satisfied, chooses the variable to
//!  put in the base. Implemented with anti-cycling rule.
/*!
  \return
  <CODE>true</CODE> if and if only the algorithm finds the optimum condition.

  \param expressions
  The matrix that contains the cost function/s.

  \param index_in
  The index of the matrix representing the variable that has to go in base,
  if needed.
*/
inline bool
check_optimality(const Matrix& expressions,
		 dimension_type& index_in) {  
  // If all the coefficients of the cost function are greater or equal
  // than 0, we have reached the optimality condition.
  // The last coefficient represents the sign, so we don't have
  // to check this one. If the optimality condition is not satisfied,
  // we have to choose the variable with the smallest index.
  dimension_type last_column_index = expressions.num_columns()-1;
  
  // We have to remember the "sign" of the cost function:
  // we will use 'sign_value' to get this info. We need this to check the 
  // optimality condition.
  bool sign_value = expressions[0][last_column_index] > 0;
  for (dimension_type i = 1; i < last_column_index; ++i)
    if (sign_value ? expressions[0][i] > 0 : expressions[0][i] < 0) {
      index_in = i;
      return false;
    }
  return true;
}

// See pag. 47 + 50 of Papadimitriou.  

//! \brief
//! Chooses the variable to put out of the base of the LP problem.
//! Implemented with anti-cycling rules.
/*!
  \return
  An integer representing the row that refers to the variable that
  has to go out of the base.

  \param tableau
  The matrix that has to be checked.

  \param index_will_be_in
  The index of the matrix that represents the variable that has to go
  out from the base.

  \param base
  The array that stores the variables in base.

  \param unbounded
  Bool that will be set to <CODE>true<\CODE> if and only if the LP problem
  is unbounded.
*/
inline int
choose_out_var(const Matrix& tableau,  
	       const int index_will_be_in,
	       std::vector<dimension_type>& base,
	       bool& unbounded) {
  // To choose the variable that has to go out from the base, we must 
  // look for the row such that row[index_will_be_in]/row[inhomogeneous_term]
  // is the smallest, but > 0, in all the rows of the tableau.
  
  // To compute the variable that has to go out from the base, we simply need
  // these 2 columns: inhomogeneous terms, variable that has to get in the
  // base(previously computed).
  
  std::vector<Coefficient> inhomogeneous;
  copy_column(tableau, 0, inhomogeneous);
  
  // This one will contain the row indexes of the tableau that will be
  // considered to compute the variable that has to go out from the base.
  std::vector<dimension_type> candidate_constraints;
  
  // We must suppose at this moment that the solution to the LP is unbounded,
  // but if we will find a row such that tableau[index_will_be_in][i] > 0  
  // && tableau[i][base[i]] > 0 or (since we don't store the constraints in a 
  // special way) tableau[index_will_be_in][i] < 0 && tableau[i][base[i]] < 0
  // we will be sure that the solution is not unbounded.
  unbounded = true;
  
  // We want to insert in candidate_constraints the indexes of the rows such
  // that tableau[index_will_be_in][i] > 0 && tableau[i][base[i]] > 0
  // or tableau[index_will_be_in][i] < 0 && tableau[i][base[i]] < 0.
  for (dimension_type i = tableau.num_rows(); i-- > 0; ) {
    const Row& tableau_i = tableau[i];
    const int sgn_a = sgn(tableau_i[index_will_be_in]);
    if (sgn_a != 0 && sgn_a == sgn(tableau_i[base[i]])) {
      candidate_constraints.push_back(i);
      unbounded = false;
    }
  }
 
  // If the problem is unbounded we have finished: in this case the return 
  // value is not important, the calling function will look to the "unbounded"
  // variable first.
  if (unbounded)
    return 0;
  
  // Now we are sure that we can compute the out_variable_row index.
  // So we start computing the lcm of the var_in_column rows in the candidate
  // constraints.
  dimension_type candidate_constraints_size = candidate_constraints.size();
  Coefficient lcm = 1;
  
  // We are not interested about the sign of 'lcm'. We will work with 
  // an absolute value later.
  for (dimension_type i = candidate_constraints_size; i-- > 0; )
    lcm_assign(lcm, tableau[candidate_constraints[i]][index_will_be_in]);
  
  // We proceed computing the right representation of inhomogeneous terms.
  for (dimension_type i = candidate_constraints_size; i-- > 0; ) {  
    const dimension_type& index = candidate_constraints[i];
    inhomogeneous[index] *= abs(lcm / tableau[index][index_will_be_in]);
  }
  // On exit this will contain the column index of tableau of the variable
  // that has to go out from the base. At the begin we suppose that the first
  // row indexed by candidate_constraints.
  dimension_type out_var_row = candidate_constraints[0];  
  
  // This is used to store the value of the inhomogeneous term of the row
  // that express the variable that has to go out from the base.
  Coefficient var_value = abs(inhomogeneous[candidate_constraints[0]]);
  
  // Now we proceed comparing the other rows indexed by candidate_constraints
  // with the first one.
  for (dimension_type i = 0; i < candidate_constraints_size; ++i) {
    const dimension_type& checked_index = candidate_constraints[i];
    const Coefficient abs_checked = abs(inhomogeneous[checked_index]);
    if (abs_checked < var_value) {
      var_value = abs_checked; 
      out_var_row = checked_index;
    }
    // But if we find a variable with the same value of the inhomogeneous
    // term of the last one chosen, but its index is smaller, we choose
    // that one. (Anti-cycling Rule)
    else if (abs_checked == var_value
	     && base[checked_index] < base[out_var_row])
	out_var_row = checked_index;
  }
  
  // Now we can set the "base" vector with the new computed base.
  base[out_var_row] = index_will_be_in;
  return out_var_row;
}

// See pag 49 of Papadimitriou.

//! \brief
//! The simplex algorithm.
/*! 
  \return
  <CODE>true</CODE> if and if only the algorithm successfully computed
  a feasible solution.

  \param tableau
  The constraints of the LP problem.

  \param expressions
  The matrix that contains the cost function-s to be maximized/minimized. 

  \param base
  The variables in the base of the LP problem.

  To solve the LP problem compute_simplex must receive the problem in this way:

  1) Linear Expressions:
  k + x_1 + x_2 + ... + x_n + s (with s > 0)
  where k is the inhomogeneous term, x_i are the coefficients of the variables
  and s is a special variable that stands for the sign of the cost function.
  In this way is possible to know if the cost function was reversed during 
  the linear combinations. Reasoning in a "PPL way" we can think that this 
  row stands for k + x_1 + x_2 + s = 0, so -s = k + x_1 + x_2 + ...  + x_n.
  We use a matrix instead of a row because in this way is very simple to have
  the cost function of the second_phase. The old objective cost function, if
  compute_simplex is called by first_phase, is stored in the second row.
  
  2) Constraints:
  These are the standard "PPL rows", so there should not be special 
  problems to understand what's in this matrix.
  
  It is assumed that \p tableau and \p expressions have the same
  number of columns (i.e., the same space dimension).
*/
bool
compute_simplex(Matrix& tableau,
		Matrix& expressions,
		std::vector<dimension_type>& base) {
  assert(tableau.num_columns() == expressions.num_columns());
  while (true) {
    // In this one will be stored the variable that will go in base
    // if and only if the optimality condition is not satisfied.
    dimension_type index_will_be_in;
    if (check_optimality(expressions, index_will_be_in)) {
      // If we are in the 1st phase, we have to express the old cost function
      // terms of the computed base.
      if (expressions.num_rows() == 2) {
	for (dimension_type i = tableau.num_rows(); i-- > 0; ) 
	  if (expressions[1][base[i]] != 0)
	    linear_combine(expressions[1], tableau[i], base[i]);
      }
      return true;
    }
    
    // We have to choose the row the contains the variable to get out
    // from the base.
    bool unbounded = false;
    dimension_type out_var_row;
    out_var_row = choose_out_var(tableau, index_will_be_in, base, unbounded);
    
    // During the search of the variable that has to get out from the base,
    // we can know if the problem is unbounded or not.
    if (unbounded) 
      return false;
    
    // We have not reached the optimality or unbounded condition.
    // So we have to reach another vertex of the Polyhedron.
    swap_base(tableau, expressions[0], index_will_be_in, out_var_row);
  }
}

// See pag 28  Papadimitriou.

//! \brief
//! Adds the slack variables to satisfy the standard form of a LP problem,
//! inserts the "sign" to the cost functions, and makes the
//! necessary swaps to express the problem with the 1st phase base.
/*!
  \param tableau
  The matrix containing the LP problem.

  \param obj
  The row containing the original cost function.

  \param base
  The vector that stores the variables in the base of the LP problem.
*/
void   
prepare_for_1st_ph_simplex(Matrix& tableau,
			   Row& old_obj,
			   Row& new_obj,
			   std::vector<dimension_type>& base) {
  // We negate the row if tableau[i][0] <= 0 to get the inhomogeneous term > 0.
  // This simplifies the insertion of the slack variables: the value of the 
  // slack variable of every constraint will be 1.
  for (dimension_type i = tableau.num_rows(); i-- > 0 ; ) 
    if (tableau[i][0] > 0)
      for (dimension_type j = tableau.num_columns(); j-- > 0; )
	negate(tableau[i][j]);
  // Insertion of the slack variables.
  for (dimension_type i = 0; i < tableau.num_rows(); ++i) {
    tableau.add_zero_columns(1);
    tableau[i][tableau.num_columns()-1] = 1;
    base[i] = tableau.num_columns()-1;
  }
  
  // We add the extra-coefficient (sign) for the cost function to the tableau.
  // This is done to keep track of the possible sign`s inversion.
  // We have to resize the old and the new cost function row and add the
  // extra-coefficient also to the old cost function.

  // Here we set the 1st phase cost function: if the first slack variable has 
  // index n+1 and the space dimension is n+m, the new cost function is:
  // k = - x_n+1 - x_n+2 - ... - x_n+m.  
  add_elements_to_row(new_obj, tableau.num_rows(), -1);

  // We have also to resize the old cost function.
  add_elements_to_row(old_obj, tableau.num_rows(), 0);
 
  // Extra variable insertion (sign).
  add_elements_to_row(new_obj, 1, 1);
  add_elements_to_row(old_obj, 1, 1);
  
  // We have to add another column to the tableau to allow linear combinations:
  // the rows must have the same space dimension.
  tableau.add_zero_columns(1);
  
  // Now we express the problem in terms of the variable in base.
  for (dimension_type i = tableau.num_rows(); i-- > 0; ) 
   linear_combine(new_obj, tableau[i], base[i]);              
  
}

// See pag 55-56 Papadimitriou.

//! \brief
//! Deletes the non necessary slack variables from the matrix
//! and prepares for the 2nd phase.
/*!
  \param tableau
  The matrix containing the constraints of the LP problem.

  \param costs
  The matrix containing the cost functions.

  \param base
  The variables in base of the LP problem.
*/
void erase_slacks(Matrix& tableau,
		  Matrix& costs,
		  std::vector<dimension_type>& base) {
  const dimension_type tableau_last_index = tableau.num_columns() - 1;
  dimension_type tableau_n_rows = tableau.num_rows();
  const dimension_type first_slack_index = tableau_last_index - tableau_n_rows;

  // Step 1: try to remove from the base all the remaining slack variables.
  for (dimension_type i = 0; i < tableau_n_rows; ++i)
    if (base[i] >= first_slack_index) {
      // Search for a non-zero element to enter the base.
      Row& tableau_i = tableau[i];
      bool redundant = true;
      for (dimension_type j = first_slack_index; j-- > 1; )
	if (tableau_i[j] != 0) {
	  swap_base(tableau, costs[0], j, i);
	  // Adjust the base.
	  base[i] = j;
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
  const dimension_type new_tableau_n_cols = first_slack_index + 1;
  const dimension_type new_tableau_last_index = first_slack_index;
   
  // Adjust the number of columns of `tableau'.
  tableau.remove_trailing_columns(tableau.num_columns() - new_tableau_n_cols);
  // Zero the last column of the tableau.
  for (dimension_type i = tableau_n_rows; i-- > 0; )
    tableau[i][new_tableau_last_index] = 0;

  // Set the new cost function:
  // first let the second expression become the only one; ...
  Row& cost_function = costs[0];
  cost_function.swap(costs[1]);
  costs.erase_to_end(1);
  // ... then properly set the element in the (new) last column,
  // encoding the kind of optimization; ...
  cost_function[new_tableau_last_index] = cost_function[tableau_last_index];
  // ... and finally remove redundant columns.
  costs.remove_trailing_columns(tableau_last_index - new_tableau_last_index);
}

// See pag 55 of Papadimitriou.

//! \brief
//! First phase of the simplex algorithm. Computes a feasible base and, if 
//! possible, solves also the second phase.
/*!
  \return
  <CODE>SOLVED_PROBLEM></CODE> if the optimization problem is both
  feasible and bounded;
  <CODE>UNFEASIBLE_PROBLEM</CODE> if the problem has no feasible solution;
  <CODE>UNBOUNDED_PROBLEM</CODE> if the problem is feasible but unbounded.

  \param tableau
  The matrix containing the constraints of the LP problem, given in
  the "compute_tableau" way.

  \param base
  The vector that stores the variables in the base of a LP problem.
*/ 
Simplex_Status
first_phase(Matrix& tableau,
	    Row& old_obj_function,
	    std::vector<dimension_type>& base) {
  // This will contain the new cost function of the 1st phase problem.
  Row new_obj_function(old_obj_function);
  
  // We need to clean this row setting all the values to 0.
  for (dimension_type i = new_obj_function.size(); i-- > 0; )  
    new_obj_function[i] = 0;
  
  // Adds the necessary slack variables to get the 1st phase problem.
  prepare_for_1st_ph_simplex(tableau, old_obj_function,
			     new_obj_function, base);
    
  // This code will build a matrix where will be stored only
  // the constraints of the LP problem.
#if 0
  Matrix tableau_constraints(0,0);
  for (dimension_type i = 0; i < tableau.num_rows(); ++i)
   insert_row_in_matrix(tableau_constraints, tableau[i]);              
#else
  Matrix& tableau_constraints = tableau;
#endif

  // Here we will build a matrix that will store the new cost function in the
  // first row, and the old cost function in the second.
  Matrix tableau_expressions(0,0);
  insert_row_in_matrix(tableau_expressions, new_obj_function); 
  insert_row_in_matrix(tableau_expressions, old_obj_function);

  // Now we can start solving the first phase problem.
  bool ok = compute_simplex(tableau_constraints, tableau_expressions, base); 
  
  // If the first phase problem succeeds and the optimum value is zero,
  // we proceed erasing the slack variables we introduced and solve the
  // problem with the base computed by the 1st phase.
  
  if (ok && tableau_expressions[0][0] == 0) {
    erase_slacks(tableau_constraints, tableau_expressions, base);
    bool result = compute_simplex(tableau_constraints,
				  tableau_expressions, 
				  base);
#if 0
    tableau.swap(tableau_constraints); 
#endif

    // Now if (result == true) we have an optimum, else
    // the problem is unbounded.
    if (result)
      return SOLVED_PROBLEM;
    else
      return UNBOUNDED_PROBLEM;
  }
  else
    // No feasible base found.
    return UNFEASIBLE_PROBLEM;
}


//! \brief
//! Assigns to \p tableau a simplex tableau representing the problem
//! given by the constraints in \p cs and the cost function \p expr,
//! inserting into \p map the informations that are required to go
//! back to the original problem.
/*!
  \return
  <CODE>UNFEASIBLE_PROBLEM</CODE> if the constraint system contains
  any trivially unfeasible constraint (tableau was not computed);
  <CODE>UNBOUNDED_PROBLEM</CODE> if an empty tableau was computed
  (the problem may be actually bounded, depending on the cost function);
  <CODE>SOLVED_PROBLEM></CODE> if a non-empty tableau was computed.

  \param cs
  A matrix containing the constraints of the problem.

  \param cost_function
  The cost function of the problem.

  \param tableau
  A matrix where to store the resulting tableau.

  \param map
  Contains all the pairs (i, j) such that Variable(i) (that was not found
  to be constrained in sign) has been split into two nonnegative variables.
  The "positive" one is represented again by Variable(i), and
  the "negative" one is represented by Variable(j).
*/
Simplex_Status
compute_tableau(const Linear_System& cs,
		Row& cost_function,
		Matrix& tableau,
		std::map<dimension_type, dimension_type>& dim_map) {
  assert(tableau.num_rows() == 0);
  assert(dim_map.size() == 0);
  const dimension_type cs_num_rows = cs.num_rows();
  const dimension_type cs_num_columns = cs.num_columns();
  // Note: we disregard the topology of the constraint system.
  // Namely, in an NNC constraint system, the epsilon dimension
  // is interpreted as a normal dimension.
  const dimension_type cs_space_dim = cs_num_columns-1;
  
  // Phase 1:
  // determine variables that are constrained to be nonnegative,
  // detect nonnegativity constraints that will not be part of the
  // tableau and count the number of inequalities.
  
  // On exit, `nonnegative_variable[j]' will be true if and only if
  // Variable(j) is bound to be nonnegative in `cs'.
  std::deque<bool> nonnegative_variable(cs_space_dim, false);
  
  // On exit, `trivially_true_constraint[j]' will be true if and only if
  // `cs[j]' is trivially true: it will not be inserted in the tableau.
  std::deque<bool> trivially_true_constraint(cs_num_rows, false);

  // On exit, this will hold the number of nonnegative variables.
  dimension_type num_nonnegative = 0;
  
  // On exit, `nonnegativity_constraint[i]' will be true if and only if
  // `cs[i]' contains a constraint that only expresses the non-negativity
  // of one variable.
  std::deque<bool> nonnegativity_constraint(cs_num_rows, false);
  dimension_type nonnegativity_constraint_number = 0;
  
  // On exit, `num_inequalities_left' will contain the number of
  // inequalities that convey more than the non-negativity of one
  // variables.  For each one of these inequalities we will have to
  // introduce a slack variable.
  dimension_type num_inequalities_left = 0;
  
  // Process each row of the `cs' matrix.
  for (dimension_type i = cs_num_rows; i-- > 0; ) { 
    const Linear_Row& cs_i = cs[i];
    bool found_a_nonzero_coeff = false;
    bool found_many_nonzero_coeffs = false;
    dimension_type nonzero_coeff_column_index = 0;
    for (dimension_type j = cs_num_columns; j-- > 1; ) {
      if (cs_i[j] != 0)
	if (found_a_nonzero_coeff) {
	  found_many_nonzero_coeffs = true;
	  if (cs_i.is_ray_or_point_or_inequality())
	    ++num_inequalities_left;
	  break;
	}
	else {
	  nonzero_coeff_column_index = j;
	  found_a_nonzero_coeff = true;
	}
    }
    // If more than one coefficient is nonzero,
    // continue with next constraint. 
    if (found_many_nonzero_coeffs)
      continue;

    if (!found_a_nonzero_coeff) {
      // All coefficients are 0.
      // The constraint is either trivially true or trivially false.
      if (cs_i.is_ray_or_point_or_inequality()) {
	if (cs_i[0] < 0)
	  // A constraint such as -1 >= 0 is trivially false.
	  return UNFEASIBLE_PROBLEM;
      }
      else
	// The constraint is an equality.
	if (cs_i[0] != 0)
	  // A constraint such as 1 == 0 is trivially false.
	  return UNFEASIBLE_PROBLEM;
      // Here the constraint is trivially true.
      trivially_true_constraint[i] = true;
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
	  ++num_inequalities_left;
      }
      // Cases 4-5: apply method B. 
      else if (cs_i.is_line_or_equality()) {
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  ++num_nonnegative;
	}
      }
      // Case 6: apply method B.
      else if (sgn_b < 0) {
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  ++num_nonnegative;
	}
	++num_inequalities_left;
      }
      // Case 7: apply method C.
      else if (sgn_a > 0) {
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  ++num_nonnegative;
	}
	nonnegativity_constraint[i] = true;
	++nonnegativity_constraint_number;
      }
      // Cases 8-9: apply method A.
      else
	++num_inequalities_left;
    }
  }

  // Now we can fill the map.
  for (dimension_type i = 0, j = nonnegative_variable.size(),
	 nnv_size = j; i < nnv_size; ++i)
    if (!nonnegative_variable[i]) {
      dim_map.insert(std::make_pair(i, j));
      ++j;
    }
  // Phase 2: 
  // set the dimension for the tableau and insert the (possibly transformed)
  // cost function as the first row.

  // This will be the new size of the rows that will be inserted in the 
  // tableau. The size is computed in this way:
  const dimension_type new_row_size
    = 2*cs_num_columns - num_nonnegative + num_inequalities_left - 1; 
   
  // We have to resize the cost function to `new_row_size'.
  Row resized_cost_function(cost_function, new_row_size, new_row_size);
  cost_function.swap(resized_cost_function);
  
  // Phase 3:
  // insert all the (possibly transformed) constraints that are not
  // nonnegativity constraints. The transformation includes both
  // the variable splitting (for variables that are unconstrained
  // in sign) and the addition of slack variables (for inequalities
  // in the original problem).
  
  // This is the index in the matrix where we will begin to work
  // with the slack variables.
  dimension_type slack_pos_index = new_row_size - num_inequalities_left; 
  
  // We must set the new size of the rows of tableau (at the moment is a 0x0).
  tableau.add_zero_columns(new_row_size);
  
  // Insertion of the constraints the tableau.
  for (dimension_type i = 0, iend = cs.num_rows(); i < iend; ++i) {
    // We are going to insert only constraints that are not trivially true
    // (5 > 3) and that are not nonnegativity_constraints (X > 0).
    if (nonnegativity_constraint[i] || trivially_true_constraint[i])
      continue;
    insert_row_in_matrix(tableau, cs[i]);
    // Here we also add a slack variable to the constraint, if we need it.
    if (cs[i].is_ray_or_point_or_inequality()) {
      tableau[tableau.num_rows() - 1][slack_pos_index] = -1;
      ++slack_pos_index;
    } 
  }
  // Last step: we proceed splitting variables in the tableau.
  const dimension_type tableau_num_rows = tableau.num_rows();
  typedef std::map<dimension_type, dimension_type>::const_iterator iter;
  for (iter map_itr = dim_map.begin(),
	 map_end = dim_map.end(); map_itr != map_end; ++map_itr) {
    for (dimension_type i = tableau_num_rows; i-- > 0; ) 
      tableau[i][(map_itr->second) +1] = -tableau[i][(map_itr->first) + 1];
    cost_function[(map_itr->second) +1] = -cost_function[(map_itr->first) + 1];
  }
  // If the computed tableau is empty, then the whole nonnegative
  // orthant is feasible, so that the problem is either unbounded or
  // has a maximum in the origin. We return anyway UNBOUNDED_PROBLEM
  // (the caller has to figure out which situation applies).
  if (tableau.num_rows() == 0) 
    return UNBOUNDED_PROBLEM;
  assert(tableau.num_columns() != 0);
  return SOLVED_PROBLEM;
}

//! \brief
//  FIXME: This comment sucks!
//  Checks if a variable is in base and assigns to 'row' the row index of which
//  is base.
/*!
  \return              <CODE>true</CODE> if and if only a is in base
  \param base          The base of the LP problem.
  \param var_to_check  The variable that has to be checked if is in base.
  \param row           Here will be written the index of the
                       row of which var_to_check is base.
*/ 
bool
is_in_base(const std::vector<dimension_type>& base,
	   const dimension_type var_to_check,
	   dimension_type& row ) {
  for (dimension_type i = base.size(); i-- > 0; )
    if (base[i] == var_to_check) {
      row = i;
      return true;
    }
  return false;
}

//! \brief
//! Computes the generator that will be given on exit of primal_simplex,
//! the problem has an optimality point.
/*!
  \return
  The computed generator.

  \param tableau
  A matrix containing the constraints of the solved problem.

  \param base
  The base of the LP problem.

  \param map
  Contains all the pairs (i, j) such that Variable(i) (that was not found
  to be constrained in sign) has been split into two nonnegative variables.
  The "positive" one is represented again by Variable(i), and the "negative"
  one is represented by Variable(j).

  \param original_space_dim
  The space dimension of the original LP problem.
*/
Generator
compute_generator(const Matrix& tableau,
		  const std::vector<dimension_type>& base,
		  const std::map<dimension_type, dimension_type>& dim_map,
		  const dimension_type original_space_dim) {
  // We will store in num[] and in den[] the numerators and
  // the denominators of every variable of the original problem.
  std::vector<Coefficient> num(original_space_dim);
  std::vector<Coefficient> den(original_space_dim);
  dimension_type row = 0;

  // We start to compute num[] and den[].
  typedef std::map<dimension_type, dimension_type>::const_iterator iter;
  iter map_end = dim_map.end();

  for (dimension_type i = original_space_dim; i-- > 0; ) {
    // Check whether the variable was split.
    iter map_iter = dim_map.find(i);
    if (map_iter == map_end)
      // The variable was not split: get its value from the tableau
      // (if it is not in the base, the value is 0).
      if (is_in_base(base, i+1, row)) {
	num[i]= -tableau[row][0];
	den[i]= tableau[row][base[row]];
      }
      else {
	num[i] = 0;
	den[i] = 1;
      }
    else {
      // The variable was split: its value is the difference
      // between the positive and the negative components.
      // (The negative component has index map[i] + 1.)
      Coefficient split_num[2];
      Coefficient split_den[2];
    
      for (dimension_type j = 0; j < 2; ++j) {
	// Like before, we he have to check if the variable is in base.
	if (is_in_base(base, j == 0 ? i+1 : map_iter->second+1, row)) {
	  split_num[j] = - tableau[row][0];
	  split_den[j] =  tableau[row][base[row]];
	}
	else {
	  split_num[j] = 0;
	  split_den[j] = 1;
	}
      }
      // We compute the lcm to compute subsequently the difference
      // between the 2 variables.
      Coefficient lcm;
      lcm_assign(lcm, split_den[0], split_den[1]);
      split_num[0] *= lcm/split_den[0];
      split_num[1] *= lcm/split_den[1];
      num[i] = split_num[0] - split_num[1];
      den[i] = lcm;
      
      // If the numerator is 0, is better to keep the denominator to 1,
      // this to increase the speed of the computation of the lcm.
      if (num[i] == 0)
	den[i] = 1;
    }
  }

  // Before computing the generator, we need to have all the denominators > 0.
  // In this way we get the (a/b) rational representation, with b > 0 and 
  // a free in sign.
  for (dimension_type i = original_space_dim; i-- > 0; )
    if (den[i] < 0 ) {
      negate(num[i]);
      negate(den[i]);
    }
  
  // To compute the generator we have to get the common denominator
  // of all the values stored in denominators[].
  Coefficient lcm = 1;
  for (dimension_type i = original_space_dim; i-- > 0; ) 
    lcm_assign(lcm, den[i]);
  
  // So we multiply the numerators.
  for (dimension_type i = original_space_dim; i-- > 0; )
   num[i] *= lcm/den[i];
  
  // At last we can build our generator.
  Linear_Expression my_generator;
  for (dimension_type i = original_space_dim; i-- > 0; )
    my_generator += Variable(i)*num[i];

  // We proceed returning g.
  Generator g = point(my_generator, lcm);
  return g;
}

Simplex_Status
primal_simplex(const Linear_System& cs,
	       Row& cost_function,
	       Generator& maximizing_point) {
  assert(cost_function.size() <= cs.num_columns());

  Matrix tableau(0,0);
  std::map<dimension_type, dimension_type> dim_map;
  Simplex_Status status
    = compute_tableau(cs, cost_function, tableau, dim_map);

  switch (status) {
  case UNFEASIBLE_PROBLEM:
    break;
  case UNBOUNDED_PROBLEM:
    // There are no constraints in the tableau, apart from those
    // stating nonnegativity of the variables. Therefore, the problem
    // will be unbounded as soon as the cost function has a variable
    // with a positive coefficient.
    for (dimension_type i = cost_function.size(); i-- > 1; )
      if (cost_function[i] > 0)
	return UNBOUNDED_PROBLEM;
    // Otherwise a maximizing solution is the origin. 
    maximizing_point = point();
    status = SOLVED_PROBLEM;
    break;
  case SOLVED_PROBLEM:
    {
      // Find a feasible solution.
      std::vector<dimension_type> base(tableau.num_rows());
      status = first_phase(tableau, cost_function, base);
      if (status == SOLVED_PROBLEM) {
	// If the constraint system is NNC, the epsilon dimension
	// has to be interpreted as a normal dimension.
	const dimension_type space_dim = cs.num_columns() - 1;
	maximizing_point = compute_generator(tableau, base,
					     dim_map, space_dim);
      }
    }
    break;
  }
  return status;
}

} // namespace


namespace PPL = Parma_Polyhedra_Library;

Simplex_Status
PPL::Constraint_System::primal_simplex(Linear_Expression& cost_function,
				       Generator& maximizing_point) const {
  return ::primal_simplex(*this, cost_function, maximizing_point);
}


Simplex_Status
PPL::Constraint_System::primal_simplex(const Linear_Expression& expr,
				       const bool maximize,
				       Coefficient& ext_n,
				       Coefficient& ext_d,
				       Generator& optimizing_point) const {
  // Strict inequality constraints are not supported.
  if (topology() == NOT_NECESSARILY_CLOSED)
    throw std::invalid_argument("PPL::Constraint_System::primal_simplex(): "
				"strict inequality constraints "
				"are not supported.");

  // Make sure the dimension of `expr' is not greater than
  // the dimension of the constraint system.
  if (space_dimension() < expr.space_dimension()) {
    std::ostringstream s;
    s << "PPL::Constraint_System::primal_simplex():" << std::endl
      << "this->space_dimension() == " << space_dimension()
      << ", " << "cost_function->space_dimension() == "
      << expr.space_dimension() << ".";
    throw std::invalid_argument(s.str());
  }

  // Work with a copy of `expr', since it may be modified.
  Linear_Expression cost_function = expr;

  // Minimization is obtained by negating the cost_function.
  if (!maximize) 
    for (dimension_type i = cost_function.size(); i-- > 0; ) 
      negate(cost_function[i]);

  Simplex_Status status
    = ::primal_simplex(*this, cost_function, optimizing_point);

  if (status == SOLVED_PROBLEM) {
    // Compute the optimal value of the cost function.
    ext_n = expr.inhomogeneous_term();
    for (dimension_type i = optimizing_point.space_dimension(); i-- > 0; )  
      ext_n += optimizing_point.coefficient(Variable(i))
	       * expr.coefficient(Variable(i));
    // Numerator and denominator should be coprime.
    normalize2(ext_n, optimizing_point.divisor(), ext_n, ext_d);
    
    // Check the computed generator for feasibility.
    assert(satisfies_all_constraints(optimizing_point));  
  }
  return status;
}
