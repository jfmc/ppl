/* PIP_Tree related class implementation: non-inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#include <ppl-config.h>
#include "PIP_Tree.defs.hh"

#include <algorithm>

namespace Parma_Polyhedra_Library {

namespace {

// Calculate positive modulo of x % y
void
mod_assign(Coefficient &z, Coefficient_traits::const_reference x,
           Coefficient_traits::const_reference y) {
  z = x%y;
  if (z < 0)
    z += y;
}

// Compute x += c * y
void
add_assign(Row& x, const Row& y, Coefficient_traits::const_reference c) {
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; )
    add_mul_assign(x[i], c, y[i]);
}

// Merge constraint system to a Matrix-form context such as x = x U y
void
merge_assign(Matrix& x,
             const Constraint_System& y,
             const Variables_Set &parameters) {
  dimension_type width = x.num_columns();
  PPL_ASSERT(parameters.size() == width-1);
  Row row(width, Row::Flags());
  Variables_Set::iterator param_begin = parameters.begin();
  Variables_Set::iterator param_end = parameters.end();
  Variables_Set::iterator pi;
  dimension_type j;
  for (Constraint_System::const_iterator y_i = y.begin(),
         y_end = y.end(); y_i != y_end; ++y_i) {
    PPL_ASSERT(y_i->is_nonstrict_inequality());
    row[0] = y_i->inhomogeneous_term();
    for (pi=param_begin, j=1; pi != param_end; ++pi, ++j)
      row[j] = y_i->coefficient(Variable(*pi));
    x.add_row(row);
  }
}

// Tranform expression "expr" into "-expr-1", using scaling
void
negate_assign(Row& x, const Row& y, const Coefficient& sc) {
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; )
    x[i] = -y[i];
  x[0] -= sc;
}

// Update given context matrix using local artificials
dimension_type
update_context(Matrix &context,
               const PIP_Tree_Node::Artificial_Parameter_Sequence &ap) {
  dimension_type ap_size = ap.size();
  if (ap_size > 0)
    context.add_zero_columns(ap_size);
  return ap_size;
}

// Update given context matrix and parameter set using local artificials
void
update_context(Variables_Set &params, Matrix &context,
                   const PIP_Tree_Node::Artificial_Parameter_Sequence &ap,
                   dimension_type &space_dimension) {
  dimension_type ap_size = update_context(context, ap);
  if (ap_size > 0) {
    for (dimension_type i = 0; i < ap_size; ++i)
      params.insert(space_dimension++);
  }
}

} // namespace

namespace IO_Operators {

std::ostream&
operator<<(std::ostream& os, const PIP_Tree_Node::Artificial_Parameter& x) {
  os << "(" << ((Linear_Expression)x) << ") div " << x.get_denominator();
  return os;
}

} // namespace IO_Operators

PIP_Tree_Node::PIP_Tree_Node()
  : constraints_(),
    artificial_parameters() {
}

PIP_Tree_Node::PIP_Tree_Node(const PIP_Tree_Node &x)
  : constraints_(x.constraints_),
    artificial_parameters(x.artificial_parameters) {
}

PIP_Tree_Node::Artificial_Parameter::Artificial_Parameter()
  : Linear_Expression(), denominator(1) {
}

PIP_Tree_Node::Artificial_Parameter
::Artificial_Parameter(const Linear_Expression &e, const Coefficient &d)
  : Linear_Expression(e), denominator(d) {
}

PIP_Tree_Node::Artificial_Parameter
::Artificial_Parameter(const Artificial_Parameter &x)
  : Linear_Expression(x), denominator(x.denominator) {
}

const Coefficient&
PIP_Tree_Node::Artificial_Parameter
::get_denominator() const {
  return denominator;
}

void
PIP_Tree_Node::Artificial_Parameter::ascii_dump(std::ostream& s) const {
  s << "\ndenominator " << denominator << "\n";
  Linear_Expression::ascii_dump(s);
}

bool
PIP_Tree_Node::Artificial_Parameter::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "denominator")
    return false;
  if (!(s >> denominator))
    return false;
  if (!Linear_Expression::ascii_load(s))
    return false;
  return true;
}

PIP_Decision_Node::~PIP_Decision_Node() {
  delete true_child;
  if (false_child)
    delete false_child;
}

PIP_Solution_Node::~PIP_Solution_Node() {
}

PIP_Solution_Node::PIP_Solution_Node()
  : PIP_Tree_Node(),
    tableau(),
    basis(),
    mapping(),
    sign(),
    solution(),
    solution_valid(false) {
}

PIP_Solution_Node::PIP_Solution_Node(const PIP_Solution_Node &x)
  : PIP_Tree_Node(x),
    tableau(x.tableau),
    basis(x.basis),
    mapping(x.mapping),
    sign(x.sign),
    solution(x.solution),
    solution_valid(x.solution_valid) {
}

PIP_Solution_Node::PIP_Solution_Node(const PIP_Solution_Node &x,
                                     bool empty_constraints)
  : PIP_Tree_Node(),
    tableau(x.tableau),
    basis(x.basis),
    mapping(x.mapping),
    sign(x.sign),
    solution(x.solution),
    solution_valid(x.solution_valid) {
  if (!empty_constraints) {
    constraints_ = x.constraints_;
    artificial_parameters = x.artificial_parameters;
  }
}

PIP_Decision_Node::PIP_Decision_Node(PIP_Tree_Node* fcp,
                                     PIP_Tree_Node* tcp)
  : PIP_Tree_Node(),
    true_child(tcp),
    false_child(fcp) {
}

PIP_Decision_Node ::PIP_Decision_Node(const PIP_Decision_Node& x)
  : PIP_Tree_Node(x),
    true_child(0),
    false_child(0) {
  if (x.true_child != 0)
    true_child = x.true_child->clone();
  if (x.false_child != 0)
    false_child = x.false_child->clone();
}

const PIP_Solution_Node*
PIP_Tree_Node::as_solution() const {
  return 0;
}

PIP_Solution_Node*
PIP_Tree_Node::as_solution() {
  return 0;
}

const PIP_Decision_Node*
PIP_Tree_Node::as_decision() const {
  return 0;
}

PIP_Decision_Node*
PIP_Tree_Node::as_decision() {
  return 0;
}

const PIP_Solution_Node*
PIP_Solution_Node::as_solution() const {
  return this;
}

PIP_Solution_Node*
PIP_Solution_Node::as_solution() {
  return this;
}

const PIP_Decision_Node*
PIP_Decision_Node::as_decision() const {
  return this;
}

PIP_Decision_Node*
PIP_Decision_Node::as_decision() {
  return this;
}

dimension_type
PIP_Tree_Node::insert_artificials(Variables_Set &params,
                                  dimension_type space_dimension) const {
  dimension_type ap_size = artificial_parameters.size();
  if (ap_size > 0) {
    for (dimension_type i = 0; i < ap_size; ++i)
      params.insert(space_dimension++);
  }
  return ap_size;
}

bool
PIP_Solution_Node::Tableau::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  const dimension_type num_rows = s.num_rows();
  if (num_rows != t.num_rows()) {
#ifndef NDEBUG
    cerr << "The PIP_Solution_Node::Tableau matrices do not have the "
         << "same number of rows."
         << endl;
#endif
    return false;
  }
  return true;
}

bool
PIP_Tree_Node::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif
  Constraint_System::const_iterator begin = constraints_.begin();
  Constraint_System::const_iterator end = constraints_.end();

  // Parameter constraint system should contain no strict inequalities.
  for (Constraint_System::const_iterator c = begin; c != end; c++)
    if (c->is_strict_inequality()) {
#ifndef NDEBUG
      cerr << "The feasible region of the PIP_Problem parameter context"
           << "is defined by a constraint system containing strict "
           << "inequalities."
	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
  return true;
}

void
PIP_Tree_Node
::add_constraint(const Row &row, const Variables_Set& parameters) {
  Linear_Expression e;
  Variables_Set::const_iterator param_begin = parameters.begin();
  Variables_Set::const_iterator param_end = parameters.end();
  Variables_Set::const_iterator pi;
  dimension_type j;
  PPL_ASSERT(dimension_type(std::distance(param_begin, param_end))+1
             == row.size());
  for (pi=param_begin, j=1; pi != param_end; ++pi, ++j)
    e += row[j] * Variable(*pi);
  constraints_.insert(e + row[0] >= 0);
}

bool
PIP_Solution_Node::OK() const {
  /* FIXME: finish me! */
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif
  if (!PIP_Tree_Node::OK())
    return false;

  // Check that every member used is OK.

  if (!tableau.OK())
    return false;

  return true;
}

bool
PIP_Decision_Node::OK() const {
  /* FIXME: finish me! */
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif
  if (!PIP_Tree_Node::OK())
    return false;

  // Decision nodes with false child must have exactly one constraint
  if (false_child) {
    dimension_type
        dist = std::distance(constraints_.begin(), constraints_.end());
    if (dist != 1) {
#ifndef NDEBUG
      cerr << "The PIP_Decision_Node has a 'false' child but does not "
           << "have exactly one parametric constraint. (" << dist << ")"
           << endl;
#endif
      return false;
    }
  }

  return true;
}

void
PIP_Decision_Node::update_tableau(dimension_type external_space_dim,
                                  dimension_type first_pending_constraint,
                                  const Constraint_Sequence &input_cs,
                                  const Variables_Set &parameters) {
  true_child->update_tableau(external_space_dim,
                             first_pending_constraint,
                             input_cs,
                             parameters);
  if (false_child)
    false_child->update_tableau(external_space_dim,
                                first_pending_constraint,
                                input_cs,
                                parameters);
}

PIP_Problem_Status
PIP_Decision_Node::solve(PIP_Tree_Node*& parent_ref, const Matrix& context,
                         const Variables_Set &params,
                         dimension_type space_dimension) {
  PIP_Problem_Status return_status;
  PIP_Problem_Status stt;
  PIP_Problem_Status stf = UNFEASIBLE_PIP_PROBLEM;
  Matrix context_true(context);
  Variables_Set parameters(params);
  update_context(parameters, context_true, artificial_parameters,
                 space_dimension);
  merge_assign(context_true, constraints_, parameters);
  stt = true_child->solve(true_child, context_true, parameters,
                          space_dimension);
  if (false_child) {
    // Decision nodes with false child must have exactly one constraint
    PPL_ASSERT(1 == std::distance(constraints_.begin(), constraints_.end()));
    Matrix context_false(context);
    update_context(context_false, artificial_parameters);
    merge_assign(context_false, constraints_, parameters);
    Row &last = context_false[context_false.num_rows()-1];
    negate_assign(last, last, 1);
    stf = false_child->solve(false_child, context_false, parameters,
                             space_dimension);
  }

  if (stt == UNFEASIBLE_PIP_PROBLEM && stf == UNFEASIBLE_PIP_PROBLEM) {
    return_status = UNFEASIBLE_PIP_PROBLEM;
    parent_ref = 0;
    delete this;
  } else
    return_status = OPTIMIZED_PIP_PROBLEM;
  return return_status;
}

void
PIP_Solution_Node::Tableau::normalize() {
  if (denominator == 1)
    return;
  dimension_type i_max = s.num_rows();
  dimension_type j_max = s.num_columns();
  dimension_type k_max = t.num_columns();
  dimension_type i, j, k;
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  gcd = denominator;

  for (i=0; i<i_max; ++i) {
    const Row &row_s = s[i];
    for (j=0; j<j_max; ++j) {
      const Coefficient &x = row_s[j];
      if (x != 0) {
        gcd_assign(gcd, x, gcd);
        if (gcd == 1)
          return;
      }
    }
    const Row &row_t = t[i];
    for (k=0; k<k_max; ++k) {
      const Coefficient &x = row_t[k];
      if (x != 0) {
        gcd_assign(gcd, x, gcd);
        if (gcd == 1)
          return;
      }
    }
  }

  // Divide the coefficients by the GCD.
  for (i=0; i<i_max; ++i) {
    Row &row_s = s[i];
    for (j=0; j<j_max; ++j) {
      Coefficient &x = row_s[j];
      exact_div_assign(x, x, gcd);
    }
    Row &row_t = t[i];
    for (k=0; k<k_max; ++k) {
      Coefficient &x = row_t[k];
      exact_div_assign(x, x, gcd);
    }
  }
  exact_div_assign(denominator, denominator, gcd);
}

void
PIP_Solution_Node::Tableau::scale(const Coefficient &ratio) {
  dimension_type i, j, k;
  dimension_type i_max = s.num_rows();
  dimension_type j_max = s.num_columns();
  dimension_type k_max = t.num_columns();
  for (i=0; i<i_max; ++i) {
    for (j=0; j<j_max; ++j)
      s[i][j] *= ratio;
    for (k=0; k<k_max; ++k)
      t[i][k] *= ratio;
  }
  denominator *= ratio;
}

void
PIP_Tree_Node::ascii_dump(std::ostream& s) const {
  s << "\nconstraints_\n";
  constraints_.ascii_dump(s);
  dimension_type artificial_parameters_size = artificial_parameters.size();
  s << "\nartificial_parameters( " << artificial_parameters_size << " )\n";
  for (dimension_type i=0; i<artificial_parameters_size; ++i)
    artificial_parameters[i].ascii_dump(s);
}

bool
PIP_Tree_Node::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "constraints_")
    return false;
  constraints_.ascii_load(s);

  if (!(s >> str) || str != "artificial_parameters(")
    return false;
  dimension_type artificial_parameters_size;
  if (!(s >> artificial_parameters_size))
    return false;
  Artificial_Parameter ap;
  for (dimension_type i=0; i<artificial_parameters_size; ++i) {
    if (!ap.ascii_load(s))
      return false;
    artificial_parameters.push_back(ap);
  }

  PPL_ASSERT(OK());
  return true;
}

PIP_Tree_Node*
PIP_Tree_Node::clone() const {
  const PIP_Solution_Node* as_s = as_solution();
  if (as_s != 0)
    return as_s->clone();
  const PIP_Decision_Node* as_d = as_decision();
  if (as_d != 0)
    return as_d->clone();
  return 0;
}

PIP_Solution_Node*
PIP_Solution_Node::clone() const {
  return new PIP_Solution_Node(*this);
}

PIP_Decision_Node*
PIP_Decision_Node::clone() const {
  return new PIP_Decision_Node(*this);
}

void
PIP_Solution_Node::Tableau::ascii_dump(std::ostream& st) const {
  st << "denominator " << denominator << "\n";
  st << "variables ";
  s.ascii_dump(st);
  st << "parameters ";
  t.ascii_dump(st);
}

bool
PIP_Solution_Node::Tableau::ascii_load(std::istream& st) {
  std::string str;
  if (!(st >> str) || str != "denominator")
    return false;
  Coefficient den;
  if (!(st >> den))
    return false;
  denominator = den;
  if (!(st >> str) || str != "variables")
    return false;
  if (!s.ascii_load(st))
    return false;
  if (!(st >> str) || str != "parameters")
    return false;
  if (!t.ascii_load(st))
    return false;
  return true;
}

void
PIP_Solution_Node::ascii_dump(std::ostream& s) const {
  PIP_Tree_Node::ascii_dump(s);

  s << "\nvariable_tableau\n";
  tableau.s.ascii_dump(s);

  s << "\nparameter_tableau\n";
  tableau.t.ascii_dump(s);
}

bool
PIP_Solution_Node::ascii_load(std::istream& s) {
  if (!PIP_Tree_Node::ascii_load(s))
    return false;

  std::string str;
  if (!(s >> str) || str != "variable_tableau")
    return false;
  if (!tableau.s.ascii_load(s))
    return false;

  if (!(s >> str) || str != "parameter_tableau")
    return false;
  if (!tableau.t.ascii_load(s))
    return false;

  solution_valid = false;
  PPL_ASSERT(OK());
  return true;
}

const Linear_Expression&
PIP_Solution_Node
::parametric_values(const Variable &v,
                    const Variables_Set& parameters) const {
  const_cast<PIP_Solution_Node&>(*this).update_solution(parameters);
  dimension_type id = v.id();
  dimension_type j;
  Variables_Set::iterator location = parameters.lower_bound(id);
  if (location == parameters.end())
    j = id;
  else {
    if (*location == id) {
#ifndef NDEBUG
      std::cerr << "PIP_Solution_Node::parametric_values(Variable): "
                   "Supplied Variable corresponds to a parameter"
                << std::endl;
#endif
      j = not_a_dimension();
    } else
      j = id - std::distance(parameters.begin(),location);
  }

  return solution[j];
}

PIP_Solution_Node::Row_Sign
PIP_Solution_Node::row_sign(const Row &x) {
  PIP_Solution_Node::Row_Sign sign = ZERO;
  for (int i = x.size(); i-- > 0; ) {
    const Coefficient &c = x[i];
    switch (sign) {
      case UNKNOWN:
        // cannot happen
        break;
      case ZERO:
        if (c < 0)
          sign = NEGATIVE;
        else if (c > 0)
          sign = POSITIVE;
        break;
      case NEGATIVE:
        if (c > 0)
          return MIXED;
        break;
      case POSITIVE:
        if (c < 0)
          return MIXED;
        break;
      case MIXED:
        // cannot happen
        break;
    }
  }
  return sign;
}

bool
PIP_Solution_Node::compatibility_check(const Matrix &ctx, const Row &cnst) {
  Matrix s(ctx);
  s.add_row(cnst);
  dimension_type i, j, k, j_, j__;
  dimension_type num_rows = s.num_rows();
  dimension_type num_cols = s.num_columns();
  bool result = false;

  /* Perform simplex pivots on the context until we find an empty solution
   * or an optimum */
  for (;;) {
    // Look for a negative RHS (=constant term, stored in matrix column 0)
    i = 0;
    while (i<num_rows && s[i][0] >= 0)
      i++;
    if (i == num_rows) {
      // No negative RHS: optimum found
      result = true;
      break;
    }
    // Find a positive m[i][j] pivot
    j = 1;
    Row &p = s[i];
    while (j<num_cols && p[j] <= 0)
      j++;
    if (j == num_cols) {
      // No positive pivot candidate: empty problem
      result = false;
      break;
    }
    // Perform a pivot operation on the matrix
    const Coefficient sij = p[j];
    for (j_=0; j_<num_cols; ++j_) {
      if (j_ == j)
        continue;
      const Coefficient sij_ = p[j_];
      if (sij_ == 0)
        // if element j of pivot row is zero, nothing to do for this column
        continue;
      for (k=0; k<num_rows; ++k) {
        if (k == i)
          continue;
        Row& row = s[k];
        Coefficient mult = row[j] * sij_;
        if (mult % sij != 0) {
          // Must scale row to stay in integer case
          Coefficient gcd;
          gcd_assign(gcd, mult, sij);
          Coefficient scale_factor = sij/gcd;
          for (j__=0; j__<num_cols; ++j__)
            row[j__] *= scale_factor;
          mult *= scale_factor;
        }
        row[j_] -= mult / sij;
      }
      s[i][j_] = 0;
    }
    if (sij != 1) {
      // Update column only if pivot != 1
      for (k=0; k<num_rows; ++k) {
        Row& row = s[k];
        Coefficient& skj = row[j];
        if (skj % sij != 0) {
          // as above, we must perform row scaling
          Coefficient gcd;
          gcd_assign(gcd, skj, sij);
          Coefficient scale_factor = sij/gcd;
          for (j__=0; j__<num_cols; ++j__)
            row[j__] *= scale_factor;
          skj *= scale_factor;
        }
        skj /= sij;
      }
    }
  }

  return result;
}

void
PIP_Solution_Node::update_tableau(dimension_type external_space_dim,
                                  dimension_type first_pending_constraint,
                                  const Constraint_Sequence &input_cs,
                                  const Variables_Set &parameters) {
  dimension_type i;
  dimension_type n_params = parameters.size();
  dimension_type n_vars = external_space_dim - n_params;
  dimension_type internal_space_dim = tableau.t.num_columns()-1;
  Constraint_Sequence::const_iterator cst;

  // Create the parameter column, corresponding to the constant term
  if (tableau.t.num_columns() == 0) {
    tableau.t.add_zero_columns(1);
    internal_space_dim = 0;
  }

  // add new columns to the tableau
  for (i=internal_space_dim; i<external_space_dim; ++i) {
    if (parameters.count(i) == 1)
      tableau.t.add_zero_columns(1);
    else {
      tableau.s.add_zero_columns(1);
      basis.push_back(true);
      mapping.push_back(tableau.s.num_columns()-1);
    }
  }
  internal_space_dim = external_space_dim;

  const Coefficient &denom = tableau.get_denominator();

  for (cst = input_cs.begin() + first_pending_constraint;
       cst < input_cs.end(); ++cst) {
    int v = 0;
    int p = 1;
    Row var(n_vars, tableau.s_capacity(), Row::Flags());
    Row param(n_params+1, tableau.t_capacity(), Row::Flags());
    Coefficient cnst_term = cst->inhomogeneous_term();
    if (cst->is_strict_inequality())
      // convert c > 0  <=>  c-1 >= 0
      cnst_term -= 1;
    param[0] = cnst_term * denom;
    for (i=0; i<internal_space_dim; i++) {
      if (parameters.count(i) == 1) {
        param[p++] = cst->coefficient(Variable(i)) * denom;
      } else {
        const Coefficient &c = cst->coefficient(Variable(i));
        dimension_type idx = mapping[v];
        if (basis[v])
          // Basic variable : add c * x_i
          var[idx] += c * denom;
        else {
          // Nonbasic variable : add c * row_i
          add_assign(var, tableau.s[idx], c);
          add_assign(param, tableau.t[idx], c);
        }
        ++v;
      }
    }
    // FIXME: must handle equality constraints
    if (row_sign(var) != ZERO) {
      /* parametric-only constraints have already been inserted in initial
        context, so no need to insert them in the tableau
      */
      tableau.s.add_row(var);
      tableau.t.add_row(param);
      sign.push_back(row_sign(param));
    }
  }
}

void
PIP_Solution_Node::update_solution(const Variables_Set& parameters) {
  if (solution_valid)
    return;
  dimension_type num_vars = tableau.s.num_columns();
  const Coefficient& d = tableau.get_denominator();
  if (solution.size() != num_vars)
    solution.resize(num_vars);
  for (dimension_type i = num_vars; i-- > 0; ) {
    Linear_Expression &sol = solution[i];
    if (basis[i]) {
      sol = Linear_Expression(0);
    } else {
      Row &row = tableau.t[mapping[i]];
      sol = Linear_Expression(row[0]/d);
      dimension_type k;
      Variables_Set::const_iterator j;
      Variables_Set::const_iterator j_end = parameters.end();
      for (j = parameters.begin(), k = 1; j != j_end; ++j, ++k)
        sol += (row[k]/d) * Variable(*j);
    }
  }
  solution_valid = true;
}

PIP_Problem_Status
PIP_Solution_Node::solve(PIP_Tree_Node*& parent_ref, const Matrix& ctx,
                         const Variables_Set &params,
                         dimension_type space_dimension) {
  Matrix context(ctx);
  Variables_Set parameters(params);
  update_context(parameters, context, artificial_parameters,
                 space_dimension);
  merge_assign(context, constraints_, parameters);
  const dimension_type n_a_d = not_a_dimension();
  Coefficient gcd;

  // Main loop of the simplex algorithm
  for(;;) {
    dimension_type i, j;
    dimension_type i_ = n_a_d;
    dimension_type i__ = n_a_d;
    dimension_type num_rows = tableau.t.num_rows();
    dimension_type num_vars = tableau.s.num_columns();
    dimension_type num_params = tableau.t.num_columns();
    Row_Sign s;

#ifdef NOISY_PIP
    tableau.ascii_dump(std::cout);
    std::cout << "context ";
    context.ascii_dump(std::cout);
#endif

    for (i=0; i<num_rows; ++i) {
      Row_Sign s = sign[i];
      if (s == UNKNOWN) {
        s = row_sign(tableau.t[i]);
        sign[i] = s;
      }
      /* Locate first row with negative parameter row */
      if (s == NEGATIVE && i_ == n_a_d)
        i_ = i;
      /* Locate first row with unknown-signed parameter row */
      if (s == MIXED && i__ == n_a_d)
        i__ = i;
    }

    /* If no negative parameter row found, try to refine the sign of
       undetermined rows using compatibility checks with the current context
    */
    if (i_ == n_a_d && i__ != n_a_d) {
      for (i=i__; i<num_rows; ++i) {
        if (sign[i] != MIXED)
          continue;
        s = ZERO;
        if (compatibility_check(context, tableau.t[i]))
          // constraint t_i(z) >= 0 is compatible with the context
          s = POSITIVE;
        Row c(num_params, Row::Flags());
        negate_assign(c, tableau.t[i], tableau.get_denominator());
        if (compatibility_check(context, c)) {
          // constraint t_i(z) < 0 <=> -t_i(z)-1 >= 0 is compatible
          if (s == POSITIVE)
            s = MIXED;
          else
            s = NEGATIVE;
        }
        if (s == NEGATIVE && i_ == n_a_d)
          // first negative row found
          i_ = i;
        if (s != MIXED) {
          // clear first mixed-sign row index if row is found to be not mixed
          if (i == i__)
            i__ = n_a_d;
        } else if (i__ == n_a_d)
          // first mixed-sign row found
          i__ = i;
        sign[i] = s;
      }
    }

    /* If there remains a row i with undetermined sign and at least one
       positive S_ij coefficient, where constraint t_i(z) > 0 is not
       compatible with the context, the row parameter can be considered
       negative
    */
    if (i_ == n_a_d && i__ != n_a_d) {
      for (i=i__; i<num_rows; ++i) {
        if (sign[i] != MIXED)
          continue;
        bool found = false;
        const Row &p = tableau.s[i];
        for (j=0; j<num_vars; ++j)
          if (p[j] > 0) {
            found = true;
            break;
          }
        if (!found)
          continue;
        Row row(tableau.t[i]);
        row[0] -= tableau.get_denominator();
        if (compatibility_check(context, row)) {
          if (i__ == n_a_d)
            i__ = i;
        } else {
          sign[i] = NEGATIVE;
          if (i_ == n_a_d)
            i_ = i;
          if (i__ == i)
            i__ = n_a_d;
        }
      }
    }

    /* If we have found a row i_ with negative parameters :
       Either the problem is empty, or a pivoting step is required
    */
    if (i_ != n_a_d) {
#ifdef NOISY_PIP
      std::cout << "Found row with negative parameters: " << i_
                << std::endl;
#endif
      dimension_type k;
      Coefficient z(0);
      Coefficient sij, cij, cij_;
      Coefficient c;
      Row &row = tableau.s[i_];
      /* Look for a positive S_ij such as the j^th column/S_ij is
         lexico-minimal
      */
      dimension_type j_ = n_a_d;
      for (j=0; j<num_vars; ++j) {
        if (row[j] > 0) {
          if (j_ == n_a_d) {
            j_ = j;
            sij = row[j];
          } else {
            /* Determine which column (j or j_) is lexico-minimal */
            dimension_type k = 0;
            do {
              dimension_type mk = mapping[k];
              if (basis[k]) {
                /* reconstitute the identity submatrix part of S */
                cij = (mk==j) ? tableau.get_denominator() : 0;
                cij_ = (mk==j_) ? tableau.get_denominator() : 0;
              } else {
                cij = tableau.s[mk][j];
                cij_ = tableau.s[mk][j_];
              }
              ++k;
            } while (k < num_vars && cij * sij == cij_ * row[j]);
            if (k < num_vars && cij * sij < cij_ * row[j]) {
              j_ = j;
              sij = row[j];
            }
          }
        }
      }

      /* If no positive S_ij: problem is empty */
      if (j_ == n_a_d) {
#ifdef NOISY_PIP
        std::cout << "No positive pivot found: Solution = _|_"
                  << std::endl;
#endif
        parent_ref = 0;
        delete this;
        return UNFEASIBLE_PIP_PROBLEM;
      }
#ifdef NOISY_PIP
      std::cout << "Pivot column: " << j_
                << std::endl;
#endif

      /* ** Perform pivot operation ** */

      /* Check if column j_ or row i_ correspond to a problem variable */
      dimension_type var_j = n_a_d;
      dimension_type var_i = n_a_d;
      for (j=0; j<num_vars; ++j) {
        if (basis[j]) {
          if (mapping[j] == j_)
            var_j = j;
        } else {
          if (mapping[j] == i_)
            var_i = j;
        }
      }
      /* update basis */
      if (var_j != n_a_d) {
        basis[var_j] = false;
        mapping[var_j] = i_;
      }
      if (var_i != n_a_d) {
        basis[var_i] = true;
        mapping[var_i] = j_;
      }

      /* create the identity matrix row corresponding to basic variable j_ */
      Row prs(num_vars, tableau.s_capacity(), Row::Flags());
      Row prt(num_params, tableau.t_capacity(), Row::Flags());
      prs[j_] = tableau.get_denominator();
      /* swap it with pivot row which would become identity after pivoting */
      prs.swap(tableau.s[i_]);
      prt.swap(tableau.t[i_]);
      sign[i_] = ZERO;
      /* save current denominator corresponding to sij */
      Coefficient sij_denom = tableau.get_denominator();
      /* Compute columns s[*][j] : s[k][j] -= s[k][j_] * prs[j] / sij */
      for (j=0; j<num_vars; ++j) {
        if (j==j_)
          continue;
        const Coefficient& prsj = prs[j];
        if (prsj == 0)
          // if element j of pivot row is zero, nothing to do for this column
          continue;
        for (k=0; k<num_rows; ++k) {
          Coefficient mult = prsj * tableau.s[k][j_];
          if (mult % sij != 0) {
            // Must scale matrix to stay in integer case
            gcd_assign(gcd, mult, sij);
            Coefficient scale_factor = sij/gcd;
            tableau.scale(scale_factor);
            mult *= scale_factor;
          }
          tableau.s[k][j] -= mult / sij;
        }
      }

      /* Compute columns t[*][j] : t[k][j] -= t[k][j_] * prt[j] / sij */
      for (j=0; j<num_params; ++j) {
        const Coefficient& prtj = prt[j];
        if (prtj == 0)
          // if element j of pivot row is zero, nothing to do for this column
          continue;
        for (k=0; k<num_rows; ++k) {
          Coefficient c = prtj * tableau.s[k][j_];
          if (c % sij != 0) {
            // Must scale matrix to stay in integer case
            gcd_assign(gcd, c, sij);
            Coefficient scale_factor = sij/gcd;
            tableau.scale(scale_factor);
            c *= scale_factor;
          }
          c /= sij;
          tableau.t[k][j] -= c;

          s = sign[k];
          if (s != MIXED) {
            switch (s) {
               case ZERO:
                if (c > 0)
                  sign[k] = NEGATIVE;
                else if (c < 0)
                  sign[k] = POSITIVE;
                break;
              case POSITIVE:
                if (c > 0)
                  sign[k] = MIXED;
                break;
              case NEGATIVE:
                if (c < 0)
                  sign[k] = MIXED;
                break;
              default:
                break;
            }
          }
        }
      }

      /* compute column s[*][j_] : s[k][j_] /= sij */
      if (sij != sij_denom) {
        // Update column only if pivot != 1
        for (k=0; k<num_rows; ++k) {
          Coefficient& c = tableau.s[k][j_];
          Coefficient numerator = c * sij_denom;
          if (numerator % sij != 0) {
            Coefficient gcd;
            gcd_assign(gcd, numerator, sij);
            Coefficient scale_factor = sij/gcd;
            tableau.scale(scale_factor);
            numerator *= scale_factor;
          }
          c = numerator / sij;
        }
      }
      solution_valid = false;
    }

    /* Otherwise, we have found a row i__ with mixed parameter sign. */
    else if (i__ != n_a_d) {
      dimension_type neg = n_a_d;
      Coefficient ns, score;

      /* Look for a constraint with mixed parameter sign with no positive
       * variable coefficients */
      for (i=i__; i<num_rows; ++i) {
        if (sign[i] != MIXED)
          continue;
        for (j=0; j<num_vars; ++j) {
          if (tableau.s[i][j] > 0)
            break;
        }
        /* Choose row with lowest score, potentially eliminating
         * implicated tautologies if some exist */
        if (j == num_vars) {
          score = 0;
          for (j=0; j<num_params; ++j)
            score += tableau.t[i][j];
          if (neg == n_a_d || score < ns) {
            neg = i;
            ns = score;
          }
        }
      }
      if (neg != n_a_d) {
        i = neg;
#ifdef NOISY_PIP
        std::cout << "Found row with unknown parameter sign and negative "
                     "variable coefficients: " << i
                  << std::endl;
#endif
        Row &r = tableau.t[i];
        context.add_row(r);
        add_constraint(r, parameters);
        sign[i] = POSITIVE;
#ifdef NOISY_PIP
        using namespace IO_Operators;
        Constraint_System::const_iterator c = constraints_.begin();
        Constraint_System::const_iterator c_end = constraints_.end();
        Constraint_System::const_iterator c1 = c;
        while (++c1 != constraints_.end())
          c = c1;
        std::cout << "Adding tautology: " << *c << std::endl;
#endif
      } else {
        /* Heuristically choose "best" pivoting row. */
        Coefficient score;
        Coefficient best = 0;
        dimension_type best_i = n_a_d;
        for (i = i__; i < num_rows; ++i) {
          if (sign[i] != MIXED)
            continue;
          const Row& row = tableau.t[i];
          score = 0;
          for (j = 0; j < num_params; ++j)
            score += row[j];
          if (best_i == n_a_d || score < best) {
            best = score;
            best_i = i;
          }
        }
        i__ = best_i;

#ifdef NOISY_PIP
        {
          using namespace IO_Operators;
          Linear_Expression e;
          Variables_Set::const_iterator p;
          dimension_type j;
          for (p = parameters.begin(), j=1; p != parameters.end(); ++p, ++j)
            e += tableau.t[i__][j] * Variable(*p);
          e += tableau.t[i__][0];
          std::cout << "Found row with mixed parameter sign: " << i__
                    << "\nSolution depends on the sign of parameter " << e
                    << std::endl;
        }
#endif
        Row test(tableau.t[i__]);

        /* Create a solution Node to become "true" version of current Node */
        PIP_Tree_Node *tru = new PIP_Solution_Node(*this, true);
        context.add_row(test);
        PIP_Problem_Status status_t = tru->solve(tru, context, parameters,
                                      space_dimension);

        /* Modify *this to become "false" version */
        Constraint_System cs;
        Artificial_Parameter_Sequence aps;
        cs.swap(constraints_);
        aps.swap(artificial_parameters);
        PIP_Tree_Node *fals = this;
        Row &testf = context[context.num_rows()-1];
        negate_assign(testf, test, 1);
        PIP_Problem_Status status_f = solve(fals, context, parameters,
                                            space_dimension);

        if (status_t == UNFEASIBLE_PIP_PROBLEM
            && status_f == UNFEASIBLE_PIP_PROBLEM) {
          parent_ref = 0;
          return UNFEASIBLE_PIP_PROBLEM;
        } else if (status_t == UNFEASIBLE_PIP_PROBLEM) {
          cs.swap(constraints_);
          aps.swap(artificial_parameters);
          add_constraint(testf, parameters);
          return OPTIMIZED_PIP_PROBLEM;
        } else if (status_f == UNFEASIBLE_PIP_PROBLEM) {
          cs.swap(tru->constraints_);
          aps.swap(tru->artificial_parameters);
          tru->add_constraint(test, parameters);
          parent_ref = tru;
          return OPTIMIZED_PIP_PROBLEM;
        }

        /* Create a decision Node to become parent of current Node */
        PIP_Decision_Node* parent = new PIP_Decision_Node(fals, tru);
        parent->add_constraint(test, parameters);

        if (!cs.empty()) {
          /* If node to be solved had tautologies, store them in a new
             decision node */
          parent = new PIP_Decision_Node(0, parent);
          cs.swap(parent->constraints_);
        }
        aps.swap(parent->artificial_parameters);

        parent_ref = parent;
        return OPTIMIZED_PIP_PROBLEM;
      }
    }

    /* Otherwise, all parameters are positive: we have found a continuous
     * solution. If the solution happens to be integer, then it is a
     * solution of the  integer problem. Otherwise, we may need to generate
     * a new cut to try and get back into the integer case. */
    else {
#ifdef NOISY_PIP
      std::cout << "All parameters are positive."
                << std::endl;
#endif
      tableau.normalize();

      // Look for a row with non integer parameter coefficients (first is okay)
      const Coefficient& d = tableau.get_denominator();
      for (i=0; i<num_rows; ++i) {
        const Row& row = tableau.t[i];
        for (j=0; j<num_params; ++j) {
          if (row[j] % d != 0)
            goto endsearch;
        }
      }
      endsearch:

      if (i == num_rows) {
        /* The solution is integer */
#ifdef NOISY_PIP
        std::cout << "Solution found for problem in current node."
                  << std::endl;
#endif
        return OPTIMIZED_PIP_PROBLEM;
      }
      else {
        /* The solution is non-integer. We have to generate a cut. */
        Coefficient mod;
        /* Look for row which will generate the "deepest" cut */
        Coefficient score;
        Coefficient best = 0;
        dimension_type best_i = i;
        for (i_ = i; i_ < num_rows; ++i_) {
          const Row& row = tableau.t[i_];
          score = 0;
          for (j = 0; j < num_params; ++j) {
            mod_assign(mod, row[j], d);
            if (mod != 0)
              score += d - mod;
          }
          if (score > best) {
            best = score;
            best_i = i_;
          }
        }
        i = best_i;

#ifdef NOISY_PIP
        std::cout << "Row " << i << " contains non-integer coefficients. "
                  << "Cut generation required."
                  << std::endl;
#endif
        tableau.s.add_zero_rows(1, Row::Flags());
        tableau.t.add_zero_rows(1, Row::Flags());

        // Test if cut to be generated must be parametric or not
        const Row& row_t1 = tableau.t[i];
        for (j=1; j<num_params; ++j) {
          if (row_t1[j] % d != 0) {
            tableau.t.add_zero_columns(1);
            context.add_zero_columns(1);
            break;
          }
        }

        const Row& row_t = tableau.t[i];
        Row& cut_s = tableau.s[num_rows];
        Row& cut_t = tableau.t[num_rows];

        if (j < num_params) {
          // Fractional parameter coefficient found: generate parametric cut
          // Generate new artificial parameter
          Linear_Expression e;
          Variables_Set::const_iterator p;
          mod_assign(mod, row_t[0], d);
          if (mod != 0)
            e += (d-mod);
          for (p=parameters.begin(), j=1; j<num_params; ++j, ++p) {
            mod_assign(mod, row_t[j], d);
            if (mod != 0)
              e += (d-mod) * Variable(*p);
          }
          artificial_parameters.push_back(Artificial_Parameter(e, d));
          parameters.insert(space_dimension);
#ifdef NOISY_PIP
          using namespace IO_Operators;
          std::cout << "Creating new parameter " << Variable(space_dimension)
                    << " = (" << e << ")/" << d
                    << std::endl;
#endif
          ++space_dimension;

          // Update current context with constraints on the new parameter
          Row ctx1(num_params+1, Row::Flags());
          Row ctx2(num_params+1, Row::Flags());
          for (j=0; j<num_params; ++j) {
            mod_assign(mod, row_t[j], d);
            if (mod != 0) {
              ctx1[j] = d - mod;
              ctx2[j] = -ctx1[j];
            } else {
              ctx1[j] = 0;
              ctx2[j] = 0;
            }
          }
          ctx1[num_params] = -d;
          ctx2[num_params] = d;
          ctx2[0] += d-1;
#ifdef NOISY_PIP
          {
            Variables_Set::const_iterator p = parameters.begin();
            Linear_Expression e1;
            Linear_Expression e2;
            for (j=1; j<=num_params; ++j, ++p) {
              e1 += ctx1[j] * Variable(*p);
              e2 += ctx2[j] * Variable(*p);
            }
            e1 += ctx1[0];
            e2 += ctx2[0];
            std::cout << "Inserting into context: "
                      << Constraint(e1 >= 0) << " ; "
                      << Constraint(e2 >= 0) << std::endl;
          }
#endif
          context.add_row(ctx1);
          context.add_row(ctx2);
          cut_t[num_params] = d*d;
        }

        // Generate new cut
        const Row& row_s = tableau.s[i];
        for (j=0; j<num_vars; ++j) {
          mod_assign(mod, row_s[j], d);
          cut_s[j] = d*mod;
        }
        for (j=0; j<num_params; ++j) {
          mod_assign(mod, row_t[j], d);
          if (mod != 0)
            cut_t[j] = d*(mod - d);
          else
            cut_t[j] = 0;
        }
#ifdef NOISY_PIP
        {
          using namespace IO_Operators;
          Linear_Expression e;
          dimension_type ti = 1;
          dimension_type si = 0;
          for (j=0; j<space_dimension; ++j) {
            if (parameters.count(j) == 1)
              e += cut_t[ti++] * Variable(j);
            else
              e += cut_s[si++] * Variable(j);
          }
          std::cout << "Adding cut: " << Constraint(e + cut_t[0] >= 0)
                    << std::endl;
        }
#endif
        sign.push_back(NEGATIVE);
      }
    }
  } // Main loop of the simplex algorithm

  return OPTIMIZED_PIP_PROBLEM;
}

} // namespace Parma_Polyhedra_Library
