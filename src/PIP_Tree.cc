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

// Compute x += c * y
void
add_assign(Row& x, const Row& y, Coefficient_traits::const_reference c) {
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; )
    add_mul_assign(x[i], c, y[i]);
}

// Merge constraint systems such as x = x U y
void
merge_assign(Constraint_System& x, const Constraint_System& y) {
  for (Constraint_System::const_iterator y_i = y.begin(),
         y_end = y.end(); y_i != y_end; ++y_i)
    x.insert(*y_i);
}

} // namespace


PIP_Decision_Node::~PIP_Decision_Node() {
  delete true_child;
  delete false_child;
}

PIP_Solution_Node::~PIP_Solution_Node() {
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

bool
PIP_Solution_Node::OK() const {
  /* FIXME: write me! */
  return true;
}

bool
PIP_Decision_Node::OK() const {
  /* FIXME: write me! */
  return true;
}

void
PIP_Decision_Node::update_tableau(PIP_Tree_Node ** /* parent_ref */,
                                  dimension_type external_space_dim,
                                  dimension_type first_pending_constraint,
                                  const Constraint_Sequence &input_cs,
                                  const Variables_Set &parameters) {
  true_child->update_tableau(&true_child,
                             external_space_dim,
                             first_pending_constraint,
                             input_cs,
                             parameters);
  if (false_child)
    false_child->update_tableau(&false_child,
                                external_space_dim,
                                first_pending_constraint,
                                input_cs,
                                parameters);
}

PIP_Problem_Status
PIP_Decision_Node::solve(PIP_Tree_Node **parent_ref,
                         const Constraint_System& context) {
  PIP_Problem_Status return_status;
  PIP_Problem_Status stt;
  PIP_Problem_Status stf = UNFEASIBLE_PIP_PROBLEM;
  Constraint_System context_true(context);
  merge_assign(context_true, constraints_);
  stt = true_child->solve(&true_child, context_true);
  if (false_child) {
    // Decision nodes with false child must have exactly one constraint
    PPL_ASSERT(1 == std::distance(constraints_.begin(), constraints_.end()));
    Constraint_System context_false(context);
    Constraint_System::const_iterator ci = constraints_.begin();
    PPL_ASSERT(ci->is_nonstrict_inequality());
    Linear_Expression expr = Linear_Expression(*ci);
    expr += 1;
    Constraint c(expr <= 0);
    context_false.insert(c);
    stf = false_child->solve(&false_child, context_false);
  }

  if (stt == UNFEASIBLE_PIP_PROBLEM && stf == UNFEASIBLE_PIP_PROBLEM) {
    return_status = UNFEASIBLE_PIP_PROBLEM;
    *parent_ref = 0;
    delete this;
  } else
    return_status = OPTIMIZED_PIP_PROBLEM;
  return return_status;
}

void
PIP_Solution_Node::Rational_Matrix::normalize() {
  //FIXME
}

void
PIP_Solution_Node::Rational_Matrix::ascii_dump(std::ostream& s) const {
  s << "denominator " << denominator << "\n";
  Matrix::ascii_dump(s);
}

bool
PIP_Solution_Node::Rational_Matrix::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "denominator")
    return false;
  Coefficient den;
  if (!(s >> den))
    return false;
  denominator = den;

  return Matrix::ascii_load(s);
}

void
PIP_Solution_Node::ascii_dump(std::ostream& s) const {
  s << "\nvariable_tableau\n";
  tableau.s.ascii_dump(s);

  s << "\nparameter_tableau\n";
  tableau.t.ascii_dump(s);
}


bool
PIP_Solution_Node::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "simplex_tableau")
    return false;
  if (!tableau.s.ascii_load(s))
    return false;

  if (!(s >> str) || str != "parameter_tableau")
    return false;
  if (!tableau.t.ascii_load(s))
    return false;

  PPL_ASSERT(OK());
  return true;
}

PIP_Solution_Node::Row_Sign
PIP_Solution_Node::row_sign(const Row &x) {
  PIP_Solution_Node::Row_Sign sign = ZERO;
  Coefficient c;
  for (int i = x.size(); i-- > 0; ) {
    c = x[i];
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

void
PIP_Solution_Node::update_tableau(PIP_Tree_Node ** /* parent_ref */,
                                  dimension_type external_space_dim,
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

  Coefficient denom_s = tableau.s.get_denominator();
  Coefficient denom_t = tableau.t.get_denominator();

  for (cst = input_cs.begin() + first_pending_constraint;
       cst < input_cs.end(); ++cst) {
    int v = 0;
    int p = 1;
    Row var(n_vars, tableau.s.capacity(), Row::Flags());
    Row param(n_params+1, tableau.t.capacity(), Row::Flags());
    Coefficient cnst_term = -cst->inhomogeneous_term();
    if (cst->is_strict_inequality())
      // convert c > 0  <=>  c-1 >= 0
      cnst_term -= 1;
    param[0] = cnst_term * denom_t;
    for (i=0; i<internal_space_dim; i++) {
      if (parameters.count(i) == 1) {
        param[p++] = cst->coefficient(Variable(i)) * denom_t;
      } else {
        Coefficient c = cst->coefficient(Variable(i));
        dimension_type idx = mapping[v];
        if (basis[v])
          // Basic variable : add c * x_i
          var[idx] += c * denom_s;
        else {
          // Nonbasic variable : add c * row_i
          add_assign(var, tableau.s[idx], c);
          add_assign(param, tableau.t[idx], c);
        }
        ++v;
      }
    }
    // FIXME: must handle equality constraints
    tableau.s.add_row(var);
    tableau.t.add_row(param);
    sign.push_back(row_sign(param));
  }
  // FIXME: decide emptiness detection (and node removal)
}

PIP_Problem_Status
PIP_Solution_Node::solve(PIP_Tree_Node** /* parent_ref */,
                         const Constraint_System& /* context */) {
  //FIXME
  return OPTIMIZED_PIP_PROBLEM;
}

} // namespace Parma_Polyhedra_Library
