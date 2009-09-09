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

namespace Parma_Polyhedra_Library {

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

void
PIP_Decision_Node::update_tableau(PIP_Tree_Node **parent_ref,
                                  dimension_type external_space_dim,
                                  dimension_type first_pending_constraint,
                                  const Constraint_Sequence &input_cs,
                                  const Variables_Set &parameters) {
  true_child->update_tableau(parent_ref,
                             external_space_dim,
                             first_pending_constraint,
                             input_cs,
                             parameters);
  if (false_child)
    false_child->update_tableau(parent_ref,
                                external_space_dim,
                                first_pending_constraint,
                                input_cs,
                                parameters);
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

void
PIP_Solution_Node::update_tableau(PIP_Tree_Node **parent_ref,
                                  dimension_type external_space_dim,
                                  dimension_type first_pending_constraint,
                                  const Constraint_Sequence &input_cs,
                                  const Variables_Set &parameters) {
  dimension_type i, j;
  dimension_type n_params = parameters.size();
  dimension_type n_vars = external_space_dim - n_params;
  dimension_type n_vars_int = tableau.s.num_columns();
  dimension_type n_constr_int = tableau.s.num_rows();
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
    Row var(n_vars, Row::Flags());
    Row param(n_params+1, Row::Flags());
    Coefficient cnst_term = -cst->inhomogeneous_term();
    if (cst->is_strict_inequality())
      // convert c > 0  <=>  c-1 >= 0
      cnst_term -= 1;
    param[0] = cnst_term * denom_t;
    for (i=0; i<internal_space_dim; i++) {
      if (parameters.count(i) == 1) {
        param[p++] = cst->coefficient(Variable(i)) * denom_t;
      } else {
        Coefficient c = cst->coefficient(Variable(i)) * denom_s;
        dimension_type idx = mapping[v];
        if (basis[v])
          // Basic variable : add c * x_i
          var[idx] += c;
        else {
          // Nonbasic variable : add c * row_i
          const Row &sr = tableau.s[idx];
          const Row &st = tableau.t[idx];
          for (j=0; j<sr.size(); j++)
            var[j] += c*sr[j];
          for (j=0; j<st.size(); j++)
            param[j] += c*st[j];
        }
        ++v;
      }
    }
    // FIXME: must handle equality constraints
    tableau.s.add_row(var);
    tableau.t.add_row(param);
  }
  // FIXME: decide emptiness detection (and node removal)
}

} // namespace Parma_Polyhedra_Library
