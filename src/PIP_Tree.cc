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

// Merge constraint system to a Matrix-form context such as x = x U y
void
merge_assign(Matrix& x, const Constraint_System& y) {
  dimension_type width = x.num_columns();
  PPL_ASSERT(y.empty() || y.begin()->space_dimension() == width-1);
  Row row(width, Row::Flags());
  for (Constraint_System::const_iterator y_i = y.begin(),
         y_end = y.end(); y_i != y_end; ++y_i) {
    PPL_ASSERT(y_i->is_nonstrict_inequality());
    for (dimension_type j=width-1; j-- > 0; )
      row[j+1] = y_i->coefficient(Variable(j));
    row[0] = -y_i->inhomogeneous_term();
    x.add_row(row);
  }
}

// Tranform expression "expr" into "-expr-1"
void
negate_assign(Row& x, const Row& y) {
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; )
    x[i] = -y[i];
  x[0] -= 1;
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
PIP_Decision_Node::solve(PIP_Tree_Node **parent_ref, const Matrix& context) {
  PIP_Problem_Status return_status;
  PIP_Problem_Status stt;
  PIP_Problem_Status stf = UNFEASIBLE_PIP_PROBLEM;
  Matrix context_true(context);
  merge_assign(context_true, constraints_);
  stt = true_child->solve(&true_child, context_true);
  if (false_child) {
    // Decision nodes with false child must have exactly one constraint
    PPL_ASSERT(1 == std::distance(constraints_.begin(), constraints_.end()));
    Matrix context_false(context);
    merge_assign(context_false, constraints_);
    Row &last = context_false[context_false.num_rows()-1];
    negate_assign(last, last);
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

bool
PIP_Solution_Node::compatibility_check(const Matrix &ctx, const Row &cnst) {
  Matrix s(ctx);
  s.add_row(cnst);
  dimension_type i, j, k, j_;
  dimension_type num_rows = s.num_rows();
  dimension_type num_cols = s.num_columns();
  bool result = false;

  /* Perform simplex pivots on the context until we find an empty solution
   * or an optimum */
  for(;;) {
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
    Coefficient sij = p[j];
    for (j_=0; j_<num_cols; ++j_) {
      if (j_ == j)
        continue;
      Coefficient sij_ = p[j_];
      for (k=0; k<num_rows; ++k) {
        Coefficient mult = s[k][j] * sij_;
        if (mult % sij != 0) {
          // Must scale row to stay in integer case
          Coefficient gcd;
          gcd_assign(gcd, mult, sij);
          Coefficient scale_factor = sij/gcd;
          add_assign(s[k], s[k], scale_factor);
          mult *= scale_factor;
        }
        s[k][j_] -= mult / sij;
      }
    }
    for (k=0; k<num_rows; ++k) {
      Coefficient skj = s[k][j];
      if (skj % sij != 0) {
        // as above, we must perform row scaling
        Coefficient gcd;
        gcd_assign(gcd, skj, sij);
        Coefficient scale_factor = sij/gcd;
        add_assign(s[k], s[k], scale_factor);
        skj *= scale_factor;
      }
      s[k][j_] = skj/sij;
    }
  }

  return result;
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
PIP_Solution_Node::solve(PIP_Tree_Node** parent_ref,
                         const Matrix& ctx) {
  Matrix context(ctx);
  merge_assign(context, constraints_);
  const dimension_type n_a_d = not_a_dimension();

  // Main loop of the simplex algorithm
  for(;;) {
    dimension_type i;
    dimension_type i_ = n_a_d;
    dimension_type i__ = n_a_d;
    dimension_type num_rows = tableau.t.num_rows();
    dimension_type num_vars = tableau.s.num_columns();
    dimension_type num_params = tableau.t.num_columns();
    Row_Sign s;

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
        negate_assign(c, tableau.t[i]);
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

    /* If we have found a row i_ with negative parameters :
       Either the problem is empty, or a pivoting step is required
    */
    if (i_ != n_a_d) {
#if NOISY_PIP
      std::cout << "Found row with negative parameters: " << i_
                << std::endl;
#endif
      dimension_type j;
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
                cij = (mk==j) ? tableau.s.get_denominator() : 0;
                cij_ = (mk==j_) ? tableau.s.get_denominator() : 0;
              } else {
                cij = tableau.s[mk][j];
                cij_ = tableau.s[mk][j_];
              }
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
#if NOISY_PIP
        std::cout << "No positive pivot found: Solution = _|_"
                  << std::endl;
#endif
        *parent_ref = 0;
        delete this;
        return UNFEASIBLE_PIP_PROBLEM;
      }
#if NOISY_PIP
      std::cout << "Pivot column: " << j_
                << std::endl;
#endif

    }

  } // Main loop of the simplex algorithm

  //FIXME
  return OPTIMIZED_PIP_PROBLEM;
}

} // namespace Parma_Polyhedra_Library
