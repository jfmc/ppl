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
#include "PIP_Problem.defs.hh"

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

// Tranform expression "expr" into "-expr-1"
void
negate_assign(Row& x, const Row& y) {
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; )
    x[i] = -y[i];
  x[0] -= 1;
}

} // namespace

PIP_Tree_Node::PIP_Tree_Node(PIP_Problem* p)
  : problem(p),
    constraints_() {
}

PIP_Tree_Node::PIP_Tree_Node(const PIP_Tree_Node &x)
  : problem(x.problem),
    constraints_(x.constraints_) {
}

PIP_Decision_Node::~PIP_Decision_Node() {
  delete true_child;
  if (false_child)
    delete false_child;
}

PIP_Solution_Node::~PIP_Solution_Node() {
}

PIP_Solution_Node::PIP_Solution_Node(PIP_Problem* p)
  : PIP_Tree_Node(p),
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
  : PIP_Tree_Node(x.problem),
    tableau(x.tableau),
    basis(x.basis),
    mapping(x.mapping),
    sign(x.sign),
    solution(x.solution),
    solution_valid(x.solution_valid) {
  if (!empty_constraints)
    constraints_ = x.constraints_;
}

PIP_Decision_Node::PIP_Decision_Node(PIP_Problem* p,
                                     PIP_Tree_Node* fcp,
                                     PIP_Tree_Node* tcp)
  : PIP_Tree_Node(p),
    true_child(tcp),
    false_child(fcp) {
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

void
PIP_Tree_Node::add_constraint(const Row &row) {
  Linear_Expression e;
  const Variables_Set &parameters = problem->parameter_space_dimensions();
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
PIP_Decision_Node::solve(PIP_Tree_Node*& parent_ref, const Matrix& context) {
  PIP_Problem_Status return_status;
  PIP_Problem_Status stt;
  PIP_Problem_Status stf = UNFEASIBLE_PIP_PROBLEM;
  Matrix context_true(context);
  const Variables_Set &parameters = problem->parameter_space_dimensions();
  merge_assign(context_true, constraints_, parameters);
  stt = true_child->solve(true_child, context_true);
  if (false_child) {
    // Decision nodes with false child must have exactly one constraint
    PPL_ASSERT(1 == std::distance(constraints_.begin(), constraints_.end()));
    Matrix context_false(context);
    merge_assign(context_false, constraints_, parameters);
    Row &last = context_false[context_false.num_rows()-1];
    negate_assign(last, last);
    stf = false_child->solve(false_child, context_false);
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
PIP_Solution_Node::Rational_Matrix::normalize() {
  if (denominator == 1)
    return;
  dimension_type i_max = num_rows();
  dimension_type j_max = num_columns();
  dimension_type i, j;
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  gcd = denominator;

  for (i=0; i<i_max; ++i) {
    const Row &row = rows[i];
    for (j=0; j<j_max; ++j) {
      const Coefficient &x = row[j];
      if (x != 0) {
        gcd_assign(gcd, x, gcd);
        if (gcd == 1)
          return;
      }
    }
  }

  // Divide the coefficients by the GCD.
  for (i=0; i<i_max; ++i) {
    Row &row = rows[i];
    for (j=0; j<j_max; ++j) {
      Coefficient &x = row[j];
      exact_div_assign(x, x, gcd);
    }
  }
  exact_div_assign(denominator, denominator, gcd);
}

void
PIP_Solution_Node::Rational_Matrix::scale(const Coefficient &ratio) {
  dimension_type i, j;
  dimension_type i_max = num_rows();
  dimension_type j_max = num_columns();
  for (i=0; i<i_max; ++i)
    for (j=0; j<j_max; ++j)
      rows[i][j] *= ratio;
}

void
PIP_Tree_Node::ascii_dump(std::ostream& s) const {
  s << "\nconstraints_\n";
  constraints_.ascii_dump(s);
}

bool
PIP_Tree_Node::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "constraints_")
    return false;
  constraints_.ascii_load(s);

  PPL_ASSERT(OK());
  return true;
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
PIP_Solution_Node::parametric_values(const Variable &v) const {
  const_cast<PIP_Solution_Node&>(*this).update_solution();
  Variables_Set& parameters = problem->parameters;
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
  dimension_type i, j, k, j_;
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
    const Coefficient &sij = p[j];
    for (j_=0; j_<num_cols; ++j_) {
      if (j_ == j)
        continue;
      const Coefficient &sij_ = p[j_];
      for (k=0; k<num_rows; ++k) {
        if (k == i)
          continue;
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
      s[i][j_] = 0;
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
      s[k][j] = skj/sij;
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

  const Coefficient &denom_s = tableau.s.get_denominator();
  const Coefficient &denom_t = tableau.t.get_denominator();

  for (cst = input_cs.begin() + first_pending_constraint;
       cst < input_cs.end(); ++cst) {
    int v = 0;
    int p = 1;
    Row var(n_vars, tableau.s.capacity(), Row::Flags());
    Row param(n_params+1, tableau.t.capacity(), Row::Flags());
    Coefficient cnst_term = cst->inhomogeneous_term();
    if (cst->is_strict_inequality())
      // convert c > 0  <=>  c-1 >= 0
      cnst_term -= 1;
    param[0] = cnst_term * denom_t;
    for (i=0; i<internal_space_dim; i++) {
      if (parameters.count(i) == 1) {
        param[p++] = cst->coefficient(Variable(i)) * denom_t;
      } else {
        const Coefficient &c = cst->coefficient(Variable(i));
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
PIP_Solution_Node::update_solution() {
  if (solution_valid)
    return;
  dimension_type num_vars = tableau.s.num_columns();
  if (solution.size() != num_vars)
    solution.resize(num_vars);
  for (dimension_type i = num_vars; i-- > 0; ) {
    Linear_Expression &sol = solution[i];
    if (basis[i]) {
      sol = Linear_Expression(0);
    } else {
      Row &row = tableau.t[mapping[i]];
      sol = Linear_Expression(row[0]);
      dimension_type k;
      Variables_Set::iterator j;
      Variables_Set::iterator j_end = problem->parameters.end();
      for (j = problem->parameters.begin(), k = 1; j != j_end; ++j, ++k)
        sol += row[k] * Variable(*j);
    }
  }
  solution_valid = true;
}

PIP_Problem_Status
PIP_Solution_Node::solve(PIP_Tree_Node*& parent_ref,
                         const Matrix& ctx) {
  Matrix context(ctx);
  const Variables_Set &parameters = problem->parameter_space_dimensions();
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
        row[0] -= 1;
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
      Row prs(num_vars, tableau.s.capacity(), Row::Flags());
      Row prt(num_params, tableau.t.capacity(), Row::Flags());
      prs[j_] = tableau.s.get_denominator();
      /* swap it with pivot row which would become identity after pivoting */
      prs.swap(tableau.s[i_]);
      prt.swap(tableau.t[i_]);
      sign[i_] = ZERO;
      for (j=0; j<num_vars; ++j) {
        if (j==j_)
          continue;
        for (k=0; k<num_rows; ++k) {
          Coefficient mult = prs[j] * tableau.s[k][j_];
          if (mult % sij != 0) {
            // Must scale matrix to stay in integer case
            gcd_assign(gcd, mult, sij);
            Coefficient scale_factor = sij/gcd;
            tableau.s.scale(scale_factor);
            mult *= scale_factor;
          }
          tableau.s[k][j] -= mult / sij;
        }
      }

      /* create the identity matrix row corresponding to basic variable j_ */
      for (j=0; j<num_params; ++j) {
        for (k=0; k<num_rows; ++k) {
          Coefficient c = prt[j] * tableau.s[k][j_];
          if (c % sij != 0) {
            // Must scale matrix to stay in integer case
            gcd_assign(gcd, c, sij);
            Coefficient scale_factor = sij/gcd;
            tableau.t.scale(scale_factor);
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

      for (k=0; k<num_rows; ++k) {
        Coefficient &c = tableau.s[k][j_];
        if (c % sij != 0) {
          Coefficient gcd;
          gcd_assign(gcd, c, sij);
          Coefficient scale_factor = sij/gcd;
          tableau.s.scale(scale_factor);
        }
        c /= sij;
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
        add_constraint(r);
        sign[i] = POSITIVE;
#ifdef NOISY_PIP
        Constraint_System::const_iterator c = constraints_.begin();
        Constraint_System::const_iterator c_end = constraints_.end();
        Constraint_System::const_iterator c1 = c;
        while (++c1 != constraints_.end())
          c = c1;
        std::cout << "Adding tautology: ";
        c->ascii_dump(std::cout);
#endif
      } else {
#ifdef NOISY_PIP
        std::cout << "Found row with mixed parameter sign: " << i__
                  << std::endl
                  << "Solution depends on the sign of parameter"
                  << std::endl;
#endif
        Row test(tableau.t[i__]);

        /* Create a solution Node to become "true" version of current Node */
        PIP_Tree_Node *tru = new PIP_Solution_Node(*this, true);
        context.add_row(test);
        PIP_Problem_Status status_t = tru->solve(tru, context);

        /* Modify *this to become "false" version */
        Constraint_System cs;
        cs.swap(constraints_);
        PIP_Tree_Node *fals = this;
        Row &testf = context[context.num_rows()-1];
        negate_assign(testf, test);
        PIP_Problem_Status status_f = solve(fals, context);

        if (status_t == UNFEASIBLE_PIP_PROBLEM
            && status_f == UNFEASIBLE_PIP_PROBLEM) {
          parent_ref = 0;
          return UNFEASIBLE_PIP_PROBLEM;
        } else if (status_t == UNFEASIBLE_PIP_PROBLEM) {
          cs.swap(constraints_);
          add_constraint(testf);
          return OPTIMIZED_PIP_PROBLEM;
        } else if (status_f == UNFEASIBLE_PIP_PROBLEM) {
          cs.swap(tru->constraints_);
          tru->add_constraint(test);
          parent_ref = tru;
          return OPTIMIZED_PIP_PROBLEM;
        }

        /* Create a decision Node to become parent of current Node */
        PIP_Decision_Node* parent
        = new PIP_Decision_Node(fals->problem, fals, tru);
        parent->add_constraint(test);

        if (!cs.empty()) {
          /* If node to be solved had tautologies, store them in a new
             decision node */
          parent = new PIP_Decision_Node(fals->problem, 0, parent);
          cs.swap(parent->constraints_);
        }

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
      tableau.s.normalize();
      tableau.t.normalize();

      if (tableau.s.is_integer() && tableau.t.is_integer()) {
        /* The solution is integer */
#ifdef NOISY_PIP
        std::cout << "Solution found for in current node."
                  << std::endl;
#endif
        return OPTIMIZED_PIP_PROBLEM;
      }
      else {
        /* The solution is non-integer. We have to generate a cut. */
        //FIXME: to be finished
#ifdef NOISY_PIP
        std::cout << "Cut generation required."
                  << std::endl;
#endif
      }
    }
  } // Main loop of the simplex algorithm

  return OPTIMIZED_PIP_PROBLEM;
}

} // namespace Parma_Polyhedra_Library
