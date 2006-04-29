/* Octagonal_Shape class implementation: non-inline template functions.
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

#ifndef PPL_Octagonal_Shape_templates_hh
#define PPL_Octagonal_Shape_templates_hh 1

#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <cassert>
#include <vector>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <algorithm>

// FIXME: this is only to get access to
// Implementation::BD_Shapes::div_round_up(),
// Implementation::BD_Shapes::numer_denom(),
// Implementation::BD_Shapes::min_assign().
#include "BD_Shape.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
Octagonal_Shape<T>::Octagonal_Shape(const Polyhedron& ph,
                                    const Complexity_Class complexity)
  : matrix(0), space_dim(0), status() {
  using Implementation::BD_Shapes::div_round_up;
  const dimension_type num_dimensions = ph.space_dimension();

  if (ph.marked_empty()) {
    *this = Octagonal_Shape(num_dimensions, EMPTY);
    return;
  }

  if (num_dimensions == 0) {
    *this = Octagonal_Shape(num_dimensions, UNIVERSE);
    return;
  }

  // Build from generators when we do not care about complexity
  // or when the process has polynomial complexity.
  if (complexity == ANY_COMPLEXITY
      || (!ph.has_pending_constraints() && ph.generators_are_up_to_date())) {
    *this = Octagonal_Shape(ph.generators());
    return;
  }

  // We cannot afford exponential complexity, we do not have a complete set
  // of generators for the polyhedron, and the polyhedron is not trivially
  // empty or zero-dimensional.  Constraints, however, are up to date.
  assert(ph.constraints_are_up_to_date());

  if (!ph.has_something_pending() && ph.constraints_are_minimized()) {
    // If the constraint system of the polyhedron is minimized,
    // the test `is_universe()' has polynomial complexity.
    if (ph.is_universe()) {
      *this = Octagonal_Shape(num_dimensions, UNIVERSE);
      return;
    }
  }

  // See if there is at least one inconsistent constraint in `ph.con_sys'.
  for (Constraint_System::const_iterator i = ph.con_sys.begin(),
	 cs_end = ph.con_sys.end(); i != cs_end; ++i)
    if (i->is_inconsistent()) {
      *this = Octagonal_Shape(num_dimensions, EMPTY);
      return;
    }

  // If `complexity' allows it, use simplex to derive the exact (modulo
  // the fact that our OSs are topologically closed) variable bounds.
  if (complexity == SIMPLEX_COMPLEXITY) {
    LP_Problem lp;
    lp.set_optimization_mode(MAXIMIZATION);

    const Constraint_System& ph_cs = ph.constraints();
    if (!ph_cs.has_strict_inequalities())
      lp.add_constraints(ph_cs);
    else
      // Adding to `lp' a topologically closed version of `ph_cs'.
      for (Constraint_System::const_iterator i = ph_cs.begin(),
	     iend = ph_cs.end(); i != iend; ++i) {
	const Constraint& c = *i;
	if (c.is_strict_inequality())
	  lp.add_constraint(Linear_Expression(c) >= 0);
	else
	  lp.add_constraint(c);
      }

    // Check for unsatisfiability.
    if (!lp.is_satisfiable()) {
      *this = Octagonal_Shape<T>(num_dimensions, EMPTY);
      return;
    }

    // Start with a universe OS that will be refined by the simplex.
    *this = Octagonal_Shape<T>(num_dimensions, UNIVERSE);
    // Get all the upper bounds.
    Generator g(point());
    TEMP_INTEGER(num);
    TEMP_INTEGER(den);
    for (dimension_type i = 0; i < num_dimensions; ++i) {
      Variable x(i);
      // Evaluate optimal upper bound for `x <= ub'.
      lp.set_objective_function(x);
      if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, num, den);
	num *= 2;
	div_round_up(matrix[2*i+1][2*i], num, den);
      }
      // Evaluate optimal upper bounds for `x + y <= ub'.
      for (dimension_type j = 0; j < i; ++j) {
	Variable y(j);
	lp.set_objective_function(x + y);
	if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	  g = lp.optimizing_point();
	  lp.evaluate_objective_function(g, num, den);
	  div_round_up(matrix[2*i+1][2*j], num, den);
	}
      }
      // Evaluate optimal upper bound for `x - y <= ub'.
      for (dimension_type j = 0; j < num_dimensions; ++j) {
	if (i == j)
	  continue;
	Variable y(j);
	lp.set_objective_function(x - y);
	if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	  g = lp.optimizing_point();
	  lp.evaluate_objective_function(g, num, den);
	  div_round_up((i < j ? matrix[2*j][2*i] : matrix[2*i+1][2*j+1]),
		       num, den);
	}
      }
      // Evaluate optimal upper bound for `y - x <= ub'.
      for (dimension_type j = 0; j < num_dimensions; ++j) {
	if (i == j)
	  continue;
	Variable y(j);
	lp.set_objective_function(x - y);
	if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	  g = lp.optimizing_point();
	  lp.evaluate_objective_function(g, num, den);
	  div_round_up((i < j ? matrix[2*j][2*i] : matrix[2*i+1][2*j+1]),
		       num, den);
	}
      }
      // Evaluate optimal upper bound for `-x - y <= ub'.
      for (dimension_type j = 0; j < i; ++j) {
	Variable y(j);
	lp.set_objective_function(-x - y);
	if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	  g = lp.optimizing_point();
	  lp.evaluate_objective_function(g, num, den);
 	  div_round_up(matrix[2*i][2*j+1], num, den);
	}
      }
      // Evaluate optimal upper bound for `-x <= ub'.
      lp.set_objective_function(-x);
      if (lp.solve() == OPTIMIZED_LP_PROBLEM) {
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, num, den);
	num *= 2;
	div_round_up(matrix[2*i][2*i+1], num, den);
      }
    }
    status.set_strongly_closed();
    assert(OK());
    return;
  }

  // Extract easy-to-find bounds from constraints.
  *this = Octagonal_Shape(ph.constraints());
}

template <typename T>
Octagonal_Shape<T>::Octagonal_Shape(const Generator_System& gs)
  : matrix(gs.space_dimension()),
    space_dim(gs.space_dimension()), status(){
  using Implementation::BD_Shapes::max_assign;
  using Implementation::BD_Shapes::div_round_up;

  const Generator_System::const_iterator gs_begin = gs.begin();
  const Generator_System::const_iterator gs_end = gs.end();
  if (gs_begin == gs_end) {
    // An empty generator system defines the empty polyhedron.
    set_empty();
    assert(OK());
    return;
  }

  typename OR_Matrix<N>::row_iterator mat_begin = matrix.row_begin();
  N tmp;

  bool mat_initialized = false;
  bool point_seen = false;
  // Going through all the points and closure points.
  for (Generator_System::const_iterator k = gs_begin; k != gs_end; ++k) {
    const Generator& g = *k;
    switch (g.type()) {
    case Generator::POINT:
      point_seen = true;
      // Intentionally fall through.
    case Generator::CLOSURE_POINT:
      if (!mat_initialized) {
	// When handling the first (closure) point, we initialize the DBM.
	mat_initialized = true;
	const Coefficient& d = g.divisor();
	for (dimension_type i = 0; i < space_dim; ++i) {
	  const Coefficient& g_i = g.coefficient(Variable(i));
	  const dimension_type di = 2*i;
	  typename OR_Matrix<N>::row_reference_type x_i = *(mat_begin+di);
	  typename OR_Matrix<N>::row_reference_type x_ii = *(mat_begin+di+1);
	  for (dimension_type j = 0; j < i; ++j) {
	    const Coefficient& g_j = g.coefficient(Variable(j));
	    const dimension_type dj = 2*j;
	    // Set for any point the hyperplanes passing in the point
	    // and having the octagonal gradient.
	    // Let be P = [P_1, P_2, ..., P_n] point.
	    // Hyperplanes: X_i - X_j = P_i - P_j.
	    div_round_up(x_i[dj], g_j - g_i, d);
	    div_round_up(x_ii[dj+1], g_i - g_j, d);
	    // Hyperplanes: X_i + X_j = P_i + P_j.
	    div_round_up(x_i[dj+1], -g_j - g_i, d);
	    div_round_up(x_ii[dj], g_i + g_j, d);
	  }
	  // Hyperplanes: X_i = P_i.
	  div_round_up(x_i[di+1], -g_i - g_i, d);
	  div_round_up(x_ii[di], g_i + g_i, d);
	}
      }
      else {
	// This is not the first point: the matrix already contains
	// valid values and we must compute maxima.
	const Coefficient& d = g.divisor();
	for (dimension_type i = 0; i < space_dim; ++i) {
	  const Coefficient& g_i = g.coefficient(Variable(i));
	  const dimension_type di = 2*i;
	  typename OR_Matrix<N>::row_reference_type x_i = *(mat_begin+di);
	  typename OR_Matrix<N>::row_reference_type x_ii = *(mat_begin+di+1);
	  for (dimension_type j = 0; j < i; ++j) {
	    const Coefficient& g_j = g.coefficient(Variable(j));
	    const dimension_type dj = 2*j;
	    // Set for any point the straight lines passing in the point
	    // and having the octagonal gradient; compute maxima values.
	    // Let be P = [P_1, P_2, ..., P_n] point.
	    // Hyperplane: X_i - X_j = max (P_i - P_j, const).
	    div_round_up(tmp, g_j - g_i, d);
	    max_assign(x_i[dj], tmp);
	    div_round_up(tmp, g_i - g_j, d);
	    max_assign(x_ii[dj+1], tmp);
	    // Hyperplane: X_i + X_j = max (P_i + P_j, const).
	    div_round_up(tmp, -g_j - g_i, d);
	    max_assign(x_i[dj+1], tmp);
	    div_round_up(tmp, g_i + g_j, d);
	    max_assign(x_ii[dj], tmp);
	  }
	  // Hyperplane: X_i = max (P_i, const).
	  div_round_up(tmp, -g_i - g_i, d);
	  max_assign(x_i[di+1], tmp);
	  div_round_up(tmp, g_i + g_i, d);
	  max_assign(x_ii[di], tmp);
	}
      }
      break;
    default:
      // Lines and rays temporarily ignored.
      break;
    }
  }

  if (!point_seen)
    // The generator system is not empty, but contains no points.
    throw std::invalid_argument("PPL::Octagonal_Shape<T>"
				"::Octagonal_Shape(gs):\n"
				"the non-empty generator system gs "
				"contains no points.");

  // Going through all the lines and rays.
  for (Generator_System::const_iterator k = gs_begin; k != gs_end; ++k) {
    const Generator& g = *k;
    switch (g.type()) {
    case Generator::LINE:
	for (dimension_type i = 0; i < space_dim; ++i) {
	  const Coefficient& g_i = g.coefficient(Variable(i));
	  const dimension_type di = 2*i;
	  typename OR_Matrix<N>::row_reference_type x_i = *(mat_begin+di);
	  typename OR_Matrix<N>::row_reference_type x_ii = *(mat_begin+di+1);
	  for (dimension_type j = 0; j < i; ++j) {
	    const Coefficient& g_j = g.coefficient(Variable(j));
	    const dimension_type dj = 2*j;
	    // Set for any line the right limit.
	    if (g_i != g_j) {
	      // Hyperplane: X_i - X_j <=/>= +Inf.
	      x_i[dj] = PLUS_INFINITY;
	      x_ii[dj+1] = PLUS_INFINITY;
	    }
	    if (g_i != -g_j) {
	      // Hyperplane: X_i + X_j <=/>= +Inf.
	      x_i[dj+1] = PLUS_INFINITY;
	      x_ii[dj] = PLUS_INFINITY;
	    }
	  }
	  if (g_i != 0) {
	    // Hyperplane: X_i <=/>= +Inf.
	    x_i[di+1] = PLUS_INFINITY;
	    x_ii[di] = PLUS_INFINITY;
	  }
	}
      break;
    case Generator::RAY:
	for (dimension_type i = 0; i < space_dim; ++i) {
	  const Coefficient& g_i = g.coefficient(Variable(i));
	  const dimension_type di = 2*i;
	  typename OR_Matrix<N>::row_reference_type x_i = *(mat_begin+di);
	  typename OR_Matrix<N>::row_reference_type x_ii = *(mat_begin+di+1);
	  for (dimension_type j = 0; j < i; ++j) {
	    const Coefficient& g_j = g.coefficient(Variable(j));
	    const dimension_type dj = 2*j;
	    // Set for any ray the right limit in the case
	    // of the binary constraints.
	    if (g_i < g_j)
	      // Hyperplane: X_i - X_j >= +Inf.
	      x_i[dj] = PLUS_INFINITY;
	    if (g_i > g_j)
	      // Hyperplane: X_i - X_j <= +Inf.
	      x_ii[dj+1] = PLUS_INFINITY;
	    if (g_i < -g_j)
	      // Hyperplane: X_i + X_j >= +Inf.
	      x_i[dj+1] = PLUS_INFINITY;
	    if (g_i > -g_j)
	      // Hyperplane: X_i + X_j <= +Inf.
	      x_ii[dj] = PLUS_INFINITY;
	  }
	  // Case: unary constraints.
	  if (g_i < 0)
	    // Hyperplane: X_i  = +Inf.
	    x_i[di+1] = PLUS_INFINITY;
	  if (g_i > 0)
	    // Hyperplane: X_i  = +Inf.
	    x_ii[di] = PLUS_INFINITY;
	}
      break;
    default:
      // Points and closure points already dealt with.
      break;
    }
  }
  status.set_strongly_closed();
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::add_constraint(const Constraint& c) {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > space_dim)
    throw_dimension_incompatible("add_constraint(c)", c);
  // Strict inequalities are not allowed.
  if (c.is_strict_inequality())
    throw_constraint_incompatible("add_constraint(c)");

  dimension_type num_vars = 0;
  dimension_type i = 0;
  dimension_type j = 0;
  Coefficient coeff;
  Coefficient term = c.inhomogeneous_term();
  // Constraints that are not octagonal differences are ignored.
  if (!extract_octagonal_difference(c, c_space_dim, num_vars,
				    i, j, coeff, term))
    return;

  if (num_vars == 0) {
    // Dealing with a trivial constraint.
    if (c.inhomogeneous_term() < 0)
      status.set_empty();
    return;
  }

  // Select the cell to be modified for the "<=" part of constraint.
  typename OR_Matrix<N>::row_iterator k = matrix.row_begin() + i;
  typename OR_Matrix<N>::row_reference_type r = *k;
  N& r_j = r[j];
  // Set `coeff' to the absolute value of itself.
  if (coeff < 0)
    coeff = -coeff;

  bool is_oct_changed = false;
  // Compute the bound for `r_j', rounding towards plus infinity.
  N d;
  div_round_up(d, term, coeff);
  if (r_j > d) {
    r_j = d;
    is_oct_changed = true;
  }

  if (c.is_equality()) {
    // Select the right row of the cell.
    if (i%2 == 0)
      ++k;
    else
      --k;

    typename OR_Matrix<N>::row_reference_type r1 = *k;
    // Select the right column of the cell.
    dimension_type h = coherent_index(j);

    N& r1_h = r1[h];
    N d;
    div_round_up(d, -term, coeff);
    if (r1_h > d) {
      r1_h = d;
      is_oct_changed = true;
    }
  }

  // This method not preserve closure.
  if (is_oct_changed && marked_strongly_closed())
    status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
dimension_type
Octagonal_Shape<T>::affine_dimension() const {
  const dimension_type n_rows = matrix.num_rows();

  // Strong closure is necessary to detect emptiness
  // and all (possibly implicit) equalities.
  strong_closure_assign();
  if (marked_empty())
    return 0;

  // The vector `leaders' is used to represent no-singular
  // equivalence classes:
  // `leaders[i] == i' if and only if `i' is the leader of its
  // equivalence class (i.e., the minimum index in the class);
  std::vector<dimension_type> leaders;
  compute_leaders(leaders);

  // Due to the splitting of variables, the affine dimension is the
  // number of non-singular positive zero-equivalence classes.
  dimension_type affine_dim = 0;
  for (dimension_type i = 0; i < n_rows; i += 2) {
    const dimension_type ci = coherent_index(i);
    // Note: disregard the singular equivalence class.
    if (leaders[i] == i && i != ci)
      ++affine_dim;
  }
  return affine_dim;
}

template <typename T>
void
Octagonal_Shape<T>::concatenate_assign(const Octagonal_Shape& y) {
  // If `y' is an empty 0-dim space octagon, let `*this' become empty.
  // If `y' is an universal 0-dim space octagon, we simply return.
  if (y.space_dim == 0) {
    if (y.marked_empty())
      set_empty();
    return;
  }

  // If `*this' is an empty 0-dim space octagon, then it is sufficient
  // to adjust the dimension of the vector space.
  if (space_dim == 0 && marked_empty()) {
      add_space_dimensions_and_embed(y.space_dim);
      assert(OK());
    return;
  }

  // This is the old number of rows in the matrix. It is equal to
  // the first index of columns to change.
  dimension_type onr = matrix.num_rows();
  // First we increase the space dimension of `*this' by adding
  // `y.space_dimension()' new dimensions.
  // The matrix for the new octagon is obtained
  // by leaving the old system of constraints in the upper left-hand side
  // (where they are at the present) and placing the constraints of `y' in the
  // lower right-hand side.
  add_space_dimensions_and_embed(y.space_dim);
  typename OR_Matrix<N>::const_element_iterator y_it = y.matrix.element_begin();
  for(typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + onr,
	iend = matrix.row_end(); i != iend; ++i) {
    typename OR_Matrix<N>::row_reference_type r = *i;
    dimension_type rs_i = i.row_size();
    for (dimension_type j = onr; j < rs_i; ++j) {
      r[j] = *y_it;
      ++y_it;
    }
  }

  // The concatenation don't preserve the closure.
  if (marked_strongly_closed())
    status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
bool
Octagonal_Shape<T>::contains(const Octagonal_Shape& y) const {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("contains(y)", y);

  // The zero-dimensional universe octagon contains any other
  // dimension-compatible octagon.
  // The zero-dimensional empty octagon only contains another
  // zero-dimensional empty octagon.
  if (space_dim == 0) {
    if (!marked_empty())
      return true;
    else
      return y.marked_empty();
  }

  // `y' needs to be transitively closed.
  y.strong_closure_assign();
  // An empty octagon is in any other dimension-compatible octagons.
  if (y.marked_empty())
    return true;

  // `*this' contains `y' if and only if every element of `*this'
  // is greater than or equal to the correspondent one of `y'.
  for (typename OR_Matrix<N>::const_element_iterator
	 i = matrix.element_begin(), j = y.matrix.element_begin(),
	 iend = matrix.element_end(); i != iend; ++i, ++j)
    if (*i < *j)
      return false;
  return true;
}

template <typename T>
bool
Octagonal_Shape<T>::is_universe() const {
  // An empty octagon isn't, of course, universe.
  if (marked_empty())
    return false;

  // If the octagon is non-empty and zero-dimensional,
  // then it is necessarily the universe octagon.
  if (space_dim == 0)
    return true;

  // An universe octagon can only contains trivial  constraints.
  for (typename OR_Matrix<N>::const_element_iterator
	 i = matrix.element_begin(),
	 iend = matrix.element_end(); i != iend; ++i)
    if (!is_plus_infinity(*i))
      return false;

  return true;
}

template <typename T>
bool
Octagonal_Shape<T>::is_strong_coherent() const {
  // This method is only used by method OK() so as to check if a
  // strongly closed matrix is also strong-coherent, as it must be.
  dimension_type num_rows = matrix.num_rows();

  // The strong-coherence is: for every indexes i and j (and i != j)
  // matrix[i][j] <= (matrix[i][ci] + matrix[cj][j])/2
  // where ci = i + 1, if i is even number or
  //       ci = i - 1, if i is odd.
  // Ditto for cj.
  for (dimension_type i = 0; i < num_rows; ++i) {
    dimension_type ci = coherent_index(i);
    typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + i;
    typename OR_Matrix<N>::const_row_reference_type m_i = *iter;
    const N& m_i_ci = m_i[ci];
    const dimension_type rs_i = matrix.row_size(i);
    for (dimension_type j = 0; j < rs_i; ++j)
      // Note: on the main diagonal only PLUS_INFINITY can occur.
      if (i != j){
	dimension_type cj = coherent_index(j);
	const N& m_cj_j = matrix[cj][j];
	if (!is_plus_infinity(m_i_ci) &&
	    !is_plus_infinity(m_cj_j)) {
	  N d;
	  // Compute (m_i_ci + m_cj_j)/2 into `d', rounding the result
	  // towards plus infinity.
	  add_assign_r(d, m_i_ci, m_cj_j, ROUND_UP);
	  div2exp_assign_r(d, d, 1, ROUND_UP);
	  if (m_i[j] > d)
	    return false;
	}
      }
  }
  return true;
}

template <typename T>
bool
Octagonal_Shape<T>::is_strongly_reduced() const {
  // An empty octagon is already transitively reduced.
  if (marked_empty())
    return true;

  // CHECK ME: is this copy wanted?
  Octagonal_Shape x = *this;
  // An octagon is reduced if removing any constraint,
  // the obtained octagon is different from previous one.
  for (typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin(),
	 iend = matrix.row_end(); iter != iend; ++iter) {
    typename OR_Matrix<N>::const_row_reference_type m_i = *iter;
    dimension_type rs_i = iter.row_size();
    dimension_type i = iter.index();
    for (dimension_type j = 0; j < rs_i; ++j) {
      if (!is_plus_infinity(m_i[j])) {
	Octagonal_Shape x_copy = *this;
	x_copy.matrix[i][j] = PLUS_INFINITY;
	if (x_copy == x)
	  return false;
      }
    }
  }
  // The octagon is just reduced.
  return true;
}

template <typename T>
Poly_Con_Relation
Octagonal_Shape<T>::relation_with(const Constraint& c) const {
  using Implementation::BD_Shapes::div_round_up;

  dimension_type c_space_dim = c.space_dimension();

  // Dimension-compatibility check.
  if (c_space_dim > space_dim)
    throw_dimension_incompatible("relation_with(c)", c);

  // The closure needs to make explicit the implicit constraints.
  strong_closure_assign();

  if (marked_empty())
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0) {
    // Trivially false zero-dimensional constraint.
    if ((c.is_equality() && c.inhomogeneous_term() != 0)
	|| (c.is_inequality() && c.inhomogeneous_term() < 0))
      return Poly_Con_Relation::is_disjoint();
    else if (c.is_strict_inequality() && c.inhomogeneous_term() == 0)
      // The constraint 0 > 0 implicitly defines the hyperplane 0 = 0;
      // thus, the zero-dimensional point also saturates it.
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_disjoint();

    // Trivially true zero-dimensional constraint.
    else if (c.is_equality() || c.inhomogeneous_term() == 0)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else
      // The zero-dimensional point saturates
      // neither the positivity constraint 1 >= 0,
      // nor the strict positivity constraint 1 > 0.
      return Poly_Con_Relation::is_included();
  }

  dimension_type num_vars = 0;
  dimension_type i = 0;
  dimension_type j = 0;
  Coefficient coeff;
  Coefficient term = c.inhomogeneous_term();
  // Constraints that are not octagonal differences are ignored.
  if (!extract_octagonal_difference(c, c_space_dim, num_vars,
				    i, j, coeff, term))
    throw_constraint_incompatible("relation_with(c)");

  if (num_vars == 0) {
    // Dealing with a trivial constraint.
    switch (sgn(c.inhomogeneous_term())) {
    case -1:
      return Poly_Con_Relation::is_disjoint();
    case 0:
      if (c.is_strict_inequality())
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_disjoint();
      else
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_included();
    case 1:
      return Poly_Con_Relation::is_included();
    }
  }

  // Select the cell to be checked for the "<=" part of constraint.
  typename OR_Matrix<N>::const_row_iterator k = matrix.row_begin() + i;
  typename OR_Matrix<N>::const_row_reference_type r = *k;
  const N& r_j = r[j];
  // Set `coeff' to the absolute value of itself.
  if (coeff < 0)
    coeff = -coeff;

  // Compute the bound for `r_j', rounding the result towards plus infinity.
  N d;
  div_round_up(d, term, coeff);
  N d1;
  div_round_up(d1, -term, coeff);

  // Select the cell to be checked for the ">=" part of constraint.
  // Select the right row of the cell.
  if (i%2 == 0)
    ++k;
  else
    --k;
    typename OR_Matrix<N>::const_row_reference_type r1 = *k;
  // Select the right column of the cell.
  dimension_type h = coherent_index(j);
  const N& r1_h = r1[h];

  switch (c.type()) {
  case Constraint::EQUALITY:
    if (d == r_j && d1 == r1_h)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else if (d > r_j && d1 < r1_h)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  case Constraint::NONSTRICT_INEQUALITY:
    if (d >= r_j && d1 >= r1_h)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else if (d >= r_j)
      return Poly_Con_Relation::is_included();
    else if (d < r_j && d1 > r1_h)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  case Constraint::STRICT_INEQUALITY:
    if (d >= r_j && d1 >= r1_h)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_disjoint();
    else if (d > r_j)
      return Poly_Con_Relation::is_included();
    else if (d <= r_j && d1 >= r1_h)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  }
  // Quiet a compiler warning: this program point is unreachable.
  throw std::runtime_error("PPL internal error");
}

#if 0
template <typename T>
Poly_Con_Relation
Octagonal_Shape<T>::relation_with(const Constraint& c) const {
  using namespace IO_Operators;
  C_Polyhedron ph(constraints());
  Poly_Con_Relation p_ret = ph.relation_with(c);
  Poly_Con_Relation o_ret = this->real_relation_with(c);
  if (p_ret != o_ret) {
    std::cout << "Relation of" <<std::endl
	      << *this << std::endl
	      << "a.k.a." << std::endl
	      << ph << std::endl
	      << "with" << std::endl
	      << c << std::endl
	      << "gives " << o_ret << " with Octagonal_Shape" << std::endl
	      << "and " << p_ret << " with C_Polyhedron" << std::endl;
  }
  return o_ret;
}
#endif

template <typename T>
Poly_Gen_Relation
Octagonal_Shape<T>::relation_with(const Generator& g) const {
  dimension_type g_space_dim = g.space_dimension();

  // Dimension-compatibility check.
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("relation_with(g)", g);

  // The empty octagon cannot subsume a generator.
  if (marked_empty())
    return Poly_Gen_Relation::nothing();

  // A universe octagon in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();


  const bool is_line = g.is_line();

  // The relation between the octagon and the given generator is obtained
  // checking if the generator satisfies all the constraints in the octagon.
  // To check if the generator satisfies all the constraints it's enough
  // studying the sign of the scalar product between the generator and
  // all the contraints in the octagon.

  // We find in `*this' all the constraints.
  for (typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin(),
	 i_end = matrix.row_end(); i_iter != i_end; i_iter += 2) {
    dimension_type i = i_iter.index();
    typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
    typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter+1);
    const N& c_i_ii = r_i[i+1];
    const N& c_ii_i = r_ii[i];
    // We have the unary constraints.
    const Variable x(i/2);
    const bool dimension_incompatible = x.space_dimension() > g_space_dim;
    N negated_c_i_ii;
    const bool is_unary_equality
      = neg_assign_r(negated_c_i_ii, c_i_ii, ROUND_NOT_NEEDED) == V_EQ
      && negated_c_i_ii == c_ii_i;
    if (is_unary_equality) {
      // The constraint has form ax = b.
      // To satisfy the constraint it's necessary that the scalar product
      // is not zero.It happens when the coefficient of the variable 'x'
      // in the generator is not zero, because the scalar
      // product has the form:
      // 'a * x_i' where x_i = g.coefficient(x).
      if (!dimension_incompatible && g.coefficient(x) != 0)
	return Poly_Gen_Relation::nothing();
    }
    // We have 0, 1 or 2 inequality constraints.
    else {
      if (!is_plus_infinity(c_i_ii)) {
	// The constraint has form -ax <= b.
	// If the generator is a line it's necessary to check if
	// the scalar product is not zero.
	if (is_line && (!dimension_incompatible && g.coefficient(x) != 0))
	  return Poly_Gen_Relation::nothing();
	else
	  // If the generator is not a line it's necessary to check
	  // that the scalar product sign is not negative and
	  // it happens when the coefficient of the variable 'x'
	  // in the generator is not negative, because the scalar
	  // product has the form:
	  // 'a * x_i' where x_i = g.coefficient(x).
	  if (g.coefficient(x) < 0)
	    return Poly_Gen_Relation::nothing();
      }
      if (!is_plus_infinity(c_ii_i)) {
	// The constraint has form ax <= b.
	if (is_line && (!dimension_incompatible && g.coefficient(x) != 0))
	  return Poly_Gen_Relation::nothing();
	else
	  // If the generator is not a line it's necessary to check
	  // that the scalar product sign is not negative and
	  // it happens when the coefficient of the variable 'x'
	  // in the generator is not positive, because the scalar
	  // product has the form:
	  // '-a * x_i' where x_i = g.coefficient(x).
	  if (g.coefficient(x) > 0)
	    return Poly_Gen_Relation::nothing();
      }
    }
  }

  // We have the binary constraints.
  for (typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin(),
	 i_end = matrix.row_end(); i_iter != i_end; i_iter += 2) {
    dimension_type i = i_iter.index();
    typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
    typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter+1);
    for (dimension_type j = 0; j < i; j += 2) {
      const N& c_i_j = r_i[j];
      const N& c_ii_jj = r_ii[j+1];
      const N& c_ii_j = r_ii[j];
      const N& c_i_jj = r_i[j+1];
      const Variable x(j/2);
      const Variable y(i/2);
      const bool x_dimension_incompatible = x.space_dimension() > g_space_dim;
      const bool y_dimension_incompatible = y.space_dimension() > g_space_dim;
      const bool is_trivial_zero = (x_dimension_incompatible
				    && g.coefficient(y) == 0)
	|| (y_dimension_incompatible && g.coefficient(x) == 0)
	|| (x_dimension_incompatible && y_dimension_incompatible);
      // FIXME! Find better names.
      N negated_c_ii_jj;
      const bool is_binary_equality
	= neg_assign_r(negated_c_ii_jj, c_ii_jj, ROUND_NOT_NEEDED) == V_EQ
	&& negated_c_ii_jj == c_i_j;
      N negated_c_i_jj;
      const bool is_a_binary_equality
	= neg_assign_r(negated_c_i_jj, c_i_jj, ROUND_NOT_NEEDED) == V_EQ
	&& negated_c_i_jj == c_ii_j;

      Coefficient g_coefficient_y;

      if (is_binary_equality || is_a_binary_equality) {
	// The scalar product has the form
	// 'a * y_i - a * x_j' (or  'a * x_j + a * y_i')
	// where y_i is equal to g.coefficient(y) or -g.coefficient(y)
	// and x_j = g.coefficient(x).
	// It is not zero when both the coefficients of the
	// variables x and y are not zero or when these coefficients
	// are not equals.
	if (is_a_binary_equality)
	  // The constraint has form ax + ay = b.
	  g_coefficient_y = -g.coefficient(y);
	else
	  // The constraint has form ax - ay = b.
 	  g_coefficient_y = g.coefficient(y);
	if (!is_trivial_zero && g.coefficient(x) != g_coefficient_y)
 	  return Poly_Gen_Relation::nothing();
      }
      else
	if (!is_plus_infinity(c_i_j) || !is_plus_infinity(c_ii_j)) {
	  if (!is_plus_infinity(c_ii_j))
	    // The constraint has form ax + ay <= b.
	    g_coefficient_y = -g.coefficient(y);
	  else
	    // The constraint has form ax - ay <= b.
	    g_coefficient_y = g.coefficient(y);
	  // The scalar product has the form
	  // '-a * x_j + a* y_i' (or '-a * x_j - a * y_i').
	  if (is_line
	      && !is_trivial_zero
	      && g.coefficient(x) != g_coefficient_y)
	    return Poly_Gen_Relation::nothing();
	  // The product scalar sign is not negative when the
	  // coefficient of the variable y is strictly smaller
	  // than the coefficient of the variable x in the generator.
	  else if (g_coefficient_y < g.coefficient(x))
	    return Poly_Gen_Relation::nothing();
	}
	else
	  if (!is_plus_infinity(c_ii_jj) || !is_plus_infinity(c_i_jj)) {
	    if (!is_plus_infinity(c_i_jj))
	      // The constraint has form -ax - ay <= b.
	      g_coefficient_y = -g.coefficient(y);
	    else
	      // The constraint has form ay - ax <= b.
	      g_coefficient_y = g.coefficient(y);
	    // The scalar product has the form
	    // 'a * x_j - a* y_i' (or 'a * x_j + a* y_i').
	    if (is_line
		&& !is_trivial_zero
		&& g.coefficient(x) != g_coefficient_y)
	      return Poly_Gen_Relation::nothing();
	    // The product scalar sign is not negative when the
	    // coefficient of the variable x is strictly smaller
	    // than the coefficient of the variable y in the generator.
	    else if (g.coefficient(x) < g_coefficient_y)
	      return Poly_Gen_Relation::nothing();
	  }
    }
  }
  // If this point is reached the constraint 'g' satisfies
  // all the constraints in the octagon.
  return Poly_Gen_Relation::subsumes();
}

template <typename T>
void
Octagonal_Shape<T>::strong_closure_assign() const {
  using Implementation::BD_Shapes::min_assign;

  // Do something only if necessary (zero-dim implies strong closure).
  if (marked_empty() || marked_strongly_closed() || space_dim == 0)
    return;

  // Even though the octagon will not change, its internal representation
  // is going to be modified by the closure algorithm.
  Octagonal_Shape& x = const_cast<Octagonal_Shape<T>&>(*this);

  // Use these type aliases for short.
  typedef typename OR_Matrix<N>::row_iterator Row_Iterator;
  typedef typename OR_Matrix<N>::row_reference_type Row_Reference;
  // Avoid recomputations.
  const dimension_type n_rows = x.matrix.num_rows();
  const Row_Iterator m_begin = x.matrix.row_begin();
  const Row_Iterator m_end = x.matrix.row_end();

  // Fill the main diagonal with zeros.
  for (Row_Iterator i = m_begin; i != m_end; ++i) {
    assert(is_plus_infinity((*i)[i.index()]));
    assign_r((*i)[i.index()], 0, ROUND_NOT_NEEDED);
  }

  // This algorithm is given by two steps: the first one is a simple
  // adaptation of the `shortest-path closure' using the Floyd-Warshall
  // algorithm; the second one is the `strong-coherence' algorithm.
  // It is important to note that after the strong-coherence,
  // the octagon is still shortest-path closed and hence, strongly closed.

  // Recall that, given an index `h', we indicate with `ch' the coherent
  // index, i.e., the index such that:
  //   ch = h + 1, if h is an even number;
  //   ch = h - 1, if h is an odd number.

  // Step 1: closure.
  for (Row_Iterator k_iter = m_begin; k_iter != m_end; ++k_iter) {
    const dimension_type k = k_iter.index();
    const dimension_type ck = coherent_index(k);
    const dimension_type rs_k = k_iter.row_size();
    Row_Reference x_k = *k_iter;
    Row_Iterator ck_iter = (k%2) ? k_iter-1 : k_iter+1;
    Row_Reference x_ck = *ck_iter;

    for (Row_Iterator i_iter = m_begin; i_iter != m_end; ++i_iter) {
      const dimension_type i = i_iter.index();
      const dimension_type ci = coherent_index(i);
      const dimension_type rs_i = i_iter.row_size();
      Row_Reference x_i = *i_iter;
      const N& x_i_k = (k < rs_i) ? x_i[k] : x_ck[ci];
      if (!is_plus_infinity(x_i_k)) {
	Row_Reference x_ci = *((i%2) ? i_iter-1 : i_iter+1);

//      UNOPTIMIZED VERSION OF THE INNER LOOP
//      The conditional expressions in the following inner loop
//      are optimized away by splitting it into three loops.
// 	for (dimension_type j = 0; j < n_rows; ++j) {
// 	  Row_Reference x_cj = *(m_begin + coherent_index(j));
// 	  const N& x_k_j = (j < rs_k) ? x_k[j] : x_cj[ck];
// 	  if (!is_plus_infinity(x_k_j)) {
// 	    N& x_i_j = (j < rs_i) ? x_i[j] : x_cj[ci];
// 	    N sum;
// 	    add_assign_r(sum, x_i_k, x_k_j, ROUND_UP);
// 	    min_assign(x_i_j, sum);
// 	  }
// 	}

	// OPTIMIZED VERSION OF THE INNER LOOP STARTS HERE.
	// The inner loop is divided in four loops, based on the relation
	// between the three indices `k', `i', and `j'.
	const dimension_type min_rs = std::min(rs_i, rs_k);
	const dimension_type max_rs = std::max(rs_i, rs_k);
	// The first part of the optimized inner loop.
	for (dimension_type j = 0; j < min_rs; ++j) {
	  const N& x_k_j = x_k[j];
	  if (!is_plus_infinity(x_k_j)) {
	    N sum;
	    add_assign_r(sum, x_i_k, x_k_j, ROUND_UP);
	    min_assign(x_i[j], sum);
	  }
	}
	// The second part of the optimized inner loop.
        // NOTE: the following two are mutually exclusive loops.
	if (rs_i == min_rs) {
	  // Note: both `rs_i' and `rs_k' are even numbers;
	  // unrolling two iterations so as to optimize away
	  // the computation of coherent indexes.
	  for (dimension_type j = rs_i; j < rs_k; j += 2) {
	    // The loop body for index `j'.
	    const N& x_k_j = x_k[j];
	    if (!is_plus_infinity(x_k_j)) {
	      N sum;
	      add_assign_r(sum, x_i_k, x_k_j, ROUND_UP);
	      assert(coherent_index(j) == j+1);
	      min_assign(x.matrix[j+1][ci], sum);
	    }
	    // The loop body for index `j+1'.
	    const N& x_k_j1 = x_k[j+1];
	    if (!is_plus_infinity(x_k_j1)) {
	      N sum;
	      add_assign_r(sum, x_i_k, x_k_j1, ROUND_UP);
	      assert(coherent_index(j+1) == j);
	      min_assign(x.matrix[j][ci], sum);
	    }
	  }
	}
	else {
	  // Note: both `rs_i' and `rs_k' are even numbers;
	  // unrolling two iterations so as to optimize away
	  // the computation of coherent indexes.
	  for (dimension_type j = rs_k; j < rs_i; j += 2) {
	    // The loop body for index `j'.
	    assert(coherent_index(j) == j+1);
	    const N& x_k_j = x.matrix[j+1][ck];
	    if (!is_plus_infinity(x_k_j)) {
	      N sum;
	      add_assign_r(sum, x_i_k, x_k_j, ROUND_UP);
	      min_assign(x_i[j], sum);
	    }
	    // The loop body for index `j+1'.
	    assert(coherent_index(j+1) == j);
	    const N& x_k_j1 = x.matrix[j][ck];
	    if (!is_plus_infinity(x_k_j1)) {
	      N sum;
	      add_assign_r(sum, x_i_k, x_k_j1, ROUND_UP);
	      min_assign(x_i[j+1], sum);
	    }
	  }
	}
	// The third part of the optimized inner loop.
	// Note: both `max_rs' and `n_rows' are even numbers;
	// unrolling two iterations so as to optimize away
	// the computation of coherent indexes.
	for (dimension_type j = max_rs; j < n_rows; j += 2) {
	  // The loop body for index `j'.
	  assert(coherent_index(j) == j+1);
	  Row_Iterator x_cj_iter = m_begin + (j+1);
	  Row_Reference x_cj = *x_cj_iter;
	  const N& x_k_j = x_cj[ck];
	  if (!is_plus_infinity(x_k_j)) {
	    N sum;
	    add_assign_r(sum, x_i_k, x_k_j, ROUND_UP);
	    min_assign(x_cj[ci], sum);
	  }
	  // The loop body for index `j+1'.
	  assert(coherent_index(j+1) == j);
	  Row_Reference x_cj1 = *(--x_cj_iter);
	  const N& x_k_j1 = x_cj1[ck];
	  if (!is_plus_infinity(x_k_j1)) {
	    N sum;
	    add_assign_r(sum, x_i_k, x_k_j1, ROUND_UP);
	    min_assign(x_cj1[ci], sum);
	  }
	}
	// OPTIMIZED VERSION OF INNER LOOP ENDS HERE.

      }
    }
  }

  // Check for emptyness: the octagon is empty if and only if there is a
  // negative value in the main diagonal.
  for (Row_Iterator i = m_begin; i != m_end; ++i) {
    N& x_i_i = (*i)[i.index()];
    if (x_i_i < 0) {
      x.status.set_empty();
      return;
    }
    else {
      assert(x_i_i == 0);
      // Restore PLUS_INFINITY on the main diagonal.
      x_i_i = PLUS_INFINITY;
    }
  }

  // Step 2: we enforce the strong coherence.
  // The strong-coherence is: for every indexes i and j
  // m_i_j <= (m_i_ci + m_cj_j)/2
  // where ci = i + 1, if i is even number or
  //       ci = i - 1, if i is odd.
  // Ditto for cj.
  for (Row_Iterator i_iter = m_begin; i_iter != m_end; ++i_iter) {
    Row_Reference x_i = *i_iter;
    const dimension_type i = i_iter.index();
    const N& x_i_ci = x_i[coherent_index(i)];
    // Avoid to do unnecessary sums.
    if (!is_plus_infinity(x_i_ci))
      for (dimension_type j = 0, rs_i = i_iter.row_size(); j < rs_i; ++j)
	if (i != j) {
	  const N& x_cj_j = x.matrix[coherent_index(j)][j];
	  if (!is_plus_infinity(x_cj_j)) {
	    N semi_sum;
	    add_assign_r(semi_sum, x_i_ci, x_cj_j, ROUND_UP);
	    div2exp_assign_r(semi_sum, semi_sum, 1, ROUND_UP);
	    min_assign(x_i[j], semi_sum);
	  }
	}
  }

  // The octagon is not empty and it is now strongly closed.
  x.status.set_strongly_closed();
}

template <typename T>
void
Octagonal_Shape<T>::incremental_strong_closure_assign(Variable var) const {
  using Implementation::BD_Shapes::min_assign;

  // `var' should be one of the dimensions of the octagon.
  dimension_type num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("incremental_strong_closure_assign(v)",
				 var.id());

  // Do something only if necessary.
  if (marked_empty() || marked_strongly_closed())
    return;

  // Zero-dimensional octagons are necessarily strongly closed.
  if (space_dim == 0)
    return;

  Octagonal_Shape& x = const_cast<Octagonal_Shape<T>&>(*this);

  // Fill the main diagonal with zeros.
  for (typename OR_Matrix<N>::row_iterator i = x.matrix.row_begin(),
	 m_end = x.matrix.row_end(); i != m_end; ++i) {
    typename OR_Matrix<N>::row_reference_type r = *i;
    assert(is_plus_infinity(r[i.index()]));
    assign_r(r[i.index()], 0, ROUND_NOT_NEEDED);
    //    r[i.index()] = 0;
  }

  // This algorithm uses the incremental Floyd-Warshall algorithm.
  // It is constituted by two steps: first we modify all
  // constraints on variable `var'. Infact,
  // the constraints on variable v are changed, and it is possible
  // that these constraints aren't tightest anymore; then
  // second we change also the other constraints.

  // Step 1: Modify all constraints on variable `var'.
  // Rule: TRANSITIVITY. Here we use `v' to indicate one of
  // the indeces of variable `var', the other index is indicated with `cv'
  const dimension_type v = 2*var.id();
  const dimension_type cv = v+1;

  typename OR_Matrix<N>::row_iterator v_iter = x.matrix.row_begin() + v;
  typename OR_Matrix<N>::row_iterator cv_iter = v_iter+1;
  typename OR_Matrix<N>::row_reference_type m_v = *v_iter;
  typename OR_Matrix<N>::row_reference_type m_cv = *cv_iter;

  dimension_type rs_v = v_iter.row_size();
  const dimension_type n_rows = 2*space_dim;

  for (typename OR_Matrix<N>::row_iterator k_iter = x.matrix.row_begin(),
	 k_end = x.matrix.row_end(); k_iter != k_end; ++k_iter) {
    dimension_type k = k_iter.index();
    const dimension_type rs_k = k_iter.row_size();
    typename OR_Matrix<N>::row_reference_type m_k = *k_iter;
    typename OR_Matrix<N>::row_reference_type m_ck = (k%2) ? *(k_iter-1) : *(k_iter+1);

    for (typename OR_Matrix<N>::row_iterator i_iter = x.matrix.row_begin(),
	   i_end = x.matrix.row_end(); i_iter != i_end; ++i_iter) {
      dimension_type i = i_iter.index();
      dimension_type rs_i = i_iter.row_size();
      typename OR_Matrix<N>::row_reference_type m_i = *i_iter;
      typename OR_Matrix<N>::row_reference_type m_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
      const N& m_i_k = (k < rs_i) ? m_i[k] : m_ck[coherent_index(i)];
      // We adjust the columns on variable 'var'.
      if (!is_plus_infinity(m_i_k)) {
	const N& m_k_v = (v < rs_k) ? m_k[v] : m_cv[coherent_index(k)];
	if (!is_plus_infinity(m_k_v)) {
	  N& m_i_v = (v < rs_i) ? m_i[v] : m_cv[coherent_index(i)];
	  N sum1;
	  add_assign_r(sum1, m_i_k, m_k_v, ROUND_UP);
	  min_assign(m_i_v, sum1);
	}

	const N& m_k_cv = (cv < rs_k) ? m_k[cv] : m_v[coherent_index(k)];
	if (!is_plus_infinity(m_k_cv)) {
	  N& m_i_cv = (cv < rs_i) ? m_i[cv] : m_v[coherent_index(i)];
	  N sum2;
	  add_assign_r(sum2, m_i_k, m_k_cv, ROUND_UP);
	  min_assign(m_i_cv, sum2);
	}
      }

      // Then we adjust the rights on variable 'var'.
      const N& m_k_i = (i < rs_k) ? m_k[i] : m_ci[coherent_index(k)];
      if (!is_plus_infinity(m_k_i)) {
	const N& m_v_k = (k < rs_v) ? m_v[k] : m_ck[cv];
	if (!is_plus_infinity(m_v_k)) {
	  N& m_v_i = (i < rs_v) ? m_v[i] : m_ci[cv];
	  N sum3;
	  add_assign_r(sum3, m_v_k, m_k_i, ROUND_UP);
	  min_assign(m_v_i, sum3);
	}

	const N& m_cv_k = (k < rs_v) ? m_cv[k] : m_ck[v];
	if (!is_plus_infinity(m_cv_k)) {
	  N& m_cv_i = (i < rs_v) ? m_cv[i] : m_ci[v];
	  N sum4;
	  add_assign_r(sum4, m_cv_k, m_k_i, ROUND_UP);
	  min_assign(m_cv_i, sum4);
	}
      }
    }
  }

  // Step 2: finally we find the tighter constraints also
  // for the other variable using the right value of `var'.
  for (typename OR_Matrix<N>::row_iterator i_iter = x.matrix.row_begin(),
	 i_end = x.matrix.row_end(); i_iter != i_end; ++i_iter) {
    dimension_type i = i_iter.index();
    dimension_type rs_i = i_iter.row_size();
    typename OR_Matrix<N>::row_reference_type m_i = *i_iter;
    typename OR_Matrix<N>::row_reference_type m_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
    const N& m_i_v = (v < rs_i) ? m_i[v] : m_cv[coherent_index(i)];
    for (dimension_type j = 0; j < n_rows; ++j) {
      dimension_type cj = coherent_index(j);
      typename OR_Matrix<N>::row_reference_type m_cj = *(x.matrix.row_begin()+cj);
      N& m_i_j = (j < rs_i) ? m_i[j] : m_cj[coherent_index(i)];
      if (!is_plus_infinity(m_i_v)) {
	const N& m_v_j = (j < rs_v) ? m_v[j] : m_cj[cv];
	if (!is_plus_infinity(m_v_j)) {
	  N sum1;
	  add_assign_r(sum1, m_i_v, m_v_j, ROUND_UP);
	  min_assign(m_i_j, sum1);
	}
      }
      const N& m_i_cv = (cv < rs_i) ? m_i[cv] : m_v[coherent_index(i)];
      if (!is_plus_infinity(m_i_cv)) {
	const N& m_cv_j = (j < rs_v) ? m_cv[j] : m_cj[v];
	if (!is_plus_infinity(m_cv_j)) {
	  N sum2;
	  add_assign_r(sum2, m_i_cv, m_cv_j, ROUND_UP);
	  min_assign(m_i_j, sum2);
	}
      }
    }
  }

  // Check for emptyness:the octagon is empty if and only if there is a
  // negative value in the main diagonal.
  for (dimension_type i = n_rows; i-- > 0; ) {
    N& x_i_i = x.matrix[i][i];
    if (x_i_i < 0) {
      x.status.set_empty();
      return;
    }
    else {
      // Restore PLUS_INFINITY on the main diagonal.
      assert(x_i_i == 0);
      x_i_i = PLUS_INFINITY;
    }
  }

  // The octagon is not empty and it is now strongly closed.
  x.status.set_strongly_closed();

  // Step 2: we enforce the strong coherence.
  // The strong-coherence is: for every indexes i and j
  // m_i_j <= (m_i_ci + m_cj_j)/2
  // where ci = i + 1, if i is even number or
  //       ci = i - 1, if i is odd.
  // Ditto for cj.
  for (typename OR_Matrix<N>::row_iterator i_iter = x.matrix.row_begin(),
	 i_end = x.matrix.row_end(); i_iter != i_end; ++i_iter) {
    typename OR_Matrix<N>::row_reference_type x_i = *i_iter;
    dimension_type rs_i = i_iter.row_size();
    dimension_type i = i_iter.index();
    dimension_type ci = coherent_index(i);
    N& x_i_ci = x_i[ci];
    // Avoid to do unnecessary sums.
    if (!is_plus_infinity(x_i_ci))
      for (dimension_type j = 0; j < rs_i; ++j) {
	if (i != j) {
	  dimension_type cj = coherent_index(j);
	  N& x_cj_j = x.matrix[cj][j];
	  if (!is_plus_infinity(x_cj_j)) {
	    N& x_i_j = x_i[j];
	    N sum;
	    add_assign_r(sum, x_i_ci, x_cj_j, ROUND_UP);
	    N d;
	    div2exp_assign_r(d, sum, 1, ROUND_UP);
	    min_assign(x_i_j, d);
	  }
	}
      }
  }

  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::compute_successors(std::vector<dimension_type>& successor) const {
  assert(!marked_empty() && marked_strongly_closed());
  assert(successor.size() == 0);
  // Variables are ordered according to their index.
  // The vector `successor' is used to indicate which variable
  // immediately follows a given one in the corresponding equivalence class.
  const dimension_type successor_size = matrix.num_rows();
  // Initially, each variable is successor of its own zero-equivalence class.
  successor.reserve(successor_size);
  for (dimension_type i = 0; i < successor_size; ++i)
    successor.push_back(i);
  // Now compute actual successors.
  for (dimension_type i = successor_size; i-- > 0; )  {
    typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin()+i;
    typename OR_Matrix<N>::const_row_reference_type m_i = *i_iter;
    typename OR_Matrix<N>::const_row_reference_type m_ci = (i%2) ?
          *(i_iter-1) : *(i_iter+1);
    for (dimension_type j = 0; j < i; ++j) {
    //for (dimension_type j = i; j-- > 0; ) {
      dimension_type cj = coherent_index(j);
      N neg_m_ci_cj;
      if (neg_assign_r(neg_m_ci_cj, m_ci[cj], ROUND_NOT_NEEDED) == V_EQ
	      && neg_m_ci_cj == m_i[j]) {
        // Choose as successor the variable having the greaterer index.
        successor[j] = i;
      }
    }
  }
}

template <typename T>
void
Octagonal_Shape<T>
::compute_leaders(std::vector<dimension_type>& leaders) const {
  assert(!marked_empty() && marked_strongly_closed());
  assert(leaders.size() == 0);
  // Variables are ordered according to their index.
  // The vector `leaders' is used to indicate the smallest variable
  // that belongs to the corresponding equivalence class.
  const dimension_type leader_size = matrix.num_rows();
  // Initially, each variable is leader of its own zero-equivalence class.
  leaders.reserve(leader_size);
  for (dimension_type i = 0; i < leader_size; ++i)
    leaders.push_back(i);
  // Now compute actual leaders.
  for (typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin(),
	 iend = matrix.row_end(); i_iter != iend; ++i_iter) {
    typename OR_Matrix<N>::const_row_reference_type m_i = *i_iter;
    dimension_type i = i_iter.index();
    typename OR_Matrix<N>::const_row_reference_type m_ci =
      (i%2) ? *(i_iter-1) : *(i_iter+1);
    for (dimension_type j = 0; j < i; ++j) {
      dimension_type cj = coherent_index(j);
      N neg_m_ci_cj;
      if (neg_assign_r(neg_m_ci_cj, m_ci[cj], ROUND_NOT_NEEDED) == V_EQ
	      && neg_m_ci_cj == m_i[j]) {
        // Choose as leader the variable having the smaller index.
	leaders[i] = leaders[j];
      }
    }
  }
}

template <typename T>
void
Octagonal_Shape<T>
::compute_leaders(std::vector<dimension_type>& successor,
		  std::vector<dimension_type>& no_sing_leaders,
		  bool& exist_sing_class,
		  dimension_type& sing_leader) const {
  assert(!marked_empty() && marked_strongly_closed());
  assert(no_sing_leaders.size() == 0);
  dimension_type successor_size = successor.size();
  std::deque<bool> dealt_with(successor_size, false);
  for (dimension_type i = 0; i < successor_size; ++i) {
    dimension_type nxt_i = successor[i];
    if (!dealt_with[i]) {
      // The index is a leader.
      // Now check if it is a leader of a singular class or not.
      if (nxt_i == coherent_index(i)) {
        exist_sing_class = true;
        sing_leader = i;
      }
      else
        no_sing_leaders.push_back(i);
    }
    // The following index isn't a leader.
    dealt_with[nxt_i] = true;
  }
}

template <typename T>
void
Octagonal_Shape<T>::strong_reduction_assign() const {

  // First find the tightest constraints for this octagon.
  strong_closure_assign();

  // If `*this' is empty, then there is nothing to reduce.
  if (marked_empty())
    return;

  // Step 1: compute zero-equivalence classes.
  // Variables corresponding to indices `i' and `j' are zero-equivalent
  // if they lie on a zero-weight loop; since the matrix is strongly
  // closed, this happens if and only if matrix[i][j] == -matrix[ci][cj].
  std::vector<dimension_type> no_sing_leaders;
  dimension_type sing_leader = 0;
  bool exist_sing_class = false;
  std::vector<dimension_type> successor;
  compute_successors(successor);
  compute_leaders(successor, no_sing_leaders, exist_sing_class, sing_leader);
  const dimension_type num_no_sing_leaders = no_sing_leaders.size();

  Octagonal_Shape<T> aux(space_dim);
  // Step 2: add to auxiliary octagon only no-redundant
  // constraints and construct a 0-cycle using only
  // the leaders of the non-singular classes.
  for (dimension_type li = 0; li < num_no_sing_leaders; ++li) {
    const dimension_type i = no_sing_leaders[li];
    const dimension_type ci = coherent_index(i);
    typename OR_Matrix<N>::const_row_reference_type m_i =
      *(matrix.row_begin()+i);
    typename OR_Matrix<N>::row_reference_type aux_i =
      *(aux.matrix.row_begin()+i);
    if (i%2 == 0) {
      // Each positive equivalence class must have a single 0-cycle
      // connecting all equivalent variables in increasing order.
      // Note: by coherence assumption, the variables in the
      // corresponding negative equivalence class are
      // automatically connected.
      if (i != successor[i]) {
        dimension_type j = i;
        dimension_type nxt_j = successor[j];
        while (j != nxt_j) {
          aux.matrix[nxt_j][j] = matrix[nxt_j][j];
          j = nxt_j;
          nxt_j = successor[j];
        }
        const dimension_type cj = coherent_index(j);
        aux.matrix[cj][ci] = matrix[cj][ci];
      }
    }

    dimension_type rs_li = (li%2) ? li :li+1;
    // Check if the constraint is redundant.
    for (dimension_type lj = 0 ; lj <= rs_li; ++lj) {
      const dimension_type j = no_sing_leaders[lj];
      const dimension_type cj = coherent_index(j);
      const N& m_i_j = m_i[j];
      const N& m_i_ci = m_i[ci];
      bool to_add = true;
      // Control if the constraint is redundant by strong-coherence,
      // that is:
      // m_i_j >= (m_i_ci + m_cj_j)/2,   where j != ci.
      if (j != ci) {
        N d;
        add_assign_r(d, m_i_ci, matrix[cj][j], ROUND_UP);
        div2exp_assign_r(d, d, 1, ROUND_UP);
        if (m_i_j >= d) {
	  to_add = false;
          continue;
	}
      }
      // Control if the constraint is redundant by strong closure, that is
      // if there is a path from i to j (i = i_0, ... , i_n = j), such that
      // m_i_j = sum_{k=0}^{n-1} m_{i_k}_{i_(k+1)}.
      // Since the octagon is already strongly closed, the above relation
      // is reduced to three case, in accordance with k, i, j inter-depend:
      // exit k such that
      // 1.) m_i_j >= m_i_k   + m_cj_ck,   if k < j < i; or
      // 2.) m_i_j >= m_i_k   + m_k,_j,    if j < k < i; or
      // 3.) m_i_j >= m_ck_ci + m_k_j,     if j < i < k.
      // Note: `i > j'.
      for (dimension_type lk = 0; lk < num_no_sing_leaders; ++lk) {
	const dimension_type k = no_sing_leaders[lk];
	if (k != i && k != j) {
	  dimension_type ck = coherent_index(k);
          N c;
          if (k < j)
	    // Case 1.
	    add_assign_r(c, m_i[k], matrix[cj][ck], ROUND_UP);
	  else if (k < i)
	    // Case 2.
	    add_assign_r(c, m_i[k], matrix[k][j], ROUND_UP);
	  else
	    // Case 3.
	    add_assign_r(c, matrix[ck][ci], matrix[k][j], ROUND_UP);

          // Checks if the constraint is redundant.
          if (m_i_j >= c) {
	    to_add = false;
	    break;
	  }
	}
      }

      // The constraint is not redundant.
      if (to_add)
	aux_i[j] = m_i_j;
    }
  }

  // If there exist a singular equivalence class, then it must have a
  // single 0-cycle connecting all the positive and negative equivalent
  // variables.
  // Note: the singular class is not connected with the other classes.
  if (exist_sing_class) {
    aux.matrix[sing_leader][sing_leader+1] = matrix[sing_leader][sing_leader+1];
    if (successor[sing_leader+1] != sing_leader+1) {
      dimension_type j = sing_leader;
      dimension_type nxt_jj = successor[j+1];
      while (nxt_jj != j+1) {
	aux.matrix[nxt_jj][j] = matrix[nxt_jj][j];
	j = nxt_jj;
	nxt_jj = successor[j+1];
      }
      aux.matrix[j+1][j] = matrix[j+1][j];
    }
    else
      aux.matrix[sing_leader+1][sing_leader] = matrix[sing_leader+1][sing_leader];
  }

  Octagonal_Shape<T>& x = const_cast<Octagonal_Shape<T>&>(*this);
  aux.status.reset_strongly_closed();

#ifndef NDEBUG
  {
    // We assume that `aux' is equal to `*this'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = aux;
    assert(x_copy == y_copy);
  }
#endif

  std::swap(x, aux);
  assert(is_strongly_reduced());
}


template <typename T>
void
Octagonal_Shape<T>::oct_hull_assign(const Octagonal_Shape& y) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("oct_hull_assign(y)", y);

  // The hull of an octagon `x' with an empty octagon is `x'.
  y.strong_closure_assign();
  if (y.marked_empty())
    return;
  strong_closure_assign();
  if (marked_empty()) {
    *this = y;
    return;
  }

  // The oct-hull consist to costruct '*this' with the maximum
  // elements selected from '*this' or 'y'.
  typename OR_Matrix<N>::const_element_iterator j = y.matrix.element_begin();
  for (typename OR_Matrix<N>::element_iterator i = matrix.element_begin(),
	 iend = matrix.element_end(); i != iend; ++i, ++j) {
    N& elem = *i;
    const N& y_elem = *j;
    if (elem < y_elem)
      elem = y_elem;
  }
  // The result is still closed.
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::oct_difference_assign(const Octagonal_Shape& y) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("oct_difference_assign(y)", y);

  Octagonal_Shape& x = *this;

  // Being lazy here is only harmful.
  // We close.
  x.strong_closure_assign();
  // The difference of an empty octagon and of an octagon `p' is empty.
  if (x.marked_empty())
    return;
  // The difference of a octagon `p' and an empty octagon is `p'.
  if (y.marked_empty())
    return;

  // If both octagons are zero-dimensional,
  // then at this point they are necessarily universe octagons,
  // so that their difference is empty.
  if (x.space_dim == 0) {
    x.set_empty();
    return;
  }

  // TODO: This is just an executable specification.
  //       Have to find a more efficient method.
  if (y.contains(x)) {
    x.set_empty();
    return;
  }

  Octagonal_Shape new_oct(space_dim, EMPTY);
  // We take a constraint of the octagon y at the time and we
  // consider its complementary. Then we intersect the union
  // of these complementaries with the octagon x.
  const Constraint_System& y_cs = y.constraints();
  for (Constraint_System::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    const Constraint& c = *i;
    // If the octagon `x' is included the octagon defined by `c',
    // then `c' _must_ be skipped, as adding its complement to `x'
    // would result in the empty octagon, and as we would obtain
    // a result that is less precise than the oct_difference.
    if (x.relation_with(c).implies(Poly_Con_Relation::is_included()))
      continue;
    Octagonal_Shape z = x;
    const Linear_Expression e = Linear_Expression(c);
    bool change = false;
    if (c.is_nonstrict_inequality())
      change = z.add_constraint_and_minimize(e <= 0);
    if (c.is_equality()) {
      Octagonal_Shape w = x;
      if (w.add_constraint_and_minimize(e <= 0))
	new_oct.oct_hull_assign(w);
      change = z.add_constraint_and_minimize(e >= 0);
    }
    if (change)
      new_oct.oct_hull_assign(z);
  }
  *this = new_oct;
  // The result is still transitively closed, because both
  // oct_hull_assign() and add_constraint_and_minimize()
  // preserve closure.
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::add_space_dimensions_and_embed(dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  dimension_type new_dim = space_dim + m;
  bool was_zero_dim_univ = (!marked_empty() && space_dim == 0);

  // To embed an n-dimension space octagon in a (n+m)-dimension space,
  // we just add `m' variables in the matrix of constraints.
  matrix.grow(new_dim);
  space_dim = new_dim;
  // If `*this' was the zero-dim space universe octagon,
  // then we can set the strongly closure flag.
  if (was_zero_dim_univ)
    status.set_strongly_closed();

  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::add_space_dimensions_and_project(dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  dimension_type n = matrix.num_rows();

  // To project an n-dimension space OS in a (space_dim+m)-dimension space,
  // we just add `m' columns and rows in the matrix of constraints.
  add_space_dimensions_and_embed(m);
  // We insert 0 where it needs.
  // Attention: now num_rows of matrix is update!
  for (typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n,
	 iend =  matrix.row_end(); i != iend; i += 2) {
    typename OR_Matrix<N>::row_reference_type x_i = *i;
    typename OR_Matrix<N>::row_reference_type x_ci = *(i+1);
    dimension_type ind = i.index();
    assign_r(x_i[ind+1], 0, ROUND_NOT_NEEDED);
    assign_r(x_ci[ind], 0, ROUND_NOT_NEEDED);
  }

  if (marked_strongly_closed())
    status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any octagon is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a octagon in a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  dimension_type max_dim_to_be_removed = to_be_removed.rbegin()->id();
  if (max_dim_to_be_removed >= space_dim)
    throw_dimension_incompatible("remove_space_dimensions(vs)",
				 max_dim_to_be_removed);

  dimension_type new_space_dim = space_dim - to_be_removed.size();

  strong_closure_assign();
  // When removing _all_ dimensions from an octagon,
  // we obtain the zero-dimensional octagon.
  if (new_space_dim == 0) {
    matrix.shrink(0);
    if (!marked_empty())
      // We set the zero_dim_univ flag.
      set_zero_dim_univ();
    space_dim = 0;
    assert(OK());
    return;
  }

  // We consider every variable and we check if it is to be removed.
  // If it is to be removed, we pass to the successive one, elsewhere
  // we move its elements in the right position.
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  dimension_type ftr = tbr->id();
  dimension_type i = ftr + 1;
  dimension_type ftr_size = 2*ftr*(ftr+1);
  typename OR_Matrix<N>::element_iterator iter = matrix.element_begin()+ftr_size;
  while (i < space_dim) {
    if (to_be_removed.count(Variable(i)))
      ++i;
    else {
      typename OR_Matrix<N>::const_row_iterator row_iter = matrix.row_begin()+2*i;
      typename OR_Matrix<N>::const_row_reference_type row_ref = *row_iter;
      typename OR_Matrix<N>::const_row_reference_type row_ref1 = *(++row_iter);
      // If variable(j) is to remove, we pass another variable,
      // else we shift its cells to up right.
      // Attention: first we shift the cells corrispondent to the first
      // row of variable(j), then we shift the cells corrispondent to the
      // second row. We recall that every variable is represented in the `matrix'
      // by two rows and two rows.
      for (dimension_type j = 0; j <= i; ++j)
	if (!to_be_removed.count(Variable(j))) {
	  *(iter++) = row_ref[2*j];
	  *(iter++) = row_ref[2*j+1];
	}
      for (dimension_type j = 0; j <= i; ++j)
	if (!to_be_removed.count(Variable(j))) {
	  *(iter++) = row_ref1[2*j];
	  *(iter++) = row_ref1[2*j+1];
	}
      ++i;
    }
  }
  // Update the space dimension.
  matrix.shrink(new_space_dim);
  space_dim = new_space_dim;
  assert(OK());
}

template <typename T>
template <typename PartialFunction>
void
Octagonal_Shape<T>::map_space_dimensions(const PartialFunction& pfunc) {
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the octagon becomes zero_dimensional.
    remove_higher_space_dimensions(0);
    assert(OK());
    return;
  }

  const dimension_type new_space_dim = pfunc.max_in_codomain() + 1;
  // If we are going to actually reduce the space dimension,
  // then shortest-path closure is required to keep precision.
    if (new_space_dim < space_dim)
    strong_closure_assign();

  // If the octagon is empty, then it is sufficient to adjust
  // the space dimension of the octagon.
  if (marked_empty()) {
    remove_higher_space_dimensions(new_space_dim);
    return;
  }

  // We create a new matrix with the new space dimension.
  OR_Matrix<N> x(new_space_dim);
  for (typename OR_Matrix<N>::row_iterator i_iter = matrix.row_begin(),
	 i_end = matrix.row_end(); i_iter != i_end; i_iter += 2) {
    dimension_type new_i;
    dimension_type i = i_iter.index()/2;
    // We copy and place in the position into `x' the only cells of the `matrix'
    // that refer to both mapped variables, the variable `i' and `j'.
    if (pfunc.maps(i, new_i)) {
      typename OR_Matrix<N>::row_reference_type r_i = *i_iter;
      typename OR_Matrix<N>::row_reference_type r_ii = *(i_iter + 1);
      dimension_type double_new_i = 2*new_i;
      typename OR_Matrix<N>::row_iterator x_iter = x.row_begin() + double_new_i;
      typename OR_Matrix<N>::row_reference_type x_i = *x_iter;
      typename OR_Matrix<N>::row_reference_type x_ii = *(x_iter + 1);
      for(dimension_type j = 0; j <= i; ++j) {
	dimension_type new_j;
	// If also the second variable is mapped, we work.
	if (pfunc.maps(j, new_j)) {
	  dimension_type dj = 2*j;
	  dimension_type double_new_j = 2*new_j;
	  // Mapped the constraints, exchanging the indexes.
	  // Attention: our matrix is pseudo-triangular.
	  // If new_j > new_i, we must consider, as rows, the rows of the variable
	  // new_j, and not of new_i ones.
	  if (new_i >= new_j) {
	    x_i[double_new_j] = r_i[dj];
	    x_ii[double_new_j] = r_ii[dj];
	    x_ii[double_new_j+1] = r_ii[dj + 1];
	    x_i[double_new_j+1] = r_i[dj + 1];
	  }
	  else {
	    typename OR_Matrix<N>::row_iterator xj_iter = x.row_begin() + double_new_j;
	    typename OR_Matrix<N>::row_reference_type x_j = *xj_iter;
	    typename OR_Matrix<N>::row_reference_type x_jj = *(xj_iter + 1);
	    x_jj[double_new_i+1] = r_i[dj];
	    x_jj[double_new_i] = r_ii[dj];
	    x_j[double_new_i+1] = r_i[dj+1];
	    x_j[double_new_i] = r_ii[dj+1];
	  }

	}
      }
    }
  }

  std::swap(matrix, x);
  space_dim = new_space_dim;
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::intersection_assign(const Octagonal_Shape& y) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("intersection_assign(y)", y);

  // If one of the two octagons is empty, the intersection is empty.
  if (marked_empty())
    return;
  if (y.marked_empty()) {
    set_empty();
    return;
  }
  // If both octagons are zero-dimensional,then at this point
  // they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (space_dim == 0)
    return;

  // To intersect two octagons we compare the constraints
  // and we choose the less values.
  bool changed = false;

  typename OR_Matrix<N>::const_element_iterator j = y.matrix.element_begin();
  for (typename OR_Matrix<N>::element_iterator i = matrix.element_begin(),
	 iend = matrix.element_end(); i != iend; ++i, ++j) {
    N& elem = *i;
    const N& y_elem = *j;
    if (y_elem < elem) {
      elem = y_elem;
      changed = true;
    }
  }

  // This method not preserve the closure.
  if (changed && marked_strongly_closed())
    status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
template <typename Iterator>
void
Octagonal_Shape<T>::CC76_extrapolation_assign(const Octagonal_Shape& y,
					      Iterator first, Iterator last,
					      unsigned* tp) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("CC76_extrapolation_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If both octagons are zero-dimensional,
  // since `*this' contains `y', we simply return `*this'.
  if (space_dim == 0)
    return;

  strong_closure_assign();
  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  y.strong_closure_assign();
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  // If there are tokens available, work on a temporary copy.
  if (tp != 0 && *tp > 0) {
    Octagonal_Shape<T> x_tmp(*this);
    x_tmp.CC76_extrapolation_assign(y, first, last, 0);
    // If the widening was not precise, use one of the available tokens.
    if (!contains(x_tmp))
      --(*tp);
    return;
  }

  // Compare each constraint in `y' to the corresponding one in `*this'.
  // The constraint in `*this' is kept as is if it is stronger than or
  // equal to the constraint in `y'; otherwise, the inhomogeneous term
  // of the constraint in `*this' is further compared with elements taken
  // from a sorted container (the stop-points, provided by the user), and
  // is replaced by the first entry, if any, which is greater than or equal
  // to the inhomogeneous term. If no such entry exists, the constraint
  // is removed altogether.
  typename OR_Matrix<N>::const_element_iterator j = y.matrix.element_begin();
  for (typename OR_Matrix<N>::element_iterator i = matrix.element_begin(),
	 iend = matrix.element_end(); i != iend; ++i, ++j) {
    const N& y_elem = *j;
    N& elem = *i;
    if (y_elem < elem) {
      Iterator k = std::lower_bound(first, last, elem);
      if (k != last) {
	if (elem < *k)
	  //	  elem = *k;
	  assign_r(elem, *k, ROUND_UP);
      }
      else
	elem = PLUS_INFINITY;
    }
  }

  status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::get_limiting_octagon(const Constraint_System& cs,
		       Octagonal_Shape& limiting_octagon) const {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type cs_space_dim = cs.space_dimension();
  // Private method: the caller has to ensure the following.
  assert(cs_space_dim <= space_dim);

  bool is_oct_changed = false;
  strong_closure_assign();
  for (Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i) {
    const Constraint& c = *i;
    dimension_type num_vars = 0;
    dimension_type i = 0;
    dimension_type j = 0;
    Coefficient coeff;
    Coefficient term = c.inhomogeneous_term();

    // Constraints that are not octagonal differences are ignored.
    if (extract_octagonal_difference(c, cs_space_dim, num_vars, i, j, coeff, term)) {
      // Select the cell to be modified for the "<=" part of the constraint.
      typename OR_Matrix<N>::const_row_iterator k = matrix.row_begin() + i;
      typename OR_Matrix<N>::const_row_reference_type r = *k;
      OR_Matrix<N>& lo_mat = limiting_octagon.matrix;
      typename OR_Matrix<N>::row_iterator h = lo_mat.row_begin() + i;
      typename OR_Matrix<N>::row_reference_type s = *h;
      N& s_j = s[j];
      if (coeff < 0)
	coeff = -coeff;
      // Compute the bound for `r_j', rounding towards plus infinity.
      N d;
      div_round_up(d, term, coeff);
      if (r[j] <= d)
	if (c.is_inequality())
	  if (s_j > d) {
	    s_j = d;
	    is_oct_changed = true;
	  }
	else {
	  // Select the right row of the cell.
	  typename OR_Matrix<N>::const_row_iterator ck = (i%2 == 0) ? ++k : --k;
	  if (i%2 == 0)
	    ++h;
	  else
	    --h;
	  typename OR_Matrix<N>::const_row_reference_type r1 = *ck;
	  typename OR_Matrix<N>::row_reference_type s1 = *h;
	  // Select the right column of the cell.
	  dimension_type cj = coherent_index(j);
	  N& s1_cj = s1[cj];
	  div_round_up(d, -term, coeff);
	  if (r1[cj] <= d)
	    if (s1_cj > d) {
	      s1_cj = d;
	      is_oct_changed = true;
	    }
	}

    }
  }
  // In general, adding a constraint does not preserve the strongly
  // closure of the octagon.
  if (is_oct_changed && limiting_octagon.marked_strongly_closed())
    limiting_octagon.status.reset_strongly_closed();
}

template <typename T>
void
Octagonal_Shape<T>
::limited_CC76_extrapolation_assign(const Octagonal_Shape& y,
				    const Constraint_System& cs,
				    unsigned* tp) {

  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("limited_CC76_extrapolation_assign(y, cs)",
				 y);
  // `cs' must be dimension-compatible with the two octagons.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_constraint_incompatible("limited_CC76_extrapolation_assign(y, cs)");

  // Strict inequalities not allowed.
  if (cs.has_strict_inequalities())
    throw_constraint_incompatible("limited_CC76_extrapolation_assign(y, cs)");

  // The limited CC76-extrapolation between two octagons in a
  // zero-dimensional space is a octagon in a zero-dimensional
  // space, too.
  if (space_dim == 0)
    return;

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  Octagonal_Shape<T> limiting_octagon(space_dim, UNIVERSE);
  get_limiting_octagon(cs, limiting_octagon);
  CC76_extrapolation_assign(y, tp);
  intersection_assign(limiting_octagon);
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::BHMZ05_widening_assign(const Octagonal_Shape& y,
					   unsigned* tp) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("BHMZ05_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // Compute the affine dimension of `y'.
  const dimension_type y_affine_dim = y.affine_dimension();
  // If the affine dimension of `y' is zero, then either `y' is
  // zero-dimensional, or it is empty, or it is a singleton.
  // In all cases, due to the inclusion hypothesis, the result is `*this'.
  if (y_affine_dim == 0)
    return;

  // If the affine dimension has changed, due to the inclusion hypothesis,
  // the result is `*this'.
  const dimension_type x_affine_dim = affine_dimension();
  assert(x_affine_dim >= y_affine_dim);
  if (x_affine_dim != y_affine_dim)
    return;

  // If there are tokens available, work on a temporary copy.
  if (tp != 0 && *tp > 0) {
    Octagonal_Shape<T> x_tmp(*this);
    x_tmp.BHMZ05_widening_assign(y, 0);
    // If the widening was not precise, use one of the available tokens.
    if (!contains(x_tmp))
      --(*tp);
    return;
  }

  // Here no token is available.
  assert(marked_strongly_closed() && y.marked_strongly_closed());
  // Minimize `y'.
  y.strong_reduction_assign();

  // Extrapolate unstable bounds.
  typename OR_Matrix<N>::const_element_iterator j = y.matrix.element_begin();
  for (typename OR_Matrix<N>::element_iterator i = matrix.element_begin(),
       iend = matrix.element_end(); i != iend; ++i, ++j) {
    N& elem = *i;
      // Note: in the following line the use of `!=' (as opposed to
      // the use of `<' that would seem -but is not- equivalent) is
      // intentional.
    if (*j != elem) {
      elem = PLUS_INFINITY;
    }
  }
  status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::limited_BHMZ05_extrapolation_assign(const Octagonal_Shape& y,
				      const Constraint_System& cs,
				      unsigned* tp) {

  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("limited_BHMZ05_extrapolation_assign(y, cs)",
				 y);
  // `cs' must be dimension-compatible with the two octagons.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_constraint_incompatible("limited_CH78_extrapolation_assign(y, cs)");

  // Strict inequalities not allowed.
  if (cs.has_strict_inequalities())
    throw_constraint_incompatible("limited_CH78_extrapolation_assign(y, cs)");

  // The limited BHMZ05-extrapolation between two octagons in a
  // zero-dimensional space is a octagon in a zero-dimensional
  // space, too.
  if (space_dim == 0)
    return;

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  Octagonal_Shape<T> limiting_octagon(space_dim, UNIVERSE);
  get_limiting_octagon(cs, limiting_octagon);
  BHMZ05_widening_assign(y, tp);
  intersection_assign(limiting_octagon);
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::CC76_narrowing_assign(const Octagonal_Shape& y) {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("CC76_narrowing_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `*this' is contained in or equal to `y'.
    const Octagonal_Shape x_copy = *this;
    const Octagonal_Shape y_copy = y;
    assert(y_copy.contains(x_copy));
  }
#endif

  // If both octagons are zero-dimensional, since `*this' contains `y',
  // we simply return '*this'.
  if (space_dim == 0)
    return;

  y.strong_closure_assign();
  // If `y' is empty, since `y' contains `*this', `*this' is empty too.
  if (y.marked_empty())
    return;
  strong_closure_assign();
  // If `*this' is empty, we return.
  if (y.marked_empty())
    return;

  // We consider a constraint of `*this', if its value is `plus_infinity',
  // we take the value of the corresponding constraint of `y'.
  bool is_oct_changed = false;
  typename OR_Matrix<N>::const_element_iterator j = y.matrix.element_begin();
  for (typename OR_Matrix<N>::element_iterator i = matrix.element_begin(),
       iend = matrix.element_end(); i != iend; ++i, ++j) {
     if (!is_plus_infinity(*i) &&
	 !is_plus_infinity(*j) &&
	 *i != *j){
      *i = *j;
      is_oct_changed = true;
    }
  }

  if (is_oct_changed && marked_strongly_closed())
    status.reset_strongly_closed();
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::deduce_v_minus_u_bounds(const dimension_type v,
			  const dimension_type last_v,
			  const Linear_Expression& sc_expr,
			  Coefficient_traits::const_reference sc_den,
			  const N& pos_sum) {
  // Deduce constraints of the form `v - u', where `u != v'.
  // Note: the strongly closure is able to deduce the constraint
  // `v - u <= ub_v - lb_u'. We can be more precise if variable `u'
  // played an active role in the computation of the upper bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // greater than zero. In particular:
  // if `q >= 1',    then `v - u <= ub_v - ub_u';
  // if `0 < q < 1', then `v - u <= ub_v - (q*ub_u + (1-q)*lb_u)'.
  mpq_class mpq_sc_den;
  assign_r(mpq_sc_den, sc_den, ROUND_NOT_NEEDED);
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      typename OR_Matrix<N>::row_iterator i_u = matrix.row_begin() + 2*u;
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u));
      if (expr_u > 0)
	if (expr_u >= sc_den) {
	  // Deducing `v - u <= ub_v - ub_u'.
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  N r_cu_u;
	  div2exp_assign_r(r_cu_u, r_cu[2*u], 1, ROUND_UP);
	  if (v < u)
	    sub_assign_r(r_u[2*v], pos_sum, r_cu_u, ROUND_UP);
	  else if (v > u) {
	    typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	    typename OR_Matrix<N>::row_reference_type r_cv = *(i_v+1);
	    sub_assign_r(r_cv[2*u+1], pos_sum, r_cu_u, ROUND_UP);
	  }
	}
	else {
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  if (!is_plus_infinity(r_u[2*u+1])) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `v - u' is computed as
	    // `ub_v - (q * ub_u + (1-q) * lb_u)', i.e.,
	    // `pos_sum + (-lb_u) - q * (ub_u + (-lb_u))'.
	    mpq_class double_minus_lb_u;
	    assign_r(double_minus_lb_u, r_u[2*u+1], ROUND_NOT_NEEDED);
	    mpq_class minus_lb_u;
	    div2exp_assign_r(minus_lb_u, double_minus_lb_u, 1, ROUND_UP);
	    mpq_class q;
	    assign_r(q, expr_u, ROUND_NOT_NEEDED);
	    div_assign_r(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class double_ub_u;
	    typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	    assign_r(double_ub_u, r_cu[2*u], ROUND_NOT_NEEDED);
	    mpq_class ub_u;
	    div2exp_assign_r(ub_u, double_ub_u, 1, ROUND_UP);
	    // Compute `ub_u - lb_u'.
	    add_assign_r(ub_u, ub_u, minus_lb_u, ROUND_NOT_NEEDED);
	    // Compute `(-lb_u) - q * (ub_u - lb_u)'.
	    sub_mul_assign_r(minus_lb_u, q, ub_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign_r(up_approx, minus_lb_u, ROUND_UP);
	    // Deducing `v - u <= ub_v - (q * ub_u + (1-q) * lb_u)'.
	    if (v < u)
	      add_assign_r(r_u[2*v], pos_sum, up_approx, ROUND_UP);
	    else if (v > u) {
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	      typename OR_Matrix<N>::row_reference_type r_cv = *(i_v+1);
	      add_assign_r(r_cv[2*u+1], pos_sum, up_approx, ROUND_UP);
	    }
	  }
	}
    }
}

template <typename T>
void
Octagonal_Shape<T>
::deduce_v_plus_u_bounds(const dimension_type v,
			 const dimension_type last_v,
			 const Linear_Expression& sc_expr,
			 Coefficient_traits::const_reference sc_den,
			 const N& pos_sum) {
  // Deduce constraints of the form `v + u', where `u != v'.
  // Note: the strongly closure is able to deduce the constraint
  // `v + u <= ub_v + ub_u'. We can be more precise if variable `u'
  // played an active role in the computation of the upper bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // less than zero. In particular:
  // if `q <= -1',    then `v + u <= ub_v + lb_u';
  // if `-1 < q < 0', then `v + u <= ub_v + (q*lb_u + (1-q)*ub_u)'.
  mpq_class mpq_sc_den;
  assign_r(mpq_sc_den, sc_den, ROUND_NOT_NEEDED);
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      typename OR_Matrix<N>::row_iterator i_u = matrix.row_begin() + 2*u;
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u));
      if (expr_u < 0) {
	TEMP_INTEGER(minus_expr_u);
	neg_assign_r(minus_expr_u, expr_u, ROUND_NOT_NEEDED);
	if (minus_expr_u >= sc_den) {
	  // Deducing `v + u <= ub_v + lb_u'.
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  N r_u_cu;
	  div2exp_assign_r(r_u_cu, r_u[2*u+1], 1, ROUND_UP);
	  if (v < u)
	    sub_assign_r(r_cu[2*v], pos_sum, r_u_cu, ROUND_UP);
	  else if (v > u) {
	    typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	    typename OR_Matrix<N>::row_reference_type r_cv = *(i_v+1);
	    sub_assign_r(r_cv[2*u], pos_sum, r_u_cu, ROUND_UP);
	  }
	}
	else {
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  if (!is_plus_infinity(r_cu[2*u])) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `v + u' is computed as
	    // `ub_v + (q * lb_u + (1-q) * ub_u)', i.e.,
	    // `pos_sum + ub_u + q * (lb_u - ub_u)'.
	    mpq_class double_ub_u;
	    assign_r(double_ub_u, r_cu[2*u], ROUND_NOT_NEEDED);
	    mpq_class ub_u;
	    div2exp_assign_r(ub_u, double_ub_u, 1, ROUND_UP);
	    mpq_class q;
	    assign_r(q, minus_expr_u, ROUND_NOT_NEEDED);
	    div_assign_r(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class double_minus_lb_u;
	    typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	    assign_r(double_minus_lb_u, r_u[2*u+1], ROUND_NOT_NEEDED);
	    mpq_class minus_lb_u;
	    div2exp_assign_r(minus_lb_u, double_minus_lb_u, 1, ROUND_UP);
	    mpq_class lb_u;
	    neg_assign_r(lb_u, minus_lb_u, ROUND_NOT_NEEDED);
	    // Compute `lb_u - ub_u'.
	    sub_assign_r(lb_u, lb_u, ub_u, ROUND_NOT_NEEDED);
	    // Compute `ub_u + q * (lb_u - ub_u)'.
	    add_mul_assign_r(ub_u, q, lb_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign_r(up_approx, ub_u, ROUND_UP);
	    // Deducing `v + u <= ub_v + (q * lb_u + (1-q) * ub_u)'.
	    if (v < u)
	      add_assign_r(r_cu[2*v], pos_sum, up_approx, ROUND_UP);
	    else if (v > u) {
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	      typename OR_Matrix<N>::row_reference_type r_cv = *(i_v+1);
	      add_assign_r(r_cv[2*u], pos_sum, up_approx, ROUND_UP);
	    }
	  }
	}
      }
    }
}

template <typename T>
void
Octagonal_Shape<T>
::deduce_u_minus_v_bounds(const dimension_type v,
			  const dimension_type last_v,
			  const Linear_Expression& sc_expr,
			  Coefficient_traits::const_reference sc_den,
			  const N& neg_sum) {
  // Deduce constraints of the form `u - v', where `u != v'.
  // Note: the strongly closure is able to deduce the constraint
  // `u - v <= ub_u - lb_v'. We can be more precise if variable `u'
  // played an active role in the computation of the lower bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // greater than zero. In particular:
  // if `q >= 1',    then `u - v <= lb_u - lb_v';
  // if `0 < q < 1', then `u - v <= (q*lb_u + (1-q)*ub_u) - lb_v'.
  mpq_class mpq_sc_den;
  assign_r(mpq_sc_den, sc_den, ROUND_NOT_NEEDED);
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      typename OR_Matrix<N>::row_iterator i_u = matrix.row_begin() + 2*u;
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u));
      if (expr_u > 0)
	if (expr_u >= sc_den) {
	  // Deducing `u - v <= lb_u - lb_v',
	  // i.e., `u - v <= (-lb_v) - (-lb_u)'.
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  N r_u_cu;
	  div2exp_assign_r(r_u_cu, r_u[2*u+1], 1, ROUND_UP);
	  if (v < u)
	    sub_assign_r(r_cu[2*v+1], neg_sum, r_u_cu, ROUND_UP);
	  else if (v > u) {
	    typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	    typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	    sub_assign_r(r_v[2*u], neg_sum, r_u_cu, ROUND_UP);
	  }
	}
	else {
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  if (!is_plus_infinity(r_cu[2*u])) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `u - v' is computed as
	    // `(q * lb_u + (1-q) * ub_u) - lb_v', i.e.,
	    // `ub_u - q * (ub_u + (-lb_u)) + neg_sum'.
	    mpq_class double_ub_u;
	    assign_r(double_ub_u, r_cu[2*u], ROUND_NOT_NEEDED);
	    mpq_class ub_u;
	    div2exp_assign_r(ub_u, double_ub_u, 1, ROUND_UP);
	    mpq_class q;
	    assign_r(q, expr_u, ROUND_NOT_NEEDED);
	    div_assign_r(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class double_minus_lb_u;
	    typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	    assign_r(double_minus_lb_u, r_u[2*u+1], ROUND_NOT_NEEDED);
	    mpq_class minus_lb_u;
	    div2exp_assign_r(minus_lb_u, double_minus_lb_u, 1, ROUND_UP);
	    // Compute `ub_u - lb_u'.
	    add_assign_r(minus_lb_u, minus_lb_u, ub_u, ROUND_NOT_NEEDED);
	    // Compute `ub_u - q * (ub_u - lb_u)'.
	    sub_mul_assign_r(ub_u, q, minus_lb_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign_r(up_approx, ub_u, ROUND_UP);
	    // Deducing `u - v <= (q*lb_u + (1-q)*ub_u) - lb_v'.
	    if (v < u)
	      add_assign_r(r_cu[2*v+1], up_approx, neg_sum, ROUND_UP);
	    else if (v > u) {
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	      typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	      add_assign_r(r_v[2*u], up_approx, neg_sum, ROUND_UP);
	    }
	  }
	}
    }
}

template <typename T>
void
Octagonal_Shape<T>
::deduce_minus_v_minus_u_bounds(const dimension_type v,
				const dimension_type last_v,
				const Linear_Expression& sc_expr,
				Coefficient_traits::const_reference sc_den,
				const N& neg_sum) {
  // Deduce constraints of the form `- v - u', where `u != v'.
  // Note: the strongly closure is able to deduce the constraint
  // `- v - u <= - lb_v - lb_u'. We can be more precise if variable `u'
  // played an active role in the computation of the lower bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // less than zero. In particular:
  // if `q <= -1',    then `-v - u <= -lb_v - ub_u';
  // if `-1 < q < 0', then `-v - u <= -lb_v - (q*ub_u + (1-q)*lb_u)'.
  mpq_class mpq_sc_den;
  assign_r(mpq_sc_den, sc_den, ROUND_NOT_NEEDED);
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      typename OR_Matrix<N>::row_iterator i_u = matrix.row_begin() + 2*u;
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u));
      if (expr_u < 0) {
	TEMP_INTEGER(minus_expr_u);
	neg_assign_r(minus_expr_u, expr_u, ROUND_NOT_NEEDED);
	if (minus_expr_u >= sc_den) {
	  // Deducing `-v - u <= -lb_v - ub_u'.
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	  N r_cu_u;
	  div2exp_assign_r(r_cu_u, r_cu[2*u], 1, ROUND_UP);
	  N negated_r_cu_u;
	  neg_assign_r(negated_r_cu_u, r_cu_u, ROUND_NOT_NEEDED);
	  if (v < u)
	    sub_assign_r(r_cu[2*v+1], negated_r_cu_u, neg_sum, ROUND_UP);
	  else if (v > u) {
	    typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	    typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	    sub_assign_r(r_v[2*u+1], negated_r_cu_u, neg_sum, ROUND_UP);
	  }
	}
	else {
	  typename OR_Matrix<N>::row_reference_type r_u = *i_u;
	  if (!is_plus_infinity(r_u[2*u+1])) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `-v - u' is computed as
	    //`-lb_v - (q*ub_u + (1-q)*lb_u)', i.e.,
	    // `-neg_sum - lb_u + q * (lb_u - ub_u)'.
	    mpq_class double_lb_u;
	    assign_r(double_lb_u, r_u[2*u+1], ROUND_NOT_NEEDED);
	    mpq_class lb_u;
	    div2exp_assign_r(lb_u, double_lb_u, 1, ROUND_UP);
	    mpq_class q;
	    assign_r(q, minus_expr_u, ROUND_NOT_NEEDED);
	    div_assign_r(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class double_ub_u;
	    typename OR_Matrix<N>::row_reference_type r_cu = *(i_u+1);
	    assign_r(double_ub_u, r_cu[2*u], ROUND_NOT_NEEDED);
	    mpq_class ub_u;
	    div2exp_assign_r(ub_u, double_ub_u, 1, ROUND_UP);
	    mpq_class minus_ub_u;
	    neg_assign_r(minus_ub_u, ub_u, ROUND_NOT_NEEDED);
	    // Compute `lb_u - ub_u'.
	    add_assign_r(minus_ub_u, lb_u, minus_ub_u, ROUND_NOT_NEEDED);
	    mpq_class minus_lb_u;
	    neg_assign_r(minus_lb_u, lb_u, ROUND_NOT_NEEDED);
	    // Compute `-lb_u + q * (lb_u - ub_u)'.
	    add_mul_assign_r(minus_lb_u, q, minus_ub_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign_r(up_approx, minus_lb_u, ROUND_UP);
	    // Deducing `-v - u <= -lb_v - (q * ub_u + (1-q) * lb_u)'.
	    if (v < u)
	      add_assign_r(r_u[2*v+1], up_approx,neg_sum, ROUND_UP);
	    else if (v > u) {
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + 2*v;
	      typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	      add_assign_r(r_v[2*u+1], up_approx, neg_sum, ROUND_UP);
	    }
	  }
	}
      }
    }
}

template <typename T>
void
Octagonal_Shape<T>::affine_image(const Variable var,
				 const Linear_Expression& expr,
				 Coefficient_traits::const_reference
				 denominator) {
  using Implementation::BD_Shapes::div_round_up;

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the octagon.
  const dimension_type num_var = var.id();
  if (space_dim < num_var + 1)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());

  strong_closure_assign();
  // The image of an empty octagon is empty too.
  if (marked_empty())
    return;

  Coefficient b = expr.inhomogeneous_term();

  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;

  // Value of inhomogeneous term of `expr' in the case `expr' is a
  // unary.
  Coefficient coeff;

  // Variable-index of the last non-zero coefficient in `expr', if any.
  dimension_type last_var_id = 0;

  // Get information about the number of non-zero coefficients in `expr'.
  // The `expr' must not be in two or plus variables.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else {
        last_var_id = i;
	coeff = expr.coefficient(Variable(last_var_id));
      }
  // `w' is the variable with index `last_var_id'.
  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t == 2, the `expr' is of the general form.
  const dimension_type n_var = 2*num_var;

  TEMP_INTEGER(minus_den);
  neg_assign_r(minus_den, denominator, ROUND_NOT_NEEDED);
  if (t == 0) {
    // Case 1: expr == b.
    // Remove all constraints on `var'.
    forget_all_octagonal_constraints(n_var);
    b *= 2;
    // Add the constraint `var == b/denominator'.
    add_octagonal_constraint(n_var+1, n_var, b, denominator);
    add_octagonal_constraint(n_var, n_var+1, b, minus_den);
    assert(OK());
    return;
  }

  if (t == 1) {
    if (coeff == denominator || coeff == minus_den) {
      // Case 2: expr = coeff*w + b, with coeff = +/- denominator.
      if (last_var_id == num_var) {
	// `expr' is of the form: coeff*v + b.
	// The `expr' is of the form: -denominator*var + n.
	// First we adjust the matrix of the octagon, swapping x_i^+ with x_i^-.
	if (coeff == denominator) {
	  if (b == 0)
	    // The transformation is the identity function.
	    return;
	  else {
	    // Translate all the constraints on `var' adding or
	    // subtracting the value `b/denominator'.
	    N d;
	    div_round_up(d, b, denominator);
	    N c;
	    div_round_up(c, b, minus_den);
	    N& m_nv_nv1 = matrix[n_var][n_var+1];
	    N& m_nv1_nv = matrix[n_var+1][n_var];
	    const dimension_type h = n_var + 2;
	    for (typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + h,
		   i_end = matrix.row_end(); i != i_end; ++i) {
	      typename OR_Matrix<N>::row_reference_type x_i = *i;
	      add_assign_r(x_i[n_var], x_i[n_var], d, ROUND_UP);
	      add_assign_r(x_i[n_var+1], x_i[n_var+1], c, ROUND_UP);
	    }
	    mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	    add_assign_r(m_nv1_nv, m_nv1_nv, d, ROUND_UP);
	    mul2exp_assign_r(c, c, 1, ROUND_IGNORE);
	    add_assign_r(m_nv_nv1, m_nv_nv1, c, ROUND_UP);
 	  }
	  status.reset_strongly_closed();
	}

	else {
	  // Here `coeff == -denominator'.
	  // Remove the binary constraints on `var'.
	  forget_binary_octagonal_constraints(n_var);
 	  typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
 	  typename OR_Matrix<N>::row_reference_type x_i = *i;
 	  typename OR_Matrix<N>::row_reference_type x_ii = *(i+1);
	  // Swap the unary constraints on `var'.
	  std::swap(x_i[n_var+1], x_ii[n_var]);
	  // Strong closure is not preserved.
	  status.reset_strongly_closed();
	  if (b != 0) {
	    // Translate all the constraints on `var' adding or
	    // subtracting the value `b/denominator'.
	    N d;
	    div_round_up(d, b, denominator);
	    N c;
	    div_round_up(c, b, minus_den);
	    N& m_nv_nv1 = matrix[n_var][n_var+1];
	    N& m_nv1_nv = matrix[n_var+1][n_var];
	    const dimension_type h = n_var + 2;
	    for (typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + h,
		   i_end = matrix.row_end(); i != i_end; ++i) {
	      typename OR_Matrix<N>::row_reference_type x_i = *i;
	      add_assign_r(x_i[n_var], x_i[n_var], d, ROUND_UP);
	      add_assign_r(x_i[n_var+1], x_i[n_var+1], c, ROUND_UP);
	    }
	    mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	    add_assign_r(m_nv1_nv, m_nv1_nv, d, ROUND_UP);
	    mul2exp_assign_r(c, c, 1, ROUND_IGNORE);
	    add_assign_r(m_nv_nv1, m_nv_nv1, c, ROUND_UP);
	  }
	  incremental_strong_closure_assign(var);
 	}
      }

      else {
	// Here `w != var', so that `expr' is of the form
	// +/-denominator * w + b.
	// Remove all constraints on `var'.
	forget_all_octagonal_constraints(n_var);
	dimension_type h = 2*last_var_id;
	// Add the new constraint `var - w = b/denominator'.
	if (coeff == denominator) {
	  if (num_var < last_var_id) {
	    add_octagonal_constraint(h, n_var, b, denominator);
	    add_octagonal_constraint(h+1, n_var+1, b, minus_den);
	  }
	  else if (num_var > last_var_id) {
	    add_octagonal_constraint(n_var+1, h+1, b, denominator);
	    add_octagonal_constraint(n_var, h, b, minus_den);
	  }
	}
	else {
	// Add the new constraint `var + w = b/denominator'.
	  if (num_var < last_var_id) {
	    add_octagonal_constraint(h+1, n_var, b, denominator);
	    add_octagonal_constraint(h, n_var+1, b, minus_den);
	  }
	  else if (num_var > last_var_id) {
	    add_octagonal_constraint(n_var+1, h, b, denominator);
	    add_octagonal_constraint(n_var, h+1, b, minus_den);
	  }
	}
	incremental_strong_closure_assign(var);
      }
      assert(OK());
      return;
    }
  }

  // General case.
  // Either t == 2, so that
  // expr == a_1*x_1 + a_2*x_2 + ... + a_n*x_n + b, where n >= 2,
  // or t == 1, expr == a*w + b, but a <> +/- denominator.
  // We will remove all the constraints on `var' and add back
  // constraints providing upper and lower bounds for `var'.

  // Compute upper approximations for `expr' and `-expr'
  // into `pos_sum' and `neg_sum', respectively, taking into account
  // the sign of `denominator'.
  // Note: approximating `-expr' from above and then negating the
  // result is the same as approximating `expr' from below.
  const bool is_sc = (denominator > 0);
  TEMP_INTEGER(minus_b);
  neg_assign_r(minus_b, b, ROUND_NOT_NEEDED);

  const Coefficient& sc_b = is_sc ? b : minus_b;
  const Coefficient& minus_sc_b = is_sc ? minus_b : b;
  const Coefficient& sc_den = is_sc ? denominator : minus_den;
  const Coefficient& minus_sc_den = is_sc ? minus_den : denominator;
  // NOTE: here, for optimization purposes, `minus_expr' is only assigned
  // when `denominator' is negative. Do not use it unless you are sure
  // it has been correctly assigned.
  Linear_Expression minus_expr;
  if (!is_sc)
    minus_expr = -expr;
  const Linear_Expression& sc_expr = is_sc ? expr : minus_expr;

  N pos_sum;
  N neg_sum;
  // Indices of the variables that are unbounded in `this->matrix'.
  // (The initializations are just to quiet a compiler warning.)
  dimension_type pos_pinf_index = 0;
  dimension_type neg_pinf_index = 0;
  // Number of unbounded variables found.
  dimension_type pos_pinf_count = 0;
  dimension_type neg_pinf_count = 0;

  // Approximate the inhomogeneous term.
  assign_r(pos_sum, sc_b, ROUND_UP);
  assign_r(neg_sum, minus_sc_b, ROUND_UP);

  // Approximate the homogeneous part of `sc_expr'.
  // Note: indices above `w' can be disregarded, as they all have
  // a zero coefficient in `sc_expr'.
  for (dimension_type i = last_var_id + 1; i-- > 0; ) {
    const Coefficient& sc_i = sc_expr.coefficient(Variable(i));
    const dimension_type j_0 = 2*i;
    const dimension_type j_1 = j_0 + 1;
    typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + j_0;
    typename OR_Matrix<N>::const_row_reference_type m_j0 = *iter;
    typename OR_Matrix<N>::const_row_reference_type m_j1 = *(iter+1);
    const int sign_i = sgn(sc_i);
    if (sign_i > 0) {
      N coeff_i;
      assign_r(coeff_i, sc_i, ROUND_UP);
      // Approximating `sc_expr'.
      if (pos_pinf_count <= 1) {
	const N& double_up_approx_i = m_j1[j_0];
	if (!is_plus_infinity(double_up_approx_i)) {
	  N up_approx_i;
	  div2exp_assign_r(up_approx_i, double_up_approx_i, 1, ROUND_UP);
	  add_mul_assign_r(pos_sum, coeff_i, up_approx_i, ROUND_UP);
	}
	else {
	  ++pos_pinf_count;
	  pos_pinf_index = i;
	}
      }
      // Approximating `-sc_expr'.
      if (neg_pinf_count <= 1) {
	const N& double_up_approx_minus_i = m_j0[j_1];
	if (!is_plus_infinity(double_up_approx_minus_i)) {
	  N up_approx_minus_i;
	  div2exp_assign_r(up_approx_minus_i,
			   double_up_approx_minus_i, 1, ROUND_UP);
	  add_mul_assign_r(neg_sum, coeff_i, up_approx_minus_i, ROUND_UP);
	}
	else {
	  ++neg_pinf_count;
	  neg_pinf_index = i;
	}
      }
    }
    else if (sign_i < 0) {
      TEMP_INTEGER(minus_sc_i);
      neg_assign_r(minus_sc_i, sc_i, ROUND_NOT_NEEDED);
      N minus_coeff_i;
      assign_r(minus_coeff_i, minus_sc_i, ROUND_UP);
      // Approximating `sc_expr'.
      if (pos_pinf_count <= 1) {
	const N& double_up_approx_minus_i = m_j0[j_1];
	if (!is_plus_infinity(double_up_approx_minus_i)) {
	  N up_approx_minus_i;
	  div2exp_assign_r(up_approx_minus_i,
			   double_up_approx_minus_i, 1, ROUND_UP);
	  add_mul_assign_r(pos_sum,
			   minus_coeff_i, up_approx_minus_i, ROUND_UP);
	}
	else {
	  ++pos_pinf_count;
	  pos_pinf_index = i;
	}
      }
      // Approximating `-sc_expr'.
      if (neg_pinf_count <= 1) {
	const N& double_up_approx_i = m_j1[j_0];
	if (!is_plus_infinity(double_up_approx_i)) {
	  N up_approx_i;
	  div2exp_assign_r(up_approx_i, double_up_approx_i, 1, ROUND_UP);
	  add_mul_assign_r(neg_sum, minus_coeff_i, up_approx_i, ROUND_UP);
	}
	else {
	  ++neg_pinf_count;
	  neg_pinf_index = i;
	}
      }
    }
  }

  // Remove all constraints with var in the rows.
  forget_all_octagonal_constraints(n_var);
  // Return immediately if no approximation could be computed.
  if (pos_pinf_count > 1 && neg_pinf_count > 1) {
    assert(OK());
    return;
  }

  // In the following, strongly closure will be definitely lost.
  status.reset_strongly_closed();

  // Before computing quotients, the denominator should be approximated
  // towards zero. Since `sc_den' is known to be positive, this amounts to
  // rounding downwards, which is achieved as usual by rounding upwards
  // `minus_sc_den' and negating again the result.
  N down_sc_den;
  assign_r(down_sc_den, minus_sc_den, ROUND_UP);
  neg_assign_r(down_sc_den, down_sc_den, ROUND_UP);

  // Exploit the upper approximation, if possible.
  if (pos_pinf_count <= 1) {
    // Compute quotient (if needed).
    if (down_sc_den != 1)
      div_assign_r(pos_sum, pos_sum, down_sc_den, ROUND_UP);
    // Add the upper bound constraint, if meaningful.
    if (pos_pinf_count == 0) {
      // Add the constraint `v <= pos_sum'.
      N double_pos_sum = pos_sum;
      mul2exp_assign_r(double_pos_sum, pos_sum, 1, ROUND_IGNORE);
      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var+1;
      typename OR_Matrix<N>::row_reference_type r = *i;
      assign_r(r[n_var], double_pos_sum, ROUND_UP);
      // Deduce constraints of the form `v - u', where `u != v'.
      deduce_v_minus_u_bounds(num_var, last_var_id, sc_expr, sc_den, pos_sum);
      // Deduce constraints of the form `v + u', where `u != v'.
      deduce_v_plus_u_bounds(num_var, last_var_id, sc_expr, sc_den, pos_sum);
    }
    else
      // Here `pos_pinf_count == 1'.
      if (pos_pinf_index != num_var) {
	if (sc_expr.coefficient(Variable(pos_pinf_index)) == sc_den) {
	  // Add the constraint `v - pos_pinf_index <= pos_sum'.
	  if (num_var < pos_pinf_index) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pos_pinf_index;
	    typename OR_Matrix<N>::row_reference_type r_i = *i;
	    assign_r(r_i[n_var], pos_sum, ROUND_UP);
	  }
	  if (num_var > pos_pinf_index) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	    typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	    assign_r(r_ci[2*pos_pinf_index+1], pos_sum, ROUND_UP);
	  }
	}
	else {
	  if (sc_expr.coefficient(Variable(pos_pinf_index)) == minus_sc_den) {
	    // Add the constraint `v + pos_pinf_index <= pos_sum'.
	    if (num_var < pos_pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pos_pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	      assign_r(r_ci[n_var], pos_sum, ROUND_UP);
	    }
	    if (num_var > pos_pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	      assign_r(r_ci[2*pos_pinf_index], pos_sum, ROUND_UP);
	    }
	  }
	}
      }
  }

  // Exploit the lower approximation, if possible.
  if (neg_pinf_count <= 1) {
    // Compute quotient (if needed).
    if (down_sc_den != 1)
      div_assign_r(neg_sum, neg_sum, down_sc_den, ROUND_UP);
    // Add the lower bound constraint, if meaningful.
    if (neg_pinf_count == 0) {
      // Add the constraint `v >= -neg_sum', i.e., `-v <= neg_sum'.
      N double_neg_sum = neg_sum;
      mul2exp_assign_r(double_neg_sum, neg_sum, 1, ROUND_IGNORE);
      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
      typename OR_Matrix<N>::row_reference_type r_i = *i;
      assign_r(r_i[n_var+1], double_neg_sum, ROUND_UP);
      // Deduce constraints of the form `u - v', where `u != v'.
      deduce_u_minus_v_bounds(num_var, last_var_id, sc_expr, sc_den, neg_sum);
      // Deduce constraints of the form `-v - u', where `u != v'.
      deduce_minus_v_minus_u_bounds(num_var, last_var_id,
				    sc_expr, sc_den, neg_sum);
    }
    else
      // Here `neg_pinf_count == 1'.
      if (neg_pinf_index != num_var) {
	if (sc_expr.coefficient(Variable(neg_pinf_index)) == sc_den) {
	// Add the constraint `v - neg_pinf_index >= -neg_sum',
	// i.e., `neg_pinf_index - v <= neg_sum'.
	  if (neg_pinf_index < num_var) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	    typename OR_Matrix<N>::row_reference_type r_i = *i;
	    assign_r(r_i[2*neg_pinf_index], neg_sum, ROUND_UP);
	  }
	  if (neg_pinf_index > num_var) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*neg_pinf_index;
	    typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	    assign_r(r_ci[n_var+1], neg_sum, ROUND_UP);
	  }
	}
	else {
	  if (sc_expr.coefficient(Variable(neg_pinf_index)) == minus_sc_den) {
	    // Add the constraint `v + neg_pinf_index >= -neg_sum',
	    // i.e., `-neg_pinf_index - v <= neg_sum'.
	    if (neg_pinf_index < num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[2*neg_pinf_index+1], neg_sum, ROUND_UP);
	    }
	    if (neg_pinf_index > num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*neg_pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[n_var+1], neg_sum, ROUND_UP);
	    }
	  }
	}
      }
  }

  incremental_strong_closure_assign(var);
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::affine_preimage(const Variable var,
				    const Linear_Expression& expr,
				    Coefficient_traits::const_reference
				    denominator) {

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the octagon.
  dimension_type num_var = var.id();
  if (space_dim < num_var + 1)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  strong_closure_assign();
  // The image of an empty octagon is empty too.
  if (marked_empty())
    return;

  const Coefficient b = expr.inhomogeneous_term();

  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;

  // Variable-index of the last non-zero coefficient in `expr', if any.
  dimension_type last_var_id = 0;

  // Get information about the number of the non-zero coefficients of `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
        last_var_id = i;

  // `w' is the variable with index `last_var_id'.
  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t == 2, the `expr' is of the general form.
  const dimension_type n_var = 2*num_var;

  if (t == 0) {
    // Case 1: expr = n; remove all costraints on `var'.
    forget_all_octagonal_constraints(n_var);
    assert(OK());
    return;
  }

  if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& a = expr.coefficient(Variable(last_var_id));
    if (a == denominator || a == -denominator) {
    // Case 2: expr = a*w + b, with a = +/- denominator.
    if (last_var_id == num_var) {
      // Apply affine_image() on the inverse of this transformation.
      affine_image(var, a*var - b, denominator);
    }
    else {
      // `expr == a*w + b', where `w != var'.
      // Remove all constraints on `var'.
      forget_all_octagonal_constraints(n_var);
    }
    assert(OK());
    return;
    }
  }
  // General case.
  // Either t == 2, so that
  // expr = a_1*x_1 + a_2*x_2 + ... + a_n*x_n + b, where n >= 2,
  // or t = 1, expr = a*w + b, but a <> +/- denominator.
  const Coefficient& expr_v = expr.coefficient(var);
  if (expr_v != 0) {
    if (expr_v > 0) {
      // The transformation is invertible.
      Linear_Expression inverse = ((expr_v + denominator)*var);
      inverse -= expr;
      affine_image(var, inverse, expr_v);
    }
    else {
      // The transformation is invertible.
      Linear_Expression inverse = ((-expr_v - denominator)*var);
      inverse += expr;
      affine_image(var, inverse, -expr_v);
    }
  }
  else
    // The transformation is not invertible: all constraints on `var' are lost.
    forget_all_octagonal_constraints(n_var);

  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::generalized_affine_image(const Variable var,
			   const Relation_Symbol relsym,
			   const Linear_Expression&  expr ,
			   Coefficient_traits::const_reference denominator) {
  using Implementation::BD_Shapes::div_round_up;

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_image(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)", "e",
				 expr);

  // `var' should be one of the dimensions of the octagon.
  dimension_type num_var = var.id();
  if (space_dim < num_var + 1)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 var.id());

  // The relation symbol cannot be a strict relation symbol.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_image(v, r, e, d)",
  		  "r is a strict relation symbol and "
  		  "*this is an Octagonal_Shape");

  if (relsym == EQUAL) {
    // The relation symbol is "==":
    // this is just an affine image computation.
    affine_image(var, expr, denominator);
    assert(OK());
    return;
  }

  // The image of an empty octagon is empty too.
  strong_closure_assign();
if (marked_empty())
    return;

 const Coefficient& b = expr.inhomogeneous_term();

  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;

  // Value of inhomogeneous term of `expr' in the case `expr' is a
  // unary.
  Coefficient coeff;

  // Variable-index of the last non-zero coefficient in `expr', if any.
  dimension_type last_var_id = 0;

  // Get information about the number of non-zero coefficients in `expr'.
  // The `expr' must not be in two or plus variables.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
        last_var_id = i;

  // `w' is the variable with index `last_var_id'.
  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t == 2, the `expr' is of the general form.
  const dimension_type n_var = 2*num_var;

  TEMP_INTEGER(minus_den);
  neg_assign_r(minus_den, denominator, ROUND_NOT_NEEDED);

  if (t == 0) {
    // Case 1: expr = b.
    // Remove all constraints on `var'.
    forget_all_octagonal_constraints(n_var);
    // Strong closure is lost.
    status.reset_strongly_closed();
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      // Add the constraint `var <= b/denominator'.
      add_octagonal_constraint(n_var+1, n_var, 2*b, denominator);
      break;
    case GREATER_THAN_OR_EQUAL:
      // Add the constraint `var >= n/denominator',
      // i.e., `-var<= -b/denominator'.
      add_octagonal_constraint(n_var, n_var+1, 2*b, minus_den);
      break;
    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
    assert(OK());
    return;
  }

  if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& a = expr.coefficient(Variable(last_var_id));
    if (a == denominator || a == minus_den) {
      // Case 2: expr == a*w + b, with a == +/- denominator.
      switch (relsym) {
      case LESS_THAN_OR_EQUAL:
	{
	  N d;
	  div_round_up(d, b, denominator);
	  if (last_var_id == num_var) {
	    // `expr' is of the form: a*v + b.
	    // Strong closure is not preserved.
	    status.reset_strongly_closed();
	    if (a == denominator) {
	      // Translate all the constraints of the form `v - w <= cost'
	      // into the constraint `v - w <= cost + b/denominator';
	      // forget each constraint `w - v <= cost1'.
	      N& m_nv_nv1 = matrix[n_var][n_var+1];
	      N& m_nv1_nv = matrix[n_var+1][n_var];
	      const dimension_type h = n_var + 2;
	      for (typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + h,
		     i_end = matrix.row_end(); i != i_end; ++i) {
	      typename OR_Matrix<N>::row_reference_type x_i = *i;
	      add_assign_r(x_i[n_var], x_i[n_var], d, ROUND_UP);
	      x_i[n_var+1] = PLUS_INFINITY;
	      }
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	      typename OR_Matrix<N>::row_reference_type r_cv = *(++i_v);
	      for (dimension_type k = n_var; k-- > 0; ) {
		r_v[k] = PLUS_INFINITY;
		add_assign_r(r_cv[k], r_cv[k], d, ROUND_UP);
	      }
	      mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	      add_assign_r(m_nv1_nv, m_nv1_nv, d, ROUND_UP);
	      m_nv_nv1 = PLUS_INFINITY;
	    }
	    else {
	      // Here `a == -denominator'.
	      // `expr' is of the form: -a*var + b.
	      N& m_nv_nv1 = matrix[n_var][n_var+1];
	      N& m_nv1_nv = matrix[n_var+1][n_var];
	      mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	      add_assign_r(m_nv1_nv, m_nv_nv1, d, ROUND_UP);
	      m_nv_nv1 = PLUS_INFINITY;
	      forget_binary_octagonal_constraints(n_var);
	    }
	  }
	  else {
	    // Here `w != v', so that `expr' is the form
	    // +/- denominator*w + b.
	    // Remove all constraints on `v'.
	    forget_all_octagonal_constraints(n_var);
	    const dimension_type h = 2*last_var_id;
	    if (a == denominator) {
	      // Add the new constraint `v - w <= b/denominator'.
	      if (num_var < last_var_id)
		add_octagonal_constraint(h, n_var, b, denominator);
	      else if (num_var > last_var_id)
		add_octagonal_constraint(n_var+1, h+1, b, denominator);
	    }
	    else {
	      // Add the new constraint `v + w <= b/denominator'.
	      if (num_var < last_var_id)
		add_octagonal_constraint(h+1, n_var, b, denominator);
	      else if (num_var > last_var_id)
		add_octagonal_constraint(n_var+1, h, b, denominator);
	    }
	  }
	  break;
	}

      case GREATER_THAN_OR_EQUAL:
	{
	  N d;
	  div_round_up(d, b, minus_den);
	  if (last_var_id == num_var) {
	    // `expr' is of the form: a*w + b.
	    // Strong closure is not preserved.
	    status.reset_strongly_closed();
	    if (a == denominator) {
	      // Translate each constraint `w - v <= cost'
	      // into the constraint `w - v <= cost - b/denominator';
	      // forget each constraint `v - w <= cost1'..
	      N& m_nv_nv1 = matrix[n_var][n_var+1];
	      N& m_nv1_nv = matrix[n_var+1][n_var];
	      const dimension_type h = n_var + 2;
	      for (typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + h,
		     i_end = matrix.row_end(); i != i_end; ++i) {
	      typename OR_Matrix<N>::row_reference_type x_i = *i;
	      x_i[n_var] = PLUS_INFINITY;
	      add_assign_r(x_i[n_var+1], x_i[n_var+1], d, ROUND_UP);
	      }
	      typename OR_Matrix<N>::row_iterator i_v = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_v = *i_v;
	      typename OR_Matrix<N>::row_reference_type r_cv = *(++i_v);
	      for (dimension_type k = n_var; k-- > 0; ) {
		add_assign_r(r_v[k], r_v[k], d, ROUND_UP);
		r_cv[k] = PLUS_INFINITY;
	      }
	      mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	      add_assign_r(m_nv_nv1, m_nv_nv1, d, ROUND_UP);
	      m_nv1_nv = PLUS_INFINITY;
	    }
	    else {
	      // Here `a == -denominator'.
	      // `expr' is of the form: -a*var + b.
	      N& m_nv_nv1 = matrix[n_var][n_var+1];
	      N& m_nv1_nv = matrix[n_var+1][n_var];
	      mul2exp_assign_r(d, d, 1, ROUND_IGNORE);
	      add_assign_r(m_nv_nv1, m_nv1_nv, d, ROUND_UP);
	      m_nv1_nv = PLUS_INFINITY;
	      forget_binary_octagonal_constraints(n_var);
	    }
	  }
	  else {
	    // Here `w != v', so that `expr' is of the form
	    // +/-denominator * w + b, with `w != v'.
	    // Remove all constraints on `v'.
	    forget_all_octagonal_constraints(n_var);
	    const dimension_type h = 2*last_var_id;
	    // We have got an expression of the following form:
	    // var1 + n, with `var1' != `var'.
	    // We remove all constraints of the form `var (+/- var1) >= const'
	    // and we add the new constraint `var +/- var1 >= n/denominator'.
	    if (a == denominator) {
	      // Add the new constraint `var - w >= b/denominator',
	      // i.e., `w - var <= -b/denominator'.
	      if (num_var < last_var_id)
		add_octagonal_constraint(h+1, n_var+1, b, minus_den);
	      else if (num_var > last_var_id)
		add_octagonal_constraint(n_var, h, b, minus_den);
	    }
	    else {
	      // Add the new constraint `var + w >= b/denominator',
	      // i.e., `-w - var <= -b/denominator'.
	      if (num_var < last_var_id)
		add_octagonal_constraint(h, n_var+1, b, minus_den);
	      else if (num_var > last_var_id)
		add_octagonal_constraint(n_var, h+1, b, minus_den);
	    }
	  }
	  break;
	}

      default:
	// We already dealt with the other cases.
	throw std::runtime_error("PPL internal error");
	break;
      }
      assert(OK());
      return;
    }
  }

  // General case.
  // Either t == 2, so that
  // expr == a_1*x_1 + a_2*x_2 + ... + a_n*x_n + b, where n >= 2,
  // or t == 1, expr == a*w + b, but a <> +/- denominator.
  // We will remove all the constraints on `v' and add back
  // a constraint providing an upper or a lower bound for `v'
  // (depending on `relsym').
  const bool is_sc = (denominator > 0);
  TEMP_INTEGER(minus_b);
  neg_assign(minus_b, b);
  const Coefficient& sc_b = is_sc ? b : minus_b;
  const Coefficient& minus_sc_b = is_sc ? minus_b : b;
  const Coefficient& sc_den = is_sc ? denominator : minus_den;
  const Coefficient& minus_sc_den = is_sc ? minus_den : denominator;
  // NOTE: here, for optimization purposes, `minus_expr' is only assigned
  // when `denominator' is negative. Do not use it unless you are sure
  // it has been correctly assigned.
  Linear_Expression minus_expr;
  if (!is_sc)
    minus_expr = -expr;
  const Linear_Expression& sc_expr = is_sc ? expr : minus_expr;

  N sum;
  // Index of variable that is unbounded in `this'.
  // (The initialization is just to quiet a compiler warning.)
  dimension_type pinf_index = 0;
  // Number of unbounded variables found.
  dimension_type pinf_count = 0;

  switch (relsym) {
  case LESS_THAN_OR_EQUAL:
    {
      // Compute an upper approximation for `sc_expr' into `sum'.

      // Approximate the inhomogeneous term.
      assign_r(sum, sc_b, ROUND_UP);
      // Approximate the homogeneous part of `sc_expr'.
      // Note: indices above `w' can be disregarded, as they all have
      // a zero coefficient in `sc_expr'.
      for (dimension_type i = last_var_id + 1; i-- > 0; ) {
	const Coefficient& sc_i = sc_expr.coefficient(Variable(i));
	const dimension_type j_0 = 2*i;
	const dimension_type j_1 = j_0 + 1;
	typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + j_0;
	typename OR_Matrix<N>::const_row_reference_type m_j0 = *iter;
	typename OR_Matrix<N>::const_row_reference_type m_j1 = *(iter+1);
	const int sign_i = sgn(sc_i);
	if (sign_i == 0)
	  continue;
	// Choose carefully: we are approximating `sc_expr'.
	const N& double_approx_i = (sign_i > 0) ? m_j1[j_0] : m_j0[j_1];
	if (is_plus_infinity(double_approx_i)) {
	  if (++pinf_count > 1)
	    break;
	  pinf_index = i;
	  continue;
	}
	N coeff_i;
	if (sign_i > 0)
	  assign_r(coeff_i, sc_i, ROUND_UP);
	else {
	  TEMP_INTEGER(minus_sc_i);
	  neg_assign(minus_sc_i, sc_i);
	  assign_r(coeff_i, minus_sc_i, ROUND_UP);
	}
	N approx_i;
	div2exp_assign_r(approx_i, double_approx_i, 1, ROUND_UP);
	add_mul_assign_r(sum, coeff_i, approx_i, ROUND_UP);
      }
      // Remove all constraints on `v'.
      forget_all_octagonal_constraints(n_var);
      status.reset_strongly_closed();
      // Return immediately if no approximation could be computed.
      if (pinf_count > 1) {
	assert(OK());
	return;
      }

      // Divide by the (sign corrected) denominator (if needed).
      if (sc_den != 1) {
	// Before computing the quotient, the denominator should be approximated
	// towards zero. Since `sc_den' is known to be positive, this amounts to
	// rounding downwards, which is achieved as usual by rounding upwards
	// `minus_sc_den' and negating again the result.
	N down_sc_den;
	assign_r(down_sc_den, minus_sc_den, ROUND_UP);
	neg_assign_r(down_sc_den, down_sc_den, ROUND_UP);
	div_assign_r(sum, sum, down_sc_den, ROUND_UP);
      }

      if (pinf_count == 0) {
	// Add the constraint `v <= pos_sum'.
	N double_sum = sum;
	mul2exp_assign_r(double_sum, sum, 1, ROUND_IGNORE);
	typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var+1;
	typename OR_Matrix<N>::row_reference_type r = *i;
	assign_r(r[n_var], double_sum, ROUND_UP);
	// Deduce constraints of the form `v - u', where `u != v'.
	deduce_v_minus_u_bounds(num_var, last_var_id, sc_expr, sc_den, sum);
	// Deduce constraints of the form `v + u', where `u != v'.
	deduce_v_plus_u_bounds(num_var, last_var_id, sc_expr, sc_den, sum);
      }
      else if (pinf_count == 1)
	if (pinf_index != num_var) {
	  if (expr.coefficient(Variable(pinf_index)) == denominator ) {
	    // Add the constraint `v - pinf_index <= sum'.
	    if (num_var < pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[n_var], sum, ROUND_UP);
	    }
	    if (num_var > pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	      assign_r(r_ci[2*pinf_index+1], sum, ROUND_UP);
	    }
	  }
	  else {
	    if (expr.coefficient(Variable(pinf_index)) == minus_den) {
	      // Add the constraint `v + pinf_index <= sum'.
	      if (num_var < pinf_index) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
		typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
		assign_r(r_ci[n_var], sum, ROUND_UP);
	      }
	      if (num_var > pinf_index) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
		typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
		assign_r(r_ci[2*pinf_index], sum, ROUND_UP);
	      }
	    }
	  }
	}
      break;
    }

  case GREATER_THAN_OR_EQUAL:
    {
      // Compute an upper approximation for `-sc_expr' into `sum'.
      // Note: approximating `-sc_expr' from above and then negating the
      // result is the same as approximating `sc_expr' from below.

      // Approximate the inhomogeneous term.
      assign_r(sum, minus_sc_b, ROUND_UP);
      // Approximate the homogeneous part of `-sc_expr'.
      for (dimension_type i = last_var_id + 1; i-- > 0; ) {
	const Coefficient& sc_i = sc_expr.coefficient(Variable(i));
	const dimension_type j_0 = 2*i;
	const dimension_type j_1 = j_0 + 1;
	typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + j_0;
	typename OR_Matrix<N>::const_row_reference_type m_j0 = *iter;
	typename OR_Matrix<N>::const_row_reference_type m_j1 = *(iter+1);
	const int sign_i = sgn(sc_i);
	if (sign_i == 0)
	  continue;
	// Choose carefully: we are approximating `-sc_expr'.
	const N& double_approx_i = (sign_i > 0) ? m_j0[j_1] : m_j1[j_0];
	if (is_plus_infinity(double_approx_i)) {
	  if (++pinf_count > 1)
	    break;
	  pinf_index = i;
	  continue;
	}
	N coeff_i;
	if (sign_i > 0)
	  assign_r(coeff_i, sc_i, ROUND_UP);
	else {
	  TEMP_INTEGER(minus_sc_i);
	  neg_assign(minus_sc_i, sc_i);
	  assign_r(coeff_i, minus_sc_i, ROUND_UP);
	}
	N approx_i;
	div2exp_assign_r(approx_i, double_approx_i, 1, ROUND_UP);
	add_mul_assign_r(sum, coeff_i, approx_i, ROUND_UP);
      }

      // Remove all constraints on `var'.
      forget_all_octagonal_constraints(n_var);
      status.reset_strongly_closed();
      // Return immediately if no approximation could be computed.
      if (pinf_count > 1) {
	assert(OK());
	return;
      }

      // Divide by the (sign corrected) denominator (if needed).
      if (sc_den != 1) {
	// Before computing the quotient, the denominator should be approximated
	// towards zero. Since `sc_den' is known to be positive, this amounts to
	// rounding downwards, which is achieved as usual by rounding upwards
	// `minus_sc_den' and negating again the result.
	N down_sc_den;
	assign_r(down_sc_den, minus_sc_den, ROUND_UP);
	neg_assign_r(down_sc_den, down_sc_den, ROUND_UP);
	div_assign_r(sum, sum, down_sc_den, ROUND_UP);
      }

      if (pinf_count == 0) {
	// Add the constraint `v >= -neg_sum', i.e., `-v <= neg_sum'.
	N double_sum = sum;
	mul2exp_assign_r(double_sum, sum, 1, ROUND_IGNORE);
	typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	typename OR_Matrix<N>::row_reference_type r_i = *i;
	assign_r(r_i[n_var+1], double_sum, ROUND_UP);
	// Deduce constraints of the form `u - v', where `u != v'.
	deduce_u_minus_v_bounds(num_var, pinf_index, sc_expr, sc_den, sum);
	// Deduce constraints of the form `-v - u', where `u != v'.
	deduce_minus_v_minus_u_bounds(num_var, pinf_index,
				      sc_expr, sc_den, sum);
      }
      else if (pinf_count == 1)
	if (pinf_index != num_var) {
	  if (expr.coefficient(Variable(pinf_index)) == denominator) {
	    // Add the constraint `v - pinf_index >= -sum',
	    // i.e., `pinf_index - v <= sum'.
	    if (pinf_index < num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[2*pinf_index], sum, ROUND_UP);
	    }
	    if (pinf_index > num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	      assign_r(r_ci[n_var], sum, ROUND_UP);
	    }
	  }
	  else {
	    if (expr.coefficient(Variable(pinf_index)) == minus_den) {
	      // Add the constraint `v + pinf_index >= -sum',
	      // i.e., `-pinf_index - v <= sum'.
	      if (pinf_index < num_var) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
		typename OR_Matrix<N>::row_reference_type r_i = *i;
		assign_r(r_i[2*pinf_index+1], sum, ROUND_UP);
	      }
	      if (pinf_index > num_var) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
		typename OR_Matrix<N>::row_reference_type r_i = *i;
		assign_r(r_i[n_var+1], sum, ROUND_UP);
	      }
	    }
	  }
	}
      break;
    }

  default:
    // We already dealt with the other cases.
    throw std::runtime_error("PPL internal error");
    break;
  }
  incremental_strong_closure_assign(var);
  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>::generalized_affine_image(const Linear_Expression& lhs,
					     const Relation_Symbol relsym,
					     const Linear_Expression& rhs) {
  // Dimension-compatibility checks.
  // The dimension of `lhs' should not be greater than the dimension
  // of `*this'.
  dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e1", lhs);

  // The dimension of `rhs' should not be greater than the dimension
  // of `*this'.
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  // Strict relation symbols are not admitted for octagons.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_image(e1, r, e2)",
		  "r is a strict relation symbol and "
		  "*this is an Octagonal_Shape");

  strong_closure_assign();
  // The image of an empty octagon is empty.
  if (marked_empty())
    return;

  // Number of non-zero coefficients in `lhs': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t_lhs = 0;
  // Index of the last non-zero coefficient in `lhs', if any.
  dimension_type j_lhs = 0;

  // Compute the number of the non-zero components of `lhs'.
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      if (t_lhs++ == 1)
	break;
      else
	j_lhs = i;
    }

  const Coefficient& b_lhs = lhs.inhomogeneous_term();

  if (t_lhs == 0) {
    // `lhs' is a constant.
    // In principle, it is sufficient to add the constraint `lhs relsym rhs'.
    // Note that this constraint is a bounded difference if `t_rhs <= 1'
    // or `t_rhs > 1' and `rhs == a*v - a*w + b_rhs'. If `rhs' is of a
    // more general form, it will be simply ignored.
    // TODO: if it is not a bounded difference, should we compute
    // approximations for this constraint?
      switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      add_constraint(lhs <= rhs);
      break;
    case EQUAL:
      add_constraint(lhs == rhs);
      break;
    case GREATER_THAN_OR_EQUAL:
      add_constraint(lhs >= rhs);
      break;
    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }

  else if (t_lhs == 1) {
    // Here `lhs == a_lhs * v + b_lhs'.
    // Independently from the form of `rhs', we can exploit the
    // method computing generalized affine images for a single variable.
    Variable v(j_lhs);
    // Compute a sign-corrected relation symbol.
    const Coefficient& den = lhs.coefficient(v);
    Relation_Symbol new_relsym = relsym;
    if (den < 0)
      if (relsym == LESS_THAN_OR_EQUAL)
	new_relsym = GREATER_THAN_OR_EQUAL;
      else if (relsym == GREATER_THAN_OR_EQUAL)
	new_relsym = LESS_THAN_OR_EQUAL;
    Linear_Expression expr = rhs - b_lhs;
    generalized_affine_image(v, new_relsym, expr, den);
  }
  else {
    // Here `lhs' is of the general form, having at least two variables.
    // Compute the set of variables occurring in `lhs'.
    bool lhs_vars_intersects_rhs_vars = false;
    std::vector<Variable> lhs_vars;
    for (dimension_type i = lhs_space_dim; i-- > 0; )
      if (lhs.coefficient(Variable(i)) != 0) {
	lhs_vars.push_back(Variable(i));
	if (rhs.coefficient(Variable(i)) != 0)
	  lhs_vars_intersects_rhs_vars = true;
      }

    if (!lhs_vars_intersects_rhs_vars) {
      // `lhs' and `rhs' variables are disjoint.
      // Cylindrificate on all variables in the lhs.
      for (dimension_type i = lhs_vars.size(); i-- > 0; ) {
	dimension_type lhs_vars_i = lhs_vars[i].id();
	forget_all_octagonal_constraints(2*lhs_vars_i);
      }
      // Constrain the left hand side expression so that it is related to
      // the right hand side expression as dictated by `relsym'.
      // TODO: if the following constraint is NOT a bounded difference,
      // it will be simply ignored. Should we compute approximations for it?
      switch (relsym) {
      case LESS_THAN_OR_EQUAL:
	add_constraint(lhs <= rhs);
	break;
      case EQUAL:
	add_constraint(lhs == rhs);
	break;
      case GREATER_THAN_OR_EQUAL:
	add_constraint(lhs >= rhs);
	break;
      default:
	// We already dealt with the other cases.
	throw std::runtime_error("PPL internal error");
	break;
      }
    }
    else {
      // Some variables in `lhs' also occur in `rhs'.

#if 1 // Simplified computation (see the TODO note below).

      for (dimension_type i = lhs_vars.size(); i-- > 0; ) {
	dimension_type lhs_vars_i = lhs_vars[i].id();
	forget_all_octagonal_constraints(2*lhs_vars_i);
      }

#else // Currently unnecessarily complex computation.

      // More accurate computation that is worth doing only if
      // the following TODO note is accurately dealt with.

      // To ease the computation, we add an additional dimension.
      const Variable new_var = Variable(space_dim);
      add_space_dimensions_and_embed(1);
      // Constrain the new dimension to be equal to `rhs'.
      // NOTE: calling affine_image() instead of add_constraint()
      // ensures some approximation is tried even when the constraint
      // is not a bounded difference.
      affine_image(new_var, rhs);
      // Cylindrificate on all variables in the lhs.
      // NOTE: enforce shortest-path closure for precision.
      strong_closure_assign();
      assert(!marked_empty());
      for (dimension_type i = lhs_vars.size(); i-- > 0; ) {
	dimension_type lhs_vars_i = lhs_vars[i].id();
	forget_all_octagonal_constraints(2*lhs_vars_i);
      }
      // Constrain the new dimension so that it is related to
      // the left hand side as dictated by `relsym'.
      // TODO: each one of the following constraints is definitely NOT
      // a bounded differences (since it has 3 variables at least).
      // Thus, the method add_constraint() will simply ignore it.
      // Should we compute approximations for this constraint?
      switch (relsym) {
      case LESS_THAN_OR_EQUAL:
	add_constraint(lhs <= new_var);
	break;
      case EQUAL:
	add_constraint(lhs == new_var);
	break;
      case GREATER_THAN_OR_EQUAL:
	add_constraint(lhs >= new_var);
	break;
      default:
	// We already dealt with the other cases.
	throw std::runtime_error("PPL internal error");
	break;
      }
      // Remove the temporarily added dimension.
      remove_higher_space_dimensions(space_dim-1);
#endif // Currently unnecessarily complex computation.
    }
  }

  assert(OK());
}

template <typename T>
void
Octagonal_Shape<T>
::generalized_affine_preimage(const Variable var,
			      const Relation_Symbol relsym,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference
			      denominator) {
  using Implementation::BD_Shapes::div_round_up;

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_preimage(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 "e", expr);

  // `var' should be one of the dimensions of the octagon.
  const dimension_type num_var = var.id();
  if (space_dim < num_var + 1)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 var.id());

  // The relation symbol cannot be a strict relation symbol.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_preimage(v, r, e, d)",
  		  "r is a strict relation symbol and "
  		  "*this is an Octagonal_Shape");

  if (relsym == EQUAL) {
    // The relation symbol is "==":
    // this is just an affine preimage computation.
    affine_preimage(var, expr, denominator);
    assert(OK());
    return;
  }

  // The image of an empty ocatgon is empty too.
  strong_closure_assign();
  if (marked_empty())
    return;

  // Check whether the preimage of this affine relation can be easily
  // computed as the image of its inverse relation.
  const Coefficient& expr_v = expr.coefficient(var);
  if (expr_v != 0) {
    const Relation_Symbol reversed_relsym = (relsym == LESS_THAN_OR_EQUAL)
      ? GREATER_THAN_OR_EQUAL : LESS_THAN_OR_EQUAL;
    const Linear_Expression inverse
      = expr - (expr_v + denominator)*var;
    TEMP_INTEGER(inverse_den);
    neg_assign(inverse_den, expr_v);
    const Relation_Symbol inverse_relsym
      = (sgn(denominator) == sgn(inverse_den)) ? relsym : reversed_relsym;
    generalized_affine_image(var, inverse_relsym, inverse, inverse_den);
    return;
  }

  // Here `var_coefficient == 0', so that the preimage cannot
  // be easily computed by inverting the affine relation.
  // Shrink the Octagonal_Shape by adding the constraint induced
  // by the affine relation.
  Coefficient b = expr.inhomogeneous_term();

  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;

  // Variable index of the last non-zero coefficient in `expr', if any.
  dimension_type last_var_id = 0;

  // Get information about the number of non-zero coefficients in `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
	last_var_id = i;

  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*j + b, where `j != v';
  // - If t == 2, the `expr' is of the general form.
  const dimension_type n_var = 2*num_var;

  if (t == 0) {
    // Case 1: expr == b.
    b *= 2;
    typename OR_Matrix<N>::row_iterator iter_v = matrix.row_begin() + n_var;
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      // Add the constraint `var <= b/denominator'.
      add_octagonal_constraint(n_var+1, n_var, b, denominator);
      break;
    case GREATER_THAN_OR_EQUAL:
      // Add the constraint `var >= b/denominator',
      // i.e., `-var <= -b/denominator',
      add_octagonal_constraint(n_var, n_var+1, -b, denominator);
      break;
    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }
  else if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& expr_last_var = expr.coefficient(Variable(last_var_id));
    N d;
    dimension_type lv_index = 2*last_var_id;
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      div_round_up(d, b, denominator);
      // Note that: `last_var_id != v', so that `expr' is of the form
      // expr_last_var * w + b, with `last_var_id != v'.
      if (expr_last_var == denominator) {
	// Add the new constraints `v - w <= b/denominator'.
	if (num_var < last_var_id)
	  add_octagonal_constraint(lv_index, n_var, d);
	if (num_var > last_var_id)
	  add_octagonal_constraint(n_var+1, lv_index+1, d);
      }
      else if (expr_last_var == -denominator) {
	// Add the new constraints `v + w <= b/denominator'.
	if (num_var < last_var_id)
	  add_octagonal_constraint(lv_index+1, n_var, d);
	if (num_var > last_var_id)
	  add_octagonal_constraint(n_var+1, lv_index, d);
      }
      else {
     	// Here expr_last_var != denominator, so that we should be adding
	// the constraint `v <= b/denominator - w'.
      	N sum;
	assign_r(sum, d, ROUND_UP);
	// Approximate the homogeneous part of `expr'.
	const int sign_last_v = sgn(expr_last_var);
	typename OR_Matrix<N>::row_iterator last_v = matrix.row_begin() + lv_index;
	typename OR_Matrix<N>::row_reference_type r_v = *last_v;
	typename OR_Matrix<N>::row_reference_type r_cv = *(++last_v);
	const N& double_approx_lv = (sign_last_v > 0) ? r_cv[lv_index] : r_v[lv_index+1];
	if (!is_plus_infinity(double_approx_lv)) {
	  N coeff_lv;
	  if (sign_last_v > 0)
	    assign_r(coeff_lv, expr_last_var, ROUND_UP);
	  else {
	    TEMP_INTEGER(minus_expr_last_v);
	    neg_assign(minus_expr_last_v, expr_last_var);
	    assign_r(coeff_lv, minus_expr_last_v, ROUND_UP);
	  }
	  N approx_lv;
	  div2exp_assign_r(approx_lv, double_approx_lv, 1, ROUND_UP);
	  N den;
	  assign_r(den, denominator, ROUND_UP);
	  div_assign_r(coeff_lv, coeff_lv, den, ROUND_UP);
          add_mul_assign_r(sum, coeff_lv, approx_lv, ROUND_UP);
	  mul2exp_assign_r(sum, sum, 1, ROUND_IGNORE);
	  add_octagonal_constraint(n_var+1, n_var, sum);
	}
      }
      break;

    case GREATER_THAN_OR_EQUAL:
      div_round_up(d, -b, denominator);
      // Note that: `last_var_id != v', so that `expr' is of the form
      // expr_last_var * w + b, with `last_var_id != v'.
      if (expr_last_var == denominator) {
    	// Add the new constraint `v - w >= b/denominator'.
	if (num_var < last_var_id)
	  add_octagonal_constraint(lv_index+1, n_var+1, d);
	if (num_var > last_var_id)
	  add_octagonal_constraint(n_var, lv_index, d);
      }
      else if (expr_last_var == -denominator) {
	// Add the new constraints `v + w >= b/denominator'.
	if (num_var < last_var_id)
	  add_octagonal_constraint(lv_index, n_var+1, d);
	if (num_var > last_var_id)
	  add_octagonal_constraint(n_var, lv_index+1, d);
      }
      else {
     	// Here expr_last_var != denominator, so that we should be adding
	// the constraint `v <= b/denominator - w'.
	N sum;
	assign_r(sum, d, ROUND_UP);
	// Approximate the homogeneous part of `expr_last_var'.
	const int sign_lv = sgn(expr_last_var);
	typename OR_Matrix<N>::row_iterator last_v = matrix.row_begin() + lv_index;
	typename OR_Matrix<N>::row_reference_type r_v = *last_v;
	typename OR_Matrix<N>::row_reference_type r_cv = *(++last_v);
	const N& double_approx_lv = (sign_lv > 0)
	  ? r_v[lv_index+1] : r_cv[lv_index];
	if (!is_plus_infinity(double_approx_lv)) {
	  N coeff_lv;
	  if (sign_lv > 0)
	    assign_r(coeff_lv, expr_last_var, ROUND_UP);
	  else {
	    TEMP_INTEGER(minus_expr_lv);
	    neg_assign(minus_expr_lv, expr_last_var);
	    assign_r(coeff_lv, minus_expr_lv, ROUND_UP);
	  }
	  N approx_lv;
	  div2exp_assign_r(approx_lv, double_approx_lv, 1, ROUND_UP);
	  N den;
	  assign_r(den, denominator, ROUND_UP);
	  div_assign_r(coeff_lv, coeff_lv, den, ROUND_UP);
	  add_mul_assign_r(sum, coeff_lv, approx_lv, ROUND_UP);
	  mul2exp_assign_r(sum, sum, 1, ROUND_IGNORE);
	  add_octagonal_constraint(n_var, n_var+1, sum);
	}
      }
      break;

    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }
  else {
    // Here t == 2, so that
    // expr == a_1*x_1 + a_2*x_2 + ... + a_n*x_n + b, where n >= 2.
    const bool is_sc = (denominator > 0);
    TEMP_INTEGER(minus_b);
    neg_assign(minus_b, b);
    const Coefficient& sc_b = is_sc ? b : minus_b;
    const Coefficient& minus_sc_b = is_sc ? minus_b : b;
    TEMP_INTEGER(minus_den);
    neg_assign(minus_den, denominator);
    const Coefficient& sc_den = is_sc ? denominator : minus_den;
    const Coefficient& minus_sc_den = is_sc ? minus_den : denominator;
    // NOTE: here, for optimization purposes, `minus_expr' is only assigned
    // when `denominator' is negative. Do not use it unless you are sure
    // it has been correctly assigned.
    Linear_Expression minus_expr;
    if (!is_sc)
      minus_expr = -expr;
    const Linear_Expression& sc_expr = is_sc ? expr : minus_expr;

    N sum;
    // Index of variable that is unbounded in `this'.
    // (The initialization is just to quiet a compiler warning.)
    dimension_type pinf_index = 0;
    // Number of unbounded variables found.
    dimension_type pinf_count = 0;

    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      {
	// Compute an upper approximation for `expr' into `sum',
	// taking into account the sign of `denominator'.

	// Approximate the inhomogeneous term.
	assign_r(sum, sc_b, ROUND_UP);

	// Approximate the homogeneous part of `sc_expr'.
	// Note: indices above `last_var_id' can be disregarded, as they all have
	// a zero coefficient in `expr'.

	for (dimension_type i = last_var_id + 1; i-- > 0; ) {
	  const Coefficient& sc_i = sc_expr.coefficient(Variable(i));
	  const dimension_type j_0 = 2*i;
	  const dimension_type j_1 = j_0 + 1;
	  typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + j_0;
	  typename OR_Matrix<N>::const_row_reference_type m_j0 = *iter;
	  typename OR_Matrix<N>::const_row_reference_type m_j1 = *(iter+1);
	  const int sign_i = sgn(sc_i);
	  if (sign_i == 0)
	    continue;
	  // Choose carefully: we are approximating `sc_expr'.
	  const N& double_approx_i = (sign_i > 0) ? m_j1[j_0] : m_j0[j_1];
	  if (is_plus_infinity(double_approx_i)) {
	    if (++pinf_count > 1)
	      break;
	    pinf_index = i;
	    continue;
	  }
	  N coeff_i;
	  if (sign_i > 0)
	    assign_r(coeff_i, sc_i, ROUND_UP);
	  else {
	    TEMP_INTEGER(minus_sc_i);
	    neg_assign(minus_sc_i, sc_i);
	    assign_r(coeff_i, minus_sc_i, ROUND_UP);
	  }
	  N approx_i;
	  div2exp_assign_r(approx_i, double_approx_i, 1, ROUND_UP);
	  add_mul_assign_r(sum, coeff_i, approx_i, ROUND_UP);
	}
	// Divide by the (sign corrected) denominator (if needed).
	if (sc_den != 1) {
	  // Before computing the quotient, the denominator should be
	  // approximated towards zero. Since `sc_den' is known to be
	  // positive, this amounts to rounding downwards, which is achieved
	  // by rounding upwards `minus_sc-den' and negating again the result.
	  N down_sc_den;
	  assign_r(down_sc_den, minus_sc_den, ROUND_UP);
	  neg_assign_r(down_sc_den, down_sc_den, ROUND_UP);
	  div_assign_r(sum, sum, down_sc_den, ROUND_UP);
	}

	if (pinf_count == 0) {
	  // Add the constraint `v <= sum'.
	  N double_sum = sum;
	  mul2exp_assign_r(double_sum, sum, 1, ROUND_IGNORE);
	  typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var+1;
	  typename OR_Matrix<N>::row_reference_type r = *i;
	  assign_r(r[n_var], double_sum, ROUND_UP);
	  // Deduce constraints of the form `v - u', where `u != v'.
	  deduce_v_minus_u_bounds(num_var, last_var_id, sc_expr, sc_den, sum);
	  // Deduce constraints of the form `v + u', where `u != v'.
	  deduce_v_plus_u_bounds(num_var, last_var_id, sc_expr, sc_den, sum);
	}
	else if (pinf_count == 1)
	  if (expr.coefficient(Variable(pinf_index)) == denominator ) {
	    // Add the constraint `v - pinf_index <= sum'.
	    if (num_var < pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[n_var], sum, ROUND_UP);
	    }
	    if (num_var > pinf_index) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	      assign_r(r_ci[2*pinf_index+1], sum, ROUND_UP);
	    }
	  }
	  else {
	    if (expr.coefficient(Variable(pinf_index)) == minus_den) {
	      // Add the constraint `v + pinf_index <= sum'.
	      if (num_var < pinf_index) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
		typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
		assign_r(r_ci[n_var], sum, ROUND_UP);
	      }
	      if (num_var > pinf_index) {
		typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
		typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
		assign_r(r_ci[2*pinf_index], sum, ROUND_UP);
	      }
	    }
	  }
	break;
      }

    case GREATER_THAN_OR_EQUAL:
      {
      // Compute an upper approximation for `-sc_expr' into `sum'.
      // Note: approximating `-sc_expr' from above and then negating the
      // result is the same as approximating `sc_expr' from below.

      // Approximate the inhomogeneous term.
      assign_r(sum, minus_sc_b, ROUND_UP);

      // Approximate the homogeneous part of `-sc_expr'.
      for (dimension_type i = last_var_id + 1; i-- > 0; ) {
	const Coefficient& sc_i = sc_expr.coefficient(Variable(i));
	const dimension_type j_0 = 2*i;
	const dimension_type j_1 = j_0 + 1;
	typename OR_Matrix<N>::const_row_iterator iter = matrix.row_begin() + j_0;
	typename OR_Matrix<N>::const_row_reference_type m_j0 = *iter;
	typename OR_Matrix<N>::const_row_reference_type m_j1 = *(iter+1);
	const int sign_i = sgn(sc_i);
	if (sign_i == 0)
	  continue;
	// Choose carefully: we are approximating `-sc_expr'.
	const N& double_approx_i = (sign_i > 0) ? m_j0[j_1] : m_j1[j_0];
	if (is_plus_infinity(double_approx_i)) {
	  if (++pinf_count > 1)
	    break;
	  pinf_index = i;
	  continue;
	}
	N coeff_i;
	if (sign_i > 0)
	  assign_r(coeff_i, sc_i, ROUND_UP);
	else {
	  TEMP_INTEGER(minus_sc_i);
	  neg_assign(minus_sc_i, sc_i);
	  assign_r(coeff_i, minus_sc_i, ROUND_UP);
	}
	N approx_i;
	div2exp_assign_r(approx_i, double_approx_i, 1, ROUND_UP);
	add_mul_assign_r(sum, coeff_i, approx_i, ROUND_UP);
      }

      // Divide by the (sign corrected) denominator (if needed).
      if (sc_den != 1) {
	// Before computing the quotient, the denominator should be
	// approximated towards zero. Since `sc_den' is known to be positive,
	// this amounts to rounding downwards, which is achieved by rounding
	// upwards `minus_sc_den' and negating again the result.
	N down_sc_den;
	assign_r(down_sc_den, minus_sc_den, ROUND_UP);
	neg_assign_r(down_sc_den, down_sc_den, ROUND_UP);
	div_assign_r(sum, sum, down_sc_den, ROUND_UP);
      }

      if (pinf_count == 0) {
	// Add the constraint `v >= -neg_sum', i.e., `-v <= neg_sum'.
	N double_sum = sum;
	mul2exp_assign_r(double_sum, sum, 1, ROUND_IGNORE);
	typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	typename OR_Matrix<N>::row_reference_type r_i = *i;
	assign_r(r_i[n_var+1], double_sum, ROUND_UP);
	// Deduce constraints of the form `u - v', where `u != v'.
	deduce_u_minus_v_bounds(num_var, pinf_index, sc_expr, sc_den, sum);
	// Deduce constraints of the form `-v - u', where `u != v'.
	deduce_minus_v_minus_u_bounds(num_var, pinf_index,
				      sc_expr, sc_den, sum);
      }
      else if (pinf_count == 1)
	if (expr.coefficient(Variable(pinf_index)) == denominator) {
	  // Add the constraint `v - pinf_index >= -sum',
	  // i.e., `pinf_index - v <= sum'.
	  if (pinf_index < num_var) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	    typename OR_Matrix<N>::row_reference_type r_i = *i;
	    assign_r(r_i[2*pinf_index], sum, ROUND_UP);
	  }
	  if (pinf_index > num_var) {
	    typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
	    typename OR_Matrix<N>::row_reference_type r_ci = *(i+1);
	    assign_r(r_ci[n_var], sum, ROUND_UP);
	  }
	}
	else {
	  if (expr.coefficient(Variable(pinf_index)) == minus_den) {
	    // Add the constraint `v + pinf_index >= -sum',
	    // i.e., `-pinf_index - v <= sum'.
	    if (pinf_index < num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + n_var;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[2*pinf_index+1], sum, ROUND_UP);
	    }
	    if (pinf_index > num_var) {
	      typename OR_Matrix<N>::row_iterator i = matrix.row_begin() + 2*pinf_index;
	      typename OR_Matrix<N>::row_reference_type r_i = *i;
	      assign_r(r_i[n_var+1], sum, ROUND_UP);
	    }
	  }
	}
      break;
      }

    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }

  // If the shrunk OS is empty, its preimage is empty too.
  if (is_empty())
    return;
  forget_all_octagonal_constraints(n_var);
  assert(OK());
}

template <typename T>
Constraint_System
Octagonal_Shape<T>::constraints() const {
  using Implementation::BD_Shapes::numer_denom;

  Constraint_System cs;
  if (space_dim == 0) {
    if (marked_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (marked_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    // Go through all the unary constraints in `matrix'.
    for (typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin(),
    	   i_end = matrix.row_end(); i_iter != i_end; i_iter += 2) {
      dimension_type i = i_iter.index();
      typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
      typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter+1);
      const N& c_i_ii = r_i[i+1];
      const N& c_ii_i = r_ii[i];
      // We have the unary constraints.
      N negated_c_i_ii;
      if (neg_assign_r(negated_c_i_ii, c_i_ii, ROUND_NOT_NEEDED) == V_EQ
	  && negated_c_i_ii == c_ii_i) {
	// We have a unary equality constraint.
	Variable x(i/2);
	Coefficient a;
	Coefficient b;
	numer_denom(c_ii_i, b, a);
	a *= 2;
	cs.insert(a*x == b);
      }
      else {
	// We have 0, 1 or 2 inequality constraints.
	if (!is_plus_infinity(c_i_ii)) {
	  Variable x(i/2);
	  Coefficient a;
	  Coefficient b;
	  numer_denom(c_i_ii, b, a);
	  a *= 2;
	  cs.insert(-a*x <= b);
	}
	if (!is_plus_infinity(c_ii_i)) {
	  Variable x(i/2);
	  Coefficient a;
	  Coefficient b;
	  numer_denom(c_ii_i, b, a);
	  a *= 2;
	  cs.insert(a*x <= b);
	}
      }
    }
    //  Go through all the binary constraints in `matrix'.
    for (typename OR_Matrix<N>::const_row_iterator i_iter = matrix.row_begin(),
    	   i_end = matrix.row_end(); i_iter != i_end; i_iter += 2) {
      dimension_type i = i_iter.index();
      typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
      typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter+1);
      for (dimension_type j = 0; j < i; j += 2) {
	const N& c_i_j = r_i[j];
	const N& c_ii_jj = r_ii[j+1];
	N negated_c_ii_jj;
	if (neg_assign_r(negated_c_ii_jj, c_ii_jj, ROUND_NOT_NEEDED) == V_EQ
	    && negated_c_ii_jj == c_i_j) {
	  // We have one equality constraint of the following form:
	  // ax - ay = b.
	  Variable x(j/2);
	  Variable y(i/2);
	  Coefficient a;
	  Coefficient b;
	  numer_denom(c_i_j, b, a);
	  cs.insert(a*x - a*y == b);
	}
	else {
	  // We have 0, 1 or 2 inequality constraints.
	  if (!is_plus_infinity(c_i_j)) {
	    Variable x(j/2);
	    Variable y(i/2);
	    Coefficient a;
	    Coefficient b;
	    numer_denom(c_i_j, b, a);
	    cs.insert(a*x - a*y <= b);
	  }
	  if (!is_plus_infinity(c_ii_jj)) {
	    Variable x(j/2);
	    Variable y(i/2);
	    Coefficient a;
	    Coefficient b;
	    numer_denom(c_ii_jj, b, a);
	    cs.insert(a*y - a*x <= b);
	  }
	}

	const N& c_ii_j = r_ii[j];
	const N& c_i_jj = r_i[j+1];
	// We have one equality constraint of the following form:
	// ax + ay = b.
	N negated_c_i_jj;
	if (neg_assign_r(negated_c_i_jj, c_i_jj, ROUND_NOT_NEEDED) == V_EQ
	    && negated_c_i_jj == c_ii_j) {
	  // We have one equality constraint of the following form:
	  // ax + ay = b.
	  Variable x(j/2);
	  Variable y(i/2);
	  Coefficient a;
	  Coefficient b;
	  numer_denom(c_ii_j, b, a);
	  cs.insert(a*x + a*y == b);
	}
	else {
	  // We have 0, 1 or 2 inequality constraints.
	  if (!is_plus_infinity(c_i_jj)) {
	    Variable x(j/2);
	    Variable y(i/2);
	    Coefficient a;
	    Coefficient b;
	    numer_denom(c_i_jj, b, a);
	    cs.insert(-a*x - a*y <= b);
	  }
	  if (!is_plus_infinity(c_ii_j)) {
	    Variable x(j/2);
	    Variable y(i/2);
	    Coefficient a;
	    Coefficient b;
	    numer_denom(c_ii_j, b, a);
	    cs.insert(a*x + a*y <= b);
	  }
	}
      }
    }
  }
  return cs;
}

/*! \relates Parma_Polyhedra_Library::Octagonal_Shape */
template <typename T>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Octagonal_Shape<T>& c) {
  typedef typename Octagonal_Shape<T>::coefficient_type N;
  if (c.is_universe())
    s << "true";
  else {
    // We control empty octagon.
    if (c.marked_empty())
      s << "false";
    else {
      bool first = true;
      for (typename OR_Matrix<N>::const_row_iterator i_iter = c.matrix.row_begin(),
	     i_end = c.matrix.row_end(); i_iter != i_end; i_iter += 2) {
	dimension_type i = i_iter.index();
	Variable v_i = Variable(i/2);
	typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
	typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter + 1);
	const N& c_i_ii = r_i[i+1];
	const N& c_ii_i = r_ii[i];
	N negated_c_i_ii;
	if (neg_assign_r(negated_c_i_ii, c_i_ii, ROUND_NOT_NEEDED) == V_EQ
	    && negated_c_i_ii == c_ii_i) {
	  // We will print an equality.
	  if (first)
	    first = false;
	  else
	    s << ", ";
	  // We have got an equality constraint with one Variable.
	  N half_c_ii_i;
	  // FIXME!
	  // If the bound is an even number we simply divide, otherwise
	  // we write the constraint in the form:
	  // `2*variable_i == bound'.
	  if (div2exp_assign_r(half_c_ii_i, c_ii_i, 1, ROUND_NOT_NEEDED)
	      == V_EQ)
	    s << v_i << " == " << half_c_ii_i;
	  else
	    s << " 2*" << v_i << " == " << c_ii_i;
	}
	else {
	  // We will print a non-strict inequality.
	  if (!is_plus_infinity(c_i_ii)) {
	    if (first)
	      first = false;
	    // We have got a constraint with an only Variable.
	    else
	      s << ", " ;
	    s << v_i;
	    N minus_c_i_ii;
	    neg_assign_r(minus_c_i_ii, c_i_ii, ROUND_DOWN);
	    N minus_half_c_i_ii;
	    div2exp_assign_r(minus_half_c_i_ii, minus_c_i_ii, 1, ROUND_NOT_NEEDED);
	    s << " >= " << minus_half_c_i_ii;
	  }
	  if(!is_plus_infinity(c_ii_i)) {
	    if (first)
	      first = false;
	    else
	      s << ", ";
	    s << v_i;
	    N half_c_ii_i;
	    div2exp_assign_r(half_c_ii_i, c_ii_i, 1, ROUND_UP);
	    s << " <= " << half_c_ii_i;
	  }
	}
      }

      for (typename OR_Matrix<N>::const_row_iterator i_iter = c.matrix.row_begin(),
	     i_end = c.matrix.row_end(); i_iter != i_end; i_iter += 2) {
	dimension_type i = i_iter.index();
	Variable v_i = Variable(i/2);
	typename OR_Matrix<N>::const_row_reference_type r_i = *i_iter;
	typename OR_Matrix<N>::const_row_reference_type r_ii = *(i_iter + 1);
	for (dimension_type j = 0; j < i; j += 2) {
	  Variable v_j = Variable(j/2);
	  const N& c_ii_jj = r_ii[j+1];
	  const N& c_i_j = r_i[j];
	  N negated_c_ii_jj;
	  if (neg_assign_r(negated_c_ii_jj, c_ii_jj, ROUND_NOT_NEEDED) == V_EQ
	      && negated_c_ii_jj == c_i_j) {
	    // We will print an equality.
	    if (first)
	      first = false;
	    else
	      s << ", ";
	    if (c_i_j >= 0) {
	      s << v_j << " - " << v_i << " == " << c_i_j;
	    }
	    else {
	      s << v_i << " - " << v_j << " == " << c_ii_jj;
	    }
	  }
	  else {
	    // We will print a non-strict inequality.
	    if (!is_plus_infinity(c_i_j)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      // We have got a constraint with two Variables.
	      if (c_i_j >= 0) {
		s << v_j << " - " << v_i << " <= " << c_i_j;
	      }
	      else {
		N negated_c_i_j;
		neg_assign_r(negated_c_i_j, c_i_j, ROUND_DOWN);
		s << v_i << " - " << v_j << " >= " << negated_c_i_j;
	      }
	    }
	    if (!is_plus_infinity(c_ii_jj)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      if (c_ii_jj >= 0) {
		s << v_i << " - " << v_j << " <= " << c_ii_jj;
	      }
	      else {
		N negated_c_ii_jj;
		neg_assign_r(negated_c_ii_jj, c_ii_jj, ROUND_DOWN);
		s << v_j << " - " << v_i << " >= " << negated_c_ii_jj;
	      }
	    }
	  }

	  const N& c_i_jj = r_i[j+1];
	  const N& c_ii_j = r_ii[j];
	  N negated_c_i_jj;
	  if (neg_assign_r(negated_c_i_jj, c_i_jj, ROUND_NOT_NEEDED) == V_EQ
	      && negated_c_i_jj == c_ii_j) {
	    // We will print an equality.
	    if (first)
	      first = false;
	    else
	      s << ", ";
	    s << v_j << " + " << v_i << " == " << c_ii_j;
	  }
	  else {
	    // We will print a non_strict inequality.
	    if (!is_plus_infinity(c_i_jj)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      // We have got a constraint with two Variables.
	      N negated_c_i_jj;
	      neg_assign_r(negated_c_i_jj, c_i_jj, ROUND_DOWN);
	      s << v_j << " + " << v_i << " >= " << negated_c_i_jj;
	    }
	    if (!is_plus_infinity(c_ii_j)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      s << v_j << " + " << v_i << " <= " << c_ii_j;
	    }
	  }
	}
      }
    }
  }
  return s;
}

template <typename T>
void
Octagonal_Shape<T>::ascii_dump(std::ostream& s) const {
  s << "space_dim "
    << space_dim
    << "\n";
  status.ascii_dump(s);
  s << "\n";
  matrix.ascii_dump(s);
}

template <typename T>
bool
Octagonal_Shape<T>::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> space_dim))
    return false;

  if(!status.ascii_load(s))
    return false;

  if(!matrix.ascii_load(s))
    return false;

  assert(OK());
  return true;
}

template <typename T>
bool
Octagonal_Shape<T>::OK() const {
  // Check whether the matrix is well-formed.
  if (!matrix.OK())
    return false;

  // Check whether the status information is legal.
  if (!status.OK())
    return false;

  // All empty octagons are OK.
  if (marked_empty())
    return true;

  // 0-dim universe octagon is OK.
  if (space_dim == 0)
    return true;

  // MINUS_INFINITY cannot occur at all.
  for (typename OR_Matrix<N>::const_row_iterator i = matrix.row_begin(),
	 iend = matrix.row_end(); i != iend; ++i) {
    typename OR_Matrix<N>::const_row_reference_type x_i = *i;
    dimension_type rs_i = i.row_size();
    for (dimension_type j = 0; j < rs_i; ++j)
      if (is_minus_infinity(x_i[j])) {
#ifndef NDEBUG
	using namespace Parma_Polyhedra_Library::IO_Operators;
	std::cerr << "Octagonal_Shape::"
		  << "matrix[" << i.index() << "][" << j << "] = "
		  << x_i[j] << "!"
		  << std::endl;
#endif
	return false;
      }
  }

  // On the main diagonal only PLUS_INFINITY can occur.
  for (typename OR_Matrix<N>::const_row_iterator i = matrix.row_begin(),
	 m_end = matrix.row_end(); i != m_end; ++i) {
    typename OR_Matrix<N>::const_row_reference_type r = *i;
    const N& m_i_i = r[i.index()];
    if (!is_plus_infinity(m_i_i)) {
#ifndef NDEBUG
      const dimension_type j = i.index();
      using namespace Parma_Polyhedra_Library::IO_Operators;
      std::cerr << "Octagonal_Shape::matrix[" << j << "][" << j << "] = "
		<< m_i_i << "!  (+inf was expected.)\n";
#endif
      return false;
    }
  }

  // Check whether the closure information is legal.
  if (marked_strongly_closed()) {
    Octagonal_Shape x = *this;
    x.status.reset_strongly_closed();
    x.strong_closure_assign();
    if (x.matrix != matrix) {
#ifndef NDEBUG
      std::cerr << "Octagonal_Shape is marked as strongly closed "
		<< "but it is not!\n";
#endif
      return false;
    }
  }

  // A closed octagon must be strong-coherent.
  if (marked_strongly_closed())
    if (!is_strong_coherent()) {
#ifndef NDEBUG
      std::cerr << "Octagonal_Shape is not strong-coherent!\n";
#endif
      return false;
    }

  // All checks passed.
  return true;
}


template <typename T>
void
Octagonal_Shape<T>
::throw_dimension_incompatible(const char* method,
			       const Octagonal_Shape& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
Octagonal_Shape<T>
::throw_dimension_incompatible(const char* method,
			       dimension_type required_dim) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
Octagonal_Shape<T>::throw_dimension_incompatible(const char* method,
						 const Constraint& c) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", c->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
Octagonal_Shape<T>::throw_dimension_incompatible(const char* method,
						 const Generator& g) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", g->space_dimension == " << g.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
Octagonal_Shape<T>::throw_constraint_incompatible(const char* method) const {
  std::ostringstream s;
  s << "PPL::Octagonal_Shape::" << method << ":\n"
    << "the constraint is incompatible.";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
Octagonal_Shape<T>
::throw_expression_too_complex(const char* method,
			       const Linear_Expression& e) const {
  using namespace IO_Operators;
  std::ostringstream s;
  s << "PPL::Octagonal_Shape::" << method << ":\n"
    << e << " is too complex.";
  throw std::invalid_argument(s.str());
}


template <typename T>
void
Octagonal_Shape<T>
::throw_dimension_incompatible(const char* method,
			       const char* name_row,
			       const Linear_Expression& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}


template <typename T>
void
Octagonal_Shape<T>::throw_generic(const char* method,
				  const char* reason) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Octagonal_Shape::" << method << ":\n"
    << reason;
  throw std::invalid_argument(s.str());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Octagonal_Shape_templates_hh)
