/* Octagon class implementation: inline functions.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Octagon_inlines_hh
#define PPL_Octagon_inlines_hh 1

#include "C_Polyhedron.defs.hh"
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
// Implementation::BD_Shapes::div_round_up().
#include "BD_Shape.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
inline dimension_type
Octagon<T>::max_space_dimension() {
  return OR_Matrix<N>::max_num_rows()/2;
}

template <typename T>
inline
Octagon<T>::Octagon(const dimension_type num_dimensions,
		    const Degenerate_Element kind)
  : matrix(num_dimensions), space_dim(num_dimensions), status() {
  if (kind == EMPTY)
    status.set_empty();
  else if (num_dimensions > 0) {
    // A (non zero-dim) universe octagon is strongly closed.
    status.set_strongly_closed();
  }
  assert(OK());
}

template <typename T>
inline
Octagon<T>::Octagon(const Octagon& y)
  : matrix(y.matrix), space_dim(y.space_dim), status(y.status) {
}

template <typename T>
inline
Octagon<T>::Octagon(const Constraint_System& cs)
  : matrix(cs.space_dimension()),
    space_dim(cs.space_dimension()) {
  if (cs.space_dimension() > 0)
    // A (non zero-dim) universe octagon is strong closed.
    status.set_strongly_closed();
  add_constraints(cs);
  assert(OK());
}

template <typename T>
inline Octagon<T>&
Octagon<T>::operator=(const Octagon& y) {
  matrix = y.matrix;
  space_dim = y.space_dim;
  status = y.status;
  return *this;
}

template <typename T>
inline
Octagon<T>::~Octagon() {
}

template <typename T>
inline void
Octagon<T>::swap(Octagon& y) {
  std::swap(matrix, y.matrix);
  std::swap(space_dim, y.space_dim);
  std::swap(status, y.status);
}

template <typename T>
inline bool
Octagon<T>::marked_empty() const {
  return status.test_empty();
}

template <typename T>
inline bool
Octagon<T>::marked_strongly_closed() const {
  return status.test_strongly_closed();
}

template <typename T>
inline dimension_type
Octagon<T>::space_dimension() const {
  return space_dim;
}

template <typename T>
inline bool
operator==(const Octagon<T>& x, const Octagon<T>& y) {
  if (x.space_dim != y.space_dim)
    // If the two octagons are dimension-incompatible, then they cannot be
    // the same octagon.
    return false;

  // Zero-dim octagons are equal if and only if they are both empty or universe.
  if (x.space_dim == 0) {
    if (x.marked_empty())
      return y.marked_empty();
    else
      return !y.marked_empty();
  }

  x.strong_closure_assign();
  y.strong_closure_assign();
  // If one of two octagons is empty, then they are equal if and only if
  // the other octagon is empty too.
  if (x.marked_empty())
    return y.marked_empty();
  if (y.marked_empty())
    return false;

  return x.matrix == y.matrix;
}

template <typename T>
inline bool
operator!=(const Octagon<T>& x, const Octagon<T>& y) {
  return !(x == y);
}

template <typename T>
inline void
Octagon<T>::set_empty() {
  status.set_empty();
  assert(OK());
  assert(is_empty());
}

template <typename T>
inline void
Octagon<T>::set_zero_dim_univ() {
  status.set_zero_dim_univ();
}

template <typename T>
inline void
strong_coherence_local_step(T& result, const T& a, const T& b, const T& c) {
  // This is an auxiliary function used in the method strong_closure_assign()
  // to realize a step of local strong-coherence.
  // It assigns to `result':
  // c,                if `a' or/and `b' are plus infinity;
  // min(c, (a+b)/2),  otherwise.
  if (is_plus_infinity(a) ||
      is_plus_infinity(b))
    result = c;
  else {
    // Compute (a+b)/2 into `result', rounding the result towards plus infinity.
    T sum;
    add_assign_r(sum, a, b, ROUND_UP);
    div2exp_assign_r(result, sum, 1, ROUND_UP);
    Implementation::BD_Shapes::min_assign(result, c);
  }
}

inline dimension_type
coherent_index(const dimension_type& i) {
  return (i%2) ? i-1 : i+1;
}

template <typename T>
inline const T&
position_cell(const OR_Matrix<T>& mat, const dimension_type i,
	      const dimension_type j) {
  return (j < mat.row_size(i)) ? mat[i][j]
    : mat[coherent_index(j)][coherent_index(i)];
}

template <typename T>
inline T&
position_cell(OR_Matrix<T>& mat, const dimension_type i,
	      const dimension_type j) {
  return (j < mat.row_size(i)) ? mat[i][j]
    : mat[coherent_index(j)][coherent_index(i)];
}

template <typename T>
inline bool
change(bool changed, T& cell, T coeff) {
  changed = false;
  if (cell > coeff) {
    cell = coeff;
    changed = true;
  }
  return changed;
}

template <typename T>
inline Constraint_System
Octagon<T>::minimized_constraints() const {
  strong_reduction_assign();
  return constraints();
}

template <typename T>
inline void
Octagon<T>::add_octagonal_constraint(typename OR_Matrix<N>::row_iterator i,
				     const dimension_type j,
				     N k) {
  using Implementation::BD_Shapes::div_round_up;

  // Private method: the caller has to ensure the following.
  assert(i.index() < 2*space_dim && j < i.row_size() && i.index() != j);
  typename OR_Matrix<N>::row_reference_type r = *i;
  N& r_j = r[j];
  if (r_j > k) {
    r_j = k;
    if (marked_strongly_closed())
      status.reset_strongly_closed();
  }
  assert(OK());
}

template <typename T>
inline void
Octagon<T>::add_octagonal_constraint(typename OR_Matrix<N>::row_iterator i,
				     const dimension_type j,
				     Coefficient_traits::const_reference num,
				     Coefficient_traits::const_reference den) {
  using Implementation::BD_Shapes::div_round_up;

  // Private method: the caller has to ensure the following.
  assert(i.index() < 2*space_dim && j < i.row_size() && i.index() != j);
  assert(den != 0);
  N k;
  div_round_up(k, num, den);
  add_octagonal_constraint(i, j, k);
}

template <typename T>
inline void
Octagon<T>::forget_all_octagonal_constraints(typename OR_Matrix<N>::row_iterator i,
					     const dimension_type v) {
  assert(v < 2*space_dim && i.index() < 2*space_dim);
  typename OR_Matrix<N>::row_reference_type r_i = *i;
  typename OR_Matrix<N>::row_reference_type r_ii = *(++i);
  for (dimension_type h = matrix.row_size(v); h-- > 0; ) {
    r_i[h] = PLUS_INFINITY;
    r_ii[h] = PLUS_INFINITY;
  }
  ++i;
  for (typename OR_Matrix<N>::row_iterator iend = matrix.row_end();
       i != iend; ++i) {
    typename OR_Matrix<N>::row_reference_type r = *i;
    r[v] = PLUS_INFINITY;
    r[v+1] = PLUS_INFINITY;
  }
}

template <typename T>
inline void
Octagon<T>::add_constraints(const Constraint_System& cs) {
  // This method not preserve closure.
  // Seen add_constraint().
  Constraint_System::const_iterator iend = cs.end();
  for (Constraint_System::const_iterator i = cs.begin(); i != iend; ++i)
    add_constraint(*i);
  assert(OK());
}

template <typename T>
inline bool
Octagon<T>::add_constraints_and_minimize(const Constraint_System& cs) {
  add_constraints(cs);
  strong_closure_assign();
  return !(marked_empty());
}

template <typename T>
inline void
Octagon<T>::remove_higher_space_dimensions(const dimension_type new_dimension) {
  // Dimension-compatibility check.
  if (new_dimension > space_dim)
    throw_dimension_incompatible("remove_higher_space_dimension(nd)",
				 new_dimension);
  // The removal of no dimensions from any octagon is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from an octagon in a 0-dim space.
  if(new_dimension == space_dim) {
    assert(OK());
    return;
  }

  strong_closure_assign();
  matrix.remove_rows(2*new_dimension);
  // When we remove all dimensions from a non-empty octagon,
  // we obtain the zero-dimensional universe octagon.
  if (new_dimension == 0 && !marked_empty())
    set_zero_dim_univ();
  space_dim = new_dimension;
  assert(OK());
}

template <typename T>
inline bool
Octagon<T>::intersection_assign_and_minimize(const Octagon& y) {
  intersection_assign(y);
  strong_closure_assign();
  return !(marked_empty());
}

template <typename T>
inline void
Octagon<T>::CC76_extrapolation_assign(const Octagon& y, unsigned* tp) {
  static N stop_points[] = {
    N(-2, ROUND_UP),
    N(-1, ROUND_UP),
    N( 0, ROUND_UP),
    N( 1, ROUND_UP),
    N( 2, ROUND_UP)
  };
  CC76_extrapolation_assign(y,
			    stop_points, 
			    stop_points
			    + sizeof(stop_points)/sizeof(stop_points[0]),
			    tp);
}

template <typename T>
inline void
Octagon<T>::time_elapse_assign(const Octagon& y) {
  // Dimension-compatibility check.
  if (space_dimension() != y.space_dimension())
    throw_dimension_incompatible("time_elapse_assign(y)", y);
  // Seen the polyhedra documentation.
  C_Polyhedron px(constraints());
  C_Polyhedron py(y.constraints());
  px.time_elapse_assign(py);
  Octagon x(px.constraints());
  swap(x);
  assert(OK());
}

template <typename T>
inline bool
Octagon<T>::add_constraint_and_minimize(const Constraint& c) {
  bool was_closed = marked_strongly_closed();
  add_constraint(c);
  // `*this' was closed and we add an only constraint to `*this',
  // thus it is many convenient to use the incremental-closure,
  // instead of the closure, because it cost only O(n2) against
  // O(n3).
  if (was_closed)
    for (dimension_type i = c.space_dimension(); i-- > 0;) {
      Variable var = Variable(i);
      if (c.coefficient(var) != 0) {
	// If the constraint `c' isn't a bounded difference
	// or the constraint `c' doesn't change `*this',
	// the incremental-closure does nothing.
	incremental_strong_closure_assign(var);
	break;
      }
    }
  else
    strong_closure_assign();
  return !(marked_empty());
}

template <typename T>
inline bool
Octagon<T>::is_empty() const {
  strong_closure_assign();
  return marked_empty();
}

template <typename T>
inline bool
Octagon<T>::strictly_contains(const Octagon& y) const {
  const Octagon<T>& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename T>
inline bool
Octagon<T>::oct_hull_assign_and_minimize(const Octagon& y) {
  oct_hull_assign(y);
  return !marked_empty();
}


template <typename T>
inline void
Octagon<T>::upper_bound_assign(const Octagon& y) {
  oct_hull_assign(y);
}

template <typename T>
inline bool
Octagon<T>::oct_hull_assign_if_exact(const Octagon&) {
  // TODO: this must be properly implemented.
  return false;
}

template <typename T>
inline bool
Octagon<T>::upper_bound_assign_if_exact(const Octagon& y) {
  return oct_hull_assign_if_exact(y);
}

template <typename T>
inline void
Octagon<T>::difference_assign(const Octagon& y) {
  oct_difference_assign(y);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Octagon */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::Octagon<T>& x,
     Parma_Polyhedra_Library::Octagon<T>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Octagon_inlines_hh)
