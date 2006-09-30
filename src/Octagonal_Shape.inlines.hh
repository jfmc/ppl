/* Octagonal_Shape class implementation: inline functions.
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

#ifndef PPL_Octagonal_Shape_inlines_hh
#define PPL_Octagonal_Shape_inlines_hh 1

#include "C_Polyhedron.defs.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <cassert>
#include <algorithm>

// FIXME: this is only to get access to
// Implementation::BD_Shapes::div_round_up().
#include "BD_Shape.defs.hh"

namespace Parma_Polyhedra_Library {

// FIXME: find the appropriate place for this.
/*! \relates Octagonal_Shape */
inline dimension_type
coherent_index(const dimension_type i) {
  return (i%2) ? i-1 : i+1;
}

template <typename T>
inline dimension_type
Octagonal_Shape<T>::max_space_dimension() {
  return OR_Matrix<N>::max_num_rows()/2;
}

template <typename T>
inline bool
Octagonal_Shape<T>::marked_empty() const {
  return status.test_empty();
}

template <typename T>
inline void
Octagonal_Shape<T>::set_empty() {
  status.set_empty();
  assert(OK());
  assert(is_empty());
}

template <typename T>
inline void
Octagonal_Shape<T>::set_zero_dim_univ() {
  status.set_zero_dim_univ();
}

template <typename T>
inline bool
Octagonal_Shape<T>::marked_strongly_closed() const {
  return status.test_strongly_closed();
}

template <typename T>
inline
Octagonal_Shape<T>::Octagonal_Shape(const dimension_type num_dimensions,
				    const Degenerate_Element kind)
  : matrix(num_dimensions), space_dim(num_dimensions), status() {
  if (kind == EMPTY)
    status.set_empty();
  else if (num_dimensions > 0)
    // A (non zero-dim) universe octagon is strongly closed.
    status.set_strongly_closed();
  assert(OK());
}

template <typename T>
inline
Octagonal_Shape<T>::Octagonal_Shape(const Octagonal_Shape& y)
  : matrix(y.matrix), space_dim(y.space_dim), status(y.status) {
}

template <typename T>
template <typename U>
inline
Octagonal_Shape<T>::Octagonal_Shape(const Octagonal_Shape<U>& y)
  : matrix(y.matrix), space_dim(y.space_dim), status() {
  // TODO: handle flags properly, possibly taking special cases into account.
  if (y.marked_empty())
    set_empty();
  else if (y.status.test_zero_dim_univ())
    set_zero_dim_univ();
}

template <typename T>
inline
Octagonal_Shape<T>::Octagonal_Shape(const Constraint_System& cs)
  : matrix(cs.space_dimension()),
    space_dim(cs.space_dimension()),
    status() {
  if (cs.space_dimension() > 0)
    // A (non zero-dim) universe octagon is strong closed.
    status.set_strongly_closed();
  add_constraints(cs);
}

template <typename T>
inline Octagonal_Shape<T>&
Octagonal_Shape<T>::operator=(const Octagonal_Shape& y) {
  matrix = y.matrix;
  space_dim = y.space_dim;
  status = y.status;
  return *this;
}

template <typename T>
inline
Octagonal_Shape<T>::~Octagonal_Shape() {
}

template <typename T>
inline void
Octagonal_Shape<T>::swap(Octagonal_Shape& y) {
  std::swap(matrix, y.matrix);
  std::swap(space_dim, y.space_dim);
  std::swap(status, y.status);
}

template <typename T>
inline dimension_type
Octagonal_Shape<T>::space_dimension() const {
  return space_dim;
}

template <typename T>
inline bool
Octagonal_Shape<T>::is_empty() const {
  strong_closure_assign();
  return marked_empty();
}

template <typename T>
inline bool
Octagonal_Shape<T>::is_topologically_closed() const {
  return true;
}

template <typename T>
inline void
Octagonal_Shape<T>::topological_closure_assign() {
  // Nothing to be done.
  return;
}

/*! \relates Octagonal_Shape */
template <typename T>
inline bool
operator==(const Octagonal_Shape<T>& x, const Octagonal_Shape<T>& y) {
  if (x.space_dim != y.space_dim)
    // Dimension-incompatible OSs are different.
    return false;

  // Zero-dim OSs are equal if and only if they are both empty or universe.
  if (x.space_dim == 0)
    if (x.marked_empty())
      return y.marked_empty();
    else
      return !y.marked_empty();

  x.strong_closure_assign();
  y.strong_closure_assign();
  // If one of two octagons is empty, then they are equal if and only if
  // the other octagon is empty too.
  if (x.marked_empty())
    return y.marked_empty();
  if (y.marked_empty())
    return false;
  // Strong closure is a canonical form.
  return x.matrix == y.matrix;
}

/*! \relates Octagonal_Shape */
template <typename T>
inline bool
operator!=(const Octagonal_Shape<T>& x, const Octagonal_Shape<T>& y) {
  return !(x == y);
}

template <typename T>
inline const typename Octagonal_Shape<T>::coefficient_type&
Octagonal_Shape<T>::matrix_at(const dimension_type i,
			      const dimension_type j) const {
  assert(i < matrix.num_rows() && j < matrix.num_rows());
  return (j < matrix.row_size(i))
    ? matrix[i][j]
    : matrix[coherent_index(j)][coherent_index(i)];
}

template <typename T>
inline typename Octagonal_Shape<T>::coefficient_type&
Octagonal_Shape<T>::matrix_at(const dimension_type i,
			      const dimension_type j) {
  assert(i < matrix.num_rows() && j < matrix.num_rows());
  return (j < matrix.row_size(i))
    ? matrix[i][j]
    : matrix[coherent_index(j)][coherent_index(i)];
}

template <typename T>
inline Constraint_System
Octagonal_Shape<T>::minimized_constraints() const {
  strong_reduction_assign();
  return constraints();
}

template <typename T>
inline void
Octagonal_Shape<T>
::add_octagonal_constraint(const dimension_type i,
			   const dimension_type j,
			   N k) {
  // Private method: the caller has to ensure the following.
#ifndef NDEBUG
  assert(i < 2*space_dim && j < 2*space_dim && i != j);
  typename OR_Matrix<N>::row_iterator m_i = matrix.row_begin() + i;
  assert(j < m_i.row_size());
#endif
  N& r_i_j = matrix[i][j];
  if (r_i_j > k) {
    r_i_j = k;
    if (marked_strongly_closed())
      status.reset_strongly_closed();
  }
}

template <typename T>
inline void
Octagonal_Shape<T>
::add_octagonal_constraint(const dimension_type i,
			   const dimension_type j,
			   Coefficient_traits::const_reference num,
			   Coefficient_traits::const_reference den) {
  using Implementation::BD_Shapes::div_round_up;
#ifndef NDEBUG
  // Private method: the caller has to ensure the following.
  assert(i < 2*space_dim && j < 2*space_dim && i != j);
  typename OR_Matrix<N>::row_iterator m_i = matrix.row_begin() + i;
  assert(j < m_i.row_size());
  assert(den != 0);
#endif
  N k;
  div_round_up(k, num, den);
  add_octagonal_constraint(i, j, k);
}

template <typename T>
inline void
Octagonal_Shape<T>
::forget_all_octagonal_constraints(const dimension_type v_id) {
  assert(v_id < space_dim);
  const dimension_type n_v = 2*v_id;
  typename OR_Matrix<N>::row_iterator m_iter = matrix.row_begin() + n_v;
  typename OR_Matrix<N>::row_reference_type r_v = *m_iter;
  typename OR_Matrix<N>::row_reference_type r_cv = *(++m_iter);
  for (dimension_type h = m_iter.row_size(); h-- > 0; ) {
    r_v[h] = PLUS_INFINITY;
    r_cv[h] = PLUS_INFINITY;
  }
  ++m_iter;
  for (typename OR_Matrix<N>::row_iterator m_end = matrix.row_end();
       m_iter != m_end; ++m_iter) {
    typename OR_Matrix<N>::row_reference_type r = *m_iter;
    r[n_v] = PLUS_INFINITY;
    r[n_v+1] = PLUS_INFINITY;
  }
}

template <typename T>
inline void
Octagonal_Shape<T>
::forget_binary_octagonal_constraints(const dimension_type v_id) {
  assert(v_id < space_dim);
  const dimension_type n_v = 2*v_id;
  typename OR_Matrix<N>::row_iterator m_iter = matrix.row_begin() + n_v;
  typename OR_Matrix<N>::row_reference_type r_v = *m_iter;
  typename OR_Matrix<N>::row_reference_type r_cv = *(++m_iter);
  for (dimension_type k = n_v; k-- > 0; ) {
    r_v[k] = PLUS_INFINITY;
    r_cv[k] = PLUS_INFINITY;
  }
  ++m_iter;
  for (typename OR_Matrix<N>::row_iterator m_end = matrix.row_end();
       m_iter != m_end; ++m_iter) {
    typename OR_Matrix<N>::row_reference_type r = *m_iter;
    r[n_v] = PLUS_INFINITY;
    r[n_v+1] = PLUS_INFINITY;
  }
}

template <typename T>
inline void
Octagonal_Shape<T>::add_constraints(const Constraint_System& cs) {
  Constraint_System::const_iterator i_end = cs.end();
  for (Constraint_System::const_iterator i = cs.begin(); i != i_end; ++i)
    add_constraint(*i);
}

template <typename T>
inline bool
Octagonal_Shape<T>::add_constraints_and_minimize(const Constraint_System& cs) {
  add_constraints(cs);
  strong_closure_assign();
  return !marked_empty();
}

template <typename T>
inline void
Octagonal_Shape<T>
::remove_higher_space_dimensions(const dimension_type new_dimension) {
  // Dimension-compatibility check.
  if (new_dimension > space_dim)
    throw_dimension_incompatible("remove_higher_space_dimension(nd)",
				 new_dimension);
  // The removal of no dimensions from any octagon is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from an octagon in a 0-dim space.
  if (new_dimension == space_dim) {
    assert(OK());
    return;
  }

  strong_closure_assign();
  matrix.shrink(new_dimension);
  // When we remove all dimensions from a non-empty octagon,
  // we obtain the zero-dimensional universe octagon.
  if (new_dimension == 0 && !marked_empty())
    set_zero_dim_univ();
  space_dim = new_dimension;
  assert(OK());
}

template <typename T>
inline bool
Octagonal_Shape<T>
::intersection_assign_and_minimize(const Octagonal_Shape& y) {
  intersection_assign(y);
  strong_closure_assign();
  return !(marked_empty());
}

template <typename T>
inline void
Octagonal_Shape<T>::CC76_extrapolation_assign(const Octagonal_Shape& y,
					      unsigned* tp) {
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
Octagonal_Shape<T>::time_elapse_assign(const Octagonal_Shape& y) {
  // Dimension-compatibility check.
  if (space_dimension() != y.space_dimension())
    throw_dimension_incompatible("time_elapse_assign(y)", y);
  // See the polyhedra documentation.
  C_Polyhedron px(constraints());
  C_Polyhedron py(y.constraints());
  px.time_elapse_assign(py);
  Octagonal_Shape<T> x(px);
  swap(x);
  assert(OK());
}

template <typename T>
inline bool
Octagonal_Shape<T>::add_constraint_and_minimize(const Constraint& c) {
  bool was_closed = marked_strongly_closed();
  add_constraint(c);
  // If the OS was strongly closed and we add a single constraint,
  // it is convenient to use the incremental strong-closure algorithm,
  // as we known that the constraint has affected two variables at most.
  // (The cost is O(n^2) instead of O(n^3).)
  if (was_closed)
    for (dimension_type i = c.space_dimension(); i-- > 0;)
      if (c.coefficient(Variable(i)) != 0) {
	incremental_strong_closure_assign(Variable(i));
	break;
      }
  else
    strong_closure_assign();
  return !(marked_empty());
}

template <typename T>
inline bool
Octagonal_Shape<T>::strictly_contains(const Octagonal_Shape& y) const {
  const Octagonal_Shape<T>& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename T>
inline bool
Octagonal_Shape<T>::oct_hull_assign_and_minimize(const Octagonal_Shape& y) {
  oct_hull_assign(y);
  return !marked_empty();
}


template <typename T>
inline void
Octagonal_Shape<T>::upper_bound_assign(const Octagonal_Shape& y) {
  oct_hull_assign(y);
}

template <typename T>
inline bool
Octagonal_Shape<T>::oct_hull_assign_if_exact(const Octagonal_Shape&) {
  // TODO: this must be properly implemented.
  return false;
}

template <typename T>
inline bool
Octagonal_Shape<T>::upper_bound_assign_if_exact(const Octagonal_Shape& y) {
  return oct_hull_assign_if_exact(y);
}

template <typename T>
inline void
Octagonal_Shape<T>::difference_assign(const Octagonal_Shape& y) {
  oct_difference_assign(y);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Octagonal_Shape<T>& x,
			    const Octagonal_Shape<T>& y,
			    const Rounding_Dir dir,
			    Temp& tmp0,
			    Temp& tmp1,
			    Temp& tmp2) {
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    return false;

  // Zero-dim OSs are equal if and only if they are both empty or universe.
  if (x.space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires strong closure.
  x.strong_closure_assign();
  y.strong_closure_assign();

  // If one of two OSs is empty, then they are equal if and only if
  // the other OS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return rectilinear_distance_assign(r, x.matrix, y.matrix, dir,
				     tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Octagonal_Shape<T>& x,
			    const Octagonal_Shape<T>& y,
			    const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return rectilinear_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Octagonal_Shape<T>& x,
			    const Octagonal_Shape<T>& y,
			    const Rounding_Dir dir) {
  return rectilinear_distance_assign<To, To, T>(r, x, y, dir);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Octagonal_Shape<T>& x,
			  const Octagonal_Shape<T>& y,
			  const Rounding_Dir dir,
			  Temp& tmp0,
			  Temp& tmp1,
			  Temp& tmp2) {
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    return false;

  // Zero-dim OSs are equal if and only if they are both empty or universe.
  if (x.space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires strong closure.
  x.strong_closure_assign();
  y.strong_closure_assign();

  // If one of two OSs is empty, then they are equal if and only if
  // the other OS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return euclidean_distance_assign(r, x.matrix, y.matrix, dir,
				   tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Octagonal_Shape<T>& x,
			  const Octagonal_Shape<T>& y,
			  const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return euclidean_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Octagonal_Shape<T>& x,
			  const Octagonal_Shape<T>& y,
			  const Rounding_Dir dir) {
  return euclidean_distance_assign<To, To, T>(r, x, y, dir);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Octagonal_Shape<T>& x,
			   const Octagonal_Shape<T>& y,
			   const Rounding_Dir dir,
			   Temp& tmp0,
			   Temp& tmp1,
			   Temp& tmp2) {
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    return false;

  // Zero-dim OSs are equal if and only if they are both empty or universe.
  if (x.space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires strong closure.
  x.strong_closure_assign();
  y.strong_closure_assign();

  // If one of two OSs is empty, then they are equal if and only if
  // the other OS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return l_infinity_distance_assign(r, x.matrix, y.matrix, dir,
				    tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename Temp, typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Octagonal_Shape<T>& x,
			   const Octagonal_Shape<T>& y,
			   const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return l_infinity_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Octagonal_Shape */
template <typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Octagonal_Shape<T>& x,
			   const Octagonal_Shape<T>& y,
			   const Rounding_Dir dir) {
  return l_infinity_distance_assign<To, To, T>(r, x, y, dir);
}

template <typename T>
inline memory_size_type
Octagonal_Shape<T>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Octagonal_Shape */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::Octagonal_Shape<T>& x,
     Parma_Polyhedra_Library::Octagonal_Shape<T>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Octagonal_Shape_inlines_hh)
