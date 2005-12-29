/* BD_Shape class implementation: inline functions.
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

#ifndef PPL_BD_Shape_inlines_hh
#define PPL_BD_Shape_inlines_hh 1

#include "C_Polyhedron.defs.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "LP_Problem.defs.hh"
#include <cassert>
#include <vector>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <algorithm>

namespace Parma_Polyhedra_Library {

namespace Implementation {
namespace BD_Shapes {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Extract the numerator and denominator components of \p from.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline void
numer_denom(const Checked_Number<T, Policy>& from,
	    Coefficient& num, Coefficient& den) {
  assert(!is_not_a_number(from)
	 && !is_minus_infinity(from)
	 && !is_plus_infinity(from));
  mpq_class q;
  assign(q, raw_value(from), ROUND_NOT_NEEDED);
  num = q.get_num();
  den = q.get_den();
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Divides \p x by \p y into \p to, rounding the result towards plus infinity.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline void
div_round_up(Checked_Number<T, Policy>& to,
	     Coefficient_traits::const_reference x,
	     Coefficient_traits::const_reference y) {
  mpq_class qx;
  mpq_class qy;
  // Note: this code assumes that a Coefficient is always convertible
  // to an mpq_class without loss of precision.
  assign(qx, raw_value(x), ROUND_NOT_NEEDED);
  assign(qy, raw_value(y), ROUND_NOT_NEEDED);
  assign_div(qx, qx, qy, ROUND_NOT_NEEDED);
  assign(to, qx, ROUND_UP);
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the minimum between \p x and \p y.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename N>
inline void
assign_min(N& x, const N& y) {
  if (x > y)
    x = y;
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the maximum between \p x and \p y.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename N>
inline void
assign_max(N& x, const N& y) {
  if (x < y)
    x = y;
}

} // namespace BD_Shapes
} // namespace Implementation


template <typename T>
inline dimension_type
BD_Shape<T>::max_space_dimension() {
  // One dimension is reserved to have a value of type dimension_type
  // that does not represent a legal dimension.
  return std::min(DB_Matrix<N>::max_num_rows() - 1,
		  DB_Matrix<N>::max_num_columns() - 1);
}

template <typename T>
inline bool
BD_Shape<T>::marked_empty() const {
  return status.test_empty();
}

template <typename T>
inline void
BD_Shape<T>::set_empty() {
  status.set_empty();
  assert(OK());
  assert(marked_empty());
}

template <typename T>
inline void
BD_Shape<T>::set_zero_dim_univ() {
  status.set_zero_dim_univ();
}

template <typename T>
inline bool
BD_Shape<T>::marked_shortest_path_closed() const {
  return status.test_shortest_path_closed();
}

template <typename T>
inline bool
BD_Shape<T>::marked_shortest_path_reduced() const {
  return status.test_shortest_path_reduced();
}

template <typename T>
inline
BD_Shape<T>::BD_Shape(const dimension_type num_dimensions,
		      const Degenerate_Element kind)
  : dbm(num_dimensions + 1), status(), redundancy_dbm() {
  if (kind == EMPTY)
    set_empty();
  else {
    if (num_dimensions > 0)
      // A (non zero-dim) universe BDS is closed.
      status.set_shortest_path_closed();
  }
  assert(OK());
}

template <typename T>
inline
BD_Shape<T>::BD_Shape(const BD_Shape& y)
  : dbm(y.dbm), status(y.status), redundancy_dbm() {
  if (y.marked_shortest_path_reduced())
    redundancy_dbm = y.redundancy_dbm;
}

template <typename T>
template <typename U>
inline
BD_Shape<T>::BD_Shape(const BD_Shape<U>& y)
  : dbm(y.dbm), status(), redundancy_dbm() {
  // TODO: handle flags properly, possibly taking special cases into account.
  if (y.marked_empty())
    set_empty();
  else if (y.status.test_zero_dim_univ())
    set_zero_dim_univ();
}

template <typename T>
inline bool
BD_Shape<T>::add_constraint_and_minimize(const Constraint& c) {
  add_constraint(c);
  shortest_path_closure_assign();
  return !marked_empty();
}

template <typename T>
inline void
BD_Shape<T>::add_constraints(const Constraint_System& cs) {
  for (Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i)
    add_constraint(*i);
  assert(OK());
}

template <typename T>
inline bool
BD_Shape<T>::add_constraints_and_minimize(const Constraint_System& cs) {
  add_constraints(cs);
  shortest_path_closure_assign();
  return !marked_empty();
}

template <typename T>
inline
BD_Shape<T>::BD_Shape(const Constraint_System& cs)
  : dbm(cs.space_dimension() + 1), status(), redundancy_dbm() {
  if (cs.space_dimension() > 0)
    // A (non zero-dim) universe BDS is shortest-path closed.
    status.set_shortest_path_closed();
  add_constraints(cs);
  assert(OK());
}

template <typename T>
inline dimension_type
BD_Shape<T>::affine_dimension() const {
  const dimension_type space_dim = space_dimension();

  // Shortest-path closure is necessary to detect emptiness
  // and all (possibly implicit) equalities.
  shortest_path_closure_assign();
  if (marked_empty())
    return 0;

  // The vector `predecessor' is used to represent equivalence classes:
  // `predecessor[i] == i' if and only if `i' is the leader of its
  // equivalence class (i.e., the minimum index in the class);
  std::vector<dimension_type> predecessor;
  compute_predecessors(predecessor);

  // Due to the fictitious variable `0', the affine dimension is one
  // less the number of equivalence classes.
  dimension_type affine_dim = 0;
  // Note: disregard the first equivalence class.
  for (dimension_type i = 1; i <= space_dim; ++i)
    if (predecessor[i] == i)
      ++affine_dim;

  return affine_dim;
}

template <typename T>
inline BD_Shape<T>&
BD_Shape<T>::operator=(const BD_Shape& y) {
  dbm = y.dbm;
  status = y.status;
  if (y.marked_shortest_path_reduced())
    redundancy_dbm = y.redundancy_dbm;
  return *this;
}

template <typename T>
inline
BD_Shape<T>::~BD_Shape() {
}

template <typename T>
inline void
BD_Shape<T>::swap(BD_Shape& y) {
  std::swap(dbm, y.dbm);
  std::swap(status, y.status);
  std::swap(redundancy_dbm, y.redundancy_dbm);
}

template <typename T>
inline dimension_type
BD_Shape<T>::space_dimension() const {
  return dbm.num_rows() - 1;
}

template <typename T>
inline bool
BD_Shape<T>::is_empty() const {
  shortest_path_closure_assign();
  return marked_empty();
}

template <typename T>
inline bool
operator==(const BD_Shape<T>& x, const BD_Shape<T>& y) {
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // Zero-dim BDSs are equal if and only if they are both empty or universe.
  if (x_space_dim == 0)
    if (x.marked_empty())
      return y.marked_empty();
    else
      return !y.marked_empty();

  // The exact equivalence test requires shortest-path closure.
  x.shortest_path_closure_assign();
  y.shortest_path_closure_assign();

  // If one of two BDSs is empty, then they are equal
  // if and only if the other BDS is empty too.
  if (x.marked_empty())
    return y.marked_empty();
  if (y.marked_empty())
    return false;
  // Check for syntactic equivalence of the two (shortest-path closed)
  // systems of bounded differences.
  return x.dbm == y.dbm;
}

template <typename T>
inline bool
operator!=(const BD_Shape<T>& x, const BD_Shape<T>& y) {
  return !(x == y);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const BD_Shape<T>& x,
			    const BD_Shape<T>& y,
			    const Rounding_Dir dir,
			    Temp& tmp0,
			    Temp& tmp1,
			    Temp& tmp2) {
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // Zero-dim BDSs are equal if and only if they are both empty or universe.
  if (x_space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires shortest-path closure.
  x.shortest_path_closure_assign();
  y.shortest_path_closure_assign();

  // If one of two BDSs is empty, then they are equal if and only if
  // the other BDS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return rectilinear_distance_assign(r, x.dbm, y.dbm, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const BD_Shape<T>& x,
			    const BD_Shape<T>& y,
			    const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return rectilinear_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename To, typename T>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const BD_Shape<T>& x,
			    const BD_Shape<T>& y,
			    const Rounding_Dir dir) {
  return rectilinear_distance_assign<To, To, T>(r, x, y, dir);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const BD_Shape<T>& x,
			  const BD_Shape<T>& y,
			  const Rounding_Dir dir,
			  Temp& tmp0,
			  Temp& tmp1,
			  Temp& tmp2) {
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // Zero-dim BDSs are equal if and only if they are both empty or universe.
  if (x_space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires shortest-path closure.
  x.shortest_path_closure_assign();
  y.shortest_path_closure_assign();

  // If one of two BDSs is empty, then they are equal if and only if
  // the other BDS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return euclidean_distance_assign(r, x.dbm, y.dbm, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const BD_Shape<T>& x,
			  const BD_Shape<T>& y,
			  const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return euclidean_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename To, typename T>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const BD_Shape<T>& x,
			  const BD_Shape<T>& y,
			  const Rounding_Dir dir) {
  return euclidean_distance_assign<To, To, T>(r, x, y, dir);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const BD_Shape<T>& x,
			   const BD_Shape<T>& y,
			   const Rounding_Dir dir,
			   Temp& tmp0,
			   Temp& tmp1,
			   Temp& tmp2) {
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // Zero-dim BDSs are equal if and only if they are both empty or universe.
  if (x_space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
    return true;
  }

  // The distance computation requires shortest-path closure.
  x.shortest_path_closure_assign();
  y.shortest_path_closure_assign();

  // If one of two BDSs is empty, then they are equal if and only if
  // the other BDS is empty too.
  if (x.marked_empty() ||  y.marked_empty()) {
   if (x.marked_empty() == y.marked_empty())
      assign(r, 0, ROUND_NOT_NEEDED);
    else
      r = PLUS_INFINITY;
   return true;
  }

  return l_infinity_distance_assign(r, x.dbm, y.dbm, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename Temp, typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const BD_Shape<T>& x,
			   const BD_Shape<T>& y,
			   const Rounding_Dir dir) {
  static Checked_Number<Temp, Extended_Number_Policy> tmp0;
  static Checked_Number<Temp, Extended_Number_Policy> tmp1;
  static Checked_Number<Temp, Extended_Number_Policy> tmp2;
  return l_infinity_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates BD_Shape */
template <typename To, typename T>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const BD_Shape<T>& x,
			   const BD_Shape<T>& y,
			   const Rounding_Dir dir) {
  return l_infinity_distance_assign<To, To, T>(r, x, y, dir);
}

template <typename T>
inline void
BD_Shape<T>::add_dbm_constraint(const dimension_type i,
				const dimension_type j,
				N k,
				Coefficient_traits::const_reference den) {
  // Private method: the caller has to ensure the following.
  assert(i <= space_dimension() && j <= space_dimension() && i != j);
  assert(den != 0);
  if (den != 1) {
    N d;
    assign(d, raw_value(-den), ROUND_UP);
    assign_neg(d, d, ROUND_UP);
    assign_div(k, k, d, ROUND_UP);
  }
  N& dbm_ij = dbm[i][j];
  if (dbm_ij > k) {
    dbm_ij = k;
    if (marked_shortest_path_closed())
      status.reset_shortest_path_closed();
  }
  assert(OK());
}

template <typename T>
inline void
BD_Shape<T>::add_dbm_constraint(const dimension_type i,
				const dimension_type j,
				Coefficient_traits::const_reference num,
				Coefficient_traits::const_reference den) {
  // Private method: the caller has to ensure the following.
  assert(i <= space_dimension() && j <= space_dimension() && i != j);
  assert(den != 0);
  N k;
  Implementation::BD_Shapes::div_round_up(k, num, den);
  add_dbm_constraint(i, j, k);
}

template <typename T>
inline bool
BD_Shape<T>::strictly_contains(const BD_Shape& y) const {
  const BD_Shape<T>& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename T>
inline bool
BD_Shape<T>::bds_hull_assign_and_minimize(const BD_Shape& y) {
  bds_hull_assign(y);
  assert(marked_empty()
	 || space_dimension() == 0 || marked_shortest_path_closed());
  return !marked_empty();
}

template <typename T>
inline void
BD_Shape<T>::upper_bound_assign(const BD_Shape& y) {
  bds_hull_assign(y);
}

template <typename T>
inline bool
BD_Shape<T>::bds_hull_assign_if_exact(const BD_Shape&) {
  // TODO: this must be properly implemented.
  return false;
}

template <typename T>
inline bool
BD_Shape<T>::upper_bound_assign_if_exact(const BD_Shape& y) {
  return bds_hull_assign_if_exact(y);
}

template <typename T>
inline void
BD_Shape<T>::difference_assign(const BD_Shape& y) {
  bds_difference_assign(y);
}

template <typename T>
inline void
BD_Shape<T>::remove_higher_space_dimensions(const dimension_type new_dim) {
  // Dimension-compatibility check: the variable having
  // maximum index is the one occurring last in the set.
  if (new_dim > space_dimension())
    throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				 new_dim);

  // The removal of no dimensions from any BDS is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a zero-dim space BDS.
  if (new_dim == space_dimension()) {
    assert(OK());
    return;
  }

  // Shortest-path closure is necessary as in remove_space_dimensions().
  shortest_path_closure_assign();
  dbm.resize_no_copy(new_dim + 1);

  // Shortest-path closure is maintained.
  // TODO: see whether or not reduction can be (efficiently!) maintained too.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();

  // If we removed _all_ dimensions from a non-empty BDS,
  // the zero-dim universe BDS has been obtained.
  if (new_dim == 0 && !marked_empty())
    set_zero_dim_univ();
  assert(OK());
}

template <typename T>
inline bool
BD_Shape<T>::intersection_assign_and_minimize(const BD_Shape& y) {
  intersection_assign(y);
  shortest_path_closure_assign();
  return !marked_empty();
}

template <typename T>
inline void
BD_Shape<T>::CC76_extrapolation_assign(const BD_Shape& y) {
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
			    + sizeof(stop_points)/sizeof(stop_points[0]));
}

template <typename T>
inline void
BD_Shape<T>::H79_widening_assign(const BD_Shape& y, unsigned* tp) {
  // See the documentation for polyhedra.
  C_Polyhedron px(constraints());
  C_Polyhedron py(y.constraints());
  px.H79_widening_assign(py, tp);
  BD_Shape x(px);
  swap(x);
  assert(OK());
}

template <typename T>
inline void
BD_Shape<T>::limited_H79_extrapolation_assign(const BD_Shape& y,
					      const Constraint_System& cs,
					      unsigned* tp) {
  // See the documentation for polyhedra.
  C_Polyhedron px(constraints());
  C_Polyhedron py(y.constraints());
  px.limited_H79_extrapolation_assign(py, cs, tp);
  BD_Shape x(px);
  swap(x);
  assert(OK());
}

template <typename T>
inline void
BD_Shape<T>::time_elapse_assign(const BD_Shape& y) {
  // Dimension-compatibility check.
  if (space_dimension() != y.space_dimension())
    throw_dimension_incompatible("time_elapse_assign(y)", y);
  // See the documentation for polyhedra.
  C_Polyhedron px(constraints());
  C_Polyhedron py(y.constraints());
  px.time_elapse_assign(py);
  BD_Shape x(px);
  swap(x);
  assert(OK());
}

template <typename T>
inline void
BD_Shape<T>::ascii_dump(std::ostream& s) const {
  status.ascii_dump(s);
  s << "\n";
  dbm.ascii_dump(s);
  // Redundancy info.
  s << "\n";
  const char separator = ' ';
  const dimension_type nrows = redundancy_dbm.size();
  s << nrows << separator << "\n";
  for (dimension_type i = 0; i < nrows;  ++i) {
    for (dimension_type j = 0; j < nrows; ++j)
      s << redundancy_dbm[i][j] << separator;
    s << "\n";
  }
}

template <typename T>
inline bool
BD_Shape<T>::ascii_load(std::istream& s) {
  if (!status.ascii_load(s))
    return false;
  if (!dbm.ascii_load(s))
    return false;
  // Load redundancy info.
  dimension_type nrows;
   if (!(s >> nrows))
    return false;
  redundancy_dbm.clear();
  redundancy_dbm.reserve(nrows);
  std::deque<bool> redundancy_row(nrows, false);
  for (dimension_type i = 0; i < nrows;  ++i) {
    for (dimension_type j = 0; j < nrows; ++j)
      if (!(s >> redundancy_row[j]))
	return false;
    redundancy_dbm.push_back(redundancy_row);
  }
  return true;
}

template <typename T>
inline void
BD_Shape<T>::forget_all_dbm_constraints(const dimension_type v) {
  assert(0 < v && v <= dbm.num_rows());
  DB_Row<N>& dbm_v = dbm[v];
  for (dimension_type i = dbm.num_rows(); i-- > 0; ) {
    dbm_v[i] = PLUS_INFINITY;
    dbm[i][v] = PLUS_INFINITY;
  }
}

template <typename T>
inline void
BD_Shape<T>::forget_binary_dbm_constraints(const dimension_type v) {
  assert(0 < v && v <= dbm.num_rows());
  DB_Row<N>& dbm_v = dbm[v];
  for (dimension_type i = dbm.num_rows()-1; i > 0; --i) {
    dbm_v[i] = PLUS_INFINITY;
    dbm[i][v] = PLUS_INFINITY;
  }
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::BD_Shape */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::BD_Shape<T>& x,
     Parma_Polyhedra_Library::BD_Shape<T>& y) {
  x.swap(y);
}

} // namespace std

// From here onwards, there should be no inline methods/functions,
// but only non-inline member/function templates.

namespace Parma_Polyhedra_Library {

template <typename T>
BD_Shape<T>::BD_Shape(const Generator_System& gs)
  : dbm(gs.space_dimension() + 1), status(), redundancy_dbm() {
  using Implementation::BD_Shapes::assign_max;
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type space_dim = space_dimension();
  const Generator_System::const_iterator gs_begin = gs.begin();
  const Generator_System::const_iterator gs_end = gs.end();
  DB_Row<N>& dbm_0 = dbm[0];
  N tmp;

  bool dbm_initialized = false;
  bool point_seen = false;
  // Going through all the points and closure points.
  for (Generator_System::const_iterator i = gs_begin; i != gs_end; ++i) {
    const Generator& g = *i;
    switch (g.type()) {
    case Generator::POINT:
      point_seen = true;
      // Intentionally fall through.
    case Generator::CLOSURE_POINT:
      if (!dbm_initialized) {
	// When handling the first (closure) point, we initialize the DBM.
	dbm_initialized = true;
	const Coefficient& d = g.divisor();
	for (dimension_type i = space_dim; i > 0; --i) {
	  const Coefficient& g_i = g.coefficient(Variable(i-1));
	  DB_Row<N>& dbm_i = dbm[i];
	  for (dimension_type j = space_dim; j > 0; --j)
	    if (i != j)
	      div_round_up(dbm_i[j], g.coefficient(Variable(j-1)) - g_i, d);
	  div_round_up(dbm_i[0], -g_i, d);
	}
	for (dimension_type j = space_dim; j > 0; --j)
	  div_round_up(dbm_0[j], g.coefficient(Variable(j-1)), d);
	// Note: no need to initialize the first element of the main diagonal.
      }
      else {
	// This is not the first point: the DBM already contains
	// valid values and we must compute maxima.
	const Coefficient& d = g.divisor();
	for (dimension_type i = space_dim; i > 0; --i) {
	  const Coefficient& g_i = g.coefficient(Variable(i-1));
	  DB_Row<N>& dbm_i = dbm[i];
	  // The loop correctly handles the case when i == j.
	  for (dimension_type j = space_dim; j > 0; --j) {
	    div_round_up(tmp, g.coefficient(Variable(j-1)) - g_i, d);
	    assign_max(dbm_i[j], tmp);
	  }
	  div_round_up(tmp, -g_i, d);
	  assign_max(dbm_i[0], tmp);
	}
	for (dimension_type j = space_dim; j > 0; --j) {
	  div_round_up(tmp, g.coefficient(Variable(j-1)), d);
	  assign_max(dbm_0[j], tmp);
	}
      }
      break;
    default:
      // Lines and rays temporarily ignored.
      break;
    }
  }

  if (!point_seen) {
    // If no point was found in `gs', the corresponding polyhedron is empty.
    set_empty();
    assert(OK());
    return;
  }

  // Going through all the lines and rays.
  for (Generator_System::const_iterator i = gs_begin; i != gs_end; ++i) {
    const Generator& g = *i;
    switch (g.type()) {
    case Generator::LINE:
      for (dimension_type i = space_dim; i > 0; --i) {
	const Coefficient& g_i = g.coefficient(Variable(i-1));
	DB_Row<N>& dbm_i = dbm[i];
	// The loop correctly handles the case when i == j.
	for (dimension_type j = space_dim; j > 0; --j)
	  if (g_i != g.coefficient(Variable(j-1)))
	    dbm_i[j] = PLUS_INFINITY;
	if (g_i != 0)
	  dbm_i[0] = PLUS_INFINITY;
      }
      for (dimension_type j = space_dim; j > 0; --j)
	if (g.coefficient(Variable(j-1)) != 0)
	  dbm_0[j] = PLUS_INFINITY;
      break;
    case Generator::RAY:
      for (dimension_type i = space_dim; i > 0; --i) {
	const Coefficient& g_i = g.coefficient(Variable(i-1));
	DB_Row<N>& dbm_i = dbm[i];
	// The loop correctly handles the case when i == j.
	for (dimension_type j = space_dim; j > 0; --j)
	  if (g_i < g.coefficient(Variable(j-1)))
	    dbm_i[j] = PLUS_INFINITY;
	if (g_i < 0)
	  dbm_i[0] = PLUS_INFINITY;
      }
      for (dimension_type j = space_dim; j > 0; --j)
	if (g.coefficient(Variable(j-1)) > 0)
	  dbm_0[j] = PLUS_INFINITY;
      break;
    default:
      // Points and closure points already dealt with.
      break;
    }
  }
  status.set_shortest_path_closed();
  assert(OK());
}

template <typename T>
BD_Shape<T>::BD_Shape(const Polyhedron& ph, const Complexity_Class complexity)
  : dbm(), status(), redundancy_dbm() {
  const dimension_type num_dimensions = ph.space_dimension();

  if (ph.marked_empty()) {
    *this = BD_Shape(num_dimensions, EMPTY);
    return;
  }

  if (num_dimensions == 0) {
    *this = BD_Shape(num_dimensions, UNIVERSE);
    return;
  }

  // Build from generators when we do not care about complexity
  // or when the process has polynomial complexity.
  if (complexity == ANY_COMPLEXITY
      || (!ph.has_pending_constraints() && ph.generators_are_up_to_date())) {
    *this = BD_Shape(ph.generators());
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
      *this = BD_Shape(num_dimensions, UNIVERSE);
      return;
    }
  }

  // See if there is at least one inconsistent constraint in `ph.con_sys'.
  for (Constraint_System::const_iterator i = ph.con_sys.begin(),
	 cs_end = ph.con_sys.end(); i != cs_end; ++i)
    if (i->is_inconsistent()) {
      *this = BD_Shape(num_dimensions, EMPTY);
      return;
    }

  // If `complexity' allows it, use simplex to derive the exact (modulo
  // the fact that our BDSs are topologically closed) variable bounds.
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
	lp.add_constraint(c.is_equality()
			  ? (Linear_Expression(c) == 0)
			  : (Linear_Expression(c) >= 0));
      }

    // Check for unsatisfiability.
    if (!lp.is_satisfiable()) {
      *this = BD_Shape(num_dimensions, EMPTY);
      return;
    }

    // Get all the upper bounds.
    LP_Problem_Status lp_status;
    Generator g(point());
    Coefficient num;
    Coefficient den;
    for (dimension_type i = 1; i <= num_dimensions; ++i) {
      Variable x(i-1);
      // Evaluate optimal upper bound for `x <= ub'.
      lp.set_objective_function(x);
      lp_status = lp.solve();
      if (lp_status == UNBOUNDED_LP_PROBLEM)
	dbm[0][i] = PLUS_INFINITY;
      else {
	assert(lp_status == OPTIMIZED_LP_PROBLEM);
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, num, den);
	div_round_up(dbm[0][i], num, den);
      }
      // Evaluate optimal upper bound for `x - y <= ub'.
      for (dimension_type j = 1; j <= num_dimensions; ++j) {
	if (i == j)
	  continue;
	Variable y(j-1);
	lp.set_objective_function(x - y);
	lp_status = lp.solve();
	if (lp_status == UNBOUNDED_LP_PROBLEM)
	  dbm[j][i] = PLUS_INFINITY;
	else {
	  assert(lp_status == OPTIMIZED_LP_PROBLEM);
	  g = lp.optimizing_point();
	  lp.evaluate_objective_function(g, num, den);
	  div_round_up(dbm[j][i], num, den);
	}
      }
      // Evaluate optimal upper bound for `-x <= ub'.
      lp.set_objective_function(-x);
      lp_status = lp.solve();
      if (lp_status == UNBOUNDED_LP_PROBLEM)
	dbm[i][0] = PLUS_INFINITY;
      else {
	assert(lp_status == OPTIMIZED_LP_PROBLEM);
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, num, den);
	div_round_up(dbm[i][0], num, den);
      }
    }
    status.set_shortest_path_closed();
    return;
  }

  // Extract easy-to-find bounds from constraints.
  *this = BD_Shape(ph.con_sys);
}

template <typename T>
void
BD_Shape<T>::add_constraint(const Constraint& c) {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > space_dimension())
    throw_dimension_incompatible("add_constraint(c)", c);
  // Strict inequalities are not allowed.
  if (c.is_strict_inequality())
    throw_constraint_incompatible("add_constraint(c)");

  dimension_type num_vars = 0;
  dimension_type i = 0;
  dimension_type j = 0;
  Coefficient coeff;
  // Constraints that are not bounded differences are ignored.
  if (!extract_bounded_difference(c, c_space_dim, num_vars, i, j, coeff))
    return;

  if (num_vars == 0) {
    // Dealing with a trivial constraint.
    if (c.inhomogeneous_term() < 0)
      set_empty();
    return;
  }

  // Select the cell to be modified for the "<=" part of the constraint,
  // and set `coeff' to the absolute value of itself.
  N& x = (coeff < 0) ? dbm[i][j] : dbm[j][i];
  N& y = (coeff < 0) ? dbm[j][i] : dbm[i][j];
  if (coeff < 0)
    coeff = -coeff;

  bool changed = false;
  // Compute the bound for `x', rounding towards plus infinity.
  N d;
  div_round_up(d, c.inhomogeneous_term(), coeff);
  if (x > d) {
    x = d;
    changed = true;
  }

  if (c.is_equality()) {
    // Also compute the bound for `y', rounding towards plus infinity.
    div_round_up(d, -c.inhomogeneous_term(), coeff);
    if (y > d) {
      y = d;
      changed = true;
    }
  }

  // In general, adding a constraint does not preserve the shortest-path
  // closure or reduction of the system of bounded differences.
  if (changed && marked_shortest_path_closed())
    status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::concatenate_assign(const BD_Shape& y) {
  BD_Shape& x = *this;
  assert(x.OK());
  assert(y.OK());

  const dimension_type x_space_dim = x.space_dimension();
  const dimension_type y_space_dim = y.space_dimension();

  // If `y' is an empty 0-dim space system of bounded differences,
  // let `*this' become empty.
  if (y_space_dim == 0 && y.marked_empty()) {
    set_empty();
    return;
  }

  // If `x' is an empty 0-dim space BDS, then it is sufficient to adjust
  // the dimension of the vector space.
  if (x_space_dim == 0 && marked_empty()) {
    dbm.grow(y_space_dim + 1);
    return;
  }
  // First we increase the space dimension of `x' by adding
  // `y.space_dimension()' new dimensions.
  // The matrix for the new system of constraints is obtained
  // by leaving the old system of constraints in the upper left-hand side
  // and placing the constraints of `y' in the lower right-hand side,
  // except the constraints as `y(i) >= cost' or `y(i) <= cost', that are
  // placed in the right position on the new matrix.
  add_space_dimensions_and_embed(y_space_dim);
  const dimension_type new_space_dim = x_space_dim + y_space_dim;
  for (dimension_type i = x_space_dim + 1; i <= new_space_dim; ++i) {
    DB_Row<N>& dbm_i = dbm[i];
    dbm_i[0] = y.dbm[i - x_space_dim][0];
    dbm[0][i] = y.dbm[0][i - x_space_dim];
    for (dimension_type j = x_space_dim + 1; j <= new_space_dim; ++j)
      dbm_i[j] = y.dbm[i - x_space_dim][j - x_space_dim];
  }

  if (marked_shortest_path_closed())
    status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
bool
BD_Shape<T>::contains(const BD_Shape& y) const {
  const BD_Shape<T>& x = *this;
  const dimension_type x_space_dim = x.space_dimension();

  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    throw_dimension_incompatible("contains(y)", y);

  // The zero-dimensional universe shape contains any other
  // dimension-compatible shape.
  // The zero-dimensional empty shape only contains another
  // zero-dimensional empty shape.
  if (x_space_dim == 0) {
    if (!marked_empty())
      return true;
    else
      return y.marked_empty();
  }

  /*
    The `y' system of bounded differences need be closed.
    In fact if, for example, in `*this' we have the constraints:

    x1 - x2 <= 1;
    x1      <= 3;
    x2      <= 2;

    in `y' the constraints are:

    x1 - x2 <= 0;
    x2      <= 1;

    without closure it returns "false", instead if we close `y' we have
    the implicit constraint

    x1      <= 1;

    and so we obtain the right result "true".
  */
  y.shortest_path_closure_assign();

  // An empty shape is contained in any other dimension-compatible shapes.
  if (y.marked_empty())
    return true;

  // `*this' contains `y' if and only if every cell of `dbm'
  // is greater than or equal to the correspondent one of `y.dbm'.
  for (dimension_type i = x_space_dim + 1; i-- > 0; ) {
    const DB_Row<N>& x_dbm_i = x.dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    for (dimension_type j = x_space_dim + 1; j-- > 0; )
      if (x_dbm_i[j] < y_dbm_i[j])
	return false;
  }
  return true;
}

template <typename T>
bool
BD_Shape<T>::is_universe() const {
  if (marked_empty())
    return false;

  const dimension_type space_dim = space_dimension();
  // If the BDS is non-empty and zero-dimensional,
  // then it is necessarily the universe BDS.
  if (space_dim == 0)
    return true;

  // A system of bounded differences defining the universe BDS can only
  // contain trivial constraints.
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    const DB_Row<N>& dbm_i = dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; )
      if (!is_plus_infinity(dbm_i[j]))
	return false;
  }
  return true;
}

template <typename T>
void
BD_Shape<T>
::compute_predecessors(std::vector<dimension_type>& predecessor) const {
  assert(!marked_empty() && marked_shortest_path_closed());
  assert(predecessor.size() == 0);
  // Variables are ordered according to their index.
  // The vector `predecessor' is used to indicate which variable
  // immediately precedes a given one in the corresponding equivalence class.
  // The `leader' of an equivalence class is the element having minimum
  // index: leaders are their own predecessors.
  const dimension_type pred_size = dbm.num_rows();
  // Initially, each variable is leader of its own zero-equivalence class.
  predecessor.reserve(pred_size);
  for (dimension_type i = 0; i < pred_size; ++i)
    predecessor.push_back(i);
  // Now compute actual predecessors.
  for (dimension_type i = pred_size; i-- > 1; )
    if (i == predecessor[i]) {
      const DB_Row<N>& dbm_i = dbm[i];
      for (dimension_type j = i; j-- > 0; )
	if (j == predecessor[j]) {
	  N negated_dbm_ji;
	  if (assign_neg(negated_dbm_ji, dbm[j][i], ROUND_NOT_NEEDED) == V_EQ
	      && negated_dbm_ji == dbm_i[j]) {
	    // Choose as predecessor the variable having the smaller index.
	    predecessor[i] = j;
	    break;
	  }
	}
    }
}

template <typename T>
void
BD_Shape<T>::compute_leaders(std::vector<dimension_type>& leaders) const {
  assert(!marked_empty() && marked_shortest_path_closed());
  assert(leaders.size() == 0);
  // Compute predecessor information.
  compute_predecessors(leaders);
  // Flatten the predecessor chains so as to obtain leaders.
  assert(leaders[0] == 0);
  for (dimension_type i = 1, iend = leaders.size(); i != iend; ++i) {
    const dimension_type l_i = leaders[i];
    assert(l_i <= i);
    if (l_i != i) {
      const dimension_type ll_i = leaders[l_i];
      assert(ll_i == leaders[ll_i]);
      leaders[i] = ll_i;
    }
  }
}

template <typename T>
bool
BD_Shape<T>::is_shortest_path_reduced() const {
  // If the BDS is empty, it is also reduced.
  if (marked_empty())
    return true;

  // A shortest-path reduced dbm is just a dbm with an indication of
  // those constraints that are redundant. If there is no indication
  // of the redundant constraints, then it cannot be reduced.
  if (!marked_shortest_path_reduced())
    return false;

  const BD_Shape x_copy = *this;
  const dimension_type x_space_dim = x_copy.space_dimension();
  x_copy.shortest_path_closure_assign();
  // If we just discovered emptyness, it cannot be reduced.
  if (x_copy.marked_empty())
    return false;

  // The vector `leader' is used to indicate which variables are equivalent.
  std::vector<dimension_type> leader(x_space_dim + 1);

  // We store the leader.
  for (dimension_type i = x_space_dim + 1; i-- > 0; )
    leader[i] = i;

  // Step 1: we store really the leader with the corrected value.
  // We search for the equivalent or zero-equivalent variables.
  // The variable(i-1) and variable(j-1) are equivalent if and only if
  // m_i_j == -(m_j_i).
  for (dimension_type i = 0; i < x_space_dim; ++i) {
    const DB_Row<N>& xdbm_i = x_copy.dbm[i];
    for (dimension_type j = i + 1; j <= x_space_dim; ++j) {
      N negated_xdbm_ji;
      if (assign_neg(negated_xdbm_ji, x_copy.dbm[j][i], ROUND_NOT_NEEDED) == V_EQ
	  && negated_xdbm_ji == xdbm_i[j])
	// Two equivalent variables have got the same leader
	// (the smaller variable).
	leader[j] = leader[i];
    }
  }

  // Step 2: we check if there are redundant constraints in the zero_cycle
  // free systems of bounded differences, considering only the leaders.
  // A constraint `c' is redundant, when there are two constraints such that
  // their sum is the same constraint with the inhomogeneous term
  // less than or equal to the `c' one.
  N c;
  for (dimension_type k = 0; k <= x_space_dim; ++k)
    if (leader[k] == k) {
      const DB_Row<N>& x_k = x_copy.dbm[k];
      for (dimension_type i = 0; i <= x_space_dim; ++i)
	if (leader[i] == i) {
	  const DB_Row<N>& x_i = x_copy.dbm[i];
	  const std::deque<bool>& redundancy_i = redundancy_dbm[i];
	  const N& x_i_k = x_i[k];
	  for (dimension_type j = 0; j <= x_space_dim; ++j)
	    if (leader[j] == j) {
	      const N& x_i_j = x_i[j];
	      if (!is_plus_infinity(x_i_j)) {
		assign_add(c, x_i_k, x_k[j], ROUND_UP);
		if (x_i_j >= c && !redundancy_i[j])
		  return false;
	      }
	    }
	}
    }

  // The vector `var_conn' is used to check if there is a single cycle
  // that connected all zero-equivalent variables between them.
  // The value `space_dim + 1' is used to indicate that the equivalence
  // class contains a single variable.
  std::vector<dimension_type> var_conn(x_space_dim + 1);
  for (dimension_type i = x_space_dim + 1; i-- > 0; )
    var_conn[i] = x_space_dim + 1;

  // Step 3: we store really the `var_conn' with the right value, putting
  // the variable with the selected variable is connected:
  // we check the row of each variable:
  // a- each leader could be connected with only zero-equivalent one,
  // b- each no-leader with only another zero-equivalent one.
  for (dimension_type i = 0; i <= x_space_dim; ++i) {
    // It count with how many variables the selected variable is
    // connected.
    dimension_type t = 0;
    dimension_type ld_i = leader[i];
    // Case a: leader.
    if (ld_i == i) {
      for (dimension_type j = 0; j <= x_space_dim; ++j) {
	dimension_type ld_j = leader[j];
	// Only the connectedness with equivalent variables
	// is considered.
	if (j != ld_j)
	  if (!redundancy_dbm[i][j]) {
	    if (t == 1)
	      // Two no-leaders couldn't connected with the same leader.
	      return false;
	    else
	      if (ld_j != i)
		// The variables isn't in the same equivalence class.
		return false;
	      else {
		++t;
		var_conn[i] = j;
	      }
	  }
      }
    }
    // Case b: no-leader.
    else {
      for (dimension_type j = 0; j <= x_space_dim; ++j) {
	if (!redundancy_dbm[i][j]) {
	  dimension_type ld_j = leader[j];
	  if (ld_i != ld_j)
	    // The variables isn't in the same equivalence class.
	    return false;
	  else {
	    if (t == 1)
	      // Two variables couldn't connected with the same leader.
	      return false;
	    else {
	      ++t;
	      var_conn[i] = j;
	    }
	  }
	  // A no-leader must be connected with
	  // another variable.
	  if (t == 0)
	    return false;
	}
      }
    }
  }

  // The vector `just_checked' is used to check if
  // a variable is already checked.
  std::vector<bool> just_checked(x_space_dim + 1);
  for (dimension_type i = x_space_dim + 1; i-- > 0; )
    just_checked[i] = false;

  // Step 4: we check if there are single cycles that
  // connected all the zero-equivalent variables between them.
  for (dimension_type i = 0; i <= x_space_dim; ++i) {
    bool jc_i = just_checked[i];
    // We don't re-control the already considered single cycles.
    if (!jc_i) {
      dimension_type v_con = var_conn[i];
      // We consider only the equivalence classes with
      // 2 or plus variables.
      if (v_con != x_space_dim + 1) {
	// There is a single cycle if taken a variable,
	// we return to this same variable.
	while (v_con != i) {
	  just_checked[v_con] = true;
	  v_con = var_conn[v_con];
	  // If we re-pass to an already considered variable,
	  // then we haven't a single cycle.
	  if (just_checked[v_con])
	    return false;
	}
      }
    }
    just_checked[i] = true;
  }

  // The system bounded differences is just reduced.
  return true;
}

template <typename T>
Poly_Con_Relation
BD_Shape<T>::relation_with(const Constraint& c) const {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type c_space_dim = c.space_dimension();
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (c_space_dim > space_dim)
    throw_dimension_incompatible("relation_with(c)", c);

  shortest_path_closure_assign();

  if (marked_empty())
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0) {
    if ((c.is_equality() && c.inhomogeneous_term() != 0)
	|| (c.is_inequality() && c.inhomogeneous_term() < 0))
      return Poly_Con_Relation::is_disjoint();
    else if (c.is_strict_inequality() && c.inhomogeneous_term() == 0)
      // The constraint 0 > 0 implicitly defines the hyperplane 0 = 0;
      // thus, the zero-dimensional point also saturates it.
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_disjoint();
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
  // Constraints that are not bounded differences are not compatible.
  if (!extract_bounded_difference(c, c_space_dim, num_vars, i, j, coeff))
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

  // Select the cell to be checked for the "<=" part of the constraint,
  // and set `coeff' to the absolute value of itself.
  const N& x = (coeff < 0) ? dbm[i][j] : dbm[j][i];
  const N& y = (coeff < 0) ? dbm[j][i] : dbm[i][j];
  if (coeff < 0)
    coeff = -coeff;
  N d;
  div_round_up(d, c.inhomogeneous_term(), coeff);
  N d1;
  div_round_up(d1, -c.inhomogeneous_term(), coeff);

  switch (c.type()) {
  case Constraint::EQUALITY:
    if (d == x && d1 == y)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else if (d < y && d1 > x)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  case Constraint::NONSTRICT_INEQUALITY:
    if (d >= x && d1 >= y)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else if (d >= x)
      return Poly_Con_Relation::is_included();
    else if (d < x && d1 > y)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  case Constraint::STRICT_INEQUALITY:
    if (d >= x && d1 >= y)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_disjoint();
    else if (d > x)
      return Poly_Con_Relation::is_included();
    else if (d <= x && d1 >= y)
      return Poly_Con_Relation::is_disjoint();
    else
      return Poly_Con_Relation::strictly_intersects();
  }
  // Quiet a compiler warning: this program point is unreachable.
  throw std::runtime_error("PPL internal error");
}

template <typename T>
Poly_Gen_Relation
BD_Shape<T>::relation_with(const Generator& g) const {
  const dimension_type space_dim = space_dimension();
  const dimension_type g_space_dim = g.space_dimension();

  // Dimension-compatibility check.
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("relation_with(g)", g);

  // The empty bdiff cannot subsume a generator.
  if (marked_empty())
    return Poly_Gen_Relation::nothing();

  // A universe BD shape in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();

  const bool is_line = g.is_line();

  // The relation between the bdiff and the given generator is obtained
  // checking if the generator satisfies all the constraints in the bdiff.
  // To check if the generator satisfies all the constraints it's enough
  // studying the sign of the scalar product between the generator and
  // all the constraints in the bdiff.

  // We find in `*this' all the constraints.
  for (dimension_type i = 0; i <= space_dim; ++i) {
    for (dimension_type j = i + 1; j <= space_dim; ++j) {
      const Variable x(j - 1);
      const bool x_dimension_incompatible = x.space_dimension() > g_space_dim;
      const N& dbm_ij = dbm[i][j];
      const N& dbm_ji = dbm[j][i];
      N negated_dbm_ji;
      const bool is_equality
	= assign_neg(negated_dbm_ji, dbm_ji, ROUND_NOT_NEEDED) == V_EQ
	&& negated_dbm_ji == dbm_ij;
      const bool dbm_ij_is_infinity = is_plus_infinity(dbm_ij);
      const bool dbm_ji_is_infinity = is_plus_infinity(dbm_ji);
      if (i != 0) {
	const Variable y(i - 1);
	const bool y_dimension_incompatible
	  = y.space_dimension() > g_space_dim;
	const bool is_trivial_zero
	  = (x_dimension_incompatible && g.coefficient(y) == 0)
	  || (y_dimension_incompatible && g.coefficient(x) == 0)
	  || (x_dimension_incompatible && y_dimension_incompatible);
	if (is_equality) {
	  // We have one equality constraint.
	  // The constraint has form ax - ay = b.
	  // The scalar product has the form
	  // 'a * y_i - a * x_j'
	  // where y_i = g.coefficient(y) and x_j = g.coefficient(x).
	  // It is not zero when both the coefficients of the
	  // variables x and y are not zero or when these coefficients
 	  if (!is_trivial_zero && g.coefficient(x) != g.coefficient(y))
	    return Poly_Gen_Relation::nothing();
	}
	else
	  // We have the binary inequality constraints.
	  if (!dbm_ij_is_infinity) {
	  // The constraint has form ax - ay <= b.
	  // The scalar product has the form
	  // 'a * y_i - a * x_j'
	    if (is_line
		&& !is_trivial_zero
		&& g.coefficient(x) != g.coefficient(y))
	      return Poly_Gen_Relation::nothing();
	    else
	      if (g.coefficient(y) < g.coefficient(x))
		return Poly_Gen_Relation::nothing();
	  }
	  else if (!dbm_ji_is_infinity) {
	    // The constraint has form ay - ax <= b.
	    // The scalar product has the form
	    // 'a * x_j - a* y_i'.
	    if (is_line
		&& !is_trivial_zero
		&& g.coefficient(x) != g.coefficient(y))
	      return Poly_Gen_Relation::nothing();
	    else if (g.coefficient(x) < g.coefficient(y))
	      return Poly_Gen_Relation::nothing();
	  }
      }
      else {
	// Here i == 0.
	if (is_equality) {
	  // The constraint has form ax = b.
	  // To satisfy the constraint it's necessary that the scalar product
	  // is not zero.It happens when the coefficient of the variable 'x'
	  // in the generator is not zero, because the scalar
	  // product has the form:
	  // 'a * x_i' where x_i = g.coefficient(x)..
	  if (!x_dimension_incompatible && g.coefficient(x) != 0)
	    return Poly_Gen_Relation::nothing();
	}
	else
	  // We have the unary inequality constraints.
	  if (!dbm_ij_is_infinity) {
	    // The constraint has form ax <= b.
	    // The scalar product has the form:
	    // '-a * x_i' where x_i = g.coefficient(x).
	    if (is_line
		&& !x_dimension_incompatible
		&& g.coefficient(x) != 0)
	      return Poly_Gen_Relation::nothing();
	    else if (g.coefficient(x) > 0)
	      return Poly_Gen_Relation::nothing();
	  }
	  else if (!dbm_ji_is_infinity) {
	    // The constraint has form -ax <= b.
	    // The scalar product has the form:
	    // 'a * x_i' where x_i = g.coefficient(x).
	    if (is_line
		&& !x_dimension_incompatible
		&& g.coefficient(x) != 0)
	      return Poly_Gen_Relation::nothing();
	    else if (g.coefficient(x) < 0)
	      return Poly_Gen_Relation::nothing();
	  }
      }
    }
  }
  return Poly_Gen_Relation::subsumes();
}

template <typename T>
void
BD_Shape<T>::shortest_path_closure_assign() const {
  using Implementation::BD_Shapes::assign_min;

  // Do something only if necessary.
  if (marked_empty() || marked_shortest_path_closed())
    return;
  const dimension_type num_dimensions = space_dimension();
  // Zero-dimensional BDSs are necessarily shortest-path closed.
  if (num_dimensions == 0)
    return;

  // Even though the BDS will not change, its internal representation
  // is going to be modified by the Floyd-Warshall algorithm.
  BD_Shape& x = const_cast<BD_Shape<T>&>(*this);

  // Fill the main diagonal with zeros.
  for (dimension_type h = num_dimensions + 1; h-- > 0; ) {
    assert(is_plus_infinity(x.dbm[h][h]));
    assign(x.dbm[h][h], 0, ROUND_NOT_NEEDED);
  }

  N sum;
  for (dimension_type k = num_dimensions + 1; k-- > 0; ) {
    const DB_Row<N>& xdbm_k = x.dbm[k];
    for (dimension_type i = num_dimensions + 1; i-- > 0; ) {
      DB_Row<N>& xdbm_i = x.dbm[i];
      const N& xdbm_i_k = xdbm_i[k];
      if (!is_plus_infinity(xdbm_i_k))
	for (dimension_type j = num_dimensions + 1; j-- > 0; ) {
	  const N& xdbm_k_j = xdbm_k[j];
	  if (!is_plus_infinity(xdbm_k_j)) {
	    // Rounding upward for correctness.
	    assign_add(sum, xdbm_i_k, xdbm_k_j, ROUND_UP);
	    assign_min(xdbm_i[j], sum);
	  }
	}
    }
  }

  // Check for emptyness: the BDS is empty if and only if there is a
  // negative value on the main diagonal of `dbm'.
  for (dimension_type h = num_dimensions + 1; h-- > 0; ) {
    N& x_dbm_hh = x.dbm[h][h];
    if (x_dbm_hh < 0) {
      x.status.set_empty();
      return;
    }
    else {
      assert(x_dbm_hh == 0);
      // Restore PLUS_INFINITY on the main diagonal.
      x_dbm_hh = PLUS_INFINITY;
    }
  }

  // The BDS is not empty and it is now shortest-path closed.
  x.status.set_shortest_path_closed();
}

template <typename T>
void
BD_Shape<T>::shortest_path_reduction_assign() const {
  // Do something only if necessary.
  if (marked_shortest_path_reduced())
    return;

  // First find the tighest constraints for this BDS.
  shortest_path_closure_assign();

  // If `*this' is empty, then there is nothing to reduce.
  if (marked_empty())
    return;

  // Step 1: compute zero-equivalence classes.
  // Variables corresponding to indices `i' and `j' are zero-equivalent
  // if they lie on a zero-weight loop; since the matrix is shortest-path
  // closed, this happens if and only if dbm[i][j] == -dbm[j][i].
  std::vector<dimension_type> predecessor;
  compute_predecessors(predecessor);
  std::vector<dimension_type> leaders;
  compute_leader_indices(predecessor, leaders);
  const dimension_type num_leaders = leaders.size();

  const dimension_type space_dim = space_dimension();
  // TODO: directly work on `redundancy_dbm' so as to minimize allocations.
  std::deque<bool> redundancy_row(space_dim + 1, true);
  std::vector<std::deque<bool> > redundancy(space_dim + 1, redundancy_row);

  // Step 2: flag non-redundant constraints in the (zero-cycle-free)
  // subsystem of bounded differences having only leaders as variables.
  N c;
  for (dimension_type l_i = 0; l_i < num_leaders; ++l_i) {
    const dimension_type i = leaders[l_i];
    const DB_Row<N>& dbm_i = dbm[i];
    std::deque<bool>& redundancy_i = redundancy[i];
    for (dimension_type l_j = 0; l_j < num_leaders; ++l_j) {
      const dimension_type j = leaders[l_j];
      if (redundancy_i[j]) {
	const N& dbm_i_j = dbm_i[j];
	redundancy_i[j] = false;
	for (dimension_type l_k = 0; l_k < num_leaders; ++l_k) {
	  const dimension_type k = leaders[l_k];
	  assign_add(c, dbm_i[k], dbm[k][j], ROUND_UP);
	  if (dbm_i_j >= c) {
	    redundancy_i[j] = true;
	    break;
	  }
	}
      }
    }
  }

  // Step 3: flag non-redundant constraints in zero-equivalence classes.
  // Each equivalence class must have a single 0-cycle connecting
  // all the equivalent variables in increasing order.
  std::deque<bool> dealt_with(space_dim + 1, false);
  for (dimension_type i = space_dim + 1; i-- > 0; )
    // We only need to deal with non-singleton zero-equivalence classes
    // that haven't already been dealt with.
    if (i != predecessor[i] && !dealt_with[i]) {
      dimension_type j = i;
      while (true) {
	const dimension_type pred_j = predecessor[j];
	if (j == pred_j) {
	  // We finally found the leader of `i'.
	  assert(redundancy[i][j]);
	  redundancy[i][j] = false;
	  // Here we dealt with `j' (i.e., `pred_j'), but it is useless
	  // to update `dealt_with' because `j' is a leader.
	  break;
	}
	// We haven't found the leader of `i' yet.
	assert(redundancy[pred_j][j]);
	redundancy[pred_j][j] = false;
	dealt_with[pred_j] = true;
	j = pred_j;
      }
    }

  // Even though shortest-path reduction is not going to change the BDS,
  // it might change its internal representation.
  BD_Shape<T>& x = const_cast<BD_Shape<T>&>(*this);
  std::swap(x.redundancy_dbm, redundancy);
  x.status.set_shortest_path_reduced();

  assert(is_shortest_path_reduced());
}

template <typename T>
void
BD_Shape<T>::bds_hull_assign(const BD_Shape& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("bds_hull_assign(y)", y);

  // The poly-hull of a polyhedron `bd' with an empty polyhedron is `bd'.
  y.shortest_path_closure_assign();
  if (y.marked_empty())
    return;
  shortest_path_closure_assign();
  if (marked_empty()) {
    *this = y;
    return;
  }

  // The bds-hull consists in constructing `*this' with the maximum
  // elements selected from `*this' and `y'.
  assert(space_dim == 0 || marked_shortest_path_closed());
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    DB_Row<N>& dbm_i = dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; ) {
      N& dbm_ij = dbm_i[j];
      const N& y_dbm_ij = y_dbm_i[j];
      if (dbm_ij < y_dbm_ij)
	dbm_ij = y_dbm_ij;
    }
  }
  // The result is still closed.
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::bds_difference_assign(const BD_Shape& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("bds_difference_assign(y)", y);

  BD_Shape new_bdiffs(space_dim, EMPTY);

  BD_Shape& x = *this;

  x.shortest_path_closure_assign();
  // The difference of an empty system of bounded differences
  // and of a system of bounded differences `p' is empty.
  if (x.marked_empty())
    return;
  y.shortest_path_closure_assign();
  // The difference of a system of bounded differences `p'
  // and an empty system of bounded differences is `p'.
  if (y.marked_empty())
    return;

  // If both systems of bounded differences are zero-dimensional,
  // then at this point they are necessarily universe system of
  // bounded differences, so that their difference is empty.
  if (space_dim == 0) {
    x.set_empty();
    return;
  }

  // TODO: This is just an executable specification.
  //       Have to find a more efficient method.
  if (y.contains(x)) {
    x.set_empty();
    return;
  }

  // We take a constraint of the system y at the time and we
  // consider its complementary. Then we intersect the union
  // of these complementaries with the system x.
  const Constraint_System& y_cs = y.constraints();
  for (Constraint_System::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    const Constraint& c = *i;
    // If the system of bounded differences `x' is included 
    // in the system of bounded differences defined by `c', 
    // then `c' _must_ be skipped, as adding its complement to `x'
    // would result in the empty system of bounded differences, 
    // and as we would obtain a result that is less precise 
    // than the bds-difference.
    if (x.relation_with(c).implies(Poly_Con_Relation::is_included()))
      continue;
    BD_Shape z = x;
    const Linear_Expression e = Linear_Expression(c);
    bool change = false;
    if (c.is_nonstrict_inequality())
      change = z.add_constraint_and_minimize(e <= 0);
    if (c.is_equality()) {
      BD_Shape w = x;
      if (w.add_constraint_and_minimize(e <= 0))
	new_bdiffs.bds_hull_assign(w);
      change = z.add_constraint_and_minimize(e >= 0);
    }
    if (change)
      new_bdiffs.bds_hull_assign(z);
  }
  *this = new_bdiffs;
  // The result is still closed, because both bds_hull_assign() and
  // add_constraint_and_minimize() preserve closure.
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::add_space_dimensions_and_embed(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  const dimension_type space_dim = space_dimension();
  const dimension_type new_space_dim = space_dim + m;
  const bool was_zero_dim_univ = (!marked_empty() && space_dim == 0);

  // To embed an n-dimension space BDS in a (n+m)-dimension space,
  // we just add `m' rows and columns in the system of bounded differences,
  // initialized to PLUS_INFINITY.
  dbm.grow(new_space_dim + 1);

  // Shortest-path closure is maintained (if it was holding).
  // TODO: see whether reduction can be (efficiently!) maintained too.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();

  // If `*this' was the zero-dim space universe BDS,
  // the we can set the shortest-path closure flag.
  if (was_zero_dim_univ)
    status.set_shortest_path_closed();

  assert(OK());
}

template <typename T>
void
BD_Shape<T>::add_space_dimensions_and_project(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  const dimension_type space_dim = space_dimension();

  // If `*this' was zero-dimensional, then we add `m' rows and columns.
  // If it also was non-empty, then we zero all the added elements
  // and set the flag for shortest-path closure.
  if (space_dim == 0) {
    dbm.grow(m + 1);
    if (!marked_empty()) {
      for (dimension_type i = m + 1; i-- > 0; ) {
	DB_Row<N>& dbm_i = dbm[i];
	for (dimension_type j = m + 1; j-- > 0; )
	  if (i != j)
	    assign(dbm_i[j], 0, ROUND_NOT_NEEDED);
      }
      status.set_shortest_path_closed();
    }
    assert(OK());
    return;
  }

  // To project an n-dimension space system of bounded differences
  // in a (n+m)-dimension space, we add `m' rows and columns.
  // In the first row and column of the matrix we add `zero' from
  // the (n+1)-th position to the end.
  const dimension_type new_space_dim = space_dim + m;
  dbm.grow(new_space_dim + 1);

  // Bottom of the matrix and first row.
  DB_Row<N>& dbm_0 = dbm[0];
  for (dimension_type i = space_dim + 1; i <= new_space_dim; ++i) {
    assign(dbm[i][0], 0, ROUND_NOT_NEEDED);
    assign(dbm_0[i], 0, ROUND_NOT_NEEDED);
  }

  if (marked_shortest_path_closed())
    status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any BDS is a no-op.
  // Note that this case also captures the only legal removal of
  // space dimensions from a BDS in a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  const dimension_type max_dim_to_be_removed = to_be_removed.rbegin()->id();
  const dimension_type old_space_dim = space_dimension();
  if (max_dim_to_be_removed >= old_space_dim)
    throw_dimension_incompatible("remove_space_dimensions(vs)",
				 max_dim_to_be_removed);

  // Shortest-path closure is necessary to keep precision.
  shortest_path_closure_assign();

  // When removing _all_ dimensions from a BDS,
  // we obtain the zero-dimensional BDS.
  const dimension_type new_space_dim = old_space_dim - to_be_removed.size();
  if (new_space_dim == 0) {
    dbm.resize_no_copy(1);
    if (!marked_empty())
      // We set the zero_dim_univ flag.
      set_zero_dim_univ();
    assert(OK());
    return;
  }

  // Shortest-path closure is maintained.
  // TODO: see whether reduction can be (efficiently!) maintained too.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();

  // For each variable to remove, we erase the corresponding column and
  // row by shifting the other columns and rows, than are not removed,
  // respectively left and above.
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst = tbr->id() + 1;
  dimension_type src = dst + 1;
  for (++tbr; tbr != tbr_end; ++tbr) {
    const dimension_type tbr_next = tbr->id() + 1;
    // All other columns and rows are moved respectively to the left
    // and above.
    while (src < tbr_next) {
      dbm[dst] = dbm[src];
      for (dimension_type i = 0; i <= old_space_dim; ++i) {
	DB_Row<N>& dbm_i = dbm[i];
	dbm_i[dst] = dbm_i[src];
      }
      ++dst;
      ++src;
    }
    ++src;
  }

  // Moving the remaining rows and columns.
  while (src <= old_space_dim) {
    dbm[dst] = dbm[src];
    for (dimension_type i = 0; i <= old_space_dim; ++i) {
      DB_Row<N>& dbm_i = dbm[i];
      dbm_i[dst] = dbm_i[src];
    }
    ++src;
    ++dst;
  }

  // Update the space dimension.
  dbm.resize_no_copy(new_space_dim + 1);
  assert(OK());
}

template <typename T>
template <typename PartialFunction>
void
BD_Shape<T>::map_space_dimensions(const PartialFunction& pfunc) {
  const dimension_type space_dim = space_dimension();
  // TODO: this implementation is just an executable specification.
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the BDS becomes zero_dimensional.
    remove_higher_space_dimensions(0);
    assert(OK());
    return;
  }

  const dimension_type new_space_dim = pfunc.max_in_codomain() + 1;
  // If we are going to actually reduce the space dimension,
  // then shortest-path closure is required to keep precision.
  if (new_space_dim < space_dim)
    shortest_path_closure_assign();

  // If the BDS is empty, then it is sufficient to adjust the
  // space dimension of the system of bounded differences.
  if (marked_empty()) {
    remove_higher_space_dimensions(new_space_dim);
    return;
  }

  // Shortest-path closure is maintained (if it was holding).
  // TODO: see whether reduction can be (efficiently!) maintained too.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();

  // We create a new matrix with the new space dimension.
  DB_Matrix<N> x(new_space_dim+1);
  // First of all we must map the unary constraints, because
  // there is the fictitious variable `zero', that can't be mapped
  // at all.
  const DB_Row<N>& dbm_0 = dbm[0];
  DB_Row<N>& x_0 = x[0];
  for (dimension_type j = 1; j <= space_dim; ++j) {
    dimension_type new_j;
    if (pfunc.maps(j - 1, new_j)) {
      x_0[new_j + 1] = dbm_0[j];
      x[new_j + 1][0] = dbm[j][0];
    }
  }
  // Now we map the binary constraints, exchanging the indexes.
  for (dimension_type i = 1; i <= space_dim; ++i) {
    dimension_type new_i;
    if (pfunc.maps(i - 1, new_i)) {
      const DB_Row<N>& dbm_i = dbm[i];
      ++new_i;
      DB_Row<N>& x_new_i = x[new_i];
      for (dimension_type j = i+1; j <= space_dim; ++j) {
	dimension_type new_j;
	if (pfunc.maps(j - 1, new_j)) {
	  ++new_j;
	  x_new_i[new_j] = dbm_i[j];
	  x[new_j][new_i] = dbm[j][i];
	}
      }
    }
  }

  std::swap(dbm, x);
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::intersection_assign(const BD_Shape& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("intersection_assign(y)", y);

  // If one of the two systems of bounded differences is empty,
  // the intersection is empty.
  if (marked_empty())
    return;
  if (y.marked_empty()) {
    set_empty();
    return;
  }

  // If both systems of bounded differences are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (space_dim == 0)
    return;

  // To intersect two systems of bounded differences we compare
  // the constraints and we choose the less values.
  bool changed = false;
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    DB_Row<N>& dbm_i = dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; ) {
      N& dbm_ij = dbm_i[j];
      const N& y_dbm_ij = y_dbm_i[j];
      if (dbm_ij > y_dbm_ij) {
	dbm_ij = y_dbm_ij;
	changed = true;
      }
    }
  }

  if (changed && marked_shortest_path_closed())
    status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
template <typename Iterator>
void
BD_Shape<T>::CC76_extrapolation_assign(const BD_Shape& y,
				       Iterator first, Iterator last) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("CC76_extrapolation_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const BD_Shape x_copy = *this;
    const BD_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If both systems of bounded differences are zero-dimensional,
  // since `*this' contains `y', we simply return `*this'.
  if (space_dim == 0)
    return;

  shortest_path_closure_assign();
  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  y.shortest_path_closure_assign();
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  // Compare each constraint in `y' to the corresponding one in `*this'.
  // The constraint in `*this' is kept as is if it is stronger than or
  // equal to the constraint in `y'; otherwise, the inhomogeneous term
  // of teh constraint in `*this' is further compared with elements taken
  // from a sorted container (the stop-points, provided by the user), and
  // is replaced by the first entry, if any, which is greater than or equal
  // to the inhomogeneous term. If no such entry exists, the constraint
  // is removed altogether.
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    DB_Row<N>& dbm_i = dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; ) {
      N& dbm_ij = dbm_i[j];
      const N& y_dbm_ij = y_dbm_i[j];
      if (y_dbm_ij < dbm_ij) {
	Iterator k = std::lower_bound(first, last, dbm_ij);
	if (k != last) {
	  if (dbm_ij < *k)
	    assign(dbm_ij, *k, ROUND_UP);
	}
	else
	  dbm_ij = PLUS_INFINITY;
      }
    }
  }
  status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::get_limiting_shape(const Constraint_System& cs,
				BD_Shape& limiting_shape) const {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type cs_space_dim = cs.space_dimension();
  // Private method: the caller has to ensure the following.
  assert(cs_space_dim <= space_dimension());

  bool changed = false;
  for (Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i) {
    const Constraint& c = *i;
    dimension_type num_vars = 0;
    dimension_type i = 0;
    dimension_type j = 0;
    Coefficient coeff;
    // Constraints that are not bounded differences are ignored.
    if (extract_bounded_difference(c, cs_space_dim, num_vars, i, j, coeff)) {
      // Select the cell to be modified for the "<=" part of the constraint,
      // and set `coeff' to the absolute value of itself.
      const N& x = (coeff < 0) ? dbm[i][j] : dbm[j][i];
      const N& y = (coeff < 0) ? dbm[j][i] : dbm[i][j];
      DB_Matrix<N>& ls_dbm = limiting_shape.dbm;
      N& ls_x = (coeff < 0) ? ls_dbm[i][j] : ls_dbm[j][i];
      N& ls_y = (coeff < 0) ? ls_dbm[j][i] : ls_dbm[i][j];
      if (coeff < 0)
	coeff = -coeff;
      // Compute the bound for `x', rounding towards plus infinity.
      N d;
      div_round_up(d, c.inhomogeneous_term(), coeff);
      if (x <= d)
	if (c.is_inequality())
	  if (ls_x > d) {
	    ls_x = d;
	    changed = true;
	  }
	else {
	  // Compute the bound for `y', rounding towards plus infinity.
	  div_round_up(d, -c.inhomogeneous_term(), coeff);
	  if (y <= d)
	    if (ls_y > d) {
	      ls_y = d;
	      changed = true;
	    }

	}
    }
  }

  // In general, adding a constraint does not preserve the shortest-path
  // closure of the system of bounded differences.
  if (changed && limiting_shape.marked_shortest_path_closed())
    limiting_shape.status.reset_shortest_path_closed();
}

template <typename T>
void
BD_Shape<T>::limited_CC76_extrapolation_assign(const BD_Shape& y,
					       const Constraint_System& cs,
					       unsigned* /*tp*/) {
  // Dimension-compatibility check.
  const dimension_type space_dim = space_dimension();
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("limited_CC76_extrapolation_assign(y, cs)",
				 y);

  // `cs' must be dimension-compatible with the two systems
  // of bounded differences.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_constraint_incompatible("limited_CC76_extrapolation_assign(y, cs)");

  // Strict inequalities not allowed.
  if (cs.has_strict_inequalities())
    throw_constraint_incompatible("limited_CC76_extrapolation_assign(y, cs)");

  // The limited CC76-extrapolation between two systems of bounded
  // differences in a zero-dimensional space is a system of bounded
  // differences in a zero-dimensional space, too.
  if (space_dim == 0)
    return;

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const BD_Shape x_copy = *this;
    const BD_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  BD_Shape<T> limiting_shape(space_dim, UNIVERSE);
  get_limiting_shape(cs, limiting_shape);
  CC76_extrapolation_assign(y);
  intersection_assign(limiting_shape);
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::CH78_widening_assign(const BD_Shape& y, unsigned* /*tp*/) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("CH78_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const BD_Shape x_copy = *this;
    const BD_Shape y_copy = y;
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

  assert(marked_shortest_path_closed() && y.marked_shortest_path_closed());
  // Minimize `y'.
  y.shortest_path_reduction_assign();

  // Extrapolate unstable bounds, taking into account redundancy in `y'.
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    DB_Row<N>& dbm_i = dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    const std::deque<bool>& y_redundancy_i = y.redundancy_dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; ) {
      N& dbm_ij = dbm_i[j];
      // Note: in the following line the use of `!=' (as opposed to
      // the use of `<' that would seem -but is not- equivalent) is
      // intentional.
      if (y_redundancy_i[j] || y_dbm_i[j] != dbm_ij)
	dbm_ij = PLUS_INFINITY;
    }
  }
  // NOTE: this will also reset the shortest-path reduction flag,
  // even though the dbm is still in reduced form. However, the
  // current implementation invariant requires that any reduced dbm
  // is closed too.
  status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::limited_CH78_extrapolation_assign(const BD_Shape& y,
					       const Constraint_System& cs,
					       unsigned* /*tp*/) {
  // Dimension-compatibility check.
  const dimension_type space_dim = space_dimension();
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("limited_CH78_extrapolation_assign(y, cs)",
				 y);
  // `cs' must be dimension-compatible with the two systems
  // of bounded differences.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_constraint_incompatible("limited_CH78_extrapolation_assign(y, cs)");

  // Strict inequalities are not allowed.
  if (cs.has_strict_inequalities())
    throw_constraint_incompatible("limited_CH78_extrapolation_assign(y, cs)");

  // The limited CH78-extrapolation between two systems of bounded
  // differences in a zero-dimensional space is a system of bounded
  // differences in a zero-dimensional space, too.
  if (space_dim == 0)
    return;

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const BD_Shape x_copy = *this;
    const BD_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  BD_Shape<T> limiting_shape(space_dim, UNIVERSE);
  get_limiting_shape(cs, limiting_shape);
  CH78_widening_assign(y);
  intersection_assign(limiting_shape);
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::CC76_narrowing_assign(const BD_Shape& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("CC76_narrowing_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const BD_Shape x_copy = *this;
    const BD_Shape y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If both systems of bounded differences are zero-dimensional,
  // since `*this' contains `y', we simply return `*this'.
  if (space_dim == 0)
    return;

  shortest_path_closure_assign();
  // If `*this' is empty, since `*this' contains `y', `y' is empty too.
  if (marked_empty())
    return;
  y.shortest_path_closure_assign();
  // If `y' is empty, we return.
  if (y.marked_empty())
    return;

  // Replace each constraint in `*this' by the corresponding constraint
  // in `y' if the inhomogeneous term of the first one is `plus_infinity'.
  bool changed = false;
  for (dimension_type i = space_dim + 1; i-- > 0; ) {
    DB_Row<N>& dbm_i = dbm[i];
    const DB_Row<N>& y_dbm_i = y.dbm[i];
    for (dimension_type j = space_dim + 1; j-- > 0; ) {
      N& dbm_ij = dbm_i[j];
      const N& y_dbm_ij = y_dbm_i[j];
      if (is_plus_infinity(dbm_ij) && !is_plus_infinity(y_dbm_ij)) {
	dbm_ij = y_dbm_ij;
	changed = true;
      }
    }
  }
  if (changed && marked_shortest_path_closed())
    status.reset_shortest_path_closed();
  assert(OK());
}

template <typename T>
void
BD_Shape<T>
::deduce_v_minus_u_bounds(const dimension_type v,
			  const dimension_type last_v,
			  const Linear_Expression& sc_expr,
			  Coefficient_traits::const_reference sc_den,
			  const N& pos_sum) {
  // Deduce constraints of the form `v - u', where `u != v'.
  // Note: the shortest-path closure is able to deduce the constraint
  // `v - u <= ub_v - lb_u'. We can be more precise if variable `u'
  // played an active role in the computation of the upper bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // greater than zero. In particular:
  // if `q >= 1',    then `v - u <= ub_v - ub_u';
  // if `0 < q < 1', then `v - u <= ub_v - (q*ub_u + (1-q)*lb_u)'.
  mpq_class mpq_sc_den;
  assign(mpq_sc_den, raw_value(sc_den), ROUND_NOT_NEEDED);
  const DB_Row<N>& dbm_0 = dbm[0];
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u-1));
      if (expr_u > 0)
	if (expr_u >= sc_den)
	  // Deducing `v - u <= ub_v - ub_u'.
	  assign_sub(dbm[u][v], pos_sum, dbm_0[u], ROUND_UP);
	else {
	  DB_Row<N>& dbm_u = dbm[u];
	  const N& dbm_u0 = dbm_u[0];
	  if (!is_plus_infinity(dbm_u0)) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `v - u' is computed as
	    // `ub_v - (q * ub_u + (1-q) * lb_u)', i.e.,
	    // `pos_sum + (-lb_u) - q * (ub_u + (-lb_u))'.
	    mpq_class minus_lb_u;
	    assign(minus_lb_u, raw_value(dbm_u0), ROUND_NOT_NEEDED);
	    mpq_class q;
	    assign(q, raw_value(expr_u), ROUND_NOT_NEEDED);
	    assign_div(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class ub_u;
	    assign(ub_u, raw_value(dbm_0[u]), ROUND_NOT_NEEDED);
	    // Compute `ub_u - lb_u'.
	    assign_add(ub_u, ub_u, minus_lb_u, ROUND_NOT_NEEDED);
	    // Compute `(-lb_u) - q * (ub_u - lb_u)'.
	    assign_sub_mul(minus_lb_u, q, ub_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign(up_approx, minus_lb_u, ROUND_UP);
	    // Deducing `v - u <= ub_v - (q * ub_u + (1-q) * lb_u)'.
	    assign_add(dbm_u[v], pos_sum, up_approx, ROUND_UP);
	  }
	}
    }
}

template <typename T>
void
BD_Shape<T>
::deduce_u_minus_v_bounds(const dimension_type v,
			  const dimension_type last_v,
			  const Linear_Expression& sc_expr,
			  Coefficient_traits::const_reference sc_den,
			  const N& neg_sum) {
  // Deduce constraints of the form `u - v', where `u != v'.
  // Note: the shortest-path closure is able to deduce the constraint
  // `u - v <= ub_u - lb_v'. We can be more precise if variable `u'
  // played an active role in the computation of the lower bound for `v',
  // i.e., if the corresponding coefficient `q == expr_u/den' is
  // greater than zero. In particular:
  // if `q >= 1',    then `u - v <= lb_u - lb_v';
  // if `0 < q < 1', then `u - v <= (q*lb_u + (1-q)*ub_u) - lb_v'.
  mpq_class mpq_sc_den;
  assign(mpq_sc_den, raw_value(sc_den), ROUND_NOT_NEEDED);
  DB_Row<N>& dbm_0 = dbm[0];
  DB_Row<N>& dbm_v = dbm[v];
  // No need to consider indices greater than `last_v'.
  for (dimension_type u = last_v; u > 0; --u)
    if (u != v) {
      const Coefficient& expr_u = sc_expr.coefficient(Variable(u-1));
      if (expr_u > 0)
	if (expr_u >= sc_den)
	  // Deducing `u - v <= lb_u - lb_v',
	  // i.e., `u - v <= (-lb_v) - (-lb_u)'.
	  assign_sub(dbm_v[u], neg_sum, dbm[u][0], ROUND_UP);
	else {
	  const N& dbm_0u = dbm_0[u];
	  if (!is_plus_infinity(dbm_0u)) {
	    // Let `ub_u' and `lb_u' be the known upper and lower bound
	    // for `u', respectively. Letting `q = expr_u/sc_den' be the
	    // rational coefficient of `u' in `sc_expr/sc_den',
	    // the upper bound for `u - v' is computed as
	    // `(q * lb_u + (1-q) * ub_u) - lb_v', i.e.,
	    // `ub_u - q * (ub_u + (-lb_u)) + neg_sum'.
	    mpq_class ub_u;
	    assign(ub_u, raw_value(dbm_0u), ROUND_NOT_NEEDED);
	    mpq_class q;
	    assign(q, raw_value(expr_u), ROUND_NOT_NEEDED);
	    assign_div(q, q, mpq_sc_den, ROUND_NOT_NEEDED);
	    mpq_class minus_lb_u;
	    assign(minus_lb_u, raw_value(dbm[u][0]), ROUND_NOT_NEEDED);
	    // Compute `ub_u - lb_u'.
	    assign_add(minus_lb_u, minus_lb_u, ub_u, ROUND_NOT_NEEDED);
	    // Compute `ub_u - q * (ub_u - lb_u)'.
	    assign_sub_mul(ub_u, q, minus_lb_u, ROUND_NOT_NEEDED);
	    N up_approx;
	    assign(up_approx, ub_u, ROUND_UP);
	    // Deducing `u - v <= (q*lb_u + (1-q)*ub_u) - lb_v'.
	    assign_add(dbm_v[u], up_approx, neg_sum, ROUND_UP);
	  }
	}
    }
}

template <typename T>
void
BD_Shape<T>::affine_image(const Variable var,
			  const Linear_Expression& expr,
			  Coefficient_traits::const_reference denominator) {
  using Implementation::BD_Shapes::div_round_up;

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the shape.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());

  // The image of an empty BDS is empty too.
  shortest_path_closure_assign();
  if (marked_empty())
    return;

  const Coefficient& b = expr.inhomogeneous_term();
  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;
  // Index of the last non-zero coefficient in `expr', if any.
  dimension_type w = 0;
  // Get information about the number of non-zero coefficients in `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
	w = i+1;

  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t == 2, the `expr' is of the general form.

  if (t == 0) {
    // Case 1: expr == b.
    // Remove all constraints on `var'.
    forget_all_dbm_constraints(v);
    // Shortest-path closure is preserved, but not reduction.
    if (marked_shortest_path_reduced())
      status.reset_shortest_path_reduced();
    // Add the constraint `var == b/denominator'.
    add_dbm_constraint(0, v, b, denominator);
    add_dbm_constraint(v, 0, -b, denominator);
    assert(OK());
    return;
  }

  if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& a = expr.coefficient(Variable(w-1));
    if (a == denominator || a == -denominator) {
      // Case 2: expr == a*w + b, with a == +/- denominator.
      if (w == v) {
	// `expr' is of the form: a*v + b.
	if (a == denominator) {
	  if (b == 0)
	    // The transformation is the identity function.
	    return;
	  else {
	    // Translate all the constraints on `var',
	    // adding or subtracting the value `b/denominator'.
	    N d;
	    div_round_up(d, b, denominator);
	    N c;
	    div_round_up(c, -b, denominator);
	    DB_Row<N>& dbm_v = dbm[v];
	    for (dimension_type i = space_dim + 1; i-- > 0; ) {
	      N& dbm_vi = dbm_v[i];
	      assign_add(dbm_vi, dbm_vi, c, ROUND_UP);
	      N& dbm_iv = dbm[i][v];
	      assign_add(dbm_iv, dbm_iv, d, ROUND_UP);
	    }
	    // Both shortest-path closure and reduction are preserved.
	  }
	}
	else {
	  // Here `a == -denominator'.
	  // Remove the binary constraints on `var'.
	  forget_binary_dbm_constraints(v);
	  // Swap the unary constraints on `var'.
	  std::swap(dbm[v][0], dbm[0][v]);
	  // Shortest-path closure is not preserved.
	  status.reset_shortest_path_closed();
	  if (b != 0) {
	    // Translate the unary constraints on `var',
	    // adding or subtracting the value `b/denominator'.
	    N c;
	    div_round_up(c, -b, denominator);
	    N& dbm_v0 = dbm[v][0];
	    assign_add(dbm_v0, dbm_v0, c, ROUND_UP);
	    N d;
	    div_round_up(d, b, denominator);
	    N& dbm_0v = dbm[0][v];
	    assign_add(dbm_0v, dbm_0v, d, ROUND_UP);
	  }
	}
      }
      else {
	// Here `w != v', so that `expr' is of the form
	// +/-denominator * w + b.
	// Remove all constraints on `var'.
	forget_all_dbm_constraints(v);
	// Shortest-path closure is preserved, but not reduction.
	if (marked_shortest_path_reduced())
	  status.reset_shortest_path_reduced();
	if (a == denominator) {
	  // Add the new constraint `v - w == b/denominator'.
	  add_dbm_constraint(w, v, b, denominator);
	  add_dbm_constraint(v, w, -b, denominator);
	}
	else {
	  // Here a == -denominator, so that we should be adding
	  // the constraint `v + w == b/denominator'.
	  // Approximate it by computing lower and upper bounds for `w'.
	  const N& dbm_w0 = dbm[w][0];
	  if (!is_plus_infinity(dbm_w0)) {
	    // Add the constraint `v <= b/denominator - lower_w'.
	    N d;
	    div_round_up(d, b, denominator);
	    assign_add(dbm[0][v], d, dbm_w0, ROUND_UP);
	    status.reset_shortest_path_closed();
	  }
	  const N& dbm_0w = dbm[0][w];
	  if (!is_plus_infinity(dbm_0w)) {
	    // Add the constraint `v >= b/denominator - upper_w'.
	    N c;
	    div_round_up(c, -b, denominator);
	    assign_add(dbm[v][0], dbm_0w, c, ROUND_UP);
	    status.reset_shortest_path_closed();
	  }
	}
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
  Linear_Expression neg_expr;
  Coefficient neg_b;
  Coefficient neg_den;
  const bool is_sc = (denominator > 0);
  if (!is_sc) {
    neg_expr = -expr;
    neg_b = -b;
    neg_den = -denominator;
  }
  const Linear_Expression& sc_expr = is_sc ? expr : neg_expr;
  const Coefficient& sc_b = is_sc ? b : neg_b;
  const Coefficient& sc_den = is_sc ? denominator : neg_den;

  N pos_sum;
  N neg_sum;
  // Indices of the variables that are unbounded in `this->dbm'.
  // (The initializations are just to quiet a compiler warning.)
  dimension_type pos_pinf_index = 0;
  dimension_type neg_pinf_index = 0;
  // Number of unbounded variables found.
  dimension_type pos_pinf_count = 0;
  dimension_type neg_pinf_count = 0;

  // Approximate the inhomogeneous term.
  assign(pos_sum, raw_value(sc_b), ROUND_UP);
  assign(neg_sum, raw_value(-sc_b), ROUND_UP);

  // Approximate the homogeneous part of `expr'.
  // Note: indices above `w' can be disregarded, as they all have
  // a zero coefficient in `expr'.
  const DB_Row<N>& dbm_0 = dbm[0];
  for (dimension_type i = w; i > 0; --i) {
    const Coefficient& sc_i = sc_expr.coefficient(Variable(i-1));
    const int sign_i = sgn(sc_i);
    if (sign_i > 0) {
      N coeff_i;
      assign(coeff_i, raw_value(sc_i), ROUND_UP);
      // Approximating `sc_expr'.
      if (pos_pinf_count <= 1) {
	const N& up_approx_i = dbm_0[i];
	if (!is_plus_infinity(up_approx_i))
	  assign_add_mul(pos_sum, coeff_i, up_approx_i, ROUND_UP);
	else {
	  ++pos_pinf_count;
	  pos_pinf_index = i;
	}
      }
      // Approximating `-sc_expr'.
      if (neg_pinf_count <= 1) {
	const N& up_approx_minus_i = dbm[i][0];
	if (!is_plus_infinity(up_approx_minus_i))
	  assign_add_mul(neg_sum, coeff_i, up_approx_minus_i, ROUND_UP);
	else {
	  ++neg_pinf_count;
	  neg_pinf_index = i;
	}
      }
    }
    else if (sign_i < 0) {
      N minus_coeff_i;
      assign(minus_coeff_i, raw_value(-sc_i), ROUND_UP);
      // Approximating `sc_expr'.
      if (pos_pinf_count <= 1) {
	const N& up_approx_minus_i = dbm[i][0];
	if (!is_plus_infinity(up_approx_minus_i))
	  assign_add_mul(pos_sum, minus_coeff_i, up_approx_minus_i, ROUND_UP);
	else {
	  ++pos_pinf_count;
	  pos_pinf_index = i;
	}
      }
      // Approximating `-sc_expr'.
      if (neg_pinf_count <= 1) {
	const N& up_approx_i = dbm_0[i];
	if (!is_plus_infinity(up_approx_i))
	  assign_add_mul(neg_sum, minus_coeff_i, up_approx_i, ROUND_UP);
	else {
	  ++neg_pinf_count;
	  neg_pinf_index = i;
	}
      }
    }
  }

  // Remove all constraints on 'v'.
  forget_all_dbm_constraints(v);
  // Shortest-path closure is maintained, but not reduction.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();
  // Return immediately if no approximation could be computed.
  if (pos_pinf_count > 1 && neg_pinf_count > 1) {
    assert(OK());
    return;
  }

  // In the following, shortest-path closure will be definitely lost.
  status.reset_shortest_path_closed();

  // Before computing quotients, the denominator should be approximated
  // towards zero. Since `sc_den' is known to be positive, this amounts to
  // rounding downwards, which is achieved as usual by rounding upwards
  // the negation and negating again the result.
  N down_sc_den;
  assign(down_sc_den, raw_value(-sc_den), ROUND_UP);
  assign_neg(down_sc_den, down_sc_den, ROUND_UP);

  // Exploit the upper approximation, if possible.
  if (pos_pinf_count <= 1) {
    // Compute quotient (if needed).
    if (down_sc_den != 1)
      assign_div(pos_sum, pos_sum, down_sc_den, ROUND_UP);
    // Add the upper bound constraint, if meaningful.
    if (pos_pinf_count == 0) {
      // Add the constraint `v <= pos_sum'.
      DB_Row<N>& dbm_0 = dbm[0];
      assign(dbm_0[v], pos_sum, ROUND_UP);
      // Deduce constraints of the form `v - u', where `u != v'.
      deduce_v_minus_u_bounds(v, w, sc_expr, sc_den, pos_sum);
    }
    else
      // Here `pos_pinf_count == 1'.
      if (pos_pinf_index != v
	  && sc_expr.coefficient(Variable(pos_pinf_index-1)) == sc_den)
	// Add the constraint `v - pos_pinf_index <= pos_sum'.
	assign(dbm[pos_pinf_index][v], pos_sum, ROUND_UP);
  }

  // Exploit the lower approximation, if possible.
  if (neg_pinf_count <= 1) {
    // Compute quotient (if needed).
    if (down_sc_den != 1)
      assign_div(neg_sum, neg_sum, down_sc_den, ROUND_UP);
    // Add the lower bound constraint, if meaningful.
    if (neg_pinf_count == 0) {
      // Add the constraint `v >= -neg_sum', i.e., `-v <= neg_sum'.
      DB_Row<N>& dbm_v = dbm[v];
      assign(dbm_v[0], neg_sum, ROUND_UP);
      // Deduce constraints of the form `u - v', where `u != v'.
      deduce_u_minus_v_bounds(v, w, sc_expr, sc_den, neg_sum);
    }
    else
      // Here `neg_pinf_count == 1'.
      if (neg_pinf_index != v
	  && sc_expr.coefficient(Variable(neg_pinf_index-1)) == sc_den)
	// Add the constraint `v - neg_pinf_index >= -neg_sum',
	// i.e., `neg_pinf_index - v <= neg_sum'.
	assign(dbm[v][neg_pinf_index], neg_sum, ROUND_UP);
  }

  assert(OK());
}

template <typename T>
void
BD_Shape<T>::affine_preimage(const Variable var,
			     const Linear_Expression& expr,
			     Coefficient_traits::const_reference denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of
  // the systems of bounded differences.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  // The image of an empty BDS is empty too.
  shortest_path_closure_assign();
  if (marked_empty())
    return;

  const Coefficient& b = expr.inhomogeneous_term();
  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;
  // Index of the last non-zero coefficient in `expr', if any.
  dimension_type j = 0;
  // Get information about the number of non-zero coefficients in `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
        j = i;

  // Now we know the form of `expr':
  // - If t == 0, then expr = b, with `b' a constant;
  // - If t == 1, then expr = a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t > 1, the `expr' is of the general form.
  if (t == 0) {
    // Case 1: expr = n; remove all constraints on `var'.
    forget_all_dbm_constraints(v);
    // Shortest-path closure is preserved, but not reduction.
    if (marked_shortest_path_reduced())
      status.reset_shortest_path_reduced();
    assert(OK());
    return;
  }

  if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& a = expr.coefficient(Variable(j));
    if (a == denominator || a == -denominator) {
      // Case 2: expr = a*w + b, with a = +/- denominator.
      if (j == var.id())
	// Apply affine_image() on the inverse of this transformation.
	affine_image(var, a*var - b, denominator);
      else {
	// `expr == a*w + b', where `w != v'.
	// Remove all constraints on `var'.
	forget_all_dbm_constraints(v);
	// Shortest-path closure is preserved, but not reduction.
	if (marked_shortest_path_reduced())
	  status.reset_shortest_path_reduced();
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
    // The transformation is invertible.
    Linear_Expression inverse((expr_v + denominator)*var);
    inverse -= expr;
    affine_image(var, inverse, expr_v);
  }
  else {
    // Transformation not invertible: all constraints on `var' are lost.
    forget_all_dbm_constraints(v);
    // Shortest-path closure is preserved, but not reduction.
    if (marked_shortest_path_reduced())
      status.reset_shortest_path_reduced();
  }
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::generalized_affine_image(const Variable var,
				      const Relation_Symbol relsym,
				      const Linear_Expression& expr,
				      Coefficient_traits::const_reference
				      denominator) {
  using Implementation::BD_Shapes::div_round_up;

  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_image(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "e", expr);

  // `var' should be one of the dimensions of the BDS.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 var.id());

  // The relation symbol cannot be a strict relation symbol.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_image(v, r, e, d)",
  		  "r is a strict relation symbol and "
  		  "*this is a BD_Shape");

  if (relsym == EQUAL) {
    // The relation symbol is "==":
    // this is just an affine image computation.
    affine_image(var, expr, denominator);
    assert(OK());
    return;
  }

  // The image of an empty BDS is empty too.
  shortest_path_closure_assign();
  if (marked_empty())
    return;

  const Coefficient& b = expr.inhomogeneous_term();
  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;
  // Index of the last non-zero coefficient in `expr', if any.
  dimension_type w = 0;
  // Get information about the number of non-zero coefficients in `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
	w = i+1;

  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*w + b, where `w' can be `v' or another
  //   variable; in this second case we have to check whether `a' is
  //   equal to `denominator' or `-denominator', since otherwise we have
  //   to fall back on the general form;
  // - If t == 2, the `expr' is of the general form.
  DB_Row<N>& dbm_0 = dbm[0];
  DB_Row<N>& dbm_v = dbm[v];

  if (t == 0) {
    // Case 1: expr == b.
    // Remove all constraints on `var'.
    forget_all_dbm_constraints(v);
    // Both shortest-path closure and reduction are lost.
    status.reset_shortest_path_closed();
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      // Add the constraint `var <= b/denominator'.
      add_dbm_constraint(0, v, b, denominator);
      break;
    case GREATER_THAN_OR_EQUAL:
      // Add the constraint `var >= b/denominator',
      // i.e., `-var <= -b/denominator',
      add_dbm_constraint(v, 0, -b, denominator);
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
    const Coefficient& a = expr.coefficient(Variable(w-1));
    if (a == denominator || a == -denominator) {
      // Case 2: expr == a*w + b, with a == +/- denominator.
      N d;
      switch (relsym) {
      case LESS_THAN_OR_EQUAL:
	div_round_up(d, b, denominator);
	if (w == v) {
	  // `expr' is of the form: a*v + b.
	  // Shortest-path closure and reduction are not preserved.
	  status.reset_shortest_path_closed();
	  if (a == denominator) {
	    // Translate each constraint `v - w <= dbm_wv'
	    // into the constraint `v - w <= dbm_wv + b/denominator';
	    // forget each constraint `w - v <= dbm_vw'.
	    for (dimension_type i = space_dim + 1; i-- > 0; ) {
	      N& dbm_iv = dbm[i][v];
	      assign_add(dbm_iv, dbm_iv, d, ROUND_UP);
	      dbm_v[i] = PLUS_INFINITY;
	    }
	  }
	  else {
	    // Here `a == -denominator'.
	    // Translate the constraint `0 - v <= dbm_v0'
	    // into the constraint `0 - v <= dbm_v0 + b/denominator'.
	    N& dbm_v0 = dbm_v[0];
	    assign_add(dbm_0[v], dbm_v0, d, ROUND_UP);
	    // Forget all the other constraints on `v'.
	    dbm_v0 = PLUS_INFINITY;
	    forget_binary_dbm_constraints(v);
	  }
	}
	else {
	  // Here `w != v', so that `expr' is of the form
	  // +/-denominator * w + b, with `w != v'.
	  // Remove all constraints on `v'.
	  forget_all_dbm_constraints(v);
	  // Shortest-path closure is preserved, but not reduction.
	  if (marked_shortest_path_reduced())
	    status.reset_shortest_path_reduced();
	  if (a == denominator)
	    // Add the new constraint `v - w <= b/denominator'.
	    add_dbm_constraint(w, v, d);
	  else {
	    // Here a == -denominator, so that we should be adding
	    // the constraint `v <= b/denominator - w'.
	    // Approximate it by computing a lower bound for `w'.
	    const N& dbm_w0 = dbm[w][0];
	    if (!is_plus_infinity(dbm_w0)) {
	      // Add the constraint `v <= b/denominator - lb_w'.
	      assign_add(dbm_0[v], d, dbm_w0, ROUND_UP);
	      // Shortest-path closure is not preserved.
	      status.reset_shortest_path_closed();
	    }
	  }
	}
	break;

      case GREATER_THAN_OR_EQUAL:
	div_round_up(d, -b, denominator);
	if (w == v) {
	  // `expr' is of the form: a*w + b.
	  // Shortest-path closure and reduction are not preserved.
	  status.reset_shortest_path_closed();
	  if (a == denominator) {
	    // Translate each constraint `w - v <= dbm_vw'
	    // into the constraint `w - v <= dbm_vw - b/denominator';
	    // forget each constraint `v - w <= dbm_wv'.
	    for (dimension_type i = space_dim + 1; i-- > 0; ) {
	      N& dbm_vi = dbm_v[i];
	      assign_add(dbm_vi, dbm_vi, d, ROUND_UP);
	      dbm[i][v] = PLUS_INFINITY;
	    }
	  }
	  else {
	    // Here `a == -denominator'.
	    // Translate the constraint `0 - v <= dbm_v0'
	    // into the constraint `0 - v <= dbm_0v - b/denominator'.
	    N& dbm_0v = dbm_0[v];
	    assign_add(dbm_v[0], dbm_0v, d, ROUND_UP);
	    // Forget all the other constraints on `v'.
	    dbm_0v = PLUS_INFINITY;
	    forget_binary_dbm_constraints(v);
	  }
	}
	else {
	  // Here `w != v', so that `expr' is of the form
	  // +/-denominator * w + b, with `w != v'.
	  // Remove all constraints on `v'.
	  forget_all_dbm_constraints(v);
	  // Shortest-path closure is preserved, but not reduction.
	  if (marked_shortest_path_reduced())
	    status.reset_shortest_path_reduced();
	  if (a == denominator)
	    // Add the new constraint `v - w >= b/denominator',
	    // i.e., `w - v <= -b/denominator'.
	    add_dbm_constraint(v, w, d);
	  else {
	    // Here a == -denominator, so that we should be adding
	    // the constraint `v >= -w + b/denominator',
	    // i.e., `-v <= w - b/denominator'.
	    // Approximate it by computing an upper bound for `w'.
	    const N& dbm_0w = dbm_0[w];
	    if (!is_plus_infinity(dbm_0w)) {
	      // Add the constraint `-v <= ub_w - b/denominator'.
	      assign_add(dbm_v[0], dbm_0w, d, ROUND_UP);
	      // Shortest-path closure is not preserved.
	      status.reset_shortest_path_closed();
	    }
	  }
	}
	break;

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
  Linear_Expression neg_expr;
  Coefficient neg_b;
  Coefficient neg_den;
  const bool is_sc = (denominator > 0);
  if (!is_sc) {
    neg_expr = -expr;
    neg_b = -b;
    neg_den = -denominator;
  }
  const Linear_Expression& sc_expr = is_sc ? expr : neg_expr;
  const Coefficient& sc_b = is_sc ? b : neg_b;
  const Coefficient& sc_den = is_sc ? denominator : neg_den;

  N sum;
  // Index of variable that is unbounded in `this->dbm'.
  // (The initialization is just to quiet a compiler warning.)
  dimension_type pinf_index = 0;
  // Number of unbounded variables found.
  dimension_type pinf_count = 0;

  switch (relsym) {
  case LESS_THAN_OR_EQUAL:
    // Compute an upper approximation for `expr' into `sum',
    // taking into account the sign of `denominator'.

    // Approximate the inhomogeneous term.
    assign(sum, raw_value(sc_b), ROUND_UP);

    // Approximate the homogeneous part of `sc_expr'.
    // Note: indices above `w' can be disregarded, as they all have
    // a zero coefficient in `expr'.
    for (dimension_type i = w; i > 0; --i) {
      const Coefficient& sc_i = sc_expr.coefficient(Variable(i-1));
      const int sign_i = sgn(sc_i);
      if (sign_i == 0)
	continue;
      // Choose carefully: we are approximating `sc_expr'.
      const N& approx_i = (sign_i > 0) ? dbm_0[i] : dbm[i][0];
      if (is_plus_infinity(approx_i)) {
	if (++pinf_count > 1)
	  break;
	pinf_index = i;
	continue;
      }
      N coeff_i;
      if (sign_i > 0)
	assign(coeff_i, raw_value(sc_i), ROUND_UP);
      else
	assign(coeff_i, raw_value(-sc_i), ROUND_UP);
      assign_add_mul(sum, coeff_i, approx_i, ROUND_UP);
    }

    // Remove all constraints on `v'.
    forget_all_dbm_constraints(v);
    // Shortest-path closure is preserved, but not reduction.
    if (marked_shortest_path_reduced())
      status.reset_shortest_path_reduced();
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
      // the negation and negating again the result.
      N down_sc_den;
      assign(down_sc_den, raw_value(-sc_den), ROUND_UP);
      assign_neg(down_sc_den, down_sc_den, ROUND_UP);
      assign_div(sum, sum, down_sc_den, ROUND_UP);
    }

    if (pinf_count == 0) {
      // Add the constraint `v <= sum'.
      add_dbm_constraint(0, v, sum);
      // Deduce constraints of the form `v - u', where `u != v'.
      deduce_v_minus_u_bounds(v, w, sc_expr, sc_den, sum);
    }
    else if (pinf_count == 1)
      if (pinf_index != v
	  && expr.coefficient(Variable(pinf_index-1)) == denominator)
	// Add the constraint `v - pinf_index <= sum'.
	add_dbm_constraint(pinf_index, v, sum);
    break;

  case GREATER_THAN_OR_EQUAL:
    // Compute an upper approximation for `-expr' into `sum',
    // taking into account the sign of `denominator'.
    // Note: approximating `-expr' from above and then negating the
    // result is the same as approximating `expr' from below.

    // Approximate the inhomogeneous term.
    assign(sum, raw_value(-sc_b), ROUND_UP);

    // Approximate the homogeneous part of `-sc_expr'.
    for (dimension_type i = expr_space_dim + 1; i > 0; --i) {
      const Coefficient& sc_i = sc_expr.coefficient(Variable(i-1));
      const int sign_i = sgn(sc_i);
      if (sign_i == 0)
	continue;
      // Choose carefully: we are approximating `-sc_expr'.
      const N& approx_i = (sign_i > 0) ? dbm[i][0] : dbm_0[i];
      if (is_plus_infinity(approx_i)) {
	if (++pinf_count > 1)
	  break;
	pinf_index = i;
	continue;
      }
      N coeff_i;
      if (sign_i > 0)
	assign(coeff_i, raw_value(sc_i), ROUND_UP);
      else
	assign(coeff_i, raw_value(-sc_i), ROUND_UP);
      assign_add_mul(sum, coeff_i, approx_i, ROUND_UP);
    }

    // Remove all constraints on `var'.
    forget_all_dbm_constraints(v);
    // Shortest-path closure is preserved, but not reduction.
    if (marked_shortest_path_reduced())
      status.reset_shortest_path_reduced();
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
      // the negation and negating again the result.
      N down_sc_den;
      assign(down_sc_den, raw_value(-sc_den), ROUND_UP);
      assign_neg(down_sc_den, down_sc_den, ROUND_UP);
      assign_div(sum, sum, down_sc_den, ROUND_UP);
    }

    if (pinf_count == 0) {
      // Add the constraint `v >= -sum', i.e., `-v <= sum'.
      add_dbm_constraint(v, 0, sum);
      // Deduce constraints of the form `u - v', where `u != v'.
      deduce_u_minus_v_bounds(v, w, sc_expr, sc_den, sum);
    }
    else if (pinf_count == 1)
      if (pinf_index != v
	  && expr.coefficient(Variable(pinf_index-1)) == denominator)
	// Add the constraint `v - pinf_index >= -sum',
	// i.e., `pinf_index - v <= sum'.
	add_dbm_constraint(v, pinf_index, sum);
    break;

  default:
    // We already dealt with the other cases.
    throw std::runtime_error("PPL internal error");
    break;
  }
  assert(OK());
}

template <typename T>
void
BD_Shape<T>::generalized_affine_image(const Linear_Expression& lhs,
				      const Relation_Symbol relsym,
				      const Linear_Expression& rhs) {
  // Dimension-compatibility checks.
  // The dimension of `lhs' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e1", lhs);

  // The dimension of `rhs' should not be greater than the dimension
  // of `*this'.
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  // Strict relation symbols are not admitted for BDSs.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_image(e1, r, e2)",
		  "r is a strict relation symbol and "
		  "*this is a BD_Shape");

  // The image of an empty BDS is empty.
  shortest_path_closure_assign();
  if (marked_empty())
    return;

  // Number of non-zero coefficients in `lhs': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t_lhs = 0;
  // Index of the last non-zero coefficient in `lhs', if any.
  dimension_type j_lhs = 0;
  // Compute the number of the non-zero components of `lhs'.
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0)
      if (t_lhs++ == 1)
	break;
      else
	j_lhs = i;

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
      for (dimension_type i = lhs_vars.size(); i-- > 0; )
	forget_all_dbm_constraints(lhs_vars[i].id() + 1);
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

      for (dimension_type i = lhs_vars.size(); i-- > 0; )
	forget_all_dbm_constraints(lhs_vars[i].id() + 1);

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
      shortest_path_closure_assign();
      assert(!marked_empty());
      for (dimension_type i = lhs_vars.size(); i-- > 0; )
	forget_all_dbm_constraints(lhs_vars[i].id() + 1);
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
BD_Shape<T>::generalized_affine_preimage(const Variable var,
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
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 "e", expr);

  // `var' should be one of the dimensions of the BDS.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 var.id());

  // The relation symbol cannot be a strict relation symbol.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_preimage(v, r, e, d)",
  		  "r is a strict relation symbol and "
  		  "*this is a BD_Shape");

  if (relsym == EQUAL) {
    // The relation symbol is "==":
    // this is just an affine preimage computation.
    affine_preimage(var, expr, denominator);
    assert(OK());
    return;
  }

  // The image of an empty BDS is empty too.
  shortest_path_closure_assign();
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
    const Coefficient inverse_den = - expr_v;
    const Relation_Symbol inverse_relsym
      = (sgn(denominator) == sgn(inverse_den)) ? relsym : reversed_relsym;
    generalized_affine_image(var, inverse_relsym, inverse, inverse_den);
    return;
  }

  // Here `var_coefficient == 0', so that the preimage cannot
  // be easily computed by inverting the affine relation.
  // Shrink the BD shape by adding the constraint induced
  // by the affine relation.
  const Coefficient& b = expr.inhomogeneous_term();
  // Number of non-zero coefficients in `expr': will be set to
  // 0, 1, or 2, the latter value meaning any value greater than 1.
  dimension_type t = 0;
  // Index of the last non-zero coefficient in `expr', if any.
  dimension_type j = 0;
  // Get information about the number of non-zero coefficients in `expr'.
  for (dimension_type i = expr_space_dim; i-- > 0; )
    if (expr.coefficient(Variable(i)) != 0)
      if (t++ == 1)
	break;
      else
	j = i+1;

  // Now we know the form of `expr':
  // - If t == 0, then expr == b, with `b' a constant;
  // - If t == 1, then expr == a*j + b, where `j != v';
  // - If t == 2, the `expr' is of the general form.
  DB_Row<N>& dbm_0 = dbm[0];

  if (t == 0) {
    // Case 1: expr == b.
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      // Add the constraint `var <= b/denominator'.
      add_dbm_constraint(0, v, b, denominator);
      break;
    case GREATER_THAN_OR_EQUAL:
      // Add the constraint `var >= b/denominator',
      // i.e., `-var <= -b/denominator',
      add_dbm_constraint(v, 0, -b, denominator);
      break;
    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }
  else if (t == 1) {
    // Value of the one and only non-zero coefficient in `expr'.
    const Coefficient& expr_j = expr.coefficient(Variable(j-1));
    N d;
    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      div_round_up(d, b, denominator);
      // Note that: `j != v', so that `expr' is of the form
      // expr_j * j + b, with `j != v'.
      if (expr_j == denominator)
	// Add the new constraint `v - j <= b/denominator'.
	add_dbm_constraint(j, v, d);
      else {
	// Here expr_j != denominator, so that we should be adding
	// the constraint `v <= b/denominator - j'.
	N sum;
	// Approximate the homogeneous part of `expr'.
	const int sign_j = sgn(expr_j);
	const N& approx_j = (sign_j > 0) ? dbm_0[j] : dbm[j][0];
	if (!is_plus_infinity(approx_j)) {
	  N coeff_j;
	  if (sign_j > 0)
	    assign(coeff_j, raw_value(expr_j), ROUND_UP);
	  else
	    assign(coeff_j, raw_value(-expr_j), ROUND_UP);
	  assign_add_mul(sum, coeff_j, approx_j, ROUND_UP);
	  add_dbm_constraint(0, v, sum);
	}
      }
      break;

    case GREATER_THAN_OR_EQUAL:
      div_round_up(d, -b, denominator);
      // Note that: `j != v', so that `expr' is of the form
      // expr_j * j + b, with `j != v'.
      if (expr_j == denominator)
	// Add the new constraint `v - j >= b/denominator'.
	add_dbm_constraint(j, v, d);
      else {
	// Here expr_j != denominator, so that we should be adding
	// the constraint `v <= b/denominator - j'.
	N sum;
	// Approximate the homogeneous part of `expr_j'.
	const int sign_j = sgn(expr_j);
	const N& approx_j = (sign_j > 0) ? dbm_0[j] : dbm[j][0];
	if (!is_plus_infinity(approx_j)) {
	  N coeff_j;
	  if (sign_j > 0)
	    assign(coeff_j, raw_value(expr_j), ROUND_UP);
	  else
	    assign(coeff_j, raw_value(-expr_j), ROUND_UP);
	  assign_add_mul(sum, coeff_j, approx_j, ROUND_UP);
	  add_dbm_constraint(0, v, sum);
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
    Linear_Expression neg_expr;
    Coefficient neg_b;
    Coefficient neg_den;
    const bool is_sc = (denominator > 0);
    if (!is_sc) {
      neg_expr = -expr;
      neg_b = -b;
      neg_den = -denominator;
    }
    const Linear_Expression& sc_expr = is_sc ? expr : neg_expr;
    const Coefficient& sc_b = is_sc ? b : neg_b;
    const Coefficient& sc_den = is_sc ? denominator : neg_den;

    N sum;
    // Index of variable that is unbounded in `this->dbm'.
    // (The initialization is just to quiet a compiler warning.)
    dimension_type pinf_index = 0;
    // Number of unbounded variables found.
    dimension_type pinf_count = 0;

    switch (relsym) {
    case LESS_THAN_OR_EQUAL:
      // Compute an upper approximation for `expr' into `sum',
      // taking into account the sign of `denominator'.

      // Approximate the inhomogeneous term.
      assign(sum, raw_value(sc_b), ROUND_UP);

      // Approximate the homogeneous part of `sc_expr'.
      // Note: indices above `w' can be disregarded, as they all have
      // a zero coefficient in `expr'.
      for (dimension_type i = j; i > 0; --i) {
	const Coefficient& sc_i = sc_expr.coefficient(Variable(i-1));
	const int sign_i = sgn(sc_i);
	if (sign_i == 0)
	  continue;
	// Choose carefully: we are approximating `sc_expr'.
	const N& approx_i = (sign_i > 0) ? dbm_0[i] : dbm[i][0];
	if (is_plus_infinity(approx_i)) {
	  if (++pinf_count > 1)
	    break;
	  pinf_index = i;
	  continue;
	}
	N coeff_i;
	if (sign_i > 0)
	  assign(coeff_i, raw_value(sc_i), ROUND_UP);
	else
	  assign(coeff_i, raw_value(-sc_i), ROUND_UP);
	assign_add_mul(sum, coeff_i, approx_i, ROUND_UP);
      }

      // Divide by the (sign corrected) denominator (if needed).
      if (sc_den != 1) {
	// Before computing the quotient, the denominator should be
	// approximated towards zero. Since `sc_den' is known to be
	// positive, this amounts to rounding downwards, which is achieved
	// as usual by rounding upwards the negation and negating again
	// the result.
	N down_sc_den;
	assign(down_sc_den, raw_value(-sc_den), ROUND_UP);
	assign_neg(down_sc_den, down_sc_den, ROUND_UP);
	assign_div(sum, sum, down_sc_den, ROUND_UP);
      }

      if (pinf_count == 0) {
	// Add the constraint `v <= sum'.
	add_dbm_constraint(0, v, sum);
	// Deduce constraints of the form `v - u', where `u != v'.
	deduce_v_minus_u_bounds(v, j, sc_expr, sc_den, sum);
      }
      else if (pinf_count == 1)
	if (expr.coefficient(Variable(pinf_index-1)) == denominator)
	  // Add the constraint `v - pinf_index <= sum'.
	  add_dbm_constraint(pinf_index, v, sum);
      break;

    case GREATER_THAN_OR_EQUAL:
      // Compute an upper approximation for `-expr' into `sum',
      // taking into account the sign of `denominator'.
      // Note: approximating `-expr' from above and then negating the
      // result is the same as approximating `expr' from below.

      // Approximate the inhomogeneous term.
      assign(sum, raw_value(-sc_b), ROUND_UP);

      // Approximate the homogeneous part of `-sc_expr'.
      for (dimension_type i = j; i > 0; --i) {
	const Coefficient& sc_i = sc_expr.coefficient(Variable(i-1));
	const int sign_i = sgn(sc_i);
	if (sign_i == 0)
	  continue;
	// Choose carefully: we are approximating `-sc_expr'.
	const N& approx_i = (sign_i > 0) ? dbm[i][0] : dbm_0[i];
	if (is_plus_infinity(approx_i)) {
	  if (++pinf_count > 1)
	    break;
	  pinf_index = i;
	  continue;
	}
	N coeff_i;
	if (sign_i > 0)
	  assign(coeff_i, raw_value(sc_i), ROUND_UP);
	else
	  assign(coeff_i, raw_value(-sc_i), ROUND_UP);
	assign_add_mul(sum, coeff_i, approx_i, ROUND_UP);
      }

      // Divide by the (sign corrected) denominator (if needed).
      if (sc_den != 1) {
	// Before computing the quotient, the denominator should be
	// approximated towards zero. Since `sc_den' is known to be
	// positive, this amounts to rounding downwards, which is
	// achieved as usual by rounding upwards the negation and
	// negating again the result.
	N down_sc_den;
	assign(down_sc_den, raw_value(-sc_den), ROUND_UP);
	assign_neg(down_sc_den, down_sc_den, ROUND_UP);
	assign_div(sum, sum, down_sc_den, ROUND_UP);
      }

      if (pinf_count == 0) {
	// Add the constraint `v >= -sum', i.e., `-v <= sum'.
	add_dbm_constraint(v, 0, sum);
	// Deduce constraints of the form `u - v', where `u != v'.
	deduce_u_minus_v_bounds(v, j, sc_expr, sc_den, sum);
      }
      else if (pinf_count == 1)
	if (pinf_index != v
	    && expr.coefficient(Variable(pinf_index-1)) == denominator)
	  // Add the constraint `v - pinf_index >= -sum',
	  // i.e., `pinf_index - v <= sum'.
	  add_dbm_constraint(v, pinf_index, sum);
      break;

    default:
      // We already dealt with the other cases.
      throw std::runtime_error("PPL internal error");
      break;
    }
  }

  // If the shrunk BD_Shape is empty, its preimage is empty too.
  // Note: DO check for emptyness here, as we will later add a line.
  if (is_empty())
    return;
  forget_all_dbm_constraints(v);
  // Shortest-path closure is preserved, but not reduction.
  if (marked_shortest_path_reduced())
    status.reset_shortest_path_reduced();
  assert(OK());
}

template <typename T>
Constraint_System
BD_Shape<T>::constraints() const {
  using Implementation::BD_Shapes::numer_denom;

  Constraint_System cs;
  const dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    if (marked_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (marked_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else if (marked_shortest_path_reduced())
    // Disregard redundant constraints.
    cs = minimized_constraints();
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    Coefficient a;
    Coefficient b;
    // Go through all the unary constraints in `dbm'.
    const DB_Row<N>& dbm_0 = dbm[0];
    for (dimension_type j = 1; j <= space_dim; ++j) {
      const Variable x(j-1);
      const N& dbm_0j = dbm_0[j];
      const N& dbm_j0 = dbm[j][0];
      N negated_dbm_j0;
      if (assign_neg(negated_dbm_j0, dbm_j0, ROUND_NOT_NEEDED) == V_EQ
	  && negated_dbm_j0 == dbm_0j) {
	// We have a unary equality constraint.
	numer_denom(dbm_0j, b, a);
	cs.insert(a*x == b);
      }
      else {
	// We have 0, 1 or 2 unary inequality constraints.
	if (!is_plus_infinity(dbm_0j)) {
	  numer_denom(dbm_0j, b, a);
	  cs.insert(a*x <= b);
	}
	if (!is_plus_infinity(dbm_j0)) {
	  numer_denom(dbm_j0, b, a);
	  cs.insert(-a*x <= b);
	}
      }
    }

    // Go through all the binary constraints in `dbm'.
    for (dimension_type i = 1; i <= space_dim; ++i) {
      const Variable y(i-1);
      const DB_Row<N>& dbm_i = dbm[i];
      for (dimension_type j = i + 1; j <= space_dim; ++j) {
	const Variable x(j-1);
	const N& dbm_ij = dbm_i[j];
	const N& dbm_ji = dbm[j][i];
	N negated_dbm_ji;
	if (assign_neg(negated_dbm_ji, dbm_ji, ROUND_NOT_NEEDED) == V_EQ
	    && negated_dbm_ji == dbm_ij) {
	  // We have a binary equality constraint.
	  numer_denom(dbm_ij, b, a);
	  cs.insert(a*x - a*y == b);
	}
	else {
	  // We have 0, 1 or 2 binary inequality constraints.
	  if (!is_plus_infinity(dbm_ij)) {
	    numer_denom(dbm_ij, b, a);
	    cs.insert(a*x - a*y <= b);
	  }
	  if (!is_plus_infinity(dbm_ji)) {
	    numer_denom(dbm_ji, b, a);
	    cs.insert(a*y - a*x <= b);
	  }
	}
      }
    }
  }
  return cs;
}

template <typename T>
Constraint_System
BD_Shape<T>::minimized_constraints() const {
  using Implementation::BD_Shapes::numer_denom;

  shortest_path_reduction_assign();
  Constraint_System cs;
  const dimension_type space_dim = space_dimension();
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

    Coefficient num;
    Coefficient den;

    // Compute leader information.
    std::vector<dimension_type> leaders;
    compute_leaders(leaders);
    std::vector<dimension_type> leader_indices;
    compute_leader_indices(leaders, leader_indices);
    const dimension_type num_leaders = leader_indices.size();

    // Go through the non-leaders to generate equality constraints.
    const DB_Row<N>& dbm_0 = dbm[0];
    for (dimension_type i = 1; i <= space_dim; ++i) {
      const dimension_type leader = leaders[i];
      if (i != leader)
	// Generate the constraint relating `i' and its leader.
	if (leader == 0) {
	  // A unary equality has to be generated.
	  assert(!is_plus_infinity(dbm_0[i]));
	  numer_denom(dbm_0[i], num, den);
	  cs.insert(den*Variable(i-1) == num);
	}
	else {
	  // A binary equality has to be generated.
	  assert(!is_plus_infinity(dbm[i][leader]));
	  numer_denom(dbm[i][leader], num, den);
	  cs.insert(den*Variable(leader-1) - den*Variable(i-1) == num);
	}
    }

    // Go through the leaders to generate inequality constraints.
    // First generate all the unary inequalities.
    const std::deque<bool>& red_0 = redundancy_dbm[0];
    for (dimension_type l_i = 1; l_i < num_leaders; ++l_i) {
      const dimension_type i = leader_indices[l_i];
      if (!red_0[i]) {
	numer_denom(dbm_0[i], num, den);
	cs.insert(den*Variable(i-1) <= num);
      }
      if (!redundancy_dbm[i][0]) {
	numer_denom(dbm[i][0], num, den);
	cs.insert(-den*Variable(i-1) <= num);
      }
    }
    // Then generate all the binary inequalities.
    for (dimension_type l_i = 1; l_i < num_leaders; ++l_i) {
      const dimension_type i = leader_indices[l_i];
      const DB_Row<N>& dbm_i = dbm[i];
      const std::deque<bool>& red_i = redundancy_dbm[i];
      for (dimension_type l_j = l_i + 1; l_j < num_leaders; ++l_j) {
	const dimension_type j = leader_indices[l_j];
	if (!red_i[j]) {
	  numer_denom(dbm_i[j], num, den);
	  cs.insert(den*Variable(j-1) - den*Variable(i-1) <= num);
	}
	if (!redundancy_dbm[j][i]) {
	  numer_denom(dbm[j][i], num, den);
	  cs.insert(den*Variable(i-1) - den*Variable(j-1) <= num);
	}
      }
    }
  }
  return cs;
}

/*! \relates Parma_Polyhedra_Library::BD_Shape */
template <typename T>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const BD_Shape<T>& c) {
  typedef typename BD_Shape<T>::coefficient_type N;
  if (c.is_universe())
    s << "true";
  else {
    // We control empty system of bounded differences.
    dimension_type n = c.space_dimension();
    if (c.marked_empty())
      s << "false";
    else {
      bool first = true;
      for (dimension_type i = 0; i <= n; ++i)
	for (dimension_type j = i + 1; j <= n; ++j) {
	  const N& c_i_j = c.dbm[i][j];
	  const N& c_j_i = c.dbm[j][i];
	  N negated_c_ji;
	  if (assign_neg(negated_c_ji, c_j_i, ROUND_NOT_NEEDED) == V_EQ
	      && negated_c_ji == c_i_j) {
	    // We will print an equality.
	    if (first)
	      first = false;
	    else
	      s << ", ";
	    if (i == 0) {
	      // We have got a equality constraint with one Variable.
	      s << Variable(j - 1);
	      s << " == " << c_i_j;
	    }
	    else {
	      // We have got a equality constraint with two Variables.
	      if (c_i_j >= 0) {
		s << Variable(j - 1);
		s << " - ";
		s << Variable(i - 1);
		s << " == " << c_i_j;
	      }
	      else {
		s << Variable(i - 1);
		s << " - ";
		s << Variable(j - 1);
		s << " == " << c_j_i;
	      }
	    }
	  }
	  else {
	    // We will print a non-strict inequality.
	    if (!is_plus_infinity(c_j_i)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      if (i == 0) {
		// We have got a constraint with an only Variable.
		s << Variable(j - 1);
		N v;
		assign_neg(v, c_j_i, ROUND_DOWN);
		s << " >= " << v;
	      }
	      else {
		// We have got a constraint with two Variables.
		if (c_j_i >= 0) {
		  s << Variable(i - 1);
		  s << " - ";
		  s << Variable(j - 1);
		  s << " <= " << c_j_i;
		}
		else {
		  s << Variable(j - 1);
		  s << " - ";
		  s << Variable(i - 1);
		  N v;
		  assign_neg(v, c_j_i, ROUND_DOWN);
		  s << " >= " << v;
		}
	      }
	    }
	    if (!is_plus_infinity(c_i_j)) {
	      if (first)
		first = false;
	      else
		s << ", ";
	      if (i == 0) {
		// We have got a constraint with an only Variable.
		s << Variable(j - 1);
		s << " <= " << c_i_j;
	      }
	      else {
		// We have got a constraint with two Variables.
		if (c_i_j >= 0) {
		  s << Variable(j - 1);
		  s << " - ";
		  s << Variable(i - 1);
		  s << " <= " << c_i_j;
		}
		else {
		  s << Variable(i - 1);
		  s << " - ";
		  s << Variable(j - 1);
		  N v;
		  assign_neg(v, c_i_j, ROUND_DOWN);
		  s << " >= " << v;
		}
	      }
	    }
	  }
	}
    }
  }
  return s;
}

template <typename T>
bool
BD_Shape<T>::OK() const {
  // Check whether the difference-bound matrix is well-formed.
  if (!dbm.OK())
    return false;

  // Check whether the status information is legal.
  if (!status.OK())
    return false;

  // An empty BDS is OK.
  if (marked_empty())
    return true;

  // MINUS_INFINITY cannot occur at all.
  for (dimension_type i = dbm.num_rows(); i-- > 0; )
    for (dimension_type j = dbm.num_rows(); j-- > 0; )
    if (is_minus_infinity(dbm[i][j])) {
#ifndef NDEBUG
      using namespace Parma_Polyhedra_Library::IO_Operators;
      std::cerr << "BD_Shape::dbm[" << i << "][" << i << "] = "
		<< dbm[i][i] << "!"
		<< std::endl;
#endif
      return false;
    }

  // On the main diagonal only PLUS_INFINITY can occur.
  for (dimension_type i = dbm.num_rows(); i-- > 0; )
    if (!is_plus_infinity(dbm[i][i])) {
#ifndef NDEBUG
      using namespace Parma_Polyhedra_Library::IO_Operators;
      std::cerr << "BD_Shape::dbm[" << i << "][" << i << "] = "
		<< dbm[i][i] << "!  (+inf was expected.)"
		<< std::endl;
#endif
      return false;
    }

  // Check whether the shortest-path closure information is legal.
  if (marked_shortest_path_closed()) {
    BD_Shape x = *this;
    x.status.reset_shortest_path_closed();
    x.shortest_path_closure_assign();
    if (x.dbm != dbm) {
#ifndef NDEBUG
      std::cerr << "BD_Shape is marked as closed but it is not!"
		<< std::endl;
#endif
      return false;
    }
  }

  // Check whether the shortest-path reduction information is legal.
  if (marked_shortest_path_reduced()) {
    // A non-redundant constraint cannot be equal to PLUS_INFINITY.
    for (dimension_type i = dbm.num_rows(); i-- > 0; )
      for (dimension_type j = dbm.num_rows(); j-- > 0; )
	if (!redundancy_dbm[i][j] && is_plus_infinity(dbm[i][j])) {
#ifndef NDEBUG
	  using namespace Parma_Polyhedra_Library::IO_Operators;
	  std::cerr << "BD_Shape::dbm[" << i << "][" << i << "] = "
		    << dbm[i][i] << " is marked as non-redundant!"
		    << std::endl;
#endif
	  return false;
	}

    BD_Shape x = *this;
    x.status.reset_shortest_path_reduced();
    x.shortest_path_reduction_assign();
    if (x.redundancy_dbm != redundancy_dbm) {
#ifndef NDEBUG
      std::cerr << "BD_Shape is marked as reduced but it is not!"
		<< std::endl;
#endif
      return false;
    }
  }

  // All checks passed.
  return true;
}

template <typename T>
void
BD_Shape<T>::throw_dimension_incompatible(const char* method,
					  const BD_Shape& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
BD_Shape<T>::throw_dimension_incompatible(const char* method,
					  dimension_type required_dim) const {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
BD_Shape<T>::throw_dimension_incompatible(const char* method,
					  const Constraint& c) const {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", c->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
BD_Shape<T>::throw_dimension_incompatible(const char* method,
					  const Generator& g) const {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", g->space_dimension == " << g.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
BD_Shape<T>::throw_constraint_incompatible(const char* method) {
  std::ostringstream s;
  s << "PPL::BD_Shape::" << method << ":" << std::endl
    << "the constraint is incompatible.";
  throw std::invalid_argument(s.str());
}

template <typename T>
void
BD_Shape<T>::throw_expression_too_complex(const char* method,
					  const Linear_Expression& e) {
  using namespace IO_Operators;
  std::ostringstream s;
  s << "PPL::BD_Shape::" << method << ":" << std::endl
    << e << " is too complex.";
  throw std::invalid_argument(s.str());
}


template <typename T>
void
BD_Shape<T>::throw_dimension_incompatible(const char* method,
					  const char* name_row,
					  const Linear_Expression& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}


template <typename T>
void
BD_Shape<T>::throw_generic(const char* method, const char* reason) {
  std::ostringstream s;
  s << "PPL::";
  s << "BD_Shape::" << method << ":" << std::endl
    << reason;
  throw std::invalid_argument(s.str());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_BD_Shape_inlines_hh)
