/* Box class implementation: inline functions.
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

#ifndef PPL_Box_inlines_hh
#define PPL_Box_inlines_hh 1

#include "Boundary.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline
Box<Interval>::Box(dimension_type num_dimensions)
  : vec(num_dimensions), empty(false), empty_up_to_date(true) {
}

template <typename Interval>
inline dimension_type
Box<Interval>::space_dimension() const {
  return vec.size();
}

template <typename Interval>
inline const Interval&
Box<Interval>::operator[](const dimension_type k) const {
  assert(k < vec.size());
  return vec[k];
}

template <typename Interval>
inline bool
Box<Interval>::is_empty() const {
  if (empty_up_to_date)
    return empty;
  else {
    empty_up_to_date = true;
    for (dimension_type k = vec.size(); k-- > 0; )
      if (vec[k].is_empty()) {
	empty = true;
	return true;
      }
    empty = false;
    return false;
  }
}

template <typename Interval>
inline bool
Box<Interval>::get_lower_bound(const dimension_type k, bool& closed,
			       Coefficient& n, Coefficient& d) const {
  assert(k < vec.size());
  const typename Interval::boundary_type& lb = vec[k].lower();

  if (test_boundary_property(lb, Boundary::UNBOUNDED))
    return false;

  closed = !test_boundary_property(lb, Boundary::OPEN);

  mpq_class lr;
  assign_r(lr, lb, ROUND_NOT_NEEDED);
  n = lr.get_num();
  d = lr.get_den();

  return true;
}

template <typename Interval>
inline bool
Box<Interval>::get_upper_bound(const dimension_type k, bool& closed,
			       Coefficient& n, Coefficient& d) const {
  assert(k < vec.size());
  const typename Interval::boundary_type& ub = vec[k].upper();

  if (test_boundary_property(ub, Boundary::UNBOUNDED))
    return false;

  closed = !test_boundary_property(ub, Boundary::OPEN);

  mpq_class ur;
  assign_r(ur, ub, ROUND_NOT_NEEDED);
  n = ur.get_num();
  d = ur.get_den();

  return true;
}

template <typename Interval>
inline void
Box<Interval>::set_empty() {
  for (dimension_type k = vec.size(); k-- > 0; )
    vec[k].set_empty();
  empty = empty_up_to_date = true;
}

template <typename Interval>
inline void
Box<Interval>::raise_lower_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  assert(k < vec.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // FIXME: intersect vec[k] with (q, +infty).
  empty_up_to_date = false;
}

template <typename Interval>
inline void
Box<Interval>::lower_upper_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  assert(k < vec.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // FIXME: intersect vec[k] with (-infty, q).
  empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_inlines_hh)
