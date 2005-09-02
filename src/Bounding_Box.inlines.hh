/* Bounding_Box class implementation: inline functions.
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

#ifndef PPL_Bounding_Box_inlines_hh
#define PPL_Bounding_Box_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Bounding_Box::Bounding_Box(dimension_type num_dimensions)
  : vec(num_dimensions), empty(false), empty_up_to_date(true) {
}

inline dimension_type
Bounding_Box::space_dimension() const {
  return vec.size();
}

inline const Interval&
Bounding_Box::operator[](const dimension_type k) const {
  assert(k < vec.size());
  return vec[k];
}

inline bool
Bounding_Box::is_empty() const {
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

inline bool
Bounding_Box::get_lower_bound(const dimension_type k, bool& closed,
			      Coefficient& n, Coefficient& d) const {
  assert(k < vec.size());
  const LBoundary& lb = vec[k].lower_bound();
  const ERational& lr = lb.bound();

  if (is_plus_infinity(lr) || is_minus_infinity(lr))
    return false;

  closed = lb.is_closed();
  n = raw_value(lr).get_num();
  d = raw_value(lr).get_den();

  return true;
}

inline bool
Bounding_Box::get_upper_bound(const dimension_type k, bool& closed,
			     Coefficient& n, Coefficient& d) const {
  assert(k < vec.size());
  const UBoundary& ub = vec[k].upper_bound();
  const ERational& ur = ub.bound();

  if (is_plus_infinity(ur) || is_minus_infinity(ur))
    return false;

  closed = ub.is_closed();
  n = raw_value(ur).get_num();
  d = raw_value(ur).get_den();

  return true;
}

inline void
Bounding_Box::set_empty() {
  for (dimension_type k = vec.size(); k-- > 0; )
    vec[k].set_empty();
  empty = empty_up_to_date = true;
}

inline void
Bounding_Box::raise_lower_bound(const dimension_type k, const bool closed,
				Coefficient_traits::const_reference n,
				Coefficient_traits::const_reference d) {
  assert(k < vec.size());
  assert(d != 0);
  mpq_class q(raw_value(n), raw_value(d));
  q.canonicalize();
  vec[k].raise_lower_bound(LBoundary(ERational(q),
				     (closed
				      ? LBoundary::CLOSED
				      : LBoundary::OPEN)));
  empty_up_to_date = false;
}

inline void
Bounding_Box::lower_upper_bound(const dimension_type k, const bool closed,
				Coefficient_traits::const_reference n,
				Coefficient_traits::const_reference d) {
  assert(k < vec.size());
  assert(d != 0);
  mpq_class q(raw_value(n), raw_value(d));
  q.canonicalize();
  vec[k].lower_upper_bound(UBoundary(ERational(q),
				     (closed
				      ? UBoundary::CLOSED
				      : UBoundary::OPEN)));
  empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Bounding_Box_inlines_hh)
