/* BoundingBox class implementation: inline functions.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_BoundingBox_inlines_hh
#define PPL_BoundingBox_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
BoundingBox::BoundingBox(dimension_type num_dimensions)
  : vec(num_dimensions), empty(false), empty_up_to_date(true) {
}

inline dimension_type
BoundingBox::space_dimension() const {
  return vec.size();
}

inline const Interval&
BoundingBox::operator[](const dimension_type k) const {
  assert(k < vec.size());
  return vec[k];
}

inline bool
BoundingBox::is_empty() const {
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
BoundingBox::get_lower_bound(const dimension_type k, bool& closed,
			     Integer& n, Integer& d) const {
  assert(k < vec.size());
  const LBoundary& lb = vec[k].lower_bound();
  const ERational& lr = lb.bound();

  if (lr.direction_of_infinity() != 0)
    return false;

  closed = lb.is_closed();
  n = lr.numerator();
  d = lr.denominator();

  return true;
}

inline bool
BoundingBox::get_upper_bound(const dimension_type k, bool& closed,
			     Integer& n, Integer& d) const {
  assert(k < vec.size());
  const UBoundary& ub = vec[k].upper_bound();
  const ERational& ur = ub.bound();

  if (ur.direction_of_infinity() != 0)
    return false;

  closed = ub.is_closed();
  n = ur.numerator();
  d = ur.denominator();
  return true;
}

inline void
BoundingBox::set_empty() {
  empty = empty_up_to_date = true;
}

inline void
BoundingBox::raise_lower_bound(const dimension_type k, const bool closed,
			       const Integer& n, const Integer& d) {
  assert(k < vec.size());
  assert(d != 0);
  vec[k].raise_lower_bound(LBoundary(ERational(n, d),
				     (closed
				      ? LBoundary::CLOSED
				      : LBoundary::OPEN)));
  empty_up_to_date = false;
}

inline void
BoundingBox::lower_upper_bound(const dimension_type k, const bool closed,
			       const Integer& n, const Integer& d) {
  assert(k < vec.size());
  assert(d != 0);
  vec[k].lower_upper_bound(UBoundary(ERational(n, d),
				     (closed
				      ? UBoundary::CLOSED
				      : UBoundary::OPEN)));
  empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_BoundingBox_inlines_hh)
