/* BoundingBox class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _BoundingBox_inlines_hh
#define _BoundingBox_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
BoundingBox::BoundingBox(unsigned int num_dimensions)
  : vec(num_dimensions), empty(false), empty_up_to_date(true) {
}

inline unsigned int
BoundingBox::space_dimension() const {
  return vec.size();
}

inline const Interval&
BoundingBox::operator[](size_t k) const {
  return vec[k];
}

inline bool
BoundingBox::is_empty() const {
  if (empty_up_to_date)
    return empty;
  else {
    empty_up_to_date = true;
    for (size_t k = vec.size(); k-- > 0; )
      if (vec[k].is_empty()) {
	empty = true;
	return true;
      }
    empty = false;
    return false;
  }
}

inline bool
BoundingBox::get_lower_bound(unsigned int k, bool& closed,
			     Integer& n, Integer& d) const {
  const LBoundary& lb = vec[k].lower_bound();
  const ExtendedRational& lr = lb.bound();

  if (lr.direction_of_infinity() != 0)
    return false;

  closed = lb.is_closed();
  n = lr.numerator();
  d = lr.denominator();

  return true;
}

inline bool
BoundingBox::get_upper_bound(unsigned int k, bool& closed,
			     Integer& n, Integer& d) const {
  const UBoundary& ub = vec[k].upper_bound();
  const ExtendedRational& ur = ub.bound();

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
BoundingBox::raise_lower_bound(unsigned int k, bool closed,
			       const Integer& n, const Integer& d) {
  vec[k].raise_lower_bound(LBoundary(ExtendedRational(n, d),
				     (closed
				      ? LBoundary::CLOSED
				      : LBoundary::OPEN)));
  empty_up_to_date = false;
}

inline void
BoundingBox::lower_upper_bound(unsigned int k, bool closed,
			       const Integer& n, const Integer& d) {
  vec[k].lower_upper_bound(UBoundary(ExtendedRational(n, d),
				     (closed
				      ? UBoundary::CLOSED
				      : UBoundary::OPEN)));
  empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // _BoundingBox_inlines_hh
