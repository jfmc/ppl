/* The BoundingBox class.
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

#ifndef _BoundingBox_hh
#define _BoundingBox_hh 1

#include <vector>
#include "Interval.hh"

namespace Parma_Polyhedra_Library {

class BoundingBox {
private:
  std::vector<Interval> vec;

public:
  BoundingBox(unsigned int dimension)
    : vec(dimension) {
  }

  unsigned int space_dimension() const {
    return vec.size();
  }

  const Interval& operator[](size_t k) const {
    return vec[k];
  }

  bool is_empty(unsigned int k) const {
    return vec[k].is_empty();
  }

  bool get_lower_bound(unsigned int k, bool closed,
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

  bool get_upper_bound(unsigned int k, bool closed,
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

  void set_empty(unsigned int k) {
    vec[k].set_empty();
  }

  void raise_lower_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    vec[k].raise_lower_bound(LBoundary(ExtendedRational(n, d),
				       (closed
					? LBoundary::CLOSED
					: LBoundary::OPEN)));
  }

  void lower_upper_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    vec[k].lower_upper_bound(UBoundary(ExtendedRational(n, d),
				       (closed
					? UBoundary::CLOSED
					: UBoundary::OPEN)));
    }
};

} // namespace Parma_Polyhedra_Library

#endif
