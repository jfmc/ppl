/* BoundingBox class implementation (non-inline functions).
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

#include "BoundingBox.defs.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

/*! \relates Parma_Polyhedra_Library::BoundingBox */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const PPL::BoundingBox& bbox) {
  if (bbox.is_empty()) {
    s << "empty";
    return s;
  }
  const dimension_type dimension = bbox.space_dimension();
  for (dimension_type k = 0; k < dimension; ++k) {
    bool closed = false;
    PPL::Integer n;
    PPL::Integer d;
    if (bbox.get_lower_bound(k, closed, n, d)) {
      s << (closed ? "[" : "(")
	<< n;
      if (d != 1)
	s << "/" << d;
      s << ", ";
    }
    else
      s << "(-inf, ";
    if (bbox.get_upper_bound(k, closed, n, d)) {
      s << n;
      if (d != 1)
	s << "/" << d;
      s << (closed ? "]" : ")");
    }
    else
      s << "+inf)";
    s << std::endl;
  }
  return s;
}
