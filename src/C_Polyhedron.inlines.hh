/* C_Polyhedron class implementation: inline functions.
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

#ifndef PPL_C_Polyhedron_inlines_hh
#define PPL_C_Polyhedron_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline dimension_type
C_Polyhedron::max_space_dimension() {
  using std::min;
  // One dimension is reserved to have a value of type dimension_type
  // that does not represent a legal dimension.
  return min(std::numeric_limits<dimension_type>::max() - 1,
	     min(ConSys::max_space_dimension(),
		 GenSys::max_space_dimension()
		 )
	     );
}

inline
C_Polyhedron::C_Polyhedron(dimension_type num_dimensions,
			   Degenerate_Kind kind)
  : Polyhedron(NECESSARILY_CLOSED, num_dimensions, kind) {
}

inline
C_Polyhedron::C_Polyhedron(const ConSys& cs)
  : Polyhedron(NECESSARILY_CLOSED, cs) {
}

inline
C_Polyhedron::C_Polyhedron(ConSys& cs)
  : Polyhedron(NECESSARILY_CLOSED, cs) {
}

inline
C_Polyhedron::C_Polyhedron(const GenSys& gs)
  : Polyhedron(NECESSARILY_CLOSED, gs) {
}

inline
C_Polyhedron::C_Polyhedron(GenSys& gs)
  : Polyhedron(NECESSARILY_CLOSED, gs) {
}

template <typename Box>
C_Polyhedron::C_Polyhedron(const Box& box, From_Bounding_Box)
  : Polyhedron(NECESSARILY_CLOSED, box) {
}

inline
C_Polyhedron::C_Polyhedron(const C_Polyhedron& y)
  : Polyhedron(y) {
}

inline C_Polyhedron&
C_Polyhedron::operator=(const C_Polyhedron& y) {
  Polyhedron::operator=(y);
  return *this;
}

inline
C_Polyhedron::~C_Polyhedron() {
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_C_Polyhedron_inlines_hh)
