/* Definition of functions yielding maximal space dimensions.
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

#ifndef PPL_max_space_dimension_hh
#define PPL_max_space_dimension_hh 1

#include "Polyhedron.defs.hh"
#include "C_Polyhedron.defs.hh"
#include "NNC_Polyhedron.defs.hh"
#include "Polyhedra_Powerset.defs.hh"
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline dimension_type
Polyhedron::max_space_dimension() {
  return std::min(C_Polyhedron::max_space_dimension(),
		  NNC_Polyhedron::max_space_dimension());
}

//! Returns the maximum space dimension this library can handle.
inline dimension_type
max_space_dimension() {
  using std::min;
  return
    min(Polyhedron::max_space_dimension(),
	min(Polyhedra_Powerset<C_Polyhedron>::max_space_dimension(),
	    Polyhedra_Powerset<NNC_Polyhedron>::max_space_dimension()
	    )
	);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_max_space_dimension_hh)
