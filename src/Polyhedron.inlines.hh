/* Polyhedron class implementation: inline functions.
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


#ifndef _Polyhedron_inlines_hh
#define _Polyhedron_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Polyhedron::Polyhedron(size_t num_dimensions, Degenerate_Kind kind)
  : PolyBase(NECESSARILY_CLOSED, num_dimensions, kind) {
}

inline
Polyhedron::Polyhedron(const Polyhedron& y)
  : PolyBase(y) {
}

inline
Polyhedron::Polyhedron(ConSys& cs)
  : PolyBase(NECESSARILY_CLOSED, cs) {
}

inline
Polyhedron::Polyhedron(GenSys& gs)
  : PolyBase(NECESSARILY_CLOSED, gs) {
}

inline Polyhedron&
Polyhedron::operator=(const Polyhedron& y) {
  PolyBase::operator=(y);
  return *this;
}

inline
Polyhedron::~Polyhedron() {
}

} // namespace Parma_Polyhedra_Library

#endif
