/* NNC_Polyhedron class implementation (non-inline functions).
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

#include <config.h>

#include "NNC_Polyhedron.defs.hh"
#include "C_Polyhedron.defs.hh"
#include "algorithms.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::NNC_Polyhedron::NNC_Polyhedron(const C_Polyhedron& y)
  : Polyhedron(NOT_NECESSARILY_CLOSED, y.space_dimension(), UNIVERSE) {
  add_constraints(y.constraints());
  assert(OK());
}

bool
PPL::NNC_Polyhedron::poly_hull_assign_if_exact(const NNC_Polyhedron& q) {
  return PPL::poly_hull_assign_if_exact(*this, q);
}
