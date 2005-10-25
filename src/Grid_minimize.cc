/* Grid class implementation: minimize() and add_and_minimize().
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
#include "Linear_Row.defs.hh"
#include "Linear_System.defs.hh"
#include "Grid.defs.hh"

namespace Parma_Polyhedra_Library {

bool
Grid::minimize(Generator_System& source, Congruence_System& dest,
	       Dimension_Kinds& dim_kinds) {
  assert(source.num_rows() > 0);

  // FIX check if source,dest minimized? prhps in callers?

  if (simplify(source, dim_kinds))
    return true;

  // Populate `dest' with the congruences characterizing the grid
  // described by the generators in `source'.
  conversion(source, dest, dim_kinds);

  // FIX can an empty gs dest result from a consistent cgs source?
  // FIX   if so ret according to conversion ret
  // FIX   perhaps trivial congruences?

  return false;
}

bool
Grid::minimize(Congruence_System& source, Linear_System& dest,
	       Dimension_Kinds& dim_kinds) {
  // FIX should grid add single cong?
  // FIX this is for simplify, at least; check minimize callers
  // FIX is spc_dim 0?
  assert(source.num_rows() > 0);

  source.normalize_moduli();
  if (simplify(source, dim_kinds))
    return true;

  // Populate `dest' with the generators characterizing the grid
  // described by the congruences in `source'.
  conversion(source, dest, dim_kinds);

  // FIX can an empty gs dest result from a consistent cgs source?
  // FIX   if so ret according a conversion ret which indicates consistency

  return false;
}

} // namespace Parma_Polyhedra_Library
