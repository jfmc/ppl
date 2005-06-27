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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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

bool
Grid::add_and_minimize(Congruence_System& source1,
		       Linear_System& dest,
		       const Congruence_System& source2,
		       Dimension_Kinds& dim_kinds) {
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());

  for (dimension_type row = 0; row < source2.num_rows(); ++row)
    source1.add_row(source2[row]);

  return minimize(source1, dest, dim_kinds);
}

bool
Grid::add_and_minimize(Generator_System& source1,
		       Congruence_System& dest,
		       const Generator_System& source2,
		       Dimension_Kinds& dim_kinds) {
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());

  for (dimension_type row = 0; row < source2.num_rows(); ++row) {
    const Generator& g = source2[row];
    if (g.is_ray())
      source1.insert(Generator::line(Linear_Expression(g)));
    else
      source1.add_row(g);
  }

  return minimize(source1, dest, dim_kinds);
}

} // namespace Parma_Polyhedra_Library
