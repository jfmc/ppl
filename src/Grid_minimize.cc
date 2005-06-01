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
Grid::minimize(Generator_System& source, Congruence_System& dest) {
  assert(source.num_rows() > 0);

  // FIX check if source,dest minimized? prhps in callers?

  if (simplify(source))
    return true;

  // Resize `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns();
  dest.resize_no_copy(dest_num_rows, dest_num_rows + 1 /* moduli */);

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Congruence& dest_i = dest[i];
    dest_i[i] = 1;
    dimension_type j = dest_num_rows;
    while (--j > i)
      dest_i[j] = 0;
    while (j > 0)
      dest_i[--j] = 0;
  }

  // Populate `dest' with the congruences characterizing the grid
  // described by the generators in `source'.
  conversion(source, dest);

  // FIX can an empty gs dest result from a consistent cgs source?
  // FIX   if so ret according to conversion ret
  // FIX   perhaps trivial congruences?

  return false;
}

bool
Grid::minimize(Congruence_System& source, Linear_System& dest) {
  // FIX should grid add single cong?
  // FIX this is for simplify, at least; check minimize callers
  // FIX is spc_dim 0?
  assert(source.num_rows() > 0);

  source.normalize_moduli();
  if (simplify(source))
    return true;

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns() - 1 /* modulus */;
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  dest.set_index_first_pending_row(dest_num_rows);
  dest.resize_no_copy(dest_num_rows, dest_num_rows);

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Linear_Row& dest_i = dest[i];
    dest_i[i] = 1;
    dest_i.set_is_line_or_equality();
    dest_i.set_necessarily_closed();
    dimension_type j = dest_num_rows;
    while (--j > i)
      dest_i[j] = 0;
    while (j > 0)
      dest_i[--j] = 0;
  }
  dest.set_necessarily_closed();

  // The identity matrix `dest' is not sorted according to the sorting
  // rules in Linear_Row.cc.
  dest.set_sorted(false);

  // Populate `dest' with the generators characterizing the grid
  // described by the congruences in `source'.
  conversion(source, dest);

  // FIX can an empty gs dest result from a consistent cgs source?
  // FIX   if so ret according a conversion ret which indicates consistency

  return false;
}

bool
Grid::add_and_minimize(Congruence_System& source1,
		       Linear_System& dest,
		       const Congruence_System& source2) {
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());

  for (dimension_type row = 0; row < source2.num_rows(); ++row)
    source1.add_row(source2[row]);

  return minimize(source1, dest);
}

bool
Grid::add_and_minimize(Generator_System& source1,
		       Congruence_System& dest,
		       const Generator_System& source2) {
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

  return minimize(source1, dest);
}

} // namespace Parma_Polyhedra_Library
