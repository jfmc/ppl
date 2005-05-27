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
  // FIX pending
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  //dest.set_index_first_pending_row(dest_num_rows);
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

  // FIX Should this check that the first congruence is consistent?
  // FIX    (or better, check a return from conversion)
  // FIX ie can an empty cgs result from a valid gs?

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
Grid::add_and_minimize(Congruence_System& source,
		       Linear_System& dest) {
  // FIX same as minimize methods? in ph expects pending

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

  // The identity matrix `dest' is not sorted (see the sorting rules
  // in Linear_Row.cc).
  dest.set_sorted(false);

  // Populate `dest' with the generators characterizing the grid
  // described by the congruences in `source'.
  conversion(source, dest);

  return false;
}

bool
Grid::add_and_minimize(Generator_System& source,
		       Congruence_System& dest) {
  // FIX same as minimize methods? in ph handles pending
  assert(source.num_pending_rows() > 0);

  if (simplify(source))
    return true;

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns();
  // FIX pending
  // Update `index_first_pending' before calling `resize_no_copy()'.
  //dest.set_index_first_pending_row(dest_num_rows);
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
  // FIX   if so ret according a conversion ret which indicates consistency

  return false;
}

bool
Grid::add_and_minimize(Congruence_System& source1,
		       Linear_System& dest,
		       const Congruence_System& source2) {
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());
#if 0
  // FIX must a cgs hold equiv of equality constraint?
  // `source1' and `source2' cannot be empty.
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
#else
  assert(source2.num_rows() > 0);
#endif
#if 0
  // `source1' and `source2' are fully sorted.
  assert(source1.is_sorted() && source1.num_pending_rows() == 0);
  assert(source2.is_sorted() && source2.num_pending_rows() == 0);
  assert(dest.num_pending_rows() == 0);
#endif

  bool added = false;

  const dimension_type old_source1_num_rows = source1.num_rows();
  // `k1' and `k2' run through the rows of `source1' and `source2', resp.
  dimension_type k1 = 0;
  dimension_type k2 = 0;
  dimension_type source2_num_rows = source2.num_rows();
  // FIX is this correct for grids, and worth it?
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the constraints from `source2', as pending rows.
    // We exploit the property that initially both `source1' and `source2'
    // are sorted and index `k1' only scans the non-pending rows of `source1',
    // so that it is not influenced by the pending rows appended to it.
    // This way no duplicate (i.e., trivially redundant) constraint
    // is introduced in `source1'.
    //const int cmp = compare(source1[k1], source2[k2]);
    const int cmp = source1[k1] == source2[k2]; // FIX
    //if (cmp == 0) { // FIX?
    if (cmp) {
      // We found the same row: there is no need to add `source2[k2]'.
      ++k2;
      // By sortedness, since `k1 < old_source1_num_rows', we can
      // increment index `k1' too.
      ++k1;
    }
#if 0
    else if (cmp < 0)
      // By sortedness, we can increment `k1'.
      ++k1;
#endif
    else {
      // Here `cmp > 0'. // FIX  == false
      // By sortedness, `source2[k2]' cannot be in `source1'.
      // We add it as a pending row of `source1' (sortedness unaffected).
      //source1.add_pending_row(source2[k2]);
      added = true, source1.add_row(source2[k2]); // FIX
      // We can increment `k2'.
      ++k2;
    }
  }
  // Have we scanned all the rows in `source2'?
  if (k2 < source2_num_rows)
    // By sortedness, all the rows in `source2' having indexes
    // greater than or equal to `k2' were not in `source1'.
    // We add them as pending rows of 'source1' (sortedness not affected).
    for ( ; k2 < source2_num_rows; ++k2)
      //source1.add_pending_row(source2[k2]);
      added = true, source1.add_row(source2[k2]); // FIX

  //if (source1.num_pending_rows() != 0)
  if (added)
    return add_and_minimize(source1, dest);

  // No row was appended to `source1', because all the constraints
  // in `source2' were already in `source1'.
  return false;
}

bool
Grid::add_and_minimize(Generator_System& source1,
		       Congruence_System& dest,
		       const Generator_System& csource2) {
  // `source1' and `csource2' must contain elements.
  assert(source1.num_rows() > 0 && csource2.num_rows() > 0);
  // `source1' and `csource2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == csource2.num_columns());
#if 0
  // `source1' and `csource2' must be fully sorted.
  assert(source1.is_sorted() && source1.num_pending_rows() == 0);
  assert(csource2.is_sorted() && csource2.num_pending_rows() == 0);
#endif
  //assert(dest.num_pending_rows() == 0); // FIX

  Linear_System source2 = csource2; // FIX must csource2 be const?
  //parameterize(source2);  // FIX check all callers do this

  const dimension_type old_source1_num_rows = source1.num_rows();
  // `k1' and `k2' run through the rows of `source1' and `source2', resp.
  dimension_type k1 = 0;
  dimension_type k2 = 0;
  dimension_type source2_num_rows = source2.num_rows();
  // FIX is this correct for grids, and worth it?
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the generators from `source2', as pending rows.
    // We exploit the property that initially both `source1' and `source2'
    // are sorted and index `k1' only scans the non-pending rows of `source1',
    // so that it is not influenced by the pending rows appended to it.
    // This way no duplicate (i.e., trivially redundant) generators
    // is introduced in `source1'.
    const int cmp = compare(source1[k1], source2[k2]);
    //const int cmp = source1[k1] == source2[k2]; // FIX
    if (cmp == 0) {
      // We found the same row: there is no need to add `source2[k2]'.
      ++k2;
      // By sortedness, since `k1 < old_source1_num_rows', we can
      // increment index `k1' too.
      ++k1;
    }
    else if (cmp < 0)
      // By sortedness, we can increment `k1'.
      ++k1;
    else {
      // Here `cmp > 0'.
      // By sortedness, `source2[k2]' cannot be in `source1'.
      // We add it as a pending row of `source1' (sortedness unaffected).
      source1.add_pending_row(source2[k2]);
      // We can increment `k2'.
      ++k2;
    }
  }
  // Have we scanned all the rows in `source2'?
  if (k2 < source2_num_rows)
    // By sortedness, all the rows in `source2' having indexes
    // greater than or equal to `k2' were not in `source1'.
    // We add them as pending rows of 'source1' (sortedness not affected).
    for ( ; k2 < source2_num_rows; ++k2)
      source1.add_pending_row(source2[k2]);

  if (source1.num_pending_rows() == 0)
    // No row was appended to `source1', because all the constraints
    // in `source2' were already in `source1'.
    // There is nothing left to do ...
    return false;

  return add_and_minimize(source1, dest);
}

} // namespace Parma_Polyhedra_Library
