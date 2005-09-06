/* Grid class implementation
   (non-inline widening-related member functions).
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

#include "Grid.defs.hh"

//#include "Bounding_Box.defs.hh" // FIX
#include <cassert>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid::select_wider_congruences(const Grid& y,
				    Congruence_System& selected_cgs) const {
  // Private method: the caller must ensure the following conditions
  // (beside the inclusion `y <= x').
  assert(space_dim == y.space_dim);
  assert(!marked_empty());
  assert(!y.marked_empty());
  assert(congruences_are_minimized());
  assert(y.congruences_are_minimized());

  // Note: row counters start at 0, to preserve the original order in
  // the selected congruences.
  for (dimension_type dim = con_sys.space_dimension(), x_row = 0, y_row = 0;
       dim > 0; --dim) {
    assert(dim_kinds[dim] == CON_VIRTUAL || dim_kinds[dim] == y.dim_kinds[dim]);
    switch (dim_kinds[dim]) {
    case PROPER_CONGRUENCE:
      {
	const Congruence& cg = con_sys[x_row];
	const Congruence& y_cg = y.con_sys[y_row];
	if (cg[dim] * y_cg.modulus() == y_cg[dim] * cg.modulus())
	  // The leading diagonal entries are equal.
	  selected_cgs.insert(cg);
	++x_row;
	++y_row;
      }
      break;
    case EQUALITY:
      selected_cgs.insert(con_sys[x_row]);
      ++x_row;
      ++y_row;
      break;
    case CON_VIRTUAL:
      y.dim_kinds[dim] == CON_VIRTUAL || ++y_row;
      break;
    }
  }
}

void
PPL::Grid::widening_assign(const Grid& const_y, unsigned* tp) {
  Grid& x = *this;
  Grid& y = const_cast<Grid&>(const_y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("H79_widening_assign(y)", "y", y);

  // As noted in definitions.dox, stable behaviour is only garaunteed
  // if y is contained in or equal to x.
#ifndef NDEBUG
  {
    // Assume y is contained in or equal to x.
    const Grid x_copy = x;
    const Grid y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // Leave `x' the same if `x' or `y' is zero-dimensional or empty.
  if (x.space_dim == 0 || x.marked_empty() || y.marked_empty())
    return;

  // Ensure that the `x' congruences are in minimal form.
  if (x.congruences_are_up_to_date()) {
    if (!x.congruences_are_minimized()) {
      x.con_sys.normalize_moduli();
      if (simplify(x.con_sys, x.dim_kinds)) {
	// `x' is empty.
	x.set_empty();
	return;
      }
      x.set_congruences_minimized();
    }
  }
  else
    if (!x.update_congruences())
      // `x' is empty.
      return;

  // Ensure that the `y' congruences are in minimal form.
  if (y.congruences_are_up_to_date()) {
    if (!y.congruences_are_minimized()) {
      y.con_sys.normalize_moduli();
      if (simplify(y.con_sys, y.dim_kinds)) {
	// `y' is empty.
	y.set_empty();
	return;
      }
      y.set_congruences_minimized();
    }
  }
  else
    if (!y.update_congruences())
      // `y' is empty.
      return;

  if (con_sys.num_equalities() < y.con_sys.num_equalities())
    return;

  // Copy into `cgs' the congruences of `x' that are common to `y',
  // according to the grid widening.
  Congruence_System cgs;
  x.select_wider_congruences(y, cgs);

  if (cgs.num_rows() == con_sys.num_rows())
    // All congruences were selected, thus the result is `x'.
    return;

  // A strict subset of the congruences was selected.

  Grid result(x.space_dim);
  result.add_recycled_congruences(cgs);

  // Check whether we are using the widening-with-tokens technique
  // and there are still tokens available.
  if (tp && *tp > 0) {
    // There are tokens available.  If `result' is not a subset of
    // `x', then it is less precise and we use one of the available
    // tokens.
    if (!x.contains(result))
      --(*tp);
  }
  else
    // No tokens.
    std::swap(x, result);

  assert(x.OK(true));
}

void
PPL::Grid::limited_extrapolation_assign(const Grid& y,
					const Congruence_System& cgs,
					unsigned* tp) {
  Grid& x = *this;

  dimension_type cgs_num_rows = cgs.num_rows();
  // If `cgs' is empty, fall back to ordinary widening.
  if (cgs_num_rows == 0) {
    x.widening_assign(y, tp);
    return;
  }

  // Check dimension compatibility.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("limited_extrapolation_assign(y, cgs)",
				 "y", y);
  // `cgs' must be dimension-compatible with the two polyhedra.
  const dimension_type cgs_space_dim = cgs.space_dimension();
  if (x.space_dim < cgs_space_dim)
    throw_dimension_incompatible("limited_extrapolation_assign(y, cgs)",
				 "cgs", cgs);

#ifndef NDEBUG
  {
    // Assume that y is contained in or equal to x.
    const Grid x_copy = x;
    const Grid y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  if (y.marked_empty())
    return;
  if (x.marked_empty())
    return;

  // The limited widening between two grids in a zero-dimensional
  // space is also a grid in a zero-dimensional space.
  if (x.space_dim == 0)
    return;

  // Update the generators of `x': these are used to select, from the
  // congruences in `cgs', those that must be added to the widened
  // grid.
  if (!x.generators_are_up_to_date() && !x.update_generators())
    // `x' is empty.
    return;

  Congruence_System new_cgs;
  // The congruences to be added need only be satisfied by all the
  // generators of `x', as `y <= x'.  Iterate upwards here, to keep
  // the relative ordering of congruences (just for aesthetics).
  for (dimension_type i = 0; i < cgs_num_rows; ++i) {
    const Congruence& cg = cgs[i];
    if (x.relation_with(cg) == Poly_Con_Relation::is_included())
      new_cgs.insert(cg);
  }
  x.widening_assign(y, tp);
  // FIX If x is the same (due to tp) then adding new_cgs is
  //     redundant.
  x.add_congruences(new_cgs);
  assert(OK());
}
