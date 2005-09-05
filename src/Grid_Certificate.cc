/* Grid_Certificate class implementation
   (non-inline member functions).
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

#include "Grid_Certificate.defs.hh"

#include "Grid.defs.hh"
#include <cassert>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

PPL::Grid_Certificate::Grid_Certificate(const Grid& cgr) {
  Grid& gr = const_cast<Grid&>(cgr);
  // As in Polyhedron it is assumed that `gr' contains points.
  assert(!gr.marked_empty());
  // One of the systems must be in minimal form.
  if (gr.congruences_are_up_to_date())
    if (gr.congruences_are_minimized()) {
      num_proper_congruences = gr.con_sys.num_proper_congruences();
      num_equalities = gr.con_sys.num_equalities();
    }
    else
      if (gr.generators_are_up_to_date() && gr.generators_are_minimized()) {
	// FIX Calculate number of congruences from generators.
	num_proper_congruences = gr.gen_sys.num_rays() + 1 /* Integrality cg. */;
	num_equalities = gr.space_dimension() + 1 - gr.gen_sys.num_rows();
      }
      else {
	// Minimize gr congruence system.
	gr.con_sys.normalize_moduli();
	// As in Polyhedron it is assumed that `gr' contains points.
#ifndef NDEBUG
	Grid::simplify(gr.con_sys, gr.dim_kinds);
#else
	assert(Grid::simplify(gr.con_sys, gr.dim_kinds));
#endif
	gr.set_congruences_minimized();

	num_proper_congruences = gr.con_sys.num_proper_congruences();
	num_equalities = gr.con_sys.num_equalities();
      }
  else {
    if (!gr.generators_are_minimized()) {
      // Minimize gr generator system.
      // As in Polyhedron it is assumed that `gr' contains points.
#ifndef NDEBUG
      Grid::simplify(gr.gen_sys, gr.dim_kinds);
#else
      assert(Grid::simplify(gr.gen_sys, gr.dim_kinds));
#endif
      gr.set_generators_minimized();
    }
    // FIX Calculate number of congruences from generators.
    num_proper_congruences = gr.gen_sys.num_rays() + 1 /* Integrality cg. */;
    num_equalities = gr.space_dimension() + 1 - gr.gen_sys.num_rows();
  }
}

int
PPL::Grid_Certificate::compare(const Grid_Certificate& y) const {
  assert(OK() && y.OK());
  if (num_equalities == y.num_equalities)
    if (num_proper_congruences == y.num_proper_congruences)
      return 0;
    else
      return num_proper_congruences > y.num_proper_congruences ? 1 : -1;
  return num_equalities > y.num_equalities ? 1 : -1;
}

int
PPL::Grid_Certificate::compare(const Grid& gr) const {
  // FIX creating gc is a waste
  Grid_Certificate gc(gr);
  return(compare(gc));
}

bool
PPL::Grid_Certificate::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // All tests passed.
  return true;
}
