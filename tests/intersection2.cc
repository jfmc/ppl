/* Intersection of a pyramid with an half-space of variable height.
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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static int
count_points(const C_Polyhedron& ph) {
  if (ph.is_empty() || ph.space_dimension() == 0)
    return 0;

  int count = 0;
  const Generator_System& gs = ph.generators();
  for (Generator_System::const_iterator i = gs.begin(), gs_end = gs.end();
       i != gs_end;
       ++i)
    if (i->type() == Generator::POINT)
      ++count;
  return count;
}

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  // This is the height of the pyramid.
  const Integer pyramid_height = 16;

  // We will intersect it with the half-spaces `z <= k' and `z >= k'
  // with k = i*(height/4) for i = -1, 0, 1, ..., 5.
  struct {
    Integer plane_height;
    int num_points_above;
    int num_points_below;
  } ph_nv[]
      = { {-1*(pyramid_height/4), 5, 0},
	  { 0*(pyramid_height/4), 5, 4},
	  { 1*(pyramid_height/4), 5, 8},
	  { 2*(pyramid_height/4), 5, 8},
	  { 3*(pyramid_height/4), 5, 8},
	  { 4*(pyramid_height/4), 1, 5},
	  { 5*(pyramid_height/4), 0, 5}
      };

  Generator_System gs;
  gs.insert(point(0*x + 0*y + 0*z));
  gs.insert(point(2*x + 0*y + 0*z));
  gs.insert(point(0*x + 2*y + 0*z));
  gs.insert(point(2*x + 2*y + 0*z));
  gs.insert(point(x + y + pyramid_height*z));
  C_Polyhedron pyramid(gs);

#if NOISY
    print_constraints(pyramid, "*** pyramid constraints ***");
    print_generators(pyramid, "*** pyramid generators ***");
#endif

  bool ok = true;

  for (dimension_type i = 0; i <= 6; ++i) {
    // Above.
    C_Polyhedron hyper_space_above(3);
    hyper_space_above.add_constraint(z >= ph_nv[i].plane_height);

    C_Polyhedron computed_result = pyramid;
    computed_result.intersection_assign_and_minimize(hyper_space_above);

    if (ok
	&& count_points(computed_result) != ph_nv[i].num_points_above)
      ok = false;

#if NOISY
    print_constraints(hyper_space_above, "*** hyper_space_above ***");
    print_generators(computed_result, "*** computed_result ***");
#endif

    // Below.
    C_Polyhedron hyper_space_below(3);
    hyper_space_below.add_constraint(z <= ph_nv[i].plane_height);

    computed_result = pyramid;
    computed_result.intersection_assign_and_minimize(hyper_space_below);

    if (ok
	&& count_points(computed_result) != ph_nv[i].num_points_below)
      ok = false;

#if NOISY
    print_constraints(hyper_space_below, "*** hyper_space_below ***");
    print_generators(computed_result, "*** computed_result ***");
#endif
  }
  return ok ? 0 : 1;
}
CATCH
