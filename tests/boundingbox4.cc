/* Test Polyhedron::bounding_box().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

static void
test1() {
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y > 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

  BoundingBox pbox(4);
  ph.shrink_bounding_box(pbox, POLYNOMIAL);

  BoundingBox nbox(4);
  ph.shrink_bounding_box(nbox);

  NNC_Polyhedron known_pph(pbox, From_Bounding_Box());
  NNC_Polyhedron known_nph(nbox, From_Bounding_Box());
  known_pph.intersection_assign_and_minimize(ph);
  known_nph.intersection_assign_and_minimize(ph);

#if NOISY
  print_generators(ph, "*** test1 ph ***");
  print_generators(known_pph, "*** test1 known_pph ***");
  print_generators(known_nph, "*** test1 known_nph ***");
#endif

  if (ph != known_pph || ph != known_nph)
    exit(1);
}

// This is a bounded NNC polyhedron with strict inequalities
// causing upper and lower bounds of the box to be open.
static void
test2() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x < 4);
  ph.add_constraint(y <= 4);

  BoundingBox pbox(2);
  ph.shrink_bounding_box(pbox, POLYNOMIAL);

  BoundingBox nbox(2);
  ph.shrink_bounding_box(nbox);

  NNC_Polyhedron known_pph(pbox, From_Bounding_Box());
  known_pph.intersection_assign_and_minimize(ph);

  NNC_Polyhedron known_nph(nbox, From_Bounding_Box());
  known_nph.intersection_assign_and_minimize(ph);

#if NOISY
  print_generators(ph, "*** test2 ph ***");
  print_generators(known_pph, "*** test2 known_pph ***");
  print_generators(known_nph, "*** test2 known_nph ***");
#endif

  if (ph != known_pph || ph != known_nph)
    exit(1);
}

// An empty polyhedron in 2D defined using strict constraints.
static void
test3() {
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x > 0);
  ph.add_constraint(x < 0);
  ph.add_constraint(y > 0);
  ph.add_constraint(y < 0);

  BoundingBox pbox(2);
  ph.shrink_bounding_box(pbox, POLYNOMIAL);

  BoundingBox nbox(2);
  ph.shrink_bounding_box(nbox);

  NNC_Polyhedron known_pph(pbox, From_Bounding_Box());
  NNC_Polyhedron known_nph(nbox, From_Bounding_Box());

#if NOISY
  print_generators(ph, "*** test3 ph ***");
  print_generators(known_pph, "*** test3 known_pph ***");
  print_generators(known_nph, "*** test3 known_nph ***");
#endif

  if (ph != known_pph || ph != known_nph)
    exit(1);
}

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();

  return 0;
}
CATCH
