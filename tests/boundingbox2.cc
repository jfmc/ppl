/* Test Polyhedron::bounding_box().
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

#include "ppl_test.hh"
#include "BBox.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

// This is a unbounded NNC polyhedron in 4D but bounded in 2D
// with strict inequality and closure points at the lower bound.
static void
test1() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y > 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test1 ph ***");
  nbox.print(cout, "*** test1 nbox ***");
  pbox.print(cout, "*** test1 pbox ***");
#endif

  BBox known_nbox(4);
  known_nbox.raise_lower_bound(1, false, -2, 3);
  known_nbox.lower_upper_bound(1, true, 4, 1);
  known_nbox.raise_lower_bound(2, false, -10, 1);
  known_nbox.lower_upper_bound(2, true, 4, 1);
  known_nbox.raise_lower_bound(3, true, 5, 1);

  BBox known_pbox(4);
  known_pbox.lower_upper_bound(1, true, 4, 1);
  known_pbox.lower_upper_bound(2, true, 4, 1);
  known_pbox.raise_lower_bound(3, true, 5, 1);

#if NOISY
  known_nbox.print(cout, "*** test_nnc9 known_nbox ***");
  known_pbox.print(cout, "*** test_nnc9 known_pbox ***");
#endif

  if (nbox != known_nbox || pbox != known_pbox || !(nbox <= pbox))
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

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, POLYNOMIAL);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test1 ph ***");
  nbox.print(cout, "*** test1 nbox ***");
  pbox.print(cout, "*** test1 pbox ***");
#endif

  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, -2, 3);
  known_nbox.lower_upper_bound(0, false, 4, 1);
  known_nbox.raise_lower_bound(1, false, -10, 1);
  known_nbox.lower_upper_bound(1, true, 4, 1);

  BBox known_pbox(2);
  known_pbox.lower_upper_bound(0, false, 4, 1);
  known_pbox.lower_upper_bound(1, true, 4, 1);

#if NOISY
  known_nbox.print(cout, "*** test_nnc10 known_nbox ***");
  known_pbox.print(cout, "*** test_nnc10 known_pbox ***");
#endif

  if (nbox != known_nbox || pbox != known_pbox || !(nbox <= pbox))
    exit(1);
}

int
main() {
  set_handlers();

  test1();
  test2();

  return 0;
}
