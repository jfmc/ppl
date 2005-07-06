/* Test building a polyhedron from a non-closed interval-based bounding box.
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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

// This is a unbounded box in 4D but bounded in 2D with strict inequalities.
void
test1() {
  Bounding_Box box(4);
  box.raise_lower_bound(1, false, -2, 3);
  box.lower_upper_bound(1, true, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);
  box.raise_lower_bound(3, true, 15, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x > -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);
  known_ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test1 ph ***");
  print_generators(known_ph, "*** test1 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a bounded NNC polyhedron with strict inequalities
// causing upper and lower bounds of the box to be open.
void
test2() {
  Bounding_Box box(4);
  box.raise_lower_bound(1, true, -2, 3);
  box.lower_upper_bound(1, false, 4, 1);
  box.raise_lower_bound(2, false, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);

  NNC_Polyhedron ph(box, From_Bounding_Box());

  Variable x(1);
  Variable y(2);

  NNC_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x < 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y > -10);

#if NOISY
  print_generators(ph, "*** test2 ph ***");
  print_generators(known_ph, "*** test2 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is an empty box in 2D.
void
test3() {
  Bounding_Box box(2);
  box.set_empty();

  NNC_Polyhedron ph(box, From_Bounding_Box());

#if NOISY
  print_constraints(ph, "*** test3 ph ***");
#endif

  NNC_Polyhedron known_ph(2, EMPTY);

#if NOISY
  print_constraints(known_ph, "*** test3 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();

  return 0;
}
CATCH
