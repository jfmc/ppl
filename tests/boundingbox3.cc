/* Test building a polyhedron from closed interval-based bounding box.
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

// The box is the xy plane.
static void
test1() {
  BoundingBox box(2);

  C_Polyhedron ph(box, From_Bounding_Box());

  C_Polyhedron known_ph(box.space_dimension());

#if NOISY
  print_generators(ph, "*** test1 ph ***");
  print_generators(known_ph, "*** test1 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This box is the closed +ve quadrant.
static void
test2() {
  BoundingBox box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test2 ph ***");
  print_generators(known_ph, "*** test2 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// A bounded box in 2D.
static void
test3() {
  BoundingBox box(2);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, 4, 1);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, 12, 3);

  C_Polyhedron ph(box, From_Bounding_Box());
  
  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y >= -10);

#if NOISY
  print_generators(ph, "*** test3 ph ***");
  print_generators(known_ph, "*** test3 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a unbounded closed box in 4D but bounded in 2D.
static void
test4() {
  BoundingBox box(4);
  box.raise_lower_bound(1, true, -2, 3);
  box.lower_upper_bound(1, true, 4, 1);
  box.raise_lower_bound(2, true, -10, 1);
  box.lower_upper_bound(2, true, 12, 3);
  box.raise_lower_bound(3, true, 15, 3);

  C_Polyhedron ph(box, From_Bounding_Box());
  
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(3*x >= -2);
  known_ph.add_constraint(x <= 4);
  known_ph.add_constraint(y <= 4);
  known_ph.add_constraint(y >= -10);
  known_ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test4 ph ***");
  print_generators(known_ph, "*** test4 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This is a zero-dimensional box.
static void
test5() {
  BoundingBox box(0);

  C_Polyhedron ph(box, From_Bounding_Box());

  C_Polyhedron known_ph;

#if NOISY
  print_generators(ph, "*** test5 ph ***");
  print_generators(known_ph, "*** test5 known_ph ***");
#endif

   if (ph != known_ph)
     exit(1);
}

// This is an empty closed box in 2D.
static void
test6() {
  BoundingBox box(2);
  box.set_empty();

  C_Polyhedron ph(box, From_Bounding_Box());

#if NOISY
  print_constraints(ph, "*** test6 ph ***");
#endif

  C_Polyhedron known_ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(known_ph, "*** test6 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This box is a single point.
static void
test7() {
  BoundingBox box(2);
  box.raise_lower_bound(0, true, 2, 1);
  box.lower_upper_bound(0, true, 2, 1);
  box.raise_lower_bound(1, true, 4, 1);
  box.lower_upper_bound(1, true, 4, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x == 2);
  known_ph.add_constraint(y == 4);

#if NOISY
  print_generators(ph, "*** test7 ph ***");
  print_generators(known_ph, "*** test7 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// This box is a closed unit square.
static void
test8() {
  BoundingBox box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

  Variable x(0);
  Variable y(1);

  ConSys known_cs;
  known_cs.insert(x >= 0);
  known_cs.insert(x <= 1);
  known_cs.insert(y >= 0);
  known_cs.insert(y <= 1);

  C_Polyhedron known_ph(known_cs);

#if NOISY
  print_generators(ph, "*** test8 ph generators ***");
  print_generators(known_ph, "*** test8 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

// Constructs the polyhedron { x >= 0, x <= 1/2, y >= 0 }
// from the corresponding box.
static void
test9() {
  BoundingBox box(2);

  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 2);
  box.raise_lower_bound(1, true, 0, 1);

  C_Polyhedron ph(box, From_Bounding_Box());

#if NOISY
  print_generators(ph, "*** test9 ph ***");
#endif

  Variable x(0);
  Variable y(1);

  C_Polyhedron known_ph(box.space_dimension());
  known_ph.add_constraint(x >= 0);
  known_ph.add_constraint(2*x <= 1);
  known_ph.add_constraint(y >= 0);

#if NOISY
  print_generators(known_ph, "*** test9 known_ph ***");
#endif

  if (ph != known_ph)
    exit(1);
}

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();

  return 0;
}
CATCH
