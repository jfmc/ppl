/* Test BD_Shape::add_space_dimensions_and_embed() and
   BD_Shape::add_space_dimensions_and_project().
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

static void
test1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(0, Polyhedron::UNIVERSE);
  TBD_Shape known_result(3, Polyhedron::UNIVERSE);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.add_space_dimensions_and_embed(3);
  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.add_space_dimension_and_embed(3) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(0);
  TBD_Shape known_result(4);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.add_space_dimensions_and_project(4);

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(4) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(A == 1);
  bd1.add_constraint(C - B >= 9);

  TBD_Shape known_result(bd1);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif

 bd1.add_space_dimensions_and_project(0);

#if NOISY
  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(0) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(1);
  TBD_Shape known_result(4);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.add_space_dimensions_and_project(3);

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(3) ***");
#endif

  if (!ok)
    exit(1);
}

int
main() TRY {

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
