/* Test BD_Shape::BD_Shape(const Generator_System&).
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
#define NOISY 1
#endif

namespace {

void
test1() {
  Generator_System gs;
  TBD_Shape bd(gs);

  TBD_Shape known_result(0, Polyhedron::EMPTY);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  if (!ok)
    exit(1);
}

void
test2() {
  Variable V(10);

  Generator_System gs;
  gs.insert(closure_point(V));
  TBD_Shape bd(gs);

  TBD_Shape known_result(11, Polyhedron::EMPTY);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  if (!ok)
    exit(1);
}

void
test3() {
  Variable V(10);

  Generator_System gs;
  gs.insert(ray(V));
  TBD_Shape bd(gs);

  TBD_Shape known_result(11, Polyhedron::EMPTY);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Generator_System gs;
  gs.insert(ray(A + B));
  gs.insert(point(1*A + 2*B + 3*C + 4*D));
  gs.insert(point(2*A + 3*B + 4*C + 5*D));
  TBD_Shape bd(gs);

  TBD_Shape known_result(4);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(B >= 2);
  known_result.add_constraint(C >= 3);
  known_result.add_constraint(C <= 4);
  known_result.add_constraint(D >= 4);
  known_result.add_constraint(D <= 5);
  known_result.add_constraint(A == B-1);
  known_result.add_constraint(C == D-1);
  known_result.add_constraint(C <= A+2);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  C_Polyhedron ph(4);
  ph.add_constraint(A >= B);
  ph.add_constraint(B >= 2*C);
  ph.add_constraint(C >= 3*D);
  ph.add_constraint(D >= 4);
  ph.add_constraint(A-D <= 50);

  TBD_Shape bd(ph.generators());

  TBD_Shape known_result(4);
  known_result.add_constraint(C <= 30);
  known_result.add_constraint(D >= 4);
  known_result.add_constraint(D <= 10);
  known_result.add_constraint(B - A <= 0);
  known_result.add_constraint(A - D <= 50);
  known_result.add_constraint(B - C >= 12);
  known_result.add_constraint(C - D <= 23);
  known_result.add_constraint(C - D >= 8);

  bool ok = (bd == known_result);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH
