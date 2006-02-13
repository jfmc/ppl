/* Test BD_Shape::BD_Shape(const Generator_System&).
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_test.hh"

namespace {

void
test1() {
  Generator_System gs;
  TBD_Shape bd(gs);

  BD_Shape<mpq_class> known_result(0, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable V(10);

  Generator_System gs;
  gs.insert(closure_point(V));

  try {
    // It is illegal to build a BD_Shape starting from a non-empty
    // generator system having no points.
    TBD_Shape bd(gs);

    // It is an error if the exception is not thrown.
    exit(1);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    // It is an error if the wrong exception is thrown.
    exit(1);
  }
}

void
test3() {
  Variable V(10);

  Generator_System gs;
  gs.insert(ray(V));

  try {
    // It is illegal to build a BD_Shape starting from a non-empty
    // generator system having no points.
    TBD_Shape bd(gs);

    // It is an error if the exception is not thrown.
    exit(1);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
    return;
  }
  catch (...) {
    // It is an error if the wrong exception is thrown.
    exit(1);
  }
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

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(B >= 2);
  known_result.add_constraint(C >= 3);
  known_result.add_constraint(C <= 4);
  known_result.add_constraint(D >= 4);
  known_result.add_constraint(D <= 5);
  known_result.add_constraint(A == B-1);
  known_result.add_constraint(C == D-1);
  known_result.add_constraint(C <= A+2);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd ***");

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

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(C <= 30);
  known_result.add_constraint(D >= 4);
  known_result.add_constraint(D <= 10);
  known_result.add_constraint(B - A <= 0);
  known_result.add_constraint(A - D <= 50);
  known_result.add_constraint(B - C >= 12);
  known_result.add_constraint(C - D <= 23);
  known_result.add_constraint(C - D >= 8);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd ***");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);

  return 0;
}
CATCH
