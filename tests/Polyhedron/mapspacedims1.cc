/* Test Polyhedron::map_space_dimensions().
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
  Partial_Function function;

  C_Polyhedron ph1(3);

  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result;

  bool ok = (ph1 == known_result);

  print_constraints(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Partial_Function function;

  C_Polyhedron ph1(3, EMPTY);

  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(0, EMPTY);

  bool ok = (ph1 == known_result);

  print_constraints(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Partial_Function function;
  function.insert(0, 2);
  function.insert(2, 0);
  function.insert(1, 1);

  Generator_System gs;
  gs.insert(point(2*C));
  gs.insert(line(A + B));
  gs.insert(ray(A + C));

  C_Polyhedron ph1(gs);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  Generator_System known_gs;
  known_gs.insert(point(2*A));
  known_gs.insert(line(C + B));
  known_gs.insert(ray(C + A));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Partial_Function function;
  function.insert(0, 1);
  function.insert(2, 0);

  Generator_System gs;
  gs.insert(point());
  gs.insert(ray(A + B));
  gs.insert(ray(A - C));

  C_Polyhedron ph1(gs);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(ray(B));
  known_gs.insert(ray(B - A));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);

  Partial_Function function;
  function.insert(2, 0);
  function.insert(3, 2);
  function.insert(4, 1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(ray(A));
  gs.insert(ray(B));

  C_Polyhedron ph1(gs);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(3, EMPTY);
  known_result.add_generator(point());

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test6() {
  Variable A(0);
  Variable B(1);

  Partial_Function function;
  function.insert(0, 0);
  function.insert(1, 1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(B));
  gs.insert(point(A + B));

  C_Polyhedron ph1(gs);
  C_Polyhedron known_result(ph1);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test7() {
  Variable A(0);
  Variable B(1);

  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);
  function.insert(2, 2);
  function.insert(3, 3);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(2*B));
  gs.insert(point(A + 2*B));

  C_Polyhedron ph1(gs);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(4, EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(point(B));
  known_result.add_generator(point(2*A));
  known_result.add_generator(point(2*A + B));

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test8() {
  Variable A(0);
  Variable B(1);

  Partial_Function function;
  function.insert(0, 0);
  function.insert(2, 1);
  function.insert(3, 2);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(ray(B));
  gs.insert(ray(A + B));

  C_Polyhedron ph1(gs);

  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(3, EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(ray(A));

  bool ok = (ph1 == known_result);

  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

void
test9() {
  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);

  C_Polyhedron ph1(3, EMPTY);

  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(2, EMPTY);

  bool ok = (ph1 == known_result);

  print_constraints(ph1, "*** After ph1.map_space_dimensions(function) ***");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  return 0;
}
CATCH
