/* Test Polyhedron::map_dimensions().
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
#include "PFunction.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

#if NOISY
static void
print_function(const PFunction& function, const std::string& intro = "",
	       std::ostream& s = std::cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}
#endif

static void
test1() {
  PFunction function;

  C_Polyhedron ph1(3);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result;

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  PFunction function;

  C_Polyhedron ph1(3, C_Polyhedron::EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result(0, C_Polyhedron::EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(0, 2);
  function.insert(2, 0);
  function.insert(1, 1);

  GenSys gs;
  gs.insert(point(2*C));
  gs.insert(line(A + B));
  gs.insert(ray(A + C));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  GenSys known_gs;
  known_gs.insert(point(2*A));
  known_gs.insert(line(C + B));
  known_gs.insert(ray(C + A));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(0, 1);
  function.insert(2, 0);

  GenSys gs;
  gs.insert(point());
  gs.insert(ray(A + B));
  gs.insert(ray(A - C));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  GenSys known_gs;
  known_gs.insert(point());
  known_gs.insert(ray(B));
  known_gs.insert(ray(B - A));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(2, 0);
  function.insert(3, 2);
  function.insert(4, 1);

  GenSys gs;
  gs.insert(point());
  gs.insert(ray(A));
  gs.insert(ray(B));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result(3, C_Polyhedron::EMPTY);
  known_result.add_generator(point());

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test6() {
  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 0);
  function.insert(1, 1);

  GenSys gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(B));
  gs.insert(point(A + B));

  C_Polyhedron ph1(gs);
  C_Polyhedron known_result(ph1);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test7() {
  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);
  function.insert(2, 2);
  function.insert(3, 3);

  GenSys gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(2*B));
  gs.insert(point(A + 2*B));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result(4, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(point(B));
  known_result.add_generator(point(2*A));
  known_result.add_generator(point(2*A + B));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 0);
  function.insert(2, 1);
  function.insert(3, 2);

  GenSys gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(ray(B));
  gs.insert(ray(A + B));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result(3, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(ray(A));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test9() {
  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  C_Polyhedron ph1(3, C_Polyhedron::EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.map_dimensions(function);

  C_Polyhedron known_result(2, C_Polyhedron::EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.map_dimensions(function) ***");
#endif

  if (!ok)
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
