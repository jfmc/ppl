/* Test BD_Shape::map_space_dimensions().
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif
#include "PFunction.hh"

#if NOISY
static void
print_function(const PFunction& function, const string& intro = "",
	       ostream& s = cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}
#endif


static void
test1() {
  PFunction function;

  TBD_Shape bd1(3);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result;

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  PFunction function;

  TBD_Shape bd1(3, EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result(0, EMPTY);

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  TBD_Shape bd1(3, EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result(2, EMPTY);

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test4() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  PFunction function;
  function.insert(0, 0);
  function.insert(2, 1);

  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 3);

  TBD_Shape bd1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result(2);
  known_result.add_constraint(x == 1);
  known_result.add_constraint(y - x <= 3);

  bool ok = (bd1 == known_result);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  PFunction function;
  function.insert(2, 0);

  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 3);
  cs.insert(z - y <= 7);
  cs.insert(y - x <= 2);

  TBD_Shape bd1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result(1);
  known_result.add_constraint(x <= 4);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}


static void
test6() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  PFunction function;
  function.insert(2, 1);
  function.insert(1, 0);


  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 1);
  cs.insert(z - y <= 7);
  cs.insert(y - x <= 1);

  TBD_Shape bd1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(bd1, "*** bd1 ***");
#endif

  bd1.map_space_dimensions(function);

  TBD_Shape known_result(2);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y - x <= 7);

#if NOISY
  print_constraints(bd1, "*** bd1.map_space_dimensions(function) ***");
#endif

  bool ok = (bd1 == known_result);

  if (!ok)
    exit(1);
}


int
main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();

  return 0;
}
CATCH
