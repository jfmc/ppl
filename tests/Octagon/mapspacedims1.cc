/* Test Octagon::map_space_dimensions().
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

  TOctagon oc1(3);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result;

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  PFunction function;

  TOctagon oc1(3, EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result(0, EMPTY);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test3() {
  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  TOctagon oc1(3, EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result(2, EMPTY);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
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

  TOctagon oc1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result(2);
  known_result.add_constraint(x == 1);
  known_result.add_constraint(y - x <= 3);

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
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

  TOctagon oc1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

 oc1.map_space_dimensions(function);

  TOctagon known_result(1);
  known_result.add_constraint(x <= 4);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
  print_constraints(known_result, "*** known_result ***");
#endif

#if 0
  cout << "oc1" << endl;
  oc1.ascii_dump(cout);

  cout << "result" << endl;
  known_result.ascii_dump(cout);
#endif

  bool ok = (oc1 == known_result);

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

  TOctagon oc1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result(2);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(y <= 2);
  known_result.add_constraint(y - x <= 7);
  known_result.add_constraint(x + y <= 4);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
  print_constraints(known_result, "*** known_result ***");

#endif

  bool ok = (oc1 == known_result);

  if (!ok)
    exit(1);
}

static void
test7() {
  PFunction function;

  TOctagon oc1;

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  oc1.map_space_dimensions(function);

  TOctagon known_result;

  bool ok = (oc1 == known_result);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test8() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  PFunction function;
  function.insert(2, 1);
  function.insert(1, 0);
  function.insert(0, 2);


  Constraint_System cs;
  cs.insert(x == 1);
  cs.insert(z - x <= 1);
  cs.insert(y - z <= -3);
  cs.insert(x - y <= 1);

  TOctagon oc1(cs);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(oc1, "*** oc1 ***");
#endif

  TOctagon known_result(3, EMPTY);
  if (known_result != oc1)
    cout << "The oc1 is not empty!" << endl;

  oc1.map_space_dimensions(function);

#if NOISY
  print_constraints(oc1, "*** oc1.map_space_dimensions(function) ***");
  print_constraints(known_result, "*** known_result ***");

#endif

  bool ok = (oc1 == known_result);

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
  test7();
  test8();

  return 0;
}
CATCH
