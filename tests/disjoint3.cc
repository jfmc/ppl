/* Test check_disjoint(const Polyhedron& x, const Polyhedron& y):
   we apply this function in some particular cases.
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

static NNC_Polyhedron
half_strip(const Generator& p, const LinExpression& e, bool closed = true) {
  assert((p.is_point() && closed) || (p.is_closure_point() && ! closed));

  LinExpression e1(p);
  e1 += 3*Variable(0);

  GenSys gs;
  gs.insert(p);
  gs.insert(ray(e));
  if (closed)
    gs.insert(point(e1, p.divisor()));
  else {
    gs.insert(closure_point(e1, p.divisor()));
    e1 -= Variable(0);
    e1 += e.coefficient(Variable(1)) * p.divisor() * Variable(1);
    gs.insert(point(e1));
  }
  NNC_Polyhedron ph(gs);
  return ph;
}

static void
test1() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1 = half_strip(point(A + B), B);

  NNC_Polyhedron ph2(2, NNC_Polyhedron::EMPTY);
  ph2.add_generator(point(3*A + B));
  ph2.add_generator(closure_point(2*A + B));
  ph2.add_generator(closure_point(4*A + 3*B));
  ph2.add_generator(ray(A - B));

  bool disjoint = check_disjoint(ph1, ph2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  if (disjoint)
    exit(1);
}

static void
test2() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1 = half_strip(point(A + B), B);
  NNC_Polyhedron ph2 = half_strip(closure_point(4*A + B), B, false);

  bool disjoint = check_disjoint(ph1, ph2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  if (!disjoint)
    exit(1);
}

static void
test3() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1 = half_strip(point(A + B), B);
  NNC_Polyhedron ph2 = half_strip(closure_point(A + B), -B, false);

  bool disjoint = check_disjoint(ph1, ph2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  if (!disjoint)
    exit(1);
}

static void
test4() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1 = half_strip(point(), B);

  NNC_Polyhedron ph2(2, NNC_Polyhedron::EMPTY);
  ph2.add_generator(point(-2*A - 2*B));
  ph2.add_generator(closure_point(2*A - 2*B));
  ph2.add_generator(closure_point(-2*A + 2*B));
  ph2.add_generator(ray(-A - B));

  bool disjoint = check_disjoint(ph1, ph2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  if (!disjoint)
    exit(1);
}

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
