/* Test Polyhedron::add_constraints_and_minimize()
   and Polyhedron::add_constraints(): the polyhedron can have
   something pending.
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

namespace {

void
test1() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 0);
  C_Polyhedron copy_ph(ph);

  Constraint_System cs1;
  cs1.insert(A == 0);
  cs1.insert(B >= 0);
  Constraint_System cs2(cs1);

  ph.add_constraints(cs1);
  copy_ph.add_constraints_and_minimize(cs2);

  bool ok = (ph == copy_ph);

#if NOISY
  print_constraints(ph, "*** After ph.add_constraints(cs1) ***");
  print_constraints(ph,
		    "*** After copy_ph.add_constraints_and_minimize(cs2) ***");
#endif

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);
  ph1.add_generator(point());
  ph1.constraints();
  ph1.add_generator(line(A + B));
  C_Polyhedron copy_ph1 = ph1;

  C_Polyhedron ph2(2, C_Polyhedron::EMPTY);
  ph2.add_generator(point());
  ph2.constraints();
  ph2.add_generator(ray(A));
  ph2.add_generator(ray(B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  Constraint_System cs1 = ph2.constraints();
  Constraint_System cs2 = ph2.constraints();

  ph1.add_constraints(cs1);
  copy_ph1.add_constraints_and_minimize(cs2);

  bool ok = (ph1 == copy_ph1);
#if NOISY
  print_constraints(ph1, "*** After add_constraints_assign ***");
  print_constraints(copy_ph1,
		    "*** After add_constraints_and_minimize ***");
#endif

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();

  return 0;
}
CATCH
