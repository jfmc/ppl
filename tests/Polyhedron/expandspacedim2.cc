/* Test Polyhedron::expand_space_dimension().
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

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

// Test using constraints for NNC polyhedron.
void
test1() {

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A - B > 2);
  ph1.add_constraint(A + 2*B < 6);
  ph1.add_constraint(B < 6);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.expand_space_dimension(B, 2);

  NNC_Polyhedron known_result(4);
  known_result.add_constraint(A - B > 2);
  known_result.add_constraint(A + 2*B < 6);
  known_result.add_constraint(B < 6);
  known_result.add_constraint(A - C > 2);
  known_result.add_constraint(A + 2*C < 6);
  known_result.add_constraint(C < 6);
  known_result.add_constraint(A - D > 2);
  known_result.add_constraint(A + 2*D < 6);
  known_result.add_constraint(D < 6);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.expand_space_dimension(B, 2) ***");
#endif

  if (!ok)
    exit(1);
}

// Test using generators for NNC polyhedron.
void
test2() {
  NNC_Polyhedron ph1(2, NNC_Polyhedron::EMPTY);
  ph1.add_generator(point(A));
  ph1.add_generator(closure_point(A + B));
  ph1.add_generator(ray(A - B));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.expand_space_dimension(A, 2);

  NNC_Polyhedron known_result(4, NNC_Polyhedron::EMPTY);
  known_result.add_generator(point(A + C + D));
  known_result.add_generator(ray(A -B + C + D));
  known_result.add_generator(closure_point(A + C + 2*D));
  known_result.add_generator(closure_point(A + 2*C + D));
  known_result.add_generator(closure_point(A + 2*C + 2*D));
  known_result.add_generator(closure_point(A + B + C + D));
  known_result.add_generator(closure_point(2*A + C + D));
  known_result.add_generator(closure_point(2*A + C + 2*D));
  known_result.add_generator(closure_point(2*A + 2*C + D));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After ph1.expand_space_dimension(A, 2) ***");
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
