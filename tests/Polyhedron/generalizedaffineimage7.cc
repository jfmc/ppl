/* Test Polyhedron::generalized_affine_image() with a linear expression
   as the left hand side: when the left hand side is a variable,
   check whether or not the same result is obtained.
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

int
test1() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);
  ph.add_constraint(A <= 4);
  ph.add_constraint(B <= 5);
  ph.add_constraint(A <= B);
#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  C_Polyhedron ph2 = ph;

  ph.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, A+2);
  ph2.generalized_affine_image(-2*B, LESS_THAN_OR_EQUAL, -2*(A+2));

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph, "--- ph after "
		   "ph.generalized_affine_image(B, GREATER_THAN_OR_EQUAL,"
		   " A+2) ---");
#endif

  return retval;
}


int
test2() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);
  ph.add_constraint(A <= 4);
  ph.add_constraint(B <= 5);
  ph.add_constraint(A <= B);
#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  C_Polyhedron ph2 = ph;

  ph.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, A+2, -2);
  ph2.generalized_affine_image(-2*B, LESS_THAN_OR_EQUAL, A+2);

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph, "--- ph after "
		   "ph.generalized_affine_image(B, GREATER_THAN_OR_EQUAL,"
		   " A+2, -2) ---");
#endif

  return retval;
}

int
test3() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(2*A <= 3);
  ph.add_constraint(7*A >= 2);
  ph.add_constraint(3*B >= 1);
  ph.add_constraint(2*A >= B);
#if NOISY
  print_generators(ph, "--- ph ---");
#endif

  C_Polyhedron ph2 = ph;

  ph.generalized_affine_image(B, LESS_THAN_OR_EQUAL, A-B+2, -3);
  ph2.generalized_affine_image(-3*B, GREATER_THAN_OR_EQUAL, A-B+2);

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph, "--- ph after "
		   "ph.generalized_affine_image(B, LESS_THAN_OR_EQUAL,"
		   " A-B+2, -3) ---");
#endif

  return retval;
}


int
test4() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph(2, NNC_Polyhedron::EMPTY);
  ph.add_generator(point(A + B));
  ph.add_generator(closure_point(2*A));
  ph.add_generator(closure_point(2*A + 2*B));
  ph.add_generator(closure_point(3*A + B));
#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  NNC_Polyhedron ph2 = ph;

  ph.generalized_affine_image(B, LESS_THAN, B+2);
  ph2.generalized_affine_image(-2*B, GREATER_THAN, -2*(B+2));

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph, "--- ph after "
		   "ph.generalized_affine_image(B, LESS_THAN, B+2) ---");
#endif

  return retval;
}

int
test5() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(B >= 0);
  ph.add_constraint(A - B >= 0);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  C_Polyhedron ph2 = ph;

  ph.generalized_affine_image(A, EQUAL, A + 2);
  ph2.generalized_affine_image(-2*A, EQUAL, -2*(A + 2));

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph,
		   "*** After ph.generalized_affine_image"
		   "(A, EQUAL, A + 2) ***");
#endif

  return retval;
}


int
test6() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);
  ph.add_generator(point(A + B));
  ph.add_generator(point(3*A + B));
  ph.add_generator(point(A + 3*B));
  ph.add_generator(point(3*A + 3*B));

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  C_Polyhedron ph2 = ph;

  ph.generalized_affine_image(A + B, GREATER_THAN_OR_EQUAL, 2*A - B + 2);
  ph2.generalized_affine_image(-3*(A + B),
			       LESS_THAN_OR_EQUAL,
			       -3*(2*A - B + 2));

  int retval = (ph == ph2) ? 0 : 1;

#if NOISY
  print_generators(ph,
		   "*** After ph.generalized_affine_image"
		   "(A + B, GREATER_THAN_OR_EQUAL, 2*A - B + 2) ***");
#endif

  return retval;
}

} // namespace

int
main() TRY {
  set_handlers();

  return test1()
    || test2()
    || test3()
    || test4()
    || test5()
    || test6();
}
CATCH
