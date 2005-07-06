/* Test Polyhedron::fold_space_dimensions().
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
#define NOISY 0
#endif

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

// Test with a universe polyhedron.
void
test1() {
  C_Polyhedron ph1(3);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(A);

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After folding {A} into B ***");
#endif

  if (!ok)
    exit(1);
}

// Test with an empty polyhedron.
void
test2() {
  C_Polyhedron ph1(3, EMPTY);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(A);

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(2, EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After folding {A} into B ***");
#endif

  if (!ok)
    exit(1);
}

// Trivial fold.
void
test3() {
  C_Polyhedron ph1(3);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A + B + C <= 2);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(3);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A + B + C <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After folding {} into B ***");
#endif

  if (!ok)
    exit(1);
}


// Test as given in [GopanDMDRS04] on page 519.
void
test4() {
  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 1);
  ph1.add_constraint(A <= 3);
  ph1.add_constraint(B >= 7);
  ph1.add_constraint(B <= 12);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(A);

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(1);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(A <= 12);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "***  After folding {A} into B ***");
#endif

  if (!ok)
    exit(1);
}


// Test that takes the expected result of the expand operation
// example given in [GopanDMDRS04] on page 519 and folds it to recover
// the unexpanded polyhedron.
void
test5() {
  C_Polyhedron ph1(3, EMPTY);
  ph1.add_generator(point(A + 2*B + 2*C));
  ph1.add_generator(point(A + 2*B + 3*C));
  ph1.add_generator(point(A + 2*B + 4*C));
  ph1.add_generator(point(A + 3*B + 2*C));
  ph1.add_generator(point(A + 3*B + 3*C));
  ph1.add_generator(point(A + 3*B + 4*C));
  ph1.add_generator(point(A + 4*B + 2*C));
  ph1.add_generator(point(A + 4*B + 3*C));
  ph1.add_generator(point(A + 4*B + 4*C));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(C);

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(2, EMPTY);
  known_result.add_generator(point(A + 2*B));
  known_result.add_generator(point(A + 3*B));
  known_result.add_generator(point(A + 4*B));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After folding {C} into B ***");
#endif

  if (!ok)
    exit(1);
}

// Test folding several dimensions into a higher dimension.
void
test6() {
  C_Polyhedron ph1(3);
  ph1.add_constraint(A >= 1);
  ph1.add_constraint(A <= 3);
  ph1.add_constraint(B >= 7);
  ph1.add_constraint(B <= 12);
  ph1.add_constraint(C == 15);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(A);
  to_fold.insert(B);

  ph1.fold_space_dimensions(to_fold, C);

  C_Polyhedron known_result(1);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(A <= 15);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "***  After folding {A,B} into C ***");
#endif

  if (!ok)
    exit(1);
}

// Test fold_space_dimensions() when there are rays.
void
test7() {
  C_Polyhedron ph1(3, EMPTY);
  ph1.add_generator(point(A));
  ph1.add_generator(ray(A + B));
  ph1.add_generator(ray(A + 2*C));

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(C);

  ph1.fold_space_dimensions(to_fold, B);

  C_Polyhedron known_result(2, EMPTY);
  known_result.add_generator(point(A));
  known_result.add_generator(ray(A));
  known_result.add_generator(ray(A + B));
  known_result.add_generator(ray(A + 2*B));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "***  After folding {C} into B ***");
#endif

  if (!ok)
    exit(1);
}

// Test folding dimensions into a lower dimension.
void
test8() {
  C_Polyhedron ph1(4);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(A + B <= 2);
  ph1.add_constraint(C >= 0);
  ph1.add_constraint(C + B <= 2);
  ph1.add_constraint(D >= 0);
  ph1.add_constraint(D + B <= 2);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(C);
  to_fold.insert(D);

  ph1.fold_space_dimensions(to_fold, A);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A + B <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "***  After folding {C,D} into A ***");
#endif

  if (!ok)
    exit(1);
}

// Test folding dimensions into an intermediate dimension.
void
test9() {
  C_Polyhedron ph1(4);
  ph1.add_constraint(A >= 0);
  ph1.add_constraint(B >= 0);
  ph1.add_constraint(A + B <= 2);
  ph1.add_constraint(C >= 0);
  ph1.add_constraint(C + B <= 2);
  ph1.add_constraint(D >= 0);
  ph1.add_constraint(D + B <= 2);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  // This is the set of the variables that we want to fold.
  Variables_Set to_fold;
  to_fold.insert(B);
  to_fold.insert(D);

  ph1.fold_space_dimensions(to_fold, C);

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(B <= 2);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "***  After folding {B,D} into C ***");
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
