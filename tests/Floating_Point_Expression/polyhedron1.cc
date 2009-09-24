/* Test Polyhedron::affine_image() on interval linear forms.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

// tests ph.affine_image(B, 3, store)
bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  FP_Interval_Abstract_Store store(3);
  FP_Interval tmp(0);
  store.set_interval(B, tmp);
  tmp.lower() = -2;
  tmp.upper() = 2;
  store.set_interval(A, tmp);
  tmp.lower() = -1;
  tmp.upper() = 1;
  store.set_interval(C, tmp);
  C_Polyhedron ph(3);
  ph.add_constraint(C == 0);
  ph.add_constraint(A == -2);
  tmp = 3;
  FP_Linear_Form l(tmp);

  ph.affine_image(B, l, store);
  print_constraints(ph, "*** ph.affine_image(B, 3, store) ***");

  C_Polyhedron known_result(3, EMPTY);
  known_result.add_generator(point(-2 * A + 3 * B));
  print_constraints(known_result, "*** know_result ***");

  bool ok = (ph == known_result);
  return ok;
}

// tests ph.affine_image(A, A + B + 1, store)
bool
test02() {
  Variable A(0);
  Variable B(1);
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(1);
  store.set_interval(A, tmp);
  store.set_interval(B, tmp);
  C_Polyhedron ph(2);
  ph.add_constraint(A >= B);
  ph.add_constraint(B >= 0);
  ph.add_constraint(A <= 3);
  FP_Linear_Form l(A);
  l += B;
  l += tmp;
  ph.affine_image(A, l, store);
  print_constraints(ph, "*** ph.affine_image(A, A + B + 1, store) ***");

  C_Polyhedron known_result(2);
  known_result.add_constraint(A - 2*B - 1 >= 0);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(A - B <= 4);
  print_constraints(known_result, "*** known_result ***");

  bool ok = (ph == known_result);

  return ok;
}

// tests ph.affine_image(A, (A + 1) / 2, store)
bool
test03() {
  Variable A(0);
  Variable B(1);
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(1 / 2.0);
  store.set_interval(A, tmp);
  store.set_interval(B, tmp);
  Generator_System gs;
  gs.insert(point());
  gs.insert(ray(A));
  gs.insert(ray(B));
  C_Polyhedron ph(gs);
  FP_Linear_Form l(A);
  l *= tmp;
  l += tmp;
  ph.affine_image(A, l, store);
  print_constraints(ph, "*** ph.affine_image(A, (A + 1) / 2, store) ***");

  C_Polyhedron known_result(2);
  known_result.add_constraint(2*A >= 1);
  known_result.add_constraint(B >= 0);
  print_constraints(known_result, "*** known_result ***");

  bool ok = (ph == known_result);

  return ok;
}

// tests ph.affine_image(A, (B + 2) / (-3), store)
bool
test04() {
  Variable A(0);
  Variable B(1);
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(-1 / 3.0);
  store.set_interval(A, tmp);
  store.set_interval(B, tmp);
  C_Polyhedron ph(2, EMPTY);
  ph.add_generator(point(A));
  FP_Linear_Form l(B);
  l *= tmp;
  tmp += tmp;
  l += tmp;
  ph.affine_image(A, l, store);
  print_constraints(ph, "*** ph.affine_image(A, (B + 2) / (-3), store) ***");

  C_Polyhedron known_result(2, EMPTY);
  known_result.add_generator(point(-2*A, 3));
  print_constraints(known_result, "*** known_result ***");

  bool ok = (ph.contains(known_result));

  return ok;
}

// tests ph.affine_image(B, (A - B + 2) / (-3), store)
bool
test05() {
  Variable A(0);
  Variable B(1);
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(2);
  store.set_interval(A, tmp);
  store.set_interval(B, tmp);
  C_Polyhedron ph(2);
  ph.add_constraint(A >= 2);
  ph.add_constraint(A <= 3);
  ph.add_constraint(B >= 1);
  ph.add_constraint(2*A >= B);
  FP_Linear_Form l(A);
  l += tmp;
  l -= B;
  l /= FP_Interval(-3);

  ph.affine_image(B, l, store);
  print_constraints(ph,
    "*** ph.affine_image(B, (A - B + 2) / (-3), store) ***");

  C_Polyhedron known_result(2, EMPTY);
  known_result.add_generator(point(2*A));
  known_result.add_generator(point(2*A - B));
  known_result.add_generator(point(9*A + B, 3));
  known_result.add_generator(point(9*A - 4*B, 3));
  print_constraints(known_result, "*** known_result ***");

  bool ok = (ph.contains(known_result));

  return ok;
}
/*
bool
test06() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2, EMPTY);

  print_constraints(ph1, "*** ph1 ***");

  ph1.affine_image(A, 2*A + B + 1);

  C_Polyhedron known_result(2, EMPTY);

  bool ok = (ph1 == known_result);

  print_constraints(ph1, "*** after ph1.affine_image(A, 2*A + B + 1) ***");

  return ok;
}

bool
test07() {
  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(B));
  gs.insert(point(A + B));
  C_Polyhedron ph(gs);

  print_generators(ph, "*** ph ***");

  ph.affine_image(A, -A - 1, -1);

  Generator_System known_gs;
  known_gs.insert(point(A));
  known_gs.insert(point(2*A));
  known_gs.insert(point(A + B));
  known_gs.insert(point(2*A + B));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph == known_result);

  print_generators(ph, "*** after ph.affine_image(A, -A - 1, -1) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 0);
  ph.add_constraint(B >= 0);
  C_Polyhedron copy_ph(ph);

  print_constraints(ph, "*** ph ***");

  ph.affine_image(A, A + 1);
  copy_ph.affine_image(A, -A - 1, -1);

  bool ok = (ph == copy_ph);

  print_generators(ph, "*** after ph.affine_image(A, A + 1) ***");
  print_generators(copy_ph,
		   "*** after copy_ph.affine_image(A, -A - 1, -1) ***");

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 0);
  ph.add_constraint(B >= 0);
  C_Polyhedron copy_ph(ph);

  print_constraints(ph, "*** ph ***");

  ph.affine_image(B, A + 1);
  copy_ph.affine_image(B, -A - 1, -1);

  bool ok = (ph == copy_ph);

  print_generators(ph, "*** after ph.affine_image(B, A + 1) ***");
  print_generators(copy_ph,
		   "*** after copy_ph.affine_image(B, -A - 1, -1) ***");

  return ok;
}
*/
} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05); /*
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09); */
END_MAIN
