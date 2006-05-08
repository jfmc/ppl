/* Test BD_Shape::affine_preimage().
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

bool
test01() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(3);
  bd1.add_constraint(x <= 2);
  bd1.add_constraint(x - y <= 3);
  bd1.add_constraint(y <= 2);

  print_constraints(bd1, "*** bd1 ***");

  bd1.affine_preimage(x, y);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(y <= 2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.affine_preimage(x, y) ***");

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 0);
  bd.add_constraint(A - B - 3 >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, B-1);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(B >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, B-1) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 2);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(A, 2*A + 2, 2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 1);
  known_result.add_constraint(B >= 0);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(A, 2*A + 2, 2) ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 2);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  bd.affine_preimage(B, Linear_Expression(3));

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 2);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.affine_preimage(B, 3) ***");

  return ok;
}

bool
test05() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x >= y);

  try {
    // This is an incorrect use of the function
    // BD_Shape::affine_preimage(v, expr, d): it is illegal
    // to apply to a expression with the denominator
    // equal to zero.
    Coefficient d = 0;
    bd.affine_preimage(x, x + 1, d);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(2);
  bd.add_constraint(x >= y);

  try {
    // This is an incorrect use of the function
    // BD_Shape::affine_preimage(v, expr, d): it is illegal
    // to apply it to an expression whose space dimension is
    // greather than the space dimension of the BDS.
    bd.affine_preimage(y, z);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
END_MAIN
