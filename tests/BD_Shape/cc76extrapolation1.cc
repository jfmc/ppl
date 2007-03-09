/* Test BD_Shape::CC76_extrapolation_assign().
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
  bd1.add_constraint(x <= 1);
  bd1.add_constraint(x - y <= 2);
  bd1.add_constraint(y - x <= 7);

  TBD_Shape bd2(3);
  bd2.add_constraint(x - y <= 2);
  bd2.add_constraint(-x <= 3);
  bd2.add_constraint(x <= 0);
  bd2.add_constraint(y - x <= 2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x <= 1);
  known_result.add_constraint(x - y <= 2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(4);
  bd1.add_constraint(A >= 0);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 25);
  bd1.add_constraint(C >= 0);
  bd1.add_constraint(C <= 29);
  bd1.add_constraint(D >= 0);
  bd1.add_constraint(D <= 27);
  bd1.add_constraint(B - A <= 25);
  bd1.add_constraint(C - A <= 29);
  bd1.add_constraint(D - A <= 27);
  bd1.add_constraint(B - C <= 2);
  bd1.add_constraint(C - B <= 6);
  bd1.add_constraint(B - D <= 2);
  bd1.add_constraint(D - B <= 4);
  bd1.add_constraint(C - D <= 4);
  bd1.add_constraint(D - C <= 4);

  TBD_Shape bd2(4);
  bd2.add_constraint(A >= 0);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 26);
  bd2.add_constraint(C >= 3);
  bd2.add_constraint(C <= 29);
  bd2.add_constraint(D >= 2);
  bd2.add_constraint(D <= 28);
  bd2.add_constraint(B - A <= 26);
  bd2.add_constraint(C - A <= 29);
  bd2.add_constraint(D - A <= 28);
  bd2.add_constraint(B - C <= 0);
  bd2.add_constraint(C - B <= 6);
  bd2.add_constraint(B - D == 2);
  bd2.add_constraint(C - D <= 4);
  bd2.add_constraint(D - C <= 2);

  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B >= 0);
  cs.insert(C >= 0);
  cs.insert(D >= 0);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
  print_constraints(cs, "*** cs ***");

  bd1.bds_hull_assign(bd2);

  bd1.limited_CC76_extrapolation_assign(bd2, cs);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(B <= 26);
  known_result.add_constraint(C >= 0);
  known_result.add_constraint(D >= 0);
  known_result.add_constraint(B - A <= 26);
  known_result.add_constraint(B - C <= 2);
  known_result.add_constraint(B - D <= 2);
  known_result.add_constraint(C - D <= 4);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "bd1.limited_CC76_extrapolation_assign(bd2, cs) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(4);
  bd1.add_constraint(A >= 0);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 39);
  bd1.add_constraint(C >= 0);
  bd1.add_constraint(C <= 40);
  bd1.add_constraint(D >= 0);
  bd1.add_constraint(D <= 40);
  bd1.add_constraint(B - A <= 39);
  bd1.add_constraint(C - A <= 40);
  bd1.add_constraint(D - A <= 40);
  bd1.add_constraint(B - C <= 0);
  bd1.add_constraint(C - B <= 1);
  bd1.add_constraint(B - D <= 0);
  bd1.add_constraint(D - B <= 2);
  bd1.add_constraint(C - D <= 0);
  bd1.add_constraint(D - C <= 1);

  TBD_Shape bd2(4);
  bd2.add_constraint(A >= 0);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 38);
  bd2.add_constraint(C >= 0);
  bd2.add_constraint(C <= 39);
  bd2.add_constraint(D >= 0);
  bd2.add_constraint(D <= 40);
  bd2.add_constraint(B - A <= 38);
  bd2.add_constraint(C - A <= 39);
  bd2.add_constraint(D - A <= 40);
  bd2.add_constraint(B - C <= 0);
  bd2.add_constraint(C - B <= 1);
  bd2.add_constraint(B - D <= 0);
  bd2.add_constraint(D - B <= 2);
  bd2.add_constraint(C - D <= 0);
  bd2.add_constraint(D - C <= 1);

  // Force the closure of `bd1'.
  bd1 == bd2;

  BD_Shape<mpq_class> known_widening(4);
  known_widening.add_constraint(A >= 0);
  known_widening.add_constraint(B >= 0);
  known_widening.add_constraint(C >= 0);
  known_widening.add_constraint(D >= 0);
  known_widening.add_constraint(D <= 40);
  known_widening.add_constraint(D - A <= 40);
  known_widening.add_constraint(B - C <= 0);
  known_widening.add_constraint(C - B <= 1);
  known_widening.add_constraint(B - D <= 0);
  known_widening.add_constraint(D - B <= 2);
  known_widening.add_constraint(C - D <= 0);
  known_widening.add_constraint(D - C <= 1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  print_constraints(bd1, "*** bd1.CC76_extrapolation_assign(bd2) ***");

  // Force the closure of `bd1'.
  bd1 == bd2;

  bool ok = (BD_Shape<mpq_class>(bd1) == known_widening);

  print_constraints(bd1, "*** bd1.closure_assign() ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 0);
  bd1.add_constraint(B >= 0);
  bd1.add_constraint(B <= 2);
  bd1.add_constraint(B - A <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= 0);
  bd2.add_constraint(B >= 0);
  bd2.add_constraint(B <= 1);
  bd2.add_constraint(B - A <= 1);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

TBD_Shape
aux_test05(int i, Variable a, Variable b, Variable c) {
  TBD_Shape bd(3);
  if (i == 0) {
    bd.add_constraint(0 <= a-b);
    bd.add_constraint(     a-b <= 0);
    bd.add_constraint(-1 <= b-c);
    bd.add_constraint(      b-c <= 1);
  }
  else {
    bd.add_constraint(-i <= a-b);
    bd.add_constraint(      a-b <= i);
    bd.add_constraint(-1 <= b-c);
    bd.add_constraint(      b-c <= 1);
    bd.add_constraint(-i <= a-c);
    bd.add_constraint(      a-c <= i);
  }

  using namespace IO_Operators;
  nout << "*** n_" << i << " ***" << endl
       << bd << endl;

  // Force closure.
  (void) (bd == bd);
  return bd;
}

bool
test05() {
  Variable a(0);
  Variable b(1);
  Variable c(2);
  unsigned i = 0;
  TBD_Shape m_i = aux_test05(i, a, b, c);
  TBD_Shape m_i_next;
  while (i < 100) {

    using namespace IO_Operators;
    nout << "*** m_" << i << " ***" << endl
	 << m_i << endl;

    m_i_next = aux_test05(++i, a, b, c);
    TBD_Shape::coefficient_type_base* no_stop_points = 0;
    m_i_next.CC76_extrapolation_assign(m_i, no_stop_points, no_stop_points);
    m_i_next.bds_hull_assign(m_i);
    // Force closure.
    (void) (m_i_next == m_i_next);
    if (m_i == m_i_next) {

      nout << "*** m_" << i << " (fixpoint) ***" << endl
	   << m_i << endl;

      return false;
    }
    m_i = m_i_next;
  }
  return true;
}

bool
test06() {
  TBD_Shape bd1(0);

  TBD_Shape bd2(0, EMPTY);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test07() {
  TBD_Shape bd1(2, EMPTY);
  TBD_Shape bd2(2, EMPTY);
  BD_Shape<mpq_class> known_result(bd2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd2.CC76_extrapolation_assign(bd1);

  bool ok = (BD_Shape<mpq_class>(bd2) == known_result);

  print_constraints(bd2,
		    "*** bd2.CC76_extrapolation_assign(bd1) ***");

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(A - B >= 2);
  bd1.add_constraint(B - C >= -1);
  bd1.add_constraint(C - A >= -3);

  TBD_Shape bd2(3);
  bd2.add_constraint(A - B >= 2);
  bd2.add_constraint(B - C >= 1);
  bd2.add_constraint(C - A >= 3);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test09() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  bd1.add_constraint(A <= 2);

  TBD_Shape bd2(2);
  bd2.add_constraint(A <= 1);
  bd2.add_constraint(B == -1);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(C <= 4);
  bd1.add_constraint(B >= 2);

  TBD_Shape bd2(3);
  bd2.add_constraint(C == 3);
  bd2.add_constraint(A - C >= 0);
  bd2.add_constraint(B - A >= 1);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(B >= 2);
  known_result.add_constraint(C - B <= 2);

  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");

  bd1.CC76_extrapolation_assign(bd2);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1,
		    "*** bd1.CC76_extrapolation_assign(bd2) ***");

  return ok;
}

bool
test11() {
  TBD_Shape bd1(1);
  TBD_Shape bd2(2);

  try {
    // This is an invalid use of the method
    // BD_Shape::CC76_extrapolation_assign(bd): it is
    // illegal to apply this method to two polyhedra that are not
    // dimension-compatible.
    bd2.CC76_extrapolation_assign(bd1);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test12() {
  TBD_Shape bd1(5);
  TBD_Shape bd2(10);

  try {
    // This is an invalid use of the method
    // BD_Shape::CC76_widening_assign(bd2): it is illegal to apply
    // this method to two polyhedra that are not dimensional
    // compatible.
    bd2.CC76_extrapolation_assign(bd1);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
END_MAIN
