/* Test Pointset_Powerset<PH>.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

bool
test01() {
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);
  Linear_Expression LE;
  bool ok1 = ps.bounds_from_above(LE);
  bool ok2 = ps.bounds_from_below(LE);

  if (!ok1 || !ok2)
    return false;

  ps.add_disjunct(C_Polyhedron(0));
  ok1 = ps.bounds_from_above(LE);
  ok2 = ps.bounds_from_below(LE);

  return ok1 && ok2;
}

bool
test02() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);
  Linear_Expression LE = x;

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x > 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool ok1 = !ps.bounds_from_above(LE);
  bool ok2 = ps.bounds_from_below(LE);

  return ok1 && ok2;
}

bool
test03() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  Linear_Expression LE = x;

  bool ok1 = ps.bounds_from_above(LE);
  bool ok2 = ps.bounds_from_below(LE);

  if (!ok1 || !ok2)
    return false;

  ps.add_disjunct(C_Polyhedron(1));

  ok1 = !ps.bounds_from_above(LE);
  ok2 = !ps.bounds_from_below(LE);

  return ok1 && ok2;
}

bool
test04() {
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);
  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Linear_Expression LE;
  bool ok = ps.maximize(LE, num, den, included)
    && num == 0 && den == 1 && !included;
  ok = ok && ps.maximize(LE, num, den, included, g)
    && num == 0 && den == 1 && !included
    && g.is_point()
    && g.divisor() == 1;

  if (!ok)
    return false;

  ps.add_disjunct(C_Polyhedron(0));
  bool ok1 = ps.maximize(LE, num, den, included)
    && num == 0 && den == 1 && included;
  ok1 = ok1 && ps.maximize(LE, num, den, included, g)
    && num == 0 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok1;
}

bool
test05() {
  Variable x(0);
  Constraint_System cs;
  Linear_Expression LE = x;

  Coefficient max_n;
  Coefficient max_d;
  bool max_included;
  Generator max_g(point());
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x >= 3);
  cs.insert(x < 14);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool ok = ps.maximize(LE, max_n, max_d, max_included)
    && max_n == 14 && max_d == 1 && !max_included;
  ok = ok && ps.maximize(LE, max_n, max_d, max_included, max_g)
    && max_n == 14 && max_d == 1 && !max_included
    && max_g.is_closure_point()
    && max_g.divisor() == 1;

  nout << max_n << "/" << max_d;
  nout << " @ ";
  print_generator(max_g);
  nout << endl;

  if (!ok)
    return false;

  cs.clear();
  cs.insert(x >= 3);
  cs.insert(x <= 14);
  ps.add_disjunct(NNC_Polyhedron(cs));

  ok = ps.maximize(LE, max_n, max_d, max_included)
    && max_n == 14 && max_d == 1 && max_included;
  ok = ok && ps.maximize(LE, max_n, max_d, max_included, max_g)
    && max_n == 14 && max_d == 1 && max_included
    && max_g.is_point()
    && max_g.divisor() == 1;

  nout << max_n << "/" << max_d;
  nout << " @ ";
  print_generator(max_g);
  nout << endl;

  return ok;
}

bool
test06() {
  Variable x(0);
  Variable y(1);
  Constraint_System cs;
  Linear_Expression LE = 9*x + y;

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Pointset_Powerset<C_Polyhedron> ps(2, EMPTY);

  cs.clear();
  cs.insert(x >= 3);
  cs.insert(3*x <= 14);
  cs.insert(y >= 5);
  cs.insert(11*y <= 87);
  ps.add_disjunct(C_Polyhedron(cs));

  bool ok = ps.maximize(LE, num, den, included)
    && num == 549 && den == 11 && included;
  ok = ok && ps.maximize(LE, num, den, included, g)
    && num == 549 && den == 11 && included
    && g.is_point()
    && g.divisor() == 33;

  nout << num << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;


  if (!ok)
    return false;

  cs.clear();
  cs.insert(x - 3*y >= 5);
  cs.insert(x <= 28);
  cs.insert(y >= 5);
  cs.insert(4*y <= 85);
  ps.add_disjunct(C_Polyhedron(cs));

  ok = ps.maximize(LE, num, den, included)
    && num == 779 && den == 3 && included;
  ok = ok && ps.maximize(LE, num, den, included, g)
    && num == 779 && den == 3 && included
    && g.is_point()
    && g.divisor() == 3;

  nout << num << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok;
}

bool
test07() {
  Variable x(0);
  Linear_Expression LE = x;

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);

  bool ok = ps.maximize(LE, num, den, included)
    && num == 0 && den == 1 && !included;
  ok = ok && ps.maximize(LE, num, den, included, g)
    && num == 0 && den == 1 && !included
    && g.is_point()
    && g.divisor() == 1;

  if (!ok)
    return false;

  ps.add_disjunct(C_Polyhedron(1));
  ok = !ps.maximize(LE, num, den, included, g);

  return ok;
}

bool
test08() {
  Linear_Expression LE;
  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);

  bool ok = ps.minimize(LE, num, den, included)
    && num == 0 && den == 1 && !included;
  ok = ok && ps.minimize(LE, num, den, included, g)
    && num == 0 && den == 1 && !included
    && g.is_point()
    && g.divisor() == 1;

  if (!ok)
    return false;

  ps.add_disjunct(C_Polyhedron(0));
  ok = ps.minimize(LE, num, den, included)
    && num == 0 && den == 1 && included;
  ok = ok && ps.minimize(LE, num, den, included, g)
    && num == 0 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << (included ? "minimum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok;
}

bool
test09() {
  Variable x(0);
  Constraint_System cs;
  Linear_Expression LE = x;

  Coefficient min_n;
  Coefficient min_d;
  bool min_included;
  Generator min_g(point());

  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(2*x > 3);
  cs.insert(x < 14);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool ok = ps.minimize(LE, min_n, min_d, min_included)
    && min_n == 3 && min_d == 2 && !min_included;
  ok = ok && ps.minimize(LE, min_n, min_d, min_included, min_g)
    && min_n == 3 && min_d == 2 && !min_included
    && min_g.is_closure_point()
    && min_g.divisor() == 2;

  nout << min_n << "/" << min_d;
  nout << " @ ";
  print_generator(min_g);
  nout << endl;

  if (!ok)
    return false;

  cs.clear();
  cs.insert(2*x >= 3);
  cs.insert(x < 14);
  ps.add_disjunct(NNC_Polyhedron(cs));

  ok = ps.minimize(LE, min_n, min_d, min_included)
    && min_n == 3 && min_d == 2 && min_included;
  ok = ok && ps.minimize(LE, min_n, min_d, min_included, min_g)
    && min_n == 3 && min_d == 2 && min_included
    && min_g.is_point()
    && min_g.divisor() == 2;

  nout << min_n << "/" << min_d;
  nout << " @ ";
  print_generator(min_g);
  nout << endl;

  return ok;
}

bool
test10() {
  Variable x(0);
  Variable y(1);
  Constraint_System cs;
  Linear_Expression LE = x + y;

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Pointset_Powerset<C_Polyhedron> ps(2, EMPTY);

  cs.clear();
  cs.insert(x >= 3);
  cs.insert(x <= 4);
  cs.insert(y >= 5);
  cs.insert(y <= 8);
  ps.add_disjunct(C_Polyhedron(cs));

  bool ok = ps.minimize(LE, num, den, included)
    && num == 8 && den == 1 && included;
  ok = ok && ps.minimize(LE, num, den, included, g)
    && num == 8 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << num << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;


  if (!ok)
    return false;

  cs.clear();
  cs.insert(x - y >= 1);
  cs.insert(x <= 8);
  cs.insert(y >= 2);
  cs.insert(y <= 10);
  ps.add_disjunct(C_Polyhedron(cs));

  ok = ps.minimize(LE, num, den, included)
    && num == 5 && den == 1 && included;
  ok = ok && ps.minimize(LE, num, den, included, g)
    && num == 5 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << num << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok;
}

bool
test11() {
  Variable x(0);
  Linear_Expression LE = x;

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);

  bool ok = ps.minimize(LE, num, den, included)
    && num == 0 && den == 1 && !included;
  ok = ok && ps.minimize(LE, num, den, included, g)
    && num == 0 && den == 1 && !included
    && g.is_point()
    && g.divisor() == 1;

  if (!ok)
    return false;

  ps.add_disjunct(C_Polyhedron(1));
  ok = !ps.minimize(LE, num, den, included, g);

  return ok;
}

bool
test12() {
  Variable x(0);
  Constraint c = (x >= 0);
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  ps.add_disjunct(C_Polyhedron(1));
  ps.add_constraint(c);
  Constraint c1 = (x >= 1);
  bool ok = ps.add_constraint_and_minimize(c1);

  return ok && ps.OK();
}

bool
test13() {
  Variable x(0);
  Congruence cg = (x %= 0);
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  ps.add_disjunct(C_Polyhedron(1));
  ps.add_congruence(cg);
  Congruence cg1 = ((x %= 0) / 2);
  bool ok = ps.add_congruence_and_minimize(cg1);

  return ok && ps.OK();
}

bool
test14() {
  Variable x(0);
  Constraint_System cs;
  cs.insert(x >= 3);
  cs.insert(x <= 4);
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  ps.add_disjunct(C_Polyhedron(1));
  ps.add_constraints(cs);
  cs.insert(x <= 3);
  bool ok = ps.add_constraints_and_minimize(cs);
  cs.insert(x <= 2);
  ok = ok && !ps.add_constraints_and_minimize(cs);

  return ok && ps.OK();
}

bool
test15() {
  Variable x(0);
  Congruence_System cgs;
  cgs.insert(x %= 0);
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  ps.add_disjunct(C_Polyhedron(1));
  ps.add_congruences(cgs);
  cgs.insert((x %= 0) / 2);
  bool ok = ps.add_congruences_and_minimize(cgs);
  cgs.insert((x %= 0) / 0);
  cgs.insert((x %= 1) / 0);
  ok = ok && !ps.add_congruences_and_minimize(cgs);

  return ok && ps.OK();
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
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);
/*   DO_TEST(test16); */
/*   DO_TEST(test17); */
/*   DO_TEST(test18); */
/*   DO_TEST(test19); */
END_MAIN
