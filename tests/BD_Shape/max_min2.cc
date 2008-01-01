/* Test BD_Shape::maximize(const Linear_Expression&, ...)
   and BD_Shape::minimize(const Linear_Expression&, ...).
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
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= -1);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = bd.maximize(5*A - 1, num, den, included)
    && num == 4 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(5*A - 1, num, den, included)
    && num == -6 && den == 1 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;


  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= -1);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = bd.maximize(-5*A - 1, num, den, included)
    && num == 4 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(-5*A - 1, num, den, included)
    && num == -6 && den == 1 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;


  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= -1);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = bd.maximize(-5*A + 1, num, den, included)
    && num == 6 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(-5*A + 1, num, den, included)
    && num == -4 && den == 1 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;


  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= -1);
  bd.add_constraint(A <= 1);
  bd.add_constraint(B <= 1);
  bd.add_constraint(B >= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = bd.maximize(5*A + 1, num, den, included)
    && num == 6 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(5*A + 1, num, den, included)
    && num == -4 && den == 1 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Linear_Expression LE(Linear_Expression(3));
  bool ok = bd.maximize(LE, num, den, included, g)
    && num == 3 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(LE, num, den, included, g)
    && num == 3 && den == 1 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  return ok;
}

bool
test06() {
  Variable A(0);

  TBD_Shape bd(1);
  bd.add_constraint(A >= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Linear_Expression LE(A);
  bool ok = !bd.maximize(LE, num, den, included, g);

  if (!ok)
    return false;

  ok = bd.minimize(LE, num, den, included, g)
    && num == 0 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok;
}

bool
test07() {
  Variable A(0);

  TBD_Shape bd(1);
  bd.add_constraint(A <= 0);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  Linear_Expression LE(A);

  bool ok = bd.maximize(LE, num, den, included, g)
    && num == 0 && den == 1 && included
    && g.is_point()
    && g.divisor() == 1;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  if (!ok)
    return false;

  ok = !bd.minimize(LE, num, den, included, g);

  return ok;
}

bool
test08() {
  Variable A(0);

  BD_Shape<mpq_class> bd(1);
  bd.add_constraint(5*A <= 2);
  bd.add_constraint(5*A >= 1);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Linear_Expression LE(3*A);

  bool ok = bd.maximize(LE, num, den, included)
    && num == 6 && den == 5 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(LE, num, den, included)
    && num == 3 && den == 5 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  return ok;
}

bool
test09() {
  Variable A(0);

  BD_Shape<mpq_class> bd(1);
  bd.add_constraint(5*A <= 2);
  bd.add_constraint(3*A >= 1);

  print_constraints(bd, "*** bd ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Linear_Expression LE(-7*A);

  bool ok = bd.maximize(LE, num, den, included)
    && num == -7 && den == 3 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = bd.minimize(LE, num, den, included)
    && num == -14 && den == 5 && included;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  return ok;
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
END_MAIN
