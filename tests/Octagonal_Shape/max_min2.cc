/* Test Octagonal_Shape::maximize(const Linear_Expression&, ...)
   and Octagonal_Shape::minimize(const Linear_Expression&, ...).
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
  Variable A(0);
  Variable B(1);

  TOctagonal_Shape oct(2);
  oct.add_constraint(A >= -1);
  oct.add_constraint(A <= 1);
  oct.add_constraint(B <= 1);
  oct.add_constraint(B >= 0);

  print_constraints(oct, "*** oct ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = oct.maximize(5*A - 1, num, den, included)
    && num == 4 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = oct.minimize(5*A - 1, num, den, included)
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

  TOctagonal_Shape oct(2);
  oct.add_constraint(A >= -1);
  oct.add_constraint(A <= 1);
  oct.add_constraint(B <= 1);
  oct.add_constraint(B >= 0);

  print_constraints(oct, "*** oct ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = oct.maximize(-5*A - 1, num, den, included)
    && num == 4 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = oct.minimize(-5*A - 1, num, den, included)
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

  TOctagonal_Shape oct(2);
  oct.add_constraint(A >= -1);
  oct.add_constraint(A <= 1);
  oct.add_constraint(B <= 1);
  oct.add_constraint(B >= 0);

  print_constraints(oct, "*** oct ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = oct.maximize(-5*A + 1, num, den, included)
    && num == 6 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = oct.minimize(-5*A + 1, num, den, included)
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

  TOctagonal_Shape oct(2);
  oct.add_constraint(A >= -1);
  oct.add_constraint(A <= 1);
  oct.add_constraint(B <= 1);
  oct.add_constraint(B >= 0);

  print_constraints(oct, "*** oct ***");

  Coefficient num;
  Coefficient den;
  bool included;
  bool ok = oct.maximize(5*A + 1, num, den, included)
    && num == 6 && den == 1 && included;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << endl;

  if (!ok)
    return false;

  ok = oct.minimize(5*A + 1, num, den, included)
    && num == -4 && den == 1 && included;

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
END_MAIN
