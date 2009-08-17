/* Testing Linear_Expression.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

// Test operator-=(Linear_Form<db_r_oc>& f1, const Linear_Form<db_r_oc>& f2):
// in this case the dimension of f2 is strictly greater than
// the dimension of f1.
bool
test01() {
  Variable A(0);
  Variable B(1);

  Linear_Form<db_r_oc> f1 = A;
  Linear_Form<db_r_oc> f2 = B;
  f1 -= f2;

  Linear_Form<db_r_oc> known_result = A - Linear_Form<db_r_oc>(B);

  bool ok = (f1 == known_result);

  nout << "*** known_result ***" << endl
       << known_result << endl;

  return ok;
}

bool
test02() {
  Variable A(15);
  Variable B(0);

  Linear_Form<db_r_oc> f1 = A;
  Linear_Form<db_r_oc> f2 = B;

  Linear_Form<db_r_oc> known_result1 = f1 + f2;

  bool ok1 = (Linear_Form<db_r_oc>(A) + B == known_result1)
    && (B + Linear_Form<db_r_oc>(A) == known_result1)
    && (A + Linear_Form<db_r_oc>(B) == known_result1)
    && (Linear_Form<db_r_oc>(B) + A == known_result1)
    && (Linear_Form<db_r_oc>(B) + Linear_Form<db_r_oc>(A) == known_result1);

  nout << "*** known_result1 ***" << endl
       << known_result1 << endl;

  Linear_Form<db_r_oc> known_result2 = f1 + f1;

  bool ok2 = (Linear_Form<db_r_oc>(A) + A == known_result2)
    && (A + Linear_Form<db_r_oc>(A) == known_result2)
    && (A + Linear_Form<db_r_oc>(A) == known_result2)
    && (Linear_Form<db_r_oc>(A) + A == known_result2)
    && (Linear_Form<db_r_oc>(A) + Linear_Form<db_r_oc>(A) == known_result2);

  nout << "*** known_result2 ***" << endl
       << known_result2 << endl;

  return ok1 && ok2;
}

bool
test03() {
  Variable A(15);
  Variable B(10);

  Linear_Form<db_r_oc> f1 = A;
  Linear_Form<db_r_oc> f2 = B;

  Linear_Form<db_r_oc> known_result1 = f1 - f2;

  bool ok1 = (Linear_Form<db_r_oc>(A) - B == known_result1)
    && (A - Linear_Form<db_r_oc>(B) == known_result1)
    && (Linear_Form<db_r_oc>(A) - Linear_Form<db_r_oc>(B) == known_result1);

  nout << "*** known_result1 ***" << endl
       << known_result1 << endl;

  Linear_Form<db_r_oc> known_result2 = f2 - f1;

  bool ok2 = (Linear_Form<db_r_oc>(B) - A == known_result2)
    && (B - Linear_Form<db_r_oc>(A) == known_result2)
    && (Linear_Form<db_r_oc>(B) - Linear_Form<db_r_oc>(A) == known_result2);

  nout << "*** known_result2 ***" << endl
       << known_result2 << endl;

  Linear_Form<db_r_oc> known_result3 = f1 - f1;

  bool ok3 = (Linear_Form<db_r_oc>(A) - A == known_result3)
    && (A - Linear_Form<db_r_oc>(A) == known_result3)
    && (Linear_Form<db_r_oc>(A) - Linear_Form<db_r_oc>(A) == known_result3);

  nout << "*** known_result3 ***" << endl
       << known_result3 << endl;

  return ok1 && ok2 && ok3;
}

// Test operator+=(Linear_Form<db_r_oc>& f1, const Linear_Form<db_r_oc>& f2):
// in this case the dimension of f2 is strictly greater than
// the dimension of f1.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Linear_Form<db_r_oc> f1 = A;
  Linear_Form<db_r_oc> f2 = B;
  f1 += f2;

  Linear_Form<db_r_oc> known_result = Linear_Form<db_r_oc>(A) + B;

  bool ok = (f1 == known_result);

  nout << "*** known_result ***" << endl
       << known_result << endl;

  return ok;
}

// Test operator+=(Linear_Form<db_r_oc>& f, Variable v):
// in this case the dimension of v is strictly greater than
// the dimension of f.
bool
test05() {
  Variable A(0);
  Variable B(1);

  Linear_Form<db_r_oc> f = A;
  db_r_oc x(2.0);
  x /= 3.0;
  f *= x;

  Linear_Form<db_r_oc> known_result = f + B;

  f += B;

  bool ok = (f == known_result);

  nout << "*** known_result ***" << endl
       << known_result << endl;

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN
