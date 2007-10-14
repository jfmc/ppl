/* Test ascii_dump() and ascii_load() methods for Term, Monomial,
   Polynomial, and Polynomial_Space objects.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "files.hh"
#include <string>
#include <fstream>

using std::string;
using std::fstream;
using std::ios_base;

namespace {

bool
test01() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Term t1(A*B*B*C*B);

  fstream f;
  open(f, my_file, ios_base::out);
  t1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Term t2;
  t2.ascii_load(f);
  close(f);

  bool ok = (t1 == t2);

  return ok;
}

bool
test02() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Term t1(A*B*B*C*B);
  t1 /= C;

  fstream f;
  open(f, my_file, ios_base::out);
  t1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Term t2;
  t2.ascii_load(f);
  close(f);

  bool ok = (t1 == t2);

  return ok;
}

bool
test03() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Term t1(A*B*B*C*B);
  Monomial m1(t1);
  m1 *= 14;
  m1 *= A;

  fstream f;
  open(f, my_file, ios_base::out);
  m1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Monomial m2;
  m2.ascii_load(f);
  close(f);

  bool ok = (m1 == m2);

  return ok;
}

bool
test04() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Term t1;
  Monomial m1(t1);

  fstream f;
  open(f, my_file, ios_base::out);
  m1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Monomial m2;
  m2.ascii_load(f);
  close(f);

  bool ok = (m1 == m2);

  return ok;
}

bool
test05() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Polynomial p1;
  p1 += 7;
  p1 *= A*B*B*A;
  p1 -= 3*A;
  p1 += 2;

  fstream f;
  open(f, my_file, ios_base::out);
  p1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Polynomial p2;
  p2.ascii_load(f);
  close(f);

  bool ok = (p1.is_equal_to(p2));

  return ok;
}

bool
test06() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);

  Polynomial_Space<2> ps1(2);
  ps1.add_polynomial_constraint(3*A*A + 2*B*B - 18 == 0);
  ps1.add_polynomial_constraint(6*A*A + 4*B*B - 36 == 0);
  ps1.add_polynomial_constraint(6*A + 2*B*B - 6 == 0);

  fstream f;
  open(f, my_file, ios_base::out);
  ps1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Polynomial_Space<2> ps2;
  ps2.ascii_load(f);
  close(f);

  bool ok = (ps1 == ps2);

  return ok;
}

bool
test07() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Polynomial_Space<3> ps1(3);
  ps1.add_polynomial_constraint(3*A*A*A + 2*B*B - 18 == 0);
  ps1.add_polynomial_constraint(6*A*A*A + 4*B*B - 36 == 0);
  ps1.add_polynomial_constraint(6*A + 2*B*B - 6 == 0);

  fstream f;
  open(f, my_file, ios_base::out);
  ps1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  Polynomial_Space<3> ps2;
  ps2.ascii_load(f);
  close(f);

  bool ok = (ps1 == ps2);

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
END_MAIN
