/* Test LP_Problem::ascii_dump() and LP_Problem::ascii_load().
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
  Constraint_System cs;
  cs.insert(A - B >= 2);
  cs.insert(B >= 0);
  Linear_Expression cost(A + 2*B);
  LP_Problem lp1(cs, cost, MAXIMIZATION);
  lp1.solve();
  fstream f;
  open(f, my_file, ios_base::out);
  lp1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  LP_Problem lp2;
  bool ok = lp2.ascii_load(f);
  return ok;
}

bool
test02() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Constraint_System cs;
  cs.insert(10*A - B >= 11);
  cs.insert(B >= 0);
  Linear_Expression cost(17*A + 2*B);
  LP_Problem lp1(cs, cost, MINIMIZATION);
  fstream f;
  open(f, my_file, ios_base::out);
  lp1.ascii_dump(f);
  f.seekp(0);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  LP_Problem lp2;
  bool ok = !lp2.ascii_load(f);
  return ok;
 }

bool
test03() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(1);

  Constraint_System cs;
  cs.insert(15*A - C >= 11);
  cs.insert(B >= 0);
  cs.insert(C <= 11);

  Linear_Expression cost(17*A + 2*B -2*C);
  LP_Problem lp1(cs, cost, MINIMIZATION);
  lp1.solve();

  fstream f;
  open(f, my_file, ios_base::out);
  lp1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "pending_input_cs");
  f.seekp(0, ios_base::cur);
  f << " Q";
  close(f);

  open(f, my_file, ios_base::in);
  LP_Problem lp2;
  bool ok = !lp2.ascii_load(f);
  close(f);

  return ok;
}

bool
test04() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Constraint_System cs;
  cs.insert(A >= 10);
  cs.insert(A <= 5);
  cs.insert(C <= 11);
  Linear_Expression cost(A + 2*B + 3*C);
  LP_Problem lp1(cs, cost, MAXIMIZATION);
  lp1.solve();
  fstream f;
  open(f, my_file, ios_base::out);
  lp1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  LP_Problem lp2;
  bool ok = lp2.ascii_load(f);
  return ok;
}

bool
test05() {
  const char* my_file = "ascii_dump_load1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Constraint_System cs;
  cs.insert(A >= 123);
  cs.insert(A + 2*B + 11*C <= 5434);
  cs.insert(C <= 11);
  Linear_Expression cost(A - 11*B);
  LP_Problem lp1(cs, cost, MINIMIZATION);
  lp1.solve();
  fstream f;
  open(f, my_file, ios_base::out);
  lp1.ascii_dump(f);
  close(f);
  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "base(");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  LP_Problem lp2;
  bool ok = !lp2.ascii_load(f);
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
