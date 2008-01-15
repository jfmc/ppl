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
  bool b = !ps.is_universe();

  ps.add_disjunct(C_Polyhedron(0));
  bool b1 = ps.is_universe();
  return b && b1;
}

bool
test02() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x < 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool b = !ps.is_universe();
  return b;
}

bool
test03() {
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  bool b = !ps.is_universe();

  ps.add_disjunct(C_Polyhedron(1));

  bool b1 = ps.is_universe();
  return b && b1;
}

bool
test04() {
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);
  bool b = ps.is_empty();

  ps.add_disjunct(C_Polyhedron(0));
  bool b1 = !ps.is_empty();
  return b && b1;
}

bool
test05() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x < 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool b = !ps.is_empty();
  return b;
}

bool
test06() {
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  bool b = ps.is_empty();

  ps.add_disjunct(C_Polyhedron(1));

  bool b1 = !ps.is_empty();
  return b && b1;
}

bool
test07() {
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);
  bool b = ps.is_topologically_closed();

  ps.add_disjunct(C_Polyhedron(0));
  bool b1 = ps.is_topologically_closed();
  return b && b1;
}

bool
test08() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 2);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool b = ps.is_topologically_closed();
  return b;
}

bool
test09() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x < 2);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool b = !ps.is_topologically_closed();
  return b;
}

bool
test10() {
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  bool b = ps.is_topologically_closed();

  ps.add_disjunct(C_Polyhedron(1));

  bool b1 = ps.is_topologically_closed();
  return b && b1;
}

bool
test11() {
  Pointset_Powerset<C_Polyhedron> ps(0, EMPTY);
  bool b = ps.is_bounded();

  ps.add_disjunct(C_Polyhedron(0));
  // A zero-dimension universe is bounded.
  bool b1 = ps.is_bounded();
  return b && b1;
}

bool
test12() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 2);
  ps.add_disjunct(NNC_Polyhedron(cs));

  bool b = !ps.is_bounded();
  return b;
}

bool
test13() {
  Pointset_Powerset<C_Polyhedron> ps(1, EMPTY);
  bool b = ps.is_bounded();

  ps.add_disjunct(C_Polyhedron(1));

  bool b1 = !ps.is_bounded();
  return b && b1;
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
END_MAIN
