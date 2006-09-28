/* Test Pointset_Powerset<PH>.
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
  Constraint_System cs = Constraint_System::zero_dim_empty();
  Pointset_Powerset<C_Polyhedron> ps(cs);
  return ps.OK();
}

bool
test02() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<NNC_Polyhedron> nnc_ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  nnc_ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x < 1);
  nnc_ps.add_disjunct(NNC_Polyhedron(cs));

  Pointset_Powerset<C_Polyhedron> c_ps(nnc_ps);

  return c_ps.OK();
}

bool
test03() {
  Variable x(0);
  Constraint_System cs;
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  c_ps.add_constraint(x == 1);

  Pointset_Powerset<NNC_Polyhedron> nnc_ps(c_ps);

  return nnc_ps.OK();
}

bool
test04() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  c_ps.concatenate_assign(c_ps);

  return c_ps.OK();
}

bool
test05() {
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);

  bool ok = c_ps.is_bottom();

  c_ps.add_disjunct(C_Polyhedron(1, UNIVERSE));

  bool ok1 = c_ps.is_top();

  c_ps.total_memory_in_bytes();
  c_ps.external_memory_in_bytes();

  return ok && ok1;
}

bool
test06() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;
  cs.insert(x >= 0);
  c_ps.add_disjunct(C_Polyhedron(cs));

  Pointset_Powerset<C_Polyhedron> c_ps1(1, EMPTY);
  Constraint_System cs1;
  cs1.insert(x >= 0);
  cs1.insert(x <= 2);
  c_ps1.add_disjunct(C_Polyhedron(cs1));

  bool ok = c_ps1.definitely_entails(c_ps);

  return ok;
}

bool
test07() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  bool ok = (c_ps.size() == 2);

  return ok;
}

bool
test08() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));
  c_ps.omega_reduce();

  bool ok = (c_ps.size() == 1);

  return ok;
}

bool
test09() {
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  bool ok = (c_ps.space_dimension() == 1);
  return ok;
}

bool
test10() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x <= 2);
  Constraint_System cs1 = cs;
  c_ps.add_disjunct(C_Polyhedron(cs));
  c_ps.drop_disjunct(c_ps.begin());

  bool ok = c_ps.empty();

  Constraint_System cs2 = cs1;
  c_ps.add_disjunct(C_Polyhedron(cs1));

  cs.insert(x >= 0);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));
  c_ps.drop_disjuncts(c_ps.begin(), c_ps.end());

  bool ok1 = c_ps.empty();

  return ok && ok1;
}

bool
test11() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  Pointset_Powerset<C_Polyhedron> c_ps1;
  c_ps1 = c_ps;

  bool ok = !c_ps.empty();
  return ok;
}

bool
test12() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  Pointset_Powerset<C_Polyhedron> c_ps1(1, EMPTY);
  c_ps.swap(c_ps1);

  bool ok = (c_ps.empty() && !c_ps1.empty());
  return ok;
}

bool
test13() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);

  Pointset_Powerset<C_Polyhedron> c_ps1(1, EMPTY);
  c_ps1.add_disjunct(C_Polyhedron(cs));
  c_ps.least_upper_bound_assign(c_ps1);

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 3);

  Pointset_Powerset<C_Polyhedron> c_ps2(1, EMPTY);
  c_ps2.add_disjunct(C_Polyhedron(cs));

  bool ok = c_ps.definitely_entails(c_ps2);
  bool ok1 = !c_ps2.definitely_entails(c_ps);

  return ok && ok1;
}

bool
test14() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);

  Pointset_Powerset<C_Polyhedron> c_ps1(1, EMPTY);
  c_ps1.add_disjunct(C_Polyhedron(cs));
  c_ps.upper_bound_assign(c_ps1);

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 3);

  Pointset_Powerset<C_Polyhedron> c_ps2(1, EMPTY);
  c_ps2.add_disjunct(C_Polyhedron(cs));

  bool ok = c_ps.definitely_entails(c_ps2);
  bool ok1 = !c_ps2.definitely_entails(c_ps);

  return ok && ok1;
}

bool
test15() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  Pointset_Powerset<C_Polyhedron> c_ps1(1, EMPTY);

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);

  c_ps.meet_assign(c_ps1);

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 2);
  Pointset_Powerset<C_Polyhedron> c_ps_expected(1, EMPTY);
  c_ps_expected.add_disjunct(C_Polyhedron(cs));

  bool ok = c_ps.definitely_entails(c_ps_expected);
  bool ok1 = !c_ps_expected.definitely_entails(c_ps);

  return ok && ok1;
}

bool
test16() {
  Variable x(0);
  Pointset_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  c_ps.collapse();

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  Pointset_Powerset<C_Polyhedron> c_ps_expected(1, EMPTY);
  c_ps_expected.add_disjunct(C_Polyhedron(cs));

  bool ok = c_ps.definitely_entails(c_ps_expected);
  bool ok1 = c_ps_expected.definitely_entails(c_ps);
  bool ok2 = (c_ps.size() == 1);

  return ok && ok1 && ok2;
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
  DO_TEST(test16);
END_MAIN
