/* Test Pointset_Powerset::intersection_preserving_enlarge_assign().
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
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps1.add_disjunct(ph);
  ps2.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k >= 1);
  ph.add_constraint(k <= 10);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);
  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(k == 0);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test02() {
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k >= 1);
  ph.add_constraint(k <= 10);

  ps1.add_disjunct(ph);
  ps2.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);
  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(k >= 1);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  return ok;
}

bool
test03() {
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i <= 25);
  ph.add_constraint(j <= 25);
  ph.add_constraint(i + j >= 25);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

  return ok;
}

bool
test04() {
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i <= 25);
  ph.add_constraint(j <= 25);
  ph.add_constraint(i + j >= 25);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 0);
  ph.add_constraint(i <= 2);
  ph.add_constraint(j >= 2);
  ph.add_constraint(j <= 9);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);
  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j <= 10);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

  return ok;
}

bool
test05() {
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 1);
  ph.add_constraint(i <= 10);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 28);
  ph.add_constraint(i <= 31);
  ph.add_constraint(j >= 1);
  ph.add_constraint(j <= 10);
  ph.add_constraint(k == 0);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i <= 25);
  ph.add_constraint(j <= 25);
  ph.add_constraint(i + j >= 25);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 30);
  ph.add_constraint(i <= 32);
  ph.add_constraint(j >= 2);
  ph.add_constraint(j <= 9);
  ph.add_constraint(k == 0);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);
  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i >= 28);
  ph.add_constraint(i <= 31);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

  return ok;
}

bool
test06() {
  Variable i(0);
  Variable j(1);
  Variable k(2);

  C_Polyhedron ph(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(3, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(3, EMPTY);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i == 1);
  ph.add_constraint(j + 1 == 0);
  ph.add_constraint(k == 3);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(i == 1);
  ph.add_constraint(j + k == 2);
  ph.add_constraint(k >= 0);
  ph.add_constraint(k <= 3);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(3, EMPTY);
  ph = C_Polyhedron(3, UNIVERSE);
  ph.add_constraint(k == 3);
  // ph.add_constraint(j + 1 == 0);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

  return ok;
}

bool
test07() {
  Variable i(0);
  Variable j(1);

  C_Polyhedron ph(2, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(2, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(2, EMPTY);

  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(i <= j);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(i == j);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(2, UNIVERSE);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

  return ok;
}

bool
test08() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps1(2, EMPTY);
  Pointset_Powerset<C_Polyhedron> ps2(2, EMPTY);

  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(A >= 0);

  ps1.add_disjunct(ph);

  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(A <= 0);

  ps2.add_disjunct(ph);

  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(B >= 0);

  ps2.add_disjunct(ph);

  Pointset_Powerset<C_Polyhedron> known_result(2, EMPTY);
  ph = C_Polyhedron(2, UNIVERSE);
  ph.add_constraint(A >= 0);
  known_result.add_disjunct(ph);

  ps1.intersection_preserving_enlarge_assign(ps2);

  bool ok = (ps1 == known_result);

  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = ps1.begin(),
         iend = ps1.end(); i != iend; ++i)
    print_constraints(i->element());

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
END_MAIN
