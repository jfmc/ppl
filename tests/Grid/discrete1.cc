/* Test Grid::is_pointed().
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

// Empty.
bool
test01() {
  Grid gr(7, EMPTY);

  bool ok = (gr.is_discrete());

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Zero dimension empty.
bool
test02() {

  Grid gr(0, EMPTY);

  bool ok = (gr.is_discrete());

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Zero dimension universe.
bool
test03() {
  Grid gr(0);

  bool ok = (gr.is_discrete());

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Point.
bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + 2*B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (gr_gs_min.is_discrete())
    && (gr_gs_needs_min.is_discrete())
    && (gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Line.
bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_min.add_generator_and_minimize(grid_line(C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_needs_min.add_generator(grid_line(C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  Grid gr_cgs_min(3);
  gr_cgs_min.add_congruence(A == 3);
  gr_cgs_min.add_congruence(B == 2);
  // Minimize the congruences.
  gr_cgs_min.minimized_congruences();

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));
  assert(copy_compare(gr_cgs_needs_min, gr_cgs_min));

  bool ok = (!gr_gs_min.is_discrete())
    && (!gr_gs_needs_min.is_discrete())
    && (!gr_cgs_needs_min.is_discrete())
    && (!gr_cgs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");
  print_congruences(gr_cgs_min, "*** gr_cgs_min ***");

  return ok;
}

// Rectilinear.
bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + B));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_needs_min.add_generator(grid_point(3*A + B));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B %= 0);
  gr_cgs_needs_min.add_congruence(C == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (gr_gs_min.is_discrete())
    && (gr_gs_needs_min.is_discrete())
    && (gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Rectilinear with lines.
bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_min.add_generator(grid_point(3*A + B));
  gr_gs_min.add_generator_and_minimize(grid_line(C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_needs_min.add_generator(grid_point(3*A + B));
  gr_gs_needs_min.add_generator(grid_line(C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B %= 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (!gr_gs_min.is_discrete())
    && (!gr_gs_needs_min.is_discrete())
    && (!gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Skew.
bool
test08() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_point(A));
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + 3*B, 4));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_point(A));
  gr_gs_needs_min.add_generator(grid_point(3*A + 3*B, 4));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence((4*B %= 0) / 3);
  gr_cgs_needs_min.add_congruence(A - B %= 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (gr_gs_min.is_discrete())
    && (gr_gs_needs_min.is_discrete())
    && (gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Skew with lines.
bool
test09() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_point(A));
  gr_gs_min.add_generator(grid_line(C));
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + 3*B, 4));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_point(A));
  gr_gs_needs_min.add_generator(grid_line(C));
  gr_gs_needs_min.add_generator(grid_point(3*A + 3*B, 4));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence((4*B %= 0) / 3);
  gr_cgs_needs_min.add_congruence(A - B %= 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (!gr_gs_min.is_discrete())
    && (!gr_gs_needs_min.is_discrete())
    && (!gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Plane.
bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr_gs_min(4, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_line(B));
  gr_gs_min.add_generator_and_minimize(grid_line(C));

  Grid gr_gs_needs_min(4, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(B));
  gr_gs_needs_min.add_generator(grid_line(C));

  Grid gr_cgs_needs_min(4);
  gr_cgs_needs_min.add_congruence(A == 0);
  gr_cgs_needs_min.add_congruence(D == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  bool ok = (!gr_gs_min.is_discrete())
    && (!gr_gs_needs_min.is_discrete())
    && (!gr_cgs_needs_min.is_discrete());

  print_congruences(gr_gs_min, "*** gr_gs_min ***");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min ***");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min ***");

  return ok;
}

// Empty.
bool
test11() {
  Variable A(0);

  Grid gr(3);
  gr.add_congruence(A == 1);
  gr.add_congruence(A == 2);

  bool ok (gr.is_discrete());

  print_congruences(gr, "*** gr ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
  NEW_TEST(test10);
  NEW_TEST(test11);
END_MAIN
