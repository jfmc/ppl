/* Test Grid::bounds_from_above() and Grid::bounds_from_below().
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

  bool ok = (gr.bounds_from_above(Linear_Expression(0))
	     && gr.bounds_from_below(Linear_Expression(0)));

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Zero dimension empty.

bool
test02() {
  Grid gr(0, EMPTY);

  bool ok = (gr.bounds_from_above(Linear_Expression(3))
	     && gr.bounds_from_below(Linear_Expression(3)));

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Zero dimension universe.

bool
test03() {
  Grid gr(0);

  bool ok = (gr.bounds_from_above(Linear_Expression(1))
	     && gr.bounds_from_below(Linear_Expression(1)));

  print_congruences(gr, "*** gr ***");

  return ok;
}

// Point.

bool
test04() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + 2*B, 3));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B, 3));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A == 1);
  gr_cgs_needs_min.add_congruence(3*B == 2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = A + B;
  bool ok = gr_gs_min.bounds_from_above(le)
    && gr_gs_min.bounds_from_below(le)
    && gr_gs_needs_min.bounds_from_above(le)
    && gr_gs_needs_min.bounds_from_below(le)
    && gr_cgs_needs_min.bounds_from_above(le)
    && gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Rectilinear line.

bool
test05() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator_and_minimize(grid_line(B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A - B;

  bool ok = !gr_gs_min.bounds_from_above(le)
    && !gr_gs_min.bounds_from_below(le)
    && !gr_gs_needs_min.bounds_from_above(le)
    && !gr_gs_needs_min.bounds_from_below(le)
    && !gr_cgs_needs_min.bounds_from_above(le)
    && !gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Line.

bool
test06() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator_and_minimize(grid_line(2*A + B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(2*A + B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A - 2*B == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A + B;
  bool ok = !gr_gs_min.bounds_from_above(le)
    && !gr_gs_min.bounds_from_below(le)
    && !gr_gs_needs_min.bounds_from_above(le)
    && !gr_gs_needs_min.bounds_from_below(le)
    && !gr_cgs_needs_min.bounds_from_above(le)
    && !gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// A line along expr in the grid.

bool
test07() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator_and_minimize(grid_line(A + 2*B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(A + 2*B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(2*A - B == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A - B;
  bool ok = gr_gs_min.bounds_from_above(le)
    && gr_gs_min.bounds_from_below(le)
    && gr_gs_needs_min.bounds_from_above(le)
    && gr_gs_needs_min.bounds_from_below(le)
    && gr_cgs_needs_min.bounds_from_above(le)
    && gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}


// A parameter along expr in the grid.

bool
test08() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator_and_minimize(grid_point(A + 2*B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_point(A + 2*B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(2*A - B == 0);
  gr_cgs_needs_min.add_congruence((B %= 0) / 2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A - B;
  bool ok = gr_gs_min.bounds_from_above(le)
    && gr_gs_min.bounds_from_below(le)
    && gr_gs_needs_min.bounds_from_above(le)
    && gr_gs_needs_min.bounds_from_below(le)
    && gr_cgs_needs_min.bounds_from_above(le)
    && gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Two lines which combine to cover any line bounded by expr.

bool
test09() {
  Variable A(0);
  Variable B(1);

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_line(A));
  gr_gs_min.add_generator_and_minimize(grid_line(B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(A));
  gr_gs_needs_min.add_generator(grid_line(B));

  Grid gr_cgs_needs_min(2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = A - B;
  bool ok = !gr_gs_min.bounds_from_above(le)
    && !gr_gs_min.bounds_from_below(le)
    && !gr_gs_needs_min.bounds_from_above(le)
    && !gr_gs_needs_min.bounds_from_below(le)
    && !gr_cgs_needs_min.bounds_from_above(le)
    && !gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// In three dimensions, lines and parameters which combine to include
// expr.

bool
test10() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_line(A));
  gr_gs_min.add_generator_and_minimize(grid_point(B + C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(A));
  gr_gs_needs_min.add_generator(grid_point(B + C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(B - C == 0);
  gr_cgs_needs_min.add_congruence(B %= 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A + B - C;
   bool ok = !gr_gs_min.bounds_from_above(le)
     && !gr_gs_min.bounds_from_below(le)
     && !gr_gs_needs_min.bounds_from_above(le)
     && !gr_gs_needs_min.bounds_from_below(le)
     && !gr_cgs_needs_min.bounds_from_above(le)
     && !gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Grid which bounds a 3D expr.

bool
test11() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point());
  gr_gs_min.add_generator(grid_line(3*B + C));
  gr_gs_min.add_generator_and_minimize(grid_line(A - 2*B));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point());
  gr_gs_needs_min.add_generator(grid_line(3*B + C));
  gr_gs_needs_min.add_generator(grid_line(A - 2*B));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(2*A + B - 3*C == 0);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = 2*A + B - 3*C;
  bool ok = gr_gs_min.bounds_from_above(le)
    && gr_gs_min.bounds_from_below(le)
    && gr_gs_needs_min.bounds_from_above(le)
    && gr_gs_needs_min.bounds_from_below(le)
    && gr_cgs_needs_min.bounds_from_above(le)
    && gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Point in 6D.

bool
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);

  Grid gr_gs_min(6, EMPTY);
  gr_gs_min.add_generator_and_minimize(grid_point(7*A - 11*B + 19*F));

  Grid gr_gs_needs_min(6, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(7*A - 11*B + 19*F));

  Grid gr_cgs_needs_min(6);
  gr_cgs_needs_min.add_congruence(A == 7);
  gr_cgs_needs_min.add_congruence(B == -11);
  gr_cgs_needs_min.add_congruence(C == 0);
  gr_cgs_needs_min.add_congruence(D == 0);
  gr_cgs_needs_min.add_congruence(E == 0);
  gr_cgs_needs_min.add_congruence(F == 19);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  Linear_Expression le = A + 2*B + 3*C + 4*D + 6*F;
  bool ok = gr_gs_min.bounds_from_above(le)
    && gr_gs_min.bounds_from_below(le)
    && gr_gs_needs_min.bounds_from_above(le)
    && gr_gs_needs_min.bounds_from_below(le)
    && gr_cgs_needs_min.bounds_from_above(le)
    && gr_cgs_needs_min.bounds_from_below(le);

  print_congruences(gr_gs_min, "*** gr_gs_min **");
  print_congruences(gr_gs_needs_min, "*** gr_gs_needs_min **");
  print_congruences(gr_cgs_needs_min, "*** gr_cgs_needs_min **");

  return ok;
}

// Space dimension exception.

bool
test13() {
  Variable A(0);
  Variable B(1);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable C(2);

  Grid gr(3, EMPTY);

  Linear_Expression le = A + 2*B + 3*C + 4*D + 6*F;

  try {
    gr.bounds_from_above(le);
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
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
  NEW_TEST(test12);
  NEW_TEST(test13);
END_MAIN
