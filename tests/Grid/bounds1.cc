/* Test Grid::bounds_from_above() and Grid::bounds_from_below().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);
Variable E(4);
Variable F(5);

// Empty.

void
test1() {
  Grid gr(7, EMPTY);

  if (gr.bounds_from_above(Linear_Expression(0))
      && gr.bounds_from_below(Linear_Expression(0)))
    return;

  exit(1);
}

// Zero dimension empty.

void
test2() {
  Grid gr(0, EMPTY);

  if (gr.bounds_from_above(Linear_Expression(3))
      && gr.bounds_from_below(Linear_Expression(3)))
    return;

  exit(1);
}

// Zero dimension universe.

void
test3() {
  Grid gr(0);

  if (gr.bounds_from_above(Linear_Expression(1))
      && gr.bounds_from_below(Linear_Expression(1)))
    return;

  exit(1);
}

// Point.

void
test4() {
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
  if (gr_gs_min.bounds_from_above(le)
      && gr_gs_min.bounds_from_below(le))
    if (gr_gs_needs_min.bounds_from_above(le)
	&& gr_gs_needs_min.bounds_from_below(le))
      if (gr_cgs_needs_min.bounds_from_above(le)
	  && gr_cgs_needs_min.bounds_from_below(le))
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should bound expr from above and below." << endl;

  exit(1);
}

// Rectilinear line.

void
test5() {
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
  if (gr_gs_min.bounds_from_above(le)
      || gr_gs_min.bounds_from_below(le))
    nout << "gr_gs_min";
  else if (gr_gs_needs_min.bounds_from_above(le)
	   || gr_gs_needs_min.bounds_from_below(le))
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.bounds_from_above(le)
	   || gr_cgs_needs_min.bounds_from_below(le))
    nout << "gr_cgs_needs_min";
  else
    return;

  nout << " bounded expr." << endl;

  exit(1);
}

// Line.

void
test6() {
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
  if (gr_gs_min.bounds_from_above(le)
      || gr_gs_min.bounds_from_below(le))
    nout << "gr_gs_min";
  else if (gr_gs_needs_min.bounds_from_above(le)
	   || gr_gs_needs_min.bounds_from_below(le))
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.bounds_from_above(le)
	   || gr_cgs_needs_min.bounds_from_below(le))
    nout << "gr_cgs_needs_min";
  else
    return;

  nout << " bounded expr." << endl;

  exit(1);
}

// A line along expr in the grid.

void
test7() {
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
  if (gr_gs_min.bounds_from_above(le)
      && gr_gs_min.bounds_from_below(le))
    if (gr_gs_needs_min.bounds_from_above(le)
	&& gr_gs_needs_min.bounds_from_below(le))
      if (gr_cgs_needs_min.bounds_from_above(le)
	  && gr_cgs_needs_min.bounds_from_below(le))
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should bound expr from above and below." << endl;

  exit(1);
}

// A parameter along expr in the grid.

void
test8() {
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
  if (gr_gs_min.bounds_from_above(le)
      && gr_gs_min.bounds_from_below(le))
    if (gr_gs_needs_min.bounds_from_above(le)
	&& gr_gs_needs_min.bounds_from_below(le))
      if (gr_cgs_needs_min.bounds_from_above(le)
	  && gr_cgs_needs_min.bounds_from_below(le))
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should bound expr from above and below." << endl;

  exit(1);
}

// Two lines which combine to cover any line bounded by expr.

void
test9() {
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
  if (gr_gs_min.bounds_from_above(le)
      || gr_gs_min.bounds_from_below(le))
    nout << "gr_gs_min";
  else if (gr_gs_needs_min.bounds_from_above(le)
	   || gr_gs_needs_min.bounds_from_below(le))
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.bounds_from_above(le)
	   || gr_cgs_needs_min.bounds_from_below(le))
    nout << "gr_cgs_needs_min";
  else
    return;

  nout << " bounded expr." << endl;

  exit(1);
}

// In three dimensions, lines and parameters which combine to include
// expr.

void
test10() {
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
  if (gr_gs_min.bounds_from_above(le)
      || gr_gs_min.bounds_from_below(le))
    nout << "gr_gs_min";
  else if (gr_gs_needs_min.bounds_from_above(le)
	   || gr_gs_needs_min.bounds_from_below(le))
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.bounds_from_above(le)
	   || gr_cgs_needs_min.bounds_from_below(le))
    nout << "gr_cgs_needs_min";
  else
    return;

  nout << " bounded expr." << endl;

  exit(1);
}

// Grid which bounds a 3D expr.

void
test11() {
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
  if (gr_gs_min.bounds_from_above(le)
      && gr_gs_min.bounds_from_below(le))
    if (gr_gs_needs_min.bounds_from_above(le)
	&& gr_gs_needs_min.bounds_from_below(le))
      if (gr_cgs_needs_min.bounds_from_above(le)
	  && gr_cgs_needs_min.bounds_from_below(le))
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should bound expr from above and below." << endl;

  exit(1);
}

// Point in 6D.

void
test12() {
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
  if (gr_gs_min.bounds_from_above(le)
      && gr_gs_min.bounds_from_below(le))
    if (gr_gs_needs_min.bounds_from_above(le)
	&& gr_gs_needs_min.bounds_from_below(le))
      if (gr_cgs_needs_min.bounds_from_above(le)
	  && gr_cgs_needs_min.bounds_from_below(le))
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should bound expr from above and below." << endl;

  exit(1);
}

// Space dimension exception.

void
test13() {
  Grid gr(3, EMPTY);

  Linear_Expression le = A + 2*B + 3*C + 4*D + 6*F;

  try {
    gr.bounds_from_above(le);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "bounds1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);

  return 0;
}
CATCH
