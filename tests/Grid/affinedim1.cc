/* Test Grid::affine_dimension().
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

// Empty.

void
test1() {
  Grid gr(7, EMPTY);

  if (gr.affine_dimension() == 0)
    return;

  exit(1);
}

// Zero dimension empty.

void
test2() {
  Grid gr(0, EMPTY);

  if (gr.affine_dimension() == 0)
    return;

  exit(1);
}

// Zero dimension universe.

void
test3() {
  Grid gr(0);

  if (gr.affine_dimension() == 0)
    return;

  exit(1);
}

// Point.

void
test4() {
  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator_and_minimize(grid_point(3*A + 2*B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.affine_dimension() == 0)
    if (gr_gs_needs_min.affine_dimension() == 0)
      if (gr_cgs_needs_min.affine_dimension() == 0)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 0." << endl;

  exit(1);
}

// Line.

void
test5() {
  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_min.add_generator_and_minimize(grid_line(C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(grid_point(3*A + 2*B));
  gr_gs_needs_min.add_generator(grid_line(C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  assert(copy_compare(gr_gs_min, gr_gs_needs_min));
  assert(copy_compare(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.affine_dimension() == 1)
    if (gr_gs_needs_min.affine_dimension() == 1)
      if (gr_cgs_needs_min.affine_dimension() == 1)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 1." << endl;

  exit(1);
}

// Rectilinear.

void
test6() {
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

  if (gr_gs_min.affine_dimension() == 1)
    if (gr_gs_needs_min.affine_dimension() == 1)
      if (gr_cgs_needs_min.affine_dimension() == 1)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 1." << endl;

  exit(1);
}

// Rectilinear with lines.

void
test7() {
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

  if (gr_gs_min.affine_dimension() == 2)
    if (gr_gs_needs_min.affine_dimension() == 2)
      if (gr_cgs_needs_min.affine_dimension() == 2)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 2." << endl;

  exit(1);
}

// Skew.

void
test8() {
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

  if (gr_gs_min.affine_dimension() == 2)
    if (gr_gs_needs_min.affine_dimension() == 2)
      if (gr_cgs_needs_min.affine_dimension() == 2)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 2." << endl;

  exit(1);
}

// Skew with lines.

void
test9() {
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

  if (gr_gs_min.affine_dimension() == 3)
    if (gr_gs_needs_min.affine_dimension() == 3)
      if (gr_cgs_needs_min.affine_dimension() == 3)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 3." << endl;

  exit(1);
}

// Plane.

void
test10() {
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

  if (gr_gs_min.affine_dimension() == 2)
    if (gr_gs_needs_min.affine_dimension() == 2)
      if (gr_cgs_needs_min.affine_dimension() == 2)
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " affine dimension should be 2." << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "affinedim1:" << endl;

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

  return 0;
}
CATCH
