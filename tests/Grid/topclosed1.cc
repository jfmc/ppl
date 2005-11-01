/* Test Grid::is_topologically_closed().
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

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

bool
topclosed1_equal(Grid& a, Grid& b) {
  // Comparing a temporary copy ensures that the underlying
  // representation of the original is left the same.
  Grid tem_a = a;
  Grid tem_b = b;
  return tem_a == tem_b;
}

// Empty.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(7, EMPTY);

  if (gr.is_topologically_closed())
    return;

  exit(1);
}

// Zero dimension empty.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(0, EMPTY);

  if (gr.is_topologically_closed())
    return;

  exit(1);
}

// Zero dimension universe.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(0);

  if (gr.is_topologically_closed())
    return;

  exit(1);
}

// Point.

void
test4() {
  nout << "test4:" << endl;

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator_and_minimize(point(3*A + 2*B));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(point(3*A + 2*B));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    if (gr_gs_needs_min.is_topologically_closed())
      if (gr_cgs_needs_min.is_topologically_closed())
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should be topologically closed." << endl;

  exit(1);
}

// Line.

void
test5() {
  nout << "test5:" << endl;

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(point(3*A + 2*B));
  gr_gs_min.add_generator_and_minimize(line(C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(point(3*A + 2*B));
  gr_gs_needs_min.add_generator(line(C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B == 2);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    if (gr_gs_needs_min.is_topologically_closed())
      if (gr_cgs_needs_min.is_topologically_closed())
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should be topologically closed." << endl;

  exit(1);
}

// Rectilinear.

void
test6() {
  nout << "test6:" << endl;

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(point(3*A + 2*B));
  gr_gs_min.add_generator_and_minimize(point(3*A + B));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(point(3*A + 2*B));
  gr_gs_needs_min.add_generator(point(3*A + B));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B %= 0);
  gr_cgs_needs_min.add_congruence(C == 0);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    nout << "gr_cgs_needs_min";
  else if (gr_gs_needs_min.is_topologically_closed())
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.is_topologically_closed())
    nout << "gr_gs_min";
  else
    return;

  nout << " was topologically closed." << endl;

  exit(1);
}

// Rectilinear with lines.

void
test7() {
  nout << "test7:" << endl;

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(point(3*A + 2*B));
  gr_gs_min.add_generator(point(3*A + B));
  gr_gs_min.add_generator_and_minimize(line(C));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(point(3*A + 2*B));
  gr_gs_needs_min.add_generator(point(3*A + B));
  gr_gs_needs_min.add_generator(line(C));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence(A == 3);
  gr_cgs_needs_min.add_congruence(B %= 0);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    nout << "gr_cgs_needs_min";
  else if (gr_gs_needs_min.is_topologically_closed())
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.is_topologically_closed())
    nout << "gr_gs_min";
  else
    return;

  nout << " was topologically closed." << endl;

  exit(1);
}

// Skew.

void
test8() {
  nout << "test8:" << endl;

  Grid gr_gs_min(2, EMPTY);
  gr_gs_min.add_generator(point());
  gr_gs_min.add_generator(point(A));
  gr_gs_min.add_generator_and_minimize(point(3*A + 3*B, 4));

  Grid gr_gs_needs_min(2, EMPTY);
  gr_gs_needs_min.add_generator(point());
  gr_gs_needs_min.add_generator(point(A));
  gr_gs_needs_min.add_generator(point(3*A + 3*B, 4));

  Grid gr_cgs_needs_min(2);
  gr_cgs_needs_min.add_congruence((4*B %= 0) / 3);
  gr_cgs_needs_min.add_congruence(A - B %= 0);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    nout << "gr_cgs_needs_min";
  else if (gr_gs_needs_min.is_topologically_closed())
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.is_topologically_closed())
    nout << "gr_gs_min";
  else
    return;

  nout << " was topologically closed." << endl;

  exit(1);
}

// Skew with lines.

void
test9() {
  nout << "test9:" << endl;

  Grid gr_gs_min(3, EMPTY);
  gr_gs_min.add_generator(point());
  gr_gs_min.add_generator(point(A));
  gr_gs_min.add_generator(line(C));
  gr_gs_min.add_generator_and_minimize(point(3*A + 3*B, 4));

  Grid gr_gs_needs_min(3, EMPTY);
  gr_gs_needs_min.add_generator(point());
  gr_gs_needs_min.add_generator(point(A));
  gr_gs_needs_min.add_generator(line(C));
  gr_gs_needs_min.add_generator(point(3*A + 3*B, 4));

  Grid gr_cgs_needs_min(3);
  gr_cgs_needs_min.add_congruence((4*B %= 0) / 3);
  gr_cgs_needs_min.add_congruence(A - B %= 0);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    nout << "gr_cgs_needs_min";
  else if (gr_gs_needs_min.is_topologically_closed())
    nout << "gr_gs_needs_min";
  else if (gr_cgs_needs_min.is_topologically_closed())
    nout << "gr_gs_min";
  else
    return;

  nout << " was topologically closed." << endl;

  exit(1);
}

// Plane.

void
test10() {
  nout << "test10:" << endl;

  Grid gr_gs_min(4, EMPTY);
  gr_gs_min.add_generator(point());
  gr_gs_min.add_generator(line(B));
  gr_gs_min.add_generator_and_minimize(line(C));

  Grid gr_gs_needs_min(4, EMPTY);
  gr_gs_needs_min.add_generator(point());
  gr_gs_needs_min.add_generator(line(B));
  gr_gs_needs_min.add_generator(line(C));

  Grid gr_cgs_needs_min(4);
  gr_cgs_needs_min.add_congruence(A == 0);
  gr_cgs_needs_min.add_congruence(D == 0);

  assert(topclosed1_equal(gr_gs_min, gr_gs_needs_min));
  assert(topclosed1_equal(gr_gs_needs_min, gr_cgs_needs_min));

  if (gr_gs_min.is_topologically_closed())
    if (gr_gs_needs_min.is_topologically_closed())
      if (gr_cgs_needs_min.is_topologically_closed())
	return;
      else nout << "gr_cgs_needs_min";
    else nout << "gr_gs_needs_min";
  else nout << "gr_gs_min";

  nout << " should be topologically closed." << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "topclosed1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();

  return 0;
}
CATCH
