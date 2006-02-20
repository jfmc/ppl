/* Test methods which add a single congruence to a grid.
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

// Many cases are covered in addgenerator1, in which the known grid is
// always created with Grid::add_congruence.

namespace {

// Test add_congruence_and_minimize.

bool
test01() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);

  print_congruences(gr, "*** gr ***");

  gr.add_congruence((A + B %= 0) / 6);

  print_congruences(gr,
		    "*** gr.add_congruence((A + B %= 0) / 6) ***");

  gr.add_congruence_and_minimize((A + B %= 0) / 3);

  Grid known_gr(2);
  known_gr.add_congruence((A + B %= 0) / 6);

  bool ok = (gr == known_gr) ;

  print_congruences(gr,
	"*** gr.add_congruence_and_minimize((A + B %= 0) / 3) ***");

  return ok;
}

// Add to an empty grid.

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3, EMPTY);

  print_congruences(gr, "*** gr ***");

  gr.add_congruence((A + B + C %= 0) / 3);

  Grid known_gr(3, EMPTY);

  bool ok = (gr == known_gr);

  print_congruences(gr, "*** gr.add_congruence((A + B + C %= 0) / 3) ***");

  return ok;
}

// Add a congruence with less dimensions than the grid.

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((B %= 0) / 7);
  gr.add_congruence((A %= 0) / 7);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(parameter(7*A));
  known_gr.add_generator(parameter(7*B));
  known_gr.add_generator(grid_line(C));

  bool ok = (gr == known_gr);

  print_congruences(gr, "***  gr.add_congruence((A %= 0) / 7) ***");

  return ok;
}

// Add a congruence and minimize with less dimensions than the grid.

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(3);
  gr.add_congruence((B %= 0) / 7);
  gr.add_congruence_and_minimize((A %= 0) / 7);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(grid_point());
  known_gr.add_generator(parameter(7*A));
  known_gr.add_generator(parameter(7*B));
  known_gr.add_generator(grid_line(C));

  bool ok = (gr == known_gr);

  print_congruences(gr, "***  gr.add_congruence((A %= 0) / 7) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
END_MAIN
