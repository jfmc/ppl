/* Test Grid::ascii_dump() and Grid::ascii_load().
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

// One dimension universe and empty.

bool
test01() {
  Grid gr1(1, EMPTY);

  print_congruences(gr1, "*** gr1 ***");

  stringstream ss1;
  gr1.ascii_dump(ss1);

  gr1.ascii_dump(vnout);

  Grid gr2(1);

  print_congruences(gr2, "*** gr2 ***");

  stringstream ss2;
  gr2.ascii_dump(ss2);

  gr2.ascii_dump(vnout);

  bool ok = (ss2.str().compare(ss1.str()));

  return ok;
}

// Many dimensioned universe and empty.

bool
test02() {
  Grid gr1(3, EMPTY);

  print_congruences(gr1, "*** gr1 ***");

  stringstream ss1;
  gr1.ascii_dump(ss1);

  gr1.ascii_dump(vnout);

  Grid gr2(3);

  print_congruences(gr2, "*** gr2 ***");

  stringstream ss2;
  gr2.ascii_dump(ss2);

  gr2.ascii_dump(vnout);

  bool ok = (ss2.str().compare(ss1.str()));

  return ok;
}

// Universe and empty, mixed dimensions.

bool
test03() {
  Grid gr1(4, EMPTY);

  print_congruences(gr1, "*** gr1 ***");

  stringstream ss1;
  gr1.ascii_dump(ss1);

  gr1.ascii_dump(vnout);

  Grid gr2(3);

  print_congruences(gr2, "*** gr2 ***");

  stringstream ss2;
  gr2.ascii_dump(ss2);

  gr2.ascii_dump(vnout);

  bool ok = (ss2.str().compare(ss1.str()));

  return ok;
}

// Grids of same dimensions.

bool
test04() {
  Variable A(0);
  Variable C(2);

  Grid gr1(4, EMPTY);
  gr1.add_generator(grid_point(3*A + C));
  gr1.add_generator(parameter(3*A));

  print_generators(gr1, "*** gr1 ***");

  stringstream ss1;
  gr1.ascii_dump(ss1);

  gr1.ascii_dump(vnout);

  Grid gr2(4);
  gr2.add_congruence(3*A == 0);

  print_congruences(gr2, "*** gr2 ***");

  stringstream ss2;
  gr2.ascii_dump(ss2);

  gr2.ascii_dump(vnout);

  bool ok = (ss2.str().compare(ss1.str()));

  return ok;
}

// Grids of mixed dimensions.

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*A + C));
  gr1.add_generator(parameter(3*A));

  print_generators(gr1, "*** gr1 ***");

  stringstream ss1;
  gr1.ascii_dump(ss1);

  gr1.ascii_dump(vnout);

  Grid gr2(4);
  gr2.add_congruence(3*A == 0);

  print_congruences(gr2, "*** gr2 ***");

  stringstream ss2;
  gr2.ascii_dump(ss2);

  gr2.ascii_dump(vnout);

  bool ok = (ss2.str().compare(ss1.str()));

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
END_MAIN
