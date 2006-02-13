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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

Variable A(0);
Variable C(2);

// One dimension universe and empty.

void
test1() {
  Grid gr1(1, EMPTY);

  stringstream ss1;
  gr1.ascii_dump(ss1);

  Grid gr2(1);

  stringstream ss2;
  gr2.ascii_dump(ss2);

  if (ss1.str().compare(ss1.str())) {
    nout << "Outputs from ascii_dump matched." << endl;
    exit(1);
  }
}

// Many dimensioned universe and empty.

void
test2() {
  Grid gr1(3, EMPTY);

  stringstream ss1;
  gr1.ascii_dump(ss1);

  Grid gr2(3);

  stringstream ss2;
  gr2.ascii_dump(ss2);

  if (ss1.str().compare(ss1.str())) {
    nout << "Outputs from operator<< matched." << endl;
    exit(1);
  }
}

// Universe and empty, mixed dimensions.

void
test3() {
  Grid gr1(4, EMPTY);

  stringstream ss1;
  gr1.ascii_dump(ss1);

  Grid gr2(3);

  stringstream ss2;
  gr2.ascii_dump(ss2);

  if (ss1.str().compare(ss1.str())) {
    nout << "Outputs from operator<< matched." << endl;
    exit(1);
  }
}

// Grids of same dimensions.

void
test4() {
  Grid gr1(4, EMPTY);
  gr1.add_generator(grid_point(3*A + C));
  gr1.add_generator(parameter(3*A));

  stringstream ss1;
  gr1.ascii_dump(ss1);

  Grid gr2(4);
  gr2.add_congruence(3*A == 0);

  stringstream ss2;
  gr2.ascii_dump(ss2);

  if (ss1.str().compare(ss1.str())) {
    nout << "Outputs from operator<< matched." << endl;
    exit(1);
  }
}

// Grids of mixed dimensions.

void
test5() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(3*A + C));
  gr1.add_generator(parameter(3*A));

  stringstream ss1;
  gr1.ascii_dump(ss1);

  Grid gr2(4);
  gr2.add_congruence(3*A == 0);

  stringstream ss2;
  gr2.ascii_dump(ss2);

  if (ss1.str().compare(ss1.str())) {
    nout << "Outputs from operator<< matched." << endl;
    exit(1);
  }
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "asciidumpload1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);

  return 0;
}
CATCH
