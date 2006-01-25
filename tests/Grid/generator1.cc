/* Test class Grid_Generator.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Point.

static void
test1() {
  nout << "test1:" << endl;

  Grid_Generator a(grid_point(A + 2*B + 3*C));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(3*C + A + 2*B));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Point with divisor.

static void
test2() {
  nout << "test2:" << endl;

  Grid_Generator a(grid_point(A + 2*B + 3*C, 5));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(15*C + 5*A + 10*B, 25));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Line.

static void
test3() {
  nout << "test3:" << endl;

  Grid_Generator a(grid_line(A + 2*B + 3*C));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_line(15*C + 5*A + 10*B));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Parameter.

static void
test4() {
  nout << "test4:" << endl;

  Grid_Generator a(parameter(A + 2*B + 3*C));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(parameter(2*B + 2*A - A + 3*C));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Parameter with divisor.

static void
test5() {
  nout << "test5:" << endl;

  Grid_Generator a(parameter(A + 2*B + 3*C, 4));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(parameter(6*B + 3*A + 9*C, 12));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Negative first coefficient.

static void
test6() {
  nout << "test6:" << endl;

  Grid_Generator a(grid_point(- A + 2*B + 3*C, 4));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Construction from Generator.

static void
test7() {
  nout << "test7:" << endl;

  Grid_Generator a(grid_point(- A + 2*B + 3*C, 4));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Construction from reference to Generator.

static void
test8() {
  nout << "test8:" << endl;

  Grid_Generator g = grid_point(- A + 2*B + 3*C, 4);
  Grid_Generator& g_ref = g;

  Grid_Generator a(g_ref);
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(6*B - 3*A + 9*C, 12));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generator a should equal Grid_Generator b." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

// Create from empty linear expression.

static void
test9() {
  nout << "test9:" << endl;

  Linear_Expression le;
  Grid_Generator a(grid_point(le));
  if (find_variation(a))
    exit(1);

  Grid_Generator b(grid_point(le));
  if (find_variation(b))
    exit(1);

  if (a == b)
    return;

  nout << "Grid_Generators a and b should be equal." << endl
       << "a:" << endl << a << endl
       << "b:" << endl << b << endl;
  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "generator1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();

  return 0;
}
CATCH
