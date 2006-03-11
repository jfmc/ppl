/* Test Grid::relation_with(g).
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

// Empty grid and point.
bool
test01() {
  Variable A(0);

  Grid gr(2, EMPTY);
  print_congruences(gr, "*** gr ***");

  bool ok = (gr.relation_with(grid_point(A)) == Poly_Gen_Relation::nothing());

  return ok;
}

// Universe and point.
bool
test02() {
  Variable A(0);

  Grid gr(2);
  print_congruences(gr, "*** gr ***");

  bool ok = (gr.relation_with(grid_point(A)) == Poly_Gen_Relation::subsumes());

  return ok;
}

// Lined grid and point.
bool
test03() {
  Variable A(0);
  Variable B(1);

  Grid_Generator_System gs;
  gs.insert(grid_point());
  gs.insert(grid_point(B));
  gs.insert(grid_line(A));

  Grid gr(gs);
  print_congruences(gr, "*** gr ***");

  bool ok
    = (gr.relation_with(grid_point(A + B)) == Poly_Gen_Relation::subsumes());

  return ok;
}

// Equality and point.
bool
test04() {
  Variable A(0);

  Grid gr(2);
  gr.add_congruence(A == 0);
  print_congruences(gr, "*** gr ***");

  bool ok
     = (gr.relation_with(grid_point(2*A)) == Poly_Gen_Relation::nothing());

  return ok;
}

// Congruences and points.
bool
test05() {
  Variable A(0);
  Variable B(1);

  Grid gr(2);
  gr.add_congruence((A - B %= 1) / 2);
  gr.add_congruence((A %= 1) / 3);
  print_congruences(gr, "*** gr ***");

  bool ok = (gr.relation_with(grid_point()) == Poly_Gen_Relation::nothing()
       && gr.relation_with(grid_point(-B)) == Poly_Gen_Relation::nothing());

  return ok;
}

// Congruence and parameter.
bool
test06() {
  Variable A(0);

  Grid gr(2);
  gr.add_congruence(2*A %= 0);
  print_congruences(gr, "*** gr ***");

  bool ok
    = (gr.relation_with(parameter(A, 2)) == Poly_Gen_Relation::subsumes());

  return ok;
}

// Congruence and line.
bool
test07() {
  Variable A(0);

  Grid gr(2);
  gr.add_congruence(2*A %= 0);
  print_congruences(gr, "*** gr ***");

  bool ok = (gr.relation_with(grid_line(A)) == Poly_Gen_Relation::nothing());

  return ok;
}

// Space dimension exception.
bool
test08() {
  Variable A(0);
  Variable C(2);

  Grid gr(2);

  try {
    gr.relation_with(grid_line(A + C));
  }
  catch (const std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl;
  }
  catch (...) {
    return false;
  }
  return true;
}

// Zero dimension universe grid.
bool
test09() {
  Grid gr(0);

  bool ok = (gr.relation_with(grid_point()) == Poly_Gen_Relation::subsumes());

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
  DO_TEST(test09);
END_MAIN
