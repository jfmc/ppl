/* Test Grid::relation_with(cg).
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

// A proper congruence and a disjoint point.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(A - B));

  if (gr.relation_with((A - B %= 1) / 2) == Poly_Con_Relation::is_disjoint())
    return;

  exit(1);
}

// A proper congruence and an included grid.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(1, Grid::EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(4*A));

  if (gr.relation_with((A %= 0) / 2) == Poly_Con_Relation::is_included())
    return;

  exit(1);
}

// A proper congruence and an intersected grid.

void
test3() {
  nout << "test3:" << endl;

  Grid gr(3, Grid::EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(2*A));

  if (gr.relation_with((A + C %= 0) / 3) == Poly_Con_Relation::strictly_intersects())
    return;

  exit(1);
}

// A line and equalities.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point());
  gr.add_generator(line(A));

  if (gr.relation_with((A + 0*B %= 0) / 0) == Poly_Con_Relation::strictly_intersects()
      && gr.relation_with((B + 0*B %= -2) / 0) == Poly_Con_Relation::is_disjoint())
    return;

  exit(1);
}

// Inclusion of a point.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(A + B));

  if (gr.relation_with(A + 0*B %= 0) == Poly_Con_Relation::is_included())
    return;

  exit(1);
}

// Empty grid.

void
test6() {
  nout << "test6:" << endl;

  Grid gr(2, Grid::EMPTY);

  if (gr.relation_with((B %= 0) / 2)
      == (Poly_Con_Relation::is_included()
	  && Poly_Con_Relation::is_disjoint()))
    return;

  exit(1);
}

// Zero dimension universe grid.

void
test7() {
  nout << "test7:" << endl;

  Grid gr;

  if (// False congruence.
      gr.relation_with(Congruence::zero_dim_false())
      == Poly_Con_Relation::is_disjoint()
      // False equality.
      && gr.relation_with((Linear_Expression(1) %= 0) / 0)
      == Poly_Con_Relation::is_disjoint()
      // Proper congruence.
      && gr.relation_with(Linear_Expression(1) %= 1)
      == Poly_Con_Relation::is_included()
      // Equality.
      && gr.relation_with(Linear_Expression(1) %= 1)
      == Poly_Con_Relation::is_included()
      // Integrality congruence.
      && gr.relation_with(Congruence::zero_dim_integrality())
      == Poly_Con_Relation::is_included())
    return;

  exit(1);
}

// A congruence and a disjoint grid.

void
test8() {
  nout << "test8:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(2*A + 5*B));

  if (gr.relation_with((5*A - 2*B == 1) / 0)
      == Poly_Con_Relation::is_disjoint())
    return;

  exit(1);
}

// A congruence and a disjoint grid.

void
test9() {
  nout << "test9:" << endl;

  Grid gr(4);

  if (gr.relation_with(A - 2*D %= 0)
      == Poly_Con_Relation::strictly_intersects())
    return;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "relations2:" << endl;

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
