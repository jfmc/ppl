/* Test Grid::relation_with(g).
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Empty grid and point.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(2, EMPTY);

  if (gr.relation_with(point(A)) == Poly_Gen_Relation::nothing())
    return;

  exit(1);
}

// Universe and point.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(2);

  if (gr.relation_with(point(A)) == Poly_Gen_Relation::subsumes())
    return;

  exit(1);
}

// Lined grid and point.

void
test3() {
  nout << "test3:" << endl;

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(B));
  gs.insert(line(A));

  Grid gr(gs);

  if (gr.relation_with(point(A + B)) == Poly_Gen_Relation::subsumes())
    return;

  exit(1);
}

// Equality and point.

void
test4() {
  nout << "test4:" << endl;

  Grid gr(2);
  gr.add_congruence((A %= 0) / 0);

  if (gr.relation_with(point(2*A)) == Poly_Gen_Relation::nothing())
    return;

  exit(1);
}

// Congruences and points.

void
test5() {
  nout << "test5:" << endl;

  Grid gr(2);
  gr.add_congruence((A - B %= 1) / 2);
  gr.add_congruence((A %= 1) / 3);

  if (gr.relation_with(point()) == Poly_Gen_Relation::nothing()
      && gr.relation_with(point(-B)) == Poly_Gen_Relation::nothing())
    return;

  exit(1);
}

// Congruence and ray.

void
test6() {
  nout << "test6:" << endl;

  Grid gr(2);
  gr.add_congruence(2*A %= 0);

  if (gr.relation_with(ray(A), 2) == Poly_Gen_Relation::subsumes())
    return;

  exit(1);
}

// Congruence and line.

void
test7() {
  nout << "test7:" << endl;

  Grid gr(2);
  gr.add_congruence(2*A %= 0);

  if (gr.relation_with(line(A), 2) == Poly_Gen_Relation::nothing())
    return;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "relations1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();

  return 0;
}
CATCH
