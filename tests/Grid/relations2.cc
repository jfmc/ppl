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

// A point and a congruence.

void
test1() {
  nout << "test1:" << endl;

  Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(A - B));

  if (gr.relation_with((A - B %= 1) / 2) == Poly_Con_Relation::is_disjoint())
    return;

  exit(1);
}

// A grid and a congruence.

void
test2() {
  nout << "test2:" << endl;

  Grid gr(1, Grid::EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(2*A));

  if (gr.relation_with((A %= 0) / 2) == Poly_Con_Relation::is_included())
    return;

  exit(1);
}

// A grid and a congruence.

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

} // namespace

int
main() TRY {
  set_handlers();

  nout << "relations2:" << endl;

  test1();
  test2();
  test3();
#if 0
  test4();
  test5();
  test6();
  test7();
#endif

  return 0;
}
CATCH
