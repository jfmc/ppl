/* Test Polyhedra_Powerset<Grid>::BHZ03_widening_assign().
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

typedef Polyhedra_Powerset<Grid> GSet;

#define find_variation find_variation_template<GSet>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

void
test1() {
  nout << "test1:" << endl;

  GSet grs1(2, EMPTY);
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence(B %= 0);
  grs1.add_disjunct(gr1);

  GSet grs2(2, EMPTY);
  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  grs2.add_disjunct(gr2);

  GSet known_grs = grs2;

  grs2.BHZ03_widening_assign<Grid_Certificate>
    (grs1, widen_fun_ref(&Grid::widening_assign));

  if (find_variation(grs2))
    exit(1);

  if (grs2 == known_grs)
    return;

  nout << "Grid set should equal known grid set." << endl
       << " grid:" << endl << grs2 << endl
       << "known:" << endl << known_grs << endl;

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "bhz03widening1:" << endl;

  test1();
#if 0
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();
  test12();
  test13();
  test14();
#endif

  return 0;
}
CATCH
