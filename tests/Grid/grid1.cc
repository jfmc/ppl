/* Grid reduction tests.
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

#define find_variation find_variation_template<Grid>

/* add_generator_and_minimize */

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point(A - B, 4));

  Grid gr(gs);
  if (find_variation(gr))
    exit(1);

  Generator g(point(A - 2*B, 4));
  gr.add_generator_and_minimize(g);

  Generator_System known_gs;
  known_gs.insert(point(  A - B, 4));
  known_gs.insert(point(0*A - B, 4));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generators(known_gs);
  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

// FIX op== requires is_included_in
#if 0
  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid: " << endl << known_gr << endl;
  exit(1);
#endif
}

/* add_generators_and_minimize */

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert( line(0*A +   B));
  gs.insert(point(3*A + 4*B));
  gs.insert(point(9*A + 0*B));

  Grid gr(2, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert( line(0*A +   B));
  known_gs.insert(point(3*A + 4*B));
  known_gs.insert(point(9*A - 0*B));

  Grid known_gr(known_gs);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid: " << endl << gr << endl
       << "known: " << endl << known_gr << endl;
  exit(1);
}

/* add_congruence_and_minimize */

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A - 3*B %= 4) / 5);

  Grid gr(cgs);
  if (find_variation(gr))
    exit(1);

  Congruence cg((2*A - B %= 3) / 4);
  gr.add_congruence_and_minimize(cg);

  if (find_variation(gr))
    exit(1);

  // FIX cf known
}

/* add_congruences_and_minimize */

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);

  Grid gr(1);

  Congruence_System cgs;
  cgs.insert((  A %= 1) / 3);
  cgs.insert((2*A %= 0) / 3);

  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Variable B(1);

  Grid gr2(2);

  cgs.clear();
  cgs.insert((7*A + 0*B %= 3) / 3);
  cgs.insert((3*A +   B %= 0) / 3);
  cgs.insert((  A + 2*B %= 1) / 3);
  cgs.insert((2*A +   B %= 1) / 3);

  gr2.add_congruences_and_minimize(cgs);

  if (find_variation(gr2))
    exit(1);

  // FIX cf known
}

/* add_generators_and_minimize, with more rows than columns.  */

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Generator_System gs;
  gs.insert(point(3*A + 7*B - 2*C + 3*D));
  gs.insert(point(0*A + 0*B +   C +   D));
  gs.insert(point(3*A + 4*B + 2*C + 0*D));
  gs.insert(point(3*A + 2*B +   C + 2*D));
  gs.insert(point(9*A + 0*B + 4*C +   D));

  Grid gr(4, Grid::EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  // FIX cf known
}

/* add_generators_and_minimize, with more dimensions.  */

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid gr(4, Grid::EMPTY);

  Generator_System gs;
  gs.insert(point( 3*A + 2*B +    C, 1));
  gs.insert(point( 0*A + 0*B +  6*C, 7));
  gs.insert(point( 7*A + 8*B +  4*C, 2));
  gs.insert(point(36*A + 0*B + 16*C, 5));

  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  // FIX check these
  known_gs.insert(point(7*A + 2660*B +  70*C, 70));
  known_gs.insert(point(0*A +  140*B +  70*C, 70));
  known_gs.insert(point(0*A +    0*B -  10*C, 70));
  known_gs.insert(point(0*A +    0*B +  154*C, 70));

  Grid known_gr(known_gs);
  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;
#if 0 // FIX
  nout << "Grid should equal known grid." << endl
       << " grid: " << endl << gr << endl
       << "known: " << endl << known_gr << endl;

  exit(1);
#endif
}

int
main() TRY {
  set_handlers();

  nout << "grid1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();

  return 0;
}
CATCH
