/* Test Grid::widening_assign.
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Initially empty.

void
test1() {
  nout << "test1:" << endl;

  Grid gr1(2, Grid::EMPTY);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);

  Grid known_gr = gr2;

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Empty after minimization.

void
test2() {
  nout << "test2:" << endl;

  Grid gr1(1);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(A == 1);

  Grid gr2(1);
  gr2.add_congruence(A %= 0);

  Grid known_gr = gr2;

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Both empty.

void
test3() {
  nout << "test3:" << endl;

  Grid gr1(1);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(A == 1);

  Grid gr2(1, Grid::EMPTY);

  Grid known_gr = gr2;

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// An equality that becomes a congruence.

void
test4() {
  nout << "test4:" << endl;

  Grid gr1(2);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(B == 1);

  Grid gr2(2);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B %= 1);

  Grid known_gr = gr2;

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Keep all congruences.

void
test5() {
  nout << "test5:" << endl;

  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);
  gr1.add_congruence(C %= 0);

  Grid gr2(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence(C %= 0);

  Grid known_gr = gr2;

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Keep some congruences.

void
test6() {
  nout << "test6:" << endl;

  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);
  gr1.add_congruence(C %= 0);

  Grid gr2(3);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((B %= 0) / 2);
  gr2.add_congruence(C %= 0);

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence((B %= 0) / 2);
  known_gr.add_congruence(C %= 0);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Already minimized.

void
test7() {
  nout << "test7:" << endl;

  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);
  gr1.add_congruence((C %= 0) / 3);

  Grid gr2(3);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((B %= 0) / 2);
  gr2.add_congruence(C %= 0);

  if (find_variation(gr2) || find_variation(gr2))
    exit(1);

  gr2.widening_assign(gr1);

  Grid known_gr(3);
  known_gr.add_congruence((B %= 0) / 2);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Both universe.

void
test8() {
  nout << "test8:" << endl;

  Grid gr1(5);
  Grid gr2(5);

  gr2.widening_assign(gr1);

  Grid known_gr(5);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// To universe.

void
test9() {
  nout << "test9:" << endl;

  Grid gr1(4);
  gr1.add_congruence((A %= 0) / 4);

  Grid gr2(4);

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(4);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Tokens.

void
test10() {
  nout << "test10:" << endl;

  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 4);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);

  unsigned int tokens = 4;

  Grid known_gr = gr2;

  gr2.widening_assign(gr1, &tokens);

  if (find_variation(gr2))
    exit(1);

#define TOKEN_MSG "`tokens' should be 3."

  if (gr2 == known_gr)
    if (tokens == 3)
      return;
    else
      nout << TOKEN_MSG << endl;
  else {
    nout << "Grid should equal known grid." << endl;
    tokens == 3 || nout << TOKEN_MSG << endl;
  }

  nout << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Zero dimension.

void
test11() {
  nout << "test11:" << endl;

  Grid gr1(0);

  Grid gr2(0);

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(0);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// The example from FIX "Widening and Extrapolation Operators" in
// RG.tex.

void
test12() {
  nout << "test12:" << endl;

  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((A + B %= 0) / 2);

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A + B %= 0) / 2);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Congruences and equalities.

void
test13() {
  nout << "test13:" << endl;

  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((A - B == 0) / 6);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((A - B == 0) / 3);

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A - B == 0) / 3);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// From generators.

void
test14() {
  nout << "test14:" << endl;

  Grid gr1(3, Grid::EMPTY);
  gr1.add_generator(point(C, 3));
  gr1.add_generator(point(C + A - 2*B, 3));

  Grid gr2(3, Grid::EMPTY);
  gr2.add_generator(point(C, 3));
  gr2.add_generator(point(2*C + A - 2*B, 6));

  gr2.widening_assign(gr1);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generator(point(C, 3));
  known_gr.add_generator( line(A - 2*B));

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "widening1:" << endl;

  test1();
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
#if 0
  test15();
  test16();
  test17();
  test18();
  test19();
  test20();
  test21();
  test22();
#endif

  return 0;
}
CATCH
