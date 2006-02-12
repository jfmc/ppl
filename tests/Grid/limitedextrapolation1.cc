/* Test Grid::limited_extrapolation_assign().
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
Variable D(3);
Variable E(4);

// Initially empty.

void
test1() {
  Grid gr1(2, EMPTY);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);

  Congruence_System cgs((A %= 0) / 2);

  Grid known_gr = gr2;

  gr2.limited_extrapolation_assign(gr1, cgs);

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
  Grid gr1(1);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(A == 1);

  Grid gr2(1);
  gr2.add_congruence(A %= 0);

  Congruence_System cgs((A %= 0) / 3);

  Grid known_gr = gr2;

  gr2.limited_extrapolation_assign(gr1, cgs);

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

// Equivalent of just widening.

void
test3() {
  Grid gr1(2);
  gr1.add_congruence(A == 0);
  gr1.add_congruence(B == 1);

  Grid gr2(2);
  gr2.add_congruence(A == 0);
  gr2.add_congruence(B %= 1);

  Grid known_gr = gr2;

  Congruence_System cgs(B %= 0);

  gr2.limited_extrapolation_assign(gr1, cgs);

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

// Keep all congruences, including a limiting congruence.

void
test4() {
  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);
  gr1.add_congruence(C %= 0);

  Grid gr2(3);
  gr2.add_congruence((A %= 0) / 2);
  gr2.add_congruence(C %= 0);

  Grid known_gr = gr2;

  Congruence_System cgs(C %= 0);

  gr2.limited_extrapolation_assign(gr1, cgs);

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
test5() {
  Grid gr1(3);
  gr1.add_congruence((A %= 0) / 2);
  gr1.add_congruence((B %= 0) / 2);
  gr1.add_congruence(C %= 0);

  Grid gr2(3);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((B %= 0) / 2);
  gr2.add_congruence(C %= 0);

  Congruence_System cgs(A + 0*C %= 0);

  Grid known_gr = gr2;

  gr2.limited_extrapolation_assign(gr1, cgs);

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

// Both universe.

void
test6() {
  Grid gr1(5);
  Grid gr2(5);

  Congruence_System cgs(0*E %= 0);

  gr2.limited_extrapolation_assign(gr1, cgs);

  Grid known_gr(5);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Keeping many limiting congruences.

void
test7() {
  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 4);
  gr1.add_congruence((B %= 0) / 4);

  Grid gr2(2);
  gr2.add_congruence((A %= 0) / 2);
  gr2.add_congruence(B %= 0);

  Congruence_System cgs;
  cgs.insert(A %= 0);
  cgs.insert(B %= 0);

  gr2.limited_extrapolation_assign(gr1, cgs);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(cgs);

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
test8() {
  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 4);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);

  unsigned int tokens = 6;

  Congruence_System cgs;
  cgs.insert((A + 0*B %= 0) / 2);

  Grid known_gr = gr2;

  gr2.limited_extrapolation_assign(gr1, cgs, &tokens);

  if (find_variation(gr2))
    exit(1);

#define TOKENS 5
#define TOKEN_MSG "`tokens' should be " PPL_TEST_XSTR(TOKENS) "."

  if (gr2 == known_gr)
    if (tokens == TOKENS)
      return;
    else
      nout << TOKEN_MSG << endl;
  else {
    nout << "Grid should equal known grid." << endl;
    tokens == TOKENS || nout << TOKEN_MSG << endl;
  }

  nout << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// 0 tokens.

void
test9() {
  Grid gr1(2);
  gr1.add_congruence((A %= 0) / 4);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);

#undef TOKENS
#define TOKENS 0

  unsigned int tokens = TOKENS;

  Congruence_System cgs;
  cgs.insert((A + 0*B %= 0) / 2);

  Grid known_gr(2);

  gr2.limited_extrapolation_assign(gr1, cgs, &tokens);

  if (find_variation(gr2))
    exit(1);

#define TOKEN_MSG "`tokens' should be " PPL_TEST_XSTR(TOKENS) "."

  if (gr2 == known_gr)
    if (tokens == TOKENS)
      return;
    else
      nout << TOKEN_MSG << endl;
  else {
    nout << "Grid should equal known grid." << endl;
    tokens == TOKENS || nout << TOKEN_MSG << endl;
  }

  nout << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// Zero dimension.

void
test10() {
  Grid gr1(0);

  Grid gr2(0);

  Congruence_System cgs((Linear_Expression::zero() %= Linear_Expression::zero()) / 4);

  gr2.limited_extrapolation_assign(gr1, cgs);

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

// Congruences and equalities.

void
test11() {
  Grid gr1(2);
  gr1.add_congruence(A %= 0);
  gr1.add_congruence((A - B == 0) / 27);

  Grid gr2(2);
  gr2.add_congruence(A %= 0);
  gr2.add_congruence((A - B == 0) / 9);

  Congruence_System cgs;
  cgs.insert((A - B == 0) / 3);

  gr2.limited_extrapolation_assign(gr1, cgs);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(A %= 0);
  known_gr.add_congruence((A - B == 0) / 3);

  if (gr2 == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr2 << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr2, known_gr);

  exit(1);
}

// From generators, with a limiting equality.

void
test12() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(C, 3));
  gr1.add_generator(grid_line(A - 2*B));
  gr1.add_generator(grid_point(C + 3*A + 3*C, 3));

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point(C, 3));
  gr2.add_generator(grid_line(A - 2*B));
  gr2.add_generator(grid_line(A + C));

  Grid known_gr = gr2;

  Congruence_System cgs;
  cgs.insert(A - 0*C == 3);

  gr2.limited_extrapolation_assign(gr1, cgs);

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

// Exception due to space dimensions of grids.

void
test13() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point(C, 3));
  gr1.add_generator(grid_point(C + A - 2*B, 3));

  Grid gr2(5, EMPTY);
  gr2.add_generator(grid_point(C, 3));
  gr2.add_generator(grid_point(2*C + A - 2*B, 6));

  Congruence_System cgs;
  cgs.insert(A - 0*C == 3);

  try {
    gr2.limited_extrapolation_assign(gr1, cgs);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Exception due to space dimension of congruence system.

void
test14() {
  Grid gr1(2, EMPTY);
  gr1.add_generator(grid_point(A));
  gr1.add_generator(parameter(B, 3));

  Grid gr2(2, EMPTY);
  gr2.add_generator(grid_point(A));
  gr2.add_generator(parameter(B, 6));

  Congruence_System cgs;
  cgs.insert(A - 0*C == 3);

  try {
    gr2.limited_extrapolation_assign(gr1, cgs);
    nout << "Exception expected." << endl;
    exit(1);
  }
  catch (const std::invalid_argument& e) {}
}

// Limit with an empty congruence system.

void
test15() {
  Grid gr1(3, EMPTY);
  gr1.add_generator(grid_point());
  gr1.add_generator(parameter(A, 3));

  Grid gr2(3, EMPTY);
  gr2.add_generator(grid_point());
  gr2.add_generator(parameter(A, 6));

  Congruence_System cgs;

  gr2.limited_extrapolation_assign(gr1, cgs);

  if (find_variation(gr2))
    exit(1);

  Grid known_gr = gr2;
  known_gr.add_generator(grid_point());
  known_gr.add_generator(grid_line(A));

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

  nout << "limitedextrapolation1:" << endl;

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);
  DO_TEST(test5);
  DO_TEST(test6);
  DO_TEST(test7);
  DO_TEST(test8);
  DO_TEST(test9);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
  DO_TEST(test14);
  DO_TEST(test15);

  return 0;
}
CATCH
