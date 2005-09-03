/* Test reduction and conversion of grids created from generators.
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

// add_generator_and_minimize, one variable.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Generator_System gs;
  gs.insert(point(A));

  Grid gr(gs);

  gr.add_generator_and_minimize(point(2*A));

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generator_and_minimize, two variables.

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point(A + B));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Generator g(point(A + 2*B));
  gr.add_generator_and_minimize(g);

  Congruence_System known_cgs;
  known_cgs.insert( 0*A + 0*B %= -1);
  known_cgs.insert((  A + 0*B %=  1) / 0);
  known_cgs.insert( 0*A + 1*B %=  1);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generators_and_minimize

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert( line(0*A +   B));
  gs.insert(point(3*A + 4*B));
  gs.insert(point(9*A + 0*B));

  Grid gr(2, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert( line(0*A +   B));
  known_gs.insert(point(3*A + 4*B));
  known_gs.insert(point(9*A + 0*B));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test0 from Chiara conversion_test.cc

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(4*A -   B + 0*C, 3));
  gs.insert( line(2*A + 3*B + 0*C));
  gs.insert(point(4*A + 0*B + 0*C, 3));
  gs.insert(point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -2) / 2);
  known_cgs.insert((-9*A + 6*B + 0*C %= 14) / 2);
  known_cgs.insert(( 0*A - 0*B + 6*C %=  0) / 2);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test1 from Chiara conversion_test.cc.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B + 3*C, 2));
  gs.insert( line( 3*A + 2*B - 4*C));
  gs.insert( line( 0*A + 0*B - 2*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(0*A + 0*B + 0*C %= -1);
  known_cgs.insert((-2*A + 3*B + 0*C %= 7) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test2 from Chiara conversion_test.cc.

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B +  3*C, 2));
  gs.insert(point( 2*A + 6*B -    C, 2));
  gs.insert(point(-1*A + 9*B +  7*C, 2));
  gs.insert( line( 0*A + 0*B -  2*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((  0*A + 0*B + 0*C %=  15) / 15);
  known_cgs.insert((-10*A + 0*B + 0*C %=   5) / 15);
  known_cgs.insert((  4*A - 6*B + 0*C %= -14) / 15);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test3 from Chiara conversion_test.cc.

void
test7() {
  nout << "test7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B + 3*C, 2));
  gs.insert( line( 2*A +   B - 2*C));
  gs.insert(point(-1*A + 9*B + 7*C, 2));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -10) / 10);
  known_cgs.insert((-2*A + 4*B + 0*C %=   9) / 10);
  known_cgs.insert(( 7*A - 4*B + 5*C %=  -4) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// test4 from Chiara conversion_test.cc.

void
test8() {
  nout << "test8:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point( 3*A +   B + 0*C, 4));
  gs.insert(point(11*A + 2*B + 0*C, 4));
  gs.insert(point( 3*A + 6*B + 0*C, 4));
  gs.insert(point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A +  0*B +  0*C %= -40) / 40);
  known_cgs.insert((20*A +  0*B +  0*C %=  15) / 40);
  known_cgs.insert((-4*A + 32*B +  0*C %=   5) / 40);
  known_cgs.insert(( 0*A +  0*B + 80*C %=   0) / 40);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test0/1 from Chiara Convert_Test.cc.

void
test9() {
  nout << "test9:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(A));
  gs.insert(point(2*A));
  gs.insert(point(A + B));
  gs.insert(point(A + C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test2 from Chiara Convert_Test.cc.

void
test10() {
  nout << "test10:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(A +   B));
  gs.insert(point(A + 2*B));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert((C %= 0) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test3 from Chiara Convert_Test.cc.

void
test11() {
  nout << "test11:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(A +   B + 0*C));
  gs.insert(point(A + 2*B + 0*C));
  gs.insert(point(A +   B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %=  1) / 0);
  known_cgs.insert(B %= 0);
  known_cgs.insert(C %= 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test4 from Chiara Convert_Test.cc.

void
test12() {
  nout << "test12:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point( 3*A +   B + 0*C, 4));
  gs.insert(point(11*A + 2*B + 0*C, 4));
  gs.insert(point( 3*A + 6*B + 0*C, 4));
  gs.insert(point( 3*A +   B + 2*C, 4));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((20*A +  0*B        %= 15) / 40);
  known_cgs.insert((-4*A + 32*B        %=  5) / 40);
  known_cgs.insert((              80*C %=  0) / 40);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test5 from Chiara Convert_Test.cc.

void
test13() {
  nout << "test13:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(0*A + 7*B + 0*C, 3));
  gs.insert( line(3*A + 2*B + 0*C));
  gs.insert( line(0*A + 0*B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -1) / 1);
  known_cgs.insert((-2*A + 3*B + 0*C %=  7) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test6 from Chiara Convert_Test.cc.

void
test14() {
  nout << "test14:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 0*B + 3*C, 4));
  gs.insert( line( 3*A + 2*B + 0*C));
  gs.insert( line( 0*A + 0*B +   C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((-4*A + 6*B + 0*C %= 1) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_generators_and_minimize, with more rows than columns.

void
test15() {
  nout << "test15:" << endl;

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

  Grid gr(4, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  // FIX check
  known_cgs.insert((  0*A +   0*B +  0*C + 0*D %= -27) / 27);
  known_cgs.insert((  9*A +   0*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-18*A +  27*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-90*A + 135*B + 27*C + 0*D %=  27) / 27);
  known_cgs.insert((-17*A +  25*B +  6*C +   D %=   7) / 27);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty grid, one dimension.

void
test16() {
  nout << "test16:" << endl;

  Grid gr(1, EMPTY);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty grid, many dimensions.

void
test17() {
  nout << "test17:" << endl;

  Grid gr(112, EMPTY);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(112, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Bigger values (param_test7a from Chiara Convert_Test.cc).

void
test18() {
  nout << "test18:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-93*A +   0*B +  39*C, 113));
  gs.insert( line( 29*A +  23*B + 111*C));
  gs.insert(point(117*A + 200*B +  88*C, 33));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((       0*A +       0*B +      0*C %=  280730) / 280730);
  known_cgs.insert((  -85767*A +  108141*B +      0*C %=   70587) / 280730);
  known_cgs.insert((-2309489*A + 1557137*B + 280730*C %= 1997619) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Even bigger values (param_test8 from Chiara Convert_Test.cc).

void
test19() {
  nout << "test19:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-9933*A + 2000*B + 3953*C, 9113));
  gs.insert(point(    0*A +    0*B + 8888*C, 7302));
  gs.insert(point(   29*A +   23*B + 1111*C, 1010));
  gs.insert(point( 2394*A + 7273*B +    0*C,   30));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;

  // Create coefficients with string constructors as they're too big
  // for the long type.

  // FIX are the coefficients used or copied?

  // 37315344498526  0  0  0  congruence, modulus = 37315344498526
  // 0  343455281759218112380  0  0  congruence, modulus = 37315344498526
  // 0  -133815138923073144612  223892066991156  0  congruence, modulus = 37315344498526
  // -22220  -31385495955559489171  93798931757298  18255  congruence, modulus = 37315344498526

  Coefficient* tem1 = new Coefficient("37315344498526");
  known_cgs.insert((     0*A +     0*B +     0*C %= -*tem1) / *tem1);

  Coefficient* tem2 = new Coefficient("343455281759218112380");
  known_cgs.insert(( *tem2*A +     0*B +     0*C %= 0) / *tem1);
  delete tem2;

  tem2 = new Coefficient("-133815138923073144612");
  Coefficient* tem3 = new Coefficient("223892066991156");
  known_cgs.insert(( *tem2*A + *tem3*B +     0*C %= 0) / *tem1);
  delete tem2; delete tem3;

  tem2 = new Coefficient("-31385495955559489171");
  tem3 = new Coefficient("93798931757298");
  known_cgs.insert(( *tem2*A + *tem3*B + 18255*C %= 22220) / *tem1);
  delete tem1; delete tem2; delete tem3;


  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Test reduce_line_with_line (param_test9 from Chiara Convert_Test.cc).

void
test20() {
  nout << "test20:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point( -A + 0*B + 3*C, 4));
  gs.insert( line(0*A + 2*B + 0*C));
  gs.insert( line(0*A + 4*B + 0*C));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((4*A + 0*B + 0*C %= -1) / 0);
  known_cgs.insert((0*A + 0*B + 4*C %=  3) / 0);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grids from a water monitor example (from param_test10 from Chiara
// Convert_test.cc).

void
test21() {
  nout << "test21:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(   A));
  gs.insert(point( 2*A +    B));
  gs.insert(point(12*A + 11*B));
  gs.insert(point(10*A + 12*B));
  gs.insert(point( 2*A + 33*B, 2));
  gs.insert(point( 4*A + 35*B, 2));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((C %= 0) / 0);
  known_cgs.insert((-2*A + 2*B %= 1) / 3);
  known_cgs.insert(( 3*A %= 0) / 3);
  known_cgs.insert(( 0*A %= 3) / 3);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// param_test11 from Chiara Convert_Test.cc.

void
test22() {
  nout << "test22:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(4*A -   B + 0*C, 3));
  gs.insert( line(2*A + 3*B));
  gs.insert(point(4*A            , 3));
  gs.insert(point(4*A -   B +   C, 3));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((-9*A + 6*B + 0*C %= 0) / 2);
  known_cgs.insert((             6*C %= 0) / 2);

  Grid known_gr(known_cgs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe grid, one dimension.

void
test23() {
  nout << "test23:" << endl;

  Grid gr(1);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe grid, many dimensions.

void
test24() {
  nout << "test24:" << endl;

  Grid gr(21);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(21);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Universe grid, zero dimensions.

void
test25() {
  nout << "test25:" << endl;

  Grid gr(0);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A generator system with only a line.

void
test26() {
  nout << "test26:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(line(0*A + 2*B + 0*C));

  try {
    Grid gr(gs);
  }
  catch (const std::invalid_argument& e) {
    return;
  }

  exit(1);
}

// A generator system containing a ray.

void
test27() {
  nout << "test27:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(0*C));
  gs.insert( line(A));
  gs.insert( line(B));
  gs.insert(  ray(-C));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
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
  test7();
  test8();
  test9();
  test10();
  test11();
  test12();
  test13();
  test14();
  test15();
  test16();
  test17();
  test18();
  test19();
  test20();
  test21();
  test22();
  test23();
  test24();
  test25();
  test26();
  test27();

  return 0;
}
CATCH
