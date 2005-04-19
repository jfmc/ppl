/* Grid reduction and conversion tests, which start with generators.
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

// FIX take out all line spacing on function name tracing ("\n\ntest1")

// FIX Comparing known_grid failures is easier with ASCII dumps.

/* add_generator_and_minimize, one variable.  */

void
test1() {
  nout << "\n\ntest1:" << endl;

  Variable A(0);

  Generator_System gs;
  gs.insert(point(A));

  Grid gr(gs);

  nout << "Adding generator..." << endl;
  gr.add_generator_and_minimize(point(2*A));
  nout << "Adding generator... done." << endl;

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(A %= 0);

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(1, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_generator_and_minimize, two variables.  */

void
test2() {
  nout << "\n\ntest2:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point(A + B));

  Grid gr(gs);
  if (find_variation(gr))
    exit(1);

  Generator g(point(A + 2*B));
  nout << "Adding generator..." << endl;
  gr.add_generator_and_minimize(g);
  nout << "Adding generator... done." << endl;

  Congruence_System known_cgs;
  known_cgs.insert( 0*A + 0*B %= -1);
  known_cgs.insert((  A + 0*B %=  1) / 0);
  known_cgs.insert( 0*A + 1*B %=  1);

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_generators_and_minimize */

void
test3() {
  nout << "\n\ntest3:" << endl;

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
  known_gs.insert(point(9*A + 0*B));

  Grid known_gr(known_gs);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;
  exit(1);
}

/* test0 from Chiara conversion_test.cc.  */

void
test4() {
  nout << "\n\ntest4:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(4*A -   B + 0*C, 3));
  gs.insert( line(2*A + 3*B + 0*C)); // FIX divisor
  gs.insert(point(4*A + 0*B + 0*C, 3));
  gs.insert(point(4*A -   B +   C, 3));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -2) / 1);
  // FIX how to create virtual rows?
  known_cgs.insert((   A + 0*B + 0*C %=  0) / -1);
  known_cgs.insert((-9*A + 6*B + 0*C %= 14) / 1);
  known_cgs.insert(( 0*A - 0*B + 6*C %=  0) / 1);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;
  exit(1);
}

/* test1 from Chiara conversion_test.cc.  */

void
test5() {
  nout << "\n\ntest5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B +  3*C, 2));
  gs.insert( line( 3*A + 2*B -  4*C));
  gs.insert( line( 0*A + 0*B -  2*C));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(0*A + 0*B + 0*C %= -1);
  known_cgs.insert((-2*A + 3*B + 0*C %= 7) / 0);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* test2 from Chiara conversion_test.cc.  */

void
test6() {
  nout << "\n\ntest6:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B +  3*C, 2));
  gs.insert(point( 2*A + 6*B -    C, 2));
  gs.insert(point(-1*A + 9*B +  7*C, 2));
  gs.insert( line( 0*A + 0*B -  2*C));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((  0*A + 0*B + 0*C %=  15) / 15);
  known_cgs.insert((-10*A + 0*B + 0*C %=   5) / 15);
  known_cgs.insert((  4*A - 6*B + 0*C %= -14) / 15);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* test3 from Chiara conversion_test.cc.  */

void
test7() {
  nout << "\n\ntest7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-1*A + 4*B + 3*C, 2));
  gs.insert( line( 2*A +   B - 2*C));
  gs.insert(point(-1*A + 9*B + 7*C, 2));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -10) / 10);
  known_cgs.insert((-2*A + 4*B + 0*C %=   9) / 10);
  known_cgs.insert(( 7*A - 4*B + 5*C %=  -4) / 0);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* test4 from Chiara conversion_test.cc.  */

void
test8() {
  nout << "\n\ntest8:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point( 3*A +   B + 0*C, 4));
  gs.insert(point(11*A + 2*B + 0*C, 4));
  gs.insert(point( 3*A + 6*B + 0*C, 4));
  gs.insert(point( 3*A +   B + 2*C, 4));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A +  0*B +  0*C %= -40) / 40);
  known_cgs.insert((20*A +  0*B +  0*C %=  15) / 40);
  known_cgs.insert((-4*A + 32*B +  0*C %=   5) / 40);
  known_cgs.insert(( 0*A +  0*B + 80*C %=   0) / 40);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* test5 from Chiara conversion_test.cc.  */

void
test9() {
  nout << "\n\ntest9:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point( 0*A + 7*B + 0*C, 3));
  gs.insert( line( 3*A + 2*B + 0*C));
  gs.insert( line( 0*A + 0*B +   C));

  Grid gr(3, Grid::EMPTY);
  gr.add_generators_and_minimize(gs);
  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert(( 0*A + 0*B + 0*C %= -1) / 1);
  known_cgs.insert((-2*A + 3*B + 0*C %=  7) / 0);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* add_generators_and_minimize, with more rows than columns.  */

void
test10() {
  nout << "\n\ntest10:" << endl;

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

  Congruence_System known_cgs;
  // FIX check
  known_cgs.insert((  0*A +   0*B +  0*C + 0*D %= -27) / 27);
  known_cgs.insert((  9*A +   0*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-18*A +  27*B +  0*C + 0*D %=   0) / 27);
  known_cgs.insert((-90*A + 135*B + 27*C + 0*D %=  27) / 27);
  known_cgs.insert((-17*A +  25*B +  6*C +   D %=   7) / 27);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(4, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* Empty grid, one dimension.  */

void
test11() {
  nout << "\n\ntest11:" << endl;

  Grid gr(1, Grid::EMPTY);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1, Grid::EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* Empty grid, many dimensions.  */

void
test12() {
  nout << "\n\ntest12:" << endl;

  Grid gr(112, Grid::EMPTY);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(112, Grid::EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* Bigger values (test7 from Chiara conversion_test2.cc).  */

void
test13() {
  nout << "\n\ntest13:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-93*A +   0*B +  39*C, 113));
  gs.insert( line( 29*A +  23*B + 111*C));
  gs.insert(point(117*A + 200*B +  88*C, 33));

  Grid gr(3, Grid::EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((       0*A +       0*B +      0*C %=  280730) / 280730);
  known_cgs.insert((     -23*A +      29*B +      0*C %=   59643) / 280730);
  known_cgs.insert((-2309489*A + 1557137*B + 280730*C %= 1997619) / 0);

  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

/* Even bigger values (test8 from Chiara conversion_test2.cc).  */

void
test14() {
  nout << "\n\ntest14:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point(-9933*A + 2000*B + 3953*C, 9113));
  gs.insert(point(   29*A +   23*B + 1111*C, 1010));
  gs.insert(point( 2394*A + 7273*B +    0*C,   30));
  gs.insert(point(    0*A +    0*B + 8888*C, 7302));

  Grid gr(3, Grid::EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;

  // Create coefficients with string constructors as they're too big
  // for long.

  // FIX are the coefficients used or copied?

  Coefficient* tem1 = new Coefficient("37315344498526");
  known_cgs.insert((     0*A +     0*B + 0*C %=        *tem1) / *tem1);

  Coefficient* tem2 = new Coefficient("419950208052071972814");
  known_cgs.insert((-*tem1*A +     0*B + 0*C %=  *tem2) / *tem1);
  delete tem2;

  tem2 = new Coefficient("-8304861576928864199088");
  Coefficient* tem3 = new Coefficient("93463651403994354999109986832");
  known_cgs.insert(( *tem2*A + *tem1*B + 0*C %=  *tem3) / *tem1);
  delete tem2; delete tem3;

  tem2 = new Coefficient("-1453742620492502229473");
  tem3 = new Coefficient("6531945920270");
  Coefficient* tem4 = new Coefficient("16360548848917233378527473980");
  known_cgs.insert(( *tem2*A + *tem3*B - 1*C %= *tem4) / *tem1);
  delete tem1; delete tem2; delete tem3; delete tem4;


  //Grid known_gr(known_cgs);  // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_congruences(known_cgs);

  if (find_variation(known_gr))
    exit(1);

  if (known_gr == gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

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

  return 0;
}
CATCH
