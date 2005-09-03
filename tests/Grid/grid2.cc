/* Test reduction and conversion of grids created from congruences.
Copyright (C) 2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

// add_congruence_and_minimize, one dimension.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);

  Grid gr(cgs);

  if (find_variation(gr))
    exit(1);

  Congruence cg(A %= 0);
  gr.add_congruence_and_minimize(cg);

  Generator_System known_gs;
  known_gs.insert(point(0*A));
  known_gs.insert(point(2*A));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruence_and_minimize, two dimensions.

void
test2() {
  nout << "test2:" << endl;

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

  // FIX check
  Generator_System known_gs;
  known_gs.insert(point(0*A - 15*B, 5));
  known_gs.insert(point(  A + 27*B, 5));
  known_gs.insert(point(0*A + 85*B, 5));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruences_and_minimize, one dimension.

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 3);

  Grid gr(1);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A));
  known_gs.insert(point(3*A));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruences_and_minimize, one dimension with factors.

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 3);

  Grid gr(1);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(1*A));
  known_gs.insert(point(4*A));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// add_congruences_and_minimize, two dimensions.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A - B %= 0) / 3);

  Grid gr(2);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(3*A));
  known_gs.insert(point(3*B));
  known_gs.insert(line(A + B));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test0 from Chiara Convert_Test.cc.

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(A %= 0);
  cgs.insert(A + B %= 0);
  cgs.insert(A + B + C %= 0);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(A));
  known_gs.insert(point(B));
  known_gs.insert(point(C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test1 from Chiara Convert_Test.cc.

void
test7() {
  nout << "test7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(  -A %= 64);
  cgs.insert(-6*A +   B + 0*C %= -8);
  cgs.insert( 3*A + 2*B +   C %= -4);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(A));
  known_gs.insert(point(B));
  known_gs.insert(point(C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Adding a false equality (cong_test2 from Chiara Convert_Test.cc).

void
test8() {
  nout << "test8:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((0*A %= -1) / 0);
  cgs.insert((  A %= -1) / 2);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test3 from Chiara Convert_Test.cc.

void
test9() {
  nout << "test9:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A         %= 0) / 2);
  cgs.insert((    B     %= 0) / 2);
  cgs.insert((A + B + C %= 0) / 2);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(2*A));
  known_gs.insert(point(2*B));
  known_gs.insert(point(2*C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test4 from Chiara Convert_Test.cc.

void
test10() {
  nout << "test10:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((3*A             %= -2) / 3);
  cgs.insert((5*A + 9*B +   C %= -1) / 3);
  cgs.insert((        B + 3*C %= -2) / 3);
  cgs.insert((      2*B + 3*C %= -2) / 3);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(-2*A + 0*B +  7*C, 3));
  known_gs.insert(point( 1*A + 0*B +    C, 3));
  known_gs.insert(point(-2*A + 9*B +  7*C, 3));
  known_gs.insert(point(-2*A + 0*B + 16*C, 3));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test5 from Chiara Convert_Test.cc.

void
test11() {
  nout << "test11:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A + 2*B +   C %= -2) / 5);
  cgs.insert((    3*B       %=  0) / 5);
  cgs.insert((      B       %=  0) / 5);
  cgs.insert((          3*C %= -4) / 5);
  cgs.insert((    3*B +   C %= -3) / 5);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(  A       - 3*C));
  known_gs.insert(point(6*A       - 3*C));
  known_gs.insert(point(  A + 5*B - 3*C));
  known_gs.insert(point(  A       + 2*C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test6 from Chiara Convert_Test.cc.

void
test12() {
  nout << "test12:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((3*A           %= -2) / 5);
  cgs.insert((      B + 2*C %=  0) / 5);
  cgs.insert((    2*B + 3*C %= -3) / 5);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(-2*A -  3*B - 6*C, 3));
  known_gs.insert(point( 3*A -  3*B - 6*C, 3));
  known_gs.insert(point(-2*A + 12*B - 6*C, 3));
  known_gs.insert(point(-2*A -  3*B + 9*C, 3));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test7 from Chiara Convert_Test.cc.

void
test13() {
  nout << "test13:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 1) / 0);
  cgs.insert((B %= 1) / 0);
  cgs.insert((C %= 1) / 0);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(A + B + C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// cong_test8 from Chiara Convert_Test.cc.

void
test14() {
  nout << "test14:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((A %= 1) / 0);
  cgs.insert(B %= 1);
  cgs.insert((C %= 1) / 0);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(A + C));
  known_gs.insert(point(A + B + C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// An empty grid constructed from congruences.

void
test15() {
  nout << "test15:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((C %= 2) / 5);
  cgs.insert((C %= 3) / 5);

  Grid gr(3);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Adding a congruence system with a capacity larger than the capacity
// of the existing system.

void
test16() {
  nout << "test16:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs1;
  cgs1.insert(B %= 2);

  Grid gr(2);
  gr.add_congruences_and_minimize(cgs1);

  gr.add_space_dimensions_and_embed(1);

  // gr.con_sys is likely to be expanded within capacity.

  Variable C(2);

  Congruence_System cgs2;
  cgs2.insert(C %= 2);

  // cgs2 is likely to now have more capacity than gr.con_sys does.

  gr.add_congruences_and_minimize(cgs2);

  if (find_variation(gr))
    exit(1);

  Congruence_System cgs3;
  cgs3.insert(B %= 2);
  cgs3.insert(C %= 2);

  Grid known_gr(cgs3);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "grid2:" << endl;

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

  return 0;
}
CATCH
