/* Reduction and conversion tests of grids created from congruences.
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

/* add_congruence_and_minimize, one dimension.  */

void
test1() {
  nout << "\n\ntest1:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 2);

  Grid gr(cgs);
  if (find_variation(gr))
    exit(1);

  Congruence cg(A %= 0);
  gr.add_congruence_and_minimize(cg);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A));
  known_gs.insert(point(2*A));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(1, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_congruence_and_minimize, two dimensions.  */

void
test2() {
  nout << "\n\ntest2:" << endl;

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

  Generator_System known_gs;
  known_gs.insert(point(0*A + 17*B));
  known_gs.insert(point(  A + 59*B));
  known_gs.insert(point(0*A + 37*B));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_congruences_and_minimize, one dimension.  */

void
test3() {
  nout << "\n\ntest3:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 0) / 3);

  Grid gr(1, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A));
  known_gs.insert(point(3*A));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(1, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_congruences_and_minimize, one dimension with factors.  */

void
test4() {
  nout << "\n\ntest4:" << endl;

  Variable A(0);

  Congruence_System cgs;
  cgs.insert((A %= 7) / 3);

  Grid gr(1, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(1*A));
  known_gs.insert(point(4*A));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(1, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* add_congruences_and_minimize, two dimensions.  */

void
test5() {
  nout << "\n\ntest5:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs;
  cgs.insert((A - B %= 0) / 3);

  Grid gr(2, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A + 0*B));
  known_gs.insert(point(A + B));
  known_gs.insert(point(3*B));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(2, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* cong_test0 from Chiara conversion_test2.cc.  */

void
test6() {
  nout << "\n\ntest6:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(0*A %= -1);
  cgs.insert(  A %= 0);
  cgs.insert(  A + B %= 0);
  cgs.insert(  A + B + C %= 0);

  Grid gr(3, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(A - B));
  known_gs.insert(point(B - C));
  known_gs.insert(point(C));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* cong_test1 from Chiara conversion_test2.cc.  */

void
test7() {
  nout << "\n\ntest7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(  0*A %= -1);
  cgs.insert(   -A %= 64);
  cgs.insert( -6*A +   B + 0*C %= -8);
  cgs.insert(  3*A + 2*B +   C %= -4);

  Grid gr(3, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(64*A + 392*B - 972*C, -1));
  known_gs.insert(point(65*A + 398*B - 987*C, -1));
  known_gs.insert(point(64*A + 391*B - 970*C, -1));
  known_gs.insert(point(64*A + 392*B - 973*C, -1));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* Adding a false equality (cong_test2 from Chiara
   conversion_test2.cc).  */

void
test8() {
  nout << "\n\ntest8:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((0*A %= -1) / 0);
  cgs.insert((  A %= -1) / 2);

  Grid gr(3, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, Grid::EMPTY);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

/* cong_test3 from Chiara conversion_test2.cc.  */

void
test9() {
  nout << "\n\ntest9:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert((  0*A %= -2) / 2);
  cgs.insert((    A %=  0) / 2);
  cgs.insert((          B %= 0) / 2);
  cgs.insert((    A +   B +   C %= 0) / 2);

  Grid gr(3, Grid::EMPTY);
  gr.add_congruences_and_minimize(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A + 0*B + 0*C));
  known_gs.insert(point(2*A + 0*B - 2*C));
  known_gs.insert(point(      2*B - 2*C));
  known_gs.insert(point(            2*C));

  //Grid known_gr(known_gs); // FIX
  Grid known_gr(3, Grid::EMPTY);
  known_gr.add_generators(known_gs);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
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
#if 0
  test10();
  test11();
  test12();
  test13();
  test14();
#endif

  return 0;
}
CATCH
