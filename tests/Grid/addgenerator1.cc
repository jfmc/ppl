/* Test Grid::add_generator*().
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

#define find_variation find_variation_template<Grid>

// grid1.cc also tests add_generator_and_minimize.

// One dimension.

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Grid gr(1, EMPTY);
  gr.add_generator(point(-A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(1);
  known_gr.add_congruence((A == -1) / 0);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Two dimensions.

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(point(A + B));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((A == 1) / 0);
  known_gr.add_congruence((B == 1) / 0);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add many generators to grid of two dimensions.

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(A + 2*B));
  gr.add_generator(point(A + B));
  gr.add_generator(point(2*A + 2*B));
  gr.add_generator(line(A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence(B %= 0);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add NNC generators.

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(closure_point(3*A, 4));
  gs.insert(point(7*A, 4));
  gs.insert(line(A - B));

  Grid gr(2, EMPTY);

  for (Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i)
    if (!(*i).is_closure_point())
      gr.add_generator(*i);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((4*A + 4*B == 7) / 0);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add generators to a grid of a higher space dimension.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4, EMPTY);
  gr.add_generator(point(7*A, 3));
  gr.add_generator(line(A - B));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);
  known_gr.add_congruence((3*A + 3*B == 7) / 0);
  known_gr.add_congruence((C == 0) / 0);
  known_gr.add_congruence((D == 0) / 0);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// add_generator_and_minimize

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);

  Grid gr(2, EMPTY);
  gr.add_generator(point());
  gr.add_generator(point(2*A + 2*B));
  gr.add_generator(point(8*A + 8*B));

  gr.add_generator_and_minimize(line(A));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(2);
  known_gr.add_congruence((B %= 0) / 2);

  if (find_variation(known_gr))
    exit(1);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

// Add a generator to a universe grid.

void
test7() {
  nout << "test7:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(4);
  gr.add_generator(point(12*A + 7*D));

  if (find_variation(gr))
    exit(1);

  Grid known_gr(4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  exit(1);
}

int
main() TRY {
  set_handlers();

  nout << "addgenerator1:" << endl;

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
