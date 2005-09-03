/* Test operator==(const Grid&, const Grid&)
   and operator!=(const Grid&, const Grid&).
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(3*A));

  Grid gr(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;
  known_cgs.insert((A %= 0) / 3);

  Grid known_gr(known_cgs);

  if (gr != known_gr) {
    nout << "gr != known_gr should return false." << endl
	 << "grid:" << endl << gr << endl
	 << "known grid:" << endl << known_gr << endl;
    exit(1);
  }

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(A - B %= 0);
  cgs.insert((C %= 0) / 7);

  Grid gr(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System gs;
  gs.insert(point());
  gs.insert( line(A + B));
  gs.insert(point(B));
  gs.insert(point(7*C));

  Grid known_gr(gs);

  if (gr != known_gr) {
    nout << "gr != known_gr should return false." << endl
	 << "grid:" << endl << gr << endl
	 << "known grid:" << endl << known_gr << endl;
    exit(1);
  }

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Congruence_System cgs;
  cgs.insert(A - B %= 0);
  cgs.insert((C %= 0) / 7);

  Grid gr(cgs);

  if (find_variation(gr))
    exit(1);

  Generator_System gs;
  gs.insert(point());
  gs.insert( line(A - B));
  gs.insert(point(B));
  gs.insert(point(7*C, 3));

  Grid known_gr(gs);

  if (gr == known_gr) {
    nout << "gr == known_gr should return false." << endl
	 << "grid:" << endl << gr << endl
	 << "known grid:" << endl << known_gr << endl;
    exit(1);
  }

  if (gr != known_gr)
    return;

  nout << "gr != known_gr should return true." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;
  exit(1);
}

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point(0*A));
  gs.insert(point(1*A));

  Grid gr1(gs);

  if (find_variation(gr1))
    exit(1);

  Generator_System known_gs;
  known_gs.insert(point(0*A + 0*B));
  known_gs.insert(point(1*A + 0*B));
  known_gs.insert(point(0*A + 1*B));

  Grid known_gr(known_gs);

  if (gr1 == known_gr) {
    nout << "gr == known_gr should return false." << endl
	 << "grid:" << endl << gr1 << endl
	 << "known grid:" << endl << known_gr << endl;
    exit(1);
  }

  gs.clear();
  gs.insert(point(0*A + 0*B));
  gs.insert(point(1*A + 0*B));
  gs.insert( line(0*A + 1*B));

  Grid gr2(gs);

  if (gr1 == gr2) {
    nout << "gr1 == gr2 should return false." << endl
	 << "gr1:" << endl << gr1 << endl
	 << "gr2:" << endl << gr2 << endl;
    exit(1);
  }
}

int
main() TRY {
  set_handlers();

  nout << "equals1:" << endl;

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
