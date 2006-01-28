/* Test construction of grids from constraints.
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

namespace {

Variable A(0);
Variable B(1);
Variable C(2);

// Grid(cs)

void
test1() {
  nout << "test1:" << endl;

  Constraint_System cs;
  cs.insert(B == 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(B == 0);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grid(cs), resulting grid empty.

void
test2() {
  nout << "test2:" << endl;

  Constraint_System cs;
  cs.insert(B < 0);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  Grid gr(cs);

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

// Grid(const cs)

void
test3() {
  nout << "test3:" << endl;

  Constraint_System cs;
  cs.insert(2*B == A);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  const Constraint_System ccs = cs;

  Grid gr(ccs);

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3);
  known_gr.add_congruence(2*B == A);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Grid(const cs), resulting grid empty.

void
test4() {
  nout << "test4:" << endl;

  Constraint_System cs;
  cs.insert(2*B >= A);
  cs.insert(A >= 0);
  cs.insert(C > 0);

  const Constraint_System ccs = cs;

  Grid gr(ccs);

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

// Space dimension exception.

void
test5() {
  nout << "test5:" << endl;

  try {
    Grid gr(Constraint_System::max_space_dimension() + 1);
    exit(1);
  }
  catch (const std::length_error& e) {}
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "grid3:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH
