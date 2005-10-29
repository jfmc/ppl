/* Test Grid::Grid(Box&, From_Bounding_Box).
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

// This constructor is also tested via coveringbox2.cc.

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

namespace {

Variable A(0);
Variable B(1);
Variable C(2);
Variable D(3);

#define SPACE_DIM 2

// Universe box.

void
test1() {
  nout << "test1:" << endl;

  Bounding_Box box(SPACE_DIM);

  Grid gr(box, From_Bounding_Box());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(SPACE_DIM);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A 2D box which is a line parallel to the x axis.

void
test2() {
  nout << "test2:" << endl;

  Bounding_Box box(SPACE_DIM);
  box.raise_lower_bound(1, true, 2, 3);
  box.lower_upper_bound(1, true, 2, 3);

  Grid gr(box, From_Bounding_Box());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(SPACE_DIM);
  known_gr.add_congruence(3*B == 2);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A 2D box that is a point, with divisors.

void
test3() {
  nout << "test3:" << endl;

  Bounding_Box box(SPACE_DIM);
  box.raise_lower_bound(0, true, -2, 3);
  box.lower_upper_bound(0, true, -2, 3);
  box.raise_lower_bound(1, true, -10, 1);
  box.lower_upper_bound(1, true, -10, 1);

  Grid gr(box, From_Bounding_Box());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(SPACE_DIM, EMPTY);
  known_gr.add_generator(point(-2*A - 30*B, 3));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A 3D box which is a 2D plane.

void
test4() {
  nout << "test4:" << endl;

  Bounding_Box box(3);
  box.raise_lower_bound(2, true, 15, 5);
  box.lower_upper_bound(2, true, 15, 5);

  Grid gr(box, From_Bounding_Box());

  if (find_variation(gr))
    exit(1);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(point(3*C));
  known_gr.add_generator( line(  A));
  known_gr.add_generator( line(  B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Zero-dimensional box.

void
test5() {
  nout << "test5:" << endl;

  Bounding_Box box(0);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr;

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Empty box in 2D.

void
test6() {
  nout << "test6:" << endl;

  Bounding_Box box(2);
  box.set_empty();

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// A 4D box containing a single 3D space.

void
test7() {
  nout << "test7:" << endl;

  Bounding_Box box(4);
  box.raise_lower_bound(3, true, 4, 1);
  box.lower_upper_bound(3, true, 4, 1);

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(4);
  known_gr.add_constraint(D == 4);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

// Unit square.

void
test8() {
  nout << "test8:" << endl;

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.lower_upper_bound(0, true, 1, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 1);

  bool caught = false;

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (std::invalid_argument e) {
    caught = true;
  }

  if (caught)
    return;

  nout << "Construction should have thrown std::invalid_argument."
       << endl;

  exit(1);
}

// Simple box with divisor and an interval bounded only from below.

void
test9() {
  nout << "test9:" << endl;

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 0, 1);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  bool caught = false;

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (std::invalid_argument e) {
    caught = true;
  }

  if (caught)
    return;

  nout << "Construction should have thrown std::invalid_argument."
       << endl;

  exit(1);
}

// Box with a dimension bounded only from above.

void
test10() {
  nout << "test10:" << endl;

  Bounding_Box box(2);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, true, 0, 1);
  box.lower_upper_bound(1, true, 1, 2);

  bool caught = false;

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (std::invalid_argument e) {
    caught = true;
  }

  if (caught)
    return;

  nout << "Construction should have thrown std::invalid_argument."
       << endl;

  exit(1);
}

// An otherwise valid box having a dimension with an open bound, where
// the open bound makes the box empty.

void
test11() {
  nout << "test11:" << endl;

  Bounding_Box box(2);
  box.raise_lower_bound(0, true, 3, 7);
  box.lower_upper_bound(0, true, 3, 7);
  box.raise_lower_bound(1, false, 1, 2);
  box.lower_upper_bound(1, true, 1, 2);

  bool caught = false;

  try {
    Grid gr(box, From_Bounding_Box());
  }
  catch (std::invalid_argument e) {
    caught = true;
  }

  if (caught)
    return;

  nout << "Construction should have thrown std::invalid_argument."
       << endl;

  exit(1);
}

// Zero-dimensional empty box.

void
test12() {
  nout << "test12:" << endl;

  Bounding_Box box(0);
  box.set_empty();

  Grid gr(box, From_Bounding_Box());

  Grid known_gr(0, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << " grid:" << endl << gr << endl
       << "known:" << endl << known_gr << endl;

  dump_grids(gr, known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "boundingbox1:" << endl;

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

  return 0;
}
CATCH
