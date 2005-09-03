/* Test Grid::map_space_dimensions().
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
#include "PFunction.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Grid>

namespace {

#if NOISY
void
print_function(const PFunction& function, const std::string& intro = "",
	       std::ostream& s = std::cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}
#endif

// Empty grid, empty mapping.

void
test1() {
  nout << "test1:" << endl;

  PFunction function;

  Grid gr(3, EMPTY);

  gr.map_space_dimensions(function);

  Grid known_gr(0, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping all dimensions.

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(0, 2);
  function.insert(2, 0);
  function.insert(1, 1);

  Generator_System gs;
  gs.insert(point(2*C));
  gs.insert(point(4*C));
  gs.insert(point(A));

  Grid gr(gs);

  gr.map_space_dimensions(function);

  Generator_System known_gs;
  known_gs.insert(point(2*A));
  known_gs.insert(point(4*A));
  known_gs.insert(point(C));

  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping all dimensions, with overlap.

void
test3() {
  nout << "test3:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(0, 1);
  function.insert(2, 0);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(2*A));
  gs.insert(line(A - C));

  Grid gr(gs);

  gr.map_space_dimensions(function);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(2*B));
  known_gs.insert(line(B - A));
  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping more dimensions than there are in the grid.

void
test4() {
  nout << "test4:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(2, 0);
  function.insert(3, 2);
  function.insert(4, 1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(B));

  Grid gr(gs);

  gr.map_space_dimensions(function);

  Generator_System known_gs;
  known_gs.insert(point(0*C));
  Grid known_gr(known_gs);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping all dimensions to themselves.

void
test5() {
  nout << "test5:" << endl;

  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 0);
  function.insert(1, 1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(B));
  gs.insert(point(A + B));

  Grid gr(gs);
  Grid known_gr(gr);

  gr.map_space_dimensions(function);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping all additional dimensions (in the mapping) to themselves.

void
test6() {
  nout << "test6:" << endl;

  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);
  function.insert(2, 2);
  function.insert(3, 3);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(point(2*B));
  gs.insert(point(A + 2*B));

  Grid gr(gs);

  gr.map_space_dimensions(function);

  Grid known_gr(4, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator(point(B));
  known_gr.add_generator(point(2*A));
  known_gr.add_generator(point(2*A + B));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping new dimensions over existing ones.

void
test7() {
  nout << "test7:" << endl;

  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 0);
  function.insert(2, 1);
  function.insert(3, 2);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(line(B));
  gs.insert(line(A + B));

  Grid gr(gs);

  gr.map_space_dimensions(function);

  Grid known_gr(3, EMPTY);
  known_gr.add_generator(point());
  known_gr.add_generator( line(A));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// Mapping away a dimension in an empty grid.

void
test8() {
  nout << "test8:" << endl;

  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  Grid gr(3, EMPTY);

  gr.map_space_dimensions(function);

  Grid known_gr(2, EMPTY);

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

// A minimized grid in which the point contains factors and the
// divisor is greater than one.

void
test9() {
  nout << "test9:" << endl;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  Grid gr(3, EMPTY);
  gr.add_generator(point(4*A, 2));
  gr.add_generator(point(4*A + B, 2));

  // Force minimization.
  if (find_variation(gr))
    exit(1);

  gr.map_space_dimensions(function);

  Grid known_gr(2, EMPTY);
  known_gr.add_generator(point(4*B, 2));
  known_gr.add_generator(point(4*B + A, 2));

  if (gr == known_gr)
    return;

  nout << "Grid should equal known grid." << endl
       << "grid:" << endl << gr << endl
       << "known grid:" << endl << known_gr << endl;

  dump_grids(gr,known_gr);

  exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "mapspacedims1:" << endl;

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();

  return 0;
}
CATCH
