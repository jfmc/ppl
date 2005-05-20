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

void
test1() {
  nout << "test1:" << endl;

  PFunction function;

  Grid gr(3, Grid::EMPTY);

  gr.map_space_dimensions(function);

  Grid known_gr;

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
  exit(1);
}

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
  exit(1);
}

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
  exit(1);
}

void
test5() {
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

  C_Polyhedron ph1(gs);
  C_Polyhedron known_result(ph1);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_space_dimensions(function);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

void
test6() {
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

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(4, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(point(B));
  known_result.add_generator(point(2*A));
  known_result.add_generator(point(2*A + B));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

void
test7() {
  Variable A(0);
  Variable B(1);

  PFunction function;
  function.insert(0, 0);
  function.insert(2, 1);
  function.insert(3, 2);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A));
  gs.insert(ray(B));
  gs.insert(ray(A + B));

  C_Polyhedron ph1(gs);

#if NOISY
  print_function(function, "*** function ***");
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(3, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(ray(A));

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.map_space_dimensions(function) ***");
#endif

  if (!ok)
    exit(1);
}

void
test8() {
  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  C_Polyhedron ph1(3, C_Polyhedron::EMPTY);

#if NOISY
  print_function(function, "*** function ***");
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.map_space_dimensions(function);

  C_Polyhedron known_result(2, C_Polyhedron::EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.map_space_dimensions(function) ***");
#endif

  if (!ok)
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
#if 0 // FIX
  test5();
  test6();
  test7();
  test8();
#endif

  return 0;
}
CATCH
