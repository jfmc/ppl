/* Test reduction and conversion of grids created from generators.
   Tests that are only expected to pass using GMP coefficients.
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

// Even bigger values (param_test8 from Chiara Convert_Test.cc).

void
test1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Grid_Generator_System gs;
  gs.insert(grid_point(-9933*A + 2000*B + 3953*C, 9113));
  gs.insert(grid_point(    0*A +    0*B + 8888*C, 7302));
  gs.insert(grid_point(   29*A +   23*B + 1111*C, 1010));
  gs.insert(grid_point( 2394*A + 7273*B +    0*C,   30));

  Grid gr(3, EMPTY);

  gr.add_generators_and_minimize(gs);

  if (find_variation(gr))
    exit(1);

  Congruence_System known_cgs;

  // Create coefficients with string constructors as they're too big
  // for the long type.

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

int
main() TRY {
  set_handlers();

  nout << "grid1_gmp:" << endl;

  DO_TEST(test1);

  return 0;
}
CATCH
