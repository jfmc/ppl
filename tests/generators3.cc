/* Test Polyhedron::generators(): we test this function in the case
   of a polyhedron that is declared empty, a zero-dimensional
   polyhedron and an empty polyhedron.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable A(0);

  C_Polyhedron ph1(2, C_Polyhedron::EMPTY);

  C_Polyhedron ph2(0);

  C_Polyhedron ph3(2);
  ph3.add_constraint(A >= 1);
  ph3.add_constraint(A <= -1);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
  print_generators(ph3, "*** ph3 ***");
#endif

  GenSys gs1 = ph1.generators();
  GenSys gs2 = ph2.generators();
  GenSys gs3 = ph3.generators();

  GenSys known_result2;
  known_result2.insert(point());
  int retval = (gs1.begin() == gs1.end()
		&& C_Polyhedron(gs2) == C_Polyhedron(known_result2)
		&& gs3.begin() == gs3.end()) ? 0 : 1;

#if NOISY
  print_generators(gs1, "*** gs1 ***");
  print_generators(gs2, "*** gs2 ***");
  print_generators(gs3, "*** gs3 ***");
#endif

  return retval;
}
CATCH
