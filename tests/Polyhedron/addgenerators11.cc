/* Test Polyhedron::add_generators_and_minimize(): we add a system
   of generators of a topologically closed NNC polyhedron to a
   necessarily closed polyhedron.
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);

  Generator_System gs1;
  gs1.insert(point(A + B));
  gs1.insert(closure_point());
  gs1.insert(ray(A));
  gs1.insert(ray(B));
  NNC_Polyhedron ph1(gs1);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.topological_closure_assign();
  Generator_System gs2 = ph1.minimized_generators();

#if NOISY
  print_generators(gs2, "*** gs2 ***");
#endif

  C_Polyhedron ph2(2, EMPTY);
  ph2.add_generators_and_minimize(gs2);

#if NOISY
  print_constraints(ph2, "*** ph2 ***");
#endif

  C_Polyhedron known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B >= 0);

  int retval = (ph2 == known_result) ? 0 : 1;

#if NOISY
  print_generators(known_result, "*** known_result ***");
#endif

  return retval;
}
CATCH
