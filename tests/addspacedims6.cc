/* Test add_space_dimensions_and_embed() for NNC_Polyhedron.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

  Variable x(0);

  NNC_Polyhedron ph1(1);

  ph1.add_constraint(x > 1);
  ph1.add_constraint(x < 5);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.add_space_dimensions_and_embed(2);

  Generator_System gs;
  gs.insert(point(2*x));
  gs.insert(closure_point(x));
  gs.insert(closure_point(5*x));

  NNC_Polyhedron ph2(gs);

#if NOISY
  print_generators(ph2, "*** ph2 ***");
#endif

  ph2.add_space_dimensions_and_embed(2);

  int retval = (ph1 == ph2) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** ph1 after add_space_dimensions_and_embed ***");
  print_generators(ph2, "*** ph2 after add_space_dimensions_and_embed ***");
#endif

  return retval;
}
CATCH

