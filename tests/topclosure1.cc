/* Test Polyhedron::topological_closure_assign(): we test this
   function in the case of a polyhedron defined by the system of
   constraints and in the case of a polyhedron defined by the
   system of generators.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
main() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A > 1);
  ph1.add_constraint(A - B > 0);
  GenSys gs;
  gs.insert(point(2*A));
  gs.insert(closure_point(A + B));
  gs.insert(ray(-B));
  gs.insert(ray(A + B));
  NNC_Polyhedron ph2(gs);
  
#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph1.topological_closure_assign();
  ph2.topological_closure_assign();

  int retval = (ph1 == ph2) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** After ph1.topological_closure_assign() ***");
  print_generators(ph2, "*** After ph2.topological_closure_assign() ***");
#endif

  return retval;
}
