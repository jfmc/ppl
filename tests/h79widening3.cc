/* Test Polyhedron::H79_widening_assign().
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

  // This is the example of Figure 3 in [BagnaraRZH02TR].
  Variable A(0);

  NNC_Polyhedron ph1(1);
  ph1.add_constraint(A > 0);
  ph1.add_constraint(A < 2);

  NNC_Polyhedron ph4(1);
  ph4.add_constraint(4*A >= 1);
  ph4.add_constraint(4*A <= 3);

  NNC_Polyhedron ph = ph4;
  ph.intersection_assign_and_minimize(ph1);
  // At this point, ph and ph4 are two different representations
  // of the same NNC polyhedron.

#if NOISY
  print_constraints(ph4, "*** ph4 ***");
  print_constraints(ph, "*** ph ***");
#endif

  ph.H79_widening_assign(ph4);

#if NOISY
  print_constraints(ph, "*** After H79_widening_assign ***");
#endif

  return 0;
}
