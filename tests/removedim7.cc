/* Testing remove_higher_dimensions() and remove_dimensions()
   for NN_Polyhedron.
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

#define NOISY 0

int
main() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  NNC_Polyhedron ph1(4);

  ph1.add_constraint(x - y == 3);
  ph1.add_constraint(z > x + 4);
  ph1.add_constraint(y < 6);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  NNC_Polyhedron ph2(ph1);

  ph1.remove_higher_dimensions(1);

  // This is the set of the variables that we want to remove.
  set<Variable> to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(Variable(3));

  ph2.remove_dimensions(to_be_removed);

  int retval = (ph1 == ph2) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** After remove_higher_dimensions(1) ***");
  print_constraints(ph2, "*** After remove_dimensions(to_be_removed) ***");
#endif

  return retval;
}
