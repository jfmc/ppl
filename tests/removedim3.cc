/* Remove some variables from the space.
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

  Variable y(1);
  Variable z(2);
  Variable w(6);

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(w);

  // A 10-dim space, empty polyhedron.
  C_Polyhedron ph(10, C_Polyhedron::EMPTY);
  ph.remove_dimensions(to_be_removed);

  // A 7-dim space, empty polyhedron.
  C_Polyhedron known_result(7, C_Polyhedron::EMPTY);

  int retval = (known_result == ph) ? 0 : 1;

#if NOISY
  print_constraints(ph, "*** ph ***");
  print_constraints(known_result, "*** known_result ***");
#endif

  return retval;
}
