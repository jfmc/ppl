/* Test Polyhedron::remove_higher_space_dimensions() and
   C_Polyhedron::remove_space_dimensions(): we obtain a zero-dimensional
   polyhedron removing all the dimensions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  C_Polyhedron ph1(3);
  ph1.add_constraint(x >= 3);
  ph1.add_constraint(x - y >= 0);

  print_constraints(ph1, "*** ph1 ***");

  C_Polyhedron ph2 = ph1;

  print_constraints(ph2, "*** ph2 ***");

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(x);

  ph1.remove_space_dimensions(to_be_removed);
  ph2.remove_higher_space_dimensions(0);

  int retval = (ph1 == ph2) ? 0 : 1;

  print_generators(ph1, "*** ph1 after remove_space_dimensions ***");
  print_generators(ph2, "*** ph2 after remove_higher_space_dimensions ***");

  return retval;
}
CATCH
