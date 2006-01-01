/* Test Polyhedron::intersection_assign_and_minimize() and
   C_Polyhedron::intersection_assign(): we intersect an empty
   polyhedron with one non-empty.
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

  C_Polyhedron ph1(2);
  ph1.add_constraint(x >= y);
  ph1.add_constraint(x >= 0);

  C_Polyhedron ph2(2, EMPTY);

  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");

  C_Polyhedron computed_result1(ph1);
  computed_result1.intersection_assign_and_minimize(ph2);

  Constraint_System cs_computed_result2 = ph1.constraints();
  C_Polyhedron computed_result2(cs_computed_result2);
  computed_result2.intersection_assign(ph2);

  C_Polyhedron known_result(2, EMPTY);

  int retval = (computed_result1 == known_result
		&& computed_result2 == known_result) ? 0 : 1;

  print_constraints(computed_result1,
		    "*** After intersection_assign_and_minimize ***");
  print_constraints(computed_result2, "*** After intersection_assign ***");

  return retval;
}
CATCH
