/* Test Polyhedron::constraints(): we compute the system of
   constraints of a polyhedron that is defined by a system of
   constraints that contains only a trivially false constraint.
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

int
main() TRY {
  set_handlers();

  Variable A(0);

  C_Polyhedron ph1(2);
  ph1.add_constraint(0*A == 1);

  print_constraints(ph1, "*** ph constraints ***");

  C_Polyhedron known_result = ph1;

  Constraint_System cs = ph1.constraints();
  C_Polyhedron ph2(cs);

  int retval = (ph2 == known_result) ? 0 : 1;

  print_constraints(cs, "*** cs ***");

  return retval;
}
CATCH
