/* Test Polyhedron::generalized_affine_image().
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
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(2*A <= 3);
  ph.add_constraint(7*A >= 2);
  ph.add_constraint(3*B >= 1);
  ph.add_constraint(2*A >= B);

  print_generators(ph, "--- ph ---");

  ph.generalized_affine_image(B, LESS_THAN_OR_EQUAL, A-B+2, -3);

  C_Polyhedron known_result(2, EMPTY);
  known_result.add_generator(point(9*A - B, 6));
  known_result.add_generator(point(2*A - 4*B, 7));
  known_result.add_generator(ray(-B));

  int retval = (ph == known_result) ? 0 : 1;

  print_generators(ph, "--- ph after "
		   "ph.generalized_affine_image(B, LESS_THAN_OR_EQUAL,"
		   " A-B+2, -3) ---");

  return retval;
}
CATCH
