/* Test BD_Shape::generalized_affine_image().
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
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(A <= 4);
  bd.add_constraint(B <= 5);
  bd.add_constraint(A <= B);

  print_constraints(bd, "*** bd ***");

  bd.generalized_affine_image(B, GREATER_THAN_OR_EQUAL, A+2);

  BD_Shape<mpq_class> known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(A <= 4);
  known_result.add_constraint(B - A >= 2);

  int retval = (BD_Shape<mpq_class>(bd) == known_result) ? 0 : 1;

  print_constraints(bd, "*** bd.generalized_affine_image"
		        "(B, GREATER_THAN_OR_EQUAL, A+2) ***");

  return retval;
}
CATCH
