/* Test Polyhedron::generalized_affine_image() with a linear expression
   as the left hand side.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);
  ph.add_generator(point(A + B));
  ph.add_generator(point(3*A + B));
  ph.add_generator(point(A + 3*B));
  ph.add_generator(point(3*A + 3*B));

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  C_Polyhedron known_result(2);
  known_result.add_constraint(A + B >= 1);

  ph.generalized_affine_image(A + B, PPL_GE, 2*A - B + 2);

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  print_generators(ph,
		   "*** After ph.generalized_affine_image"
		   "(A + B, PPL_GE, 2*A - B + 2) ***");
#endif

  return retval;
}
CATCH
