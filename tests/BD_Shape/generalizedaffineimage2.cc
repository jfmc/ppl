/* Test BD_Shape::generalized_affine_image(): if the relation is
   `==' the resulting bdiff is equal to the bdiff obtained
   with BD_Shape::affine_image().
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
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(B >= 0);
  bd.add_constraint(A - B >= 0);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  TBD_Shape known_result(bd);

  bd.generalized_affine_image(A, EQUAL, A + 2);

  known_result.affine_image(A, A + 2);

  int retval = (bd == known_result) ? 0 : 1;

#if NOISY
  print_constraints(bd, "*** bd.generalized_affine_image"
		    "(A, EQUAL, A + 2) ***");
#endif

  return retval;
}
CATCH
