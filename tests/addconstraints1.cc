/* Adds the zero-dim inconsistent constraint to a polyhedron.
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

  // For const-correctness, making a copy is required.
  ConSys cs1 = ConSys::zero_dim_empty();
  C_Polyhedron ph1;
  ph1.add_constraints_and_minimize(cs1);

  ConSys cs2;
  cs2.insert(LinExpression::zero() >= 7);
  C_Polyhedron ph2;
  ph2.add_constraints_and_minimize(cs2);

  ConSys cs3;
  cs3.insert(LinExpression::zero() >= -3);
  C_Polyhedron ph3;
  ph3.add_constraints_and_minimize(cs3);

  C_Polyhedron empty_result(0, C_Polyhedron::EMPTY);
  C_Polyhedron univ_result;

  int retval = (ph1 == empty_result
		&& ph2 == empty_result
		&& ph3 == univ_result) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(ph3, "*** ph3 ***");
  print_constraints(empty_result, "*** empty_result ***");
  print_constraints(univ_result, "*** univ_result ***");
#endif

  return retval;
}
CATCH
