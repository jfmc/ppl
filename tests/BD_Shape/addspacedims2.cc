/* Test BD_Shape::add_space_dimensions_and_project(): 
   we add two variables to a BD_Shape.
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
  Variable x(0);
  //Variable y(1);
  Variable z(2);
  Variable w(3);

  TBD_Shape bd(2);

  bd.add_constraint(x <= 2);

#if NOISY
  print_constraints(bd, "*** bd ***");
#endif

  bd.add_space_dimensions_and_project(2);

  TBD_Shape known_result(4);
  known_result.add_constraint(x <= 2);
  known_result.add_constraint(w == 0);
  known_result.add_constraint(z == 0);

#if NOISY
  print_constraints(bd, "*** bd.add_space_dimensions_and_project(2) ***");
#endif

  int retval = (bd == known_result) ? 0 : 1;

  return retval;

}
CATCH
