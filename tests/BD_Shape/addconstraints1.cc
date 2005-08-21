/* Test BD_Shape::add_constraints_and_minimize().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(2);
  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(B == 5);
  bd1.add_constraints_and_minimize(cs);

#if NOISY
  print_constraints(bd1, "*** bd1.add_constraints_and_minimize(cs) ***");
#endif

  TBD_Shape known_result(2);
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(B == 5);
  known_result.add_constraint(B - A <= 5);

  int retval = (bd1 == known_result) ? 0 : 1;

#if NOISY
  print_constraints(known_result, "*** known_result ***");
#endif

  return retval;
}
CATCH
