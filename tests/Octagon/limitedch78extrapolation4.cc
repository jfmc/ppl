/* Test Octagon::limited_CH78_extrapolation_assign().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  Variable A(0);
  Variable B(2);
  Variable C(3);

  TOctagon oct1(3, EMPTY);

#if NOISY
  print_constraints(oct1, "*** oct1 ****");
#endif

  TOctagon oct2(3, EMPTY);

#if NOISY
  print_constraints(oct2, "*** oct2 ****");
#endif

  Constraint_System cs;
  cs.insert(A + B <= 0);

#if NOISY
  print_constraints(cs, "*** cs ****");
#endif

  oct1.limited_CH78_extrapolation_assign(oct2, cs);

  TOctagon known_result(3, EMPTY);

#if NOISY
  print_constraints(oct1,
		    "*** oct1.limited_CH78_extrapolation_assign(oct2) ***");
#endif

  return oct1 == known_result ? 0 : 1;
}
CATCH
