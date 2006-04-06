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
  TOctagon oct1(0, EMPTY);

#if NOISY
  print_constraints(oct1, "*** oct1 ****");
#endif

  TOctagon oct2(0, EMPTY);

#if NOISY
  print_constraints(oct2, "*** oct2 ****");
#endif

  Constraint_System cs;

#if NOISY
  print_constraints(cs, "*** cs ****");
#endif

  TOctagon computed_result = oct1;
  computed_result.limited_CH78_extrapolation_assign(oct2, cs);

  TOctagon known_result(0, EMPTY);

#if NOISY
  print_constraints(computed_result,
		    "*** oct1.limited_CH78_extrapolation_assign(oct2) ***");
#endif

  return (computed_result == known_result) ? 0 : 1;
}
CATCH
