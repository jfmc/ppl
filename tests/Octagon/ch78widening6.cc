/* Test Octagon::CH78_widening_assign().
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
  Variable B(1);
  Variable C(2);
  Variable D(3);


  TOctagon oc1(4);
  TOctagon oc2(4);
  TOctagon known_result(4);

  oc1.add_constraint(B <= 1);
  oc1.add_constraint(C <= 7);
  oc1.add_constraint(D >= 0);
  oc1.add_constraint(A + B <= 10);
  oc1.add_constraint(C - A <= 10);
  oc1.add_constraint(D - A <= 10);
  oc1.add_constraint(B - C <= 0);
  oc1.add_constraint(C - B <= 1);
  oc1.add_constraint(B - D <= 0);


  oc2.add_constraint(B <= 1);
  oc2.add_constraint(C <= 7);
  oc2.add_constraint(D >= 0);
  oc2.add_constraint(A + B <= 9);
  oc2.add_constraint(C - A <= 9);
  oc2.add_constraint(D - A <= 10);
  oc2.add_constraint(B - C <= 0);
  oc2.add_constraint(C - B <= 1);
  oc2.add_constraint(B - D <= 0);


#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  oc1.CH78_widening_assign(oc2);

  known_result.add_constraint(B <= 1);
  known_result.add_constraint(C <= 7);
  known_result.add_constraint(D >= 0);
  known_result.add_constraint(D - A <= 10);
  known_result.add_constraint(B - C <= 0);
  known_result.add_constraint(C - B <= 1);
  known_result.add_constraint(B - D <= 0);


#if NOISY
  print_constraints(oc1, "*** oc1.CH78_widening_assign(oc2) ***");
#endif

  int retval = (oc1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
