/* Test Octagon::intersection_assign().
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
  Variable x(0);
  Variable y(1);

  TOctagon oc1(3);
  TOctagon oc2(3);
  TOctagon known_result(3);

  oc1.add_constraint(x <= 3);
  oc1.add_constraint(x - y <= 4);
  oc1.add_constraint(x + y <= 6);


  oc2.add_constraint(-y <= -2);
  oc2.add_constraint(x - y <= 5);
  oc2.add_constraint(x + y <= 7);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  if (oc1.intersection_assign_and_minimize(oc2)) {
#if NOISY
    print_constraints(oc1, "*** oc1.intersection_assign(oc2) ***");
#endif
  }

  Constraint_System cs;
  cs.insert(x <= 3);
  cs.insert(-y <= -2);
  cs.insert(x - y <= 4);
  cs.insert(x + y <= 6);

  known_result.add_constraints_and_minimize(cs);


  int retval = (oc1 == known_result) ? 0 : 1;

  return retval;

}
CATCH
