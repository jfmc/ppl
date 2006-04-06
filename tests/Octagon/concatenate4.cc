/* Test Octagon::concatenate_assign().
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

#ifndef NOISY
#define NOIY 0
#endif

int
main() TRY {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);
  Variable x7(6);
  Variable x8(7);
  Variable x9(8);


  TOctagon oc1(6);
  TOctagon oc2(3);


  oc1.add_constraint(x2 + x3 <= 1);
  oc1.add_constraint(x3 <= 2);
  oc1.add_constraint(x6 - x5 <= 2);
  oc1.add_constraint(x5 <= 3);

  oc2.add_constraint(x2 + x3 <= 77);
  oc2.add_constraint(x3 <= 7);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  oc1.concatenate_assign(oc2);

  TOctagon known_result(9);

  known_result.add_constraint(x2 + x3 <= 1);
  known_result.add_constraint(x3 <= 2);
  known_result.add_constraint(x6 - x5 <= 2);
  known_result.add_constraint(x5 <= 3);
  known_result.add_constraint(x8 + x9 <= 77);
  known_result.add_constraint(x9 <= 7);


#if NOISY
  print_constraints(oc1, "*** oc1.concatenate_assign(oc2) ***");
#endif

  int retval = (oc1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
