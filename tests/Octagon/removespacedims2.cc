/* Remove some variables from the space.
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
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  Variable x6(5);

  TOctagon oc1(6);

  oc1.add_constraint(x2 - x3 <= 0);
  oc1.add_constraint(x3 <= 2);
  oc1.add_constraint(x5 <= 3);


#if NOISY
  print_constraints(oc1, "*** oc1 ***");
#endif


  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(x3);
  to_be_removed.insert(x5);
  oc1.remove_space_dimensions(to_be_removed);


  Octagon<mpq_class> known_result(4);

  known_result.add_constraint(x2 <= 2);

#if NOISY
  print_constraints(oc1, "*** oc1.remove_space_dimensions({x3,x5}) ***");
  print_constraints(known_result, "*** known_result ***");
#endif

  int retval = (oc1 == known_result) ? 0 : 1;


  return retval;
}
CATCH
