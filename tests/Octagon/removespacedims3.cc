/* Remove the higher variables from the space.
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

  TOctagon oct1(6);

  oct1.add_constraint(x1 >= 1);
  oct1.add_constraint(x1 + x3 >= 2);
  oct1.add_constraint(x2 - x3 <= 4);
  oct1.add_constraint(x4 - x1  >= 0);
  oct1.add_constraint(x6 <= 7);
  oct1.add_constraint(x5 + x4 >= 1);


#if NOISY
  print_constraints(oct1, "*** oct1 ***");
#endif

  oct1.remove_higher_space_dimensions(3);

  Octagon<mpq_class> known_result(3);

  known_result.add_constraint(x1 >= 1);
  known_result.add_constraint(x1 + x3 >= 2);
  known_result.add_constraint(x2 - x3 <= 4);

#if NOISY
  print_constraints(oct1, "*** oct1.remove_higher_space_dimensions(3) ***");
#endif

  int retval = (oct1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
