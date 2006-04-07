/* Test Octagon::CC76_extrapolation_assign().
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
  typedef TOctagon::base_type bt;
  bt stop_points[] = { bt(-2), bt(-1), bt(0), bt(1), bt(2) };

  Variable x(0);
  Variable y(1);

  TOctagon oct1(2);
  TOctagon oct2(2);
  Octagon<mpq_class> known_result(2);

  oct1.add_constraint(x <= 1);
  oct1.add_constraint(x - y <= 2);
  oct1.add_constraint(y - x <= 7);

  oct2.add_constraint(x - y <= 2);
  oct2.add_constraint(-x <= 3);
  oct2.add_constraint(x <= 0);
  oct2.add_constraint(y - x <= 2);

#if NOISY
  print_constraints(oct1, "*** oct1 ***");
  print_constraints(oct2, "*** oct2 ***");
#endif

  oct1.CC76_extrapolation_assign(oct2,
				 stop_points,
				 stop_points
				 + sizeof(stop_points)/sizeof(stop_points[0]));

  known_result.add_constraint(x <= 1);
  known_result.add_constraint(x - y <= 2);

#if NOISY
  print_constraints(oct1, "*** oct1.CC76_extrapolation_assign(oct2) ***");
#endif

  int retval = (oct1 == known_result) ? 0 : 1;

  return retval;
}
CATCH
