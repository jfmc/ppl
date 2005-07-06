/* Test time_elapse_assign() on Polyhedra_Powerset.
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
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(2);

  ph1.add_constraint(x >= 0);
  ph1.add_constraint(y >= 0);
  ph1.add_constraint(x + y - 2 <= 0);

  C_Polyhedron ph2(2);

  ph2.add_constraint(x == -1);
  ph2.add_constraint(y == -1);

  Polyhedra_Powerset<C_Polyhedron> ps1(2, EMPTY);
  ps1.add_disjunct(ph1);
  ps1.add_disjunct(ph2);

  C_Polyhedron ph3(2);

  ph3.add_constraint(x >= 2);
  ph3.add_constraint(x <= 4);
  ph3.add_constraint(y == 3);

  Polyhedra_Powerset<C_Polyhedron> ps2(2, EMPTY);
  ps2.add_disjunct(ph3);

#if NOISY
  cout << "ps1 = " << ps1 << endl;
  cout << "ps2 = " << ps2 << endl;
#endif

  ps1.time_elapse_assign(ps2);

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(2*x));
  known_gs.insert(point(2*y));
  known_gs.insert(ray(2*x + 3*y));
  known_gs.insert(ray(4*x + 3*y));

  C_Polyhedron known_ph1(known_gs);

  known_gs.clear();
  known_gs.insert(point(-x - y));
  known_gs.insert(ray(2*x + 3*y));
  known_gs.insert(ray(4*x + 3*y));
  C_Polyhedron known_ph2(known_gs);

  Polyhedra_Powerset<C_Polyhedron> known_result(2, EMPTY);
  // Inserting out of order on purpose.
  known_result.add_disjunct(known_ph2);
  known_result.add_disjunct(known_ph1);

  int retval = (ps1 == known_result) ? 0 : 1;

#if NOISY
  cout << "ps1.time_elapse_assign(ps2) = " << ps1 << endl;
#endif

  return retval;
}
CATCH
