/* Different ways of creating an empty BDiffs.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
  Variable z(2);

  TBD_Shape bd1(4);
  TBD_Shape bd2(4);


  bd1.add_constraint(-x <= 2);
  bd1.add_constraint(y - x <= -9);
  bd1.add_constraint(x - y <= -7);

  bool empty = bd1.is_empty();

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  cout << "*** bd1.is_empty() ***" << endl;
  cout << (empty ? "true" : "false") << endl;
#endif


  bd2.add_constraint(-x <= 7);
  bd2.add_constraint(y - x <= 1);
  bd2.add_constraint(-y <= 2);
  bd2.add_constraint(z - x <= 1);

  bool empty1 = bd2.is_empty();

#if NOISY
  print_constraints(bd2, "*** bd2 ***");
  cout << "*** bd2.is_empty() ***" << endl;
  cout << (empty1 ? "true" : "false") << endl;
#endif

  return (!empty1 && empty) ? 0 : 1;

}
CATCH
