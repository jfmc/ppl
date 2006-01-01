/* Different ways of creating an empty BD_Shape.
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

int

main() TRY {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);
  // Variable x6(5);

  TBD_Shape bd1(6);
  TBD_Shape bd2(6);

  bd1.add_constraint(x1 <= 3);
  bd1.add_constraint(x4 <= 3);
  bd1.add_constraint(x2 - x1 <= 0);
  bd1.add_constraint(x3 - x1 <= -2);
  bd1.add_constraint(x5 - x1 <= 2);
  bd1.add_constraint(-x2 <= 0);
  bd1.add_constraint(x3 - x2 <= 5);
  bd1.add_constraint(x4 - x3 <= -6);
  bd1.add_constraint(x1 - x4 <= 5);
  bd1.add_constraint(x5 - x4 <= 2);
  bd1.add_constraint(-x5 <= -5);
  bd1.add_constraint(x3 - x5 <= 7);

  bool empty = bd1.is_empty();

  nout << "*** bd1.is_empty() ***" << endl;
  nout << (empty ? "true" : "false") << endl;

  bd2.add_constraint(x1 <= 3);
  bd2.add_constraint(x4 <= 3);
  bd2.add_constraint(x2 - x1 <= 0);
  bd2.add_constraint(x3 - x1 <= 2);
  bd2.add_constraint(x5 - x1 <= 2);
  bd2.add_constraint(-x2 <= 0);
  bd2.add_constraint(x3 - x2 <= 5);
  bd2.add_constraint(x4 - x3 <= 6);
  bd2.add_constraint(x1 - x4 <= 5);
  bd2.add_constraint(x5 - x4 <= 2);
  bd2.add_constraint(-x5 <= 5);
  bd2.add_constraint(x3 - x5 <= 7);

  bool empty1 = bd2.is_empty();

  nout << "*** bd2.is_empty() ***" << endl;
  nout << (empty1 ? "true" : "false") << endl;

  return (!empty1 && empty) ? 0 : 1;

}
CATCH
