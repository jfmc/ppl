/* Test Polyhedron::is_bounded().
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

  // This is a non-bounded polyhedron.
  C_Polyhedron ph1(2);
  ph1.add_constraint(x >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  if (ph1.is_bounded())
    return 1;

  // This is a bounded polyhedron (it is a square);
  C_Polyhedron ph2(2);
  ph2.add_constraint(x >= 2);
  ph2.add_constraint(y >= 2);
  ph2.add_constraint(x <= 4);
  ph2.add_constraint(y <= 4);

#if NOISY
  print_constraints(ph2, "*** ph2 ***");
#endif

  if (!ph2.is_bounded())
    return 1;

  // This is a universal, zero-dimensional polyhedron.
  C_Polyhedron ph3;

#if NOISY
  print_constraints(ph3, "*** ph3 ***");
#endif

  if (!ph3.is_bounded())
    return 1;

  // This is an empty, zero-dimensional polyhedron.
  C_Polyhedron ph4;
  ph4.add_constraint(LinExpression(-3) >= 0);

#if NOISY
  print_constraints(ph4, "*** ph4 ***");
#endif

  if (!ph4.is_bounded())
    return 1;

  // This is an empty polyhedron.
  C_Polyhedron ph5(4, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph5, "*** ph5 ***");
#endif

  if (!ph5.is_bounded())
    return 1;

  return 0;
}
CATCH
