/* Test C_Polyhedron::C_Polyhedron(const NNC_Polyhedron& y).
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

void
test1() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph1(3);
  ph1.add_constraint(x >= 5);
  ph1.add_constraint(y > x - 3);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  C_Polyhedron ph2(ph1);

#if NOISY
  print_constraints(ph2, "*** ph2 ***");
#endif

  C_Polyhedron known_result(3);
  known_result.add_constraint(x >= 5);
  known_result.add_constraint(y >= x - 3);

  if (ph2 != known_result)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A < 2);
  ph1.add_constraint(B > 0);
  ph1.add_constraint(A - B > 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
#endif

  C_Polyhedron ph2(ph1);

#if NOISY
  print_constraints(ph2, "*** ph2 ***");
#endif

  C_Polyhedron known_result(2);
  known_result.add_constraint(A <= 2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(A - B >= 0);

  if (ph2 != known_result)
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();

  return 0;
}
CATCH
