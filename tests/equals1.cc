/* Test operator==(const Polyhedron&, const Polyhedron&).
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

int
main() try {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(100*A-100*B <= 0);
  ph1.add_constraint(-100*A <= -300);
  ph1.add_constraint(-100*B <= -300);
  ph1.add_constraint(100*A <= 30400);
  ph1.add_constraint(100*B <= 30400);
  ph1.add_constraint(-258*A+209*B <= 7093);
  ph1.add_constraint(258*A-209*B <= 8907);
  ph1.add_constraint(151*A-214*B <= 5393);
  ph1.add_constraint(-151*A+214*B <= 10606);

  C_Polyhedron ph2(2);
  ph2.add_constraint(258*A - 209*B >= -7093);
  ph2.add_constraint(151*A - 214*B >= -10606);
  ph2.add_constraint(A >= 3);
  ph2.add_constraint(-A + B >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif

  bool ok = (ph1 == ph2);

  return ok ? 0 : 1;
}
CATCH
