/* Test Polyhedron::BHRZ03_widening_assign().
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

#ifndef NOISY
#define NOISY 0
#endif

namespace {

bool
dimensions() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs1;
  gs1.insert(point());
  gs1.insert(point(A));
  gs1.insert(point(B));
  gs1.insert(point(A + B));
  gs1.insert(point(C));
  gs1.insert(point(A + C));
  gs1.insert(point(B + C));
  gs1.insert(point(A + B + C));
  C_Polyhedron ph1(gs1);

  C_Polyhedron ph1_copy(ph1);

  Generator_System gs2;
  gs2.insert(point(0*C));
  gs2.insert(point(A));
  gs2.insert(point(B));
  gs2.insert(point(A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph1.BHRZ03_widening_assign(ph2);

#if NOISY
  print_constraints(ph1, "*** After BHRZ03_widening_assign ***");
#endif

  return ph1 == ph1_copy;
}

bool
lines() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(A >= 0);

  C_Polyhedron ph1_copy(ph1);

  C_Polyhedron ph2(2);
  ph2.add_constraint(A >= 0);
  ph2.add_constraint(B >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
#endif

  ph1.BHRZ03_widening_assign(ph2);

#if NOISY
  print_constraints(ph1, "*** After BHRZ03_widening_assign ***");
#endif

  return ph1 == ph1_copy;
}

bool
points() {
  Variable A(0);
  Variable B(1);

  Generator_System gs1;
  gs1.insert(point());
  gs1.insert(point(2*A));
  gs1.insert(point(2*B));

  C_Polyhedron ph1(gs1);
  C_Polyhedron ph1_copy(ph1);

  Generator_System gs2;
  gs2.insert(point());
  gs2.insert(point(A));
  gs2.insert(point(B));
  gs2.insert(point(A + B));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph1.BHRZ03_widening_assign(ph2);

#if NOISY
  print_constraints(ph1, "*** After BHRZ03_widening_assign ***");
#endif

  return ph1 == ph1_copy;
}

} // namespace

int
main() TRY {
  set_handlers();

  if (!dimensions())
    return 1;
  if (!lines())
    return 1;
  if (!points())
    return 1;

  return 0;
}
CATCH
