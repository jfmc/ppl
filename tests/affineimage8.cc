/* Test Polyhedron::affine_image(): we apply this function to a
   polyhedron that can have something pendig.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

static void
test1() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 0);
  ph.add_constraint(B >= 0);
  C_Polyhedron copy_ph(ph);

#if 0
  print_constraints(ph, "*** ph ***");
#endif

  ph.affine_image(A, A + 1);
  copy_ph.affine_image(A, -A - 1, -1);

  bool ok = (ph == copy_ph);

#if 0
  print_generators(ph, "*** After ph.affine_image(A, A + 1) ***");
  print_generators(copy_ph,
		   "*** After copy_ph.affine_image(A, -A - 1, -1) ***");
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 0);
  ph.add_constraint(B >= 0);
  C_Polyhedron copy_ph(ph);

#if 0
  print_constraints(ph, "*** ph ***");
#endif

  ph.affine_image(B, A + 1);
  copy_ph.affine_image(B, -A - 1, -1);

  bool ok = (ph == copy_ph);

#if 0
  print_generators(ph, "*** After ph.affine_image(B, A + 1) ***");
  print_generators(copy_ph,
		   "*** After copy_ph.affine_image(B, -A - 1, -1) ***");
#endif

  if (!ok)
    exit(1);
}

int
main() {
  set_handlers();
  
  test1();
  test2();
  
  return 0;
}
