/* Test Polyhedron::relation_with(g) and Polyhedron::relation_with(c):
   the polyhedron can have something pending.
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
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

static void
test1() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.generators();
  ph.add_constraint(A >= 2);
  ph.add_constraint(B == 0);

  Poly_Gen_Relation rel = ph.relation_with(ray(A + B));

  Poly_Gen_Relation known_rel = Poly_Gen_Relation::nothing();

  bool ok = (rel == known_rel);

#if NOISY
  print_constraints(ph, "*** ph ***");
  cout << "ph.relation_with(ray(A + B)) == " << rel << endl;
#endif

  if (!ok)
    exit(1);
}

static void
test2() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);
  ph.add_generator(point());
  ph.constraints();
  ph.add_generator(ray(A));
  ph.add_generator(ray(B));

  Poly_Con_Relation rel = ph.relation_with(A == 0);

  Poly_Con_Relation known_rel = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_rel);

#if NOISY
  print_constraints(ph, "*** ph ***");
  cout << "ph.relation_with(A == 0) == " << rel << endl;
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
