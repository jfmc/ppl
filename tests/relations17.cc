/* Test Polyhedron::relation_with(c).
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

void
test1() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1(2, NNC_Polyhedron::EMPTY);
  ph1.add_generator(point(A + B));
  
  Poly_Con_Relation rel = ph1.relation_with(A - B == 0);
  Poly_Con_Relation  known_rel = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  bool ok = (rel == known_rel);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  cout << "ph1.relation_with(A - B == 0) = " << rel << endl;
#endif
  
  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  GenSys gs;
  gs.insert(point());
  gs.insert(ray(A));
  gs.insert(line(B));

  NNC_Polyhedron ph1(gs);
  
  Poly_Con_Relation rel = ph1.relation_with(A >= 1);
  Poly_Con_Relation  known_rel = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_rel);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  cout << "ph1.relation_with(A >= 1) = " << rel << endl;
#endif
  
  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);

  GenSys gs;
  gs.insert(line(A));
  gs.insert(ray(B));
  gs.insert(point());
  NNC_Polyhedron ph1(gs);

  Poly_Con_Relation rel = ph1.relation_with(A > 1);
  Poly_Con_Relation  known_rel = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_rel);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  cout << "ph1.relation_with(A > 1) = " << rel << endl;
#endif
  
  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);

  GenSys gs;
  gs.insert(line(B));
  gs.insert(point());
  gs.insert(ray(A));
  C_Polyhedron ph1(gs);

  Poly_Con_Relation rel = ph1.relation_with(B >= 1);
  Poly_Con_Relation  known_rel = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_rel);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  cout << "ph1.relation_with(B >= 1) = " << rel << endl;
#endif
  
  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);

  GenSys gs;
  gs.insert(point(A));
  gs.insert(point(A + B));
  C_Polyhedron ph1(gs);

  Poly_Con_Relation rel = ph1.relation_with(B == 1);
  Poly_Con_Relation  known_rel = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_rel);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  cout << "ph1.relation_with(B == 1) = " << rel << endl;
#endif
  
  if (!ok)
    exit(1);
}

int
main() {
  set_handlers();

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
