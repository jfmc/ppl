/* Test Polyhedron::relation_with(c): in this test `c' is an equality.
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
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);

  GenSys gs1;
  gs1.insert(point());
  gs1.insert(line(A + B));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(ray(A));
  gs2.insert(point(B));
  gs2.insert(point(-B));
  C_Polyhedron ph2(gs2);

  Poly_Con_Relation rel1 = ph1.relation_with(A == 0);
  Poly_Con_Relation rel2 = ph2.relation_with(A == 0);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
  cout << "ph1.relation_with(A == 0) == " << rel1 << endl;
  cout << "ph2.relation_with(A == 0) == " << rel2 << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();
  return (rel1 == known_result && rel2 == known_result) ? 0 : 1;
}
CATCH
