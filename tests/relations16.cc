/* Test Polyhedron::relation_with(c): in this test `c' is
   a strict inequality.
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

  GenSys gs;
  gs.insert(point(A));
  gs.insert(line(B));
  C_Polyhedron ph(gs);

  Poly_Con_Relation rel = ph.relation_with(B > 0);

#if NOISY
  print_generators(ph, "*** ph ***");
  cout << "ph.relation_with(B > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();
  return (rel == known_result) ? 0 : 1;
}
CATCH
