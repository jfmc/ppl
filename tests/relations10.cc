/* Test Polyhedron::relation_with(g): the system of constraints
   of the polyhedron contains only an equality and the generator
   `g' is a point.
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

int
main() {
  set_handlers();

  Variable A(0);
  
  C_Polyhedron ph(2);
  ph.add_constraint(A == 0);

  Poly_Gen_Relation rel = ph.relation_with(point(2*A));

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(point(2*A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();

  return (rel == known_result) ? 0 : 1;
}
