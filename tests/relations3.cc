/* Testing C_Polyhedron::relation_with(g): we verify that a zero-dimensional
   generator is subsumed by a zero-dimensional, universal polyhedron.
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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  set_handlers();

  C_Polyhedron ph;

#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  Generator g = point();

#if NOISY
  print_generator(g, "--- g ---");
#endif

  Poly_Gen_Relation rel = ph.relation_with(g);

  Poly_Gen_Relation known_rel = Poly_Gen_Relation::subsumes();
  int retval = (rel == known_rel) ? 0 : 1;

#if NOISY
  cout << "ph.relation_with(v()) == " << rel << endl;
#endif
  return retval;

}

