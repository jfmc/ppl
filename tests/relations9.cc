/* Testing Polyhedron::relation_with(c).
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

  Variable x(0);
  Polyhedron ph(1);
  ph.insert(x >= 0);

#if NOISY
  print_generators(ph, "--- ph ---");
#endif

  // A point on the positive side.
  Generator pg(point(1*x));
  // A point on the negative side.
  Generator ng(point(-1*x));

#if NOISY
  print_generator(pg, "--- pg ---");
  print_generator(ng, "--- ng ---");
#endif

  Poly_Gen_Relation rel_with_pg = ph.relation_with(pg);
  Poly_Gen_Relation rel_with_ng = ph.relation_with(ng);
#if NOISY
  cout << "ph.relation_with(y == -1) == " << rel_with_pg << endl;
  cout << "ph.relation_with(y == -1) == " << rel_with_ng << endl;
#endif

  return (rel_with_pg.implies(Poly_Gen_Relation::subsumes())
	  && !rel_with_ng.implies(Poly_Gen_Relation::subsumes())) ? 0 : 1;
}
