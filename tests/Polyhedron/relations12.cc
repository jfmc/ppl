/* Test Polyhedron::relation_with(c): we apply this function
   to a bounded polyhedron and with an equality.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

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

  Generator_System gs;
  gs.insert(point(A + 0*B));
  gs.insert(point(3*A));
  C_Polyhedron ph(gs);

#if NOISY
  print_generators(ph, "*** ph ***");
#endif

  Poly_Con_Relation rel = ph.relation_with(B == 0);

  Poly_Con_Relation known_rel = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  int retval = (rel == known_rel) ? 0 : 1;

#if NOISY
  cout << "ph.relation_with(B == 0) == " << rel << endl;
#endif
  return retval;
}
CATCH
