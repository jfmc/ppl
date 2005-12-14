/* Test Polyhedron::relation_with(c).
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

using namespace Parma_Polyhedra_Library::IO_Operators;

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);

  Constraint_System cs;
  cs.insert(x + y >= 1);
  cs.insert(y >= 5);

  C_Polyhedron ph(cs);

  print_generators(ph, "--- ph ---");

  // An equality constraint non-intersecting the polyhedron.
  Constraint c(y == -1);

  print_constraint(c, "--- c ---");

  Poly_Con_Relation rel = ph.relation_with(c);

  nout << "ph.relation_with(c) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();
  return (rel == known_result) ? 0 : 1;
}
CATCH
