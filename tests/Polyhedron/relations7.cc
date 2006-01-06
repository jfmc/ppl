/* Test Polyhedron::relation_with(c): we apply this function to
   a zero-dimensional, universal polyhedron.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

  Poly_Con_Relation rel = Poly_Con_Relation::nothing();
  Poly_Con_Relation known_result = Poly_Con_Relation::nothing();

  C_Polyhedron ph;
  print_generators(ph, "--- ph ---");

  // A false inequality constraint.
  Constraint c_false1(Linear_Expression(-1) >= 0);
  print_constraint(c_false1, "--- c_false1 ---");

  rel = ph.relation_with(c_false1);

  nout << "ph.relation_with(c_false1) == " << rel << endl;

  known_result = Poly_Con_Relation::is_disjoint();
  if (rel != known_result)
    return 1;

  // A false equality constraint.
  Constraint c_false2(Linear_Expression(5) == -2);
  print_constraint(c_false2, "--- c_false2 ---");

  rel = ph.relation_with(c_false2);

  nout << "ph.relation_with(c_false2) == " << rel << endl;

  known_result = Poly_Con_Relation::is_disjoint();
  if (rel != known_result)
    return 1;

  // A saturated inequality.
  Constraint c_saturated1(Linear_Expression(3) >= 3);
  print_constraint(c_saturated1, "--- c_saturated1 ---");

  rel = ph.relation_with(c_saturated1);

  nout << "ph.relation_with(c_saturated1) == " << rel << endl;

  known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();
  if (rel != known_result)
    return 1;

  // A saturated equality.
  Constraint c_saturated2(Linear_Expression(1) == 1);
  print_constraint(c_saturated2, "--- c_saturated2 ---");

  rel = ph.relation_with(c_saturated2);

  nout << "ph.relation_with(c_saturated2) == " << rel << endl;

  known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();
  if (rel != known_result)
    return 1;

  // A satisfied inequality which is not saturated.
  Constraint c_satisfied(Linear_Expression(7) >= 5);
  print_constraint(c_satisfied, "--- c_satisfied ---");

  rel = ph.relation_with(c_satisfied);

  nout << "ph.relation_with(c_satisfied) == " << rel << endl;

  known_result = Poly_Con_Relation::is_included();
  if (rel != known_result)
    return 1;

  // All tests passed.
  return 0;
}
CATCH
