/* Testing Polyhedron::relation_with(c): we apply this function to
   a zero-dimensional, universal polyhedron.
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

  Relation_Poly_Con rel;

  Polyhedron ph;
#if NOISY
  print_generators(ph, "--- ph ---");
#endif

  // A false inequality constraint.
  Constraint c_false1(LinExpression(-1) >= 0);
#if NOISY
  print_constraint(c_false1, "--- c_false1 ---");
#endif

  rel = ph.relation_with(c_false1);
#if NOISY
  cout << "ph.relation_with(c_false1) == " << rel << endl;
#endif

  if (rel != IS_DISJOINT)
    return 1;

  // A false equality constraint.
  Constraint c_false2(LinExpression(5) == -2);
#if NOISY
  print_constraint(c_false2, "--- c_false2 ---");
#endif

  rel = ph.relation_with(c_false2);
#if NOISY
  cout << "ph.relation_with(c_false2) == " << rel << endl;
#endif

  if (rel != IS_DISJOINT)
    return 1;

  // A saturated inequality.
  Constraint c_saturated1(LinExpression(3) >= 3);
#if NOISY
  print_constraint(c_saturated1, "--- c_saturated1 ---");
#endif

  rel = ph.relation_with(c_saturated1);
#if NOISY
  cout << "ph.relation_with(c_saturated1) == " << rel << endl;
#endif

  if (rel != SATURATES)
    return 1;

  // A saturated equality.
  Constraint c_saturated2(LinExpression(1) == 1);
#if NOISY
  print_constraint(c_saturated2, "--- c_saturated2 ---");
#endif

  rel = ph.relation_with(c_saturated2);
#if NOISY
  cout << "ph.relation_with(c_saturated2) == " << rel << endl;
#endif

  if (rel != SATURATES)
    return 1;

  // A satisfied inequality which is not saturated.
  Constraint c_satisfied(LinExpression(7) >= 5);
#if NOISY
  print_constraint(c_satisfied, "--- c_satisfied ---");
#endif

  rel = ph.relation_with(c_satisfied);
#if NOISY
  cout << "ph.relation_with(c_satisfied) == " << rel << endl;
#endif

  if (rel != IS_INCLUDED)
    return 1;

  // All test passed.
  return 0;
}
