/* An example of iteration to a post-fixpoint.
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

#define NOISY 0

void
shift_rename_add(const C_Polyhedron& p, size_t offset, C_Polyhedron& q) {
  if (p.space_dimension() == 0)
    exit(1);

  if (p.check_empty())
    exit(1);

  const ConSys& cs = p.constraints();
  for (ConSys::const_iterator
	 i = cs.begin(), cs_end = cs.end(); i != cs_end; ++i)
    if (offset > 0)
      q.add_constraint(*i >> offset);
    else
      q.add_constraint(*i);
}


int
main() {
  set_handlers();

  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);

  // This is the base case:
  // append(A,B,C) :- A = [], B = C.
  C_Polyhedron base(3);
  base.add_constraint(A == 0);
  base.add_constraint(B >= 0);
  base.add_constraint(C == B);
#if NOISY
  print_constraints(base, "*** base ***");
#endif

  // This is the inductive case:
  // append(A,B,C) :- A = [X|D], B = E, C = [X|F], append(D,E,F).
  C_Polyhedron inductive(6);
  inductive.add_constraint(A + F == C + D);
  inductive.add_constraint(B == E);
  inductive.add_constraint(C + D >= A);
  inductive.add_constraint(D >= 0);
  inductive.add_constraint(B >= 0);
  inductive.add_constraint(A >= D + 1);
#if NOISY
  print_constraints(inductive, "*** inductive ***");
#endif

  // Initialize the fixpoint iteration.
  C_Polyhedron current = base;
#if NOISY
  print_constraints(current, "*** start ***");
#endif

  // Contains the polyhedron computed at the previous iteration.
  C_Polyhedron previous;
  do {
    previous = current;
    current = inductive;
    shift_rename_add(previous, 3, current);
#if NOISY
    print_constraints(current, "*** after shift_rename_add ***");
#endif
    set<Variable> dimensions_to_remove;
    // Deliberately inserted out of order (!).
    dimensions_to_remove.insert(D);
    dimensions_to_remove.insert(F);
    dimensions_to_remove.insert(E);
    assert(current.OK());
    current.remove_dimensions(dimensions_to_remove);
    assert(current.OK());
#if NOISY
    print_constraints(current, "*** after remove_dimensions ***");
#endif
    current.poly_hull_assign_and_minimize(previous);
#if NOISY
    print_constraints(current, "*** after poly_hull_assign_and_minimize***");
#endif
  } while (current != previous);

  C_Polyhedron expected(3);
  expected.add_constraint(A + B == C);
  expected.add_constraint(B >= 0);
  expected.add_constraint(C >= B);
#if NOISY
    print_constraints(expected, "*** expected ***");
#endif

  return current == expected ? 0 : 1;
}
