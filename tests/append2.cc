/* An example of iteration to a post-fixpoint.
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

#ifndef NOISY
#define NOISY 0
#endif

static void
shift_rename_add(const C_Polyhedron& p,
		 dimension_type offset,
		 C_Polyhedron& q) {
  C_Polyhedron r(offset);
  r.concatenate_assign(p);
  q.intersection_assign(r);
}

static void
append_init(C_Polyhedron& base, C_Polyhedron& induct, C_Polyhedron& expect,
            dimension_type& offset, unsigned int& arity,
	    unsigned int& num_vars) {
  offset = 3;
  arity = 3;
  num_vars = 6;
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);

  // This is the base case:
  // append(A,B,C) :- A = [], B = C.
  base.add_dimensions_and_embed(3);
  base.add_constraint(A == 0);
  base.add_constraint(B >= 0);
  base.add_constraint(C == B);
#if NOISY
  print_constraints(base, "*** base ***");
#endif

  // This is the inductive case:
  // append(A,B,C) :- A = [X|D], B = E, C = [X|F], append(D,E,F).
  induct.add_dimensions_and_embed(6);
  induct.add_constraint(A + F == C + D);
  induct.add_constraint(B == E);
  induct.add_constraint(C + D >= A);
  induct.add_constraint(D >= 0);
  induct.add_constraint(B >= 0);
  induct.add_constraint(A >= D + 1);
#if NOISY
  print_constraints(induct, "*** inductive ***");
#endif

  expect.add_dimensions_and_embed(3);
  expect.add_constraint(A + B == C);
  expect.add_constraint(B >= 0);
  expect.add_constraint(C >= B);
}

static void
fix_point(C_Polyhedron& start, C_Polyhedron& induct, C_Polyhedron& finish,
          dimension_type offset, unsigned int arity, unsigned int num_vars) {
  // Initialize the fixpoint iteration.
  C_Polyhedron current = start;
#if NOISY
  print_constraints(current, "*** start ***");
#endif

  // Contains the polyhedron computed at the previous iteration.
  C_Polyhedron previous;
  do {
    previous = current;
    current = induct;
    shift_rename_add(previous, offset, current);
#if NOISY
    print_constraints(current, "*** after shift_rename_add ***");
#endif

    Variables_Set dimensions_to_remove;
    for (unsigned int i = num_vars-1 ; i >= arity; --i )
      dimensions_to_remove.insert(Variable(i));
    current.remove_dimensions(dimensions_to_remove);

#if NOISY
    print_constraints(current, "*** after remove_dimensions ***");
#endif
    current.poly_hull_assign_and_minimize(previous);
#if NOISY
    print_constraints(current, "*** after poly_hull_assign_and_minimize***");
#endif
  } while (current != previous);
  finish = current;
}

int
main() {
  set_handlers();

  C_Polyhedron start;
  C_Polyhedron induct;
  C_Polyhedron expect;
  dimension_type recursive_offset;
  unsigned int arity;
  unsigned int num_vars;
  append_init(start, induct, expect, recursive_offset, arity, num_vars);
  C_Polyhedron final;
  fix_point(start, induct, final, recursive_offset, arity, num_vars);

#if NOISY
    print_constraints(expect, "*** expected ***");
#endif
  return final == expect ? 0 : 1;
}
