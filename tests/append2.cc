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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

void
shift_rename_insert(const Polyhedron& p, size_t offset, Polyhedron& q) {
  if (p.space_dimension() == 0)
    exit(1);

  if (p.check_empty())
    exit(1);

  const ConSys& cs = p.constraints();
  for (ConSys::const_iterator
	 i = cs.begin(), cs_end = cs.end(); i != cs_end; ++i)
    if (offset > 0)
      q.insert(*i >> offset);
    else
      q.insert(*i);
}


void
append_init(Polyhedron& base, Polyhedron& inductive, Polyhedron& expected,
            size_t& offset, unsigned int& arity, unsigned int& num_vars) {
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
  base.insert(A == 0);
  base.insert(B >= 0);
  base.insert(C == B);
#if NOISY
  print_constraints(base, "*** base ***");
#endif

  // This is the inductive case:
  // append(A,B,C) :- A = [X|D], B = E, C = [X|F], append(D,E,F).
  inductive.add_dimensions_and_embed(6);
  inductive.insert(A + F == C + D);
  inductive.insert(B == E);
  inductive.insert(C + D >= A);
  inductive.insert(D >= 0);
  inductive.insert(B >= 0);
  inductive.insert(A >= D + 1);
#if NOISY
  print_constraints(inductive, "*** inductive ***");
#endif

  expected.add_dimensions_and_embed(3);
  expected.insert(A + B == C);
  expected.insert(B >= 0);
  expected.insert(C >= B);
}

void
fix_point(Polyhedron& start, Polyhedron& induct, Polyhedron& finish,
          size_t offset, unsigned int arity, unsigned int num_vars) {
  // Initialize the fixpoint iteration.
  Polyhedron current = start;
#if NOISY
  print_constraints(current, "*** start ***");
#endif

  // Contains the polyhedron computed at the previous iteration.
  Polyhedron previous;
  do {
    previous = current;
    current = induct;
    shift_rename_insert(previous, offset, current);
#if NOISY
    print_constraints(current, "*** after shift_rename_insert ***");
#endif

    set<Variable> dimensions_to_remove;
    for (unsigned int i = num_vars-1 ; i >= arity; --i )
      dimensions_to_remove.insert(Variable(i));
    current.remove_dimensions(dimensions_to_remove);

#if NOISY
    print_constraints(current, "*** after remove_dimensions ***");
#endif
    current.convex_hull_assign_and_minimize(previous);
#if NOISY
    print_constraints(current, "*** after convex_hull_assign_and_minimize***");
#endif
  } while (current != previous);
  finish = current;
}

int
main() {
  set_handlers();

  Polyhedron start;
  Polyhedron induct;
  Polyhedron expect;
  size_t recursive_offset;
  unsigned int arity;
  unsigned int num_vars;
  append_init(start, induct, expect, recursive_offset, arity, num_vars);
  Polyhedron final;
  fix_point(start, induct, final, recursive_offset, arity, num_vars);

#if NOISY
    print_constraints(expect, "*** expected ***");
#endif
  return final == expect ? 0 : 1;
}
