/* An example of iteration to a post-fixpoint.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

// Neutralize relops for GCC 2.96.
#ifndef __SGI_STL_INTERNAL_RELOPS
#define __SGI_STL_INTERNAL_RELOPS
#endif

#include "ppl.hh"
#include "print.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

void
shift_rename_insert(const Polyhedron& p, size_t offset, Polyhedron& q) {
  if (p.is_zero_dim())
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
  base.insert(A == 0);
  base.insert(B >= 0);
  base.insert(C == B);
#if NOISY
  print_constraints(base, "*** base ***");
#endif

  // This is the inductive case:
  // append(A,B,C) :- A = [X|D], B = E, C = [X|F], append(D,E,F).
  inductive.insert(A + F == C + D);
  inductive.insert(B == E);
  inductive.insert(C + D >= A);
  inductive.insert(D >= 0);
  inductive.insert(B >= 0);
  inductive.insert(A >= D + 1);
#if NOISY
  print_constraints(inductive, "*** inductive ***");
#endif

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
    current.convex_hull_assign(previous);
#if NOISY
    print_constraints(current, "*** after convex_hull_assign ***");
#endif
    current.widening_assign(previous);
#if NOISY
    print_constraints(current, "*** after widening_assign ***");
#endif

  } while (current != previous);
  finish = current;
}

void
append_size_rel(Polyhedron& ph) {
  Polyhedron start;
  Polyhedron induct;
  Polyhedron expect;
  size_t recursive_offset;
  unsigned int arity;
  unsigned int num_vars;
  append_init(start, induct, expect, recursive_offset, arity, num_vars);
  fix_point(start, induct, ph, recursive_offset, arity, num_vars);
}

void
permute_init(Polyhedron& base, Polyhedron& inductive, Polyhedron& expected,
             size_t& offset, unsigned int& arity, unsigned int& num_vars) {
  arity = 2;
  offset = 10;
  num_vars = 12;
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);
  Variable J(9);
  Variable K(10);
  Variable L(11);

  // This is the base case:
  // permute(A,B) :- A = [], B = [].
  base.insert(A == 0);
  base.insert(B == 0);
#if NOISY
  print_constraints(base, "*** base ***");
#endif
  // This is the inductive case:
  // permute(A,B) :- B = [X|C], 
  //                 E = [X|G], F = A, append(D,E,F),
  //                 D = H, I = G, append(H,I,J),
  //                 K = J, L = C, permute(K,L).
  inductive.insert(B == C + 1);
  inductive.insert(E == G + 1);
  inductive.insert(F == A);
  Polyhedron ph_append;
  append_size_rel(ph_append);
  shift_rename_insert(ph_append, 3, inductive);
  shift_rename_insert(ph_append, 7, inductive);
  inductive.insert(D + G == H + I);
  inductive.insert(D == H);
  inductive.insert(I == G);
  inductive.insert(K == J);
  inductive.insert(L == C);
  inductive.insert(A >= 0);
  inductive.insert(C >= 0);
#if NOISY
  print_constraints(inductive, "*** inductive ***");
#endif

  expected.insert(A == B);
  expected.insert(A >= 0);
  expected.insert(B >= 0);
}

int
main() {
  Polyhedron start;
  Polyhedron induct;
  Polyhedron expect;
  size_t recursive_offset;
  unsigned int arity;
  unsigned int num_vars;
  permute_init(start, induct, expect, recursive_offset, arity, num_vars);
  Polyhedron final;
  fix_point(start, induct, final, recursive_offset, arity, num_vars);

#if NOISY
    print_constraints(expect, "*** expected ***");
#endif
  return final == expect ? 0 : 1;
}
