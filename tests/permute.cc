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
#define NOISY 1
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
            dimension_type& offset, unsigned int& arity) {
  offset = 3;
  arity = 3;
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
          dimension_type offset, unsigned int arity) {
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
    dimension_type current_dim;
    current_dim = current.space_dimension();
    for (dimension_type i = current_dim-1 ; i >= arity; --i )
      dimensions_to_remove.insert(Variable(i));
    current.remove_dimensions(dimensions_to_remove);

#if NOISY
    print_constraints(current, "*** after remove_dimensions ***");
#endif
    current.poly_hull_assign_and_minimize(previous);
#if NOISY
    print_constraints(current, "*** after poly_hull_assign_and_minimize***");
#endif
    current.H79_widening_assign(previous);
#if NOISY
    print_constraints(current, "*** after H79_widening_assign ***");
#endif

  } while (current != previous);
  finish = current;
}

static void
append_size_rel(C_Polyhedron& ph) {
  C_Polyhedron start;
  C_Polyhedron induct;
  C_Polyhedron expect;
  dimension_type recursive_offset;
  unsigned int arity;
  append_init(start, induct, expect, recursive_offset, arity);
  fix_point(start, induct, ph, recursive_offset, arity);
}

static void
permute_init(C_Polyhedron& base, C_Polyhedron& induct, C_Polyhedron& expect,
             dimension_type& offset, unsigned int& arity) {
  arity = 2;
  offset = 10;
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
  base.add_dimensions_and_embed(2);
  base.add_constraint(A == 0);
  base.add_constraint(B == 0);
#if NOISY
  print_constraints(base, "*** base ***");
#endif
  // This is the inductive case:
  // permute(A,B) :- B = [X|C],
  //                 E = [X|G], F = A, append(D,E,F),
  //                 D = H, I = G, append(H,I,J),
  //                 K = J, L = C, permute(K,L).
  induct.add_dimensions_and_embed(12);
  induct.add_constraint(B == C + 1);
  induct.add_constraint(E == G + 1);
  induct.add_constraint(F == A);
  C_Polyhedron ph_append;
  append_size_rel(ph_append);
  shift_rename_add(ph_append, 3, induct);
  shift_rename_add(ph_append, 7, induct);
  induct.add_constraint(D + G == H + I);
  induct.add_constraint(D == H);
  induct.add_constraint(I == G);
  induct.add_constraint(K == J);
  induct.add_constraint(L == C);
  induct.add_constraint(A >= 0);
  induct.add_constraint(C >= 0);
#if NOISY
  print_constraints(induct, "*** inductive ***");
#endif

  expect.add_dimensions_and_embed(2);
  expect.add_constraint(A == B);
  expect.add_constraint(A >= 0);
  expect.add_constraint(B >= 0);
}

int
main() try {
  set_handlers();

  C_Polyhedron start;
  C_Polyhedron induct;
  C_Polyhedron expect;
  dimension_type recursive_offset;
  unsigned int arity;
  permute_init(start, induct, expect, recursive_offset, arity);
  C_Polyhedron final;
  fix_point(start, induct, final, recursive_offset, arity);

#if NOISY
  print_constraints(expect, "*** expected ***");
#endif

  C_Polyhedron final1;
  final1 = induct;
  shift_rename_add(final, recursive_offset, final1);

#if NOISY
  print_constraints(final1, "*** after shift_rename_add ***");
#endif
  C_Polyhedron final2;
  final2 = final1;
  Variable A(0);
  Variable B(1);
  Variable K(10);
  Variable L(11);
  final2.add_constraint(B - L >= 1);
  final2.add_constraint(A - K >= 1);

#if NOISY
  if (final2 == final1)
    print_constraints(final2, "*** termination condition satisfied ***");
  else
    print_constraints(final2, "*** termination condition not satisfied ***");
#endif

  return final2 == final1 ? 0 : 1;
}
catch (const std::exception& e) {
  cerr << "std::exception caught: "
       << e.what() << " (type == " << typeid(e).name() << ")"
       << endl;
  return 1;
}
