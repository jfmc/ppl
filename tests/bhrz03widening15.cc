/* Test Polyhedron::BHRZ03_widening_assign().
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
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

static GenSys
g_0() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  GenSys new_gs;
  new_gs.insert(ray(A + C));
  new_gs.insert(ray(-B + C));
  new_gs.insert(ray(-A + C));
  new_gs.insert(ray(B + C));
  return new_gs;
}

static Generator
splitting_facet(const Generator& r1,
		const Generator& r2,
		unsigned magic_number) {
  LinExpression expr;
  expr += LinExpression(r1);
  expr += LinExpression(r2);
  // NOTE: I am not *sure* this dirty kludge of using such
  // a magic number will always succeed.
  expr *= magic_number + 1;
  Variable C(2);
  expr -= C;
  return ray(expr);
} 

static GenSys
double_generators(const GenSys& gs, unsigned magic_number) {
  GenSys new_gs;
  GenSys::const_iterator i = gs.begin();
  GenSys::const_iterator gs_end = gs.end();
  while (true) {
    const Generator& g = *i;
    new_gs.insert(g);
    i++;
    if (i != gs_end)
      new_gs.insert(splitting_facet(g, *i, magic_number));
    else {
      // Split the last facet.
      GenSys::const_iterator gs_begin = gs.begin();
      new_gs.insert(splitting_facet(g, *gs_begin, magic_number));
      break;
    }
  }
  return new_gs;
}


static C_Polyhedron
p(unsigned n) {

  unsigned needed_facets = n + 4;

  unsigned magic_number = 1;
  unsigned magic_factor = 4;
  GenSys gs = g_0();
  unsigned gs_facets = 4;

  while (gs_facets * 2 <= needed_facets) {
    magic_number *= magic_factor;
    gs = double_generators(gs, magic_number);
    gs_facets *= 2;
  }

  if (gs_facets < needed_facets) {
    magic_number *= magic_factor;
    GenSys gs2 = double_generators(gs, magic_number);
    GenSys::const_iterator gs2_i = gs2.begin();
    for ( ; gs_facets < needed_facets; gs_facets++) {
      // Skip the even indexed facets of gs2.
      gs2_i++;
      // Add the odd indexed facets of gs2.
      gs.insert(*gs2_i++);
    }
  }
  gs.insert(point());
  C_Polyhedron ph = C_Polyhedron(gs);
  return ph;
}


int
main() {
  set_handlers();

  // Chain condition for widenings:
  // for each increasing chain of descriptions p_0, p_1, ..., p_i, ...,
  // the sequence q_0, q_1, ..., q_i, ... defined by q_0 = p_0 and
  // for each i >= 1, q_i = q_{i-1} \nabla p_i is ultimately stationary.

  // Initialization: set q_0.
  C_Polyhedron q_i_minus_1 = p(0);

  for (unsigned i = 1; i <= 100; ++i) {

#if NOISY
    cout << "*** Result of the previous iteration:" << endl;
    cout << q_i_minus_1.generators() << endl;
#endif
    C_Polyhedron p_i = p(i);
#if NOISY
    cout << "*** New stuff:" << endl;
    cout << p_i.generators() << endl;
#endif
    C_Polyhedron q_i = q_i_minus_1;
    q_i.poly_hull_assign(p_i);

#if NOISY
    cout << "*** Poly-hull of previous with new:" << endl;
    cout << q_i.generators() << endl;
#endif
   
    q_i.BHRZ03_widening_assign(q_i_minus_1);

#if NOISY
    cout << "*** Result of widening poly-hull with new:" << endl;
    cout << q_i.generators() << endl;
#endif
    if (q_i == q_i_minus_1) {
      C_Polyhedron known_result(3, C_Polyhedron::UNIVERSE);

      int retval = (q_i == known_result) ? 0 : 1;

#if NOISY
      print_constraints(q_i, "*** The constraints of the fix point ***");
      print_generators(q_i, "*** The generators of the fix point ***");
#endif

      return retval;
    }
    q_i_minus_1 = q_i;
  }
  return 1;
}
