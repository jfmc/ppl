/* Test Polyhedron::BHRZ03_widening_assign().
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

static Generator_System
g_0() {
  Variable A(0);
  Variable B(1);
  Generator_System new_gs;
  new_gs.insert(point(A));
  new_gs.insert(point(-B));
  new_gs.insert(point(-A));
  new_gs.insert(point(B));
  return new_gs;
}

static Generator
splitting_segment(const Generator& p1,
		  const Generator& p2,
		  unsigned magic_number) {
  const Coefficient& d1 = p1.divisor();
  const Coefficient& d2 = p2.divisor();
  Linear_Expression expr = d2 * Linear_Expression(p1);
  expr += d1 * Linear_Expression(p2);
  // The divisor for the average is 2 * d1 * d2.
  // by carefully taking a smaller divisor, we obtain a point
  // that won't be redundant in the polyhedron.
  // NOTE: I am not *sure* this dirty kludge of using such
  // a magic number will always succeed.
  return point((magic_number+1)*expr, magic_number*2*d1*d2);
}

static Generator_System
double_generators(const Generator_System& gs, unsigned magic_number) {
  Generator_System new_gs;
  Generator_System::const_iterator i = gs.begin();
  Generator_System::const_iterator gs_end = gs.end();
  while (true) {
    const Generator& g = *i;
    new_gs.insert(g);
    ++i;
    if (i != gs_end)
      new_gs.insert(splitting_segment(g, *i, magic_number));
    else {
      // Split the last segment.
      Generator_System::const_iterator gs_begin = gs.begin();
      new_gs.insert(splitting_segment(g, *gs_begin, magic_number));
      break;
    }
  }
  return new_gs;
}

static C_Polyhedron
p(unsigned n) {

  unsigned needed_vertices = n + 4;

  unsigned magic_number = 1;
  unsigned magic_factor = 4;
  Generator_System gs = g_0();
  unsigned gs_vertices = 4;

  while (gs_vertices * 2 <= needed_vertices) {
    magic_number *= magic_factor;
    gs = double_generators(gs, magic_number);
    gs_vertices *= 2;
  }

  if (gs_vertices < needed_vertices) {
    magic_number *= magic_factor;
    Generator_System gs2 = double_generators(gs, magic_number);
    Generator_System::const_iterator gs2_i = gs2.begin();
    for ( ; gs_vertices < needed_vertices; ++gs_vertices) {
      // Skip the even indexed vertices of gs2.
      ++gs2_i;
      // Add the odd indexed vertices of gs2.
      gs.insert(*gs2_i++);
    }
  }

  C_Polyhedron ph = C_Polyhedron(gs);
  return ph;
}


int
main() TRY {
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
      C_Polyhedron known_result(2);

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
CATCH
