/* Test Polyhedron::BBRZ02_widening_assign().
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

#ifndef NOISY
#define NOISY 0
#endif

static Variable A(0);
static Variable B(1);
static Variable C(2);
static Variable D(3);
static Variable E(4);

const GenSys&
fixed_part() {
  static GenSys gs;
  static bool initialized = false;
  if (!initialized) {
    gs.insert(point());
    gs.insert(ray(C));
    gs.insert(ray(D));
    gs.insert(ray(E));
    gs.insert(ray(A + D));
    gs.insert(ray(A + B + E));
    initialized = true;
  }
  return gs;
}

C_Polyhedron
p(unsigned n) {
  C_Polyhedron ph = fixed_part();
  n += 2;
  ph.add_generator(ray(A + (n-1)*B + E));
  if (n % 2 == 0) {
    // Even.
    unsigned m = n / 2;
    ph.add_generator(ray(m*B + E));
    ph.add_generator(ray(A + (m-1)*B + D));
  }
  else {
    // Odd.
    ph.add_generator(ray(n*B + 2*E));
    ph.add_generator(ray(2*A + (n-2)*B + 2*D));
  }
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
    std::cout << "*** Result of the previous iteration:" << std::endl;
    std::cout << q_i_minus_1.generators() << std::endl;
#endif
    C_Polyhedron p_i = p(i);
#if NOISY
    std::cout << "*** New stuff:" << std::endl;
    std::cout << p_i.generators() << std::endl;
#endif
    C_Polyhedron q_i = q_i_minus_1;
    q_i.poly_hull_assign(p_i);

#if NOISY
    std::cout << "*** Poly-hull of previous with new:" << std::endl;
    std::cout << q_i.generators() << std::endl;
#endif
   
    q_i.BBRZ02_widening_assign(q_i_minus_1);

#if NOISY
    std::cout << "*** Result of widening poly-hull with new:" << std::endl;
    std::cout << q_i.generators() << std::endl;
#endif
    if (q_i == q_i_minus_1)
      return 0;
    q_i_minus_1 = q_i;
  }
  return 1;
}
