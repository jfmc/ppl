/* Test Polyhedra_Powerset<PH>::H79_widening_assign().
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
#include <vector>

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

Variable x(0);
Variable y(1);

typedef Polyhedra_PowerSet<C_Polyhedron> PSet;

const C_Polyhedron&
P(unsigned n) {
  static std::vector<C_Polyhedron> p;
  if (p.size() < 5) {
    p.resize(5, C_Polyhedron(2));
    p[2].add_constraint(0 <= x);
    p[2].add_constraint(x <= 4);
    p[2].add_constraint(0 <= y);
    p[2].add_constraint(y <= 4);
    p[1] = p[2];
    p[1].add_constraint(x-y <= 3);
    p[0] = p[1];
    p[0].add_constraint(x+y >= 1);

    p[3].add_constraint(0 <= x);
    p[3].add_constraint(x <= 8);
    p[3].add_constraint(0 <= y);
    p[3].add_constraint(y <= 8);
    p[3].add_constraint(x-y >= -6);
    p[4] = p[3];
    p[3].add_constraint(5*x-y >= -2);
    p[3].add_constraint(x+3*y >= 3);
    p[4].add_constraint(4*x-y >= -3);
    p[4].add_constraint(x+2*y >= 2);
  }

  if (n >= p.size()) {
    unsigned new_size = p.size();
    while (n >= new_size)
      new_size *= 2;
    p.resize(p.size()*2);
  }

  if (p[n].is_universe()) {
    p[n] = P(n-5);
    p[n].affine_image(x, 2*x);
    p[n].affine_image(y, (Integer(1) << (n/5 + 2)) - 2*y);
  }

  return p[n];
}

PSet
S(unsigned n) {
  PSet s(2, Polyhedron::EMPTY);
  switch (n % 4) {
  case 0:
#if NOISY
    cout << "S" << n << " = { " << "P" << n + n/4 << " }" << endl;
#endif
    s.add_disjunct(P(n + n/4));
    break;
  case 1:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << n + n/4 << ", "
	 << "P" << n + 2 + n/4 << " }" << endl;
#endif
    s.add_disjunct(P(n + n/4));
    s.add_disjunct(P(n + 2 + n/4));
    break;
  case 2:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << n + n/4 << ", "
	 << "P" << n + 1 + n/4 << " }" << endl;
#endif
    s.add_disjunct(P(n + n/4));
    s.add_disjunct(P(n + 1 + n/4));
    break;
  case 3:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << n - 1 + n/4 << ", "
	 << "P" << n + 1 + n/4 << " }" << endl;
#endif
    s.add_disjunct(P(n - 1 + n/4));
    s.add_disjunct(P(n + 1 + n/4));
    break;
  }    
  return s;
}

using namespace Parma_Polyhedra_Library::IO_Operators;

int
main() TRY {
  set_handlers();

  PSet T = S(0);
#if NOISY
  cout << "T0 = " << T << endl;
#endif
  bool converged = false;
  for (unsigned n = 1; !converged && n <= 100; ++n) {
    PSet U = T;
#if NOISY
    cout << "S" << n << " = " << S(1) << endl;
#endif
    T.H79_widening_assign(S(n), 3);
#if NOISY
    cout << "T" << n << " = " << T << endl;
#endif
    if (T.definitely_entails(U))
      converged = true;
  }

  return !converged ? 0 : 1;
}
CATCH
