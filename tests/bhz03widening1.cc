/* Test Polyhedra_Powerset<PH>::BHZ03_widening_assign().
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
    p[3].add_constraint(x+y <= 14);
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
    p[n] = P(n-4);
    p[n].affine_image(x, 2*x);
    p[n].affine_image(y, 8 - 2*y);
  }

  return p[n];
}

PSet
S(unsigned n) {
  PSet s(2, Polyhedron::EMPTY);
  if (n == 0) {
#if NOISY
    cout << "S0 = { P0 }" << endl;
#endif
    s.add_disjunct(P(0));
    return s;
  }

  const int p_base = (n-1)/3*4;

  switch (n % 3) {
  case 1:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << p_base + 1 << ", "
	 << "P" << p_base + 3 << " }" << endl;
#endif
    s.add_disjunct(P(p_base + 1));
    s.add_disjunct(P(p_base + 3));
    break;
  case 2:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << p_base + 2 << ", "
	 << "P" << p_base + 3 << " }" << endl;
#endif
    s.add_disjunct(P(p_base + 2));
    s.add_disjunct(P(p_base + 3));
    break;
  case 0:
#if NOISY
    cout << "S" << n << " = { "
	 << "P" << p_base + 2 << ", "
	 << "P" << p_base + 4 << " }" << endl;
#endif
    s.add_disjunct(P(p_base + 2));
    s.add_disjunct(P(p_base + 4));
    break;
  }    
  return s;
}

void
my_output_function(ostream& s, const Variable& v) {
  s << char('x' + v.id());
}

using namespace Parma_Polyhedra_Library::IO_Operators;

int
main() TRY {
  set_handlers();

  // Install the alternate output function.
  Variable::set_output_function(my_output_function);

  PSet T = S(0);
#if NOISY
  cout << "T0 = " << T << endl;
#endif
  bool converged = false;
  for (unsigned n = 1; !converged && n <= 20; ++n) {
    PSet Sn = S(n);
#if NOISY
    cout << "S" << n << " = " << Sn << endl;
#endif
    Sn.upper_bound_assign(T);
    Sn.BHZ03_widening_assign(T, &Polyhedron::H79_widening_assign);
#if NOISY
    cout << "T" << n << " = " << Sn << endl;
#endif
    if (Sn.definitely_entails(T))
      converged = true;
    else
      std::swap(Sn, T);
  }

  return converged ? 0 : 1;
}
CATCH
