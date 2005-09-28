/* Test Polyhedra_Powerset<PH>::BHZ03_widening_assign().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"
#include <vector>

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

Variable X(0);
Variable Y(1);

typedef Polyhedra_Powerset<C_Polyhedron> PSet;

} // namespace

int
main() TRY {
  set_handlers();

  C_Polyhedron p1(2);
  p1.add_constraint(X >= 0);
  p1.add_constraint(Y >= 0);
  p1.add_constraint(X <= 2);
  p1.add_constraint(Y <= 1);

  C_Polyhedron p2(2);
  p2.add_constraint(X >= 0);
  p2.add_constraint(Y >= 2);
  p2.add_constraint(X <= 1);
  p2.add_constraint(Y <= 3);

  C_Polyhedron p3(2);
  p3.add_constraint(X >= 3);
  p3.add_constraint(Y >= 1);
  p3.add_constraint(X <= 4);
  p3.add_constraint(Y <= 3);

  PSet T1(2, EMPTY);
  T1.add_disjunct(p1);
  T1.add_disjunct(p2);
  T1.add_disjunct(p3);

  C_Polyhedron p4(2);
  p4.add_constraint(X >= 0);
  p4.add_constraint(Y >= 4);
  p4.add_constraint(X <= 2);
  p4.add_constraint(Y <= 5);

  PSet T2(2, EMPTY);
  T2.add_disjunct(p1);
  T2.add_disjunct(p2);
  T2.add_disjunct(p3);
  T2.add_disjunct(p4);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;

  cout << "T1 = " << T1 << endl
       << "T2 = " << T2 << endl;
#endif

  PSet old_T2 = T2;
  T2.BHZ03_widening_assign<BHRZ03_Certificate>
    (T1, widen_fun_ref(&Polyhedron::H79_widening_assign));

  C_Polyhedron pd(2);
  pd.add_constraint(X >= 0);
  pd.add_constraint(X <= 4);
  pd.add_constraint(X + 2*Y >= 10);

  PSet known_result = old_T2;
  known_result.add_disjunct(pd);

#if NOISY
  cout << "T2.BHZ03(T1, H79)" << " = " << T2 << endl;
  cout << "known result" << " = " << known_result << endl;
#endif

  return
    (T2 == known_result
     && T2.geometrically_covers(old_T2) && T2.geometrically_covers(T1))
    ? 0
    : 1;
}
CATCH
