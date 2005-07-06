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

namespace {

Variable X(0);
Variable Y(1);

typedef Polyhedra_Powerset<C_Polyhedron> PSet;

// This tests the third case of the widening definition when the widening
// takes the the default option of returning a singleton set consisting
// of the convex hull of the set to be widened.
void
test1() {
  C_Polyhedron p1(2);
  p1.add_constraint(X >= 0);
  p1.add_constraint(Y >= 0);
  p1.add_constraint(X <= 2);
  p1.add_constraint(Y <= 1);

  C_Polyhedron p3(2);
  p3.add_constraint(X >= 3);
  p3.add_constraint(Y >= 1);
  p3.add_constraint(X <= 4);
  p3.add_constraint(Y <= 3);

  C_Polyhedron p4(2);
  p4.add_constraint(X >= 0);
  p4.add_constraint(Y >= 4);
  p4.add_constraint(X <= 2);
  p4.add_constraint(Y <= 5);

  PSet T1(2, EMPTY);
  T1.add_disjunct(p1);
  T1.add_disjunct(p3);
  T1.add_disjunct(p4);

  C_Polyhedron p2(2);
  p2.add_constraint(X >= 0);
  p2.add_constraint(Y >= 2);
  p2.add_constraint(X <= 1);
  p2.add_constraint(Y <= 3);

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
  T2.BHZ03_widening_assign(T1,
			   widen_fun_ref(&Polyhedron::H79_widening_assign));

  C_Polyhedron phull_T2(2);
  phull_T2.add_constraint(X >= 0);
  phull_T2.add_constraint(X <= 4);
  phull_T2.add_constraint(Y >= 0);
  phull_T2.add_constraint(Y <= 5);
  phull_T2.add_constraint(X - 2*Y <= 2);
  phull_T2.add_constraint(X + Y <= 7);

  PSet known_result(2, EMPTY);
  known_result.add_disjunct(phull_T2);

#if NOISY
  cout << "T2.BHZ03(T1, H79)" << " = " << T2 << endl;
  cout << "known result" << " = " << known_result << endl;
#endif

  if (T2 != known_result ||
      !T2.geometrically_covers(old_T2) || !T2.geometrically_covers(T1))
    exit(1);
}

// This tests the first case of the widening definition when the widening
// does nothing as the lgo for the polyhull is decreasing.
void
test2() {
  C_Polyhedron p1(2);
  p1.add_constraint(Y >= 2);
  p1.add_constraint(Y - X <= 2);
  p1.add_constraint(X + Y <= 4);

  C_Polyhedron p2(2);
  p2.add_constraint(X >= 0);
  p2.add_constraint(Y >= 0);
  p2.add_constraint(X <= 1);
  p2.add_constraint(Y <= 1);

  C_Polyhedron p3(2);
  p3.add_constraint(X >= 2);
  p3.add_constraint(Y >= 0);
  p3.add_constraint(X <= 4);
  p3.add_constraint(Y <= 1);

  C_Polyhedron p4(2);
  p4.add_constraint(X >= 3);
  p4.add_constraint(Y >= 2);
  p4.add_constraint(X <= 4);
  p4.add_constraint(Y <= 3);

  PSet T1(2, EMPTY);
  T1.add_disjunct(p1);
  T1.add_disjunct(p2);
  T1.add_disjunct(p3);
  T1.add_disjunct(p4);

  C_Polyhedron q1(2);
  q1.add_constraint(X >= 0);
  q1.add_constraint(Y >= 0);
  q1.add_constraint(X <= 4);
  q1.add_constraint(Y <= 4);

  C_Polyhedron q2(2);
  q2.add_constraint(X >= 5);
  q2.add_constraint(Y >= 3);
  q2.add_constraint(X <= 6);
  q2.add_constraint(Y <= 4);

  C_Polyhedron q3(2);
  q3.add_constraint(X >= 5);
  q3.add_constraint(Y >= 0);
  q3.add_constraint(X <= 6);
  q3.add_constraint(Y <= 2);

  PSet T2(2, EMPTY);
  T2.add_disjunct(q1);
  T2.add_disjunct(q2);
  T2.add_disjunct(q3);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;

  cout << "T1 = " << T1 << endl
       << "T2 = " << T2 << endl;
#endif

  PSet old_T2 = T2;
  T2.BHZ03_widening_assign(T1,
			   widen_fun_ref(&Polyhedron::H79_widening_assign));

#if NOISY
  cout << "T2.BHZ03(T1, H79)" << " = " << T2 << endl;
#endif

  if (T2 != old_T2 ||
      !T2.geometrically_covers(old_T2) || !T2.geometrically_covers(T1))
    exit(1);
}

// This tests the first case of the widening definition when the widening
// does nothing; the polyhull is stable with respect to the certificate
// and the multiset ordering for this certificate is decreasing.
void
test3() {
  C_Polyhedron p1(2);
  p1.add_constraint(X >= 1);
  p1.add_constraint(Y >= 4);
  p1.add_constraint(X <= 7);
  p1.add_constraint(Y <= 7);
  p1.add_constraint(X - Y <= 2);
  p1.add_constraint(X + Y >= 6);

  C_Polyhedron p2(2);
  p2.add_constraint(X >= 1);
  p2.add_constraint(Y >= 1);
  p2.add_constraint(X <= 3);
  p2.add_constraint(Y <= 3);

  C_Polyhedron p3(2);
  p3.add_constraint(X >= 5);
  p3.add_constraint(Y >= 1);
  p3.add_constraint(X <= 7);
  p3.add_constraint(Y <= 3);

  PSet T1(2, EMPTY);
  T1.add_disjunct(p1);
  T1.add_disjunct(p2);
  T1.add_disjunct(p3);

  C_Polyhedron q1(2);
  q1.add_constraint(X >= 0);
  q1.add_constraint(Y >= 0);
  q1.add_constraint(X <= 8);
  q1.add_constraint(Y <= 8);

  C_Polyhedron q2(2);
  q2.add_constraint(X >= 10);
  q2.add_constraint(Y >= 6);
  q2.add_constraint(X <= 12);
  q2.add_constraint(Y <= 8);

  C_Polyhedron q3(2);
  q3.add_constraint(X >= 10);
  q3.add_constraint(Y >= 0);
  q3.add_constraint(X <= 12);
  q3.add_constraint(Y <= 4);

  PSet T2(2, EMPTY);
  T2.add_disjunct(q1);
  T2.add_disjunct(q2);
  T2.add_disjunct(q3);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;

  cout << "T1 = " << T1 << endl
       << "T2 = " << T2 << endl;
#endif

  PSet old_T2 = T2;
  T2.BHZ03_widening_assign(T1,
			   widen_fun_ref(&Polyhedron::H79_widening_assign));

#if NOISY
  cout << "T2.BHZ03(T1, H79)" << " = " << T2 << endl;
#endif

  if (T2 != old_T2 ||
      !T2.geometrically_covers(old_T2) || !T2.geometrically_covers(T1))
    exit(1);
}

// This tests the first case of the widening definition when the widening
// of the elements of the set reduces the multiset ordering.
void
test4() {
  C_Polyhedron p1(2);
  p1.add_constraint(Y >= 2);
  p1.add_constraint(Y <= 3);
  p1.add_constraint(Y - X <= 2);
  p1.add_constraint(X + Y <= 8);

  C_Polyhedron p2(2);
  p2.add_constraint(X >= 0);
  p2.add_constraint(Y >= 0);
  p2.add_constraint(X <= 1);
  p2.add_constraint(Y <= 1);

  C_Polyhedron p3(2);
  p3.add_constraint(X >= 5);
  p3.add_constraint(Y >= 0);
  p3.add_constraint(X <= 8);
  p3.add_constraint(Y <= 1);

  C_Polyhedron p4(2);
  p4.add_constraint(X >= 7);
  p4.add_constraint(Y >= 4);
  p4.add_constraint(X <= 8);
  p4.add_constraint(Y <= 5);

  PSet T1(2, EMPTY);
  T1.add_disjunct(p1);
  T1.add_disjunct(p2);
  T1.add_disjunct(p3);
  T1.add_disjunct(p4);

  C_Polyhedron q1(2);
  q1.add_constraint(Y >= 2);
  q1.add_constraint(Y <= 4);
  q1.add_constraint(Y - X <= 2);
  q1.add_constraint(X + Y <= 8);

  PSet T2(2, EMPTY);
  T2.add_disjunct(q1);
  T2.add_disjunct(p2);
  T2.add_disjunct(p3);
  T2.add_disjunct(p4);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;

  cout << "T1 = " << T1 << endl
       << "T2 = " << T2 << endl;
#endif

  PSet old_T2 = T2;
  T2.BHZ03_widening_assign(T1,
			   widen_fun_ref(&Polyhedron::H79_widening_assign));

  C_Polyhedron r1(2);
  r1.add_constraint(Y >= 2);
  r1.add_constraint(Y - X <= 2);
  r1.add_constraint(X + Y <= 8);

  PSet known_result(2, EMPTY);
  known_result.add_disjunct(r1);
  known_result.add_disjunct(p2);
  known_result.add_disjunct(p3);
  known_result.add_disjunct(p4);

#if NOISY
  cout << "T2.BHZ03(T1, H79)" << " = " << T2 << endl;
#endif

  if (T2 != known_result ||
      !T2.geometrically_covers(old_T2) || !T2.geometrically_covers(T1))
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
