/* Test BD_Shape::relation_with(c).
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void test1() {
  // The zero-dim universe bdiff.
  TBD_Shape bd(0);
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(0) > 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(0 > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test2() {
  // The zero-dim universe bdiff.
  TBD_Shape bd(0);
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(0) > 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(0 > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test3() {
  // The zero-dim universe bdiff.
  TBD_Shape bd(0);
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(1) > 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

static void test4() {
  // An empty bdiff.
  TBD_Shape bd(1);
  bd.add_constraint(Linear_Expression(0) >= 1);
 
  Variable A(0);

  Poly_Con_Relation rel = bd.relation_with(A > 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A > 0) = " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test5() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs(A - B == 3);
  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(A - B > 3);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A - B > 3) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test6() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs(A - B <= 3);
  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(A - B > 3);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A - B > 3) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test7() {
  Variable A(0);

  Constraint_System cs;
  cs.insert(A <= 1);

  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(A > 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1);
}

static void test8() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A - B <= 3);
 
  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(A - B > 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A - B > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1);
}

static void test9() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A - B <= 3);
 
  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(A > 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(A > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

static void test10() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A == 0);
  cs.insert(B <= -1);
  cs.insert(A - B <= 2);
 
  TBD_Shape bd(cs);

  Poly_Con_Relation rel = bd.relation_with(B - A > 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(B - A > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

int main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();

  return 0;
}
CATCH


