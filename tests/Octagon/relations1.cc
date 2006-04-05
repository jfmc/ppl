/* Test Octagon::relation_with(c).
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

#ifndef NOISY
#define NOISY 0
#endif


static void test1() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2, EMPTY);

  Poly_Con_Relation rel = oct.relation_with(A >= 0);

#if NOISY
  print_constraints(oct, "--- oct ---");
  cout << "oct.relation_with(A >= 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test2() {
  Variable x(0);
  Variable y(1);

  TOctagon oct(2);
  oct.add_constraint(y <= -1);

  Constraint c(y >= 0);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();
  
  if (rel != known_result)
    exit(1); 
}

static void test3() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oct(3);
  oct.add_constraint(y <= 1);

  Constraint c(y >= 0);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1); 
}

static void test4() {
  Variable x(0);
  Variable y(1);

  TOctagon oct(2);
  oct.add_constraint(x - y == 0);

  Constraint c(x + y == 0);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1); 
}

static void test5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
  Variable w(3);

  TOctagon oct(4);
  oct.add_constraint(x - y == 0);
  oct.add_constraint(w <= 1);
  oct.add_constraint(x + z >= 3);
  oct.add_constraint(z - w <= 2);

  Constraint c(x + w >= 0);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();
  
  if (rel != known_result)
    exit(1); 
}

static void test6() {
  Variable x(0);
  Variable y(1);

  Constraint_System cs;
  cs.insert(x + y >= 1);
  cs.insert(y >= 5);

  TOctagon oct(cs);

  // An equality constraint non-intersecting the octagon.
  Constraint c(y == -1);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();
  
  if (rel != known_result)
    exit(1); 
}

static void test7() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A == 0);
  cs.insert(B <= -1);
  cs.insert(A - B <= 2);
 
  TOctagon oct(cs);

  Poly_Con_Relation rel = oct.relation_with(Constraint::zero_dim_false());

#if NOISY
  print_constraints(oct, "--- oct ---");
  cout << "oct.relation_with(Constraint::zero_dim_false()) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test8() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TOctagon oct(3);
  oct.add_constraint(x == 1);
  oct.add_constraint(y <= 0);
  oct.add_constraint(z >= 2);
  
  Constraint c(x > 1);
  Poly_Con_Relation rel = oct.relation_with(c);
  
#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test9() {
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
  Variable M(12);
  Variable N(13);
  Variable O(14);
  Variable P(15);
  Variable Q(16);
  Variable R(17);
  Variable S(18);
  Variable T(19);
  Variable U(20);
  Variable V(21);
  Variable W(22);
  Variable X(23);
  Variable Y(24);
  Variable Z(25);
  Variable A1(26);
  Variable B1(27);
  Variable C1(28);
  Variable D1(29);
  Variable E1(30);
  Variable F1(31);
  Variable G1(32);
  Variable H1(33);
 
  TOctagon oct(34);
  oct.add_constraint(C >= 4);
  oct.add_constraint(D >= 1);
  oct.add_constraint(G == 0);
  oct.add_constraint(L >= 5);
  oct.add_constraint(O >= 1);
  oct.add_constraint(A1 >= -1);
  oct.add_constraint(A1 <= 511);
  oct.add_constraint(C1 >= 0);
  oct.add_constraint(H1 >= 1);
  oct.add_constraint(C + D >= 5);
  oct.add_constraint(E - F == 0);
  oct.add_constraint(C - G >= 4);
  oct.add_constraint(C + G >= 4);
  oct.add_constraint(D - G >= 1);
  oct.add_constraint(D + G >= 1);
  oct.add_constraint(J - K == 0);
  oct.add_constraint(L - C >= 1);
  oct.add_constraint(C + L >= 9);
  oct.add_constraint(D + L >= 6);
  oct.add_constraint(L - G >= 5);
  oct.add_constraint(G + L >= 5);
  oct.add_constraint(M - N == 0);
  oct.add_constraint(C + O >= 5);
  oct.add_constraint(D + O >= 2);
  oct.add_constraint(O - G >= 1);
  oct.add_constraint(G + O >= 1);
  oct.add_constraint(L + O >= 6);
  oct.add_constraint(R - S == 0);
  oct.add_constraint(U - V == 0);
  oct.add_constraint(W - X == 0);
  oct.add_constraint(Y - Z == 0);
  oct.add_constraint(A1 - C <= 507);
  oct.add_constraint(C + A1 >= 3);
  oct.add_constraint(A1 - D <= 510);
  oct.add_constraint(D + A1 >= 0);
  oct.add_constraint(G - A1 <= 1);
  oct.add_constraint(A1 - G <= 511);
  oct.add_constraint(G + A1 >= -1);
  oct.add_constraint(G + A1 <= 511);
  oct.add_constraint(A1 - L <= 506);
  oct.add_constraint(L + A1 >= 4);
  oct.add_constraint(A1 - O <= 510);
  oct.add_constraint(O + A1 >= 0);
  oct.add_constraint(C + C1 >= 4);
  oct.add_constraint(D + C1 >= 1);
  oct.add_constraint(G -C1 <= 0);
  oct.add_constraint(G + C1 >= 0);
  oct.add_constraint(L + C1 >= 5);
  oct.add_constraint(C1 - O <= 0);
  oct.add_constraint(O + C1 >= 1);
  oct.add_constraint(A1 - C1 <= 511);
  oct.add_constraint(A1 + C1 >= -1);
  oct.add_constraint(D1 - E1 == 0);
  oct.add_constraint(C + H1 >= 5);
  oct.add_constraint(D + H1 >= 2);
  oct.add_constraint(H1 - G >= 1);
  oct.add_constraint(G + H1 >= 1);
  oct.add_constraint(L + H1 >= 6);
  oct.add_constraint(O + H1 >= 2);
  oct.add_constraint(A1 - H1 <= 510);
  oct.add_constraint(A1 + H1 >= 0);
  oct.add_constraint(C1 + H1 >= 1);

  Constraint c(-C1 >= -511);
  Poly_Con_Relation rel = oct.relation_with(c);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_constraint(c, "--- c ---");
  cout << "oct.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

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

  return 0;
}
CATCH
