/* Testing Polyhedron::relation_with(c) when c is a strict inequality.
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

#include "ppl_install.hh"
#include "print.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

void test1() {
  // The zero-dim universe polyhedron.
  Polyhedron ph;
  Poly_Con_Relation rel = ph.relation_with(LinExpression(0) > 0);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(0 > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

void test2() {
  // The zero-dim universe polyhedron.
  Polyhedron ph;
  Poly_Con_Relation rel = ph.relation_with(LinExpression(0) > 1);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(0 > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

void test3() {
  // The zero-dim universe polyhedron.
  Polyhedron ph;
  Poly_Con_Relation rel = ph.relation_with(LinExpression(1) > 0);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(1 > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

void test4() {
  // An empty polyhedron.
  Polyhedron ph(1);
  ph.add_constraint(LinExpression(0) >= 1);
  Variable A(0);
  Poly_Con_Relation rel = ph.relation_with(A > 0);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(A > 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

void test5() {
  Variable A(0);
  Variable B(1);
  ConSys cs(A + B == 3);
  Polyhedron ph(cs);

  Poly_Con_Relation rel = ph.relation_with(A + B > 3);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(A + B > 3) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

void test6() {
  Variable A(0);
  Variable B(1);
  ConSys cs(A + B <= 3);
  Polyhedron ph(cs);

  Poly_Con_Relation rel = ph.relation_with(A + B > 3);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(A + B > 3) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}
  
void test7() {
  Variable A(0);
  Variable B(1);
  ConSys cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A + B <= 3);
  Polyhedron ph(cs);

  Poly_Con_Relation rel = ph.relation_with(A + 2*B < 10);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(A + 2*B < 10) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

void test8() {
  Variable A(0);
  Variable B(1);
  ConSys cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A + B <= 3);
  Polyhedron ph(cs);

  Poly_Con_Relation rel = ph.relation_with(A + B > 1);

#if NOISY
  print_generators(ph, "--- ph ---");
  cout << "ph.relation_with(A + B > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1);
}


int main() {

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();

  return 0;
}
