/* Test Octagon::relation_with(g): we verify that a generator
   is not subsumed by an empty octagon.
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
  Variable x(0);

  TOctagon oct(2, EMPTY);

  Generator g = point(x);
  Poly_Gen_Relation rel = oct.relation_with(g);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_generator(g, "--- g ---");
  cout << "oct.relation_with(v(A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();

  if (rel != known_result)
    exit(1);
}

static void test2() {
  TOctagon oct;

  Generator g = point();
  Poly_Gen_Relation rel = oct.relation_with(g);

#if NOISY
  print_constraints(oct, "--- oct ---");
  print_generator(g, "--- g ---");
  cout << "oct.relation_with(v()) == " << rel << endl;
#endif

  
  Poly_Gen_Relation known_result = Poly_Gen_Relation::subsumes();
  
  if (rel != known_result)
    exit(1);
}

static void test3() {
  Variable A(0);

  TOctagon oct(2);
  oct.add_constraint(A == 0);

  Poly_Gen_Relation rel = oct.relation_with(point(2*A));

#if NOISY
  print_constraints(oct, "--- oct ---");
  cout << "oct.relation_with(point(2*A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();

  if (rel != known_result) 
    exit(1);
}

static void test4() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A + B >= 0);
  oct.add_constraint(B >= 0);

  Poly_Gen_Relation rel1 = oct.relation_with(point(B));
  Poly_Gen_Relation rel2 = oct.relation_with(point(-B));

#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(point(B)) == " << rel1 << endl;
  cout << "oct.relation_with(point(-B)) == " << rel2 << endl;
#endif

  Poly_Gen_Relation known_result1 = Poly_Gen_Relation::subsumes();
  Poly_Gen_Relation known_result2 = Poly_Gen_Relation::nothing();

  if (rel1 != known_result1 && rel2 != known_result2) 
    exit(1);
}

static void test5() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A >= 0);
  oct.add_constraint(B == 1);

  Poly_Gen_Relation rel = oct.relation_with(ray(-A));

#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(ray(-A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();
  if (rel != known_result) 
    exit(1);
}

static void test6() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A <= 0);
  oct.add_constraint(B == 1);

  Poly_Gen_Relation rel = oct.relation_with(ray(-A));

#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(ray(-A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::subsumes();
  if (rel != known_result) 
    exit(1);;
}

static void test7() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A >= 0);
  oct.add_constraint(B >= -1);

  Poly_Gen_Relation rel = oct.relation_with(line(A));

#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(line(A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();
  if (rel != known_result) 
    exit(1);
}

static void test8() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A == 0);
  oct.add_constraint(B >= -1);

  Poly_Gen_Relation rel = oct.relation_with(line(-A));

#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(line(A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();
  
  if (rel != known_result) 
    exit(1);
}

static void test9() {
  Variable A(0);
  Variable B(1);

  TOctagon oct	(2);
  oct.add_constraint(A <= 0);
  oct.add_constraint(B == 2);

  Poly_Gen_Relation rel = oct.relation_with(closure_point(A));

#if NOISY
  print_constraints(oct	, "*** oct	 ***");
  cout << "oct.relation_with(line(A)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();
  
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
