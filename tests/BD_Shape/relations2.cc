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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

static void test1() {
  // The zero-dim universe BDS.
  TBD_Shape bd(0);
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(0) >= 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(0 >= 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included()
    && Poly_Con_Relation::saturates();

  if (rel != known_result)
    exit(1);
}

static void test2() {
  // The zero-dim universe BDS.
  TBD_Shape bd(0);
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(0) >= 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(0 >= 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test3() {
  // The zero-dim universe BDS.
  TBD_Shape bd;
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(1) >= 0);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 >= 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

static void test4() {
  Variable x(0);
  // Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x == 1);

  Constraint c(x > 1);
  Poly_Con_Relation rel = bd.relation_with(c);

#if NOISY
  print_constraints(bd, "--- bd ---");
  print_constraint(c, "--- c ---");
  cout << "bd.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();
 
  if (rel != known_result)
    exit(1);
}

static void test5() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);
  bd.add_constraint(x == 1);
  bd.add_constraint(y <= 0);
  bd.add_constraint(z >= 2);

  Constraint c(x > 1);
  Poly_Con_Relation rel = bd.relation_with(c);

#if NOISY
  print_constraints(bd, "--- bd ---");
  print_constraint(c, "--- c ---");
  cout << "bd.relation_with(c) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test6() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd(2);
  bd.add_constraint(x == 0);
  bd.add_constraint(y >= 1);

  Poly_Con_Relation rel = bd.relation_with(-y >= -1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(-y >= -1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  if (rel != known_result)
    exit(1);
}

static void test7() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBD_Shape bd(3);

  bd.add_constraint(x - y <= 2);
  bd.add_constraint(x - z >= -1);
  bd.add_constraint(y <= 3);

  Poly_Con_Relation rel = bd.relation_with(y > 3);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(y > 3) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  if (rel != known_result)
    exit(1);
}

static void test8() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
 
  TBD_Shape bd(3);

  bd.add_constraint(x - y <= 2);
  bd.add_constraint(x - z >= -1);
  bd.add_constraint(y <= 3);

  Poly_Con_Relation rel = bd.relation_with(-y >= -4);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 >= 0) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  if (rel != known_result)
    exit(1);
}

static void test9() {
  Variable A(0);

  TBD_Shape bd(1);
  bd.add_constraint(A <= 0);
  bd.add_constraint(A >= -2);

  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(0) >= -1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(0 >= -1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

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


