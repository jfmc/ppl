/* Test BDiffs::relation_with(c).
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

  TBD_Shape bd(1);
  bd.add_constraint(A >= 0);

  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(1) >= 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 >= 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  if (rel != known_result) 
    exit(1);
}

static void test2() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A == 1);
  bd.add_constraint(B >= 2);
  
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(1) > 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 > 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  if (rel != known_result) 
    exit(1);
}

static void test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A == 1);
  bd.add_constraint(B >= 2);
  bd.add_constraint(C <= 1);
   
  Poly_Con_Relation rel = bd.relation_with(Linear_Expression(1) == 1);

#if NOISY
  print_constraints(bd, "--- bd ---");
  cout << "bd.relation_with(1 == 1) == " << rel << endl;
#endif

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  if (rel != known_result) 
    exit(1);
}

int main() TRY {
  test1();
  test2();
  test3();

  return 0;
}
CATCH


