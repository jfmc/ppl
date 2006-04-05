/* Test Octagon::relation_with(g).
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

  TOctagon oct(2);
  oct.add_constraint(A >= 2);
  oct.add_constraint(A + B <= 3);

  Poly_Gen_Relation rel = oct.relation_with(ray(A + B));
  
#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(ray(A + B)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::nothing();
  
  if (rel != known_result) 
    exit(1);
}

static void test2() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A >= 2);
  oct.add_constraint(A + B <= 3);
  oct.add_constraint(A - B >= 0);

  Poly_Gen_Relation rel = oct.relation_with(ray(-2*B));
  
#if NOISY
  print_constraints(oct, "*** oct ***");
  cout << "oct.relation_with(ray(-2*B)) == " << rel << endl;
#endif

  Poly_Gen_Relation known_result = Poly_Gen_Relation::subsumes();
  
  if (rel != known_result) 
    exit(1);
}

int main() TRY {

  test1();
  test2();

  return 0;
} 
CATCH
