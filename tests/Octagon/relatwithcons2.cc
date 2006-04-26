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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

namespace {

bool 
test01() {
  // The zero-dim universe octagon.
  TOctagon oct(0);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(0) > 0);

  using namespace IO_Operators;
  nout << "oct.relation_with(0 > 0) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test02() {
  // The zero-dim universe octagon.
  TOctagon oct(0);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(0) > 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(0 > 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test03() {
  // The zero-dim universe octagon.
  TOctagon oct(0);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(1) > 0);

  using namespace IO_Operators;
  nout << "oct.relation_with(1 > 0) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test04() {
  Variable A(0);

  // An empty octagon.
  TOctagon oct(1, EMPTY);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(A > 0);

  using namespace IO_Operators;
  nout << "oct.relation_with(A > 0) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included()
    && Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test05() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs(A + B == 3);
  TOctagon oct(cs);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(A + B > 3);

  using namespace IO_Operators;
  nout << "oct.relation_with(A + B > 3) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test06() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs(A + B <= 3);
  TOctagon oct(cs);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(A + B > 3);

  using namespace IO_Operators;
  nout << "oct.relation_with(A + B > 3) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test07() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A + B <= 3);
  TOctagon oct(cs);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(A + B < 10);

  using namespace IO_Operators;
  nout << "oct.relation_with(A + B < 10) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test08() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A + B <= 3);
  TOctagon oct(cs);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(A + B > 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(A + B > 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::strictly_intersects();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test09() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(B >= 0);
  cs.insert(A + B <= 3);
  TOctagon oct(cs);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(B - A > 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(B - A > 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test10() {
  Variable A(0);

  TOctagon oct(1);
  oct.add_constraint(A >= 0);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(1) >= 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(1 >= 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test11() {
  Variable A(0);
  Variable B(1);

  TOctagon oct(2);
  oct.add_constraint(A == 1);
  oct.add_constraint(B >= 2);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(1) > 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(1 > 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_disjoint();

  bool ok = (rel == known_result);

  return ok;
}

bool  
test12() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oct(3);
  oct.add_constraint(A == 1);
  oct.add_constraint(B >= 2);
  oct.add_constraint(C <= 1);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(1) == 1);

  using namespace IO_Operators;
  nout << "oct.relation_with(1 == 1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::saturates()
    && Poly_Con_Relation::is_included();

  bool ok = (rel == known_result);

  return ok;
}

bool 
test13() {
  Variable A(0);

  TOctagon oct(1);
  oct.add_constraint(A >= 0);

  print_constraints(oct, "--- oct ---");

  Poly_Con_Relation rel = oct.relation_with(Linear_Expression(0) >= -1);

  using namespace IO_Operators;
  nout << "oct.relation_with(0 >= -1) == " << rel << endl;

  Poly_Con_Relation known_result = Poly_Con_Relation::is_included();

  bool ok = (rel == known_result);

  return ok;
}


} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
END_MAIN

