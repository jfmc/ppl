/* Test BD_Shape::relation_with().
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 1);

  try {
    // This is an incorrect use of function
    // BD_Shape::relation_with(c):
    // it is illegal to use a constraint that is
    // dimensional incompatible with the BDS.
    Poly_Con_Relation rel = bd.relation_with(C - B <= 2);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(3);
  bd.add_constraint(A >= 1);

  try {
    // This is an incorrect use of function
    // BD_Shape::relation_with(c):
    // it is illegal to use a constraint that is
    // not a bounded difference.
    Poly_Con_Relation rel = bd.relation_with(A - 2*B <= 2);
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 1);

  try {
    // This is an incorrect use of function
    // BD_Shape::relation_with(c):
    // it is illegal to use a generator that is
    // dimensional incompatible with the BDS.
    Poly_Gen_Relation rel = bd.relation_with(ray(C));
  }
  catch (std::invalid_argument& e) {
    nout << "std::invalid_argument: " << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
END_MAIN
