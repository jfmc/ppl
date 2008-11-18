/* Test Partially_Reduced_Product<>:: Shrink_Using_Congruences_Reduction()
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

using namespace Parma_Polyhedra_Library::IO_Operators;

typedef NNC_Polyhedron Poly;

typedef Domain_Product<Poly, Grid>::Shrink_Using_Congruences_Product SUCProduct;
namespace {

// Shrink_Using_Congruences_Reduction with non-strict constraints and
// equality found. Positive coefficients.
bool
test01() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(3*A >= 7);
  cs.insert(3*A <= 16);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2);

  known_sucp.refine_with_constraint(A == 4);
  known_sucp.refine_with_congruence((A %= 0)/ 2);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

  return ok;
}

// Shrink_Using_Congruences_Reduction with non-strict constraints and
// equality found. Negative coefficients.
bool
test02() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(3*A >= -10);
  cs.insert(2*A <= -3);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2);

  known_sucp.refine_with_constraint(A == -2);
  known_sucp.refine_with_congruence((A %= 0)/ 2);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

  return ok;
}

// Shrink_Using_Congruences_Reduction with non-strict constraints and
// equality found.
bool
test03() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(A >= 0);
  cs.insert(A <= 3);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2);

  known_sucp.refine_with_constraints(cs);
  known_sucp.refine_with_congruence((A %= 0)/ 2);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

  return ok;
}

// Shrink_Using_Congruences_Reduction with strict lower bound and an equality
// is found.
bool
test04() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(A > 0);
  cs.insert(A <= 3);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2);

  known_sucp.refine_with_constraint(A == 2);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

  return ok;
}

// Shrink_Using_Congruences_Reduction with strict upper bound and an equality
// is found.
bool
test05() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(A < 3);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2);

  known_sucp.refine_with_constraint(A == 2);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

  return ok;
}

// Shrink_Using_Congruences_Reduction where emptiness is found.
bool
test06() {
  Variable A(0);
  Variable B(1);

  SUCProduct sucp(2);
  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(A < 2);
  sucp.refine_with_constraints(cs);
  sucp.refine_with_congruence((A %= 0)/ 2);

  SUCProduct known_sucp(2, EMPTY);

  bool ok = sucp.OK();

  print_congruences(sucp, "*** after ok check: sucp congruences ***");
  print_constraints(sucp, "*** after ok check: sucp constraints ***");

  if (ok) {
    ok = ok && sucp == known_sucp;

    print_congruences(sucp, "*** after known_sucp check: sucp congruences ***");
    print_constraints(sucp, "*** after known_sucp check: sucp constraints ***");
  }

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
END_MAIN
