/* Test that the right exceptions are thrown in case of incorrect uses.
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

using std::invalid_argument;
using std::length_error;
using std::domain_error;

namespace {

void
test01() {
  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 6);
  cs.insert(A > -6);
  LP_Problem lp(cs.space_dimension());

  try {
    // This tries to build an invalid LP_Problem object: the feasible
    // region can not be defined using strict inequalities.
    lp.add_constraints(cs);

    exit(1);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test02() {
  Variable A(0);
  LP_Problem lp;

  try {
    // This tries to build an invalid LP_Problem object: the space dimension
    // of the objective function can not be greater than the space dimension
    // of the feasible region.
    lp.set_objective_function(A);

    exit(1);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test03() {
  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 6);
  cs.insert(A <= 0);
  LP_Problem lp(cs.space_dimension(), cs, A, MAXIMIZATION);

  try {
    // We cannot extract a feasible point from an unsatisfiable LP_Problem.
    Generator fp = lp.feasible_point();

    exit(1);
  }
  catch (domain_error& e) {
    nout << "domain_error: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test04() {
  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 6);
  LP_Problem lp(cs.space_dimension(), cs, A, MAXIMIZATION);

  try {
    // We cannot extract an optimizing point from an unbounded LP_Problem.
    Generator fp = lp.optimizing_point();

    exit(1);
  }
  catch (domain_error& e) {
    nout << "domain_error: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test05() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs;
  cs.insert(A >= 6);
  LP_Problem lp(cs.space_dimension(), cs, A, MAXIMIZATION);
  Generator p = point(A + B);
  Coefficient num;
  Coefficient den;

  try {
    // This tries to evaluate the objective function on a space dimension
    // incompatible generator.
    lp.evaluate_objective_function(p, num, den);

    exit(1);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test06() {
  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 6);
  LP_Problem lp(cs.space_dimension(), cs, A, MAXIMIZATION);
  Generator r = ray(A);
  Coefficient num;
  Coefficient den;

  try {
    // This tries to evaluate the objective function on a ray.
    lp.evaluate_objective_function(r, num, den);

    exit(1);
  }
  catch (invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test07() {
  try {
    // This tries to overflow the maximum space dimension.
    LP_Problem lp(LP_Problem::max_space_dimension() + 1);

    exit(1);
  }
  catch (length_error& e) {
    nout << "length_error: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

void
test08() {
  LP_Problem lp(1);
  try {
    // This tries to overflow the maximum space dimension.
    lp.add_space_dimensions_and_embed(LP_Problem::max_space_dimension());

    exit(1);
  }
  catch (length_error& e) {
    nout << "length_error: " << e.what() << endl << endl;
  }
  catch (...) {
    exit(1);
  }
}

} // namespace

int
main() TRY {
  set_handlers();

  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();

  return 0;
}
CATCH
