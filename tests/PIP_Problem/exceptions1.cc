/* Test that the right exceptions are thrown in case of incorrect uses.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

bool
test01() {
  Constraint_System cs;
  Variable x(0);
  Variables_Set params(x);

  try {
    // This is an incorrect use of the constructor:
    // the parameters in `params' should be space dimensions
    // that are valid for the PIP_Problem we are going to build.
    PIP_Problem pip(0, cs.begin(), cs.end(), params);
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test02() {
  Constraint_System cs;

  try {
    // This is an incorrect use of the constructor:
    // invalid space dimension required.
    PIP_Problem pip(1 + PIP_Problem::max_space_dimension(),
                    cs.begin(), cs.end(), Variables_Set());
  }
  catch (std::length_error& e) {
    nout << "length_error: " << e.what() << endl << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test03() {
  try {
    // This is an incorrect use of the constructor:
    // invalid space dimension required.
    PIP_Problem pip(1 + PIP_Problem::max_space_dimension());
  }
  catch (std::length_error& e) {
    nout << "length_error: " << e.what() << endl << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test04() {
  Variable X(0);
  Constraint_System cs;
  cs.insert(X < 1);

  try {
    // This is an incorrect use of the constructor:
    // strict constraints are not allowed.
    PIP_Problem pip(cs.space_dimension(),
                    cs.begin(), cs.end(),
                    Variables_Set());
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
    return true;
  }
  catch (...) {
  }
  return false;
}

bool
test05() {
  Variable X(0);
  Variable Y(1);
  Variable Z(2);
  Constraint_System cs;
  cs.insert(X == 0);
  cs.insert(Y == 1);
  cs.insert(Z <= 2);

  try {
    // This is an incorrect use of the constructor:
    // the space dimensions of the constraints should not be greater
    // than the space dimension of the PIP problem.
    PIP_Problem pip(2, cs.begin(), cs.end(), Variables_Set());
  }
  catch (std::invalid_argument& e) {
    nout << "invalid_argument: " << e.what() << endl << endl;
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
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN
