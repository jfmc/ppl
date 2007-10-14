/* Maps the space dimensions of a polynomial space.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

  // Bottom polynomial space, undefined mapping.

  Partial_Function function;
  Polynomial_Space<2> ps(3, Polynomial_Space<2>::BOTTOM);

  ps.map_space_dimensions(function);

  Polynomial_Space<2> known_result(0, Polynomial_Space<2>::BOTTOM);
  bool ok = (ps == known_result);

  return ok;
}

bool
test02() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);

  // Mapping all dimensions.
  Partial_Function function;
  function.insert(0, 2);
  function.insert(2, 0);
  function.insert(1, 1);

  Polynomial_Space<2> ps(3);
  ps.add_constraint(Linear_Expression(1 + x1 - x2) == 0);
  ps.add_polynomial_constraint(Polynomial(x1*x2) == 4);

  ps.map_space_dimensions(function);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(1 + x1 - x0) == 0);
  known_result.add_polynomial_constraint(Polynomial(x0*x1) == 4);

  bool ok = (ps == known_result);

  return ok;
}

bool
test03() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);

  // Mapping all dimensions to themselves.
  Partial_Function function;
  function.insert(0, 0);
  function.insert(1, 1);
  function.insert(2, 2);

  Polynomial_Space<2> ps(3);
  ps.add_constraint(Linear_Expression(1 + x1 - x2) == 0);
  ps.add_polynomial_constraint(Polynomial(x1*x2) == 4);

  ps.map_space_dimensions(function);

  Polynomial_Space<2> known_result(3);
  known_result.add_constraint(Linear_Expression(1 + x1 - x2) == 0);
  known_result.add_polynomial_constraint(Polynomial(x1*x2) == 4);

  bool ok = (ps == known_result);

  return ok;
}

bool
test04() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);

  // Partial function.
  Partial_Function function;
  function.insert(0, 1);
  function.insert(2, 0);

  Polynomial_Space<2> ps(3);
  ps.add_constraint(Linear_Expression(1 + 2*x0 - x2) == 0);
  ps.add_polynomial_constraint(Polynomial(x1*x2) == 4);

  ps.map_space_dimensions(function);

  Polynomial_Space<2> known_result(2);
  known_result.add_constraint
    (Linear_Expression(1 + 2*x1 - x0) == 0);

  bool ok = (ps == known_result);

  return ok;
}

bool
test05() {
  Variable x0(0);
  Variable x1(1);
  Variable x2(2);

  // Removing all dimensions.
  Partial_Function function;

  Polynomial_Space<2> ps(3);
  ps.add_constraint(Linear_Expression(1 + 2*x0 - x2) == 0);
  ps.add_polynomial_constraint(Polynomial(x1*x2) == 4);

  ps.map_space_dimensions(function);

  Polynomial_Space<2> known_result(0);

  bool ok = (ps == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
  DO_TEST(test03)
  DO_TEST(test04)
  DO_TEST(test05)
END_MAIN
