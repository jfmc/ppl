/* Checking if Polynomial_spaces are the bottom or the top.
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
  Variable x1(0);
  Variable x2(1);

  // Testing if the default polynomial space is the top.

  Polynomial_Space<3> ps1(2);

  bool known_result = true;

  bool ok = (ps1.is_top() == known_result);

  return ok;
}

bool
test02() {
  Variable x1(0);
  Variable x2(1);

  // Testing if the default polynomial space is not the bottom.

  Polynomial_Space<3> ps1(2);

  bool known_result = false;

  bool ok = (ps1.is_bottom() == known_result);

  return ok;
}

bool
test03() {
  Variable x1(0);
  Variable x2(1);

  // Testing if the bottom polynomial space is indeed the bottom.

  Polynomial_Space<3> ps1(2, Polynomial_Space<3>::BOTTOM);

  bool known_result = true;

  bool ok = (ps1.is_bottom() == known_result);

  return ok;
}

bool
test04() {
  Variable x1(0);
  Variable x2(1);

  // Testing if the bottom polynomial space is not the top.

  Polynomial_Space<3> ps1(2, Polynomial_Space<3>::BOTTOM);

  bool known_result = false;

  bool ok = (ps1.is_top() == known_result);

  return ok;
}

bool
test05() {
  Variable x1(0);
  Variable x2(1);

  // Testing if the polynomial space space(x1) is not the bottom.

  Polynomial_Space<3> ps1(2);
  ps1.add_constraint(Linear_Expression(x1) == 0);

  bool known_result = false;

  bool ok = (ps1.is_bottom() == known_result);

  return ok;
}

bool
test06() {
  Variable x1(0);
  Variable x2(1);

  // Testing if the polynomial space space(x1) is not the top.

  Polynomial_Space<3> ps1(2);
  ps1.add_constraint(Linear_Expression(x1) == 0);

  bool known_result = false;

  bool ok = (ps1.is_top() == known_result);

  return ok;
}


} // namespace

BEGIN_MAIN
  DO_TEST(test01)
  DO_TEST(test02)
  DO_TEST(test03)
  DO_TEST(test04)
  DO_TEST(test05)
  DO_TEST(test06)
END_MAIN
