/* Getting the polynomial constraints of a Polynomial_Space.
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

  // Testing that the Polynomial_Space built from the constraints of
  // a Polynomial_Space is the same as the original one.

  Polynomial_Space<2> ps1(2);
  ps1.add_constraint(Linear_Expression(x1) == 0);
  ps1.add_constraint(Linear_Expression(x2) == 0);

  Polynomial_Space<2> known_result(ps1.space_dimension(),
				   ps1.polynomial_constraints());

  bool ok = (ps1 == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01)
END_MAIN
