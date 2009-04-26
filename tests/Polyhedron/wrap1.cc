/* Test Polyhedron::wrap_assign().
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
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2, UNIVERSE);
  ph.add_constraint(x + 1024 == 8*y);
  ph.add_constraint(-64 <= x);
  ph.add_constraint(x <= 448);

  print_constraints(ph, "*** ph ***");

  Variables_Set vars(x, y);

  ph.wrap_assign(vars, BITS_8, UNSIGNED, OVERFLOW_WRAPS, true, 10);

  print_constraints(ph.minimized_constraints(), "*** ph.wrap_assign(...) ***");

  bool ok = true;

  return ok;
}

bool
test02() {
  // FIXME: to be written.
  bool ok = true;

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
