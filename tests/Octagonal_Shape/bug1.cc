/* Test Octagonal_Shape::affine_preimage().
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
bug1() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Octagonal_Shape<float> oc(3);
  oc.add_constraint(A <= -1);

  print_constraints(oc, "*** oc ***");

  oc.affine_preimage(A, 3*A, 2);

  Octagonal_Shape<mpq_class> mpq_known_result(3);
  mpq_known_result.add_constraint(3*A <= -2);

  bool ok = check_result(oc, mpq_known_result,
			 "9.54e-8", "9.54e-8", "9.54e-8");

  print_constraints(oc, "*** oc.affine_preimage(A, 3*A, 2) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(bug1);
END_MAIN
