/* Test BD_Shape::add_space_dimensions_and_embed() and
   BD_Shape::add_space_dimensions_and_project().
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

void
test1() {
  TBD_Shape bd1(0, UNIVERSE);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_embed(3);

  BD_Shape<mpq_class> known_result(3, UNIVERSE);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimension_and_embed(3) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(0);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(4);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(4) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TBD_Shape bd1(3);
  bd1.add_constraint(A == 1);
  bd1.add_constraint(C - B >= 9);

  BD_Shape<mpq_class> known_result(bd1);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(0);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(0) ***");

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  if (!ok)
    exit(1);
}

void
test4() {
  //Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  TBD_Shape bd1(1);

  print_constraints(bd1, "*** bd1 ***");

  bd1.add_space_dimensions_and_project(3);

  BD_Shape<mpq_class> known_result(4);
  known_result.add_constraint(B == 0);
  known_result.add_constraint(C == 0);
  known_result.add_constraint(D == 0);

  bool ok = (BD_Shape<mpq_class>(bd1) == known_result);

  print_constraints(bd1, "*** bd1.add_space_dimensions_and_project(3) ***");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  DO_TEST(test1);
  DO_TEST(test2);
  DO_TEST(test3);
  DO_TEST(test4);

  return 0;
}
CATCH
