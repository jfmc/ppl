/* Remove the higher variables from the space.
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
  Variable x1(0);
  Variable x2(1);

  TBD_Shape bd(2);
  bd.add_constraint(x1 <= 2);
  bd.add_constraint(x2 <= 10);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(0);

  BD_Shape<mpq_class> known_result(0, UNIVERSE);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(0) ***");

  if (!ok)
    exit(1);
}

void
test2() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);

  TBD_Shape bd(4);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= -2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);
  bd.add_constraint(x4 >= 3);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(1);

  BD_Shape<mpq_class> known_result(1, EMPTY);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(1) ***");

  if (!ok)
    exit(1);
}

void
test3() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);
  Variable x5(4);

  TBD_Shape bd(5);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= 2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);
  bd.add_constraint(x4 >= 3);
  bd.add_constraint(x5 - x3 == 2);

  print_constraints(bd, "*** bd ***");

  bd.remove_higher_space_dimensions(3);

  BD_Shape<mpq_class> known_result(3);
  known_result.add_constraint(x1 - x2 <=1);
  known_result.add_constraint(x2 - x3 <= 2);
  known_result.add_constraint(x3 - x1 <= 0);
  known_result.add_constraint(x2 >= 5);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(3) ***");

  if (!ok)
    exit(1);
}

void
test4() {
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  TBD_Shape bd(3);
  bd.add_constraint(x1 - x2 <=1);
  bd.add_constraint(x2 - x3 <= 2);
  bd.add_constraint(x3 - x1 <= 0);
  bd.add_constraint(x2 >= 5);

  print_constraints(bd, "*** bd ***");

  BD_Shape<mpq_class> known_result(bd);

  bd.remove_higher_space_dimensions(3);

  bool ok = (BD_Shape<mpq_class>(bd) == known_result);

  print_constraints(bd, "*** bd.remove_higher_space_dimensions(3) ***");

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
