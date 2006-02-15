/* Test BD_Shape::max_space_dimension().
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

bool
test01() {
  BD_Shape<mpq_class> bd1(1);
  BD_Shape<long> bd2(1);
  BD_Shape<int> bd3(1);
  BD_Shape<signed char> bd4(1);

  dimension_type max_spacedim1 = bd1.max_space_dimension();
  dimension_type max_spacedim2 = bd2.max_space_dimension();
  dimension_type max_spacedim3 = bd3.max_space_dimension();
  dimension_type max_spacedim4 = bd4.max_space_dimension();

  nout << endl
       << "The maximum space-dimension of a system of bounded differences "
       << endl
       << "of Rational is: "
       << endl
       << max_spacedim1
       << endl; 

  nout << endl
       << "The maximum space-dimension of a system of bounded differences "
       << endl
       << "of long: "
       << endl
       << max_spacedim2
       << endl; 

  nout << endl
       << "The maximum space-dimension of a system of bounded differences "
       << endl
       << "of int: "
       << endl
       << max_spacedim3
       << endl; 

  nout << endl
       << "The maximum space-dimension of a system of bounded differences "
       << endl
       << "of signed char"
       << endl
       << max_spacedim4
       << endl;

  if (max_spacedim1 < max_spacedim2) {

    print_constraints(bd1, "*** bd1 ***");
    print_constraints(bd2, "*** bd2 ***");

  }

  if (max_spacedim3 < max_spacedim4) {

    print_constraints(bd3, "*** bd3 ***");
    print_constraints(bd4, "*** bd4 ***");

  }
  // FIXME!!!
  return true;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
END_MAIN
