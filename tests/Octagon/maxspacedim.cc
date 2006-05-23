/* Test Octagonal_Shape::max_space_dimension().
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
  Octagonal_Shape<mpq_class> oc1(1);
  Octagonal_Shape<long> oc2(1);
  Octagonal_Shape<int> oc3(1);
  Octagonal_Shape<signed char> oc4(1);

  dimension_type max_spacedim1 = oc1.max_space_dimension();
  dimension_type max_spacedim2 = oc2.max_space_dimension();
  dimension_type max_spacedim3 = oc3.max_space_dimension();
  dimension_type max_spacedim4 = oc4.max_space_dimension();

  nout << endl
       << "The maximum space-dimension of an octagon of Rational is: "
       << endl
       << max_spacedim1
       << endl;

  nout << endl
       << "The maximum space-dimension of an octagon of long is: "
       << endl
       << max_spacedim2
       << endl;

  nout << endl
       << "The maximum space-dimension of an octagon of int is: "
       << endl
       << max_spacedim3
       << endl;

  nout << endl
       << "The maximum space-dimension of an octagon of signed char is:"
       << endl
       << max_spacedim4
       << endl;

  if (max_spacedim1 < max_spacedim2) {

    print_constraints(oc1, "*** oc1 ***");
    print_constraints(oc2, "*** oc2 ***");

  }

  if (max_spacedim3 < max_spacedim4) {

    print_constraints(oc3, "*** oc3 ***");
    print_constraints(oc4, "*** oc4 ***");

  }

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN

