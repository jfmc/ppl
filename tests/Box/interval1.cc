/* Test Box::add_space_dimensions_and_embed():
   we add two variables to a Box.
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

struct Floating_Point_Real_Interval_Info_Policy {
  const_bool(store_unbounded, false);
  const_bool(store_open, true);
  const_bool(store_integer, false);
  const_bool(store_empty, true);
  const_bool(store_singleton, false);
  const_int(next_bit, 0);
  const_bool(handle_infinity, false);
  const_bool(check_inexact, false);
  const_bool(check_empty_args, false);
  const_bool(check_integer_args, false);
};

typedef Interval_Info_Bitset<unsigned int,
			     Floating_Point_Real_Interval_Info_Policy>
Floating_Point_Real_Interval_Info;

typedef Interval<double, Floating_Point_Real_Interval_Info> Float_Interval;

bool
test01() {
  Float_Interval x;
  assign(x, 2);
  Float_Interval two;
  assign(two, 2);
  Float_Interval y;

  while (true) {
    nout << "x = " << x << endl;
    // Compute x = (x+(2/x))/2.
    div_assign(y, two, x);
    add_assign(x, x, y);
    div_assign(x, x, two);
  }

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
