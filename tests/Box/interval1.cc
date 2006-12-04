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
  const_bool_nodef(store_special, false);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, false);
  const_bool_nodef(may_contain_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef Interval_Restriction_None<Interval_Info_Bitset<unsigned int, Floating_Point_Real_Interval_Info_Policy> > Floating_Point_Real_Interval_Info;

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
