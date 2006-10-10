/* Test the Variables_Set class.
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
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);

  Variables_Set v_set(A, F);
  v_set.insert(C);
  v_set.insert(B);
  v_set.insert(G);
  // FIXME: Here, and in the other test, use operator==.
  return v_set.size() == Variables_Set(A, G).size();
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);

  Variables_Set v_set(F, A);
  return v_set.empty();
}

bool
test03() {
  Variable first(Variable::max_space_dimension()-1);
  Variable last(0);
  Variables_Set v_set(first, last);
  return v_set.empty();
}

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);

  Variables_Set vs1(A, C);
  Variables_Set vs2(E, I);
  Variables_Set vs_union;
  Variables_Set vs_difference;
  Variables_Set vs_intersection;
  std::set_union(vs1.begin(), vs1.end(),
		 vs2.begin(), vs2.end(),
		 std::inserter(vs_union, vs_union.begin()),
		 Variable::Compare());
  if (vs_union.size() != 8)
    return false;

  std::set_difference(vs1.begin(), vs1.end(),
		      vs2.begin(), vs2.end(),
		      std::inserter(vs_difference, vs_difference.begin()),
		      Variable::Compare());
  if (vs_difference.size() != 3)
    return false;

  std::set_intersection(vs1.begin(), vs1.end(),
			vs2.begin(), vs2.end(),
			std::inserter(vs_intersection,
				      vs_intersection.begin()),
			Variable::Compare());
  return vs_intersection.empty();
}


bool
test05() {
  Variable A(0);

  Variables_Set v_set;

  if (!v_set.empty())
    return false;

  v_set.insert(Variable(A));
  if (v_set.space_dimension() != 1)
    return false;

  v_set.insert(Variable(Variable::max_space_dimension()-1));
  return v_set.space_dimension() == Variable::max_space_dimension();
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN
