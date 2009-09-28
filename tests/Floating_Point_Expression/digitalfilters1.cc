/* Test Rate Limiter on differents abstract domains.
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

/*
This file tests a rate limiter that, given random input flows of floating
point values X and D, bounded respectively by [-128, 128] and [0, 16],
computes an output flow Y that tries to follow X while having a change rate
limited by D. The pseudo-code of such rate limiter is the following:

input X, D;
output Y;
R = (-inf, +inf);
S = (-inf, +inf);
for (n = 0; n < N; ++n) {
  S = Y;
  R = X - S;
  Y = X;
  if (R <= -D)
    Y = S - D;
  if (R >= D)
    Y = S + D;
}
*/

// tests rate limiter using intervals abstract domain.
bool
test01() {
  Variable X(0); //input
  Variable D(1); //input
  Variable Y(2); //output
  Variable S(3);
  Variable R(4);
  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval tmp(-128);
  tmp.join_assign(128);
  abstract_store.set_interval(X, tmp);
  abstract_store.set_interval(Y, tmp);
  tmp.lower() = 0;
  tmp.upper() = 16;
  abstract_store.set_interval(D, tmp);

  //if (R <= -D) Y = S - D;
  FP_Interval_Abstract_Store as_then(abstract_store);
  as_then.refine_with_constraint(R <= -D);
  as_then.set_interval(Y, abstract_store.get_interval(S)
                        - abstract_store.get_interval(D));
  abstract_store.refine_with_constraint(R > -D);
  abstract_store.upper_bound_assign(as_then);

  //if (R >= D)  Y = S + D;
  as_then = abstract_store;
  as_then.refine_with_constraint(R >= D);
  as_then.set_interval(Y, abstract_store.get_interval(S)
                        + abstract_store.get_interval(D));
  abstract_store.refine_with_constraint(R > D);
  abstract_store.upper_bound_assign(as_then);

  nout << "Y in " << abstract_store.get_interval(Y) << endl;
  return true;
}

// tests rate limiter using bounded differences abstract domain.
bool
test02() {

  return true;
}

// tests rate limiter using octagons abstract domain.
bool
test03() {

  return true;
}

// tests rate limiter using polyhedra abstract domain.
bool
test04() {

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  //DO_TEST(test02);
  //DO_TEST(test03);
  //DO_TEST(test04);
END_MAIN
