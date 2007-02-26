/* Test Box::add_space_dimensions_and_embed():
   we add two variables to a Box.
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
#include <complex>
#include <cmath>
#include <cstdio>

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

  for (int i = 0; i <= 100; ++i) {
    nout << "x = " << x << endl;
    // Compute x = (x+(2/x))/2.
    div_assign(y, two, x);
    add_assign(x, x, y);
    div_assign(x, x, two);
  }

  return true;
}

//typedef double number_type;
typedef Float_Interval number_type;

void
polynomial_evaluate(const std::vector<number_type>& P,
		    const std::complex<number_type>& x,
		    std::complex<number_type>& P_x) {
  // Note: the coefficient of the leading term is implicitly 1.
  P_x = std::complex<number_type>(number_type(1.0), number_type(0.0));
  for (int i = P.size(); i >= 1; --i)
    P_x = P_x*x + P[i-1];
}

void
solve(const std::vector<number_type>& P,
      std::vector<std::complex<number_type> >& roots) {
  const int degree = P.size();
  if (degree < 1)
    throw std::invalid_argument("the polynomial must have degree at least 1");

  // Initial estimates are given by roots of unity.
  std::vector<std::complex<number_type> >x(5);
  double theta = 2*M_PI/degree;
  for (int i = 0; i < degree; ++i)
    x[i] = std::complex<number_type>(number_type(cos(i*theta)),
					number_type(sin(i*theta)));

  while (true) {
    for (int i = 0; i < degree; ++i)
      nout << "x[" << i << "] = " << x[i] << endl;
    for (int i = 0; i < degree; ++i) {
      std::complex<number_type> P_x_i;
      polynomial_evaluate(P, x[i], P_x_i);
      std::complex<number_type> d(number_type(1.0), number_type(0.0));
      for (int j = 0; j < degree; ++j)
	if (i != j)
	  d *= (x[i] - x[j]);
      P_x_i /= d;
      x[i] -= P_x_i;
    }
  }
  roots.resize(degree+1);
  for (int i = 0; i < degree; ++i)
    roots[i] = x[i];
}


bool test02() {
  std::vector<number_type> P(4);
  // x^4 + 5*x^3 + 7*x^2 + 134*x + 1
  P[3] = 5;
  P[2] = 7;
  P[1] = 134;
  P[0] = 1;
  std::vector<std::complex<number_type> > roots;
  solve(P, roots);
  return true;
}

bool test03() {
  std::vector<number_type> P(2);
  // x^2 - 1
  P[1] = 0;
  P[0] = -1;
  std::vector<std::complex<number_type> > roots;
  solve(P, roots);
  return true;
}

bool test04() {
  std::vector<number_type> P(2);
  // x^2 - 1
  P[1] = 0;
  P[0] = -1;
  for (double d = 0.0; d <= 10.0; d += 1.0) {
    std::complex<number_type> P_x_i;
    polynomial_evaluate(P,
			std::complex<number_type>(number_type(d),
						  number_type(0.0)),
			P_x_i);
    nout << d << " " << P_x_i << endl;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  //DO_TEST(test01);
  //DO_TEST(test02);
  DO_TEST(test03);
  //DO_TEST(test04);
END_MAIN
