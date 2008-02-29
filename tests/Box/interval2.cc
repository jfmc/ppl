/* Test Box::add_space_dimensions_and_embed():
   we add two variables to a Box.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

typedef Interval<long double, Floating_Point_Real_Interval_Info> Float_Interval;

bool
test01() {
  Float_Interval x;
  assign(x, 2);
  Float_Interval two;
  assign(two, 2);
  Float_Interval y;

  for (int i = 0; i <= 10000; ++i) {
    nout << "x = " << x << endl;
    // Compute x = (x+(2/x))/2.
    div_assign(y, two, x);
    add_assign(x, x, y);
    div_assign(x, x, two);
  }

  return true;
}

typedef double number_type;
typedef Float_Interval interval_type;

bool contains(const std::complex<interval_type>& i,
              const std::complex<number_type>& s) {
  return contains(i.real(), s.real()) && contains(i.imag(), s.imag());
}

void
polynomial_evaluate(const std::vector<number_type>& P,
                    const std::complex<number_type>& x,
                    std::complex<number_type>& P_x,
                    const std::vector<interval_type>& iP,
                    const std::complex<interval_type>& ix,
                    std::complex<interval_type>& iP_x) {
  // Note: the coefficient of the leading term is implicitly 1.
  P_x = std::complex<number_type>(number_type(1.0), number_type(0.0));
  iP_x = std::complex<interval_type>(interval_type(1.0), interval_type(0.0));
  assert(contains(iP_x, P_x));
  for (int i = P.size(); i >= 1; --i) {
    P_x = P_x*x + P[i-1];
    iP_x = iP_x*ix + iP[i-1];
    assert(contains(iP_x, P_x));
  }
}

void
solve(const std::vector<number_type>& P,
      std::vector<std::complex<number_type> >& roots,
      const std::vector<interval_type>& iP,
      std::vector<std::complex<interval_type> >& iroots) {
  const unsigned degree = P.size();
  assert(degree == iP.size());
  if (degree < 1)
    throw std::invalid_argument("the polynomial must have degree at least 1");

  // Initial estimates are given by roots of unity.
  std::vector<std::complex<number_type> >x(5);
  std::vector<std::complex<interval_type> >ix(5);
  double theta = 2*M_PI/degree;
  for (unsigned i = 0; i < degree; ++i) {
    x[i] = std::complex<number_type>(number_type(cos(i*theta)),
                                     number_type(sin(i*theta)));
    ix[i] = std::complex<interval_type>(interval_type(cos(i*theta)),
                                        interval_type(sin(i*theta)));
    assert(contains(ix[i], x[i]));
  }

  while (true) {
    for (unsigned i = 0; i < degree; ++i) {
      nout << "x[" << i << "] = " << x[i] << endl;
    }
    for (unsigned i = 0; i < degree; ++i) {
      std::complex<number_type> P_x_i;
      std::complex<interval_type> iP_x_i;
      polynomial_evaluate(P, x[i], P_x_i, iP, ix[i], iP_x_i);
      std::complex<number_type> d(number_type(1.0), number_type(0.0));
      std::complex<interval_type> id(interval_type(1.0), interval_type(0.0));
      assert(contains(id, d));
      for (unsigned j = 0; j < degree; ++j)
        if (i != j) {
          assert(contains(id, d));
          assert(contains(ix[i], x[i]));
          assert(contains(ix[j], x[j]));
          d *= (x[i] - x[j]);
          id *= (ix[i] - ix[j]);
          assert(contains(id, d));
        }
      P_x_i /= d;
      iP_x_i /= id;
      assert(contains(iP_x_i, P_x_i));
      x[i] -= P_x_i;
      ix[i] -= iP_x_i;
      assert(contains(ix[i], x[i]));
    }
  }
  roots.resize(degree+1);
  iroots.resize(degree+1);
  for (unsigned i = 0; i < degree; ++i) {
    roots[i] = x[i];
    iroots[i] = ix[i];
  }
}


bool test02() {
  std::vector<number_type> P(4);
  // x^4 + 5*x^3 + 7*x^2 + 134*x + 1
  P[3] = 5;
  P[2] = 7;
  P[1] = 134;
  P[0] = 1;
  std::vector<std::complex<number_type> > roots;
  std::vector<interval_type> iP(4);
  // x^4 + 5*x^3 + 7*x^2 + 134*x + 1
  iP[3] = 5;
  iP[2] = 7;
  iP[1] = 134;
  iP[0] = 1;
  std::vector<std::complex<interval_type> > iroots;
  solve(P, roots, iP, iroots);
  return true;
}

bool test03() {
  std::vector<number_type> P(2);
  // x^2 - 1
  P[1] = 0;
  P[0] = -1;
  std::vector<std::complex<number_type> > roots;
  std::vector<interval_type> iP(2);
  // x^2 - 1
  iP[1] = 0;
  iP[0] = -1;
  std::vector<std::complex<interval_type> > iroots;
  solve(P, roots, iP, iroots);
  return true;
}

#if 0
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
#endif

} // namespace

BEGIN_MAIN
//DO_TEST(test01);
//DO_TEST(test02);
  DO_TEST(test03);
  //DO_TEST(test04);
END_MAIN
