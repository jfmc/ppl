/* Test operators on Term, Monomial, Polinomial and Polynomial_Constraint.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

using namespace IO_Operators;

namespace {

// Test operators on Term.
bool
test01() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Term t1 = x;
  Term t2 = x*x;
  Term t3 = t2*y;
  Term t4 = y*t3;
  t1 *= z;
  t2 *= t3;
  Term t5 = pow(x, 4);
  t5 *= z;
  Term t6 = pow(t5, 3);
  pow_assign(t6, 2);

  bool ok = (t1 == x * z
	     && t2 == pow(x, 4) * y
	     && t3 == pow(x, 2) * y
	     && t4 == pow(x, 2) * pow(y, 2)
	     && t5 == pow(x, 4) * z
	     && t6 == pow(x, 24) * pow(z, 6));

  return ok;
}

// Test operators on Monomial.
bool
test02() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Term t1 = x;
  Term t2 = x*x;
  Monomial m0(7);
  Monomial m1(x);
  Monomial m2(t2);
  Monomial m3 = m1*3;
  Monomial m4 = 4*m2;
  Monomial m5 = m3*y;
  Monomial m6 = y*m4;
  Monomial m7 = m5*t1;
  Monomial m8 = t2*m6;
  m3 *= 5;
  m4 *= t1;
  m5 *= m1;
  Monomial m9 = pow(m2, 4);
  pow_assign(m9, 2);
  bool ok = (m0 == Monomial(7)
 	     && m1 == Monomial(x)
 	     && m2 == Monomial(pow(x, 2))
 	     && m3 == 15 * Monomial(x)
  	     && m4 == Monomial(4) * pow(x, 3)
	     && m5 == Monomial(3) * pow(x, 2) * y
	     && m6 == Monomial(4) * pow(x, 2) * y
	     && m7 == Monomial(3) * pow(x, 2) * y
 	     && m8 == Monomial(4) * pow(x, 4) * y
	     && m9 == Monomial(pow(x, 16)));
  return ok;
}

// Test operators on Polynomial.
bool
test03() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Term t1 = x;
  Term t2 = x*x;
  Monomial m1(x);
  Monomial m2(t2);
  Polynomial p1(3);
  Polynomial p2(y);
  Polynomial p3 = t1;
  Polynomial p4 = m1;

  Polynomial p5 = p1*7;
  Polynomial p6 = 5*p2;
  Polynomial p7 = p3*y;
  Polynomial p8 = z*p4;
  Polynomial p9 = p5*t1;
  Polynomial pa = t2*p6;
  Polynomial pb = p7*m1;
  Polynomial pc = m2*p8;
  Polynomial pd = pa*pb;

  Polynomial pe = p1 + 7;
  Polynomial pf = 5 + p2;
  Polynomial pg = p3 + y;
  Polynomial ph = z + p4;
  Polynomial pi = p5 + t1;
  Polynomial pj = t2 + p6;
  Polynomial pk = p7 + m1;
  Polynomial pl = m2 + p8;
  Polynomial pm = pa + pb;

  Polynomial pn = p1 - 7;
  Polynomial po = 5 - p2;
  Polynomial pp = p3 - y;
  Polynomial pq = z - p4;
  Polynomial pr = p5 - t1;
  Polynomial ps = t2 - p6;
  Polynomial pt = p7 - m1;
  Polynomial pu = m2 - p8;
  Polynomial pv = pa - pb;
  Polynomial pw = pow(pv, 3);

  p5 *= 7;
  p7 *= y;
  p9 *= t1;
  pb *= m1;
  pd *= pa;

  pe += 7;
  pg += y;
  pi += t1;
  pk += m1;
  pm += pb;

  pn -= 7;
  pp -= y;
  pr -= t1;
  pt -= m1;
  pv -= pa;
  pow_assign(pw, 2);

  Polynomial pbug;
  Polynomial pbug2 = Coefficient_one() - pbug;
  assert(pbug2.is_equal_to(Polynomial(Coefficient_one())));

  bool ok = (p1.is_equal_to(Polynomial(3))
	     && p2.is_equal_to(Polynomial(y))
	     && p3.is_equal_to(Polynomial(x))
	     && p4.is_equal_to(Polynomial(x))
	     && p5.is_equal_to(Polynomial(147))
	     && p6.is_equal_to(5 * y)
	     && p7.is_equal_to(x * pow(y, 2))
	     && p8.is_equal_to(x * z)
 	     && p9.is_equal_to(21 * pow(x, 2))
 	     && pa.is_equal_to(5 * pow(x, 2) * y)
 	     && pb.is_equal_to(pow(x, 3) * y)
 	     && pc.is_equal_to(pow(x, 3) * z)
 	     && pd.is_equal_to(25 * pow(x, 6) * pow(y, 3))
 	     && pe.is_equal_to(Polynomial(17))
 	     && pf.is_equal_to(5 + y)
 	     && pg.is_equal_to(x + 2*y)
 	     && ph.is_equal_to(z + p4)
 	     && pi.is_equal_to(21 + 2 * x)
 	     && pj.is_equal_to(pow(x, 2) + 5 * y)
 	     && pk.is_equal_to(x * y + 2 * x)
 	     && pl.is_equal_to(pow(x, 2) + x * z)
	     && pm.is_equal_to(6* pow(x, 2) * y + pow(x, 3) * y));
  return ok;
}

Polynomial
substitute(const Polynomial& p, Variable x, const Polynomial& q) {
  Polynomial result;
  for (Polynomial::const_iterator i = p.begin(),
	 iend = p.end(); i != iend; ++i) {
    const Monomial& m = *i;
    const Term& t = m.term();
    exponent_type x_exp = t.exponent(x);
    if (x_exp == 0)
      result += m;
    else {
      Term new_t;
      for (dimension_type i = t.space_dimension(); i-- > 0; ) {
	exponent_type exp = t.exponent(Variable(i));
	if (Term(x) != Variable(i) && exp > 0)
	  new_t *= pow(Variable(i), exp);
      }
      Polynomial new_q = pow(q, x_exp);
      result += m.coefficient() * new_t * new_q;
    }
  }
  return result;
}

#define P(x) std::cout << #x << ": " << x << std::endl;
void
test_Linear_Expression() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  Polynomial p1(1);
  Polynomial p2(x);
  Polynomial p3 = 3*x;
  Polynomial p4 = 4*x + 3*y;
  Polynomial p5 = 4*x + 3*y + 7;
  p1 += 4*x + 3*y + 7;
  p2 -= 4*x + 3*y + 7;
  p3 *= p2;
  Polynomial p6 = x*x*y + 4*x + 3;
  P(p6);
  Polynomial p7 = x*x*y - 4*x + 3;
  Polynomial p8 = 5*z - x*x*y - 4*x + 3;
  p6 += x+2;
  p6 -= x+2;
  p6 *= x+2;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
END_MAIN

// int
// main() TRY {
//   using namespace std;
//   using namespace IO_Operators;

//   test_Term();
//   test_Monomial();
//   test_Polynomial();
//   test_Linear_Expression();

// #if 0
//   Term m = x;
//   cout << m << endl;
//   m *= m;
//   cout << m << endl;
//   Term n = m*y*x;
//   cout << n << endl;
//   Polynomial p = n+2*m+x*n+12;
//   cout << p << endl;
//   Polynomial q = p*p;
//   cout << q << endl;
//   cout << (q >= q*q-99) << endl;
//   cout << (q >= p) << endl;
//   cout << p.degree() << endl
//        << q.degree() << endl
//        << (p*q).degree() << endl;

//   cout << "Iterating forward:" << endl;
//   for (Polynomial::const_iterator i = q.begin(),
// 	 q_end = q.end(); i != q_end; ++i)
//     cout << *i << endl;

//   cout << "Iterating backward:" << endl;
//   for (Polynomial::const_reverse_iterator i = q.rbegin(),
// 	 q_rend = q.rend(); i != q_rend; ++i)
//     cout << *i << endl;
// #endif

//   {
//     Polynomial p1(x*x + 2*x*y + y*y);
//     Polynomial p2(x + y);
//     exact_div_assign(p1, p2);
//     assert(p1.is_equal_to(p2));
//   }

//   cout << endl;
//   cout << "BUG!!" << endl;
//   cout << "Constant polynomial 2 : " << Polynomial(2) << endl;
// }
// CATCH
