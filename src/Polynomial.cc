/* Polynomial class implementation (non-inline functions).
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

#include <ppl-config.h>
#include "Polynomial.defs.hh"
#include "Polynomial.inlines.hh"
#include "Linear_Expression.defs.hh"
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

PPL::Polynomial::Polynomial(const Linear_Expression& e)
  : map() {
  const Coefficient& k = e.inhomogeneous_term();
  if (k != 0)
    map.insert(Polynomial::Map::value_type(Term::one(), k));
  for (dimension_type d = 0,
	 e_space_dim = e.space_dimension(); d < e_space_dim; ++d) {
    Variable v(d);
    const Coefficient& c = e.coefficient(v);
    if (c != 0)
      map.insert(map.end(), Polynomial::Map::value_type(Term(v), c));
  }
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator+(const Polynomial& p1, const Polynomial& p2) {
  Polynomial p;

  Polynomial::Map::const_iterator i1 = p1.map.begin();
  Polynomial::Map::const_iterator i2 = p2.map.begin();
  Polynomial::Map::const_iterator p1_end = p1.map.end();
  Polynomial::Map::const_iterator p2_end = p2.map.end();

  while (i1 != p1_end && i2 != p2_end) {
    if (Polynomial::less(*i1, *i2))
      p.map.insert(p.map.end(), *i1++);
    else if (Polynomial::less(*i2, *i1))
      p.map.insert(p.map.end(), *i2++);
    else {
      TEMP_INTEGER(c);
      c = i1->second;
      c += i2->second;
      if (c != 0)
	p.map.insert(p.map.end(), Polynomial::Map::value_type(i1->first, c));
      ++i1;
      ++i2;
    }
  }
  while (i1 != p1_end)
    p.map.insert(p.map.end(), *i1++);
  while (i2 != p2_end)
    p.map.insert(p.map.end(), *i2++);
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator+(Coefficient_traits::const_reference n,
	       const Polynomial& p) {
  Polynomial q(p);
  if (n != 0) {
    Polynomial::Map::iterator i = q.map.begin();
    if (i != q.map.end() && i->first == Term::one()) {
      i->second += n;
      if (i->second == 0)
	q.map.erase(i);
    }
    else
      q.map.insert(i, Polynomial::Map::value_type(Term::one(), n));
  }
  assert(q.OK());
  return q;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator-(const Polynomial& p) {
  Polynomial q(p);
  for (Polynomial::Map::iterator i = q.map.begin(),
	 q_end = q.map.end(); i != q_end; ++i)
    neg_assign(i->second);
  assert(q.OK());
  return q;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator-(const Polynomial& p1, const Polynomial& p2) {
  Polynomial p;

  Polynomial::Map::const_iterator i1 = p1.map.begin();
  Polynomial::Map::const_iterator i2 = p2.map.begin();
  Polynomial::Map::const_iterator p1_end = p1.map.end();
  Polynomial::Map::const_iterator p2_end = p2.map.end();

  while(i1 != p1_end && i2 != p2_end) {
    if (Polynomial::less(*i1, *i2))
      p.map.insert(p.map.end(), *i1++);
    else if (Polynomial::less(*i2, *i1)) {
      Polynomial::Map::iterator i = p.map.insert(p.map.end(), *i2++);
      neg_assign(i->second);
    }
    else {
      TEMP_INTEGER(c);
      c = i1->second;
      c -= i2->second;
      if (c != 0)
	p.map.insert(p.map.end(), Polynomial::Map::value_type(i1->first, c));
      ++i1;
      ++i2;
    }
  }
  while (i1 != p1_end)
    p.map.insert(p.map.end(), *i1++);
  while (i2 != p2_end) {
    Polynomial::Map::iterator i = p.map.insert(p.map.end(), *i2++);
    neg_assign(i->second);
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator-(Coefficient_traits::const_reference n, const Polynomial& p) {
  Polynomial q(p);
  for (Polynomial::Map::iterator i = q.map.begin(),
	 q_end = q.map.end(); i != q_end; ++i)
    neg_assign(i->second);
  if (n != 0) {
    Polynomial::Map::iterator i = q.map.begin();
    if (i != q.map.end() && i->first == Term::one()) {
      i->second += n;
      if (i->second == 0)
	q.map.erase(i);
    }
    else
      q.map.insert(i, Polynomial::Map::value_type(Term::one(), n));
  }
  assert(q.OK());
  return q;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator*(Coefficient_traits::const_reference n, const Polynomial& p) {
  Polynomial q;
  if (n != 0) {
    q = p;
    for (Polynomial::Map::iterator i = q.map.begin(),
	   q_end = q.map.end(); i != q_end; ++i)
      i->second *= n;
  }
  assert(q.OK());
  return q;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator+=(Polynomial& p1, const Polynomial& p2) {
  Polynomial::Map::iterator i1 = p1.map.begin();
  Polynomial::Map::iterator p1_end = p1.map.end();
  Polynomial::Map::const_iterator i2 = p2.map.begin();
  Polynomial::Map::const_iterator p2_end = p2.map.end();

  while (i1 != p1_end && i2 != p2_end) {
    if (Polynomial::less(*i1, *i2))
      ++i1;
    else if (Polynomial::less(*i2, *i1)) {
      p1.map.insert(i1, *i2++);
      p1_end = p1.map.end();
    }
    else {
      i1->second += i2->second;
      if (i1->second == 0) {
	Polynomial::Map::iterator n1 = i1;
	++n1;
	p1.map.erase(i1);
	i1 = n1;
      }
      else
	++i1;
      ++i2;
    }
  }
  while (i2 != p2_end)
    p1.map.insert(p1.map.end(), *i2++);
  assert(p1.OK());
  return p1;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator+=(Polynomial& p, const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Polynomial::max_space_dimension())
    throw std::length_error("PPL::operator+=(p, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  std::pair<Polynomial::Map::iterator, bool>
    r = p.map.insert(Polynomial::Map::value_type(Term(v), Coefficient_one()));
  if (!r.second) {
    const Coefficient& sum = r.first->second += Coefficient_one();
    if (sum == 0)
      p.map.erase(r.first);
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator-=(Polynomial& p1, const Polynomial& p2) {
  Polynomial::Map::iterator i1 = p1.map.begin();
  Polynomial::Map::iterator p1_end = p1.map.end();
  Polynomial::Map::const_iterator i2 = p2.map.begin();
  Polynomial::Map::const_iterator p2_end = p2.map.end();

  while (i1 != p1_end && i2 != p2_end) {
    if (Polynomial::less(*i1, *i2))
      ++i1;
    else if (Polynomial::less(*i2, *i1)) {
      Polynomial::Map::iterator i = p1.map.insert(i1, *i2++);
      neg_assign(i->second);
      p1_end = p1.map.end();
    }
    else {
      i1->second -= i2->second;
      if (i1->second == 0) {
	Polynomial::Map::iterator n1 = i1;
	++n1;
	p1.map.erase(i1);
	i1 = n1;
      }
      else
	++i1;
      ++i2;
    }
  }
  while (i2 != p2_end) {
    Polynomial::Map::iterator i = p1.map.insert(p1.map.end(), *i2++);
    neg_assign(i->second);
  }
  assert(p1.OK());
  return p1;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator-=(Polynomial& p, const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Polynomial::max_space_dimension())
    throw std::length_error("PPL::operator-=(p, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  std::pair<Polynomial::Map::iterator, bool>
    r = p.map.insert(Polynomial::Map::value_type(Term(v), Coefficient_one()));
  if (r.second)
    neg_assign(r.first->second);
  else {
    const Coefficient& diff = r.first->second -= Coefficient_one();
    if (diff == 0)
      p.map.erase(r.first);
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial
PPL::operator*(const Polynomial& p1, const Polynomial& p2) {
  Polynomial p;
  for (Polynomial::const_iterator i1 = p1.begin(),
	 p1_end = p1.end(); i1 != p1_end; ++i1) {
    const Coefficient& c1 = i1->coefficient();
    const Term& t1 = i1->term();
    for (Polynomial::const_iterator i2 = p2.begin(),
	   p2_end = p2.end(); i2 != p2_end; ++i2) {
      const Coefficient& c2 = i2->coefficient();
      const Term& t2 = i2->term();
      Term t = t1*t2;
      TEMP_INTEGER(c);
      c = c1 * c2;
      assert(c != 0);
      std::pair<Polynomial::Map::iterator, bool>
	r = p.map.insert(Polynomial::Map::value_type(t, c));
      if (!r.second) {
	const Coefficient& sum = r.first->second += c;
	if (sum == 0)
	  p.map.erase(r.first);
      }
    }
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator*=(Polynomial& p, Coefficient_traits::const_reference n) {
  if (n == 0)
    p.map.clear();
  else
    for (Polynomial::Map::iterator i = p.map.begin(),
	   p_end = p.map.end(); i != p_end; ++i)
      i->second *= n;
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator*=(Polynomial& p, const Variable v) {
  // Here we exploit two properties of the term ordering:
  // 1) stability under multiplication: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each term t != 0, less(t*t1, t*t2);
  // 2) extensivity under multiplication: for any terms t1 and t2
  //    with t2 != 0, either t1 == t1*t2 or less(t1, t1*t2).
  // Note: using the reverse_iterator is required to keep map ordering
  // at intermediate steps (i.e., to be exception safe).
  for (Polynomial::Map::reverse_iterator i = p.map.rbegin(),
	 p_rend = p.map.rend(); i != p_rend; ++i)
    // FIXME: by calling operator*=(Term&, const Variable), we repeat
    // a tests that could instead be done only once before the loop.
    const_cast<Term&>(i->first) *= v;
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator*=(Polynomial& p, const Term& t) {
  // Here we exploit two properties of the term ordering:
  // 1) stability under multiplication: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each term t != 0, less(t*t1, t*t2);
  // 2) extensivity under multiplication: for any terms t1 and t2
  //    with t2 != 0, either t1 == t1*t2 or less(t1, t1*t2).
  // Note: using the reverse_iterator is required to keep map ordering
  // at intermediate steps (i.e., to be exception safe).
  for (Polynomial::Map::reverse_iterator i = p.map.rbegin(),
	 p_rend = p.map.rend(); i != p_rend; ++i)
    const_cast<Term&>(i->first) *= t;
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator*=(Polynomial& p, const Monomial& m) {
  // Here we exploit two properties of the term ordering:
  // 1) stability under multiplication: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each term t != 0, less(t*t1, t*t2);
  // 2) extensivity under multiplication: for any terms t1 and t2
  //    with t2 != 0, either t1 == t1*t2 or less(t1, t1*t2).
  // Note: using the reverse_iterator is required to keep map ordering
  // at intermediate steps (i.e., to be exception safe).
  if (m == Monomial::zero())
    p.map.clear();
  for (Polynomial::Map::reverse_iterator i = p.map.rbegin(),
	 p_rend = p.map.rend(); i != p_rend; ++i) {
    const_cast<Term&>(i->first) *= m.term();
    i->second *= m.coefficient();
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::operator*=(Polynomial& p1, const Polynomial& p2) {
  switch (p2.map.size()) {
  case 0:
    p1.map.clear();
    break;
  case 1:
    p1 *= *p2.begin();
    break;
  default:
    p1 = p1 * p2;
    break;
  }
  assert(p1.OK());
  return p1;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::exact_div_assign(Polynomial& p, Coefficient_traits::const_reference n) {
  assert(n != 0);
  for (Polynomial::Map::iterator i = p.map.begin(),
	 p_end = p.map.end(); i != p_end; ++i) {
    Coefficient& i_coeff = i->second;
    exact_div_assign(i_coeff, i_coeff, n);
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::exact_div_assign(Polynomial& p, const Variable v) {
  // Here we exploit two properties of the term ordering:
  // 1) stability under exact division: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each variable t dividing both
  //    t1 and t2, less(t1/t, t2/t);
  // 2) reductivity under exact division: for any terms t1 and t2
  //    such that t2 divides t1, either t1 == t1/t2 or less(t1/t2, t1).
  for (Polynomial::Map::iterator i = p.map.begin(),
	 p_end = p.map.end(); i != p_end; ++i)
    exact_div_assign(const_cast<Term&>(i->first), v);
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::exact_div_assign(Polynomial& p, const Term& t) {
  // Here we exploit two properties of the term ordering:
  // 1) stability under exact division: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each term t dividing both
  //    t1 and t2, less(t1/t, t2/t);
  // 2) reductivity under exact division: for any terms t1 and t2
  //    such that t2 divides t1, either t1 == t1/t2 or less(t1/t2, t1).
  for (Polynomial::Map::iterator i = p.map.begin(),
	 p_end = p.map.end(); i != p_end; ++i)
    exact_div_assign(const_cast<Term&>(i->first), t);
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::exact_div_assign(Polynomial& p, const Monomial& m) {
  assert(m != Monomial::zero());
  // Here we exploit two properties of the term ordering:
  // 1) stability under exact division: for any terms t1 and t2,
  //    less(t1, t2) implies that, for each term t dividing both
  //    t1 and t2, less(t1/t, t2/t);
  // 2) reductivity under exact division: for any terms t1 and t2
  //    such that t2 divides t1, either t1 == t1/t2 or less(t1/t2, t1).
  for (Polynomial::Map::iterator i = p.map.begin(),
	 p_end = p.map.end(); i != p_end; ++i) {
    exact_div_assign(const_cast<Term&>(i->first), m.term());
    Coefficient& i_coeff = i->second;
    exact_div_assign(i_coeff, i_coeff, m.coefficient());
  }
  assert(p.OK());
  return p;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
PPL::Polynomial&
PPL::exact_div_assign(Polynomial& p1, const Polynomial& p2) {
#ifndef NDEBUG
  Polynomial p1_copy(p1);
#endif
  assert(!p2.is_equal_to(Polynomial::zero()));
  // FIXME: this is just an executable specification.
  const Monomial& p2_leading_monomial = *(p2.begin());
  Polynomial quotient;
  while (!p1.is_equal_to(Polynomial::zero())) {
    Monomial m(*p1.begin());
    exact_div_assign(m, p2_leading_monomial);
    quotient += m;
    p1 -= m*p2;
  }
  p1.swap(quotient);
#ifndef NDEBUG
  assert(p1_copy.is_equal_to(p1 * p2));
#endif
  assert(p1.OK());
  return p1;
}

PPL::dimension_type
PPL::Polynomial::space_dimension() const {
  dimension_type d = 0;
  for (const_iterator i = begin(), p_end = end(); i != p_end; ++i) {
    dimension_type e = i->term().space_dimension();
    if (e > d)
      d = e;
  }
  return d;
}

PPL::degree_type
PPL::Polynomial::degree() const {
  degree_type d = 0;
  for (const_iterator i = begin(), p_end = end(); i != p_end; ++i) {
    degree_type e = i->term().degree();
    if (e > d)
      d = e;
  }
  return d;
}

PPL::degree_type
PPL::Polynomial::degree(const Variable v) const {
  degree_type d = 0;
  for (const_iterator i = begin(), p_end = end(); i != p_end; ++i) {
    degree_type e = i->term().degree(v);
    if (e > d)
      d = e;
  }
  return d;
}

void
PPL::pow_assign(Polynomial& p, const dimension_type n) {
  Polynomial acc(1);
  for (dimension_type i = n; i-- > 0; )
    acc *= p;
  p.swap(acc);
}

PPL::memory_size_type
PPL::Polynomial::external_memory_in_bytes() const {
  // FIXME: this is just an under-approximation.
  memory_size_type sz = map.size() * sizeof(Map::value_type);
  for (Polynomial::const_iterator i = map.begin(),
	 i_end = map.end(); i != i_end; ++i) {
    sz += i->first.external_memory_in_bytes();
    sz += PPL::external_memory_in_bytes(i->second);
  }
  return sz;
}

bool
PPL::Polynomial::OK() const {
  for (Polynomial::const_iterator i = map.begin(),
	 map_end = map.end(); i != map_end; ++i) {
    if (!i->term().OK())
      return false;

    if (i->coefficient() == 0) {
#ifndef NDEBUG
      std::cerr << "Polynomial has a null, non unit monomial."
		<< std::endl;
#endif
      return false;
    }
  }
  return true;
}

/*! \relates Parma_Polyhedra_Library::Polynomial */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Polynomial& p) {
  bool first = true;
  for (Polynomial::Map::const_iterator i = p.map.begin(),
	 map_end = p.map.end(); i != map_end; ++i) {
    TEMP_INTEGER(ev);
    ev = i->second;
    assert(ev != 0);
    if (!first) {
      if (ev > 0)
	s << " + ";
      else {
	s << " - ";
	neg_assign(ev);
      }
    }
    else
      first = false;
    if (ev == -1)
      s << "-";
    else if (ev != 1)
      s << ev;
    const Term& t = i->first;
    if (t == Term::one()) {
      if (ev == 1 || ev == -1)
	s << Coefficient_one();
    }
    else {
      if (ev != 1 && ev != -1)
	s << '*';
      s << t;
    }
  }
  if (first)
    // The null polynomial.
    s << Coefficient_zero();
  return s;
}


void
PPL::Polynomial::normalize() {
  // Compute the GCD of all the coefficients.
  Map::iterator p_begin = map.begin();
  Map::iterator p_end = map.end();
  Map::iterator i;
  TEMP_INTEGER(gcd);
  for (i = p_begin; i != p_end; ++i) {
    const Coefficient& c_i = i->second;
    if (const int c_i_sign = sgn(c_i)) {
      gcd = c_i;
      if (c_i_sign < 0)
	neg_assign(gcd);
      goto compute_gcd;
    }
  }
  // We reach this point only if all the coefficients were zero.
  return;

 compute_gcd:
  if (gcd == 1)
    return;
  for ( ; i != p_end; ++i) {
    const Coefficient& c_i = i->second;
    if (c_i != 0) {
      // Note: we use the ternary version instead of a more concise
      // gcd_assign(gcd, c_i) to take advantage of the fact that
      // `gcd' will decrease very rapidly (see D. Knuth, The Art of
      // Computer Programming, second edition, Section 4.5.2,
      // Algorithm C, and the discussion following it).  Our
      // implementation of gcd_assign(x, y, z) for checked numbers is
      // optimized for the case where `z' is smaller than `y', so that
      // on checked numbers we gain.  On the other hand, for the
      // implementation of gcd_assign(x, y, z) on GMP's unbounded
      // integers we cannot make any assumption, so here we draw.
      // Overall, we win.
      gcd_assign(gcd, c_i, gcd);
      if (gcd == 1)
	return;
    }
  }
  // Divide the coefficients by the GCD.
  for (i = p_begin; i != p_end; ++i) {
    Coefficient& c = i->second;
    exact_div_assign(c, c, gcd);
  }
}

void
PPL::Polynomial::sign_normalize() {
  Map::reverse_iterator i = map.rbegin();
  Map::reverse_iterator p_rend = map.rend();
  if (i != p_rend && i->second < 0)
    // Negate all the coefficients.
    for ( ; i != p_rend; ++i)
      neg_assign(i->second);
}

void
PPL::Polynomial::linear_combine(const Polynomial& y,
				const Term& t,
				Coefficient_traits::const_reference x_t,
				Coefficient_traits::const_reference y_t) {
  Polynomial& x = *this;
  assert(x_t != 0 && y_t != 0
	 && x_t == x.coefficient(t) && y_t == y.coefficient(t));
  // Let g be the GCD between `x[t]' and `y[t]'.
  // For each i the following computes
  //   x[i] = x[i]*y[t]/g - y[i]*x[t]/g.
  TEMP_INTEGER(normalized_x_t);
  TEMP_INTEGER(normalized_y_t);
  normalize2(x_t, y_t, normalized_x_t, normalized_y_t);

  Map::iterator x_i = x.map.begin();
  Map::iterator x_end = x.map.end();
  Map::const_iterator y_i = y.map.begin();
  Map::const_iterator y_end = y.map.end();

  // Declare a temporary here to avoid multiple (de-)allocations
  // in the following loop.
  TEMP_INTEGER(temp_x_i_coeff);

  while (x_i != x_end && y_i != y_end) {
    if (less(*x_i, *y_i)) {
      Coefficient& x_i_coeff = x_i->second;
      x_i_coeff *= normalized_y_t;
      ++x_i;
    }
    else if (less(*y_i, *x_i)) {
      temp_x_i_coeff = y_i->second;
      temp_x_i_coeff *= normalized_x_t;
      neg_assign(temp_x_i_coeff);
      x.map.insert(x_i, std::make_pair(y_i->first, temp_x_i_coeff));
      ++y_i;
      x_end = x.map.end();
    }
    else {
      // CHECK ME: should we check x_i == x_t and if so erase
      // the monomial without computing?
      Coefficient& x_i_coeff = x_i->second;
      x_i_coeff *= normalized_y_t;
      sub_mul_assign(x_i_coeff, y_i->second, normalized_x_t);
      if (x_i_coeff == 0) {
	Map::iterator n1 = x_i;
	++n1;
	x.map.erase(x_i);
	x_i = n1;
      }
      else
	++x_i;
      ++y_i;
    }
  }
  while (x_i != x_end) {
    Coefficient& x_i_coeff = x_i->second;
    x_i_coeff *= normalized_y_t;
    ++x_i;
  }
  while (y_i != y_end) {
    temp_x_i_coeff = y_i->second;
    temp_x_i_coeff *= normalized_x_t;
    neg_assign(temp_x_i_coeff);
    x.map.insert(x_i, std::make_pair(y_i->first, temp_x_i_coeff));
    ++y_i;
    x_end = x.map.end();
  }
  x.primitive_form_assign();
  assert(x.OK());
}

void
PPL::Polynomial::shift_space_dimensions(const Variables_Set& unused) {
  // Note: here we exploiting the stability of the term ordering
  // under the shifting of space dimensions.
  for (Map::iterator i = map.begin(), i_end = map.end(); i != i_end; ++i) {
    Term& t = const_cast<Term&>(i->first);
    t.shift_space_dimensions(unused);
  }
  assert(OK());
}

void
PPL::Polynomial
::permute_space_dimensions(const std::vector<dimension_type>& perm) {
  Map permuted_map;
  for (Map::iterator i = map.begin(), i_end = map.end(); i != i_end; ++i) {
    Term permuted_term(i->first, perm);
    // Steel the coefficient from `*i', instead of copying it.
    std::swap(permuted_map[permuted_term], i->second);
  }
  std::swap(map, permuted_map);
  assert(OK());
}

void
PPL::Polynomial::ascii_dump(std::ostream& s) const {
  s << map.size() << " :";
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i) {
    s << ' ';
    i->ascii_dump(s);
  }
}

PPL_OUTPUT_DEFINITIONS(Polynomial)

bool
PPL::Polynomial::ascii_load(std::istream& s) {
  std::string str;

  dimension_type sz;
  if (!(s >> sz))
    return false;

  if (!(s >> str) || str != ":")
    return false;

  map.clear();
  Monomial m;
  for (dimension_type i = 0; i < sz; ++i) {
    if (!m.ascii_load(s))
      return false;
    map.insert(m);
  }

  // Check for well formedness.
  assert(OK());
  return true;
}
