/* E_Rational class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_E_Rational_inlines_hh
#define PPL_E_Rational_inlines_hh 1

#include <iostream>
#include <sstream>
#include <cassert>

namespace Parma_Polyhedra_Library {

inline
E_Rational::E_Rational()
  : n() {
}

inline
E_Rational::E_Rational(const mpq_class& y)
  : n(y) {
}

inline
E_Rational::E_Rational(const E_Rational& y)
  : n(y.n) {
}

inline E_Rational&
E_Rational::operator=(const E_Rational& y) {
  n = y.n;
  return *this;
}

inline E_Rational&
E_Rational::operator=(const mpq_class& y) {
  n = y;
  return *this;
}

inline bool
E_Rational::is_plus_infinity() const {
  return n.get_den() == 0 && n.get_num() == 1;
}

inline bool
E_Rational::is_finite() const {
  return n.get_den() != 0;
}

inline bool
E_Rational::is_zero() const {
  return n.get_num() == 0 && n.get_den() != 0;
}

inline bool
E_Rational::is_minus_infinity() const {
  return n.get_den() == 0 && n.get_num() == -1;
}

inline bool
E_Rational::is_nan() const {
  return n.get_den() == 0 && n.get_num() == 0;
}

inline bool
E_Rational::is_positive() const {
  if (n.get_num() > 0 && n.get_den() > 0)
    return true;
  if (n.get_num() < 0 && n.get_den() < 0)
    return true;
  return false;
}

inline bool
E_Rational::is_negative() const {
  if (n.get_num() < 0 && n.get_den() > 0)
    return true;
  if (n.get_num() > 0 && n.get_den() < 0)
    return true;
  return false;
}

inline bool
E_Rational::is_nonpositive() const {
  if (n.get_num() <= 0 && n.get_den() > 0)
    return true;
  if (n.get_num() >= 0 && n.get_den() < 0)
    return true;
  return false;
}

inline bool
E_Rational::is_nonnegative() const {
   if (n.get_num() >= 0 && n.get_den() > 0)
    return true;
  if (n.get_num() <= 0 && n.get_den() < 0)
    return true;
  return false;
}

inline const mpq_class&
E_Rational::pinf() {
  static mpq_class p(1, 0);
  return p;
}

inline const mpq_class&
E_Rational::max() {
  return pinf();
}

inline const mpq_class&
E_Rational::minf() {
  static mpq_class m(-1, 0);
  return m;
}

inline const mpq_class&
E_Rational::min() {
  return minf();
}

inline const mpq_class&
E_Rational::nnum() {
  static mpq_class n(mpz_class(0), 0);
  return n;
}

inline const E_Rational&
E_Rational::plus_infinity() {
  static E_Rational p(pinf());
  return p;
}

inline
E_Rational::E_Rational(const Plus_Infinity&)
  : n(pinf()) {
}

inline const E_Rational&
E_Rational::finite_max() {
  return plus_infinity();
}

inline const E_Rational&
E_Rational::minus_infinity() {
  static E_Rational m(minf());
  return m;
}

inline const E_Rational&
E_Rational::finite_min() {
  return minus_infinity();
}

inline const E_Rational&
E_Rational::not_a_number() {
  static E_Rational n(nnum());
  return n;
}

inline const mpq_class&
E_Rational::number() const {
  assert(is_finite());
  return n;
}

inline void
E_Rational::numer_denom(Coefficient& numer, Coefficient& denom) const {
  assert(is_finite());
  const_cast<mpq_class&>(n).canonicalize();
  numer = n.get_num();
  denom = n.get_den();
}

inline bool
E_Rational::OK() const {
  // Special values are OK.
  if (is_plus_infinity() || is_minus_infinity() || is_nan())
    return true;

  // Ordinary, finite values must be canonicalized.
  E_Rational copy = *this;
  copy.n.canonicalize();
  if (copy.n.get_num() != n.get_num()) {
#ifndef NDEBUG
    std::cerr << "The E_Rational is not canonicalized!"
	      << std::endl;
#endif
    return false;
  }

  // All checks passed.
  return true;
}

inline bool
operator==(const E_Rational& x, const E_Rational& y) {
  return !x.is_nan() && !y.is_nan() && x.n == y.n;
}

inline bool
operator!=(const E_Rational& x, const E_Rational& y) {
  return !(x == y);
}

inline bool
operator<(const E_Rational& x, const E_Rational& y) {
  if (x.is_finite() && y.is_finite())
    return x.n < y.n;
  else if (x.is_plus_infinity() || x.is_nan() || y.is_nan())
    return false;
  else
    return !y.is_minus_infinity();
}

inline bool
operator<=(const E_Rational& x, const E_Rational& y) {
  if (x.is_finite() && y.is_finite())
    return x.n <= y.n;
  else if (x.is_nan() || y.is_nan())
    return false;
  else if (x.is_minus_infinity() || y.is_plus_infinity())
    return true;
  else
    return false;
}

inline bool
operator>(const E_Rational& x, const E_Rational& y) {
  return y < x;
}

inline bool
operator>=(const E_Rational& x, const E_Rational& y) {
  return y <= x;
}

inline E_Rational
operator+(const E_Rational& x) {
  return x;
}

inline E_Rational
operator-(const E_Rational& x) {
  E_Rational r;
  if (x.is_nan())
    r.n = x.n;
  else
    r.n = -x.n;
  return r;
}

inline E_Rational
operator+(const E_Rational& x, const E_Rational& y) {
  E_Rational r;
  if (x.is_nan())
    r.n = x.n;
  else if (y.is_nan())
    r.n = y.n;
  else if (x.is_plus_infinity())
    if (y.is_minus_infinity())
      r.n = E_Rational::nnum();
    else
      r.n = x.n;
  else if (x.is_minus_infinity())
    if (y.is_plus_infinity())
      r.n = E_Rational::nnum();
    else
      r.n = x.n;
  else if (y.is_plus_infinity())
    if (x.is_minus_infinity())
      r.n = E_Rational::nnum();
    else
      r.n = y.n;
  else if (y.is_minus_infinity())
    if (x.is_plus_infinity())
      r.n = E_Rational::nnum();
    else
      r.n = y.n;
  else
    r.n = x.n + y.n;
  return r;
}

inline E_Rational
operator/(const E_Rational& x, const E_Rational& y) {
  E_Rational r;
  if (y.n == 0)
    throw;
  else if (x.n == E_Rational::nnum())
    r.n = x.n;
  else if (y.n == E_Rational::nnum())
    r.n = y.n;
  else if (x.n == E_Rational::pinf())
    if (y.n == E_Rational::pinf() || y.n == E_Rational::minf())
      r.n = E_Rational::nnum();
    else if (y.n > 0)
      r.n = x.n;
    else
      r.n = E_Rational::minf();
  else if (x.n == E_Rational::minf())
    if (y.n == E_Rational::pinf() || y.n == E_Rational::minf())
      r.n = E_Rational::nnum();
    else if (y.n > 0)
      r.n = x.n;
    else
      r.n = E_Rational::pinf();
  else if (y.n == E_Rational::pinf() || y.n == E_Rational::minf())
    r.n = 0;
  else
    r.n = x.n/y.n;
  return r;
}

inline E_Rational
negate_round_up(const E_Rational& x) {
  return -x;
}

inline E_Rational
negate_round_down(const E_Rational& x) {
  return -x;
}

inline E_Rational
add_round_up(const E_Rational& x, const E_Rational& y) {
  return x+y;
}

inline E_Rational
add_round_down(const E_Rational& x, const E_Rational& y) {
  return x+y;
}

template <>
inline E_Rational
div_round_up<E_Rational>(const Coefficient& x, const Coefficient& y) {
  E_Rational z;
  if (y == 0)
    if (x == 0)
      z = E_Rational::not_a_number();
    else if (x > 0)
      z = E_Rational::plus_infinity();
    else
      z = E_Rational::minus_infinity();
  else {
    Checked::assign<Checked::Transparent_Policy>(z.n.get_num(), raw_value(x), ROUND_IGNORE);
    Checked::assign<Checked::Transparent_Policy>(z.n.get_den(), raw_value(y), ROUND_IGNORE);
    z.n.canonicalize();
  }
  return z;
}

template <>
inline E_Rational
div_round_down<E_Rational>(const Coefficient& x, const Coefficient& y) {
  return div_round_up<E_Rational>(x, y);
}


inline E_Rational
div_round_up(const E_Rational& x, const Coefficient& y) {
  E_Rational z;
  if (x.is_nan())
    z = E_Rational::not_a_number();
  if (x.is_plus_infinity())
    if (y < 0)
      z = E_Rational::minus_infinity();
    else if (y > 0)
      z = E_Rational::plus_infinity();
    else 
      z = E_Rational::not_a_number();
  if (x.is_minus_infinity())
    if (y > 0)
      z = E_Rational::minus_infinity();
    else if (y < 0)
      z = E_Rational::plus_infinity();    
    else 
      z = E_Rational::not_a_number();
  if (y == 0)
    if (x.is_zero())
      z = E_Rational::not_a_number();
    else if (x.is_positive())
      z = E_Rational::plus_infinity();
    else
      z = E_Rational::minus_infinity();
  else {
    Coefficient numer, denom;
    x.numer_denom(numer, denom);
    denom *= y;
    Checked::assign<Checked::Transparent_Policy>(z.n.get_num(), raw_value(numer), ROUND_IGNORE);
    Checked::assign<Checked::Transparent_Policy>(z.n.get_den(), raw_value(denom), ROUND_IGNORE);
    z.n.canonicalize();
  }
  return z;
}

inline E_Rational 
div_round_down(const E_Rational& x, const Coefficient& y) {
  return div_round_up(x, y);
}

inline bool
is_plus_infinity(const E_Rational& x) {
  return x.is_plus_infinity();
}

inline bool
is_nan(const E_Rational& x) {
  return x.is_nan();
}

inline bool
is_negative(const E_Rational& x) {
  return x.is_negative();
}

inline bool
is_nonnegative(const E_Rational& x) {
  return x.is_nonnegative();
}

inline bool
exact_neg(E_Rational& to, const E_Rational& x) {
  to = -x;
  return true;
}

inline void
div_round_up(E_Rational& to,
	     const Coefficient& x,
	     const Coefficient& y) {
  to = div_round_up<E_Rational>(x, y);
}

inline void
add_round_up(E_Rational& to,
	     const E_Rational& x,
	     const E_Rational& y) {
  to = add_round_up(x, y);
}

inline void add_round_down(E_Rational& to,
			   const E_Rational& x,
			   const E_Rational& y) {
  to = add_round_down(x, y);
}

inline void
negate_round_down(E_Rational& to,
		  const E_Rational& x) {
  to = negate_round_down(x);
}

inline void
numer_denom(const E_Rational& from,
	    Coefficient& num, Coefficient& den) {
  from.numer_denom(num, den);
}

namespace IO_Operators {

inline std::ostream&
operator<<(std::ostream& s, const E_Rational& x) {
  if (x.n == E_Rational::nnum())
    s << "nan";
  else if (x.n == E_Rational::minf())
    s << "-inf";
  else if (x.n == E_Rational::pinf())
    s << "+inf";
  else {
    const_cast<mpq_class&>(x.n).canonicalize();
    s << x.n;
  }
  return s;
}

inline std::istream&
operator>>(std::istream& s, E_Rational& x) {
  std::string str;
  if (s >> str) {
    if (str == "nan")
      x.n = E_Rational::nnum();
    else if (str == "-inf")
      x.n = E_Rational::minf();
    else if (str == "+inf")
      x.n = E_Rational::pinf();
    else {
      std::istringstream ss(str);
      if (!(ss >> x.n))
	s.setstate(std::ios::failbit); 
    }
  }
  return s;
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_E_Rational_inlines_hh)
