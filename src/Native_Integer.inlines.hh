/* Native_Integer class implementation: inline functions.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Native_Integer_inlines_hh
#define PPL_Native_Integer_inlines_hh 1

#include <iostream>
#include <limits>
#include <cassert>

namespace Parma_Polyhedra_Library {

namespace {

//! \brief
//! Traits class to define the properties needed for the implementation
//! of the Native_Integer class.
template <typename T>
struct native_integer_traits {
};

#define DEFINE_NATIVE_INTEGER_TRAITS(type) \
template <> \
struct native_integer_traits<type> { \
  typedef unsigned type unsigned_counterpart; \
};

// signed char is special because `unsigned signed char' is not C++.
template <>
struct native_integer_traits<signed char> {
  typedef unsigned char unsigned_counterpart;
};

DEFINE_NATIVE_INTEGER_TRAITS(short)
DEFINE_NATIVE_INTEGER_TRAITS(int)
DEFINE_NATIVE_INTEGER_TRAITS(long)
DEFINE_NATIVE_INTEGER_TRAITS(long long)

//! \brief
//! If \p x is a non negative value of a native integral type, returns the
//! integer square root of \p x.  Returns 0 otherwise.
template <typename T>
inline T
uisqrt(T x) {
  T r = 0;
  if (!std::numeric_limits<T>::is_signed || x >= 0)
    for (T t = T(1) << (std::numeric_limits<T>::digits-2); t != 0; t >>= 2) {
      T s = r + t;
      if (s <= x) {
	x -= s;
	r = s + t;
      }
      r >>= 1;
    }
  return r;
}

//! Returns the integer square root of \p x.
template <typename T>
inline T
isqrt(T x) {
  assert(x >= 0);
  typedef typename native_integer_traits<T>::unsigned_counterpart U;
  return T(uisqrt(U(x)));
}

//! Returns the absolute value of \p x.
template <typename T>
inline T
abs(T x) {
  return x >= 0 ? x : -x;
}

//! Returns the greatest common divisor of \p x and \p y.
template <typename T>
inline T
gcd(T x,T y) {
  if (x == 0)
    return abs(y);
  else if (y == 0)
         return abs(x);
  x = abs(x);
  y = abs(y);
  T r = 0;
  while (y != 0) {
    r = x % y;
    x = y;
    y = r;
  }
  return x;
}

//! Returns the least common multiple of \p x and \p y.
template <typename T>
inline T
lcm(T x, T y) {
  if (x == 0 || y == 0)
    return 0;
  x = abs(x);
  y = abs(y);
  T g = gcd(x , y);
  x /= g;
  x *= y;
  return x;
}

} // namespace

template <typename T>
inline
Native_Integer<T>::Native_Integer()
  : v(0) {
}

#define PPL_INTEGER_CONSTRUCT_FROM_NATIVE(native) \
template <typename T> \
inline \
Native_Integer<T>::Native_Integer(native z) \
  : v(z) { \
}

PPL_INTEGER_CONSTRUCT_FROM_NATIVE(signed char)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(short)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(int)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(long)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(long long)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(unsigned char)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(unsigned short)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(unsigned int)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(unsigned long)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(unsigned long long)

template <typename T>
inline
Native_Integer<T>::Native_Integer(const mpz_class& z)
  : v(z.get_si()) {
}

template <typename T>
inline T&
Native_Integer<T>::raw_value() {
  return v;
}

template <typename T>
inline const T&
Native_Integer<T>::raw_value() const {
  return v;
}

#if 0
template <typename T>
inline
Native_Integer<T>::operator mpz_class() const {
  return v;
}

template <>
inline
Native_Integer<long long>::operator mpz_class() const {
  mpz_class n;
  if (v >= 0)
    mpz_import(n.get_mpz_t(), 1, 1, sizeof(long long), 0, 0, &v);
  else {
    long long abs_v = abs(v);
    mpz_import(n.get_mpz_t(), 1, 1, sizeof(long long), 0, 0, &abs_v);
    mpz_neg(n.get_mpz_t(), n.get_mpz_t());
  }
  return n;
}
#endif

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator=(const Native_Integer<T>& y) {
  v = y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator+=(const Native_Integer<T>& y) {
  v += y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator-=(const Native_Integer<T>& y) {
  v -= y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator*=(const Native_Integer<T>& y) {
  v *= y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator/=(const Native_Integer<T>& y) {
  v /= y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator%=(const Native_Integer<T>& y) {
  v %= y.v;
  return *this;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator++() {
  ++v;
  return *this;
}

template <typename T>
inline Native_Integer<T>
Native_Integer<T>::operator++(int) {
  Native_Integer tmp = *this;
  ++v;
  return tmp;
}

template <typename T>
inline Native_Integer<T>&
Native_Integer<T>::operator--() {
  --v;
  return *this;
}

template <typename T>
inline Native_Integer<T>
Native_Integer<T>::operator--(int) {
  Native_Integer tmp = *this;
  --v;
  return tmp;
}

template <typename T>
inline const T&
raw_value(const Native_Integer<T>& x) {
  return x.raw_value();
}

template <typename T>
inline T&
raw_value(Native_Integer<T>& x) {
  return x.raw_value();
}

template <typename T>
inline Native_Integer<T>
operator+(const Native_Integer<T>& x) {
  return x;
}

template <typename T>
inline Native_Integer<T>
operator-(const Native_Integer<T>& x) {
  Native_Integer<T> tmp = x;
  negate(tmp);
  return tmp;
}

template <typename T>
inline Native_Integer<T>
operator+(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp += y;
  return tmp;
}

template <typename T>
inline Native_Integer<T>
operator-(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp -= y;
  return tmp;
}

template <typename T>
inline Native_Integer<T>
operator*(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp *= y;
  return tmp;
}

template <typename T>
inline Native_Integer<T>
operator/(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp /= y;
  return tmp;
}

template <typename T>
inline Native_Integer<T>
operator%(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp %= y;
  return tmp;
}

template <typename T>
inline bool
operator==(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() == y.raw_value();
}

template <typename T>
inline bool
operator!=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() != y.raw_value();
}

template <typename T>
inline bool
operator>=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() >= y.raw_value();
}

template <typename T>
inline bool
operator>(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() > y.raw_value();
}

template <typename T>
inline bool
operator<=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() <= y.raw_value();
}

template <typename T>
inline bool
operator<(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() < y.raw_value();
}

template <typename T>
inline int
sgn(const Native_Integer<T>& x) {
  if(x.raw_value() > 0)
    return 1;
  else if (x.raw_value() == 0)
    return 0;
  else
    return -1;
}

template <typename T>
inline int
cmp(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return sgn(x-y);
}

template <typename T>
inline void
negate(Native_Integer<T>& x) {
  x.raw_value() = -x.raw_value();
}

template <typename T>
inline void
add_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z) {
  x.raw_value() += y.raw_value() * z.raw_value();
}

template <typename T>
inline void
sub_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z) {
  x.raw_value() -= y.raw_value() * z.raw_value();
}

template <typename T>
inline void
gcd_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() = gcd(x.raw_value(), y.raw_value());
}


template <typename T>
inline void
gcd_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = gcd(y.raw_value(), z.raw_value());
}

template <typename T>
inline void
lcm_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() = lcm(x.raw_value(), y.raw_value());
}

template <typename T>
inline void
lcm_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = lcm(y.raw_value(), z.raw_value());
}

template <typename T>
inline void
exact_div_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() /= y.raw_value();
}

template <typename T>
inline void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = y.raw_value() / z.raw_value();
}

template <typename T>
inline void
sqrt_assign(Native_Integer<T>& x) {
  assert(x.raw_value() >= 0);
  x.raw_value() = isqrt(x.raw_value());
}

template <typename T>
inline void
sqrt_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  assert(x.raw_value() >= 0);
  x.raw_value() = isqrt(y.raw_value());
}

template <typename T>
inline std::ostream&
operator<<(std::ostream& os, const Native_Integer<T>& x) {
  return os << x.raw_value();
}

template <typename T>
inline std::istream&
operator>>(std::istream& is, Native_Integer<T>& x) {
  return is >> x.raw_value();
}

PPL_INTEGER_DEFINE_NON_MEMBERS(Native_Integer)

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Native_Integer_inlines_hh)
