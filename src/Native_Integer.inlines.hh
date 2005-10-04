/* Native_Integer class implementation: inline functions.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Native_Integer_inlines_hh
#define PPL_Native_Integer_inlines_hh 1

#include "compiler.hh"
#include "globals.types.hh"
#include <iostream>
#include <limits>
#include <cassert>
#include <cstdlib>

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
/*! \relates Native_Integer */
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
/*! \relates Native_Integer */
template <typename T>
inline T
isqrt(T x) {
  assert(x >= 0);
  typedef typename native_integer_traits<T>::unsigned_counterpart U;
  return T(uisqrt(U(x)));
}

//! Returns the absolute value of \p x.
/*! \relates Native_Integer */
template <typename T>
inline T
abs(T x) {
  return x >= 0 ? x : -x;
}

//! Returns the greatest common divisor of \p x and \p y.
/*! \relates Native_Integer */
template <typename T>
inline T
gcd(T x,T y) {
  while (y != 0) {
    T r = x % y;
    x = y;
    y = r;
  }
  return abs(x);
}

//! Returns the least common multiple of \p x and \p y.
/*! \relates Native_Integer */
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
Native_Integer<T>::Native_Integer(const native z) \
  : v(static_cast<T>(z)) { \
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
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(float32_t)
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(float64_t)
#ifdef FLOAT96_TYPE
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(float96_t)
#endif
#ifdef FLOAT128_TYPE
PPL_INTEGER_CONSTRUCT_FROM_NATIVE(float128_t)
#endif

#define PPL_SIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(type) \
template <> \
inline \
Native_Integer<type>::Native_Integer(const char* y) \
  : v(strtol(y, 0, 0)) { \
}

template <>
inline
Native_Integer<long long>::Native_Integer(const char* y)
  : v(strtoll(y, 0, 0)) {
}

PPL_SIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(signed char)
PPL_SIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(short)
PPL_SIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(int)
PPL_SIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(long)

#define PPL_UNSIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(type) \
template <> \
inline \
Native_Integer<type>::Native_Integer(const char* y) \
  : v(strtoul(y, 0, 0)) { \
}

PPL_UNSIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(unsigned char)
PPL_UNSIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(unsigned short)
PPL_UNSIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(unsigned int)
PPL_UNSIGNED_SMALL_NATIVE_CONSTRUCT_FROM_C_STRING(unsigned long)

template <>
inline
Native_Integer<unsigned long long>::Native_Integer(const char* y)
  : v(strtoull(y, 0, 0)) {
}

#define PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(type) \
template <> \
inline \
Native_Integer<type>::Native_Integer(const mpz_class& y) { \
  if (sizeof(type) <= sizeof(long)) \
    v = y.get_si(); \
  else { \
    mpz_export(&v, 0, 1, sizeof(type), 0, 0, y.get_mpz_t()); \
    if (sgn(y) < 0) \
      v = -v; \
    } \
}

PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(signed char)
PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(short)
PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(int)
PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(long)
PPL_SIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(long long)

#define PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(type) \
template <> \
inline \
Native_Integer<type>::Native_Integer(const mpz_class& y) { \
  if (sizeof(type) <= sizeof(unsigned long)) \
    v = y.get_ui(); \
  else \
    mpz_export(&v, 0, 1, sizeof(type), 0, 0, y.get_mpz_t()); \
}

PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(unsigned char)
PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(unsigned short)
PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(unsigned int)
PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(unsigned long)
PPL_UNSIGNED_NATIVE_CONSTRUCT_FROM_MPZ_CLASS(unsigned long long)

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

/*! \relates Native_Integer */
template <typename T>
inline const T&
raw_value(const Native_Integer<T>& x) {
  return x.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline T&
raw_value(Native_Integer<T>& x) {
  return x.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
memory_size_type
total_memory_in_bytes(const Native_Integer<T>& x) {
  return sizeof(x);
}

/*! \relates Native_Integer */
template <typename T>
memory_size_type
external_memory_in_bytes(const Native_Integer<T>& x) {
  used(x);
  return 0;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator+(const Native_Integer<T>& x) {
  return x;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator-(const Native_Integer<T>& x) {
  Native_Integer<T> tmp = x;
  negate(tmp);
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator+(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp += y;
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator-(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp -= y;
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator*(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp *= y;
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator/(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp /= y;
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline Native_Integer<T>
operator%(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  Native_Integer<T> tmp = x;
  tmp %= y;
  return tmp;
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator==(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() == y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator!=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() != y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator>=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() >= y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator>(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() > y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator<=(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() <= y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline bool
operator<(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return x.raw_value() < y.raw_value();
}

/*! \relates Native_Integer */
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

/*! \relates Native_Integer */
template <typename T>
inline int
cmp(const Native_Integer<T>& x, const Native_Integer<T>& y) {
  return sgn(x-y);
}

/*! \relates Native_Integer */
template <typename T>
inline void
negate(Native_Integer<T>& x) {
  x.raw_value() = -x.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline void
add_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z) {
  x.raw_value() += y.raw_value() * z.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline void
sub_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z) {
  x.raw_value() -= y.raw_value() * z.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline void
gcd_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() = gcd(x.raw_value(), y.raw_value());
}


/*! \relates Native_Integer */
template <typename T>
inline void
gcd_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = gcd(y.raw_value(), z.raw_value());
}

/*! \relates Native_Integer */
template <typename T>
inline void
lcm_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() = lcm(x.raw_value(), y.raw_value());
}

/*! \relates Native_Integer */
template <typename T>
inline void
lcm_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = lcm(y.raw_value(), z.raw_value());
}

/*! \relates Native_Integer */
template <typename T>
inline void
exact_div_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  x.raw_value() /= y.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y, const Native_Integer<T>& z) {
  x.raw_value() = y.raw_value() / z.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline void
sqrt_assign(Native_Integer<T>& x) {
  assert(x.raw_value() >= 0);
  x.raw_value() = isqrt(x.raw_value());
}

/*! \relates Native_Integer */
template <typename T>
inline void
sqrt_assign(Native_Integer<T>& x, const Native_Integer<T>& y) {
  assert(x.raw_value() >= 0);
  x.raw_value() = isqrt(y.raw_value());
}

/*! \relates Native_Integer */
template <typename T>
inline std::ostream&
operator<<(std::ostream& os, const Native_Integer<T>& x) {
  return os << x.raw_value();
}

/*! \relates Native_Integer */
template <typename T>
inline std::istream&
operator>>(std::istream& is, Native_Integer<T>& x) {
  return is >> x.raw_value();
}

template <typename To>
inline Native_Integer<To>&
assign(Native_Integer<To>& to, const mpz_class& from, Rounding_Dir dir) {
  used(dir);
  To& raw_value = to.raw_value();
  if (sizeof(To) <= sizeof(long))
    raw_value = from.get_si();
  else {
    mpz_export(&raw_value, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
    if (sgn(from) < 0)
      raw_value = -raw_value;
  }
  return to;
}

PPL_INTEGER_DEFINE_NON_MEMBERS(Native_Integer)

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Native_Integer_inlines_hh)
