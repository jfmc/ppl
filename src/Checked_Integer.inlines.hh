/* Checked_Integer class implementation: inline functions.
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

#ifndef PPL_Checked_Integer_inlines_hh
#define PPL_Checked_Integer_inlines_hh 1

#include <iostream>
#include <limits>
#include <cassert>
#include <stdexcept>

namespace Parma_Polyhedra_Library {

namespace {

//! \brief
//! Traits class to define the properties needed for the implementation
//! of the Checked_Integer class.
template <typename T>
struct checked_integer_traits {
};

#define DEFINE_CHECKED_INTEGER_TRAITS(type) \
template <> \
struct checked_integer_traits<type> { \
  typedef unsigned type unsigned_counterpart; \
};

// signed char is special because `unsigned signed char' is not C++.
template <>
struct checked_integer_traits<signed char> {
  typedef unsigned char unsigned_counterpart;
};

DEFINE_CHECKED_INTEGER_TRAITS(short)
DEFINE_CHECKED_INTEGER_TRAITS(int)
DEFINE_CHECKED_INTEGER_TRAITS(long)
DEFINE_CHECKED_INTEGER_TRAITS(long long)

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
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
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
gcd(T x, T y) {
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

//! \brief
//! Returns 1 if \p x is a positive value, -1 if \p x is negative.
//! Returns 0 if \p x is zero.
template <typename T>
inline int
sign(T x) {
  if (x > 0)
    return 1;
  else if (x == 0)
    return 0;
  else
    return -1;
}

//! Returns the unsigned counterpart of \p x.
template <typename T>
inline typename checked_integer_traits<T>::unsigned_counterpart
take_unsigned(T x) {
  // FIXME: we assume a 2-complement representation.
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
  if (x >= 0 || x == std::numeric_limits<T>::min())
    return static_cast<U>(x);
  else
    return static_cast<U>(-x);
}

//! \brief
//! Ckeck for positive or negative overflow.
//! Returns the signed counterpart of \p m signed with \p s.
template <typename T>
inline T
make_signed(int s,
	    typename checked_integer_traits<T>::unsigned_counterpart m) {
  // FIXME: we assume a 2-complement representation.
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
  static const U min_modulus = static_cast<U>(std::numeric_limits<T>::min());
  static const U max_modulus = static_cast<U>(std::numeric_limits<T>::max());

  switch (s) {
  case 1:
    if (m > max_modulus)
      throw std::overflow_error("Positive overflow.");
    return static_cast<T>(m);
  case -1:
    if (m >= min_modulus) {
      if (m > min_modulus)
	throw std::overflow_error("Negative overflow.");
      else
	return std::numeric_limits<T>::min();
    }
    return -static_cast<T>(m);
  default:
    return 0;
  }
}

//! Returns the checked sum of \p x and \p y.
template <typename T>
inline T
checked_add(T x, T y) {
  int sx = sign(x);
  if (sx != sign(y))
    return x + y;
  // Here `x' and `y' have the same sign.
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
  // Compute the modulus of the result.
  U modulus = take_unsigned(x) + take_unsigned(y);
  return make_signed<T>(sx, modulus);
}

//! Returns the checked subtraction between \p x and \p y.
template <typename T>
inline T
checked_sub(T x, T y) {
  int sy = sign(y);
  if (sy == 0)
    return x;
  int sx = sign(x);
  if (sx == 0)
    return make_signed<T>(-1*sy, take_unsigned(y));
  if (sx == sy)
    return x - y;
  // Here `x' and `y' have different sign.
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
  // Compute the modulus of the result.
  U modulus = take_unsigned(x) + take_unsigned(y);
  return make_signed<T>(sx, modulus);
}

//! Returns the hight part of \p x.
template <typename U>
inline U
hight_part(U x) {
  return x >> std::numeric_limits<U>::digits / 2;
}

//! Returns the low part of \p x.
template <typename U>
inline U
low_part(U x) {
  return x &
  (std::numeric_limits<U>::max() >> (std::numeric_limits<U>::digits / 2));
}

//! Returns the checked multiplication between \p x and \p y.
template <typename T>
inline T
checked_mul(T x, T y) {
  int sx = sign(x);
  int sy = sign(y);
  if (sx == 0 || sy == 0)
    return 0;

  // Here `x' and `y' are not zero.
  typedef typename checked_integer_traits<T>::unsigned_counterpart U;
  U ux = take_unsigned(x);
  U uy = take_unsigned(y);
  U modulus = 0;
  
  // Make the four half parts.
  U hight_ux = hight_part<U>(ux);
  U low_ux = low_part<U>(ux);
  U hight_uy = hight_part<U>(uy);
  U low_uy = low_part<U>(uy);
  
  // Form the optimized partial products and check for overflow.
  // Multiply only the parts that are not zero!
  // In worst case compute only two partial products.
  if (hight_ux == 0) {
    if (hight_uy == 0)
      modulus = low_ux * low_uy;
    else {
        U lux_huy = low_ux * hight_uy;
        modulus = low_ux * low_uy + (low_part<U>(lux_huy)
	          << std::numeric_limits<U>::digits / 2);
        if (hight_part<U>(lux_huy) != 0)
	  // hight_part of lux_huy is not representable.
	  throw std::overflow_error("Overflow.");
    }
  }
  else {
      if (hight_uy == 0) {
        U hux_luy = hight_ux * low_uy;
	modulus = low_ux * low_uy + (low_part<U>(hux_luy)
	          << std::numeric_limits<U>::digits / 2);
        if (hight_part<U>(hux_luy) != 0)
	  // hight_part of hux_luy is not representable.
	  throw std::overflow_error("Overflow.");
      }
      else // hight_ux != 0 && hight_uy != 0 --> overflow! 
          throw std::overflow_error("Overflow."); 
  }

  // Returns the signed result.
  return make_signed<T>(sx*sy, modulus);
}

} // namespace

template <typename T>
inline
Checked_Integer<T>::Checked_Integer()
  : value_(0) {
}

template <typename T>
inline
Checked_Integer<T>::Checked_Integer(const Checked_Integer& x)
  : value_(x.value_) {
}

template <typename T>
inline
Checked_Integer<T>::~Checked_Integer() {
}

#define PPL_INTEGER_CONSTRUCT_FROM_CHECKED(checked) \
template <typename T> \
inline \
Checked_Integer<T>::Checked_Integer(checked z) { \
  if (z > std::numeric_limits<T>::max()) \
    throw std::overflow_error("Positive overflow."); \
  else if (z < std::numeric_limits<T>::min()) \
    throw std::overflow_error("Negative Overflow."); \
  value_ = z; \
}

PPL_INTEGER_CONSTRUCT_FROM_CHECKED(signed char)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(unsigned char)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(short)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(unsigned short)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(int)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(unsigned int)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(long)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(unsigned long)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(long long)
PPL_INTEGER_CONSTRUCT_FROM_CHECKED(unsigned long long)

template <typename T>
inline
Checked_Integer<T>::Checked_Integer(const mpz_class& z) {
  if (z.get_si() > std::numeric_limits<T>::max())
    throw std::overflow_error("Positive overflow.");
  else if (z.get_si() < std::numeric_limits<T>::min())
    throw std::overflow_error("Negative Overflow.");
  value_ = z.get_si();
}

template <typename T>
inline
Checked_Integer<T>::operator mpz_class() const {
  return value_;
}

template <>
inline
Checked_Integer<long long>::operator mpz_class() const {
  mpz_class n;
  if (value_ >= 0)
    mpz_import(n.get_mpz_t(), 1, 1, sizeof(long long), 0, 0, &value_);
  else {
    long long abs_value_ = abs(value_);
    mpz_import(n.get_mpz_t(), 1, 1, sizeof(long long), 0, 0, &abs_value_);
    mpz_neg(n.get_mpz_t(), n.get_mpz_t());
  }
  return n;
}

template <typename T>
inline void
Checked_Integer<T>::swap(Checked_Integer<T>& y) {
  std::swap(value_ , y.value_);
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator=(const Checked_Integer<T> y) {
  value_ = y.value_;
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator+=(const Checked_Integer<T> y) {
  value_ = checked_add(value_, y.value_);
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator-=(const Checked_Integer<T> y) {
  value_ = checked_sub(value_, y.value_);
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator*=(const Checked_Integer<T> y) {
  value_ = checked_mul(value_, y.value_);
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator/=(const Checked_Integer<T> y) {
  if (value_ == std::numeric_limits<T>::min() &&
      y.value_ == -1 )
    throw std::overflow_error("Positive overflow.");
  value_ /= y.value_;
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator%=(const Checked_Integer<T> y) {
  value_ %= y.value_;
  return *this;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator++() {
  if (value_ == std::numeric_limits<T>::max())
    throw std::overflow_error("Positive overflow.");
  ++value_;
  return *this;
}

template <typename T>
inline Checked_Integer<T>
Checked_Integer<T>::operator++(int) {
  Checked_Integer tmp(*this);
  if (value_ == std::numeric_limits<T>::max())
    throw std::overflow_error("Positive overflow.");
  ++value_;
  return tmp;
}

template <typename T>
inline Checked_Integer<T>&
Checked_Integer<T>::operator--() {
  if (value_ == std::numeric_limits<T>::min())
    throw std::overflow_error("Negative overflow.");
  --value_;
  return *this;
}

template <typename T>
inline Checked_Integer<T>
Checked_Integer<T>::operator--(int) {
  Checked_Integer tmp(*this);
  if (value_ == std::numeric_limits<T>::min())
    throw std::overflow_error("Negative overflow.");
  --value_;
  return tmp;
}

template <typename T>
inline Checked_Integer<T>
operator+(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  Checked_Integer<T> tmp = x;
  tmp += y;
  return tmp;
}

template <typename T>
inline Checked_Integer<T>
operator+(const Checked_Integer<T> x) {
  return x;
}

template <typename T>
inline Checked_Integer<T>
operator-(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  Checked_Integer<T> tmp = x;
  tmp -= y;
  return tmp;
}

template <typename T>
inline void
negate(Checked_Integer<T>& x) {
  if (x == std::numeric_limits<T>::min())
    // The negate value of x is not representable.
    throw std::overflow_error("Positive overflow.");
  x.value_ = -x.value_;
}

template <typename T>
inline Checked_Integer<T>
operator-(const Checked_Integer<T> x) {
  Checked_Integer<T> tmp = x;
  negate(tmp);
  return tmp;
}

template <typename T>
inline Checked_Integer<T>
operator*(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  Checked_Integer<T> tmp = x;
  tmp *= y;
  return tmp;
}

template <typename T>
inline Checked_Integer<T>
operator/(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  Checked_Integer<T> tmp = x;
  tmp /= y;
  return tmp;
}

template <typename T>
inline Checked_Integer<T>
operator%(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  Checked_Integer<T> tmp = x;
  tmp %= y;
  return tmp;
}

template <typename T>
inline void
gcd_assign(Checked_Integer<T>& x, const Checked_Integer<T> y) {
  x.value_ = gcd(x.value_, y.value_);
}


template <typename T>
inline void
gcd_assign(Checked_Integer<T>& x,
	   const Checked_Integer<T> y, const Checked_Integer<T> z) {
  x.value_ = gcd(y.value_, z.value_);
}

template <typename T>
inline void
lcm_assign(Checked_Integer<T>& x, const Checked_Integer<T> y) {
  x.value_ = lcm(x.value_, y.value_);
}

template <typename T>
inline void
lcm_assign(Checked_Integer<T>& x,
	   const Checked_Integer<T> y, const Checked_Integer<T> z) {
  x.value_ = lcm(y.value_, z.value_);
}

template <typename T>
inline void
exact_div_assign(Checked_Integer<T>& x, const Checked_Integer<T> y) {
  x.value_ /= y.value_;
}

template <typename T>
inline void
exact_div_assign(Checked_Integer<T>& x,
		 const Checked_Integer<T> y, const Checked_Integer<T> z) {
  x.value_ = y.value_ / z.value_;
}

template <typename T>
inline void
sqrt_assign(Checked_Integer<T>& x) {
  assert(x.value_ >= 0);
  x.value_ = isqrt(x.value_);
}

template <typename T>
inline void
sqrt_assign(Checked_Integer<T>& x, const Checked_Integer<T> y) {
  assert(x.value_ >= 0);
  x.value_ = isqrt(y.value_);
}

template <typename T>
inline int
sgn(const Checked_Integer<T> x) {
  return sign(x.value_);
}

template <typename T>
inline int
cmp(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return sgn(x-y);
}

inline const Integer&
Integer_zero() {
  static Integer z(0);
  return z;
}

inline const Integer&
Integer_one() {
  static Integer o(1);
  return o;
}

template <typename T>
inline bool
operator==(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ == y.value_;
}

template <typename T>
inline bool
operator!=(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ != y.value_;
}

template <typename T>
inline bool
operator>=(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ >= y.value_;
}

template <typename T>
inline bool
operator>(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ > y.value_;
}

template <typename T>
inline bool
operator<=(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ <= y.value_;
}

template <typename T>
inline bool
operator<(const Checked_Integer<T> x, const Checked_Integer<T> y) {
  return x.value_ < y.value_;
}

template <typename T>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Integer<T> x) {
  return os << x.value_;
}

template <typename T>
inline std::istream&
operator>>(std::istream& is, Checked_Integer<T>& x) {
  return is >> x.value_;
}

PPL_INTEGER_DEFINE_NON_MEMBERS(Checked_Integer)

} // namespace Parma_Polyhedra_Library

namespace std {

template <typename T>
inline void
swap(Parma_Polyhedra_Library::Checked_Integer<T>& x,
     Parma_Polyhedra_Library::Checked_Integer<T>& y) {
     x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Checked_Integer_inlines_hh)
