/* Checked_Number class implementation: inline functions.
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

#ifndef PPL_Checked_Number_inlines_hh
#define PPL_Checked_Number_inlines_hh 1

#include <stdexcept>

namespace Parma_Polyhedra_Library {

static void
bad_result(Checked::Result r) {
  switch (r) {
  case Checked::V_NEG_OVERFLOW:
    throw std::overflow_error("Negative overflow.");
  case Checked::V_POS_OVERFLOW:
    throw std::overflow_error("Positive overflow.");
  case Checked::V_UNKNOWN:
    throw std::overflow_error("Undefined result.");
  case Checked::V_DOMAIN:
    throw std::domain_error("Result is out of numeric domain.");
  default:
    throw std::logic_error("Unexpected inexact computation.");
  }
}

static inline void
check_result(Checked::Result r) {
  if (r != Checked::V_EQ)
    bad_result(r);
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number()
 : v(0) {
}

#define DEF_CTOR(type) \
template <typename T, typename Policy> \
inline \
Checked_Number<T, Policy>::Checked_Number(const type y) { \
  check_result(Checked::assign<Policy>(v, y)); \
}

DEF_CTOR(signed char)
DEF_CTOR(short)
DEF_CTOR(int)
DEF_CTOR(long)
DEF_CTOR(long long)
DEF_CTOR(unsigned char)
DEF_CTOR(unsigned short)
DEF_CTOR(unsigned int)
DEF_CTOR(unsigned long)
DEF_CTOR(unsigned long long)
DEF_CTOR(float32_t)
DEF_CTOR(float64_t)
#ifdef FLOAT96_TYPE
DEF_CTOR(float96_t)
#endif
#ifdef FLOAT128_TYPE
DEF_CTOR(float128_t)
#endif
DEF_CTOR(mpq_class&)
DEF_CTOR(mpz_class&)
DEF_CTOR(char*)

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::operator T() const {
  if (Policy::convertible)
    return v;
}

template <typename T, typename Policy>
inline T&
Checked_Number<T, Policy>::raw_value() {
  return v;
}

template <typename T, typename Policy>
inline const T&
Checked_Number<T, Policy>::raw_value() const {
  return v;
}

template <typename T, typename Policy>
inline const T&
raw_value(const Checked_Number<T, Policy>& x) {
  return x.raw_value();
}

template <typename T, typename Policy>
inline T&
raw_value(Checked_Number<T, Policy>& x) {
  return x.raw_value();
}

template <typename T, typename Policy>
size_t
total_memory_in_bytes(const Checked_Number<T, Policy>& x) {
  return sizeof(x);
}

template <typename T, typename Policy>
size_t
external_memory_in_bytes(const Checked_Number<T, Policy>&) {
  return 0;
}

#define DEF_INCREMENT(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f() { \
  check_result(Checked::fun<Policy>(v, v, T(1))); \
  return *this; \
}\
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
Checked_Number<T, Policy>::f(int) {\
  T r = v;\
  check_result(Checked::fun<Policy>(v, v, T(1)));\
  return r;\
}

DEF_INCREMENT(operator ++, add)
DEF_INCREMENT(operator --, sub)

template <typename T, typename Policy>
inline void
Checked_Number<T, Policy>::swap(Checked_Number<T, Policy>& y) {
  std::swap(v, y.v);
}

template <typename T, typename Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Checked_Number<T, Policy>& y) {
  v = y.v;
  return *this;
}

#define DEF_BINARY_ASSIGN(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<T, Policy>& y) { \
  check_result(Checked::fun<Policy>(v, v, y.v)); \
  return *this; \
}

DEF_BINARY_ASSIGN(operator +=, add)
DEF_BINARY_ASSIGN(operator -=, sub)
DEF_BINARY_ASSIGN(operator *=, mul)
DEF_BINARY_ASSIGN(operator /=, div)
DEF_BINARY_ASSIGN(operator %=, mod)

#define DEF_BINARY(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  T r; \
  check_result(Checked::fun<Policy>(r, x.raw_value(), y.raw_value())); \
  return r; \
}

DEF_BINARY(operator +, add)
DEF_BINARY(operator -, sub)
DEF_BINARY(operator *, mul)
DEF_BINARY(operator /, div)
DEF_BINARY(operator %, mod)

#define DEF_BINARY_OTHER(f, fun, type) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const type x, const Checked_Number<T, Policy>& y) { \
  T r; \
  check_result(Checked::assign<Policy>(r, x)); \
  check_result(Checked::fun<Policy>(r, r, y.raw_value())); \
  return r; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const type y) { \
  T r; \
  check_result(Checked::assign<Policy>(r, y)); \
  check_result(Checked::fun<Policy>(r, x.raw_value(), r)); \
  return r; \
}

#define DEF_BINARIES_OTHER(type) \
DEF_BINARY_OTHER(operator +, add, type) \
DEF_BINARY_OTHER(operator -, sub, type) \
DEF_BINARY_OTHER(operator *, mul, type) \
DEF_BINARY_OTHER(operator /, div, type) \
DEF_BINARY_OTHER(operator %, mod, type)

DEF_BINARIES_OTHER(signed char)
DEF_BINARIES_OTHER(short)
DEF_BINARIES_OTHER(int)
DEF_BINARIES_OTHER(long)
DEF_BINARIES_OTHER(long long)
DEF_BINARIES_OTHER(unsigned char)
DEF_BINARIES_OTHER(unsigned short)
DEF_BINARIES_OTHER(unsigned int)
DEF_BINARIES_OTHER(unsigned long)
DEF_BINARIES_OTHER(unsigned long long)
DEF_BINARIES_OTHER(float32_t)
DEF_BINARIES_OTHER(float64_t)
#ifdef FLOAT96_TYPE
DEF_BINARIES_OTHER(float96_t)
#endif
#ifdef FLOAT128_TYPE
DEF_BINARIES_OTHER(float128_t)
#endif
DEF_BINARIES_OTHER(mpz_class&)
DEF_BINARIES_OTHER(mpq_class&)

#define DEF_COMPARE(f, op) \
template <typename T, typename Policy> \
inline bool \
f(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  return x.raw_value() op y.raw_value(); \
}

DEF_COMPARE(operator ==, ==)
DEF_COMPARE(operator !=, !=)
DEF_COMPARE(operator >=, >=)
DEF_COMPARE(operator >, >)
DEF_COMPARE(operator <=, <=)
DEF_COMPARE(operator <, <)

#define DEF_COMPARE_OTHER(f, op, type) \
template <typename T, typename Policy> \
inline bool \
f(const type x, const Checked_Number<T, Policy>& y) { \
  T r; \
  check_result(Checked::assign<Policy>(r, x)); \
  return r op y.raw_value(); \
} \
template <typename T, typename Policy> \
inline bool \
f(const Checked_Number<T, Policy>& x, const type y) { \
  T r; \
  check_result(Checked::assign<Policy>(r, y)); \
  return x.raw_value() op r; \
}

#define DEF_COMPARES_OTHER(type) \
DEF_COMPARE_OTHER(operator ==, ==, type) \
DEF_COMPARE_OTHER(operator !=, !=, type) \
DEF_COMPARE_OTHER(operator >=, >=, type) \
DEF_COMPARE_OTHER(operator >, >, type) \
DEF_COMPARE_OTHER(operator <=, <=, type) \
DEF_COMPARE_OTHER(operator <, <, type)

DEF_COMPARES_OTHER(signed char)
DEF_COMPARES_OTHER(short)
DEF_COMPARES_OTHER(int)
DEF_COMPARES_OTHER(long)
DEF_COMPARES_OTHER(long long)
DEF_COMPARES_OTHER(unsigned char)
DEF_COMPARES_OTHER(unsigned short)
DEF_COMPARES_OTHER(unsigned int)
DEF_COMPARES_OTHER(unsigned long)
DEF_COMPARES_OTHER(unsigned long long)
DEF_COMPARES_OTHER(float32_t)
DEF_COMPARES_OTHER(float64_t)
#ifdef FLOAT96_TYPE
DEF_COMPARES_OTHER(float96_t)
#endif
#ifdef FLOAT128_TYPE
DEF_COMPARES_OTHER(float128_t)
#endif
DEF_COMPARES_OTHER(mpz_class&)
DEF_COMPARES_OTHER(mpq_class&)

template <typename T, typename Policy>
inline Checked_Number<T, Policy>
operator+(const Checked_Number<T, Policy>& x) {
  return x;
}

template <typename T, typename Policy>
inline Checked_Number<T, Policy>
operator-(const Checked_Number<T, Policy>& x) {
  T r;
  check_result(Checked::neg<Policy>(r, x.raw_value()));
  return r;
}

#define DEF_ASSIGN_FUN2_1(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x) { \
  check_result(Checked::fun<Policy>(x.raw_value(), x.raw_value())); \
}

#define DEF_ASSIGN_FUN2_2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  check_result(Checked::fun<Policy>(x.raw_value(), y.raw_value())); \
}

#define DEF_ASSIGN_FUN3_2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  check_result(Checked::fun<Policy>(x.raw_value(), x.raw_value(), y.raw_value())); \
}

#define DEF_ASSIGN_FUN3_3(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, \
  const Checked_Number<T, Policy>& y, const Checked_Number<T, Policy>& z) { \
  check_result(Checked::fun<Policy>(x.raw_value(), y.raw_value(), z.raw_value())); \
}

DEF_ASSIGN_FUN2_1(sqrt_assign, sqrt)
DEF_ASSIGN_FUN2_2(sqrt_assign, sqrt)

DEF_ASSIGN_FUN2_1(negate, neg)
DEF_ASSIGN_FUN2_2(negate, neg)

DEF_ASSIGN_FUN3_2(exact_div_assign, div)
DEF_ASSIGN_FUN3_3(exact_div_assign, div)

DEF_ASSIGN_FUN3_3(add_mul_assign, add_mul)

DEF_ASSIGN_FUN3_3(sub_mul_assign, sub_mul)

DEF_ASSIGN_FUN3_2(gcd_assign, gcd)
DEF_ASSIGN_FUN3_3(gcd_assign, gcd)

DEF_ASSIGN_FUN3_2(lcm_assign, lcm)
DEF_ASSIGN_FUN3_3(lcm_assign, lcm)


template <typename T, typename Policy>
inline int
sgn(const Checked_Number<T, Policy>& x) {
  Checked::Result r = Checked::sgn<Policy>(x.raw_value());
  switch (r) {
  case Checked::V_LT:
    return -1;
  case Checked::V_EQ:
    return 0;
  case Checked::V_GT:
    return 1;
  default:
    throw(0);
  }
}

template <typename T, typename Policy>
inline int
cmp(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) {
  Checked::Result r = Checked::cmp<Policy>(x.raw_value(), y.raw_value());
  switch (r) {
  case Checked::V_LT:
    return -1;
  case Checked::V_EQ:
    return 0;
  case Checked::V_GT:
    return 1;
  default:
    throw(0);
  }
}

template <typename T, typename Policy>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x) {
  check_result(Checked::print<Policy>(os, x.raw_value()));
  return os;
}
 
template <typename T, typename Policy>
inline std::istream& operator>>(std::istream& is, Checked_Number<T, Policy>& x) {
  check_result(Checked::input<Policy>(is, x.raw_value()));
  return is;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Checked_Number_inlines_hh)
