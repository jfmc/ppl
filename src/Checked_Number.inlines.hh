/* Abstract checked arithmetic with exception throwing
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

#include <stdexcept>

namespace Parma_Polyhedra_Library {

using namespace Checked;

static void
bad_result(Result r) {
  switch (r) {
  case V_NEG_OVERFLOW:
    throw std::overflow_error("Negative overflow.");
  case V_POS_OVERFLOW:
    throw std::overflow_error("Positive overflow.");
  case V_NAN:
    throw std::domain_error("Got Not A Number.");
  default:
    throw std::logic_error("Unexpected inexact computation.");
  }
}

static inline void
check_result(Result r) {
  if (r != V_EQ)
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
  check_result(assign<Policy>(v, y)); \
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

template <typename T, typename Policy>
inline T&
Checked_Number<T, Policy>::value() {
  return v;
}

template <typename T, typename Policy>
Checked_Number<T, Policy>::operator mpz_class() const {
  mpz_class r;
  Checked::assign<Policy>(r, v);
  return r;
}

template <typename T, typename Policy>
inline const T&
Checked_Number<T, Policy>::value() const {
  return v;
}

#define DEF_INCREMENT(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f() { \
  check_result(fun<Policy>(v, v, T(1))); \
  return *this; \
}\
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
Checked_Number<T, Policy>::f(int) {\
  T r = v;\
  check_result(fun<Policy>(v, v, T(1)));\
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
Checked_Number<T, Policy>::operator=(const Checked_Number<T, Policy> REF y) {
  v = y.v;
  return *this;
}

#define DEF_BINARY_ASSIGN(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<T, Policy> REF y) { \
  check_result(fun<Policy>(v, v, y.v)); \
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
f(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y) { \
  T r; \
  check_result(fun<Policy>(r, x.value(), y.value())); \
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
f(const type x, const Checked_Number<T, Policy> REF y) { \
  T r; \
  check_result(assign<Policy>(r, x)); \
  check_result(fun<Policy>(r, r, y.value())); \
  return r; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy> REF x, const type y) { \
  T r; \
  check_result(assign<Policy>(r, y)); \
  check_result(fun<Policy>(r, x.value(), r)); \
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
f(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y) { \
  return x.value() op y.value(); \
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
f(const type x, const Checked_Number<T, Policy> REF y) { \
  T r; \
  check_result(assign<Policy>(r, x)); \
  return r op y.value(); \
} \
template <typename T, typename Policy> \
inline bool \
f(const Checked_Number<T, Policy> REF x, const type y) { \
  T r; \
  check_result(assign<Policy>(r, y)); \
  return x.value() op r; \
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
operator+(const Checked_Number<T, Policy> REF x) {
  return x;
}

template <typename T, typename Policy>
inline Checked_Number<T, Policy>
operator-(const Checked_Number<T, Policy> REF x) {
  T r;
  check_result(neg<Policy>(r, x.value()));
  return r;
}

#define DEF_ASSIGN_FUN1(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x) { \
  check_result(fun<Policy>(x.value(), x.value())); \
} \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y) { \
  check_result(fun<Policy>(x.value(), y.value())); \
}

#define DEF_ASSIGN_FUN2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y) { \
  check_result(fun<Policy>(x.value(), x.value(), y.value())); \
} \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z) { \
  check_result(fun<Policy>(x.value(), y.value(), z.value())); \
}

DEF_ASSIGN_FUN1(sqrt_assign, sqrt)
DEF_ASSIGN_FUN1(negate, neg)
DEF_ASSIGN_FUN2(exact_div_assign, div)
DEF_ASSIGN_FUN2(gcd_assign, gcd)
DEF_ASSIGN_FUN2(lcm_assign, lcm)


template <typename T, typename Policy>
inline int
sgn(const Checked_Number<T, Policy> REF x) {
  return Checked::sgn(x.value());
}

template <typename T, typename Policy>
inline int
cmp(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y) {
  return cmp(x.value(), y.value());
}

template <typename T, typename Policy>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy> REF x) {
  return os << x.value();
}

template <typename T, typename Policy>
inline std::istream& operator>>(std::istream& is, Checked_Number<T, Policy>& x) {
  return is >> x.value();
}

} // namespace Parma_Polyhedra_Library
