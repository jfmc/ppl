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
bad_result(Result r)
{
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
check_result(Result r)
{
  if (r != V_EQ)
    bad_result(r);
}

template <typename T>
inline
Checked_Number<T>::Checked_Number()
 : v(0) {
}

template <typename T>
inline
Checked_Number<T>::Checked_Number(const T REF y)
  : v(y) {
}

template <typename T>
template <typename T1>
inline
Checked_Number<T>::Checked_Number(const T1 REF y) {
  check_result(assign<Checked_Number_Policy>(v, y));
}

template <typename T>
inline T&
Checked_Number<T>::value() {
  return v;
}

template <typename T>
inline const T&
Checked_Number<T>::value() const {
  return v;
}

#define DEF_INCREMENT(f, fun) \
template <typename T> \
inline Checked_Number<T>& \
Checked_Number<T>::f() { \
  check_result(fun<Checked_Number_Policy>(v, v, T(1))); \
  return *this; \
}\
template <typename T> \
inline Checked_Number<T> \
Checked_Number<T>::f(int) {\
  T r = v;\
  check_result(fun<Checked_Number_Policy>(v, v, T(1)));\
  return r;\
}

DEF_INCREMENT(operator ++, add)
DEF_INCREMENT(operator --, sub)

template <typename T>
inline void
Checked_Number<T>::swap(Checked_Number<T>& y) {
  std::swap(v, y.v);
}

template <typename T>
inline Checked_Number<T>&
Checked_Number<T>::operator=(const Checked_Number<T> REF y) {
  v = y.v;
}

#define DEF_BINARY_ASSIGN(f, fun) \
template <typename T> \
inline Checked_Number<T>& \
Checked_Number<T>::f(const Checked_Number<T> REF y) { \
  check_result(fun<Checked_Number_Policy>(v, v, y.v)); \
  return *this; \
}

DEF_BINARY_ASSIGN(operator +=, add)
DEF_BINARY_ASSIGN(operator -=, sub)
DEF_BINARY_ASSIGN(operator *=, mul)
DEF_BINARY_ASSIGN(operator /=, div)
DEF_BINARY_ASSIGN(operator %=, mod)

#define DEF_BINARY(f, fun) \
template <typename T> \
inline Checked_Number<T> \
f(const Checked_Number<T> REF x, const Checked_Number<T> REF y) { \
  T r; \
  check_result(fun<Checked_Number_Policy>(r, x.value(), y.value())); \
  return r; \
} \
template <typename T, typename T1> \
inline Checked_Number<T> \
f(const T1 x, const Checked_Number<T> REF y) { \
  T r; \
  check_result(assign<Checked_Number_Policy>(r, x)); \
  check_result(fun<Checked_Number_Policy>(r, r, y.value())); \
  return r; \
} \
template <typename T, typename T1> \
inline Checked_Number<T> \
f(const Checked_Number<T> REF x, const T1 y) { \
  T r; \
  check_result(assign<Checked_Number_Policy>(r, y)); \
  check_result(fun<Checked_Number_Policy>(r, x.value(), r)); \
  return r; \
}

DEF_BINARY(operator +, add)
DEF_BINARY(operator -, sub)
DEF_BINARY(operator *, mul)
DEF_BINARY(operator /, div)
DEF_BINARY(operator %, mod)

#define DEF_COMPARE(f, op) \
template <typename T> \
inline bool \
f(const Checked_Number<T> REF x, const Checked_Number<T> REF y) { \
  return x.value() op y.value(); \
} \
template <typename T, typename T1> \
inline bool \
f(const T1 x, const Checked_Number<T> REF y) { \
  T r; \
  check_result(assign<Checked_Number_Policy>(r, x)); \
  return r op y.value(); \
} \
template <typename T, typename T1> \
inline bool \
f(const Checked_Number<T> REF x, const T1 y) { \
  T r; \
  check_result(assign<Checked_Number_Policy>(r, y)); \
  return x.value() op r; \
}

DEF_COMPARE(operator ==, ==)
DEF_COMPARE(operator !=, !=)
DEF_COMPARE(operator >=, >=)
DEF_COMPARE(operator >, >)
DEF_COMPARE(operator <=, <=)
DEF_COMPARE(operator <, <)

template <typename T>
inline Checked_Number<T>
operator+(const Checked_Number<T> REF x) {
  return x;
}

template <typename T>
inline Checked_Number<T>
operator-(const Checked_Number<T> REF x) {
  T r;
  check_result(neg<Checked_Number_Policy>(r, x.value()));
  return r;
}

#define DEF_ASSIGN_FUN1(f, fun) \
template <typename T> \
inline void \
f(Checked_Number<T>& x) { \
  check_result(fun<Checked_Number_Policy>(x.value(), x.value())); \
} \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y) { \
  check_result(fun<Checked_Number_Policy>(x.value(), y.value())); \
}

#define DEF_ASSIGN_FUN2(f, fun) \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y) { \
  check_result(fun<Checked_Number_Policy>(x.v, x.value(), y.value())); \
} \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y, const Checked_Number<T> REF z) { \
  check_result(fun<Checked_Number_Policy>(x.value(), y.value(), z.value())); \
}

DEF_ASSIGN_FUN1(sqrt_assign, sqrt)
DEF_ASSIGN_FUN1(negate, neg)
DEF_ASSIGN_FUN2(exact_div_assign, div)
DEF_ASSIGN_FUN2(gcd_assign, gcd)
DEF_ASSIGN_FUN2(lcm_assign, lcm)


template <typename T>
inline int
sgn(const Checked_Number<T> x) {
  return sgn(x.v);
}

template <typename T>
inline int
cmp(const Checked_Number<T> x, const Checked_Number<T> y) {
  return cmp(x.v, y.v);
}

template <typename T>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Number<T> REF x)
{
  return os << x.v;
}

template <typename T>
inline std::istream& operator>>(std::istream& is, Checked_Number<T>& x)
{
  return is >> x.v;
}

} // namespace Parma_Polyhedra_Library
