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

#include "checked.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

static void
bad_result(Result_Info r)
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
check_result(Result_Info r)
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
  check_result(checked_assignexact(v, y));
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
  check_result(fun(v, v, T(1))); \
  return *this; \
}\
template <typename T> \
inline Checked_Number<T> \
Checked_Number<T>::f(int) {\
  T r = v;\
  check_result(fun(v, v, T(1)));\
  return r;\
}

DEF_INCREMENT(operator ++, checked_addexact)
DEF_INCREMENT(operator --, checked_subexact)

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
  check_result(fun(v, v, y.v)); \
  return *this; \
}

DEF_BINARY_ASSIGN(operator +=, checked_addexact)
DEF_BINARY_ASSIGN(operator -=, checked_subexact)
DEF_BINARY_ASSIGN(operator *=, checked_mulexact)
DEF_BINARY_ASSIGN(operator /=, checked_divexact)
DEF_BINARY_ASSIGN(operator %=, checked_modexact)

#define DEF_BINARY(f, fun) \
template <typename T> \
inline Checked_Number<T> \
f(const Checked_Number<T> REF x, const Checked_Number<T> REF y) { \
  T r; \
  check_result(fun(r, x.value(), y.value())); \
  return r; \
} \
template <typename T, typename T1> \
inline Checked_Number<T> \
f(const T1 x, const Checked_Number<T> REF y) { \
  T r; \
  check_result(checked_assignexact(r, x)); \
  check_result(fun(r, r, y.value())); \
  return r; \
} \
template <typename T, typename T1> \
inline Checked_Number<T> \
f(const Checked_Number<T> REF x, const T1 y) { \
  T r; \
  check_result(checked_assignexact(r, y)); \
  check_result(fun(r, x.value(), r)); \
  return r; \
}

DEF_BINARY(operator +, checked_addexact)
DEF_BINARY(operator -, checked_subexact)
DEF_BINARY(operator *, checked_mulexact)
DEF_BINARY(operator /, checked_divexact)
DEF_BINARY(operator %, checked_modexact)

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
  check_result(checked_assignexact(r, x)); \
  return r op y.value(); \
} \
template <typename T, typename T1> \
inline bool \
f(const Checked_Number<T> REF x, const T1 y) { \
  T r; \
  check_result(checked_assignexact(r, y)); \
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
  check_result(checked_negexact(r, x.value()));
  return r;
}

#define DEF_ASSIGN_FUN1(f, fun) \
template <typename T> \
inline void \
f(Checked_Number<T>& x) { \
  check_result(fun(x.value(), x.value())); \
} \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y) { \
  check_result(fun(x.value(), y.value())); \
}

#define DEF_ASSIGN_FUN2(f, fun) \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y) { \
  check_result(fun(x.v, x.value(), y.value())); \
} \
template <typename T> \
inline void \
f(Checked_Number<T>& x, const Checked_Number<T> REF y, const Checked_Number<T> REF z) { \
  check_result(fun(x.value(), y.value(), z.value())); \
}

DEF_ASSIGN_FUN1(sqrt_assign, checked_sqrtexact)
DEF_ASSIGN_FUN1(negate, checked_negexact)
DEF_ASSIGN_FUN2(exact_div_assign, checked_divexact)
DEF_ASSIGN_FUN2(gcd_assign, checked_gcd)
DEF_ASSIGN_FUN2(lcm_assign, checked_lcm)


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
