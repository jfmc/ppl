/* Extended_Number class implementation: inline functions.
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

#ifndef PPL_Extended_Number_inlines_hh
#define PPL_Extended_Number_inlines_hh 1

#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <typename T, typename Policy>
inline void
Extended_Number<T, Policy>::check_result(Result) {
// Reserved for future implementation according to the policy.
}

template <typename T, typename Policy>
inline T&
Extended_Number<T, Policy>::raw_value() {
  return v;
}

template <typename T, typename Policy>
inline const T&
Extended_Number<T, Policy>::raw_value() const {
  return v;
}

template <typename T, typename Policy>
inline Result
Extended_Number<T, Policy>::classify(bool nan, bool inf, bool sign) {
  return Checked::classify<Policy>(v, nan, inf, sign);
}

template <typename T, typename Policy>
inline
Extended_Number<T, Policy>::Extended_Number() {
}

template <typename T, typename Policy>
inline Result
Extended_Number<T, Policy>::assign(const Minus_Infinity&, const Rounding&) {
  return Checked::set_special<Policy>(v, VC_MINUS_INFINITY);
}

template <typename T, typename Policy>
inline Result
Extended_Number<T, Policy>::assign(const Plus_Infinity&, const Rounding&) {
  return Checked::set_special<Policy>(v, VC_PLUS_INFINITY);
}

template <typename T, typename Policy>
inline Result
Extended_Number<T, Policy>::assign(const Not_A_Number&, const Rounding&) {
  return Checked::set_special<Policy>(v, VC_NAN);
}

#define FUNC1(name, func) \
template <typename T, typename Policy> \
template <typename From> \
inline Result \
Extended_Number<T, Policy>::name(const From& x, const Rounding& mode) { \
  return Checked::func<Policy, Checked::Transparent_Policy>(v, x, mode); \
} \
template <typename T, typename Policy> \
template <typename From, typename From_Policy> \
inline Result \
Extended_Number<T, Policy>::name(const Extended_Number<From, From_Policy>& x, const Rounding& mode) { \
  return Checked::func<Policy, From_Policy>(v, x.raw_value(), mode); \
}

FUNC1(assign, assign_ext)
FUNC1(assign_neg, neg_ext)
FUNC1(assign_abs, abs_ext)
FUNC1(assign_sqrt, sqrt_ext)

#undef FUNC1

#define FUNC2(name, func) \
template <typename T, typename Policy> \
template <typename From1, \
	  typename From2> \
inline Result \
Extended_Number<T, Policy>::name(const From1& x, const From2& y, const Rounding& mode) { \
  return Checked::func<Policy, Checked::Transparent_Policy, Checked::Transparent_Policy>(v, x, y, mode); \
} \
template <typename T, typename Policy> \
template <typename From1, \
	  typename From2, typename From2_Policy> \
inline Result \
Extended_Number<T, Policy>::name(const From1& x, const Extended_Number<From2, From2_Policy>& y, const Rounding& mode) { \
  return Checked::func<Policy, Checked::Transparent_Policy, From2_Policy>(v, x, y.raw_value(), mode); \
} \
template <typename T, typename Policy> \
template <typename From1, typename From1_Policy, \
	  typename From2> \
inline Result \
Extended_Number<T, Policy>::name(const Extended_Number<From1, From1_Policy>& x, const From2& y, const Rounding& mode) { \
  return Checked::func<Policy, From1_Policy, Checked::Transparent_Policy>(v, x.raw_value(), y, mode); \
} \
template <typename T, typename Policy> \
template <typename From1, typename From1_Policy, \
	  typename From2, typename From2_Policy> \
inline Result \
Extended_Number<T, Policy>::name(const Extended_Number<From1, From1_Policy>& x, const Extended_Number<From2, From2_Policy>& y, const Rounding& mode) { \
  return Checked::func<Policy, From1_Policy, From2_Policy>(v, x.raw_value(), y.raw_value(), mode); \
}

  FUNC2(assign_add, add_ext)
  FUNC2(assign_sub, sub_ext)
  FUNC2(assign_mul, mul_ext)
  FUNC2(assign_div, div_ext)
  FUNC2(assign_mod, mod_ext)

#undef FUNC2

template <typename From1, typename From1_Policy,
	  typename From2, typename From2_Policy>
inline Result
compare(const Extended_Number<From1, From1_Policy>& x, const Extended_Number<From2, From2_Policy>& y) { \
  return Checked::cmp_ext<From1_Policy, From2_Policy>(x.raw_value(), y.raw_value()); \
}

template <typename T, typename Policy>
inline const T&
raw_value(const Extended_Number<T, Policy>& x) {
  return x.raw_value();
}

template <typename T, typename Policy>
inline T&
raw_value(Extended_Number<T, Policy>& x) {
  return x.raw_value();
}

template <typename T, typename Policy>
template <typename From>
Extended_Number<T, Policy>::Extended_Number(const From& y) {
  check_result(assign(y, Rounding::CURRENT));
}

#define DEF_INCREMENT(f, fun) \
template <typename T, typename Policy> \
inline Extended_Number<T, Policy>& \
Extended_Number<T, Policy>::f() { \
  check_result(fun(*this, T(1), Rounding(Rounding::CURRENT))); \
  return *this; \
}\
template <typename T, typename Policy> \
inline Extended_Number<T, Policy> \
Extended_Number<T, Policy>::f(int) {\
  T r = v;\
  check_result(fun(*this, T(1), Rounding(Rounding::CURRENT)));\
  return r;\
}

DEF_INCREMENT(operator ++, assign_add)
DEF_INCREMENT(operator --, sssign_add)

template <typename T, typename Policy>
inline void
Extended_Number<T, Policy>::swap(Extended_Number<T, Policy>& y) {
  std::swap(v, y.v);
}

#define DEF_BINARY_ASSIGN(f, fun) \
template <typename T, typename Policy> \
template <typename From> \
inline Extended_Number<T, Policy>& \
Extended_Number<T, Policy>::f(const From& y) { \
  check_result(fun(*this, y, Rounding(Rounding::CURRENT))); \
  return *this; \
}

DEF_BINARY_ASSIGN(operator =, assign)
DEF_BINARY_ASSIGN(operator +=, assign_add)
DEF_BINARY_ASSIGN(operator -=, assign_sub)
DEF_BINARY_ASSIGN(operator *=, assign_mul)
DEF_BINARY_ASSIGN(operator /=, assign_div)
DEF_BINARY_ASSIGN(operator %=, assign_mod)

#define DEF_BINARY(f, fun) \
template <typename T, typename Policy> \
inline Extended_Number<T, Policy> \
f(const Extended_Number<T, Policy>& x, const Extended_Number<T, Policy>& y) { \
  Extended_Number<T, Policy> r; \
  Extended_Number<T, Policy>::check_result(r.fun(x, y, Rounding(Rounding::CURRENT))); \
  return r; \
}\
template <typename T, typename Policy, typename From> \
inline Extended_Number<T, Policy> \
f(const From& x, const Extended_Number<T, Policy>& y) { \
  Extended_Number<T, Policy> r; \
  Extended_Number<T, Policy>::check_result(r.fun(x, y, Rounding(Rounding::CURRENT))); \
  return r; \
}\
template <typename T, typename Policy, typename From> \
inline Extended_Number<T, Policy> \
f(const Extended_Number<T, Policy>& x, const From& y) { \
  Extended_Number<T, Policy> r; \
  Extended_Number<T, Policy>::check_result(r.fun(x, y, Rounding(Rounding::CURRENT))); \
  return r; \
}

DEF_BINARY(operator +, assign_add)
DEF_BINARY(operator -, assign_sub)
DEF_BINARY(operator *, assign_mul)
DEF_BINARY(operator /, assign_div)
DEF_BINARY(operator %, assign_mod)

#define DEF_COMPARE(f, fun) \
template <typename From1, typename From1_Policy, \
          typename From2, typename From2_Policy> \
inline bool \
f(const Extended_Number<From1, From1_Policy>& x, const Extended_Number<From2, From2_Policy>& y) { \
  return Checked::fun<From1_Policy, From2_Policy>(x.raw_value(), y.raw_value()); \
}\
template <typename From1, \
          typename From2, typename From2_Policy> \
inline bool \
f(const From1& x, const Extended_Number<From2, From2_Policy>& y) { \
  return Checked::fun<Checked::Transparent_Policy, From2_Policy>(x, y.raw_value()); \
}\
template <typename From1, typename From1_Policy, \
          typename From2> \
inline bool \
f(const Extended_Number<From1, From1_Policy>& x, const From2& y) { \
  return Checked::fun<From1_Policy, Checked::Transparent_Policy>(x.raw_value(), y); \
}

DEF_COMPARE(operator ==, eq_ext)
DEF_COMPARE(operator !=, ne_ext)
DEF_COMPARE(operator >=, ge_ext)
DEF_COMPARE(operator >, gt_ext)
DEF_COMPARE(operator <=, le_ext)
DEF_COMPARE(operator <, lt_ext)

/*! \relates Extended_Number */
template <typename T, typename Policy>
inline Extended_Number<T, Policy>
operator+(const Extended_Number<T, Policy>& x) {
  return x;
}

/*! \relates Extended_Number */
template <typename T, typename Policy>
inline Extended_Number<T, Policy>
operator-(const Extended_Number<T, Policy>& x) {
  Extended_Number<T, Policy> r;
  Extended_Number<T, Policy>::check_result(r.assign_neg(x, Rounding(Rounding::CURRENT)));
  return r;
}

/*! \relates Extended_Number */
template <typename T, typename Policy>
inline int
sgn(const Extended_Number<T, Policy>& x) {
  Result r = x.classify(false, false, true);
  switch (r) {
  case V_LT:
    return -1;
  case V_EQ:
    return 0;
  case V_GT:
    return 1;
  default:
    throw(0);
  }
}

/*! \relates Extended_Number */
template <typename From1, typename From2>
inline int
cmp(const From1& x, const From2& y) {
  Result r = compare(x, y);
  switch (r) {
  case V_LT:
    return -1;
  case V_EQ:
    return 0;
  case V_GT:
    return 1;
  default:
    throw(0);
  }
}

/*! \relates Extended_Number */
template <typename T, typename Policy>
inline std::ostream&
operator<<(std::ostream& os, const Extended_Number<T, Policy>& x) {
  Extended_Number<T, Policy>::check_result(Checked::print_ext<Policy>(os, x.raw_value(), Numeric_Format(), Rounding(Rounding::CURRENT)));
  return os;
}

#if 0
/*! \relates Extended_Number */
template <typename T, typename Policy>
inline std::istream& operator>>(std::istream& is, Extended_Number<T, Policy>& x) {
  Extended_Number<T, Policy>::check_result(Checked::input_ext<Policy>(is, x.raw_value(), Rounding(Rounding::CURRENT)));
  return is;
}
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Extended_Number_inlines_hh)
