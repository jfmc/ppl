/* Checked_Number class implementation: inline functions.
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

#ifndef PPL_Checked_Number_inlines_hh
#define PPL_Checked_Number_inlines_hh 1

#include "globals.types.hh"
#include <stdexcept>
#include <sstream>

namespace Parma_Polyhedra_Library {

inline void
Checked_Number_Default_Policy::handle_result(Result r) {
  if (is_special(r))
    throw_result_exception(r);
}

inline void
Extended_Number_Policy::handle_result(Result r) {
  switch (r) {
  case V_EQ:
  case V_LT:
  case V_GT:
  case V_GE:
  case V_LE:
    break;
  default:
    throw_result_exception(r);
    break;
  }
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number()
 : v(0) {
}

#if 0
template <typename T, typename Policy>
template <typename From, typename From_Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Checked_Number<From, From_Policy>& y) {
  Policy::handle_result(Checked::assign_ext<Policy, From_Policy>(v, y.raw_value(), Policy::ROUND_DEFAULT));
}
#endif

#define DEF_CTOR(type) \
template <typename T, typename Policy> \
inline \
Checked_Number<T, Policy>::Checked_Number(const type x) { \
  Policy::handle_result(Checked::assign_ext<Policy, Checked::Transparent_Policy>(v, x, Policy::ROUND_DEFAULT)); \
}

DEF_CTOR(signed char)
DEF_CTOR(signed short)
DEF_CTOR(signed int)
DEF_CTOR(signed long)
DEF_CTOR(signed long long)
DEF_CTOR(unsigned char)
DEF_CTOR(unsigned short)
DEF_CTOR(unsigned int)
DEF_CTOR(unsigned long)
DEF_CTOR(unsigned long long)
DEF_CTOR(float)
DEF_CTOR(double)
#if CXX_SUPPORTS_LONG_DOUBLE
DEF_CTOR(long double)
#endif
DEF_CTOR(mpq_class&)
DEF_CTOR(mpz_class&)

#undef DEF_CTOR

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const char* x) {
  std::istringstream s(x);
  Policy::handle_result(Checked::input<Policy>(v, s, Policy::ROUND_DEFAULT));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Not_A_Number& x) {
  Policy::handle_result(Checked::assign<Policy>(v, x, Policy::ROUND_DEFAULT));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Minus_Infinity& x) {
  Policy::handle_result(Checked::assign<Policy>(v, x, Policy::ROUND_DEFAULT));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Plus_Infinity& x) {
  Policy::handle_result(Checked::assign<Policy>(v, x, Policy::ROUND_DEFAULT));
}

template <typename T, typename Policy>
inline bool
is_minus_infinity(const T& x) {
  return Checked::is_minf<Checked::Transparent_Policy>(x);
}

template <typename T, typename Policy>
inline bool
is_plus_infinity(const T& x) {
  return Checked::is_pinf<Checked::Transparent_Policy>(x);
}

template <typename T, typename Policy>
inline bool
is_not_a_number(const T& x) {
  return Checked::is_nan<Checked::Transparent_Policy>(x);
}

template <typename T, typename Policy>
inline bool
is_integer(const T& x) {
  return Checked::is_int<Checked::Transparent_Policy>(x);
}

template <typename T, typename Policy>
inline bool
is_minus_infinity(const Checked_Number<T, Policy>& x) {
  return Checked::is_minf<Policy>(x.raw_value());
}

template <typename T, typename Policy>
inline bool
is_plus_infinity(const Checked_Number<T, Policy>& x) {
  return Checked::is_pinf<Policy>(x.raw_value());
}

template <typename T, typename Policy>
inline bool
is_not_a_number(const Checked_Number<T, Policy>& x) {
  return Checked::is_nan<Policy>(x.raw_value());
}

template <typename T, typename Policy>
inline bool
is_integer(const Checked_Number<T, Policy>& x) {
  return Checked::is_int<Policy>(x.raw_value());
}

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

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline const T&
raw_value(const Checked_Number<T, Policy>& x) {
  return x.raw_value();
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline T&
raw_value(Checked_Number<T, Policy>& x) {
  return x.raw_value();
}

template <typename T, typename Policy>
inline bool
Checked_Number<T, Policy>::OK() const {
  return true;
}

template <typename T, typename Policy>
inline Result
Checked_Number<T, Policy>::classify(bool nan, bool inf, bool sign) const {
  return Checked::classify<Policy>(v, nan, inf, sign);
}

namespace Checked {

inline memory_size_type
external_memory_in_bytes(const mpz_class& x) {
  return x.get_mpz_t()[0]._mp_alloc * SIZEOF_MP_LIMB_T;
}

inline memory_size_type
total_memory_in_bytes(const mpz_class& x) {
  return sizeof(x) + external_memory_in_bytes(x);
}

inline memory_size_type
external_memory_in_bytes(const mpq_class& x) {
  return external_memory_in_bytes(x.get_num())
    + external_memory_in_bytes(x.get_den());
}

inline memory_size_type
total_memory_in_bytes(const mpq_class& x) {
  return sizeof(x) + external_memory_in_bytes(x);
}

template <typename T>
inline memory_size_type
external_memory_in_bytes(T) {
  return 0;
}

template <typename T>
inline memory_size_type
total_memory_in_bytes(T) {
  return sizeof(T);
}

} // namespace Checked

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline memory_size_type
total_memory_in_bytes(const Checked_Number<T, Policy>& x) {
  return Checked::total_memory_in_bytes(raw_value(x));
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline memory_size_type
external_memory_in_bytes(const Checked_Number<T, Policy>& x) {
  return Checked::external_memory_in_bytes(raw_value(x));
}

template <typename To, typename To_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, const Minus_Infinity& x, Rounding_Dir dir) {
  return Checked::assign<To_Policy>(to.raw_value(), x, dir);
}
template <typename To, typename To_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, const Plus_Infinity& x, Rounding_Dir dir) {
  return Checked::assign<To_Policy>(to.raw_value(), x, dir);
}
template <typename To, typename To_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, const Not_A_Number& x, Rounding_Dir dir) {
  return Checked::assign<To_Policy>(to.raw_value(), x, dir);
}

template <typename To, typename To_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, const char* x, Rounding_Dir dir) {
  std::istringstream s(x);
  return Checked::input<To_Policy>(to.raw_value(), s, dir);
}

template <typename To, typename To_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, char* x, Rounding_Dir dir) {
  return assign(to, const_cast<const char *>(x), dir);
}

template <typename To, typename To_Policy,
	  typename From>
inline Result
assign(Checked_Number<To, To_Policy>& to, const From& x, Rounding_Dir dir) {
  return Checked::assign_ext<To_Policy, Checked::Transparent_Policy>(to.raw_value(), x, dir);
}
template <typename To, typename To_Policy,
	  typename From, typename From_Policy>
inline Result
assign(Checked_Number<To, To_Policy>& to, const Checked_Number<From, From_Policy>& x, Rounding_Dir dir) {
  return Checked::assign_ext<To_Policy, From_Policy>(to.raw_value(), x.raw_value(), dir);
}

#define FUNC1(name, func) \
template <typename To, typename To_Policy, \
          typename From> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const From& x, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Checked::Transparent_Policy>(to.raw_value(), x, dir); \
} \
template <typename To, typename To_Policy, \
          typename From, typename From_Policy> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const Checked_Number<From, From_Policy>& x, Rounding_Dir dir) { \
  return Checked::func<To_Policy, From_Policy>(to.raw_value(), x.raw_value(), dir); \
}

FUNC1(assign_neg, neg_ext)
FUNC1(assign_abs, abs_ext)
FUNC1(assign_sqrt, sqrt_ext)

#undef FUNC1

#define FUNC1(name, func) \
template <typename To, typename To_Policy, \
          typename From> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const From& x, int exp, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Checked::Transparent_Policy>(to.raw_value(), x, exp, dir); \
} \
template <typename To, typename To_Policy, \
          typename From, typename From_Policy> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const Checked_Number<From, From_Policy>& x, int exp, Rounding_Dir dir) { \
  return Checked::func<To_Policy, From_Policy>(to.raw_value(), x.raw_value(), exp, dir); \
}

FUNC1(assign_mul2exp, mul2exp_ext)
FUNC1(assign_div2exp, div2exp_ext)

#undef FUNC1

#define FUNC2(name, func) \
template <typename To, typename To_Policy, \
          typename From1, \
	  typename From2> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const From1& x, const From2& y, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Checked::Transparent_Policy, Checked::Transparent_Policy>(to.raw_value(), x, y, dir); \
} \
template <typename To, typename To_Policy, \
          typename From1, \
	  typename From2, typename Policy2> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const From1& x, const Checked_Number<From2, Policy2>& y, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Checked::Transparent_Policy, Policy2>(to.raw_value(), x, y.raw_value(), dir); \
} \
template <typename To, typename To_Policy, \
          typename From1, typename Policy1, \
	  typename From2> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const Checked_Number<From1, Policy1>& x, const From2& y, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Policy1, Checked::Transparent_Policy>(to.raw_value(), x.raw_value(), y, dir); \
} \
template <typename To, typename To_Policy, \
          typename From1, typename Policy1, \
	  typename From2, typename Policy2> \
inline Result \
name(Checked_Number<To, To_Policy>& to, const Checked_Number<From1, Policy1>& x, const Checked_Number<From2, Policy2>& y, Rounding_Dir dir) { \
  return Checked::func<To_Policy, Policy1, Policy2>(to.raw_value(), x.raw_value(), y.raw_value(), dir); \
}

FUNC2(assign_add, add_ext)
FUNC2(assign_sub, sub_ext)
FUNC2(assign_mul, mul_ext)
FUNC2(assign_div, div_ext)
FUNC2(assign_rem, rem_ext)
FUNC2(assign_gcd, gcd_ext)
FUNC2(assign_lcm, lcm_ext)
FUNC2(assign_add_mul, add_mul_ext)
FUNC2(assign_sub_mul, sub_mul_ext)

#undef FUNC2

#define DEF_INCREMENT(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f() { \
  Policy::handle_result(fun(*this, *this, T(1), Policy::ROUND_DEFAULT)); \
  return *this; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
Checked_Number<T, Policy>::f(int) {\
  T r = v;\
  Policy::handle_result(fun(*this, *this, T(1), Policy::ROUND_DEFAULT));\
  return r;\
}

DEF_INCREMENT(operator ++, assign_add)
DEF_INCREMENT(operator --, assign_sub)

#undef DEF_INCREMENT

template <typename T, typename Policy>
inline void
swap(Checked_Number<T, Policy>& x, Checked_Number<T, Policy>& y) {
  std::swap(x.raw_value(), y.raw_value());
}

template <typename T, typename Policy>
template <typename From, typename From_Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Checked_Number<From, From_Policy>& y) {
  Policy::handle_result(assign(*this, y, Policy::ROUND_DEFAULT));
  return *this;
}
template <typename T, typename Policy>
template <typename From>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const From& y) {
  Policy::handle_result(assign(*this, y, Policy::ROUND_DEFAULT));
  return *this;
}

#define DEF_BINARY_OP_ASSIGN(f, fun) \
template <typename T, typename Policy> \
template <typename From_Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<T, From_Policy>& y) { \
  Policy::handle_result(fun(*this, *this, y, Policy::ROUND_DEFAULT)); \
  return *this; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const T& y) { \
  Policy::handle_result(fun(*this, *this, y, Policy::ROUND_DEFAULT)); \
  return *this; \
} \
template <typename T, typename Policy> \
template <typename From, typename From_Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<From, From_Policy>& y) { \
  Checked_Number<T, Policy> cy(y); \
  Policy::handle_result(fun(*this, *this, cy, Policy::ROUND_DEFAULT)); \
  return *this; \
} \
template <typename T, typename Policy> \
template <typename From> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const From& y) { \
  Checked_Number<T, Policy> cy(y); \
  Policy::handle_result(fun(*this, *this, cy, Policy::ROUND_DEFAULT)); \
  return *this; \
}

DEF_BINARY_OP_ASSIGN(operator +=, assign_add)
DEF_BINARY_OP_ASSIGN(operator -=, assign_sub)
DEF_BINARY_OP_ASSIGN(operator *=, assign_mul)
DEF_BINARY_OP_ASSIGN(operator /=, assign_div)
DEF_BINARY_OP_ASSIGN(operator %=, assign_rem)

#undef DEF_BINARY_OP_ASSIGN

#define DEF_BINARY_OP_TYPE(f, fun, Type) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Type x, const Checked_Number<T, Policy>& y) { \
  Checked_Number<T, Policy> r(x); \
  Policy::handle_result(fun(r, r, y, Policy::ROUND_DEFAULT)); \
  return r; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const Type y) { \
  Checked_Number<T, Policy> r(y); \
  Policy::handle_result(fun(r, x, r, Policy::ROUND_DEFAULT)); \
  return r; \
}

#define DEF_BINARY_OP(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  Checked_Number<T, Policy> r; \
  Policy::handle_result(fun(r, x, y, Policy::ROUND_DEFAULT)); \
  return r; \
} \
DEF_BINARY_OP_TYPE(f, fun, signed char) \
DEF_BINARY_OP_TYPE(f, fun, signed short) \
DEF_BINARY_OP_TYPE(f, fun, signed int) \
DEF_BINARY_OP_TYPE(f, fun, signed long) \
DEF_BINARY_OP_TYPE(f, fun, signed long long) \
DEF_BINARY_OP_TYPE(f, fun, unsigned char) \
DEF_BINARY_OP_TYPE(f, fun, unsigned short) \
DEF_BINARY_OP_TYPE(f, fun, unsigned int) \
DEF_BINARY_OP_TYPE(f, fun, unsigned long) \
DEF_BINARY_OP_TYPE(f, fun, unsigned long long) \
DEF_BINARY_OP_TYPE(f, fun, float) \
DEF_BINARY_OP_TYPE(f, fun, double) \
DEF_BINARY_OP_TYPE(f, fun, long double) \
DEF_BINARY_OP_TYPE(f, fun, mpz_class&) \
DEF_BINARY_OP_TYPE(f, fun, mpq_class&)

DEF_BINARY_OP(operator +, assign_add)
DEF_BINARY_OP(operator -, assign_sub)
DEF_BINARY_OP(operator *, assign_mul)
DEF_BINARY_OP(operator /, assign_div)
DEF_BINARY_OP(operator %, assign_rem)

#undef DEF_BINARY_OP_TYPE
#undef DEF_BINARY_OP

#define DEF_COMPARE_TYPE(f, fun, Type) \
template <typename From, typename From_Policy> \
inline bool \
f(const Type x, const Checked_Number<From, From_Policy>& y) { \
  return Checked::fun<Checked::Transparent_Policy, From_Policy>(x, y.raw_value()); \
} \
template <typename From, typename From_Policy> \
inline bool \
f(const Checked_Number<From, From_Policy>& x, const Type y) { \
  return Checked::fun<From_Policy, Checked::Transparent_Policy>(x.raw_value(), y); \
}

#define DEF_COMPARE(f, fun) \
template <typename T1, typename Policy1, \
          typename T2, typename Policy2> \
inline bool \
f(const Checked_Number<T1, Policy1>& x, const Checked_Number<T2, Policy2>& y) { \
  return Checked::fun<Policy1, Policy2>(x.raw_value(), y.raw_value()); \
} \
DEF_COMPARE_TYPE(f, fun, signed char) \
DEF_COMPARE_TYPE(f, fun, signed short) \
DEF_COMPARE_TYPE(f, fun, signed int) \
DEF_COMPARE_TYPE(f, fun, signed long) \
DEF_COMPARE_TYPE(f, fun, signed long long) \
DEF_COMPARE_TYPE(f, fun, unsigned char) \
DEF_COMPARE_TYPE(f, fun, unsigned short) \
DEF_COMPARE_TYPE(f, fun, unsigned int) \
DEF_COMPARE_TYPE(f, fun, unsigned long) \
DEF_COMPARE_TYPE(f, fun, unsigned long long) \
DEF_COMPARE_TYPE(f, fun, float) \
DEF_COMPARE_TYPE(f, fun, double) \
DEF_COMPARE_TYPE(f, fun, long double) \
DEF_COMPARE_TYPE(f, fun, mpz_class&) \
DEF_COMPARE_TYPE(f, fun, mpq_class&)


DEF_COMPARE(operator ==, eq_ext)
DEF_COMPARE(operator !=, ne_ext)
DEF_COMPARE(operator >=, ge_ext)
DEF_COMPARE(operator >, gt_ext)
DEF_COMPARE(operator <=, le_ext)
DEF_COMPARE(operator <, lt_ext)

#undef DEF_COMPARE_TYPE
#undef DEF_COMPARE

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline Checked_Number<T, Policy>
operator+(const Checked_Number<T, Policy>& x) {
  return x;
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline Checked_Number<T, Policy>
operator-(const Checked_Number<T, Policy>& x) {
  Checked_Number<T, Policy> r;
  Policy::handle_result(assign_neg(r, x, Policy::ROUND_DEFAULT));
  return r;
}

#define DEF_ASSIGN_FUN2_1(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x) { \
  Policy::handle_result(fun(x, x, Policy::ROUND_DEFAULT)); \
}

#define DEF_ASSIGN_FUN2_2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  Policy::handle_result(fun(x, y, Policy::ROUND_DEFAULT)); \
}

#define DEF_ASSIGN_FUN3_2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  Policy::handle_result(fun(x, x, y, Policy::ROUND_DEFAULT)); \
}

#define DEF_ASSIGN_FUN3_3(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y, const Checked_Number<T, Policy>& z) { \
  Policy::handle_result(fun(x, y, z, Policy::ROUND_DEFAULT)); \
}

DEF_ASSIGN_FUN2_1(sqrt_assign, assign_sqrt)
DEF_ASSIGN_FUN2_2(sqrt_assign, assign_sqrt)

DEF_ASSIGN_FUN2_1(negate, assign_neg)
DEF_ASSIGN_FUN2_2(negate, assign_neg)

DEF_ASSIGN_FUN3_2(exact_div_assign, assign_div)
DEF_ASSIGN_FUN3_3(exact_div_assign, assign_div)

DEF_ASSIGN_FUN3_3(add_mul_assign, assign_add_mul)

DEF_ASSIGN_FUN3_3(sub_mul_assign, assign_sub_mul)

DEF_ASSIGN_FUN3_2(gcd_assign, assign_gcd)
DEF_ASSIGN_FUN3_3(gcd_assign, assign_gcd)

DEF_ASSIGN_FUN3_2(lcm_assign, assign_lcm)
DEF_ASSIGN_FUN3_3(lcm_assign, assign_lcm)

#undef DEF_ASSIGN_FUN2_1
#undef DEF_ASSIGN_FUN2_2
#undef DEF_ASSIGN_FUN3_2
#undef DEF_ASSIGN_FUN3_3

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline int
sgn(const Checked_Number<T, Policy>& x) {
  Result r = Checked::sgn_ext<Policy>(x.raw_value());
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

/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
inline int
cmp(const Checked_Number<T1, Policy1>& x,
    const Checked_Number<T2, Policy2>& y) {
  Result r = Checked::cmp_ext<Policy1, Policy2>(x.raw_value(), y.raw_value());
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

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x) {
  Policy::handle_result(Checked::output_ext<Policy>(os, x.raw_value(), Numeric_Format(), Policy::ROUND_DEFAULT));
  return os;
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline std::istream& operator>>(std::istream& is, Checked_Number<T, Policy>& x) {
  Result r = Checked::input_ext<Policy>(x.raw_value(), is, Policy::ROUND_DEFAULT);
  if (r == V_CVT_STR_UNK)
    is.setstate(std::ios::failbit);
  else
    Policy::handle_result(r);
  return is;
}

template <typename T>
T
plus_infinity() {
  return PLUS_INFINITY;
}

template <typename T>
T
minus_infinity() {
  return MINUS_INFINITY;
}

template <typename T>
T
not_a_number() {
  return NOT_A_NUMBER;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Checked_Number_inlines_hh)
