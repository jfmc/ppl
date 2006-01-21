/* Checked_Number class implementation: inline functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <stdexcept>
#include <sstream>

namespace Parma_Polyhedra_Library {

inline Rounding_Dir
rounding_dir(Rounding_Dir dir) {
  if (dir == ROUND_NOT_NEEDED) {
#ifdef DEBUG_ROUND_NOT_NEEDED
    return ROUND_DIRECT;
#else
    return ROUND_IGNORE;
#endif
  }
  return dir;
}

inline Result
check_result(Result r, Rounding_Dir dir) {
  if (dir == ROUND_NOT_NEEDED && !is_special(r)) {
#ifdef DEBUG_ROUND_NOT_NEEDED
    // FIXME: this is wrong. If an overflow happens the Result may be V_LT or V_GT. What's the better way to cope with that?
    assert(r == V_EQ);
#else
    return V_EQ;
#endif
  }
  return r;
}


inline void
Checked_Number_Transparent_Policy::handle_result(Result) {
}

inline void
Checked_Number_Default_Policy::handle_result(Result r) {
  if (is_special(r))
    throw_result_exception(r);
}

inline void
Extended_Number_Policy::handle_result(Result r) {
  if (is_special(r))
    throw_result_exception(r);
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number()
 : v(0) {
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Checked_Number& y) {
  // TODO: avoid default construction of value member
  Checked::copy<Policy>(v, y.raw_value());
}

template <typename T, typename Policy>
template <typename From, typename From_Policy>
inline
Checked_Number<T, Policy>
::Checked_Number(const Checked_Number<From, From_Policy>& y) {
  // TODO: avoid default construction of value member
  Rounding_Dir dir = Policy::ROUND_DEFAULT_CONSTRUCTOR;
  Policy::handle_result(check_result(Checked::assign_ext<Policy, From_Policy>
				     (v,
				      y.raw_value(),
				      rounding_dir(dir)),
				     dir));
}

// TODO: avoid default construction of value member
#define DEF_CTOR(type) \
template <typename T, typename Policy> \
inline \
Checked_Number<T, Policy>::Checked_Number(const type x, Rounding_Dir dir) { \
  Policy::handle_result(check_result(Checked::assign_ext<Policy, Default_From_Policy>(v, x, rounding_dir(dir)), dir)); \
} \
template <typename T, typename Policy> \
inline \
Checked_Number<T, Policy>::Checked_Number(const type x) { \
  Rounding_Dir dir = Policy::ROUND_DEFAULT_CONSTRUCTOR; \
  Policy::handle_result(check_result(Checked::assign_ext<Policy, Default_From_Policy>(v, x, rounding_dir(dir)), dir)); \
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
DEF_CTOR(long double)
DEF_CTOR(mpq_class&)
DEF_CTOR(mpz_class&)

#undef DEF_CTOR

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const char* x, Rounding_Dir dir) {
  std::istringstream s(x);
  Policy::handle_result(check_result(Checked::input<Policy>(v, s, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const char* x) {
  std::istringstream s(x);
  Rounding_Dir dir = Policy::ROUND_DEFAULT_CONSTRUCTOR;
  Policy::handle_result(check_result(Checked::input<Policy>(v, s, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Not_A_Number& x, Rounding_Dir dir) {
  // TODO: avoid default construction of value member
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Not_A_Number& x) {
  // TODO: avoid default construction of value member
  Rounding_Dir dir = ROUND_IGNORE;
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Minus_Infinity& x, Rounding_Dir dir) {
  // TODO: avoid default construction of value member
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Minus_Infinity& x) {
  // TODO: avoid default construction of value member
  Rounding_Dir dir = Policy::ROUND_DEFAULT_CONSTRUCTOR_INF;
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Plus_Infinity& x, Rounding_Dir dir) {
  // TODO: avoid default construction of value member
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T, typename Policy>
inline
Checked_Number<T, Policy>::Checked_Number(const Plus_Infinity& x) {
  // TODO: avoid default construction of value member
  Rounding_Dir dir = Policy::ROUND_DEFAULT_CONSTRUCTOR_INF;
  Policy::handle_result(check_result(Checked::assign<Policy>(v, x, rounding_dir(dir)), dir));
}

template <typename T>
inline bool
is_minus_infinity(const T& x) {
  return Checked::is_minf<typename Native_Checked_From_Wrapper<T>::Policy>(Native_Checked_From_Wrapper<T>::raw_value(x));
}

template <typename T>
inline bool
is_plus_infinity(const T& x) {
  return Checked::is_pinf<typename Native_Checked_From_Wrapper<T>::Policy>(Native_Checked_From_Wrapper<T>::raw_value(x));
}

template <typename T>
inline bool
is_not_a_number(const T& x) {
  return Checked::is_nan<typename Native_Checked_From_Wrapper<T>::Policy>(Native_Checked_From_Wrapper<T>::raw_value(x));
}

template <typename T>
inline bool
is_integer(const T& x) {
  return Checked::is_int<typename Native_Checked_From_Wrapper<T>::Policy>(Native_Checked_From_Wrapper<T>::raw_value(x));
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

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline memory_size_type
total_memory_in_bytes(const Checked_Number<T, Policy>& x) {
  return Checked::total_memory_in_bytes(x.raw_value());
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline memory_size_type
external_memory_in_bytes(const Checked_Number<T, Policy>& x) {
  return Checked::external_memory_in_bytes(x.raw_value());
}

template <typename To>
inline Result
assign_r(To& to, const Minus_Infinity& x, Rounding_Dir dir) {
  return check_result(Checked::assign<typename Native_Checked_To_Wrapper<To>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), x, rounding_dir(dir)), dir);
}
template <typename To>
inline Result
assign_r(To& to, const Plus_Infinity& x, Rounding_Dir dir) {
  return check_result(Checked::assign<typename Native_Checked_To_Wrapper<To>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), x, rounding_dir(dir)), dir);
}
template <typename To>
inline Result
assign_r(To& to, const Not_A_Number& x, Rounding_Dir dir) {
  return check_result(Checked::assign<typename Native_Checked_To_Wrapper<To>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), x, rounding_dir(dir)), dir);
}

template <typename To>
inline Result
assign_r(To& to, const char* x, Rounding_Dir dir) {
  std::istringstream s(x);
  return check_result(Checked::input<typename Native_Checked_To_Wrapper<To>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), s, rounding_dir(dir)), dir);
}

#define FUNC1(name, func) \
template <typename To, typename From> \
inline Result \
name(To& to, const From& x, Rounding_Dir dir) { \
  return check_result(Checked::func<typename Native_Checked_To_Wrapper<To>::Policy, typename Native_Checked_From_Wrapper<From>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), Native_Checked_From_Wrapper<From>::raw_value(x), rounding_dir(dir)), dir); \
}

FUNC1(construct, construct_ext)
FUNC1(assign_r, assign_ext)
FUNC1(neg_assign_r, neg_ext)
FUNC1(abs_assign_r, abs_ext)
FUNC1(sqrt_assign_r, sqrt_ext)

#undef FUNC1

#define FUNC1(name, func) \
template <typename To, typename From> \
inline Result \
name(To& to, const From& x, int exp, Rounding_Dir dir) { \
  return check_result(Checked::func<typename Native_Checked_To_Wrapper<To>::Policy, typename Native_Checked_From_Wrapper<From>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), Native_Checked_From_Wrapper<From>::raw_value(x), exp, rounding_dir(dir)), dir); \
}

FUNC1(mul2exp_assign_r, mul2exp_ext)
FUNC1(div2exp_assign_r, div2exp_ext)

#undef FUNC1

#define FUNC2(name, func) \
template <typename To, \
          typename From1, \
	  typename From2> \
inline Result \
name(To& to, const From1& x, const From2& y, Rounding_Dir dir) { \
  return check_result(Checked::func<typename Native_Checked_To_Wrapper<To>::Policy, typename Native_Checked_From_Wrapper<From1>::Policy, typename Native_Checked_From_Wrapper<From2>::Policy>(Native_Checked_To_Wrapper<To>::raw_value(to), Native_Checked_From_Wrapper<From1>::raw_value(x), Native_Checked_From_Wrapper<From2>::raw_value(y), rounding_dir(dir)), dir); \
}

FUNC2(add_assign_r, add_ext)
FUNC2(sub_assign_r, sub_ext)
FUNC2(mul_assign_r, mul_ext)
FUNC2(div_assign_r, div_ext)
FUNC2(rem_assign_r, rem_ext)
FUNC2(gcd_assign_r, gcd_ext)
FUNC2(lcm_assign_r, lcm_ext)
FUNC2(add_mul_assign_r, add_mul_ext)
FUNC2(sub_mul_assign_r, sub_mul_ext)

#undef FUNC2

#define DEF_INCREMENT(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f() { \
  Policy::handle_result(fun(*this, *this, T(1), Policy::ROUND_DEFAULT_OPERATOR)); \
  return *this; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
Checked_Number<T, Policy>::f(int) {\
  T r = v;\
  Policy::handle_result(fun(*this, *this, T(1), Policy::ROUND_DEFAULT_OPERATOR));\
  return r;\
}

DEF_INCREMENT(operator ++, add_assign_r)
DEF_INCREMENT(operator --, sub_assign_r)

#undef DEF_INCREMENT

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline void
swap(Checked_Number<T, Policy>& x, Checked_Number<T, Policy>& y) {
  std::swap(x.raw_value(), y.raw_value());
}

template <typename T, typename Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Checked_Number<T, Policy>& y) {
  Checked::copy<Policy>(v, y.raw_value());
  return *this;
}
template <typename T, typename Policy>
template <typename From, typename From_Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Checked_Number<From, From_Policy>& y) {
  Policy::handle_result(assign_r(*this, y, Policy::ROUND_DEFAULT_OPERATOR));
  return *this;
}
template <typename T, typename Policy>
template <typename From>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const From& y) {
  Policy::handle_result(assign_r(*this, y, Policy::ROUND_DEFAULT_OPERATOR));
  return *this;
}
template <typename T, typename Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Not_A_Number& y) {
  Policy::handle_result(assign_r(*this, y, Policy::ROUND_IGNORE));
  return *this;
}
template <typename T, typename Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Minus_Infinity& y) {
  Policy::handle_result(assign_r(*this, y, Policy::ROUND_DEFAULT_ASSIGN_INF));
  return *this;
}
template <typename T, typename Policy>
inline Checked_Number<T, Policy>&
Checked_Number<T, Policy>::operator=(const Plus_Infinity& y) {
  Policy::handle_result(assign_r(*this, y, Policy::ROUND_DEFAULT_ASSIGN_INF));
  return *this;
}

#define DEF_BINARY_OP_ASSIGN(f, fun) \
template <typename T, typename Policy> \
template <typename From_Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<T, From_Policy>& y) { \
  Policy::handle_result(fun(*this, *this, y, Policy::ROUND_DEFAULT_OPERATOR)); \
  return *this; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const T& y) { \
  Policy::handle_result(fun(*this, *this, y, Policy::ROUND_DEFAULT_OPERATOR)); \
  return *this; \
} \
template <typename T, typename Policy> \
template <typename From, typename From_Policy> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const Checked_Number<From, From_Policy>& y) { \
  Checked_Number<T, Policy> cy(y); \
  Policy::handle_result(fun(*this, *this, cy, Policy::ROUND_DEFAULT_OPERATOR)); \
  return *this; \
} \
template <typename T, typename Policy> \
template <typename From> \
inline Checked_Number<T, Policy>& \
Checked_Number<T, Policy>::f(const From& y) { \
  Checked_Number<T, Policy> cy(y); \
  Policy::handle_result(fun(*this, *this, cy, Policy::ROUND_DEFAULT_OPERATOR)); \
  return *this; \
}

DEF_BINARY_OP_ASSIGN(operator +=, add_assign_r)
DEF_BINARY_OP_ASSIGN(operator -=, sub_assign_r)
DEF_BINARY_OP_ASSIGN(operator *=, mul_assign_r)
DEF_BINARY_OP_ASSIGN(operator /=, div_assign_r)
DEF_BINARY_OP_ASSIGN(operator %=, rem_assign_r)

#undef DEF_BINARY_OP_ASSIGN

#define DEF_BINARY_OP_TYPE(f, fun, Type) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Type x, const Checked_Number<T, Policy>& y) { \
  Checked_Number<T, Policy> r(x); \
  Policy::handle_result(fun(r, r, y, Policy::ROUND_DEFAULT_OPERATOR)); \
  return r; \
} \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const Type y) { \
  Checked_Number<T, Policy> r(y); \
  Policy::handle_result(fun(r, x, r, Policy::ROUND_DEFAULT_OPERATOR)); \
  return r; \
}

#define DEF_BINARY_OP(f, fun) \
template <typename T, typename Policy> \
inline Checked_Number<T, Policy> \
f(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  Checked_Number<T, Policy> r; \
  Policy::handle_result(fun(r, x, y, Policy::ROUND_DEFAULT_OPERATOR)); \
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

DEF_BINARY_OP(operator +, add_assign_r)
DEF_BINARY_OP(operator -, sub_assign_r)
DEF_BINARY_OP(operator *, mul_assign_r)
DEF_BINARY_OP(operator /, div_assign_r)
DEF_BINARY_OP(operator %, rem_assign_r)

#undef DEF_BINARY_OP_TYPE
#undef DEF_BINARY_OP

#define DEF_COMPARE_TYPE(f, fun, Type) \
template <typename From, typename From_Policy> \
inline bool \
f(const Type x, const Checked_Number<From, From_Policy>& y) { \
  return Checked::fun<Default_From_Policy, From_Policy>(x, y.raw_value()); \
} \
template <typename From, typename From_Policy> \
inline bool \
f(const Checked_Number<From, From_Policy>& x, const Type y) { \
  return Checked::fun<From_Policy, Default_From_Policy>(x.raw_value(), y); \
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
  Policy::handle_result(neg_assign_r(r, x, Policy::ROUND_DEFAULT_OPERATOR));
  return r;
}

#define DEF_ASSIGN_FUN2_1(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x) { \
  Policy::handle_result(fun(x, x, Policy::ROUND_DEFAULT_FUNCTION));	\
}

#define DEF_ASSIGN_FUN2_2(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y) { \
  Policy::handle_result(fun(x, y, Policy::ROUND_DEFAULT_FUNCTION)); \
}

#define DEF_ASSIGN_FUN3_3(f, fun) \
template <typename T, typename Policy> \
inline void \
f(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y, const Checked_Number<T, Policy>& z) { \
  Policy::handle_result(fun(x, y, z, Policy::ROUND_DEFAULT_FUNCTION)); \
}

DEF_ASSIGN_FUN2_2(sqrt_assign, sqrt_assign_r)

DEF_ASSIGN_FUN2_1(neg_assign, neg_assign_r)
DEF_ASSIGN_FUN2_2(neg_assign, neg_assign_r)

DEF_ASSIGN_FUN3_3(exact_div_assign, div_assign_r)

DEF_ASSIGN_FUN3_3(add_mul_assign, add_mul_assign_r)

DEF_ASSIGN_FUN3_3(sub_mul_assign, sub_mul_assign_r)

DEF_ASSIGN_FUN3_3(gcd_assign, gcd_assign_r)

DEF_ASSIGN_FUN3_3(lcm_assign, lcm_assign_r)

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
inline Result
output(std::ostream& os, const Checked_Number<T, Policy>& x, const Numeric_Format& fmt, Rounding_Dir dir) {
  return check_result(Checked::output_ext<Policy>(os, x.raw_value(), fmt, rounding_dir(dir)), dir);
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x) {
  Policy::handle_result(output(os, x, Numeric_Format(), ROUND_IGNORE));
  return os;
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline Result
input(Checked_Number<T, Policy>& x, std::istream& is, Rounding_Dir dir) {
  return check_result(Checked::input_ext<Policy>(x.raw_value(), is, rounding_dir(dir)), dir);
}

/*! \relates Checked_Number */
template <typename T, typename Policy>
inline std::istream& operator>>(std::istream& is, Checked_Number<T, Policy>& x) {
  Result r = input(x, is, Policy::ROUND_DEFAULT_INPUT);
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
