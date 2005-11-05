/* Specialized "checked" functions for native floating-point numbers.
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

#ifndef PPL_checked_float_inlines_hh
#define PPL_checked_float_inlines_hh 1

#include "Float.defs.hh"
#include <cmath>

namespace std {

inline double
fma(double x, double y, double z) {
#if HAVE_DECL_FMA
  return ::fma(x, y, z);
#else
  return x*y + z;
#endif
}

inline float
fma(float x, float y, float z) {
#if HAVE_DECL_FMAF
  return ::fmaf(x, y, z);
#else
  return x*y + z;
#endif
}

inline long double
fma(long double x, long double y, long double z) {
#if HAVE_DECL_FMAL
  return ::fmal(x, y, z);
#else
  return x*y + z;
#endif
}

inline float
rint(float x) {
  return ::rintf(x);
}

inline double
rint(double x) {
  return ::rint(x);
}

#if HAVE_DECL_RINTL
inline long double
rint(long double x) {
  return ::rintl(x);
}
#endif

}

namespace Parma_Polyhedra_Library {

namespace Checked {

inline bool
fpu_direct_rounding(Rounding_Dir dir) {
  return dir == ROUND_DIRECT || dir == ROUND_IGNORE;
}

inline bool
fpu_inverse_rounding(Rounding_Dir dir) {
  return dir == ROUND_INVERSE;
}

//
// The FPU mode is "round down".
//
// The result of the rounded down multiplication is thus computed directly.
//
//   a = 0.3
//   b = 0.1
//   c_i = a * b = 0.03
//   c = c_i = 0.0
//
// To obtain the result of the rounded up multiplication
// we do -(-a * b).
//
//   a = 0.3
//   b = 0.1
//   c_i = -a * b = -0.03
//
// Here c_i should be forced to lose excess precision, otherwise the
// FPU will truncate using the rounding mode in force, which is "round down".
//
//   c_i = -c_i = 0.03
//   c = c_i = 0.0
//
// Wrong result: we should have obtained c = 0.1.

inline float
limit_precision(float v) {
  volatile float x = v;
  return x;
}

inline double
limit_precision(double v) {
  volatile double x = v;
  return x;
}

inline long double
limit_precision(long double v) {
#if __GNUC__ >= 4
  return v;
#else
  // Not really needed for floating point operations done with the
  // maximum available precision, but this avoids a bug in GCC 3.4.3
  // that causes excessive optimization compiling -(-a * b).
  // See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=21032
  // and http://gcc.gnu.org/bugzilla/show_bug.cgi?id=21067.
  volatile long double x = v;
  return x;
#endif
}

template <typename Policy, typename T>
inline Result
classify_float(const T v, bool nan, bool inf, bool sign) {
  typename Float<T>::Type f(v);
  if ((nan || sign) && f.is_nan())
    return VC_NAN;
  if (inf) {
    int i = f.is_inf();
    if (i < 0)
      return VC_MINUS_INFINITY;
    if (i > 0)
      return VC_PLUS_INFINITY;
  }
  if (sign) {
    if (v < 0)
      return V_LT;
    if (v > 0)
      return V_GT;
    return V_EQ;
  }
  return VC_NORMAL;
}

template <typename Policy, typename T>
inline bool
is_nan_float(const T v) {
  typename Float<T>::Type f(v);
  return f.is_nan();
}

template <typename Policy, typename T>
inline bool
is_minf_float(const T v) {
  typename Float<T>::Type f(v);
  return f.is_inf() < 0;
}

template <typename Policy, typename T>
inline bool
is_pinf_float(const T v) {
  typename Float<T>::Type f(v);
  return f.is_inf() > 0;
}

template <typename T>
inline bool
is_inf_float(const T v) {
  typename Float<T>::Type f(v);
  return f.is_inf() != 0;
}

template <typename Policy, typename T>
inline bool
is_int_float(const T v) {
  return rint(v) == v;
}

template <typename Policy, typename T>
inline Result
set_special_float(T& v, Result r) {
  switch (classify(r)) {
  case VC_MINUS_INFINITY:
    v = -HUGE_VAL;
    break;
  case VC_PLUS_INFINITY:
    v = HUGE_VAL;
    break;
  case VC_NAN:
    v = NAN;
    break;
  default:
    break;
  }
  return r;
}

template <typename T>
inline void
pred_float(T& v) {
  typename Float<T>::Type f(v);
  assert(!f.is_nan());
  assert(f.is_inf() >= 0);
  if (f.is_zero() > 0) {
    f.negate();
    f.inc();
  }
  else if (f.sign_bit()) {
    f.inc();
  }
  else {
    f.dec();
  }
  v = f.value();
}

template <typename T>
inline void
succ_float(T& v) {
  typename Float<T>::Type f(v);
  assert(!f.is_nan());
  assert(f.is_inf() <= 0);
  if (f.is_zero() < 0) {
    f.negate();
    f.inc();
  }
  else if (!f.sign_bit()) {
    f.inc();
  }
  else {
    f.dec();
  }
  v = f.value();
}

SPECIALIZE_CLASSIFY(float, float)
SPECIALIZE_IS_NAN(float, float)
SPECIALIZE_IS_MINF(float, float)
SPECIALIZE_IS_PINF(float, float)
SPECIALIZE_SET_SPECIAL(float, float)

SPECIALIZE_CLASSIFY(float, double)
SPECIALIZE_IS_NAN(float, double)
SPECIALIZE_IS_MINF(float, double)
SPECIALIZE_IS_PINF(float, double)
SPECIALIZE_SET_SPECIAL(float, double)

SPECIALIZE_CLASSIFY(float, long double)
SPECIALIZE_IS_NAN(float, long double)
SPECIALIZE_IS_MINF(float, long double)
SPECIALIZE_IS_PINF(float, long double)
SPECIALIZE_SET_SPECIAL(float, long double)

template <typename Policy, typename To>
inline Result
round_lt_float(To& to, Rounding_Dir dir) {
  if (dir == ROUND_DOWN) {
    pred_float(to);
    return V_GT;
  }
  return V_LT;
}

template <typename Policy, typename To>
inline Result
round_gt_float(To& to, Rounding_Dir dir) {
  if (dir == ROUND_UP) {
    succ_float(to);
    return V_LT;
  }
  return V_GT;
}

template <typename Policy>
inline void
prepare_inexact(Rounding_Dir dir) {
  if (Policy::fpu_check_inexact && dir != ROUND_IGNORE)
    fpu_reset_inexact();
}

template <typename Policy>
inline Result 
result_relation(Rounding_Dir dir) {
  if (Policy::fpu_check_inexact) {
    if (!fpu_check_inexact())
      return V_EQ;
    switch (dir) {
    case ROUND_DOWN:
      return V_GT;
    case ROUND_UP:
      return V_LT;
    default:
      return V_NE;
    }
  }
  else {
    switch (dir) {
    case ROUND_DOWN:
      return V_GE;
    case ROUND_UP:
      return V_LE;
    default:
      return V_LGE;
    }
  }
}

template <typename Policy, typename From, typename To>
inline Result 
assign_float_float_exact(To& to, const From from, Rounding_Dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(from)))
    return VC_NAN;
  to = from;
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_float_float_inexact(To& to, const From from, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(from)))
    return VC_NAN;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = from;
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(-from);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = from;
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename From, typename To>
inline Result
assign_float_float(To& to, const From from, Rounding_Dir dir) {
  if (sizeof(From) > sizeof(To))
    return assign_float_float_inexact<Policy>(to, from, dir);
  else
    return assign_float_float_exact<Policy>(to, from, dir);
}

template <typename Policy, typename Type>
inline Result 
neg_float(Type& to, const Type from, Rounding_Dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(from)))
    return VC_NAN;
  to = -from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
add_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  if (CHECK_P(Policy::check_inf_add_inf, is_inf_float(x) && x == -y))
    return V_INF_ADD_INF;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = x + y;
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(-x - y);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = x + y;
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result 
sub_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  if (CHECK_P(Policy::check_inf_sub_inf, is_inf_float(x) && x == y))
    return V_INF_SUB_INF;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = x - y;
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(y - x);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = x - y;
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result 
mul_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  if (CHECK_P(Policy::check_inf_mul_zero, (x == 0 && is_inf_float(y)) ||
	    (y == 0 && is_inf_float(x))))
      return V_INF_MUL_ZERO;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = x * y;
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(x * -y);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = x * y;
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result 
div_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  if (CHECK_P(Policy::check_inf_div_inf, is_inf_float(x) && is_inf_float(y)))
      return V_INF_DIV_INF;
  if (CHECK_P(Policy::check_div_zero, y == 0)) {
    to = NAN;
    return V_DIV_ZERO;
  }
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = x / y;
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(x / -y);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = x / y;
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result 
rem_float(Type& to, const Type x, const Type y, Rounding_Dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  if (CHECK_P(Policy::check_inf_mod, is_inf_float(x)))
    return V_INF_MOD;
  if (CHECK_P(Policy::check_div_zero, y == 0)) {
    to = NAN;
    return V_MOD_ZERO;
  }
  to = std::fmod(x, y);
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
mul2exp_float(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return div2exp<Policy>(to, x, -exp, dir);
  assert(static_cast<unsigned int>(exp) < sizeof(unsigned long long) * 8);
  return mul<Policy>(to, x, static_cast<Type>(1ULL << exp), dir);
}

template <typename Policy, typename Type>
inline Result 
div2exp_float(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return mul2exp<Policy>(to, x, -exp, dir);
  assert(static_cast<unsigned int>(exp) < sizeof(unsigned long long) * 8);
  return div<Policy>(to, x, static_cast<Type>(1ULL << exp), dir);
}

template <typename Policy, typename Type>
inline Result
abs_float(Type& to, const Type from, Rounding_Dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(from)))
    return VC_NAN;
  to = from < 0 ? -from : from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sqrt_float(Type& to, const Type from, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(from)))
    return VC_NAN;
  if (CHECK_P(Policy::check_sqrt_neg, from < 0)) {
    to = NAN;
    return V_SQRT_NEG;
  }
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = std::sqrt(from);
  else {
    int old = fpu_save_rounding_direction(dir);
    to = std::sqrt(from);
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result
sgn_float(const Type x) {
  return classify<Policy>(x, false, false, true);
}

template <typename Policy, typename Type>
inline Result
cmp_float(const Type x, const Type y) {
  if (x > y)
    return V_GT;
  if (x < y)
    return V_LT;
  if (x == y)
    return V_EQ;
  return V_UNORD_COMP;
}

template <typename Policy, typename To, typename From>
inline Result 
assign_float_int_exact(To& to, const From from, Rounding_Dir) {
  to = from;
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_float_int_inexact(To& to, const From from, Rounding_Dir dir) {
  long double v = from;
  return assign_float_float<Policy>(to, v, dir);
}

template <typename Policy, typename To, typename From>
inline Result 
assign_float_int(To& to, const From from, Rounding_Dir dir) {
  if (sizeof(From) * 8 > Float<To>::Type::MANTISSA_BITS)
    return assign_float_int_inexact<Policy>(to, from, dir);
  else
    return assign_float_int_exact<Policy>(to, from, dir);
}

template <typename Policy, typename T>
inline Result
set_neg_overflow_float(T& to, Rounding_Dir dir) {
  switch (dir) {
  case ROUND_UP:
    {
      to = Float<T>::Type::NEG_MAX;
      return V_LT;
    }
  default:
    to = -HUGE_VAL;
    return V_GT;
  }
}

template <typename Policy, typename T>
inline Result
set_pos_overflow_float(T& to, Rounding_Dir dir) {
  switch (dir) {
  case ROUND_DOWN:
    {
      to = Float<T>::Type::POS_MAX;
      return V_GT;
    }
  default:
    to = HUGE_VAL;
    return V_LT;
  }
}

template <typename Policy, typename T>
inline Result
assign_float_mpz(T& to, const mpz_class& _from, Rounding_Dir dir)
{
  mpz_srcptr from = _from.get_mpz_t();
  int sign = mpz_sgn(from);
  if (sign == 0) {
    to = 0;
    return V_EQ;
  }
  size_t exponent = mpz_sizeinbase(from, 2) - 1;
  if (exponent > (size_t) Float<T>::Type::EXPONENT_MAX) {
    if (sign < 0)
      return set_neg_overflow_float<Policy>(to, dir);
    else
      return set_pos_overflow_float<Policy>(to, dir);
  }
  unsigned long zeroes = mpn_scan1(from->_mp_d, 0);
  size_t significative_bits = exponent - zeroes;
  mpz_t mantissa;
  mpz_init(mantissa);
  if (exponent > Float<T>::Type::MANTISSA_BITS)
    mpz_tdiv_q_2exp(mantissa, from, exponent - Float<T>::Type::MANTISSA_BITS);
  else
    mpz_mul_2exp(mantissa, from, Float<T>::Type::MANTISSA_BITS - exponent);
  typename Float<T>::Type f(to);
  f.build(sign < 0, mantissa, exponent);
  mpz_clear(mantissa);
  to = f.value();
  if (significative_bits > Float<T>::Type::MANTISSA_BITS) {
    if (sign < 0)
      return round_lt_float<Policy>(to, dir);
    else
      return round_gt_float<Policy>(to, dir);
  }
  return V_EQ;
}

template <typename Policy, typename T>
inline Result
assign_float_mpq(T& to, const mpq_class& from, Rounding_Dir dir)
{
  const mpz_class& _num = from.get_num();
  const mpz_class& _den = from.get_den();
  if (_den == 1)
    return assign_float_mpz<Policy>(to, _num, dir);
  mpz_srcptr num = _num.get_mpz_t();
  mpz_srcptr den = _den.get_mpz_t();
  int sign = mpz_sgn(num);
  signed long exponent = mpz_sizeinbase(num, 2) - mpz_sizeinbase(den, 2);
  if (exponent < Float<T>::Type::EXPONENT_MIN_DENORM) {
    to = 0;
  inexact:
    if (sign < 0)
      return round_lt_float<Policy>(to, dir);
    else
      return round_gt_float<Policy>(to, dir);
  }
  if (exponent > (signed int) Float<T>::Type::EXPONENT_MAX + 1) {
  overflow:
    if (sign < 0)
      return set_neg_overflow_float<Policy>(to, dir);
    else
      return set_pos_overflow_float<Policy>(to, dir);
  }
  unsigned int needed_bits = Float<T>::Type::MANTISSA_BITS + 1;
  if (exponent < Float<T>::Type::EXPONENT_MIN)
    needed_bits -= Float<T>::Type::EXPONENT_MIN - exponent;
  mpz_t mantissa;
  mpz_init(mantissa);
  signed long shift = needed_bits - exponent;
  if (shift > 0) {
    mpz_mul_2exp(mantissa, num, shift);
    num = mantissa;
  }
  else if (shift < 0) {
    mpz_mul_2exp(mantissa, den, -shift);
    den = mantissa;
  }
  mpz_t r;
  mpz_init(r);
  mpz_tdiv_qr(mantissa, r, num, den);
  size_t bits = mpz_sizeinbase(mantissa, 2);
  bool inexact = (mpz_sgn(r) != 0);
  mpz_clear(r);
  if (bits == needed_bits + 1) {
    inexact = (inexact || mpz_odd_p(mantissa));
    mpz_div_2exp(mantissa, mantissa, 1);
  }
  else
    --exponent;
  if (exponent > (signed int)Float<T>::Type::EXPONENT_MAX) {
    mpz_clear(mantissa);
    goto overflow;
  } else if (exponent < Float<T>::Type::EXPONENT_MIN - 1) {
    /* Denormalized */
    exponent = Float<T>::Type::EXPONENT_MIN - 1;
  }
  typename Float<T>::Type f(to);
  f.build(sign < 0, mantissa, exponent);
  mpz_clear(mantissa);
  to = f.value();
  if (inexact)
    goto inexact;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
add_mul_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(to)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = std::fma(to, x, y);
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(std::fma(-to, -x, y));
  else {
    int old = fpu_save_rounding_direction(dir);
    to = std::fma(to, x, y);
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result 
sub_mul_float(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(to)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)) || CHECK_P(Policy::check_nan_args, is_nan<Policy>(y)))
    return VC_NAN;
  prepare_inexact<Policy>(dir);
  if (fpu_direct_rounding(dir))
    to = std::fma(to, x, -y);
  else if (fpu_inverse_rounding(dir))
    to = -limit_precision(std::fma(-to, -x, -y));
  else {
    int old = fpu_save_rounding_direction(dir);
    to = std::fma(to, x, -y);
    fpu_restore_rounding_direction(old);
  }
  return result_relation<Policy>(dir);
}

template <typename Policy, typename Type>
inline Result
output_float(std::ostream& os, Type& from, const Numeric_Format&, Rounding_Dir) {
  if (from == 0)
    os << "0";
  else if (is_minf<Policy>(from))
    os << "-inf";
  else if (is_pinf<Policy>(from))
    os << "+inf";
  else if (is_nan<Policy>(from))
    os << "nan";
  else {
    int old_precision = os.precision(10000);
    os << from;
    os.precision(old_precision);
  }
  return V_EQ;
}

template <typename Policy, typename To>
inline Result
assign_float_minf(To& to, const Minus_Infinity&, Rounding_Dir) {
  to = -HUGE_VAL;
  return V_EQ;
}

template <typename Policy, typename To>
inline Result
assign_float_pinf(To& to, const Plus_Infinity&, Rounding_Dir) {
  to = HUGE_VAL;
  return V_EQ;
}

template <typename Policy, typename To>
inline Result
assign_float_nan(To& to, const Not_A_Number&, Rounding_Dir) {
  to = NAN;
  return V_EQ;
}

SPECIALIZE_ASSIGN(float_float_exact, float, float)
SPECIALIZE_ASSIGN(float_float, float, double)
SPECIALIZE_ASSIGN(float_float, float, long double)
SPECIALIZE_ASSIGN(float_float_exact, double, float)
SPECIALIZE_ASSIGN(float_float_exact, double, double)
SPECIALIZE_ASSIGN(float_float, double, long double)
SPECIALIZE_ASSIGN(float_float_exact, long double, float)
SPECIALIZE_ASSIGN(float_float_exact, long double, double)
SPECIALIZE_ASSIGN(float_float_exact, long double, long double)

SPECIALIZE_ASSIGN(float_int, float, signed char)
SPECIALIZE_ASSIGN(float_int, float, signed short)
SPECIALIZE_ASSIGN(float_int, float, signed int)
SPECIALIZE_ASSIGN(float_int, float, signed long)
SPECIALIZE_ASSIGN(float_int, float, signed long long)
SPECIALIZE_ASSIGN(float_int, float, unsigned char)
SPECIALIZE_ASSIGN(float_int, float, unsigned short)
SPECIALIZE_ASSIGN(float_int, float, unsigned int)
SPECIALIZE_ASSIGN(float_int, float, unsigned long)
SPECIALIZE_ASSIGN(float_int, float, unsigned long long)

SPECIALIZE_ASSIGN(float_int, double, signed char)
SPECIALIZE_ASSIGN(float_int, double, signed short)
SPECIALIZE_ASSIGN(float_int, double, signed int)
SPECIALIZE_ASSIGN(float_int, double, signed long)
SPECIALIZE_ASSIGN(float_int, double, signed long long)
SPECIALIZE_ASSIGN(float_int, double, unsigned char)
SPECIALIZE_ASSIGN(float_int, double, unsigned short)
SPECIALIZE_ASSIGN(float_int, double, unsigned int)
SPECIALIZE_ASSIGN(float_int, double, unsigned long)
SPECIALIZE_ASSIGN(float_int, double, unsigned long long)

SPECIALIZE_ASSIGN(float_int, long double, signed char)
SPECIALIZE_ASSIGN(float_int, long double, signed short)
SPECIALIZE_ASSIGN(float_int, long double, signed int)
SPECIALIZE_ASSIGN(float_int, long double, signed long)
SPECIALIZE_ASSIGN(float_int, long double, signed long long)
SPECIALIZE_ASSIGN(float_int, long double, unsigned char)
SPECIALIZE_ASSIGN(float_int, long double, unsigned short)
SPECIALIZE_ASSIGN(float_int, long double, unsigned int)
SPECIALIZE_ASSIGN(float_int, long double, unsigned long)
SPECIALIZE_ASSIGN(float_int, long double, unsigned long long)

SPECIALIZE_COPY(generic, float)
SPECIALIZE_IS_INT(float, float)
SPECIALIZE_ASSIGN(float_minf, float, Minus_Infinity)
SPECIALIZE_ASSIGN(float_pinf, float, Plus_Infinity)
SPECIALIZE_ASSIGN(float_nan, float, Not_A_Number)
SPECIALIZE_NEG(float, float, float)
SPECIALIZE_ABS(float, float, float)
SPECIALIZE_ADD(float, float, float, float)
SPECIALIZE_SUB(float, float, float, float)
SPECIALIZE_MUL(float, float, float, float)
SPECIALIZE_DIV(float, float, float, float)
SPECIALIZE_REM(float, float, float, float)
SPECIALIZE_MUL2EXP(float, float, float)
SPECIALIZE_DIV2EXP(float, float, float)
SPECIALIZE_SQRT(float, float, float)
SPECIALIZE_GCD(exact, float, float, float)
SPECIALIZE_LCM(gcd_exact, float, float, float)
SPECIALIZE_SGN(float, float)
SPECIALIZE_CMP(float, float, float)
SPECIALIZE_ADD_MUL(float, float, float, float)
SPECIALIZE_SUB_MUL(float, float, float, float)
SPECIALIZE_INPUT(generic, float)
SPECIALIZE_OUTPUT(float, float)

SPECIALIZE_COPY(generic, double)
SPECIALIZE_IS_INT(float, double)
SPECIALIZE_ASSIGN(float_minf, double, Minus_Infinity)
SPECIALIZE_ASSIGN(float_pinf, double, Plus_Infinity)
SPECIALIZE_ASSIGN(float_nan, double, Not_A_Number)
SPECIALIZE_NEG(float, double, double)
SPECIALIZE_ABS(float, double, double)
SPECIALIZE_ADD(float, double, double, double)
SPECIALIZE_SUB(float, double, double, double)
SPECIALIZE_MUL(float, double, double, double)
SPECIALIZE_DIV(float, double, double, double)
SPECIALIZE_REM(float, double, double, double)
SPECIALIZE_MUL2EXP(float, double, double)
SPECIALIZE_DIV2EXP(float, double, double)
SPECIALIZE_SQRT(float, double, double)
SPECIALIZE_GCD(exact, double, double, double)
SPECIALIZE_LCM(gcd_exact, double, double, double)
SPECIALIZE_SGN(float, double)
SPECIALIZE_CMP(float, double, double)
SPECIALIZE_ADD_MUL(float, double, double, double)
SPECIALIZE_SUB_MUL(float, double, double, double)
SPECIALIZE_INPUT(generic, double)
SPECIALIZE_OUTPUT(float, double)

SPECIALIZE_COPY(generic, long double)
SPECIALIZE_IS_INT(float, long double)
SPECIALIZE_ASSIGN(float_minf, long double, Minus_Infinity)
SPECIALIZE_ASSIGN(float_pinf, long double, Plus_Infinity)
SPECIALIZE_ASSIGN(float_nan, long double, Not_A_Number)
SPECIALIZE_NEG(float, long double, long double)
SPECIALIZE_ABS(float, long double, long double)
SPECIALIZE_ADD(float, long double, long double, long double)
SPECIALIZE_SUB(float, long double, long double, long double)
SPECIALIZE_MUL(float, long double, long double, long double)
SPECIALIZE_DIV(float, long double, long double, long double)
SPECIALIZE_REM(float, long double, long double, long double)
SPECIALIZE_MUL2EXP(float, long double, long double)
SPECIALIZE_DIV2EXP(float, long double, long double)
SPECIALIZE_SQRT(float, long double, long double)
SPECIALIZE_GCD(exact, long double, long double, long double)
SPECIALIZE_LCM(gcd_exact, long double, long double, long double)
SPECIALIZE_SGN(float, long double)
SPECIALIZE_CMP(float, long double, long double)
SPECIALIZE_ADD_MUL(float, long double, long double, long double)
SPECIALIZE_SUB_MUL(float, long double, long double, long double)
SPECIALIZE_INPUT(generic, long double)
SPECIALIZE_OUTPUT(float, long double)

SPECIALIZE_ASSIGN(float_mpz, float, mpz_class)
SPECIALIZE_ASSIGN(float_mpz, double, mpz_class)
SPECIALIZE_ASSIGN(float_mpz, long double, mpz_class)
SPECIALIZE_ASSIGN(float_mpq, float, mpq_class)
SPECIALIZE_ASSIGN(float_mpq, double, mpq_class)
SPECIALIZE_ASSIGN(float_mpq, long double, mpq_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
