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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_checked_float_inlines_hh
#define PPL_checked_float_inlines_hh 1

#include "Float.defs.hh"

namespace std {

inline double
fma(double x, double y, double z) {
  return ::fma(x, y, z);
}

inline float
fma(float x, float y, float z) {
  return ::fmaf(x, y, z);
}

inline long double
fma(long double x, long double y, long double z) {
  return ::fmal(x, y, z);
}

}

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename T>
inline Result
classify_float(const T v, bool nan, bool inf, bool sign) {
  Float<T> f(v);
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

template <typename Policy, typename T>
inline Result
pred_float(T& v) {
  Float<T> f(v);
  assert(!f.is_nan());
  assert(f.is_inf() >= 0);
  if (f.is_zero() > 0) {
    f.negate();
    f.inc();
  }
  else if (f.sign_bit()) {
    f.inc();
    if (Policy::fpu_classify && f.is_inf())
      return VC_MINUS_INFINITY;
  }
  else {
    f.dec();
  }
  v = f.value();
  return VC_NORMAL;
}

template <typename Policy, typename T>
inline Result
succ_float(T& v) {
  Float<T> f(v);
  assert(!f.is_nan());
  assert(f.is_inf() <= 0);
  if (f.is_zero() < 0) {
    f.negate();
    f.inc();
  }
  else if (!f.sign_bit()) {
    f.inc();
    if (Policy::fpu_classify && f.is_inf())
      return VC_PLUS_INFINITY;
  }
  else {
    f.dec();
  }
  v = f.value();
  return VC_NORMAL;
}

SPECIALIZE_CLASSIFY(float, float32_t)
SPECIALIZE_SET_SPECIAL(float, float32_t)
SPECIALIZE_PRED(float, float32_t)
SPECIALIZE_SUCC(float, float32_t)

SPECIALIZE_CLASSIFY(float, float64_t)
SPECIALIZE_SET_SPECIAL(float, float64_t)
SPECIALIZE_PRED(float, float64_t)
SPECIALIZE_SUCC(float, float64_t)

#ifdef FLOAT96_TYPE
SPECIALIZE_CLASSIFY(float, float96_t)
SPECIALIZE_SET_SPECIAL(float, float96_t)
SPECIALIZE_PRED(float, float96_t)
SPECIALIZE_SUCC(float, float96_t)
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_CLASSIFY(float, float128_t)
SPECIALIZE_SET_SPECIAL(float, float128_t)
SPECIALIZE_PRED(float, float128_t)
SPECIALIZE_SUCC(float, float128_t)
#endif

template <typename Policy>
inline void
prepare_inexact() {
  if (Policy::fpu_check_inexact)
    fpu_reset_inexact();
}

template <typename Policy>
inline Result 
result_relation(const Rounding& mode) {
  Result r;
  if (Policy::fpu_check_inexact) {
    if (!fpu_check_inexact())
      return V_EQ;
    r = VC_NORMAL;
  } else
    r = V_EQ;
  switch (mode.direction()) {
  case Rounding::DOWN:
    return static_cast<Result>(r | V_GT);
  case Rounding::UP:
    return static_cast<Result>(r | V_LT);
  default:
    return static_cast<Result>(r | V_NE);
  }
}

template <typename Policy, typename Type>
inline Result 
classify_float_(const Type x) {
  if (Policy::fpu_classify)
    return classify<Policy>(x, true, true, false);
  return VC_NORMAL;
}

template <typename Policy, typename From, typename To>
inline Result 
assign_float_float_exact(To& to, const From from, const Rounding&) {
  to = from;
  return static_cast<Result>(classify_float_<Policy>(to) | V_EQ);
}

template <typename Policy, typename To, typename From>
inline Result
assign_float_float(To& to, const From from, const Rounding& mode) {
  prepare_inexact<Policy>();
  to = from;
  return static_cast<Result>(classify_float_<Policy>(to) | 
			     result_relation<Policy>(mode));
}

template <typename Policy, typename Type>
inline Result 
assign_result_inexact(Type& to, const Type from, const Rounding& mode) {
  to = from;
  return static_cast<Result>(classify_float_<Policy>(to) | 
			     result_relation<Policy>(mode));
}

template <typename Policy, typename Type>
inline Result 
neg_float(Type& to, const Type from, const Rounding& mode) {
  return assign_float_float_exact<Policy>(to, -from, mode);
}

template <typename Policy, typename Type>
inline Result 
add_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x + y, mode);
}

template <typename Policy, typename Type>
inline Result 
sub_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x - y, mode);
}

template <typename Policy, typename Type>
inline Result 
mul_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x * y, mode);
}

template <typename Policy, typename Type>
inline Result 
div_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_divbyzero && y == 0) {
    to = NAN;
    return V_DIV_ZERO;
  }
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x / y, mode);
}

template <typename Policy, typename Type>
inline Result 
rem_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_divbyzero && y == 0) {
    to = NAN;
    return V_MOD_ZERO;
  }
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::fmod(x, y, mode));
}

template <typename Policy, typename Type>
inline Result
abs_float(Type& to, const Type from, const Rounding& mode) {
  return assign_float_float_exact<Policy>(to, std::abs(from), mode);
}

template <typename Policy, typename Type>
inline Result
sqrt_float(Type& to, const Type from, const Rounding& mode) {
  if (Policy::check_sqrt_neg && from < 0) {
    to = NAN;
    return V_SQRT_NEG;
  }
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::sqrt(from), mode);
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
assign_float_int_exact(To& to, const From from, const Rounding&) {
  to = from;
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_float_int(To& to, const From from, const Rounding& mode) {
  prepare_inexact<Policy>();
  to = from;
  return result_relation<Policy>(mode);
}


template <typename Policy, typename Type>
inline Result 
add_mul_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::fma(to, x, y), mode);
}

template <typename Policy, typename Type>
inline Result 
sub_mul_float(Type& to, const Type x, const Type y, const Rounding& mode) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::fma(to, x, -y), mode);
}

#define ASSIGN_R2(Smaller, Larger) \
SPECIALIZE_ASSIGN(float_float_exact, Larger, Smaller) \
SPECIALIZE_ASSIGN(float_float, Smaller, Larger)

SPECIALIZE_ASSIGN(float_int_exact, float32_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, uint8_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, uint16_t)
SPECIALIZE_ASSIGN(float_int, float32_t, int32_t)
SPECIALIZE_ASSIGN(float_int, float32_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float32_t, uint32_t)
SPECIALIZE_ASSIGN(float_int, float32_t, uint64_t)
SPECIALIZE_ASSIGN(float_float, float32_t, float32_t)
ASSIGN_R2(float32_t, float64_t)

SPECIALIZE_NEG(float, float32_t, float32_t)
SPECIALIZE_ABS(float, float32_t, float32_t)
SPECIALIZE_ADD(float, float32_t, float32_t)
SPECIALIZE_SUB(float, float32_t, float32_t)
SPECIALIZE_MUL(float, float32_t, float32_t)
SPECIALIZE_DIV(float, float32_t, float32_t)
SPECIALIZE_REM(float, float32_t, float32_t)
SPECIALIZE_SQRT(float, float32_t, float32_t)
SPECIALIZE_GCD(generic, float32_t, float32_t)
SPECIALIZE_LCM(generic, float32_t, float32_t)
SPECIALIZE_SGN(float, float32_t)
SPECIALIZE_CMP(float, float32_t, float32_t)
SPECIALIZE_ADD_MUL(float, float32_t, float32_t)
SPECIALIZE_SUB_MUL(float, float32_t, float32_t)
SPECIALIZE_PRINT(generic, float32_t)
SPECIALIZE_INPUT(generic, float32_t)

SPECIALIZE_ASSIGN(float_int_exact, float64_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, uint8_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, uint16_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, uint32_t)
SPECIALIZE_ASSIGN(float_int, float64_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float64_t, uint64_t)
SPECIALIZE_ASSIGN(float_float, float64_t, float64_t)

SPECIALIZE_NEG(float, float64_t, float64_t)
SPECIALIZE_ABS(float, float64_t, float64_t)
SPECIALIZE_ADD(float, float64_t, float64_t)
SPECIALIZE_SUB(float, float64_t, float64_t)
SPECIALIZE_MUL(float, float64_t, float64_t)
SPECIALIZE_DIV(float, float64_t, float64_t)
SPECIALIZE_REM(float, float64_t, float64_t)
SPECIALIZE_SQRT(float, float64_t, float64_t)
SPECIALIZE_GCD(generic, float64_t, float64_t)
SPECIALIZE_LCM(generic, float64_t, float64_t)
SPECIALIZE_SGN(float, float64_t)
SPECIALIZE_CMP(float, float64_t, float64_t)
SPECIALIZE_ADD_MUL(float, float64_t, float64_t)
SPECIALIZE_SUB_MUL(float, float64_t, float64_t)
SPECIALIZE_PRINT(generic, float64_t)
SPECIALIZE_INPUT(generic, float64_t)

#ifdef FLOAT96_TYPE
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int64_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, uint8_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, uint16_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, uint32_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, uint64_t)
SPECIALIZE_ASSIGN(float_float, float96_t, float96_t)
ASSIGN_R2(float32_t, float96_t)
ASSIGN_R2(float64_t, float96_t)

SPECIALIZE_NEG(float, float96_t, float96_t)
SPECIALIZE_ABS(float, float96_t, float96_t)
SPECIALIZE_ADD(float, float96_t, float96_t)
SPECIALIZE_SUB(float, float96_t, float96_t)
SPECIALIZE_MUL(float, float96_t, float96_t)
SPECIALIZE_DIV(float, float96_t, float96_t)
SPECIALIZE_REM(float, float96_t, float96_t)
SPECIALIZE_SQRT(float, float96_t, float96_t)
SPECIALIZE_GCD(generic, float96_t, float96_t)
SPECIALIZE_LCM(generic, float96_t, float96_t)
SPECIALIZE_SGN(float, float96_t)
SPECIALIZE_CMP(float, float96_t, float96_t)
SPECIALIZE_ADD_MUL(float, float96_t, float96_t)
SPECIALIZE_SUB_MUL(float, float96_t, float96_t)
SPECIALIZE_PRINT(generic, float96_t)
SPECIALIZE_INPUT(generic, float96_t)
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int64_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, uint8_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, uint16_t)
SPECIALIZE_ASSIGN(float_int_exact, float127_t, uint32_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, uint64_t)
SPECIALIZE_ASSIGN(float_float, float128_t, float128_t)
ASSIGN_R2(float32_t, float128_t)
ASSIGN_R2(float64_t, float128_t)
#ifdef FLOAT96_TYPE
ASSIGN_R2(float96_t, float128_t)
#endif

SPECIALIZE_NEG(float, float128_t, float128_t)
SPECIALIZE_ABS(float, float128_t, float128_t)
SPECIALIZE_ADD(float, float128_t, float128_t)
SPECIALIZE_SUB(float, float128_t, float128_t)
SPECIALIZE_MUL(float, float128_t, float128_t)
SPECIALIZE_DIV(float, float128_t, float128_t)
SPECIALIZE_REM(float, float128_t, float128_t)
SPECIALIZE_SQRT(float, float128_t, float128_t)
SPECIALIZE_GCD(generic, float128_t, float128_t)
SPECIALIZE_LCM(generic, float128_t, float128_t)
SPECIALIZE_SGN(float, float128_t)
SPECIALIZE_CMP(float, float128_t, float128_t)
SPECIALIZE_ADD_MUL(float, float128_t, float128_t)
SPECIALIZE_SUB_MUL(float, float128_t, float128_t)
SPECIALIZE_PRINT(generic, float128_t)
SPECIALIZE_INPUT(generic, float128_t)
#endif

#undef ASSIGN_R2

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
