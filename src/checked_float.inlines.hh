/* Specialized checked functions for native floats
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

#ifndef PPL_checked_float_inlines_hh
#define PPL_checked_float_inlines_hh 1

#include <cassert>
#include <cmath>
#include <fenv.h>

// Please do not remove the space separating `#' from `include':
// this ensures that the directive will not be moved during the
// procedure that automatically creates the library's include file
// (see `Makefile.am' in the `src' directory).
# include <endian.h>

#include "float.types.hh"

#define USE_FPU_ROUNDING_MODE
#define USE_FPU_INEXACT

namespace Parma_Polyhedra_Library {

#ifdef USE_FPU_ROUNDING_MODE
static int
fpu_rounding_mode = fegetround();
#endif

inline void
fpu_set_rounding_mode(int mode) {
#ifdef USE_FPU_ROUNDING_MODE
  if (mode != fpu_rounding_mode) {
    fesetround(mode);
    fpu_rounding_mode = mode;
  }
#endif
}

inline int
fpu_get_rounding_mode() {
#ifdef USE_FPU_ROUNDING_MODE
  return fpu_rounding_mode;
#else
  return -1;
#endif
}

inline void
fpu_reset_inexact() {
#ifdef USE_FPU_INEXACT
  feclearexcept(FE_ALL_EXCEPT);
#endif
}

/* 0 no, 1 yes, -1 maybe */
inline int
fpu_is_inexact() {
#ifdef USE_FPU_INEXACT
  return fetestexcept(FE_INEXACT) != 0;
#else
  return -1;
#endif
}

namespace Checked {

template <typename T>
struct Float;

template<>
class Float<float32_t> {
private:
  union {
    float32_t _value;
    u_int32_t word;
  } u;
  static const u_int32_t SGN_MASK = 0x80000000;
  static const u_int32_t POS_INF = 0x7f800000;
  static const u_int32_t NEG_INF = 0xff800000;
  static const u_int32_t POS_ZERO = 0x00000000;
  static const u_int32_t NEG_ZERO = 0x80000000;
public:
  Float(float32_t v);
  float32_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
};

inline
Float<float32_t>::Float(float32_t v) {
  u._value = v;
}

inline float32_t
Float<float32_t>::value() {
  return u._value;
}

inline int
Float<float32_t>::is_inf() const {
  if (u.word == NEG_INF)
    return -1;
  if (u.word == POS_INF)
    return 1;
  return 0;
}

inline int
Float<float32_t>::is_nan() const {
  return (u.word & ~SGN_MASK) > POS_INF;
}

inline int
Float<float32_t>::is_zero() const {
  if (u.word == NEG_ZERO)
    return -1;
  if (u.word == POS_ZERO)
    return 1;
  return 0;
}

inline void
Float<float32_t>::negate() {
  u.word ^= SGN_MASK;
}

inline int
Float<float32_t>::sign_bit() const {
  return !!(u.word & SGN_MASK);
}

inline void
Float<float32_t>::dec() {
  u.word--;
}

inline void
Float<float32_t>::inc() {
  u.word++;
}

template<>
class Float<float64_t> {
private:
  union {
    float64_t _value;
    struct {
#if __FLOAT_WORD_ORDER == LITTLE_ENDIAN
      u_int32_t lsp;
      u_int32_t msp;
#endif
#if __FLOAT_WORD_ORDER == BIG_ENDIAN
      u_int32_t msp;
      u_int32_t lsp;
#endif
    } parts;
  } u;
  static const u_int32_t MSP_SGN_MASK = 0x80000000;
  static const u_int32_t MSP_POS_INF = 0x7ff00000;
  static const u_int32_t MSP_NEG_INF = 0xfff00000;
  static const u_int32_t MSP_POS_ZERO = 0x00000000;
  static const u_int32_t MSP_NEG_ZERO = 0x80000000;
  static const u_int32_t LSP_INF = 0;
  static const u_int32_t LSP_ZERO = 0;
  static const u_int32_t LSP_MAX = 0xffffffff;
public:
  Float(float64_t v);
  float64_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
};

inline
Float<float64_t>::Float(float64_t v) {
  u._value = v;
}

inline float64_t
Float<float64_t>::value() {
  return u._value;
}

inline int
Float<float64_t>::is_inf() const {
  if (u.parts.lsp != LSP_INF)
    return 0;
  if (u.parts.msp == MSP_NEG_INF)
    return -1;
  if (u.parts.msp == MSP_POS_INF)
    return 1;
  return 0;
}

inline int
Float<float64_t>::is_nan() const {
  u_int32_t a = u.parts.msp & ~MSP_SGN_MASK;
  return a > MSP_POS_INF || (a == MSP_POS_INF && u.parts.lsp != LSP_INF);
}

inline int
Float<float64_t>::is_zero() const {
  if (u.parts.lsp != LSP_ZERO)
    return 0;
  if (u.parts.msp == MSP_NEG_ZERO)
    return -1;
  if (u.parts.msp == MSP_POS_ZERO)
    return 1;
  return 0;
}

inline void
Float<float64_t>::negate() {
  u.parts.msp ^= MSP_SGN_MASK;
}

inline int
Float<float64_t>::sign_bit() const {
  return !!(u.parts.msp & MSP_SGN_MASK);
}

inline void
Float<float64_t>::dec() {
  if (u.parts.lsp == 0) {
    u.parts.msp--;
    u.parts.lsp = LSP_MAX;
  }
  else
    u.parts.lsp--;
}

inline void
Float<float64_t>::inc() {
  if (u.parts.lsp == LSP_MAX) {
    u.parts.msp++;
    u.parts.lsp = 0;
  }
  else
    u.parts.lsp++;
}

#ifdef FLOAT96_TYPE

template<>
class Float<float96_t> {
private:
  union {
    float96_t _value;
    struct {
#if __FLOAT_WORD_ORDER == LITTLE_ENDIAN
      u_int64_t lsp;
      u_int32_t msp;
#endif
#if __FLOAT_WORD_ORDER == BIG_ENDIAN
      u_int32_t msp;
      u_int64_t lsp;
#endif
    } parts;
  } u;
  static const u_int32_t MSP_SGN_MASK = 0x00008000;
  static const u_int32_t MSP_POS_INF = 0x00007fff;
  static const u_int32_t MSP_NEG_INF = 0x0000ffff;
  static const u_int32_t MSP_POS_ZERO = 0x00000000;
  static const u_int32_t MSP_NEG_ZERO = 0x00008000;
  static const u_int64_t LSP_INF = 0x8000000000000000ULL;
  static const u_int64_t LSP_ZERO = 0;
  static const u_int64_t LSP_DMAX = 0x7fffffffffffffffULL;
  static const u_int64_t LSP_NMAX = 0xffffffffffffffffULL;
public:
  Float(float96_t v);
  float96_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
};

inline
Float<float96_t>::Float(float96_t v) {
  u._value = v;
}

inline float96_t
Float<float96_t>::value() {
  return u._value;
}

inline int
Float<float96_t>::is_inf() const {
  if (u.parts.lsp != LSP_INF)
    return 0;
  u_int32_t a = u.parts.msp & MSP_NEG_INF;
  if (a == MSP_NEG_INF)
    return -1;
  if (a == MSP_POS_INF)
    return 1;
  return 0;
}

inline int
Float<float96_t>::is_nan() const {
  return (u.parts.msp & MSP_POS_INF) == MSP_POS_INF &&
    u.parts.lsp != LSP_INF;
}

inline int
Float<float96_t>::is_zero() const {
  if (u.parts.lsp != LSP_ZERO)
    return 0;
  u_int32_t a = u.parts.msp & MSP_NEG_INF;
  if (a == MSP_NEG_ZERO)
    return -1;
  if (a == MSP_POS_ZERO)
    return 1;
  return 0;
}

inline void
Float<float96_t>::negate() {
  u.parts.msp ^= MSP_SGN_MASK;
}

inline int
Float<float96_t>::sign_bit() const {
  return !!(u.parts.msp & MSP_SGN_MASK);
}

inline void
Float<float96_t>::dec() {
  if ((u.parts.lsp & LSP_DMAX) == 0) {
    u.parts.msp--;
    u.parts.lsp = (u.parts.msp & MSP_NEG_INF) == 0 ? LSP_DMAX : LSP_NMAX;
  }
  else
    u.parts.lsp--;
}

inline void
Float<float96_t>::inc() {
  if ((u.parts.lsp & LSP_DMAX) == LSP_DMAX) {
    u.parts.msp++;
    u.parts.lsp = LSP_DMAX + 1;
  }
  else
    u.parts.lsp++;
}

#endif

#ifdef FLOAT128_TYPE

template<>
class Float<float128_t> {
private:
  union {
    float128_t _value;
    struct {
#if __FLOAT_WORD_ORDER == LITTLE_ENDIAN
      u_int64_t lsp;
      u_int64_t msp;
#endif
#if __FLOAT_WORD_ORDER == BIG_ENDIAN
      u_int64_t msp;
      u_int64_t lsp;
#endif
    } parts;
  } u;
  static const u_int64_t MSP_SGN_MASK = 0x8000000000000000ULL;
  static const u_int64_t MSP_POS_INF = 0x7fff000000000000ULL;
  static const u_int64_t MSP_NEG_INF = 0xffff000000000000ULL;
  static const u_int64_t MSP_POS_ZERO = 0x0000000000000000ULL;
  static const u_int64_t MSP_NEG_ZERO = 0x8000000000000000ULL;
  static const u_int64_t LSP_INF = 0;
  static const u_int64_t LSP_ZERO = 0;
  static const u_int64_t LSP_MAX = 0xffffffffffffffffULL;
public:
  Float(float128_t v);
  float128_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
};

inline
Float<float128_t>::Float(float128_t v) {
  u._value = v;
}

inline float128_t
Float<float128_t>::value() {
  return u._value;
}

inline int
Float<float128_t>::is_inf() const {
  if (u.parts.lsp != LSP_INF)
    return 0;
  if (u.parts.msp == MSP_NEG_INF)
    return -1;
  if (u.parts.msp == MSP_POS_INF)
    return 1;
  return 0;
}

inline int
Float<float128_t>::is_nan() const {
  return (u.parts.msp & ~MSP_SGN_MASK) == MSP_POS_INF &&
    u.parts.lsp != LSP_INF;
}

inline int
Float<float128_t>::is_zero() const {
  if (u.parts.lsp != LSP_ZERO)
    return 0;
  if (u.parts.msp == MSP_NEG_ZERO)
    return -1;
  if (u.parts.msp == MSP_POS_ZERO)
    return 1;
  return 0;
}

inline void
Float<float128_t>::negate() {
  u.parts.msp ^= MSP_SGN_MASK;
}

inline int
Float<float128_t>::sign_bit() const {
  return !!(u.parts.msp & MSP_SGN_MASK);
}

inline void
Float<float128_t>::dec() {
  if (u.parts.lsp == 0) {
    u.parts.msp--;
    u.parts.lsp = LSP_MAX;
  }
  else
    u.parts.lsp--;
}

inline void
Float<float128_t>::inc() {
  if (u.parts.lsp == LSP_MAX) {
    u.parts.msp++;
    u.parts.lsp = 0;
  }
  else
    u.parts.lsp++;
}

#endif

template <typename Policy, typename T>
inline Result
value_type_float(const T v) {
  Float<T> f(v);
  if (Policy::check_nan_arg && f.is_nan())
    return V_UNKNOWN;
  if (Policy::check_infinity_arg) {
    int i = f.is_inf();
    if (i < 0)
      return V_NEG_OVERFLOW;
    if (i > 0)
      return V_POS_OVERFLOW;
  }
  return V_EQ;
}

template <typename Policy, typename T>
inline void
set_special_float(T& v, const Result r) {
  switch (r) {
  case V_NEG_OVERFLOW:
    v = -HUGE_VAL;
    return;
  case V_POS_OVERFLOW:
    v = HUGE_VAL;
    return;
  case V_DOMAIN:
  case V_UNKNOWN:
    v = NAN;
    return;
  }
}

template <typename Policy, typename T>
inline Result
pred_float(T& v) {
  Float<T> f(v);
  if (Policy::check_nan_arg) {
    if (f.is_nan())
      return V_UNKNOWN;
  }
  if (Policy::check_infinity_arg) {
    if (f.is_inf() < 0)
      return V_NEG_OVERFLOW;
  }
  Result r = V_EQ;
  if (f.is_zero() > 0) {
    f.negate();
    f.inc();
  }
  else if (f.sign_bit) {
    f.inc();
    if (Policy::check_overflow && f.is_inf())
      r = V_NEG_OVERFLOW;
  }
  else {
    f.dec();
  }
  v = f.value();
  return r;
}

template <typename Policy, typename T>
inline Result
succ_float(T& v) {
  Float<T> f(v);
  if (Policy::check_nan_arg) {
    if (f.is_nan())
      return V_UNKNOWN;
  }
  if (Policy::check_infinity_arg) {
    if (f.is_inf() > 0)
      return V_POS_OVERFLOW;
  }
  Result r = V_EQ;
  if (f.is_zero() < 0) {
    f.negate();
    f.inc();
  }
  else if (!f.sign_bit) {
    f.inc();
    if (Policy::check_overflow && f.is_inf())
      r = V_POS_OVERFLOW;
  }
  else {
    f.dec();
  }
  v = f.value();
  return V_EQ;
}

SPECIALIZE_VALUE_TYPE(float, float32_t);
SPECIALIZE_SET_SPECIAL(float, float32_t);
SPECIALIZE_PRED(float, float32_t);
SPECIALIZE_SUCC(float, float32_t);

SPECIALIZE_VALUE_TYPE(float, float64_t);
SPECIALIZE_SET_SPECIAL(float, float64_t);
SPECIALIZE_PRED(float, float64_t);
SPECIALIZE_SUCC(float, float64_t);

#ifdef FLOAT96_TYPE
SPECIALIZE_VALUE_TYPE(float, float96_t);
SPECIALIZE_SET_SPECIAL(float, float96_t);
SPECIALIZE_PRED(float, float96_t);
SPECIALIZE_SUCC(float, float96_t);
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_VALUE_TYPE(float, float128_t);
SPECIALIZE_SET_SPECIAL(float, float128_t);
SPECIALIZE_PRED(float, float128_t);
SPECIALIZE_SUCC(float, float128_t);
#endif

template <typename Policy, typename Type>
inline Result 
check_inexact(const Type v) {
  if (!Policy::check_inexact)
    return V_EQ;
  int r;
  switch (fpu_is_inexact()) {
  case 0:
    return V_EQ;
  case 1:
    r = 0;
    break;
  default:
    r = V_EQ;
    break;
  }
  switch (fpu_get_rounding_mode()) {
  case -1:
#ifdef FE_TONEAREST
  case FE_TONEAREST:
#endif
    r |= V_GT | V_LT;
#ifdef FE_DOWNWARD
  case FE_DOWNWARD:
    r |= V_GT;
#endif
#ifdef FE_UPWARD
  case FE_UPWARD:
    r |= V_LT;
#endif
#ifdef FE_TOWARDZERO
  case FE_TOWARDZERO:
    if (v <= 0)
      r |= V_LT;
    if (v >= 0)
      r |= V_GT;
#endif
  }
  return static_cast<Result>(r);
}

template <typename Policy>
inline void
prepare_inexact() {
  if (Policy::check_inexact)
    fpu_reset_inexact();
}

template <typename Policy, typename From, typename To>
inline Result 
assign_float_float_exact(To& to, const From from) {
  Float<From> f(from);
  Result r = V_EQ;
  if (Policy::check_nan_arg && f.is_nan())
    r = V_UNKNOWN;
  else if (Policy::check_infinity_arg) {
    int i = f.is_inf();
    if (i < 0)
      r = V_NEG_OVERFLOW;
    else if (i > 0)
      r = V_POS_OVERFLOW;
  }
  to = from;
  return r;
}

template<typename Policy, typename To, typename From>
inline Result
assign_float_float(To& to, const From from) {
  prepare_inexact<Policy>();
  Result r = assign_float_float_exact<Policy>(to, from);
  if (r == V_EQ)
    r = check_inexact<Policy>(from);
  return r;
}

template <typename Policy, typename Type>
inline Result 
assign_result_exact(Type& to, const Type from) {
  Float<Type> f(from);
  Result r = V_EQ;
  if (Policy::check_nan_result && f.is_nan())
    r = V_UNKNOWN;
  else if (Policy::check_overflow) {
    int i = f.is_inf();
    if (i < 0)
      r = V_NEG_OVERFLOW;
    else if (i > 0)
      r = V_POS_OVERFLOW;
  }
  to = from;
  return r;
}

template <typename Policy, typename Type>
inline Result 
assign_result_inexact(Type& to, const Type from) {
  Result r = assign_result_exact<Policy>(to, from);
  if (r == V_EQ)
    r = check_inexact<Policy>(from);
  return r;
}

template <typename Policy, typename Type>
inline Result 
neg_float(Type& to, const Type from) {
  return assign_result_exact<Policy>(to, -from);
}

template <typename Policy, typename Type>
inline Result 
add_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x + y);
}

template <typename Policy, typename Type>
inline Result 
sub_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x - y);
}

template <typename Policy, typename Type>
inline Result 
mul_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x * y);
}

template <typename Policy, typename Type>
inline Result 
div_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, x / y);
}

template <typename Policy, typename Type>
inline Result 
mod_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::fmod(x, y));
}

template <typename Policy, typename Type>
inline Result
abs_float(Type& to, const Type from) {
  return assign_result_exact<Policy>(to, std::abs(from));
}

template <typename Policy, typename Type>
inline Result
sqrt_float(Type& to, const Type from) {
  prepare_inexact<Policy>();
  return assign_result_inexact<Policy>(to, std::sqrt(from));
}

template <typename Policy, typename To, typename From>
inline Result 
assign_float_int_exact(To& to, const From from) {
  to = from;
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_float_int(To& to, const From from) {
  prepare_inexact<Policy>();
  to = from;
  return check_inexact<Policy>(to);
}

// This is needed to return stricter result when 
// rounding mode is set up or down
template <typename Policy, typename Type>
inline Result 
sub_mul_float(Type& to, const Type x, const Type y) {
  return add_mul<Policy>(to, x, -y);
}

#define ASSIGN_R2(Smaller, Larger) \
SPECIALIZE_ASSIGN(float_float_exact, Larger, Smaller) \
SPECIALIZE_ASSIGN(float_float, Smaller, Larger)

SPECIALIZE_ASSIGN(float_int_exact, float32_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float32_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int, float32_t, int32_t)
SPECIALIZE_ASSIGN(float_int, float32_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float32_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int, float32_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float, float32_t, float32_t)
ASSIGN_R2(float32_t, float64_t)

SPECIALIZE_NEG(float, float32_t, float32_t)
SPECIALIZE_ABS(float, float32_t, float32_t)
SPECIALIZE_ADD(float, float32_t, float32_t)
SPECIALIZE_SUB(float, float32_t, float32_t)
SPECIALIZE_MUL(float, float32_t, float32_t)
SPECIALIZE_DIV(float, float32_t, float32_t)
SPECIALIZE_MOD(float, float32_t, float32_t)
SPECIALIZE_SQRT(float, float32_t, float32_t)
SPECIALIZE_SUB_MUL(float, float32_t, float32_t)

SPECIALIZE_ASSIGN(float_int_exact, float64_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float64_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int, float64_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float64_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float, float64_t, float64_t)

SPECIALIZE_NEG(float, float64_t, float64_t)
SPECIALIZE_ABS(float, float64_t, float64_t)
SPECIALIZE_ADD(float, float64_t, float64_t)
SPECIALIZE_SUB(float, float64_t, float64_t)
SPECIALIZE_MUL(float, float64_t, float64_t)
SPECIALIZE_DIV(float, float64_t, float64_t)
SPECIALIZE_MOD(float, float64_t, float64_t)
SPECIALIZE_SQRT(float, float64_t, float64_t)
SPECIALIZE_SUB_MUL(float, float64_t, float64_t)

#ifdef FLOAT96_TYPE
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, int64_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float96_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float, float96_t, float96_t)
ASSIGN_R2(float32_t, float96_t)
ASSIGN_R2(float64_t, float96_t)

SPECIALIZE_NEG(float, float96_t, float96_t)
SPECIALIZE_ABS(float, float96_t, float96_t)
SPECIALIZE_ADD(float, float96_t, float96_t)
SPECIALIZE_SUB(float, float96_t, float96_t)
SPECIALIZE_MUL(float, float96_t, float96_t)
SPECIALIZE_DIV(float, float96_t, float96_t)
SPECIALIZE_MOD(float, float96_t, float96_t)
SPECIALIZE_SQRT(float, float96_t, float96_t)
SPECIALIZE_SUB_MUL(float, float96_t, float96_t)
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, int64_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int_exact, float127_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int_exact, float128_t, u_int64_t)
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
SPECIALIZE_MOD(float, float128_t, float128_t)
SPECIALIZE_SQRT(float, float128_t, float128_t)
SPECIALIZE_SUB_MUL(float, float128_t, float128_t)
#endif

#undef ASSIGN_R2

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
