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

typedef union {
  float32_t value;
  u_int32_t word;
} float32_iec559_union;

inline void
get_float32_iec559_word(float32_t v, u_int32_t& w) {
  float32_iec559_union u;
  u.value = v;
  w = u.word;
}

inline void
set_float32_iec559_word(float32_t &v, u_int32_t w) {
  float32_iec559_union u;
  u.word = w;
  v = u.value;
}

#define REAL32_SGN_MASK  0x80000000
#define REAL32_PINF    0x7f800000
#define REAL32_NINF    0xff800000
#define REAL32_PZERO    0x00000000
#define REAL32_NZERO    0x80000000
#define REAL32_PEPS    0x00000001
#define REAL32_NEPS    0x80000001

inline Result
check_normal_parts(u_int32_t w) {
  u_int32_t a = w & ~REAL32_SGN_MASK;
  if (a == REAL32_PINF)
    return (w & REAL32_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= REAL32_PINF)
    return V_NAN;
  return V_EQ;
}

inline Result
check_normal_(float32_t v) {
  u_int32_t w;
  get_float32_iec559_word(v, w);
  return check_normal_parts(w);
}

template <typename Policy>
inline Result
pred(float32_t& to) {
  u_int32_t w;
  get_float32_iec559_word(to, w);
  if (Policy::check_normal) {
    Result r = check_normal_parts(w);
    if (r != V_EQ)
      return r;
  }
  if (w & REAL32_SGN_MASK) {
    ++w;
    if (w == REAL32_NINF)
      return V_NEG_OVERFLOW;
  } else if (w == REAL32_PZERO) {
    w = REAL32_NEPS;
  } else {
    --w;
  }
  set_float32_iec559_word(to, w);
  return V_EQ;
}

template <typename Policy>
inline Result
succ(float32_t &to) {
  u_int32_t w;
  get_float32_iec559_word(to, w);
  if (Policy::check_normal) {
    Result r = check_normal_parts(w);
    if (r != V_EQ)
      return r;
  }
  if (!(w & REAL32_SGN_MASK)) {
    ++w;
    if (w == REAL32_PINF)
      return V_POS_OVERFLOW;
  } else if (w == REAL32_NZERO) {
    w = REAL32_PEPS;
  } else {
    --w;
  }
  set_float32_iec559_word(to, w);
  return V_EQ;
}

typedef union {
  float64_t value;
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
} float64_iec559_union;

inline void
get_float64_iec559_parts(float64_t v, u_int32_t& lsp, u_int32_t& msp) {
  float64_iec559_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

inline void
set_float64_iec559_parts(float64_t& v, u_int32_t lsp, u_int32_t msp) {
  float64_iec559_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define REAL64_MSP_SGN_MASK  0x80000000
#define REAL64_MSP_PINF    0x7ff00000
#define REAL64_MSP_NINF    0xfff00000
#define REAL64_MSP_NEG    0x80000000
#define REAL64_MSP_POS    0x00000000
#define REAL64_LSP_INF    0
#define REAL64_LSP_EPS    1
#define REAL64_LSP_MAX    0xffffffff

inline Result
check_normal_parts(u_int32_t lsp, u_int32_t msp) {
  u_int32_t a = msp & ~REAL64_MSP_SGN_MASK;
  if (a == REAL64_MSP_PINF && lsp == REAL64_LSP_INF)
    return (msp & REAL64_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= REAL64_MSP_PINF)
    return V_NAN;
  return V_EQ;
}

inline Result
check_normal_(float64_t v) {
  u_int32_t lsp, msp;
  get_float64_iec559_parts(v, lsp, msp);
  return check_normal_parts(lsp, msp);
}

template <typename Policy>
inline Result
pred(float64_t& to) {
  u_int32_t lsp, msp;
  get_float64_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (msp & REAL64_MSP_SGN_MASK) {
    if (lsp == REAL64_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL64_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL64_MSP_POS) {
        msp = REAL64_MSP_NEG;
        lsp = REAL64_LSP_EPS;
      } else {
        lsp = REAL64_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float64_iec559_parts(to, lsp, msp);
  return V_EQ;
}

template <typename Policy>
inline Result
succ(float64_t& to) {
  u_int32_t lsp, msp;
  get_float64_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (!(msp & REAL64_MSP_SGN_MASK)) {
    if (lsp == REAL64_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL64_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL64_MSP_NEG) {
        lsp = REAL64_MSP_POS;
        msp = REAL64_LSP_EPS;
      } else {
        lsp = REAL64_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float64_iec559_parts(to, lsp, msp);
  return V_EQ;
}

#ifdef FLOAT96_TYPE

typedef union {
  float96_t value;
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
} float96_iec559_union;

inline void
get_float96_iec559_parts(float96_t v, u_int64_t& lsp, u_int32_t& msp) {
  float96_iec559_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

inline void
set_float96_iec559_parts(float96_t &v, u_int64_t lsp, u_int32_t msp) {
  float96_iec559_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define REAL96_MSP_SGN_MASK  0x00008000
#define REAL96_MSP_PINF    0x00007fff
#define REAL96_MSP_NINF    0x0000ffff
#define REAL96_MSP_NEG    0x00008000
#define REAL96_MSP_POS    0x00000000
#define REAL96_LSP_INF    0
#define REAL96_LSP_EPS    1
#define REAL96_LSP_MAX    0xffffffffffffffffULL

inline Result
check_normal_parts(u_int64_t lsp, u_int32_t msp) {
  u_int32_t a = msp & ~REAL96_MSP_SGN_MASK;
  if (a == REAL96_MSP_PINF && lsp == REAL96_LSP_INF)
    return (msp & REAL96_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= REAL96_MSP_PINF)
    return V_NAN;
  return V_EQ;
}

inline Result
check_normal_(float96_t v) {
  u_int64_t lsp;
  u_int32_t msp;
  get_float96_iec559_parts(v, lsp, msp);
  return check_normal_parts(lsp, msp);
}

template <typename Policy>
inline Result
pred(float96_t& to) {
  u_int64_t lsp;
  u_int32_t msp;
  get_float96_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (msp & REAL96_MSP_SGN_MASK) {
    if (lsp == REAL96_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL96_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL96_MSP_POS) {
        msp = REAL96_MSP_NEG;
        lsp = REAL96_LSP_EPS;
      } else {
        lsp = REAL96_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float96_iec559_parts(to, lsp, msp);
  return V_EQ;
}

template <typename Policy>
inline Result
succ(float96_t& to) {
  u_int64_t lsp;
  u_int32_t msp;
  get_float96_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (!(msp & REAL96_MSP_SGN_MASK)) {
    if (lsp == REAL96_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL96_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL96_MSP_NEG) {
        lsp = REAL96_MSP_POS;
        msp = REAL96_LSP_EPS;
      } else {
        lsp = REAL96_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float96_iec559_parts(to, lsp, msp);
  return V_EQ;
}

#endif

#ifdef FLOAT128_TYPE

typedef union {
  float128_t value;
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
} float128_iec559_union;

inline void
get_float128_iec559_parts(float128_t v, u_int64_t& lsp, u_int64_t& msp) {
  float128_iec559_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

inline void
set_float128_iec559_parts(float128_t& v, u_int64_t lsp, u_int64_t msp) {
  float128_iec559_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define REAL128_MSP_SGN_MASK  0x8000000000000000
#define REAL128_MSP_PINF    0x7fff000000000000
#define REAL128_MSP_NINF    0xffff000000000000
#define REAL128_MSP_NEG    0x8000000000000000
#define REAL128_MSP_POS    0x0000000000000000
#define REAL128_LSP_INF    0
#define REAL128_LSP_EPS    1
#define REAL128_LSP_MAX    0xffffffffffffffff

inline Result
check_normal_parts(u_int64_t lsp, u_int64_t msp) {
  u_int64_t a = msp & ~REAL128_MSP_SGN_MASK;
  if (a == REAL128_MSP_PINF && lsp == REAL128_LSP_INF)
    return (msp & REAL128_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= REAL128_MSP_PINF)
    return V_NAN;
  return V_EQ;
}

inline Result
check_normal_(float128_t v) {
  u_int64_t lsp, msp;
  get_float128_iec559_parts(v, lsp, msp);
  return check_normal_parts(lsp, msp);
}

template <typename Policy>
inline Result
pred(float128_t& to) {
  u_int64_t lsp, msp;
  get_float128_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (msp & REAL128_MSP_SGN_MASK) {
    if (lsp == REAL128_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL128_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL128_MSP_POS) {
        msp = REAL128_MSP_NEG;
        lsp = REAL128_LSP_EPS;
      } else {
        lsp = REAL128_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float128_iec559_parts(to, lsp, msp);
  return V_EQ;
}

template <typename Policy>
inline Result
succ(float128_t& to) {
  u_int64_t lsp, msp;
  get_float128_iec559_parts(to, lsp, msp);
  if (Policy::check_normal) {
    Result r = check_normal_parts(lsp, msp);
    if (r != V_EQ)
      return r;
  }
  if (!(msp & REAL128_MSP_SGN_MASK)) {
    if (lsp == REAL128_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == REAL128_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == REAL128_MSP_NEG) {
        lsp = REAL128_MSP_POS;
        msp = REAL128_LSP_EPS;
      } else {
        lsp = REAL128_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float128_iec559_parts(to, lsp, msp);
  return V_EQ;
}

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

template <typename Policy, typename Type>
inline Result
check_normal(const Type v) {
  return Policy::check_normal ? check_normal_(v) : V_EQ;
}

template <typename Policy, typename Type>
inline Result
check_result(const Type v) {
  return Policy::check_overflow ? check_normal_(v) : V_EQ;
}
    
template <typename Policy, typename From, typename To>
inline Result 
assign_float_float_exact(To& to, const From from) {
  Result r = check_normal<Policy>(from);
  if (r != V_EQ)
    return r;
  to = from;
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_float_float(To& to, const From from) {
  prepare_inexact<Policy>();
  Result r = assign_float_float_exact<Policy>();
  if (r != V_EQ)
    return r;
  return check_inexact<Policy>(from);
}

template <typename Policy, typename Type>
inline Result 
neg_float(Type& to, const Type from) {
  Result r = check_normal<Policy>(from);
  if (r != V_EQ)
    return r;
  to = -from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
assign_float_float_inexact_(Type& to, const Type from) {
  Result r = check_result<Policy>(from);
  if (r != V_EQ)
    return r;
  to = from;
  return check_inexact<Policy>(from);
}

template <typename Policy, typename Type>
inline Result 
add_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, x + y);
}

template <typename Policy, typename Type>
inline Result 
sub_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, x - y);
}

template <typename Policy, typename Type>
inline Result 
mul_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, x * y);
}

template <typename Policy, typename Type>
inline Result 
div_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, x / y);
}

template <typename Policy, typename Type>
inline Result 
mod_float(Type& to, const Type x, const Type y) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, std::fmod(x, y));
}

template <typename Policy, typename Type>
inline Result
abs_float(Type& to, const Type from) {
  Result r = check_normal<Policy>(from);
  if (r != V_EQ)
    return r;
  to = std::abs(from);
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sqrt_float(Type& to, const Type from) {
  prepare_inexact<Policy>();
  return assign_float_float_inexact_<Policy>(to, std::sqrt(from));
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
specialize_all(float, float128_t, float128_t)

SPECIALIZE_NEG(float, float128_t, float128_t)
SPECIALIZE_ABS(float, float128_t, float128_t)
SPECIALIZE_ADD(float, float128_t, float128_t)
SPECIALIZE_SUB(float, float128_t, float128_t)
SPECIALIZE_MUL(float, float128_t, float128_t)
SPECIALIZE_DIV(float, float128_t, float128_t)
SPECIALIZE_MOD(float, float128_t, float128_t)
SPECIALIZE_SQRT(float, float128_t, float128_t)
#endif

#undef ASSIGN_R2

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
