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

#include "checked_float.defs.hh"

#define PARANOID 1

namespace Parma_Polyhedra_Library {

typedef union {
  float32_iec559_t value;
  u_int32_t word;
} float32_union;

static inline void
get_float32_iec559_word(float32_iec559_t v, u_int32_t& w) {
  float32_union u;
  u.value = v;
  w = u.word;
}

static inline void
set_float32_iec559_word(float32_iec559_t &v, u_int32_t w) {
  float32_union u;
  u.word = w;
  v = u.value;
}

#define R32_SGN_MASK  0x80000000
#define R32_PINF    0x7f800000
#define R32_NINF    0xff800000
#define R32_PZERO    0x00000000
#define R32_NZERO    0x80000000
#define R32_PEPS    0x00000001
#define R32_NEPS    0x80000001

Result_Info
checked_pred(float32_iec559_t& to)
{
  u_int32_t w;
  get_float32_iec559_word(to, w);
#if PARANOID
  u_int32_t a = w & ~R32_SGN_MASK;
  if (a == R32_PINF)
    return (w & R32_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R32_PINF)
    return V_NAN;
#endif
  if (w & R32_SGN_MASK) {
    ++w;
    if (w == R32_NINF)
      return V_NEG_OVERFLOW;
  } else if (w == R32_PZERO) {
    w = R32_NEPS;
  } else {
    --w;
  }
  set_float32_iec559_word(to, w);
  return V_EQ;
}

Result_Info
checked_succ(float32_iec559_t &to)
{
  u_int32_t w;
  get_float32_iec559_word(to, w);
#if PARANOID
  u_int32_t a = w & ~R32_SGN_MASK;
  if (a == R32_PINF)
    return (w & R32_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R32_PINF)
    return V_NAN;
#endif
  if (!(w & R32_SGN_MASK)) {
    ++w;
    if (w == R32_PINF)
      return V_POS_OVERFLOW;
  } else if (w == R32_NZERO) {
    w = R32_PEPS;
  } else {
    --w;
  }
  set_float32_iec559_word(to, w);
  return V_EQ;
}

typedef union {
  float64_iec559_t value;
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
} float64_union;

static inline void
get_float64_iec559_parts(float64_iec559_t v, u_int32_t& lsp, u_int32_t& msp) {
  float64_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

static inline void
set_float64_iec559_parts(float64_iec559_t& v, u_int32_t lsp, u_int32_t msp) {
  float64_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define R64_MSP_SGN_MASK  0x80000000
#define R64_MSP_PINF    0x7ff00000
#define R64_MSP_NINF    0xfff00000
#define R64_MSP_NEG    0x80000000
#define R64_MSP_POS    0x00000000
#define R64_LSP_INF    0
#define R64_LSP_EPS    1
#define R64_LSP_MAX    0xffffffff

Result_Info
checked_pred(float64_iec559_t& to)
{
  u_int32_t lsp, msp;
  get_float64_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int32_t a = msp & ~R64_MSP_SGN_MASK;
  if (a == R64_MSP_PINF && lsp == R64_LSP_INF)
    return (msp & R64_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R64_MSP_PINF)
    return V_NAN;
#endif
  if (msp & R64_MSP_SGN_MASK) {
    if (lsp == R64_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R64_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R64_MSP_POS) {
        msp = R64_MSP_NEG;
        lsp = R64_LSP_EPS;
      } else {
        lsp = R64_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float64_iec559_parts(to, lsp, msp);
  return V_EQ;
}

Result_Info
checked_succ(float64_iec559_t& to)
{
  u_int32_t lsp, msp;
  get_float64_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int32_t a = msp & ~R64_MSP_SGN_MASK;
  if (a == R64_MSP_PINF && lsp == R64_LSP_INF)
    return (msp & R64_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R64_MSP_PINF)
    return V_NAN;
#endif
  if (!(msp & R64_MSP_SGN_MASK)) {
    if (lsp == R64_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R64_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R64_MSP_NEG) {
        lsp = R64_MSP_POS;
        msp = R64_LSP_EPS;
      } else {
        lsp = R64_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float64_iec559_parts(to, lsp, msp);
  return V_EQ;
}

#ifdef FLOAT96_IEC559_TYPE

typedef union {
  float96_iec559_t value;
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
} float96_union;

static inline void
get_float96_iec559_parts(float96_iec559_t v, u_int64_t& lsp, u_int32_t& msp) {
  float96_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

static inline void
set_float96_iec559_parts(float96_iec559_t &v, u_int64_t lsp, u_int32_t msp) {
  float96_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define R96_MSP_SGN_MASK  0x00008000
#define R96_MSP_PINF    0x00007fff
#define R96_MSP_NINF    0x0000ffff
#define R96_MSP_NEG    0x00008000
#define R96_MSP_POS    0x00000000
#define R96_LSP_INF    0
#define R96_LSP_EPS    1
#define R96_LSP_MAX    0xffffffffffffffffULL

Result_Info
checked_pred(float96_iec559_t& to)
{
  u_int64_t lsp;
  u_int32_t msp;
  get_float96_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int32_t a = msp & ~R96_MSP_SGN_MASK;
  if (a == R96_MSP_PINF && lsp == R96_LSP_INF)
    return (msp & R96_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R96_MSP_PINF)
    return V_NAN;
#endif
  if (msp & R96_MSP_SGN_MASK) {
    if (lsp == R96_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R96_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R96_MSP_POS) {
        msp = R96_MSP_NEG;
        lsp = R96_LSP_EPS;
      } else {
        lsp = R96_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float96_iec559_parts(to, lsp, msp);
  return V_EQ;
}

Result_Info
checked_succ(float96_iec559_t& to)
{
  u_int64_t lsp;
  u_int32_t msp;
  get_float96_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int32_t a = msp & ~R96_MSP_SGN_MASK;
  if (a == R96_MSP_PINF && lsp == R96_LSP_INF)
    return (msp & R96_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R96_MSP_PINF)
    return V_NAN;
#endif
  if (!(msp & R96_MSP_SGN_MASK)) {
    if (lsp == R96_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R96_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R96_MSP_NEG) {
        lsp = R96_MSP_POS;
        msp = R96_LSP_EPS;
      } else {
        lsp = R96_LSP_MAX;
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

#ifdef FLOAT128_IEC559_TYPE

typedef union {
  float128_iec559_t value;
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
} float128_union;

static inline void
get_float128_iec559_parts(float128_iec559_t v, u_int64_t& lsp, u_int64_t& msp) {
  float128_union u;
  u.value = v;
  lsp = u.parts.lsp;
  msp = u.parts.msp;
}

static inline void
set_float128_iec559_parts(float128_iec559_t& v, u_int64_t lsp, u_int64_t msp) {
  float128_union u;
  u.parts.lsp = lsp;
  u.parts.msp = msp;
  v = u.value;
}

#define R128_MSP_SGN_MASK  0x8000000000000000
#define R128_MSP_PINF    0x7fff000000000000
#define R128_MSP_NINF    0xffff000000000000
#define R128_MSP_NEG    0x8000000000000000
#define R128_MSP_POS    0x0000000000000000
#define R128_LSP_INF    0
#define R128_LSP_EPS    1
#define R128_LSP_MAX    0xffffffffffffffff

Result_Info
checked_pred(float128_iec559_t& to)
{
  u_int64_t lsp, msp;
  get_float128_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int64_t a = msp & ~R128_MSP_SGN_MASK;
  if (a == R128_MSP_PINF && lsp == R128_LSP_INF)
    return (msp & R128_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R128_MSP_PINF)
    return V_NAN;
#endif
  if (msp & R128_MSP_SGN_MASK) {
    if (lsp == R128_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R128_MSP_NINF)
        return V_NEG_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R128_MSP_POS) {
        msp = R128_MSP_NEG;
        lsp = R128_LSP_EPS;
      } else {
        lsp = R128_LSP_MAX;
        --msp;
      }
    } else {
      --lsp;
    }
  }
  set_float128_iec559_parts(to, lsp, msp);
  return V_EQ;
}

Result_Info
checked_succ(float128_iec559_t& to)
{
  u_int64_t lsp, msp;
  get_float128_iec559_parts(to, lsp, msp);
#if PARANOID
  u_int64_t a = msp & ~R128_MSP_SGN_MASK;
  if (a == R128_MSP_PINF && lsp == R128_LSP_INF)
    return (msp & R128_MSP_SGN_MASK) ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (a >= R128_MSP_PINF)
    return V_NAN;
#endif
  if (!(msp & R128_MSP_SGN_MASK)) {
    if (lsp == R128_LSP_MAX) {
      lsp = 0;
      ++msp;
      if (msp == R128_MSP_PINF)
        return V_POS_OVERFLOW;
    } else {
      ++lsp;
    }
  } else {
    if (lsp == 0) {
      if (msp == R128_MSP_NEG) {
        lsp = R128_MSP_POS;
        msp = R128_LSP_EPS;
      } else {
        lsp = R128_LSP_MAX;
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

} // namespace Parma_Polyhedra_Library

