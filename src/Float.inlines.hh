/* IEC 559 floating point format related functions.
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

#ifndef PPL_Float_inlines_hh
#define PPL_Float_inlines_hh 1

#include "Float.defs.hh"

namespace Parma_Polyhedra_Library {

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

inline void
Float<float32_t>::build(bool negative, mpz_t mantissa, int exponent) {
  unsigned long m = mpz_get_ui(mantissa);
  u.word = negative ? SGN_MASK : 0;
  u.word |= static_cast<uint32_t>(exponent + (1 << (EXPONENT_BITS - 1)) - 1) << MANTISSA_BITS;
  u.word |= m & ((1UL << MANTISSA_BITS) - 1);
}

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
  uint32_t a = u.parts.msp & ~MSP_SGN_MASK;
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

inline void
Float<float64_t>::build(bool negative, mpz_t mantissa, int exponent) {
  u.parts.msp = (negative ? MSP_SGN_MASK : 0);
  u.parts.msp |= static_cast<uint32_t>(exponent + (1 << (EXPONENT_BITS - 1)) - 1) << (MANTISSA_BITS - 32);
#if ULONG_MAX == 0xffffffffUL
  u.parts.lsp = mpz_get_ui(mantissa);
  mpz_tdiv_q_2exp(mantissa, mantissa, 32);
  unsigned long m = mpz_get_ui(mantissa);
#else
  unsigned long m = mpz_get_ui(mantissa);
  u.parts.lsp = m;
  m >>= 32;
#endif
  u.parts.msp |= m & ((1UL << (MANTISSA_BITS - 32)) - 1);
}
    
#ifdef FLOAT96_TYPE

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
  uint32_t a = u.parts.msp & MSP_NEG_INF;
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
  uint32_t a = u.parts.msp & MSP_NEG_INF;
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

inline void
Float<float96_t>::build(bool negative, mpz_t mantissa, int exponent) {
  u.parts.msp = (negative ? MSP_SGN_MASK : 0);
  u.parts.msp |= static_cast<uint32_t>(exponent + (1 << (EXPONENT_BITS - 1)) - 1);
#if ULONG_MAX == 0xffffffffUL
  mpz_export(&u.parts.lsp, 0, 1, 8, 0, 0, mantissa);
#else
  u.parts.lsp = mpz_get_ui(mantissa);
#endif
}
    
#endif

#ifdef FLOAT128_TYPE

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

inline void
Float<float128_t>::build(bool negative, mpz_t mantissa, int exponent) {
  unsigned long m = mpz_get_ui(mantissa);
  u.parts.msp = (negative ? MSP_SGN_MASK : 0);
  u.parts.msp |= static_cast(uint64_t)(exponent + (1 << (EXPONENT_BITS - 1)) - 1) << (MANTISSA_BITS - 64);
  u.parts.msp |= ;
#if ULONG_MAX == 0xffffffffUL
  mpz_export(&u.parts.lsp, 0, 1, 8, 0, 0, mantissa)
#else
  u.parts.lsp = mpz_get_ui(mantissa);
#endif
  mpz_tdiv_q_2exp(mantissa, mantissa, 64);
  uint64_t m;
#if ULONG_MAX == 0xffffffffUL
  mpz_export(&u.parts.lsp, 0, 1, 8, 0, 0, mantissa)
#else
  m = mpz_get_ui(mantissa);
#endif
  u.parts.msp |= m & ((1UL << (MANTISSA_BITS - 64)) - 1);
}
    
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Float_inlines_hh)
