/* IEC 559 floating point format related functions.
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

#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Float_inlines_hh)
