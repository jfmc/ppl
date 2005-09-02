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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Float_defs_hh
#define PPL_Float_defs_hh 1

#include "compiler.hh"
#include "float.types.hh"
#include <gmp.h>
#include <cassert>
#include <cmath>

#ifndef NAN
#define NAN (HUGE_VAL - HUGE_VAL)
#endif

namespace Parma_Polyhedra_Library {

template <typename T>
struct Float {
  static const bool fpu_related = false;
};

template <>
class Float<float32_t> {
private:
  static const uint32_t SGN_MASK = 0x80000000;
  static const uint32_t EXP_MASK = 0x7f800000;
  static const uint32_t POS_INF = 0x7f800000;
  static const uint32_t NEG_INF = 0xff800000;
  static const uint32_t POS_ZERO = 0x00000000;
  static const uint32_t NEG_ZERO = 0x80000000;
  union {
    float32_t _value;
    uint32_t word;
  } u;
public:
  static const int EXPONENT_BITS = 8;
  static const int MANTISSA_BITS = 23;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN - MANTISSA_BITS;
  Float(float32_t v);
  float32_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void build(bool negative, mpz_t mantissa, int exponent);
  static const bool fpu_related = true;
};

template <>
class Float<float64_t> {
private:
  union {
    float64_t _value;
    struct {
#ifdef WORDS_BIGENDIAN
      uint32_t msp;
      uint32_t lsp;
#else
      uint32_t lsp;
      uint32_t msp;
#endif
    } parts;
  } u;
  static const uint32_t MSP_SGN_MASK = 0x80000000;
  static const uint32_t MSP_POS_INF = 0x7ff00000;
  static const uint32_t MSP_NEG_INF = 0xfff00000;
  static const uint32_t MSP_POS_ZERO = 0x00000000;
  static const uint32_t MSP_NEG_ZERO = 0x80000000;
  static const uint32_t LSP_INF = 0;
  static const uint32_t LSP_ZERO = 0;
  static const uint32_t LSP_MAX = 0xffffffff;
public:
  static const int EXPONENT_BITS = 11;
  static const int MANTISSA_BITS = 52;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN - MANTISSA_BITS;
  Float(float64_t v);
  float64_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void build(bool negative, mpz_t mantissa, int exponent);
  static const bool fpu_related = true;
};

#ifdef FLOAT96_TYPE

template <>
class Float<float96_t> {
private:
  union {
    float96_t _value;
    struct {
#ifdef WORDS_BIGENDIAN
      uint32_t msp;
      uint64_t lsp;
#else
      uint64_t lsp;
      uint32_t msp;
#endif
    } parts;
  } u;
  static const uint32_t MSP_SGN_MASK = 0x00008000;
  static const uint32_t MSP_POS_INF = 0x00007fff;
  static const uint32_t MSP_NEG_INF = 0x0000ffff;
  static const uint32_t MSP_POS_ZERO = 0x00000000;
  static const uint32_t MSP_NEG_ZERO = 0x00008000;
  static const uint64_t LSP_INF = 0x8000000000000000ULL;
  static const uint64_t LSP_ZERO = 0;
  static const uint64_t LSP_DMAX = 0x7fffffffffffffffULL;
  static const uint64_t LSP_NMAX = 0xffffffffffffffffULL;
public:
  static const int EXPONENT_BITS = 15;
  static const int MANTISSA_BITS = 63;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN - MANTISSA_BITS;
  Float(float96_t v);
  float96_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void build(bool negative, mpz_t mantissa, int exponent);
  static const bool fpu_related = true;
};

#endif

#ifdef FLOAT128_TYPE

template <>
class Float<float128_t> {
private:
  union {
    float128_t _value;
    struct {
#ifdef WORDS_BIGENDIAN
      uint64_t msp;
      uint64_t lsp;
#else
      uint64_t lsp;
      uint64_t msp;
#endif
    } parts;
  } u;
  static const uint64_t MSP_SGN_MASK = 0x8000000000000000ULL;
  static const uint64_t MSP_POS_INF = 0x7fff000000000000ULL;
  static const uint64_t MSP_NEG_INF = 0xffff000000000000ULL;
  static const uint64_t MSP_POS_ZERO = 0x0000000000000000ULL;
  static const uint64_t MSP_NEG_ZERO = 0x8000000000000000ULL;
  static const uint64_t LSP_INF = 0;
  static const uint64_t LSP_ZERO = 0;
  static const uint64_t LSP_MAX = 0xffffffffffffffffULL;
public:
  static const int EXPONENT_BITS = 15;
  static const int MANTISSA_BITS = 112;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN - MANTISSA_BITS;
  Float(float128_t v);
  float128_t value();
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void build(bool negative, mpz_t mantissa, int exponent);
  static const bool fpu_related = true;
};

#endif

} // namespace Parma_Polyhedra_Library

#include "Float.inlines.hh"

#endif // !defined(PPL_Float_defs_hh)
