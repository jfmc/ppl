/* IEC 559 floating point format related functions.
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

#ifndef PPL_Float_defs_hh
#define PPL_Float_defs_hh 1

#include "compiler.hh"
#include <gmp.h>
#include <cassert>
#include <cmath>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifndef NAN
#define NAN (HUGE_VAL - HUGE_VAL)
#endif

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

struct float_ieee754_single {
  uint32_t word;
  static const uint32_t SGN_MASK = 0x80000000;
  static const uint32_t EXP_MASK = 0x7f800000;
  static const uint32_t POS_INF = 0x7f800000;
  static const uint32_t NEG_INF = 0xff800000;
  static const uint32_t POS_ZERO = 0x00000000;
  static const uint32_t NEG_ZERO = 0x80000000;
  static const unsigned int EXPONENT_BITS = 8;
  static const unsigned int MANTISSA_BITS = 23;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN
					- static_cast<int>(MANTISSA_BITS);
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void set_max(bool negative);
  void build(bool negative, mpz_t mantissa, int exponent);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

struct float_ieee754_double {
#ifdef WORDS_BIGENDIAN
  uint32_t msp;
  uint32_t lsp;
#else
  uint32_t lsp;
  uint32_t msp;
#endif
  static const uint32_t MSP_SGN_MASK = 0x80000000;
  static const uint32_t MSP_POS_INF = 0x7ff00000;
  static const uint32_t MSP_NEG_INF = 0xfff00000;
  static const uint32_t MSP_POS_ZERO = 0x00000000;
  static const uint32_t MSP_NEG_ZERO = 0x80000000;
  static const uint32_t LSP_INF = 0;
  static const uint32_t LSP_ZERO = 0;
  static const uint32_t LSP_MAX = 0xffffffff;
  static const unsigned int EXPONENT_BITS = 11;
  static const unsigned int MANTISSA_BITS = 52;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN
					- static_cast<int>(MANTISSA_BITS);
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void set_max(bool negative);
  void build(bool negative, mpz_t mantissa, int exponent);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

struct float_intel_double_extended {
#ifdef WORDS_BIGENDIAN
  uint32_t msp;
  uint64_t lsp;
#else
  uint64_t lsp;
  uint32_t msp;
#endif
  static const uint32_t MSP_SGN_MASK = 0x00008000;
  static const uint32_t MSP_POS_INF = 0x00007fff;
  static const uint32_t MSP_NEG_INF = 0x0000ffff;
  static const uint32_t MSP_POS_ZERO = 0x00000000;
  static const uint32_t MSP_NEG_ZERO = 0x00008000;
  static const uint64_t LSP_INF = 0x8000000000000000ULL;
  static const uint64_t LSP_ZERO = 0;
  static const uint64_t LSP_DMAX = 0x7fffffffffffffffULL;
  static const uint64_t LSP_NMAX = 0xffffffffffffffffULL;
  static const unsigned int EXPONENT_BITS = 15;
  static const unsigned int MANTISSA_BITS = 63;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN
					- static_cast<int>(MANTISSA_BITS);
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void set_max(bool negative);
  void build(bool negative, mpz_t mantissa, int exponent);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

struct float_ieee754_quad {
#ifdef WORDS_BIGENDIAN
  uint64_t msp;
  uint64_t lsp;
#else
  uint64_t lsp;
  uint64_t msp;
#endif
  static const uint64_t MSP_SGN_MASK = 0x8000000000000000ULL;
  static const uint64_t MSP_POS_INF = 0x7fff000000000000ULL;
  static const uint64_t MSP_NEG_INF = 0xffff000000000000ULL;
  static const uint64_t MSP_POS_ZERO = 0x0000000000000000ULL;
  static const uint64_t MSP_NEG_ZERO = 0x8000000000000000ULL;
  static const uint64_t LSP_INF = 0;
  static const uint64_t LSP_ZERO = 0;
  static const uint64_t LSP_MAX = 0xffffffffffffffffULL;
  static const unsigned int EXPONENT_BITS = 15;
  static const unsigned int MANTISSA_BITS = 112;
  static const int EXPONENT_MAX = (1 << (EXPONENT_BITS - 1)) - 1;
  static const int EXPONENT_BIAS = EXPONENT_MAX;
  static const int EXPONENT_MIN = -EXPONENT_MAX + 1;
  static const int EXPONENT_MIN_DENORM = EXPONENT_MIN
					- static_cast<int>(MANTISSA_BITS);
  int is_inf() const;
  int is_nan() const;
  int is_zero() const;
  int sign_bit() const;
  void negate();
  void dec();
  void inc();
  void set_max(bool negative);
  void build(bool negative, mpz_t mantissa, int exponent);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
class Float {
public:
  static const bool fpu_related = false;
};

#ifdef CXX_FLOAT_BINARY_FORMAT
#define USEABLE_FLOAT 1
template <>
class Float<float> {
public:
  typedef CXX_FLOAT_BINARY_FORMAT Binary;
  union {
    float number;
    Binary binary;
  } u;
  Float();
  Float(float v);
  float value();
  static const bool fpu_related = true;
};
#else
#define USEABLE_FLOAT 0
#endif

#ifdef CXX_DOUBLE_BINARY_FORMAT
#define USEABLE_DOUBLE 1
template <>
class Float<double> {
public:
  typedef CXX_DOUBLE_BINARY_FORMAT Binary;
  union {
    double number;
    Binary binary;
  } u;
  Float();
  Float(double v);
  double value();
  static const bool fpu_related = true;
};
#else
#define USEABLE_DOUBLE 0
#endif

#ifdef CXX_LONG_DOUBLE_BINARY_FORMAT
#define USEABLE_LONG_DOUBLE 1
template <>
class Float<long double> {
public:
  typedef CXX_LONG_DOUBLE_BINARY_FORMAT Binary;
  union {
    long double number;
    Binary binary;
  } u;
  Float();
  Float(long double v);
  long double value();
  static const bool fpu_related = true;
};
#else
#define USEABLE_LONG_DOUBLE 0
#endif

} // namespace Parma_Polyhedra_Library

#include "Float.inlines.hh"

#endif // !defined(PPL_Float_defs_hh)
