/* Specialized "checked" functions for native integer numbers.
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

#ifndef PPL_checked_int_inlines_hh
#define PPL_checked_int_inlines_hh 1

#include "Limits.hh"
#include "float.types.hh"
#include <stdint.h>
#include <cerrno>
#include <cstdlib>
#include <climits>
#include <string>

#if !HAVE_DECL_STRTOLL
signed long long
strtoll(const char* nptr, char** endptr, int base);
#endif

#if !HAVE_DECL_STRTOULL
unsigned long long
strtoull(const char* nptr, char** endptr, int base);
#endif

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename Type>
inline Type
plus_infinity_int() {
  return Limits<Type>::max;
}

template <typename Policy, typename Type>
inline Type
minus_infinity_int() {
  return Limits<Type>::min >= 0 ? Limits<Type>::max - 1 : Limits<Type>::min;
}


template <typename Policy, typename Type>
inline Type
not_a_number_int() {
  return Limits<Type>::min >= 0 
	  ? Limits<Type>::max - Policy::store_infinity * 2 
	  : Limits<Type>::min + Policy::store_infinity;
}

template <typename Policy, typename Type>
inline Type
min_int() {
  return Limits<Type>::min
    + (Limits<Type>::min >= 0 ? 0
       : (Policy::store_infinity + Policy::store_nan));
}

template <typename Policy, typename Type>
inline Type
max_int() {
  return Limits<Type>::max
    - (Limits<Type>::min >= 0
       ? (2 * Policy::store_infinity + Policy::store_nan)
       : Policy::store_infinity);
}

template <typename Policy, typename To>
inline Result
set_neg_overflow_int(To& to, Rounding_Dir dir) {
  if (dir == ROUND_UP) {
    to = min_int<Policy, To>();
    return V_LT;
  }
  else {
    if (Policy::store_infinity) {
      to = minus_infinity_int<Policy, To>();
      return V_GT;
    }
    return V_NEG_OVERFLOW;
  }
}

template <typename Policy, typename To>
inline Result
set_pos_overflow_int(To& to, Rounding_Dir dir) {
  if (dir == ROUND_DOWN) {
    to = max_int<Policy, To>();
    return V_GT;
  }
  else {
    if (Policy::store_infinity) {
      to = plus_infinity_int<Policy, To>();
      return V_LT;
    }
    return V_POS_OVERFLOW;
  }
}

template <typename Policy, typename To>
inline Result
round_lt_int_no_overflow(To& to, Rounding_Dir dir) {
  if (dir == ROUND_DOWN) {
    to--;
    return V_GT;
  }
  return V_LT;
}

template <typename Policy, typename To>
inline Result
round_gt_int_no_overflow(To& to, Rounding_Dir dir) {
  if (dir == ROUND_UP) {
    to++;
    return V_LT;
  }
  return V_GT;
}

template <typename Policy, typename To>
inline Result
round_lt_int(To& to, Rounding_Dir dir) {
  if (dir == ROUND_DOWN) {
    if (to == min_int<Policy, To>()) {
      if (Policy::store_infinity) {
	to = minus_infinity_int<Policy, To>();
	return V_GT;
      }
      return V_NEG_OVERFLOW;
    } else {
      to--;
      return V_GT;
    }
  }
  return V_LT;
}

template <typename Policy, typename To>
inline Result
round_gt_int(To& to, Rounding_Dir dir) {
  if (dir == ROUND_UP) {
    if (to == max_int<Policy, To>()) {
      if (Policy::store_infinity) {
	to = plus_infinity_int<Policy, To>();
	return V_LT;
      }
      return V_POS_OVERFLOW;
    } else {
      to++;
      return V_LT;
    }
  }
  return V_GT;
}

template <typename Policy, typename Type>
inline Result
classify_int(const Type v, bool nan, bool inf, bool sign) {
  if (Policy::store_nan && (nan || sign) && v == not_a_number_int<Policy, Type>())
    return VC_NAN;
  if (!inf & !sign)
    return VC_NORMAL;
  if (Policy::store_infinity) {
    if (v == minus_infinity_int<Policy, Type>())
      return inf ? VC_MINUS_INFINITY : V_LT;
    if (v == plus_infinity_int<Policy, Type>())
      return inf ? VC_PLUS_INFINITY : V_GT;
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

SPECIALIZE_CLASSIFY(int, signed char)
SPECIALIZE_CLASSIFY(int, signed short)
SPECIALIZE_CLASSIFY(int, signed int)
SPECIALIZE_CLASSIFY(int, signed long)
SPECIALIZE_CLASSIFY(int, signed long long)
SPECIALIZE_CLASSIFY(int, unsigned char)
SPECIALIZE_CLASSIFY(int, unsigned short)
SPECIALIZE_CLASSIFY(int, unsigned int)
SPECIALIZE_CLASSIFY(int, unsigned long)
SPECIALIZE_CLASSIFY(int, unsigned long long)

template <typename Policy, typename Type>
inline bool
is_nan_int(const Type v) {
  return Policy::store_nan && v == not_a_number_int<Policy, Type>();
}

SPECIALIZE_IS_NAN(int, signed char)
SPECIALIZE_IS_NAN(int, signed short)
SPECIALIZE_IS_NAN(int, signed int)
SPECIALIZE_IS_NAN(int, signed long)
SPECIALIZE_IS_NAN(int, signed long long)
SPECIALIZE_IS_NAN(int, unsigned char)
SPECIALIZE_IS_NAN(int, unsigned short)
SPECIALIZE_IS_NAN(int, unsigned int)
SPECIALIZE_IS_NAN(int, unsigned long)
SPECIALIZE_IS_NAN(int, unsigned long long)

template <typename Policy, typename Type>
inline bool
is_minf_int(const Type v) {
  return Policy::store_infinity && v == minus_infinity_int<Policy, Type>();
}

SPECIALIZE_IS_MINF(int, signed char)
SPECIALIZE_IS_MINF(int, signed short)
SPECIALIZE_IS_MINF(int, signed int)
SPECIALIZE_IS_MINF(int, signed long)
SPECIALIZE_IS_MINF(int, signed long long)
SPECIALIZE_IS_MINF(int, unsigned char)
SPECIALIZE_IS_MINF(int, unsigned short)
SPECIALIZE_IS_MINF(int, unsigned int)
SPECIALIZE_IS_MINF(int, unsigned long)
SPECIALIZE_IS_MINF(int, unsigned long long)

template <typename Policy, typename Type>
inline bool
is_pinf_int(const Type v) {
  return Policy::store_infinity && v == plus_infinity_int<Policy, Type>();
}

SPECIALIZE_IS_PINF(int, signed char)
SPECIALIZE_IS_PINF(int, signed short)
SPECIALIZE_IS_PINF(int, signed int)
SPECIALIZE_IS_PINF(int, signed long)
SPECIALIZE_IS_PINF(int, signed long long)
SPECIALIZE_IS_PINF(int, unsigned char)
SPECIALIZE_IS_PINF(int, unsigned short)
SPECIALIZE_IS_PINF(int, unsigned int)
SPECIALIZE_IS_PINF(int, unsigned long)
SPECIALIZE_IS_PINF(int, unsigned long long)

template <typename Policy, typename Type>
inline bool
is_int_int(const Type v) {
  return !is_nan<Policy>(v);
}

SPECIALIZE_IS_INT(int, signed char)
SPECIALIZE_IS_INT(int, signed short)
SPECIALIZE_IS_INT(int, signed int)
SPECIALIZE_IS_INT(int, signed long)
SPECIALIZE_IS_INT(int, signed long long)
SPECIALIZE_IS_INT(int, unsigned char)
SPECIALIZE_IS_INT(int, unsigned short)
SPECIALIZE_IS_INT(int, unsigned int)
SPECIALIZE_IS_INT(int, unsigned long)
SPECIALIZE_IS_INT(int, unsigned long long)

template <typename Policy, typename Type>
inline Result
set_special_int(Type& v, Result r) {
  Result t = classify(r);
  if (Policy::store_nan && t == VC_NAN)
    v = not_a_number_int<Policy, Type>();
  else if (Policy::store_infinity) {
    switch (t) {
    case VC_MINUS_INFINITY:
      v = minus_infinity_int<Policy, Type>();
      break;
    case VC_PLUS_INFINITY:
      v = plus_infinity_int<Policy, Type>();
      break;
    default:
      break;
    }
  }
  return r;
}

SPECIALIZE_SET_SPECIAL(int, signed char)
SPECIALIZE_SET_SPECIAL(int, signed short)
SPECIALIZE_SET_SPECIAL(int, signed int)
SPECIALIZE_SET_SPECIAL(int, signed long)
SPECIALIZE_SET_SPECIAL(int, signed long long)
SPECIALIZE_SET_SPECIAL(int, unsigned char)
SPECIALIZE_SET_SPECIAL(int, unsigned short)
SPECIALIZE_SET_SPECIAL(int, unsigned int)
SPECIALIZE_SET_SPECIAL(int, unsigned long)
SPECIALIZE_SET_SPECIAL(int, unsigned long long)

template <typename Policy, typename To, typename From>
inline Result
assign_signed_int_signed_int(To& to, const From from, Rounding_Dir dir) {
  if (sizeof(To) <= sizeof(From)) {
    if (CHECK_P(Policy::check_overflow, from < static_cast<From>(min_int<Policy, To>())))
      return set_neg_overflow_int<Policy>(to, dir);
    if (CHECK_P(Policy::check_overflow, from > static_cast<From>(max_int<Policy, To>())))
      return set_pos_overflow_int<Policy>(to, dir);
  }
  to = To(from);
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_signed_int_unsigned_int(To& to, const From from, Rounding_Dir dir) {
  if (sizeof(To) <= sizeof(From)) {
    if (CHECK_P(Policy::check_overflow, from > static_cast<From>(max_int<Policy, To>())))
      return set_pos_overflow_int<Policy>(to, dir);
  }
  to = To(from);
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_signed_int(To& to, const From from, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_overflow, from < 0))
    return set_neg_overflow_int<Policy>(to, dir);
  if (sizeof(To) < sizeof(From)) {
    if (CHECK_P(Policy::check_overflow, from > static_cast<From>(max_int<Policy, To>())))
      return set_pos_overflow_int<Policy>(to, dir);
  }
  to = To(from);
  return V_EQ;
}

template <typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_unsigned_int(To& to, const From from, Rounding_Dir dir) {
  if (sizeof(To) <= sizeof(From)) {
    if (CHECK_P(Policy::check_overflow, from > static_cast<From>(max_int<Policy, To>())))
      return set_pos_overflow_int<Policy>(to, dir);
  }
  to = To(from);
  return V_EQ;
}


#define ASSIGN2_SIGNED_SIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(signed_int_signed_int, Smaller, Larger) \
SPECIALIZE_ASSIGN(signed_int_signed_int, Larger, Smaller)

#define ASSIGN2_UNSIGNED_UNSIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(unsigned_int_unsigned_int, Smaller, Larger) \
SPECIALIZE_ASSIGN(unsigned_int_unsigned_int, Larger, Smaller)

#define ASSIGN2_UNSIGNED_SIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(unsigned_int_signed_int, Smaller, Larger) \
SPECIALIZE_ASSIGN(signed_int_unsigned_int, Larger, Smaller)

#define ASSIGN2_SIGNED_UNSIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(signed_int_unsigned_int, Smaller, Larger) \
SPECIALIZE_ASSIGN(unsigned_int_signed_int, Larger, Smaller)

#define ASSIGN_SIGNED(Type) SPECIALIZE_ASSIGN(signed_int_signed_int, Type, Type)
#define ASSIGN_UNSIGNED(Type) SPECIALIZE_ASSIGN(unsigned_int_unsigned_int, Type, Type)

ASSIGN_SIGNED(signed char)
ASSIGN_SIGNED(signed short)
ASSIGN_SIGNED(signed int)
ASSIGN_SIGNED(signed long)
ASSIGN_SIGNED(signed long long)
ASSIGN_UNSIGNED(unsigned char)
ASSIGN_UNSIGNED(unsigned short)
ASSIGN_UNSIGNED(unsigned int)
ASSIGN_UNSIGNED(unsigned long)
ASSIGN_UNSIGNED(unsigned long long)

ASSIGN2_SIGNED_SIGNED(signed char, signed short)
ASSIGN2_SIGNED_SIGNED(signed char, signed int)
ASSIGN2_SIGNED_SIGNED(signed char, signed long)
ASSIGN2_SIGNED_SIGNED(signed char, signed long long)
ASSIGN2_SIGNED_SIGNED(signed short, signed int)
ASSIGN2_SIGNED_SIGNED(signed short, signed long)
ASSIGN2_SIGNED_SIGNED(signed short, signed long long)
ASSIGN2_SIGNED_SIGNED(signed int, signed long)
ASSIGN2_SIGNED_SIGNED(signed int, signed long long)
ASSIGN2_SIGNED_SIGNED(signed long, signed long long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned char, unsigned short)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned char, unsigned int)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned char, unsigned long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned char, unsigned long long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned short, unsigned int)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned short, unsigned long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned short, unsigned long long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned int, unsigned long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned int, unsigned long long)
ASSIGN2_UNSIGNED_UNSIGNED(unsigned long, unsigned long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, signed short)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, signed int)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, signed long)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, signed long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, signed int)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, signed long)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, signed long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned int, signed long)
ASSIGN2_UNSIGNED_SIGNED(unsigned int, signed long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned long, signed long long)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned char)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned short)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(signed short, unsigned short)
ASSIGN2_SIGNED_UNSIGNED(signed short, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(signed short, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(signed short, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(signed int, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(signed int, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(signed int, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(signed long, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(signed long, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(signed long long, unsigned long long)

template <typename Policy, typename To, typename From>
inline Result
assign_int_float_check_min_max(To& to, const From from, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_overflow, (from < min_int<Policy, To>())))
    return set_neg_overflow_int<Policy>(to, dir);
  if (CHECK_P(Policy::check_overflow, (from > max_int<Policy, To>())))
    return set_pos_overflow_int<Policy>(to, dir);
  to = static_cast<To>(from);
  if (dir == ROUND_IGNORE)
    return V_LGE;
  if (from < to)
    return round_lt_int<Policy>(to, dir);
  else if (from > to)
    return round_gt_int<Policy>(to, dir);
  else
    return V_EQ;
}

SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint8_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint16_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint32_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint64_t, float32_t)

SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint8_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint16_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint32_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint64_t, float64_t)

#ifdef FLOAT96_TYPE
SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint8_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint16_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint32_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint64_t, float96_t)
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint8_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint16_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint32_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, uint64_t, float128_t)
#endif

#undef ASSIGN2_SIGNED_SIGNED
#undef ASSIGN2_UNSIGNED_UNSIGNED
#undef ASSIGN2_UNSIGNED_SIGNED
#undef ASSIGN2_SIGNED_UNSIGNED

template <typename Policy, typename To>
inline Result
assign_signed_int_mpz(To& to, const mpz_class& from, Rounding_Dir dir) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(signed long))
      to = from.get_si();
    else {
      To v;
      mpz_export(&v, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
      if (::sgn(from) < 0)
	return neg<Policy>(to, v, dir);
      to = v;
    }
    return V_EQ;
  }
  if (sizeof(To) <= sizeof(signed long)) {
    if (from.fits_slong_p()) {
      signed long v = from.get_si();
      if (v < Limits<To>::min)
	return set_neg_overflow_int<Policy>(to, dir);
      if (v > Limits<To>::max)
	return set_pos_overflow_int<Policy>(to, dir);
      to = v;
      return V_EQ;
    }
  }
  else {
    mpz_srcptr m = from.get_mpz_t();
    size_t sz = mpz_size(m);
    if (sz <= sizeof(To) / sizeof(mp_limb_t)) {
      if (sz == 0) {
	to = 0;
	return V_EQ;
      }
      To v;
      mpz_export(&v, 0, 1, sizeof(To), 0, 0, m);
      if (v >= 0) {
	if (::sgn(from) < 0)
	  return neg<Policy>(to, v, dir);
	to = v;
	return V_EQ;
      }
    }
  }
  return ::sgn(from) < 0 ? set_neg_overflow_int<Policy>(to, dir) : set_pos_overflow_int<Policy>(to, dir);
}

SPECIALIZE_ASSIGN(signed_int_mpz, signed char, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, signed short, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, signed int, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, signed long, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, signed long long, mpz_class)

template <typename Policy, typename To>
inline Result
assign_unsigned_int_mpz(To& to, const mpz_class& from, Rounding_Dir dir) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(unsigned long))
      to = from.get_ui();
    else
      mpz_export(&to, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
    return V_EQ;
  }
  if (::sgn(from) < 0)
    return set_neg_overflow_int<Policy>(to, dir);
  if (sizeof(To) <= sizeof(unsigned long)) {
    if (from.fits_ulong_p()) {
      unsigned long v = from.get_ui();
      if (v > Limits<To>::max)
	return set_pos_overflow_int<Policy>(to, dir);
      to = v;
      return V_EQ;
    }
  }
  else {
    mpz_srcptr m = from.get_mpz_t();
    size_t sz = mpz_size(m);
    if (sz <= sizeof(To) / sizeof(mp_limb_t)) {
      if (sz == 0)
	to = 0;
      else
	mpz_export(&to, 0, 1, sizeof(To), 0, 0, m);
      return V_EQ;
    }
  }
  return set_pos_overflow_int<Policy>(to, dir);
}

SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned char, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned short, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned int, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long long, mpz_class)

template <typename Policy, typename To>
inline Result
assign_int_mpq(To& to, const mpq_class& from, Rounding_Dir dir) {
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  mpz_class q;
  mpz_ptr _q = q.get_mpz_t();
  if (dir == ROUND_IGNORE) {
    mpz_tdiv_q(_q, n, d);
    Result r = assign<Policy>(to, q, dir);
    if (r != V_EQ)
      return r;
    return V_LGE;
  }
  mpz_t rem;
  int sign;
  mpz_init(rem);
  mpz_tdiv_qr(_q, rem, n, d);
  sign = mpz_sgn(rem);
  mpz_clear(rem);
  Result r = assign<Policy>(to, q, dir);
  if (r != V_EQ)
    return r;
  switch (sign) {
  case -1:
    return round_lt_int<Policy>(to, dir);
  case 1:
    return round_gt_int<Policy>(to, dir);
  default:
    return V_EQ;
  }
}

SPECIALIZE_ASSIGN(int_mpq, signed char, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, signed short, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, signed int, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, signed long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, signed long long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned char, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned short, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned int, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned long long, mpq_class)

template <typename Policy, typename To>
inline Result
assign_int_minf(To& to, const Minus_Infinity&, Rounding_Dir dir) {
  if (Policy::store_infinity) {
    to = minus_infinity_int<Policy, To>();
    return V_EQ;
  }
  if (dir == ROUND_UP) {
    to = min_int<Policy, To>();
    return V_LT;
  }
  return VC_MINUS_INFINITY;
}

template <typename Policy, typename To>
inline Result
assign_int_pinf(To& to, const Plus_Infinity&, Rounding_Dir dir) {
  if (Policy::store_infinity) {
    to = plus_infinity_int<Policy, To>();
    return V_EQ;
  }
  if (dir == ROUND_DOWN) {
    to = max_int<Policy, To>();
    return V_GT;
  }
  return VC_PLUS_INFINITY;
}

template <typename Policy, typename To>
inline Result
assign_int_nan(To& to, const Not_A_Number&, Rounding_Dir) {
  if (Policy::store_nan) {
    to = not_a_number_int<Policy, To>();
    return V_EQ;
  }
  return VC_NAN;
}

SPECIALIZE_ASSIGN(int_minf, signed char, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, signed short, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, signed int, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, signed long, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, signed long long, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, unsigned char, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, unsigned short, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, unsigned int, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, unsigned long, Minus_Infinity)
SPECIALIZE_ASSIGN(int_minf, unsigned long long, Minus_Infinity)

SPECIALIZE_ASSIGN(int_pinf, signed char, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, signed short, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, signed int, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, signed long, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, signed long long, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, unsigned char, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, unsigned short, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, unsigned int, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, unsigned long, Plus_Infinity)
SPECIALIZE_ASSIGN(int_pinf, unsigned long long, Plus_Infinity)

SPECIALIZE_ASSIGN(int_nan, signed char, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, signed short, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, signed int, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, signed long, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, signed long long, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, unsigned char, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, unsigned short, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, unsigned int, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, unsigned long, Not_A_Number)
SPECIALIZE_ASSIGN(int_nan, unsigned long long, Not_A_Number)

#if UCHAR_MAX == 0xff
#define CHAR_BITS 8
#else
#error "Unexpected max for unsigned char"
#endif

#if USHRT_MAX == 0xffff
#define SHRT_BITS 16
#else
#error "Unexpected max for unsigned short"
#endif

#if UINT_MAX == 0xffffffff
#define INT_BITS 32
#else
#error "Unexpected max for unsigned int"
#endif

#if ULONG_MAX == 0xffffffffUL
#define LONG_BITS 32
#elif ULONG_MAX == 0xffffffffffffffffULL
#define LONG_BITS 64
#else
#error "Unexpected max for unsigned long"
#endif

#if ULLONG_MAX == 0xffffffffffffffffULL
#define LONG_LONG_BITS 64
#else
#error "Unexpected max for unsigned long long"
#endif


template <typename T>
struct Larger;

// The following may be tuned for performance on specific architecture.
//
// Current guidelines:
//   - avoid division where possible (larger type variant for mul)
//   - use larger type variant for types smaller than architecture bit size

template <>
struct Larger<signed char> {
  static const bool use_for_neg = true;
  static const bool use_for_add = true;
  static const bool use_for_sub = true;
  static const bool use_for_mul = true;
  typedef int_fast16_t Type_For_Neg;
  typedef int_fast16_t  Type_For_Add;
  typedef int_fast16_t  Type_For_Sub;
  typedef int_fast16_t  Type_For_Mul;
};

template <>
struct Larger<unsigned char> {
  static const bool use_for_neg = true;
  static const bool use_for_add = true;
  static const bool use_for_sub = true;
  static const bool use_for_mul = true;
  typedef int_fast16_t Type_For_Neg;
  typedef uint_fast16_t Type_For_Add;
  typedef int_fast16_t Type_For_Sub;
  typedef uint_fast16_t Type_For_Mul;
};

template <>
struct Larger<signed short> {
  static const bool use_for_neg = true;
  static const bool use_for_add = true;
  static const bool use_for_sub = true;
  static const bool use_for_mul = true;
  typedef int_fast32_t Type_For_Neg;
  typedef int_fast32_t Type_For_Add;
  typedef int_fast32_t Type_For_Sub;
  typedef int_fast32_t Type_For_Mul;
};

template <>
struct Larger<unsigned short> {
  static const bool use_for_neg = true;
  static const bool use_for_add = true;
  static const bool use_for_sub = true;
  static const bool use_for_mul = true;
  typedef int_fast32_t Type_For_Neg;
  typedef uint_fast32_t Type_For_Add;
  typedef int_fast32_t Type_For_Sub;
  typedef uint_fast32_t Type_For_Mul;
};

template <>
struct Larger<signed int> {
  static const bool use_for_neg = (LONG_BITS == 64);
  static const bool use_for_add = (LONG_BITS == 64);
  static const bool use_for_sub = (LONG_BITS == 64);
  static const bool use_for_mul = true;
  typedef int_fast64_t Type_For_Neg;
  typedef int_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef int_fast64_t Type_For_Mul;
};

template <>
struct Larger<unsigned int> {
  static const bool use_for_neg = (LONG_BITS == 64);
  static const bool use_for_add = (LONG_BITS == 64);
  static const bool use_for_sub = (LONG_BITS == 64);
  static const bool use_for_mul = true;
  typedef int_fast64_t Type_For_Neg;
  typedef uint_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef uint_fast64_t Type_For_Mul;
};

template <>
struct Larger<signed long> {
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
  static const bool use_for_mul = (LONG_BITS == 32);
  typedef int_fast64_t Type_For_Neg;
  typedef int_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef int_fast64_t Type_For_Mul;
};

template <>
struct Larger<unsigned long> {
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
  static const bool use_for_mul = (LONG_BITS == 32);
  typedef int_fast64_t Type_For_Neg;
  typedef uint_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef uint_fast64_t Type_For_Mul;
};

template <>
struct Larger<signed long long> {
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
  static const bool use_for_mul = false;
  typedef int_fast64_t Type_For_Neg;
  typedef int_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef int_fast64_t Type_For_Mul;
};

template <>
struct Larger<unsigned long long> {
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
  static const bool use_for_mul = false;
  typedef int_fast64_t Type_For_Neg;
  typedef uint_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef uint_fast64_t Type_For_Mul;
};

template <typename Policy, typename Type>
inline Result
neg_int_larger(Type& to, const Type x, Rounding_Dir dir) {
  typename Larger<Type>::Type_For_Neg l = x;
  l = -l;
  return assign<Policy>(to, l, dir);
}

template <typename Policy, typename Type>
inline Result
add_int_larger(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  typename Larger<Type>::Type_For_Add l = x;
  l += y;
  return assign<Policy>(to, l, dir);
}

template <typename Policy, typename Type>
inline Result
sub_int_larger(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  typename Larger<Type>::Type_For_Sub l = x;
  l -= y;
  return assign<Policy>(to, l, dir);
}

template <typename Policy, typename Type>
inline Result
mul_int_larger(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  typename Larger<Type>::Type_For_Mul l = x;
  l *= y;
  return assign<Policy>(to, l, dir);
}

template <typename Policy, typename Type>
inline Result
neg_signed_int(Type& to, const Type from, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_neg)
    return neg_int_larger<Policy>(to, from, dir);
  if (CHECK_P(Policy::check_overflow, (from < -max_int<Policy, Type>())))
    return set_pos_overflow_int<Policy>(to, dir);
  to = -from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
neg_unsigned_int(Type& to, const Type from, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_neg)
    return neg_int_larger<Policy>(to, from, dir);
  if (CHECK_P(Policy::check_overflow, from != 0))
    return set_neg_overflow_int<Policy>(to, dir);
  to = from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
add_signed_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_add)
    return add_int_larger<Policy>(to, x, y, dir);
  if (Policy::check_overflow) {
    if (y >= 0) {
      if (x > max_int<Policy, Type>() - y)
	return set_pos_overflow_int<Policy>(to, dir);
    }
    else if (x < min_int<Policy, Type>() - y)
	return set_neg_overflow_int<Policy>(to, dir);
  }
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
add_unsigned_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_add)
    return add_int_larger<Policy>(to, x, y, dir);
  if (CHECK_P(Policy::check_overflow, (x > max_int<Policy, Type>() - y)))
    return set_pos_overflow_int<Policy>(to, dir);
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sub_signed_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_sub)
    return sub_int_larger<Policy>(to, x, y, dir);
  if (Policy::check_overflow) {
    if (y >= 0) {
      if (x < min_int<Policy, Type>() + y)
	return set_neg_overflow_int<Policy>(to, dir);
    }
    else if (x > max_int<Policy, Type>() + y)
	return set_pos_overflow_int<Policy>(to, dir);
  }
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sub_unsigned_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_sub)
    return sub_int_larger<Policy>(to, x, y, dir);
  if (CHECK_P(Policy::check_overflow, (x < min_int<Policy, Type>() + y)))
    return set_neg_overflow_int<Policy>(to, dir);
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
mul_signed_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y, dir);
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (y == -1)
    return neg_signed_int<Policy>(to, x, dir);
  if (x >= 0) {
    if (y > 0) {
      if (x > max_int<Policy, Type>() / y)
	return set_pos_overflow_int<Policy>(to, dir);
    }
    else {
      if (x > min_int<Policy, Type>() / y)
	return set_neg_overflow_int<Policy>(to, dir);
    }
  }
  else {
    if (y < 0) {
      if (x < max_int<Policy, Type>() / y)
	return set_pos_overflow_int<Policy>(to, dir);
    }
    else {
      if (x < min_int<Policy, Type>() / y)
	return set_neg_overflow_int<Policy>(to, dir);
    }
  }
  to = x * y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
mul_unsigned_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (Policy::check_overflow && Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y, dir);
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (x > max_int<Policy, Type>() / y)
    return set_pos_overflow_int<Policy>(to, dir);
  to = x * y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
div_signed_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_div_zero, y == 0))
    return set_special<Policy>(to, V_DIV_ZERO);
  if (Policy::check_overflow && y == -1)
    return neg_signed_int<Policy>(to, x, dir);
  to = x / y;
  if (dir == ROUND_IGNORE)
    return V_LGE;
  Type m = x % y;
  if (m < 0)
    return round_lt_int_no_overflow<Policy>(to, dir);
  else if (m > 0)
    return round_gt_int_no_overflow<Policy>(to, dir);
  else
    return V_EQ;
}

template <typename Policy, typename Type>
inline Result
div_unsigned_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_div_zero, y == 0))
    return set_special<Policy>(to, V_DIV_ZERO);
  to = x / y;
  if (dir == ROUND_IGNORE)
    return V_GE;
  Type m = x % y;
  if (m == 0)
    return V_EQ;
  return round_gt_int<Policy>(to, dir);
}

template <typename Policy, typename Type>
inline Result
rem_int(Type& to, const Type x, const Type y, Rounding_Dir) {
  if (CHECK_P(Policy::check_div_zero, y == 0))
    return set_special<Policy>(to, V_MOD_ZERO);
  to = x % y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
div2exp_unsigned_int(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return mul2exp<Policy>(to, x, -exp, dir);
  if (static_cast<unsigned int>(exp) >= sizeof(Type) * 8) {
    to = 0;
    if (dir == ROUND_IGNORE)
      return V_GE;
    if (x == 0)
      return V_EQ;
    return round_gt_int_no_overflow<Policy>(to, dir);
  }
  to = x >> exp;
  if (dir == ROUND_IGNORE)
    return V_GE;
  if (x & ((static_cast<Type>(1) << exp) - 1))
    return round_gt_int_no_overflow<Policy>(to, dir);
  else
    return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
div2exp_signed_int(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return mul2exp<Policy>(to, x, -exp, dir);
  if (static_cast<unsigned int>(exp) >= sizeof(Type) * 8) {
  zero:
    to = 0;
    if (dir == ROUND_IGNORE)
      return V_LGE;
    if (x < 0)
      return round_lt_int_no_overflow<Policy>(to, dir);
    else if (x > 0) 
      return round_gt_int_no_overflow<Policy>(to, dir);
    else
      return V_EQ;
  }
  if (static_cast<unsigned int>(exp) >= sizeof(Type) * 8 - 1) {
    if (x == Limits<Type>::min) {
      to = -1;
      return V_EQ;
    }
    goto zero;
  }
#if 0
  to = x / (static_cast<Type>(1) << exp);
  if (dir == ROUND_IGNORE)
    return V_GE;
  Type r = x % (static_cast<Type>(1) << exp);
  if (r < 0)
    return round_lt_int_no_overflow<Policy>(to, dir);
  else if (r > 0)
    return round_gt_int_no_overflow<Policy>(to, dir);
  else
    return V_EQ;
#else
  // Faster but compiler implementation dependent (see C++98 5.8.3)
  to = x >> exp;
  if (dir == ROUND_IGNORE)
    return V_GE;
  if (x & ((static_cast<Type>(1) << exp) - 1))
    return round_gt_int_no_overflow<Policy>(to, dir);
  return V_EQ;
#endif
}

template <typename Policy, typename Type>
inline Result 
mul2exp_unsigned_int(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return div2exp<Policy>(to, x, -exp, dir);
  if (!Policy::check_overflow) {
    to = x << exp;
    return V_EQ;
  }
  if (static_cast<unsigned int>(exp) >= sizeof(Type) * 8) {
    if (x == 0) {
      to = 0;
      return V_EQ;
    }
    return set_pos_overflow_int<Policy>(to, dir);
  }
  if (x & (((static_cast<Type>(1) << exp) - 1) << (sizeof(Type) * 8 - exp)))
    return set_pos_overflow_int<Policy>(to, dir);
  Type n = x << exp;
  if (n > max_int<Policy, Type>())
    return set_pos_overflow_int<Policy>(to, dir);
  to = n;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
mul2exp_signed_int(Type& to, const Type x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return div2exp<Policy>(to, x, -exp, dir);
  if (!Policy::check_overflow) {
    to = x << exp;
    return V_EQ;
  }
  if (static_cast<unsigned int>(exp) >= sizeof(Type) * 8 - 1) {
    if (x < 0)
      return set_neg_overflow_int<Policy>(to, dir);
    else if (x > 0)
      return set_pos_overflow_int<Policy>(to, dir);
    else {
      to = 0;
      return V_EQ;
    }
  }
  Type mask = ((static_cast<Type>(1) << exp) - 1) << (sizeof(Type) * 8 - 1 - exp);
  Type n;
  if (x < 0) {
    if ((x & mask) != mask)
      return set_neg_overflow_int<Policy>(to, dir);
    n = x << exp;
    if (n < min_int<Policy, Type>())
      return set_neg_overflow_int<Policy>(to, dir);
  }
  else {
    if (x & mask)
      return set_pos_overflow_int<Policy>(to, dir);
    n = x << exp;
    if (n > max_int<Policy, Type>())
      return set_pos_overflow_int<Policy>(to, dir);
  }
  to = n;
  return V_EQ;
}

template <typename Type>
inline void
isqrtrem_(Type& q, Type& r, const Type from) {
  q = 0;
  r = from;
  Type t(1);
  for (t <<= 8 * sizeof(Type) - 2; t != 0; t >>= 2) {
    Type s = q + t;
    if (s <= r) {
      r -= s;
      q = s + t;
    }
    q >>= 1;
  }
}

template <typename Policy, typename Type>
inline Result
sqrt_unsigned_int(Type& to, const Type from, Rounding_Dir dir) {
  Type rem;
  isqrtrem_(to, rem, from);
  if (dir == ROUND_IGNORE)
    return V_GE;
  if (rem == 0)
    return V_EQ;
  return round_gt_int<Policy>(to, dir);
}

template <typename Policy, typename Type>
inline Result
sqrt_signed_int(Type& to, const Type from, Rounding_Dir dir) {
  if (CHECK_P(Policy::check_sqrt_neg, from < 0))
    return set_special<Policy>(to, V_SQRT_NEG);
  return sqrt_unsigned_int<Policy>(to, from, dir);
}

template <typename Policy, typename Type>
inline Result
add_mul_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  Type z;
  Result r = mul<Policy>(z, x, y, dir);
  switch (r) {
  case V_NEG_OVERFLOW:
  case V_LT:
    if (to <= 0) {
      to = z;
      return r;
    }
    return set_special<Policy>(to, V_UNKNOWN_NEG_OVERFLOW);
  case V_POS_OVERFLOW:
  case V_GT:
    if (to >= 0) {
      to = z;
      return r;
    }
    return set_special<Policy>(to, V_UNKNOWN_POS_OVERFLOW);
  default:
    return add<Policy>(to, to, z, dir);
  }
}

template <typename Policy, typename Type>
inline Result
sub_mul_int(Type& to, const Type x, const Type y, Rounding_Dir dir) {
  Type z;
  Result r = mul<Policy>(z, x, y, dir);
  switch (r) {
  case V_NEG_OVERFLOW:
  case V_LT:
    if (to >= 0)
      return set_pos_overflow_int<Policy>(to, dir);
    return V_UNKNOWN_NEG_OVERFLOW;
  case V_POS_OVERFLOW:
  case V_GT:
    if (to <= 0)
      return set_neg_overflow_int<Policy>(to, dir);
    return V_UNKNOWN_POS_OVERFLOW;
  default:
    return sub<Policy>(to, to, z, dir);
  }
}

template <typename Policy, typename Type>
inline Result
output_char(std::ostream& os, Type& from, const Numeric_Format&, Rounding_Dir) {
  os << (int) from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
output_int(std::ostream& os, Type& from, const Numeric_Format&, Rounding_Dir) {
  os << from;
  return V_EQ;
}

SPECIALIZE_NEG(signed_int, signed char, signed char)
SPECIALIZE_NEG(signed_int, signed short, signed short)
SPECIALIZE_NEG(signed_int, signed int, signed int)
SPECIALIZE_NEG(signed_int, signed long, signed long)
SPECIALIZE_NEG(signed_int, signed long long, signed long long)
SPECIALIZE_NEG(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_NEG(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_NEG(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_NEG(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_NEG(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_ADD(signed_int, signed char, signed char, signed char)
SPECIALIZE_ADD(signed_int, signed short, signed short, signed short)
SPECIALIZE_ADD(signed_int, signed int, signed int, signed int)
SPECIALIZE_ADD(signed_int, signed long, signed long, signed long)
SPECIALIZE_ADD(signed_int, signed long long, signed long long, signed long long)
SPECIALIZE_ADD(unsigned_int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_ADD(unsigned_int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_ADD(unsigned_int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_ADD(unsigned_int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_ADD(unsigned_int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_SUB(signed_int, signed char, signed char, signed char)
SPECIALIZE_SUB(signed_int, signed short, signed short, signed short)
SPECIALIZE_SUB(signed_int, signed int, signed int, signed int)
SPECIALIZE_SUB(signed_int, signed long, signed long, signed long)
SPECIALIZE_SUB(signed_int, signed long long, signed long long, signed long long)
SPECIALIZE_SUB(unsigned_int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_SUB(unsigned_int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_SUB(unsigned_int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_SUB(unsigned_int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_SUB(unsigned_int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_MUL(signed_int, signed char, signed char, signed char)
SPECIALIZE_MUL(signed_int, signed short, signed short, signed short)
SPECIALIZE_MUL(signed_int, signed int, signed int, signed int)
SPECIALIZE_MUL(signed_int, signed long, signed long, signed long)
SPECIALIZE_MUL(signed_int, signed long long, signed long long, signed long long)
SPECIALIZE_MUL(unsigned_int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_MUL(unsigned_int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_MUL(unsigned_int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_MUL(unsigned_int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_MUL(unsigned_int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_DIV(signed_int, signed char, signed char, signed char)
SPECIALIZE_DIV(signed_int, signed short, signed short, signed short)
SPECIALIZE_DIV(signed_int, signed int, signed int, signed int)
SPECIALIZE_DIV(signed_int, signed long, signed long, signed long)
SPECIALIZE_DIV(signed_int, signed long long, signed long long, signed long long)
SPECIALIZE_DIV(unsigned_int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_DIV(unsigned_int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_DIV(unsigned_int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_DIV(unsigned_int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_DIV(unsigned_int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_REM(int, signed char, signed char, signed char)
SPECIALIZE_REM(int, signed short, signed short, signed short)
SPECIALIZE_REM(int, signed int, signed int, signed int)
SPECIALIZE_REM(int, signed long, signed long, signed long)
SPECIALIZE_REM(int, signed long long, signed long long, signed long long)
SPECIALIZE_REM(int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_REM(int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_REM(int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_REM(int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_REM(int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_MUL2EXP(signed_int, signed char, signed char)
SPECIALIZE_MUL2EXP(signed_int, signed short, signed short)
SPECIALIZE_MUL2EXP(signed_int, signed int, signed int)
SPECIALIZE_MUL2EXP(signed_int, signed long, signed long)
SPECIALIZE_MUL2EXP(signed_int, signed long long, signed long long)
SPECIALIZE_MUL2EXP(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_MUL2EXP(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_MUL2EXP(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_MUL2EXP(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_MUL2EXP(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_DIV2EXP(signed_int, signed char, signed char)
SPECIALIZE_DIV2EXP(signed_int, signed short, signed short)
SPECIALIZE_DIV2EXP(signed_int, signed int, signed int)
SPECIALIZE_DIV2EXP(signed_int, signed long, signed long)
SPECIALIZE_DIV2EXP(signed_int, signed long long, signed long long)
SPECIALIZE_DIV2EXP(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_DIV2EXP(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_DIV2EXP(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_DIV2EXP(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_DIV2EXP(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_SQRT(signed_int, signed char, signed char)
SPECIALIZE_SQRT(signed_int, signed short, signed short)
SPECIALIZE_SQRT(signed_int, signed int, signed int)
SPECIALIZE_SQRT(signed_int, signed long, signed long)
SPECIALIZE_SQRT(signed_int, signed long long, signed long long)
SPECIALIZE_SQRT(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_SQRT(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_SQRT(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_SQRT(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_SQRT(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_ABS(generic, signed char, signed char)
SPECIALIZE_ABS(generic, signed short, signed short)
SPECIALIZE_ABS(generic, signed int, signed int)
SPECIALIZE_ABS(generic, signed long, signed long)
SPECIALIZE_ABS(generic, signed long long, signed long long)
SPECIALIZE_ABS(generic, unsigned char, unsigned char)
SPECIALIZE_ABS(generic, unsigned short, unsigned short)
SPECIALIZE_ABS(generic, unsigned int, unsigned int)
SPECIALIZE_ABS(generic, unsigned long, unsigned long)
SPECIALIZE_ABS(generic, unsigned long long, unsigned long long)

SPECIALIZE_GCD(exact, signed char, signed char, signed char)
SPECIALIZE_GCD(exact, signed short, signed short, signed short)
SPECIALIZE_GCD(exact, signed int, signed int, signed int)
SPECIALIZE_GCD(exact, signed long, signed long, signed long)
SPECIALIZE_GCD(exact, signed long long, signed long long, signed long long)
SPECIALIZE_GCD(exact, unsigned char, unsigned char, unsigned char)
SPECIALIZE_GCD(exact, unsigned short, unsigned short, unsigned short)
SPECIALIZE_GCD(exact, unsigned int, unsigned int, unsigned int)
SPECIALIZE_GCD(exact, unsigned long, unsigned long, unsigned long)
SPECIALIZE_GCD(exact, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_LCM(gcd_exact, signed char, signed char, signed char)
SPECIALIZE_LCM(gcd_exact, signed short, signed short, signed short)
SPECIALIZE_LCM(gcd_exact, signed int, signed int, signed int)
SPECIALIZE_LCM(gcd_exact, signed long, signed long, signed long)
SPECIALIZE_LCM(gcd_exact, signed long long, signed long long, signed long long)
SPECIALIZE_LCM(gcd_exact, unsigned char, unsigned char, unsigned char)
SPECIALIZE_LCM(gcd_exact, unsigned short, unsigned short, unsigned short)
SPECIALIZE_LCM(gcd_exact, unsigned int, unsigned int, unsigned int)
SPECIALIZE_LCM(gcd_exact, unsigned long, unsigned long, unsigned long)
SPECIALIZE_LCM(gcd_exact, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_SGN(generic, signed char)
SPECIALIZE_SGN(generic, signed short)
SPECIALIZE_SGN(generic, signed int)
SPECIALIZE_SGN(generic, signed long)
SPECIALIZE_SGN(generic, signed long long)
SPECIALIZE_SGN(generic, unsigned char)
SPECIALIZE_SGN(generic, unsigned short)
SPECIALIZE_SGN(generic, unsigned int)
SPECIALIZE_SGN(generic, unsigned long)
SPECIALIZE_SGN(generic, unsigned long long)

SPECIALIZE_CMP(generic, signed char, signed char)
SPECIALIZE_CMP(generic, signed short, signed short)
SPECIALIZE_CMP(generic, signed int, signed int)
SPECIALIZE_CMP(generic, signed long, signed long)
SPECIALIZE_CMP(generic, signed long long, signed long long)
SPECIALIZE_CMP(generic, unsigned char, unsigned char)
SPECIALIZE_CMP(generic, unsigned short, unsigned short)
SPECIALIZE_CMP(generic, unsigned int, unsigned int)
SPECIALIZE_CMP(generic, unsigned long, unsigned long)
SPECIALIZE_CMP(generic, unsigned long long, unsigned long long)

SPECIALIZE_ADD_MUL(int, signed char, signed char, signed char)
SPECIALIZE_ADD_MUL(int, signed short, signed short, signed short)
SPECIALIZE_ADD_MUL(int, signed int, signed int, signed int)
SPECIALIZE_ADD_MUL(int, signed long, signed long, signed long)
SPECIALIZE_ADD_MUL(int, signed long long, signed long long, signed long long)
SPECIALIZE_ADD_MUL(int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_ADD_MUL(int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_ADD_MUL(int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_ADD_MUL(int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_ADD_MUL(int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_SUB_MUL(int, signed char, signed char, signed char)
SPECIALIZE_SUB_MUL(int, signed short, signed short, signed short)
SPECIALIZE_SUB_MUL(int, signed int, signed int, signed int)
SPECIALIZE_SUB_MUL(int, signed long, signed long, signed long)
SPECIALIZE_SUB_MUL(int, signed long long, signed long long, signed long long)
SPECIALIZE_SUB_MUL(int, unsigned char, unsigned char, unsigned char)
SPECIALIZE_SUB_MUL(int, unsigned short, unsigned short, unsigned short)
SPECIALIZE_SUB_MUL(int, unsigned int, unsigned int, unsigned int)
SPECIALIZE_SUB_MUL(int, unsigned long, unsigned long, unsigned long)
SPECIALIZE_SUB_MUL(int, unsigned long long, unsigned long long, unsigned long long)

SPECIALIZE_INPUT(generic, signed char)
SPECIALIZE_INPUT(generic, signed short)
SPECIALIZE_INPUT(generic, signed int)
SPECIALIZE_INPUT(generic, signed long)
SPECIALIZE_INPUT(generic, signed long long)
SPECIALIZE_INPUT(generic, unsigned char)
SPECIALIZE_INPUT(generic, unsigned short)
SPECIALIZE_INPUT(generic, unsigned int)
SPECIALIZE_INPUT(generic, unsigned long)
SPECIALIZE_INPUT(generic, unsigned long long)

SPECIALIZE_OUTPUT(char, signed char)
SPECIALIZE_OUTPUT(int, signed short)
SPECIALIZE_OUTPUT(int, signed int)
SPECIALIZE_OUTPUT(int, signed long)
SPECIALIZE_OUTPUT(int, signed long long)
SPECIALIZE_OUTPUT(char, unsigned char)
SPECIALIZE_OUTPUT(int, unsigned short)
SPECIALIZE_OUTPUT(int, unsigned int)
SPECIALIZE_OUTPUT(int, unsigned long)
SPECIALIZE_OUTPUT(int, unsigned long long)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
