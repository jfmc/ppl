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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_checked_int_inlines_hh
#define PPL_checked_int_inlines_hh 1

// Please do not remove the space separating `#' from `include':
// this ensures that the directive will not be moved during the
// procedure that automatically creates the library's include file
// (see `Makefile.am' in the `src' directory).
# include <climits>

#include "Limits.hh"
#include "float.types.hh"
#include <stdint.h>
#include <cerrno>
#include <cstdlib>
#include <gmpxx.h>

#if !HAVE_DECL_STRTOLL
long long int
strtoll(const char* nptr, char** endptr, int base);
#endif

#if !HAVE_DECL_STRTOULL
unsigned long long int
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
    - (Limits<Type>::min >= 0 ? 2 : 1) * Policy::store_infinity
    - Policy::store_nan;
}

template <typename Policy, typename Type>
inline Result
set_neg_overflow_int(Type& to, const Rounding& mode) {
  switch (mode.direction()) {
  case Rounding::UP:
    to = min_int<Policy, Type>();
    return V_LT;
  default:
    if (Policy::store_infinity)
      to = minus_infinity_int<Policy, Type>();
    return V_NEG_OVERFLOW;
  }
}

template <typename Policy, typename Type>
inline Result
set_pos_overflow_int(Type& to, const Rounding& mode) {
  switch (mode.direction()) {
  case Rounding::DOWN:
    to = max_int<Policy, Type>();
    return V_GT;
  default:
    if (Policy::store_infinity)
      to = plus_infinity_int<Policy, Type>();
    return V_POS_OVERFLOW;
  }
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
SPECIALIZE_CLASSIFY(int, short)
SPECIALIZE_CLASSIFY(int, int)
SPECIALIZE_CLASSIFY(int, long)
SPECIALIZE_CLASSIFY(int, long long)
SPECIALIZE_CLASSIFY(int, unsigned char)
SPECIALIZE_CLASSIFY(int, unsigned short)
SPECIALIZE_CLASSIFY(int, unsigned int)
SPECIALIZE_CLASSIFY(int, unsigned long)
SPECIALIZE_CLASSIFY(int, unsigned long long)

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
SPECIALIZE_SET_SPECIAL(int, short)
SPECIALIZE_SET_SPECIAL(int, int)
SPECIALIZE_SET_SPECIAL(int, long)
SPECIALIZE_SET_SPECIAL(int, long long)
SPECIALIZE_SET_SPECIAL(int, unsigned char)
SPECIALIZE_SET_SPECIAL(int, unsigned short)
SPECIALIZE_SET_SPECIAL(int, unsigned int)
SPECIALIZE_SET_SPECIAL(int, unsigned long)
SPECIALIZE_SET_SPECIAL(int, unsigned long long)

template<typename Policy, typename Type>
inline Result
pred_int(Type& to) {
  Result r = classify<Policy>(to, true, true, false);
  assert(r != VC_NAN);
  assert(r != VC_MINUS_INFINITY);
  if (r == VC_PLUS_INFINITY)
    to = max_int<Policy, Type>();
  else if (to == min_int<Policy, Type>())
    return set_special<Policy>(to, VC_MINUS_INFINITY);
  else
    --to;
  return VC_NORMAL;
}

template<typename Policy, typename Type>
inline Result
succ_int(Type& to) {
  Result r = classify<Policy>(to, true, true, false);
  assert(r != VC_NAN);
  assert(r != VC_PLUS_INFINITY);
  if (r == VC_MINUS_INFINITY)
    to = min_int<Policy, Type>();
  else if (to == max_int<Policy, Type>())
    return set_special<Policy>(to, VC_PLUS_INFINITY);
  else 
    ++to;
  return VC_NORMAL;
}

template<typename Policy, typename To, typename From>
inline Result
assign_signed_int_signed_int(To& to, const From from, const Rounding& mode) {
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from < static_cast<From>(min_int<Policy, To>()))
      return set_neg_overflow_int<Policy>(to, mode);
    if (from > static_cast<From>(max_int<Policy, To>()))
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_signed_int_unsigned_int(To& to, const From from, const Rounding& mode) {
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from > static_cast<From>(max_int<Policy, To>()))
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_signed_int(To& to, const From from, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (from < 0)
      return set_neg_overflow_int<Policy>(to, mode);
    if (sizeof(To) < sizeof(From)
	&& from > static_cast<From>(max_int<Policy, To>()))
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_unsigned_int(To& to, const From from, const Rounding& mode) {
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from > static_cast<From>(max_int<Policy, To>()))
      return set_pos_overflow_int<Policy>(to, mode);
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
ASSIGN_SIGNED(short)
ASSIGN_SIGNED(int)
ASSIGN_SIGNED(long)
ASSIGN_SIGNED(long long)
ASSIGN_UNSIGNED(unsigned char)
ASSIGN_UNSIGNED(unsigned short)
ASSIGN_UNSIGNED(unsigned int)
ASSIGN_UNSIGNED(unsigned long)
ASSIGN_UNSIGNED(unsigned long long)

ASSIGN2_SIGNED_SIGNED(signed char, short)
ASSIGN2_SIGNED_SIGNED(signed char, int)
ASSIGN2_SIGNED_SIGNED(signed char, long)
ASSIGN2_SIGNED_SIGNED(signed char, long long)
ASSIGN2_SIGNED_SIGNED(short, int)
ASSIGN2_SIGNED_SIGNED(short, long)
ASSIGN2_SIGNED_SIGNED(short, long long)
ASSIGN2_SIGNED_SIGNED(int, long)
ASSIGN2_SIGNED_SIGNED(int, long long)
ASSIGN2_SIGNED_SIGNED(long, long long)
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
ASSIGN2_UNSIGNED_SIGNED(unsigned char, short)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, int)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, long)
ASSIGN2_UNSIGNED_SIGNED(unsigned char, long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, int)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, long)
ASSIGN2_UNSIGNED_SIGNED(unsigned short, long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned int, long)
ASSIGN2_UNSIGNED_SIGNED(unsigned int, long long)
ASSIGN2_UNSIGNED_SIGNED(unsigned long, long long)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned char)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned short)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(signed char, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(short, unsigned short)
ASSIGN2_SIGNED_UNSIGNED(short, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(short, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(short, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(int, unsigned int)
ASSIGN2_SIGNED_UNSIGNED(int, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(int, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(long, unsigned long)
ASSIGN2_SIGNED_UNSIGNED(long, unsigned long long)
ASSIGN2_SIGNED_UNSIGNED(long long, unsigned long long)

template<typename Policy, typename To, typename From>
inline Result
assign_int_float_check_min_max(To& to, const From from, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (from < min_int<Policy, To>())
      return set_neg_overflow_int<Policy>(to, mode);
    if (from > max_int<Policy, To>())
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = static_cast<To>(from);
  if (Policy::round_inexact && mode.direction() != Rounding::IGNORE) {
    Result r;
    if (from < to)
      r = V_LT;
    else if (from > to)
      r = V_GT;
    else
      return V_EQ;
    return round<Policy>(to, r, mode);
  }
  return V_LGE;
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
assign_signed_int_c_string(To& to, const c_string from, const Rounding& mode) {
  errno = 0;
  char *end;
  long v = strtol(from, &end, 0);
  if (errno == ERANGE)
    return v < 0 ? set_neg_overflow_int<Policy>(to, mode) : set_pos_overflow_int<Policy>(to, mode);
  if (errno || *end)
    return set_special<Policy>(to, V_CVT_STR_UNK);
  return assign<Policy>(to, v, mode);
}

template <typename Policy, typename To>
inline Result
assign_unsigned_int_c_string(To& to, c_string from, const Rounding& mode) {
  errno = 0;
  char *end;
  unsigned long v = strtoul(from, &end, 0);
  if ((errno && errno != ERANGE) || *end)
    return set_special<Policy>(to, V_CVT_STR_UNK);
  char c;
  do {
    c = *from++;
  } while (isspace(c));
  if (c == '-') {
    if (errno || v != 0)
      return set_neg_overflow_int<Policy>(to, mode);
  }
  else {
    if (errno == ERANGE)
      return set_pos_overflow_int<Policy>(to, mode);
  }
  return assign<Policy>(to, v, mode);
}

template <typename Policy, typename To>
inline Result
assign_long_long_c_string(To& to, c_string from, const Rounding& mode) {
  errno = 0;
  char *end;
  long long v = strtoll(from, &end, 0);
  if (errno == ERANGE)
    return v < 0 ? set_neg_overflow_int<Policy>(to, mode) : set_pos_overflow_int<Policy>(to, mode);
  if (errno || *end)
    return V_CVT_STR_UNK;
  to = v;
  return V_EQ;
}

template <typename Policy, typename To>
inline Result
assign_unsigned_long_long_c_string(To& to, c_string from, const Rounding& mode) {
  errno = 0;
  char *end;
  unsigned long long v = strtoull(from, &end, 0);
  if ((errno && errno != ERANGE) || *end)
    return V_CVT_STR_UNK;
  char c;
  do {
    c = *from++;
  } while (isspace(c));
  if (c == '-') {
    if (errno || v != 0)
      return set_neg_overflow_int<Policy>(to, mode);
  }
  else {
    if (errno == ERANGE)
      return set_pos_overflow_int<Policy>(to, mode);
  }
  return assign<Policy>(to, v, mode);
}

SPECIALIZE_ASSIGN(signed_int_c_string, signed char, c_string)
SPECIALIZE_ASSIGN(signed_int_c_string, short, c_string)
SPECIALIZE_ASSIGN(signed_int_c_string, int, c_string)
SPECIALIZE_ASSIGN(signed_int_c_string, long, c_string)
SPECIALIZE_ASSIGN(long_long_c_string, long long, c_string)

SPECIALIZE_ASSIGN(unsigned_int_c_string, unsigned char, c_string)
SPECIALIZE_ASSIGN(unsigned_int_c_string, unsigned short, c_string)
SPECIALIZE_ASSIGN(unsigned_int_c_string, unsigned int, c_string)
SPECIALIZE_ASSIGN(unsigned_int_c_string, unsigned long, c_string)
SPECIALIZE_ASSIGN(unsigned_long_long_c_string, unsigned long long, c_string)

template <typename Policy, typename To>
inline Result
assign_signed_int_mpz(To& to, const mpz_class& from, const Rounding& mode) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(long))
      to = from.get_si();
    else {
      To v;
      mpz_export(&v, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
      if (::sgn(from) < 0)
	return neg<Policy>(to, v, mode);
      to = v;
    }
    return V_EQ;
  }
  if (sizeof(To) <= sizeof(long)) {
    if (from.fits_slong_p()) {
      long v = from.get_si();
      if (v < Limits<To>::min)
	return set_neg_overflow_int<Policy>(to, mode);
      if (v > Limits<To>::max)
	return set_pos_overflow_int<Policy>(to, mode);
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
	  return neg<Policy>(to, v, mode);
	to = v;
	return V_EQ;
      }
    }
  }
  return ::sgn(from) < 0 ? set_neg_overflow_int<Policy>(to, mode) : set_pos_overflow_int<Policy>(to, mode);
}

SPECIALIZE_ASSIGN(signed_int_mpz, signed char, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, short, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, int, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, long, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, long long, mpz_class)

template <typename Policy, typename To>
inline Result
assign_unsigned_int_mpz(To& to, const mpz_class& from, const Rounding& mode) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(unsigned long))
      to = from.get_ui();
    else
      mpz_export(&to, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
    return V_EQ;
  }
  if (::sgn(from) < 0)
    return set_neg_overflow_int<Policy>(to, mode);
  if (sizeof(To) <= sizeof(unsigned long)) {
    if (from.fits_ulong_p()) {
      unsigned long v = from.get_ui();
      if (v > Limits<To>::max)
	return set_pos_overflow_int<Policy>(to, mode);
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
  return set_pos_overflow_int<Policy>(to, mode);
}

SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned char, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned short, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned int, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long long, mpz_class)

template <typename Policy, typename To>
inline Result
assign_int_mpq(To& to, const mpq_class& from, const Rounding& mode) {
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  mpz_t q;
  mpz_t rem;
  mpz_init(q);
  bool use_round = (Policy::round_inexact && mode.direction() != Rounding::IGNORE);
  if (use_round) {
    mpz_init(rem);
    mpz_tdiv_qr(q, rem, n, d);
  }
  else {
    mpz_divexact(q, n, d);
  }
  Result r = assign<Policy>(to, q, mode);
  if (r == V_EQ) {
    if (use_round) {
      switch (mpz_sgn(rem)) {
      case -1:
	r = V_LT;
	break;
      case 1:
	r = V_GT;
	break;
      default:
	return V_EQ;
      }
      return round<Policy>(to, r, mode);
    }
    return V_LGE;
  }
  return r;
}

SPECIALIZE_ASSIGN(int_mpq, signed char, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, short, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, int, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, long long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned char, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned short, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned int, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned long, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, unsigned long long, mpq_class)

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

#if ULONG_MAX == 0xffffffffL
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


template<typename T>
struct Larger;

// The following may be tuned for performance on specific architecture.
//
// Current guidelines:
//   - avoid division where possibile (larger type variant for mul)
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
struct Larger<short> {
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
struct Larger<int> {
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
struct Larger<long> {
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
struct Larger<long long> {
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
neg_int_larger(Type& to, const Type x, const Rounding& mode) {
  typename Larger<Type>::Type_For_Neg l = x;
  l = -l;
  return assign<Policy>(to, l, mode);
}

template <typename Policy, typename Type>
inline Result
add_int_larger(Type& to, const Type x, const Type y, const Rounding& mode) {
  typename Larger<Type>::Type_For_Add l = x;
  l += y;
  return assign<Policy>(to, l, mode);
}

template <typename Policy, typename Type>
inline Result
sub_int_larger(Type& to, const Type x, const Type y, const Rounding& mode) {
  typename Larger<Type>::Type_For_Sub l = x;
  l -= y;
  return assign<Policy>(to, l, mode);
}

template <typename Policy, typename Type>
inline Result
mul_int_larger(Type& to, const Type x, const Type y, const Rounding& mode) {
  typename Larger<Type>::Type_For_Mul l = x;
  l *= y;
  return assign<Policy>(to, l, mode);
}

template <typename Policy, typename Type>
inline Result
neg_signed_int(Type& to, const Type from, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_neg)
      return neg_int_larger<Policy>(to, from, mode);
    if (from < -max_int<Policy, Type>())
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = -from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
neg_unsigned_int(Type& to, const Type from, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_neg)
      return neg_int_larger<Policy>(to, from, mode);
    if (from != 0)
      return set_neg_overflow_int<Policy>(to, mode);
  }
  to = from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
add_signed_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_add)
      return add_int_larger<Policy>(to, x, y, mode);
    if (y >= 0) {
      if (x > max_int<Policy, Type>() - y)
	return set_pos_overflow_int<Policy>(to, mode);
    }
    else if (x < min_int<Policy, Type>() - y)
	return set_neg_overflow_int<Policy>(to, mode);
  }
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
add_unsigned_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_add)
      return add_int_larger<Policy>(to, x, y, mode);
    if (x > max_int<Policy, Type>() - y)
      return set_pos_overflow_int<Policy>(to, mode);
  }
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sub_signed_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_sub)
      return sub_int_larger<Policy>(to, x, y, mode);
    if (y >= 0) {
      if (x < min_int<Policy, Type>() + y)
	return set_neg_overflow_int<Policy>(to, mode);
    }
    else if (x > max_int<Policy, Type>() + y)
	return set_pos_overflow_int<Policy>(to, mode);
  }
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
sub_unsigned_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_sub)
      return sub_int_larger<Policy>(to, x, y, mode);
    if (x < min_int<Policy, Type>() + y)
      return set_neg_overflow_int<Policy>(to, mode);
  }
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
mul_signed_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y, mode);
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (y == -1)
    return neg_signed_int<Policy>(to, x, mode);
  if (x >= 0) {
    if (y > 0) {
      if (x > max_int<Policy, Type>() / y)
	return set_pos_overflow_int<Policy>(to, mode);
    }
    else {
      if (x > min_int<Policy, Type>() / y)
	return set_neg_overflow_int<Policy>(to, mode);
    }
  }
  else {
    if (y < 0) {
      if (x < max_int<Policy, Type>() / y)
	return set_pos_overflow_int<Policy>(to, mode);
    }
    else {
      if (x < min_int<Policy, Type>() / y)
	return set_neg_overflow_int<Policy>(to, mode);
    }
  }
  to = x * y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
mul_unsigned_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y, mode);
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (x > max_int<Policy, Type>() / y)
    return set_pos_overflow_int<Policy>(to, mode);
  to = x * y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
div_signed_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_divbyzero && y == 0)
    return set_special<Policy>(to, V_DIV_ZERO);
  if (Policy::check_overflow && y == -1)
    return neg_signed_int<Policy>(to, x, mode);
  to = x / y;
  if (Policy::round_inexact && mode.direction() != Rounding::IGNORE) {
    Type m = x % y;
    Result r;
    if (m < 0)
      r = V_LT;
    else if (m > 0)
      r = V_GT;
    else
      return V_EQ;
    return round<Policy>(to, r, mode);
  }
  return V_LGE;
}

template <typename Policy, typename Type>
inline Result
div_unsigned_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  if (Policy::check_divbyzero && y == 0)
    return set_special<Policy>(to, V_DIV_ZERO);
  to = x / y;
  if (Policy::round_inexact && mode.direction() != Rounding::IGNORE) {
    Type m = x % y;
    if (m == 0)
      return V_EQ;
    return round<Policy>(to, V_GT, mode);
  }
  return V_GE;
}

template <typename Policy, typename Type>
inline Result
rem_int(Type& to, const Type x, const Type y, const Rounding&) {
  if (Policy::check_divbyzero && y == 0)
    return set_special<Policy>(to, V_MOD_ZERO);
  to = x % y;
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
sqrt_unsigned_int(Type& to, const Type from, const Rounding& mode) {
  Type rem;
  isqrtrem_(to, rem, from);
  if (Policy::round_inexact && mode.direction() != Rounding::IGNORE) {
    if (rem == 0)
      return V_EQ;
    return round<Policy>(to, V_GT, mode);
  }
  return V_GE;
}

template <typename Policy, typename Type>
inline Result
sqrt_signed_int(Type& to, const Type from, const Rounding& mode) {
  if (Policy::check_sqrt_neg && from < 0)
    return set_special<Policy>(to, V_SQRT_NEG);
  return sqrt_unsigned_int<Policy>(to, from, mode);
}

template <typename Policy, typename Type>
inline Result
add_mul_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  Type z;
  Result r = mul<Policy>(z, x, y, mode);
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
    return add<Policy>(to, to, z, mode);
  }
}

template <typename Policy, typename Type>
inline Result
sub_mul_int(Type& to, const Type x, const Type y, const Rounding& mode) {
  Type z;
  Result r = mul<Policy>(z, x, y, mode);
  switch (r) {
  case V_NEG_OVERFLOW:
  case V_LT:
    if (to >= 0)
      return set_pos_overflow_int<Policy>(to, mode);
    return V_UNKNOWN_NEG_OVERFLOW;
  case V_POS_OVERFLOW:
  case V_GT:
    if (to <= 0)
      return set_neg_overflow_int<Policy>(to, mode);
    return V_UNKNOWN_POS_OVERFLOW;
  default:
    return sub<Policy>(to, to, z, mode);
  }
}

template <typename Policy, typename Type>
inline Result
print_char(std::ostream& os, const Type x, const Numeric_Format& format, const Rounding& mode) {
  used(format);
  used(mode);
  os << (int) x;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
input_char(std::istream& is, Type& x, const Rounding& mode) {
  used(mode);
  int i;
  is >> i;
  return assign<Policy>(x, i, mode);
}

SPECIALIZE_PRED(int, signed char)
SPECIALIZE_PRED(int, short)
SPECIALIZE_PRED(int, int)
SPECIALIZE_PRED(int, long)
SPECIALIZE_PRED(int, long long)
SPECIALIZE_PRED(int, unsigned char)
SPECIALIZE_PRED(int, unsigned short)
SPECIALIZE_PRED(int, unsigned int)
SPECIALIZE_PRED(int, unsigned long)
SPECIALIZE_PRED(int, unsigned long long)

SPECIALIZE_SUCC(int, signed char)
SPECIALIZE_SUCC(int, short)
SPECIALIZE_SUCC(int, int)
SPECIALIZE_SUCC(int, long)
SPECIALIZE_SUCC(int, long long)
SPECIALIZE_SUCC(int, unsigned char)
SPECIALIZE_SUCC(int, unsigned short)
SPECIALIZE_SUCC(int, unsigned int)
SPECIALIZE_SUCC(int, unsigned long)
SPECIALIZE_SUCC(int, unsigned long long)

SPECIALIZE_NEG(signed_int, signed char, signed char)
SPECIALIZE_NEG(signed_int, short, short)
SPECIALIZE_NEG(signed_int, int, int)
SPECIALIZE_NEG(signed_int, long, long)
SPECIALIZE_NEG(signed_int, long long, long long)
SPECIALIZE_NEG(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_NEG(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_NEG(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_NEG(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_NEG(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_ADD(signed_int, signed char, signed char)
SPECIALIZE_ADD(signed_int, short, short)
SPECIALIZE_ADD(signed_int, int, int)
SPECIALIZE_ADD(signed_int, long, long)
SPECIALIZE_ADD(signed_int, long long, long long)
SPECIALIZE_ADD(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_ADD(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_ADD(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_ADD(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_ADD(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_SUB(signed_int, signed char, signed char)
SPECIALIZE_SUB(signed_int, short, short)
SPECIALIZE_SUB(signed_int, int, int)
SPECIALIZE_SUB(signed_int, long, long)
SPECIALIZE_SUB(signed_int, long long, long long)
SPECIALIZE_SUB(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_SUB(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_SUB(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_SUB(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_SUB(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_MUL(signed_int, signed char, signed char)
SPECIALIZE_MUL(signed_int, short, short)
SPECIALIZE_MUL(signed_int, int, int)
SPECIALIZE_MUL(signed_int, long, long)
SPECIALIZE_MUL(signed_int, long long, long long)
SPECIALIZE_MUL(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_MUL(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_MUL(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_MUL(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_MUL(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_DIV(signed_int, signed char, signed char)
SPECIALIZE_DIV(signed_int, short, short)
SPECIALIZE_DIV(signed_int, int, int)
SPECIALIZE_DIV(signed_int, long, long)
SPECIALIZE_DIV(signed_int, long long, long long)
SPECIALIZE_DIV(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_DIV(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_DIV(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_DIV(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_DIV(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_MOD(int, signed char, signed char)
SPECIALIZE_MOD(int, short, short)
SPECIALIZE_MOD(int, int, int)
SPECIALIZE_MOD(int, long, long)
SPECIALIZE_MOD(int, long long, long long)
SPECIALIZE_MOD(int, unsigned char, unsigned char)
SPECIALIZE_MOD(int, unsigned short, unsigned short)
SPECIALIZE_MOD(int, unsigned int, unsigned int)
SPECIALIZE_MOD(int, unsigned long, unsigned long)
SPECIALIZE_MOD(int, unsigned long long, unsigned long long)

SPECIALIZE_SQRT(signed_int, signed char, signed char)
SPECIALIZE_SQRT(signed_int, short, short)
SPECIALIZE_SQRT(signed_int, int, int)
SPECIALIZE_SQRT(signed_int, long, long)
SPECIALIZE_SQRT(signed_int, long long, long long)
SPECIALIZE_SQRT(unsigned_int, unsigned char, unsigned char)
SPECIALIZE_SQRT(unsigned_int, unsigned short, unsigned short)
SPECIALIZE_SQRT(unsigned_int, unsigned int, unsigned int)
SPECIALIZE_SQRT(unsigned_int, unsigned long, unsigned long)
SPECIALIZE_SQRT(unsigned_int, unsigned long long, unsigned long long)

SPECIALIZE_ABS(generic, signed char, signed char)
SPECIALIZE_ABS(generic, short, short)
SPECIALIZE_ABS(generic, int, int)
SPECIALIZE_ABS(generic, long, long)
SPECIALIZE_ABS(generic, long long, long long)
SPECIALIZE_ABS(generic, unsigned char, unsigned char)
SPECIALIZE_ABS(generic, unsigned short, unsigned short)
SPECIALIZE_ABS(generic, unsigned int, unsigned int)
SPECIALIZE_ABS(generic, unsigned long, unsigned long)
SPECIALIZE_ABS(generic, unsigned long long, unsigned long long)

SPECIALIZE_GCD(generic, signed char, signed char)
SPECIALIZE_GCD(generic, short, short)
SPECIALIZE_GCD(generic, int, int)
SPECIALIZE_GCD(generic, long, long)
SPECIALIZE_GCD(generic, long long, long long)
SPECIALIZE_GCD(generic, unsigned char, unsigned char)
SPECIALIZE_GCD(generic, unsigned short, unsigned short)
SPECIALIZE_GCD(generic, unsigned int, unsigned int)
SPECIALIZE_GCD(generic, unsigned long, unsigned long)
SPECIALIZE_GCD(generic, unsigned long long, unsigned long long)

SPECIALIZE_LCM(generic, signed char, signed char)
SPECIALIZE_LCM(generic, short, short)
SPECIALIZE_LCM(generic, int, int)
SPECIALIZE_LCM(generic, long, long)
SPECIALIZE_LCM(generic, long long, long long)
SPECIALIZE_LCM(generic, unsigned char, unsigned char)
SPECIALIZE_LCM(generic, unsigned short, unsigned short)
SPECIALIZE_LCM(generic, unsigned int, unsigned int)
SPECIALIZE_LCM(generic, unsigned long, unsigned long)
SPECIALIZE_LCM(generic, unsigned long long, unsigned long long)

SPECIALIZE_SGN(generic, signed char)
SPECIALIZE_SGN(generic, short)
SPECIALIZE_SGN(generic, int)
SPECIALIZE_SGN(generic, long)
SPECIALIZE_SGN(generic, long long)
SPECIALIZE_SGN(generic, unsigned char)
SPECIALIZE_SGN(generic, unsigned short)
SPECIALIZE_SGN(generic, unsigned int)
SPECIALIZE_SGN(generic, unsigned long)
SPECIALIZE_SGN(generic, unsigned long long)

SPECIALIZE_CMP(generic, signed char, signed char)
SPECIALIZE_CMP(generic, short, short)
SPECIALIZE_CMP(generic, int, int)
SPECIALIZE_CMP(generic, long, long)
SPECIALIZE_CMP(generic, long long, long long)
SPECIALIZE_CMP(generic, unsigned char, unsigned char)
SPECIALIZE_CMP(generic, unsigned short, unsigned short)
SPECIALIZE_CMP(generic, unsigned int, unsigned int)
SPECIALIZE_CMP(generic, unsigned long, unsigned long)
SPECIALIZE_CMP(generic, unsigned long long, unsigned long long)

SPECIALIZE_ADD_MUL(int, signed char, signed char)
SPECIALIZE_ADD_MUL(int, short, short)
SPECIALIZE_ADD_MUL(int, int, int)
SPECIALIZE_ADD_MUL(int, long, long)
SPECIALIZE_ADD_MUL(int, long long, long long)
SPECIALIZE_ADD_MUL(int, unsigned char, unsigned char)
SPECIALIZE_ADD_MUL(int, unsigned short, unsigned short)
SPECIALIZE_ADD_MUL(int, unsigned int, unsigned int)
SPECIALIZE_ADD_MUL(int, unsigned long, unsigned long)
SPECIALIZE_ADD_MUL(int, unsigned long long, unsigned long long)

SPECIALIZE_SUB_MUL(int, signed char, signed char)
SPECIALIZE_SUB_MUL(int, short, short)
SPECIALIZE_SUB_MUL(int, int, int)
SPECIALIZE_SUB_MUL(int, long, long)
SPECIALIZE_SUB_MUL(int, long long, long long)
SPECIALIZE_SUB_MUL(int, unsigned char, unsigned char)
SPECIALIZE_SUB_MUL(int, unsigned short, unsigned short)
SPECIALIZE_SUB_MUL(int, unsigned int, unsigned int)
SPECIALIZE_SUB_MUL(int, unsigned long, unsigned long)
SPECIALIZE_SUB_MUL(int, unsigned long long, unsigned long long)

SPECIALIZE_PRINT(char, signed char)
SPECIALIZE_PRINT(generic, short)
SPECIALIZE_PRINT(generic, int)
SPECIALIZE_PRINT(generic, long)
SPECIALIZE_PRINT(generic, long long)
SPECIALIZE_PRINT(char, unsigned char)
SPECIALIZE_PRINT(generic, unsigned short)
SPECIALIZE_PRINT(generic, unsigned int)
SPECIALIZE_PRINT(generic, unsigned long)
SPECIALIZE_PRINT(generic, unsigned long long)

SPECIALIZE_INPUT(char, signed char)
SPECIALIZE_INPUT(generic, short)
SPECIALIZE_INPUT(generic, int)
SPECIALIZE_INPUT(generic, long)
SPECIALIZE_INPUT(generic, long long)
SPECIALIZE_INPUT(char, unsigned char)
SPECIALIZE_INPUT(generic, unsigned short)
SPECIALIZE_INPUT(generic, unsigned int)
SPECIALIZE_INPUT(generic, unsigned long)
SPECIALIZE_INPUT(generic, unsigned long long)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
