/* Specialized "checked" functions for native integer numbers.
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

#ifndef PPL_checked_int_inlines_hh
#define PPL_checked_int_inlines_hh 1

// Please do not remove the space separating `#' from `include':
// this ensures that the directive will not be moved during the
// procedure that automatically creates the library's include file
// (see `Makefile.am' in the `src' directory).
# include <limits.h>

#include <stdint.h>
#include <cerrno>
#include "Limits.hh"
#include "float.types.hh"

#define CHECKED_INT_POS_OVERFLOW(Type) \
(Limits<Type>::max)

#define CHECKED_INT_NEG_OVERFLOW(Type) \
(Limits<Type>::min >= 0 ? Limits<Type>::max - 1 : Limits<Type>::min)

#define CHECKED_INT_UNKNOWN(Type) \
(Limits<Type>::min >= 0 ? Limits<Type>::max - 2 : Limits<Type>::min + 1)

#define CHECKED_INT_MIN(Type, Policy) \
(Limits<Type>::min \
 + (Limits<Type>::min >= 0 \
    ? 0 \
    : (Policy::store_overflows + Policy::store_unknown)))

#define CHECKED_INT_MAX(Type, Policy) \
(Limits<Type>::max \
 - (Limits<Type>::min >= 0 ? 2 : 1) \
 * Policy::store_overflows - Policy::store_unknown)


namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename Type>
inline Result
value_type_signed_int(const Type v) {
  if (Policy::store_unknown && v == CHECKED_INT_UNKNOWN(Type))
    return V_UNKNOWN;
  if (Policy::store_overflows) {
    if (v == CHECKED_INT_NEG_OVERFLOW(Type))
      return V_NEG_OVERFLOW;
    if (v == CHECKED_INT_POS_OVERFLOW(Type))
      return V_POS_OVERFLOW;
  }
  return V_EQ;
}

SPECIALIZE_VALUE_TYPE(signed_int, signed char)
SPECIALIZE_VALUE_TYPE(signed_int, short)
SPECIALIZE_VALUE_TYPE(signed_int, int)
SPECIALIZE_VALUE_TYPE(signed_int, long)
SPECIALIZE_VALUE_TYPE(signed_int, long long)

template <typename Policy, typename Type>
inline void
set_special_signed_int(Type& v, const Result r) {
  if (Policy::store_unknown && (r == V_UNKNOWN || r == V_DOMAIN)) {
    v = CHECKED_INT_UNKNOWN(Type);
    return;
  }
  if (Policy::store_overflows) {
    if (r == V_NEG_OVERFLOW) {
      v = CHECKED_INT_NEG_OVERFLOW(Type);
      return;
    }
    if (r == V_POS_OVERFLOW) {
      v = CHECKED_INT_POS_OVERFLOW(Type);
      return;
    }
  }
}

SPECIALIZE_SET_SPECIAL(signed_int, signed char)
SPECIALIZE_SET_SPECIAL(signed_int, short)
SPECIALIZE_SET_SPECIAL(signed_int, int)
SPECIALIZE_SET_SPECIAL(signed_int, long)
SPECIALIZE_SET_SPECIAL(signed_int, long long)

template <typename Policy, typename Type>
inline Result
value_type_unsigned_int(const Type v) {
  if (Policy::store_unknown && v == CHECKED_INT_UNKNOWN(Type))
    return V_UNKNOWN;
  if (Policy::store_overflows) {
    if (v == CHECKED_INT_NEG_OVERFLOW(Type))
      return V_NEG_OVERFLOW;
    if (v == CHECKED_INT_POS_OVERFLOW(Type))
      return V_POS_OVERFLOW;
  }
  return V_EQ;
}

SPECIALIZE_VALUE_TYPE(unsigned_int, unsigned char)
SPECIALIZE_VALUE_TYPE(unsigned_int, unsigned short)
SPECIALIZE_VALUE_TYPE(unsigned_int, unsigned int)
SPECIALIZE_VALUE_TYPE(unsigned_int, unsigned long)
SPECIALIZE_VALUE_TYPE(unsigned_int, unsigned long long)

template <typename Policy, typename Type>
inline void
set_special_unsigned_int(Type& v, const Result r) {
  if (Policy::store_unknown && (r == V_UNKNOWN || r == V_DOMAIN)) {
      v = CHECKED_INT_UNKNOWN(Type);
      return;
  }
  if (Policy::store_overflows) {
    if (r == V_NEG_OVERFLOW) {
      v = CHECKED_INT_NEG_OVERFLOW(Type);
      return;
    }
    if (r == V_POS_OVERFLOW) {
      v = CHECKED_INT_POS_OVERFLOW(Type);
      return;
    }
  }
}

SPECIALIZE_SET_SPECIAL(unsigned_int, unsigned char)
SPECIALIZE_SET_SPECIAL(unsigned_int, unsigned short)
SPECIALIZE_SET_SPECIAL(unsigned_int, unsigned int)
SPECIALIZE_SET_SPECIAL(unsigned_int, unsigned long)
SPECIALIZE_SET_SPECIAL(unsigned_int, unsigned long long)

template<typename Policy, typename Type>
inline Result 
pred_int(Type& to) {
  Result r = value_type<Policy>(to);
  if (r == V_UNKNOWN || r == V_NEG_OVERFLOW)
    return r;
  if (r == V_POS_OVERFLOW) {
    to = CHECKED_INT_MAX(Type, Policy);
    return V_EQ;
  }
  if (Policy::check_overflow && to == CHECKED_INT_MIN(Type, Policy)) {
    r = V_NEG_OVERFLOW;
    to = CHECKED_INT_NEG_OVERFLOW(Type);
    return r;
  }
  --to;
  return V_EQ;
}

template<typename Policy, typename Type>
inline Result 
succ_int(Type& to) {
  Result r = value_type<Policy>(to);
  if (r == V_UNKNOWN || r == V_POS_OVERFLOW)
    return r;
  if (r == V_NEG_OVERFLOW) {
    to = CHECKED_INT_MIN(Type, Policy);
    return V_EQ;
  }
  if (Policy::check_overflow && to == CHECKED_INT_MAX(Type, Policy)) {
    r = V_POS_OVERFLOW;
    to = CHECKED_INT_POS_OVERFLOW(Type);
    return r;
  }
  ++to;
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_signed_int_signed_int(To& to, const From from) {
  Result r;
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from < static_cast<From>(CHECKED_INT_MIN(To, Policy))) {
      r = V_NEG_OVERFLOW;
      goto bad;
    }
    if (from > static_cast<From>(CHECKED_INT_MAX(To, Policy))) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = To(from);
  r = V_EQ;
 bad:
  return r;
}

template<typename Policy, typename To, typename From>
inline Result
assign_signed_int_unsigned_int(To& to, const From from) {
  Result r;
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from > static_cast<From>(CHECKED_INT_MAX(To, Policy))) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = To(from);
  r = V_EQ;
 bad:
  return r;
}

template<typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_signed_int(To& to, const From from) {
  Result r;
  if (Policy::check_overflow) {
    if (from < 0) {
      r = V_NEG_OVERFLOW;
      goto bad;
    }
    if (sizeof(To) < sizeof(From)
	&& from > static_cast<From>(CHECKED_INT_MAX(To, Policy))) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = To(from);
  r = V_EQ;
 bad:
  return r;
}

template<typename Policy, typename To, typename From>
inline Result
assign_unsigned_int_unsigned_int(To& to, const From from) {
  Result r;
  if (Policy::check_overflow && sizeof(To) <= sizeof(From)) {
    if (from > static_cast<From>(CHECKED_INT_MAX(To, Policy))) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = To(from);
  r = V_EQ;
 bad:
  return r;
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
assign_int_float_check_min_max(To& to, const From from) {
  Result r;
  if (Policy::check_overflow) {
    if (from < CHECKED_INT_MIN(To, Policy)) {
      r = V_NEG_OVERFLOW;
      goto bad;
    }
    if (from > CHECKED_INT_MAX(To, Policy)) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = static_cast<To>(from);
  r = V_EQ;
  if (Policy::check_inexact) {
    if (from < to)
      r = V_LT;
    else if (from > to)
      r = V_GT;
  }
 bad:
  return r;
}

SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int8_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int16_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int32_t, float32_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int64_t, float32_t)

SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int8_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int16_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int32_t, float64_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int64_t, float64_t)

#ifdef FLOAT96_TYPE
SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int8_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int16_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int32_t, float96_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int64_t, float96_t)
#endif

#ifdef FLOAT128_TYPE
SPECIALIZE_ASSIGN(int_float_check_min_max, int8_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int16_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int32_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, int64_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int8_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int16_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int32_t, float128_t)
SPECIALIZE_ASSIGN(int_float_check_min_max, u_int64_t, float128_t)
#endif

#undef ASSIGN2_SIGNED_SIGNED
#undef ASSIGN2_UNSIGNED_UNSIGNED
#undef ASSIGN2_UNSIGNED_SIGNED
#undef ASSIGN2_SIGNED_UNSIGNED

template <typename Policy, typename To>
inline Result 
assign_signed_int_c_string(To& to, const c_string from) {
  errno = 0;
  char *end;
  long v = strtol(from, &end, 0);
  if (errno == ERANGE)
    return v < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (errno || *end)
    return V_DOMAIN;
  return assign<Policy>(to, v);
}

template <typename Policy, typename To>
inline Result 
assign_unsigned_int_c_string(To& to, c_string from) {
  errno = 0;
  char *end;
  unsigned long v = strtoul(from, &end, 0);
  if ((errno && errno != ERANGE) || *end)
    return V_DOMAIN;
  char c;
  do {
    c = *from++;
  } while (isspace(c));
  if (c == '-') {
    if (errno || v != 0)
      return V_NEG_OVERFLOW;
  }
  else {
    if (errno == ERANGE)
      return V_POS_OVERFLOW;
  }
  return assign<Policy>(to, v);
}

template <typename Policy, typename To>
inline Result 
assign_long_long_c_string(To& to, c_string from) {
  errno = 0;
  char *end;
  long long v = strtoll(from, &end, 0);
  if (errno == ERANGE)
    return v < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  if (errno || *end)
    return V_DOMAIN;
  to = v;
  return V_EQ;
}

template <typename Policy, typename To>
inline Result 
assign_unsigned_long_long_c_string(To& to, c_string from) {
  errno = 0;
  char *end;
  unsigned long long v = strtoull(from, &end, 0);
  if ((errno && errno != ERANGE) || *end)
    return V_DOMAIN;
  char c;
  do {
    c = *from++;
  } while (isspace(c));
  if (c == '-') {
    if (errno || v != 0)
      return V_NEG_OVERFLOW;
  }
  else {
    if (errno == ERANGE)
      return V_POS_OVERFLOW;
  }
  return assign<Policy>(to, v);
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
#elif LONG_MAX == 0xffffffffffffffffULL
#define LONG_BITS 64
#else
#error "Unexpected max for unsigned long"
#endif

#if ULONG_LONG_MAX == 0xffffffffffffffffULL
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
struct Larger<char> {
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
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
  static const bool use_for_mul = true;
  typedef int_fast64_t Type_For_Neg;
  typedef int_fast64_t Type_For_Add;
  typedef int_fast64_t Type_For_Sub;
  typedef int_fast64_t Type_For_Mul;
};

template <>
struct Larger<unsigned int> {
  static const bool use_for_neg = false;
  static const bool use_for_add = false;
  static const bool use_for_sub = false;
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
neg_int_larger(Type& to, const Type x) {
  typename Larger<Type>::Type_For_Neg l = x;
  l = -l;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
add_int_larger(Type& to, const Type x, const Type y) {
  typename Larger<Type>::Type_For_Add l = x;
  l += y;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
sub_int_larger(Type& to, const Type x, const Type y) {
  typename Larger<Type>::Type_For_Sub l = x;
  l -= y;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
mul_int_larger(Type& to, const Type x, const Type y) {
  typename Larger<Type>::Type_For_Mul l = x;
  l *= y;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
neg_signed_int(Type& to, const Type from) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_neg)
      return neg_int_larger<Policy>(to, from);
    if (from < -CHECKED_INT_MAX(Type, Policy)) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = -from;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
neg_unsigned_int(Type& to, const Type from) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_neg)
      return neg_int_larger<Policy>(to, from);
    if (from != 0) {
      r = V_NEG_OVERFLOW;
      goto bad;
    }
  }
  to = from;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
add_signed_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_add)
      return add_int_larger<Policy>(to, x, y);
    if (y >= 0) {
      if (x > CHECKED_INT_MAX(Type, Policy) - y) {
	r = V_POS_OVERFLOW;
	goto bad;
      }
    }
    else if (x < CHECKED_INT_MIN(Type, Policy) - y) {
	r = V_NEG_OVERFLOW;
	goto bad;
    }
  }
  to = x + y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
add_unsigned_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_add)
      return add_int_larger<Policy>(to, x, y);
    if (x > CHECKED_INT_MAX(Type, Policy) - y) {
      r = V_POS_OVERFLOW;
      goto bad;
    }
  }
  to = x + y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
sub_signed_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_sub)
      return sub_int_larger<Policy>(to, x, y);
    if (y >= 0) {
      if (x < CHECKED_INT_MIN(Type, Policy) + y) {
	r = V_NEG_OVERFLOW;
	goto bad;
      }
    }
    else if (x > CHECKED_INT_MAX(Type, Policy) + y) {
	r = V_POS_OVERFLOW;
	goto bad;
    }
  }
  to = x - y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
sub_unsigned_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_overflow) {
    if (Larger<Type>::use_for_sub)
      return sub_int_larger<Policy>(to, x, y);
    if (x < CHECKED_INT_MIN(Type, Policy) + y) {
      r = V_NEG_OVERFLOW;
      goto bad;
    }
  }
  to = x - y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
mul_signed_int(Type& to, const Type x, const Type y) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y);
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (y == -1)
    return neg_signed_int<Policy>(to, x);
  Result r;
  if (x >= 0) {
    if (y > 0) {
      if (x > CHECKED_INT_MAX(Type, Policy) / y) {
	r = V_POS_OVERFLOW;
	goto bad;
      }
    }
    else {
      if (x > CHECKED_INT_MIN(Type, Policy) / y) {
	r = V_NEG_OVERFLOW;
	goto bad;
      }
    }
  }
  else {
    if (y < 0) {
      if (x < CHECKED_INT_MAX(Type, Policy) / y) {
	r = V_POS_OVERFLOW;
	goto bad;
      }
    }
    else {
      if (x < CHECKED_INT_MIN(Type, Policy) / y) {
	r = V_NEG_OVERFLOW;
	goto bad;
      }
    }
  }
  to = x * y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
mul_unsigned_int(Type& to, const Type x, const Type y) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (Larger<Type>::use_for_mul)
    return mul_int_larger<Policy>(to, x, y);
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  Result r;
  if (x > CHECKED_INT_MAX(Type, Policy) / y) {
    r = V_POS_OVERFLOW;
    goto bad;
  }
  to = x * y;
  r = V_EQ;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
div_signed_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_divbyzero && y == 0) {
    r = V_UNKNOWN;
    goto bad;
  }
  if (Policy::check_overflow && y == -1)
    return neg_signed_int<Policy>(to, x);
  r = V_EQ;
  if (Policy::check_inexact) {
    Type m = x % y;
    if (m < 0)
      r = V_LT;
    if (m > 0)
      r = V_GT;
  }
  to = x / y;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
div_unsigned_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_divbyzero && y == 0) {
    r = V_UNKNOWN;
    goto bad;
  }
  r = V_EQ;
  if (Policy::check_inexact) {
    Type m = x % y;
    if (m > 0)
      r = V_GT;
  }
  to = x / y;
 bad:
  return r;
}

template <typename Policy, typename Type>
inline Result 
mod_int(Type& to, const Type x, const Type y) {
  Result r;
  if (Policy::check_divbyzero && y == 0) {
    r = V_UNKNOWN;
    goto bad;
  }
  to = x % y;
  r = V_EQ;
 bad:
  return r;
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
sqrt_unsigned_int(Type& to, const Type from) {
  Type rem;
  isqrtrem_(to, rem, from);
  Result r = V_EQ;
  if (Policy::check_inexact && rem > 0)
    r = V_GT;
  return r;
}

template <typename Policy, typename Type>
inline Result 
sqrt_signed_int(Type& to, const Type from) {
  Result r;
  if (Policy::check_sqrt_neg && from < 0) {
    r = V_DOMAIN;
    goto bad;
  }
  r = sqrt_unsigned_int<Policy>(to, from);
 bad:
  return r;
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

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
