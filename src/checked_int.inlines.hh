/* Specialized checked functions for native integers
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

#include <limits>
#include "float.types.hh"

namespace Parma_Polyhedra_Library {

namespace Checked {

template<typename Policy, typename Type>
inline Result 
pred_int(Type& to) {
  if (Policy::check_overflow && to == std::numeric_limits<Type>::min())
    return V_NEG_OVERFLOW;
  --to;
  return V_EQ;
}

template<typename Policy, typename Type>
inline Result 
succ_int(Type& to) {
  if (Policy::check_overflow && to == std::numeric_limits<Type>::max())
    return V_POS_OVERFLOW;
  ++to;
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_int_int(To& to, const From from) {
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_int_int_check_min(To& to, const From from) {
  if (Policy::check_overflow && from < static_cast<From>(std::numeric_limits<To>::min()))
    return V_NEG_OVERFLOW;
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_int_int_check_max(To& to, const From from) {
  if (Policy::check_overflow && from > static_cast<From>(std::numeric_limits<To>::max()))
    return V_POS_OVERFLOW;
  to = To(from);
  return V_EQ;
}

template<typename Policy, typename To, typename From>
inline Result
assign_int_int_check_min_max(To& to, const From from) {
  if (Policy::check_overflow) {
    if (from < static_cast<From>(std::numeric_limits<To>::min()))
      return V_NEG_OVERFLOW;
    if (from > static_cast<From>(std::numeric_limits<To>::max()))
      return V_POS_OVERFLOW;
  }
  to = To(from);
  return V_EQ;
}


#define ASSIGN2_SIGNED_SIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(int_int, Larger, Smaller) \
SPECIALIZE_ASSIGN(int_int_check_min_max, Smaller, Larger)

#define ASSIGN2_UNSIGNED_UNSIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(int_int, Larger, Smaller) \
SPECIALIZE_ASSIGN(int_int_check_max, Smaller, Larger)

#define ASSIGN2_UNSIGNED_SIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(int_int, Larger, Smaller) \
SPECIALIZE_ASSIGN(int_int_check_min_max, Smaller, Larger)

#define ASSIGN2_SIGNED_UNSIGNED(Smaller, Larger) \
SPECIALIZE_ASSIGN(int_int_check_min, Larger, Smaller) \
SPECIALIZE_ASSIGN(int_int_check_max, Smaller, Larger)


ASSIGN2_SIGNED_SIGNED(int8_t, int16_t)
ASSIGN2_SIGNED_SIGNED(int8_t, int32_t)
ASSIGN2_SIGNED_SIGNED(int8_t, int64_t)
ASSIGN2_SIGNED_SIGNED(int16_t, int32_t)
ASSIGN2_SIGNED_SIGNED(int16_t, int64_t)
ASSIGN2_SIGNED_SIGNED(int32_t, int64_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int8_t, u_int16_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int8_t, u_int32_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int8_t, u_int64_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int16_t, u_int32_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int16_t, u_int64_t)
ASSIGN2_UNSIGNED_UNSIGNED(u_int32_t, u_int64_t)
ASSIGN2_UNSIGNED_SIGNED(u_int8_t, int16_t)
ASSIGN2_UNSIGNED_SIGNED(u_int8_t, int32_t)
ASSIGN2_UNSIGNED_SIGNED(u_int8_t, int64_t)
ASSIGN2_UNSIGNED_SIGNED(u_int16_t, int32_t)
ASSIGN2_UNSIGNED_SIGNED(u_int16_t, int64_t)
ASSIGN2_UNSIGNED_SIGNED(u_int32_t, int64_t)
ASSIGN2_SIGNED_UNSIGNED(int8_t, u_int8_t)
ASSIGN2_SIGNED_UNSIGNED(int8_t, u_int16_t)
ASSIGN2_SIGNED_UNSIGNED(int8_t, u_int32_t)
ASSIGN2_SIGNED_UNSIGNED(int8_t, u_int64_t)
ASSIGN2_SIGNED_UNSIGNED(int16_t, u_int16_t)
ASSIGN2_SIGNED_UNSIGNED(int16_t, u_int32_t)
ASSIGN2_SIGNED_UNSIGNED(int16_t, u_int64_t)
ASSIGN2_SIGNED_UNSIGNED(int32_t, u_int32_t)
ASSIGN2_SIGNED_UNSIGNED(int32_t, u_int64_t)
ASSIGN2_SIGNED_UNSIGNED(int64_t, u_int64_t)

template<typename Policy, typename To, typename From>
inline Result
assign_int_float_check_min_max(To& to, const From from) {
  if (Policy::check_overflow) {
    if (from < std::numeric_limits<To>::min())
      return V_NEG_OVERFLOW;
    if (from > std::numeric_limits<To>::max())
      return V_POS_OVERFLOW;
  }
  to = static_cast<To>(from);
  if (Policy::check_inexact) {
    if (from < to)
      return V_LT;
    if (from > to)
      return V_GT;
  }
  return V_EQ;
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

template <typename Policy, typename Type>
inline Result 
neg_signed_int(Type& to, const Type from) {
  if (Policy::check_overflow && from == std::numeric_limits<Type>::min())
    return V_POS_OVERFLOW;
  to = -from;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
neg_unsigned_int(Type& to, const Type from) {
  if (!Policy::check_overflow || to == 0) {
    to = from;
    return V_EQ;
  }
  return V_NEG_OVERFLOW;
}

template <typename Policy, typename Type>
inline Result 
add_signed_int(Type& to, const Type x, const Type y) {
  if (Policy::check_overflow) {
    if (y >= 0) {
      if (x > std::numeric_limits<Type>::max() - y)
	return V_POS_OVERFLOW;
    } else if (x < std::numeric_limits<Type>::min() - y)
	return V_NEG_OVERFLOW;
  }
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
add_unsigned_int(Type& to, const Type x, const Type y) {
  if (Policy::check_overflow) {
    if (x > std::numeric_limits<Type>::max() - y)
      return V_POS_OVERFLOW;
  }
  to = x + y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
sub_signed_int(Type& to, const Type x, const Type y) {
  if (Policy::check_overflow) {
    if (y >= 0) {
      if (x < std::numeric_limits<Type>::min() + y)
	return V_NEG_OVERFLOW;
    } else if (x > std::numeric_limits<Type>::max() + y)
	return V_POS_OVERFLOW;
  }
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
sub_unsigned_int(Type& to, const Type x, const Type y) {
  if (Policy::check_overflow) {
    if (x < std::numeric_limits<Type>::min() + y)
      return V_NEG_OVERFLOW;
  }
  to = x - y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
mul_signed_int(Type& to, const Type x, const Type y) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (y == -1)
    return neg_signed_int<Policy>(to, x);
  Type n = x * y;
  if (n / y != x) {
    if ((x > 0) ^ (y > 0))
      return V_NEG_OVERFLOW;
    else
      return V_POS_OVERFLOW;
  }
  to = n;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
mul_unsigned_int(Type& to, const Type x, const Type y) {
  if (!Policy::check_overflow) {
    to = x * y;
    return V_EQ;
  }
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  Type n = x * y;
  if (n / y != x)
    return V_POS_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
div_signed_int(Type& to, const Type x, const Type y) {
  if (Policy::check_divbyzero && y == 0)
    return V_NAN;
  if (Policy::check_overflow && y == -1)
    return neg_signed_int<Policy>(to, x);
  if (Policy::check_inexact) {
    Type r = x % y;
    to = x / y;
    if (r < 0)
      return V_LT;
    if (r > 0)
      return V_GT;
  } else
    to = x / y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
div_unsigned_int(Type& to, const Type x, const Type y) {
  if (Policy::check_divbyzero && y == 0)
    return V_NAN;
  if (Policy::check_inexact) {
    Type r = x % y;
    to = x / y;
    if (r > 0)
      return V_GT;
  } else
    to = x / y;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
mod_int(Type& to, const Type x, const Type y) {
  if (Policy::check_divbyzero && y == 0)
    return V_NAN;
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
sqrt_unsigned_int(Type& to, const Type from) {
  Type r;
  isqrtrem_(to, r, from);
  if (Policy::check_inexact && r > 0)
    return V_GT;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result 
sqrt_signed_int(Type& to, const Type from) {
  if (Policy::check_sqrt_neg && from < 0)
    return V_NAN;
  return sqrt_unsigned_int<Policy>(to, from);
}

template<typename T>
struct Larger_Types;

template <>
struct Larger_Types<int8_t> {
  typedef int32_t Neg;
  typedef int32_t Add;
  typedef int32_t Sub;
  typedef int32_t Mul;
};

template <>
struct Larger_Types<u_int8_t> {
  typedef int32_t Neg;
  typedef u_int32_t Add;
  typedef int32_t Sub;
  typedef u_int32_t Mul;
};

template <>
struct Larger_Types<int16_t> {
  typedef int32_t Neg;
  typedef int32_t Add;
  typedef int32_t Sub;
  typedef int32_t Mul;
};

template <>
struct Larger_Types<u_int16_t> {
  typedef int32_t Neg;
  typedef u_int32_t Add;
  typedef int32_t Sub;
  typedef u_int32_t Mul;
};

template <>
struct Larger_Types<int32_t> {
  typedef int64_t Neg;
  typedef int64_t Add;
  typedef int64_t Sub;
  typedef int64_t Mul;
};

template <>
struct Larger_Types<u_int32_t> {
  typedef int64_t Neg;
  typedef u_int64_t Add;
  typedef int64_t Sub;
  typedef u_int64_t Mul;
};


template <typename Policy, typename Type>
inline Result 
neg_int_larger(Type& to, const Type x) {
  typename Larger_Types<Type>::Neg l = x;
  l = -l;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
add_int_larger(Type& to, const Type x, const Type y) {
  typename Larger_Types<Type>::Add l = x;
  l += y;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
sub_int_larger(Type& to, const Type x, const Type y) {
  typename Larger_Types<Type>::Sub l = x;
  l -= y;
  return assign<Policy>(to, l);
}

template <typename Policy, typename Type>
inline Result 
mul_int_larger(Type& to, const Type x, const Type y) {
  typename Larger_Types<Type>::Mul l = x;
  l *= y;
  return assign<Policy>(to, l);
}

/* 
   TODO: Optimize architecture dependant use of int_larger or
   (un)signed_int variants for neg, add, sub, mul.

   The following should be ok of ia32.

   Choosen guidelines:
   - avoid division where possibile (int_larger variant for mul)
   - use int_larger variant for types smaller than architecture bit size
*/

SPECIALIZE_PRED(int, int8_t)
SPECIALIZE_SUCC(int, int8_t)
SPECIALIZE_NEG(int_larger, int8_t, int8_t)
SPECIALIZE_ADD(int_larger, int8_t, int8_t)
SPECIALIZE_SUB(int_larger, int8_t, int8_t)
SPECIALIZE_MUL(int_larger, int8_t, int8_t)
SPECIALIZE_DIV(signed_int, int8_t, int8_t)
SPECIALIZE_MOD(int, int8_t, int8_t)
SPECIALIZE_SQRT(signed_int, int8_t, int8_t)

SPECIALIZE_PRED(int, int16_t)
SPECIALIZE_SUCC(int, int16_t)
SPECIALIZE_NEG(int_larger, int16_t, int16_t)
SPECIALIZE_ADD(int_larger, int16_t, int16_t)
SPECIALIZE_SUB(int_larger, int16_t, int16_t)
SPECIALIZE_MUL(int_larger, int16_t, int16_t)
SPECIALIZE_DIV(signed_int, int16_t, int16_t)
SPECIALIZE_MOD(int, int16_t, int16_t)
SPECIALIZE_SQRT(signed_int, int16_t, int16_t)

SPECIALIZE_PRED(int, int32_t)
SPECIALIZE_SUCC(int, int32_t)
SPECIALIZE_NEG(signed_int, int32_t, int32_t)
SPECIALIZE_ADD(signed_int, int32_t, int32_t)
SPECIALIZE_SUB(signed_int, int32_t, int32_t)
SPECIALIZE_MUL(int_larger, int32_t, int32_t)
SPECIALIZE_DIV(signed_int, int32_t, int32_t)
SPECIALIZE_MOD(int, int32_t, int32_t)
SPECIALIZE_SQRT(signed_int, int32_t, int32_t)

SPECIALIZE_PRED(int, int64_t)
SPECIALIZE_SUCC(int, int64_t)
SPECIALIZE_NEG(signed_int, int64_t, int64_t)
SPECIALIZE_ADD(signed_int, int64_t, int64_t)
SPECIALIZE_SUB(signed_int, int64_t, int64_t)
SPECIALIZE_MUL(signed_int, int64_t, int64_t)
SPECIALIZE_DIV(signed_int, int64_t, int64_t)
SPECIALIZE_MOD(int, int64_t, int64_t)
SPECIALIZE_SQRT(signed_int, int64_t, int64_t)

SPECIALIZE_PRED(int, u_int8_t)
SPECIALIZE_SUCC(int, u_int8_t)
SPECIALIZE_NEG(int_larger, u_int8_t, u_int8_t)
SPECIALIZE_ADD(int_larger, u_int8_t, u_int8_t)
SPECIALIZE_SUB(int_larger, u_int8_t, u_int8_t)
SPECIALIZE_MUL(int_larger, u_int8_t, u_int8_t)
SPECIALIZE_DIV(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_MOD(int, u_int8_t, u_int8_t)
SPECIALIZE_SQRT(unsigned_int, u_int8_t, u_int8_t)

SPECIALIZE_PRED(int, u_int16_t)
SPECIALIZE_SUCC(int, u_int16_t)
SPECIALIZE_NEG(int_larger, u_int16_t, u_int16_t)
SPECIALIZE_ADD(int_larger, u_int16_t, u_int16_t)
SPECIALIZE_SUB(int_larger, u_int16_t, u_int16_t)
SPECIALIZE_MUL(int_larger, u_int16_t, u_int16_t)
SPECIALIZE_DIV(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_MOD(int, u_int16_t, u_int16_t)
SPECIALIZE_SQRT(unsigned_int, u_int16_t, u_int16_t)

SPECIALIZE_PRED(int, u_int32_t)
SPECIALIZE_SUCC(int, u_int32_t)
SPECIALIZE_NEG(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_ADD(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_SUB(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_MUL(int_larger, u_int32_t, u_int32_t)
SPECIALIZE_DIV(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_MOD(int, u_int32_t, u_int32_t)
SPECIALIZE_SQRT(unsigned_int, u_int32_t, u_int32_t)

SPECIALIZE_PRED(int, u_int64_t)
SPECIALIZE_SUCC(int, u_int64_t)
SPECIALIZE_NEG(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_ADD(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_SUB(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_MUL(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_DIV(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_MOD(int, u_int64_t, u_int64_t)
SPECIALIZE_SQRT(unsigned_int, u_int64_t, u_int64_t)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_int_inlines_hh)
