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

#include <limits>
#include "float.types.hh"

namespace Parma_Polyhedra_Library {

template<typename Type>
inline Result_Info 
checked_pred_int(Type& to) {
  if (to == std::numeric_limits<Type>::min())
    return V_NEG_OVERFLOW;
  --to;
  return V_EQ;
}

template<typename Type>
inline Result_Info 
checked_succ_int(Type& to) {
  if (to == std::numeric_limits<Type>::max())
    return V_POS_OVERFLOW;
  ++to;
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_int_int(To& to, From from) {
  to = To(from);
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_int_int_check_min(To& to, From from) {
  if (from < static_cast<From>(std::numeric_limits<To>::min()))
    return V_NEG_OVERFLOW;
  to = To(from);
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_int_int_check_max(To& to, From from) {
  if (from > static_cast<From>(std::numeric_limits<To>::max()))
    return V_POS_OVERFLOW;
  to = To(from);
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_int_int_check_min_max(To& to, From from) {
  if (from < static_cast<From>(std::numeric_limits<To>::min()))
    return V_NEG_OVERFLOW;
  if (from > static_cast<From>(std::numeric_limits<To>::max()))
    return V_POS_OVERFLOW;
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

template<typename To, typename From>
inline Result_Info
checked_assign_int_float_check_min_max(To& to, From from) {
  if (from < std::numeric_limits<To>::min())
    return V_NEG_OVERFLOW;
  if (from > std::numeric_limits<To>::max())
    return V_POS_OVERFLOW;
  to = static_cast<To>(from);
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_int_float_check_min_max_check_inexact(To& to, From from) {
  Result_Info r = checked_assign_int_float_check_min_max(to, from);
  if (r != V_EQ)
    return r;
  if (from < to)
    return V_LT;
  if (from > to)
    return V_GT;
  return V_EQ;
}

SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int8_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int16_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int32_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int64_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int8_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int16_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int32_t, float32_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int64_t, float32_iec559_t)

SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int8_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int16_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int32_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int64_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int8_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int16_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int32_t, float64_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int64_t, float64_iec559_t)

#ifdef FLOAT96_IEC559_TYPE
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int8_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int16_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int32_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int64_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int8_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int16_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int32_t, float96_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int64_t, float96_iec559_t)
#endif

#ifdef FLOAT128_IEC559_TYPE
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int8_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int16_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int32_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, int64_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int8_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int16_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int32_t, float128_iec559_t)
SPECIALIZE_ASSIGN_INEXACT(int_float_check_min_max, u_int64_t, float128_iec559_t)
#endif

#undef ASSIGN2_SIGNED_SIGNED
#undef ASSIGN2_UNSIGNED_UNSIGNED
#undef ASSIGN2_UNSIGNED_SIGNED
#undef ASSIGN2_SIGNED_UNSIGNED

template <typename Type>
inline Result_Info 
checked_neg_signed_int(Type& to, Type from) {
  if (from == 0) {
    to = from;
    return V_EQ;
  }
  Type n = -from;
  if (to < 0) {
    if (n <= 0)
      return V_POS_OVERFLOW;
  } else if (n >= 0)
    return V_NEG_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_neg_unsigned_int(Type& to, Type from) {
  if (to == 0) {
    to = from;
    return V_EQ;
  }
  return V_NEG_OVERFLOW;
}

template <typename Type>
inline Result_Info 
checked_add_signed_int(Type& to, Type x, Type y) {
  Type n = x + y;
  if (y < 0) {
    if (n >= x)
      return V_NEG_OVERFLOW;
  } else if (n < x)
    return V_POS_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_add_unsigned_int(Type& to, Type x, Type y) {
  Type n = x + y;
  if (n < x)
    return V_POS_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_sub_signed_int(Type& to, Type x, Type y) {
  Type n = x - y;
  if (y < 0) {
    if (n <= x)
      return V_POS_OVERFLOW;
  } else if (n > x)
    return V_NEG_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_sub_unsigned_int(Type& to, Type x, Type y) {
  Type n = x - y;
  if (n > x)
    return V_NEG_OVERFLOW;
  to = n;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_mul_signed_int(Type& to, Type x, Type y) {
  if (y == 0) {
    to = 0;
    return V_EQ;
  }
  if (y == -1)
    return checked_neg_signed_int(to, x);
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

template <typename Type>
inline Result_Info 
checked_mul_unsigned_int(Type& to, Type x, Type y) {
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

template <typename Type>
inline Result_Info 
checked_div_signed_int(Type& to, Type x, Type y) {
  if (y == -1)
    return checked_neg_signed_int(to, x);
  to = x / y;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_div_signed_int_check_inexact(Type& to, Type x, Type y) {
  if (y == 0)
    return V_NAN;
  if (y == -1)
    return checked_neg_signed_int(to, x);
  Type r = x % y;
  to = x / y;
  if (r < 0)
    return V_LT;
  if (r > 0)
    return V_GT;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_div_unsigned_int(Type& to, Type x, Type y) {
  to = x / y;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_div_unsigned_int_check_inexact(Type& to, Type x, Type y) {
  if (y == 0)
    return V_NAN;
  Type r = x % y;
  to = x / y;
  if (r > 0)
    return V_GT;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_mod_int(Type& to, Type x, Type y) {
  to = x % y;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_mod_int_check_inexact(Type& to, Type x, Type y) {
  if (y == 0)
    return V_NAN;
  to = x % y;
  return V_EQ;
}

template <typename Type>
inline void 
isqrtrem_(Type& q, Type& r, Type from) {
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

template <typename Type>
inline Result_Info 
checked_sqrt_unsigned_int(Type& to, Type from) {
  Type r;
  isqrtrem_(to, r, from);
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_sqrt_signed_int(Type& to, Type from) {
  return checked_sqrt_unsigned_int(to, from);
}

template <typename Type>
inline Result_Info 
checked_sqrt_unsigned_int_check_inexact(Type& to, Type from) {
  Type r;
  isqrtrem_(to, r, from);
  if (r > 0)
    return V_GT;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_sqrt_signed_int_check_inexact(Type& to, Type from) {
  if (from < 0)
    return V_NAN;
  return checked_sqrt_unsigned_int_check_inexact(to, from);
}

template<typename T>
struct Larger_Types;

template <>
struct Larger_Types<int8_t> {
  typedef int16_t Neg;
  typedef int16_t Add;
  typedef int16_t Sub;
  typedef int16_t Mul;
};

template <>
struct Larger_Types<u_int8_t> {
  typedef int16_t Neg;
  typedef u_int16_t Add;
  typedef int16_t Sub;
  typedef u_int16_t Mul;
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


template <typename Type>
inline Result_Info 
checked_int_larger_neg(Type& to, Type x) {
  typename Larger_Types<Type>::Neg l = x;
  l = -l;
  return checked_assign(to, l);
}

template <typename Type>
inline Result_Info 
checked_int_larger_add(Type& to, Type x, Type y) {
  typename Larger_Types<Type>::Add l = x;
  l += y;
  return checked_assign(to, l);
}

template <typename Type>
inline Result_Info 
checked_int_larger_sub(Type& to, Type x, Type y) {
  typename Larger_Types<Type>::Sub l = x;
  l -= y;
  return checked_assign(to, l);
}

template <typename Type>
inline Result_Info 
checked_int_larger_mul(Type& to, Type x, Type y) {
  typename Larger_Types<Type>::Mul l = x;
  l *= y;
  return checked_assign(to, l);
}

SPECIALIZE_PRED(int, int8_t)
SPECIALIZE_SUCC(int, int8_t)
SPECIALIZE_NEG(signed_int, int8_t, int8_t)
SPECIALIZE_ADD(signed_int, int8_t, int8_t)
SPECIALIZE_SUB(signed_int, int8_t, int8_t)
SPECIALIZE_MUL(signed_int, int8_t, int8_t)
SPECIALIZE_DIV_INEXACT(signed_int, int8_t, int8_t)
SPECIALIZE_MOD_INEXACT(int, int8_t, int8_t)
SPECIALIZE_SQRT_INEXACT(signed_int, int8_t, int8_t)

SPECIALIZE_PRED(int, int16_t)
SPECIALIZE_SUCC(int, int16_t)
SPECIALIZE_NEG(signed_int, int16_t, int16_t)
SPECIALIZE_ADD(signed_int, int16_t, int16_t)
SPECIALIZE_SUB(signed_int, int16_t, int16_t)
SPECIALIZE_MUL(signed_int, int16_t, int16_t)
SPECIALIZE_DIV_INEXACT(signed_int, int16_t, int16_t)
SPECIALIZE_MOD_INEXACT(int, int16_t, int16_t)
SPECIALIZE_SQRT_INEXACT(signed_int, int16_t, int16_t)

SPECIALIZE_PRED(int, int32_t)
SPECIALIZE_SUCC(int, int32_t)
SPECIALIZE_NEG(signed_int, int32_t, int32_t)
SPECIALIZE_ADD(signed_int, int32_t, int32_t)
SPECIALIZE_SUB(signed_int, int32_t, int32_t)
SPECIALIZE_MUL(signed_int, int32_t, int32_t)
SPECIALIZE_DIV_INEXACT(signed_int, int32_t, int32_t)
SPECIALIZE_MOD_INEXACT(int, int32_t, int32_t)
SPECIALIZE_SQRT_INEXACT(signed_int, int32_t, int32_t)

SPECIALIZE_PRED(int, int64_t)
SPECIALIZE_SUCC(int, int64_t)
SPECIALIZE_NEG(signed_int, int64_t, int64_t)
SPECIALIZE_ADD(signed_int, int64_t, int64_t)
SPECIALIZE_SUB(signed_int, int64_t, int64_t)
SPECIALIZE_MUL(signed_int, int64_t, int64_t)
SPECIALIZE_DIV_INEXACT(signed_int, int64_t, int64_t)
SPECIALIZE_MOD_INEXACT(int, int64_t, int64_t)
SPECIALIZE_SQRT_INEXACT(signed_int, int64_t, int64_t)

SPECIALIZE_PRED(int, u_int8_t)
SPECIALIZE_SUCC(int, u_int8_t)
SPECIALIZE_NEG(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_ADD(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_SUB(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_MUL(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_DIV_INEXACT(unsigned_int, u_int8_t, u_int8_t)
SPECIALIZE_MOD_INEXACT(int, u_int8_t, u_int8_t)
SPECIALIZE_SQRT_INEXACT(unsigned_int, u_int8_t, u_int8_t)

SPECIALIZE_PRED(int, u_int16_t)
SPECIALIZE_SUCC(int, u_int16_t)
SPECIALIZE_NEG(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_ADD(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_SUB(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_MUL(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_DIV_INEXACT(unsigned_int, u_int16_t, u_int16_t)
SPECIALIZE_MOD_INEXACT(int, u_int16_t, u_int16_t)
SPECIALIZE_SQRT_INEXACT(unsigned_int, u_int16_t, u_int16_t)

SPECIALIZE_PRED(int, u_int32_t)
SPECIALIZE_SUCC(int, u_int32_t)
SPECIALIZE_NEG(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_ADD(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_SUB(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_MUL(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_DIV_INEXACT(unsigned_int, u_int32_t, u_int32_t)
SPECIALIZE_MOD_INEXACT(int, u_int32_t, u_int32_t)
SPECIALIZE_SQRT_INEXACT(unsigned_int, u_int32_t, u_int32_t)

SPECIALIZE_PRED(int, u_int64_t)
SPECIALIZE_SUCC(int, u_int64_t)
SPECIALIZE_NEG(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_ADD(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_SUB(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_MUL(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_DIV_INEXACT(unsigned_int, u_int64_t, u_int64_t)
SPECIALIZE_MOD_INEXACT(int, u_int64_t, u_int64_t)
SPECIALIZE_SQRT_INEXACT(unsigned_int, u_int64_t, u_int64_t)

} // namespace Parma_Polyhedra_Library

