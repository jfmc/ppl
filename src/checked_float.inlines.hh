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

#include <cmath>

#define USE_FPU_ROUNDING_MODE
#define USE_FPU_INEXACT

namespace Parma_Polyhedra_Library {

#ifdef USE_FPU_ROUNDING_MODE
static int
fpu_rounding_mode = fegetround();
#endif

inline void
fpu_set_rounding_mode(int mode) {
#ifdef USE_FPU_ROUNDING_MODE
  if (mode != fpu_rounding_mode) {
    fesetround(mode);
    fpu_rounding_mode = mode;
  }
#endif
}

inline int
fpu_get_rounding_mode() {
#ifdef USE_FPU_ROUNDING_MODE
  return fpu_rounding_mode;
#else
  return -1;
#endif
}

inline void
fpu_reset_inexact() {
#ifdef USE_FPU_INEXACT
  feclearexcept(FE_ALL_EXCEPT);
#endif
}

/* 0 no, 1 yes, -1 maybe */
inline int
fpu_is_inexact() {
#ifdef USE_FPU_INEXACT
  return fetestexcept(FE_INEXACT) != 0;
#else
  return -1;
#endif
}

template <typename Type>
inline Result_Info 
check_inexact(Type v) {
  int r;
  switch (fpu_is_inexact()) {
  case 0:
    return V_EQ;
  case 1:
    r = 0;
    break;
  default:
    r = V_EQ;
    break;
  }
  switch (fpu_get_rounding_mode()) {
  case -1:
#ifdef FE_TONEAREST
  case FE_TONEAREST:
#endif
    r |= V_GT | V_LT;
#ifdef FE_DOWNWARD
  case FE_DOWNWARD:
    r |= V_GT;
#endif
#ifdef FE_UPWARD
  case FE_UPWARD:
    r |= V_LT;
#endif
#ifdef FE_TOWARDZERO
  case FE_TOWARDZERO:
    if (v <= 0)
      r |= V_LT;
    if (v >= 0)
      r |= V_GT;
  }
#endif
  return static_cast<Result_Info>(r);
}

template <typename From, typename To>
inline Result_Info 
checked_assign_float_float_check_normal(To& to, From from) {
  if (isnan(from))
    return V_NAN;
  int i = isinf(from);
  if (i < 0)
    return V_NEG_OVERFLOW;
  if (i > 0)
    return V_POS_OVERFLOW;
  to = from;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_assign_float_inexact(Type& to, Type from) {
  Result_Info r = checked_assign_float_float_check_normal(to, from);
  return r != V_EQ ? r : check_inexact(from);
}

template <typename Type>
inline Result_Info 
checked_neg_float(Type& to, Type from) {
  to = -from;
  return V_EQ;
}

template <typename Type>
inline Result_Info 
checked_add_float(Type& to, Type x, Type y) {
  return checked_assign_float_float_check_normal(to, x + y);
}

template <typename Type>
inline Result_Info 
checked_sub_float(Type& to, Type x, Type y) {
  return checked_assign_float_float_check_normal(to, x - y);
}

template <typename Type>
inline Result_Info 
checked_mul_float(Type& to, Type x, Type y) {
  return checked_assign_float_float_check_normal(to, x * y);
}

template <typename Type>
inline Result_Info 
checked_div_float(Type& to, Type x, Type y) {
  return checked_assign_float_float_check_normal(to, x / y);
}

template <typename Type>
inline Result_Info 
checked_mod_float(Type& to, Type x, Type y) {
  return checked_assign_float_float_check_normal(to, std::fmod(x, y));
}

template <typename Type>
inline Result_Info 
checked_add_float_check_inexact(Type& to, Type x, Type y) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, x + y);
}

template <typename Type>
inline Result_Info 
checked_sub_float_check_inexact(Type& to, Type x, Type y) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, x - y);
}

template <typename Type>
inline Result_Info 
checked_mul_float_check_inexact(Type& to, Type x, Type y) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, x * y);
}

template <typename Type>
inline Result_Info 
checked_div_float_check_inexact(Type& to, Type x, Type y) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, x / y);
}

template <typename Type>
inline Result_Info 
checked_mod_float_check_inexact(Type& to, Type x, Type y) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, std::fmod(x, y));
}

template <typename Type>
inline Result_Info
checked_abs_float(Type& to, Type from)
{
  to = std::abs(from);
  return V_EQ;
}

template <typename Type>
inline Result_Info
checked_sqrt_float(Type& to, Type from)
{
  return checked_assign_float_float_check_normal(to, std::sqrt(from));
}

template <typename Type>
inline Result_Info
checked_sqrt_float_check_inexact(Type& to, Type from)
{
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, std::sqrt(from));
}

template <typename To, typename From>
inline Result_Info 
checked_assign_float_int(To& to, From from) {
  to = from;
  return V_EQ;
}

template<typename To, typename From>
inline Result_Info
checked_assign_float_float_check_normal_check_inexact(To& to, From from) {
  fpu_reset_inexact();
  return checked_assign_float_inexact(to, To(from));
}

template<typename To, typename From>
inline Result_Info
checked_assign_float_int_check_inexact(To& to, From from) {
  fpu_reset_inexact();
  to = from;
  return check_inexact(to);
}

#define ASSIGN_R2(Smaller, Larger) \
SPECIALIZE_ASSIGN(float_float_check_normal, Larger, Smaller) \
SPECIALIZE_ASSIGN_INEXACT(float_float_check_normal, Smaller, Larger)

SPECIALIZE_ASSIGN(float_int, float32_iec559_t, int8_t)
SPECIALIZE_ASSIGN(float_int, float32_iec559_t, int16_t)
SPECIALIZE_ASSIGN(float_int, float32_iec559_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int, float32_iec559_t, u_int16_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float32_iec559_t, int32_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float32_iec559_t, int64_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float32_iec559_t, u_int32_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float32_iec559_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float_check_normal, float32_iec559_t, float32_iec559_t)
ASSIGN_R2(float32_iec559_t, float64_iec559_t)

SPECIALIZE_NEG(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_ABS(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_ADD_INEXACT(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_SUB_INEXACT(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_MUL_INEXACT(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_DIV_INEXACT(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_MOD_INEXACT(float, float32_iec559_t, float32_iec559_t)
SPECIALIZE_SQRT_INEXACT(float, float32_iec559_t, float32_iec559_t)

SPECIALIZE_ASSIGN(float_int, float64_iec559_t, int8_t)
SPECIALIZE_ASSIGN(float_int, float64_iec559_t, int16_t)
SPECIALIZE_ASSIGN(float_int, float64_iec559_t, int32_t)
SPECIALIZE_ASSIGN(float_int, float64_iec559_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int, float64_iec559_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int, float64_iec559_t, u_int32_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float64_iec559_t, int64_t)
SPECIALIZE_ASSIGN_INEXACT(float_int, float64_iec559_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float_check_normal, float64_iec559_t, float64_iec559_t)

SPECIALIZE_NEG(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_ABS(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_ADD_INEXACT(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_SUB_INEXACT(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_MUL_INEXACT(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_DIV_INEXACT(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_MOD_INEXACT(float, float64_iec559_t, float64_iec559_t)
SPECIALIZE_SQRT_INEXACT(float, float64_iec559_t, float64_iec559_t)

#ifdef FLOAT96_IEC559_TYPE
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, int8_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, int16_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, int32_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int, float96_iec559_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float_check_normal, float96_iec559_t, float96_iec559_t)
ASSIGN_R2(float32_iec559_t, float96_iec559_t)
ASSIGN_R2(float64_iec559_t, float96_iec559_t)

SPECIALIZE_NEG(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_ABS(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_ADD_INEXACT(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_SUB_INEXACT(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_MUL_INEXACT(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_DIV_INEXACT(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_MOD_INEXACT(float, float96_iec559_t, float96_iec559_t)
SPECIALIZE_SQRT_INEXACT(float, float96_iec559_t, float96_iec559_t)
#endif

#ifdef FLOAT128_IEC559_TYPE
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, int8_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, int16_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, int32_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, int64_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, u_int8_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, u_int16_t)
SPECIALIZE_ASSIGN(float_int, float127_iec559_t, u_int32_t)
SPECIALIZE_ASSIGN(float_int, float128_iec559_t, u_int64_t)
SPECIALIZE_ASSIGN(float_float_check_normal, float128_iec559_t, float128_iec559_t)
ASSIGN_R2(float32_iec559_t, float128_iec559_t)
ASSIGN_R2(float64_iec559_t, float128_iec559_t)
#ifdef FLOAT96_IEC559_TYPE
ASSIGN_R2(float96_iec559_t, float128_iec559_t)
#endif
specialize_all(float, float128_iec559_t, float128_iec559_t)

SPECIALIZE_NEG(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_ABS(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_ADD_INEXACT(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_SUB_INEXACT(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_MUL_INEXACT(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_DIV_INEXACT(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_MOD_INEXACT(float, float128_iec559_t, float128_iec559_t)
SPECIALIZE_SQRT_INEXACT(float, float128_iec559_t, float128_iec559_t)
#endif

#undef ASSIGN_R2

} // namespace Parma_Polyhedra_Library

