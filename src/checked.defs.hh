/* Abstract checked arithmetic function container
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

#ifndef PPL_checked_defs_hh
#define PPL_checked_defs_hh 1

#include <iostream>

namespace Parma_Polyhedra_Library {

struct Check_Overflow_Policy {
  static const int check_overflow = 1;
  static const int check_nan_result = 0;
  static const int check_inexact = 0;
  static const int check_divbyzero = 0;
  static const int check_sqrt_neg = 0;
  static const int check_nan_arg = 0;
  static const int check_infinity_arg = 0;
  static const int store_unknown = 0;
  static const int store_overflows = 0;
  static const int convertible = 1;
};

namespace Checked {

struct Policy_Safe {
  /* Check for overflowed result */
  static const int check_overflow = 1;
  /* Check for inexact result */
  static const int check_inexact = 1;
  /* Check for division by zero attempt */
  static const int check_divbyzero = 1;
  /* Check for sqrt of negative number attempt */
  static const int check_sqrt_neg = 1;
  /* Store unknown special value */
  static const int store_unknown = 1;
  /* Store overflow special values */
  static const int store_overflows = 1;
  /* Check for float NaN argument */
  static const int check_nan_arg = 1;
  /* Check for float infinity argument */
  static const int check_infinity_arg = 1;
  /* Check for float NaN result */
  static const int check_nan_result = 1;
};


enum Result {
  // Returned result is exact
  V_EQ = 1,

  // Returned result is inexact and rounded up
  V_LT = 2,

  // Returned result is inexact and rounded down
  V_GT = 4,

  // Returned result is inexact
  V_NE = V_LT | V_GT,

  // Returned result may be inexact and rounded up
  V_LE = V_EQ | V_LT,

  // Returned result may be inexact and rounded down
  V_GE = V_EQ | V_GT,

  // Returned result may be inexact
  V_LGE = V_LT | V_EQ | V_GT,

  // Returned result may not be the nearest representable of exact result
  // To be OR'ed with values above
  V_APPROX = 8,

  // Special results (no numeric result is returned)
  // Keep all of these > V_UNKNOWN

  // The result is unknown
  V_UNKNOWN = 16,

  // Result is out of numeric domain
  V_DOMAIN = 17,

  // Negative overflow
  V_NEG_OVERFLOW = 18,

  // Positive overflow
  V_POS_OVERFLOW = 19

};


/*
  It's a pity that function partial specialization is not permitted by C++.
  We use class encapsulated function and partial specialization of containing classes.
*/

#define FUNCTION_CLASS(name) name ## _function_struct

#define DECLARE_FUN1(name, ret_type, qual) \
template <typename Policy, typename Type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename Type> \
inline ret_type name(qual Type& arg) { \
  return FUNCTION_CLASS(name)<Policy, Type>::function(arg); \
}

#define DECLARE_FUN2(name, ret_type, qual1, qual2) \
template <typename Policy, typename Type1, typename Type2> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename Type1, typename Type2> \
inline ret_type name(qual1 Type1& arg1, qual2 Type2& arg2) { \
  return FUNCTION_CLASS(name)<Policy, Type1, Type2>::function(arg1, arg2); \
}
  
#define DECLARE_FUN3(name, ret_type, qual1, qual2, qual3) \
template <typename Policy, typename Type1, typename Type2, typename Type3> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename Type1, typename Type2, typename Type3> \
inline ret_type name(qual1 Type1& arg1, qual2 Type2& arg2, qual3 Type3& arg3) { \
  return FUNCTION_CLASS(name)<Policy, Type1, Type2, Type3>::function(arg1, arg2, arg3); \
}
  
#define SPECIALIZE_FUN1(name, suf, ret_type, qual, type) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(qual type& arg) { \
    return name ## _ ## suf<Policy>(arg); \
  } \
};

#define SPECIALIZE_FUN2(name, suf, ret_type, qual1, type1, qual2, type2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type1, type2> { \
  static inline ret_type function(qual1 type1& arg1, qual2 type2 &arg2) { \
    return name ## _ ## suf<Policy>(arg1, arg2); \
  } \
};

#define SPECIALIZE_FUN3(name, suf, ret_type, qual1, type1, qual2, type2, qual3, type3) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, type1, type2, type3> { \
  static inline Result function(qual1 type1& arg1, qual2 type2 &arg2, qual3 type3 &arg3) { \
    return name ## _ ## suf<Policy>(arg1, arg2, arg3); \
  } \
};

#define SPECIALIZE_PRED(suf, type) SPECIALIZE_FUN1(pred, suf, Result, , type)
#define SPECIALIZE_SUCC(suf, type) SPECIALIZE_FUN1(succ, suf, Result, , type)
#define SPECIALIZE_ASSIGN(suf, To, From) SPECIALIZE_FUN2(assign, suf, Result, , To, const, From)
#define SPECIALIZE_NEG(suf, To, From) SPECIALIZE_FUN2(neg, suf, Result, , To, const, From)
#define SPECIALIZE_ABS(suf, To, From) SPECIALIZE_FUN2(abs, suf, Result, , To, const, From)
#define SPECIALIZE_SQRT(suf, To, From) SPECIALIZE_FUN2(sqrt, suf, Result, , To, const, From)
#define SPECIALIZE_ADD(suf, To, From) SPECIALIZE_FUN3(add, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_SUB(suf, To, From) SPECIALIZE_FUN3(sub, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_MUL(suf, To, From) SPECIALIZE_FUN3(mul, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_DIV(suf, To, From) SPECIALIZE_FUN3(div, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_MOD(suf, To, From) SPECIALIZE_FUN3(mod, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_ADD_MUL(suf, To, From) SPECIALIZE_FUN3(add_mul, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_SUB_MUL(suf, To, From) SPECIALIZE_FUN3(sub_mul, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_GCD(suf, To, From) SPECIALIZE_FUN3(gcd, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_LCM(suf, To, From) SPECIALIZE_FUN3(lcm, suf, Result, , To, const, From, const, From)
#define SPECIALIZE_SGN(suf, From) SPECIALIZE_FUN1(sgn, suf, Result, const, From)
#define SPECIALIZE_CMP(suf, From) SPECIALIZE_FUN2(cmp, suf, Result, const, From, const, From)
#define SPECIALIZE_SET_SPECIAL(suf, type) SPECIALIZE_FUN2(set_special, suf, void, , type, const, Result)
#define SPECIALIZE_VALUE_TYPE(suf, type) SPECIALIZE_FUN1(value_type, suf, Result, const, type)

DECLARE_FUN1(pred, Result, )
DECLARE_FUN1(succ, Result, )
DECLARE_FUN2(assign, Result, , const)
DECLARE_FUN2(neg, Result, , const)
DECLARE_FUN2(abs, Result, , const)
DECLARE_FUN2(sqrt, Result, , const)
DECLARE_FUN3(add, Result, , const, const)
DECLARE_FUN3(sub, Result, , const, const)
DECLARE_FUN3(mul, Result, , const, const)
DECLARE_FUN3(div, Result, , const, const)
DECLARE_FUN3(mod, Result, , const, const)
DECLARE_FUN3(add_mul, Result, , const, const)
DECLARE_FUN3(sub_mul, Result, , const, const)
DECLARE_FUN3(gcd, Result, , const, const)
DECLARE_FUN3(lcm, Result, , const, const)
DECLARE_FUN1(sgn, Result, const)
DECLARE_FUN2(cmp, Result, const, const)
DECLARE_FUN1(value_type, Result, const)
DECLARE_FUN2(set_special, void, , const)

template <typename Policy, typename To, typename From>
Result assign_ext(To& to, const From& from);

template <typename Policy, typename Type>
Result sgn_ext(const Type& x);

template <typename Policy, typename Type1, typename Type2>
Result cmp_ext(const Type1& x, const Type2& y);

template <typename Policy, typename To, typename From>
Result neg_ext(To& to, const From& x);

template <typename Policy, typename To, typename From>
Result abs_ext(To& to, const From& x);

template <typename Policy, typename To, typename From1, typename From2>
Result add_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result sub_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result mul_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result div_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result mod_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result add_mul_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result sub_mul_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From>
Result sqrt_ext(To& to, const From& x);

template <typename Policy, typename To, typename From1, typename From2>
Result gcd_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename To, typename From1, typename From2>
Result lcm_ext(To& to, const From1& x, const From2& y);

template <typename Policy, typename Type>
void print_ext(std::ostream& os, const Type& x);

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#include "checked.inlines.hh"

#endif // !defined(PPL_checked_defs_hh)
