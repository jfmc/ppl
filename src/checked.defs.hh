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
#include "Rounding.defs.hh"
#include "Numeric_Format.defs.hh"

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A policy checking for overflows.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Check_Overflow_Policy {
  static const int check_overflow = 1;
  static const int check_divbyzero = 0;
  static const int check_sqrt_neg = 0;
  static const int round_inexact = 1;
  static const int store_nan = 0;
  static const int store_infinity = 0;
  static const int convertible = 1;
  static const int fpu_classify = 0;
  static const int fpu_check_inexact = 0;
};

typedef const char* c_string;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Types and functions implementing checked numbers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
namespace Checked {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A policy that specifies a total adherence to behavior of the underlying type.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Transparent_Policy {
  //! Check for overflowed result.
  static const int check_overflow = 0;
  //! Check for attempts to divide by zero.
  static const int check_divbyzero = 0;
  //! Check for attempts to take the square root of a negative number.
  static const int check_sqrt_neg = 0;
  //! Support rounding mode
  static const int round_inexact = 0;
  //! Store unknown special value.
  static const int store_nan = 0;
  //! Store overflow special values.
  static const int store_infinity = 0;
  //! Representation is identical to primitive.
  static const int convertible = 1;
  //! Return information for special values.
  static const int fpu_classify = 0;
  //! Check for FPU inexact result.
  static const int fpu_check_inexact = 0;
};


// It is a pity that function partial specialization is not permitted
// by C++.  To (partly) overcome this limitation, we use class
// encapsulated functions and partial specialization of containing
// classes.

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
  
#define DECLARE_FUN4(name, ret_type, qual1, qual2, qual3, qual4) \
template <typename Policy, typename Type1, typename Type2, typename Type3, typename Type4> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename Type1, typename Type2, typename Type3, typename Type4> \
inline ret_type name(qual1 Type1& arg1, qual2 Type2& arg2, qual3 Type3& arg3, qual4 Type4& arg4) { \
  return FUNCTION_CLASS(name)<Policy, Type1, Type2, Type3, Type4>::function(arg1, arg2, arg3, arg4); \
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

#define SPECIALIZE_FUN4(name, suf, ret_type, qual1, type1, qual2, type2, qual3, type3, qual4, type4) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, type1, type2, type3, type4> { \
  static inline Result function(qual1 type1& arg1, qual2 type2 &arg2, qual3 type3 &arg3, qual4 type4 &arg4) { \
    return name ## _ ## suf<Policy>(arg1, arg2, arg3, arg4); \
  } \
};

#define nonconst

#define SPECIALIZE_PRED(suf, Type) \
  SPECIALIZE_FUN1(pred, suf, Result, nonconst, Type)
#define SPECIALIZE_SUCC(suf, Type) \
  SPECIALIZE_FUN1(succ, suf, Result, nonconst, Type)
#define SPECIALIZE_SGN(suf, From) \
  SPECIALIZE_FUN1(sgn, suf, Result, const, From)
#define SPECIALIZE_CMP(suf, Type1, Type2) \
  SPECIALIZE_FUN2(cmp, suf, Result, const, Type1, const, Type2)
#define SPECIALIZE_SET_SPECIAL(suf, Type) \
  SPECIALIZE_FUN2(set_special, suf, void, nonconst, Type, const, Result)
#define SPECIALIZE_CLASSIFY(suf, Type) \
  SPECIALIZE_FUN4(classify, suf, Result, const, Type, const, bool, const, bool, const, bool)
#define SPECIALIZE_ASSIGN(suf, To, From) \
  SPECIALIZE_FUN3(assign, suf, Result, nonconst, To, const, From, const, Rounding)
#define SPECIALIZE_NEG(suf, To, From) \
  SPECIALIZE_FUN3(neg, suf, Result, nonconst, To, const, From, const, Rounding)
#define SPECIALIZE_ABS(suf, To, From) \
  SPECIALIZE_FUN3(abs, suf, Result, nonconst, To, const, From, const, Rounding)
#define SPECIALIZE_SQRT(suf, To, From) \
  SPECIALIZE_FUN3(sqrt, suf, Result, nonconst, To, const, From, const, Rounding)
#define SPECIALIZE_ADD(suf, To, From) \
  SPECIALIZE_FUN4(add, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_SUB(suf, To, From) \
  SPECIALIZE_FUN4(sub, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_MUL(suf, To, From) \
  SPECIALIZE_FUN4(mul, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_DIV(suf, To, From) \
  SPECIALIZE_FUN4(div, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_MOD(suf, To, From) \
  SPECIALIZE_FUN4(mod, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_ADD_MUL(suf, To, From) \
  SPECIALIZE_FUN4(add_mul, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_SUB_MUL(suf, To, From) \
  SPECIALIZE_FUN4(sub_mul, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_GCD(suf, To, From) \
  SPECIALIZE_FUN4(gcd, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_LCM(suf, To, From) \
  SPECIALIZE_FUN4(lcm, suf, Result, nonconst, To, const, From, const, From, const, Rounding)
#define SPECIALIZE_PRINT(suf, Type) \
  SPECIALIZE_FUN4(print, suf, void, nonconst, std::ostream, const, Type, const, Numeric_Format, const, Rounding)
#define SPECIALIZE_INPUT(suf, Type) \
  SPECIALIZE_FUN3(input, suf, Result, nonconst, std::istream, nonconst, Type, const, Rounding)


DECLARE_FUN1(pred,        Result, )
DECLARE_FUN1(succ,        Result, )
DECLARE_FUN3(assign,      Result, nonconst, const,    const)
DECLARE_FUN3(neg,         Result, nonconst, const,    const)
DECLARE_FUN3(abs,         Result, nonconst, const,    const)
DECLARE_FUN3(sqrt,        Result, nonconst, const,    const)
DECLARE_FUN4(add,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(sub,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(mul,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(div,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(mod,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(add_mul,     Result, nonconst, const,    const,  const)
DECLARE_FUN4(sub_mul,     Result, nonconst, const,    const,  const)
DECLARE_FUN4(gcd,         Result, nonconst, const,    const,  const)
DECLARE_FUN4(lcm,         Result, nonconst, const,    const,  const)
DECLARE_FUN1(sgn,         Result, const)
DECLARE_FUN2(cmp,         Result, const,    const)
DECLARE_FUN4(classify,    Result, const,    const,    const,  const)
DECLARE_FUN2(set_special, void,   nonconst, const)
DECLARE_FUN4(print,       Result, nonconst, const,    const,  const)
DECLARE_FUN3(input,       Result, nonconst, nonconst, const)

template <typename Policy, typename To>
Result round(To& to, Result r, const Rounding& mode);

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#include "checked.inlines.hh"
#include "checked_int.inlines.hh"
#include "checked_float.inlines.hh"
#include "checked_mpz.inlines.hh"
#include "checked_mpq.inlines.hh"

#endif // !defined(PPL_checked_defs_hh)
