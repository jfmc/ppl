/* Abstract checked arithmetic function container
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

#ifndef PPL_checked_defs_hh
#define PPL_checked_defs_hh 1

#include <iostream>
#include <gmpxx.h>
#include "Rounding.defs.hh"
#include "Numeric_Format.defs.hh"

namespace Parma_Polyhedra_Library {

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
  //! Check for attempts to add infinities with different sign.
  static const int check_inf_add_inf = 0;
  //! Check for attempts to sub infinities with same sign.
  static const int check_inf_sub_inf = 0;
  //! Check for attempts to mul infinities by zero.
  static const int check_inf_mul_zero = 0;
  //! Check for attempts to divide by zero.
  static const int check_div_zero = 0;
  //! Check for attempts to divide infinities.
  static const int check_inf_div_inf = 0;
  //! Check for attempts to compute remainder of infinities.
  static const int check_inf_mod = 0;
  //! Check for attempts to take the square root of a negative number.
  static const int check_sqrt_neg = 0;
  //! Store unknown special value.
  static const int store_nan = 0;
  //! Store overflow special values.
  static const int store_infinity = 0;
  //! Representation is identical to primitive.
  static const int convertible = 1;
  //! Check for FPU inexact result.
  static const int fpu_check_inexact = 0;
  //! Check for NaN arguments
  static const int check_nan_args = 1;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A policy checking for overflows.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Check_Overflow_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int store_nan = 0;
  static const int store_infinity = 0;
  static const int convertible = 1;
  static const int fpu_check_inexact = 0;
  static const int check_nan_args = 1;
};

// It is a pity that function partial specialization is not permitted
// by C++.  To (partly) overcome this limitation, we use class
// encapsulated functions and partial specialization of containing
// classes.

#define FUNCTION_CLASS(name) name ## _function_struct

#define DECLARE_FUN1_0_0(name, ret_type, qual, type) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(qual type& arg) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(arg); \
}

#define DECLARE_FUN1_0_1(name, ret_type, qual, type, after1) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(qual type& arg, after1 a1) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(arg, a1); \
}

#define DECLARE_FUN1_0_2(name, ret_type, qual, type, after1, after2) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(qual type& arg, after1 a1, after2 a2) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(arg, a1, a2); \
}

#define DECLARE_FUN1_0_3(name, ret_type, qual, type, after1, after2, after3) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(qual type& arg, after1 a1, after2 a2, after3 a3) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(arg, a1, a2, a3); \
}

#define DECLARE_FUN1_1_1(name, ret_type, before1, qual, type, after1) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(before1 b1, qual type& arg, after1 a1) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(b1, arg, a1); \
}

#define DECLARE_FUN1_1_2(name, ret_type, before1, qual, type, after1, after2) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(before1 b1, qual type& arg, after1 a1, after2 a2) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(b1, arg, a1, a2); \
}

#define DECLARE_FUN1_2_2(name, ret_type, before1, before2, qual, type, after1, after2) \
template <typename Policy, typename type> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type> \
inline ret_type name(before1 b1, before2 b2, qual type& arg, after1 a1, after2 a2) { \
  return FUNCTION_CLASS(name)<Policy, type>::function(b1, b2, arg, a1, a2); \
}

#define DECLARE_FUN2_0_0(name, ret_type, qual1, type1, qual2, type2) \
template <typename Policy, typename type1, typename type2> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type1, typename type2> \
inline ret_type name(qual1 type1& arg1, qual2 type2& arg2) { \
  return FUNCTION_CLASS(name)<Policy, type1, type2>::function(arg1, arg2); \
}

#define DECLARE_FUN2_0_1(name, ret_type, qual1, type1, qual2, type2, after1) \
template <typename Policy, typename type1, typename type2> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type1, typename type2> \
inline ret_type name(qual1 type1& arg1, qual2 type2& arg2, after1 a1) { \
  return FUNCTION_CLASS(name)<Policy, type1, type2>::function(arg1, arg2, a1); \
}

#define DECLARE_FUN2_0_2(name, ret_type, qual1, type1, qual2, type2, after1, after2) \
template <typename Policy, typename type1, typename type2> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type1, typename type2> \
inline ret_type name(qual1 type1& arg1, qual2 type2& arg2, after1 a1, after2 a2) { \
  return FUNCTION_CLASS(name)<Policy, type1, type2>::function(arg1, arg2, a1, a2); \
}

#define DECLARE_FUN3_0_1(name, ret_type, qual1, type1, qual2, type2, qual3, type3, after1) \
template <typename Policy, typename type1, typename type2, typename type3> \
struct FUNCTION_CLASS(name); \
template <typename Policy, typename type1, typename type2, typename type3> \
inline ret_type name(qual1 type1& arg1, qual2 type2& arg2, qual3 type3& arg3, after1 a1) { \
  return FUNCTION_CLASS(name)<Policy, type1, type2, type3>::function(arg1, arg2, arg3, a1); \
}

#define DECLARE_FUN5_0_1(name, ret_type,				\
			 qual1, type1, qual2, type2, qual3, type3,	\
			 qual4, type4, qual5, type5,			\
			 after1)					\
template <typename Policy,						\
	  typename type1, typename type2, typename type3,		\
	  typename type4, typename type5>				\
struct FUNCTION_CLASS(name);						\
template <typename Policy,						\
	  typename type1, typename type2, typename type3,		\
	  typename type4, typename type5>				\
inline ret_type name(qual1 type1& arg1, qual2 type2& arg2,		\
		     qual3 type3& arg3, qual4 type4& arg4,		\
		     qual5 type5& arg5,	after1 a1) {			\
  return FUNCTION_CLASS(name)<Policy, type1, type2, type3, type4, type5> \
    ::function(arg1, arg2, arg3, arg4, arg5, a1);			\
}

#define SPECIALIZE_FUN1_0_0(name, suf, ret_type, qual, type) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(qual type& arg) { \
    return name ## _ ## suf<Policy>(arg); \
  } \
};

#define SPECIALIZE_FUN1_0_1(name, suf, ret_type, qual, type, after1) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(qual type& arg, after1 a1) { \
    return name ## _ ## suf<Policy>(arg, a1); \
  } \
};

#define SPECIALIZE_FUN1_0_2(name, suf, ret_type, qual, type, after1, after2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(qual type& arg, after1 a1, after2 a2) { \
    return name ## _ ## suf<Policy>(arg, a1, a2); \
  } \
};

#define SPECIALIZE_FUN1_0_3(name, suf, ret_type, qual, type, after1, after2, after3) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(qual type& arg, after1 a1, after2 a2, after3 a3) { \
    return name ## _ ## suf<Policy>(arg, a1, a2, a3); \
  } \
};

#define SPECIALIZE_FUN1_1_1(name, suf, ret_type, before1, qual, type, after1) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(before1 b1, qual type& arg, after1 a1) { \
    return name ## _ ## suf<Policy>(b1, arg, a1); \
  } \
};

#define SPECIALIZE_FUN1_1_2(name, suf, ret_type, before1, qual, type, after1, after2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(before1 b1, qual type& arg, after1 a1, after2 a2) { \
    return name ## _ ## suf<Policy>(b1, arg, a1, a2); \
  } \
};

#define SPECIALIZE_FUN1_2_2(name, suf, ret_type, before1, before2, qual, type, after1, after2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type> { \
  static inline ret_type function(before1 b1, before2 b2, qual type& arg, after1 a1, after2 a2) { \
    return name ## _ ## suf<Policy>(b1, b2, arg, a1, a2); \
  } \
};

#define SPECIALIZE_FUN2_0_0(name, suf, ret_type, qual1, type1, qual2, type2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type1, type2> { \
  static inline ret_type function(qual1 type1& arg1, qual2 type2 &arg2) { \
    return name ## _ ## suf<Policy>(arg1, arg2); \
  } \
};

#define SPECIALIZE_FUN2_0_1(name, suf, ret_type, qual1, type1, qual2, type2, after1) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type1, type2> { \
  static inline ret_type function(qual1 type1& arg1, qual2 type2 &arg2, after1 a1) { \
    return name ## _ ## suf<Policy>(arg1, arg2, a1); \
  } \
};

#define SPECIALIZE_FUN2_0_2(name, suf, ret_type, qual1, type1, qual2, type2, after1, after2) \
template <typename Policy> \
struct FUNCTION_CLASS(name)<Policy, type1, type2> { \
  static inline ret_type function(qual1 type1& arg1, qual2 type2 &arg2, after1 a1, after2 a2) { \
    return name ## _ ## suf<Policy>(arg1, arg2, a1, a2); \
  } \
};

#define SPECIALIZE_FUN3_0_1(name, suf, ret_type, qual1, type1, qual2, type2, qual3, type3, after1) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, type1, type2, type3> { \
  static inline Result function(qual1 type1& arg1, qual2 type2 &arg2, qual3 type3 &arg3, after1 a1) { \
    return name ## _ ## suf<Policy>(arg1, arg2, arg3, a1); \
  } \
};

#define SPECIALIZE_FUN5_0_1(name, suf, ret_type,			\
			    qual1, type1, qual2, type2, qual3, type3,	\
			    qual4, type4, qual5, type5, after1)		\
template <typename Policy>						\
struct FUNCTION_CLASS(name) <Policy,					\
			     type1, type2, type3, type4, type5> {	\
  static inline Result							\
  function(qual1 type1& arg1, qual2 type2 &arg2, qual3 type3 &arg3,	\
	   qual4 type4 &arg4, qual5 type5 &arg5, after1 a1) {		\
    return name ## _ ## suf<Policy>(arg1, arg2, arg3, arg4, arg5, a1);	\
  }									\
};

#define nonconst

#define SPECIALIZE_SGN(suf, From) \
  SPECIALIZE_FUN1_0_0(sgn, suf, Result, const, From)
#define SPECIALIZE_CMP(suf, Type1, Type2) \
  SPECIALIZE_FUN2_0_0(cmp, suf, Result, const, Type1, const, Type2)
#define SPECIALIZE_SET_SPECIAL(suf, Type) \
  SPECIALIZE_FUN1_0_1(set_special, suf, Result, nonconst, Type, Result)
#define SPECIALIZE_CLASSIFY(suf, Type) \
  SPECIALIZE_FUN1_0_3(classify, suf, Result, const, Type, bool, bool, bool)
#define SPECIALIZE_IS_NAN(suf, Type) \
  SPECIALIZE_FUN1_0_0(is_nan, suf, bool, const, Type)
#define SPECIALIZE_IS_MINF(suf, Type) \
  SPECIALIZE_FUN1_0_0(is_minf, suf, bool, const, Type)
#define SPECIALIZE_IS_PINF(suf, Type) \
  SPECIALIZE_FUN1_0_0(is_pinf, suf, bool, const, Type)
#define SPECIALIZE_IS_INT(suf, Type) \
  SPECIALIZE_FUN1_0_0(is_int, suf, bool, const, Type)
#define SPECIALIZE_ASSIGN(suf, To, From) \
  SPECIALIZE_FUN2_0_1(assign, suf, Result, nonconst, To, const, From, Rounding_Dir)
#define SPECIALIZE_NEG(suf, To, From) \
  SPECIALIZE_FUN2_0_1(neg, suf, Result, nonconst, To, const, From, Rounding_Dir)
#define SPECIALIZE_ABS(suf, To, From) \
  SPECIALIZE_FUN2_0_1(abs, suf, Result, nonconst, To, const, From, Rounding_Dir)
#define SPECIALIZE_SQRT(suf, To, From) \
  SPECIALIZE_FUN2_0_1(sqrt, suf, Result, nonconst, To, const, From, Rounding_Dir)
#define SPECIALIZE_ADD(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(add, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_SUB(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(sub, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_MUL(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(mul, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_DIV(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(div, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_REM(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(rem, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_MUL2EXP(suf, To, From) \
  SPECIALIZE_FUN2_0_2(mul2exp, suf, Result, nonconst, To, const, From, int, Rounding_Dir)
#define SPECIALIZE_DIV2EXP(suf, To, From) \
  SPECIALIZE_FUN2_0_2(div2exp, suf, Result, nonconst, To, const, From, int, Rounding_Dir)
#define SPECIALIZE_ADD_MUL(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(add_mul, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_SUB_MUL(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(sub_mul, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_GCD(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(gcd, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_GCDEXT(suf, To, From1, From2, From3, From4)		\
  SPECIALIZE_FUN5_0_1(gcdext, suf, Result, nonconst, To,		\
		      const, From1, const, From2, nonconst, From3, nonconst, From4, Rounding_Dir)
#define SPECIALIZE_LCM(suf, To, From1, From2) \
  SPECIALIZE_FUN3_0_1(lcm, suf, Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
#define SPECIALIZE_INPUT(suf, Type) \
  SPECIALIZE_FUN1_0_2(input, suf, Result, nonconst, Type, std::istream&, Rounding_Dir)
#define SPECIALIZE_OUTPUT(suf, Type) \
  SPECIALIZE_FUN1_1_2(output, suf, Result, std::ostream&, const, Type, const Numeric_Format&, Rounding_Dir)


DECLARE_FUN1_0_0(sgn,         Result, const, From)
DECLARE_FUN2_0_0(cmp,         Result, const, Type1, const, Type2)
DECLARE_FUN1_0_1(set_special, Result, nonconst, Type, Result)
DECLARE_FUN1_0_3(classify,    Result, const, Type, bool, bool, bool)
DECLARE_FUN1_0_0(is_nan,      bool, const, Type)
DECLARE_FUN1_0_0(is_minf,     bool, const, Type)
DECLARE_FUN1_0_0(is_pinf,     bool, const, Type)
DECLARE_FUN1_0_0(is_int,      bool, const, Type)
DECLARE_FUN2_0_1(assign,      Result, nonconst, To, const, From, Rounding_Dir)
DECLARE_FUN2_0_1(neg,         Result, nonconst, To, const, From, Rounding_Dir)
DECLARE_FUN2_0_1(abs,         Result, nonconst, To, const, From, Rounding_Dir)
DECLARE_FUN2_0_1(sqrt,        Result, nonconst, To, const, From, Rounding_Dir)
DECLARE_FUN3_0_1(add,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(sub,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(mul,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(div,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(rem,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN2_0_2(mul2exp,     Result, nonconst, To, const, From, int, Rounding_Dir)
DECLARE_FUN2_0_2(div2exp,     Result, nonconst, To, const, From, int, Rounding_Dir)
DECLARE_FUN3_0_1(add_mul,     Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(sub_mul,     Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN3_0_1(gcd,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN5_0_1(gcdext,      Result, nonconst, To, const, From1, const, From2,
		 nonconst, From3, nonconst, From4, Rounding_Dir)
DECLARE_FUN3_0_1(lcm,         Result, nonconst, To, const, From1, const, From2, Rounding_Dir)
DECLARE_FUN1_0_2(input,       Result, nonconst, Type, std::istream&, Rounding_Dir)
DECLARE_FUN1_1_2(output,      Result, std::ostream&, const, Type, const Numeric_Format&, Rounding_Dir)

template <typename Policy, typename To>
Result round(To& to, Result r, Rounding_Dir dir);

Result input_mpq(mpq_class& to, std::istream& is);

} // namespace Checked

struct Minus_Infinity {
};

struct Plus_Infinity {
};

struct Not_A_Number {
};

extern Minus_Infinity MINUS_INFINITY;
extern Plus_Infinity PLUS_INFINITY;
extern Not_A_Number NOT_A_NUMBER;

} // namespace Parma_Polyhedra_Library


#define CHECK_P(cond, check) ((cond) ? (check) : (assert(!(check)), false))

#include "checked.inlines.hh"
#include "checked_int.inlines.hh"
#include "checked_float.inlines.hh"
#include "checked_mpz.inlines.hh"
#include "checked_mpq.inlines.hh"
#include "checked_ext.inlines.hh"

#endif // !defined(PPL_checked_defs_hh)
