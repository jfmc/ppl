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

namespace Parma_Polyhedra_Library {

namespace Checked {

struct Policy_Safe {
  static const int check_overflow = 1;
  static const int check_exact = 1;
  static const int check_divbyzero = 1;
  static const int check_sqrt_neg = 1;
  static const int check_normal = 1;
};


/* This is a kind of bit mask (with logical OR semantic), with special
   values for underflow and overflow */ 
enum Result {
  V_EQ = 1,
  V_LT = 2,
  V_GT = 4,
  V_LE = V_EQ | V_LT,
  V_GE = V_EQ | V_GT,
  V_NE = V_LT | V_GT,
  V_APPROX = V_LT | V_EQ | V_GT,
  V_NAN = 0,
  V_NEG_OVERFLOW = 8,
  V_POS_OVERFLOW = 9
};


/*
  It's a pity that function partial specialization is not permitted by C++.
  We use class encapsulated function and partial specialization of containing classes.
*/

#define FUNCTION_CLASS(name) name ## _

#define DECLARE_FUN1(name) \
template <typename Policy, typename To> \
struct FUNCTION_CLASS(name) { \
  static Result function(To& to); \
}; \
template <typename Policy, typename To> \
inline Result name(To& to) { \
  return FUNCTION_CLASS(name) <Policy, To>::function(to); \
}

#define DECLARE_FUN2(name) \
template <typename Policy, typename To, typename From> \
struct FUNCTION_CLASS(name) { \
  static Result function(To& to, From from); \
}; \
template <typename Policy, typename To, typename From> \
inline Result name(To& to, From from) { \
  return FUNCTION_CLASS(name) <Policy, To, From>::function(to, from); \
}
  
#define DECLARE_FUN3(name) \
template <typename Policy, typename To, typename From> \
struct FUNCTION_CLASS(name) { \
  static Result function(To& to, From x, From y); \
}; \
template <typename Policy, typename To, typename From> \
inline Result name(To& to, From x, From y) { \
  return FUNCTION_CLASS(name) <Policy, To, From>::function(to, x, y); \
}
  
  
DECLARE_FUN1(pred)
DECLARE_FUN1(succ)
DECLARE_FUN2(assign)
DECLARE_FUN2(neg)
DECLARE_FUN2(abs)
DECLARE_FUN2(sqrt)
DECLARE_FUN3(add)
DECLARE_FUN3(sub)
DECLARE_FUN3(mul)
DECLARE_FUN3(div)
DECLARE_FUN3(mod)
DECLARE_FUN3(gcd)
DECLARE_FUN3(lcm)


#define SPECIALIZE_FUN1(name, suf, Type) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, Type> { \
  static inline Result function(Type& to) { \
    return name ## _ ## suf<Policy>(to); \
  } \
};

#define SPECIALIZE_FUN2(name, suf, To, From) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, To, From> { \
  static inline Result function(To& to, From from) { \
    return name ## _ ## suf<Policy>(to, from); \
  } \
};

#define SPECIALIZE_FUN3(name, suf, To, From) \
template <typename Policy> \
struct FUNCTION_CLASS(name) <Policy, To, From> { \
  static inline Result function(To& to, From x, From y) { \
    return name ## _ ## suf<Policy>(to, x, y); \
  } \
};

#define SPECIALIZE_PRED(suf, Type) SPECIALIZE_FUN1(pred, suf, Type)
#define SPECIALIZE_SUCC(suf, Type) SPECIALIZE_FUN1(succ, suf, Type)
#define SPECIALIZE_ASSIGN(suf, To, From) SPECIALIZE_FUN2(assign, suf, To, From)
#define SPECIALIZE_NEG(suf, To, From) SPECIALIZE_FUN2(neg, suf, To, From)
#define SPECIALIZE_ABS(suf, To, From) SPECIALIZE_FUN2(abs, suf, To, From)
#define SPECIALIZE_SQRT(suf, To, From) SPECIALIZE_FUN2(sqrt, suf, To, From)
#define SPECIALIZE_ADD(suf, To, From) SPECIALIZE_FUN3(add, suf, To, From)
#define SPECIALIZE_SUB(suf, To, From) SPECIALIZE_FUN3(sub, suf, To, From)
#define SPECIALIZE_MUL(suf, To, From) SPECIALIZE_FUN3(mul, suf, To, From)
#define SPECIALIZE_DIV(suf, To, From) SPECIALIZE_FUN3(div, suf, To, From)
#define SPECIALIZE_MOD(suf, To, From) SPECIALIZE_FUN3(mod, suf, To, From)
#define SPECIALIZE_GCD(suf, To, From) SPECIALIZE_FUN3(gcd, suf, To, From)
#define SPECIALIZE_LCM(suf, To, From) SPECIALIZE_FUN3(lcm, suf, To, From)

} // namespace Checked

template <typename From>
int sgn(From x);

template <typename From>
int cmp(From x, From y);

} // namespace Parma_Polyhedra_Library

#include "checked.inlines.hh"

#endif // !defined(PPL_checked_defs_hh)
	
