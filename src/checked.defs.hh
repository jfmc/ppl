/* Abstract checked arithmetic functions
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

#include "Result_Info.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename To, typename From>
Result_Info checked_assign(To& to, From from);

template <typename To>
Result_Info checked_pred(To& to);

template <typename To>
Result_Info checked_succ(To& to);

template <typename To, typename From>
Result_Info checked_neg(To& to, From);

template <typename To, typename From>
Result_Info checked_add(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_sub(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_mul(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_div(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_mod(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_abs(To& to, From from);

template <typename From>
int sgn(From x);

template <typename From>
int cmp(From x, From y);

template <typename Type>
void swap(Type& x, Type& y);

template <typename To, typename From>
Result_Info checked_gcd(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_lcm(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_sqrt(To& to, From from);

template <typename To, typename From>
inline Result_Info checked_assignexact(To& to, From from);

template <typename To, typename From>
inline Result_Info checked_negexact(To& to, From from);

template <typename To, typename From>
Result_Info checked_addexact(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_subexact(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_mulexact(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_divexact(To& to, From x, From y);

template <typename To, typename From>
Result_Info checked_modexact(To& to, From x, From y);

template  <typename To, typename From>
Result_Info checked_sqrtexact(To& to, From from);

#define SPECIALIZE_FUN1(fun, suf, Type) \
inline Result_Info \
checked_ ## fun(Type& to) { \
  return checked_ ## fun ## _ ## suf(to); \
}

#define SPECIALIZE_FUN2(fun, suf, To, From) \
inline Result_Info \
checked_ ## fun(To& to, From from) { \
  return checked_ ## fun ## _ ## suf(to, from); \
}

#define SPECIALIZE_FUN3(fun, suf, To, From) \
inline Result_Info \
checked_ ## fun(To& to, From x, From y) { \
  return checked_ ## fun ## _ ## suf(to, x, y); \
}

#define SPECIALIZE_FUN1_inexact(fun, suf, Type) \
SPECIALIZE_FUN1(fun, suf ## _check_inexact, Type) \
inline Result_Info \
checked_ ## fun ## exact(Type& to) { \
  return checked_ ## fun ## _ ## suf(to); \
}

#define SPECIALIZE_FUN2_INEXACT(fun, suf, To, From) \
SPECIALIZE_FUN2(fun, suf ## _check_inexact, To, From) \
inline Result_Info \
checked_ ## fun ## exact (To& to, From from) { \
  return checked_ ## fun ## _ ## suf(to, from); \
}

#define SPECIALIZE_FUN3_INEXACT(fun, suf, To, From) \
SPECIALIZE_FUN3(fun, suf ## _check_inexact, To, From) \
inline Result_Info \
checked_ ## fun ## exact (To& to, From x, From y) { \
  return checked_ ## fun ## _ ## suf(to, x, y); \
}

#define SPECIALIZE_PRED(suf, Type) SPECIALIZE_FUN1(pred, suf, Type)
#define SPECIALIZE_SUCC(suf, Type) SPECIALIZE_FUN1(succ, suf, Type)
#define SPECIALIZE_ASSIGN(suf, To, From) SPECIALIZE_FUN2(assign, suf, To, From)
#define SPECIALIZE_NEG(suf, To, From) SPECIALIZE_FUN2(neg, suf, To, From)
#define SPECIALIZE_ADD(suf, To, From) SPECIALIZE_FUN3(add, suf, To, From)
#define SPECIALIZE_SUB(suf, To, From) SPECIALIZE_FUN3(sub, suf, To, From)
#define SPECIALIZE_MUL(suf, To, From) SPECIALIZE_FUN3(mul, suf, To, From)
#define SPECIALIZE_DIV(suf, To, From) SPECIALIZE_FUN3(div, suf, To, From)
#define SPECIALIZE_MOD(suf, To, From) SPECIALIZE_FUN3(mod, suf, To, From)
#define SPECIALIZE_ABS(suf, To, From) SPECIALIZE_FUN2(abs, suf, To, From)
#define SPECIALIZE_SQRT(suf, To, From) SPECIALIZE_FUN2(sqrt, suf, To, From)

#define SPECIALIZE_ASSIGN_INEXACT(suf, To, From) SPECIALIZE_FUN2_INEXACT(assign, suf, To, From)
#define SPECIALIZE_NEG_INEXACT(suf, To, From) SPECIALIZE_FUN2_INEXACT(neg, suf, To, From)
#define SPECIALIZE_ADD_INEXACT(suf, To, From) SPECIALIZE_FUN3_INEXACT(add, suf, To, From)
#define SPECIALIZE_SUB_INEXACT(suf, To, From) SPECIALIZE_FUN3_INEXACT(sub, suf, To, From)
#define SPECIALIZE_MUL_INEXACT(suf, To, From) SPECIALIZE_FUN3_INEXACT(mul, suf, To, From)
#define SPECIALIZE_DIV_INEXACT(suf, To, From) SPECIALIZE_FUN3_INEXACT(div, suf, To, From)
#define SPECIALIZE_MOD_INEXACT(suf, To, From) SPECIALIZE_FUN3_INEXACT(mod, suf, To, From)
#define SPECIALIZE_SQRT_INEXACT(suf, To, From) SPECIALIZE_FUN2_INEXACT(sqrt, suf, To, From)

} // namespace Parma_Polyhedra_Library

#include "checked.inlines.hh"

#endif // !defined(PPL_checked_defs_hh)
