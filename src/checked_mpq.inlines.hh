/* Specialized "checked" functions for GMP's mpq_class numbers.
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

#ifndef PPL_checked_mpq_inlines_hh
#define PPL_checked_mpq_inlines_hh 1

#include "checked_mpz.inlines.hh"
#include <gmpxx.h>

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy>
inline Result
classify_mpq(const mpq_class& v, bool nan, bool inf, bool sign) {
  if ((Policy::store_nan || Policy::store_infinity)
      && ::sgn(v.get_den()) == 0) {
    int s = ::sgn(v.get_num());
    if (Policy::store_nan && (nan || sign) && s == 0)
      return VC_NAN;
    if (!inf && !sign)
      return VC_NORMAL;
    if (Policy::store_infinity) {
      if (s < 0)
	return inf ? VC_MINUS_INFINITY : V_LT;
      if (s > 0)
	return inf ? VC_PLUS_INFINITY : V_GT;
    }
  }
  if (sign)
    return sgn<Policy>(v);
  return VC_NORMAL;
}

SPECIALIZE_CLASSIFY(mpq, mpq_class)

template <typename Policy>
inline bool
is_nan_mpq(const mpq_class& v) {
  return Policy::store_nan && ::sgn(v.get_den()) == 0 && ::sgn(v.get_num()) == 0;
}

SPECIALIZE_IS_NAN(mpq, mpq_class)

template <typename Policy>
inline bool
is_minf_mpq(const mpq_class& v) {
  return Policy::store_infinity && ::sgn(v.get_den()) == 0 && ::sgn(v.get_num()) < 0;
}

SPECIALIZE_IS_MINF(mpq, mpq_class)

template <typename Policy>
inline bool
is_pinf_mpq(const mpq_class& v) {
  return Policy::store_infinity && ::sgn(v.get_den()) == 0 && ::sgn(v.get_num()) > 0;
}

SPECIALIZE_IS_PINF(mpq, mpq_class)

template <typename Policy>
inline Result
set_special_mpq(mpq_class& v, Result r) {
  int num;
  Result t = classify(r);
  if (Policy::store_nan && t == VC_NAN)
    num = 0;
  else if (Policy::store_infinity) {
    switch (t) {
    case VC_MINUS_INFINITY:
      num = -1;
      break;
    case VC_PLUS_INFINITY:
      num = 1;
      break;
    default:
      return r;
    }
  }
  else
    return r;
  v.get_num() = num;
  v.get_den() = 0;
  return r;
}

SPECIALIZE_SET_SPECIAL(mpq, mpq_class)

template <typename Policy>
inline Result
pred_mpq(mpq_class&) {
  throw 0;
}

SPECIALIZE_PRED(mpq, mpq_class)

template <typename Policy>
inline Result
succ_mpq(mpq_class&) {
  throw 0;
}

SPECIALIZE_SUCC(mpq, mpq_class)

template <typename Policy>
inline Result
assign_mpq_mpz(mpq_class& to, const mpz_class& from, const Rounding&) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_mpz, mpq_class, mpz_class)

template <typename Policy, typename From>
inline Result
assign_mpq_signed_int(mpq_class& to, const From from, const Rounding&) {
  if (sizeof(From) <= sizeof(long))
    to = static_cast<long>(from);
  else {
    mpz_ptr m = to.get_num().get_mpz_t();
    if (from >= 0)
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
    else {
      From n = -from;
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &n);
      mpz_neg(m, m);
    }
    to.get_den() = 1;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, signed char)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, short)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, int)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, long)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, long long)

template <typename Policy, typename From>
inline Result
assign_mpq_unsigned_int(mpq_class& to, const From from, const Rounding&) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else {
    mpz_import(to.get_num().get_mpz_t(), 1, 1, sizeof(From), 0, 0, &from);
    to.get_den() = 1;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, unsigned char)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, unsigned short)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, unsigned int)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, unsigned long)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, unsigned long long)

template <typename Policy, typename From>
inline Result
assign_mpq_float(mpq_class& to, const From from, const Rounding&) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_float, mpq_class, float)
SPECIALIZE_ASSIGN(mpq_float, mpq_class, double)

template <typename Policy>
inline Result
neg_mpq(mpq_class& to, const mpq_class& from, const Rounding&) {
  mpq_neg(to.get_mpq_t(), from.get_mpq_t());
  return V_EQ;
}

SPECIALIZE_NEG(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
add_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y, const Rounding&) {
  to = x + y;
  return V_EQ;
}

SPECIALIZE_ADD(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
sub_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y, const Rounding&) {
  to = x - y;
  return V_EQ;
}

SPECIALIZE_SUB(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
mul_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y, const Rounding&) {
  to = x * y;
  return V_EQ;
}

SPECIALIZE_MUL(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
div_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y, const Rounding&) {
  if (Policy::check_divbyzero && sgn(y) == 0)
    return set_special<Policy>(to, V_DIV_ZERO);
  to = x / y;
  return V_EQ;
}

SPECIALIZE_DIV(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
rem_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y, const Rounding&) {
  if (Policy::check_divbyzero && sgn(y) == 0)
    return set_special<Policy>(to, V_MOD_ZERO);
  to = x / y;
  to.get_num() %= to.get_den();
  return V_EQ;
}

SPECIALIZE_REM(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
abs_mpq(mpq_class& to, const mpq_class& from, const Rounding&) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
from_c_string_mpq(mpq_class& to, const char* from, const Rounding&) {
  if (mpq_set_str(to.get_mpq_t(), from, 10) != 0)
    return set_special<Policy>(to, V_CVT_STR_UNK);
  to.canonicalize();
  return V_EQ;
}

template <typename Policy>
inline Result
to_c_string_mpq(char* str, size_t size, const mpq_class& from, const Numeric_Format&, const Rounding&) {
  std::string s = from.get_str();
  strncpy(str, s.c_str(), size);
  return V_EQ;
}

SPECIALIZE_FROM_C_STRING(mpq, mpq_class)
SPECIALIZE_TO_C_STRING(mpq, mpq_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library


#endif // !defined(PPL_checked_mpq_inlines_hh)
