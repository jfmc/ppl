/* Specialized "checked" functions for GMP's mpz_class numbers.
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

#ifndef PPL_checked_mpz_inlines_hh
#define PPL_checked_mpz_inlines_hh 1

#include "Limits.hh"
#include <gmpxx.h>
#include <cmath>

namespace Parma_Polyhedra_Library {

namespace Checked {

inline mp_size_t
get_mp_size(const mpz_class &v) {
  return v.get_mpz_t()->_mp_size;
}

inline void
set_mp_size(mpz_class &v, mp_size_t size) {
  v.get_mpz_t()->_mp_size = size;
}

template <typename Policy>
inline Result
classify_mpz(const mpz_class& v, bool nan, bool inf, bool sign) {
  if (Policy::store_nan || Policy::store_infinity) {
    mp_size_t s = get_mp_size(v);
    if (Policy::store_nan && (nan || sign) && s == Limits<mp_size_t>::min + 1)
      return V_UNKNOWN;
    if (!inf && !sign)
      return V_NORMAL;
    if (Policy::store_infinity) {
      if (s == Limits<mp_size_t>::min)
	return inf ? V_MINUS_INFINITY : V_LT;
      if (s == Limits<mp_size_t>::max)
	return inf ? V_PLUS_INFINITY : V_GT;
    }
  }
  if (sign)
    return sgn<Policy>(v);
  return V_NORMAL;
}

SPECIALIZE_CLASSIFY(mpz, mpz_class)

template <typename Policy>
inline Result
set_special_mpz(mpz_class& v, Result r) {
  mp_size_t s;
  Result t = type(r);
  if (Policy::store_nan && t == V_UNKNOWN)
    s = Limits<mp_size_t>::min + 1;
  else if (Policy::store_infinity) {
    switch (t) {
    case V_MINUS_INFINITY:
      s = Limits<mp_size_t>::min;
      break;
    case V_PLUS_INFINITY:
      s = Limits<mp_size_t>::max;
      break;
    default:
      return r;
    }
  }
  else
    return r;
  set_mp_size(v, s);
  return r;
}

SPECIALIZE_SET_SPECIAL(mpz, mpz_class)

template <typename Policy>
inline Result
pred_mpz(mpz_class& to) {
  assert(classify<Policy>(to, true, true, false) == V_NORMAL);
  --to;
  return V_NORMAL;
}

SPECIALIZE_PRED(mpz, mpz_class)

template <typename Policy>
inline Result
succ_mpz(mpz_class& to) {
  assert(classify<Policy>(to, true, true, false) == V_NORMAL);
  ++to;
  return V_NORMAL;
}

SPECIALIZE_SUCC(mpz, mpz_class)

template <typename Policy>
inline Result
assign_mpz_mpq(mpz_class& to, const mpq_class& from, const Rounding& mode) {
  if (Policy::round_inexact) {
    mpz_srcptr n = from.get_num().get_mpz_t();
    mpz_srcptr d = from.get_den().get_mpz_t();
    mpz_t rem;
    mpz_init(rem);
    mpz_tdiv_qr(to.get_mpz_t(), rem, n, d);
    Result r;
    switch (mpz_sgn(rem)) {
    case -1:
      r = V_LT;
      break;
    case 1:
      r = V_GT;
      break;
    default:
      return V_EQ;
    }
    return round<Policy>(to, r, mode);
  }
  else {
    to = from;
    return V_LGE;
  }
}

SPECIALIZE_ASSIGN(mpz_mpq, mpz_class, mpq_class)

template <typename Policy, typename From>
inline Result
assign_mpz_signed_int(mpz_class& to, const From from, const Rounding&) {
  if (sizeof(From) <= sizeof(long))
    to = static_cast<long>(from);
  else {
    mpz_ptr m = to.get_mpz_t();
    if (from >= 0)
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
    else {
      From n = -from;
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &n);
      mpz_neg(m, m);
    }
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, signed char)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, short)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, long)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, long long)

template <typename Policy, typename From>
inline Result
assign_mpz_unsigned_int(mpz_class& to, const From from, const Rounding&) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else
    mpz_import(to.get_mpz_t(), 1, 1, sizeof(From), 0, 0, &from);
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, unsigned char)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, unsigned short)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, unsigned int)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, unsigned long)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, unsigned long long)

template <typename Policy, typename From>
inline Result
assign_mpz_float(mpz_class& to, const From from, const Rounding& mode) {
  if (Policy::round_inexact) {
    double n = rint(from);
    to = n;
    Result r;
    if (from < n)
      r = V_LT;
    else if (from > n)
      r = V_GT;
    else
      return V_EQ;
    return round<Policy>(to, r, mode);
  }
  else {
    to = from;
    return V_LGE;
  }
}

SPECIALIZE_ASSIGN(mpz_float, mpz_class, float)
SPECIALIZE_ASSIGN(mpz_float, mpz_class, double)

template <typename Policy>
inline Result
assign_mpz_c_string(mpz_class& to, c_string from, const Rounding& mode) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_c_string, mpz_class, c_string)

template <typename Policy>
inline Result
neg_mpz(mpz_class& to, const mpz_class& from, const Rounding&) {
  mpz_neg(to.get_mpz_t(), from.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_NEG(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
add_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  to = x + y;
  return V_EQ;
}

SPECIALIZE_ADD(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
sub_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  to = x - y;
  return V_EQ;
}

SPECIALIZE_SUB(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  to = x * y;
  return V_EQ;
}

SPECIALIZE_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
div_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding& mode) {
  if (Policy::check_divbyzero && ::sgn(y) == 0)
    return set_special<Policy>(to, V_UNKNOWN);
  if (Policy::round_inexact) {
    mpz_t rem;
    mpz_init(rem);
    mpz_tdiv_qr(to.get_mpz_t(), rem, x.get_mpz_t(), y.get_mpz_t());
    Result r;
    switch (mpz_sgn(rem)) {
    case -1:
      r = V_LT;
      break;
    case 1:
      r = V_GT;
      break;
    default:
      return V_EQ;
    }
    return round<Policy>(to, r, mode);
  }
  else {
    mpz_divexact(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
    return V_LGE;
  }
}

SPECIALIZE_DIV(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
mod_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  if (Policy::check_divbyzero && ::sgn(y) == 0)
    return set_special<Policy>(to, V_UNKNOWN);
  to = x % y;
  return V_EQ;
}

SPECIALIZE_MOD(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
abs_mpz(mpz_class& to, const mpz_class& from, const Rounding&) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
add_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  mpz_addmul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_ADD_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
sub_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  mpz_submul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_SUB_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
gcd_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  mpz_gcd(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_GCD(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
lcm_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, const Rounding&) {
  mpz_lcm(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_LCM(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result
sqrt_mpz(mpz_class& to, const mpz_class& from, const Rounding& mode) {
  if (Policy::check_sqrt_neg && from < 0)
    return set_special<Policy>(to, V_DOMAIN);
  if (Policy::round_inexact) {
    mpz_class r;
    mpz_sqrtrem(to.get_mpz_t(), r.get_mpz_t(), from.get_mpz_t());
    if (r == 0)
      return V_EQ;
    return round<Policy>(to, V_GT, mode);
  }
  else {
    to = sqrt(from);
    return V_GE;
  }
}

SPECIALIZE_SQRT(mpz, mpz_class, mpz_class)

template <typename Policy, typename Type>
inline Result
sgn_mp(const Type& x) {
  int i = ::sgn(x);
  return i > 0 ? V_GT : i == 0 ? V_EQ : V_LT;
}

SPECIALIZE_SGN(mp, mpz_class)
SPECIALIZE_SGN(mp, mpq_class)

template <typename Policy, typename Type>
inline Result
cmp_mp(const Type& x, const Type& y) {
  int i = ::cmp(x, y);
  return i > 0 ? V_GT : i == 0 ? V_EQ : V_LT;
}

SPECIALIZE_CMP(mp, mpz_class, mpz_class)
SPECIALIZE_CMP(mp, mpq_class, mpq_class)

SPECIALIZE_PRINT(generic, mpz_class)
SPECIALIZE_INPUT(generic, mpz_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_mpz_inlines_hh)
