/* Specialized checked functions for GMP mpz
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

#include <cmath>
#include <gmpxx.h>
#include "Limits.hh"

namespace Parma_Polyhedra_Library {

namespace Checked {

typedef int mpz_size_t;

inline mpz_size_t
get_mpz_size(const mpz_class &v)
{
  return v.get_mpz_t()->_mp_size;
}

inline void
set_mpz_size(mpz_class &v, mpz_size_t size)
{
  v.get_mpz_t()->_mp_size = size;
}

template <typename Policy>
inline Result
value_type_mpz(const mpz_class& v) {
  mpz_size_t s = get_mpz_size(v);
  if (Policy::store_unknown && s == Limits<mpz_size_t>::min + 1)
    return V_UNKNOWN;
  if (Policy::store_overflows) {
    if (s == Limits<mpz_size_t>::min)
      return V_NEG_OVERFLOW;
    if (s == Limits<mpz_size_t>::max)
      return V_POS_OVERFLOW;
  }
  return V_EQ;
}

SPECIALIZE_VALUE_TYPE(mpz, mpz_class)

template <typename Policy>
inline void
set_special_mpz(mpz_class& v, const Result r) {
  if (Policy::store_unknown && (r == V_UNKNOWN || r == V_DOMAIN))
    set_mpz_size(v, Limits<mpz_size_t>::min + 1);
  else if (Policy::store_overflows) {
    if (r == V_NEG_OVERFLOW)
      set_mpz_size(v, Limits<mpz_size_t>::min);
    else if (r == V_POS_OVERFLOW)
      set_mpz_size(v, Limits<mpz_size_t>::max);
  }
}

SPECIALIZE_SET_SPECIAL(mpz, mpz_class)

template <typename Policy>
inline Result
assign_mpz_mpq(mpz_class& to, const mpq_class& from) {
  if (Policy::check_inexact) {
    mpz_srcptr n = from.get_num().get_mpz_t();
    mpz_srcptr d = from.get_den().get_mpz_t();
    mpz_t r;
    mpz_init(r);
    mpz_tdiv_qr(to.get_mpz_t(), r, n, d);
    switch (mpz_sgn(r)) {
    case -1:
      return V_LT;
    case 1:
      return V_GT;
    }
  }
  else {
    to = from;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_mpq, mpz_class, mpq_class)

template <typename Policy, typename From>
inline Result
assign_mpz_signed_int(mpz_class& to, const From from) {
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
assign_mpz_unsigned_int(mpz_class& to, const From from) {
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
assign_mpz_float(mpz_class& to, const From from) {
  if (Policy::check_inexact) {
    double n = rint(from);
    to = n;
    if (from < to)
      return V_LT;
    if (from > to)
      return V_GT;
  }
  else {
    to = from;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_float, mpz_class, float)
SPECIALIZE_ASSIGN(mpz_float, mpz_class, double)

template <typename Policy>
inline Result 
pred_mpz(mpz_class& to) {
  if (value_type<Policy>(to) != V_EQ)
    throw(0);
  --to;
  return V_EQ;
}

SPECIALIZE_PRED(mpz, mpz_class)

template <typename Policy>
inline Result 
succ_mpz(mpz_class& to) {
  if (value_type<Policy>(to) != V_EQ)
    throw(0);
  ++to;
  return V_EQ;
}

SPECIALIZE_SUCC(mpz, mpz_class)

template <typename Policy>
inline Result 
neg_mpz(mpz_class& to, const mpz_class& from) {
  mpz_neg(to.get_mpz_t(), from.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_NEG(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
add_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x + y;
  return V_EQ;
}

SPECIALIZE_ADD(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
sub_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x - y;
  return V_EQ;
}

SPECIALIZE_SUB(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x * y;
  return V_EQ;
}

SPECIALIZE_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
div_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  if (Policy::check_divbyzero && sgn(y) == 0)
    return V_UNKNOWN;
  if (Policy::check_inexact) {
    mpz_t r;
    mpz_init(r);
    mpz_tdiv_qr(to.get_mpz_t(), r, x.get_mpz_t(), y.get_mpz_t());
    switch (mpz_sgn(r)) {
    case -1:
      return V_LT;
    case 1:
      return V_GT;
    }
  }
  else {
    mpz_divexact(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  }
  return V_EQ;
}

SPECIALIZE_DIV(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
mod_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  if (Policy::check_divbyzero && sgn(y) == 0)
    return V_UNKNOWN;
  to = x % y;
  return V_EQ;
}

SPECIALIZE_MOD(mpz, mpz_class, mpz_class)

template <typename Policy, typename To>
inline Result 
assign_signed_int_mpz(To& to, const mpz_class& from) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(long))
      to = from.get_si();
    else {
      To v;
      mpz_export(&v, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
      if (sgn(from) < 0)
	return neg<Policy>(to, v);
      to = v;
    }
    return V_EQ;
  }
  if (sizeof(To) <= sizeof(long)) {
    if (from.fits_slong_p()) {
      long v = from.get_si();
      if (v < Limits<To>::min)
	return V_NEG_OVERFLOW;
      if (v > Limits<To>::max)
	return V_POS_OVERFLOW;
      to = v;
      return V_EQ;
    }
  }
  else {
    mpz_srcptr m = from.get_mpz_t();
    size_t sz = mpz_size(m);
    if (sz <= sizeof(To) / sizeof(mp_limb_t)) {
      if (sz == 0) {
	to = 0;
	return V_EQ;
      }
      To v;
      mpz_export(&v, 0, 1, sizeof(To), 0, 0, m);
      if (v >= 0) {
	if (sgn(from) < 0)
	  return neg<Policy>(to, v);
	to = v;
	return V_EQ;
      }
    }
  }
  return sgn(from) < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
}

SPECIALIZE_ASSIGN(signed_int_mpz, signed char, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, short, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, int, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, long, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, long long, mpz_class)

template <typename Policy, typename To>
inline Result 
assign_unsigned_int_mpz(To& to, const mpz_class& from) {
  if (!Policy::check_overflow) {
    if (sizeof(To) <= sizeof(unsigned long))
      to = from.get_ui();
    else 
      mpz_export(&to, 0, 1, sizeof(To), 0, 0, from.get_mpz_t());
    return V_EQ;
  }
  if (sgn(from) < 0)
    return V_NEG_OVERFLOW;
  if (sizeof(To) <= sizeof(unsigned long)) {
    if (from.fits_ulong_p()) {
      unsigned long v = from.get_ui();
      if (v > Limits<To>::max)
	return V_POS_OVERFLOW;
      to = v;
      return V_EQ;
    }
  }
  else {
    mpz_srcptr m = from.get_mpz_t();
    size_t sz = mpz_size(m);
    if (sz <= sizeof(To) / sizeof(mp_limb_t)) {
      if (sz == 0)
	to = 0;
      else
	mpz_export(&to, 0, 1, sizeof(To), 0, 0, m);
      return V_EQ;
    }
  }
  return V_POS_OVERFLOW;
}

SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned char, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned short, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned int, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned long long, mpz_class)

template <typename Policy>
inline Result 
abs_mpz(mpz_class& to, const mpz_class& from) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
add_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  mpz_addmul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_ADD_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
sub_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  mpz_submul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_SUB_MUL(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
gcd_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  mpz_gcd(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_GCD(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
lcm_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  mpz_lcm(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_LCM(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
sqrt_mpz(mpz_class& to, const mpz_class& from) {
  if (Policy::check_sqrt_neg && from < 0)
    return V_DOMAIN;
  if (Policy::check_inexact) {
    mpz_class r;
    mpz_sqrtrem(to.get_mpz_t(), r.get_mpz_t(), from.get_mpz_t());
    if (r > 0)
      return V_GT;
    if (r < 0)
      return V_LT;
  }
  else {
    to = sqrt(from);
  }
  return V_EQ;
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

SPECIALIZE_CMP(mp, mpz_class)
SPECIALIZE_CMP(mp, mpq_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_mpz_inlines_hh)
