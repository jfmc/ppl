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

#include <limits>
#include <cmath>
#include <gmpxx.h>

namespace Parma_Polyhedra_Library {

namespace Checked {

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
  } else {
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

SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int8_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int16_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int32_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int64_t)

template <typename Policy, typename From>
inline Result
assign_mpz_unsigned_int(mpz_class& to, const From from) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else
    mpz_import(to.get_mpz_t(), 1, 1, sizeof(From), 0, 0, &from);
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, u_int8_t)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, u_int16_t)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, u_int32_t)
SPECIALIZE_ASSIGN(mpz_unsigned_int, mpz_class, u_int64_t)


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
  } else {
    to = from;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_float, mpz_class, float)
SPECIALIZE_ASSIGN(mpz_float, mpz_class, double)

template <typename Policy>
inline Result 
pred_mpz(mpz_class& to) {
  --to;
  return V_EQ;
}

SPECIALIZE_PRED(mpz, mpz_class)

template <typename Policy>
inline Result 
succ_mpz(mpz_class& to) {
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
    return V_NAN;
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
  } else {
    mpz_divexact(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  }
  return V_EQ;
}

SPECIALIZE_DIV(mpz, mpz_class, mpz_class)

template <typename Policy>
inline Result 
mod_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  if (Policy::check_divbyzero && sgn(y) == 0)
    return V_NAN;
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
      if (v < std::numeric_limits<To>::min())
	return V_NEG_OVERFLOW;
      if (v > std::numeric_limits<To>::max())
	return V_POS_OVERFLOW;
      to = v;
      return V_EQ;
    }
  } else {
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

SPECIALIZE_ASSIGN(signed_int_mpz, int8_t, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, int16_t, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, int32_t, mpz_class)
SPECIALIZE_ASSIGN(signed_int_mpz, int64_t, mpz_class)

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
      if (v > std::numeric_limits<To>::max())
	return V_POS_OVERFLOW;
      to = v;
      return V_EQ;
    }
  } else {
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

SPECIALIZE_ASSIGN(unsigned_int_mpz, u_int8_t, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, u_int16_t, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, u_int32_t, mpz_class)
SPECIALIZE_ASSIGN(unsigned_int_mpz, u_int64_t, mpz_class)

template <typename Policy>
inline Result 
abs_mpz(mpz_class& to, const mpz_class& from) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(mpz, mpz_class, mpz_class)

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
    return V_NAN;
  if (Policy::check_inexact) {
    mpz_class r;
    mpz_sqrtrem(to.get_mpz_t(), r.get_mpz_t(), from.get_mpz_t());
    if (r > 0)
      return V_GT;
    if (r < 0)
      return V_LT;
  } else {
    to = sqrt(from);
  }
  return V_EQ;
}

SPECIALIZE_SQRT(mpz, mpz_class, mpz_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library

namespace std {

inline void
swap(mpz_class& x, mpz_class& y) {
  mpz_swap(x.get_mpz_t(), y.get_mpz_t());
}

} // namespace std

#endif // !defined(PPL_checked_mpz_inlines_hh)
