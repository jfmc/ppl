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

#include <limits>
#include <cmath>
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

inline Result_Info
checked_assign_mpz_mpq(mpz_class& to, const mpq_class& from) {
  to = from;
  return V_EQ;
}

inline Result_Info
checked_assign_mpz_mpq_check_inexact(mpz_class& to, const mpq_class& from) {
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
  default:
    return V_EQ;
  }
}

SPECIALIZE_ASSIGN_INEXACT(mpz_mpq, mpz_class, const mpq_class&)

template <typename From>
inline Result_Info
checked_assign_mpz_signed_int(mpz_class& to, From from) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else {
    mpz_ptr m = to.get_mpz_t();
    if (from >= 0)
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
    else {
      from = -from;
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
      mpz_neg(m, m);
    }
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int8_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int16_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int32_t)
SPECIALIZE_ASSIGN(mpz_signed_int, mpz_class, int64_t)

template <typename From>
inline Result_Info
checked_assign_mpz_unsigned_int(mpz_class& to, From from) {
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


template <typename From>
inline Result_Info
checked_assign_mpz_float(mpz_class& to, From from) {
  to = from;
  return V_EQ;
}

template <typename From>
inline Result_Info
checked_assign_mpz_float_check_inexact(mpz_class& to, From from) {
  double n = rint(from);
  to = n;
  if (from < to)
    return V_LT;
  if (from > to)
    return V_GT;
  return V_EQ;
}

SPECIALIZE_ASSIGN_INEXACT(mpz_float, mpz_class, float)
SPECIALIZE_ASSIGN_INEXACT(mpz_float, mpz_class, double)

inline Result_Info 
checked_pred(mpz_class& to) {
  --to;
  return V_EQ;
}

inline Result_Info 
checked_succ(mpz_class& to) {
  ++to;
  return V_EQ;
}

inline Result_Info 
checked_neg(mpz_class& to, const mpz_class& from) {
  mpz_neg(to.get_mpz_t(), from.get_mpz_t());
  return V_EQ;
}

inline Result_Info 
checked_add(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x + y;
  return V_EQ;
}

inline Result_Info 
checked_sub(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x - y;
  return V_EQ;
}

inline Result_Info 
checked_mul(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x * y;
  return V_EQ;
}

inline Result_Info 
checked_div_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  mpz_divexact(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

inline Result_Info 
checked_div_mpz_check_inexact(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  if (sgn(y) == 0)
    return V_NAN;
  mpz_t r;
  mpz_init(r);
  mpz_tdiv_qr(to.get_mpz_t(), r, x.get_mpz_t(), y.get_mpz_t());
  switch (mpz_sgn(r)) {
  case -1:
    return V_LT;
  case 0:
    return V_EQ;
  case 1:
    return V_GT;
  }
}

SPECIALIZE_DIV_INEXACT(mpz, mpz_class, const mpz_class&)

inline Result_Info 
checked_mod(mpz_class& to, const mpz_class& x, const mpz_class& y) {
  to = x % y;
  return V_EQ;
}

inline Result_Info
checked_assign(mpq_class& to, const mpz_class& from) {
  to = from;
  return V_EQ;
}

inline Result_Info
checked_assign(long& to, const mpz_class& from)
{
  if (from.fits_slong_p()) {
    to = from.get_si();
    return V_EQ;
  }
  return sgn(from) < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
}

inline Result_Info
checked_assign(unsigned long& to, const mpz_class& from)
{
  if (from.fits_ulong_p()) {
    to = from.get_ui();
    return V_EQ;
  }
  return sgn(from) < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
}

template <typename To>
inline Result_Info
checked_assign_signed_int_mpz(To& to, const mpz_class& from)
{
  if (from.fits_slong_p()) {
    long v = from.get_si();
    if (v < std::numeric_limits<To>::min())
      return V_NEG_OVERFLOW;
    if (v > std::numeric_limits<To>::max())
      return V_POS_OVERFLOW;
    to = v;
    return V_EQ;
  }
  return sgn(from) < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
}

SPECIALIZE_ASSIGN(signed_int_mpz, signed char, const mpz_class&)
SPECIALIZE_ASSIGN(signed_int_mpz, short, const mpz_class&)
SPECIALIZE_ASSIGN(signed_int_mpz, int, const mpz_class&)

template <typename To>
inline Result_Info
checked_assign_unsigned_int_mpz(To& to, const mpz_class& from)
{
  if (from.fits_ulong_p()) {
    unsigned long v = from.get_ui();
    if (v > std::numeric_limits<To>::max())
      return V_POS_OVERFLOW;
    to = v;
    return V_EQ;
  }
  return sgn(from) < 0 ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
}

SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned char, const mpz_class&)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned short, const mpz_class&)
SPECIALIZE_ASSIGN(unsigned_int_mpz, unsigned int, const mpz_class&)

template <>
inline Result_Info
checked_abs(mpz_class& to, const mpz_class& from)
{
  to = abs(from);
  return V_EQ;
}

template <>
inline Result_Info
checked_gcd(mpz_class& to, const mpz_class& x, const mpz_class& y)
{
  mpz_gcd(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

template <>
inline Result_Info
checked_lcm(mpz_class& to, const mpz_class& x, const mpz_class& y)
{
  mpz_lcm(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

template <>
inline Result_Info
checked_sqrt(mpz_class& to, const mpz_class& from)
{
  if (from < 0)
    return V_NAN;
  mpz_class r;
  mpz_sqrtrem(to.get_mpz_t(), r.get_mpz_t(), from.get_mpz_t());
  if (r > 0)
    return V_GT;
  if (r < 0)
    return V_LT;
  return V_EQ;
}

template <>
inline Result_Info
checked_sqrtexact(mpz_class& to, const mpz_class& from)
{
  to = sqrt(from);
  return V_EQ;
}

} // namespace Parma_Polyhedra_Library

inline void
std::swap(mpz_class& x, mpz_class& y)
{
  mpz_swap(x.get_mpz_t(), y.get_mpz_t());
}
