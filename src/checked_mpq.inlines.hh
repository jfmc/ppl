/* Specialized checked functions for GMP mpq
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

#include "checked.defs.hh"
#include "checked_mpz.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename From>
inline Result_Info
checked_assign_mpq_signed_int(mpq_class& to, From from) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else {
    mpz_ptr m = to.get_num().get_mpz_t();
    if (from >= 0)
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
    else {
      from = -from;
      mpz_import(m, 1, 1, sizeof(From), 0, 0, &from);
      mpz_neg(m, m);
    }
    to.get_den() = 1;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, int8_t)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, int16_t)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, int32_t)
SPECIALIZE_ASSIGN(mpq_signed_int, mpq_class, int64_t)

template <typename From>
inline Result_Info
checked_assign_mpq_unsigned_int(mpq_class& to, From from) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else {
    mpz_import(to.get_num().get_mpz_t(), 1, 1, sizeof(From), 0, 0, &from);
    to.get_den() = 1;
  }
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, u_int8_t)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, u_int16_t)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, u_int32_t)
SPECIALIZE_ASSIGN(mpq_unsigned_int, mpq_class, u_int64_t)

template <typename To>
inline Result_Info
checked_assign_int_mpq(To& to, const mpq_class& from) {
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  mpz_t q;
  mpz_init(q);
  mpz_divexact(q, n, d);
  return checked_assign(to, q);
}

template <typename To>
inline Result_Info
checked_assign_int_mpq_check_inexact(To& to, const mpq_class& from) {
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  mpz_t q, r;
  mpz_init(q);
  mpz_init(r);
  mpz_tdiv_qr(q, r, n, d);
  Result_Info ret = checked_assign(to, q);
  if (ret != V_EQ)
    return ret;
  switch (mpz_sgn(r)) {
  case -1:
    return V_LT;
  case 1:
    return V_GT;
  default:
    return V_EQ;
  }
}

SPECIALIZE_ASSIGN_INEXACT(int_mpq, int8_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, int16_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, int32_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, int64_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, u_int8_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, u_int16_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, u_int32_t, const mpq_class&)
SPECIALIZE_ASSIGN_INEXACT(int_mpq, u_int64_t, const mpq_class&)

template <typename From>
inline Result_Info
checked_assign_mpq_float(mpq_class& to, From from) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_float, mpq_class, float)
SPECIALIZE_ASSIGN(mpq_float, mpq_class, double)

inline Result_Info 
checked_pred(mpq_class&) {
  throw 0;
}

inline Result_Info 
checked_succ(mpq_class&) {
  throw 0;
}

inline Result_Info 
checked_neg(mpq_class& to, const mpq_class& from) {
  mpq_neg(to.get_mpq_t(), from.get_mpq_t());
  return V_EQ;
}

inline Result_Info 
checked_add(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x + y;
  return V_EQ;
}

inline Result_Info 
checked_sub(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x - y;
  return V_EQ;
}

inline Result_Info 
checked_mul(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x * y;
  return V_EQ;
}

inline Result_Info 
checked_div(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x / y;
  return V_EQ;
}

inline Result_Info 
checked_mod(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x / y;
  to.get_num() %= to.get_den();
  return V_EQ;
}

template <>
inline Result_Info
checked_abs(mpq_class& to, const mpq_class& from)
{
  to = abs(from);
  return V_EQ;
}

} // namespace Parma_Polyhedra_Library

inline void
std::swap(mpq_class& x, mpq_class& y)
{
  mpq_swap(x.get_mpq_t(), y.get_mpq_t());
}
