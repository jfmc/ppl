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

#ifndef PPL_checked_mpq_inlines_hh
#define PPL_checked_mpq_inlines_hh 1

#include <gmpxx.h>
#include "checked_mpz.inlines.hh"

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy>
inline Result 
assign_mpq_mpz(mpq_class& to, const mpz_class& from) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_mpz, mpq_class, mpz_class)

template <typename Policy, typename From>
inline Result
assign_mpq_signed_int(mpq_class& to, const From from) {
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
assign_mpq_unsigned_int(mpq_class& to, const From from) {
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

template <typename Policy, typename To>
inline Result
assign_int_mpq(To& to, const mpq_class& from) {
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  mpz_t q, r;
  mpz_init(q);
  if (Policy::check_inexact) {
    mpz_init(r);
    mpz_tdiv_qr(q, r, n, d);
  } else {
    mpz_divexact(q, n, d);
  }
  Result ret = assign<Policy>(to, q);
  if (Policy::check_inexact && ret == V_EQ) {
    switch (mpz_sgn(r)) {
    case -1:
      return V_LT;
    case 1:
      return V_GT;
    }
  }
  return ret;
}

SPECIALIZE_ASSIGN(int_mpq, int8_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, int16_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, int32_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, int64_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, u_int8_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, u_int16_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, u_int32_t, mpq_class)
SPECIALIZE_ASSIGN(int_mpq, u_int64_t, mpq_class)

template <typename Policy, typename From>
inline Result
assign_mpq_float(mpq_class& to, const From from) {
  to = from;
  return V_EQ;
}

SPECIALIZE_ASSIGN(mpq_float, mpq_class, float)
SPECIALIZE_ASSIGN(mpq_float, mpq_class, double)

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
neg_mpq(mpq_class& to, const mpq_class& from) {
  mpq_neg(to.get_mpq_t(), from.get_mpq_t());
  return V_EQ;
}

SPECIALIZE_NEG(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result 
add_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x + y;
  return V_EQ;
}

SPECIALIZE_ADD(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result 
sub_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x - y;
  return V_EQ;
}

SPECIALIZE_SUB(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result 
mul_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x * y;
  return V_EQ;
}

SPECIALIZE_MUL(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result 
div_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x / y;
  return V_EQ;
}

SPECIALIZE_DIV(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result 
mod_mpq(mpq_class& to, const mpq_class& x, const mpq_class& y) {
  to = x / y;
  to.get_num() %= to.get_den();
  return V_EQ;
}

SPECIALIZE_MOD(mpq, mpq_class, mpq_class)

template <typename Policy>
inline Result
abs_mpq(mpq_class& to, const mpq_class& from) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(mpq, mpq_class, mpq_class)

} // namespace Checked

} // namespace Parma_Polyhedra_Library


namespace std {

inline void
swap(mpq_class& x, mpq_class& y) {
  mpq_swap(x.get_mpq_t(), y.get_mpq_t());
}

} // namespace std

#endif // !defined(PPL_checked_mpq_inlines_hh)
