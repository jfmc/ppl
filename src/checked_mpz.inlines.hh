/* Specialized "checked" functions for GMP's mpz_class numbers.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_checked_mpz_inlines_hh
#define PPL_checked_mpz_inlines_hh 1

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy>
inline Result
round_lt_mpz(mpz_class& to, Rounding_Dir dir) {
  if (round_down(dir)) {
    --to;
    return V_GT;
  }
  return V_LT;
}

template <typename Policy>
inline Result
round_gt_mpz(mpz_class& to, Rounding_Dir dir) {
  if (round_up(dir)) {
    ++to;
    return V_LT;
  }
  return V_GT;
}

// FIXME: change this when Autoconf will support AC_C_TYPEOF.
#ifdef __GNUC__
//! Type of the _mp_size field of GMP's __mpz_struct.
typedef __typeof__(__mpz_struct()._mp_size) mp_size_field_t;
#else
//! This is assumed to be the type of the _mp_size field of GMP's __mpz_struct.
typedef int mp_size_field_t;
#endif

inline mp_size_field_t
get_mp_size(const mpz_class &v) {
  return v.get_mpz_t()->_mp_size;
}

inline void
set_mp_size(mpz_class &v, mp_size_field_t size) {
  v.get_mpz_t()->_mp_size = size;
}

template <typename Policy>
inline Result
classify_mpz(const mpz_class& v, bool nan, bool inf, bool sign) {
  if (Policy::may_be_nan || Policy::may_be_infinity) {
    mp_size_field_t s = get_mp_size(v);
    if (Policy::may_be_nan
	&& (nan || sign)
	&& s == Limits<mp_size_field_t>::min + 1)
      return VC_NAN;
    if (!inf && !sign)
      return VC_NORMAL;
    if (Policy::may_be_infinity) {
      if (s == Limits<mp_size_field_t>::min)
	return inf ? VC_MINUS_INFINITY : V_LT;
      if (s == Limits<mp_size_field_t>::max)
	return inf ? VC_PLUS_INFINITY : V_GT;
    }
  }
  if (sign)
    return sgn<Policy>(v);
  return VC_NORMAL;
}

SPECIALIZE_CLASSIFY(classify_mpz, mpz_class)

template <typename Policy>
inline bool
is_nan_mpz(const mpz_class& v) {
  return Policy::may_be_nan
    && get_mp_size(v) == Limits<mp_size_field_t>::min + 1;
}

SPECIALIZE_IS_NAN(is_nan_mpz, mpz_class)

template <typename Policy>
inline bool
is_minf_mpz(const mpz_class& v) {
  return Policy::may_be_infinity
    && get_mp_size(v) == Limits<mp_size_field_t>::min;
}

SPECIALIZE_IS_MINF(is_minf_mpz, mpz_class)

template <typename Policy>
inline bool
is_pinf_mpz(const mpz_class& v) {
  return Policy::may_be_infinity
    && get_mp_size(v) == Limits<mp_size_field_t>::max;
}

SPECIALIZE_IS_PINF(is_pinf_mpz, mpz_class)

template <typename Policy>
inline bool
is_int_mpz(const mpz_class& v) {
  return !is_nan<Policy>(v);
}

SPECIALIZE_IS_INT(is_int_mpz, mpz_class)

template <typename Policy>
inline Result
set_special_mpz(mpz_class& v, Result r) {
  Result c = classify(r);
  if (Policy::may_be_nan && c == VC_NAN)
    set_mp_size(v, Limits<mp_size_field_t>::min + 1);
  else if (Policy::may_be_infinity) {
    switch (c) {
    case VC_MINUS_INFINITY:
      set_mp_size(v, Limits<mp_size_field_t>::min);
      break;
    case VC_PLUS_INFINITY:
      set_mp_size(v, Limits<mp_size_field_t>::max);
      break;
    default:
      break;
    }
  }
  return r;
}

SPECIALIZE_SET_SPECIAL(set_special_mpz, mpz_class)

template <typename To_Policy, typename From_Policy>
inline void
copy_mpz(mpz_class& to, const mpz_class& from) {
  if (is_nan_mpz<From_Policy>(from))
    assert(To_Policy::may_be_nan);
  else if (is_minf_mpz<From_Policy>(from) || is_pinf_mpz<From_Policy>(from))
    assert(To_Policy::may_be_infinity);
  else {
    to = from;
    return;
  }
  set_mp_size(to, get_mp_size(from));
}

SPECIALIZE_COPY(copy_mpz, mpz_class)

template <typename To_Policy, typename From_Policy, typename From>
inline Result
construct_mpz_base(mpz_class& to, const From from, Rounding_Dir) {
    new (&to) mpz_class(from);
    return V_EQ;
}

SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, signed char)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, signed short)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, signed int)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, signed long)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, unsigned char)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, unsigned short)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, unsigned int)
SPECIALIZE_CONSTRUCT(construct_mpz_base, mpz_class, unsigned long)

SPECIALIZE_ASSIGN(assign_same, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy, typename From>
inline Result
assign_mpz_base(mpz_class& to, const From from, Rounding_Dir) {
    to = static_cast<signed long>(from);
    return V_EQ;
}

SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, signed char)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, signed short)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, signed int)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, signed long)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, unsigned char)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, unsigned short)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, unsigned int)
SPECIALIZE_ASSIGN(assign_mpz_base, mpz_class, unsigned long)

template <typename To_Policy, typename From_Policy, typename From>
inline Result
assign_mpz_signed_int(mpz_class& to, const From from, Rounding_Dir) {
  if (sizeof(From) <= sizeof(signed long))
    to = static_cast<signed long>(from);
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

SPECIALIZE_ASSIGN(assign_mpz_signed_int, mpz_class, signed long long)

template <typename To_Policy, typename From_Policy, typename From>
inline Result
assign_mpz_unsigned_int(mpz_class& to, const From from, Rounding_Dir) {
  if (sizeof(From) <= sizeof(unsigned long))
    to = static_cast<unsigned long>(from);
  else
    mpz_import(to.get_mpz_t(), 1, 1, sizeof(From), 0, 0, &from);
  return V_EQ;
}

SPECIALIZE_ASSIGN(assign_mpz_unsigned_int, mpz_class, unsigned long long)

template <typename To_Policy, typename From_Policy, typename From>
inline Result
assign_mpz_float(mpz_class& to, const From from, Rounding_Dir dir) {
  if (is_nan<From_Policy>(from))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(from))
    return assign<To_Policy, void>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(from))
    return assign<To_Policy, void>(to, PLUS_INFINITY, dir);
  if (round_ignore(dir)) {
    to = from;
    return V_LGE;
  }
  From n = rint(from);
  to = n;
  if (from < n)
    return round_lt_mpz<To_Policy>(to, dir);
  else if (from > n)
    return round_gt_mpz<To_Policy>(to, dir);
  else
    return V_EQ;
}

SPECIALIZE_ASSIGN(assign_mpz_float, mpz_class, float)
SPECIALIZE_ASSIGN(assign_mpz_float, mpz_class, double)

template <typename To_Policy, typename From_Policy>
inline Result
assign_mpz_mpq(mpz_class& to, const mpq_class& from, Rounding_Dir dir) {
  if (round_ignore(dir)) {
    to = from;
    return V_LGE;
  }
  mpz_srcptr n = from.get_num().get_mpz_t();
  mpz_srcptr d = from.get_den().get_mpz_t();
  if (round_down(dir)) {
    mpz_fdiv_q(to.get_mpz_t(), n, d);
    return mpz_divisible_p(n, d) ? V_EQ : V_GT;
  }
  else {
    assert(round_up(dir));
    mpz_cdiv_q(to.get_mpz_t(), n, d);
    return mpz_divisible_p(n, d) ? V_EQ : V_LT;
  }
}

SPECIALIZE_ASSIGN(assign_mpz_mpq, mpz_class, mpq_class)

template <typename To_Policy, typename From_Policy, typename To>
inline Result
assign_mp_minf(To& to, const Minus_Infinity&, Rounding_Dir) {
  if (To_Policy::may_be_infinity) {
    set_special<To_Policy>(to, VC_MINUS_INFINITY);
    return V_EQ;
  }
  return VC_MINUS_INFINITY;
}

template <typename To_Policy, typename From_Policy, typename To>
inline Result
assign_mp_pinf(To& to, const Plus_Infinity&, Rounding_Dir) {
  if (To_Policy::may_be_infinity) {
    set_special<To_Policy>(to, VC_PLUS_INFINITY);
    return V_EQ;
  }
  return VC_PLUS_INFINITY;
}

template <typename To_Policy, typename From_Policy, typename To>
inline Result
assign_mp_nan(To& to, const Not_A_Number&, Rounding_Dir) {
  if (To_Policy::may_be_nan) {
    set_special<To_Policy>(to, VC_NAN);
    return V_EQ;
  }
  return VC_NAN;
}

SPECIALIZE_ASSIGN(assign_mp_minf, mpz_class, Minus_Infinity)
SPECIALIZE_ASSIGN(assign_mp_pinf, mpz_class, Plus_Infinity)
SPECIALIZE_ASSIGN(assign_mp_nan, mpz_class, Not_A_Number)
SPECIALIZE_ASSIGN(assign_mp_minf, mpq_class, Minus_Infinity)
SPECIALIZE_ASSIGN(assign_mp_pinf, mpq_class, Plus_Infinity)
SPECIALIZE_ASSIGN(assign_mp_nan, mpq_class, Not_A_Number)

template <typename To_Policy, typename From_Policy>
inline Result
floor_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir) {
  to = from;
  return V_EQ;
}

SPECIALIZE_FLOOR(floor_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
ceil_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir) {
  to = from;
  return V_EQ;
}

SPECIALIZE_CEIL(ceil_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
trunc_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir) {
  to = from;
  return V_EQ;
}

SPECIALIZE_TRUNC(trunc_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
neg_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir) {
  mpz_neg(to.get_mpz_t(), from.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_NEG(neg_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
add_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  to = x + y;
  return V_EQ;
}

SPECIALIZE_ADD(add_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
sub_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  to = x - y;
  return V_EQ;
}

SPECIALIZE_SUB(sub_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  to = x * y;
  return V_EQ;
}

SPECIALIZE_MUL(mul_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
div_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y,
	Rounding_Dir dir) {
  if (CHECK_P(To_Policy::check_div_zero, ::sgn(y) == 0))
    return set_special<To_Policy>(to, V_DIV_ZERO);
  mpz_srcptr n = x.get_mpz_t();
  mpz_srcptr d = y.get_mpz_t();
  if (round_ignore(dir)) {
    mpz_divexact(to.get_mpz_t(), n, d);
    return V_LGE;
  }
  if (round_down(dir)) {
    mpz_fdiv_q(to.get_mpz_t(), n, d);
    return mpz_divisible_p(n, d) ? V_EQ : V_GT;
  }
  else {
    assert(round_up(dir));
    mpz_cdiv_q(to.get_mpz_t(), n, d);
    return mpz_divisible_p(n, d) ? V_EQ : V_LT;
  }
}

SPECIALIZE_DIV(div_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
rem_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  if (CHECK_P(To_Policy::check_div_zero, ::sgn(y) == 0))
    return set_special<To_Policy>(to, V_MOD_ZERO);
  to = x % y;
  return V_EQ;
}

SPECIALIZE_REM(rem_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
mul2exp_mpz(mpz_class& to, const mpz_class& x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return div2exp<To_Policy, From_Policy>(to, x, -exp, dir);
  mpz_mul_2exp(to.get_mpz_t(), x.get_mpz_t(), exp);
  return V_EQ;
}

SPECIALIZE_MUL2EXP(mul2exp_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
div2exp_mpz(mpz_class& to, const mpz_class& x, int exp, Rounding_Dir dir) {
  if (exp < 0)
    return mul2exp<To_Policy, From_Policy>(to, x, -exp, dir);
  mpz_srcptr n = x.get_mpz_t();
  if (round_ignore(dir)) {
    mpz_tdiv_q_2exp(to.get_mpz_t(), x.get_mpz_t(), exp);
    return V_LGE;
  }
  if (round_down(dir)) {
    mpz_fdiv_q_2exp(to.get_mpz_t(), n, exp);
    return mpz_divisible_2exp_p(n, exp) ? V_EQ : V_GT;
  }
  else {
    assert(round_up(dir));
    mpz_cdiv_q_2exp(to.get_mpz_t(), n, exp);
    return mpz_divisible_2exp_p(n, exp) ? V_EQ : V_LT;
  }
}

SPECIALIZE_DIV2EXP(div2exp_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
abs_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir) {
  to = abs(from);
  return V_EQ;
}

SPECIALIZE_ABS(abs_mpz, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
add_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y,
	    Rounding_Dir) {
  mpz_addmul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_ADD_MUL(add_mul_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
sub_mul_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y,
	    Rounding_Dir) {
  mpz_submul(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_SUB_MUL(sub_mul_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
gcd_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  mpz_gcd(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_GCD(gcd_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
gcdext_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y,
	   mpz_class& s, mpz_class& t, Rounding_Dir) {
  mpz_gcdext(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t(),
	     s.get_mpz_t(), t.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_GCDEXT(gcdext_mpz, mpz_class, mpz_class, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From1_Policy, typename From2_Policy>
inline Result
lcm_mpz(mpz_class& to, const mpz_class& x, const mpz_class& y, Rounding_Dir) {
  mpz_lcm(to.get_mpz_t(), x.get_mpz_t(), y.get_mpz_t());
  return V_EQ;
}

SPECIALIZE_LCM(lcm_mpz, mpz_class, mpz_class, mpz_class)

template <typename To_Policy, typename From_Policy>
inline Result
sqrt_mpz(mpz_class& to, const mpz_class& from, Rounding_Dir dir) {
  if (CHECK_P(To_Policy::check_sqrt_neg, from < 0))
    return set_special<To_Policy>(to, V_SQRT_NEG);
  if (round_ignore(dir)) {
    to = sqrt(from);
    return V_GE;
  }
  mpz_class r;
  mpz_sqrtrem(to.get_mpz_t(), r.get_mpz_t(), from.get_mpz_t());
  if (r == 0)
    return V_EQ;
  return round_gt_mpz<To_Policy>(to, dir);
}

SPECIALIZE_SQRT(sqrt_mpz, mpz_class, mpz_class)

template <typename Policy, typename Type>
inline Result
sgn_mp(const Type& x) {
  int i = ::sgn(x);
  return i > 0 ? V_GT : i == 0 ? V_EQ : V_LT;
}

SPECIALIZE_SGN(sgn_mp, mpz_class)
SPECIALIZE_SGN(sgn_mp, mpq_class)

template <typename Policy1, typename Policy2, typename Type>
inline Result
cmp_mp(const Type& x, const Type& y) {
  int i = ::cmp(x, y);
  return i > 0 ? V_GT : i == 0 ? V_EQ : V_LT;
}

SPECIALIZE_CMP(cmp_mp, mpz_class, mpz_class)
SPECIALIZE_CMP(cmp_mp, mpq_class, mpq_class)

template <typename Policy>
inline Result
output_mpz(std::ostream& os, const mpz_class& from, const Numeric_Format&,
	   Rounding_Dir) {
  os << from;
  return V_EQ;
}

SPECIALIZE_INPUT(input_generic, mpz_class)
SPECIALIZE_OUTPUT(output_mpz, mpz_class)

inline memory_size_type
external_memory_in_bytes(const mpz_class& x) {
  return x.get_mpz_t()[0]._mp_alloc * SIZEOF_MP_LIMB_T;
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_checked_mpz_inlines_hh)
