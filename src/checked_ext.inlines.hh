/* Checked extended arithmetic functions.
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

namespace Parma_Polyhedra_Library {

namespace Checked {

#define handle_ext_natively(Type) (Float<Type>::fpu_related)

template <typename Policy, typename Type>
inline Result
sgn_ext(const Type& x) {
  if (is_nan<Policy>(x))
    return VC_NAN;
  else if (is_minf<Policy>(x))
    return V_LT;
  else if (is_pinf<Policy>(x))
    return V_GT;
  else
    return sgn<Policy>(x);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
assign_ext(To& to, const From& from, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return assign<To_Policy>(to, from, dir);
  if (is_nan<From_Policy>(from))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(from))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(from))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return assign<To_Policy>(to, from, dir);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
neg_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return neg<To_Policy>(to, x, dir);
  if (is_nan<From_Policy>(x))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else
    return neg<To_Policy>(to, x, dir);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
abs_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return abs<To_Policy>(to, x, dir);
  if (is_nan<From_Policy>(x))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x) || is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return abs<To_Policy>(to, x, dir);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return add<To_Policy>(to, x, y, dir);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_ADD_INF);
    else
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_ADD_INF);
    else
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
  }
  else {
    if (is_minf<From2_Policy>(y))
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
    else if (is_pinf<From2_Policy>(y)) 
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
    else
      return add<To_Policy>(to, x, y, dir);
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return sub<To_Policy>(to, x, y, dir);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_SUB_INF);
    else
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_SUB_INF);
    else
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
  }
  else {
    if (is_minf<From2_Policy>(y))
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
    else if (is_pinf<From2_Policy>(y)) 
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
    else
      return sub<To_Policy>(to, x, y, dir);
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return mul<To_Policy>(to, x, y, dir);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
    case V_GT:
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
    default:
      return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
      case V_LT:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      case V_GT:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    }
  }
  else {
    if (is_minf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      case V_GT:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    } else if (is_pinf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      case V_GT:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    }
    else
      return mul<To_Policy>(to, x, y, dir);
  }
}
    
// FIXME: optimize, remove classify, permit specialization of set_special
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  Result rto;
  Result rx;
  Result ry;
  Result r;
  if ((rto = classify<To_Policy>(to, true, true, false)) == VC_NAN
      || (rx = classify<From1_Policy>(x, true, true, true)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, true)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (!(is_special(rto) || is_special(rx) || is_special(ry)))
    return add_mul<To_Policy>(to, x, y, dir);
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    r = rto;
  else {
    rx = sign(rx);
    ry = sign(ry);
    if (rx == V_EQ || ry == V_EQ)
      return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    else if (rx == ry)
      r = rto == VC_MINUS_INFINITY ? V_INF_ADD_INF : VC_PLUS_INFINITY;
    else
      r = rto == VC_PLUS_INFINITY ? V_INF_ADD_INF : VC_MINUS_INFINITY;
  }
  return set_special<To_Policy>(to, r);
}

// FIXME: optimize, remove classify, permit specialization of set_special
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  Result rto;
  Result rx;
  Result ry;
  Result r;
  if ((rto = classify<To_Policy>(to, true, true, false)) == VC_NAN
      || (rx = classify<From1_Policy>(x, true, true, true)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, true)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (!(is_special(rto) || is_special(rx) || is_special(ry)))
    return sub_mul<To_Policy>(to, x, y, dir);
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    r = rto;
  else {
    rx = sign(rx);
    ry = sign(ry);
    if (rx == V_EQ || ry == V_EQ)
      return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    else if (rx == ry)
      r = rto == VC_PLUS_INFINITY ? V_INF_SUB_INF : VC_MINUS_INFINITY;
    else
      r = rto == VC_MINUS_INFINITY ? V_INF_SUB_INF : VC_PLUS_INFINITY;
  }
  return set_special<To_Policy>(to, r);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
div_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return div<To_Policy>(to, x, y, dir);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_DIV_INF);
    else {
      switch (sgn<From2_Policy>(y)) {
      case V_LT:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      case V_GT:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      default:
	return set_special<To_Policy>(to, V_DIV_ZERO);
      }
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_DIV_INF);
    else {
      switch (sgn<From2_Policy>(y)) {
      case V_LT:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      case V_GT:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      default:
	return set_special<To_Policy>(to, V_DIV_ZERO);
      }
    }
  }
  else {
    if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y)) {
      to = 0;
      return V_EQ;
    }
    else
      return div<To_Policy>(to, x, y, dir);
  }
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
rem_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return rem<To_Policy>(to, x, y, dir);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x) || is_pinf<From1_Policy>(x))
    return set_special<To_Policy>(to, V_INF_MOD);
  else {
    if (is_minf<From1_Policy>(y) || is_pinf<From2_Policy>(y)) {
      to = x;
      return V_EQ;
    }
    else
      return rem<To_Policy>(to, x, y, dir);
  }
}
    
template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
mul2exp_ext(To& to, const From& x, int exp, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return mul2exp<To_Policy>(to, x, exp, dir);
  if (is_nan<From_Policy>(x))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return mul2exp<To_Policy>(to, x, exp, dir);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
div2exp_ext(To& to, const From& x, int exp, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return div2exp<To_Policy>(to, x, exp, dir);
  if (is_nan<From_Policy>(x))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return div2exp<To_Policy>(to, x, exp, dir);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
sqrt_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return sqrt<To_Policy>(to, x, dir);
  if (is_nan<From_Policy>(x))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return set_special<To_Policy>(to, V_SQRT_NEG);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return sqrt<To_Policy>(to, x, dir);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
gcd_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x) || is_pinf<From1_Policy>(x))
    return abs_ext<To_Policy, From2_Policy>(to, y, dir);
  else if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))
    return abs_ext<To_Policy, From1_Policy>(to, x, dir);
  else
    return gcd<To_Policy>(to, x, y, dir);
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
lcm_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x) || is_pinf<From1_Policy>(x) ||
	   is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else
    return lcm<To_Policy>(to, x, y, dir);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline Result
cmp_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return cmp<Policy1>(x, y);
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return V_UNORD_COMP;
  else if (is_minf<Policy1>(x))
    return is_minf<Policy2>(y) ? V_EQ : V_LT;
  else if (is_pinf<Policy1>(x))
    return is_pinf<Policy2>(y) ? V_EQ : V_GT;
  else {
    if (is_minf<Policy2>(y))
      return V_GT;
    if (is_pinf<Policy2>(y))
      return V_LT;
    return cmp<Policy1>(x, y);
  }
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
lt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x < y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return false;
  else if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return false;
  else if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return true;
  else
    return x < y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
gt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x > y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return false;
  else if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return false;
  else if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return true;
  else
    return x > y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
le_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x <= y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return false;
  else if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return true;
  else if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return false;
  else
    return x <= y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ge_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x >= y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return false;
  else if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return true;
  else if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return false;
  else
    return x >= y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
eq_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x == y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return false;
  if (is_minf<Policy1>(x))
    return is_minf<Policy2>(y);
  if (is_pinf<Policy1>(x))
    return is_pinf<Policy2>(y);
  return x == y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ne_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x != y;
  if (is_nan<Policy1>(x) || is_nan<Policy2>(y))
    return true;
  if (is_minf<Policy1>(x))
    return !is_minf<Policy2>(y);
  if (is_pinf<Policy1>(x))
    return !is_pinf<Policy2>(y);
  return x != y;
}

template <typename Policy, typename Type>
inline Result
output_ext(std::ostream& os, const Type& x, const Numeric_Format& format, Rounding_Dir dir) {
  if (handle_ext_natively(Type))
    return output<Policy>(os, x, format, dir);
  if (is_nan<Policy>(x)) {
    os << "nan";
    return VC_NAN;
  }
  else if (is_minf<Policy>(x)) {
    os << "-inf";
    return V_EQ;
  }
  else if (is_pinf<Policy>(x)) {
    os << "+inf";
    return V_EQ;
  }
  else
    return output<Policy>(os, x, format, dir);
}

template <typename To_Policy, typename To>
inline Result
input_ext(To& to, std::istream& is, Rounding_Dir dir) {
  return input<To_Policy>(to, is, dir);
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
