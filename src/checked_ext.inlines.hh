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

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
assign_ext(To& to, const From& from, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return assign<To_Policy>(to, from, mode);
  switch (classify<From_Policy>(from, true, true, false)) {
  case VC_NAN:
    return set_special<To_Policy>(to, VC_NAN);
  case VC_MINUS_INFINITY:
    return set_special<To_Policy>(to, VC_MINUS_INFINITY);
  case VC_PLUS_INFINITY:
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  default:
    return assign<To_Policy>(to, from, mode);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
neg_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return neg<To_Policy>(to, x, mode);
  switch (classify<From_Policy>(x, true, true, false)) {
  case VC_NAN:
    return set_special<To_Policy>(to, VC_NAN);
  case VC_MINUS_INFINITY:
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  case VC_PLUS_INFINITY:
    return set_special<To_Policy>(to, VC_MINUS_INFINITY);
  default:
    return neg<To_Policy>(to, x, mode);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
abs_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return abs<To_Policy>(to, x, mode);
  switch (classify<From_Policy>(x, true, true, false)) {
  case VC_NAN:
    return set_special<To_Policy>(to, VC_NAN);
  case VC_MINUS_INFINITY:
  case VC_PLUS_INFINITY:
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  default:
    return abs<To_Policy>(to, x, mode);
  }
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return add<To_Policy>(to, x, y, mode);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_ADD_INF);
    else
      return set_special<To_Policy>(to, VC_MINUS_INFINITY);
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_ADD_INF);
    else
      return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  }
  else {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, VC_MINUS_INFINITY);
    else if (is_pinf<From2_Policy>(y)) 
      return set_special<To_Policy>(to, VC_PLUS_INFINITY);
    else
      return add<To_Policy>(to, x, y, mode);
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return sub<To_Policy>(to, x, y, mode);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_SUB_INF);
    else
      return set_special<To_Policy>(to, VC_MINUS_INFINITY);
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_SUB_INF);
    else
      return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  }
  else {
    if (is_minf<From2_Policy>(y))
      return set_special<To_Policy>(to, VC_PLUS_INFINITY);
    else if (is_pinf<From2_Policy>(y)) 
      return set_special<To_Policy>(to, VC_MINUS_INFINITY);
    else
      return sub<To_Policy>(to, x, y, mode);
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
mul_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return mul<To_Policy>(to, x, y, mode);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    switch (classify<From2_Policy>(y, false, false, true)) {
    case V_LT:
      return set_special<To_Policy>(to, VC_PLUS_INFINITY);
    case V_GT:
      return set_special<To_Policy>(to, VC_MINUS_INFINITY);
    default:
      return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    switch (classify<From2_Policy>(y, false, false, true)) {
      case V_LT:
	return set_special<To_Policy>(to, VC_MINUS_INFINITY);
      case V_GT:
	return set_special<To_Policy>(to, VC_PLUS_INFINITY);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
    }
  }
  else {
    if (is_minf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	return set_special<To_Policy>(to, VC_PLUS_INFINITY);
      case V_GT:
	return set_special<To_Policy>(to, VC_MINUS_INFINITY);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    } else if (is_pinf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	return set_special<To_Policy>(to, VC_MINUS_INFINITY);
      case V_GT:
	return set_special<To_Policy>(to, VC_PLUS_INFINITY);
      default:
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    }
    else
      return mul<To_Policy>(to, x, y, mode);
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_mul_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rto;
  Result rx;
  Result ry;
  Result r;
  if ((rto = classify<To_Policy>(to, true, true, false)) == VC_NAN
      || (rx = classify<From1_Policy>(x, true, true, true)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, true)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (!(is_special(rto) || is_special(rx) || is_special(ry)))
    return add_mul<To_Policy>(to, x, y, mode);
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

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_mul_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rto;
  Result rx;
  Result ry;
  Result r;
  if ((rto = classify<To_Policy>(to, true, true, false)) == VC_NAN
      || (rx = classify<From1_Policy>(x, true, true, true)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, true)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (!(is_special(rto) || is_special(rx) || is_special(ry)))
    return sub_mul<To_Policy>(to, x, y, mode);
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
div_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return div<To_Policy>(to, x, y, mode);
  if (is_nan<From1_Policy>(x) || is_nan<From2_Policy>(y))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))
      return set_special<To_Policy>(to, V_INF_DIV_INF);
    else {
      switch (sgn<From2_Policy>(y)) {
      case V_LT:
	return set_special<To_Policy>(to, VC_PLUS_INFINITY);
      case V_GT:
	return set_special<To_Policy>(to, VC_MINUS_INFINITY);
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
	return set_special<To_Policy>(to, VC_MINUS_INFINITY);
      case V_GT:
	return set_special<To_Policy>(to, VC_PLUS_INFINITY);
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
      return div<To_Policy>(to, x, y, mode);
  }
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
rem_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    return rem<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, false)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (rx == VC_NORMAL) {
    if (ry == VC_NORMAL)
      return rem<To_Policy>(to, x, y, mode);
    else {
      to = x;
      return V_EQ;
    }
  }
  else
    return set_special<To_Policy>(to, V_INF_MOD);
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
sqrt_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    return sqrt<To_Policy>(to, x, mode);
  Result r = classify<From_Policy>(x, true, true, false);
  if (r == VC_NORMAL)
    return sqrt<To_Policy>(to, x, mode);
  else if (r == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (r == VC_MINUS_INFINITY)
    return set_special<To_Policy>(to, V_SQRT_NEG);
  else
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  return set_special<To_Policy>(to, r);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
gcd_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, false)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (rx == VC_NORMAL) {
    if (ry == VC_NORMAL)
      return gcd<To_Policy>(to, x, y, mode);
    else {
      to = x;
      return V_EQ;
    }
  }
  else if (ry == VC_NORMAL) {
    to = y;
    return V_EQ;
  }
  else
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
lcm_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == VC_NAN
      || (ry = classify<From2_Policy>(y, true, true, false)) == VC_NAN)
    return set_special<To_Policy>(to, VC_NAN);
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return lcm<To_Policy>(to, x, y, mode);
  else
    return set_special<To_Policy>(to, VC_PLUS_INFINITY);
  return set_special<To_Policy>(to, r);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline Result
cmp_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return cmp<Policy1>(x, y);
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return V_UNORD_COMP;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return cmp<Policy1>(x, y);
  else if (rx == ry)
    return V_EQ;
  else if (rx == VC_MINUS_INFINITY || ry == VC_PLUS_INFINITY)
    return V_LT;
  else
    return V_GT;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
lt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x < y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return false;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x < y;
  else if (rx == ry)
    return false;
  return (rx == VC_MINUS_INFINITY || ry == VC_PLUS_INFINITY);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
gt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x > y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return false;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x > y;
  else if (rx == ry)
    return false;
  return (rx == VC_PLUS_INFINITY || ry == VC_MINUS_INFINITY);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
le_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x <= y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return false;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x <= y;
  return (rx == VC_MINUS_INFINITY || ry == VC_PLUS_INFINITY);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ge_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x >= y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return false;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x >= y;
  return (rx == VC_PLUS_INFINITY || ry == VC_MINUS_INFINITY);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
eq_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x == y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return false;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x == y;
  return rx == ry;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ne_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    return x != y;
  Result rx;
  Result ry;
  if ((rx = classify<Policy1>(x, true, true, false)) == VC_NAN
      || (ry = classify<Policy2>(y, true, true, false)) == VC_NAN)
    return true;
  else if (rx == VC_NORMAL && ry == VC_NORMAL)
    return x != y;
  return rx != ry;
}

template <typename Policy, typename Type>
inline Result
to_c_string_ext(char *str, size_t size, const Type& x, const Numeric_Format& format, const Rounding& mode) {
  if (handle_ext_natively(Type))
    return to_c_string<Policy>(str, size, x, format, mode);
  Result rx = classify<Policy>(x, true, true, false);
  switch (rx) {
  case VC_NORMAL:
    return to_c_string<Policy>(str, size, x, format, mode);
    break;
  case VC_MINUS_INFINITY:
    strncpy(str, "-inf", size);
    break;
  case VC_PLUS_INFINITY:
    strncpy(str, "+inf", size);
    break;
  default:
    strncpy(str, "nan", size);
    break;
  }
  return rx;
}

template <typename Policy, typename Type>
inline Result
from_c_string_ext(Type& x, const char *str, const Rounding& mode) {
  if (handle_ext_natively(Type))
    return from_c_string<Policy>(x, str, mode);
  if (strcmp(str, "-inf") == 0)
    return set_special<Policy>(x, VC_MINUS_INFINITY);
  if (strcmp(str, "+inf") == 0)
    return set_special<Policy>(x, VC_PLUS_INFINITY);
  if (strcmp(str, "nan") == 0)
    return set_special<Policy>(x, VC_NAN);
  return from_c_string<Policy>(x, str, mode);
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
