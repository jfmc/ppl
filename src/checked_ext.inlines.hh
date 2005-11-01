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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

namespace Parma_Polyhedra_Library {

namespace Checked {

#define handle_ext_natively(Type) (Float<Type>::fpu_related)

template <typename Policy, typename Type>
inline Result
sgn_ext(const Type& x) {
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x)))
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
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(from)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(from))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(from))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else {
  native:
    return assign<To_Policy>(to, from, dir);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
neg_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(x)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else {
  native:
    return neg<To_Policy>(to, x, dir);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
abs_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(x)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x) || is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else {
  native:
    return abs<To_Policy>(to, x, dir);
  }
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_add_inf, is_pinf<From2_Policy>(y)))
      goto inf_add_inf;
    else
      goto minf;
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_add_inf, is_minf<From2_Policy>(y))) {
    inf_add_inf:
      return set_special<To_Policy>(to, V_INF_ADD_INF);
    }
    else
      goto pinf;
  }
  else {
    if (is_minf<From2_Policy>(y)) {
    minf:
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
    }
    else if (is_pinf<From2_Policy>(y)) {
    pinf:
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
    }
    else {
    native:
      return add<To_Policy>(to, x, y, dir);
    }
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_sub_inf, is_minf<From2_Policy>(y)))
      goto inf_sub_inf;
    else
      goto minf;
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_sub_inf, is_pinf<From2_Policy>(y))) {
    inf_sub_inf:
      return set_special<To_Policy>(to, V_INF_SUB_INF);
    }
    else
      goto pinf;
  }
  else {
    if (is_pinf<From2_Policy>(y)) {
    minf:
      return assign<To_Policy>(to, MINUS_INFINITY, dir);
    }
    else if (is_minf<From2_Policy>(y)) {
    pinf:
      return assign<To_Policy>(to, PLUS_INFINITY, dir);
    }
    else {
    native:
      return sub<To_Policy>(to, x, y, dir);
    }
  }
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto pinf;
    case V_GT:
      goto minf;
    default:
      goto inf_mul_zero;
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto minf;
    case V_GT:
      goto pinf;
    default:
      goto inf_mul_zero;
    }
  }
  else {
    if (is_minf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	goto pinf;
      case V_GT:
	goto minf;
      default:
	goto inf_mul_zero;
      }
    } else if (is_pinf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
      minf:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      case V_GT:
      pinf:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
	goto pinf;
      default:
      inf_mul_zero:
	assert(To_Policy::check_inf_mul_zero);
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    }
    else {
    native:
      return mul<To_Policy>(to, x, y, dir);
    }
  }
}

    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(To_Policy::check_nan_args, is_nan<To_Policy>(to)) ||
      CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) ||
      CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto a_pinf;
    case V_GT:
      goto a_minf;
    default:
      goto inf_mul_zero;
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto a_minf;
    case V_GT:
      goto a_pinf;
    default:
      goto inf_mul_zero;
    }
  }
  else {
    if (is_minf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	goto a_pinf;
      case V_GT:
	goto a_minf;
      default:
	goto inf_mul_zero;
      }
    } else if (is_pinf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
      a_minf:
	if (CHECK_P(To_Policy::check_inf_add_inf, is_pinf<To_Policy>(to)))
	  goto inf_add_inf;
	else
	  goto minf;
      case V_GT:
      a_pinf:
	if (CHECK_P(To_Policy::check_inf_add_inf, is_minf<To_Policy>(to))) {
	inf_add_inf:
	  return set_special<To_Policy>(to, V_INF_ADD_INF);
	}
	else
	  goto pinf;
      default:
      inf_mul_zero:
	assert(To_Policy::check_inf_mul_zero);
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    }
    else {
      if (is_minf<To_Policy>(to)) {
      minf:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      }
      if (is_pinf<To_Policy>(to)) {
      pinf:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      }
    native:
      return add_mul<To_Policy>(to, x, y, dir);
    }
  }
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(To_Policy::check_nan_args, is_nan<To_Policy>(to)) ||
      CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) ||
      CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto a_pinf;
    case V_GT:
      goto a_minf;
    default:
      goto inf_mul_zero;
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    switch (sgn_ext<From2_Policy>(y)) {
    case V_LT:
      goto a_minf;
    case V_GT:
      goto a_pinf;
    default:
      goto inf_mul_zero;
    }
  }
  else {
    if (is_minf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
	goto a_pinf;
      case V_GT:
	goto a_minf;
      default:
	goto inf_mul_zero;
      }
    } else if (is_pinf<From2_Policy>(y)) {
      switch (sgn<From1_Policy>(x)) {
      case V_LT:
      a_minf:
	if (CHECK_P(To_Policy::check_inf_sub_inf, is_minf<To_Policy>(to)))
	  goto inf_sub_inf;
	else
	  goto pinf;
      case V_GT:
      a_pinf:
	if (CHECK_P(To_Policy::check_inf_sub_inf, is_pinf<To_Policy>(to))) {
	inf_sub_inf:
	  return set_special<To_Policy>(to, V_INF_SUB_INF);
	}
	else
	  goto minf;
      default:
      inf_mul_zero:
	assert(To_Policy::check_inf_mul_zero);
	return set_special<To_Policy>(to, V_INF_MUL_ZERO);
      }
    }
    else {
      if (is_minf<To_Policy>(to)) {
      minf:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      }
      if (is_pinf<To_Policy>(to)) {
      pinf:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      }
    native:
      return sub_mul<To_Policy>(to, x, y, dir);
    }
  }
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
div_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  if (is_minf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_div_inf, is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y)))
      goto inf_div_inf;
    else {
      switch (sgn<From2_Policy>(y)) {
      case V_LT:
	goto pinf;
      case V_GT:
	goto minf;
      default:
	goto div_zero;
      }
    }
  }
  else if (is_pinf<From1_Policy>(x)) {
    if (CHECK_P(To_Policy::check_inf_div_inf, is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y))) {
    inf_div_inf:
      return set_special<To_Policy>(to, V_INF_DIV_INF);
    }
    else {
      switch (sgn<From2_Policy>(y)) {
      case V_LT:
      minf:
	return assign<To_Policy>(to, MINUS_INFINITY, dir);
      case V_GT:
      pinf:
	return assign<To_Policy>(to, PLUS_INFINITY, dir);
      default:
      div_zero:
	assert(To_Policy::check_div_zero);
	return set_special<To_Policy>(to, V_DIV_ZERO);
      }
    }
  }
  else {
    if (is_minf<From2_Policy>(y) || is_pinf<From2_Policy>(y)) {
      to = 0;
      return V_EQ;
    }
    else {
    native:
      return div<To_Policy>(to, x, y, dir);
    }
  }
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
rem_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From1) && handle_ext_natively(From2))
    goto native;
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (CHECK_P(To_Policy::check_inf_mod, is_minf<From1_Policy>(x) || is_pinf<From1_Policy>(x)))
    return set_special<To_Policy>(to, V_INF_MOD);
  else {
    if (is_minf<From1_Policy>(y) || is_pinf<From2_Policy>(y)) {
      to = x;
      return V_EQ;
    }
    else {
    native:
      return rem<To_Policy>(to, x, y, dir);
    }
  }
}
    
template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
mul2exp_ext(To& to, const From& x, int exp, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(x)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else {
  native:
    return mul2exp<To_Policy>(to, x, exp, dir);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
div2exp_ext(To& to, const From& x, int exp, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(x)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return assign<To_Policy>(to, MINUS_INFINITY, dir);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else {
  native:
    return div2exp<To_Policy>(to, x, exp, dir);
  }
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
sqrt_ext(To& to, const From& x, Rounding_Dir dir) {
  if (handle_ext_natively(To) && handle_ext_natively(From))
    goto native;
  if (CHECK_P(From_Policy::check_nan_args, is_nan<From_Policy>(x)))
    return set_special<To_Policy>(to, VC_NAN);
  else if (is_minf<From_Policy>(x))
    return set_special<To_Policy>(to, V_SQRT_NEG);
  else if (is_pinf<From_Policy>(x))
    return assign<To_Policy>(to, PLUS_INFINITY, dir);
  else {
  native:
    return sqrt<To_Policy>(to, x, dir);
  }
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
gcd_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
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
  if (CHECK_P(From1_Policy::check_nan_args, is_nan<From1_Policy>(x)) || CHECK_P(From2_Policy::check_nan_args, is_nan<From2_Policy>(y)))
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
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
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
  native:
    return cmp<Policy1>(x, y);
  }
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
lt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return false;
  if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return false;
  if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return true;
 native:
  return x < y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
gt_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return false;
  if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return false;
  if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return true;
 native:
  return x > y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
le_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return false;
  if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return true;
  if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return false;
 native:
  return x <= y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ge_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return false;
  if (is_pinf<Policy1>(x) || is_minf<Policy2>(y))
    return true;
  if (is_minf<Policy1>(x) || is_pinf<Policy2>(y))
    return false;
 native:
  return x >= y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
eq_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return false;
  if (is_minf<Policy1>(x))
    return is_minf<Policy2>(y);
  if (is_pinf<Policy1>(x))
    return is_pinf<Policy2>(y);
 native:
  return x == y;
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline bool
ne_ext(const Type1& x, const Type2& y) {
  if (handle_ext_natively(Type1) && handle_ext_natively(Type2))
    goto native;
  if (CHECK_P(Policy1::check_nan_args, is_nan<Policy1>(x)) || CHECK_P(Policy2::check_nan_args, is_nan<Policy2>(y)))
    return true;
  if (is_minf<Policy1>(x))
    return !is_minf<Policy2>(y);
  if (is_pinf<Policy1>(x))
    return !is_pinf<Policy2>(y);
 native:
  return x != y;
}

template <typename Policy, typename Type>
inline Result
output_ext(std::ostream& os, const Type& x, const Numeric_Format& format, Rounding_Dir dir) {
  if (handle_ext_natively(Type))
    goto native;
  if (CHECK_P(Policy::check_nan_args, is_nan<Policy>(x))) {
    os << "nan";
    return VC_NAN;
  }
  if (is_minf<Policy>(x)) {
    os << "-inf";
    return V_EQ;
  }
  if (is_pinf<Policy>(x)) {
    os << "+inf";
    return V_EQ;
  }
 native:
  return output<Policy>(os, x, format, dir);
}

template <typename To_Policy, typename To>
inline Result
input_ext(To& to, std::istream& is, Rounding_Dir dir) {
  return input<To_Policy>(to, is, dir);
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
