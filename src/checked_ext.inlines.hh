/* Checked extended arithmetic functions.
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

namespace Parma_Polyhedra_Library {

namespace Checked {

#define handle_ext(Type) (Float<Type>::fpu_related)

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
assign_ext(To& to, const From& from, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From))
    return assign<To_Policy>(to, from, mode);
  Result r = classify<From_Policy>(from, true, true, false);
  if (r == V_NORMAL)
    return assign<To_Policy>(to, from, mode);
  return set_special<To_Policy>(to, r);
}

template <typename Policy1, typename Policy2,
	  typename Type1, typename Type2>
inline Result
cmp_ext(const Type1& x, const Type2& y) {
  if (handle_ext(Type1) && handle_ext(Type2))
    return cmp<Policy1>(x, y);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<Policy1>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<Policy2>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL && ry == V_NORMAL)
    r = cmp<Policy1>(x, y);
  else if (rx == ry)
    r = V_EQ;
  else if (rx == V_MINUS_INFINITY || ry == V_PLUS_INFINITY)
    r = V_LT;
  else
    r = V_GT;
  return r;
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
neg_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From))
    return neg<To_Policy>(to, x, mode);
  Result r = classify<From_Policy>(x, true, true, false);
  if (r == V_NORMAL)
    return neg<To_Policy>(to, x, mode);
  else if (r == V_MINUS_INFINITY)
    r = V_PLUS_INFINITY;
  else if (r == V_PLUS_INFINITY)
    r = V_MINUS_INFINITY;
  return set_special<To_Policy>(to, r);
}

template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
abs_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From))
    return abs<To_Policy>(to, x, mode);
  Result r = classify<From_Policy>(x, true, true, false);
  if (r == V_NORMAL)
    return abs<To_Policy>(to, x, mode);
  else if (r == V_MINUS_INFINITY)
    r = V_PLUS_INFINITY;
  return set_special<To_Policy>(to, r);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
add_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From1) && handle_ext(From2))
    return add<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL && ry == V_NORMAL)
    return add<To_Policy>(to, x, y, mode);
  else if (rx == V_NORMAL)
    r = ry;
  else if (ry == V_NORMAL || rx == ry)
    r = rx;
  else
    r = V_UNKNOWN;
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
sub_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From1) && handle_ext(From2))
    return sub<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL && ry == V_NORMAL)
    return sub<To_Policy>(to, x, y, mode);
  else if (rx == V_NORMAL)
    r = ry == V_PLUS_INFINITY ? V_MINUS_INFINITY : V_PLUS_INFINITY;
  else if (ry == V_NORMAL || rx != ry)
    r = rx;
  else
    r = V_UNKNOWN;
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
mul_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From1) && handle_ext(From2))
    return mul<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, true)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, true)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (!(is_special(rx) || is_special(ry)))
    return mul<To_Policy>(to, x, y, mode);
  else {
    rx = sign(rx);
    ry = sign(ry);
    if (rx == V_EQ || ry == V_EQ)
      r = V_UNKNOWN;
    else if (rx == ry)
      r = V_PLUS_INFINITY;
    else
      r = V_MINUS_INFINITY;
  }
  return set_special<To_Policy>(to, r);
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
div_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From1) && handle_ext(From2))
    return div<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, true)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, true)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (!is_special(rx)) {
    if (!is_special(ry))
      return div<To_Policy>(to, x, y, mode);
    else {
      to = 0;
      r = V_EQ;
    }
  }
  else if (!is_special(ry)) {
    if (ry == V_EQ)
      r = V_UNKNOWN;
    else {
      rx = sign(rx);
      if (rx == ry)
	r = V_PLUS_INFINITY;
      else
	r = V_MINUS_INFINITY;
    }
  }
  else
    r = V_UNKNOWN;
  return set_special<To_Policy>(to, r);
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
mod_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From1) && handle_ext(From2))
    return mod<To_Policy>(to, x, y, mode);
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL) {
    if (ry == V_NORMAL)
      return mod<To_Policy>(to, x, y, mode);
    else {
      to = x;
      r = V_EQ;
    }
  }
  else
    r = V_UNKNOWN;
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From_Policy,
	  typename To, typename From>
inline Result
sqrt_ext(To& to, const From& x, const Rounding& mode) {
  if (handle_ext(To) && handle_ext(From))
    return sqrt<To_Policy>(to, x, mode);
  Result r = classify<From_Policy>(x, true, true, false);
  if (r == V_NORMAL)
    return sqrt<To_Policy>(to, x, mode);
  else if (r == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (r == V_MINUS_INFINITY)
    r = V_DOMAIN;
  else
    r = V_PLUS_INFINITY;
  return set_special<To_Policy>(to, r);
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
gcd_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL) {
    if (ry == V_NORMAL)
      return gcd<To_Policy>(to, x, y, mode);
    else {
      to = x;
      r = V_EQ;
    }
  }
  else if (ry == V_NORMAL) {
    to = y;
    r = V_EQ;
  }
  else
    r = V_PLUS_INFINITY;
  return set_special<To_Policy>(to, r);
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy,
	  typename To, typename From1, typename From2>
inline Result
lcm_ext(To& to, const From1& x, const From2& y, const Rounding& mode) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = classify<From1_Policy>(x, true, true, false)) == V_UNKNOWN
      || (ry = classify<From2_Policy>(y, true, true, false)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_NORMAL && ry == V_NORMAL)
    return lcm<To_Policy>(to, x, y, mode);
  else
    r = V_PLUS_INFINITY;
  return set_special<To_Policy>(to, r);
}

template <typename Policy, typename Type>
inline Result
print_ext(std::ostream& os, const Type& x, const Numeric_Format& format, const Rounding& mode) {
  if (handle_ext(Type))
    return print<Policy>(os, x, format, mode);
  Result rx = classify<Policy>(x, true, true, false);
  switch (rx) {
  case V_NORMAL:
    return print<Policy>(os, x, format, mode);
    break;
  case V_MINUS_INFINITY:
    os << "-inf";
    break;
  case V_PLUS_INFINITY:
    os << "inf";
    break;
  default:
    os << "nan";
    break;
  }
  return rx;
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
