/* Abstract checked arithmetic functions: fallbacks.
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

#include <cassert>

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename Type>
struct FUNCTION_CLASS(assign)<Policy, Type, Type> {
  static inline Result function(Type& to, const Type& from) {
    to = from;
    return V_EQ;
  }
};

template <typename Policy, typename To, typename From>
inline Result
abs_generic(To& to, const From& from) {
  if (from < 0)
    return neg<Policy>(to, from);
  to = from;
  return V_EQ;
}

inline Result
neg(Result r) {
  assert(r < V_UNKNOWN);
  Result ret = (Result) (r & (V_EQ | V_APPROX));
  if (r & V_LT)
    ret = (Result) (ret | V_GT);
  if (r & V_GT)
    ret = (Result) (ret | V_LT);
  return ret;
}

inline Result
add(Result r1, Result r2) {
  assert(r1 < V_UNKNOWN);
  assert(r2 < V_UNKNOWN);
  if (r1 == V_EQ)
    return r2;
  if (r2 == V_EQ)
    return r1;
  Result ret = V_APPROX;
  if (((r1 & V_LT) && (r2 & V_GT))
      || ((r1 & V_GT) && (r2 & V_LT)))
    return (Result) (ret | V_LGE);
  if ((r1 & r2) & V_EQ)
    ret = (Result) (ret | V_EQ);
  ret = (Result) (ret | (r1 & (V_LT | V_GT)));
  return ret;
}

inline Result
sub(Result r1, Result r2) {
  return add(r1, neg(r2));
}

template <typename Policy, typename Type>
inline Result
sgn(Result xr, const Type& x) {
  switch (xr) {
  case V_UNKNOWN:
  case V_DOMAIN:
    return V_UNKNOWN;
  case V_NEG_OVERFLOW:
    return V_LT;
  case V_POS_OVERFLOW:
    return V_GT;
  default:
    return sgn<Policy>(x);
  }
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
add(To& to, Result xr, const From1& x, Result yr, const From2& y) {
  if (xr == V_DOMAIN || yr == V_DOMAIN)
    return V_DOMAIN;
  if (xr == V_UNKNOWN || yr == V_UNKNOWN)
    return V_UNKNOWN;
  if (xr == V_NEG_OVERFLOW)
    return sgn<Policy>(yr, y) == V_GT ? V_UNKNOWN : xr;
  if (yr == V_NEG_OVERFLOW)
    return sgn<Policy>(xr, x) == V_GT ? V_UNKNOWN : yr;
  if (xr == V_POS_OVERFLOW)
    return sgn<Policy>(yr, y) == V_LT ? V_UNKNOWN : xr;
  if (yr == V_POS_OVERFLOW)
    return sgn<Policy>(xr, x) == V_LT ? V_UNKNOWN : yr;
  Result rr = add<Policy>(to, x, y);
  if (rr >= V_UNKNOWN)
    return rr;
  return add(rr, add(xr, yr));
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
sub(To& to, Result xr, const From1& x, Result yr, const From2& y) {
  if (xr == V_DOMAIN || yr == V_DOMAIN)
    return V_DOMAIN;
  if (xr == V_UNKNOWN || yr == V_UNKNOWN)
    return V_UNKNOWN;
  if (xr == V_NEG_OVERFLOW)
    return sgn<Policy>(yr, y) == V_LT ? V_UNKNOWN : xr;
  if (yr == V_NEG_OVERFLOW)
    return sgn<Policy>(xr, x) == V_LT ? V_UNKNOWN : yr;
  if (xr == V_POS_OVERFLOW)
    return sgn<Policy>(yr, y) == V_GT ? V_UNKNOWN : xr;
  if (yr == V_POS_OVERFLOW)
    return sgn<Policy>(xr, x) == V_GT ? V_UNKNOWN : yr;
  Result rr = sub<Policy>(to, x, y);
  if (rr >= V_UNKNOWN)
    return rr;
  return add(rr, sub(xr, yr));
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
add_mul_generic(To& to, const From1& x, const From2& y) {
  To temp;
  Result r = mul<Policy>(temp, x, y);
  return add<Policy>(to, V_EQ, to, r, temp);
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
sub_mul_generic(To& to, const From1& x, const From2& y) {
  To temp;
  Result r = mul<Policy>(temp, x, y);
  return sub<Policy>(to, V_EQ, to, r, temp);
}

template <typename Policy, typename To, typename From>
inline Result
gcd_common(To& to, const From& x, const From& y) {
  To nx = x;
  To ny = y;
  To r;
  while (ny != 0) {
    Result ret = mod<Policy>(r, nx, ny);
    if (ret != V_EQ)
      assert(ret == V_EQ);
    nx = ny;
    ny = r;
  }
  to = nx;
  return V_EQ;
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
gcd_generic(To& to, const From1& x, const From2& y) {
  if (x == 0)
    return abs<Policy>(to, y);
  if (y == 0)
    return abs<Policy>(to, x);
  To nx, ny;
  Result r;
  r = abs<Policy>(nx, x);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y);
  assert(r == V_EQ);
  return gcd_common<Policy>(to, nx, ny);
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
lcm_generic(To& to, const From1& x, const From2& y) {
  if (x == 0 || y == 0) {
    to = 0;
    return V_EQ;
  }
  To nx, ny;
  Result r;
  r = abs<Policy>(nx, x);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y);
  assert(r == V_EQ);
  To gcd;
  r = gcd_common<Policy>(gcd, nx, ny);
  assert(r == V_EQ);
  r = div<Policy>(to, nx, gcd);
  assert(r == V_EQ);
  return mul<Policy>(to, to, ny);
}

template <typename Policy, typename Type>
inline Result
sgn_generic(const Type& x) {
  if (x > 0)
    return V_GT;
  if (x < 0)
    return V_LT;
  if (!Policy::check_nan_arg || x == 0)
    return V_EQ;
  return V_UNKNOWN;
}

template <typename Policy, typename Type>
inline Result
cmp_generic(const Type& x, const Type& y) {
  if (x > y)
    return V_GT;
  if (x < y)
    return V_LT;
  if (!Policy::check_nan_arg || x == y)
    return V_EQ;
  return V_UNKNOWN;
}

template <typename To_Policy, typename From_Policy, typename To, typename From>
inline Result
assign_ext(To& to, const From& from) {
  Result r = value_type<From_Policy>(from);
  if (r == V_EQ)
    r = assign<To_Policy>(to, from);
  set_special<To_Policy>(to, r);
  return r;
}

template <typename Policy, typename Type>
inline Result
sgn_ext(const Type& x) {
  Result r = value_type<Policy>(x);
  if (r == V_EQ)
    r = sgn<Policy>(x);
  else if (r == V_NEG_OVERFLOW)
    r = V_LT;
  else if (r == V_POS_OVERFLOW)
    r = V_GT;
  return r;
}

template <typename Policy1, typename Policy2, typename Type1, typename Type2>
inline Result
cmp_ext(const Type1& x, const Type2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<Policy1>(x)) == V_UNKNOWN
      || (ry = value_type<Policy2>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ && ry == V_EQ)
    r = cmp<Policy1>(x, y);
  else if (rx == ry)
    r = V_EQ;
  else if (rx == V_NEG_OVERFLOW || ry == V_POS_OVERFLOW)
    r = V_LT;
  else
    r = V_GT;
  return r;
}

template <typename To_Policy, typename From_Policy, typename To, typename From>
inline Result
neg_ext(To& to, const From& x) {
  Result r = value_type<From_Policy>(x);
  if (r == V_EQ)
    r = neg<To_Policy>(to, x);
  else if (r == V_NEG_OVERFLOW)
    r = V_POS_OVERFLOW;
  else if (r == V_POS_OVERFLOW)
    r = V_NEG_OVERFLOW;
  set_special<To_Policy>(to, r);
  return r;
}

template <typename To_Policy, typename From_Policy, typename To, typename From>
inline Result
abs_ext(To& to, const From& x) {
  Result r = value_type<From_Policy>(x);
  if (r == V_EQ)
    r = abs<To_Policy>(to, x);
  else if (r == V_NEG_OVERFLOW)
    r = V_POS_OVERFLOW;
  set_special<To_Policy>(to, r);
  return r;
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
add_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ && ry == V_EQ)
    r = add<To_Policy>(to, x, y);
  else if (rx == V_EQ)
    r = ry;
  else if (ry == V_EQ || rx == ry)
    r = rx;
  else
    r = V_UNKNOWN;
  set_special<To_Policy>(to, r);
  return r;
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
sub_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ && ry == V_EQ)
    r = sub<To_Policy>(to, x, y);
  else if (rx == V_EQ)
    r = ry == V_POS_OVERFLOW ? V_NEG_OVERFLOW : V_POS_OVERFLOW;
  else if (ry == V_EQ || rx != ry)
    r = rx;
  else
    r = V_UNKNOWN;
  set_special<To_Policy>(to, r);
  return r;
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
mul_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ && ry == V_EQ)
    r = mul<To_Policy>(to, x, y);
  else {
    Result sx = sgn_ext<From1_Policy>(x);
    Result sy = sgn_ext<From2_Policy>(y);
    if (sx == V_EQ || sy == V_EQ)
      r = V_UNKNOWN;
    else if (sx == sy)
      r = V_POS_OVERFLOW;
    else
      r = V_NEG_OVERFLOW;
  }
  set_special<To_Policy>(to, r);
  return r;
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
div_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ) {
    if (ry == V_EQ)
      r = div<To_Policy>(to, x, y);
    else {
      to = 0;
      r = V_EQ;
    }
  }
  else if (ry == V_EQ) {
    Result sy = sgn<From2_Policy>(y);
    if (sy == V_EQ)
      r = V_UNKNOWN;
    else {
      Result sx = rx == V_NEG_OVERFLOW ? V_LT : V_GT;
      if (sx == sy)
	r = V_POS_OVERFLOW;
      else
	r = V_NEG_OVERFLOW;
    }
  }
  else
    r = V_UNKNOWN;
  set_special<To_Policy>(to, r);
  return r;
}
    
	
template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
mod_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ) {
    if (ry == V_EQ)
      r = mod<To_Policy>(to, x, y);
    else {
      to = x;
      r = V_EQ;
    }
  }
  else
    r = V_UNKNOWN;
  set_special<To_Policy>(to, r);
  return r;
}
    
template <typename To_Policy, typename From_Policy, typename To, typename From>
inline Result
sqrt_ext(To& to, const From& x) {
  Result r = value_type<From_Policy>(x);
  if (r == V_EQ)
    r = sqrt<To_Policy>(to, x);
  else if (r == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (r == V_NEG_OVERFLOW)
    r = V_DOMAIN;
  else
    r = V_POS_OVERFLOW;
  set_special<To_Policy>(to, r);
  return r;
}

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
gcd_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ) {
    if (ry == V_EQ)
      r = gcd<To_Policy>(to, x, y);
    else {
      to = x;
      r = V_EQ;
    }
  }
  else if (ry == V_EQ) {
    to = y;
    r = V_EQ;
  }
  else
    r = V_POS_OVERFLOW;
  set_special<To_Policy>(to, r);
  return r;
}
    
template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
inline Result
lcm_ext(To& to, const From1& x, const From2& y) {
  Result rx;
  Result ry;
  Result r;
  if ((rx = value_type<From1_Policy>(x)) == V_UNKNOWN
      || (ry = value_type<From2_Policy>(y)) == V_UNKNOWN)
    r = V_UNKNOWN;
  else if (rx == V_EQ && ry == V_EQ)
    r = lcm<To_Policy>(to, x, y);
  else
    r = V_POS_OVERFLOW;
  set_special<To_Policy>(to, r);
  return r;
}

template <typename Policy, typename Type>
inline void
print_ext(std::ostream& os, const Type& x) {
  Result rx = value_type<Policy>(x);
  if (rx == V_EQ)
    os << x;
  else if (rx == V_NEG_OVERFLOW)
    os << "-inf";
  else if (rx == V_POS_OVERFLOW)
    os << "inf";
  else
    os << "nan";
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
