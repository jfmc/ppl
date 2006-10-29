/* Abstract checked arithmetic functions: fall-backs.
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

#include "compiler.hh"
#include "globals.types.hh"
#include <cassert>

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename Type>
struct FUNCTION_CLASS(construct)<Policy, Type, Type> {
  static inline Result function(Type& to, const Type& from, Rounding_Dir) {
    new (&to) Type(from);
    return V_EQ;
  }
};

template <typename Policy, typename To, typename From>
struct FUNCTION_CLASS(construct) {
  static inline Result function(To& to, const From& from, Rounding_Dir dir) {
    new (&to) To();
    return assign<Policy>(to, from, dir);
  }
};

template <typename Policy, typename Type>
struct FUNCTION_CLASS(assign)<Policy, Type, Type> {
  static inline Result function(Type& to, const Type& from, Rounding_Dir) {
    to = from;
    return V_EQ;
  }
};

template <typename Policy, typename Type>
inline void
copy_generic(Type& to, const Type& from) {
  to = from;
}

template <typename Policy, typename To, typename From>
inline Result
abs_generic(To& to, const From& from, Rounding_Dir dir) {
  if (from < 0)
    return neg<Policy>(to, from, dir);
  to = from;
  return V_EQ;
}

inline Result
neg(Result r) {
  assert(!is_special(r));
  Result ret = static_cast<Result>(r & V_EQ);
  if (r & V_LT)
    ret = static_cast<Result>(ret | V_GT);
  if (r & V_GT)
    ret = static_cast<Result>(ret | V_LT);
  return ret;
}

inline Result
add(Result r1, Result r2) {
  assert(!is_special(r1));
  assert(!is_special(r2));
  if (r1 == V_EQ)
    return r2;
  if (r2 == V_EQ)
    return r1;
  if (((r1 & V_LT) && (r2 & V_GT))
      || ((r1 & V_GT) && (r2 & V_LT)))
    return V_LGE;
  return static_cast<Result>((((r1 & r2) & V_EQ) ? V_EQ : 0) |
			       (r1 & (V_LT | V_GT)));
}

inline Result
sub(Result r1, Result r2) {
  return add(r1, neg(r2));
}

template <typename Policy, typename To, typename From>
inline void
gcd_exact_noabs(To& to, const From& x, const From& y) {
  To nx = x;
  To ny = y;
  To rm;
  while (ny != 0) {
    // The following is derived from the assumption that x % y
    // is always representable. This is true for both native integers
    // and IEC 559 floating point numbers.
    rem<Policy>(rm, nx, ny, ROUND_NOT_NEEDED);
    nx = ny;
    ny = rm;
  }
  to = nx;
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
gcd_exact(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  gcd_exact_noabs<Policy>(to, x, y);
  return abs<Policy>(to, to, dir);
}

template <typename Policy, typename To1,
	  typename From1, typename From2, typename To2, typename To3>
inline Result
gcdext_exact(To1& to, const From1& x, const From2& y, To2& s, To3& t,
	     Rounding_Dir dir) {
  if (y == 0) {
    if (x == 0) {
      s = 0;
      t = 1;
      return V_EQ;
    }
    else {
      if (x < 0)
	s = -1;
      else
	s = 1;
      t = 0;
      return abs<Policy>(to, x, dir);
    }
  }

  s = 1;
  t = 0;
  bool negative_x = x < 0;
  bool negative_y = y < 0;

  Result r;
  r = abs<Policy>(to, x, dir);
  if (r != V_EQ)
    return r;

  From2 ay;
  r = abs<Policy>(ay, y, dir);
  if (r != V_EQ)
    return r;

  // If COPY_GMP is defined then s is favored when the absolute
  // values of the given numbers are equal.  For instance if x and y
  // are both 5 then s will be 1 and t will be 0, instead of the other
  // way round.  This is to match the behavior of GMP.
#define COPY_GMP
#ifdef COPY_GMP
  if (to == ay)
    goto sign_check;
#endif

  {
    To2 v1 = 0;
    To3 v2 = 1;
    To1 v3 = static_cast<To1>(ay);
    while (true) {
      To1 q = to / v3;
      // Remainder, next candidate GCD.
      To1 t3 = to - q*v3;
      To2 t1 = s - static_cast<To2>(q)*v1;
      To3 t2 = t - static_cast<To3>(q)*v2;
      s = v1;
      t = v2;
      to = v3;
      if (t3 == 0)
	break;
      v1 = t1;
      v2 = t2;
      v3 = t3;
    }
  }

#ifdef COPY_GMP
 sign_check:
#endif
  if (negative_x) {
    r = neg<Policy>(s, s, dir);
    if (r != V_EQ)
      return r;
  }
  if (negative_y)
    return neg<Policy>(t, t, dir);
  return V_EQ;
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
lcm_gcd_exact(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (x == 0 || y == 0) {
    to = 0;
    return V_EQ;
  }
  To nx, ny;
  Result r;
  r = abs<Policy>(nx, x, dir);
  if (r != V_EQ)
    return r;
  r = abs<Policy>(ny, y, dir);
  if (r != V_EQ)
    return r;
  To gcd;
  gcd_exact_noabs<Policy>(gcd, nx, ny);
  // The following is derived from the assumption that x / gcd(x, y)
  // is always representable. This is true for both native integers
  // and IEC 559 floating point numbers.
  div<Policy>(to, nx, gcd, ROUND_NOT_NEEDED);
  return mul<Policy>(to, to, ny, dir);
}

template <typename Policy, typename Type>
inline Result
sgn_generic(const Type& x) {
  if (x > 0)
    return V_GT;
  if (x == 0)
    return V_EQ;
  return V_LT;
}

template <typename Policy, typename Type>
inline Result
cmp_generic(const Type& x, const Type& y) {
  if (x > y)
    return V_GT;
  if (x < y)
    return V_LT;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
input_generic(Type& to, std::istream& is, Rounding_Dir dir) {
  mpq_class q;
  Result r = input_mpq(q, is);
  if (r == VC_MINUS_INFINITY)
    return assign<Policy>(to, MINUS_INFINITY, dir);
  if (r == VC_PLUS_INFINITY)
    return assign<Policy>(to, PLUS_INFINITY, dir);
  if (r == V_EQ)
    return assign<Policy>(to, q, dir);
  return set_special<Policy>(to, r);
}

template <typename T>
inline memory_size_type
external_memory_in_bytes(T) {
  return 0;
}

template <typename T>
inline memory_size_type
total_memory_in_bytes(T& x) {
  return sizeof(x) + external_memory_in_bytes(x);
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
