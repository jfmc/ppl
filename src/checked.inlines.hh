/* Abstract checked arithmetic functions: fallbacks
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
struct FUNCTION_CLASS(abs) {
  static inline Result function(To& to, const From from) {
    if (from < 0)
      return neg<Policy>(to, from);
    to = from;
    return V_EQ;
  }
};

template <typename Policy, typename To, typename From>
inline Result
gcd_common(To& to, const From x, const From y) {
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

template <typename Policy, typename To, typename From>
struct FUNCTION_CLASS(gcd) {
  static inline Result function(To& to, const From x, const From y) {
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
};

template <typename Policy, typename To, typename From>
struct FUNCTION_CLASS(lcm) {
  static inline Result function(To& to, const From x, const From y) {
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
};

template <typename From>
inline int
sgn(const From x) {
  return x > 0 ? 1 : x == 0 ? 0 : -1;
}

template <typename From>
inline int
cmp(const From x, const From y) {
  return x > y ? 1 : x == y ? 0 : -1;
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
