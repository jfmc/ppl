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

namespace Parma_Polyhedra_Library {

template <typename To, typename From>
inline Result_Info
checked_assignexact(To& to, From from) {
  return checked_assign(to, from);
}

template <typename To, typename From>
inline Result_Info
checked_negexact(To& to, From from) {
  return checked_neg(to, from);
}

template <typename To, typename From>
inline Result_Info
checked_addexact(To& to, From x, From y) {
  return checked_add(to, x, y);
}

template <typename To, typename From>
inline Result_Info
checked_subexact(To& to, From x, From y) {
  return checked_sub(to, x, y);
}

template <typename To, typename From>
inline Result_Info
checked_mulexact(To& to, From x, From y) {
  return checked_mul(to, x, y);
}

template <typename To, typename From>
inline Result_Info
checked_divexact(To& to, From x, From y) {
  return checked_div(to, x, y);
}

template <typename To, typename From>
inline Result_Info
checked_modexact(To& to, From x, From y) {
  return checked_mod(to, x, y);
}

template <typename To, typename From>
inline Result_Info
checked_sqrtexact(To& to, From from) {
  return checked_sqrt(to, from);
}

template <typename Type>
inline Result_Info
checked_assign(Type& to, Type from) {
  to = from;
  return V_EQ;
}

template <typename To, typename From>
inline Result_Info
checked_abs(To& to, From from) {
  if (from < 0)
    return checked_neg(to, from);
  to = from;
  return V_EQ;
}

template <typename From>
inline int
sgn(From x) {
  return x > 0 ? 1 : x == 0 ? 0 : -1;
}

template <typename From>
inline int
cmp(From x, From y) {
  return x > y ? 1 : x == y ? 0 : -1;
}

template <typename To, typename From>
Result_Info
checked_gcd_(To& to, From x, From y) {
  To nx = x;
  To ny = y;
  To r;
  while (ny != 0) {
    checked_modexact(r, nx, ny);
    nx = ny;
    ny = r;
  }
  to = nx;
  return V_EQ;
}

template <typename To, typename From>
Result_Info
checked_gcd(To& to, From x, From y) {
  if (x == 0)
    return checked_abs(to, y);
  if (y == 0)
    return checked_abs(to, x);
  To nx, ny;
  checked_abs(nx, x);
  checked_abs(ny, y);
  return checked_gcd_(to, nx, ny);
}

template <typename To, typename From>
Result_Info
checked_lcm(To& to, From x, From y) {
  if (x == 0 || y == 0) {
    to = 0;
    return V_EQ;
  }
  To nx, ny;
  checked_abs(nx, x);
  checked_abs(ny, y);
  To gcd;
  checked_gcd_(gcd, nx, ny);
  checked_divexact(to, nx, gcd);
  return checked_mul(to, to, ny);
}

} // namespace Parma_Polyhedra_Library
