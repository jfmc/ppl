/* E_NIT class implementation: inline functions.
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

#ifndef PPL_E_NIT_inlines_hh
#define PPL_E_NIT_inlines_hh 1

#include <limits>
#include <sstream>
#include <string>

namespace Parma_Polyhedra_Library {

template <typename T>
inline T
E_NIT<T>::pinf() {
  return std::numeric_limits<T>::max();
}

template <typename T>
inline T
E_NIT<T>::max() {
  return std::numeric_limits<T>::max() - 1;
}

template <typename T>
inline T
E_NIT<T>::min() {
  return std::numeric_limits<T>::min() + 2;
}

template <typename T>
inline T
E_NIT<T>::minf() {
  return std::numeric_limits<T>::min() + 1;
}

template <typename T>
inline T
E_NIT<T>::nnum() {
  return std::numeric_limits<T>::min();
}

template <typename T>
inline
E_NIT<T>::E_NIT()
  : n() {
}

template <typename T>
inline
E_NIT<T>::E_NIT(const Coefficient& y) {
  if (y > 0 && y > max())
    n = pinf();
  if (y < 0 && y < min())
    n = minf();
  else
    Checked::assign<Checked::Transparent_Policy>(n, raw_value(y), ROUND_IGNORE);
}

template <typename T>
inline
E_NIT<T>::E_NIT(T y) {
  if (y > 0 && y > max())
    n = pinf();
  if (y < 0 && y < min())
    n = minf();
  else
    n = y;
}

template <typename T>
inline
E_NIT<T>::E_NIT(const E_NIT& y)
  : n(y.n) {
}

template <typename T>
inline
E_NIT<T>::E_NIT(const Plus_Infinity&)
  : n(pinf()) {
}

template <typename T>
inline E_NIT<T>&
E_NIT<T>::operator=(T y) {
  if (y > 0 && y > max())
    n = pinf();
  if (y < 0 && y < min())
    n = minf();
  else
    n = y;
  return *this;
}

template <typename T>
inline E_NIT<T>&
E_NIT<T>::operator=(E_NIT y) {
  n = y.n;
  return *this;
}

template <typename T>
inline E_NIT<T>&
E_NIT<T>::operator=(const Coefficient& y) {
  if (y > 0 && y > max())
    n = pinf();
  if (y < 0 && y < min())
    n = minf();
  else
    Checked::assign<Checked::Transparent_Policy>(n, raw_value(y), ROUND_IGNORE);
  return *this;
}

template <typename T>
inline E_NIT<T>&
E_NIT<T>::operator=(const Plus_Infinity&) {
  n = pinf();
}

template <typename T>
inline bool
E_NIT<T>::is_plus_infinity() const {
  return n == pinf();
}

template <typename T>
inline bool
E_NIT<T>::is_finite() const {
  return n >= min() && n <= max();
}

template <typename T>
inline bool
E_NIT<T>::is_zero() const {
  return n == 0;
}

template <typename T>
inline bool
E_NIT<T>::is_minus_infinity() const {
  return n == minf();
}

template <typename T>
inline bool
E_NIT<T>::is_nan() const {
  return n == nnum();
}

template <typename T>
inline bool
E_NIT<T>::is_positive() const {
  return n > 0;
}

template <typename T>
inline bool
E_NIT<T>::is_negative() const {
  return n < 0;
}

template <typename T>
inline bool
E_NIT<T>::is_nonpositive() const {
  return n <= 0;
}

template <typename T>
inline bool
E_NIT<T>::is_nonnegative() const {
  return n >= 0;
}

template <typename T>
inline E_NIT<T>
E_NIT<T>::plus_infinity() {
  return E_NIT(pinf());
}

#define PLUS_INFINITY plus_infinity<T>()

template <typename T>
inline E_NIT<T>
E_NIT<T>::finite_max() {
  return E_NIT(max());
}

template <typename T>
inline E_NIT<T>
E_NIT<T>::finite_min() {
  return E_NIT(min());
}

template <typename T>
inline E_NIT<T>
E_NIT<T>::minus_infinity() {
  return E_NIT(minf());
}

template <typename T>
inline E_NIT<T>
E_NIT<T>::not_a_number() {
  return E_NIT(nnum());
}

template <typename T>
inline T
E_NIT<T>::number() const {
  assert(is_finite());
  return n;
}

template <typename T>
inline void
E_NIT<T>::numer_denom(Coefficient& numer, Coefficient& denom) const {
  assert(is_finite());
  numer = number();
  denom = 1;
}

template <typename T>
inline bool
E_NIT<T>::OK() const {
  return true;
}

template <typename T>
inline bool
operator==(E_NIT<T> x, E_NIT<T> y) {
  return !x.is_nan() && !y.is_nan() && x.n == y.n;
}

template <typename T>
inline bool
operator!=(E_NIT<T> x, E_NIT<T> y) {
  return !(x == y);
}

template <typename T>
inline bool
operator<(E_NIT<T> x, E_NIT<T> y) {
  if (x.is_nan() || y.is_nan())
    return false;
  else
    return x.n < y.n;
}

template <typename T>
inline bool
operator<=(E_NIT<T> x, E_NIT<T> y) {
  if (x.is_nan() || y.is_nan())
    return false;
  else
    return x.n <= y.n;
}

template <typename T>
inline bool
operator>(E_NIT<T> x, E_NIT<T> y) {
  return y < x;
}

template <typename T>
inline bool
operator>=(E_NIT<T> x, E_NIT<T> y) {
  return y <= x;
}

template <typename T>
inline E_NIT<T>
operator+(E_NIT<T> x) {
  return x;
}

template <typename T>
inline E_NIT<T>
operator-(E_NIT<T> x) {
  // FIXME: we are making assumptions here that must be justified.
  E_NIT<T> r;
  if (x.is_nan())
    r.n = x.n;
  else
    r.n = -x.n;
  return r;
}

template <typename T>
inline E_NIT<T>
operator+(E_NIT<T> x, E_NIT<T> y) {
  E_NIT<T> r;
  if (x.is_nan())
    r.n = x.n;
  else if (y.is_nan())
    r.n = y.n;
  else if (x.is_plus_infinity())
    if (y.is_minus_infinity())
      r.n = E_NIT<T>::nnum();
    else
      r.n = x.n;
  else if (x.is_minus_infinity())
    if (y.is_plus_infinity())
      r.n = E_NIT<T>::nnum();
    else
      r.n = x.n;
  else if (y.is_plus_infinity())
    if (x.is_minus_infinity())
      r.n = E_NIT<T>::nnum();
    else
      r.n = y.n;
  else if (y.is_minus_infinity())
    if (x.is_plus_infinity())
      r.n = E_NIT<T>::nnum();
    else
      r.n = y.n;
  else if (x.n > 0 && y.n > 0)
    if (x.n > E_NIT<T>::max() - y.n)
      r.n = E_NIT<T>::pinf();
    else
      r.n = x.n + y.n;
  else if (x.n < 0 && y.n < 0)
    if (x.n < E_NIT<T>::min() - y.n)
      r.n = E_NIT<T>::minf();
    else
      r.n = x.n + y.n;
  else
    r.n = x.n + y.n;
  return r;
}

template <typename T>
inline E_NIT<T>
operator/(E_NIT<T> x, E_NIT<T> y) {
  E_NIT<T> r;
  if (y.n == 0)
    throw;
  else if (x.is_nan())
    r.n = x.n;
  else if (y.is_nan())
    r.n = y.n;
  else if (x.is_plus_infinity())
    if (y.is_plus_infinity() || y.is_minus_infinity())
      r.n = E_NIT<T>::nnum();
    else if (y.n > 0)
      r.n = x.n;
    else
      r.n = E_NIT<T>::minf();
  else if (x.is_minus_infinity())
    if (y.is_plus_infinity() || y.is_minus_infinity())
      r.n = E_NIT<T>::nnum();
    else if (y.n > 0)
      r.n = x.n;
    else
      r.n = E_NIT<T>::pinf();
  else if (y.is_plus_infinity() || y.is_minus_infinity())
    r.n = 0;
  else
    r.n = x.n/y.n;
  return r;
}

template <typename T>
inline E_NIT<T>
negate_round_up(E_NIT<T> x) {
  return -x;
}

template <typename T>
inline E_NIT<T>
negate_round_down(E_NIT<T> x) {
  return -x;
}

template <typename T>
inline E_NIT<T>
add_round_up(E_NIT<T> x, E_NIT<T> y) {
  return x + y;
}

template <typename T>
inline E_NIT<T>
add_round_down(E_NIT<T> x, E_NIT<T> y) {
  return x + y;
}

#if 0
template <typename T>
inline E_NIT<T>
div_round_up(E_NIT<T> x, E_NIT<T> y) {
  // We are assuming that integer division and remainder round
  // "towards-zero".  Notice that the rounding direction of signed
  // integer division is unspecified in C89/C94/C++98, and specified
  // to be "towards-zero" in C99.  A (non-normative) footnote in C++98,
  // says that
  //
  //   "According to work underway toward the revision of ISO C, the
  //   preferred algorithm for integer division follows the rules
  //   defined in the ISO Fortran standard, ISO/IEC 1539:1991, in which
  //   the quotient is always rounded toward zero."
  //
  // Moreover, we know of no C or C++ compiler that does not round
  // "towards-zero" for signed integer division and remainder.
  E_NIT<T> z = x/y;
  if (x%y != 0 && ((x >= 0 && y >= 0) || (x < 0 && y < 0)))
    ++z;
  return z;
}

template <typename T>
inline E_NIT<T>
div_round_down(E_NIT<T> x, E_NIT<T> y) {
  // We are assuming that integer division and remainder round
  // "towards-zero".  Notice that the rounding direction of signed
  // integer division is unspecified in C89/C94/C++98, and specified
  // to be "towards-zero" in C99.  A (non-normative) footnote in C++98,
  // says that
  //
  //   "According to work underway toward the revision of ISO C, the
  //   preferred algorithm for integer division follows the rules
  //   defined in the ISO Fortran standard, ISO/IEC 1539:1991, in which
  //   the quotient is always rounded toward zero."
  //
  // Moreover, we know of no C or C++ compiler that does not round
  // "towards-zero" for signed integer division and remainder.
  E_NIT<T> z = x/y;
  if (x%y != 0 && (x >= 0 || y >= 0) && (x < 0 || y < 0))
    --z;
  return z;
}
#endif

// FIXME: this implementation of div_round_up(const Coefficient&, const Coefficient&)
// is a hack to get around the lack of partial specialization of function
// templates in C++/98.

namespace {

template <typename T>
inline void
div_round_up(const Coefficient& x, const Coefficient& y, E_NIT<T>& z) {
  // GMP's division and remainder round towards zero.
  Coefficient r = x/y;
  if (x%y != 0 && ((x >= 0 && y >= 0) || (x < 0 && y < 0)))
    ++r;
  // `z' may be set to plus or minus infinity here, if type `T'
  // cannot represent `r'.
  z = r;
  if (z.is_minus_infinity())
    z = E_NIT<T>::finite_min();
}

template <typename T>
inline void
div_round_down(const Coefficient& x, const Coefficient& y, E_NIT<T>& z) {
  // GMP's division and remainder round towards zero.
  Coefficient r = x/y;
  if (x%y != 0 && (x >= 0 || y >= 0) && (x < 0 || y < 0))
    --r;
  // `z' may be set to plus or minus infinity here, if type `T'
  // cannot represent `r'.
  z = r;
  if (z.is_plus_infinity())
    z = E_NIT<T>::finite_max();
}

} // namespace

#define DEFINE_DIV_ROUND(direction, type) \
template <> \
inline E_NIT<type> \
div_round_##direction<E_NIT<type> >(const Coefficient& x, const Coefficient& y) { \
  E_NIT<type> z; \
  div_round_up(x, y, z); \
  return z; \
}

DEFINE_DIV_ROUND(up, signed char)
DEFINE_DIV_ROUND(up, int)
DEFINE_DIV_ROUND(up, long)
// DEFINE_DIV_ROUND(up, long long)
DEFINE_DIV_ROUND(down, signed char)
DEFINE_DIV_ROUND(down, int)
DEFINE_DIV_ROUND(down, long)
// DEFINE_DIV_ROUND(down, long long)

#undef DEFINE_DIV_ROUND

template <typename T>
inline E_NIT<T>
div_round_up(E_NIT<T> x, int y) {
  E_NIT<T> z;
  assert(x.is_finite());
  z = div_round_up<E_NIT<T> >(Coefficient(x.number()), Coefficient(y));
  return z;
}

template <typename T>
inline E_NIT<T>
div_round_down(E_NIT<T> x, int y) {
  E_NIT<T> z;
  assert(x.is_finite());
  z = div_round_down<E_NIT<T> >(Coefficient(x.number()), Coefficient(y));
  return z;
}

template <typename T>
inline bool
is_plus_infinity(const E_NIT<T>& x) {
  return x.is_plus_infinity();
}

template <typename T>
inline bool
is_nan(const E_NIT<T>& x) {
  return x.is_nan();
}

template <typename T>
inline bool
is_negative(const E_NIT<T>& x) {
  return x.is_negative();
}

template <typename T>
inline bool
is_nonnegative(const E_NIT<T>& x) {
  return x.is_nonnegative();
}

template <typename T>
inline bool
exact_neg(E_NIT<T>& to, const E_NIT<T>& x) {
  to = -x;
  return true;
}

template <typename T>
inline void
div_round_up(E_NIT<T>& to,
	     const Coefficient& x,
	     const Coefficient& y) {
  div_round_up(x, y, to);
}

template <typename T>
inline void
add_round_up(E_NIT<T>& to,
	     const E_NIT<T>& x,
	     const E_NIT<T>& y) {
  to = add_round_up(x, y);
}

template <typename T>
inline void add_round_down(E_NIT<T>& to,
			   const E_NIT<T>& x,
			   const E_NIT<T>& y) {
  to = add_round_down(x, y);
}

template <typename T>
inline void
negate_round_down(E_NIT<T>& to,
		  const E_NIT<T>& x) {
  to = negate_round_down(x);
}

template <typename T>
inline void
numer_denom(const E_NIT<T>& from,
	    Coefficient& num, Coefficient& den) {
  from.numer_denom(num, den);
}

namespace IO_Operators {

template <typename T>
inline std::ostream&
operator<<(std::ostream& s, E_NIT<T> x) {
  if (x.is_plus_infinity())
    s << "+inf";
  else if (x.is_minus_infinity())
    s << "-inf";
  else if (x.is_nan())
    s << "nan";
  else
    s << x.number();
  return s;
}

template <>
inline std::ostream&
operator<< <signed char>(std::ostream& s, E_NIT<signed char> x) {
  if (x.is_plus_infinity())
    s << "+inf";
  else if (x.is_minus_infinity())
    s << "-inf";
  else if (x.is_nan())
    s << "nan";
  else
    s << int(x.number());
  return s;
}

template <typename T>
inline std::istream&
operator>>(std::istream& s, E_NIT<T>& x) {
  std::string str;
  if (s >> str) {
    if (str == "nan")
      x.n = E_NIT<T>::nnum();
    else if (str == "-inf")
      x.n = E_NIT<T>::minf();
    else if (str == "+inf")
      x.n = E_NIT<T>::pinf();
    else {
      std::istringstream ss(str);
      if (!(ss >> x.n))
	s.setstate(std::ios::failbit); 
    }
  }
  return s;
}

template <>
inline std::istream&
operator>><signed char>(std::istream& s, E_NIT<signed char>& x) {
  std::string str;
  if (s >> str) {
    if (str == "nan")
      x.n = E_NIT<signed char>::nnum();
    else if (str == "-inf")
      x.n = E_NIT<signed char>::minf();
    else if (str == "+inf")
      x.n = E_NIT<signed char>::pinf();
    else {
      std::istringstream ss(str);
      int y;
      if (!(ss >> y))
	s.setstate(std::ios::failbit);
      else
	x = y;
    }
  }
  return s;
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_E_NIT_inlines_hh)
