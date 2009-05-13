/* Interval class implementation: non-inline template functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Interval_templates_hh
#define PPL_Interval_templates_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename Boundary, typename Info>
template <typename From, typename Iterator>
typename Enable_If<Is_Interval<From>::value, void>::type
Interval<Boundary, Info>::CC76_widening_assign(const From& y,
					       Iterator first,
					       Iterator last) {
  // We assume that `y' is contained in or equal to `*this'.
  assert(contains(y));
  Interval<Boundary, Info>& x = *this;

  // Upper bound.
  if (!x.upper_is_boundary_infinity()) {
    Boundary& x_ub = x.upper();
    const Boundary& y_ub = y.upper();
    assert(!y.upper_is_boundary_infinity() && y_ub <= x_ub);
    if (y_ub < x_ub) {
      Iterator k = std::lower_bound(first, last, x_ub);
      if (k != last) {
	if (x_ub < *k)
	  x_ub = *k;
      }
      else
	x.upper_extend();
    }
  }

  // Lower bound.
  if (!x.lower_is_boundary_infinity()) {
    Boundary& x_lb = x.lower();
    const Boundary& y_lb = y.lower();
    assert(!y.lower_is_boundary_infinity() && y_lb >= x_lb);
    if (y_lb > x_lb) {
      Iterator k = std::lower_bound(first, last, x_lb);
      if (k != last) {
	if (x_lb < *k) {
	  if (k != first)
	    x_lb = *--k;
	  else
	    x.lower_extend();
	}
      }
      else
	x_lb = *--k;
    }
  }
}

template <typename Boundary, typename Info>
inline std::istream&
operator>>(std::istream& is, Interval<Boundary, Info>& x) {
  // Eat leading white space.
  int c;
  do {
    c = is.get();
  } while (isspace(c));

  // Get the opening parenthesis and handle the empty interval case.
  bool lower_open = false;
  if (c == '(')
    lower_open = true;
  else if (c == '[') {
    c = is.get();
    if (c == ']') {
      // Empty interval.
      x.assign(EMPTY);
      return is;
    }
    else
      is.unget();
  }
  else {
    is.unget();
    is.setstate(std::ios_base::failbit);
    return is;
  }

  // Get the lower bound.
  Boundary lower_bound;
  Result lower_r  = input(lower_bound, is, ROUND_DOWN);
  if (lower_r == V_CVT_STR_UNK || lower_r == V_NAN) {
    is.setstate(std::ios_base::failbit);
    return is;
  }
  lower_r = result_relation_class(lower_r);

  // Match the comma separating the lower and upper bounds.
  do {
    c = is.get();
  } while (isspace(c));
  if (c != ',') {
    is.unget();
    is.setstate(std::ios_base::failbit);
    return is;
  }

  // Get the upper bound.
  Boundary upper_bound;
  Result upper_r = input(upper_bound, is, ROUND_UP);
  if (upper_r == V_CVT_STR_UNK || upper_r == V_NAN) {
    is.setstate(std::ios_base::failbit);
    return is;
  }
  upper_r = result_relation_class(upper_r);

  // Get the closing parenthesis.
  do {
    c = is.get();
  } while (isspace(c));
  bool upper_open = false;
  if (c == ')')
    upper_open = true;
  else if (c != ']') {
    is.unget();
    is.setstate(std::ios_base::failbit);
    return is;
  }

  // Buld interval.
  bool lower_boundary_infinity = false;
  bool upper_boundary_infinity = false;
  switch (lower_r) {
  case V_EQ:
    break;
  case V_LE:
    lower_open = true;
    break;
  case V_EQ_MINUS_INFINITY:
  case V_GT_MINUS_INFINITY:
    lower_boundary_infinity = true;
    break;
  case V_EQ_PLUS_INFINITY:
  case V_LT_PLUS_INFINITY:
    if (upper_r == V_EQ_PLUS_INFINITY || upper_r == V_LT_PLUS_INFINITY)
      x.assign(UNIVERSE);
    else
      x.assign(EMPTY);
    return is;
  default:
    assert(false);
  }
  switch (upper_r) {
  case V_EQ:
    break;
  case V_GE:
    upper_open = true;
    break;
  case V_EQ_MINUS_INFINITY:
  case V_GT_MINUS_INFINITY:
    if (lower_r == V_EQ_MINUS_INFINITY || lower_r == V_GT_MINUS_INFINITY)
      x.assign(UNIVERSE);
    else
      x.assign(EMPTY);
    return is;
  case V_EQ_PLUS_INFINITY:
  case V_LT_PLUS_INFINITY:
    upper_boundary_infinity = true;
    break;
  default:
    assert(false);
  }

  if (!lower_boundary_infinity
      && !upper_boundary_infinity
      && (lower_bound > upper_bound
	  || (lower_open && lower_bound == upper_bound)))
    x.assign(EMPTY);
  else {
    if (lower_boundary_infinity)
      special_set_boundary_infinity(LOWER, x.lower(), x.info());
    else
      assign(LOWER, x.lower(), x.info(), LOWER, lower_bound, SCALAR_INFO, lower_open);
    if (upper_boundary_infinity)
      special_set_boundary_infinity(UPPER, x.upper(), x.info());
    else
      assign(UPPER, x.upper(), x.info(), UPPER, upper_bound, SCALAR_INFO, upper_open);
  }
  return is;
}

template <typename Boundary, typename Info>
template <typename From>
typename Enable_If<Is_Interval<From>::value, bool>::type
Interval<Boundary, Info>::simplify_using_context_assign(const From& y) {
  // FIXME: the following code wrongly assumes that intervals are closed
  // and have no restrictions. It must be generalized.
  if (lt(UPPER, upper(), info(), LOWER, f_lower(y), f_info(y))) {
    lower_extend();
    return false;
  }
  if (gt(LOWER, lower(), info(), UPPER, f_upper(y), f_info(y))) {
    upper_extend();
    return false;
  }
  // Weakening the upper bound.
  if (!upper_is_boundary_infinity() && !y.upper_is_boundary_infinity()
      && y.upper() <= upper())
    upper_extend();
  // Weakening the lower bound.
  if (!lower_is_boundary_infinity() && !y.lower_is_boundary_infinity()
      && y.lower() >= lower())
    lower_extend();
  return true;
}

template <typename Boundary, typename Info>
template <typename From>
typename Enable_If<Is_Interval<From>::value, void>::type
Interval<Boundary, Info>::empty_intersection_assign(const From&) {
  // FIXME: write me.
  assign(EMPTY);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Interval_templates_hh)
