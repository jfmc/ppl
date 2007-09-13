/* Interval class implementation: non-inline template functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace Parma_Polyhedra_Library {

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
  if (lower_r == V_CVT_STR_UNK || lower_r == VC_NAN) {
    is.setstate(std::ios_base::failbit);
    return is;
  }

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
  if (upper_r == V_CVT_STR_UNK || upper_r == VC_NAN) {
    is.setstate(std::ios_base::failbit);
    return is;
  }

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
  bool lower_unbounded = false;
  bool upper_unbounded = false;
  switch (lower_r) {
  case V_EQ:
    break;
  case V_LE:
    lower_open = true;
    break;
  case VC_MINUS_INFINITY:
  case V_NEG_OVERFLOW:
    lower_unbounded = true;
    break;
  case VC_PLUS_INFINITY:
  case V_POS_OVERFLOW:
    if (upper_r == VC_PLUS_INFINITY || upper_r == V_POS_OVERFLOW)
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
  case VC_MINUS_INFINITY:
  case V_NEG_OVERFLOW:
    if (lower_r == VC_MINUS_INFINITY || lower_r == V_NEG_OVERFLOW)
      x.assign(UNIVERSE);
    else
      x.assign(EMPTY);
    return is;
  case VC_PLUS_INFINITY:
  case V_POS_OVERFLOW:
    upper_unbounded = true;
    break;
  default:
    assert(false);
  }

  if (!lower_unbounded
      && !upper_unbounded
      && (lower_bound > upper_bound
	  || (lower_open && lower_bound == upper_bound)))
    x.assign(EMPTY);
  else {
    x.assign(UNIVERSE);
    if (!lower_unbounded)
      x.refine_existential((lower_open ? GREATER_THAN : GREATER_OR_EQUAL),
			   lower_bound);
    if (!upper_unbounded)
      x.refine_existential((upper_open ? LESS_THAN : LESS_OR_EQUAL),
			   upper_bound);
  }
  return is;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Interval_templates_hh)
