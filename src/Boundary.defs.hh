/* Interval boundary functions.
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

#ifndef PPL_Boundary_defs_hh
#define PPL_Boundary_defs_hh 1

#include "Checked_Number.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Boundary_NS {

enum Integer_Property_Value {
  MAYBE_SINGLETON_INTEGER = 0,
  NOT_SINGLETON_INTEGER = 1,
  ONLY_INTEGERS_TO_NORMALIZE = 2,
  ONLY_INTEGERS_NORMALIZED = 3
};

struct Integer_Property {
  typedef Integer_Property_Value Value;
  static const Value default_value = MAYBE_SINGLETON_INTEGER;
  static const Value unsupported_value = MAYBE_SINGLETON_INTEGER;
};

static const Integer_Property& INTEGER = *(Integer_Property*)0;

struct Property {
  enum Type {
    SPECIAL_,
    OPEN_
  };
  typedef bool Value;
  static const Value default_value = true;
  static const Value unsupported_value = false;
  Property(Type t)
    : type(t) {
  }
  Type type;
};

static const Property SPECIAL(Property::SPECIAL_);
static const Property OPEN(Property::OPEN_);

enum Type {
  LOWER = ROUND_DOWN,
  UPPER = ROUND_UP
};

inline Rounding_Dir
round_dir(Type t, bool check = false) {
  if (check)
    return static_cast<Rounding_Dir>(t | ROUND_FPU_CHECK_INEXACT);
  else
    return static_cast<Rounding_Dir>(t);
}

template <typename Policy, typename T>
inline bool
maybe_check_minus_infinity(const T& v) {
  return Policy::handle_infinity && is_minus_infinity(v);
}

template <typename Policy, typename T>
inline bool
maybe_check_plus_infinity(const T& v) {
  return Policy::handle_infinity && is_plus_infinity(v);
}

template <typename T, typename Info>
inline Result
special_set_unbounded(Type type, T&, Info& info) {
  info.set_boundary_property(type, SPECIAL);
  return V_EQ;
}

template <typename T, typename Info>
inline Result
special_set_minus_infinity(Type type, T& x, Info& info) {
  if (type == LOWER)
    return special_set_unbounded(type, x, info);
  else
    return assign_r(x, MINUS_INFINITY, round_dir(type));
}

template <typename T, typename Info>
inline Result
special_set_plus_infinity(Type type, T& x, Info& info) {
  if (type == UPPER)
    return special_set_unbounded(type, x, info);
  else
    return assign_r(x, PLUS_INFINITY, round_dir(type));
}


template <typename T, typename Info>
inline bool
special_is_unbounded(Type, const T&, const Info&) {
  return true;
}

template <typename T, typename Info>
inline bool
special_is_minus_infinity(Type type, const T&, const Info&) {
  return type == LOWER;
}

template <typename T, typename Info>
inline bool
special_is_plus_infinity(Type type, const T&, const Info&) {
  return type == UPPER;
}

template <typename T, typename Info>
inline bool
special_is_open(Type, const T&, const Info&) {
  return Info::infinity_is_open;
}

template <typename T, typename Info>
inline Result
set_unbounded(Type type, T& x, Info& info) {
  COMPILE_TIME_CHECK(Info::store_special
		     || std::numeric_limits<T>::is_bounded
		     || std::numeric_limits<T>::has_infinity,
		     "Unbounded is not representable");
  if (Info::store_special)
    return special_set_unbounded(type, x, info);
  else if (type == LOWER)
    return assign_r(x, MINUS_INFINITY, ROUND_UP);
  else
    return assign_r(x, PLUS_INFINITY, ROUND_DOWN);
}

template <typename T, typename Info>
inline Result
set_minus_infinity(Type type, T& x, Info& info, bool open = false) {
  COMPILE_TIME_CHECK(Info::store_special
		     || std::numeric_limits<T>::has_infinity,
		     "Minus infinity is not representable");
  if (type == UPPER)
    assert(open == false);
  else if (Info::infinity_is_open)
    assert(open == true);
  Result r;
  if (Info::store_special)
    r = special_set_minus_infinity(type, x, info);
  else
    r = assign_r(x, MINUS_INFINITY, round_dir(type));
  assert(r != VC_MINUS_INFINITY);
  if (!Info::infinity_is_open && (open || r != V_EQ))
    info.set_boundary_property(type, OPEN);
  return r;
}

template <typename T, typename Info>
inline Result
set_plus_infinity(Type type, T& x, Info& info, bool open = false) {
  COMPILE_TIME_CHECK(Info::store_special
		     || std::numeric_limits<T>::has_infinity,
		     "Minus infinity is not representable");
  if (type == LOWER)
    assert(open == false);
  else if (Info::infinity_is_open)
    assert(open == true);
  Result r;
  if (Info::store_special)
    r = special_set_plus_infinity(type, x, info);
  else
    r = assign_r(x, PLUS_INFINITY, round_dir(type));
  assert(r != VC_MINUS_INFINITY);
  if (!Info::infinity_is_open && (open || r != V_EQ))
    info.set_boundary_property(type, OPEN);
  return r;
}

template <typename T, typename Info>
inline Result
set_open(Type type, T& x, Info& info) {
  if (Info::store_open)
    info.set_boundary_property(type, OPEN);
  else
    switch (info.get_interval_property(INTEGER)) {
    case ONLY_INTEGERS_TO_NORMALIZE:
      assert(false);
      break;
    case ONLY_INTEGERS_NORMALIZED:
      // FIXME: check/no check?
      if (type == LOWER)
	return add_assign_r(x, x, T(1, ROUND_NOT_NEEDED), ROUND_DOWN);
      else
	return sub_assign_r(x, x, T(1, ROUND_NOT_NEEDED), ROUND_UP);
    default:
      break;
    }
  return V_EQ;
}

template <typename T, typename Info>
inline bool
is_unbounded(Type type, const T& x, const Info& info) {
  if (Info::store_special)
    return info.get_boundary_property(type, SPECIAL)
      && special_is_unbounded(type, x, info);
  else if (std::numeric_limits<T>::has_infinity) {
    if (type == LOWER)
      return is_minus_infinity(x);
    else
      return is_plus_infinity(x);
  }
  else if (std::numeric_limits<T>::is_bounded) {
    if (type == LOWER)
      return x == std::numeric_limits<T>::min();
    else
      return x == std::numeric_limits<T>::max();
  } else
    return false;
}

template <typename T, typename Info>
inline bool
is_minus_infinity(Type type, const T& x, const Info& info) {
  if (Info::store_special)
    return info.get_boundary_property(type, SPECIAL)
      && special_is_minus_infinity(type, x, info);
  else
    return Parma_Polyhedra_Library::is_minus_infinity(x);
}

template <typename T, typename Info>
inline bool
is_plus_infinity(Type type, const T& x, const Info& info) {
  if (Info::store_special)
    return info.get_boundary_property(type, SPECIAL)
      && special_is_plus_infinity(type, x, info);
  else
    return Parma_Polyhedra_Library::is_plus_infinity(x);
}

template <typename T, typename Info>
inline bool
is_any_infinity(Type type, const T& x, const Info& info) {
  return is_minus_infinity(type, x, info)
    || is_plus_infinity(type, x, info);
}

template <typename T, typename Info>
inline bool
is_boundary_infinity(Type type, const T& x, const Info& info) {
  if (Info::store_special)
    return info.get_boundary_property(type, SPECIAL)
      && special_is_unbounded(type, x, info);
  else if (type == LOWER)
    return is_minus_infinity(type, x, info);
  else
    return is_plus_infinity(type, x, info);
}

template <typename Info>
inline bool
infinity_is_open(Type type, const Info& info) {
  if (Info::store_open)
    return info.get_boundary_property(type, OPEN);
  else
    return Info::infinity_is_open;
}

template <typename T, typename Info>
inline bool
is_open(Type type, const T& x, const Info& info) {
  if (Info::store_open)
    return info.get_boundary_property(type, OPEN);
  else
    return Info::infinity_is_open
      && is_boundary_infinity(type, x, info);
}

template <typename T, typename Info>
inline void
sgn(Type type, const T& x, const Info& info) {
  if (info.get_boundary_property(type, SPECIAL))
    return type == LOWER ? -1 : 1;
  int sign = sgn(x);
  if (x == 0 && info.get_boundary_property(type, OPEN))
    return type == LOWER ? -1 : 1;
  return sign;
}

template <typename T1, typename Info1, typename T2, typename Info2>
inline bool
eq(Type type1, const T1& x1, const Info1& info1,
   Type type2, const T2& x2, const Info2& info2) {
  if (type1 == type2) {
    if (is_open(type1, x1, info1)
	!= is_open(type2, x2, info2))
      return false;
  }
  else if (is_open(type1, x1, info1)
	   || is_open(type2, x2, info2))
    return false;
  if (info1.get_boundary_property(type1, SPECIAL)) {
    if (type2 == LOWER)
      return is_minus_infinity(type2, x2, info2);
    else
      return is_plus_infinity(type2, x2, info2);
  }
  else if (info2.get_boundary_property(type2, SPECIAL)) {
    if (type1 == LOWER)
      return is_minus_infinity(type1, x1, info1);
    else
      return is_plus_infinity(type1, x1, info1);
  }
  else
    return x1 == x2;
}

template <typename T1, typename Info1, typename T2, typename Info2>
inline bool
lt(Type type1, const T1& x1, const Info1& info1,
   Type type2, const T2& x2, const Info2& info2) {
  if (info1.get_boundary_property(type1, SPECIAL)) {
    if (type1 == LOWER) {
      if (is_open(type1, x1, info1))
	return !is_minus_infinity(type2, x2, info2);
      else
	return is_open(type2, x2, info2)
	  || !is_minus_infinity(type2, x2, info2);
    }
    else
      return is_open(type1, x1, info1)
	&& !is_open(type2, x2, info2)
	&& is_plus_infinity(type2, x2, info2);
  }
  else if (info2.get_boundary_property(type2, SPECIAL)) {
    if (type2 == LOWER) {
      return is_open(type2, x2, info2)
	&& !is_open(type1, x1, info1)
	&& is_minus_infinity(type1, x1, info1);
    }
    else
      return is_open(type2, x2, info2)
	&& !is_open(type1, x1, info1)
	&& is_plus_infinity(type1, x1, info1);
  }
  else if (is_open(type1, x1, info1)) {
    if (type1 == UPPER
	&& (type2 == LOWER
	    || !is_open(type2, x2, info2)))
      goto le;
  }
  else if (type2 == LOWER
	   && is_open(type2, x2, info2)) {
  le:
    return x1 <= x2;
  }
  return x1 < x2;
}

template <typename T1, typename Info1, typename T2, typename Info2>
inline bool
gt(Type type1, const T1& x1, const Info1& info1,
   Type type2, const T2& x2, const Info2& info2) {
  return lt(type2, x2, info2, type1, x1, info1);
}

template <typename T1, typename Info1, typename T2, typename Info2>
inline bool
le(Type type1, const T1& x1, const Info1& info1,
   Type type2, const T2& x2, const Info2& info2) {
  return !gt(type1, x1, info1, type2, x2, info2);
}

template <typename T1, typename Info1, typename T2, typename Info2>
inline bool
ge(Type type1, const T1& x1, const Info1& info1,
   Type type2, const T2& x2, const Info2& info2) {
  return !lt(type1, x1, info1, type2, x2, info2);
}

template <typename T, typename Info>
inline Result
adjust_boundary(Boundary_NS::Type type, T& x, Info& info,
		bool open, Result r) {
  if (type == LOWER) {
    switch (r) {
    case V_NEG_OVERFLOW:
      open = true;
      /* Fall through */
    case VC_MINUS_INFINITY:
      return set_minus_infinity(type, x, info, open);
    case V_GT:
      set_open(type, x, info);
      // FIXME: what to return?
      return V_GT;
    case V_GE:
    case V_EQ:
      if (open)
	set_open(type, x, info);
      // FIXME: what to return?
      return r;
    default:
      assert(false);
      return VC_NAN;
    }
  }
  else {
    switch (r) {
    case V_POS_OVERFLOW:
      open = true;
      /* Fall through */
    case VC_PLUS_INFINITY:
      return set_plus_infinity(type, x, info, open);
    case V_LT:
      set_open(type, x, info);
      // FIXME: what to return?
      return V_LT;
    case V_LE:
    case V_EQ:
      if (open)
	set_open(type, x, info);
      // FIXME: what to return?
      return r;
    default:
      assert(false);
      return VC_NAN;
    }
  }
}

enum Operand { INF, INF0,
	       ADD1 = INF, ADD2 = INF,
	       SUB1 = INF, SUB2 = INF,
	       MUL1 = INF0, MUL2 = INF0,
	       DIV1 = INF0, DIV2 = INF };

template <typename T, typename Info>
inline bool
is_invariant(Operand o, Type type, const T& x, const Info& info) {
  switch (o) {
  case INF0:
    if (x == (signed char)0)
      return true;
    /* Fall through */
  case INF:
    return is_any_infinity(type, x, info);
  default:
    assert(false);
    return false;
  }
}

template <typename To, typename To_Info, typename T, typename Info>
inline Result
assign(Type to_type, To& to, To_Info& to_info,
       Type type, const T& x, const Info& info) {
  assert(to_type == type);
  if (info.get_boundary_property(type, SPECIAL)) {
    bool open = special_is_open(type, x, info);
    if (special_is_minus_infinity(type, x, info))
      return set_minus_infinity(to_type, to, to_info, open);
    else if (special_is_plus_infinity(type, x, info))
      return set_plus_infinity(to_type, to, to_info, open);
    else {
      assert(false);
      return VC_NAN;
    }
  }
  bool open = info.get_boundary_property(type, OPEN);
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 assign_r(to, x, dir));
}

template <typename To, typename To_Info, typename T, typename Info>
inline Result
min_assign(Type to_type, To& to, To_Info& to_info,
	   Type type, const T& x, const Info& info) {
  if (lt(type, x, info, to_type, to, to_info)) {
    to_info.clear_boundary_properties(to_type);
    return assign(to_type, to, to_info, type, x, info);
  }
  return V_EQ;
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
min_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
  if (lt(type1, x1, info1, type2, x2, info2))
    return assign(to_type, to, to_info, type1, x1, info1);
  else
    return assign(to_type, to, to_info, type2, x2, info2);
}

template <typename To, typename To_Info, typename T, typename Info>
inline Result
max_assign(Type to_type, To& to, To_Info& to_info,
	   Type type, const T& x, const Info& info) {
  if (gt(type, x, info, to_type, to, to_info)) {
    to_info.clear_boundary_properties(to_type);
    return assign(to_type, to, to_info, type, x, info);
  }
  return V_EQ;
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
max_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
  if (gt(type1, x1, info1, type2, x2, info2))
    return assign(to_type, to, to_info, type1, x1, info1);
  else
    return assign(to_type, to, to_info, type2, x2, info2);
}

template <typename To, typename To_Info, typename T, typename Info>
inline Result
neg_assign(Type to_type, To& to, To_Info& to_info,
	   Type type, const T& x, const Info& info) {
  assert(to_type != type);
  if (info.get_boundary_property(type, SPECIAL)) {
    bool open = special_is_open(type, x, info);
    if (special_is_minus_infinity(type, x, info))
      return set_plus_infinity(to_type, to, to_info, open);
    else if (special_is_plus_infinity(type, x, info))
      return set_minus_infinity(to_type, to, to_info, open);
    else {
      assert(false);
      return VC_NAN;
    }
  }
  bool open = info.get_boundary_property(type, OPEN);
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 neg_assign_r(to, x, dir));
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
add_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
  assert(type1 == type2);
#if 0
  if (info1.get_boundary_property(type1, SPECIAL)) {
    bool open1 = special_is_open(type1, x1, info1);
  }
  else if (info2.get_boundary_property(type2, SPECIAL)) {
    bool open2 = special_is_open(type2, x2, info2);
  }
  // FIXME: missing special check
  // FIXME: missing singularities check
#endif
  bool open = is_open(type1, x1, info1)
    ? (is_open(type2, x2, info2) || !is_invariant(ADD2, type2, x2, info2))
    : (is_open(type2, x2, info2) && !is_invariant(ADD1, type1, x1, info1));
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 add_assign_r(to, x1, x2, dir));
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
sub_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
  assert(type1 != type2);
#if 0
  // FIXME: missing special check
  // FIXME: missing singularities check
#endif
  bool open = is_open(type1, x1, info1)
    ? (is_open(type2, x2, info2) || !is_invariant(SUB2, type2, x2, info2))
    : (is_open(type2, x2, info2) && !is_invariant(SUB1, type1, x1, info1));
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 sub_assign_r(to, x1, x2, dir));
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
mul_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
#if 0
  // FIXME: missing special check
  // FIXME: missing singularities check
#endif
  bool open = is_open(type1, x1, info1)
    ? (is_open(type2, x2, info2) || !is_invariant(MUL2, type2, x2, info2))
    : (is_open(type2, x2, info2) && !is_invariant(MUL1, type1, x1, info1));
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 mul_assign_r(to, x1, x2, dir));
}

template <typename To, typename To_Info, typename T1, typename Info1, typename T2, typename Info2>
inline Result
div_assign(Type to_type, To& to, To_Info& to_info,
	   Type type1, const T1& x1, const Info1& info1,
	   Type type2, const T2& x2, const Info2& info2) {
#if 0
  // FIXME: missing special check
  // FIXME: missing singularities check
#endif
  bool open = is_open(type1, x1, info1)
    ? (is_open(type2, x2, info2) || !is_invariant(DIV2, type2, x2, info2))
    : (is_open(type2, x2, info2) && !is_invariant(DIV1, type1, x1, info1));
  Rounding_Dir dir = round_dir(to_type, (To_Info::check_inexact
					 || (To_Info::store_open && !open)));
  return adjust_boundary(to_type, to, to_info, open,
			 div_assign_r(to, x1, x2, dir));
}

}

}

#endif // !defined(PPL_Boundary_defs_hh)
