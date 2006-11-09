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

struct Property {
  enum Type {
    UNBOUNDED_,
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

static const Property UNBOUNDED(Property::UNBOUNDED_);
static const Property OPEN(Property::OPEN_);

enum Type {
  LOWER = ROUND_DOWN,
  UPPER = ROUND_UP
};

inline Rounding_Dir
round_dir_nocheck(Type t) {
  return static_cast<Rounding_Dir>(t);
}

inline Rounding_Dir
round_dir_check(Type t) {
  return static_cast<Rounding_Dir>(t | ROUND_FPU_CHECK_INEXACT);
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

template <typename To, typename To_Info>
inline Result
set_minus_infinity(Type to_type, To& to, To_Info&) {
  Result r = assign_r(to, MINUS_INFINITY, round_dir_nocheck(to_type));
  assert(r != VC_PLUS_INFINITY);
  return r;
}

template <typename To, typename To_Info>
inline Result
set_plus_infinity(Type to_type, To& to, To_Info&) {
  Result r = assign_r(to, PLUS_INFINITY, round_dir_nocheck(to_type));
  assert(r != VC_PLUS_INFINITY);
  return r;
}

template <typename To, typename To_Info>
inline Result
set_unbounded(Type to_type, To& to, To_Info& to_info) {
  if (To_Info::store_unbounded)
    to_info.set_boundary_property(to_type, UNBOUNDED);
  else {
    Result r;
    if (to_type == LOWER)
      r = assign_r(to, MINUS_INFINITY, ROUND_NOT_NEEDED);
    else
      r = assign_r(to, PLUS_INFINITY, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    if (to_info.store_open) {
      to_info.set_boundary_property(to_type, OPEN);
      return V_EQ;
    }
    else
      return to_type == LOWER ? V_GT : V_LT;
  }
}

template <typename From, typename From_Info>
inline bool
is_unbounded(Type from_type, From& from, From_Info& from_info) {
  return from_info.get_boundary_property(from_type, UNBOUNDED)
    || (from_info.get_boundary_property(from_type, OPEN)
	&& (from_type == LOWER ? is_minus_infinity(from) : is_plus_infinity(from)));
}

template <typename T, typename Info>
inline void
sgn(Type type, T v, const Info& info) {
  if (info.get_boundary_property(type, UNBOUNDED))
    return type == LOWER ? -1 : 1;
  int sign = sgn(v);
  if (v == 0 && info.get_boundary_property(type, OPEN))
    return type == LOWER ? -1 : 1;
  return sign;
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
eq(Type from1_type, const From1& from1, const From1_Info& from1_info,
   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED))
    return from1_type == from2_type
      && is_unbounded(from2_type, from2, from2_info);
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED))
    return from1_type == from2_type
      && is_unbounded(from1_type, from1, from1_info);
  if (from1_info.get_boundary_property(from1_type, OPEN)) {
    if (from1_type != from2_type
	|| !from2_info.get_boundary_property(from2_type, OPEN))
	return false;
  }
  else if (from2_info.get_boundary_property(from2_type, OPEN))
    return false;
  return from1 == from2;
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
lt(Type from1_type, const From1& from1, const From1_Info& from1_info,
   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED)) {
    if (is_unbounded(from2_type, from2, from2_info))
      return from1_type == LOWER && from2_type == UPPER;
    else if (from1_type == LOWER)
      return !maybe_check_minus_infinity<From2_Info>(from2);
    else
      return maybe_check_plus_infinity<From2_Info>(from2);
  }
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED)) {
    if (is_unbounded(from1_type, from1, from1_info))
      return from1_type == LOWER && from2_type == UPPER;
    else if (from2_type == LOWER)
      return maybe_check_minus_infinity<From1_Info>(from1);
    else
      return !maybe_check_plus_infinity<From1_Info>(from1);
  }
  else if (from1_info.get_boundary_property(from1_type, OPEN)) {
    if (from1_type == UPPER && (from2_type == LOWER || !from2_info.get_boundary_property(from2_type, OPEN)))
      goto le;
  }
  else if (from2_type == LOWER && from2_info.get_boundary_property(from2_type, OPEN)) {
  le:
    return from1 <= from2;
  }
  return from1 < from2;
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
gt(Type from1_type, const From1& from1, const From1_Info& from1_info,
   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return lt(from2_type, from2, from2_info, from1_type, from1, from1_info);
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
le(Type from1_type, const From1& from1, const From1_Info& from1_info,
   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return !gt(from1_type, from1, from1_info, from2_type, from2, from2_info);
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
ge(Type from1_type, const From1& from1, const From1_Info& from1_info,
   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return !lt(from1_type, from1, from1_info, from2_type, from2, from2_info);
}

template <typename Info>
inline Result
adjust_boundary_info(Boundary_NS::Type type, Info& info, Result r) {
  if (type == LOWER) {
    switch (r) {
    case V_NEG_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, UNBOUNDED);
      return V_GT;
    case V_GT:
      info.set_boundary_property(type, OPEN);
      /* Fall through */
    case V_GE:
    case V_EQ:
      return r;
    default:
      assert(false);
      return VC_NAN;
    }
  }
  else {
    switch (r) {
    case V_POS_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, UNBOUNDED);
      return V_LT;
    case V_LT:
      info.set_boundary_property(type, OPEN);
      /* Fall through */
    case V_LE:
    case V_EQ:
      return r;
    default:
      assert(false);
      return VC_NAN;
    }
  }
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline Result
assign(Type to_type, To& to, To_Info& to_info,
       Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.get_boundary_property(from_type, UNBOUNDED)) {
    return set_unbounded(to_type, to, to_info);
  }
  if (To_Info::store_open) {
    if (from_info.get_boundary_property(from_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, assign_r(to, from, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, assign_r(to, from, round_dir_nocheck(to_type)));
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline Result
min_assign(Type to_type, To& to, To_Info& to_info,
	   Type from_type, const From& from, const From_Info& from_info) {
  if (lt(from_type, from, from_info, to_type, to, to_info)) {
    to_info.clear_boundary_properties(to_type);
    return assign(to_type, to, to_info, from_type, from, from_info);
  }
  return V_EQ;
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
min_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (lt(from1_type, from1, from1_info, from2_type, from2, from2_info))
    return assign(to_type, to, to_info, from1_type, from1, from1_info);
  else
    return assign(to_type, to, to_info, from2_type, from2, from2_info);
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline Result
max_assign(Type to_type, To& to, To_Info& to_info,
	   Type from_type, const From& from, const From_Info& from_info) {
  if (gt(from_type, from, from_info, to_type, to, to_info)) {
    to_info.clear_boundary_properties(to_type);
    return assign(to_type, to, to_info, from_type, from, from_info);
  }
  return V_EQ;
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
max_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (gt(from1_type, from1, from1_info, from2_type, from2, from2_info))
    return assign(to_type, to, to_info, from1_type, from1, from1_info);
  else
    return assign(to_type, to, to_info, from2_type, from2, from2_info);
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline Result
neg_assign(Type to_type, To& to, To_Info& to_info,
	   Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.get_boundary_property(from_type, UNBOUNDED)) {
    return set_unbounded(to_type, to, to_info);
  }
  if (To_Info::store_open) {
    if (from_info.get_boundary_property(from_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, round_dir_nocheck(to_type)));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
add_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  assert(from1_type == from2_type);
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED)) {
    if (is_unbounded(from2_type, from2, from2_info))
      goto unbounded;
    else if (maybe_check_minus_infinity<From2_Info>(from2))
      goto minf;
    else if (maybe_check_plus_infinity<From2_Info>(from2))
      goto pinf;
    else
      goto unbounded;
  }
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
    minf:
      return set_minus_infinity(to_type, to, to_info);
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
    pinf:
      return set_plus_infinity(to_type, to, to_info);
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.get_boundary_property(from1_type, OPEN) ||
	from2_info.get_boundary_property(from2_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, round_dir_nocheck(to_type)));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
sub_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  assert(from1_type != from2_type);
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED)) {
    if (is_unbounded(from2_type, from2, from2_info))
      goto unbounded;
    else if (maybe_check_minus_infinity<From2_Info>(from2))
      goto pinf;
    else if (maybe_check_plus_infinity<From2_Info>(from2))
	goto minf;
    else
      goto unbounded;
  }
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
    minf:
      return set_minus_infinity(to_type, to, to_info);
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
    pinf:
      return set_plus_infinity(to_type, to, to_info);
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.get_boundary_property(from1_type, OPEN) ||
	from2_info.get_boundary_property(from2_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, round_dir_nocheck(to_type)));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
mul_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED)) {
    if (is_unbounded(from2_type, from2, from2_info))
      goto unbounded;
    else if (maybe_check_minus_infinity<From2_Info>(from2)) {
      if (from1_type == LOWER)
	goto pinf;
      else
	goto minf;
    }
    else if (maybe_check_plus_infinity<From2_Info>(from2)) {
      if (from1_type == LOWER)
	goto minf;
      else
	goto pinf;
    }
    else
      goto unbounded;
  }
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER)
	goto pinf;
      else
	goto minf;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER) {
      minf:
	return set_minus_infinity(to_type, to, to_info);
      }
      else {
      pinf:
	return set_plus_infinity(to_type, to, to_info);
      }
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.get_boundary_property(from1_type, OPEN) ||
	from2_info.get_boundary_property(from2_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, mul_assign_r(to, from1, from2, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, mul_assign_r(to, from1, from2, round_dir_nocheck(to_type)));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline Result
div_assign(Type to_type, To& to, To_Info& to_info,
	   Type from1_type, const From1& from1, const From1_Info& from1_info,
	   Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.get_boundary_property(from1_type, UNBOUNDED)) {
    if (is_unbounded(from2_type, from2, from2_info))
      goto unbounded;
    else if (maybe_check_minus_infinity<From2_Info>(from2) ||
	     maybe_check_plus_infinity<From2_Info>(from2)) {
      assign_r(to, 0, ROUND_NOT_NEEDED);
      return V_EQ;
    }
    else
      goto unbounded;
  }
  else if (from2_info.get_boundary_property(from2_type, UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER)
	goto pinf;
      else
	goto minf;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER) {
      minf:
	return set_minus_infinity(to_type, to, to_info);
      }
      else {
      pinf:
	return set_plus_infinity(to_type, to, to_info);
      }
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.get_boundary_property(from1_type, OPEN) ||
	from2_info.get_boundary_property(from2_type, OPEN))
      to_info.set_boundary_property(to_type, OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, div_assign_r(to, from1, from2, round_dir_check(to_type)));
  }
  else
    return adjust_boundary_info(to_type, to_info, div_assign_r(to, from1, from2, round_dir_nocheck(to_type)));
}

}

}

#endif // !defined(PPL_Boundary_defs_hh)
