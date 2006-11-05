/* Interval class declaration.
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

#ifndef PPL_Interval_defs_hh
#define PPL_Interval_defs_hh 1

#include "globals.types.hh"
#include "Interval.types.hh"
#include "Interval_Info.defs.hh"
#include <iosfwd>

// Temporary!
#include <iostream>

namespace Parma_Polyhedra_Library {

using namespace Boundary;
using namespace Interval_;

template <typename Boundary, typename Info>
class Interval : private Info {
private:
  Info& w_info() const {
    return const_cast<Interval&>(*this);
  }
  bool is_empty_() const {
    return lt(UPPER, upper(), info(), LOWER, lower(), info());
  }
  bool is_singleton_() const {
    return eq(LOWER, lower(), info(), UPPER, upper(), info());
  }
  bool is_singleton_integer() const {
    return is_singleton() && is_integer(lower());
  }

public:
  typedef Boundary boundary_type;

  typedef Interval_::Property Property;

  bool OK() const {
    if (is_not_a_number(lower())) {
#ifndef NDEBUG
      std::cerr << "The lower boundary is not a number." << std::endl;
#endif
      return false;
    }
    if (is_not_a_number(upper())) {
#ifndef NDEBUG
      std::cerr << "The upper boundary is not a number." << std::endl;
#endif
      return false;
    }
    if (info().test_boundary_property(LOWER, UNBOUNDED) &&
	lower_is_open()) {
#ifndef NDEBUG
      std::cerr << "The lower boundary is marked unbounded and open." << std::endl;
#endif
      return false;
    }
    if (info().test_boundary_property(UPPER, UNBOUNDED) &&
	upper_is_open()) {
#ifndef NDEBUG
      std::cerr << "The upper boundary is marked unbounded and open." << std::endl;
#endif
      return false;
    }
    if (info().test_interval_property(CARDINALITY_0) &&
	info().test_interval_property(CARDINALITY_IS) != is_empty_()) {
#ifndef NDEBUG
      std::cerr << "The interval empty flag is incongruent with actual content." << std::endl;
#endif
      return false;
    }
    if (info().test_interval_property(CARDINALITY_1) &&
	info().test_interval_property(CARDINALITY_IS) != is_singleton_()) {
#ifndef NDEBUG
      std::cerr << "The interval singleton flag is incongruent with actual content." << std::endl;
#endif
      return false;
    }
#if 0
    if (info().test_interval_property(ONLY_INTEGERS)) {
      if (!is_integer(lower())) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to contain only integers, but lower boundary is not an integer." << std::endl;
#endif
	return false;
      }
      if (!is_integer(upper())) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to contain only integers, but upper boundary is not an integer." << std::endl;
#endif
	return false;
      }
    }
#endif
    if (info().test_interval_property(NOT_ONLY_INTEGERS) &&
	is_singleton_integer()) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to contain not only integer, but actual content is a singleton integer." << std::endl;
#endif
      return false;
    }
    if (info().test_interval_property(CARDINALITY_IS) &&
	info().test_interval_property(CARDINALITY_0) == info().test_interval_property(CARDINALITY_1)) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to know its cardinality, but this is unspecified or ambiguous." << std::endl;
#endif
      return false;
    }

    // Everything OK.
    return true;
  }

  Info& info() {
    return *this;
  }
  const Info& info() const {
    return *this;
  }
  Boundary& lower() {
    return lower_;
  }
  const Boundary& lower() const {
    return lower_;
  }
  Boundary& upper() {
    return upper_;
  }
  const Boundary& upper() const {
    return upper_;
  }
  I_Result set_empty() {
    assign_r(lower(), 1, ROUND_NOT_NEEDED);
    assign_r(upper(), 0, ROUND_NOT_NEEDED);
    return I_EMPTY;
  }
  bool is_empty() const {
    if (info().test_interval_property(CARDINALITY_IS))
      return info().test_interval_property(CARDINALITY_0);
    else if (info().test_interval_property(CARDINALITY_0))
      return false;
    if (is_empty_()) {
      w_info().set_interval_property(CARDINALITY_IS);
      w_info().set_interval_property(CARDINALITY_0);
      w_info().set_interval_property(CARDINALITY_1, false);
      return true;
    }
    else {
      w_info().set_interval_property(CARDINALITY_0);
      return false;
    }
  }
  bool is_singleton() const {
    if (info().test_interval_property(CARDINALITY_IS))
      return info().test_interval_property(CARDINALITY_1);
    else if (info().test_interval_property(CARDINALITY_1))
      return false;
    if (is_singleton_()) {
      w_info().set_interval_property(CARDINALITY_IS);
      w_info().set_interval_property(CARDINALITY_0, false);
      w_info().set_interval_property(CARDINALITY_1);
      return true;
    }
    else {
      w_info().set_interval_property(CARDINALITY_1);
      return false;
    }
  }
  bool contains_only_integers() const {
    if (info().test_interval_property(ONLY_INTEGERS))
      return true;
    if (info().test_interval_property(NOT_ONLY_INTEGERS))
      return false;
    if (is_singleton_integer()) {
      w_info().set_interval_property(ONLY_INTEGERS);
      return true;
    }
    else {
      w_info().set_interval_property(NOT_ONLY_INTEGERS);
      return false;
    }
  }
  bool lower_is_open() const {
    return info().test_boundary_property(LOWER, OPEN);
  }
  bool upper_is_open() const {
    return info().test_boundary_property(UPPER, OPEN);
  }
  void lower_set_open(bool v = true) {
    return info().set_boundary_property(LOWER, OPEN, v);
  }
  void upper_set_open(bool v = true) {
    return info().set_boundary_property(UPPER, OPEN, v);
  }
  bool lower_is_unbounded() const {
    return Boundary::is_unbounded(LOWER, lower(), info());
  }
  bool upper_is_unbounded() const {
    return Boundary::is_unbounded(UPPER, upper(), info());
  }
  void lower_set_unbounded() {
    return set_unbounded(LOWER, lower(), info());
  }
  void upper_set_unbounded() {
    return set_unbounded(UPPER, upper(), info());
  }
  bool is_unbounded() const {
    return lower_is_unbounded() || upper_is_unbounded();
  }
  bool is_universe() const {
    return lower_is_unbounded() && upper_is_unbounded()
      && !contains_only_integers();
  }
  bool is_topologically_closed() const {
    return is_empty() ||
      ((!lower_is_open() || lower_is_unbounded())
       && (!upper_is_open() || upper_is_unbounded()));
  }
  bool contains_integer_point() const {
    Boundary l;
    if (lower_is_open()) {
      add_assign_r(l, lower(), Boundary(1), ROUND_DOWN);
      floor_assign_r(l, l, ROUND_DOWN);
    }
    else
      ceil_assign_r(l, lower(), ROUND_DOWN);
    Boundary u;
    if (upper_is_open()) {
      sub_assign_r(l, upper(), Boundary(1), ROUND_UP);
      ceil_assign_r(l, l, ROUND_UP);
    }
    else
      floor_assign_r(l, upper(), ROUND_UP);
    return u >= l;
  }
  Boundary lower_;
  Boundary upper_;
};

template <typename Boundary, typename Info>
inline bool
contains_only_integers(const Interval<Boundary, Info>& x) {
  return x.contains_only_integers();
}
template <typename Boundary, typename Info>
inline bool
is_empty(const Interval<Boundary, Info>& x) {
  return x.is_empty();
}
template <typename Boundary, typename Info>
inline bool
is_singleton(const Interval<Boundary, Info>& x) {
  return x.is_singleton();
}

namespace Interval_ {

template <typename Boundary, typename Info>
inline const Boundary&
lower(const Interval<Boundary, Info>& x) {
  return x.lower();
}
template <typename Boundary, typename Info>
inline const Boundary&
upper(const Interval<Boundary, Info>& x) {
  return x.upper();
}
template <typename Boundary, typename Info>
inline const Info&
info(const Interval<Boundary, Info>& x) {
  return x.info();
}
template <typename Boundary, typename Info>
inline const Info&
info_open(const Interval<Boundary, Info>& x) {
  // FIXME: do the right thing!!!
  return x.info();
}

struct Scalar_As_Interval_Policy {
  static const bool handle_infinity = false;
  static const bool check_inexact = false;
  static const bool check_empty_args = false;
  static const bool check_integer_args = true;
};

typedef Interval_Info_Null<Scalar_As_Interval_Policy> Scalar_As_Interval_Info;

template <typename T>
inline const T&
lower(const T& x) {
  return x;
}
template <typename T>
inline const T&
upper(const T& x) {
  return x;
}
template <typename T>
inline const Scalar_As_Interval_Info&
info(const T&) {
  return *static_cast<Scalar_As_Interval_Info*>(0);
}
template <typename T>
inline const Scalar_As_Interval_Info&
info_open(const T&) {
  // FIXME: do the right thing!!!
  return *static_cast<Scalar_As_Interval_Info*>(0);
}
template <typename T>
inline bool
is_empty(const T& x) {
  return is_not_a_number(x);
}
template <typename T>
inline bool
contains_only_integers(const T& x) {
  return is_integer(x);
}
template <typename T>
inline bool
is_singleton(const T& x) {
  return true;
}

template <typename T>
inline bool
maybe_check_empty(const T& x) {
  if (info(x).check_empty_args)
    return is_empty(x);
  else {
    assert(is_empty(x));
    return false;
  }
}

template <typename T>
inline bool
check_integer(const T& x) {
  // TOTHINK: use the policy for x or for the result?
  if (info(x).check_integer_args)
    return contains_only_integers(x);
  else
    return info(x).test_interval_property(ONLY_INTEGERS);
}

}

inline bool
is_integer(const char*) {
  // FIXME:
  return false;
}

inline bool
is_not_a_number(const char*) {
  // FIXME:
  return false;
}

template <typename T1, typename T2>
inline bool
same_object(const T1&, const T2&) {
  return false;
}

template <typename T>
inline bool
same_object(const T& x, const T& y) {
  return &x == &y;
}

// FIXME: add specializations for gmp (others?)
template <typename T>
inline void
assign_or_swap(T& to, const T& from) {
  to = from;
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From1& l, const From2& u, bool integer = false) {
  if (maybe_check_empty(l) || maybe_check_empty(u))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign(LOWER, to.lower(), to_info, LOWER, l, info(l));
  I_Result ru = assign(UPPER, to.upper(), to_info, UPPER, u, info(u));
  if (integer)
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign(LOWER, to.lower(), to_info,
		       LOWER, lower(x), info(x));
  I_Result ru = assign(UPPER, to.upper(), to_info,
		       UPPER, upper(x), info(x));
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (maybe_check_empty(to))
    return assign(to, x);
  if (maybe_check_empty(x))
    return static_cast<I_Result>(I_L_EQ | I_U_EQ);
  if (To_Info::store_integer) {
    if (info(x).test_interval_property(NOT_ONLY_INTEGERS)) {
      to.info().set_interval_property(NOT_ONLY_INTEGERS);
      to.info().set_interval_property(ONLY_INTEGERS, false);
    }
    else if (to.info().test_interval_property(ONLY_INTEGERS)
	     && !check_integer(x))
      to.info().set_interval_property(ONLY_INTEGERS, false);
  }
  if (to.info().test_interval_property(CARDINALITY_IS)) {
    to.info().set_interval_property(CARDINALITY_IS, false);
    to.info().set_interval_property(CARDINALITY_0, true);
    to.info().set_interval_property(CARDINALITY_1, false);
  }
  I_Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = max_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x))
    return assign(to, y);
  if (maybe_check_empty(y))
    return assign(to, x);
  To_Info to_info;
  if (To_Info::store_integer) {
    if (check_integer(x)) {
      if (check_integer(y))
	to_info.set_interval_property(ONLY_INTEGERS);
    }
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS)
	     || info(y).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  I_Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = max_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (To_Info::store_integer) {
    if (!to.info().test_interval_property(ONLY_INTEGERS)) {
      if (check_integer(x))
	to.info().set_interval_property(ONLY_INTEGERS);
      if (!info(x).test_interval_property(NOT_ONLY_INTEGERS))
	to.info().set_interval_property(NOT_ONLY_INTEGERS, false);
    }
  }
  to.info().set_interval_property(CARDINALITY_IS, false);
  to.info().set_interval_property(CARDINALITY_0, false);
  to.info().set_interval_property(CARDINALITY_1, false);
  I_Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  // FIXME: boundary normalization for integer interval?
  // FIXME: check empty (policy based)?
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  To_Info to_info;
  if (To_Info::store_integer) {
    if (check_integer(x) || check_integer(y))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS)
	     && info(y).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  I_Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = min_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  // FIXME: boundary normalization for integer interval?
  // FIXME: check empty (policy based)?
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
refine(Interval<To_Boundary, To_Info>& to, Relation_Symbol rel, const From& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  switch (rel) {
  case LESS_THAN:
    {
      I_Result ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info_open(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return static_cast<I_Result>(I_L_EQ | ru);
    }
  case LESS_THAN_OR_EQUAL:
    {
      I_Result ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return static_cast<I_Result>(I_L_EQ | ru);
    }
  case GREATER_THAN:
    {
      I_Result rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info_open(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return static_cast<I_Result>(rl | I_U_EQ);
    }
  case GREATER_THAN_OR_EQUAL:
    {
      I_Result rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return static_cast<I_Result>(rl | I_U_EQ);
    }
  case EQUAL:
    return intersect_assign(to, x);
  case NOT_EQUAL:
    {
      if (!is_singleton(x) || !To_Info::store_open)
	return static_cast<I_Result>(I_L_EQ | I_U_EQ);
      I_Result rl, ru;
      if (!to.lower_is_open() && to.lower() == lower(x)) {
	to.lower_set_open();
	rl = I_L_GT;
      }
      else
	rl = I_L_EQ;
      if (!to.upper_is_open() && to.upper() == lower(x)) {
	to.upper_set_open();
	ru = I_U_LT;
      }
      else
	ru = I_U_EQ;
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return static_cast<I_Result>(rl | ru);
    }
  default:
    assert(false);
    return I_EMPTY;
  }
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
operator==(const Interval<Boundary, Info>& x, const T& y) {
  if (x.is_empty())
    return is_empty(y);
  else if (is_empty(y))
    return false;
  return x.contains_only_integers() == contains_only_integers(y)
    && eq(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
    && eq(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y));
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
operator!=(const Interval<Boundary, Info>& x, const T& y) {
  return !(x == y);
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
contains(const Interval<Boundary, Info>& x, const T& y) {
  return le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
    && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y))
    && (!x.contains_only_integers() || contains_only_integers(y));
}

template <typename To_Boundary, typename To_Info,
	  typename T>
inline I_Result
neg_assign(Interval<To_Boundary, To_Info>& to, const T& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), lower(x))) {
    static To_Boundary to_lower;
    rl = neg_assign(LOWER, to_lower, to_info, UPPER, upper(x), info(x));
    ru = neg_assign(UPPER, to.upper(), to_info, LOWER, lower(x), info(x));
    assign_or_swap(to.lower(), to_lower);
  } else {
    rl = neg_assign(LOWER, to.lower(), to_info, UPPER, upper(x), info(x));
    ru = neg_assign(UPPER, to.upper(), to_info, LOWER, lower(x), info(x));
  }
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
add_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = add_assign(LOWER, to.lower(), to_info,
			   LOWER, lower(x), info(x),
			   LOWER, lower(y), info(y));
  I_Result ru = add_assign(UPPER, to.upper(), to_info,
			   UPPER, upper(x), info(x),
			   UPPER, upper(y), info(y));
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
sub_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), lower(y))) {
    static To_Boundary to_lower;
    rl = sub_assign(LOWER, to_lower, to_info,
		    LOWER, lower(x), info(x),
		    UPPER, upper(y), info(y));
    ru = sub_assign(UPPER, to.upper(), to_info,
		    UPPER, upper(x), info(x),
		    LOWER, lower(y), info(y));
    assign_or_swap(to.lower(), to_lower);
  }
  else {
    rl = sub_assign(LOWER, to.lower(), to_info,
		    LOWER, lower(x), info(x),
		    UPPER, upper(y), info(y));
    ru = sub_assign(UPPER, to.upper(), to_info,
		    UPPER, upper(x), info(x),
		    LOWER, lower(y), info(y));
  }
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename Boundary, typename Info>
inline std::ostream&
operator<<(std::ostream& os, const Interval<Boundary, Info>& x) {
  if (x.is_empty())
    return os << "[]";
  if (x.is_singleton())
    return os << x.lower();
  if (x.info().test_boundary_property(LOWER, UNBOUNDED))
    os << "(-inf";
  else {
    os << (x.lower_is_open() ? "(" : "[");
    os << x.lower();
  }
  os << (x.contains_only_integers() ? " .. " : ", ");
  if (x.info().test_boundary_property(UPPER, UNBOUNDED))
    os << "+inf)";
  else {
    os << x.upper();
    os << (x.upper_is_open() ? ")" : "]");
  }
  return os;
}

}

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
