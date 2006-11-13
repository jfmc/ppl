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

enum I_Result {
  I_EMPTY = 0,
  I_L_EQ = V_EQ,
  I_L_GT = V_GT,
  I_L_GE = V_GE,
  I_U_EQ = V_EQ << 6,
  I_U_LT = V_LT << 6,
  I_U_LE = V_LE << 6,
  I_SINGULARITIES = 1 << 12
};

inline I_Result
combine(Result l, Result u) {
  return static_cast<I_Result>(l | (u << 6));
}

using namespace Boundary_NS;
using namespace Interval_NS;

template <typename Boundary, typename Info>
class Interval : private Info {
private:
  COMPILE_TIME_CHECK(!Info::store_special || !std::numeric_limits<Boundary>::has_infinity, "store_special is senseless when boundary type may contains infinity");
  COMPILE_TIME_CHECK(!Info::infinity_is_open || !Info::store_open, "infinity_is_open is senseless when boundary may be marked as open");
  Info& w_info() const {
    return const_cast<Interval&>(*this);
  }
  bool is_empty_() const {
    normalize_integer();
    return lt(UPPER, upper(), info(), LOWER, lower(), info());
  }
  bool is_singleton_() const {
    normalize_integer();
    return eq(LOWER, lower(), info(), UPPER, upper(), info());
  }
  bool is_singleton_integer() const {
    return is_singleton() && is_integer(lower());
  }
  I_Result normalize_integer_() const {
    Interval& i = const_cast<Interval&>(*this);
    Result rl = V_EQ;
    if (!info().get_boundary_property(LOWER, SPECIAL)) {
      if (is_integer(lower())) {
	if (lower_is_open()) {
	  i.info().set_boundary_property(LOWER, OPEN, false);
	  rl = adjust_boundary(LOWER, i.lower(), i.info(), false,
			       add_assign_r(i.lower(), lower(), Boundary(1, ROUND_NOT_NEEDED), ROUND_DOWN));
	}
      }
      else {
	i.info().set_boundary_property(LOWER, OPEN, false);
	rl = adjust_boundary(LOWER, i.lower(), i.info(), false,
			     ceil_assign_r(i.lower(), lower(), ROUND_DOWN));
      }
    }
    Result ru = V_EQ;
    if (!info().get_boundary_property(UPPER, SPECIAL)) {
      if (is_integer(upper())) {
	if (upper_is_open()) {
	  i.info().set_boundary_property(UPPER, OPEN, false);
	  ru = adjust_boundary(UPPER, i.upper(), i.info(), false,
			       sub_assign_r(i.upper(), upper(), Boundary(1, ROUND_NOT_NEEDED), ROUND_UP));
	}
      }
      else {
	i.info().set_boundary_property(UPPER, OPEN, false);
	ru = adjust_boundary(UPPER, i.upper(), i.info(), false,
			     floor_assign_r(i.lower(), lower(), ROUND_UP));
      }
    }
    i.info().set_interval_property(INTEGER, ONLY_INTEGERS_NORMALIZED);
    return combine(rl, ru);
  }

public:
  typedef Boundary boundary_type;

  typedef Interval_NS::Property Property;

  bool OK() const {
    if (is_not_a_number(lower())) {
#ifndef NDEBUG
      std::cerr << "The lower boundary is not a number." << std::endl;
#endif
      return false;
    }
    if (is_plus_infinity(lower())) {
      if (!Info::handle_infinity) {
	std::cerr << "The lower boundary is unexpectedly +inf." << std::endl;
	return false;
      }
      if (info().get_boundary_property(LOWER, OPEN)) {
	std::cerr << "The lower boundary is +inf open." << std::endl;
	return false;
      }
    }
    if (is_not_a_number(upper())) {
#ifndef NDEBUG
      std::cerr << "The upper boundary is not a number." << std::endl;
#endif
      return false;
    }
    if (is_minus_infinity(upper())) {
      if (!Info::handle_infinity) {
	std::cerr << "The upper boundary is unexpectedly -inf." << std::endl;
	return false;
      }
      if (info().get_boundary_property(UPPER, OPEN)) {
	std::cerr << "The upper boundary is +inf open." << std::endl;
	return false;
      }
    }
    if (info().get_interval_property(CARDINALITY_0) &&
	info().get_interval_property(CARDINALITY_IS) != is_empty_()) {
#ifndef NDEBUG
      std::cerr << "The interval empty flag is incongruent with actual content." << std::endl;
#endif
      return false;
    }
    if (info().get_interval_property(CARDINALITY_1) &&
	info().get_interval_property(CARDINALITY_IS) != is_singleton_()) {
#ifndef NDEBUG
      std::cerr << "The interval singleton flag is incongruent with actual content." << std::endl;
#endif
      return false;
    }
    if (info().get_interval_property(INTEGER) == ONLY_INTEGERS_NORMALIZED) {
      if (!is_integer(lower())) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to contain only integers and to be normalized, but lower boundary is not an integer." << std::endl;
#endif
	return false;
      }
      if (!is_integer(upper())) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to contain only integers and to be normalized, but upper boundary is not an integer." << std::endl;
#endif
	return false;
      }
    }
    else if (info().get_interval_property(INTEGER) == NOT_SINGLETON_INTEGER &&
	     is_singleton_integer()) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to not be a singleton integer, but actual content is a singleton integer." << std::endl;
#endif
      return false;
    }
    if (info().get_interval_property(CARDINALITY_IS) &&
	info().get_interval_property(CARDINALITY_0) == info().get_interval_property(CARDINALITY_1)) {
#ifndef NDEBUG
      std::cerr << "The interval is marked to know its cardinality, but this is unspecified or ambiguous." << std::endl;
#endif
      return false;
    }

    // Everything OK.
    return true;
  }

  //! Swaps \p *this with \p y.
  void swap(Interval& y);

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
    if (info().get_interval_property(CARDINALITY_IS))
      return info().get_interval_property(CARDINALITY_0);
    else if (info().get_interval_property(CARDINALITY_0))
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
    if (info().get_interval_property(CARDINALITY_IS))
      return info().get_interval_property(CARDINALITY_1);
    else if (info().get_interval_property(CARDINALITY_1))
      return false;
    if (is_singleton_()) {
      w_info().set_interval_property(CARDINALITY_IS);
      w_info().set_interval_property(CARDINALITY_0, false);
      w_info().set_interval_property(CARDINALITY_1);
      return true;
    }
    else {
      w_info().set_interval_property(CARDINALITY_1);
      if (info().get_interval_property(INTEGER) == MAYBE_SINGLETON_INTEGER)
	w_info().set_interval_property(INTEGER, NOT_SINGLETON_INTEGER);
      return false;
    }
  }
  I_Result normalize_integer() const {
    if (info().get_interval_property(INTEGER) != ONLY_INTEGERS_TO_NORMALIZE)
      return combine(V_EQ, V_EQ);
    else
      return normalize_integer_();
  }
  Integer_Property::Value integer_property() const {
    Integer_Property::Value v;
    v = info().get_interval_property(INTEGER);
    if (v == MAYBE_SINGLETON_INTEGER) {
      if (is_singleton_integer())
	v = ONLY_INTEGERS_NORMALIZED;
      else
	v = NOT_SINGLETON_INTEGER;
      w_info().set_interval_property(INTEGER, v);
    }
    return v;
  }
  bool contains_only_integers() const {
    return integer_property() >= ONLY_INTEGERS_TO_NORMALIZE;
  }
  bool lower_is_open() const {
    return info().get_boundary_property(LOWER, OPEN);
  }
  bool upper_is_open() const {
    return info().get_boundary_property(UPPER, OPEN);
  }
  Result lower_set_open() {
    return set_open(LOWER, lower(), info());
  }
  Result upper_set_open() {
    return set_open(UPPER, upper(), info());
  }
  bool lower_is_unbounded() const {
    return Boundary_NS::is_unbounded(LOWER, lower(), info());
  }
  bool upper_is_unbounded() const {
    return Boundary_NS::is_unbounded(UPPER, upper(), info());
  }
  bool is_unbounded() const {
    return lower_is_unbounded() || upper_is_unbounded();
  }
  Result lower_set_unbounded() {
    info().set_interval_property(CARDINALITY_IS, false);
    info().set_interval_property(CARDINALITY_0, true);
    info().set_interval_property(CARDINALITY_1, false);
    return set_unbounded(LOWER, lower(), info());
  }
  Result upper_set_unbounded() {
    info().set_interval_property(CARDINALITY_IS, false);
    info().set_interval_property(CARDINALITY_0, true);
    info().set_interval_property(CARDINALITY_1, false);
    return set_unbounded(UPPER, upper(), info());
  }
  bool is_universe() const {
    return lower_is_unbounded() && upper_is_unbounded()
      && !contains_only_integers();
  }
  I_Result set_universe() {
    info().clear();
    info().set_interval_property(CARDINALITY_0, true);
    Result rl = set_unbounded(LOWER, lower(), info());
    Result ru = set_unbounded(UPPER, upper(), info());
    return combine(rl, ru);
  }
  bool is_topologically_closed() const {
    // FIXME: review
    return is_empty() ||
      ((lower_is_unbounded() || !lower_is_open())
       && (upper_is_unbounded() || !upper_is_open()));
  }
  bool contains_integer_point() const {
    if (is_empty())
      return false;
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
inline I_Result
normalize_integer(const Interval<Boundary, Info>& x) {
  return x.normalize_integer();
}
template <typename Boundary, typename Info>
inline bool
contains_only_integers(const Interval<Boundary, Info>& x) {
  return x.contains_only_integers();
}
template <typename Boundary, typename Info>
inline Integer_Property::Value
integer_property(const Interval<Boundary, Info>& x) {
  return x.integer_property();
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

namespace Interval_NS {

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

struct Scalar_As_Interval_Policy {
  const_bool_nodef(handle_infinity, false);
  const_bool_nodef(check_inexact, false);
  const_bool_nodef(check_empty_args, false);
  const_bool_nodef(check_integer_args, true);
  const_bool_nodef(infinity_is_open, false);
};

typedef Interval_Info_Null<Scalar_As_Interval_Policy> Scalar_As_Interval_Info;

static const Scalar_As_Interval_Info& SCALAR_INFO = *static_cast<Scalar_As_Interval_Info*>(0);

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
  return SCALAR_INFO;
}
template <typename T>
inline I_Result
normalize_integer(const T&) {
  return combine(V_EQ, V_EQ);
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
inline Integer_Property::Value
integer_property(const T& x) {
  return is_integer(x) ? ONLY_INTEGERS_NORMALIZED : NOT_SINGLETON_INTEGER;
}

template <typename T>
struct Info {
  typedef Scalar_As_Interval_Info type;
};

template <typename Boundary, typename I>
struct Info<Interval<Boundary, I> > {
  typedef I type;
};

template <typename T>
inline bool
maybe_check_empty(const T& x) {
  if (Info<T>::type::check_empty_args)
    return is_empty(x);
  else {
    assert(!is_empty(x));
    return false;
  }
}

template <typename T>
inline bool
is_singleton(const T& x) {
  return !maybe_check_empty(x);
}


template <typename T>
inline Integer_Property::Value
lazy_integer_property(const T& x) {
  // TOTHINK: use the policy for x or for the result?
  if (Info<T>::type::check_integer_args)
    return integer_property(x);
  else
    return info(x).get_interval_property(INTEGER);
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
  Result rl = assign(LOWER, to.lower(), to_info, LOWER, l, info(l));
  Result ru = assign(UPPER, to.upper(), to_info, UPPER, u, info(u));
  if (integer)
    to_info.set_interval_property(INTEGER,ONLY_INTEGERS_TO_NORMALIZE);
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  Result rl = assign(LOWER, to.lower(), to_info,
		     LOWER, lower(x), info(x));
  Result ru = assign(UPPER, to.upper(), to_info,
		     UPPER, upper(x), info(x));
  if (To_Info::store_integer)
    to_info.set_interval_property(INTEGER, lazy_integer_property(x));
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  // FIXME: needed only for not only/only integers mixing.
  to.normalize_integer();
  normalize_integer(x);
  if (maybe_check_empty(to))
    return assign(to, x);
  if (maybe_check_empty(x))
    return combine(V_EQ, V_EQ);
  if (To_Info::store_integer) {
    Integer_Property::Value to_i = to.integer_property();
    if (to_i >= ONLY_INTEGERS_TO_NORMALIZE) {
      Integer_Property::Value x_i = integer_property(x);
      if (x_i < to_i)
	to.info().set_interval_property(INTEGER, x_i);
    }
  }
  if (to.info().get_interval_property(CARDINALITY_IS)) {
    to.info().set_interval_property(CARDINALITY_IS, false);
    to.info().set_interval_property(CARDINALITY_0, true);
    to.info().set_interval_property(CARDINALITY_1, false);
  }
  Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = max_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  // FIXME: needed only for not only/only integers mixing.
  normalize_integer(x);
  normalize_integer(y);
  if (maybe_check_empty(x))
    return assign(to, y);
  if (maybe_check_empty(y))
    return assign(to, x);
  To_Info to_info;
  if (To_Info::store_integer) {
    Integer_Property::Value x_i = integer_property(x);
    if (x_i >= ONLY_INTEGERS_TO_NORMALIZE) {
      Integer_Property::Value y_i = integer_property(y);
      if (x_i < y_i)
	x_i = y_i;
    }
    to_info.set_interval_property(INTEGER, x_i);
  }
  Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = max_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (To_Info::store_integer) {
    Integer_Property::Value to_i = lazy_integer_property(to);
    if (to_i == ONLY_INTEGERS_NORMALIZED) {
      if (lazy_integer_property(x) != ONLY_INTEGERS_NORMALIZED)
	to.info().set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
    }
    else if (to_i != ONLY_INTEGERS_TO_NORMALIZE) {
      if (lazy_integer_property(x) >= ONLY_INTEGERS_TO_NORMALIZE)
	to.info().set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
      else
	to.info().set_interval_property(INTEGER, MAYBE_SINGLETON_INTEGER);
    }
  }
  to.info().set_interval_property(CARDINALITY_IS, false);
  to.info().set_interval_property(CARDINALITY_0, false);
  to.info().set_interval_property(CARDINALITY_1, false);
  Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  // FIXME: boundary normalization for integer interval?
  // FIXME: check empty (policy based)?
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  To_Info to_info;
  if (To_Info::store_integer) {
    Integer_Property::Value x_i = lazy_integer_property(x);
    Integer_Property::Value y_i = lazy_integer_property(y);
    if (x_i == ONLY_INTEGERS_NORMALIZED && y_i == ONLY_INTEGERS_NORMALIZED)
      to.info().set_interval_property(INTEGER, ONLY_INTEGERS_NORMALIZED);
    else if (x_i >= ONLY_INTEGERS_TO_NORMALIZE || y_i >= ONLY_INTEGERS_TO_NORMALIZE)
      to.info().set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
    else
      to_info().set_interval_property(INTEGER, MAYBE_SINGLETON_INTEGER);
  }
  Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = min_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  // FIXME: boundary normalization for integer interval?
  // FIXME: check empty (policy based)?
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
refine(Interval<To_Boundary, To_Info>& to, Relation_Symbol rel, const From& x) {
  normalize_integer(x);
  if (maybe_check_empty(x))
    return to.set_empty();
  switch (rel) {
  case LESS_THAN:
    {
      if (lt(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x)))
	return combine(V_EQ, V_EQ);
      Result ru;
      to.info().clear_boundary_properties(UPPER);
      ru = assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
      ru = to.upper_set_open();
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return combine(V_EQ, ru);
    }
  case LESS_THAN_OR_EQUAL:
    {
      Result ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return combine(V_EQ, ru);
    }
  case GREATER_THAN:
    {
      if (gt(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x)))
	return combine(V_EQ, V_EQ);
      Result rl;
      to.info().clear_boundary_properties(LOWER);
      rl = assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
      rl = to.lower_set_open();
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return combine(rl, V_EQ);
    }
  case GREATER_THAN_OR_EQUAL:
    {
      Result rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return combine(rl, V_EQ);
    }
  case EQUAL:
    return intersect_assign(to, x);
  case NOT_EQUAL:
    {
      if (!is_singleton(x))
	return combine(V_EQ, V_EQ);
      if (!to.lower_is_open() && to.lower() == lower(x))
	to.lower_set_open();
      if (!to.upper_is_open() && to.upper() == lower(x))
	to.upper_set_open();
      // FIXME: boundary normalization for integer interval?
      // FIXME: check empty (policy based)?
      return combine(V_EQ, V_EQ);
    }
  default:
    assert(false);
    return I_EMPTY;
  }
}

template <typename T1, typename T2>
inline bool
i_eq(const T1& x, const T2& y) {
  if (is_empty(x))
    return is_empty(y);
  else if (is_empty(y))
    return false;
  return contains_only_integers(x) == contains_only_integers(y)
    && eq(LOWER, lower(x), info(x), LOWER, lower(y), info(y))
    && eq(UPPER, upper(x), info(x), UPPER, upper(y), info(y));
}

template <typename Boundary1, typename Info1,
	  typename Boundary2, typename Info2>
inline bool
operator==(const Interval<Boundary1, Info1>& x,
	   const Interval<Boundary2, Info2>& y) {
  return i_eq(x, y);
}
template <typename T,
	  typename Boundary, typename Info>
inline bool
operator==(const T& x, const Interval<Boundary, Info>& y) {
  return i_eq(x, y);
}
template <typename Boundary, typename Info,
	  typename T>
inline bool
operator==(const Interval<Boundary, Info>& x, const T& y) {
  return i_eq(x, y);
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
  normalize_integer(x);
  normalize_integer(y);
  if (is_empty(y))
    return true;
  if (x.is_empty())
    return false;
  if (x.contains_only_integers()) {
    if (!contains_only_integers(y))
      return false;
  }
  return le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
    && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y));
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
strictly_contains(const Interval<Boundary, Info>& x, const T& y) {
  normalize_integer(x);
  normalize_integer(y);
  if (is_empty(y))
    return !x.is_empty();
  if (x.is_empty())
    return false;
  if (x.contains_only_integers()) {
    if (!contains_only_integers(y))
      return false;
  }
  else if (contains_only_integer(y))
    return le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
      && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y));
  return (lt(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
	  && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y)))
    || (le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
	&& gt(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y)));
}

template <typename To_Boundary, typename To_Info,
	  typename T>
inline I_Result
neg_assign(Interval<To_Boundary, To_Info>& to, const T& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  Result rl, ru;
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
  if (To_Info::store_integer)
    to_info.set_interval_property(INTEGER, lazy_integer_property(x));
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
add_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  normalize_integer(x);
  normalize_integer(y);
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  Result rl = add_assign(LOWER, to.lower(), to_info,
			 LOWER, lower(x), info(x),
			 LOWER, lower(y), info(y));
  Result ru = add_assign(UPPER, to.upper(), to_info,
			 UPPER, upper(x), info(x),
			 UPPER, upper(y), info(y));
  if (To_Info::store_integer
      && lazy_integer_property(x) >= ONLY_INTEGERS_TO_NORMALIZE
      && lazy_integer_property(y) >= ONLY_INTEGERS_TO_NORMALIZE)
    to_info.set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
sub_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  normalize_integer(x);
  normalize_integer(y);
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  Result rl, ru;
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
  if (To_Info::store_integer
      && lazy_integer_property(x) >= ONLY_INTEGERS_TO_NORMALIZE
      && lazy_integer_property(y) >= ONLY_INTEGERS_TO_NORMALIZE)
    to_info.set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
  to.info() = to_info;
  return combine(rl, ru);
}

static const signed char C_ZERO = 0;

/**
+---+-----------+-----------------+-----------+
| * |    -y-    |       -y+       |    +y+    |
+---+-----------+-----------------+-----------+
|-x-|xu*yu,xl*yl|   xl*yu,xl*yl   |xl*yu,xu*yl|
+---+-----------+-----------------+-----------+
|-x+|xu*yl,xl*yl|min(xl*yu,xu*yl),|xl*yu,xu*yu|
|   |           |max(xl*yl,xu*yu) |           |
+---+-----------+-----------------+-----------+
|+x+|xu*yl,xl*yu|   xu*yl,xu*yu   |xl*yl,xu*yu|
+---+-----------+-----------------+-----------+
**/
template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
mul_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  normalize_integer(x);
  normalize_integer(y);
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  Result rl, ru;
  static To_Boundary to_lower;
  if (ge(LOWER, lower(x), info(x), LOWER, C_ZERO, SCALAR_INFO)) {
    if (ge(LOWER, lower(y), info(y), LOWER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
    }
    else if (le(UPPER, upper(y), info(y), UPPER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
    }
    else {
      rl = mul_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
    }
  }
  else if (le(UPPER, upper(x), info(x), UPPER, C_ZERO, SCALAR_INFO)) {
    if (ge(LOWER, lower(y), info(y), LOWER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
    }
    else if (le(UPPER, upper(y), info(y), UPPER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
    }
    else {
      rl = mul_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
    }
  }
  else {
    if (ge(LOWER, lower(y), info(y), LOWER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
    }
    else if (le(UPPER, upper(y), info(y), UPPER, C_ZERO, SCALAR_INFO)) {
      rl = mul_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
      }
    else {
      static To_Boundary tmp;
      To_Info tmp_info;
      Result tmp_r;
      tmp_r = mul_assign(LOWER, tmp, tmp_info,
			 UPPER, upper(x), info(x),
			 LOWER, lower(y), info(y));
      rl = mul_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
      if (gt(LOWER, to_lower, to_info, LOWER, tmp, tmp_info)) {
	to_lower = tmp;
	rl = tmp_r;
      }
      tmp_info.clear();
      tmp_r = mul_assign(UPPER, tmp, tmp_info,
			 UPPER, upper(x), info(x),
			 UPPER, upper(y), info(y));
      ru = mul_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
      if (lt(UPPER, to.upper(), to_info, UPPER, tmp, tmp_info)) {
	to.upper() = tmp;
	ru = tmp_r;
      }
    }
  }
  assign_or_swap(to.lower(), to_lower);
  if (To_Info::store_integer
      && lazy_integer_property(x) >= ONLY_INTEGERS_TO_NORMALIZE
      && lazy_integer_property(y) >= ONLY_INTEGERS_TO_NORMALIZE)
    to_info.set_interval_property(INTEGER, ONLY_INTEGERS_TO_NORMALIZE);
  to.info() = to_info;
  return combine(rl, ru);
}

/**
+---+-----------+-----------+
| / |    -y-    |    +y+    |
+---+-----------+-----------+
|-x-|xu/yl,xl/yu|xl/yl,xu/yu|
+---+-----------+-----------+
|-x+|xu/yu,xl/yu|xl/yl,xu/yl|
+---+-----------+-----------+
|+x+|xu/yu,xl/yl|xl/yu,xu/yl|
+---+-----------+-----------+
**/
template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
div_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  normalize_integer(x);
  normalize_integer(y);
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  Result rl, ru;
  static To_Boundary to_lower;
  if (ge(LOWER, lower(y), info(y), LOWER, C_ZERO, SCALAR_INFO)) {
    if (ge(LOWER, lower(x), info(x), LOWER, C_ZERO, SCALAR_INFO)) {
      rl = div_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
    }
    else if (le(UPPER, upper(x), info(x), UPPER, C_ZERO, SCALAR_INFO)) {
      rl = div_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
    }
    else {
      rl = div_assign(LOWER, to_lower, to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
    }
  }
  else if (le(UPPER, upper(y), info(y), UPPER, C_ZERO, SCALAR_INFO)) {
    if (ge(LOWER, lower(x), info(x), LOWER, C_ZERO, SCALAR_INFO)) {
      rl = div_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      LOWER, lower(y), info(y));
    }
    else if (le(UPPER, upper(x), info(x), UPPER, C_ZERO, SCALAR_INFO)) {
      rl = div_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      LOWER, lower(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
    }
    else {
      rl = div_assign(LOWER, to_lower, to_info,
		      UPPER, upper(x), info(x),
		      UPPER, upper(y), info(y));
      ru = div_assign(UPPER, to.upper(), to_info,
		      LOWER, lower(x), info(x),
		      UPPER, upper(y), info(y));
    }
  }
  else {
#if 0
    if (!contains_only_integers(y))
      return to.set_unbounded();
#endif
  }
  assign_or_swap(to.lower(), to_lower);
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename Boundary, typename Info>
inline std::ostream&
operator<<(std::ostream& os, const Interval<Boundary, Info>& x) {
  if (x.is_empty())
    return os << "[]";
  if (x.is_singleton())
    return os << x.lower();
  os << (x.lower_is_open() ? "(" : "[");
  if (x.info().get_boundary_property(LOWER, SPECIAL))
    os << "-inf";
  else
    os << x.lower();
  os << (x.contains_only_integers() ? " .. " : ", ");
  if (x.info().get_boundary_property(UPPER, SPECIAL))
    os << "+inf";
  else
    os << x.upper();
  os << (x.upper_is_open() ? ")" : "]");
  return os;
}

}

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
