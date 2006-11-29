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

enum Ternary { T_YES, T_NO, T_MAYBE };

inline I_Result
combine(Result l, Result u) {
  return static_cast<I_Result>(l | (u << 6));
}

using namespace Boundary_NS;
using namespace Interval_NS;

struct Interval_ {
};

template <typename Boundary, typename Info>
class Interval : public Interval_, private Info {
private:
  COMPILE_TIME_CHECK(!Info::store_special || !std::numeric_limits<Boundary>::has_infinity, "store_special is senseless when boundary type may contains infinity");
  COMPILE_TIME_CHECK(!Info::infinity_is_open || !Info::store_open, "infinity_is_open is senseless when boundary may be marked as open");
  Info& w_info() const {
    return const_cast<Interval&>(*this);
  }
  bool is_empty_nocache() const {
    return lt(UPPER, upper(), info(), LOWER, lower(), info());
  }
  bool is_singleton_nocache() const {
    return eq(LOWER, lower(), info(), UPPER, upper(), info());
  }
  Result normalize_lower() const {
    Result r;
    if (info().get_boundary_property(LOWER, NORMALIZED)
	|| info().get_boundary_property(LOWER, SPECIAL))
      r = V_EQ;
    else {
      if (info().get_boundary_property(LOWER, OPEN)) {
	r = info().restrict(lower(), V_GT);
	if (r != V_GT)
	  w_info().set_boundary_property(LOWER, OPEN, false);
      }
      else {
	r = info().restrict(lower(), V_GE);
	if (r == V_GT)
	  w_info().set_boundary_property(LOWER, OPEN);
      }
      w_info().set_boundary_property(LOWER, NORMALIZED);
    }
    return r;
  }
  Result normalize_upper() const {
    Result r;
    if (info().get_boundary_property(UPPER, NORMALIZED)
	|| info().get_boundary_property(UPPER, SPECIAL))
      r = V_EQ;
    else {
      if (info().get_boundary_property(UPPER, OPEN)) {
	r = info().restrict(upper(), V_LT);
	if (r != V_LT)
	  w_info().set_boundary_property(UPPER, OPEN, false);
      }
      else {
	r = info().restrict(upper(), V_LE);
	if (r == V_LT)
	  w_info().set_boundary_property(UPPER, OPEN);
      }
      w_info().set_boundary_property(UPPER, NORMALIZED);
    }
    return r;
  }


public:
  typedef Boundary boundary_type;
  typedef Info info_type;

  typedef Interval_NS::Property Property;

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
  Ternary is_empty_cached() const {
    if (info().get_interval_property(CARDINALITY_0))
      return info().get_interval_property(CARDINALITY_IS) ? T_YES : T_NO;
    else if (info().get_interval_property(CARDINALITY_IS))
      return T_NO;
    else
      return T_MAYBE;
  }
  Ternary is_singleton_cached() const {
    if (info().get_interval_property(CARDINALITY_1))
      return info().get_interval_property(CARDINALITY_IS) ? T_YES : T_NO;
    else if (info().get_interval_property(CARDINALITY_IS))
      return T_NO;
    else
      return T_MAYBE;
  }
  I_Result set_empty() {
    assign_r(lower(), 1, ROUND_NOT_NEEDED);
    assign_r(upper(), 0, ROUND_NOT_NEEDED);
    info().clear();
    info().set_interval_property(CARDINALITY_IS);
    info().set_interval_property(CARDINALITY_0);
    return I_EMPTY;
  }
  bool is_empty() const {
    switch (is_empty_cached()) {
    case T_NO:
      return false;
    case T_YES:
      return true;
    case T_MAYBE:
      bool r = is_empty_nocache();
      if (r) {
	w_info().set_interval_property(CARDINALITY_IS, r);
	w_info().set_interval_property(CARDINALITY_1, false);
      }
      w_info().set_interval_property(CARDINALITY_0);
      return r;
    }
    assert(false);
    return false;
  }
  bool is_singleton() const {
    switch (is_singleton_cached()) {
    case T_NO:
      return false;
    case T_YES:
      return true;
    case T_MAYBE:
      bool r = is_singleton_nocache();
      if (r) {
	w_info().set_interval_property(CARDINALITY_IS, r);
	w_info().set_interval_property(CARDINALITY_0, false);
      }
      w_info().set_interval_property(CARDINALITY_1);
      return r;
    }
    assert(false);
    return false;
  }
  I_Result normalize() const {
    Result rl = normalize_lower();
    Result ru = normalize_upper();
    info().normalize();
    return combine(rl, ru);
  }
  bool has_restriction() const {
    return info().has_restriction();
  }
  bool lower_is_open() const {
    return is_open(LOWER, lower(), info());
  }
  bool upper_is_open() const {
    return is_open(UPPER, upper(), info());
  }
  Result lower_shrink() {
    return shrink(LOWER, lower(), info());
  }
  Result upper_shrink() {
    return shrink(UPPER, upper(), info());
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
  bool is_universe() const {
    return lower_is_unbounded() && upper_is_unbounded()
      && !has_restriction();
  }
  I_Result set_universe() {
    info().clear();
    info().set_interval_property(CARDINALITY_0, true);
    info().set_interval_property(CARDINALITY_1, true);
    Result rl = set_unbounded(LOWER, lower(), info());
    Result ru = set_unbounded(UPPER, upper(), info());
    return combine(rl, ru);
  }
  I_Result lower_set_unbounded() {
    info().set_interval_property(CARDINALITY_IS, false);
    info().set_interval_property(CARDINALITY_0, false);
    info().set_interval_property(CARDINALITY_1, false);
    info().clear_boundary_properties(LOWER);
    Result rl = set_unbounded(LOWER, lower(), info());
    return combine(rl, V_EQ);
  }
  I_Result upper_set_unbounded() {
    info().set_interval_property(CARDINALITY_IS, false);
    info().set_interval_property(CARDINALITY_0, false);
    info().set_interval_property(CARDINALITY_1, false);
    info().clear_boundary_properties(UPPER);
    Result ru = set_unbounded(UPPER, upper(), info());
    return combine(V_EQ, ru);
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

  bool OK() const {
    if (!Info::may_be_empty && is_empty()) {
#ifndef NDEBUG
	std::cerr << "The interval is unexpectedly empty." << std::endl;
#endif
	return false;
    }

    if (is_open(LOWER, lower(), info())) {
      if (is_plus_infinity(LOWER, lower(), info())) {
#ifndef NDEBUG
	std::cerr << "The lower boundary is +inf open." << std::endl;
#endif
      }
    }
    else if (is_minus_infinity(LOWER, lower(), info())
	     || is_plus_infinity(LOWER, lower(), info())) {
#ifndef NDEBUG
      std::cerr << "The lower boundary is unexpectedly infinity." << std::endl;
#endif
      return false;
    }
    if (!info().get_boundary_property(LOWER, SPECIAL)) {
      if (is_not_a_number(lower())) {
#ifndef NDEBUG
	std::cerr << "The lower boundary is not a number." << std::endl;
#endif
	return false;
      }
#if 0
      if (info().get_boundary_property(LOWER, NORMALIZED)
	  && !info().is_restricted(lower())) {
#ifndef NDEBUG
	std::cerr << "The lower boundary is marked to be normalized, but actually it's not." << std::endl;
#endif
	return false;
      }
#endif
    }

    if (is_open(UPPER, upper(), info())) {
      if (is_minus_infinity(UPPER, upper(), info())) {
#ifndef NDEBUG
	std::cerr << "The upper boundary is -inf open." << std::endl;
#endif
      }
    }
    else if (is_minus_infinity(UPPER, upper(), info())
	     || is_plus_infinity(UPPER, upper(), info())) {
#ifndef NDEBUG
      std::cerr << "The upper boundary is unexpectedly infinity." << std::endl;
#endif
      return false;
    }
    if (!info().get_boundary_property(UPPER, SPECIAL)) {
      if (is_not_a_number(upper())) {
#ifndef NDEBUG
	std::cerr << "The upper boundary is not a number." << std::endl;
#endif
	return false;
      }
#if 0
      if (info().get_boundary_property(UPPER, NORMALIZED)
	  && !info().is_restricted(upper())) {
#ifndef NDEBUG
	std::cerr << "The upper boundary is marked to be normalized, but actually it's not." << std::endl;
#endif
	return false;
      }
#endif
    }

    Ternary t;

    t = is_empty_cached();
    if (t == T_YES) {
      if (!is_empty_nocache()) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to be empty but actually it is not empty." << std::endl;
#endif
	return false;
      }
    }
    else if (t == T_NO) {
      if (is_empty_nocache()) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to be not empty but actually it is empty." << std::endl;
#endif
	return false;
      }
    }

    t = is_singleton_cached();
    if (t == T_YES) {
      if (!is_singleton_nocache()) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to be singleton but actually it is not singleton." << std::endl;
#endif
	return false;
      }
    }
    else if (t == T_NO) {
      if (is_singleton_nocache()) {
#ifndef NDEBUG
	std::cerr << "The interval is marked to be not singleton but actually it is singleton." << std::endl;
#endif
	return false;
      }
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

private:
  Boundary lower_;
  Boundary upper_;
};

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
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, true);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
  const_bool_nodef(infinity_is_open, false);
};

typedef Interval_Info_Null<Scalar_As_Interval_Policy> Scalar_As_Interval_Info;

static const Scalar_As_Interval_Info& SCALAR_INFO = *static_cast<Scalar_As_Interval_Info*>(0);

template <typename T>
inline typename Enable_If<!Is_Same_Or_Derived<Interval_, T>::value, const T&>::type
lower(const T& x) {
  return x;
}
template <typename T>
inline typename Enable_If<!Is_Same_Or_Derived<Interval_, T>::value, const T&>::type
upper(const T& x) {
  return x;
}
template <typename T>
inline typename Enable_If<!Is_Same_Or_Derived<Interval_, T>::value, const Scalar_As_Interval_Info&>::type
info(const T&) {
  return SCALAR_INFO;
}
template <typename T>
inline typename Enable_If<!Is_Same_Or_Derived<Interval_, T>::value, bool>::type
is_empty(const T& x) {
  return is_not_a_number(x);
}

template <typename T>
inline typename Enable_If<!Is_Same_Or_Derived<Interval_, T>::value, bool>::type
is_singleton(const T& x) {
  return !is_empty(x);
}

template <typename T>
inline Ternary
is_empty_lazy(const T& x) {
  if (info(x).get_interval_property(CARDINALITY_0))
    return info(x).get_interval_property(CARDINALITY_IS) ? T_YES : T_NO;
  else
    return T_MAYBE;
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

template <typename T>
inline bool
is_singleton_integer(const T& x) {
  return is_singleton(x) && is_integer(lower(x));
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

template <typename T>
inline void
assign_or_swap(T& to, const T& from) {
  to = from;
}
template <typename T>
inline void
assign_or_swap(mpz_class& to, mpz_class& from) {
  std::swap(to, from);
}
template <typename T>
inline void
assign_or_swap(mpq_class& to, mpq_class& from) {
  std::swap(to, from);
}

template <typename T>
inline bool
check_empty_arg(const T& x) {
  if (info(x).may_be_empty)
    return is_empty(x);
  else {
    assert(!is_empty(x));
    return false;
  }
}

template <typename Boundary, typename Info>
inline I_Result
check_empty_result(const Interval<Boundary, Info>& x, I_Result r) {
  if (Info::check_empty_result && is_empty(x))
    return I_EMPTY;
  else
    return r;
}

template <typename T1, typename T2>
inline typename Enable_If<Is_Same_Or_Derived<Interval_, T1>::value || Is_Same_Or_Derived<Interval_, T2>::value, bool>::type
operator==(const T1& x, const T2& y) {
  if (check_empty_arg(x))
    return check_empty_arg(y);
  else if (check_empty_arg(y))
    return false;
  return eq_restriction(x, y)
    && eq(LOWER, lower(x), info(x), LOWER, lower(y), info(y))
    && eq(UPPER, upper(x), info(x), UPPER, upper(y), info(y));
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
contains(const Interval<Boundary, Info>& x, const T& y) {
  if (check_empty_arg(y))
    return true;
  if (check_empty_arg(x))
    return false;
  if (!contains_restriction(x, y))
      return false;
  return le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
    && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y));
}

template <typename Boundary, typename Info,
	  typename T>
inline bool
strictly_contains(const Interval<Boundary, Info>& x, const T& y) {
  if (check_empty_arg(y))
    return !check_empty_arg(x);
  if (check_empty_arg(x))
    return false;
  if (!contains_restriction(x, y))
      return false;
  else if (!eq_restriction(x, y))
    return le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
      && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y));
  return (lt(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
	  && ge(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y)))
    || (le(LOWER, x.lower(), x.info(), LOWER, lower(y), info(y))
	&& gt(UPPER, x.upper(), x.info(), UPPER, upper(y), info(y)));
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From1& l, const From2& u) {
  to.info().clear();
  Result rl = assign(LOWER, to.lower(), to.info(), LOWER, l, info(l));
  Result ru = assign(UPPER, to.upper(), to.info(), UPPER, u, info(u));
  return check_empty_result(to, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (check_empty_arg(x))
    return to.set_empty();
  To_Info to_info;
  assign_restriction(to_info, x);
  Result rl = assign(LOWER, to.lower(), to_info,
		     LOWER, lower(x), info(x));
  Result ru = assign(UPPER, to.upper(), to_info,
		     UPPER, upper(x), info(x));
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
join_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (check_empty_arg(to))
    return assign(to, x);
  if (check_empty_arg(x))
    return combine(V_EQ, V_EQ);
  join_restriction(to.info(), to, x);
  to.info().set_interval_property(CARDINALITY_IS, false);
  to.info().set_interval_property(CARDINALITY_0);
  to.info().set_interval_property(CARDINALITY_1, false);
  Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = max_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
join_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (check_empty_arg(x))
    return assign(to, y);
  if (check_empty_arg(y))
    return assign(to, x);
  To_Info to_info;
  join_restriction(to_info, x, y);
  to_info.set_interval_property(CARDINALITY_0);
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
  intersect_restriction(to.info(), to, x);
  // FIXME: more accurate?
  to.info().set_interval_property(CARDINALITY_IS, false);
  to.info().set_interval_property(CARDINALITY_0, false);
  to.info().set_interval_property(CARDINALITY_1, false);
  Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  return check_empty_result(to, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  To_Info to_info;
  intersect_restriction(to_info, x, y);
  // FIXME: more accurate?
  to_info().set_interval_property(CARDINALITY_IS, false);
  to_info().set_interval_property(CARDINALITY_0, false);
  to_info().set_interval_property(CARDINALITY_1, false);
  Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = min_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  to.info() = to_info;
  return check_empty_result(to, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
refine(Interval<To_Boundary, To_Info>& to, Relation_Symbol rel, const From& x) {
  if (check_empty_arg(x))
    return to.set_empty();
  switch (rel) {
  case LESS_THAN:
    {
      if (lt(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x)))
	return combine(V_EQ, V_EQ);
      to.info().clear_boundary_properties(UPPER);
      Result ru = assign(UPPER, to.upper(), to.info(),
			 UPPER, upper(x), info(x), true);
      to.normalize();
      return check_empty_result(to, combine(V_EQ, ru));
    }
  case LESS_THAN_OR_EQUAL:
    {
      if (le(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x)))
	return combine(V_EQ, V_EQ);
      to.info().clear_boundary_properties(UPPER);
      Result ru = assign(UPPER, to.upper(), to.info(),
			 UPPER, upper(x), info(x));
      to.normalize();
      return check_empty_result(to, combine(V_EQ, ru));
    }
  case GREATER_THAN:
    {
      if (gt(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x)))
	return combine(V_EQ, V_EQ);
      to.info().clear_boundary_properties(LOWER);
      Result rl = assign(LOWER, to.lower(), to.info(),
			 LOWER, lower(x), info(x), true);
      to.normalize();
      return check_empty_result(to, combine(rl, V_EQ));
    }
  case GREATER_THAN_OR_EQUAL:
    {
      if (ge(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x)))
	return combine(V_EQ, V_EQ);
      to.info().clear_boundary_properties(LOWER);
      Result rl = assign(LOWER, to.lower(), to.info(),
			 LOWER, lower(x), info(x));
      to.normalize();
      return check_empty_result(to, combine(rl, V_EQ));
    }
  case EQUAL:
    return intersect_assign(to, x);
  case NOT_EQUAL:
    {
      if (check_empty_arg(to))
	return I_EMPTY;
      if (!is_singleton(x))
	return combine(V_EQ, V_EQ);
      if (eq(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x)))
	to.lower_shrink();
      if (eq(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x)))
	to.upper_shrink();
      to.normalize();
      return check_empty_result(to, combine(V_EQ, V_EQ));
    }
  default:
    assert(false);
    return I_EMPTY;
  }
}

template <typename To_Boundary, typename To_Info,
	  typename T>
inline I_Result
neg_assign(Interval<To_Boundary, To_Info>& to, const T& x) {
  if (check_empty_arg(x))
    return to.set_empty();
  To_Info to_info;
  neg_restriction(to_info, x);
  Result rl, ru;
  static To_Boundary to_lower;
  rl = neg_assign(LOWER, to_lower, to_info, UPPER, upper(x), info(x));
  ru = neg_assign(UPPER, to.upper(), to_info, LOWER, lower(x), info(x));
  assign_or_swap(to.lower(), to_lower);
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
add_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (check_empty_arg(x) || check_empty_arg(y))
    return to.set_empty();
  To_Info to_info;
  add_restriction(to_info, x, y);
  Result rl = add_assign(LOWER, to.lower(), to_info,
			 LOWER, lower(x), info(x),
			 LOWER, lower(y), info(y));
  Result ru = add_assign(UPPER, to.upper(), to_info,
			 UPPER, upper(x), info(x),
			 UPPER, upper(y), info(y));
  to.info() = to_info;
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
sub_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (check_empty_arg(x) || check_empty_arg(y))
    return to.set_empty();
  To_Info to_info;
  sub_restriction(to_info, x, y);
  Result rl, ru;
  static To_Boundary to_lower;
  rl = sub_assign(LOWER, to_lower, to_info,
		  LOWER, lower(x), info(x),
		  UPPER, upper(y), info(y));
  ru = sub_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  LOWER, lower(y), info(y));
  assign_or_swap(to.lower(), to_lower);
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
  if (check_empty_arg(x) || check_empty_arg(y))
    return to.set_empty();
  To_Info to_info;
  mul_restriction(to_info, x, y);
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
  if (check_empty_arg(x) || check_empty_arg(y))
    return to.set_empty();
  To_Info to_info;
  div_restriction(to_info, x, y);
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
  if (check_empty_arg(x))
    return os << "[]";
  if (x.is_singleton())
    return os << x.lower();
  os << (x.lower_is_open() ? "(" : "[");
  if (x.info().get_boundary_property(LOWER, SPECIAL))
    os << "-inf";
  else
    os << x.lower();
  os << ", ";
  if (x.info().get_boundary_property(UPPER, SPECIAL))
    os << "+inf";
  else
    os << x.upper();
  os << (x.upper_is_open() ? ")" : "]");
  output_restriction(os, x.info());
  return os;
}

}

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
