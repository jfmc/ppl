/* Declarations for the Interval class and its constituents.
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

#ifndef PPL_Interval_defs_hh
#define PPL_Interval_defs_hh 1

#include "globals.defs.hh"
#include "meta_programming.hh"
#include "Interval.types.hh"
#include "Interval_Info.defs.hh"
#include <iosfwd>

// Temporary!
#include <iostream>

namespace Parma_Polyhedra_Library {

enum I_Result {
  I_EMPTY = 0,
  /*
  I_L_EQ = V_EQ,
  I_L_GT = V_GT,
  I_L_GE = V_GE,
  I_U_EQ = V_EQ << 6,
  I_U_LT = V_LT << 6,
  I_U_LE = V_LE << 6,
  */
  I_MAYBE_EMPTY = 1 << 12,
  I_SINGULARITIES = 1 << 13
};

enum Ternary { T_YES, T_NO, T_MAYBE };

inline I_Result
combine(Result l, Result u) {
  return static_cast<I_Result>(l | (u << 6));
}

using namespace Boundary_NS;
using namespace Interval_NS;

struct Interval_Base {};

template <typename T, typename Enable = void>
struct Is_Singleton : public Is_Native_Or_Checked<T> {};

template <typename T>
struct Is_Interval : public Is_Same_Or_Derived<Interval_Base, T> {};

//! A generic, not necessarily closed, possibly restricted interval.
/*! \ingroup PPL_CXX_interface
  FIXME: to be written.
*/
template <typename Boundary, typename Info>
class Interval : public Interval_Base, private Info {
private:
  COMPILE_TIME_CHECK(!Info::store_special || !std::numeric_limits<Boundary>::has_infinity, "store_special is senseless when boundary type may contains infinity");
  Info& w_info() const {
    return const_cast<Interval&>(*this);
  }
  bool is_empty_nocache() const {
    return lt(UPPER, upper(), info(), LOWER, lower(), info());
  }
  bool is_singleton_nocache() const {
    return eq(LOWER, lower(), info(), UPPER, upper(), info());
  }
  Result lower_normalize() const {
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
  Result upper_normalize() const {
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

  void lower_load() {
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    lower_loaded = 1;
#endif
  }

  void upper_load() {
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    upper_loaded = 1;
#endif
  }

  void complete_init() {
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    assert(lower_loaded);
    assert(upper_loaded);
    completed = 1;
#endif
  }

  void complete_init_internal() {
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    lower_loaded = 1;
    upper_loaded = 1;
    completed = 1;
#endif
  }

  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Interval&>::type
  operator=(const T& x) {
    assign(x);
    return *this;
  }

  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Interval&>::type
  operator+=(const T& x) {
    add_assign(*this, x);
    return *this;
  }
  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Interval&>::type
  operator-=(const T& x) {
    sub_assign(*this, x);
    return *this;
  }
  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Interval&>::type
  operator*=(const T& x) {
    mul_assign(*this, x);
    return *this;
  }
  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Interval&>::type
  operator/=(const T& x) {
    div_assign(*this, x);
    return *this;
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
  bool has_restriction() const {
    return info().has_restriction();
  }
  I_Result normalize() const {
    assert(OK());
    if (has_restriction()) {
      Result rl = lower_normalize();
      Result ru = upper_normalize();
      // FIXME: this invalidation is not needed if interval is unchanged
      invalidate_cardinality_cache();
      info().normalize();
      assert(OK());
      return combine(rl, ru);
    }
    else
      return combine(V_EQ, V_EQ);
  }
  bool lower_is_open() const {
    assert(OK());
    return is_open(LOWER, lower(), info());
  }
  bool upper_is_open() const {
    assert(OK());
    return is_open(UPPER, upper(), info());
  }
  Result lower_shrink() {
    assert(OK());
    return shrink(LOWER, lower(), info());
  }
  Result upper_shrink() {
    assert(OK());
    return shrink(UPPER, upper(), info());
  }
  bool lower_is_unbounded() const {
    assert(OK());
    return Boundary_NS::is_unbounded(LOWER, lower(), info());
  }
  bool upper_is_unbounded() const {
    assert(OK());
    return Boundary_NS::is_unbounded(UPPER, upper(), info());
  }
  bool is_unbounded() const {
    assert(OK());
    return lower_is_unbounded() || upper_is_unbounded();
  }
  bool is_universe() const {
    assert(OK());
    return lower_is_unbounded() && upper_is_unbounded()
      && !has_restriction();
  }
  void invalidate_cardinality_cache() const {
    w_info().set_interval_property(CARDINALITY_IS, false);
    w_info().set_interval_property(CARDINALITY_0, false);
    w_info().set_interval_property(CARDINALITY_1, false);
  }
  template <typename T>
  I_Result lower_set_uninit(const T& x, bool open = false) {
    info().clear_boundary_properties(LOWER);
    Result rl = Boundary_NS::assign(LOWER, lower(), info(), LOWER, x, f_info(x, open));
    lower_load();
    return combine(rl, V_EQ);
  }
  I_Result lower_set_uninit(const Unbounded&) {
    info().clear_boundary_properties(LOWER);
    Result rl = set_unbounded(LOWER, lower(), info());
    lower_load();
    return combine(rl, V_EQ);
  }
  template <typename T>
  I_Result lower_set(const T& x, bool open = false) {
    assert(OK());
    info().clear_boundary_properties(LOWER);
    Result rl = Boundary_NS::assign(LOWER, lower(), info(), LOWER, x, f_info(x, open));
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  I_Result lower_set(const Unbounded&) {
    assert(OK());
    info().clear_boundary_properties(LOWER);
    Result rl = set_unbounded(LOWER, lower(), info());
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  template <typename T>
  I_Result lower_narrow(const T& x, bool open = false) {
    assert(OK());
    if (ge(LOWER, lower(), info(), LOWER, x, f_info(x, open)))
      return combine(V_EQ, V_EQ);
    return lower_set(x, open);
  }
  template <typename T>
  I_Result lower_widen(const T& x, bool open = false) {
    assert(OK());
    if (le(LOWER, lower(), info(), LOWER, x, f_info(x, open)))
      return combine(V_EQ, V_EQ);
    return lower_set(x, open);
  }
  I_Result lower_widen(const Unbounded&) {
    assert(OK());
    if (lower_is_unbounded())
      return combine(V_EQ, V_EQ);
    info().clear_boundary_properties(LOWER);
    Result rl = set_unbounded(LOWER, lower(), info());
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  template <typename T>
  I_Result upper_set_uninit(const T& x, bool open = false) {
    info().clear_boundary_properties(UPPER);
    Result rl = Boundary_NS::assign(UPPER, upper(), info(), UPPER, x, f_info(x, open));
    upper_load();
    return combine(rl, V_EQ);
  }
  I_Result upper_set_uninit(const Unbounded&) {
    info().clear_boundary_properties(UPPER);
    Result rl = set_unbounded(UPPER, upper(), info());
    upper_load();
    return combine(rl, V_EQ);
  }
  template <typename T>
  I_Result upper_set(const T& x, bool open = false) {
    assert(OK());
    info().clear_boundary_properties(UPPER);
    Result rl = Boundary_NS::assign(UPPER, upper(), info(), UPPER, x, f_info(x, open));
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  I_Result upper_set(const Unbounded&) {
    assert(OK());
    info().clear_boundary_properties(UPPER);
    Result rl = set_unbounded(UPPER, upper(), info());
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  template <typename T>
  I_Result upper_narrow(const T& x, bool open = false) {
    assert(OK());
    if (le(UPPER, upper(), info(), UPPER, x, f_info(x, open)))
      return combine(V_EQ, V_EQ);
    return upper_set(x, open);
  }
  template <typename T>
  I_Result upper_widen(const T& x, bool open = false) {
    assert(OK());
    if (ge(UPPER, upper(), info(), UPPER, x, f_info(x, open)))
      return combine(V_EQ, V_EQ);
    return upper_set(x, open);
  }
  I_Result upper_widen(const Unbounded&) {
    assert(OK());
    if (upper_is_unbounded())
      return combine(V_EQ, V_EQ);
    info().clear_boundary_properties(UPPER);
    Result rl = set_unbounded(UPPER, upper(), info());
    invalidate_cardinality_cache();
    assert(OK());
    return combine(rl, V_EQ);
  }
  I_Result assign(Degenerate_Element e) {
    info().clear();
    switch (e) {
    case EMPTY:
      info().set_interval_property(CARDINALITY_IS);
      info().set_interval_property(CARDINALITY_0);
      lower_set_uninit(1);
      upper_set_uninit(0);
      break;
    case UNIVERSE:
      info().set_interval_property(CARDINALITY_0, true);
      info().set_interval_property(CARDINALITY_1, true);
      lower_set_uninit(UNBOUNDED);
      upper_set_uninit(UNBOUNDED);
      break;
    default:
      assert(0);
      break;
    }
    complete_init();
    assert(OK());
    return I_EMPTY;
  }
  I_Result set_infinities() {
    info().clear();
    info().set_interval_property(CARDINALITY_0, true);
    info().set_interval_property(CARDINALITY_1, true);
    // FIXME: what about restrictions?
    Result rl = Boundary_NS::set_minus_infinity(LOWER, lower(), info());
    Result ru = Boundary_NS::set_plus_infinity(UPPER, upper(), info());
    complete_init_internal();
    assert(OK());
    return combine(rl, ru);
  }
  bool is_topologically_closed() const {
    assert(OK());
    // FIXME: review
    return is_empty() ||
      ((lower_is_unbounded() || !lower_is_open())
       && (upper_is_unbounded() || !upper_is_open()));
  }
  bool is_infinity() const {
    assert(OK());
    if (is_reverse_infinity(LOWER, lower(), info()))
      return 1;
    else if (is_reverse_infinity(UPPER, upper(), info()))
      return -1;
    else
      return 0;
  }
  bool contains_integer_point() const {
    assert(OK());
    if (is_empty())
      return false;
    if (is_unbounded())
      return true;
    Boundary l;
    if (lower_is_open()) {
      add_assign_r(l, lower(), Boundary(1), ROUND_DOWN);
      floor_assign_r(l, l, ROUND_DOWN);
    }
    else
      ceil_assign_r(l, lower(), ROUND_DOWN);
    Boundary u;
    if (upper_is_open()) {
      sub_assign_r(u, upper(), Boundary(1), ROUND_UP);
      ceil_assign_r(u, u, ROUND_UP);
    }
    else
      floor_assign_r(u, upper(), ROUND_UP);
    return u >= l;
  }

  void ascii_dump(std::ostream& s) const {
    s << *this << std::endl;
  }
  bool ascii_load(std::istream& s) {
    s >> *this;
    return s;
  }

  bool OK() const {
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    if (!completed) {
	std::cerr << "The interval initialization has not been completed." << std::endl;
	return false;
    }
#endif
#if 0
    if (!Info::may_be_empty && is_empty()) {
#ifndef NDEBUG
	std::cerr << "The interval is unexpectedly empty." << std::endl;
#endif
	return false;
    }
#endif

    if (is_open(LOWER, lower(), info())) {
      if (is_plus_infinity(LOWER, lower(), info())) {
#ifndef NDEBUG
	std::cerr << "The lower boundary is +inf open." << std::endl;
#endif
      }
    }
    else if (!Info::may_contain_infinity
	     && (is_minus_infinity(LOWER, lower(), info())
		 || is_plus_infinity(LOWER, lower(), info()))) {
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
    else if (!Info::may_contain_infinity
	     && (is_minus_infinity(UPPER, upper(), info())
		 || is_plus_infinity(UPPER, upper(), info()))) {
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

  Interval()
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    : lower_loaded(0), upper_loaded(0), completed(0)
#endif
  {
  }

  template <typename T>
  explicit Interval(const T& x)
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
    : lower_loaded(0), upper_loaded(0), completed(0)
#endif
  {
    assign(x);
  }

  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
  contains(const T& y) const;

  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
  strictly_contains(const T& y) const;

  template <typename T>
  typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
  is_disjoint_from(const T& y) const;

  template <typename From1, typename From2>
  I_Result assign(const From1& l, const From2& u);

  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  assign(const From& x);

  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  join_assign(const From& x);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  join_assign(const From1& x, const From2& y);

  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  intersect_assign(const From& x);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  intersect_assign(const From1& x, const From2& y);

  /*! \brief
    Refines \p to according to the existential relation \p rel with \p x.

    The \p to interval is restricted to become, upon successful exit,
    the smallest interval of its type that contains the set
    \f[
      \{\,
        a \in \mathtt{to}
      \mid
        \exists b \in \mathtt{x} \st a \mathrel{\mathtt{rel}} b
      \,\}.
    \f]
    \return
    ???
  */
  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  refine_existential(Relation_Symbol rel, const From& x);

  /*! \brief
    Refines \p to so that it satisfies the universal relation \p rel with \p x.

    The \p to interval is restricted to become, upon successful exit,
    the smallest interval of its type that contains the set
    \f[
      \{\,
        a \in \mathtt{to}
      \mid
        \forall b \in \mathtt{x} \itc a \mathrel{\mathtt{rel}} b
      \,\}.
    \f]
    \return
    ???
  */
  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  refine_universal(Relation_Symbol rel, const From& x);

  template <typename From>
  typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
  neg_assign(const From& x);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  add_assign(const From1& x, const From2& y);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  sub_assign(const From1& x, const From2& y);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  mul_assign(const From1& x, const From2& y);

  template <typename From1, typename From2>
  typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
		      && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
  div_assign(const From1& x, const From2& y);

private:
  Boundary lower_;
  Boundary upper_;
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
  unsigned int lower_loaded:1;
  unsigned int upper_loaded:1;
  unsigned int completed:1;
#endif
};


template <typename Boundary, typename Info>
inline bool
f_is_empty(const Interval<Boundary, Info>& x) {
  return x.is_empty();
}
template <typename Boundary, typename Info>
inline bool
f_is_singleton(const Interval<Boundary, Info>& x) {
  return x.is_singleton();
}
template <typename Boundary, typename Info>
inline int
is_infinity(const Interval<Boundary, Info>& x) {
  return x.is_infinity();
}

namespace Interval_NS {

template <typename Boundary, typename Info>
inline const Boundary&
f_lower(const Interval<Boundary, Info>& x) {
  return x.lower();
}
template <typename Boundary, typename Info>
inline const Boundary&
f_upper(const Interval<Boundary, Info>& x) {
  return x.upper();
}
template <typename Boundary, typename Info>
inline const Info&
f_info(const Interval<Boundary, Info>& x) {
  return x.info();
}

struct Scalar_As_Interval_Policy {
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, true);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef Interval_Restriction_None<Interval_Info_Null<Scalar_As_Interval_Policy> > Scalar_As_Interval_Info;

const Scalar_As_Interval_Info SCALAR_INFO;

typedef Interval_Restriction_None<Interval_Info_Null_Open<Scalar_As_Interval_Policy> > Scalar_As_Interval_Info_Open;

template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, const T&>::type
f_lower(const T& x) {
  return x;
}
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, const T&>::type
f_upper(const T& x) {
  return x;
}
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, const Scalar_As_Interval_Info&>::type
f_info(const T&) {
  return SCALAR_INFO;
}
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, Scalar_As_Interval_Info_Open>::type
f_info(const T&, bool open) {
  return Scalar_As_Interval_Info_Open(open);
}

template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, bool>::type
f_is_empty(const T& x) {
  return is_not_a_number(x);
}

template <typename T>
inline typename Enable_If<Is_Singleton<T>::value, bool>::type
f_is_singleton(const T& x) {
  return !f_is_empty(x);
}

template <typename T>
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, Ternary>::type
f_is_empty_lazy(const T& x) {
  if (f_info(x).get_interval_property(CARDINALITY_0))
    return f_info(x).get_interval_property(CARDINALITY_IS) ? T_YES : T_NO;
  else
    return T_MAYBE;
}

} // Interval_NS

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
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
is_singleton_integer(const T& x) {
  return is_singleton(x) && is_integer(f_lower(x));
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
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
check_empty_arg(const T& x) {
  if (f_info(x).may_be_empty)
    return f_is_empty(x);
  else {
    assert(!f_is_empty(x));
    return false;
  }
}

template <typename Boundary, typename Info>
inline I_Result
check_empty_result(const Interval<Boundary, Info>& x, I_Result r) {
  if (Info::check_empty_result && f_is_empty(x))
    return I_EMPTY;
  else
    return static_cast<I_Result>(r | I_MAYBE_EMPTY);
}

template <typename T1, typename T2>
inline typename Enable_If<((Is_Singleton<T1>::value || Is_Interval<T1>::value)
			   && (Is_Singleton<T2>::value || Is_Interval<T2>::value)
			   && (Is_Interval<T1>::value || Is_Interval<T2>::value)),
			  bool>::type
operator==(const T1& x, const T2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x))
    return check_empty_arg(y);
  else if (check_empty_arg(y))
    return false;
  return eq_restriction(f_info(x), f_info(y))
    && eq(LOWER, f_lower(x), f_info(x), LOWER, f_lower(y), f_info(y))
    && eq(UPPER, f_upper(x), f_info(x), UPPER, f_upper(y), f_info(y));
}

template <typename T1, typename T2>
inline typename Enable_If<((Is_Singleton<T1>::value || Is_Interval<T1>::value)
			   && (Is_Singleton<T2>::value || Is_Interval<T2>::value)
			   && (Is_Interval<T1>::value || Is_Interval<T2>::value)),
			  bool>::type
operator!=(const T1& x, const T2& y) {
  return !(x == y);
}

template <typename Boundary, typename Info>
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
Interval<Boundary, Info>::contains(const T& y) const {
  assert(OK());
  assert(f_OK(y));
  if (check_empty_arg(y))
    return true;
  if (check_empty_arg(*this))
    return false;
  if (!contains_restriction(info(), f_info(y)))
      return false;
  return le(LOWER, lower(), info(), LOWER, f_lower(y), f_info(y))
    && ge(UPPER, upper(), info(), UPPER, f_upper(y), f_info(y));
}

template <typename Boundary, typename Info>
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
Interval<Boundary, Info>::strictly_contains(const T& y) const {
  assert(OK());
  assert(f_OK(y));
  if (check_empty_arg(y))
    return !check_empty_arg(*this);
  if (check_empty_arg(*this))
    return false;
  if (!contains_restriction(y))
      return false;
  else if (!eq_restriction(*this, y))
    return le(LOWER, lower(), info(), LOWER, f_lower(y), f_info(y))
      && ge(UPPER, upper(), info(), UPPER, f_upper(y), f_info(y));
  return (lt(LOWER, lower(), info(), LOWER, f_lower(y), f_info(y))
	  && ge(UPPER, upper(), info(), UPPER, f_upper(y), f_info(y)))
    || (le(LOWER, lower(), info(), LOWER, f_lower(y), f_info(y))
	&& gt(UPPER, upper(), info(), UPPER, f_upper(y), f_info(y)));
}

template <typename Boundary, typename Info>
template <typename T>
inline typename Enable_If<Is_Singleton<T>::value || Is_Interval<T>::value, bool>::type
Interval<Boundary, Info>::is_disjoint_from(const T& y) const {
  assert(OK());
  assert(f_OK(y));
  if (check_empty_arg(*this) || check_empty_arg(y))
    return true;
//   CHECKME.
//   if (!contains_restriction(info(), f_info(y)))
//       return false;
  return gt(LOWER, lower(), info(), UPPER, f_upper(y), f_info(y))
    || lt(UPPER, upper(), info(), LOWER, f_lower(y), f_info(y));
}

template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline I_Result
Interval<To_Boundary, To_Info>::assign(const From1& l, const From2& u) {
  info().clear();
  Result rl = Boundary_NS::assign(LOWER, lower(), info(), LOWER, l, f_info(l));
  Result ru = Boundary_NS::assign(UPPER, upper(), info(), UPPER, u, f_info(u));
  complete_init_internal();
  assert(OK());
  return check_empty_result(*this, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::assign(const From& x) {
  assert(f_OK(x));
  if (check_empty_arg(x))
    return assign(EMPTY);
  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!assign_restriction(to_info, x))
    return assign(EMPTY);
  Result rl = Boundary_NS::assign(LOWER, lower(), to_info,
				  LOWER, f_lower(x), f_info(x));
  Result ru = Boundary_NS::assign(UPPER, upper(), to_info,
				  UPPER, f_upper(x), f_info(x));
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::join_assign(const From& x) {
  assert(f_OK(x));
  if (check_empty_arg(*this))
    return assign(x);
  if (check_empty_arg(x))
    return combine(V_EQ, V_EQ);
  if (!join_restriction(info(), *this, x))
    return assign(EMPTY);
  info().set_interval_property(CARDINALITY_IS, false);
  info().set_interval_property(CARDINALITY_0);
  info().set_interval_property(CARDINALITY_1, false);
  Result rl, ru;
  rl = min_assign(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x));
  ru = max_assign(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x));
  assert(OK());
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::join_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x))
    return assign(y);
  if (check_empty_arg(y))
    return assign(x);
  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!join_restriction(to_info, x, y))
    return assign(EMPTY);
  to_info.set_interval_property(CARDINALITY_0);
  Result rl, ru;
  rl = min_assign(LOWER, lower(), to_info,
		  LOWER, f_lower(x), f_info(x),
		  LOWER, f_lower(y), f_info(y));
  ru = max_assign(UPPER, upper(), to_info,
		  UPPER, f_upper(x), f_info(x),
		  UPPER, f_upper(y), f_info(y));
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::intersect_assign(const From& x) {
  assert(f_OK(x));
  if (!intersect_restriction(info(), *this, x))
    return assign(EMPTY);
  // FIXME: more accurate?
  invalidate_cardinality_cache();
  Result rl, ru;
  rl = max_assign(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x));
  ru = min_assign(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x));
  assert(OK());
  return check_empty_result(*this, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::intersect_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!intersect_restriction(to_info, x, y))
    return assign(EMPTY);
  Result rl, ru;
  rl = max_assign(LOWER, lower(), to_info,
		  LOWER, f_lower(x), f_info(x),
		  LOWER, f_lower(y), f_info(y));
  ru = min_assign(UPPER, upper(), to_info,
		  UPPER, f_upper(x), f_info(x),
		  UPPER, f_upper(y), f_info(y));
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return check_empty_result(*this, combine(rl, ru));
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::refine_existential(Relation_Symbol rel, const From& x) {
  assert(OK());
  assert(f_OK(x));
  if (check_empty_arg(x))
    return assign(EMPTY);
  switch (rel) {
  case LESS_THAN:
    {
      if (lt(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(UPPER);
      Result ru = Boundary_NS::assign(UPPER, upper(), info(),
				      UPPER, f_upper(x), f_info(x), true);
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, ru));
    }
  case LESS_OR_EQUAL:
    {
      if (le(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(UPPER);
      Result ru = Boundary_NS::assign(UPPER, upper(), info(),
				      UPPER, f_upper(x), f_info(x));
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, ru));
    }
  case GREATER_THAN:
    {
      if (gt(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(LOWER);
      Result rl = Boundary_NS::assign(LOWER, lower(), info(),
				      LOWER, f_lower(x), f_info(x), true);
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(rl, V_EQ));
    }
  case GREATER_OR_EQUAL:
    {
      if (ge(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(LOWER);
      Result rl = Boundary_NS::assign(LOWER, lower(), info(),
				      LOWER, f_lower(x), f_info(x));
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(rl, V_EQ));
    }
  case EQUAL:
    return intersect_assign(x);
  case NOT_EQUAL:
    {
      if (!f_is_singleton(x))
	return combine(V_EQ, V_EQ);
      if (check_empty_arg(*this))
	return I_EMPTY;
      if (eq(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x)))
	lower_shrink();
      if (eq(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x)))
	upper_shrink();
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, V_EQ));
    }
  default:
    assert(false);
    return I_EMPTY;
  }
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::refine_universal(Relation_Symbol rel, const From& x) {
  assert(OK());
  assert(f_OK(x));
  if (check_empty_arg(x))
    return combine(V_EQ, V_EQ);
  switch (rel) {
  case LESS_THAN:
    {
      if (lt(UPPER, upper(), info(), LOWER, f_lower(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(UPPER);
      Result ru = Boundary_NS::assign(UPPER, upper(), info(),
				      LOWER, f_lower(x), f_info(x), true);
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, ru));
    }
  case LESS_OR_EQUAL:
    {
      if (le(UPPER, upper(), info(), LOWER, f_lower(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(UPPER);
      Result ru = Boundary_NS::assign(UPPER, upper(), info(),
				      LOWER, f_lower(x), f_info(x));
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, ru));
    }
  case GREATER_THAN:
    {
      if (gt(LOWER, lower(), info(), UPPER, f_upper(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(LOWER);
      Result rl = Boundary_NS::assign(LOWER, lower(), info(),
				      UPPER, f_upper(x), f_info(x), true);
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(rl, V_EQ));
    }
  case GREATER_OR_EQUAL:
    {
      if (ge(LOWER, lower(), info(), UPPER, f_upper(x), f_info(x)))
	return combine(V_EQ, V_EQ);
      info().clear_boundary_properties(LOWER);
      Result rl = Boundary_NS::assign(LOWER, lower(), info(),
				      UPPER, f_upper(x), f_info(x));
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(rl, V_EQ));
    }
  case EQUAL:
    if (!f_is_singleton(x))
      return assign(EMPTY);
    return intersect_assign(x);
  case NOT_EQUAL:
    {
      if (check_empty_arg(*this))
	return I_EMPTY;
      if (eq(LOWER, lower(), info(), LOWER, f_lower(x), f_info(x)))
	lower_shrink();
      if (eq(UPPER, upper(), info(), UPPER, f_upper(x), f_info(x)))
	upper_shrink();
      invalidate_cardinality_cache();
      normalize();
      return check_empty_result(*this, combine(V_EQ, V_EQ));
    }
  default:
    assert(false);
    return I_EMPTY;
  }
}

template <typename To_Boundary, typename To_Info>
template <typename From>
inline typename Enable_If<Is_Singleton<From>::value || Is_Interval<From>::value, I_Result>::type
Interval<To_Boundary, To_Info>::neg_assign(const From& x) {
  assert(f_OK(x));
  if (check_empty_arg(x))
    return assign(EMPTY);
  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!neg_restriction(to_info, x))
    return assign(EMPTY);
  Result rl, ru;
  DIRTY_TEMP(To_Boundary, to_lower);
  rl = Boundary_NS::neg_assign(LOWER, to_lower, to_info, UPPER, f_upper(x), f_info(x));
  ru = Boundary_NS::neg_assign(UPPER, upper(), to_info, LOWER, f_lower(x), f_info(x));
  assign_or_swap(lower(), to_lower);
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::add_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x) || check_empty_arg(y))
    return assign(EMPTY);
  int inf = Parma_Polyhedra_Library::is_infinity(x);
  if (inf) {
    if (Parma_Polyhedra_Library::is_infinity(y) == -inf)
      return assign(EMPTY);
  }
  else
    inf = Parma_Polyhedra_Library::is_infinity(y);
  if (inf < 0)
    return assign(MINUS_INFINITY);
  else if (inf > 0)
    return assign(PLUS_INFINITY);
  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!add_restriction(to_info, x, y))
    return assign(EMPTY);
  Result rl = Boundary_NS::add_assign(LOWER, lower(), to_info,
				      LOWER, f_lower(x), f_info(x),
				      LOWER, f_lower(y), f_info(y));
  Result ru = Boundary_NS::add_assign(UPPER, upper(), to_info,
				      UPPER, f_upper(x), f_info(x),
				      UPPER, f_upper(y), f_info(y));
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::sub_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x) || check_empty_arg(y))
    return assign(EMPTY);
  int inf = Parma_Polyhedra_Library::is_infinity(x);
  if (inf) {
    if (Parma_Polyhedra_Library::is_infinity(y) == inf)
      return assign(EMPTY);
  }
  else
    inf = -Parma_Polyhedra_Library::is_infinity(y);
  if (inf < 0)
    return assign(MINUS_INFINITY);
  else if (inf > 0)
    return assign(PLUS_INFINITY);

  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!sub_restriction(to_info, x, y))
    return assign(EMPTY);
  Result rl, ru;
  DIRTY_TEMP(To_Boundary, to_lower);
  rl = Boundary_NS::sub_assign(LOWER, to_lower, to_info,
			       LOWER, f_lower(x), f_info(x),
			       UPPER, f_upper(y), f_info(y));
  ru = Boundary_NS::sub_assign(UPPER, upper(), to_info,
			       UPPER, f_upper(x), f_info(x),
			       LOWER, f_lower(y), f_info(y));
  assign_or_swap(lower(), to_lower);
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

/**
+---------+-----------+-----------+-----------------+
|    *    |  yl > 0   |  yu < 0   |  yl < 0, yu > 0 |
+---------+-----------+-----------+-----------------+
| xl > 0  |xl*yl,xu*yu|xu*yl,xl*yu|   xu*yl,xu*yu   |
+---------+-----------+-----------+-----------------+
| xu < 0  |xl*yu,xu*yl|xu*yu,xl*yl|   xl*yu,xl*yl   |
+---------+-----------+-----------+-----------------+
|xl<0 xu>0|xl*yu,xu*yu|xu*yl,xl*yl|min(xl*yu,xu*yl),|
|         |           |           |max(xl*yl,xu*yu) |
+---------+-----------+-----------+-----------------+
**/
template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::mul_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x) || check_empty_arg(y))
    return assign(EMPTY);
  int xls = sgn_b(LOWER, f_lower(x), f_info(x));
  int xus = xls > 0 ? 1 : sgn_b(UPPER, f_upper(x), f_info(x));
  int yls = sgn_b(LOWER, f_lower(y), f_info(y));
  int yus = yls > 0 ? 1 : sgn_b(UPPER, f_upper(y), f_info(y));
  int inf = Parma_Polyhedra_Library::is_infinity(x);
  int ls, us;
  if (inf) {
    ls = yls;
    us = yus;
    goto inf;
  }
  else {
    inf = Parma_Polyhedra_Library::is_infinity(y);
    if (inf) {
      ls = xls;
      us = xus;
    inf:
      if (ls == 0 && us == 0)
	return assign(EMPTY);
      if (ls == -us)
	return set_infinities();
      if (ls < 0 || us < 0)
	inf = -inf;
      if (inf < 0)
	return assign(MINUS_INFINITY);
      else
	return assign(PLUS_INFINITY);
    }
  }

  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!mul_restriction(to_info, x, y))
    return assign(EMPTY);
  Result rl, ru;
  DIRTY_TEMP(To_Boundary, to_lower);

  if (xls >= 0) {
    if (yls >= 0) {
      // 0 <= xl <= xu, 0 <= yl <= yu
      rl = mul_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
      ru = mul_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
    }
    else if (yus <= 0) {
      // 0 <= xl <= xu, yl <= yu <= 0
      rl = mul_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
      ru = mul_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
    }
    else {
      // 0 <= xl <= xu, yl < 0 < yu
      rl = mul_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
      ru = mul_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
    }
  }
  else if (xus <= 0) {
    if (yls >= 0) {
      // xl <= xu <= 0, 0 <= yl <= yu
      rl = mul_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
      ru = mul_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
    }
    else if (yus <= 0) {
      // xl <= xu <= 0, yl <= yu <= 0
      rl = mul_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
      ru = mul_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
    }
    else {
      // xl <= xu <= 0, yl < 0 < yu
      rl = mul_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
      ru = mul_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
    }
  }
  else if (yls >= 0) {
    // xl < 0 < xu, 0 <= yl <= yu
    rl = mul_assign_z(LOWER, to_lower, to_info,
		      LOWER, f_lower(x), f_info(x), xls,
		      UPPER, f_upper(y), f_info(y), yus);
    ru = mul_assign_z(UPPER, upper(), to_info,
		      UPPER, f_upper(x), f_info(x), xus,
		      UPPER, f_upper(y), f_info(y), yus);
  }
  else if (yus <= 0) {
    // xl < 0 < xu, yl <= yu <= 0
    rl = mul_assign_z(LOWER, to_lower, to_info,
		      UPPER, f_upper(x), f_info(x), xus,
		      LOWER, f_lower(y), f_info(y), yls);
    ru = mul_assign_z(UPPER, upper(), to_info,
		      LOWER, f_lower(x), f_info(x), xls,
		      LOWER, f_lower(y), f_info(y), yls);
  }
  else {
    // xl < 0 < xu, yl < 0 < yu
    DIRTY_TEMP(To_Boundary, tmp);
    DIRTY_TEMP(To_Info, tmp_info);
    tmp_info.clear();
    Result tmp_r;
    tmp_r = Boundary_NS::mul_assign(LOWER, tmp, tmp_info,
				    UPPER, f_upper(x), f_info(x),
				    LOWER, f_lower(y), f_info(y));
    rl = Boundary_NS::mul_assign(LOWER, to_lower, to_info,
				 LOWER, f_lower(x), f_info(x),
				 UPPER, f_upper(y), f_info(y));
    if (gt(LOWER, to_lower, to_info, LOWER, tmp, tmp_info)) {
      to_lower = tmp;
      rl = tmp_r;
    }
    tmp_info.clear();
    tmp_r = Boundary_NS::mul_assign(UPPER, tmp, tmp_info,
				    UPPER, f_upper(x), f_info(x),
				    UPPER, f_upper(y), f_info(y));
    ru = Boundary_NS::mul_assign(UPPER, upper(), to_info,
				 LOWER, f_lower(x), f_info(x),
				 LOWER, f_lower(y), f_info(y));
    if (lt(UPPER, upper(), to_info, UPPER, tmp, tmp_info)) {
      upper() = tmp;
      ru = tmp_r;
    }
  }
  assign_or_swap(lower(), to_lower);
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

/**
+-----------+-----------+-----------+
|     /     |  yu < 0   |  yl > 0   |
+-----------+-----------+-----------+
|   xu<=0   |xu/yl,xl/yu|xl/yl,xu/yu|
+-----------+-----------+-----------+
|xl<=0 xu>=0|xu/yu,xl/yu|xl/yl,xu/yl|
+-----------+-----------+-----------+
|   xl>=0   |xu/yu,xl/yl|xl/yu,xu/yl|
+-----------+-----------+-----------+
**/
template <typename To_Boundary, typename To_Info>
template <typename From1, typename From2>
inline typename Enable_If<((Is_Singleton<From1>::value || Is_Interval<From1>::value)
			   && (Is_Singleton<From2>::value || Is_Interval<From2>::value)), I_Result>::type
Interval<To_Boundary, To_Info>::div_assign(const From1& x, const From2& y) {
  assert(f_OK(x));
  assert(f_OK(y));
  if (check_empty_arg(x) || check_empty_arg(y))
    return assign(EMPTY);
  int yls = sgn_b(LOWER, f_lower(y), f_info(y));
  int yus = yls > 0 ? 1 : sgn_b(UPPER, f_upper(y), f_info(y));
  if (yls == 0 && yus == 0)
    return assign(EMPTY);
  int inf = Parma_Polyhedra_Library::is_infinity(x);
  if (inf) {
    if (Parma_Polyhedra_Library::is_infinity(y))
      return assign(EMPTY);
    if (yls == -yus)
      return set_infinities();
    if (yls < 0 || yus < 0)
      inf = -inf;
    if (inf < 0)
      return assign(MINUS_INFINITY);
    else
      return assign(PLUS_INFINITY);
  }
  int xls = sgn_b(LOWER, f_lower(x), f_info(x));
  int xus = xls > 0 ? 1 : sgn_b(UPPER, f_upper(x), f_info(x));

  DIRTY_TEMP(To_Info, to_info);
  to_info.clear();
  if (!div_restriction(to_info, x, y))
    return assign(EMPTY);
  Result rl, ru;
  DIRTY_TEMP(To_Boundary, to_lower);
  if (yls >= 0) {
    if (xls >= 0) {
      rl = div_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
      ru = div_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
    }
    else if (xus <= 0) {
      rl = div_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
      ru = div_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
    }
    else {
      rl = div_assign_z(LOWER, to_lower, to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
      ru = div_assign_z(UPPER, upper(), to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
    }
  }
  else if (yus <= 0) {
    if (xls >= 0) {
      rl = div_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
      ru = div_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			LOWER, f_lower(y), f_info(y), yls);
    }
    else if (xus <= 0) {
      rl = div_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			LOWER, f_lower(y), f_info(y), yls);
      ru = div_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
    }
    else {
      rl = div_assign_z(LOWER, to_lower, to_info,
			UPPER, f_upper(x), f_info(x), xus,
			UPPER, f_upper(y), f_info(y), yus);
      ru = div_assign_z(UPPER, upper(), to_info,
			LOWER, f_lower(x), f_info(x), xls,
			UPPER, f_upper(y), f_info(y), yus);
    }
  }
  else {
    // FIXME: restrictions
    return static_cast<I_Result>(assign(UNIVERSE) | I_SINGULARITIES);
  }
  assign_or_swap(lower(), to_lower);
  assign_or_swap(info(), to_info);
  complete_init_internal();
  assert(OK());
  return combine(rl, ru);
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator+(const Interval<B, Info>& x, const T& y) {
  Interval<B, Info> z;
  z.add_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator+(const T& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.add_assign(x, y);
  return z;
}

template <typename B, typename Info>
inline Interval<B, Info>
operator+(const Interval<B, Info>& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.add_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator-(const Interval<B, Info>& x, const T& y) {
  Interval<B, Info> z;
  z.sub_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator-(const T& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.sub_assign(x, y);
  return z;
}

template <typename B, typename Info>
inline Interval<B, Info>
operator-(const Interval<B, Info>& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.sub_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator*(const Interval<B, Info>& x, const T& y) {
  Interval<B, Info> z;
  z.mul_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator*(const T& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.mul_assign(x, y);
  return z;
}

template <typename B, typename Info>
inline Interval<B, Info>
operator*(const Interval<B, Info>& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.mul_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator/(const Interval<B, Info>& x, const T& y) {
  Interval<B, Info> z;
  z.div_assign(x, y);
  return z;
}

template <typename B, typename Info, typename T>
inline typename Enable_If<Is_Singleton<T>::value, Interval<B, Info> >::type
operator/(const T& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.div_assign(x, y);
  return z;
}

template <typename B, typename Info>
inline Interval<B, Info>
operator/(const Interval<B, Info>& x, const Interval<B, Info>& y) {
  Interval<B, Info> z;
  z.div_assign(x, y);
  return z;
}

template <typename Boundary, typename Info>
inline std::ostream&
operator<<(std::ostream& os, const Interval<Boundary, Info>& x) {
  assert(x.OK());
  if (check_empty_arg(x))
    return os << "[]";
  if (x.is_singleton()) {
    output(os, x.lower(), Numeric_Format(), ROUND_NOT_NEEDED);
    return os;
  }
  os << (x.lower_is_open() ? "(" : "[");
  if (x.info().get_boundary_property(LOWER, SPECIAL))
    os << "-inf";
  else
    output(os, x.lower(), Numeric_Format(), ROUND_NOT_NEEDED);
  os << ", ";
  if (x.info().get_boundary_property(UPPER, SPECIAL))
    os << "+inf";
  else
    output(os, x.upper(), Numeric_Format(), ROUND_NOT_NEEDED);
  os << (x.upper_is_open() ? ")" : "]");
  output_restriction(os, x.info());
  return os;
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

/*! \brief
  Helper class to select the appropriate numerical type to perform
  boundary computations so as to reduce the chances of overflow without
  incurring too much overhead.
*/
template <typename Interval_Boundary_Type> struct Select_Temp_Boundary_Type;

template <typename Interval_Boundary_Type>
struct Select_Temp_Boundary_Type {
  typedef Interval_Boundary_Type type;
};

template <>
struct Select_Temp_Boundary_Type<float> {
  typedef double type;
};

template <>
struct Select_Temp_Boundary_Type<signed char> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<unsigned char> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<signed short> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<unsigned short> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<signed int> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<unsigned int> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<signed long> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<unsigned long> {
  typedef signed long long type;
};

template <>
struct Select_Temp_Boundary_Type<unsigned long long> {
  typedef signed long long type;
};

} // namespace Parma_Polyhedra_Library

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
