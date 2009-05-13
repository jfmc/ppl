/* Circular Interval class.
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

#ifndef PPL_Circular_Interval_defs_hh
#define PPL_Circular_Interval_defs_hh 1

#include <algorithm>
#include <iostream>
#include <string>
#include <cassert>

#include "intervals.defs.hh"
#include "Bool4.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename U>
void neg_wrap_unsigned(U& to, const U& x, const U& modulo) {
  if (modulo && x == 0)
    to = x;
  else
    to = modulo - x;
}

template <typename U>
void neg_wrap_signed(U& to, const U& x, const U& modulo) {
  if (modulo && x == modulo >> 1)
    to = x;
  else
    to = -x;
}

template <typename U>
void add_wrap_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  to = x + y;
  if (to >= modulo)
    to -= modulo;
}

template <typename U>
void add_wrap_signed(U& to, const U& x, const U& y, const U& modulo) {
  to = x + y;
  if (modulo) {
    assert((modulo & U(modulo - 1)) == 0);
    to = (to + (modulo >> 1)) % modulo - (modulo >> 1);
  }
}

template <typename U>
void sub_wrap_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  if (x >= y)
    to = x - y;
  else
    to = modulo - y + x;
}

template <typename U>
void sub_wrap_signed(U& to, const U& x, const U& y, const U& modulo) {
  to = x + y;
  if (modulo) {
    assert((modulo & U(modulo - 1)) == 0);
    to = (to + (modulo >> 1)) % modulo - (modulo >> 1);
  }
}

template <typename U>
void mul_wrap_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  to = x * y;
  if (modulo) {
    assert((modulo & U(modulo - 1)) == 0);
    to %= modulo;
  }
}

template <typename U>
void mul_wrap_signed(U& to, const U& x, const U& y, const U& modulo) {
  to = x * y;
  if (modulo) {
    assert((modulo & U(modulo - 1)) == 0);
    to = (to + (modulo >> 1)) % modulo - (modulo >> 1);
  }
}

template <typename U>
void div_wrap_unsigned(U& to, const U& x, const U& y, const U&) {
  to = x / y;
}

template <typename U>
void div_wrap_signed(U& to, const U& x, const U& y) {
  if (y == U(-1)) {
    neg_wrap_signed(to, x);
    return;
  }
  to = to_signed(x) / to_signed(y);
}

template <typename U>
bool add_saturates_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  if (x > U(modulo - 2) - y)
    return true;
  to = x + y;
  return false;
}

template <typename U>
bool mul_saturates_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  if (y == 0)
    to = 0;
  else {
    if (x > U(modulo - 2) / y)
      return true;
    to = x * y;
  }
  return false;
}

template <typename U>
bool add_mul_saturates_unsigned(U& to, const U& x, const U& y, const U& modulo) {
  U m;
  if (mul_saturates_unsigned(m, x, y, modulo))
    return true;
  return add_saturates_unsigned(to, to, m, modulo);
}

struct Circular_Interval_Base {
};

template <typename Value_, typename Policy_>
class Circular_Interval : public Circular_Interval_Base {
public:
  typedef Value_ boundary_type;
  typedef Value_ delta_type;
  typedef Policy_ Policy;
  typedef I_Result (Circular_Interval::*value_unary_method)(const Circular_Interval&);
  typedef I_Result (Circular_Interval::*value_binary_method)(const Circular_Interval&, const Circular_Interval&);
  typedef Bool4 (Circular_Interval::*bool_binary_method)(const Circular_Interval&);
private:
  // Return 0 if v is inside the interval or the distance between end and v
  delta_type distance_from_end(const boundary_type& v) const {
    assert(delta_ < delta_max());
    delta_type d;
    sub_wrap_unsigned(d, v, start_, Policy::modulo);
    return d > delta_ ? d - delta_ : 0;
  }
  // Return 0 if v is inside the interval or the distance between v and start
  delta_type distance_to_start(const boundary_type& v) const {
    assert(delta_ < delta_max());
    delta_type d;
    sub_wrap_unsigned(d, v, start_, Policy::modulo);
    return d > delta_ ? Policy::modulo - d : 0;
  }
  // Return 0 if v is outside the interval or 1 + the distance between v and end
  delta_type distance_to_end(const boundary_type& v) const {
    assert(delta_ < delta_max());
    delta_type d;
    sub_wrap_unsigned(d, v, start_, Policy::modulo);
    return d > delta_ ? 0 : delta_ + 1 - d;
  }
  // Return 0 if v is outside the interval or 1 + the distance between start and v
  delta_type distance_from_start(const boundary_type& v) const {
    assert(delta_ < delta_max());
    delta_type d;
    sub_wrap_unsigned(d, v, start_, Policy::modulo);
    return d > delta_ ? 0 : d + 1;
  }
  static const boundary_type delta_max() {
    return Policy::modulo - 1;
  }
  bool wider_or_after(const Circular_Interval& x) const {
    if (maybe_check_empty())
      return false;
    if (delta_ > x.delta_)
      return true;
    if (delta_ < x.delta_)
      return false;
    return start_ > x.start_;
  }
  bool not_degenerate_nor_smaller_contains(const Circular_Interval& x) const {
    delta_type d;
    sub_wrap_unsigned(d, x.start_, start_, Policy::modulo);
    return d <= delta_ - x.delta_;
  }
  I_Result lower_extend(const boundary_type &v) {
    assert(!is_empty());
    if (lower_is_domain_inf() || start_ <= v)
      return I_NOT_EMPTY | I_EXACT | I_UNCHANGED;
    delta_ += start_ - v;
    start_ = v;
    return I_SOME | I_UNIVERSE | I_EXACT | I_CHANGED;
  }
  I_Result upper_extend(const boundary_type &v) {
    assert(!is_empty());
    if (upper_is_domain_sup() || start_ + delta_ >= v)
      return I_NOT_EMPTY | I_EXACT | I_UNCHANGED;
    delta_ = v - start_;
    return I_SOME | I_UNIVERSE | I_EXACT | I_CHANGED;
  }
public:
  bool OK() const {
    return delta_ < delta_max() ||
      (delta_ == delta_max() && start_ <= 1);
  }
  Circular_Interval() {
  }
  template <typename T>
  Circular_Interval(const T& x) {
    assign(x);
  }
  template <typename L, typename U>
  Circular_Interval(const L& l, const U& u) {
    assign(l, u);
  }
  static const boundary_type domain_inf() {
    return 0;
  }
  static const boundary_type domain_sup() {
    return Policy::modulo - 1;
  }
  I_Result classify() const {
    if (delta_ == delta_max()) {
      if (start_ == 0)
	return I_UNIVERSE;
      assert(start_ == 1);
      return I_EMPTY;
    }
    else if (delta_ == 0)
      return I_SINGLETON;
    else
      return I_SOME;
  }
  bool is_empty() const {
    return delta_ == delta_max() && start_ == 1;
  }
  bool maybe_check_empty() const {
    return Policy::may_be_empty && is_empty();
  }
  bool is_universe() const {
    return delta_ == delta_max() && start_ == 0;
  }
  bool is_singleton() const {
    return delta_ == 0;
  }
  bool is_bounded() const {
    return true;
  }
  static bool is_always_topologically_closed() {
    return true;
  }
  bool is_topologically_closed() const {
    return true;
  }
  void topological_closure_assign() {
  }
  bool contains_integer_point() const {
    return !maybe_check_empty();
  }
  I_Constraint<boundary_type> lower_constraint() const {
    assert(!is_empty());
    return i_constraint(GREATER_OR_EQUAL, lower());
  }
  I_Constraint<boundary_type> upper_constraint() const {
    assert(!is_empty());
    return i_constraint(LESS_OR_EQUAL, upper());
  }
  bool lower_is_open() const {
    assert(!is_empty());
    return false;
  }
  bool upper_is_open() const {
    assert(!is_empty());
    return false;
  }
  boundary_type start() const {
    return start_;
  }
  delta_type delta() const {
    return delta_;
  }
  boundary_type end() const {
    assert(!is_empty());
    boundary_type u;
    add_wrap_unsigned(u, start_, delta_, Policy::modulo);
    return u;
  }
  bool is_splitted() const {
    assert(!is_empty());
    return delta_ > domain_sup() - start_;
  }
  boundary_type lower() const {
    assert(!is_empty());
    if (is_splitted())
      return domain_inf();
    return start_;
  }
  boundary_type upper() const {
    assert(!is_empty());
    if (is_splitted())
      return domain_sup();
    return end();
  }
  bool lower_is_domain_inf() const {
    if (maybe_check_empty())
      return false;
    return start_ == domain_inf() || is_splitted();
  }
  bool upper_is_domain_sup() const {
    if (maybe_check_empty())
      return false;
    return delta_ >= domain_sup() - start_;
  }
  bool lower_is_boundary_infinity() const {
    return false;
  }
  bool upper_is_boundary_infinity() const {
    return false;
  }
  bool contains(const boundary_type& v) const {
    if (maybe_check_empty())
      return false;
    delta_type d;
    sub_wrap_unsigned(d, v, start_, Policy::modulo);
    return d <= delta_;
  }
  bool contains(const Circular_Interval& x) const {
    if (x.maybe_check_empty())
      return true;
    if (maybe_check_empty())
      return false;
    if (delta_ == delta_max())
      return true;
    if (delta_ < x.delta_)
      return false;
    return not_degenerate_nor_smaller_contains(x);
  }
  bool strictly_contains(const Circular_Interval& x) const {
    if (maybe_check_empty())
      return false;
    if (x.maybe_check_empty())
      return true;
    if (delta_ <= x.delta_)
      return false;
    if (delta_ == delta_max())
      return true;
    return not_degenerate_nor_smaller_contains(x);
  }
  bool equal(const Circular_Interval& x) const {
    return start_ == x.start_ && delta_ == x.delta_;
  }
  bool is_disjoint_from(const Circular_Interval& x) const {
    if (maybe_check_empty() || x.maybe_check_empty())
      return true;
    return !contains(x.start_) && !x.contains(start_);
  }
  bool can_be_exactly_joined_to(const Circular_Interval& x) const {
    if (contains(x) || x.contains(*this))
      return true;
    return distance_from_end(x.start_) <= 1 || x.distance_from_end(start_) <= 1;
  }
  I_Result lower_extend() {
    return lower_extend(domain_inf());
  }
  template <typename C>
  typename Enable_If<Is_Same_Or_Derived<I_Constraint_Base, C>::value, I_Result>::type
  lower_extend(const C& c) {
    assert(!is_empty());
    boundary_type v;
    Result rel = c.convert_integer(v);
    switch (rel) {
    case V_LGE:
    case V_LT_PLUS_INFINITY:
      return lower_extend();
    case V_NAN:
      break;
    case V_GE:
    case V_EQ:
      return lower_extend(v);
    default:
      assert(false);
    }
    return I_NOT_EMPTY | I_EXACT | I_UNCHANGED;
  }
  I_Result upper_extend() {
    return upper_extend(domain_sup());
  }
  template <typename C>
  typename Enable_If<Is_Same_Or_Derived<I_Constraint_Base, C>::value, I_Result>::type
  upper_extend(const C& c) {
    assert(!is_empty());
    boundary_type v;
    Result rel = c.convert_integer(v);
    switch (rel) {
    case V_LGE:
    case V_LT_PLUS_INFINITY:
      return upper_extend();
    case V_NAN:
      break;
    case V_LE:
    case V_EQ:
      return upper_extend(v);
    default:
      assert(false);
    }
    return I_NOT_EMPTY | I_EXACT | I_UNCHANGED;
  }
  I_Result remove_sup() {
    assert(!is_empty());
    if (delta_ == 0)
      return assign(EMPTY) | I_CHANGED;
    else
      --delta_;
    return I_NOT_UNIVERSE | I_EXACT | I_CHANGED;
  }
  I_Result remove_inf() {
    assert(!is_empty());
    if (delta_ == 0)
      return assign(EMPTY) | I_CHANGED;
    else {
      --delta_;
      add_wrap_unsigned(start_, start_, delta_type(1), Policy::modulo);
    }
    return I_NOT_UNIVERSE | I_EXACT | I_CHANGED;
  }
#if 0
  void push() {
    assert(delta_ != delta_max());
    if (delta_ == delta_max() - 1)
      assign(UNIVERSE);
    else
      ++delta_;
  }
  void unshift() {
    assert(delta_ != delta_max());
    if (delta_ == delta_max() - 1)
      assign(UNIVERSE);
    else {
      ++delta_;
      sub_wrap_unsigned(start_, start_, delta_type(1), Policy::modulo);
    }
  }
#endif
  I_Result build() {
    return assign(UNIVERSE);
  }
  template <typename C>
  typename Enable_If<Is_Same_Or_Derived<I_Constraint_Base, C>::value, I_Result>::type
  build(const C& c) {
    start_ = 0; // Just to silent a warning
    Result rel = c.convert_integer(start_);
    switch (rel) {
    case V_LGE:
    case V_GT_MINUS_INFINITY:
    case V_LT_PLUS_INFINITY:
      return assign(UNIVERSE);
    default:
      return assign(EMPTY);
    case V_EQ:
      if (start_ < domain_inf() || start_ > domain_sup())
	return assign(EMPTY);
      delta_ = 0;
      return I_SINGLETON | I_EXACT;
    case V_NE:
      if (start_ < domain_inf() || start_ > domain_sup())
	return assign(UNIVERSE);
      add_wrap_unsigned(start_, start_, delta_type(1), Policy::modulo);
      delta_ = delta_max() - 1;
      return I_SOME | I_EXACT;
    case V_LE:
      if (start_ >= domain_sup())
	return assign(UNIVERSE);
      if (start_ < domain_inf())
	return assign(EMPTY);
      delta_ = start_ - domain_inf();
      start_ = domain_inf();
      return I_NOT_DEGENERATE | I_EXACT;
    case V_GE:
      if (start_ <= domain_inf())
	return assign(UNIVERSE);
      if (start_ > domain_sup())
	return assign(EMPTY);
      delta_ = domain_sup() - start_;
      return I_NOT_DEGENERATE | I_EXACT;
    }
  }
  template <typename C1, typename C2>
  typename Enable_If<Is_Same_Or_Derived<I_Constraint_Base, C1>::value &&
		     Is_Same_Or_Derived<I_Constraint_Base, C2>::value, I_Result>::type
  build(const C1& c1, const C2& c2) {
    switch (c1.rel()) {
    case V_LGE:
      return build(c2);
    case V_NAN:
      return assign(EMPTY);
    default:
      break;
    }
    switch (c2.rel()) {
    case V_LGE:
      return build(c1);
    case V_NAN:
      return assign(EMPTY);
    default:
      break;
    }
    build(c1);
    I_Result r = add_constraint(c2);
    return r - (I_CHANGED | I_UNCHANGED);
  }
  template <typename C>
  typename Enable_If<Is_Same_Or_Derived<I_Constraint_Base, C>::value, I_Result>::type
  add_constraint(const C& c) {
    Circular_Interval x;
    x.build(c);
    return intersect_assign(x);
  }
  I_Result assign(Degenerate_Element e) {
    delta_ = delta_max();
    switch (e) {
    default:
      assert(0);
      /* Fall through */
    case EMPTY:
      start_ = 1;
      return I_EMPTY | I_EXACT;
    case UNIVERSE:
      start_ = 0;
      return I_UNIVERSE | I_EXACT;
    }
  }
  I_Result assign(const boundary_type& v) {
    assert(v <= domain_sup());
    start_ = v;
    delta_ = 0;
    return I_SINGLETON | I_EXACT;
  }
  I_Result assign(const boundary_type& s, const boundary_type& e) {
    assert(s <= domain_sup());
    assert(e <= domain_sup());
    sub_wrap_unsigned(delta_, e, s, Policy::modulo);
    if (delta_ == delta_max()) {
      start_ = 0;
      return I_UNIVERSE | I_EXACT;
    }
    start_ = s;
    return I_NOT_EMPTY | I_EXACT;
  }
  I_Result assign(const Circular_Interval& x) {
    start_ = x.start_;
    delta_ = x.delta_;
    return I_ANY | I_EXACT;
  }
  template <typename From, typename From_Policy>
  I_Result assign(const Circular_Interval<From, From_Policy>& x) {
    if (delta_max() == x.delta_max()) {
      start_ = x.start_;
      delta_ = x.delta_;
      return I_ANY | I_EXACT;
    }
    if (x.maybe_check_empty())
      return assign(EMPTY);
    if (x.delta_ >= delta_max())
      return assign(UNIVERSE);
    if (delta_max() > x.delta_max()) {
      if (x.is_splitted()) {
	start_ = x.domain_inf();
	delta_ = x.domain_sup() - x.domain_inf();
	return I_NOT_DEGENERATE | I_INEXACT;
      }
    }
    else if (Policy::modulo) {
      start_ = x.start_ % Policy::modulo;
      delta_ = x.delta_;
      return I_NOT_EMPTY | I_EXACT;
    }
    start_ = x.start_;
    delta_ = x.delta_;
    return I_NOT_DEGENERATE | I_EXACT;
  }
  I_Result neg_assign(const Circular_Interval& x) {
    if (x.maybe_check_empty())
      return assign(EMPTY);
    if (x.is_universe())
      return assign(UNIVERSE);
    start_ = x.end();
    neg_wrap_unsigned(start_, start_, Policy::modulo);
    delta_ = x.delta_;
    return I_NOT_DEGENERATE | I_EXACT;
  }
  I_Result add_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.maybe_check_empty() || y.maybe_check_empty())
      return assign(EMPTY);
    if (x.is_universe() || y.is_universe() ||
	add_saturates_unsigned(delta_, x.delta_, y.delta_, Policy::modulo))
      return assign(UNIVERSE);
    add_wrap_unsigned(start_, x.start_, y.start_, Policy::modulo);
    return I_NOT_DEGENERATE | I_EXACT;
  }
  I_Result sub_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.maybe_check_empty() || y.maybe_check_empty())
      return assign(EMPTY);
    if (x.is_universe() || y.is_universe() ||
	add_saturates_unsigned(delta_, x.delta_, y.delta_, Policy::modulo))
      return assign(UNIVERSE);
    sub_wrap_unsigned(start_, x.start_, y.end(), Policy::modulo);
    return I_NOT_DEGENERATE | I_EXACT;
  }
  I_Result mul_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.maybe_check_empty() || y.maybe_check_empty())
      return assign(EMPTY);
    I_Result r;
    delta_type d;
    if (y.delta_ == 0 ?
	mul_saturates_unsigned(d, y.start_, x.delta_, Policy::modulo) :
	(add_saturates_unsigned(d, x.start_, x.delta_, Policy::modulo) ||
	 mul_saturates_unsigned(d, d, y.delta_, Policy::modulo) ||
	 add_mul_saturates_unsigned(d, y.start_, x.delta_, Policy::modulo))) {
      assign(UNIVERSE);
      r = I_UNIVERSE;
    }
    else {
      mul_wrap_unsigned(start_, x.start_, y.start_, Policy::modulo);
      delta_ = d;
      r = I_NOT_DEGENERATE;
    }
    // FIXME: We can do more to detect if result is approximated or exact
    // Perhaps the greater effort shoud be explicitly requested using a
    // flag in approximation direction argument
    return r;
  }
  I_Result div_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.maybe_check_empty() || y.maybe_check_empty())
      return assign(EMPTY);
    I_Result r = y.contains(0) ? I_SINGULARITIES : (I_Result)0;
    boundary_type yl = y.lower();
    delta_type yd = y.delta_;
    if (yl == 0) {
      if (yd == 0)
	return assign(EMPTY) | r;
      if (y.start_ == 0)
	--yd;
      yl = y.end() == 0 ? y.start_ : 1;
    }
    if (yd == 0) {
      if (yl == 1) {
	assign(x);
	return I_NOT_EMPTY | I_EXACT;
      }
      if (!x.is_splitted()) {
	start_ = x.start_ / yl;
	delta_ = (x.start_ + x.delta_) / yl - start_;
	return I_NOT_DEGENERATE | I_EXACT | r;
      }
    }
    boundary_type u = x.upper();
    div_wrap_unsigned(start_, x.lower(), y.upper(), Policy::modulo);
    div_wrap_unsigned(u, u, yl, Policy::modulo);
    sub_wrap_unsigned(delta_, u, start_, Policy::modulo);
    return I_NOT_EMPTY | r;
  }
  I_Result rem_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.maybe_check_empty() || y.maybe_check_empty())
      return assign(EMPTY);
    I_Result r = y.contains(0) ? I_SINGULARITIES : (I_Result)0;
    if (!y.is_singleton() || x.is_splitted()) {
      // FIXME: can we do something better?
      start_ = 0;
      delta_ = y.upper() - 1;
      return I_NOT_DEGENERATE | r;
    }
    // Only one divisor
    boundary_type yl = y.start_;
    if (yl == 0)
      return assign(EMPTY) | r;
    if (x.delta_ >= yl - 1) {
      start_ = 0;
      delta_ = yl - 1;
      return I_NOT_DEGENERATE | I_EXACT;
    }
    boundary_type s = x.start_ % yl;
    if (x.delta_ < yl - s) {
      start_ = s;
      delta_ = x.delta_;
      return I_NOT_DEGENERATE | I_EXACT;
    }
    start_ = 0;
    delta_ = yl - 1;
    return I_NOT_DEGENERATE;
  }
  I_Result complement_assign(const Circular_Interval& x) {
    if (x.maybe_check_empty())
      return assign(UNIVERSE);
    if (x.is_universe())
      return assign(EMPTY);
    add_wrap_unsigned(start_, x.start_, delta_type(x.delta_ + 1), Policy::modulo);
    delta_ = delta_type(Policy::modulo - 2) - x.delta_;
    return I_NOT_DEGENERATE | I_EXACT;
  }
  I_Result join_assign(const Circular_Interval& y) {
    if (contains(y))
      return I_ANY | I_EXACT | I_UNCHANGED;
    if (y.contains(*this)) {
      assign(y);
      return I_NOT_EMPTY | I_EXACT | I_CHANGED;
    }
    delta_type xy = distance_from_end(y.start_);
    delta_type yx = y.distance_from_end(start_);
    if (xy <= 1) {
      if (yx <= 1)
	return assign(UNIVERSE) | I_CHANGED;
      assign(start_, y.end());
      return I_SOME | I_EXACT | I_CHANGED;
    }
    else if (yx <= 1) {
      assign(y.start_, end());
      return I_SOME | I_EXACT | I_CHANGED;
    }
    else if (xy < yx || (xy == yx && start_ <= y.start_))
      assign(start_, y.end());
    else
      assign(y.start_, end());
    return I_SOME | I_INEXACT | I_CHANGED;
  }
  I_Result join_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.contains(y))
      return assign(x);
    if (y.contains(x)) {
      assign(y);
      return I_NOT_EMPTY | I_EXACT;
    }
    delta_type xy = x.distance_from_end(y.start_);
    delta_type yx = y.distance_from_end(x.start_);
    if (xy <= 1) {
      if (yx <= 1)
	return assign(UNIVERSE);
      assign(x.start_, y.end());
      return I_SOME | I_EXACT;
    }
    else if (yx <= 1) {
      assign(y.start_, x.end());
      return I_SOME | I_EXACT;
    }
    else if (xy < yx || (xy == yx && x.start_ <= y.start_))
      assign(x.start_, y.end());
    else
      assign(y.start_, x.end());
    return I_SOME | I_INEXACT;
  }
  I_Result intersect_assign(const Circular_Interval& y) {
    if (y.contains(*this))
      return I_ANY | I_EXACT | I_UNCHANGED;
    if (contains(y)) {
      assign(y);
      return I_NOT_UNIVERSE | I_EXACT | I_CHANGED;
    }
    delta_type xy = distance_to_end(y.start_);
    delta_type yx = y.distance_to_end(start_);
    if (xy == 0) {
      if (yx == 0)
	return assign(EMPTY) | I_CHANGED;
      assign(start_, y.end());
      return I_NOT_DEGENERATE | I_EXACT | I_CHANGED;
    }
    else if (yx == 0) {
      assign(y.start_, end());
      return I_NOT_DEGENERATE | I_EXACT | I_CHANGED;
    }
    // We could prefer to return an unchanged interval here
    else if (!y.wider_or_after(*this)) {
      assign(y);
      return I_NOT_DEGENERATE | I_INEXACT | I_UNCHANGED;
    }
    return I_NOT_DEGENERATE | I_INEXACT | I_UNCHANGED;
  }
  I_Result intersect_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (y.contains(x))
      return assign(x);
    if (x.contains(y)) {
      assign(y);
      return I_NOT_UNIVERSE | I_EXACT;
    }
    delta_type xy = x.distance_to_end(y.start_);
    delta_type yx = y.distance_to_end(x.start_);
    if (xy == 0) {
      if (yx == 0)
	return assign(EMPTY);
      assign(x.start_, y.end());
      return I_NOT_DEGENERATE | I_EXACT;
    }
    else if (yx == 0) {
      assign(y.start_, x.end());
      return I_NOT_DEGENERATE | I_EXACT;
    }
    else if (y.wider_or_after(x))
      assign(x);
    else
      assign(y);
    return I_NOT_DEGENERATE | I_INEXACT;
  }
  I_Result empty_intersection_assign(const Circular_Interval& x) {
    return complement_assign(x);
  }
  I_Result difference_assign(const Circular_Interval& y) {
    if (is_disjoint_from(y))
      return I_ANY | I_EXACT | I_UNCHANGED;
    if (is_universe())
      return complement_assign(y) | I_CHANGED;
    if (y.contains(*this))
      return assign(EMPTY) | I_CHANGED;
    if (distance_from_start(y.start_) <= 1) {
      sub_wrap_unsigned(delta_, end(), y.end(), Policy::modulo);
      --delta_;
      add_wrap_unsigned(start_, y.end(), delta_type(1), Policy::modulo);
      return I_NOT_DEGENERATE | I_EXACT | I_CHANGED;
    }
    if (distance_to_end(y.end()) <= 1) {
      sub_wrap_unsigned(delta_, y.start_, start_, Policy::modulo);
      --delta_;
      return I_NOT_DEGENERATE | I_EXACT | I_CHANGED;
    }
    boundary_type s;
    add_wrap_unsigned(s, y.start_, delta_type(y.delta_ + 1), Policy::modulo);
    delta_type d = delta_type(Policy::modulo - 2) - y.delta_;
    if (delta_ < y.delta_ || !not_degenerate_nor_smaller_contains(y)) {
      start_ = s;
      delta_ = d;
      return I_NOT_DEGENERATE | I_EXACT | I_CHANGED;
    }
    // We could prefer to return an unchanged interval here
    if (d < delta_ || (d == delta_ && s < start_)) {
      start_ = s;
      delta_ = d;
    }
    return I_NOT_DEGENERATE | I_INEXACT | I_UNCHANGED;
  }
  I_Result difference_assign(const Circular_Interval& x, const Circular_Interval& y) {
    if (x.is_disjoint_from(y))
      return assign(x);
    if (x.is_universe())
      return complement_assign(y);
    if (y.contains(x))
      return assign(EMPTY);
    if (x.distance_from_start(y.start_) <= 1) {
      sub_wrap_unsigned(delta_, x.end(), y.end(), Policy::modulo);
      --delta_;
      add_wrap_unsigned(start_, y.end(), delta_type(1), Policy::modulo);
      return I_NOT_DEGENERATE | I_EXACT;
    }
    if (x.distance_to_end(y.end()) <= 1) {
      sub_wrap_unsigned(delta_, y.start_, x.start_, Policy::modulo);
      --delta_;
      start_ = x.start_;
      return I_NOT_DEGENERATE | I_EXACT;
    }
    boundary_type s;
    add_wrap_unsigned(s, y.start_, delta_type(y.delta_ + 1), Policy::modulo);
    delta_type d = delta_type(Policy::modulo - 2) - y.delta_;
    if (x.delta_ < y.delta_ || !x.not_degenerate_nor_smaller_contains(y)) {
      start_ = s;
      delta_ = d;
      return I_NOT_DEGENERATE | I_EXACT;
    }
    if (d < x.delta_ || (d == x.delta_ && s < x.start_)) {
      start_ = s;
      delta_ = d;
    }
    else
      assign(x);
    return I_NOT_DEGENERATE | I_INEXACT;
  }
  bool simplify_using_context_assign(const Circular_Interval& x) {
    if (lower() < x.lower()) {
      lower_extend();
      return false;
    }
    if (upper() > x.upper()) {
      upper_extend();
      return false;
    }
    if (lower() == x.lower())
      lower_extend();
    if (upper() == x.upper())
      upper_extend();
    return true;
  }
  bool operator==(const Circular_Interval& x) const {
    return start_ == x.start_ && delta_ == x.delta_;
  }
  Bool4 eq(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (delta_ == 0 && x.delta_ == 0)
      return Bool4(start_ == x.start_);
    if (is_disjoint_from(x))
      return Bool4(false);
    else
      return Bool4(UNIVERSE);
  }
  Bool4 ne(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (delta_ == 0 && x.delta_ == 0)
      return Bool4(start_ != x.start_);
    if (is_disjoint_from(x))
      return Bool4(true);
    else
      return Bool4(UNIVERSE);
  }
  Bool4 lt(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (upper() < x.lower())
      return Bool4(true);
    if (lower() >= x.upper())
      return Bool4(false);
    return Bool4(UNIVERSE);
  }
  Bool4 le(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (upper() <= x.lower())
      return Bool4(true);
    if (lower() > x.upper())
      return Bool4(false);
    return Bool4(UNIVERSE);
  }
  Bool4 gt(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (lower() > x.upper())
      return Bool4(true);
    if (upper() <= x.lower())
      return Bool4(false);
    return Bool4(UNIVERSE);
  }
  Bool4 ge(const Circular_Interval& x) {
    if (maybe_check_empty() || x.maybe_check_empty())
      return Bool4(EMPTY);
    if (lower() >= x.upper())
      return Bool4(true);
    if (upper() < x.lower())
      return Bool4(false);
    return Bool4(UNIVERSE);
  }
  void print(std::ostream& os) const {
    if (is_empty())
      os << "[]";
    else if (is_singleton())
      os << start();
    else {
      os << "[";
      boundary_type s = start();
      boundary_type e = end();
      if (s < e)
	os << s << " .. " << e;
      else {
	os << domain_inf();
	if (domain_inf() < e)
	  os << " .. " << e;
	os << ", " << s;
	if (s < domain_sup())
	  os << " .. " << domain_sup();
      }
      os << "]";
    }
  }
  void swap(Circular_Interval& x) {
    std::swap(start_, x.start_);
    std::swap(delta_, x.delta_);
  }
  memory_size_type external_memory_in_bytes() const {
    return Parma_Polyhedra_Library::external_memory_in_bytes(start_) +
      Parma_Polyhedra_Library::external_memory_in_bytes(delta_);
  }
  void ascii_dump(std::ostream& s) const {
    using Parma_Polyhedra_Library::ascii_dump;
    s << "start ";
    ascii_dump(s, start_);
    s << " delta ";
    ascii_dump(s, delta_);
    s << '\n';
  }
  bool ascii_load(std::istream& s) {
    using Parma_Polyhedra_Library::ascii_load;
    std::string str;
    if (!(s >> str) || str != "start")
      return false;
    if (!ascii_load(s, start_))
      return false;
    if (!(s >> str) || str != "delta")
      return false;
    if (!ascii_load(s, delta_))
      return false;
    assert(OK());
    return true;
  }
protected:
  boundary_type start_;
  delta_type delta_;
};

template <typename Value, typename Policy>
inline std::ostream&
operator<<(std::ostream& s,
	   const Circular_Interval<Value, Policy>& x) {
  x.print(s);
  return s;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Circular_Interval_defs_hh)
