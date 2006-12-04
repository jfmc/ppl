/* Interval_Restriction class declaration.
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

#ifndef PPL_Interval_Restriction_defs_hh
#define PPL_Interval_Restriction_defs_hh 1

#include "meta_programming.hh"

namespace Parma_Polyhedra_Library {

struct Interval_;

template <typename T, typename Enable = void>
struct Boundary_Value {
  typedef T type;
};

template <typename T>
struct Boundary_Value<T, typename Enable_If<Is_Same_Or_Derived<Interval_, T>::value>::type > {
  typedef typename T::boundary_type type;
};

class Interval_Restriction_None_Base {
public:
  bool has_restriction() const {
    return false;
  }
  void normalize() const {
  }
  template <typename T>
  Result restrict(T&, Result dir) const {
    return dir;
  }
};

inline bool
eq_restriction(const Interval_Restriction_None_Base&, const Interval_Restriction_None_Base) {
  return true;
}

template <typename T>
inline bool
contains_restriction(const Interval_Restriction_None_Base&, const T&) {
  return true;
}

template <typename T>
inline void
assign_restriction(Interval_Restriction_None_Base&, const T&) {
}

template <typename T1, typename T2>
inline void
join_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

template <typename T1, typename T2>
inline void
intersect_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

template <typename T>
inline void
neg_restriction(Interval_Restriction_None_Base&, const T&) {
}

template <typename T1, typename T2>
inline void
add_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

template <typename T1, typename T2>
inline void
sub_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

template <typename T1, typename T2>
inline void
mul_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

template <typename T1, typename T2>
inline void
div_restriction(Interval_Restriction_None_Base&, const T1&, const T2&) {
}

inline void
output_restriction(std::ostream&, const Interval_Restriction_None_Base&) {
}

template <typename Base>
class Interval_Restriction_None : public Interval_Restriction_None_Base,
				 public Base {
public:
  Interval_Restriction_None() {
  };
};

class Interval_Restriction_Integer_Base {
};

template <typename Base>
class Interval_Restriction_Integer : public Interval_Restriction_Integer_Base, public Base {
public:
  Interval_Restriction_Integer() {
  }
  void set_integer(bool v = true) {
    return set_bit(Base::bitset, integer_bit, v);
  }
  bool get_integer() const {
    return get_bit(Base::bitset, integer_bit);
  }

  const_int_nodef(integer_bit, Base::next_bit);
  const_int_nodef(next_bit, integer_bit + 1);
  bool has_restriction() const {
    return get_integer();
  }
  void normalize() const {
  }
  template <typename T>
  Result restrict(T& x, Result dir) const {
    if (!has_restriction())
      return dir;
    switch (dir) {
    case V_GT:
      if (is_integer(x))
	return add_assign_r(x, x, static_cast<T>(1), ROUND_DOWN);
      /* Fall through */
    case V_GE:
      return ceil_assign_r(x, x, ROUND_DOWN);
    case V_LT:
      if (is_integer(x))
	sub_assign_r(x, x, static_cast<T>(1), ROUND_UP);
      /* Fall through */
    case V_LE:
      return floor_assign_r(x, x, ROUND_UP);
    default:
      assert(false);
      return dir;
    }
  }
};

class Simple_Restriction_Integer : public Interval_Restriction_Integer_Base {
public:
  Simple_Restriction_Integer(bool i)
    : integer(i) {
  }
  bool get_integer() const {
    return integer;
  }
private:
  bool integer;
};

template <typename From, typename Base, typename Enable = void>
struct Restriction_Integer;

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Native_Or_Checked<From>::value>::type> {
  typedef Simple_Restriction_Integer type;
  static type get(const From& x) {
    return Simple_Restriction_Integer(is_integer(x));
  }
};

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_None_Base, typename From::info_type>::value>::type> {
  typedef Simple_Restriction_Integer type;
  static type get(const From& x) {
    return Simple_Restriction_Integer(x.is_singleton() && is_integer(x.lower()));
  }
};

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, typename From::info_type>::value>::type> {
  typedef Interval_Restriction_Integer<Base> type;
  static const type& get(const From& x) {
    return x.info();
  }
};

template <typename T1, typename T2>
inline typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, T1>::value && Is_Same_Or_Derived<Interval_Restriction_Integer_Base, T2>::value, bool>::type
eq_restriction(const T1& x, const T2& y) {
  return x.get_integer() == y.get_integer();
}

template <typename T1, typename T2>
inline typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, T1>::value && Is_Same_Or_Derived<Interval_Restriction_Integer_Base, T2>::value, bool>::type
contains_restriction(const T1& x, const T2& y) {
  return !x.get_integer() || y.get_integer();
}

template <typename Base, typename From>
inline void
assign_restriction(Interval_Restriction_Integer<Base>& to, const From& x) {
  to.set_integer(Restriction_Integer<From, Base>::get(x).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
join_restriction(Interval_Restriction_Integer<Base>& to, const From1& x, const From2& y) {
  to.set_integer(Restriction_Integer<From1, Base>::get(x).get_integer()
		 && Restriction_Integer<From2, Base>::get(y).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
intersect_restriction(Interval_Restriction_Integer<Base>& to, const From1& x, const From2& y) {
  to.set_integer(Restriction_Integer<From1, Base>::get(x).get_integer()
		 || Restriction_Integer<From2, Base>::get(y).get_integer());
}

template <typename Base, typename From>
inline void
neg_restriction(Interval_Restriction_Integer<Base>& to, const From& x) {
  to.set_integer(Restriction_Integer<From, Base>::get(x).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
add_restriction(Interval_Restriction_Integer<Base>& to, const From1& x, const From2& y) {
  to.set_integer(Restriction_Integer<From1, Base>::get(x).get_integer()
		 && Restriction_Integer<From2, Base>::get(y).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
sub_restriction(Interval_Restriction_Integer<Base>& to, const From1& x, const From2& y) {
  to.set_integer(Restriction_Integer<From1, Base>::get(x).get_integer()
		 && Restriction_Integer<From2, Base>::get(y).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
mul_restriction(Interval_Restriction_Integer<Base>& to, const From1& x, const From2& y) {
  to.set_integer(Restriction_Integer<From1, Base>::get(x).get_integer()
		 && Restriction_Integer<From2, Base>::get(y).get_integer());
}

template <typename Base, typename From1, typename From2>
inline void
div_restriction(Interval_Restriction_Integer<Base>& to, const From1&, const From2&) {
  to.set_integer(false);
}

template <typename Base>
inline void
output_restriction(std::ostream& s, const Interval_Restriction_Integer<Base>& x) {
  if (x.get_integer())
    s << "i";
}

class Interval_Restriction_Integer_Modulo_Base {
};

template <typename T, typename Base>
class Interval_Restriction_Integer_Modulo : public Interval_Restriction_Integer_Modulo_Base, public Base {
public:
  COMPILE_TIME_CHECK(std::numeric_limits<T>::is_exact, "Type for modulo values must be exact.");
  Interval_Restriction_Integer_Modulo() {
    // FIXME: we'd have speed benefits with uninitialized info? (Dirty_Temp)
    clear();
  }
  bool has_restriction() const {
    return divisor != 0;
  }
  void clear() {
    remainder = 0;
    divisor = 0;
    Base::clear();
  }
  void normalize() const {
  }
  template <typename V>
  Result restrict(V& x, Result dir) const {
    if (!has_restriction())
      return dir;
    DIRTY_TEMP(V, n);
    DIRTY_TEMP(V, div);
    Result r;
    r = assign_r(div, divisor, ROUND_CHECK);
    assert(r == V_EQ);
    int s;
    r = rem_assign_r(n, x, div, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    s = sgn(n);
    switch (dir) {
    case V_GT:
      if (s >= 0) {
	r = sub_assign_r(n, div, n, ROUND_NOT_NEEDED);
	assert(r == V_EQ);
	return add_assign_r(x, x, n, ROUND_DOWN);
      }
      else
	return sub_assign_r(x, x, n, ROUND_DOWN);
    case V_GE:
      if (s > 0) {
	r = sub_assign_r(n, div, n, ROUND_NOT_NEEDED);
	assert(r == V_EQ);
	return add_assign_r(x, x, n, ROUND_DOWN);
      }
      else if (s < 0)
	return sub_assign_r(x, x, n, ROUND_DOWN);
      else
	return V_EQ;
    case V_LT:
      if (s <= 0) {
	r = add_assign_r(n, div, n, ROUND_NOT_NEEDED);
	assert(r == V_EQ);
	return sub_assign_r(x, x, n, ROUND_UP);
      }
      else
	return sub_assign_r(x, x, n, ROUND_UP);
    case V_LE:
      if (s < 0) {
	r = add_assign_r(n, div, n, ROUND_NOT_NEEDED);
	assert(r == V_EQ);
	return sub_assign_r(x, x, n, ROUND_UP);
      }
      else if (s > 0)
	return sub_assign_r(x, x, n, ROUND_UP);
      else
	return V_EQ;
    default:
      assert(false);
      return dir;
    }
  }
  void assign_or_swap(Interval_Restriction_Integer_Modulo& x) {
    Parma_Polyhedra_Library::assign_or_swap(remainder, x.remainder);
    Parma_Polyhedra_Library::assign_or_swap(divisor, x.divisor);
  }
  typedef T modulo_type;
  T remainder;
  T divisor;
};

template <typename T, typename Base>
struct Slow_Copy<Interval_Restriction_Integer_Modulo<T, Base> > : public Bool<Slow_Copy<T>::value> {};


template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, typename From::info_type>::value>::type> {
  typedef Simple_Restriction_Integer type;
  static type get(const From& x) {
    return Simple_Restriction_Integer(x.info().divisor != 0);
  }
};

template <typename T>
struct Simple_Restriction_Integer_Modulo : public Interval_Restriction_Integer_Modulo_Base {
  template <typename From>
  Simple_Restriction_Integer_Modulo(const From& r, const From& d)
    : remainder(r), divisor(d) {
  }
  typedef T modulo_type;
  T remainder;
  T divisor;
};

template <typename From, typename T, typename Base, typename Enable = void>
struct Restriction_Integer_Modulo;

template <typename From, typename T, typename Base>
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Native_Or_Checked<From>::value>::type> {
  typedef Simple_Restriction_Integer_Modulo<T> type;
  static const type& get(const From& x) {
    static const type integer(0, 1);
    static const type not_integer(0, 0);
    if (is_integer(x))
      return integer;
    else
      return not_integer;
  }
};

template <typename From, typename T, typename Base>
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_None_Base, typename From::info_type>::value>::type> {
  typedef Simple_Restriction_Integer_Modulo<T> type;
  static const type& get(const From& x) {
    static const type integer(0, 1);
    static const type not_integer(0, 0);
    if (x.is_singleton() && is_integer(x.lower()))
      return integer;
    else
      return not_integer;
  }
};

template <typename From, typename T, typename Base>
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, typename From::info_type>::value>::type> {
  typedef Simple_Restriction_Integer_Modulo<T> type;
  static const type& get(const From& x) {
    static const type integer(0, 1);
    static const type not_integer(0, 0);
    if (x.info().get_integer())
      return integer;
    else
      return not_integer;
  }
};

template <typename From, typename T, typename Base>
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, typename From::info_type>::value>::type> {
  typedef Interval_Restriction_Integer_Modulo<T, Base> type;
  static const type& get(const From& x) {
    return x.info();
  }
};

template <typename T1, typename T2>
inline typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, T1>::value && Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, T2>::value, bool>::type
eq_restriction(const T1& x, const T2& y) {
  return x.remainder == y.remainder
    && x.divisor == y.divisor;
}

template <typename T1, typename T2>
inline typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, T1>::value && Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, T2>::value, bool>::type
contains_restriction(const T1& x, const T2& y) {
  if (x.divisor == 0)
    return true;
  if (y.divisor == 0)
    return false;
  if (x.divisor == y.divisor)
    return x.remainder == y.remainder;
  DIRTY_TEMP(typename T1::modulo_type, v);
  Result r;
  r = rem_assign_r(v, y.divisor, x.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  if (v != 0)
    return false;
  r = rem_assign_r(v, y.remainder, x.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  return v == x.remainder;
}

template <typename T, typename Base>
inline void
set_unrestricted(Interval_Restriction_Integer_Modulo<T, Base>& to) {
  to.remainder = 0;
  to.divisor = 0;
}

template <typename T, typename Base>
inline void
set_integer(Interval_Restriction_Integer_Modulo<T, Base>& to) {
  to.remainder = 0;
  to.divisor = 1;
}

template <typename T, typename Base, typename From>
inline void
assign_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From& x) {
  to.remainder = Restriction_Integer_Modulo<From, T, Base>::get(x).remainder;
  to.divisor = Restriction_Integer_Modulo<From, T, Base>::get(x).divisor;
}

template <typename T, typename Base, typename From1, typename From2>
inline void
join_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  if (rx.divisor == 0)
    return set_unrestricted(to);
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (ry.divisor == 0)
    return set_unrestricted(to);
  else if (rx.divisor == 1 && ry.divisor == 1
      && is_singleton(x) && is_singleton(y)) {
    DIRTY_TEMP(typename Boundary_Value<From1>::type, a);
    DIRTY_TEMP(typename Boundary_Value<From2>::type, b);
    Result r;
    r = abs_assign_r(a, lower(x), ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = abs_assign_r(b, lower(y), ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    if (a > b)
      r = sub_assign_r(a, a, b, ROUND_CHECK);
    else
      r = sub_assign_r(a, b, a, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = assign_r(to.divisor, a, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = rem_assign_r(b, b, a, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = assign_r(to.remainder, b, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
  }
  else if (contains_restriction(rx, ry)) {
    to.remainder = rx.remainder;
    to.divisor = rx.divisor;
  }
  else if (contains_restriction(ry, rx)) {
    to.remainder = ry.remainder;
    to.divisor = ry.divisor;
  }
  else
    set_integer(to);
}

template <typename T, typename Base, typename From1, typename From2>
inline void
intersect_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (rx.divisor == 0) {
    to.remainder = ry.remainder;
    to.divisor = ry.divisor;
    return;
  }
  if (ry.divisor == 0) {
    to.remainder = rx.remainder;
    to.divisor = rx.divisor;
    return;
  }
  DIRTY_TEMP(T, d);
  Result r;
  r = lcm_assign_r(d, rx.divisor, ry.divisor, ROUND_DIRECT);
  if (r != V_EQ)
    return set_integer(to);
  to.divisor = d;
  // FIXME: to be completed
}

template <typename T, typename Base, typename From>
inline void
neg_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From& x) {
  assign_restriction(to, x);
}

template <typename T>
inline void
addmod(T& to, const T& x, const T& y, const T& to_m, const T& y_m) {
  Result r;
  if (std::numeric_limits<T>::is_bounded) {
    r = sub_assign_r(to, y_m, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    if (x <= to) {
      r = add_assign_r(to, x, y, ROUND_NOT_NEEDED);
      assert(r == V_EQ);
    }
    else {
      r = sub_assign_r(to, x, to, ROUND_NOT_NEEDED);
      assert(r == V_EQ);
    }
  }
  else {
    r = add_assign_r(to, x, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
  }
  r = rem_assign_r(to, to, to_m, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
}

template <typename M, typename T>
inline bool
assign_rem(M& rem, const T& n, const M& div) {
  DIRTY_TEMP(T, divisor);
  DIRTY_TEMP(T, remainder);
  Result r;
  r = assign_r(divisor, div, ROUND_CHECK);
  if (r != V_EQ)
    return false;
  r = rem_assign_r(remainder, n, divisor, ROUND_CHECK);
  if (r != V_EQ)
    return false;
  if (sgn(remainder) < 0) {
    r = add_assign_r(remainder, remainder, divisor, ROUND_CHECK);
    if (r != V_EQ)
      return false;
  }
  r = assign_r(rem, remainder, ROUND_CHECK);
  return r == V_EQ;
}


template <typename T, typename Base, typename From1, typename From2>
inline void
add_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  if (rx.divisor == 0)
    return set_unrestricted(to);
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (ry.divisor == 0)
    return set_unrestricted(to);
  Result r;
  DIRTY_TEMP(T, rem);
  if (is_singleton(x)) {
    if (is_singleton(y))
      return set_integer(to);
    if (!assign_rem(rem, lower(x), ry.divisor))
      return set_integer(to);
    r = assign_r(to.divisor, ry.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    addmod(to.remainder, rem, ry.remainder, to.divisor, ry.divisor);
  }
  else if (is_singleton(y)) {
    if (!assign_rem(rem, lower(y), rx.divisor))
      return set_integer(to);
    r = assign_r(to.divisor, rx.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    addmod(to.remainder, rx.remainder, rem, to.divisor, to.divisor);
  }
  else {
    r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    addmod(to.remainder, rx.remainder, ry.remainder, to.divisor, ry.divisor);
  }
}

template <typename T>
inline void
submod(T& to, const T& x, const T& y, const T& to_m, const T& y_m) {
  Result r;
  if (x >= y) {
    r = sub_assign_r(to, x, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
  }
  else {
    r = sub_assign_r(to, y_m, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = add_assign_r(to, x, to, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
  }
  r = rem_assign_r(to, to, to_m, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
}

template <typename T, typename Base, typename From1, typename From2>
inline void
sub_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  if (rx.divisor == 0) {
  unrestricted:
    to.remainder = 0;
    to.divisor = 0;
    return;
  }
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (ry.divisor == 0)
    goto unrestricted;
  Result r;
  DIRTY_TEMP(T, rem);
  if (is_singleton(x)) {
    if (is_singleton(y))
      return set_integer(to);
    if (!assign_rem(rem, lower(x), ry.divisor))
      return set_integer(to);
    r = assign_r(to.divisor, ry.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    submod(to.remainder, rem, ry.remainder, to.divisor, ry.divisor);
  }
  else if (is_singleton(y)) {
    if (!assign_rem(rem, lower(y), rx.divisor))
      return set_integer(to);
    r = assign_r(to.divisor, rx.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    submod(to.remainder, rx.remainder, rem, to.divisor, to.divisor);
  }
  else {
    r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    submod(to.remainder, rx.remainder, ry.remainder, to.divisor, ry.divisor);
  }
}

template <typename T>
inline void
mulmod(T& to, const T& x, const T& y, const T& to_m) {
  Result r;
  if (std::numeric_limits<T>::is_bounded) {
    DIRTY_TEMP(mpz_class, a);
    DIRTY_TEMP(mpz_class, b);
    r = assign_r(a, x, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = assign_r(b, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = mul_assign_r(a, a, b, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = assign_r(b, to_m, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = rem_assign_r(a, a, b, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = assign_r(to, a, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
  }
  else {
    r = mul_assign_r(to, x, y, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    r = rem_assign_r(to, to, to_m, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
  }
}


template <typename T, typename Base, typename From1, typename From2>
inline void
mul_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  if (rx.divisor == 0) {
  unrestricted:
    to.remainder = 0;
    to.divisor = 0;
    return;
  }
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (ry.divisor == 0)
    goto unrestricted;
  Result r;
  DIRTY_TEMP(T, mul);
  if (is_singleton(x)) {
    if (is_singleton(y))
      return set_integer(to);
    DIRTY_TEMP(typename Boundary_Value<From1>::type, n);
    r = abs_assign_r(n, lower(x), ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = assign_r(mul, n, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = mul_assign_r(to.remainder, mul, ry.remainder, ROUND_NOT_NEEDED);
    if (r != V_EQ)
      return set_integer(to);
    r = mul_assign_r(to.divisor, mul, ry.divisor, ROUND_NOT_NEEDED);
    if (r != V_EQ)
      return set_integer(to);
  }
  else if (is_singleton(y)) {
    DIRTY_TEMP(typename Boundary_Value<From2>::type, n);
    r = abs_assign_r(n, lower(y), ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = assign_r(mul, n, ROUND_CHECK);
    if (r != V_EQ)
      return set_integer(to);
    r = mul_assign_r(to.remainder, rx.remainder, mul, ROUND_NOT_NEEDED);
    if (r != V_EQ)
      return set_integer(to);
    r = mul_assign_r(to.divisor, rx.divisor, mul, ROUND_NOT_NEEDED);
    if (r != V_EQ)
      return set_integer(to);
  }
  else {
    r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    mulmod(to.remainder, rx.remainder, ry.remainder, to.divisor);
  }
}

template <typename T, typename Base, typename From1, typename From2>
inline void
div_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  if (is_singleton(y)) {
    if (is_singleton(x)) {
    }
  }
  to.remainder = 0;
  to.divisor = 0;
}

template <typename T, typename Base>
inline void
output_restriction(std::ostream& s, const Interval_Restriction_Integer_Modulo<T, Base>& x) {
  if (x.divisor == 1)
    s << "i";
  else if (x.divisor != 0)
    s << "{" << x.remainder << "%" << x.divisor << "}";
}

}

#endif // !defined(PPL_Interval_Info_defs_hh)
