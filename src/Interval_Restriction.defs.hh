
#ifndef PPL_Interval_Restriction_defs_hh
#define PPL_Interval_Restriction_defs_hh 1

namespace Parma_Polyhedra_Library {

struct Interval_;

template <typename T, typename Enable = void>
struct Boundary_Value {
  typedef T type;
};

template <typename T>
struct Boundary_Value<T, typename Enable_If<Is_Same_Or_Derived<Interval_, T>::value, void>::type > {
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
  Result restrict(T&, Result) const {
    return V_EQ;
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
};

class Interval_Restriction_Integer_Base {
};

template <typename Base>
class Interval_Restriction_Integer : public Interval_Restriction_Integer_Base, public Base {
public:
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
  Result restrict(T& x, Result r) const {
    if (!has_restriction())
      return V_EQ;
    switch (r) {
    case V_GT:
      if (is_integer(x))
	return add_assign_r(x, x, (T)1, ROUND_DOWN);
      /* Fall through */
    case V_GE:
      return ceil_assign_r(x, x, ROUND_DOWN);
    case V_LT:
      if (is_integer(x))
	sub_assign_r(x, x, (T)1, ROUND_UP);
      /* Fall through */
    case V_LE:
      return floor_assign_r(x, x, ROUND_UP);
    default:
      assert(false);
      return V_EQ;
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
struct Restriction_Integer<From, Base, typename Enable_If<Is_Native_Or_Checked<From>::value, void>::type> {
  typedef Simple_Restriction_Integer type;
  static type get(const From& x) {
    return Simple_Restriction_Integer(is_integer(x));
  }
};

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_None_Base, typename From::info_type>::value, void>::type> {
  typedef Simple_Restriction_Integer type;
  static type get(const From& x) {
    return Simple_Restriction_Integer(x.is_singleton() && is_integer(x.lower()));
  }
};

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, typename From::info_type>::value, void>::type> {
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
  bool has_restriction() const {
    return divisor != 0;
  }
  void normalize() const {
  }
  template <typename V>
  Result restrict(V& x, Result r) const {
    if (!has_restriction())
      return V_EQ;
    static V n;
    static V div;
    r = assign_r(div, divisor, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    assert(r == V_EQ);
    int s;
    r = rem_assign_r(n, x, div, ROUND_NOT_NEEDED);
    assert(r == V_EQ);
    s = sgn(n);
    switch (r) {
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
      return V_EQ;
    }
  }
  typedef T modulo_type;
  T remainder;
  T divisor;
};

template <typename From, typename Base>
struct Restriction_Integer<From, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, typename From::info_type>::value, void>::type> {
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
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Native_Or_Checked<From>::value, void>::type> {
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
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_None_Base, typename From::info_type>::value, void>::type> {
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
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Base, typename From::info_type>::value, void>::type> {
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
struct Restriction_Integer_Modulo<From, T, Base, typename Enable_If<Is_Same_Or_Derived<Interval_Restriction_Integer_Modulo_Base, typename From::info_type>::value, void>::type> {
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
  static typename T1::modulo_type v;
  Result r;
  r = rem_assign_r(v, y.divisor, x.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  if (v != 0)
    return false;
  r = rem_assign_r(v, y.remainder, x.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  return v == x.remainder;
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
  else if (rx.divisor == 1 && ry.divisor == 1
      && is_singleton(x) && is_singleton(y)) {
    static typename Boundary_Value<From1>::type a;
    static typename Boundary_Value<From2>::type b;
    Result r;
    r = abs_assign_r(a, lower(x), ROUND_DIRECT);
    if (r != V_EQ)
      goto integer;
    r = abs_assign_r(b, lower(y), ROUND_DIRECT);
    if (r != V_EQ)
      goto integer;
    if (a > b)
      r = sub_assign_r(a, a, b, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    else
      r = sub_assign_r(a, b, a, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    if (r != V_EQ)
      goto integer;
    r = assign_r(to.divisor, a, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    if (r != V_EQ)
      goto integer;
    r = rem_assign_r(b, b, a, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    if (r != V_EQ)
      goto integer;
    r = assign_r(to.remainder, b, static_cast<Rounding_Dir>(ROUND_DIRECT | ROUND_FPU_CHECK_INEXACT));
    if (r != V_EQ)
      goto integer;
  }
  else if (contains_restriction(rx, ry)) {
    to.remainder = rx.remainder;
    to.divisor = rx.divisor;
  }
  else if (contains_restriction(ry, rx)) {
    to.remainder = ry.remainder;
    to.divisor = rx.divisor;
  }
  else {
  integer:
    to.remainder = 0;
    to.divisor = 1;
  }
}

template <typename T, typename Base, typename From1, typename From2>
inline void
intersect_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
  typedef Restriction_Integer_Modulo<From1, T, Base> Rx;
  const typename Rx::type& rx = Rx::get(x);
  if (rx.divisor == 0)
    return assign_restriction(to, y);
  typedef Restriction_Integer_Modulo<From2, T, Base> Ry;
  const typename Ry::type& ry = Ry::get(y);
  if (ry.divisor == 0)
    return assign_restriction(to, x);
  static T d;
  Result r;
  r = lcm_assign_r(d, rx.divisor, ry.divisor, ROUND_DIRECT);
  if (r != V_EQ) {
    to.remainder = 0;
    to.divisor = 1;
    return;
  }
  to.divisor = d;
  // FIXME: to be completed
}

template <typename T, typename Base, typename From>
inline void
neg_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From& x) {
  to.remainder = Restriction_Integer_Modulo<From, T, Base>::get(x).remainder;
  to.divisor = Restriction_Integer_Modulo<From, T, Base>::get(x).divisor;
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

template <typename T, typename Base, typename From1, typename From2>
inline void
add_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1& x, const From2& y) {
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
  r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  addmod(to.remainder, rx.remainder, ry.remainder, to.divisor, ry.divisor);
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
  r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  submod(to.remainder, rx.remainder, ry.remainder, to.divisor, ry.divisor);
}

template <typename T>
inline void
mulmod(T& to, const T& x, const T& y, const T& to_m) {
  Result r;
  if (std::numeric_limits<T>::is_bounded) {
    static mpz_class a, b;
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
  r = gcd_assign_r(to.divisor, rx.divisor, ry.divisor, ROUND_NOT_NEEDED);
  assert(r == V_EQ);
  mulmod(to.remainder, rx.remainder, ry.remainder, to.divisor);
}

template <typename T, typename Base, typename From1, typename From2>
inline void
div_restriction(Interval_Restriction_Integer_Modulo<T, Base>& to, const From1&, const From2&) {
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
