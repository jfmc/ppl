
#ifndef PPL_Interval_Info_defs_hh
#define PPL_Interval_Info_defs_hh 1


#include "Boundary.defs.hh"


namespace Parma_Polyhedra_Library {

namespace Interval_NS {

struct Property {
  enum Type {
    CARDINALITY_0_,
    CARDINALITY_1_,
    CARDINALITY_IS_
  };
  typedef bool Value;
  static const Value default_value = true;
  static const Value unsupported_value = false;
  Property(Type t)
    : type(t) {
  }
  Type type;
};

const Property CARDINALITY_0(Property::CARDINALITY_0_);
const Property CARDINALITY_1(Property::CARDINALITY_1_);
const Property CARDINALITY_IS(Property::CARDINALITY_IS_);

template <typename T>
inline void
init_bits(T& bits) {
  bits = 0;
}

template <typename T>
inline void
reset_bits(T& bits) {
  bits = 0;
}

template <typename T>
inline void
reset_bit(T& bits, unsigned int bit) {
  bits &= ~(static_cast<T>(1) << bit);
}

template <typename T>
inline void
set_bit(T& bits, unsigned int bit, bool value) {
  if (value)
    bits |= static_cast<T>(1) << bit;
  else
    reset_bit(bits, bit);
}

template <typename T>
inline bool
get_bit(const T& bits, unsigned int bit) {
  return bits & (static_cast<T>(1) << bit);
}

template <typename T>
inline void
set_bits(T& bits, unsigned int start, unsigned int len, T value) {
  bits &= ~(((static_cast<T>(1) << len) - 1) << start);
  bits |= value << start;
}

template <typename T>
inline T
get_bits(T& bits, unsigned int start, unsigned int len) {
  return (bits >> start) & ((static_cast<T>(1) << len) - 1);
}

}

using namespace Interval_NS;
using namespace Boundary_NS;

class Interval_No_Restrictions_Base {
public:
  bool has_restrictions() const {
    return false;
  }
  void normalize() const {
  }
  template <typename T>
  bool is_restricted(const T&) const {
    return false;
  }
  template <typename T>
  Result restrict(T&, Result) const {
    return V_EQ;
  }
};

inline bool
eq_restrictions(const Interval_No_Restrictions_Base&, const Interval_No_Restrictions_Base) {
  return true;
}
template <typename T>
inline bool
contains_restrictions(const Interval_No_Restrictions_Base&, const T&) {
  return true;
}
template <typename T>
inline void
assign_restrictions(Interval_No_Restrictions_Base&, const T&) {
}
template <typename T1, typename T2>
inline void
convex_hull_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
template <typename T1, typename T2>
inline void
intersect_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
template <typename T>
inline void
neg_restrictions(Interval_No_Restrictions_Base&, const T&) {
}
template <typename T1, typename T2>
inline void
add_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
template <typename T1, typename T2>
inline void
sub_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
template <typename T1, typename T2>
inline void
mul_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
template <typename T1, typename T2>
inline void
div_restrictions(Interval_No_Restrictions_Base&, const T1&, const T2&) {
}
inline void
output_restrictions(std::ostream&, const Interval_No_Restrictions_Base&) {
}

template <typename Base>
class Interval_No_Restrictions : public Interval_No_Restrictions_Base,
				 public Base {
};

class Interval_Integer_Base {
};

template <typename Base>
class Interval_Integer : public Interval_Integer_Base, public Base {
public:
  void set_integer(bool v = true) {
    return set_bit(Base::bitset, integer_bit, v);
  }
  bool get_integer() const {
    return get_bit(Base::bitset, integer_bit);
  }

  const_int_nodef(integer_bit, Base::next_bit);
  const_int_nodef(next_bit, integer_bit + 1);
  bool has_restrictions() const {
    return get_integer();
  }
  void normalize() const {
  }
  template <typename T>
  bool is_restricted(const T& x) const {
    return is_integer(x);
  }
  template <typename T>
  Result restrict(T&, Result) const {
    return V_EQ;
  }
};

template <typename T1, typename T2>
inline bool
eq_restrictions(const Interval_Integer<T1>& x,
		const Interval_Integer<T2>& y) {
  return x.get_integer() == y.get_integer();
}
template <typename T1, typename T2>
inline bool
contains_restrictions(const Interval_Integer<T1>& x,
		      const Interval_Integer<T2>& y) {
  return !x.get_integer() || y.get_integer();
}
template <typename To, typename T>
inline void
assign_restrictions(Interval_Integer<To>& to, const T& x) {
  to.set_integer(is_integer(x));
}
template <typename To, typename T1, typename T2>
inline void
convex_hull_restrictions(Interval_Integer<To>& to, const T1& x, const T2& y) {
  to.set_integer(is_integer(x) && is_integer(y));
}
template <typename To, typename T1, typename T2>
inline void
intersect_restrictions(Interval_Integer<To>& to, const T1& x, const T2& y) {
  to.set_integer(is_integer(x) || is_integer(y));

}
template <typename To, typename T>
inline void
neg_restrictions(Interval_Integer<To>& to, const T& x) {
  to.set_integer(is_integer(x));
}
template <typename To, typename T1, typename T2>
inline void
add_restrictions(Interval_Integer<To>& to, const T1& x, const T2& y) {
  to.set_integer(is_integer(x) && is_integer(y));
}
template <typename To, typename T1, typename T2>
inline void
sub_restrictions(Interval_Integer<To>& to, const T1& x, const T2& y) {
  to.set_integer(is_integer(x) && is_integer(y));
}
template <typename To, typename T1, typename T2>
inline void
mul_restrictions(Interval_Integer<To>& to, const T1& x, const T2& y) {
  to.set_integer(is_integer(x) && is_integer(y));
}
template <typename To, typename T1, typename T2>
inline void
div_restrictions(Interval_Integer<To>& to, const T1&, const T2&) {
  to.set_integer(false);
}

template <typename T>
inline void
output_restrictions(std::ostream& s, const Interval_Integer<T>& x) {
  if (x.get_integer())
    s << "i";
}

template <typename Policy>
class Interval_Info_Null {
public:
  const_bool_nodef(may_be_empty, Policy::may_be_empty);
  const_bool_nodef(may_be_infinity, Policy::may_be_infinity);
  const_bool_nodef(check_empty_result, Policy::check_empty_result);
  const_bool_nodef(check_inexact, Policy::check_inexact);
  const_bool_nodef(infinity_is_open, Policy::infinity_is_open);
  const_bool_nodef(store_special, false);
  const_bool_nodef(store_open, false);
  const_bool_nodef(cache_normalized, false);
  const_bool_nodef(cache_empty, false);
  const_bool_nodef(cache_singleton, false);
  void clear() {
  }
  void clear_boundary_properties(Boundary_Type) {
  }

  template <typename Property>
  void set_boundary_property(Boundary_Type, const Property&, typename Property::Value = Property::default_value) {
  }
  template <typename Property>
  typename Property::Value get_boundary_property(Boundary_Type, const Property&) const {
    return Property::unsupported_value;
  }
  template <typename Property>
  void set_interval_property(const Property&, typename Property::Value = Property::default_value) {
  }
  template <typename Property>
  typename Property::Value get_interval_property(const Property&) const {
    return Property::unsupported_value;
  }

  //! Swaps \p *this with \p y.
  void swap(Interval_Info_Null& y);
};

template <typename T, typename Policy>
class Interval_Info_Bitset {
public:
  const_bool_nodef(may_be_empty, Policy::may_be_empty);
  const_bool_nodef(may_be_infinity, Policy::may_be_infinity);
  const_bool_nodef(check_empty_result, Policy::check_empty_result);
  const_bool_nodef(check_inexact, Policy::check_inexact);
  const_bool_nodef(infinity_is_open, Policy::infinity_is_open);
  const_bool_nodef(store_special, Policy::store_special);
  const_bool_nodef(store_open, Policy::store_open);
  const_bool_nodef(cache_normalized, Policy::cache_normalized);
  const_bool_nodef(cache_empty, Policy::cache_empty);
  const_bool_nodef(cache_singleton, Policy::cache_singleton);
  const_int_nodef(lower_infinity_bit, Policy::next_bit);
  const_int_nodef(lower_open_bit, lower_infinity_bit + store_special);
  const_int_nodef(lower_normalized_bit, lower_open_bit + store_open);
  const_int_nodef(upper_infinity_bit, lower_normalized_bit + cache_normalized);
  const_int_nodef(upper_open_bit, upper_infinity_bit + store_special);
  const_int_nodef(upper_normalized_bit, upper_open_bit + store_open);
  const_int_nodef(cardinality_0_bit, upper_normalized_bit + cache_normalized);
  const_int_nodef(cardinality_1_bit, cardinality_0_bit + cache_empty);
  const_int_nodef(cardinality_is_bit, cardinality_1_bit + cache_singleton);
  const_int_nodef(next_bit, cardinality_is_bit + (cache_empty || cache_singleton));
  Interval_Info_Bitset() {
    init_bits(bitset);
  }

  void clear() {
    reset_bits(bitset);
  }
  void clear_boundary_properties(Boundary_Type t) {
    set_boundary_property(t, SPECIAL, false);
    set_boundary_property(t, OPEN, false);
  }
  void set_boundary_property(Boundary_Type t, const Boundary_NS::Property& p, bool value = true) {
    switch (p.type) {
    case Boundary_NS::Property::SPECIAL_:
      if (store_special) {
	if (t == LOWER)
	  set_bit(bitset, lower_infinity_bit, value);
	else
	  set_bit(bitset, upper_infinity_bit, value);
      }
      break;
    case Boundary_NS::Property::OPEN_:
      if (store_open) {
	if (t == LOWER)
	  set_bit(bitset, lower_open_bit, value);
	else
	  set_bit(bitset, upper_open_bit, value);
      }
      break;
    case Boundary_NS::Property::NORMALIZED_:
      if (cache_normalized) {
	if (t == LOWER)
	  set_bit(bitset, lower_normalized_bit, value);
	else
	  set_bit(bitset, upper_normalized_bit, value);
      }
      break;
    default:
      break;
    }
  }
  bool get_boundary_property(Boundary_Type t, const Boundary_NS::Property& p) const {
    switch (p.type) {
    case Boundary_NS::Property::SPECIAL_:
      if (!store_special)
	return false;
      if (t == LOWER)
	return get_bit(bitset, lower_infinity_bit);
      else
	return get_bit(bitset, upper_infinity_bit);
    case Boundary_NS::Property::OPEN_:
      if (!store_open)
	return false;
      else if (t == LOWER)
	return get_bit(bitset, lower_open_bit);
      else
	return get_bit(bitset, upper_open_bit);
    case Boundary_NS::Property::NORMALIZED_:
      if (!cache_normalized)
	return false;
      else if (t == LOWER)
	return get_bit(bitset, lower_normalized_bit);
      else
	return get_bit(bitset, upper_normalized_bit);
    default:
      return false;
    }
  }
  void set_interval_property(const Interval_NS::Property& p, bool value = true) {
    switch (p.type) {
    case Interval_NS::Property::CARDINALITY_0_:
      if (cache_empty)
	set_bit(bitset, cardinality_0_bit, value);
      break;
    case Interval_NS::Property::CARDINALITY_1_:
      if (cache_singleton)
	set_bit(bitset, cardinality_1_bit, value);
      break;
    case Interval_NS::Property::CARDINALITY_IS_:
      if (cache_empty || cache_singleton)
	set_bit(bitset, cardinality_is_bit, value);
      break;
    default:
      break;
    }
  }
  bool get_interval_property(Interval_NS::Property p) const {
    switch (p.type) {
    case Interval_NS::Property::CARDINALITY_0_:
      return cache_empty && get_bit(bitset, cardinality_0_bit);
    case Interval_NS::Property::CARDINALITY_1_:
      return cache_singleton && get_bit(bitset, cardinality_1_bit);
    case Interval_NS::Property::CARDINALITY_IS_:
      return (cache_empty || cache_singleton) && get_bit(bitset, cardinality_is_bit);
    default:
      return false;
    }
  }

  //! Swaps \p *this with \p y.
  void swap(Interval_Info_Bitset& y);

protected:
  T bitset;
};

}

#include "Interval_Info.inlines.hh"

#endif // !defined(PPL_Interval_Info_defs_hh)
