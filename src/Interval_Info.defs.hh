
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

template <typename Policy>
class Interval_Info_Null {
public:
  const_bool_nodef(handle_infinity, Policy::handle_infinity);
  const_bool_nodef(check_inexact, Policy::check_inexact);
  const_bool_nodef(check_empty_args, Policy::check_empty_args);
  const_bool_nodef(check_integer_args, Policy::check_integer_args);
  const_bool_nodef(infinity_is_open, Policy::infinity_is_open);
  const_bool_nodef(store_special, false);
  const_bool_nodef(store_open, false);
  const_bool_nodef(store_integer, false);
  const_bool_nodef(store_empty, false);
  const_bool_nodef(store_singleton, false);
  void clear() {
  }
  void clear_boundary_properties(Boundary_NS::Type) {
  }

  template <typename Property>
  void set_boundary_property(Boundary_NS::Type, const Property&, typename Property::Value = Property::default_value) {
  }
  template <typename Property>
  typename Property::Value get_boundary_property(Boundary_NS::Type, const Property&) const {
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
  const_bool_nodef(handle_infinity, Policy::handle_infinity);
  const_bool_nodef(check_inexact, Policy::check_inexact);
  const_bool_nodef(check_empty_args, Policy::check_empty_args);
  const_bool_nodef(check_integer_args, Policy::check_integer_args);
  const_bool_nodef(infinity_is_open, Policy::infinity_is_open);
  const_bool_nodef(store_special, Policy::store_special);
  const_bool_nodef(store_open, Policy::store_open);
  const_bool_nodef(store_integer, Policy::store_integer);
  const_bool_nodef(store_empty, Policy::store_empty);
  const_bool_nodef(store_singleton, Policy::store_singleton);
  const_int_nodef(lower_infinity_bit, Policy::next_bit);
  const_int_nodef(lower_open_bit, lower_infinity_bit + store_special);
  const_int_nodef(upper_infinity_bit, lower_open_bit + store_open);
  const_int_nodef(upper_open_bit, upper_infinity_bit + store_special);
  const_int_nodef(integer_bit, upper_open_bit + store_open);
  const_int_nodef(cardinality_0_bit, integer_bit + store_integer * 2);
  const_int_nodef(cardinality_1_bit, cardinality_0_bit + store_empty);
  const_int_nodef(cardinality_is_bit, cardinality_1_bit + store_singleton);
  const_int_nodef(next_bit, cardinality_is_bit + (store_empty || store_singleton));
  Interval_Info_Bitset() {
    init_bits(bitset);
  }

  void clear() {
    reset_bits(bitset);
  }
  void clear_boundary_properties(Boundary_NS::Type t) {
    set_boundary_property(t, SPECIAL, false);
    set_boundary_property(t, OPEN, false);
  }
  void set_boundary_property(Boundary_NS::Type t, const Boundary_NS::Property& p, bool value = true) {
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
    default:
      break;
    }
  }
  bool get_boundary_property(Boundary_NS::Type t, const Boundary_NS::Property& p) const {
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
      if (t == LOWER)
	return get_bit(bitset, lower_open_bit);
      else
	return get_bit(bitset, upper_open_bit);
    default:
      return false;
    }
  }
  void set_interval_property(const Integer_Property&, Integer_Property::Value value) {
    if (store_integer)
      set_bits(bitset, integer_bit, 2, static_cast<unsigned int>(value));
  }
  Integer_Property::Value get_interval_property(const Integer_Property&) const {
    if (store_integer)
      return static_cast<Integer_Property::Value>(get_bits(bitset, integer_bit, 2));
    else
      return Integer_Property::unsupported_value;
  }
  void set_interval_property(const Interval_NS::Property& p, bool value = true) {
    switch (p.type) {
    case Interval_NS::Property::CARDINALITY_0_:
      if (store_empty)
	set_bit(bitset, cardinality_0_bit, value);
      break;
    case Interval_NS::Property::CARDINALITY_1_:
      if (store_singleton)
	set_bit(bitset, cardinality_1_bit, value);
      break;
    case Interval_NS::Property::CARDINALITY_IS_:
      if (store_empty || store_singleton)
	set_bit(bitset, cardinality_is_bit, value);
      break;
    default:
      break;
    }
  }
  bool get_interval_property(Interval_NS::Property p) const {
    switch (p.type) {
    case Interval_NS::Property::CARDINALITY_0_:
      return store_empty && get_bit(bitset, cardinality_0_bit);
    case Interval_NS::Property::CARDINALITY_1_:
      return store_singleton && get_bit(bitset, cardinality_1_bit);
    case Interval_NS::Property::CARDINALITY_IS_:
      return (store_empty || store_singleton) && get_bit(bitset, cardinality_is_bit);
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
