
#ifndef PPL_Interval_Info_defs_hh
#define PPL_Interval_Info_defs_hh 1

#include "Boundary.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Interval_NS {

enum Integer_Property_Value {
  MAYBE_SINGLETON_INTEGER = 0,
  NOT_SINGLETON_INTEGER = 1,
  ONLY_INTEGERS_TO_NORMALIZE = 2,
  ONLY_INTEGERS_NORMALIZED = 3
};

struct Integer_Property {
  typedef Integer_Property_Value Value;
  static const Value default_value = MAYBE_SINGLETON_INTEGER;
  static const Value unsupported_value = MAYBE_SINGLETON_INTEGER;
};

static const Integer_Property& INTEGER = *(Integer_Property*)0;


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
  static const bool handle_infinity = Policy::handle_infinity;
  static const bool check_inexact = Policy::check_inexact;
  static const bool check_empty_args = Policy::check_empty_args;
  static const bool check_integer_args = Policy::check_integer_args;
  static const bool store_unbounded = false;
  static const bool store_open = false;
  static const bool store_integer = false;
  static const bool store_empty = false;
  static const bool store_singleton = false;
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
};

template <typename T, typename Policy>
class Interval_Info_Bitset {
public:
  static const bool handle_infinity = Policy::handle_infinity;
  static const bool check_inexact = Policy::check_inexact;
  static const bool check_empty_args = Policy::check_empty_args;
  static const bool check_integer_args = Policy::check_integer_args;
  static const bool store_unbounded = Policy::store_unbounded;
  static const bool store_open = Policy::store_open;
  static const bool store_integer = Policy::store_integer;
  static const bool store_empty = Policy::store_empty;
  static const bool store_singleton = Policy::store_singleton;
  static const unsigned int lower_unbounded_bit = Policy::next_bit;
  static const unsigned int lower_open_bit = lower_unbounded_bit + store_unbounded;
  static const unsigned int upper_unbounded_bit = lower_open_bit + store_open;
  static const unsigned int upper_open_bit = upper_unbounded_bit + store_unbounded;
  static const unsigned int integer_bit = upper_open_bit + store_open;
  static const unsigned int cardinality_0_bit = integer_bit + store_integer * 2;
  static const unsigned int cardinality_1_bit = cardinality_0_bit + store_empty;
  static const unsigned int cardinality_is_bit = cardinality_1_bit + store_singleton;
  static const unsigned int next_bit = cardinality_is_bit + (store_empty || store_singleton);
  Interval_Info_Bitset() {
    init_bits(bitset);
  }
  void clear() {
    reset_bits(bitset);
  }
  void clear_boundary_properties(Boundary_NS::Type t) {
    if (store_unbounded)
      reset_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit);
    if (store_open)
      reset_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit);
  }
  void set_boundary_property(Boundary_NS::Type t, const Boundary_NS::Property& p, bool value = true) {
    switch (p.type) {
    case Boundary_NS::Property::UNBOUNDED_:
      if (store_unbounded)
	set_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit, value);
      break;
    case Boundary_NS::Property::OPEN_:
      if (store_open)
	set_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit, value);
      break;
    default:
      break;
    }
  }
  bool get_boundary_property(Boundary_NS::Type t, const Boundary_NS::Property& p) const {
    switch (p.type) {
    case Boundary_NS::Property::UNBOUNDED_:
      return store_unbounded
	&& get_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit);
    case Boundary_NS::Property::OPEN_:
      return store_open
	&& get_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit);
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
protected:
  T bitset;
};

}

namespace std {

using namespace Parma_Polyhedra_Library;

template <typename Policy>
inline void
swap(Interval_Info_Null<Policy>&, Interval_Info_Null<Policy>&) {
}

template <typename T, typename Policy>
inline void
swap(Interval_Info_Bitset<T, Policy>& x, Interval_Info_Bitset<T, Policy>& y) {
  std::swap(x.bitset, y.bitset);
}

}

#endif // !defined(PPL_Interval_Info_defs_hh)
