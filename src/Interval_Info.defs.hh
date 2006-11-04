
#ifndef PPL_Interval_Info_defs_hh
#define PPL_Interval_Info_defs_hh 1

#include "Boundary.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Interval_ {
enum Property {
  ONLY_INTEGERS,
  NOT_ONLY_INTEGERS,
  CARDINALITY_0,
  CARDINALITY_1,
  CARDINALITY_IS,
};

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
test_bit(const T& bits, unsigned int bit) {
  return bits & (static_cast<T>(1) << bit);
}

}

using namespace Interval_;
using namespace Boundary;

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
  void clear_boundary_properties(Boundary::Type) {
  }
  void set_boundary_property(Boundary::Type, Boundary::Property, bool = true) {
  }
  bool test_boundary_property(Boundary::Type, Boundary::Property) const {
    return false;
  }
  void set_interval_property(Interval_::Property, bool = true) {
  }
  bool test_interval_property(Interval_::Property) const {
    return false;
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
  static const unsigned int only_integers_bit = upper_open_bit + store_open;
  static const unsigned int not_only_integers_bit = only_integers_bit + store_integer;
  static const unsigned int cardinality_0_bit = not_only_integers_bit + store_integer;
  static const unsigned int cardinality_1_bit = cardinality_0_bit + store_empty;
  static const unsigned int cardinality_is_bit = cardinality_1_bit + store_singleton;
  static const unsigned int next_bit = cardinality_is_bit + (store_empty || store_singleton);
  Interval_Info_Bitset() {
    init_bits(bitset);
  }
  void clear() {
    reset_bits(bitset);
  }
  void clear_boundary_properties(Boundary::Type t) {
    if (store_unbounded)
      reset_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit);
    if (store_open)
      reset_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit);
  }
  void set_boundary_property(Boundary::Type t, Boundary::Property p, bool value = true) {
    switch (p) {
    case Boundary::UNBOUNDED:
      if (store_unbounded)
	set_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit, value);
      break;
    case Boundary::OPEN:
      if (store_open)
	set_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit, value);
      break;
    default:
      break;
    }
  }
  bool test_boundary_property(Boundary::Type t, Boundary::Property p) const {
    switch (p) {
    case UNBOUNDED:
      return store_unbounded
	&& test_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit);
    case OPEN:
      return store_open
	&& test_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit);
    default:
      return false;
    }
  }
  void set_interval_property(Interval_::Property p, bool value = true) {
    switch (p) {
    case ONLY_INTEGERS:
      if (store_integer)
	set_bit(bitset, only_integers_bit, value);
      break;
    case NOT_ONLY_INTEGERS:
      if (store_integer)
	set_bit(bitset, not_only_integers_bit, value);
      break;
    case CARDINALITY_0:
      if (store_empty)
	set_bit(bitset, cardinality_0_bit, value);
      break;
    case CARDINALITY_1:
      if (store_singleton)
	set_bit(bitset, cardinality_1_bit, value);
      break;
    case CARDINALITY_IS:
      if (store_empty || store_singleton)
	set_bit(bitset, cardinality_is_bit, value);
      break;
    default:
      break;
    }
  }
  bool test_interval_property(Interval_::Property p) const {
    switch (p) {
    case ONLY_INTEGERS:
      return store_integer && test_bit(bitset, only_integers_bit);
    case NOT_ONLY_INTEGERS:
      return store_integer && test_bit(bitset, not_only_integers_bit);
    case CARDINALITY_0:
      return store_empty && test_bit(bitset, cardinality_0_bit);
    case CARDINALITY_1:
      return store_singleton && test_bit(bitset, cardinality_1_bit);
    case CARDINALITY_IS:
      return (store_empty || store_singleton) && test_bit(bitset, cardinality_is_bit);
    default:
      return false;
    }
  }
protected:
  T bitset;
};

template <typename Info>
inline I_Result
adjust_boundary_info(Boundary::Type type, Info& info, Result r) {
  if (type == LOWER) {
    switch (r) {
    case V_NEG_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, UNBOUNDED);
      return I_L_GT;
    case V_GT:
      info.set_boundary_property(type, OPEN);
      return I_L_GT;
    case V_GE:
      return I_L_GE;
    case V_EQ:
      return I_L_EQ;
    default:
      assert(false);
      return I_EMPTY;
    }
  }
  else {
    switch (r) {
    case V_POS_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, UNBOUNDED);
      return I_U_LT;
    case V_LT:
      info.set_boundary_property(type, OPEN);
      return I_U_LT;
    case V_LE:
      return I_U_LE;
    case V_EQ:
      return I_U_EQ;
    default:
      assert(false);
      return I_EMPTY;
    }
  }
}


}

#endif // !defined(PPL_Interval_Info_defs_hh)
