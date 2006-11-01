// -*- compile-command: "g++ -S -O2 -W -Wall -I/home/abramo/bench_modified -I/home/abramo/altnum_modified/ppl/src p.cc" -*-

#include "Checked_Number.defs.hh"

using namespace Parma_Polyhedra_Library;

#define LOWER ROUND_DOWN
#define UPPER ROUND_UP

typedef Rounding_Dir Boundary_Type;

template <typename T>
inline void
clear_bits(T& bits) {
  bits = 0;
}

template <typename T>
inline void
set_bit(T& bits, unsigned int bit) {
  bits |= static_cast<T>(1) << bit;
}

template <typename T>
inline void
clear_bit(T& bits, unsigned int bit) {
  bits &= ~(static_cast<T>(1) << bit);
}

template <typename T>
inline bool
test_bit(const T& bits, unsigned int bit) {
  return bits & (static_cast<T>(1) << bit);
}

class Interval_Info {
public:
  enum Boundary_Property {
    UNBOUNDED,
    OPEN
  };
  enum Interval_Property {
    DEFINITELY_INTEGER,
    DEFINITELY_FRACTIONAL,
    DEFINITELY_NOT_EMPTY,
    DEFINITELY_NOT_SINGLETON,
    DEFINITELY_NOT_OTHERWISE,
  };
};

class Interval_Info_Null : public Interval_Info {
public:
  static const bool store_unbounded = false;
  static const bool store_open = false;
  static const bool store_integer = false;
  static const bool store_empty = false;
  static const bool store_singleton = false;
  void clear() {
  }
  void set_boundary_property(Boundary_Type, Boundary_Property) {
  }
  bool test_boundary_property(Boundary_Type, Boundary_Property) const {
    return false;
  }
  void set_interval_property(Interval_Property) {
  }
  bool test_interval_property(Interval_Property) const {
    return false;
  }
};


template <typename T, typename Policy>
class Interval_Info_Bitset : public Interval_Info {
public:
  static const bool store_unbounded = Policy::store_unbounded;
  static const bool store_open = Policy::store_open;
  static const bool store_integer = Policy::store_integer;
  static const bool store_empty = Policy::store_empty;
  static const bool store_singleton = Policy::store_empty;
  static const unsigned int lower_unbounded_bit = Policy::next_bit;
  static const unsigned int lower_open_bit = lower_unbounded_bit + store_unbounded;
  static const unsigned int upper_unbounded_bit = lower_open_bit + store_open;
  static const unsigned int upper_open_bit = upper_unbounded_bit + store_unbounded;
  static const unsigned int definitely_integer_bit = upper_open_bit + store_open;
  static const unsigned int definitely_fractional_bit = definitely_integer_bit + store_integer;
  static const unsigned int definitely_not_empty_bit = definitely_fractional_bit + store_integer;
  static const unsigned int definitely_not_singleton_bit = definitely_not_empty_bit + store_empty;
  static const unsigned int definitely_not_otherwise_bit = definitely_not_singleton_bit + store_singleton;
  static const unsigned int next_bit = definitely_not_otherwise_bit + (store_empty || store_singleton);
  void clear() {
    clear_bits(bitset);
  }
  void set_boundary_property(Boundary_Type t, Boundary_Property p) {
    switch (p) {
    case UNBOUNDED:
      if (store_unbounded)
	set_bit(bitset, t == LOWER ? lower_unbounded_bit : upper_unbounded_bit);
      break;
    case OPEN:
      if (store_open)
	set_bit(bitset, t == LOWER ? lower_open_bit : upper_open_bit);
      break;
    default:
      break;
    }
  }
  bool test_boundary_property(Boundary_Type t, Boundary_Property p) const {
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
  void set_interval_property(Interval_Property p) {
    switch (p) {
    case DEFINITELY_INTEGER:
      if (store_integer)
	set_bit(bitset, definitely_integer_bit);
      break;
    case DEFINITELY_FRACTIONAL:
      if (store_integer)
	set_bit(bitset, definitely_fractional_bit);
      break;
    case DEFINITELY_NOT_EMPTY:
      if (store_empty)
	set_bit(bitset, definitely_not_empty_bit);
      break;
    case DEFINITELY_NOT_SINGLETON:
      if (store_singleton)
	set_bit(bitset, definitely_not_singleton_bit);
      break;
    case DEFINITELY_NOT_OTHERWISE:
      if (store_empty || store_singleton)
	set_bit(bitset, definitely_not_otherwise_bit);
      break;
    default:
      break;
    }
  }
  bool test_interval_property(Interval_Property p) const {
    switch (p) {
    case DEFINITELY_INTEGER:
      return store_integer && test_bit(bitset, definitely_integer_bit);
    case DEFINITELY_FRACTIONAL:
      return store_integer && test_bit(bitset, definitely_fractional_bit);
    case DEFINITELY_NOT_EMPTY:
      return store_empty && test_bit(bitset, definitely_not_empty_bit);
    case DEFINITELY_NOT_SINGLETON:
      return store_singleton && test_bit(bitset, definitely_not_singleton_bit);
    case DEFINITELY_NOT_OTHERWISE:
      return (store_empty || store_singleton) && test_bit(bitset, definitely_not_otherwise_bit);
    default:
      return false;
    }
  }
protected:
  T bitset;
};

template <typename Boundary, typename Info, typename Policy>
class Interval : public Info {
public:
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
  bool is_empty() const {
    if (test_interval_property(Info::DEFINITELY_NOT_EMPTY))
      return false;
    if ((!Info::store_singleton || test_interval_property(Info::DEFINITELY_NOT_SINGLETON))
	&& test_interval_property(Info::DEFINITELY_NOT_OTHERWISE))
      return true;
    if (!test_boundary_property(LOWER, Info::UNBOUNDED) &&
	!test_boundary_property(UPPER, Info::UNBOUNDED) &&
	gt(lower_, upper_)) {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_SINGLETON);
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_OTHERWISE);
      return true;
    }
    else {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_EMPTY);
      return false;
    }
  }
  bool is_singleton() const {
    if (test_interval_property(Info::DEFINITELY_NOT_SINGLETON))
      return false;
    if ((!Info::store_empty || test_interval_property(Info::DEFINITELY_NOT_EMPTY))
	&& test_interval_property(Info::DEFINITELY_NOT_OTHERWISE))
      return true;
    if (!test_boundary_property(LOWER, Info::UNBOUNDED) &&
	!test_boundary_property(UPPER, Info::UNBOUNDED) &&
	lower_ == upper_) {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_EMPTY);
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_OTHERWISE);
      return true;
    }
    else {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_SINGLETON);
      return false;
    }
  }
  bool is_integer() const {
    if (test_interval_property(Info::DEFINITELY_INTEGER))
      return true;
    if (test_interval_property(Info::DEFINITELY_FRACTIONAL))
      return false;
    if (is_singleton() && ::is_integer(lower_)) {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_INTEGER);
      return true;
    }
    else {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_FRACTIONAL);
      return false;
    }
  }
  Boundary lower_;
  Boundary upper_;
};

template <typename Info>
inline void adjust_boundary_info(Boundary_Type type, Info& info, Result r) {
  if (type == LOWER) {
    switch (r) {
    case V_NEG_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, Info::UNBOUNDED);
      break;
    case V_GT:
      info.set_boundary_property(type, Info::OPEN);
      break;
    case V_GE:
    case V_EQ:
      break;
    default:
      assert(false);
      break;
    }
  }
  else {
    switch (r) {
    case V_POS_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, Info::UNBOUNDED);
      break;
    case V_LT:
      info.set_boundary_property(type, Info::OPEN);
      break;
    case V_LE:
    case V_EQ:
      break;
    default:
      assert(false);
      break;
    }
  }
}

template <typename To, typename To_Info>
inline void
set_unbounded(Boundary_Type to_type, To& to, To_Info& to_info) {
  if (To_Info::store_unbounded)
    to_info.set_boundary_property(to_type, To_Info::UNBOUNDED);
  else {
    Result r;
    if (to_type == LOWER) 
      r = assign_r(to, MINUS_INFINITY, to_type);
    else
      r = assign_r(to, PLUS_INFINITY, to_type);
    assert(r == V_EQ);
    to_info.set_boundary_property(to_type, To_Info::OPEN);
  }
}

template <typename T, typename Info>
inline void
sign_boundary(Boundary_Type type, T v, const Info& info) {
  if (info.test_boundary_property(type, Info::UNBOUNDED))
    return type == LOWER ? -1 : 1;
  int sign = sgn(v);
  if (v == 0 && info.test_boundary_property(type, Info::OPEN))
    return type == LOWER ? -1 : 1;
  return sign;
}


template <typename To, typename To_Info, typename From, typename From_Info>
inline void
assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.test_boundary_property(from_type, From_Info::UNBOUNDED))
    return set_unbounded(to_type, to, to_info);
  if (To_Info::store_open
      && !from_info.test_boundary_property(from_type, From_Info::OPEN))
    adjust_boundary_info(to_type, to_info, assign_r(to, from, to_type | ROUND_FPU_CHECK_INEXACT));
  else {
    adjust_boundary_info(to_type, to_info, assign_r(to, from, to_type));
    if (To_Info::store_open)
      to_info.set_boundary_property(to_type, To_Info::OPEN);
  }
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline void
neg_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
	     Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.test_boundary_property(from_type, From_Info::UNBOUNDED))
    return set_unbounded(to_type, to, to_info);
  if (To_Info::store_open
      && !from_info.test_boundary_property(from_type, From_Info::OPEN))
    adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  else {
    adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, to_type));
    if (To_Info::store_open)
      to_info.set_boundary_property(to_type, To_Info::OPEN);
  }
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline void
add_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
	     Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	     Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (is_minus_infinity(from2))
	goto minf;
      if (is_plus_infinity(from2))
	goto pinf;
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (is_minus_infinity(from1)) {
    minf:
      Result r = assign_r(to, MINUS_INFINITY, to_type);
      assert(r == V_EQ);
      return;
    }
    else if (is_plus_infinity(from1)) {
    pinf:
      Result r = assign_r(to, PLUS_INFINITY, to_type);
      assert(r == V_EQ);
      return;
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  if (To_Info::store_open 
      && !from1_info.test_boundary_property(from1_type, From1_Info::OPEN)
      && !from2_info.test_boundary_property(from2_type, From2_Info::OPEN)) {
    adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else {
    adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, to_type));
    if (To_Info::store_open)
      to_info.set_boundary_property(to_type, To_Info::OPEN);
  }
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline void
sub_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
	     Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	     Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (is_minus_infinity(from2))
	goto pinf;
      if (is_plus_infinity(from2))
	goto minf;
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (is_minus_infinity(from1)) {
    minf:
      Result r = assign_r(to, MINUS_INFINITY, to_type);
      assert(r == V_EQ);
      return;
    }
    else if (is_plus_infinity(from1)) {
    pinf:
      Result r = assign_r(to, PLUS_INFINITY, to_type);
      assert(r == V_EQ);
      return;
    }
    else {
    unbounded:
      return set_unbounded(to_type, to, to_info);
    }
  }
  if (To_Info::store_open 
      && !from1_info.test_boundary_property(from1_type, From1_Info::OPEN)
      && !from2_info.test_boundary_property(from2_type, From2_Info::OPEN)) {
    adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else {
    adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, to_type));
    if (To_Info::store_open)
      to_info.set_boundary_property(to_type, To_Info::OPEN);
  }
}

