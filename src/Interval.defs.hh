// -*- compile-command: "g++ -S -O2 -W -Wall -I/home/abramo/bench_modified -I/home/abramo/altnum_modified/ppl/src p.cc" -*-

#include "Checked_Number.defs.hh"

using namespace Parma_Polyhedra_Library;

#define LOWER ROUND_DOWN
#define UPPER ROUND_UP

typedef Rounding_Dir Boundary_Type;

template <typename T>
inline void
init_bits(T& bits) {
  bits = 0;
}

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
    DEFINITELY_NOT_INTEGER,
    DEFINITELY_NOT_EMPTY,
    DEFINITELY_NOT_SINGLETON,
    DEFINITELY_NOT_OTHERWISE,
  };
};

template <typename Policy>
class Interval_Info_Null : public Interval_Info {
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


enum I_Result {
  I_EMPTY = 0,
  I_L_EQ = 1,
  I_L_GT = 2,
  I_L_GE = I_L_GT | I_L_EQ,
  I_U_EQ = 4,
  I_U_LT = 8,
  I_U_LE = I_U_EQ | I_U_LT,
  I_SINGULARITIES = 16
};


template <typename T, typename Policy>
class Interval_Info_Bitset : public Interval_Info {
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
  static const unsigned int definitely_integer_bit = upper_open_bit + store_open;
  static const unsigned int definitely_not_integer_bit = definitely_integer_bit + store_integer;
  static const unsigned int definitely_not_empty_bit = definitely_not_integer_bit + store_integer;
  static const unsigned int definitely_not_singleton_bit = definitely_not_empty_bit + store_empty;
  static const unsigned int definitely_not_otherwise_bit = definitely_not_singleton_bit + store_singleton;
  static const unsigned int next_bit = definitely_not_otherwise_bit + (store_empty || store_singleton);
  Interval_Info_Bitset() {
    init_bits(bitset);
  }
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
    case DEFINITELY_NOT_INTEGER:
      if (store_integer)
	set_bit(bitset, definitely_not_integer_bit);
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
    case DEFINITELY_NOT_INTEGER:
      return store_integer && test_bit(bitset, definitely_not_integer_bit);
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

template <typename Policy, typename T>
inline bool
maybe_check_minus_infinity(const T& v) {
  return Policy::handle_infinity && is_minus_infinity(v);
}

template <typename Policy, typename T>
inline bool
maybe_check_plus_infinity(const T& v) {
  return Policy::handle_infinity && is_plus_infinity(v);
}


template <typename Boundary, typename Info>
class Interval : private Info {
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
  I_Result set_empty() {
    assign_r(lower_, 1, ROUND_NOT_NEEDED);
    assign_r(upper_, 0, ROUND_NOT_NEEDED);
    return I_EMPTY;
  }
  bool is_empty() const {
    if (test_interval_property(Info::DEFINITELY_NOT_EMPTY))
      return false;
    if ((!Info::store_singleton || test_interval_property(Info::DEFINITELY_NOT_SINGLETON))
	&& test_interval_property(Info::DEFINITELY_NOT_OTHERWISE))
      return true;
    if (!test_boundary_property(LOWER, Info::UNBOUNDED) &&
	!test_boundary_property(UPPER, Info::UNBOUNDED) &&
	lower_ > upper_) {
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
    if (test_interval_property(Info::DEFINITELY_NOT_INTEGER))
      return false;
    if (is_singleton() && ::is_integer(lower_)) {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_INTEGER);
      return true;
    }
    else {
      (const_cast<Interval*>(this))->set_interval_property(Info::DEFINITELY_NOT_INTEGER);
      return false;
    }
  }
  Boundary lower_;
  Boundary upper_;
};

template <typename Boundary, typename Info>
inline bool
is_integer(const Interval<Boundary, Info>& x) {
  return x.is_integer();
}


template <typename Info>
inline I_Result adjust_boundary_info(Boundary_Type type, Info& info, Result r) {
  if (type == LOWER) {
    switch (r) {
    case V_NEG_OVERFLOW:
      assert(Info::store_unbounded);
      info.set_boundary_property(type, Info::UNBOUNDED);
      return I_L_GT;
    case V_GT:
      info.set_boundary_property(type, Info::OPEN);
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
      info.set_boundary_property(type, Info::UNBOUNDED);
      return I_U_LT;
    case V_LT:
      info.set_boundary_property(type, Info::OPEN);
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

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
eq_boundary(Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  assert(from1_type == from2_type);
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED))
    return from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED);
  else if (from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED))
      return false;
  if (from1.test_boundary_property(from1_type, From1_Info::OPEN)
      != from2.test_boundary_property(from2_type, From2_Info::OPEN))
    return false;
  return from1 == from2;
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
lt_boundary(Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  assert(from1_type == from2_type);
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED))
      return false;
    else if (from1_type == LOWER)
      return !maybe_check_minus_infinity<From2_Info>(from2);
    else
      return maybe_check_plus_infinity<From2_Info>(from2);
  }
  else if (from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
    if (from2_type == LOWER)
      return maybe_check_minus_infinity<From1_Info>(from1);
    else
      return !maybe_check_plus_infinity<From1_Info>(from1);
  }
  else if (from2_type == LOWER) {
    if (from2.test_boundary_property(from2_type, From2_Info::OPEN))
      if (!from1.test_boundary_property(from1_type, From1_Info::OPEN))
	return from1 <= from2;
  }
  else {
    if (from1.test_boundary_property(from1_type, From1_Info::OPEN))
      if (!from2.test_boundary_property(from2_type, From2_Info::OPEN))
	return from1 <= from2;
  }
  return from1 < from2;
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
gt_boundary(Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return lt_boundary(from2_type, from2, from2_info,
		     from1_type, from1, from1_info);
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
le_boundary(Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return !gt_boundary(from2_type, from2, from2_info,
		      from1_type, from1, from1_info);
}

template <typename From1, typename From1_Info, typename From2, typename From2_Info>
inline bool
ge_boundary(Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
	    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  return !lt_boundary(from2_type, from2, from2_info,
		      from1_type, from1, from1_info);
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline I_Result
assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.test_boundary_property(from_type, From_Info::UNBOUNDED)) {
    set_unbounded(to_type, to, to_info);
    return to_type == LOWER ? I_L_EQ : I_U_EQ;
  }
  if (To_Info::store_open) {
    if (from_info.test_boundary_property(from_type, From_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, assign_r(to, from, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, assign_r(to, from, to_type));
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline I_Result
min_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (lt_boundary(from_type, from, from_info, to_type, to, to_info))
    return assign_boundary(to_type, to, to_info, from_type, from, from_info);
  return to_type == LOWER ? I_L_EQ : I_U_EQ;
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
min_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (lt_boundary(from1_type, from1, from1_info, from2_type, from2, from2_info))
    return assign_boundary(to_type, to, to_info, from1_type, from1, from1_info);
  else
    return assign_boundary(to_type, to, to_info, from2_type, from2, from2_info);
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline I_Result
max_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (gt_boundary(from_type, from, from_info, to_type, to, to_info))
    return assign_boundary(to_type, to, to_info, from_type, from, from_info);
  return to_type == LOWER ? I_L_EQ : I_U_EQ;
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
max_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (gt_boundary(from1_type, from1, from1_info, from2_type, from2, from2_info))
    return assign_boundary(to_type, to, to_info, from1_type, from1, from1_info);
  else
    return assign_boundary(to_type, to, to_info, from2_type, from2, from2_info);
}

template <typename To, typename To_Info, typename From, typename From_Info>
inline I_Result
neg_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from_type, const From& from, const From_Info& from_info) {
  if (from_info.test_boundary_property(from_type, From_Info::UNBOUNDED)) {
    set_unbounded(to_type, to, to_info);
    return to_type == LOWER ? I_L_EQ : I_U_EQ;
  }
  if (To_Info::store_open) {
    if (from_info.test_boundary_property(from_type, From_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, neg_assign_r(to, from, to_type));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
add_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (maybe_check_minus_infinity<From2_Info>(from2))
	goto minf;
      if (maybe_check_plus_infinity<From2_Info>(from2))
	goto pinf;
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
    minf:
      Result r = assign_r(to, MINUS_INFINITY, to_type);
      used(r);
      assert(r == V_EQ);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
    pinf:
      Result r = assign_r(to, PLUS_INFINITY, to_type);
      used(r);
      assert(r == V_EQ);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
    else {
    unbounded:
      set_unbounded(to_type, to, to_info);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.test_boundary_property(from1_type, From1_Info::OPEN) ||
	from2_info.test_boundary_property(from2_type, From2_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, add_assign_r(to, from1, from2, to_type));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
sub_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (maybe_check_minus_infinity<From2_Info>(from2))
	goto pinf;
      if (maybe_check_plus_infinity<From2_Info>(from2))
	goto minf;
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
    minf:
      Result r = assign_r(to, MINUS_INFINITY, to_type);
      used(r);
      assert(r == V_EQ);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
    pinf:
      Result r = assign_r(to, PLUS_INFINITY, to_type);
      used(r);
      assert(r == V_EQ);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
    else {
    unbounded:
      set_unbounded(to_type, to, to_info);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.test_boundary_property(from1_type, From1_Info::OPEN) ||
	from2_info.test_boundary_property(from2_type, From2_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, sub_assign_r(to, from1, from2, to_type));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
mul_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (maybe_check_minus_infinity<From2_Info>(from2)) {
	if (from1_type == LOWER)
	  goto pinf;
	else
	  goto minf;
      }
      if (maybe_check_plus_infinity<From2_Info>(from2)) {
	if (from1_type == LOWER)
	  goto minf;
	else
	  goto pinf;
      }
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER)
	goto pinf;
      else
	goto minf;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER) {
      minf:
	Result r = assign_r(to, MINUS_INFINITY, to_type);
	used(r);
	assert(r == V_EQ);
	return to_type == LOWER ? I_L_EQ : I_U_EQ;
      }
      else {
      pinf:
	Result r = assign_r(to, PLUS_INFINITY, to_type);
	used(r);
	assert(r == V_EQ);
	return to_type == LOWER ? I_L_EQ : I_U_EQ;
      }
    }
    else {
    unbounded:
      set_unbounded(to_type, to, to_info);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.test_boundary_property(from1_type, From1_Info::OPEN) ||
	from2_info.test_boundary_property(from2_type, From2_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, mul_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, mul_assign_r(to, from1, from2, to_type));
}

template <typename To, typename To_Info, typename From1, typename From1_Info, typename From2, typename From2_Info>
inline I_Result
div_assign_boundary(Boundary_Type to_type, To& to, To_Info& to_info,
		    Boundary_Type from1_type, const From1& from1, const From1_Info& from1_info,
		    Boundary_Type from2_type, const From2& from2, const From2_Info& from2_info) {
  if (from1_info.test_boundary_property(from1_type, From1_Info::UNBOUNDED)) {
    if (!from2_info.test_boundary_property(from2_type, From2_Info::UNBOUNDED)) {
      if (maybe_check_minus_infinity<From2_Info>(from2) || 
	  maybe_check_plus_infinity<From2_Info>(from2)) {
	assign_r(to, 0, ROUND_IGNORE);
	return to_type == LOWER ? I_L_EQ : I_U_EQ;
      }
    }
    goto unbounded;
  }
  else if (from2_info.test_boundary_property(from2_type, From1_Info::UNBOUNDED)) {
    if (maybe_check_minus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER)
	goto pinf;
      else
	goto minf;
    }
    else if (maybe_check_plus_infinity<From1_Info>(from1)) {
      if (from2_type == LOWER) {
      minf:
	Result r = assign_r(to, MINUS_INFINITY, to_type);
	used(r);
	assert(r == V_EQ);
	return to_type == LOWER ? I_L_EQ : I_U_EQ;
      }
      else {
      pinf:
	Result r = assign_r(to, PLUS_INFINITY, to_type);
	used(r);
	assert(r == V_EQ);
	return to_type == LOWER ? I_L_EQ : I_U_EQ;
      }
    }
    else {
    unbounded:
      set_unbounded(to_type, to, to_info);
      return to_type == LOWER ? I_L_EQ : I_U_EQ;
    }
  }
  // FIXME: missing singularities check
  // FIXME: invariant open operand gives closed result
  if (To_Info::store_open) {
    if (from1_info.test_boundary_property(from1_type, From1_Info::OPEN) ||
	from2_info.test_boundary_property(from2_type, From2_Info::OPEN))
      to_info.set_boundary_property(to_type, To_Info::OPEN);
    else
      goto check_inexact;
  }
  if (To_Info::check_inexact) {
  check_inexact:
    return adjust_boundary_info(to_type, to_info, div_assign_r(to, from1, from2, static_cast<Rounding_Dir>(to_type | ROUND_FPU_CHECK_INEXACT)));
  }
  else
    return adjust_boundary_info(to_type, to_info, div_assign_r(to, from1, from2, to_type));
}

template <typename Boundary, typename Info>
inline const Boundary&
i_lower(const Interval<Boundary, Info>& x) {
  return x.lower();
}
template <typename Boundary, typename Info>
inline const Boundary&
i_upper(const Interval<Boundary, Info>& x) {
  return x.upper();
}
template <typename Boundary, typename Info>
inline const Info&
i_info(const Interval<Boundary, Info>& x) {
  return x.info();
}
template <typename Boundary, typename Info>
inline bool
i_maybe_check_empty(const Interval<Boundary, Info>& x) {
  return Info::check_empty_args && x.is_empty();
}

struct Scalar_As_Interval_Policy {
  static const bool handle_infinity = false;
  static const bool check_inexact = false;
  static const bool check_empty_args = false;
  static const bool check_integer_args = true;
};

typedef Interval_Info_Null<Scalar_As_Interval_Policy> Scalar_As_Interval_Info;

template <typename T>
inline const T&
i_lower(const T& x) {
  return x;
}
template <typename T>
inline const T&
i_upper(const T& x) {
  return x;
}
template <typename T>
inline const Scalar_As_Interval_Info&
i_info(const T&) {
  return *static_cast<Scalar_As_Interval_Info*>(0);
}
template <typename T>
inline bool
i_maybe_check_empty(const T& x) {
  return i_info(x).check_empty_args
    && is_not_a_number(x);
}

inline bool
i_maybe_check_empty(const char*) {
  return false;
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

// FIXME: add specializations for gmp (others?)
template <typename T>
inline void
assign_or_swap(T& to, const T& from) {
  to = from;
}

template <typename T>
inline bool
check_integer(const T& x) {
  // TOTHINK: use the policy for x or for the result?
  if (i_info(x).check_integer_args)
    return is_integer(x);
  else
    return i_info(x).test_interval_property(i_info(x).DEFINITELY_INTEGER);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From1& l, const From2& u, bool integer = false) {
  if (i_maybe_check_empty(l) || i_maybe_check_empty(u))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign_boundary(LOWER, to.lower(), to_info,
				LOWER, l, i_info(l));
  I_Result ru = assign_boundary(UPPER, to.upper(), to_info,
				UPPER, u, i_info(u));
  if (integer)
    to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (i_maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign_boundary(LOWER, to.lower(), to_info,
				LOWER, i_lower(x), i_info(x));
  I_Result ru = assign_boundary(UPPER, to.upper(), to_info,
				UPPER, i_upper(x), i_info(x));
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
    else if (i_info(x).test_interval_property(i_info(x).DEFINITELY_NOT_INTEGER))
      to_info.set_interval_property(To_Info::DEFINITELY_NOT_INTEGER);
  }
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
union_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (i_maybe_check_empty(x))
    return assign(to, y);
  if (i_maybe_check_empty(y))
    return assign(to, x);
  To_Info to_info;
  if (To_Info::store_integer) {
    if (check_integer(x)) {
      if (check_integer(y))
	to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
    }
    else if (i_info(x).test_interval_property(i_info(x).DEFINITELY_NOT_INTEGER)
	     && i_info(y).test_interval_property(i_info(y).DEFINITELY_NOT_INTEGER))
      to_info.set_interval_property(To_Info::DEFINITELY_NOT_INTEGER);
  }
  I_Result rl, ru;
  rl = min_assign_boundary(LOWER, to.lower(), to_info,
			   LOWER, i_lower(x), i_info(x),
			   LOWER, i_lower(y), i_info(y));
  ru = max_assign_boundary(UPPER, to.upper(), to_info,
			   UPPER, i_upper(x), i_info(x),
			   UPPER, i_upper(y), i_info(y));
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  To_Info to_info;
  if (i_info(x).text_interval_property(i_info(x).DEFINITELY_NOT_INTEGER)) {
    if (check_integer(y))
      goto empty;
    else
      goto not_integer;
  }
  if (i_info(y).text_interval_property(i_info(y).DEFINITELY_NOT_INTEGER)) {
    if (check_integer(x)) {
    empty:
      return to.set_empty();
    }
    else {
    not_integer:
      to_info.set_interval_property(To_Info::DEFINITELY_NOT_INTEGER);
    }
  }
  else if (To_Info::store_integer) {
    if (check_integer(x) || check_integer(y))
      to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
  }
  I_Result rl, ru;
  rl = max_assign_boundary(LOWER, to.lower(), to_info,
			   LOWER, i_lower(x), i_info(x),
			   LOWER, i_lower(y), i_info(y));
  ru = min_assign_boundary(UPPER, to.upper(), to_info,
			   UPPER, i_upper(x), i_info(x),
			   UPPER, i_upper(y), i_info(y));
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename T>
inline I_Result
neg_assign(Interval<To_Boundary, To_Info>& to, const T& x) {
  if (i_maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), i_lower(x))) {
    static To_Boundary to_lower;
    rl = neg_assign_boundary(LOWER, to_lower, to_info,
			     UPPER, i_upper(x), i_info(x));
    ru = neg_assign_boundary(UPPER, to.upper(), to_info,
			     LOWER, i_lower(x), i_info(x));
    assign_or_swap(to.lower(), to_lower);
  } else {
    rl = neg_assign_boundary(LOWER, to.lower(), to_info,
			     UPPER, i_upper(x), i_info(x));
    ru = neg_assign_boundary(UPPER, to.upper(), to_info,
			     LOWER, i_lower(x), i_info(x));
  }
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
    else if (i_info(x).test_interval_property(i_info(x).DEFINITELY_NOT_INTEGER))
      to_info.set_interval_property(To_Info::DEFINITELY_NOT_INTEGER);
  }
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
add_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (i_maybe_check_empty(x) || i_maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = add_assign_boundary(LOWER, to.lower(), to_info,
				    LOWER, i_lower(x), i_info(x),
				    LOWER, i_lower(y), i_info(y));
  I_Result ru = add_assign_boundary(UPPER, to.upper(), to_info,
				    UPPER, i_upper(x), i_info(x),
				    UPPER, i_upper(y), i_info(y));
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
sub_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (i_maybe_check_empty(x) || i_maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), i_lower(y))) {
    static To_Boundary to_lower;
    rl = sub_assign_boundary(LOWER, to_lower, to_info,
			     LOWER, i_lower(x), i_info(x),
			     UPPER, i_upper(y), i_info(y));
    ru = sub_assign_boundary(UPPER, to.upper(), to_info,
			     UPPER, i_upper(x), i_info(x),
			     LOWER, i_lower(y), i_info(y));
    assign_or_swap(to.lower(), to_lower);
  }
  else {
    rl = sub_assign_boundary(LOWER, to.lower(), to_info,
			     LOWER, i_lower(x), i_info(x),
			     UPPER, i_upper(y), i_info(y));
    ru = sub_assign_boundary(UPPER, to.upper(), to_info,
			     UPPER, i_upper(x), i_info(x),
			     LOWER, i_lower(y), i_info(y));
  }
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(To_Info::DEFINITELY_INTEGER);
  to.info() = to_info;
  return static_cast<I_Result> (rl | ru);
}

template <typename Boundary, typename Info>
inline std::ostream&
operator<<(std::ostream& os, const Interval<Boundary, Info>& x) {
  if (x.is_empty()) 
    return os << "[]";
  if (x.is_singleton())
    return os << x.lower();
  if (x.info().test_boundary_property(LOWER, Info::UNBOUNDED))
    os << "(-inf";
  else {
    os << (x.info().test_boundary_property(LOWER, Info::OPEN) ? "(" : "[");
    os << x.lower();
  }
  os << (x.is_integer() ? " .. " : ", ");
  if (x.info().test_boundary_property(UPPER, Info::UNBOUNDED))
    os << "+inf)";
  else {
    os << x.upper();
    os << (x.info().test_boundary_property(UPPER, Info::OPEN) ? ")" : "]");
  }
  return os;
}
