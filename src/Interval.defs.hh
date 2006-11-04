/* Interval class declaration.
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

#ifndef PPL_Interval_defs_hh
#define PPL_Interval_defs_hh 1

#include "Interval.types.hh"
#include "Interval_Info.defs.hh"

namespace Parma_Polyhedra_Library {

using namespace Boundary;
using namespace Interval_;

template <typename Boundary, typename Info>
class Interval : private Info {
private:
  Info& w_info() const {
    return const_cast<Interval&>(*this);
  }
public:
  typedef Interval_::Property Property;
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
    if (info().test_interval_property(CARDINALITY_IS))
      return info().test_interval_property(CARDINALITY_0);
    else if (info().test_interval_property(CARDINALITY_0))
      return false;
    if (lt(UPPER, upper_, info(), LOWER, lower_, info())) {
      w_info().set_interval_property(CARDINALITY_IS);
      w_info().set_interval_property(CARDINALITY_0);
      w_info().set_interval_property(CARDINALITY_1, false);
      return true;
    }
    else {
      w_info().set_interval_property(CARDINALITY_0);
      return false;
    }
  }
  bool is_singleton() const {
    if (info().test_interval_property(CARDINALITY_IS))
      return info().test_interval_property(CARDINALITY_1);
    else if (info().test_interval_property(CARDINALITY_1))
      return false;
    if (eq(LOWER, lower_, info(), UPPER, upper_, info())) {
      w_info().set_interval_property(CARDINALITY_IS);
      w_info().set_interval_property(CARDINALITY_0, false);
      w_info().set_interval_property(CARDINALITY_1);
      return true;
    }
    else {
      w_info().set_interval_property(CARDINALITY_1);
      return false;
    }
  }
  bool is_integer() const {
    if (info().test_interval_property(ONLY_INTEGERS))
      return true;
    if (info().test_interval_property(NOT_ONLY_INTEGERS))
      return false;
    if (is_singleton() && Parma_Polyhedra_Library::is_integer(lower_)) {
      w_info().set_interval_property(ONLY_INTEGERS);
      return true;
    }
    else {
      w_info().set_interval_property(NOT_ONLY_INTEGERS);
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

namespace Interval_ {

template <typename Boundary, typename Info>
inline const Boundary&
lower(const Interval<Boundary, Info>& x) {
  return x.lower();
}
template <typename Boundary, typename Info>
inline const Boundary&
upper(const Interval<Boundary, Info>& x) {
  return x.upper();
}
template <typename Boundary, typename Info>
inline const Info&
info(const Interval<Boundary, Info>& x) {
  return x.info();
}
template <typename Boundary, typename Info>
inline bool
maybe_check_empty(const Interval<Boundary, Info>& x) {
  if (Info::check_empty_args)
    return x.is_empty();
  else {
    assert(x.is_empty());
    return false;
  }
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
lower(const T& x) {
  return x;
}
template <typename T>
inline const T&
upper(const T& x) {
  return x;
}
template <typename T>
inline const Scalar_As_Interval_Info&
info(const T&) {
  return *static_cast<Scalar_As_Interval_Info*>(0);
}
template <typename T>
inline bool
maybe_check_empty(const T& x) {
  if (info(x).check_empty_args)
    return is_not_a_number(x);
  else {
    assert(is_not_a_number(x));
    return false;
  }
}

template <typename T>
inline bool
check_integer(const T& x) {
  // TOTHINK: use the policy for x or for the result?
  if (info(x).check_integer_args)
    return is_integer(x);
  else
    return info(x).test_interval_property(ONLY_INTEGERS);
}

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

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From1& l, const From2& u, bool integer = false) {
  if (maybe_check_empty(l) || maybe_check_empty(u))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign(LOWER, to.lower(), to_info, LOWER, l, info(l));
  I_Result ru = assign(UPPER, to.upper(), to_info, UPPER, u, info(u));
  if (integer)
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = assign(LOWER, to.lower(), to_info,
		       LOWER, lower(x), info(x));
  I_Result ru = assign(UPPER, to.upper(), to_info,
		       UPPER, upper(x), info(x));
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (maybe_check_empty(to))
    return assign(to, x);
  if (maybe_check_empty(x))
    return static_cast<I_Result>(I_L_EQ | I_U_EQ);
  if (To_Info::store_integer) {
    if (info(x).test_interval_property(NOT_ONLY_INTEGERS)) {
      to.info().set_interval_property(NOT_ONLY_INTEGERS);
      to.info().set_interval_property(ONLY_INTEGERS, false);
    }
    else if (to.info().test_interval_property(ONLY_INTEGERS)
	     && !check_integer(x))
      to.info().set_interval_property(ONLY_INTEGERS, false);
  }
  if (to.info().test_interval_property(CARDINALITY_IS)) {
    to.info().set_interval_property(CARDINALITY_IS, false);
    to.info().set_interval_property(CARDINALITY_0, true);
    to.info().set_interval_property(CARDINALITY_1, false);
  }
  I_Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = max_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
convex_hull_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x))
    return assign(to, y);
  if (maybe_check_empty(y))
    return assign(to, x);
  To_Info to_info;
  if (To_Info::store_integer) {
    if (check_integer(x)) {
      if (check_integer(y))
	to_info.set_interval_property(ONLY_INTEGERS);
    }
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS)
	     || info(y).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  I_Result rl, ru;
  rl = min_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = max_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From& x) {
  if (To_Info::store_integer) {
    if (!to.info().test_interval_property(ONLY_INTEGERS)) {
      if (check_integer(x))
	to.info().set_interval_property(ONLY_INTEGERS);
      if (!info(x).test_interval_property(NOT_ONLY_INTEGERS))
	to.info().set_interval_property(NOT_ONLY_INTEGERS, false);
    }
  }
  to.info().set_interval_property(CARDINALITY_IS, false);
  to.info().set_interval_property(CARDINALITY_0, false);
  to.info().set_interval_property(CARDINALITY_1, false);
  I_Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to.info(), LOWER, lower(x), info(x));
  ru = min_assign(UPPER, to.upper(), to.info(), UPPER, upper(x), info(x));
  // FIXME: check empty (policy based)?
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
intersect_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  To_Info to_info;
  if (To_Info::store_integer) {
    if (check_integer(x) || check_integer(y))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS)
	     && info(y).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  I_Result rl, ru;
  rl = max_assign(LOWER, to.lower(), to_info,
		  LOWER, lower(x), info(x),
		  LOWER, lower(y), info(y));
  ru = min_assign(UPPER, to.upper(), to_info,
		  UPPER, upper(x), info(x),
		  UPPER, upper(y), info(y));
  // FIXME: check empty (policy based)?
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename T>
inline I_Result
neg_assign(Interval<To_Boundary, To_Info>& to, const T& x) {
  if (maybe_check_empty(x))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), lower(x))) {
    static To_Boundary to_lower;
    rl = neg_assign(LOWER, to_lower, to_info, UPPER, upper(x), info(x));
    ru = neg_assign(UPPER, to.upper(), to_info, LOWER, lower(x), info(x));
    assign_or_swap(to.lower(), to_lower);
  } else {
    rl = neg_assign(LOWER, to.lower(), to_info, UPPER, upper(x), info(x));
    ru = neg_assign(UPPER, to.upper(), to_info, LOWER, lower(x), info(x));
  }
  if (To_Info::store_integer) {
    if (check_integer(x))
      to_info.set_interval_property(ONLY_INTEGERS);
    else if (info(x).test_interval_property(NOT_ONLY_INTEGERS))
      to_info.set_interval_property(NOT_ONLY_INTEGERS);
  }
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
add_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl = add_assign(LOWER, to.lower(), to_info,
			   LOWER, lower(x), info(x),
			   LOWER, lower(y), info(y));
  I_Result ru = add_assign(UPPER, to.upper(), to_info,
			   UPPER, upper(x), info(x),
			   UPPER, upper(y), info(y));
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename To_Boundary, typename To_Info,
	  typename From1, typename From2>
inline I_Result
sub_assign(Interval<To_Boundary, To_Info>& to, const From1& x, const From2& y) {
  if (maybe_check_empty(x) || maybe_check_empty(y))
    return to.set_empty();
  To_Info to_info;
  I_Result rl, ru;
  // TOTHINK: it's better to avoid the test and use to_lower unconditionally?
  if (same_object(to.lower(), lower(y))) {
    static To_Boundary to_lower;
    rl = sub_assign(LOWER, to_lower, to_info,
		    LOWER, lower(x), info(x),
		    UPPER, upper(y), info(y));
    ru = sub_assign(UPPER, to.upper(), to_info,
		    UPPER, upper(x), info(x),
		    LOWER, lower(y), info(y));
    assign_or_swap(to.lower(), to_lower);
  }
  else {
    rl = sub_assign(LOWER, to.lower(), to_info,
		    LOWER, lower(x), info(x),
		    UPPER, upper(y), info(y));
    ru = sub_assign(UPPER, to.upper(), to_info,
		    UPPER, upper(x), info(x),
		    LOWER, lower(y), info(y));
  }
  if (To_Info::store_integer && check_integer(x) && check_integer(y))
    to_info.set_interval_property(ONLY_INTEGERS);
  to.info() = to_info;
  return static_cast<I_Result>(rl | ru);
}

template <typename Boundary, typename Info>
inline std::ostream&
operator<<(std::ostream& os, const Interval<Boundary, Info>& x) {
  if (x.is_empty())
    return os << "[]";
  if (x.is_singleton())
    return os << x.lower();
  if (x.info().test_boundary_property(LOWER, UNBOUNDED))
    os << "(-inf";
  else {
    os << (x.info().test_boundary_property(LOWER, OPEN) ? "(" : "[");
    os << x.lower();
  }
  os << (x.is_integer() ? " .. " : ", ");
  if (x.info().test_boundary_property(UPPER, UNBOUNDED))
    os << "+inf)";
  else {
    os << x.upper();
    os << (x.info().test_boundary_property(UPPER, OPEN) ? ")" : "]");
  }
  return os;
}

}

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
