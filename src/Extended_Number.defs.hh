/* Extended_Number class declaration.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Extended_Number_defs_hh
#define PPL_Extended_Number_defs_hh 1

#include "Extended_Number.types.hh"
#include "checked_ext.defs.hh"
#include <gmpxx.h>
#include <iostream>

namespace Parma_Polyhedra_Library {

struct Minus_Infinity {
};

struct Plus_Infinity {
};

struct Not_A_Number {
};

struct Extended_Policy {
  static const int check_overflow = 1;
  static const int check_divbyzero = 1;
  static const int check_sqrt_neg = 1;
  static const int round_inexact = 1;
  static const int store_nan = 1;
  static const int store_infinity = 1;
  static const int fpu_classify = 0;
  static const int fpu_check_inexact = 0;
};

template <typename T, typename Policy>
class Extended_Number {
public:
  //! Default constructor.
  Extended_Number();

  T& raw_value();
  const T& raw_value() const;

  Result classify(bool nan = true, bool inf = true, bool sign = true);

  Result assign(const Minus_Infinity&, const Rounding& mode = Rounding::IGNORE);
  Result assign(const Plus_Infinity&, const Rounding& mode = Rounding::IGNORE);
  Result assign(const Not_A_Number&, const Rounding& mode = Rounding::IGNORE);

#define FUNC1(name) \
  template <typename From> \
  Result name(const From& x, const Rounding& mode = Rounding::IGNORE); \
  template <typename From, typename From_Policy> \
  Result name(const Extended_Number<From, From_Policy>& x, const Rounding& mode = Rounding::IGNORE);

  FUNC1(assign)
  FUNC1(assign_neg)
  FUNC1(assign_abs)
  FUNC1(assign_sqrt)

#undef FUNC1

#define FUNC2(name) \
  template <typename From1, typename From2> \
  Result name(const From1& x, const From2& y, const Rounding& mode = Rounding::IGNORE); \
  template <typename From1, \
	    typename From2, typename From2_Policy> \
  Result name(const From1& x, const Extended_Number<From2, From2_Policy>& y, const Rounding& mode = Rounding::IGNORE); \
  template <typename From1, typename From1_Policy, \
	    typename From2> \
  Result name(const Extended_Number<From1, From1_Policy>& x, const From2& y, const Rounding& mode = Rounding::IGNORE); \
  template <typename From1, typename From1_Policy, \
	    typename From2, typename From2_Policy> \
  Result name(const Extended_Number<From1, From1_Policy>& x, const Extended_Number<From2, From2_Policy>& y, const Rounding& mode = Rounding::IGNORE);

  FUNC2(assign_add)
  FUNC2(assign_sub)
  FUNC2(assign_mul)
  FUNC2(assign_div)
  FUNC2(assign_rem)

#undef FUNC2

  template <typename From>
  Extended_Number(const From& y);

  template <typename From>
  Extended_Number& operator=(const From& y);
  template <typename From>
  Extended_Number& operator+=(const From& y);
  template <typename From>
  Extended_Number& operator-=(const From& y);
  template <typename From>
  Extended_Number& operator*=(const From& y);
  template <typename From>
  Extended_Number& operator/=(const From& y);
  template <typename From>
  Extended_Number& operator%=(const From& y);
  Extended_Number& operator++();
  Extended_Number  operator++(int);
  Extended_Number& operator--();
  Extended_Number  operator--(int);

  //! Swaps \p *this with \p y.
  void swap(Extended_Number& y);

  static void bad_result(Result r);
  static void check_result(Result r);

private:
  T v;
};

extern Minus_Infinity MINUS_INFINITY;
extern Plus_Infinity PLUS_INFINITY;
extern Not_A_Number NOT_A_NUMBER;

template <typename T, typename Policy>
const T&
raw_value(const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
T&
raw_value(Extended_Number<T, Policy>& x);

#if 0
//! Returns the total size in bytes of the memory occupied by \p x.
template <typename T, typename Policy>
size_t
total_memory_in_bytes(const Extended_Number<T, Policy>& x);

//! Returns the size in bytes of the memory managed by \p x.
template <typename T, typename Policy>
size_t
external_memory_in_bytes(const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator+(const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator-(const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator+(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator-(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator*(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator/(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
Extended_Number<T, Policy>
operator%(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator==(const Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator!=(const Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator>=(const Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator>(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator<=(const Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
bool
operator<(const Extended_Number<T, Policy>& x,
	  const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
int
sgn(const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
int
cmp(const Extended_Number<T, Policy>& x, const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
void
negate(Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
void
add_mul_assign(Extended_Number<T, Policy>& x,
	       const Extended_Number<T, Policy>& y,
	       const Extended_Number<T, Policy>& z);

template <typename T, typename Policy>
void
sub_mul_assign(Extended_Number<T, Policy>& x,
	       const Extended_Number<T, Policy>& y,
	       const Extended_Number<T, Policy>& z);

template <typename T, typename Policy>
void
gcd_assign(Extended_Number<T, Policy>& x, const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
void
gcd_assign(Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y,
	   const Extended_Number<T, Policy>& z);

template <typename T, typename Policy>
void
lcm_assign(Extended_Number<T, Policy>& x, const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
void
lcm_assign(Extended_Number<T, Policy>& x,
	   const Extended_Number<T, Policy>& y,
	   const Extended_Number<T, Policy>& z);

template <typename T, typename Policy>
void
exact_div_assign(Extended_Number<T, Policy>& x,
		 const Extended_Number<T, Policy>& y);

template <typename T, typename Policy>
void
exact_div_assign(Extended_Number<T, Policy>& x,
		 const Extended_Number<T, Policy>& y,
		 const Extended_Number<T, Policy>& z);

template <typename T, typename Policy>
void sqrt_assign(Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
void sqrt_assign(Extended_Number<T, Policy>& x,
		 const Extended_Number<T, Policy>& y);

#endif

template <typename T, typename Policy>
std::ostream&
operator<<(std::ostream& os, const Extended_Number<T, Policy>& x);

template <typename T, typename Policy>
std::istream&
operator>>(std::istream& is, Extended_Number<T, Policy>& x);

} // namespace Parma_Polyhedra_Library

#include "Extended_Number.inlines.hh"

#endif // !defined(PPL_Extended_Number_defs_hh)
