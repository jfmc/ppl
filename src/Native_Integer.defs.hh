/* Native_Integer class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Native_Integer_defs_hh
#define PPL_Native_Integer_defs_hh 1

#include "Native_Integer.types.hh"
#include "float.types.hh"
#include <gmpxx.h>
#include <limits>


template <typename T>
class Parma_Polyhedra_Library::Native_Integer {
public:
  //! Default constructor.
  Native_Integer();

#if 0
  // Don't enable this: with GCC, the presence of a copy constructor
  // (even if it is defined exactly as the default one) inhibits some
  // important optimizations.
  //! Copy-constructor.
  Native_Integer(const Native_Integer& y);
#endif

  Native_Integer(const signed char y);
  Native_Integer(const short y);
  Native_Integer(const int y);
  Native_Integer(const long y);
  Native_Integer(const long long y);

  Native_Integer(const unsigned char y);
  Native_Integer(const unsigned short y);
  Native_Integer(const unsigned int y);
  Native_Integer(const unsigned long y);
  Native_Integer(const unsigned long long y);
  Native_Integer(const float32_t y);
  Native_Integer(const float64_t y);
#ifdef FLOAT96_TYPE
  Native_Integer(const float96_t y);
#endif
#ifdef FLOAT128_TYPE
  Native_Integer(const float128_t y);
#endif
  Native_Integer(const mpq_class& y);
  Native_Integer(const mpz_class& y);
  Native_Integer(const char* y);

  operator T() const;
  T& raw_value();
  const T& raw_value() const;

  Native_Integer& operator=(const Native_Integer& y);
  Native_Integer& operator+=(const Native_Integer& y);
  Native_Integer& operator-=(const Native_Integer& y);
  Native_Integer& operator*=(const Native_Integer& y);
  Native_Integer& operator/=(const Native_Integer& y);
  Native_Integer& operator%=(const Native_Integer& y);
  Native_Integer& operator++();
  Native_Integer  operator++(int);
  Native_Integer& operator--();
  Native_Integer  operator--(int);

private:
  T v;
};

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, type)        \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(cl<T> x, type y);                                                          \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(type x, cl<T> y);

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, op)                        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed char)         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned char)       \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed short)        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned short)      \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed int)          \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned int)        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long)         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long)       \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long long)    \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATORS(cl)                           \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator+)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator-)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator*)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator/)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator%)

#define PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, type)               \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(cl<T> x, type y);                                                          \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(type x, cl<T> y);

#define PPL_DECLARE_RELATIONAL_OPERATOR(cl, op)                               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed char)                \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned char)              \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed short)               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned short)             \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed int)                 \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned int)               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long)                \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long)              \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long long)           \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DECLARE_RELATIONAL_OPERATORS(cl)                                  \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator==)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator!=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator<=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator<)                                \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator>=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator>)

#define PPL_INTEGER_DECLARE_NON_MEMBERS(cl)                                   \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATORS(cl)                                   \
PPL_DECLARE_RELATIONAL_OPERATORS(cl)


#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, type)         \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(cl<T> x, type y) {                                                         \
  return op(x, cl<T>(y));                                                     \
}                                                                             \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(type x, cl<T> y) {                                                         \
  return op(cl<T>(x), y);                                                     \
}

#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, op)                         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed char)          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned char)        \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed short)         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned short)       \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed int)           \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned int)         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long)          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long)        \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long long)     \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATORS(cl)                            \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator+)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator-)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator*)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator/)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator%)

#define PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, type)                \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(cl<T> x, type y) {                                                         \
  return op(x, cl<T>(y));                                                     \
}                                                                             \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(type x, cl<T> y) {                                                         \
  return op(cl<T>(x), y);                                                     \
}

#define PPL_DEFINE_RELATIONAL_OPERATOR(cl, op)                                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed char)                 \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned char)               \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed short)                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned short)              \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed int)                  \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned int)                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long)                 \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long)               \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long long)            \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DEFINE_RELATIONAL_OPERATORS(cl)                                   \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator==)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator!=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator<=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator<)                                 \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator>=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator>)

#define PPL_INTEGER_DEFINE_NON_MEMBERS(cl)                                    \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATORS(cl)                                    \
PPL_DEFINE_RELATIONAL_OPERATORS(cl)

namespace Parma_Polyhedra_Library {

template <typename T>
const T&
raw_value(const Native_Integer<T>& x);

template <typename T>
T&
raw_value(Native_Integer<T>& x);

//! Returns the total size in bytes of the memory occupied by \p x.
template <typename T>
size_t
total_memory_in_bytes(const Native_Integer<T>& x);

//! Returns the size in bytes of the memory managed by \p x.
template <typename T>
size_t
external_memory_in_bytes(const Native_Integer<T>& x);

template <typename T>
Native_Integer<T>
operator+(const Native_Integer<T>& x);

template <typename T>
Native_Integer<T>
operator-(const Native_Integer<T>& x);

template <typename T>
Native_Integer<T>
operator+(const Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
Native_Integer<T>
operator-(const Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
Native_Integer<T>
operator*(const Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
Native_Integer<T>
operator/(const Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
Native_Integer<T>
operator%(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

template <typename T>
bool
operator==(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

template <typename T>
bool
operator!=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

template <typename T>
bool
operator>=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

template <typename T>
bool
operator>(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

template <typename T>
bool
operator<=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

template <typename T>
bool
operator<(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

template <typename T>
int
sgn(const Native_Integer<T>& x);

template <typename T>
int
cmp(const Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
void
negate(Native_Integer<T>& x);

template <typename T>
void
add_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z);

template <typename T>
void
sub_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z);

template <typename T>
void
gcd_assign(Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
void
gcd_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y,
	   const Native_Integer<T>& z);

template <typename T>
void
lcm_assign(Native_Integer<T>& x, const Native_Integer<T>& y);

template <typename T>
void
lcm_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y,
	   const Native_Integer<T>& z);

template <typename T>
void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y);

template <typename T>
void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y,
		 const Native_Integer<T>& z);

template <typename T>
void sqrt_assign(Native_Integer<T>& x);

template <typename T>
void sqrt_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y);

template <typename T>
std::ostream&
operator<<(std::ostream& os, const Native_Integer<T>& x);

template <typename T>
std::istream&
operator>>(std::istream& is, Native_Integer<T>& x);

PPL_INTEGER_DECLARE_NON_MEMBERS(Native_Integer)

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::numeric_limits</CODE>.
template <typename T>
class numeric_limits<Parma_Polyhedra_Library::Native_Integer<T> >
  : public numeric_limits<T> {
public:
  static const bool is_specialized = false;
};

} // namespace std

#include "Native_Integer.inlines.hh"

#endif // !defined(PPL_Native_Integer_defs_hh)
