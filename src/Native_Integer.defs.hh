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

//! A wrapper for unchecked native integer types.
/*!
  The wrapper and related functions implement an interface which is common
  to all kinds of coefficient types, therefore allowing for a uniform
  coding style.

  \warning
  Native integer coefficients do not check for overflows and therefore
  are likely to produce unreliable results. We are currently using them
  as a tool to estimate the overhead incurred by the <EM>checked</EM>
  integral types.
*/
template <typename T>
class Parma_Polyhedra_Library::Native_Integer {
public:
  //! \name Constructors
  //@{

  //! Default constructor.
  Native_Integer();

#if 0
  // Don't enable this: with GCC, the presence of a copy constructor
  // (even if it is defined exactly as the default one) inhibits some
  // important optimizations.
  //! Copy-constructor.
  Native_Integer(const Native_Integer& y);
#endif

  //! Direct initialization from a signed char value.
  Native_Integer(const signed char y);
  //! Direct initialization from a signed short value.
  Native_Integer(const short y);
  //! Direct initialization from an signed int value.
  Native_Integer(const int y);
  //! Direct initialization from a signed long value.
  Native_Integer(const long y);
  //! Direct initialization from a signed long long value.
  Native_Integer(const long long y);

  //! Direct initialization from an unsigned char value.
  Native_Integer(const unsigned char y);
  //! Direct initialization from an unsigned short value.
  Native_Integer(const unsigned short y);
  //! Direct initialization from an unsigned int value.
  Native_Integer(const unsigned int y);
  //! Direct initialization from an unsigned long value.
  Native_Integer(const unsigned long y);
  //! Direct initialization from an unsigned long long value.
  Native_Integer(const unsigned long long y);

  //! Direct initialization from a 32 bits floating-point value.
  Native_Integer(const float32_t y);
  //! Direct initialization from a 64 bits floating-point value.
  Native_Integer(const float64_t y);
#ifdef FLOAT96_TYPE
  //! Direct initialization from a 96 bits floating-point value.
  Native_Integer(const float96_t y);
#endif
#ifdef FLOAT128_TYPE
  //! Direct initialization from a 128 bits floating-point value.
  Native_Integer(const float128_t y);
#endif

  //! Direct initialization from a GMP unbounded rational value.
  Native_Integer(const mpq_class& y);
  //! Direct initialization from a GMP unbounded integer value.
  Native_Integer(const mpz_class& y);
  //! Direct initialization from a C string value.
  Native_Integer(const char* y);

  //@} // Constructors


  //! \name Accessors and Conversions
  //@{

  //! \brief
  //! Conversion operator:
  //! returns a copy of the undelying native integer value.
  operator T() const;
  //! Returns a reference to the underlying native integer value.
  T& raw_value();
  //! Returns a const reference to the underlying native integer value.
  const T& raw_value() const;

  //@} // Accessors and Conversions


  //! \name Assignment Operators
  //@{

  //! Assignment operator.
  Native_Integer& operator=(const Native_Integer& y);
  //! Add and assign operator.
  Native_Integer& operator+=(const Native_Integer& y);
  //! Subtract and assign operator.
  Native_Integer& operator-=(const Native_Integer& y);
  //! Multiply and assign operator.
  Native_Integer& operator*=(const Native_Integer& y);
  //! Divide and assign operator.
  Native_Integer& operator/=(const Native_Integer& y);
  //! Compute modulus and assign operator.
  Native_Integer& operator%=(const Native_Integer& y);

  //@} // Assignment Operators


  //! \name Increment and Decrement Operators
  //@{

  //! Pre-increment operator.
  Native_Integer& operator++();
  //! Post-increment operator.
  Native_Integer  operator++(int);
  //! Pre-decrement operator.
  Native_Integer& operator--();
  //! Post-decrement operator.
  Native_Integer  operator--(int);

  //@} // Increment and Decrement Operators

private:
  //! The underlying native integer value.
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

//! \name Accessor Functions
//@{

//! Returns a const reference to the underlying native integer value.
/*! \relates Native_Integer */
template <typename T>
const T&
raw_value(const Native_Integer<T>& x);

//! Returns a reference to the underlying native integer value.
/*! \relates Native_Integer */
template <typename T>
T&
raw_value(Native_Integer<T>& x);

//@} // Accessor Functions

//! \name Memory Size Inspection Functions
//@{

//! Returns the total size in bytes of the memory occupied by \p x.
/*! \relates Native_Integer */
template <typename T>
size_t
total_memory_in_bytes(const Native_Integer<T>& x);

//! Returns the size in bytes of the memory managed by \p x.
/*! \relates Native_Integer */
template <typename T>
size_t
external_memory_in_bytes(const Native_Integer<T>& x);

//@} // Memory Size Inspection Functions

//! \name Arithmetic Operators
//@{

//! Unary plus operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator+(const Native_Integer<T>& x);

//! Unary minus operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator-(const Native_Integer<T>& x);

//! Addition operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator+(const Native_Integer<T>& x, const Native_Integer<T>& y);

//! Subtraction operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator-(const Native_Integer<T>& x, const Native_Integer<T>& y);

//! Multiplication operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator*(const Native_Integer<T>& x, const Native_Integer<T>& y);

//! Integer division operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator/(const Native_Integer<T>& x, const Native_Integer<T>& y);

//! Modulus operator.
/*! \relates Native_Integer */
template <typename T>
Native_Integer<T>
operator%(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

//! Assigns to \p x its negation.
/*! \relates Native_Integer */
template <typename T>
void
negate(Native_Integer<T>& x);

//! Assigns to \p x the value <CODE>x + y * z</CODE>.
/*! \relates Native_Integer */
template <typename T>
void
add_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z);

//! Assigns to \p x the value <CODE>x - y * z</CODE>.
/*! \relates Native_Integer */
template <typename T>
void
sub_mul_assign(Native_Integer<T>& x,
	       const Native_Integer<T>& y,
	       const Native_Integer<T>& z);

//! Assigns to \p x the greatest common divisor of \p x and \p y.
/*! \relates Native_Integer */
template <typename T>
void
gcd_assign(Native_Integer<T>& x, const Native_Integer<T>& y);

//! Assigns to \p x the greatest common divisor of \p y and \p z.
/*! \relates Native_Integer */
template <typename T>
void
gcd_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y,
	   const Native_Integer<T>& z);

//! Assigns to \p x the least common multiple of \p x and \p y.
/*! \relates Native_Integer */
template <typename T>
void
lcm_assign(Native_Integer<T>& x, const Native_Integer<T>& y);

//! Assigns to \p x the least common multiple of \p y and \p z.
/*! \relates Native_Integer */
template <typename T>
void
lcm_assign(Native_Integer<T>& x,
	   const Native_Integer<T>& y,
	   const Native_Integer<T>& z);

//! Assigns to \p x the integer division of \p x and \p y.
/*! \relates Native_Integer */
template <typename T>
void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y);

//! Assigns to \p x the integer division of \p y and \p z.
/*! \relates Native_Integer */
template <typename T>
void
exact_div_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y,
		 const Native_Integer<T>& z);

//! Assigns to \p x its integer square root.
/*! \relates Native_Integer */
template <typename T>
void sqrt_assign(Native_Integer<T>& x);

//! Assigns to \p x the integer square root of \p y.
/*! \relates Native_Integer */
template <typename T>
void sqrt_assign(Native_Integer<T>& x,
		 const Native_Integer<T>& y);

//@} // Arithmetic Operators


//! \name Relational Operators and Comparison Functions
//@{

//! Equality operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator==(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

//! Disequality operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator!=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

//! Greater than or equal to operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator>=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

//! Greater than operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator>(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

//! Less than or equal to operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator<=(const Native_Integer<T>& x,
	   const Native_Integer<T>& y);

//! Less than operator.
/*! \relates Native_Integer */
template <typename T>
bool
operator<(const Native_Integer<T>& x,
	  const Native_Integer<T>& y);

//! \brief
//! Returns \f$-1\f$, \f$0\f$ or \f$1\f$ depending on whether the value
//! of \p x is negative, zero or positive, respectively.
/*! \relates Native_Integer */
template <typename T>
int
sgn(const Native_Integer<T>& x);

//! \brief
//! Returns a negative, zero or positive value depending on whether
//! \p x is lower than, equal to or greater than \p y, respectively.
/*! \relates Native_Integer */
template <typename T>
int
cmp(const Native_Integer<T>& x, const Native_Integer<T>& y);

//@} // Relational Operators and Comparison Functions

//! \name Input-Output Operators
//@{

//! Output operator.
/*! \relates Native_Integer */
template <typename T>
std::ostream&
operator<<(std::ostream& os, const Native_Integer<T>& x);

//! Input operator.
/*! \relates Native_Integer */
template <typename T>
std::istream&
operator>>(std::istream& is, Native_Integer<T>& x);

//@} // Input-Output Operators

PPL_INTEGER_DECLARE_NON_MEMBERS(Native_Integer)

} // namespace Parma_Polyhedra_Library

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::numeric_limits</CODE>.
/*!
  The Native_Integer<T> wrapper shares the same limits of the
  underlying native integer type T.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
class numeric_limits<Parma_Polyhedra_Library::Native_Integer<T> >
  : public numeric_limits<T> {
public:
  static const bool is_specialized = false;
};

} // namespace std

#include "Native_Integer.inlines.hh"

#endif // !defined(PPL_Native_Integer_defs_hh)
