/* Checked_Number class declaration.
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

#ifndef PPL_Checked_Number_defs_hh
#define PPL_Checked_Number_defs_hh 1

#include "float.types.hh"
#include "Checked_Number.types.hh"
#include "checked.defs.hh"
#include <gmpxx.h>
#include <iostream>

namespace Parma_Polyhedra_Library {

//! A wrapper for native numeric types implementing a given policy.
/*!
  The wrapper and related functions implement an interface which is common
  to all kinds of coefficient types, therefore allowing for a uniform
  coding style. This class also implements the policy encoded by the
  second template parameter. The default policy is to perform the detection
  of overflow errors.
*/
template <typename T, typename Policy>
class Checked_Number {
public:
  //! \name Constructors
  //@{

  //! Default constructor.
  Checked_Number();

#if 0
  // Don't enable this: with GCC, the presence of a copy constructor
  // (even if it is defined exactly as the default one) inhibits some
  // important optimizations.
  //! Copy-constructor.
  Checked_Number(const Checked_Number& y);
#endif

  //! Direct initialization from a signed char value.
  Checked_Number(const signed char y);
  //! Direct initialization from a signed short value.
  Checked_Number(const short y);
  //! Direct initialization from a signed int value.
  Checked_Number(const int y);
  //! Direct initialization from a signed long value.
  Checked_Number(const long y);
  //! Direct initialization from a signed long long value.
  Checked_Number(const long long y);

  //! Direct initialization from an unsigned char value.
  Checked_Number(const unsigned char y);
  //! Direct initialization from an unsigned short value.
  Checked_Number(const unsigned short y);
  //! Direct initialization from an unsigned int value.
  Checked_Number(const unsigned int y);
  //! Direct initialization from an unsigned long value.
  Checked_Number(const unsigned long y);
  //! Direct initialization from an unsigned long long value.
  Checked_Number(const unsigned long long y);

  //! Direct initialization from a 32 bits floating-point value.
  Checked_Number(const float32_t y);
  //! Direct initialization from a 64 bits floating-point value.
  Checked_Number(const float64_t y);
#ifdef FLOAT96_TYPE
  //! Direct initialization from a 96 bits floating-point value.
  Checked_Number(const float96_t y);
#endif
#ifdef FLOAT128_TYPE
  //! Direct initialization from a 128 bits floating-point value.
  Checked_Number(const float128_t y);
#endif

  //! Direct initialization from a GMP unbounded rational value.
  Checked_Number(const mpq_class& y);
  //! Direct initialization from a GMP unbounded integer value.
  Checked_Number(const mpz_class& y);
  //! Direct initialization from a C string value.
  Checked_Number(const c_string y);

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
  Checked_Number& operator=(const Checked_Number& y);
  //! Add and assign operator.
  Checked_Number& operator+=(const Checked_Number& y);
  //! Subtract and assign operator.
  Checked_Number& operator-=(const Checked_Number& y);
  //! Multiply and assign operator.
  Checked_Number& operator*=(const Checked_Number& y);
  //! Divide and assign operator.
  Checked_Number& operator/=(const Checked_Number& y);
  //! Compute modulus and assign operator.
  Checked_Number& operator%=(const Checked_Number& y);

  //@} // Assignment Operators


  //! \name Increment and Decrement Operators
  //@{

  //! Pre-increment operator.
  Checked_Number& operator++();
  //! Post-increment operator.
  Checked_Number  operator++(int);
  //! Pre-decrement operator.
  Checked_Number& operator--();
  //! Post-decrement operator.
  Checked_Number  operator--(int);

  //@} // Increment and Decrement Operators

  //! Swaps \p *this with \p y.
  void swap(Checked_Number& y);

  static void bad_result(Result r);
  static void check_result(Result r);

private:
  //! The underlying native integer value.
  T v;

};

//! \name Accessor Functions
//@{

//! Returns a const reference to the underlying native integer value.
/*! \relates Checked_Number */
template <typename T, typename Policy>
const T&
raw_value(const Checked_Number<T, Policy>& x);

//! Returns a reference to the underlying native integer value.
/*! \relates Checked_Number */
template <typename T, typename Policy>
T&
raw_value(Checked_Number<T, Policy>& x);

//@} // Accessor Functions

//! \name Memory Size Inspection Functions
//@{

//! Returns the total size in bytes of the memory occupied by \p x.
/*! \relates Checked_Number */
template <typename T, typename Policy>
size_t
total_memory_in_bytes(const Checked_Number<T, Policy>& x);

//! Returns the size in bytes of the memory managed by \p x.
/*! \relates Checked_Number */
template <typename T, typename Policy>
size_t
external_memory_in_bytes(const Checked_Number<T, Policy>& x);

//@} // Memory Size Inspection Functions

//! \name Arithmetic Operators
//@{

//! Unary plus operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator+(const Checked_Number<T, Policy>& x);

//! Unary minus operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator-(const Checked_Number<T, Policy>& x);

//! Addition operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator+(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! Subtraction operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator-(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! Multiplication operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator*(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! Integer division operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator/(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! Modulus operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
Checked_Number<T, Policy>
operator%(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);


//! Assigns to \p x its negation.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
negate(Checked_Number<T, Policy>& x);

//! Assigns to \p x the value <CODE>x + y * z</CODE>.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
add_mul_assign(Checked_Number<T, Policy>& x,
	       const Checked_Number<T, Policy>& y,
	       const Checked_Number<T, Policy>& z);

//! Assigns to \p x the value <CODE>x - y * z</CODE>.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
sub_mul_assign(Checked_Number<T, Policy>& x,
	       const Checked_Number<T, Policy>& y,
	       const Checked_Number<T, Policy>& z);

//! Assigns to \p x the greatest common divisor of \p x and \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
gcd_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y);

//! Assigns to \p x the greatest common divisor of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
gcd_assign(Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y,
	   const Checked_Number<T, Policy>& z);

//! Assigns to \p x the least common multiple of \p x and \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
lcm_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y);

//! Assigns to \p x the least common multiple of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
lcm_assign(Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y,
	   const Checked_Number<T, Policy>& z);

//! Assigns to \p x the integer division of \p x and \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
exact_div_assign(Checked_Number<T, Policy>& x,
		 const Checked_Number<T, Policy>& y);

//! Assigns to \p x the integer division of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
exact_div_assign(Checked_Number<T, Policy>& x,
		 const Checked_Number<T, Policy>& y,
		 const Checked_Number<T, Policy>& z);

//! Assigns to \p x its integer square root.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void sqrt_assign(Checked_Number<T, Policy>& x);

//! Assigns to \p x the integer square root of \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void sqrt_assign(Checked_Number<T, Policy>& x,
		 const Checked_Number<T, Policy>& y);

//@} // Arithmetic Operators


//! \name Relational Operators and Comparison Functions
//@{

//! Equality operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator==(const Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y);

//! Disequality operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator!=(const Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y);

//! Greater than or equal to operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator>=(const Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y);

//! Greater than operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator>(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! Less than or equal to operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator<=(const Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y);

//! Less than operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
bool
operator<(const Checked_Number<T, Policy>& x,
	  const Checked_Number<T, Policy>& y);

//! \brief
//! Returns \f$-1\f$, \f$0\f$ or \f$1\f$ depending on whether the value
//! of \p x is negative, zero or positive, respectively.
/*! \relates Checked_Number */
template <typename T, typename Policy>
int
sgn(const Checked_Number<T, Policy>& x);

//! \brief
//! Returns a negative, zero or positive value depending on whether
//! \p x is lower than, equal to or greater than \p y, respectively.
/*! \relates Checked_Number */
template <typename T, typename Policy>
int
cmp(const Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y);

//@} // Relational Operators and Comparison Functions

//! \name Input-Output Operators
//@{

//! Output operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x);

//! Input operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
std::istream&
operator>>(std::istream& is, Checked_Number<T, Policy>& x);

//@} // Input-Output Operators

} // namespace Parma_Polyhedra_Library

#include "Checked_Number.inlines.hh"

#endif // !defined(PPL_Checked_Number_defs_hh)
