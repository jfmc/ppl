/* Checked_Number class declaration.
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

#ifndef PPL_Checked_Number_defs_hh
#define PPL_Checked_Number_defs_hh 1

#include "Checked_Number.types.hh"
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Checked_Number_Transparent_Policy {
  //! Checks for overflowed result.
  static const int check_overflow = 0;

  //! Checks for attempts to add infinities with different sign.
  static const int check_inf_add_inf = 0;

  //! Checks for attempts to sub infinities with same sign.
  static const int check_inf_sub_inf = 0;

  //! Checks for attempts to mul infinities by zero.
  static const int check_inf_mul_zero = 0;

  //! Checks for attempts to divide by zero.
  static const int check_div_zero = 0;

  //! Checks for attempts to divide infinities.
  static const int check_inf_div_inf = 0;

  //! Checks for attempts to compute remainder of infinities.
  static const int check_inf_mod = 0;

  //! Checks for attempts to take the square root of a negative number.
  static const int check_sqrt_neg = 0;

  //! Handles not-a-number special value.
  static const int handle_nan = 0;

  //! Handles infinity special values.
  static const int handle_infinity = 0;

  //! Representation is identical to primitive.
  static const int convertible = 1;

  //! Checks for FPU inexact result.
  static const int fpu_check_inexact = 0;

  //! Checks for NaN arguments
  static const int check_nan_args = 0;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_NATIVE;
  static void handle_result(Result r);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Checked_Number_Default_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int handle_nan = 0;
  static const int handle_infinity = 0;
  static const int convertible = 1;
  static const int fpu_check_inexact = 0;
  static const int check_nan_args = 1;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_NATIVE;
  static void handle_result(Result r);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct Extended_Number_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int handle_nan = 1;
  static const int handle_infinity = 1;
  // Do not uncomment the following.
  // The compile time error on conversions is the expected behavior.
  // static const int convertible = 0;
#ifdef DEBUG_ROUND_NOT_NEEDED
  static const int fpu_check_inexact = 1;
#else
  static const int fpu_check_inexact = 0;
#endif
  static const int check_nan_args = 1;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR_INF = ROUND_NOT_NEEDED;
  static const Rounding_Dir ROUND_DEFAULT_ASSIGN_INF = ROUND_NOT_NEEDED;
  // Do not uncomment the following.
  // The compile time error is the expected behavior.
  // static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_UP;
  static void handle_result(Result r);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
struct WRD_Extended_Number_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int handle_nan = 1;
  static const int handle_infinity = 1;
  // Do not uncomment the following.
  // The compile time error on conversions is the expected behavior.
  // static const int convertible = 0;
#ifdef DEBUG_ROUND_NOT_NEEDED
  static const int fpu_check_inexact = 1;
#else
  static const int fpu_check_inexact = 0;
#endif
  static const int check_nan_args = 0;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR_INF = ROUND_NOT_NEEDED;
  static const Rounding_Dir ROUND_DEFAULT_ASSIGN_INF = ROUND_NOT_NEEDED;
  // Do not uncomment the following.
  // The compile time error is the expected behavior.
  // static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_UP;
  static void handle_result(Result r);
};

typedef Checked::Check_Overflow_Policy Default_To_Policy;
typedef Checked_Number_Transparent_Policy Default_From_Policy;

template <typename T>
struct Native_Checked_From_Wrapper {
  typedef Default_From_Policy Policy;
  static const T& raw_value(const T& v) {
    return v;
  }
};

template <typename T, typename P>
struct Native_Checked_From_Wrapper<Checked_Number<T, P> > {
  typedef P Policy;
  static const T& raw_value(const Checked_Number<T, P>& v) {
    return v.raw_value();
  }
};

template <typename T>
struct Native_Checked_To_Wrapper {
  typedef Default_To_Policy Policy;
  static T& raw_value(T& v) {
    return v;
  }
};

template <typename T, typename P>
struct Native_Checked_To_Wrapper<Checked_Number<T, P> > {
  typedef P Policy;
  static T& raw_value(Checked_Number<T, P>& v) {
    return v.raw_value();
  }
};

//! A wrapper for numeric types implementing a given policy.
/*! \ingroup PPL_CXX_interface
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

  //! Copy-constructor.
  Checked_Number(const Checked_Number& y);

  //! Direct initialization from a Checked_Number and rounding mode.
  template <typename From, typename From_Policy>
  Checked_Number(const Checked_Number<From, From_Policy>& y, Rounding_Dir dir);

  //! Direct initialization from a signed char and rounding mode.
  Checked_Number(const signed char y, Rounding_Dir dir);

  //! Direct initialization from a signed short and rounding mode.
  Checked_Number(const signed short y, Rounding_Dir dir);

  //! Direct initialization from a signed int and rounding mode.
  Checked_Number(const signed int y, Rounding_Dir dir);

  //! Direct initialization from a signed long and rounding mode.
  Checked_Number(const signed long y, Rounding_Dir dir);

  //! Direct initialization from a signed long long and rounding mode.
  Checked_Number(const signed long long y, Rounding_Dir dir);

  //! Direct initialization from an unsigned char and rounding mode.
  Checked_Number(const unsigned char y, Rounding_Dir dir);

  //! Direct initialization from an unsigned short and rounding mode.
  Checked_Number(const unsigned short y, Rounding_Dir dir);

  //! Direct initialization from an unsigned int and rounding mode.
  Checked_Number(const unsigned int y, Rounding_Dir dir);

  //! Direct initialization from an unsigned long and rounding mode.
  Checked_Number(const unsigned long y, Rounding_Dir dir);

  //! Direct initialization from an unsigned long long and rounding mode.
  Checked_Number(const unsigned long long y, Rounding_Dir dir);

#if PPL_SUPPORTED_FLOAT
  //! Direct initialization from a float and rounding mode.
  Checked_Number(const float y, Rounding_Dir dir);
#endif

#if PPL_SUPPORTED_DOUBLE
  //! Direct initialization from a double and rounding mode.
  Checked_Number(const double y, Rounding_Dir dir);
#endif

#if PPL_SUPPORTED_LONG_DOUBLE
  //! Direct initialization from a long double and rounding mode.
  Checked_Number(const long double y, Rounding_Dir dir);
#endif

  //! Direct initialization from a rational and rounding mode.
  Checked_Number(const mpq_class& y, Rounding_Dir dir);

  //! Direct initialization from an unbounded integer and rounding mode.
  Checked_Number(const mpz_class& y, Rounding_Dir dir);

  //! Direct initialization from a C string and rounding mode.
  Checked_Number(const char* y, Rounding_Dir dir);

  //! Direct initialization from minus infinity and rounding mode.
  Checked_Number(const Minus_Infinity& y, Rounding_Dir dir);

  //! Direct initialization from plus infinity and rounding mode.
  Checked_Number(const Plus_Infinity& y, Rounding_Dir dir);

  //! Direct initialization from NAN and rounding mode.
  Checked_Number(const Not_A_Number& y, Rounding_Dir dir);

  //! Direct initialization from a Checked_Number, default rounding mode.
  template <typename From, typename From_Policy>
  explicit Checked_Number(const Checked_Number<From, From_Policy>& y);

  //! Direct initialization from a signed char, default rounding mode.
  Checked_Number(const signed char y);

  //! Direct initialization from a signed short, default rounding mode.
  Checked_Number(const signed short y);

  //! Direct initialization from a signed int, default rounding mode.
  Checked_Number(const signed int y);

  //! Direct initialization from a signed long, default rounding mode.
  Checked_Number(const signed long y);

  //! Direct initialization from a signed long long, default rounding mode.
  Checked_Number(const signed long long y);

  //! Direct initialization from an unsigned char, default rounding mode.
  Checked_Number(const unsigned char y);

  //! Direct initialization from an unsigned short, default rounding mode.
  Checked_Number(const unsigned short y);

  //! Direct initialization from an unsigned int, default rounding mode.
  Checked_Number(const unsigned int y);

  //! Direct initialization from an unsigned long, default rounding mode.
  Checked_Number(const unsigned long y);

  //! Direct initialization from an unsigned long long, default rounding mode.
  Checked_Number(const unsigned long long y);

  //! Direct initialization from a float, default rounding mode.
  Checked_Number(const float y);

  //! Direct initialization from a double, default rounding mode.
  Checked_Number(const double y);

  //! Direct initialization from a long double, default rounding mode.
  Checked_Number(const long double y);

  //! Direct initialization from a rational, default rounding mode.
  Checked_Number(const mpq_class& y);

  //! Direct initialization from an unbounded integer, default rounding mode.
  Checked_Number(const mpz_class& y);

  //! Direct initialization from a C string, default rounding mode.
  Checked_Number(const char* y);

  //! Direct initialization from minus infinity, default rounding mode.
  Checked_Number(const Minus_Infinity& y);

  //! Direct initialization from plus infinity, default rounding mode.
  Checked_Number(const Plus_Infinity& y);

  //! Direct initialization from NAN, default rounding mode.
  Checked_Number(const Not_A_Number& y);

  //@} // Constructors

  //! \name Accessors and Conversions
  //@{

  //! Conversion operator: returns a copy of the underlying numeric value.
  operator T() const;

  //! Returns a reference to the underlying numeric value.
  T& raw_value();

  //! Returns a const reference to the underlying numeric value.
  const T& raw_value() const;

  //@} // Accessors and Conversions

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! Classifies *this.
  /*!
    Returns the appropriate Result characterizing:
    - whether \p *this is NAN,
      if \p nan is <CODE>true</CODE>;
    - whether \p *this is a (positive or negative) infinity,
      if \p inf is <CODE>true</CODE>;
    - the sign of \p *this,
      if \p sign is <CODE>true</CODE>.
  */
  Result classify(bool nan = true, bool inf = true, bool sign = true) const;

  //! \name Assignment Operators
  //@{

  //! Assignment operator.
  Checked_Number& operator=(const Checked_Number& y);

  //! Assignment operator.
  template <typename From, typename From_Policy>
  Checked_Number& operator=(const Checked_Number<From, From_Policy>& y);

  //! Assignment operator.
  template <typename From>
  Checked_Number& operator=(const From& y);

  //! Assignment operator.
  Checked_Number& operator=(const Not_A_Number& y);

  //! Assignment operator.
  Checked_Number& operator=(const Minus_Infinity& y);

  //! Assignment operator.
  Checked_Number& operator=(const Plus_Infinity& y);

  //! Add and assign operator.
  template <typename From_Policy>
  Checked_Number& operator+=(const Checked_Number<T, From_Policy>& y);

  //! Add and assign operator.
  Checked_Number& operator+=(const T& y);

  //! Add and assign operator.
  template <typename From, typename From_Policy>
  Checked_Number& operator+=(const Checked_Number<From, From_Policy>& y);

  template <typename From>
  Checked_Number& operator+=(const From& y);

  //! Subtract and assign operator.
  template <typename From_Policy>
  Checked_Number& operator-=(const Checked_Number<T, From_Policy>& y);

  //! Subtract and assign operator.
  Checked_Number& operator-=(const T& y);

  //! Subtract and assign operator.
  template <typename From, typename From_Policy>
  Checked_Number& operator-=(const Checked_Number<From, From_Policy>& y);

  //! Subtract and assign operator.
  template <typename From>
  Checked_Number& operator-=(const From& y);

  //! Multiply and assign operator.
  template <typename From_Policy>
  Checked_Number& operator*=(const Checked_Number<T, From_Policy>& y);

  //! Multiply and assign operator.
  Checked_Number& operator*=(const T& y);
  template <typename From, typename From_Policy>

  //! Multiply and assign operator.
  Checked_Number& operator*=(const Checked_Number<From, From_Policy>& y);

  //! Multiply and assign operator.
  template <typename From>
  Checked_Number& operator*=(const From& y);

  //! Divide and assign operator.
  template <typename From_Policy>
  Checked_Number& operator/=(const Checked_Number<T, From_Policy>& y);

  //! Divide and assign operator.
  Checked_Number& operator/=(const T& y);

  //! Divide and assign operator.
  template <typename From, typename From_Policy>
  Checked_Number& operator/=(const Checked_Number<From, From_Policy>& y);

  //! Divide and assign operator.
  template <typename From>
  Checked_Number& operator/=(const From& y);

  //! Compute remainder and assign operator.
  template <typename From_Policy>
  Checked_Number& operator%=(const Checked_Number<T, From_Policy>& y);

  //! Compute remainder and assign operator.
  Checked_Number& operator%=(const T& y);

  //! Compute remainder and assign operator.
  template <typename From, typename From_Policy>
  Checked_Number& operator%=(const Checked_Number<From, From_Policy>& y);

  //! Compute remainder and assign operator.
  template <typename From>
  Checked_Number& operator%=(const From& y);

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

private:
  //! The underlying numeric value.
  T v;
};

template <typename T, typename Policy>
bool is_not_a_number(const Checked_Number<T, Policy>& x);
template <typename T, typename Policy>
bool is_minus_infinity(const Checked_Number<T, Policy>& x);
template <typename T, typename Policy>
bool is_plus_infinity(const Checked_Number<T, Policy>& x);

template <typename To>
Result assign_r(To& to, const Minus_Infinity& x, Rounding_Dir dir);
template <typename To>
Result assign_r(To& to, const Plus_Infinity& x, Rounding_Dir dir);
template <typename To>
Result assign_r(To& to, const Not_A_Number& x, Rounding_Dir dir);
template <typename To>
Result assign_r(To& to, const char* x, Rounding_Dir dir);
template <typename To, typename To_Policy>
Result assign_r(To& to, char* x, Rounding_Dir dir);

#define FUNC1(name) \
template <typename To, typename From> \
Result name(To& to, const From& x, Rounding_Dir dir);

FUNC1(assign_r)
FUNC1(neg_assign_r)
FUNC1(abs_assign_r)
FUNC1(sqrt_assign_r)

#undef FUNC1

#define FUNC1(name) \
template <typename To, typename From> \
Result name(To& to, const From& x, int exp, Rounding_Dir dir);

FUNC1(mul2exp_assign_r)
FUNC1(div2exp_assign_r)

#undef FUNC1

#define FUNC2(name) \
template <typename To, typename From1, typename From2> \
Result name(To& to, const From1& x, const From2& y, Rounding_Dir dir);

FUNC2(add_assign_r)
FUNC2(sub_assign_r)
FUNC2(mul_assign_r)
FUNC2(div_assign_r)
FUNC2(rem_assign_r)
FUNC2(gcd_assign_r)
FUNC2(lcm_assign_r)
FUNC2(add_mul_assign_r)
FUNC2(sub_mul_assign_r)

#undef FUNC2

#define FUNC4(name) \
template <typename To1, typename From1, typename From2,		\
	  typename To2, typename To3>				\
Result name(To1& to, const From1& x, const From2& y,		\
	    To2& s, To3& t, Rounding_Dir dir);

FUNC4(gcdext_assign_r)

#undef FUNC4

//! Swaps \p *this with \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void swap(Checked_Number<T, Policy>& x, Checked_Number<T, Policy>& y);

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

//! Assigns to \p x its negation.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
neg_assign(Checked_Number<T, Policy>& x);

//! Assigns to \p x the negation of \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
neg_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y);

//! Assigns to \p x its absolute value.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
abs_assign(Checked_Number<T, Policy>& x);

//! Assigns to \p x the absolute value of \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
abs_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy>& y);

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

//! Assigns to \p x the greatest common divisor of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
gcd_assign(Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y,
	   const Checked_Number<T, Policy>& z);

/*! \brief
  Assigns to \p x the greatest common divisor of \p y and \p z,
  setting \p s and \p t such that s*y + t*z = x = gcd(y, z).
*/
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
gcdext_assign(Checked_Number<T, Policy>& x,
	      const Checked_Number<T, Policy>& y,
	      const Checked_Number<T, Policy>& z,
	      Checked_Number<T, Policy>& s,
	      Checked_Number<T, Policy>& t);

//! Assigns to \p x the least common multiple of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
lcm_assign(Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y,
	   const Checked_Number<T, Policy>& z);

/*! \brief
  If \p z divides \p y, assigns to \p x the quotient of the integer
  division of \p y and \p z.

  \relates Checked_Number
  The behavior is undefined if \p z does not divide \p y.
*/
template <typename T, typename Policy>
void
exact_div_assign(Checked_Number<T, Policy>& x,
		 const Checked_Number<T, Policy>& y,
		 const Checked_Number<T, Policy>& z);

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
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator==(const Checked_Number<T1, Policy1>& x,
	   const Checked_Number<T2, Policy2>& y);

//! Disequality operator.
/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator!=(const Checked_Number<T1, Policy1>& x,
	   const Checked_Number<T2, Policy2>& y);

//! Greater than or equal to operator.
/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator>=(const Checked_Number<T1, Policy1>& x,
	   const Checked_Number<T2, Policy2>& y);

//! Greater than operator.
/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator>(const Checked_Number<T1, Policy1>& x,
	  const Checked_Number<T2, Policy2>& y);

//! Less than or equal to operator.
/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator<=(const Checked_Number<T1, Policy1>& x,
	   const Checked_Number<T2, Policy2>& y);

//! Less than operator.
/*! \relates Checked_Number */
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
bool
operator<(const Checked_Number<T1, Policy1>& x,
	  const Checked_Number<T2, Policy2>& y);

/*! \brief
  Returns \f$-1\f$, \f$0\f$ or \f$1\f$ depending on whether the value
  of \p x is negative, zero or positive, respectively.

  \relates Checked_Number
*/
template <typename T, typename Policy>
int
sgn(const Checked_Number<T, Policy>& x);

/*! \brief
  Returns a negative, zero or positive value depending on whether
  \p x is lower than, equal to or greater than \p y, respectively.

  \relates Checked_Number
*/
template <typename T1, typename Policy1,
	  typename T2, typename Policy2>
int
cmp(const Checked_Number<T1, Policy1>& x,
    const Checked_Number<T2, Policy2>& y);

//@} // Relational Operators and Comparison Functions

//! \name Input-Output Operators
//@{

/*! \relates Checked_Number */
template <typename T, typename Policy>
Result
output(std::ostream& os,
       const Checked_Number<T, Policy>& x,
       const Numeric_Format& fmt,
       Rounding_Dir dir);

//! Output operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x);

//! Input function.
/*!
  \relates Checked_Number

  \param is
  Input stream to read from;

  \param x
  Number (possibly extended) to assign to in case of successful reading;

  \param dir
  Rounding mode to be applied.

  \return
  Result of the input operation.  Success, success with imprecision,
  overflow, parsing error: all possibilities are taken into account,
  checked for, and properly reported.

  This function attempts reading a (possibly extended) number from the given
  stream \p is, possibly rounding as specified by \p dir, assigning the result
  to \p x upon success, and returning the appropriate Result.

  The input syntax allows the specification of:
  - plain base-10 integer numbers as <CODE>34976098</CODE>,
    <CODE>-77</CODE> and <CODE>+13</CODE>;
  - base-10 integer numbers in scientific notation as <CODE>15e2</CODE>
    and <CODE>15*^2</CODE> (both meaning \f$15 \cdot 10^2 = 1500\f$),
    <CODE>9200e-2</CODE> and <CODE>-18*^+11111111111111111</CODE>;
  - base-10 rational numbers in fraction notation as
    <CODE>15/3</CODE> and <CODE>15/-3</CODE>;
  - base-10 rational numbers in fraction/scientific notation as
    <CODE>15/30e-1</CODE> (meaning \f$5\f$) and <CODE>15*^-3/29e2</CODE>
    (meaning \f$3/580000\f$);
  - base-10 rational numbers in floating point notation as
    <CODE>71.3</CODE> (meaning \f$713/10\f$) and
    <CODE>-0.123456</CODE> (meaning \f$-1929/15625\f$);
  - base-10 rational numbers in floating point scientific notation as
    <CODE>2.2e-1</CODE> (meaning \f$11/50\f$) and <CODE>-2.20001*^+3</CODE>
    (meaning \f$-220001/100\f$);
  - integers and rationals (in fractional, floating point and scientific
    notations) specified by using Mathematica-style bases, in the range
    from 2 to 36, as
    <CODE>2^^11</CODE> (meaning \f$3\f$),
    <CODE>36^^z</CODE> (meaning \f$35\f$),
    <CODE>36^^xyz</CODE> (meaning \f$44027\f$),
    <CODE>2^^11.1</CODE> (meaning \f$7/2\f$),
    <CODE>10^^2e3</CODE> (meaning \f$2000\f$),
    <CODE>8^^2e3</CODE> (meaning \f$1024\f$),
    <CODE>8^^2.1e3</CODE> (meaning \f$1088\f$),
    <CODE>8^^20402543.120347e7</CODE> (meaning \f$9073863231288\f$),
    <CODE>8^^2.1</CODE> (meaning \f$17/8\f$);
    note that the base and the exponent are always written as plain
    base-10 integer numbers; also, when an ambiguity may arise, the
    character <CODE>e</CODE> is interpreted as a digit, so that
    <CODE>16^^1e2</CODE> (meaning \f$482\f$) is different from
    <CODE>16^^1*^2</CODE> (meaning \f$256\f$);
  - the C-style hexadecimal prefix <CODE>0x</CODE> is interpreted as
    the Mathematica-style prefix <CODE>16^^</CODE>;
  - special values like <CODE>inf</CODE> and <CODE>+inf</CODE>
    (meaning \f$+\infty\f$), <CODE>-inf</CODE> (meaning \f$-\infty\f$),
    and <CODE>nan</CODE> (meaning "not a number").

  The rationale behind the accepted syntax can be summarized as follows:
  - if the syntax is accepted by Mathematica, then this function
    accepts it with the same semantics;
  - if the syntax is acceptable as standard C++ integer or floating point
    literal (except for octal notation and type suffixes, which are not
    supported), then this function accepts it with the same semantics;
  - natural extensions of the above are accepted with the natural
    extensions of the semantics;
  - special values are accepted.

  Valid syntax is more formally and completely specified by the
  following grammar, with the additional provisos that everything is
  <EM>case insensitive</EM>, that the syntactic category
  <CODE>BDIGIT</CODE> is further restricted by the current base
  and that for all bases above 14, any <CODE>e</CODE> is always
  interpreted as a digit and never as a delimiter for the exponent part
  (if such a delimiter is desired, it has to be written as <CODE>*^</CODE>).

\code
number	: NAN					INF	: 'inf'
	| SIGN INF					;
	| INF
	| num					NAN	: 'nan'
	| num DIV num					;
	;
						SIGN	: '-'
num     : unum						| '+'
        | SIGN unum					;

unum	: unum1					EXP	: 'e'
	| HEX unum1					| '*^'
	| base BASE unum1				;
	;
						POINT	: '.'
unum1	: mantissa					;
	| mantissa EXP exponent
	;					DIV	: '/'
							;
mantissa: bdigits
        | POINT bdigits				MINUS	: '-'
	| bdigits POINT					;
	| bdigits POINT bdigits
	;					PLUS	: '+'
						;
exponent: SIGN digits
	| digits				HEX	: '0x'
	;					;

bdigits : BDIGIT				BASE	: '^^'
	| bdigits BDIGIT				;
	;
						DIGIT   : '0' .. '9'
digits  : DIGIT						;
	| digits DIGIT
	;					BDIGIT  : '0' .. '9'
							| 'a' .. 'z'
							;
\endcode
*/
template <typename T, typename Policy>
Result
input(std::istream& is, Checked_Number<T, Policy>& x, Rounding_Dir dir);

//! Input operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
std::istream&
operator>>(std::istream& is, Checked_Number<T, Policy>& x);

//@} // Input-Output Operators

void throw_result_exception(Result r);

template <typename T>
T
plus_infinity();

template <typename T>
T
minus_infinity();

template <typename T>
T
not_a_number();

} // namespace Parma_Polyhedra_Library

#include "Checked_Number.inlines.hh"
#include "checked_numeric_limits.hh"

#endif // !defined(PPL_Checked_Number_defs_hh)
