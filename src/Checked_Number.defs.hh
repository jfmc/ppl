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

struct Checked_Number_Transparent_Policy {
  //! Check for overflowed result.
  static const int check_overflow = 0;
  //! Check for attempts to add infinities with different sign.
  static const int check_inf_add_inf = 0;
  //! Check for attempts to sub infinities with same sign.
  static const int check_inf_sub_inf = 0;
  //! Check for attempts to mul infinities by zero.
  static const int check_inf_mul_zero = 0;
  //! Check for attempts to divide by zero.
  static const int check_div_zero = 0;
  //! Check for attempts to divide infinities.
  static const int check_inf_div_inf = 0;
  //! Check for attempts to compute remainder of infinities.
  static const int check_inf_mod = 0;
  //! Check for attempts to take the square root of a negative number.
  static const int check_sqrt_neg = 0;
  //! Store unknown special value.
  static const int store_nan = 0;
  //! Store overflow special values.
  static const int store_infinity = 0;
  //! Representation is identical to primitive.
  static const int convertible = 1;
  //! Check for FPU inexact result.
  static const int fpu_check_inexact = 0;
  //! Check for NaN arguments
  static const int check_nan_args = 0;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_NATIVE;
  static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_NATIVE;
  static void handle_result(Result r);
};

struct Checked_Number_Default_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int store_nan = 0;
  static const int store_infinity = 0;
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

struct Extended_Number_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int store_nan = 1;
  static const int store_infinity = 1;
  // Don't uncomment the following.
  // The compile time error on conversions is the expected behaviour.
  // static const int convertible = 0;
#ifdef DEBUG_ROUND_NOT_NEEDED
  static const int fpu_check_inexact = 1;
#else
  static const int fpu_check_inexact = 0;
#endif
  static const int check_nan_args = 1;
  static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR_INF = ROUND_NOT_NEEDED;
  static const Rounding_Dir ROUND_DEFAULT_ASSIGN_INF = ROUND_NOT_NEEDED;
  // Don't uncomment the following.
  // The compile time error is the expected behaviour.
  // static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_UP;
  static void handle_result(Result r);
};

typedef Checked::Check_Overflow_Policy Default_To_Policy;
typedef Checked_Number_Transparent_Policy Default_From_Policy;

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

  // Don't enable this: with GCC, the presence of a copy constructor
  // (even if it is defined exactly as the default one) inhibits some
  // important optimizations.
  //! Copy-constructor.
  Checked_Number(const Checked_Number& y);

#if 0
  //! Direct initialization from a Checked_Number.
  template <typename From, typename From_Policy>
  Checked_Number(const Checked_Number<From, From_Policy>& y);
#endif

  //! Direct initialization from a signed char value.
  Checked_Number(const signed char y, Rounding_Dir dir);
  //! Direct initialization from a signed short value.
  Checked_Number(const signed short y, Rounding_Dir dir);
  //! Direct initialization from a signed int value.
  Checked_Number(const signed int y, Rounding_Dir dir);
  //! Direct initialization from a signed long value.
  Checked_Number(const signed long y, Rounding_Dir dir);
  //! Direct initialization from a signed long long value.
  Checked_Number(const signed long long y, Rounding_Dir dir);

  //! Direct initialization from an unsigned char value.
  Checked_Number(const unsigned char y, Rounding_Dir dir);
  //! Direct initialization from an unsigned short value.
  Checked_Number(const unsigned short y, Rounding_Dir dir);
  //! Direct initialization from an unsigned int value.
  Checked_Number(const unsigned int y, Rounding_Dir dir);
  //! Direct initialization from an unsigned long value.
  Checked_Number(const unsigned long y, Rounding_Dir dir);
  //! Direct initialization from an unsigned long long value.
  Checked_Number(const unsigned long long y, Rounding_Dir dir);

  //! Direct initialization from a float value.
  Checked_Number(const float y, Rounding_Dir dir);
  //! Direct initialization from a double value.
  Checked_Number(const double y, Rounding_Dir dir);
  //! Direct initialization from a long double value.
  Checked_Number(const long double y, Rounding_Dir dir);

  //! Direct initialization from a GMP unbounded rational value.
  Checked_Number(const mpq_class& y, Rounding_Dir dir);
  //! Direct initialization from a GMP unbounded integer value.
  Checked_Number(const mpz_class& y, Rounding_Dir dir);
  //! Direct initialization from a C string value.
  Checked_Number(const char* y, Rounding_Dir dir);

  Checked_Number(const Minus_Infinity& y, Rounding_Dir dir);
  Checked_Number(const Plus_Infinity& y, Rounding_Dir dir);
  Checked_Number(const Not_A_Number& y, Rounding_Dir dir);

  //! Direct initialization from a signed char value.
  Checked_Number(const signed char y);
  //! Direct initialization from a signed short value.
  Checked_Number(const signed short y);
  //! Direct initialization from a signed int value.
  Checked_Number(const signed int y);
  //! Direct initialization from a signed long value.
  Checked_Number(const signed long y);
  //! Direct initialization from a signed long long value.
  Checked_Number(const signed long long y);

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

  //! Direct initialization from a float value.
  Checked_Number(const float y);
  //! Direct initialization from a double value.
  Checked_Number(const double y);
  //! Direct initialization from a long double value.
  Checked_Number(const long double y);

  //! Direct initialization from a GMP unbounded rational value.
  Checked_Number(const mpq_class& y);
  //! Direct initialization from a GMP unbounded integer value.
  Checked_Number(const mpz_class& y);
  //! Direct initialization from a C string value.
  Checked_Number(const char* y);

  Checked_Number(const Minus_Infinity& y);
  Checked_Number(const Plus_Infinity& y);
  Checked_Number(const Not_A_Number& y);

  //@} // Constructors

  //! \name Accessors and Conversions
  //@{

  /*! \brief
    Conversion operator:
    returns a copy of the underlying native integer value.
  */
  operator T() const;

  //! Returns a reference to the underlying native integer value.
  T& raw_value();

  //! Returns a const reference to the underlying native integer value.
  const T& raw_value() const;

  //@} // Accessors and Conversions

  bool OK() const;
  Result classify(bool nan = true, bool inf = true, bool sign = true) const;

  //! \name Assignment Operators
  //@{

  //! Assignment operator.
  Checked_Number& operator=(const Checked_Number& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator=(const Checked_Number<From, From_Policy>& y);
  template <typename From>
  Checked_Number& operator=(const From& y);
  Checked_Number& operator=(const Not_A_Number& y);
  Checked_Number& operator=(const Minus_Infinity& y);
  Checked_Number& operator=(const Plus_Infinity& y);
  //! Add and assign operator.
  template <typename From_Policy>
  Checked_Number& operator+=(const Checked_Number<T, From_Policy>& y);
  Checked_Number& operator+=(const T& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator+=(const Checked_Number<From, From_Policy>& y);
  template <typename From>
  Checked_Number& operator+=(const From& y);
  //! Subtract and assign operator.
  template <typename From_Policy>
  Checked_Number& operator-=(const Checked_Number<T, From_Policy>& y);
  Checked_Number& operator-=(const T& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator-=(const Checked_Number<From, From_Policy>& y);
  template <typename From>
  Checked_Number& operator-=(const From& y);
  //! Multiply and assign operator.
  template <typename From_Policy>
  Checked_Number& operator*=(const Checked_Number<T, From_Policy>& y);
  Checked_Number& operator*=(const T& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator*=(const Checked_Number<From, From_Policy>& y);
  template <typename From>
  Checked_Number& operator*=(const From& y);
  //! Divide and assign operator.
  template <typename From_Policy>
  Checked_Number& operator/=(const Checked_Number<T, From_Policy>& y);
  Checked_Number& operator/=(const T& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator/=(const Checked_Number<From, From_Policy>& y);
  template <typename From>
  Checked_Number& operator/=(const From& y);
  //! Compute remainder and assign operator.
  template <typename From_Policy>
  Checked_Number& operator%=(const Checked_Number<T, From_Policy>& y);
  Checked_Number& operator%=(const T& y);
  template <typename From, typename From_Policy>
  Checked_Number& operator%=(const Checked_Number<From, From_Policy>& y);
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
  //! The underlying native integer value.
  T v;

};

template <typename To, typename To_Policy>
Result assign_r(Checked_Number<To, To_Policy>& to,
		const Minus_Infinity& x, Rounding_Dir dir);
template <typename To, typename To_Policy>
Result assign_r(Checked_Number<To, To_Policy>& to,
		const Plus_Infinity& x, Rounding_Dir dir);
template <typename To, typename To_Policy>
Result assign_r(Checked_Number<To, To_Policy>& to,
		const Not_A_Number& x, Rounding_Dir dir);
template <typename To, typename To_Policy>
Result assign_r(Checked_Number<To, To_Policy>& to,
		const char* x, Rounding_Dir dir);
template <typename To, typename To_Policy>
Result assign_r(Checked_Number<To, To_Policy>& to,
		char* x, Rounding_Dir dir);

#define FUNC1(name) \
template <typename To, typename From> \
Result name(To& to, const From& x, Rounding_Dir dir); \
template <typename To, typename To_Policy, typename From> \
Result name(Checked_Number<To, To_Policy>& to, \
            const From& x, \
	    Rounding_Dir dir); \
template <typename To, typename To_Policy, \
          typename From, typename From_Policy> \
Result name(Checked_Number<To, To_Policy>& to, \
            const Checked_Number<From, From_Policy>& x, \
            Rounding_Dir dir);

FUNC1(assign_r)
FUNC1(neg_assign_r)
FUNC1(abs_assign_r)
FUNC1(sqrt_assign_r)

#undef FUNC1

#define FUNC1(name) \
template <typename To, typename From> \
Result name(To& to, const From& x, int exp, Rounding_Dir dir); \
template <typename To, typename To_Policy, typename From> \
Result name(Checked_Number<To, To_Policy>& to, \
            const From& x, \
            int exp, Rounding_Dir dir); \
template <typename To, typename To_Policy, \
          typename From, typename From_Policy> \
Result name(Checked_Number<To, To_Policy>& to, \
            const Checked_Number<From, From_Policy>& x, \
            int exp, Rounding_Dir dir); \
template <typename To, typename From, typename From_Policy> \
Result name(To& to, \
            const Checked_Number<From, From_Policy>& x, \
            int exp, Rounding_Dir dir);

FUNC1(mul2exp_assign_r)
FUNC1(div2exp_assign_r)

#undef FUNC1

#define FUNC2(name) \
template <typename To, typename From1, typename From2> \
Result name(To& to, const From1& x, const From2& y, Rounding_Dir dir); \
template <typename To, typename To_Policy, typename From1, typename From2> \
Result name(Checked_Number<To, To_Policy>& to, \
            const From1& x, \
            const From2& y, \
            Rounding_Dir dir); \
template <typename To, typename From1, typename From2, typename Policy2> \
Result name(To& to, \
            const From1& x, \
            const Checked_Number<From2, Policy2>& y, \
	    Rounding_Dir dir); \
template <typename To, typename To_Policy, \
          typename From1, typename From2, typename Policy2> \
Result name(Checked_Number<To, To_Policy>& to, \
	    const From1& x, \
            const Checked_Number<From2, Policy2>& y, \
	    Rounding_Dir dir); \
template <typename To, typename From1, typename Policy1, typename From2> \
Result name(To& to, \
	    const Checked_Number<From1, Policy1>& x, \
            const From2& y, \
	    Rounding_Dir dir); \
template <typename To, typename To_Policy, \
          typename From1, typename Policy1, typename From2> \
Result name(Checked_Number<To, To_Policy>& to, \
	    const Checked_Number<From1, Policy1>& x, \
            const From2& y, \
	    Rounding_Dir dir); \
template <typename To, \
          typename From1, typename Policy1, \
	  typename From2, typename Policy2> \
Result name(To& to, \
            const Checked_Number<From1, Policy1>& x, \
	    const Checked_Number<From2, Policy2>& y, \
            Rounding_Dir dir); \
template <typename To, typename To_Policy, \
          typename From1, typename Policy1, \
          typename From2, typename Policy2> \
Result name(Checked_Number<To, To_Policy>& to, \
	    const Checked_Number<From1, Policy1>& x, \
	    const Checked_Number<From2, Policy2>& y, \
            Rounding_Dir dir);

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

//! Swaps \p *this with \p y.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void swap(Checked_Number<T, Policy>& x, Checked_Number<T, Policy>& y);

#if 0
template <typename T, typename Policy>
class Float<Checked_Number<T, Policy> > {
public:
  typedef Float<T>::Type Type;
};
#endif

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

//! Assigns to \p x the least common multiple of \p y and \p z.
/*! \relates Checked_Number */
template <typename T, typename Policy>
void
lcm_assign(Checked_Number<T, Policy>& x,
	   const Checked_Number<T, Policy>& y,
	   const Checked_Number<T, Policy>& z);

//! Assigns to \p x the integer division of \p y and \p z.
/*! \relates Checked_Number */
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
cmp(const Checked_Number<T1, Policy1>& x, const Checked_Number<T2, Policy2>& y);

//@} // Relational Operators and Comparison Functions

//! \name Input-Output Operators
//@{

/*! \relates Checked_Number */
template <typename T, typename Policy>
Result
output(std::ostream& os, const Checked_Number<T, Policy>& x, const Numeric_Format& fmt, Rounding_Dir dir);

//! Output operator.
/*! \relates Checked_Number */
template <typename T, typename Policy>
std::ostream&
operator<<(std::ostream& os, const Checked_Number<T, Policy>& x);

//! Input function.
/*!
  FIXME: this must be completed.

  The accepted syntax is specified by the following grammar, with the
  additional proviso that everything is <EM>case insensitive</EM>
  and that the syntactic category <CODE>BDIGIT</CODE> is further restriced
  by the current base.

\code
number	: NAN
	| SIGN INF
	| INF
	| num
	| num DIV num
	;

num     : unum
        | SIGN unum

unum	: unum1
	| HEX unum1
	| base BASE unum1
	;

unum1	: mantissa
	| mantissa EXP exponent
	;

mantissa: bdigits
        | POINT bdigits
	| bdigits POINT bdigits
	;

exponent: SIGN digits
	| digits
	;

bdigits : BDIGIT
	| bdigits BDIGIT
	;

digits  : DIGIT
	| digits DIGIT
	;

INF	: 'inf'
	;

NAN	: 'nan'
	;

SIGN	: '-'
	| '+'
	;

EXP	: 'e'
	| '*^'
	;

POINT	: '.'
	;

DIV	: '/'
	;

MINUS	: '-'
	;

PLUS	: '+'
	;

HEX	: '0x'
	;

BASE	: '^^'
	;

DIGIT   : '0' .. '9'
	;

BDIGIT  : '0' .. '9'
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

#endif // !defined(PPL_Checked_Number_defs_hh)
