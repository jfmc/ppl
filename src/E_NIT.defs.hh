/* E_NIT class declaration.   
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
site: http://www.cs.unipr.it/ppl/ .*/

#ifndef PPL_E_NIT_defs_hh
#define PPL_E_NIT_defs_hh 1

#include "E_NIT.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x is equal to \p y.
/*! \relates E_NIT */
template <typename T>
bool operator==(E_NIT<T> x, E_NIT<T> y);

//! Returns <CODE>true</CODE> if and only if \p x is different from \p y.
/*! \relates E_NIT */
template <typename T>
bool operator!=(E_NIT<T> x, E_NIT<T> y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is less than \p y.
/*! \relates E_NIT */
template <typename T>
bool operator<(E_NIT<T> x, E_NIT<T> y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is less than or equal to \p y.
/*! \relates E_NIT */
template <typename T>
bool operator<=(E_NIT<T> x, E_NIT<T> y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is greater than \p y.
/*! \relates E_NIT */
template <typename T>
bool operator>(E_NIT<T> x, E_NIT<T> y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is greater than or equal to \p y.
/*! \relates E_NIT */
template <typename T>
bool operator>=(E_NIT<T> x, E_NIT<T> y);

//! Returns \p x.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> operator+(E_NIT<T> x);

//! Returns - \p x.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> operator-(E_NIT<T> x);

//! Returns the sum of \p x and \p y.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> operator+(E_NIT<T> x, E_NIT<T> y);

//! Returns the C++ integer division of \p x and \p y.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> operator/(E_NIT<T> x, E_NIT<T> y);

//! Returns - \p x, rounded up if necessary.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> negate_round_up(E_NIT<T> x);

//! Returns - \p x, rounded down if necessary.
/*! \relates E_NIT */
template <typename T>
E_NIT<T> negate_round_down(E_NIT<T> x);

//! Returns the sum of \p x and \p y, rounded up if necessary.
template <typename T>
E_NIT<T> add_round_up(E_NIT<T> x, E_NIT<T> y);

//! Returns the sum of \p x and \p y, rounded down if necessary.
template <typename T>
E_NIT<T> add_round_down(E_NIT<T> x, E_NIT<T> y);

//! Returns the division of \p x by \p y, rounded up if necessary.
template <typename T>
E_NIT<T> div_round_up(E_NIT<T> x, E_NIT<T> y);

//! Returns the division of \p x by \p y, rounded down if necessary.
template <typename T>
E_NIT<T> div_round_down(E_NIT<T> x, E_NIT<T> y);

//! Returns the division of \p x by \p y, rounded up if necessary.
template <typename U>
U div_round_up(const Coefficient& x, const Coefficient& y);

//! Returns the division of \p x by \p y, rounded down if necessary.
template <typename U>
U div_round_down(const Coefficient& x, const Coefficient& y);

// FIXME
template <typename T>
E_NIT<T> div_round_up(E_NIT<T> x, int y);

template <typename T>
E_NIT<T> div_round_down(E_NIT<T> x, int y);

namespace IO_Operators {

//! Writes a textual representation of \p x to \p s.
/*! \relates Parma_Polyhedra_Library::E_NIT */
template <typename T>
std::ostream& operator<<(std::ostream& s, E_NIT<T> x);

//! \brief
//! Specialization for <CODE>signed char</CODE>
//! (to print <CODE>65</CODE> instead of <CODE>A</CODE>).
/*! \relates Parma_Polyhedra_Library::E_NIT */
template <>
std::ostream& operator<< <signed char>(std::ostream& s, E_NIT<signed char> x);

//! \brief
//! Reads a textual representation of an object of type T
//! and it's assigned to \p x.
/*! \relates Parma_Polyhedra_Library::E_Rational */
template <typename T>
std::istream& operator>>(std::istream& s, E_NIT<T>& x);

//! \brief
//! Specialization for <CODE>signed char</CODE>
//! (to read <CODE>65</CODE> instead of <CODE>A</CODE>).
/*! \relates Parma_Polyhedra_Library::E_NIT */
template <>
std::istream& operator>><signed char>(std::istream& s, E_NIT<signed char>& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! \brief
//! Extends a native integral type \p T with plus infinity,
//! minus infinity,  and not-a-number.
template <typename T>
class Parma_Polyhedra_Library::E_NIT {
public:
  //! Default constructor: provides no guarantees at all.
  E_NIT();

  //! Constructor with explicit initialization from a value of type \p T.
  explicit E_NIT(T y);

  explicit E_NIT(const Coefficient& y);

  //! Copy-constructor.
  E_NIT(const E_NIT& y);

  //! Assignment operator.
  E_NIT& operator=(E_NIT y);

  //! Assignment operator from a value of type \p T
  E_NIT& operator=(T y);

  //! Assignment operator from an Coefficient.
  E_NIT& operator=(const Coefficient& y);

  //! Returns <CODE>true</CODE> if and only if \p *this is plus infinity.
  bool is_plus_infinity() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a finite number.
  bool is_finite() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is zero.
  bool is_zero() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is minus infinity.
  bool is_minus_infinity() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is not a number.
  bool is_nan() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is greater than zero.
  bool is_positive() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is less than zero.
  bool is_negative() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is less than 
  //! or equal zero.
  bool is_nonpositive() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is greater than 
  //! or equal zero.
  bool is_nonnegative() const;

  //! If \p *this is a finite a number, returns its value.
  T number() const;

  //! \brief
  //! If \p *this is a finite a number, sets \p n and \p d to the numerator
  //! and denominator of \p *this, respectively.
  /*!
    Under the assumption that \p *this is a finite a number,
    \p n and \p d will be assigned coprime integer numbers
    \f$ n \f$ and \f$ d \f$ such that \p *this is equal to \f$ n/d \f$.
  */
  void numer_denom(Coefficient& n, Coefficient& d) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  static E_NIT plus_infinity();

  static E_NIT finite_max();

  static E_NIT finite_min();

  static E_NIT minus_infinity();

  static E_NIT not_a_number();

private:
  friend bool operator==<>(E_NIT x, E_NIT y);
  friend bool operator!=<>(E_NIT x, E_NIT y);
  friend bool operator< <>(E_NIT x, E_NIT y);
  friend bool operator<=<>(E_NIT x, E_NIT y);
  friend bool operator><>(E_NIT x, E_NIT y);
  friend bool operator>=<>(E_NIT x, E_NIT y);
  friend E_NIT operator+<>(E_NIT x);
  friend E_NIT operator-<>(E_NIT x);
  friend E_NIT operator+<>(E_NIT x, E_NIT y);
  friend E_NIT operator/<>(E_NIT x, E_NIT y);
#if !defined(__GNUC__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ > 3)
  friend std::istream&
  Parma_Polyhedra_Library::IO_Operators::operator>><>(std::istream& s,
						      E_NIT<T>& x);
#else
  // This is too lax than wanted.
  template <typename S>
  friend std::istream&
  Parma_Polyhedra_Library::IO_Operators::operator>>(std::istream& s,
						    E_NIT<S>& x);
#endif

protected:
  static inline T pinf();

  static inline T max();

  static inline T min();

  static inline T minf();

  static inline T nnum();

  //! Holds the native integer used for the encoding.
  T n;
};

#include "E_NIT.inlines.hh"

#endif // !defined(PPL_E_NIT_defs_hh)
