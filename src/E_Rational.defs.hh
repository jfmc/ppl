/* E_Rational class declaration.   
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
site: http://www.cs.unipr.it/ppl/ .*/

#ifndef PPL_E_Rational_defs_hh
#define PPL_E_Rational_defs_hh 1

#include "E_Rational.types.hh"
#include "Coefficient.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x is equal to \p y.
/*! \relates E_Rational */
bool operator==(const E_Rational& x, const E_Rational& y);

//! Returns <CODE>true</CODE> if and only if \p x is different from \p y.
/*! \relates E_Rational */
bool operator!=(const E_Rational& x, const E_Rational& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is less than \p y.
/*! \relates E_Rational */
bool operator<(const E_Rational& x, const E_Rational& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is less than or equal to \p y.
/*! \relates E_Rational */
bool operator<=(const E_Rational& x, const E_Rational& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is greater than \p y.
/*! \relates E_Rational */
bool operator>(const E_Rational& x, const E_Rational& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if \p x and \p y represent
//! numbers and \p x is greater than or equal to \p y.
/*! \relates E_Rational */
bool operator>=(const E_Rational& x, const E_Rational& y);

//! Returns \p x.
/*! \relates E_Rational */
E_Rational operator+(const E_Rational& x);

//! Returns - \p x.
/*! \relates E_Rational */
E_Rational operator-(const E_Rational& x);

//! Returns the sum of \p x and \p y.
/*! \relates E_Rational */
E_Rational operator+(const E_Rational& x, const E_Rational& y);

//! Returns the quotient of division of \p x by \p y.
/*! \relates E_Rational */
E_Rational operator/(const E_Rational& x, const E_Rational& y);

//! Returns - \p x, rounded up if necessary.
/*! \relates E_Rational */
E_Rational negate_round_up(const E_Rational& x);

//! Returns - \p x, rounded down if necessary.
/*! \relates E_Rational */
E_Rational negate_round_down(const E_Rational& x);

//! Returns the sum of \p x by \p y.
E_Rational add_round_up(const E_Rational& x, const E_Rational& y);

//! Returns the sum of \p x by \p y.
E_Rational add_round_down(const E_Rational& x, const E_Rational& y);

//! Returns the division of \p x by \p y, rounded up if necessary.
template <typename U>
U div_round_up(const Coefficient& x, const Coefficient& y);

//! Returns the division of \p x by \p y, rounded down if necessary.
template <typename U>
U div_round_down(const Coefficient& x, const Coefficient& y);

//! Returns the division of \p x by \p y, rounded up if necessary.
E_Rational div_round_up(const E_Rational& x, const Coefficient& y);

//! Returns the division of \p x by \p y, rounded down if necessary.
E_Rational div_round_down(const E_Rational& x, const Coefficient& y);

namespace IO_Operators {

//! Writes a textual representation of \p x to \p s.
/*! \relates Parma_Polyhedra_Library::E_Rational */
std::ostream& operator<<(std::ostream& s, const E_Rational& x);

//! \brief
//! Reads a textual representation of an object of type T
//! and it's assigned to \p x.
/*! \relates Parma_Polyhedra_Library::E_Rational */
std::istream& operator>>(std::istream& s, E_Rational& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! \brief
//! Extends a GMP rational number with plus infinity,
//! minus infinity,  and not-a-number.
class Parma_Polyhedra_Library::E_Rational {
public:
  //! Default constructor: provides no guarantees at all.
  E_Rational();

  //! Copy-constructor.
  E_Rational(const E_Rational& y);

  //! Constructor with explicit initialization from an \p mpz_class object.
  explicit E_Rational(const mpq_class& y);

  //! Assignment operator.
  E_Rational& operator=(const E_Rational& y);

  //! Assignment operator.
  E_Rational& operator=(const mpq_class& y);

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
  const mpq_class& number() const;

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

  static const E_Rational& plus_infinity();

  static const E_Rational& finite_max();

  static const E_Rational& finite_min();

  static const E_Rational& minus_infinity();

  static const E_Rational& not_a_number();

private:
  friend bool Parma_Polyhedra_Library::operator==(const E_Rational& x,
						  const E_Rational& y);
  friend bool Parma_Polyhedra_Library::operator!=(const E_Rational& x, const E_Rational& y);
  friend bool Parma_Polyhedra_Library::operator<(const E_Rational& x,
						 const E_Rational& y);
  friend bool Parma_Polyhedra_Library::operator<=(const E_Rational& x,
						  const E_Rational& y);
  friend bool Parma_Polyhedra_Library::operator>(const E_Rational& x,
						 const E_Rational& y);
  friend bool Parma_Polyhedra_Library::operator>=(const E_Rational& x,
						  const E_Rational& y);
  friend E_Rational Parma_Polyhedra_Library::operator+(const E_Rational& x);
  friend E_Rational Parma_Polyhedra_Library::operator-(const E_Rational& x);
  friend E_Rational Parma_Polyhedra_Library::operator+(const E_Rational& x,
						       const E_Rational& y);
  friend E_Rational Parma_Polyhedra_Library::operator/(const E_Rational& x,
						       const E_Rational& y);
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const E_Rational& x);
  friend std::istream&
  Parma_Polyhedra_Library::IO_Operators::operator>>(std::istream& s,
						    E_Rational& x);

  template <typename U>
  friend U
  Parma_Polyhedra_Library::div_round_up(const Coefficient& x,
					const Coefficient& y);
  friend E_Rational
  Parma_Polyhedra_Library::div_round_up(const E_Rational& x,
					const Coefficient& y);

protected:
  static inline const mpq_class& pinf();

  static inline const mpq_class& max();

  static inline const mpq_class& min();

  static inline const mpq_class& minf();

  static inline const mpq_class& nnum();

  //! Holds the GMP rational number used for the encoding.
  mpq_class n;
};

#include "E_Rational.inlines.hh"

#endif // !defined(PPL_E_Rational_defs_hh)
