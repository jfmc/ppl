/* Declarations for the Interval class and its constituents.
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

#ifndef PPL_Interval_defs_hh
#define PPL_Interval_defs_hh 1

#include "Interval.types.hh"

#include "Integer.types.hh"
#include <gmpxx.h>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is equal to \p y.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator==(const ERational& x, const ERational& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator!=(const ERational& x, const ERational& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x is less than or equal to  \p y.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator<=(const ERational& x, const ERational& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x is greater than or equal to  \p y.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator>=(const ERational& x, const ERational& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is less than \p y.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator<(const ERational& x, const ERational& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is greater than \p y.
/*! \relates ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator>(const ERational& x, const ERational& y);

namespace IO_Operators {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Output operator.
/*! \relates Parma_Polyhedra_Library::ERational */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
std::ostream& operator<<(std::ostream& s, const ERational& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An extended rational number.
/*!
  An object of class ERational represents an element of the extended
  set of rational numbers \f$\Qset \union \{ -\infty, +\infty \}\f$.
  Elements of class ERational are totally ordered by the usual
  extension of the natural ordering on rational numbers, so that
  \f$-\infty < q < +\infty\f$ for all \f$q \in \Qset\f$.

  A finite rational number \f$q \in \Qset\f$ is internally represented
  by a \p mpq_class object
  (see the GMP's manual available at http://swox.com/gmp/ ).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::ERational {
public:
  //! Builds the finite rational number \p num / \p den.
  /*!
    An undefined behavior is obtained if \p den is equal to zero.
  */
  ERational(const Integer& num, const Integer& den);

  //! \brief
  //! Builds \f$+\infty\f$ (resp., \f$-\infty\f$)
  //! if \p sign is equal to <CODE>'+'</CODE> (resp., <CODE>'-'</CODE>).
  /*!
    An undefined behavior is obtained for any other value of \p sign.
  */
  explicit ERational(char sign);

  //! Copy constructor.
  ERational(const ERational& y);

  //! Assignment operator.
  ERational& operator=(const ERational& y);

  //! \brief
  //! Returns a negative integer if \p *this is equal to \f$-\infty\f$,
  //! zero if \p *this is an extended rational having a finite value,
  //! a positive integer if \p *this is equal to \f$+\infty\f$.
  int direction_of_infinity() const;

  //! Returns the numerator of the canonical form for \p *this.
  /*!
    The result is undefined if \p *this represents an infinity.
  */
  const Integer& numerator() const;

  //! Returns the denominator of the canonical form for \p *this.
  /*!
    The result is undefined if \p *this represent an infinity.
  */
  const Integer& denominator() const;

  friend bool
  Parma_Polyhedra_Library::operator==(const ERational& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const ERational& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const ERational& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const ERational& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const ERational& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const ERational& x, const ERational& y);
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const ERational& x);

private:
  //! Positive if \f$+\infty\f$, negative if \f$-\infty\f$, zero otherwise.
  int e;

  //! The finite value: valid only if \p e is equal to zero.
  mpq_class v;
};


namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is less than \p y.
/*! \relates Boundary */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator<(const Boundary& x, const Boundary& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is greater than \p y.
/*! \relates Boundary */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator>(const Boundary& x, const Boundary& y);

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An extended rational bound of an interval.
/*!
  An object of class Boundary represents either an upper or a lower
  bound of an interval over the set of extended rational numbers. 
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Boundary {
protected:
  //! Kinds of bounds.
  enum Flag {
    //! An open upper bound.
    NEG = -1,
    //! A closed (lower or upper) bound.
    ZERO = 0,
    //! An open lower bound.
    POS = 1
  };

  //! The extended rational value of the bound.
  ERational value;
  //! The kind of the bound.
  Flag flag;

  //! Builds a bound of kind \p f and having value \p v.   
  Boundary(const ERational& v, Flag f);

  friend bool
  Parma_Polyhedra_Library::operator<(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Boundary& x, const Boundary& y);

public:
  //! Returns <CODE>true</CODE> if and only if \p *this is a closed bound.
  bool is_closed() const;

  //! Returns a const reference to the value of the bound.
  const ERational& bound() const;
};


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The lower bound of an extended rational interval.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::LBoundary : public Boundary {
public:
  //! Kinds of lower bounds.
  enum OpenClosed {
    //! An open lower bound.
    OPEN = Boundary::POS,
    //! A closed lower bound.
    CLOSED = Boundary::ZERO
  };

  //! Builds a lower bound of kind \p f and having value \p v.
  LBoundary(const ERational& v, OpenClosed f);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The upper bound of an extended rational interval.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::UBoundary : public Boundary {
public:
  //! Kinds of upper bounds.
  enum OpenClosed {
    //! An open upper bound.
    OPEN = Boundary::NEG,
    //! A closed upper bound.
    CLOSED = Boundary::ZERO
  };

  //! Builds an upper bound of kind \p f and having value \p v.   
  UBoundary(const ERational& v, OpenClosed f);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A interval over the set of rational numbers.
/*!
  An object of class Interval represents a closed/half-closed/open
  interval over the set of rational numbers \f$\Qset\f$.

  Note that, even though the implementation is capable to represent
  any interval on the set of <EM>extended</EM> rational numbers,
  the available public methods only allows for the construction
  and manipulation of intervals over \f$\Qset\f$.
  Namely, it is not possible to create a non-empty interval having
  a <EM>closed</EM> bound whose value is \f$-\infty\f$ or \f$+\infty\f$.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Interval {
public:
  //! Constructs the universe interval \f$(-\infty, +\infty) = \Qset\f$.
  Interval();

  //! Returns <CODE>true</CODE> if and only if \p *this is empty.
  bool is_empty() const;

  //! Returns a const reference to the interval's lower bound.
  const LBoundary& lower_bound() const;

  //! Returns a const reference to the interval's upper bound.
  const UBoundary& upper_bound() const;

  //! \brief
  //! Raises the interval's lower bound, if \p new_lower is higher
  //! than the current one.
  void raise_lower_bound(LBoundary new_lower);

  //! \brief
  //! Lowers the interval's upper bound, if \p new_upper is lower
  //! than the current one.
  void lower_upper_bound(UBoundary new_upper);

  //! Turns \p *this into the empty interval.
  void set_empty();

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! The interval's lower bound.
  LBoundary lower;

  //! The interval's upper bound.
  UBoundary upper;
};

#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
