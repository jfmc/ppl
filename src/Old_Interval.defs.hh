/* Declarations for the Old_Interval class and its constituents.
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

#ifndef PPL_Old_Interval_defs_hh
#define PPL_Old_Interval_defs_hh 1

#include "Old_Interval.types.hh"
#include "Coefficient.defs.hh"
#include "Checked_Number.defs.hh"
#include <gmpxx.h>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

typedef Checked_Number<mpq_class, Extended_Number_Policy> ERational;

}

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is less than \p y.
/*! \relates Old_Boundary */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator<(const Old_Boundary& x, const Old_Boundary& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is greater than \p y.
/*! \relates Old_Boundary */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator>(const Old_Boundary& x, const Old_Boundary& y);

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An extended rational bound of an interval.
/*! \ingroup PPL_CXX_interface
  An object of class Old_Boundary represents either an upper or a lower
  bound of an interval over the set of extended rational numbers.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Old_Boundary {
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
  Old_Boundary(const ERational& v, Flag f);

  friend bool
  Parma_Polyhedra_Library::operator<(const Old_Boundary& x, const Old_Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Old_Boundary& x, const Old_Boundary& y);

public:
  //! Returns <CODE>true</CODE> if and only if \p *this is a closed bound.
  bool is_closed() const;

  //! Returns a const reference to the value of the bound.
  const ERational& bound() const;

  //! Returns a reference to the value of the bound.
  ERational& bound();
};


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The lower bound of an extended rational interval.
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::LBoundary : public Old_Boundary {
public:
  //! Kinds of lower bounds.
  enum Open_Closed {
    //! An open lower bound.
    OPEN = Old_Boundary::POS,
    //! A closed lower bound.
    CLOSED = Old_Boundary::ZERO
  };

  //! Builds a lower bound of kind \p f and having value \p v.
  LBoundary(const ERational& v, Open_Closed f);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The upper bound of an extended rational interval.
/*! \ingroup PPL_CXX_interface */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::UBoundary : public Old_Boundary {
public:
  //! Kinds of upper bounds.
  enum Open_Closed {
    //! An open upper bound.
    OPEN = Old_Boundary::NEG,
    //! A closed upper bound.
    CLOSED = Old_Boundary::ZERO
  };

  //! Builds an upper bound of kind \p f and having value \p v.
  UBoundary(const ERational& v, Open_Closed f);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A interval over the set of rational numbers.
/*! \ingroup PPL_CXX_interface
  An object of class Old_Interval represents a closed/half-closed/open
  interval over the set of rational numbers \f$\Qset\f$.

  Note that, even though the implementation is capable to represent
  any interval on the set of <EM>extended</EM> rational numbers,
  the available public methods only allows for the construction
  and manipulation of intervals over \f$\Qset\f$.
  Namely, it is not possible to create a non-empty interval having
  a <EM>closed</EM> bound whose value is \f$-\infty\f$ or \f$+\infty\f$.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Old_Interval {
public:
  //! Constructs the universe interval \f$(-\infty, +\infty) = \Qset\f$.
  Old_Interval();

  //! Returns <CODE>true</CODE> if and only if \p *this is empty.
  bool is_empty() const;

  //! Returns a const reference to the interval's lower bound.
  const LBoundary& lower_bound() const;

  //! Returns a reference to the interval's lower bound.
  LBoundary& lower_bound();

  //! Returns a const reference to the interval's upper bound.
  const UBoundary& upper_bound() const;

  //! Returns a reference to the interval's upper bound.
  UBoundary& upper_bound();

  /*! \brief
    Raises the interval's lower bound, if \p new_lower is higher
    than the current one.
  */
  void raise_lower_bound(LBoundary new_lower);

  /*! \brief
    Lowers the interval's upper bound, if \p new_upper is lower
    than the current one.
  */
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

#include "Old_Interval.inlines.hh"

#endif // !defined(PPL_Old_Interval_defs_hh)
