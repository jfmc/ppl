/* Declarations for the Interval class and its constituents.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

// Put them in the namespace here to declare them friend later.
bool operator==(const ERational& x, const ERational& y);
bool operator!=(const ERational& x, const ERational& y);
bool operator<=(const ERational& x, const ERational& y);
bool operator>=(const ERational& x, const ERational& y);
bool operator<(const ERational& x, const ERational& y);
bool operator>(const ERational& x, const ERational& y);

namespace IO_Operators {

std::ostream& operator<<(std::ostream& s, const ERational& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


class Parma_Polyhedra_Library::ERational {
public:
  ERational(const Integer& num, const Integer& den);

  explicit ERational(char sign);

  ERational(const ERational& y);

  ERational& operator=(const ERational& y);

  int direction_of_infinity() const;

  const Integer& numerator() const;

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
  //! Positive if +infinity, negative if -infinity, zero otherwise.
  int e;

  //! The finite value: valid only if \p e=0.
  mpq_class v;
};


namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friend later.
bool operator==(const Boundary& x, const Boundary& y);
bool operator!=(const Boundary& x, const Boundary& y);
bool operator<=(const Boundary& x, const Boundary& y);
bool operator>=(const Boundary& x, const Boundary& y);
bool operator<(const Boundary& x, const Boundary& y);
bool operator>(const Boundary& x, const Boundary& y);

bool operator==(const Boundary& x, const ERational& y);
bool operator!=(const Boundary& x, const ERational& y);
bool operator<=(const Boundary& x, const ERational& y);
bool operator>=(const Boundary& x, const ERational& y);
bool operator<(const Boundary& x, const ERational& y);
bool operator>(const Boundary& x, const ERational& y);

bool operator<=(const UBoundary& x, const LBoundary& y);
bool operator>=(const LBoundary& x, const UBoundary& y);
bool operator<(const LBoundary& x, const UBoundary& y);
bool operator>(const UBoundary& x, const LBoundary& y);

bool operator<=(const UBoundary& x, const ERational& y);
bool operator>=(const LBoundary& x, const ERational& y);
bool operator<(const LBoundary& x, const ERational& y);
bool operator>(const UBoundary& x, const ERational& y);

} // namespace Parma_Polyhedra_Library


class Parma_Polyhedra_Library::Boundary {
protected:
  enum Flag { NEG = -1, ZERO = 0, POS = 1 };

  ERational value;
  Flag flag;

  Boundary(const ERational& v, Flag f);

  friend bool
  Parma_Polyhedra_Library::operator==(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Boundary& x, const Boundary& y);

  friend bool
  Parma_Polyhedra_Library::operator==(const Boundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const Boundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const Boundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const Boundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const Boundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Boundary& x, const ERational& y);

  friend bool
  Parma_Polyhedra_Library::operator<=(const UBoundary& x, const LBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const LBoundary& x, const UBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const LBoundary& x, const UBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const UBoundary& x, const LBoundary& y);

  friend bool
  Parma_Polyhedra_Library::operator<=(const UBoundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const LBoundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const LBoundary& x, const ERational& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const UBoundary& x, const ERational& y);

public:
  bool is_closed() const;

  const ERational& bound() const;
};


class Parma_Polyhedra_Library::LBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::POS, CLOSED = Boundary::ZERO };

  LBoundary();
  LBoundary(const ERational& v, OpenClosed f);

  bool OK() const;
};

class Parma_Polyhedra_Library::UBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::NEG, CLOSED = Boundary::ZERO };

  UBoundary();
  UBoundary(const ERational& v, OpenClosed f);

  bool OK() const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  An object of this class is an interval (finite, infinite, open,
  closed or half-closed).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Interval {
public:
  //! Construct the interval (-infinity, +infinity).
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

  //! Turn \p *this into the empty interval.
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
