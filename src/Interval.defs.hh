/* Declarations for the Interval class and its constituents.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friend later.
bool operator==(const ExtendedRational& x, const ExtendedRational& y);
bool operator!=(const ExtendedRational& x, const ExtendedRational& y);
bool operator< (const ExtendedRational& x, const ExtendedRational& y);
bool operator> (const ExtendedRational& x, const ExtendedRational& y);
bool operator<=(const ExtendedRational& x, const ExtendedRational& y);
bool operator>=(const ExtendedRational& x, const ExtendedRational& y);

} // namespace Parma_Polyhedra_Library


class Parma_Polyhedra_Library::ExtendedRational {
private:
  //! Positive if +infinity, negative if -infinity, zero otherwise.
  int e;
  //! The finite value: valid only if \p e=0.
  mpq_class v;

public:
  ExtendedRational(const mpq_class& n);

  ExtendedRational(const Integer& num, const Integer& den);

  explicit ExtendedRational(char sign);

  ExtendedRational(const ExtendedRational& y);

  ExtendedRational& operator=(const ExtendedRational& y);

  int direction_of_infinity() const;

#if 0
  const Integer& numerator() const;

  const Integer& denominator() const;
#else
  void canonicalize() const;

  Integer numerator() const;

  Integer denominator() const;
#endif

  friend bool
  Parma_Polyhedra_Library::operator==(const ExtendedRational& x,
				      const ExtendedRational& y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const ExtendedRational& x,
				      const ExtendedRational& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const ExtendedRational& x,
				     const ExtendedRational& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const ExtendedRational& x,
				     const ExtendedRational& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const ExtendedRational& x,
				      const ExtendedRational& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const ExtendedRational& x,
				      const ExtendedRational& y);
};  


namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friend later.
bool operator==(const Boundary& x, const Boundary& y);
bool operator!=(const Boundary& x, const Boundary& y);
bool operator< (const Boundary& x, const Boundary& y);
bool operator> (const Boundary& x, const Boundary& y);
bool operator<=(const Boundary& x, const Boundary& y);
bool operator>=(const Boundary& x, const Boundary& y);

bool operator==(const Boundary& x, ExtendedRational y);
bool operator!=(const Boundary& x, ExtendedRational y);
bool operator< (const Boundary& x, ExtendedRational y);
bool operator> (const Boundary& x, ExtendedRational y);
bool operator<=(const Boundary& x, ExtendedRational y);
bool operator>=(const Boundary& x, ExtendedRational y);

bool operator< (const LBoundary& x, const UBoundary& y);
bool operator> (const UBoundary& x, const LBoundary& y);
bool operator<=(const UBoundary& x, const LBoundary& y);
bool operator>=(const LBoundary& x, const UBoundary& y);

bool operator< (const LBoundary& x, ExtendedRational y);
bool operator> (const UBoundary& x, ExtendedRational y);
bool operator<=(const UBoundary& x, ExtendedRational y);
bool operator>=(const LBoundary& x, ExtendedRational y);

} // namespace Parma_Polyhedra_Library


class Parma_Polyhedra_Library::Boundary {
protected:
  enum Flag { NEG = -1, ZERO = 0, POS = 1 };

  ExtendedRational value;
  Flag flag;

  Boundary(const ExtendedRational& v, Flag f);

  friend bool
  Parma_Polyhedra_Library::operator==(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const Boundary& x, const Boundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const Boundary& x, const Boundary& y);

  friend bool
  Parma_Polyhedra_Library::operator==(const Boundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator!=(const Boundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator<(const Boundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator>(const Boundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const Boundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const Boundary& x, ExtendedRational y);

  friend bool
  Parma_Polyhedra_Library::operator< (const LBoundary& x, const UBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator> (const UBoundary& x, const LBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const UBoundary& x, const LBoundary& y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const LBoundary& x, const UBoundary& y);

  friend bool
  Parma_Polyhedra_Library::operator< (const LBoundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator> (const UBoundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator<=(const UBoundary& x, ExtendedRational y);
  friend bool
  Parma_Polyhedra_Library::operator>=(const LBoundary& x, ExtendedRational y);

public:
  bool is_closed() const;

  const ExtendedRational& bound() const;
};


class Parma_Polyhedra_Library::LBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::POS, CLOSED = Boundary::ZERO };

  LBoundary();
  LBoundary(const ExtendedRational& v, OpenClosed f);

  bool OK() const;
};

class Parma_Polyhedra_Library::UBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::NEG, CLOSED = Boundary::ZERO };

  UBoundary();
  UBoundary(const ExtendedRational& v, OpenClosed f);

  bool OK() const;
};

class Parma_Polyhedra_Library::Interval {
public:
  //! Construct the interval (-infinity, +infinity).
  Interval();

  bool is_empty() const;

  const LBoundary& lower_bound() const;

  const UBoundary& upper_bound() const;

  void raise_lower_bound(LBoundary new_lower);

  void lower_upper_bound(UBoundary new_upper);

  void set_empty();

  bool OK() const;

private:
  LBoundary lower;
  UBoundary upper;
};


#include "Interval.inlines.hh"

#endif // !defined(PPL_Interval_defs_hh)
