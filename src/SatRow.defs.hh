/* SatRow class declaration.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _SatRow_defs_hh
#define _SatRow_defs_hh 1

#include "BitSet.defs.hh"
#include <iosfwd>
#include "SatRow.types.hh"

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friends later.
  int compare(const SatRow& x, const SatRow& y);
  bool operator ==(const SatRow& x, const SatRow& y);
  bool operator !=(const SatRow& x, const SatRow& y);
  bool operator  <(const SatRow& x, const SatRow& y);
  bool operator  >(const SatRow& x, const SatRow& y);
  bool operator <=(const SatRow& x, const SatRow& y);
  bool operator >=(const SatRow& x, const SatRow& y);
  void set_union(const SatRow& x, const SatRow& y, SatRow& z);
}

class Parma_Polyhedra_Library::SatRow {
private:
  //! Set of Bits representing the row.
  BitSet vec;
public:
  //! Default constructor.
  SatRow();
  //! Copy-constructor.
  SatRow(const SatRow& y);
  //! Destructor.
  ~SatRow();

  //! Assignment operator.
  SatRow& operator =(const SatRow& y);

  //! Swaps \p *this with \p y.
  void swap(SatRow& y);

  bool operator [](size_t k) const;

  //! Sets the bit in position \p i.
  void set(size_t i);
  //! Clears the bit in position \p i.
  void clear(size_t i);
  //! Clears bits from position \p i (included) onwards.
  void clear_from(size_t i);

  //! Clears all the bit of the row.
  void clear();

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! The basic comparison function.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  friend int Parma_Polyhedra_Library::compare(const SatRow& x,
					      const SatRow& y);

  //! Set-theoretic comparisons.
  //@{
  friend bool Parma_Polyhedra_Library::operator ==(const SatRow& x,
						   const SatRow& y);
  friend bool Parma_Polyhedra_Library::operator !=(const SatRow& x,
						   const SatRow& y);
  friend bool Parma_Polyhedra_Library::operator  <(const SatRow& x,
						   const SatRow& y);
  friend bool Parma_Polyhedra_Library::operator  >(const SatRow& x,
						   const SatRow& y);
  friend bool Parma_Polyhedra_Library::operator <=(const SatRow& x,
						   const SatRow& y);
  friend bool Parma_Polyhedra_Library::operator >=(const SatRow& x,
						   const SatRow& y);
  //@}

  //! Set-theoretic union.
  friend void Parma_Polyhedra_Library::set_union(const SatRow& x,
						 const SatRow& y,
						 SatRow& z);

  //! Return the size of the row.
  size_t size();

  //! Returns the index of the first set bit and 
  //! -1 if no bit is set.
  int first() const;

  //! Returns the index of the first set bit after \p pos 
  //! and -1 if no bit after \p pos is set.
  int next(int pos) const;

  //! Returns the index of the last set bit and -1 if no bit is set.
  int last() const;

  //! Returns the index of the first set bit before \p pos 
  //! and -1 if no bits before \p pos is set.
  int prev(int pos) const;

  //! Returns the number of set bits in the row.
  size_t count_ones() const;

  //! Returns <CODE>true</CODE> if no bit is set in the row.
  bool empty() const;

  bool OK() const;
};

namespace Parma_Polyhedra_Library {
  std::ostream& operator <<(std::ostream& s, const SatRow& r);
}


namespace std {
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*!
    Specialize std::swap to use the fast swap that is provided
    as a member function instead of using the default algorithm
    (which creates a temporary and uses assignment).
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void swap(Parma_Polyhedra_Library::SatRow& x,
	    Parma_Polyhedra_Library::SatRow& y);
}

#if !OUTLINE
#include "SatRow.inlines.hh"
#endif

#endif
