/* Saturation_Row class declaration.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Saturation_Row_defs_hh
#define PPL_Saturation_Row_defs_hh 1

#include "Saturation_Row.types.hh"
#include "globals.types.hh"
#include <iosfwd>
#include <gmp.h>
#include <vector>

namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friends later.

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are equal.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator==(const Saturation_Row& x, const Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are not equal.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator!=(const Saturation_Row& x, const Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The basic comparison function.
/*! \relates Saturation_Row
  Compares \p x with \p y starting from the least significant bits.
  The ordering is total and has the following property: if \p x and \p y
  are two rows seen as sets of naturals, if \p x is a strict subset
  of \p y, then \p x comes before \p y.

  Returns
  - -1 if \p x comes before \p y in the ordering;
  -  0 if \p x and \p y are equal;
  -  1 if \p x comes after \p y in the ordering.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int compare(const Saturation_Row& x, const Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Set-theoretic inclusion test.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool subset_or_equal(const Saturation_Row& x, const Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Set-theoretic inclusion test: sets \p strict_subset to a boolean
//! indicating whether the inclusion is strict or not.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool subset_or_equal(const Saturation_Row& x, const Saturation_Row& y,
		     bool& strict_subset);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Set-theoretic strict inclusion test.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool strict_subset(const Saturation_Row& x, const Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Set-theoretic union.
/*! \relates Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void set_union(const Saturation_Row& x,
	       const Saturation_Row& y,
	       Saturation_Row& z);

} // namespace Parma_Polyhedra_Library

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A row of a saturation matrix.
/*!
  An object of this class represents a single row of a saturation matrix.
  The saturation row corresponds to a constraint and a system of generators
  (resp., a generator and a system of constraints) and records whether or
  not the constraint is saturated by each one of the generators (resp.,
  the generator saturates each one of the constraints).

  The saturation relation is encoded by using a bitset, so that the
  constraint is saturated by the \f$i\f$-th generator in the system
  (resp., the generator saturates the \f$i\f$-th constraint in the system)
  if and only if the \f$i\f$-th bit is not set.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Saturation_Row {
public:
  //! Default constructor.
  Saturation_Row();

  //! Copy-constructor.
  Saturation_Row(const Saturation_Row& y);

  //! Destructor.
  ~Saturation_Row();

  //! Assignment operator.
  Saturation_Row& operator=(const Saturation_Row& y);

  //! Swaps \p *this with \p y.
  void swap(Saturation_Row& y);

  //! Returns the truth value corresponding to the bit in position \p k.
  bool operator[](unsigned long k) const;

  //! Sets the bit in position \p k.
  void set(unsigned long k);

  //! Clears the bit in position \p k.
  void clear(unsigned long k);

  //! Clears bits from position \p k (included) onward.
  void clear_from(unsigned long k);

  //! Clears all the bits of the row.
  void clear();

  friend int
  Parma_Polyhedra_Library::compare(const Saturation_Row& x,
				   const Saturation_Row& y);

  friend bool
  Parma_Polyhedra_Library::operator==(const Saturation_Row& x,
				      const Saturation_Row& y);

  friend bool
  Parma_Polyhedra_Library::operator!=(const Saturation_Row& x,
				      const Saturation_Row& y);

  friend bool
  Parma_Polyhedra_Library::subset_or_equal(const Saturation_Row& x,
					   const Saturation_Row& y);

  friend bool
  Parma_Polyhedra_Library::subset_or_equal(const Saturation_Row& x,
					   const Saturation_Row& y,
					   bool& strict_subset);

  friend bool
  Parma_Polyhedra_Library::strict_subset(const Saturation_Row& x,
					 const Saturation_Row& y);

  friend void
  Parma_Polyhedra_Library::set_union(const Saturation_Row& x,
				     const Saturation_Row& y,
				     Saturation_Row& z);

  //! Returns the index of the first set bit or ULONG_MAX if no bit is set.
  unsigned long first() const;

  //! \brief
  //! Returns the index of the first set bit after \p position
  //! or ULONG_MAX if no bit after \p position is set.
  unsigned long next(unsigned long position) const;

  //! Returns the index of the last set bit or ULONG_MAX if no bit is set.
  unsigned long last() const;

  //! \brief
  //! Returns the index of the first set bit before \p position
  //! or ULONG_MAX if no bits before \p position is set.
  unsigned long prev(unsigned long position) const;

  //! Returns the number of set bits in the row.
  unsigned long count_ones() const;

  //! Returns <CODE>true</CODE> if no bit is set in the row.
  bool empty() const;

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks if all the invariants are satisfied
  bool OK() const;

private:
  //! Bit-vector representing the row.
  mpz_t vec;

  //! Assuming \p w is nonzero, returns the index of the first set bit in \p w.
  static unsigned int first_one(mp_limb_t w);

  //! Assuming \p w is nonzero, returns the index of the last set bit in \p w.
  static unsigned int last_one(mp_limb_t w);
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Saturation_Row& x,
	  Parma_Polyhedra_Library::Saturation_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::iter_swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Saturation_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
iter_swap(std::vector<Parma_Polyhedra_Library::Saturation_Row>::iterator x,
	  std::vector<Parma_Polyhedra_Library::Saturation_Row>::iterator y);

} // namespace std

#include "Saturation_Row.inlines.hh"

#endif // !defined(PPL_Saturation_Row_defs_hh)
