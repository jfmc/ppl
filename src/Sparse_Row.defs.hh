/* Sparse_Row class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Sparse_Row_defs_hh
#define PPL_Sparse_Row_defs_hh 1

#include "Sparse_Row.types.hh"
#include "Unlimited_Sparse_Row.defs.hh"
#include "Coefficient.defs.hh"
#include <list>
#include <vector>

namespace Parma_Polyhedra_Library {

//! A finite sparse sequence of coefficients.
class Sparse_Row {

public:
  typedef Unlimited_Sparse_Row::key_type key_type;
  typedef Unlimited_Sparse_Row::data_type data_type;
  typedef Unlimited_Sparse_Row::value_type value_type;
  typedef Unlimited_Sparse_Row::pointer pointer;
  typedef Unlimited_Sparse_Row::reference reference;
  typedef Unlimited_Sparse_Row::size_type size_type;

  //! Constructs a row from a std::vector.
  Sparse_Row(const std::vector<data_type>& v);

  //! Constructs a row of the specified size.
  Sparse_Row(size_type n=0);

  //! Constructs a row of the specified size from an Unlimited_Sparse_Row.
  Sparse_Row(const Unlimited_Sparse_Row &x,size_type n);

  //! This method, with this signature, is needed for compatibility with
  //! Dense_Row. It can be called on any row, and it resizes it to \p sz.
  void construct(dimension_type sz);

  //! This method, with this signature, is needed for compatibility with
  //! Dense_Row. It can be called on any row, and it resizes it to \p sz.
  void construct(dimension_type sz, dimension_type capacity);

  //! Resizes the row to the specified size.
  void resize(size_type n);

  //! Returns the size of the row.
  size_type size() const;

private:
  Unlimited_Sparse_Row row;
  size_type size_;

public:
  //! A const iterator that may skip some zeros in the sequence.
  typedef Unlimited_Sparse_Row::const_iterator const_iterator;

  //! An iterator that may skip some zeros in the sequence.
  typedef Unlimited_Sparse_Row::iterator iterator;

  //! Resets to zero the value pointed by i.
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  iterator reset(iterator first,iterator last);

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  iterator find(const key_type &c);
  iterator lower_bound(const key_type &c);
  iterator upper_bound(const key_type &c);
  const_iterator find(const key_type &c) const;
  const_iterator lower_bound(const key_type &c) const;
  const_iterator upper_bound(const key_type &c) const;

  operator const Unlimited_Sparse_Row&() const;

  PPL_OUTPUT_DECLARATIONS

  bool ascii_load(std::istream& s);

  //! Checks the invariant.
  bool OK() const;
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Sparse_Row_defs_hh)
