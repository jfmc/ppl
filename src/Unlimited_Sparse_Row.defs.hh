/* Unlimited_Sparse_Row class declaration.
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

#ifndef PPL_Unlimited_Sparse_Row_defs_hh
#define PPL_Unlimited_Sparse_Row_defs_hh 1

#include "Unlimited_Sparse_Row.types.hh"
#include "Coefficient.defs.hh"
#include <list>
#include <vector>

namespace Parma_Polyhedra_Library {

//! A finite, but unlimited (i.e. has no size) sparse sequence of coefficients.
class Unlimited_Sparse_Row {

public:
  typedef std::pair<const dimension_type,Coefficient> value_type;

  template <typename Compare>
  class value_key_comparison;

  template <typename Compare>
  class key_value_comparison;

  template <typename Compare>
  static Unlimited_Sparse_Row::value_key_comparison<Compare>
  value_key_compare(Compare comp);

  template <typename Compare>
  static Unlimited_Sparse_Row::key_value_comparison<Compare>
  key_value_compare(Compare comp);

  //! Constructs an unlimited row of zeroes.
  Unlimited_Sparse_Row();

  //! Constructs an unlimited row from a std::vector.
  Unlimited_Sparse_Row(const std::vector<Coefficient> &v);

private:
  typedef std::list<std::pair<dimension_type,Coefficient> > list_t;

public:
  //! A const iterator that may skip some zeros in the sequence.
  typedef list_t::const_iterator const_iterator;

  //! An iterator that may skip some zeros in the sequence.
  class iterator;

  //! Resets to zero the value pointed to by i.
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  iterator reset(iterator first,iterator last);

  //! Resets to zero the i-th element.
  void reset(dimension_type i);

  //! Resets to zero the elements in [i,j).
  void reset(dimension_type i,dimension_type j);

  //! Resets to zero the elements in [i,+infinity).
  void reset_after(dimension_type i);

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  //! Executes func on each non-zero element and may execute it on some zeros.
  //! func should take a (Coefficient&) or (const Coefficient&) argument.
  template <typename Func>
  void for_each_nonzero(Func func);

  //! Executes func on each non-zero element and may execute it on some zeros.
  //! func should take a (const Coefficient&) argument.
  template <typename Func>
  void for_each_nonzero(Func func) const;

  iterator find(const dimension_type c);
  iterator lower_bound(const dimension_type c);
  iterator upper_bound(const dimension_type c);
  const_iterator find(const dimension_type c) const;
  const_iterator lower_bound(const dimension_type c) const;
  const_iterator upper_bound(const dimension_type c) const;

  bool operator==(const Unlimited_Sparse_Row &x) const;
  bool operator!=(const Unlimited_Sparse_Row &x) const;

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Checks the invariant.
  bool OK() const;

private:
  //! The std::list that contains the coefficients
  list_t data;
};

class Unlimited_Sparse_Row::iterator {

public:
  typedef list_t::iterator::iterator_category iterator_category;
  typedef std::pair<const dimension_type,Coefficient> value_type;
  typedef ptrdiff_t difference_type;
  typedef std::pair<const dimension_type,Coefficient>* pointer;
  typedef std::pair<const dimension_type,Coefficient&> reference;

  iterator();

  reference operator*();

  iterator& operator++();
  iterator operator++(int);

  iterator& operator--();
  iterator operator--(int);

  bool operator==(const iterator &x) const;
  bool operator!=(const iterator &x) const;

  operator Unlimited_Sparse_Row::const_iterator();

private:
  iterator(list_t::iterator i);

  list_t::iterator itr;

  friend class Unlimited_Sparse_Row;
};

template <typename Compare>
class Unlimited_Sparse_Row::value_key_comparison
  : public std::binary_function<Unlimited_Sparse_Row::value_type,
                                dimension_type, bool> {
public:
  value_key_comparison(Compare comp);

  bool operator()(const Unlimited_Sparse_Row::value_type& x,
                  const dimension_type y) const;

private:
  Compare comp_;
};

template <typename Compare>
class Unlimited_Sparse_Row::key_value_comparison
  : public std::binary_function<dimension_type,
                                Unlimited_Sparse_Row::value_type, bool> {
public:
  key_value_comparison(Compare comp);

  bool operator()(const dimension_type x,
                  const Unlimited_Sparse_Row::value_type& y) const;

private:
  Compare comp_;
};

} // namespace Parma_Polyhedra_Library

#include "Unlimited_Sparse_Row.templates.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_defs_hh)
