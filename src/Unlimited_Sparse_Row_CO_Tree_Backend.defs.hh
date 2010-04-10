/* Unlimited_Sparse_Row_CO_Tree_Backend class declaration.
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

#ifndef PPL_Unlimited_Sparse_Row_CO_Tree_Backend_defs_hh
#define PPL_Unlimited_Sparse_Row_CO_Tree_Backend_defs_hh 1

#include "Unlimited_Sparse_Row_CO_Tree_Backend.types.hh"
#include "CO_Tree.defs.hh"

namespace Parma_Polyhedra_Library {

class Unlimited_Sparse_Row_CO_Tree_Backend {

private:
  typedef Unlimited_Sparse_Row_CO_Tree_Backend This;

public:

  typedef CO_Tree::value_type value_type;

  Unlimited_Sparse_Row_CO_Tree_Backend();

  Unlimited_Sparse_Row_CO_Tree_Backend(const This& x);

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  bool OK() const;

  This& operator=(const This& x);

  void swap(This& x);

  class dangerous_iterator;
  class iterator;
  class const_iterator;

  dangerous_iterator begin_dangerous();
  dangerous_iterator end_dangerous();

  iterator begin();
  iterator end();

  const_iterator begin() const;
  const_iterator end() const;

  dangerous_iterator find_dangerous(const dimension_type c);
  dangerous_iterator lower_bound_dangerous(const dimension_type c);
  iterator find(const dimension_type c);
  iterator lower_bound(const dimension_type c);
  const_iterator find(const dimension_type c) const;
  const_iterator lower_bound(const dimension_type c) const;

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  dangerous_iterator find_dangerous(const dimension_type c,
                                    dangerous_iterator itr);
  //! Lower bound of key c, assuming it is in [itr,end()) .
  dangerous_iterator lower_bound_dangerous(const dimension_type c,
                                           dangerous_iterator itr);

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  iterator find(const dimension_type c, iterator itr);
  //! Lower bound of key c, assuming it is in [itr,end()) .
  iterator lower_bound(const dimension_type c, iterator itr);

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  const_iterator find(const dimension_type c, const_iterator itr) const;
  //! Lower bound of key c, assuming it is in [itr,end()) .
  const_iterator lower_bound(const dimension_type c,
                             const_iterator itr) const;

  //! A faster equivalent of
  //! itr1=find_dangerous(c1); itr2=find_dangerous(c2); .
  void find2_dangerous(const dimension_type c1, const dimension_type c2,
                       dangerous_iterator& itr1, dangerous_iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1, const dimension_type c2,
             iterator& itr1, iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1,const dimension_type c2,
             const_iterator& itr1,const_iterator& itr2) const;

  //! Inserts x before pos and returns an iterator to the inserted element.
  //! This operation invalidates all C::dangerous_iterator objects equal to
  //! pos.
  dangerous_iterator insert(dangerous_iterator pos, const value_type& x);

  //! Inserts the pair (i, x) before pos and returns an iterator to the
  //! inserted element.
  //! This operation invalidates all dangerous_iterator objects equal to pos.
  dangerous_iterator insert(dangerous_iterator pos, dimension_type i,
                            const Coefficient& x);

  //! Equivalent to insert(end_dangerous(), x).
  void push_back(const value_type& x);

  //! Erases the element pointed to by pos.
  //! This operation invalidates all C::dangerous_iterators objects equal to
  //! pos and ++pos.
  dangerous_iterator erase(dangerous_iterator pos);

  //! Erases the element in [first,last).
  //!  This operation invalidates all dangerous_iterators equal to last.
  dangerous_iterator erase(dangerous_iterator first, dangerous_iterator last);

  //! Moves all elements in x before position. This operation invalidates all
  //! dangerous_iterators equal to the former position and all
  //! dangerous_iterators pointing to x.
  //! The returned iterator is a valid iterator pointing to the first inserted
  //! element. \p position is modified to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position, This& x);

  //! Moves element i of x before position. This operation invalidates all
  //! dangerous_iterators equal to the former position, i and ++i.
  //! The returned iterator is a valid iterator pointing to the inserted
  //! element. \p position is modified to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position, This& x,
                            dangerous_iterator i);

  //! Moves elements [first,last) in x before position. This operation
  //! invalidates all dangerous_iterators equal to the former position, and in
  //! [first,last] (note that last is invalidated, too). The returned iterator
  //! is a valid iterator pointing to the first moved element.
  //! \p position is modified to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position, This& x,
                            dangerous_iterator first,
                            dangerous_iterator last);


private:

  CO_Tree tree;
};

class Unlimited_Sparse_Row_CO_Tree_Backend::iterator {

public:

  typedef std::bidirectional_iterator_tag iterator_category;
  typedef Unlimited_Sparse_Row_CO_Tree_Backend::value_type value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  iterator(CO_Tree* x = 0);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  iterator& operator++();
  iterator& operator--();

  value_type& operator*();
  value_type* operator->();

  const value_type& operator*() const;
  const value_type* operator->() const;

  operator const_iterator() const;

private:
  CO_Tree::inorder_iterator itr;
};

class Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
  : public Unlimited_Sparse_Row_CO_Tree_Backend::iterator {

public:

  dangerous_iterator(CO_Tree* x = 0);
  dangerous_iterator(const iterator& itr);

  //! Returns a dangerous_iterator pointing to the element after i.
  static dangerous_iterator next(const iterator& i);

  operator const_iterator() const;

private:

};

class Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator {

public:

  typedef std::bidirectional_iterator_tag iterator_category;
  typedef const Unlimited_Sparse_Row_CO_Tree_Backend::value_type
    value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  const_iterator(const CO_Tree* x = 0);
  const_iterator(const CO_Tree::inorder_iterator& x);
  const_iterator(const CO_Tree::inorder_const_iterator& x);

  const_iterator& operator++();
  const_iterator& operator--();

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

  value_type& operator*() const;
  value_type* operator->() const;

private:
  CO_Tree::inorder_const_iterator itr;
};

} // namespace Parma_Polyhedra_Library

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Unlimited_Sparse_Row_CO_Tree_Backend */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Unlimited_Sparse_Row_CO_Tree_Backend& x,
          Parma_Polyhedra_Library::Unlimited_Sparse_Row_CO_Tree_Backend& y);

} // namespace std

#include "Unlimited_Sparse_Row_CO_Tree_Backend.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_CO_Tree_Backend_defs_hh)
