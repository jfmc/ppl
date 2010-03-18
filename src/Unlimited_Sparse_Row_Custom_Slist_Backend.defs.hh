/* Unlimited_Sparse_Row_Custom_Slist_Backend class declaration.
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

#ifndef PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_defs_hh
#define PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_defs_hh 1

#include "Unlimited_Sparse_Row_Custom_Slist_Backend.types.hh"
#include "Coefficient.defs.hh"

namespace Parma_Polyhedra_Library {

class Unlimited_Sparse_Row_Custom_Slist_Backend {

private:
  //! Saves a lot of typing and line breaks.
  typedef Unlimited_Sparse_Row_Custom_Slist_Backend This;

  template <typename Compare>
  class value_key_comparison;

  template <typename Compare>
  class key_value_comparison;

  template <typename Compare>
  static
  Unlimited_Sparse_Row_Custom_Slist_Backend::value_key_comparison<Compare>
  value_key_compare(const Compare& comp);

  template <typename Compare>
  static
  Unlimited_Sparse_Row_Custom_Slist_Backend::key_value_comparison<Compare>
  key_value_compare(const Compare& comp);

public:
  typedef std::pair<dimension_type,Coefficient> value_type;

  //! An iterator on the list's elements. Warning: unlike iterator, operations
  //! that add or remove adjacent elements may invalidate the iterator, see
  //! their documentation for details.
  class dangerous_iterator;

  class iterator;
  class const_iterator;

  //! Constructs an empty list.
  Unlimited_Sparse_Row_Custom_Slist_Backend();

  //! Copy constructor.
  Unlimited_Sparse_Row_Custom_Slist_Backend(const This& x);

  //! Destructor.
  ~Unlimited_Sparse_Row_Custom_Slist_Backend();

  //! Deletes all the elements in the list.
  void clear();

  //! Returns \p true if the list is empty.
  bool empty() const;

  //! Adds \p x at the beginning of the list. If the list is empty, end()
  //! dangerous_iterators are invalidated, otherwise dangerous_iterators to
  //! the former first element are invalidated.
  void push_front(const value_type& x);

  //! Adds \p x at the end of the list. end() dangerous_iterators are
  //! invalidated.
  void push_back(const value_type& x);

  //! Removes the first element. dangerous_iterators pointing to the
  //! former *second* element are invalidated.
  /*!
    pop_back() is not implemented because it would be O(n).
    */
  void pop_front();

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
  iterator find(const dimension_type c,iterator itr);
  //! Lower bound of key c, assuming it is in [itr,end()) .
  iterator lower_bound(const dimension_type c,iterator itr);

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  const_iterator find(const dimension_type c,const_iterator itr) const;
  //! Lower bound of key c, assuming it is in [itr,end()) .
  const_iterator lower_bound(const dimension_type c,const_iterator itr) const;

  //! A faster equivalent of
  //! itr1=find_dangerous(c1); itr2=find_dangerous(c2); .
  void find2_dangerous(const dimension_type c1,const dimension_type c2,
                       dangerous_iterator& itr1,dangerous_iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1,const dimension_type c2,
             iterator& itr1,iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1,const dimension_type c2,
             const_iterator& itr1,const_iterator& itr2) const;

  //! Inserts the pair (i,x) before pos. Warning: this operation invalidates
  //! all dangerous_iterators equal to pos. Returns an iterator to the added
  //! element.
  dangerous_iterator insert(dangerous_iterator pos,dimension_type i,
                            const Coefficient& x);

  //! Inserts the pair x before pos. Warning: this operation invalidates
  //! all dangerous_iterators equal to pos. Returns an iterator to the added
  //! element.
  dangerous_iterator insert(dangerous_iterator pos,
                            const std::pair<dimension_type,Coefficient>& x);

  //! Erases the element pointed to by pos. Warning: this operation
  //! invalidates all dangerous_iterators equal to ++pos.
  dangerous_iterator erase(dangerous_iterator pos);

  //! Erases the element in [first,last). This operation invalidates all
  //! dangerous_iterators equal to last.
  dangerous_iterator erase(dangerous_iterator first,dangerous_iterator last);

  //! Moves all elements of the list x before position. This operation
  //! invalidates all dangerous_iterators equal to position and all
  //! dangerous_iterators pointing to x.
  //! Returns an iterator pointing to the first added element.
  //! \p position is updated to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position,This& x);

  //! Moves element i of the list x before position. This operation
  //! invalidates all dangerous_iterators equal to position, and ++i.
  //! Returns an iterator pointing to the added element.
  //! \p position is updated to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position,This& x,
                            dangerous_iterator i);

  //! Moves element [first,last) of the list x before position. This operation
  //! invalidates all dangerous_iterators equal to position and last.
  //! Returns an iterator pointing to the first added element.
  //! \p position is updated to keep it valid.
  dangerous_iterator splice(dangerous_iterator& position,This& x,
                            dangerous_iterator first,dangerous_iterator last);

  //! Assigns \p x to \p (*this) .
  This& operator=(const This& x);

  //! Swaps *this and x.
  void swap(This& x);

  bool operator==(const This& x) const;
  bool operator!=(const This& x) const;

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  bool OK() const;

private:
  //! The type of the elements of the list.
  //! The last element has \p next==NULL .
  struct list_elem {
    list_elem();
    list_elem(const value_type& data1,list_elem* next1);
    list_elem(dimension_type i,const Coefficient& x,list_elem* next1);
    value_type data;
    list_elem* next;
  };

  //! A pointer to the first element of the list.
  //! It is NULL when the list is empty.
  list_elem* first;

  //! If the list is empty, points to \p first .
  //! Otherwise, points to the \p next member of the last element.
  //! This is needed for O(1) push_back() operations.
  list_elem** last;
};

class Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator {

public:

  typedef std::forward_iterator_tag iterator_category;
  typedef Unlimited_Sparse_Row_Custom_Slist_Backend::value_type value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  dangerous_iterator(list_elem** p1=NULL);

  static dangerous_iterator next(iterator i);

  value_type& operator*();
  value_type* operator->();

  dangerous_iterator& operator++();
  dangerous_iterator operator++(int);

  bool operator==(const dangerous_iterator& x) const;
  bool operator!=(const dangerous_iterator& x) const;

  operator iterator();
  operator const_iterator() const;

  bool OK() const;

private:
#ifndef NDEBUG
  // Needed only by OK()
  list_elem* q;
#endif

  list_elem** p;

  friend class Unlimited_Sparse_Row_Custom_Slist_Backend;
};

class Unlimited_Sparse_Row_Custom_Slist_Backend::iterator {

public:

  typedef std::forward_iterator_tag iterator_category;
  typedef Unlimited_Sparse_Row_Custom_Slist_Backend::value_type value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  iterator(list_elem* p1=NULL);

  value_type& operator*();
  value_type* operator->();

  iterator& operator++();
  iterator operator++(int);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  operator const_iterator() const;

private:
  list_elem* p;

  friend class Unlimited_Sparse_Row_Custom_Slist_Backend;
  // For next().
  friend class Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator;
};

class Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator {

public:
  typedef std::forward_iterator_tag iterator_category;
  typedef const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type
    value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  const_iterator(list_elem* const p1=NULL);

  value_type& operator*() const;
  value_type* operator->() const;

  const_iterator& operator++();
  const_iterator operator++(int);

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

private:
  const list_elem* p;

  friend class Unlimited_Sparse_Row_Custom_Slist_Backend;
};

template <typename Compare>
class Unlimited_Sparse_Row_Custom_Slist_Backend::value_key_comparison
  : public std::binary_function<Unlimited_Sparse_Row_Custom_Slist_Backend
                                ::value_type&,
                                dimension_type, bool> {
public:
  value_key_comparison(const Compare& comp);

  bool operator()(const
                  Unlimited_Sparse_Row_Custom_Slist_Backend::value_type& x,
                  const dimension_type y) const;

private:
  Compare comp_;
};

template <typename Compare>
class Unlimited_Sparse_Row_Custom_Slist_Backend::key_value_comparison
  : public std::binary_function<dimension_type,
                                Unlimited_Sparse_Row_Custom_Slist_Backend
                                ::value_type&, bool> {
public:
  key_value_comparison(const Compare& comp);

  bool operator()(const dimension_type x,
                  const Unlimited_Sparse_Row_Custom_Slist_Backend
                  ::value_type& y) const;

private:
  Compare comp_;
};
}

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Unlimited_Sparse_Row_Custom_Slist_Backend
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(
  Parma_Polyhedra_Library::Unlimited_Sparse_Row_Custom_Slist_Backend& x,
  Parma_Polyhedra_Library::Unlimited_Sparse_Row_Custom_Slist_Backend& y);

} // namespace std


#include "Unlimited_Sparse_Row_Custom_Slist_Backend.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_defs_hh)
