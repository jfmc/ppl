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

#ifndef USE_PPL_SPARSE_BACKEND_STD_LIST
#ifndef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST

// No sparse backend defined, assuming Std_List backend
#define USE_PPL_SPARSE_BACKEND_STD_LIST

#endif // !defined(USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST)
#endif // !defined(USE_PPL_SPARSE_BACKEND_STD_LIST)

#ifdef USE_PPL_SPARSE_BACKEND_STD_LIST
#include "Unlimited_Sparse_Row_Std_List_Backend.defs.hh"

// If other options are specified, ignore them.
#undef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
#include "Unlimited_Sparse_Row_Custom_Slist_Backend.defs.hh"
#endif


#include "Coefficient.defs.hh"
#include <list>
#include <vector>

namespace Parma_Polyhedra_Library {

//! A finite, but unlimited (i.e. has no size) sparse sequence of
//! coefficients.
/*!
  The storage backend can be configured at build time by defining the
  appropriate macro. The default storage backend is std::list.

  Unlimited_Sparse_Row::dangerous_iterator is an iterator provided by the
  backend, that may be invalidated by some operations. Read the method
  documentation for more information.

  Other storage backend can be added, if they meet the following requirements.
  Here, we call C the candidate class to be used as backend.

  * C must have a default constructor, a copy constructor and an assignment
    operator.
  * C must have three iterator types: C::const_iterator, C::iterator and
    C::dangerous_iterator.
  * Conversion operators must be defined from C::dangerous_iterator to
    C::iterator, from C::dangerous_iterator to C::const_iterator and from
    C::iterator to C::const_iterator.
  * C must have const begin() and end() methods that return a
    C::const_iterator .
  * C must have non-const begin() and end() methods that return a
    C::dangerous_iterator .
  * C::const_iterator, C::iterator and C::dangerous_iterator must meet the
    forward-iterator requirements and have typedefs (or nested types) for
    iterator_category, value_type, difference_type, pointer and reference.
  * C::value_type should be \p std::pair<dimension_type,Coefficient> .
  * C must have a swap() method.
  * C must have a const OK() method, returning bool.
  * C::const_iterator must have a default constructor
  * C::const_iterator must define operator*(), operator->(), and operator ==
    and !=.
  * C::iterator must have a default constructor
  * C::iterator must define operator*() returning a C::iterator::reference, a
    non-const reference.
  * C::iterator must define operator->() returning a C::iterator::pointer, a
    non-const pointer.
  * C::iterator must have operator == and !=.
  * C::dangerous_iterator must follow all the requirements for C::iterator.
  * C must have a method
    insert(C::dangerous_iterator pos,const C::value_type& x)
    returning a C::dangerous_iterator. This method inserts x before pos and
    returns an iterator to the inserted element.
    This operation invalidates all C::dangerous_iterator objects equal to pos.
  * C must have a method erase(C::dangerous_iterator pos) returning a
    C::dangerous_iterator, that erases the element pointed to by pos.
    This operation invalidates all C::dangerous_iterators objects equal to
    pos and ++pos.
  * C must have a method
    erase(C::dangerous_iterator first,C::dangerous_iterator last) that returns
    a C::dangerous_iterator, that erases the element in [first,last).
    This operation invalidates all dangerous_iterators equal to last.
  * C must have a method splice(C::dangerous_iterator& position,C& x) that
    returns a C::dangerous_iterator, that moves all elements in x before
    position. This operation invalidates all dangerous_iterators equal to
    the former position and all dangerous_iterators pointing to x.
    The returned iterator is a valid iterator pointing to the first inserted
    element. \p position is modified to keep it valid.
  * C must have a method
    splice(C::dangerous_iterator& position,C& x,C::dangerous_iterator i)
    returning a C::dangerous_iterator, that moves element i of x
    before position. This operation invalidates all dangerous_iterators equal
    to the former position, i and ++i.
    The returned iterator is a valid iterator pointing to the inserted
    element. \p position is modified to keep it valid.
  * C must have a method
    splice(C::dangerous_iterator& position,C& x,
           C::dangerous_iterator first,C::dangerous_iterator last)
    returning a C::dangerous_iterator, that moves elements [first,last) in
    x before position. This operation invalidates all dangerous_iterators
    equal to the former position, and in [first,last] (note that last is
    invalidated, too). The returned iterator is a valid iterator pointing to
    the first moved element. \p position is modified to keep it valid.
*/
class Unlimited_Sparse_Row {

public:
  typedef std::pair<dimension_type,Coefficient> value_type;

  template <typename Compare>
  class value_key_comparison;

  template <typename Compare>
  class key_value_comparison;

  template <typename Compare>
  static Unlimited_Sparse_Row::value_key_comparison<Compare>
  value_key_compare(const Compare& comp);

  template <typename Compare>
  static Unlimited_Sparse_Row::key_value_comparison<Compare>
  key_value_compare(const Compare& comp);

  //! Constructs an unlimited row of zeroes.
  Unlimited_Sparse_Row();

  //! Constructs an unlimited row from a std::vector.
  Unlimited_Sparse_Row(const std::vector<Coefficient> &v);

private:

#ifdef USE_PPL_SPARSE_BACKEND_STD_LIST
  typedef Unlimited_Sparse_Row_Std_List_Backend list_t;
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
  typedef Unlimited_Sparse_Row_Custom_Slist_Backend list_t;
#endif

public:
  //! A const iterator that may skip some zeros in the sequence.
  typedef list_t::const_iterator const_iterator;

  // FIXME: this allows violating the internal invariant, use with care.
  //! An iterator that may skip some zeros in the sequence.
  typedef list_t::iterator iterator;

  // FIXME: this allows violating the internal invariant, use with care.
  //! An iterator that may skip some zeros in the sequence.
  //! May be invalidated by apparently unrelated operations, use with care.
  //! See the method documentation for details.
  typedef list_t::dangerous_iterator dangerous_iterator;

  //! Swaps (*this) and x.
  void swap(Unlimited_Sparse_Row& x);

  //! Swaps the i-th element with the j-th element.
  //! Iterators pointing to these elements are invalidated.
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  void swap(iterator i, iterator j);

  //! Resets to zero the value pointed to by i.
  //! dangerous_iterator objects equal to i and ++i are invalidated.
  dangerous_iterator reset(dangerous_iterator i);

  //! Resets to zero the values in the range [first,last).
  //! All dangerous_iterator objects in [first,last] are invalidated (note
  //! that last is invalidated, too).
  dangerous_iterator reset(dangerous_iterator first,
                           dangerous_iterator last);

  //! Resets to zero the i-th element.
  //! For each dangerous_iterator itr that pointed to i, dangerous_iterator
  //! objects equal to itr and ++itr are invalidated.
  void reset(dimension_type i);

  //! Resets to zero the elements in [i,j).
  //! For each dangerous_iterator i_itr that pointed to i, and j_itr that
  //! pointed to j, dangerous_iterator objects in [i_itr,j_itr] are
  //! invalidated (note that j_itr is invalidated, too).
  void reset(dimension_type i,dimension_type j);

  //! Resets to zero the elements in [i,+infinity).
  void reset_after(dimension_type i);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! For read-only access it's better to use get(), that avoids allocating
  //! space for zeroes. Both methods are O(n).
  //! If i was not previously stored, or reset(i) was called, this operation
  //! invalidates dangerous_iterator objects equal to the former
  //! lower_bound(i).
  Coefficient& operator[](const dimension_type i);

  //! Equivalent to get(), provided for convenience.
  const Coefficient& operator[](const dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    This function is O(n).
  */
  const Coefficient& get(const dimension_type i) const;

  dangerous_iterator begin();
  dangerous_iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func,const dimension_type n);

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func,const dimension_type n) const;

  dangerous_iterator find(const dimension_type c);
  dangerous_iterator lower_bound(const dimension_type c);
  dangerous_iterator upper_bound(const dimension_type c);
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

template <typename Compare>
class Unlimited_Sparse_Row::value_key_comparison
  : public std::binary_function<Unlimited_Sparse_Row::value_type&,
                                dimension_type, bool> {
public:
  value_key_comparison(const Compare& comp);

  bool operator()(const Unlimited_Sparse_Row::value_type& x,
                  const dimension_type y) const;

private:
  Compare comp_;
};

template <typename Compare>
class Unlimited_Sparse_Row::key_value_comparison
  : public std::binary_function<dimension_type,
                                Unlimited_Sparse_Row::value_type&, bool> {
public:
  key_value_comparison(const Compare& comp);

  bool operator()(const dimension_type x,
                  const Unlimited_Sparse_Row::value_type& y) const;

private:
  Compare comp_;
};

} // namespace Parma_Polyhedra_Library

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Unlimited_Sparse_Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Unlimited_Sparse_Row& x,
    Parma_Polyhedra_Library::Unlimited_Sparse_Row& y);

} // namespace std


#include "Unlimited_Sparse_Row.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_defs_hh)
