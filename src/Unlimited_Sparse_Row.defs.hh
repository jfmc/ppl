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

#include "CO_Tree.defs.hh"

namespace Parma_Polyhedra_Library {

//! An unlimited and sparse row of Coefficient.
/*!
  This class is implemented using a CO_Tree. See the documentation of CO_Tree
  for details on the implementation and the performance.
*/
class Unlimited_Sparse_Row {

public:

  typedef CO_Tree::value_type value_type;

  //! Constructs an unlimited row of non-stored zeroes.
  Unlimited_Sparse_Row();

  //! The copy constructor.
  Unlimited_Sparse_Row(const Unlimited_Sparse_Row& x);

  //! The assignment operator.
  Unlimited_Sparse_Row& operator=(const Unlimited_Sparse_Row& x);

  //! Resets all the elements of this row.
  /*!
    This method takes $O(n)$ time.
  */
  void clear();

  //! Swaps x with *this.
  /*!
    This method takes $O(1)$ time.
  */
  void swap(Unlimited_Sparse_Row& x);

  //! An iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::iterator
  */
  typedef CO_Tree::iterator iterator;

  //! A const iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::const_iterator
  */
  typedef CO_Tree::const_iterator const_iterator;

  //! Swaps the i-th element with the j-th element.
  /*!
    This operation invalidates existing iterators.

    This method takes $O(log(n))$ expected time.
  */
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  /*!
    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  void swap(iterator i, iterator j);

  //! Resets to zero the value pointed to by i.
  /*!
    By calling this method instead of getting a reference to the value and
    setting it to zero, the element will no longer be stored.

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes $O(k*log(n))$ amortized time, with k the number of
    elements in [first,last).
  */
  iterator reset(iterator first, iterator last);

  //! Resets to zero the i-th element.
  /*!
    By calling this method instead of getting a reference to the value and
    setting it to zero, the element will no longer be stored.

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  void reset(dimension_type i);

  //! Resets to zero the elements with index greater than or equal to i.
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes $O(k*log(n))$ amortized time, with k the number of
    elements with index greater than or equal to i.
  */
  void reset_after(dimension_type i);

  //! For every j greater than i, assigns (*this)[j-1] = (*this)[j].
  /*!
    This operation invalidates existing iterators.

    This method takes $O(k+log(n))$ amortized time, with k the number of
    elements with index greater than or equal to i.
  */
  void delete_element_and_shift(dimension_type i);

  //! Adds \p n zeroes, beginning from index i.
  /*!
    Existing elements with index greater than or equal to i are shifted to
    the right by n positions.

    Existing iterators are not invalidated, but are shifted to the right by n
    if they pointed at or after index i (i.e. they point to the same,
    possibly shifted, values as before).

    This method takes $O(k+log(n))$ expected time, with k the number of
    elements with index greater than or equal to i and n the number of stored
    elements (not the parameter to this method).
  */
  void add_zeroes_and_shift(dimension_type n, dimension_type i);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of the row
    and normalizes them by the GCD itself.

    This method takes $O(n)$ time.
  */
  void normalize();

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method takes $O(n)$ time.
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Unlimited_Sparse_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method takes $O(n)$ time.
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Unlimited_Sparse_Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method takes $O(n)$ time.
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Unlimited_Sparse_Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Gets a reference to the i-th element.
  /*!
    For read-only access it's better to use get(), that avoids allocating
    space for zeroes.

    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  Coefficient& operator[](dimension_type i);

  //! Equivalent to (*this)[i] = x; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ amortized time.
  */
  iterator find_create(dimension_type i);

  //! Equivalent to (*this)[i]=x; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i]=x;</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$ and the row already contains
    an element with this index, this method takes $O(1)$ time.
  */
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i];</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$ and the row already contains
    an element with this index, this method takes $O(1)$ time.
  */
  iterator find_create(iterator itr, dimension_type i);

  //! Equivalent to get(i), provided for convenience.
  const Coefficient& operator[](dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This method takes $O(log(n))$ amortized time.
  */
  const Coefficient& get(dimension_type i) const;

  //! Returns an iterator that points at the first stored element.
  /*!
    This method takes $O(1)$ expected time.
  */
  iterator begin();

  //! Returns an iterator that points after the last stored element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is kept valid.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.

    This method takes $O(1)$ time.
  */
  const iterator& end();

  //! Equivalent to cbegin().
  const_iterator begin() const;

  //! Equivalent to cend().
  const const_iterator& end() const;

  //! Returns an iterator that points at the first element.
  /*!
    This method takes $O(1)$ expected time.
  */
  const_iterator cbegin() const;

  //! Returns an iterator that points after the last element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is updated at each operation that modifies the structure.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.

    This method takes $O(1)$ time.
  */
  const const_iterator& cend() const;

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method takes $O(log(n))$ expected time.
  */
  iterator find(dimension_type i);

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not affect the result of this method, as long it
    is a valid iterator for this row. \p itr may even be end().

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$, this method takes $O(1)$ time.
  */
  iterator find(iterator itr, dimension_type i);

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method takes $O(log(n))$ expected time.
  */
  const_iterator find(dimension_type i) const;

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not affect the result of this method, as long it
    is a valid iterator for this row. \p itr may even be end().

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$, this method takes $O(1)$ time.
  */
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method takes $O(log(n))$ expected time.
  */
  iterator lower_bound(dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not affect the result of this method, as long it
    is a valid iterator for this row. \p itr may even be end().

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$, this method takes $O(1)$ time.
  */
  iterator lower_bound(iterator itr, dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method takes $O(log(n))$ expected time.
  */
  const_iterator lower_bound(dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not affect the result of this method, as long it
    is a valid iterator for this row. \p itr may even be end().

    This method takes $O(log(n))$ expected time. If the distance between
    \p itr and the searched position is $O(1)$, this method takes $O(1)$ time.
  */
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  //! Compares x with *this.
  /*!
    Stored zeroes and considered equal to non-stored elements.

    This method takes $O(n)$ time.
  */
  bool operator==(const Unlimited_Sparse_Row& x) const;

  //! Compares x with *this.
  /*!
    Stored zeroes and considered equal to non-stored elements.

    This method takes $O(n)$ time.
  */
  bool operator!=(const Unlimited_Sparse_Row& x) const;

  //! Loads the row from an ASCII representation generated using ascii_dump().
  /*!
    This method takes $O(n)$ time.
  */
  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Returns the size in bytes of the memory managed by \p *this.
  /*!
    This method takes $O(1)$ time.
  */
  memory_size_type external_memory_in_bytes() const;

private:
  //! The tree used to store the elements.
  CO_Tree tree;
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
#include "Unlimited_Sparse_Row.templates.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_defs_hh)
