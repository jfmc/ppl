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

namespace std {

// This is declared here because is friend of both classes.
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Swaps x and y.
/*!
  y should either have the same size of x, or have size 0.
  This allows swapping with default-constructed Sparse_Rows.

  \relates Parma_Polyhedra_Library::Sparse_Row_Reference
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Sparse_Row_Reference x,
          Parma_Polyhedra_Library::Sparse_Row& y);

}

namespace Parma_Polyhedra_Library {

//! A finite sparse sequence of coefficients.
/*!
  This class is implemented using an Unlimited_Sparse_Row, that is
  implemented using a CO_Tree.
  See the documentation of those classes for more details.
*/
class Sparse_Row {

public:
  //! A const iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::const_iterator
  */
  typedef Unlimited_Sparse_Row::const_iterator const_iterator;

  //! An iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::iterator
  */
  typedef Unlimited_Sparse_Row::iterator iterator;

  //! Constructs a row with the specified size.
  /*!
    The row will contain only non-stored zeroes.
  */
  explicit Sparse_Row(dimension_type n = 0);

  //! Constructs a row of the specified size from an Unlimited_Sparse_Row.
  /*!
    The elements of the unlimited sparse row with indexes greater than or
    equal to n are ignored.
  */
  Sparse_Row(const Unlimited_Sparse_Row &x, dimension_type n);

  //! Constructs a Sparse_Row from a Sparse_Row_Reference.
  Sparse_Row(const Sparse_Row_Reference& x);

  //! Assigns a Sparse_Row_Reference to *this.
  /*!
    All stored elements in \p x must have index lower than size().
  */
  Sparse_Row& operator=(const Unlimited_Sparse_Row& x);

  //! Assigns a Sparse_Row_Reference to *this.
  Sparse_Row& operator=(const Sparse_Row_Reference& x);

  //! Resets all the elements of this row.
  /*!
    This method takes O(n) time.
  */
  void clear();

  //! Swaps *this and x.
  void swap(Sparse_Row& x);

  //! Swaps (*this) and x.
  /*!
    (*this) should either have the same size of x, or have size 0.
    This allows swapping with default-constructed Sparse_Rows.
  */
  void swap(Sparse_Row_Reference x);

  //! Swaps the i-th element with the j-th element.
  /*!
    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  /*!
    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  void swap(iterator i, iterator j);

  //! Resizes the row to size \p n.
  /*!
    This method, with this signature, is needed for compatibility with
    Dense_Row.
  */
  void construct(dimension_type n);

  //! Resizes the row to size \p n.
  /*!
    \param capacity is ignored.

    This method, with this signature, is needed for compatibility with
    Dense_Row.
  */
  void construct(dimension_type n, dimension_type capacity);

  //! Resizes the row to the specified size.
  void resize(dimension_type n);

  //! Resizes the row to size \p n.
  /*!
    This method, with this signature, is needed for compatibility with
    Dense_Row.
  */
  void shrink(dimension_type n);

  //! Returns the size of the row.
  dimension_type size() const;

private:
  //! The Unlimited_Sparse_Row that stores the row's elements.
  Unlimited_Sparse_Row row;

  //! The size of the row.
  /*!
    The elements contained in this row have indexes that are less than size_.
  */
  dimension_type size_;

public:
  //! Resets to zero the value pointed to by i.
  /*!
    By calling this method instead of getting a reference to the value and
    setting it to zero, the element will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(k*log(n)) time, with k the number of elements in
    [first,last).
  */
  iterator reset(iterator first, iterator last);

  //! Resets to zero the elements with index greater than or equal to i.
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(k*log(n)) time, with k the number of elements with
    index greater than or equal to i.
  */
  void reset_after(dimension_type i);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of the row
    and normalizes them by the GCD itself.

    This method is O(n).
  */
  void normalize();

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Unlimited_Sparse_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Sparse_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Sparse_Row_Reference& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Unlimited_Sparse_Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Sparse_Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Sparse_Row_Reference& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Unlimited_Sparse_Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Sparse_Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Sparse_Row_Reference& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Gets a reference to the i-th element.
  /*!
    For read-only access it's better to use get(), that avoids allocating
    space for zeroes.

    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  Coefficient& operator[](dimension_type i);

  //! Equivalent to get(i), provided for convenience.
  const Coefficient& operator[](dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This method is O(log(n)).
  */
  const Coefficient& get(dimension_type i) const;

  //! Returns an iterator that points at the first stored element.
  iterator begin();

  //! Returns an iterator that points after the last stored element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is kept valid.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const iterator& end();

  //! Equivalent to cbegin().
  const_iterator begin() const;

  //! Equivalent to cend().
  const const_iterator& end() const;

  //! Returns an iterator that points at the first element.
  const_iterator cbegin() const;

  //! Returns an iterator that points after the last element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is updated at each operation that modifies the structure.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const const_iterator& cend() const;

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator find(dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator lower_bound(dimension_type i);

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator find(dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator lower_bound(dimension_type i) const;

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not change the result. \p itr may even be end().

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find(iterator itr, dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator lower_bound(iterator itr, dimension_type i);

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not change the result. \p itr may even be end().

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  //! Equivalent to (*this)[i] = x; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  iterator find_create(dimension_type i);

  //! Equivalent to (*this)[i]=x; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i]=x;</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i];</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find_create(iterator itr, dimension_type i);

  //! Returns a Sparse_Row_Reference that refers to this row.
  operator Sparse_Row_Reference();

  //! Returns the underlying Unlimited_Sparse_Row.
  operator const Unlimited_Sparse_Row&() const;

  PPL_OUTPUT_DECLARATIONS

  //! Loads the row from an ASCII representation generated using ascii_dump().
  bool ascii_load(std::istream& s);

private:
  //! Checks the invariant.
  bool OK() const;

  friend void std::swap(Sparse_Row_Reference x, Sparse_Row& y);
};

//! A reference to a finite sparse sequence of coefficients.
/*!
  This class is implemented using a reference to an Unlimited_Sparse_Row,
  that is implemented using a CO_Tree.
  See the documentation of those classes for more details.

  This class is useful to wrap an Unlimited_Sparse_Row reference and provide
  the same interface as Sparse_Row.
*/
class Sparse_Row_Reference {

public:
  //! A const iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::const_iterator
  */
  typedef Unlimited_Sparse_Row::const_iterator const_iterator;

  //! An iterator on the row elements
  /*!
    This iterator skips non-stored zeroes.
    \see CO_Tree::iterator
  */
  typedef Unlimited_Sparse_Row::iterator iterator;

  /*!
    \brief Constructs a Sparse_Row_Reference of size \p size that references
           \p row.

    \p row must not contain stored elements with index greater than or equal
    to \p size.
  */
  Sparse_Row_Reference(Unlimited_Sparse_Row& row, dimension_type size);

  //! Copies \p x into the row referenced by *this.
  /*!
    \p x must not contain stored elements with index greater than or equal
    to size().
  */
  Sparse_Row_Reference& operator=(const Unlimited_Sparse_Row& x);

  //! Copies the row referenced by \p x into the row referenced by *this.
  /*!
    x.size() must be equal to size().
  */
  Sparse_Row_Reference& operator=(const Sparse_Row_Reference& x);

  //! Copies \p x into the row referenced by *this.
  /*!
    x.size() must be equal to size().
  */
  Sparse_Row_Reference& operator=(const Sparse_Row& x);

  //! Resets all the elements of this row.
  /*!
    This method takes O(n) time.
  */
  void clear();

  //! Swaps this row referenced by *this with the row referenced by x.
  /*!
    x.size() must be equal to size().
  */
  void swap(Sparse_Row_Reference x);

  //! Swaps x with the row referenced by *this.
  /*!
    x should either have the same size of (*this), or have size 0.
    This allows swapping with default-constructed Sparse_Rows.
  */
  void swap(Sparse_Row& x);

  //! Swaps the i-th element with the j-th element.
  /*!
    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  /*!
    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  void swap(iterator i, iterator j);

  //! Returns the size of the referenced row.
  dimension_type size() const;

  //! Resets to zero the value pointed to by i.
  /*!
    By calling this method instead of getting a reference to the value and
    setting it to zero, the element will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(k*log(n)) time, with k the number of elements in
    [first,last).
  */
  iterator reset(iterator first, iterator last);

  //! Resets to zero the i-th element.
  /*!
    By calling this method instead of getting a reference to the value and
    setting it to zero, the element will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(log(n)) time.
  */
  void reset(dimension_type i);

  //! Resets to zero the elements with indexes in [i,j).
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O((j-i)*log(n)) time.
  */
  void reset(dimension_type i, dimension_type j);

  //! Resets to zero the elements with index greater than or equal to i.
  /*!
    By calling this method instead of getting a reference to the values and
    setting them to zero, the elements will no longer be stored.

    This operation invalidates existing iterators.

    This method takes O(k*log(n)) time, with k the number of elements with
    index greater than or equal to i.
  */
  void reset_after(dimension_type i);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of the row
    and normalizes them by the GCD itself.

    This method is O(n).
  */
  void normalize();

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Unlimited_Sparse_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Sparse_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing if c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Sparse_Row_Reference& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Unlimited_Sparse_Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Sparse_Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, 0) must do nothing, for every c1.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Sparse_Row_Reference& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Unlimited_Sparse_Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Sparse_Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
             f(c1) must be equivalent to g(c1, 0).
    \param g should take a Coefficient& and a const Coefficient&.
             g(c1, c2) must do nothing when both c1 and c2 are zero.
    \param h should take a Coefficient& and a const Coefficient&.
             h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.

    This method is O(n).
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Sparse_Row_Reference& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! Gets a reference to the i-th element.
  /*!
    For read-only access it's better to use get(), that avoids allocating
    space for zeroes.

    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  Coefficient& operator[](dimension_type i);

  //! Equivalent to get(i), provided for convenience.
  const Coefficient& operator[](dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    If possible, use the find_create(), find() or lower_bound() methods with
    a hint instead of this, to improve performance.

    This method is O(log(n)).
  */
  const Coefficient& get(dimension_type i) const;

  //! Returns an iterator that points at the first stored element.
  iterator begin();

  //! Returns an iterator that points after the last stored element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is kept valid.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const iterator& end();

  //! Equivalent to cbegin().
  const_iterator begin() const;

  //! Equivalent to cend().
  const const_iterator& end() const;

  //! Returns an iterator that points at the first element.
  const_iterator cbegin() const;

  //! Returns an iterator that points after the last element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is updated at each operation that modifies the structure.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const const_iterator& cend() const;

  /*!
    \brief Executes func on each non-zero element and may execute it on some
           zeroes.

    This signature is needed for compatibility with Dense_Row.
    \param func A functor that takes a (Coefficient&) or
                (const Coefficient&) argument.
    \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func, const dimension_type n);

  /*!
    \brief Executes func on each non-zero element and may execute it on some
           zeros.

    This signature is needed for compatibility with Dense_Row.
    \param func A functor that takes a (Coefficient&) or
                (const Coefficient&) argument.
    \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func,const dimension_type n) const;

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator find(dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator lower_bound(dimension_type i);

  //! Looks for an element with key i.
  /*!
    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator find(dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator lower_bound(dimension_type i) const;

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not change the result. \p itr may even be end().

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find(iterator itr, dimension_type i);

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  iterator lower_bound(iterator itr, dimension_type i);

  //! Looks for an element with key i.
  /*!
    \p itr is used as a hint. This method will be faster if the searched
    element is near to \p itr.

    The value of \p itr does not change the result. \p itr may even be end().

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Lower bound of key i.
  /*!
    \returns an iterator to the first element with index greater than or
             equal to i.
             If there are no such elements, returns end().

    If possible, use the find() method that takes a hint iterator, to improve
    performance.

    This method is O(log(n)).
  */
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  //! Equivalent to (*this)[i] = x; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If possible, use versions of this method that take a hint, to improve
    performance.

    This operation invalidates existing iterators.

    This method is O(log(n)).
  */
  iterator find_create(dimension_type i);

  //! Equivalent to (*this)[i]=x; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i]=x;</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i]; find(i); , but faster.
  /*!
    If \p itr points near the added element, this is faster, even faster than
    <CODE>(*this)[i];</CODE>.

    The value of \p itr does not change the result. \p itr may even be end().

    This operation invalidates existing iterators.

    This method is O(1) if the distance between \p itr and the searched
    element is O(1), otherwise it is O(log(n)).
  */
  iterator find_create(iterator itr, dimension_type i);

  //! Returns the underlying Unlimited_Sparse_Row.
  operator const Unlimited_Sparse_Row&() const;

private:

  //! Checks the internal invariant.
  bool OK() const;

  /*!
    \brief A functor class that applies a functor to the second element of its
           argument.
  */
  template <typename Func>
  class applier_to_data :
    public std::unary_function<std::pair<dimension_type, Coefficient&>,void> {
  public:
    applier_to_data(const Func& func);
    void operator()(std::pair<dimension_type, Coefficient&> x) const;
  private:
    Func f;
  };

  template <typename Func>
  static applier_to_data<Func> apply_to_data(const Func& func);

private:

  //! A reference to the row used to store the elements.
  Unlimited_Sparse_Row& row;

  //! The size of the row.
  const dimension_type size_;

  friend void std::swap(Sparse_Row_Reference x, Sparse_Row& y);
};

} // namespace Parma_Polyhedra_Library

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Sparse_Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Sparse_Row& x,
          Parma_Polyhedra_Library::Sparse_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Similar to the standard swap, but doesn't need references.
/*! \relates Parma_Polyhedra_Library::Sparse_Row_Reference */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Sparse_Row_Reference x,
          Parma_Polyhedra_Library::Sparse_Row_Reference y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Swaps a Sparse_Row with a Sparse_Row_Reference.
/*! \relates Parma_Polyhedra_Library::Sparse_Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Sparse_Row& x,
          Parma_Polyhedra_Library::Sparse_Row_Reference y);

} // namespace std

#include "Sparse_Row.templates.hh"
#include "Sparse_Row.inlines.hh"

#endif // !defined(PPL_Sparse_Row_defs_hh)
