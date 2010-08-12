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

/**
 * \brief An unlimited and sparse row of Coefficient.
 *
 * This class is implemented using a CO_Tree. See the documentation of CO_Tree
 * for details on the implementation and the performance.
 */
class Unlimited_Sparse_Row {

private:
  typedef Unlimited_Sparse_Row This;

public:

  typedef CO_Tree::value_type value_type;

  //! Constructs an unlimited row of non-stored zeroes.
  Unlimited_Sparse_Row();

  //! The copy constructor.
  Unlimited_Sparse_Row(const This& x);

  /**
   * \brief Constructs an unlimited row from an std::vector.
   *
   * Zero elements in the vector are not stored.
   */
  explicit Unlimited_Sparse_Row(const std::vector<Coefficient> &v);

  //! The assignment operator.
  This& operator=(const This& x);

  //! Swaps x with *this.
  void swap(This& x);

  /**
   * \brief An iterator on the row elements
   *
   * This iterator skips non-stored zeroes.
   * \see CO_Tree::iterator
   */
  typedef CO_Tree::iterator iterator;

  /**
   * \brief A const iterator on the row elements
   *
   * This iterator skips non-stored zeroes.
   * \see CO_Tree::const_iterator
   */
  typedef CO_Tree::const_iterator const_iterator;

  /**
   * \brief Swaps the i-th element with the j-th element.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(log(n)) time.
   */
  void swap(dimension_type i, dimension_type j);

  /**
   * \brief Swaps the element pointed to by i with the element pointed to by j.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(log(n)) time.
   */
  void swap(iterator i, iterator j);

  /**
   * \brief Resets to zero the value pointed to by i.
   *
   * By calling this method instead of getting a reference to the value and
   * setting it to zero, the element will no longer be stored.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(log(n)) time.
   */
  iterator reset(iterator i);

  /**
   * \brief Resets to zero the values in the range [first,last).
   *
   * By calling this method instead of getting a reference to the values and
   * setting them to zero, the elements will no longer be stored.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(k*log(n)) time, with k the number of elements in
   * [first,last).
   */
  iterator reset(iterator first, iterator last);

  /**
   * \brief Resets to zero the i-th element.
   *
   * By calling this method instead of getting a reference to the value and
   * setting it to zero, the element will no longer be stored.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(log(n)) time.
   */
  void reset(dimension_type i);

  /**
   * \brief Resets to zero the elements with indexes in [i,j).
   *
   * By calling this method instead of getting a reference to the values and
   * setting them to zero, the elements will no longer be stored.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O((j-i)*log(n)) time.
   */
  void reset(dimension_type i, dimension_type j);

  /**
   * \brief Resets to zero the elements with index greater than or equal to i.
   *
   * By calling this method instead of getting a reference to the values and
   * setting them to zero, the elements will no longer be stored.
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(k*log(n)) time, with k the number of elements with
   * index greater than or equal to i.
   */
  void reset_after(dimension_type i);

  /**
   * \brief For every j greater than i, assigns (*this)[j-1] = (*this)[j].
   *
   * This operation invalidates existing iterators.
   *
   * This method takes O(k+log(n)) time, with k the number of elements with
   * index greater than or equal to i.
   */
  void delete_element_and_shift(dimension_type i);

  /**
   * \brief Adds \p n zeroes, beginning from index i.
   * 
   * Existing elements with index greater than or equal to i are shifted to
   * the right by n positions.
   *
   * Existing iterators are not invalidated, but are shifted to the right by n
   * if they pointed at or after index i (i.e. they point to the same,
   * possibly shifted, values as before).
   *
   * This method takes O(k+log(n)) time, with k the number of elements with
   * index greater than or equal to i and n the number of stored elements
   * (not the parameter to this method).
   */
  void add_zeroes_and_shift(dimension_type n, dimension_type i);

  /**
   * \brief Normalizes the modulo of coefficients so that they are mutually prime.
   *
   * Computes the Greatest Common Divisor (GCD) among the elements of the row
   * and normalizes them by the GCD itself.
   *
   * This method is O(n).
   */
  void normalize();

  /**
   * \brief Calls g(x[i],y[i]), for each i.
   *
   * \param f should take a Coefficient&.
   *          f(c1) must be equivalent to g(c1, 0).
   * \param g should take a Coefficient& and a const Coefficient&.
   *          g(c1, c2) must do nothing if c1 is zero.
   *
   * This method is O(n).
   */
  template <typename Func1, typename Func2>
  void combine_needs_first(const This& y, const Func1& f, const Func2& g);

  /**
   * \brief Calls g(x[i],y[i]), for each i.
   *
   * \param g should take a Coefficient& and a const Coefficient&.
   *          g(c1, 0) must do nothing, for every c1.
   * \param h should take a Coefficient& and a const Coefficient&.
   *          h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
   *
   * This method is O(n).
   */
  template <typename Func1, typename Func2>
  void combine_needs_second(const This& y, const Func1& g, const Func2& h);

  /**
   * \brief Calls g(x[i],y[i]), for each i.
   *
   * \param f should take a Coefficient&.
   *          f(c1) must be equivalent to g(c1, 0).
   * \param g should take a Coefficient& and a const Coefficient&.
   *          g(c1, c2) must do nothing when both c1 and c2 are zero.
   * \param h should take a Coefficient& and a const Coefficient&.
   *          h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
   *
   * This method is O(n).
   */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const This& y, const Func1& f, const Func2& g, const Func3& h);

  /**
   * \brief After this call, get(i) == x.
   *
   * This is slower than assign_if_nonzero() because it needs to check whether
   * the element with index i is zero.
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(log(n)).
   */
  void assign(dimension_type i, const Coefficient& x);

  /**
   * \brief Equivalent to <CODE>if (x != 0) find_create(i, x);</CODE>.
   *
   * This is faster than assign(i, x), and yields the same result when the
   * element with index i is zero.
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(log(n)).
   */
  void assign_if_nonzero(dimension_type i, const Coefficient& x);

  /**
   * \brief Gets a reference to the i-th element.
   *
   * For read-only access it's better to use get(), that avoids allocating
   * space for zeroes.
   *
   * If possible, use the find_create(), find() or lower_bound() methods with
   * a hint instead of this, to improve performance.
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(log(n)).
   */
  Coefficient& operator[](dimension_type i);

  /**
   * \brief Equivalent to (*this)[i] = x; find(i); , but faster.
   *
   * If possible, use versions of this method that take a hint, to improve
   * performance.
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(log(n)).
   */
  iterator find_create(dimension_type i, const Coefficient& x);

  /**
   * \brief Equivalent to (*this)[i]; find(i); , but faster.
   *
   * If possible, use versions of this method that take a hint, to improve
   * performance.
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(log(n)).
   */
  iterator find_create(dimension_type i);

  /**
   * \brief Equivalent to (*this)[i]=x; find(i); , but faster.
   *
   * If \p itr points near the added element, this is faster, even faster than
   * <CODE>(*this)[i]=x;</CODE>.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  /**
   * \brief Equivalent to (*this)[i]; find(i); , but faster.
   *
   * If \p itr points near the added element, this is faster, even faster than
   * <CODE>(*this)[i];</CODE>.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This operation invalidates existing iterators.
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  iterator find_create(iterator itr, dimension_type i);

  //! Equivalent to get(i), provided for convenience.
  const Coefficient& operator[](dimension_type i) const;

  /**
   * \brief Gets the i-th element in the sequence.
   *
   * If possible, use the find_create(), find() or lower_bound() methods with
   * a hint instead of this, to improve performance.
   *
   * This method is O(log(n)).
   */
  const Coefficient& get(dimension_type i) const;

  /**
   * \brief Returns an iterator that points before the first element.
   *
   * This method always returns a reference to the same internal iterator,
   * that is kept valid.
   * Client code can keep a const reference to that iterator instead of
   * keep updating a local iterator.
   */
  const iterator& before_begin();

  //! Returns an iterator that points at the first element.
  iterator begin();

  /**
   * \brief Returns an iterator that points after the last element.
   *
   * This method always returns a reference to the same internal iterator,
   * that is kept valid.
   * Client code can keep a const reference to that iterator instead of
   * keep updating a local iterator.
   */
  const iterator& end();

  /**
   * \brief Returns an iterator that points before the first element.
   *
   * This method always returns a reference to the same internal iterator,
   * that is kept valid.
   * Client code can keep a const reference to that iterator instead of
   * keep updating a local iterator.
   */
  const const_iterator& before_begin() const;

  //! Returns an iterator that points at the first element.
  const_iterator begin() const;

  /**
   * \brief Returns an iterator that points after the last element.
   *
   * This method always returns a reference to the same internal iterator,
   * that is kept valid.
   * Client code can keep a const reference to that iterator instead of
   * keep updating a local iterator.
   */
  const const_iterator& end() const;

  /**
   * \brief Calls func on every stored value.
   *
   * \param func A functor that takes a (Coefficient&) or
   *             (const Coefficient&) argument.
   * \param n ignored, needed for compatibility with Dense_Row.
   *
   * This method is O(n).
   */
  template <typename Func>
  void for_each_nonzero(const Func& func, const dimension_type n);

  /**
   * \brief Calls func onevery stored value.
   *
   * \param func A functor that takes a (Coefficient&) or
   *             (const Coefficient&) argument.
   * \param n ignored, needed for compatibility with Dense_Row.
   *
   * This method is O(n).
   */
  template <typename Func>
  void for_each_nonzero(const Func& func, dimension_type n) const;

  /**
   * \brief Looks for an element with key i.
   *
   * If possible, use the find() method that takes a hint iterator, to improve
   * performance.
   *
   * This method is O(log(n)).
   */
  iterator find(dimension_type i);

  /**
   * \brief Looks for an element with key i.
   *
   * \p itr is used as a hint. This method will be faster if the searched
   * element is near to \p itr.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  iterator find(iterator itr, dimension_type i);

  /**
   * \brief Looks for an element with key i.
   *
   * If possible, use the find() method that takes a hint iterator, to improve
   * performance.
   *
   * This method is O(log(n)).
   */
  const_iterator find(dimension_type i) const;

  /**
   * \brief Looks for an element with key i.
   *
   * \p itr is used as a hint. This method will be faster if the searched
   * element is near to \p itr.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  const_iterator find(const_iterator itr, dimension_type i) const;

  /**
   * \brief Lower bound of key i.
   *
   * \returns an iterator to the first element with index greater than or
   *          equal to i.
   *          If there are no such elements, returns end().
   *
   * If possible, use the find() method that takes a hint iterator, to improve
   * performance.
   *
   * This method is O(log(n)).
   */
  iterator lower_bound(dimension_type i);

  /**
   * \brief Lower bound of key i.
   *
   * \returns an iterator to the first element with index greater than or
   *          equal to i.
   *          If there are no such elements, returns end().
   *
   * \p itr is used as a hint. This method will be faster if the searched
   * element is near to \p itr.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  iterator lower_bound(iterator itr, dimension_type i);

  /**
   * \brief Lower bound of key i.
   *
   * \returns an iterator to the first element with index greater than or
   *          equal to i.
   *          If there are no such elements, returns end().
   *
   * If possible, use the find() method that takes a hint iterator, to improve
   * performance.
   *
   * This method is O(log(n)).
   */
  const_iterator lower_bound(dimension_type i) const;

  /**
   * \brief Lower bound of key i.
   *
   * \returns an iterator to the first element with index greater than or
   *          equal to i.
   *          If there are no such elements, returns end().
   *
   * \p itr is used as a hint. This method will be faster if the searched
   * element is near to \p itr.
   *
   * The value of \p itr does not change the result. \p itr may even be
   * before_begin() or end().
   *
   * This method is O(1) if the distance between \p itr and the searched
   * element is O(1), otherwise it is O(log(n)).
   */
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  /**
   * \brief Compares x with *this.
   *
   * Stored zeroes and considered equal to non-stored elements.
   */
  bool operator==(const This &x) const;

  /**
   * \brief Compares x with *this.
   *
   * Stored zeroes and considered equal to non-stored elements.
   */
  bool operator!=(const This &x) const;

  //! Loads the row from an ASCII representation generated using ascii_dump().
  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Returns the size in bytes of the memory managed by \p *this.
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
