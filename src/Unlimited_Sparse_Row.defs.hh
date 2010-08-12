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

class Unlimited_Sparse_Row {

private:
  typedef Unlimited_Sparse_Row This;

public:

  typedef CO_Tree::value_type value_type;

  Unlimited_Sparse_Row();

  Unlimited_Sparse_Row(const This& x);

  //! Constructs an unlimited row from an std::vector.
  explicit Unlimited_Sparse_Row(const std::vector<Coefficient> &v);

  This& operator=(const This& x);

  void swap(This& x);

  typedef CO_Tree::iterator iterator;
  typedef CO_Tree::const_iterator const_iterator;

  //! Swaps the i-th element with the j-th element.
  //! Iterators pointing to these elements are invalidated.
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  void swap(iterator i, iterator j);

  //! Resets to zero the value pointed to by i.
  //! iterator objects equal to i and ++i are invalidated.
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  //! All iterator objects in [first,last] are invalidated (note
  //! that last is invalidated, too).
  iterator reset(iterator first, iterator last);

  //! Resets to zero the i-th element.
  //! For each iterator itr that pointed to i, iterator
  //! objects equal to itr and ++itr are invalidated.
  void reset(dimension_type i);

  //! Resets to zero the elements in [i,j).
  //! For each iterator i_itr that pointed to i, and j_itr that
  //! pointed to j, iterator objects in [i_itr,j_itr] are
  //! invalidated (note that j_itr is invalidated, too).
  void reset(dimension_type i, dimension_type j);

  //! Resets to zero the elements in [i,+infinity).
  void reset_after(dimension_type i);

  //! For each j>i, assigns (*this)[j-1] = (*this)[j].
  //! Invalidates iterators pointing to the i-th and (i+1)-th
  //! element. Other iterators remain valid and point to the same values, but
  //! their index will increase by 1 if it was >i+1.
  void delete_element_and_shift(dimension_type i);

  //! Adds \p n zeroes, beginning from index i. Existing elements with index
  //! greater than or equal to i are shifted to the right by n positions.
  void add_zeroes_and_shift(dimension_type n, dimension_type i);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
    \param g should take a Coefficient& and a const Coefficient&.
    g(c1, c2) must do nothing if c1 is zero.
    f(c1) must be equivalent to g(c1, 0).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const This& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, 0) must do nothing.
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const This& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, c2) must do nothing if both c1 and c2 are zero.
    f(c1) must be equivalent to g(c1, 0).
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const This& y,
               const Func1& f, const Func2& g, const Func3& h);

  //! After this call, get(i) == x.
  //! This is slower than <CODE>if (x != 0) find_create(i,x);</CODE> because
  //! it needs to check whether the element with index i is zero.
  void assign(dimension_type i, const Coefficient& x);

  //! Equivalent to <CODE>if (x != 0) find_create(i, x);</CODE>, provided
  //! for convenience. This is faster than assign(i, x).
  void assign_if_nonzero(dimension_type i, const Coefficient& x);

  //! For read-only access it's better to use get(), that avoids allocating
  //! space for zeroes. Both methods are O(n).
  //! If i was not previously stored, or reset(i) was called, this operation
  //! invalidates iterator objects equal to the former
  //! lower_bound(i).
  Coefficient& operator[](dimension_type i);

  //! Equivalent to find_create(i, x, begin()) .
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Equivalent to find_create(i, begin()) .
  iterator find_create(dimension_type i);

  //! Equivalent to (*this)[i]=x , needs itr to point before the added
  //! element. If itr points near the added element, this is faster.
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Equivalent to (*this)[i] , needs itr to point before the added
  //! element. If itr points near the added element, this is faster.
  iterator find_create(iterator itr, dimension_type i);

  //! Equivalent to get(), provided for convenience.
  const Coefficient& operator[](dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    This function is O(n).
  */
  const Coefficient& get(dimension_type i) const;

  //! Returns an iterator that points before the first element.
  //! This method always returns a reference to the same internal iterator,
  //! that is updated at each operation that modifies the structure.
  //! Client code can keep a const reference to that iterator instead of
  //! keep updating a local iterator.
  const iterator& before_begin();

  //! Returns an iterator that points before the first element.
  iterator begin();

  //! Returns an iterator that points after the last element.
  //! This method always returns a reference to the same internal iterator,
  //! that is updated at each operation that modifies the structure.
  //! Client code can keep a const reference to that iterator instead of
  //! keep updating a local iterator.
  const iterator& end();

  //! Returns an iterator that points before the first element.
  //! This method always returns a reference to the same internal iterator,
  //! that is updated at each operation that modifies the structure.
  //! Client code can keep a const reference to that iterator instead of
  //! keep updating a local iterator.
  const const_iterator& before_begin() const;

  //! Returns an iterator that points before the first element.
  const_iterator begin() const;

  //! Returns an iterator that points after the last element.
  //! This method always returns a reference to the same internal iterator,
  //! that is updated at each operation that modifies the structure.
  //! Client code can keep a const reference to that iterator instead of
  //! keep updating a local iterator.
  const const_iterator& end() const;

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func, const dimension_type n);

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func, dimension_type n) const;

  //! Looks for an element with key i.
  iterator find(dimension_type i);
  //! Looks for an element with key i, assuming it is in [itr,end()) .
  iterator find(iterator itr, dimension_type i);

  //! Looks for an element with key i.
  const_iterator find(dimension_type i) const;
  //! Looks for an element with key i, assuming it is in [itr,end()) .
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Lower bound of key i.
  iterator lower_bound(dimension_type );
  //! Lower bound of key i, assuming it is in [itr,end()) .
  iterator lower_bound(iterator itr, dimension_type i);

  //! Lower bound of key i.
  const_iterator lower_bound(dimension_type i) const;
  //! Lower bound of key i, assuming it is in [itr,end()) .
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  bool operator==(const This &x) const;
  bool operator!=(const This &x) const;

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

private:

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
