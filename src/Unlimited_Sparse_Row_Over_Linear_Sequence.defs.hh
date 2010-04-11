/* Unlimited_Sparse_Row_Over_Linear_Sequence class declaration.
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

#ifndef PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_defs_hh
#define PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_defs_hh 1

#include "Coefficient.defs.hh"
#include <list>
#include <vector>

#include "Unlimited_Sparse_Row_Over_Linear_Sequence.types.hh"

#include "Unlimited_Sparse_Row.types.hh"

#include "Unlimited_Sparse_Row_Std_List_Backend.defs.hh"
#include "Unlimited_Sparse_Row_Custom_Slist_Backend.defs.hh"
#include "Unlimited_Sparse_Row_Std_Vector_Backend.defs.hh"

namespace Parma_Polyhedra_Library {

//! A finite, but unlimited (i.e. has no size) sparse sequence of
//! coefficients.
/*!
  The storage backend can be configured at build time by defining the
  appropriate macro. The default storage backend is std::list.

  Unlimited_Sparse_Row_Over_Linear_Sequence::dangerous_iterator is an iterator
  provided by the backend, that may be invalidated by some operations. Read
  the method documentation for more information.

  Other storage backend can be added, if they meet the following requirements.
  Here, we call C the candidate class to be used as backend.

  * C must have a default constructor, a copy constructor and an assignment
    operator.
  * C must have three iterator types: C::const_iterator, C::iterator and
    C::dangerous_iterator.
  * C::const_iterator, C::iterator and C::dangerous_iterator must be different
    types.
  * Conversion operators must be defined from C::dangerous_iterator to
    C::iterator, from C::dangerous_iterator to C::const_iterator and from
    C::iterator to C::const_iterator.
  * C must have const begin() and end() methods that return a
    C::const_iterator .
  * C must have non-const begin() and end() methods that return a
    C::iterator .
  * C must have non-const begin_dangerous() and end_dangerous() methods that
    return a C::dangerous_iterator .
  * C::const_iterator, C::iterator and C::dangerous_iterator must meet the
    forward-iterator requirements and have typedefs (or nested types) for
    iterator_category, value_type, difference_type, pointer and reference.
  * C::value_type should be \p std::pair<dimension_type,Coefficient> .
  * C must have a swap() method.
  * C must have a const OK() method, returning bool.
  * C must have a const external_memory_in_bytes() method, returning a
    memory_size_type. This method returns the size in bytes of the memory
    managed by \p *this.
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
  * C::dangerous_iterator must have a static method next(C::iterator i)
    that returns a dangerous_iterator pointing to the element after i.
  * C must have a method find_dangerous(dimension_type i) returning a
    C::dangerous_iterator. This method returns a dangerous_iterator pointing
    to the element with index \p i, or end_dangerous() if there is no such
    element.
  * C must have a method
    find_dangerous(dimension_type i, dangerous_iterator itr) returning a
    C::dangerous_iterator. This method returns a dangerous_iterator pointing
    to the element with index \p i, or end_dangerous() if there is no such
    element. \p itr must be an iterator pointing to an element with index
    less than i.
  * C must have a method find(dimension_type i) returning a C::iterator.
    This method returns a dangerous_iterator pointing to the element with
    index \p i, or end() if there is no such element.
  * C must have a method find(dimension_type i, iterator itr) returning a
    C::iterator. This method returns an iterator pointing to the element with
    index \p i, or end() if there is no such element. \p itr must be an
    iterator pointing to an element with index less than i.
  * C must have a const method find(dimension_type i) returning a
    C::const_iterator. This method returns a const_iterator pointing to
    the element with index \p i, or end() if there is no such element.
  * C must have a const method find(dimension_type i, const_iterator itr)
    returning a C::const_iterator. This method returns an iterator pointing to
    the element with index \p i, or end() if there is no such element.
    \p itr must be an iterator pointing to an element with index less than i.
  * C must have a method lower_bound_dangerous(dimension_type i) returning a
    C::dangerous_iterator. This method returns a dangerous_iterator pointing
    to the element with index \p i, or to the last element with index less
    than \p i, if there is no such element.
  * C must have a method
    lower_bound_dangerous(dimension_type i, dangerous_iterator itr) returning
    a C::dangerous_iterator. This method returns a dangerous_iterator pointing
    to the element with index \p i, or to the last element with index less
    than \p i, if there is no such element.
    \p itr must be an iterator pointing to an element with index less than i.
  * C must have a method lower_bound(dimension_type i) returning a C::iterator.
    This method returns a dangerous_iterator pointing to the element with
    index \p i, or to the last element with index less than \p i, if there is
    no such element.
  * C must have a method lower_bound(dimension_type i, iterator itr) returning a
    C::iterator. This method returns an iterator pointing to the element with
    index \p i, or to the last element with index less than \p i, if there is
    no such element.
    \p itr must be an iterator pointing to an element with index less than i.
  * C must have a const method lower_bound(dimension_type i) returning a
    C::const_iterator. This method returns a const_iterator pointing to
    the element with index \p i, or to the last element with index less than
    \p i, if there is no such element.
  * C must have a const method lower_bound(dimension_type i, const_iterator itr)
    returning a C::const_iterator. This method returns an iterator pointing to
    the element with index \p i, or to the last element with index less than
    \p i, if there is no such element.
    \p itr must be an iterator pointing to an element with index less than i.
  * C must have a method
    find2_dangerous(dimension_type c1, dimension_type c2,
                    C::dangerous_iterator& itr1, C::dangerous_iterator& itr2)
    returning void, equivalent to<BR>
    itr1=find_dangerous(c1); itr2=find_dangerous(c2);
  * C must have a method
    find2(dimension_type c1, dimension_type c2,
          iterator& itr1, iterator& itr2);
    returning void, equivalent to<BR>
    itr1=find(c1); itr2=find(c2);
  * C must have a const method
    find2(dimension_type c1, dimension_type c2,
          C::const_iterator& itr1, C::const_iterator& itr2)
    returning void, equivalent to<BR>
    itr1=find(c1); itr2=find(c2);
  * C must have a method
    insert(C::dangerous_iterator pos, const C::value_type& x)
    returning a C::dangerous_iterator. This method inserts x before pos and
    returns an iterator to the inserted element.
    This operation invalidates all C::dangerous_iterator objects equal to pos.
  * C must have a method
    insert(C::dangerous_iterator pos, dimension_type i, const Coefficient& x)
    returning a C::dangerous_iterator. This method inserts the pair (i, x)
    before pos and returns an iterator to the inserted element.
    This operation invalidates all C::dangerous_iterator objects equal to pos.
  * C must have a method push_back(const C::value_type& x) returning void,
    equivalent to<BR>
    insert(end_dangerous(), x);
  * C must have a method erase(C::dangerous_iterator pos) returning a
    C::dangerous_iterator, that erases the element pointed to by pos.
    This operation invalidates all C::dangerous_iterators objects equal to
    pos and ++pos.
  * C must have a method
    erase(C::dangerous_iterator first, C::dangerous_iterator last) that
    returns a C::dangerous_iterator, that erases the element in [first,last).
    This operation invalidates all dangerous_iterators equal to last.
  * C must have a method splice(C::dangerous_iterator& position, C& x) that
    returns a C::dangerous_iterator, that moves all elements in x before
    position. This operation invalidates all dangerous_iterators equal to
    the former position and all dangerous_iterators pointing to x.
    The returned iterator is a valid iterator pointing to the first inserted
    element. \p position is modified to keep it valid.
  * C must have a method
    splice(C::dangerous_iterator& position,C& x, C::dangerous_iterator i)
    returning a C::dangerous_iterator, that moves element i of x
    before position. This operation invalidates all dangerous_iterators equal
    to the former position, i and ++i.
    The returned iterator is a valid iterator pointing to the inserted
    element. \p position is modified to keep it valid.
  * C must have a method
    splice(C::dangerous_iterator& position, C& x,
           C::dangerous_iterator first, C::dangerous_iterator last)
    returning a C::dangerous_iterator, that moves elements [first,last) in
    x before position. This operation invalidates all dangerous_iterators
    equal to the former position, and in [first,last] (note that last is
    invalidated, too). The returned iterator is a valid iterator pointing to
    the first moved element. \p position is modified to keep it valid.
  * As a special exception, if PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES is
    defined in this file, right after the backend's include, every type of
    iterator and every reference to an element of the backend is invalidated
    by operations that add or remove elements in the list.
  * If PPL_SPARSE_BACKEND_SLOW_INSERTIONS is defined in this file,
    this means that insertion in the middle is slow. If this is the case,
    insertion at the end is used, is possible. This is only a suggestion, and
    doesn't weaken the backend requirements.
*/
class Unlimited_Sparse_Row_Over_Linear_Sequence {

public:
  typedef std::pair<dimension_type, Coefficient> value_type;

  //! Constructs an unlimited row of zeroes.
  Unlimited_Sparse_Row_Over_Linear_Sequence();

  //! Constructs an unlimited row from a std::vector.
  Unlimited_Sparse_Row_Over_Linear_Sequence(const std::vector<Coefficient> &v);

private:

#ifdef USE_PPL_SPARSE_BACKEND_STD_LIST
  typedef Unlimited_Sparse_Row_Std_List_Backend list_t;
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
  typedef Unlimited_Sparse_Row_Custom_Slist_Backend list_t;
#endif

#ifdef USE_PPL_SPARSE_BACKEND_STD_VECTOR
  typedef Unlimited_Sparse_Row_Std_Vector_Backend list_t;
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CO_TREE
  // Use the std::list-based backend, so this class compiles.
  // However, this class will not be used as sparse backend.
  typedef Unlimited_Sparse_Row_Std_List_Backend list_t;
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
  void swap(Unlimited_Sparse_Row_Over_Linear_Sequence& x);

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

  //! For each j>i, assigns (*this)[j-1] = (*this)[j].
  //! Invalidates dangerous_iterators pointing to the i-th and (i+1)-th
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
  void combine_needs_first(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, 0) must do nothing.
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
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
  void combine(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
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
  //! invalidates dangerous_iterator objects equal to the former
  //! lower_bound(i).
  Coefficient& operator[](const dimension_type i);

  //! Equivalent to find_create(i, x, begin_dangerous()) .
  iterator find_create(const dimension_type i, const Coefficient& x);

  //! Equivalent to find_create(x.first, x.second, begin_dangerous()) .
  iterator find_create(const std::pair<dimension_type, Coefficient>& x);

  //! Equivalent to find_create(i, begin_dangerous()) .
  iterator find_create(const dimension_type i);

  //! Equivalent to (*this)[i]=x , needs itr to point before the added
  //! element. If itr points near the added element, this is faster.
  iterator find_create(const dimension_type i, const Coefficient& x,
                       iterator itr);

  //! Equivalent to (*this)[x.first]=x.second , needs itr to point before the
  //! added element. If itr points near the added element, this is faster.
  iterator find_create(const std::pair<dimension_type, Coefficient>& x,
                       iterator itr);

  //! Equivalent to (*this)[i] , needs itr to point before the added
  //! element. If itr points near the added element, this is faster.
  iterator find_create(const dimension_type i, iterator itr);

  //! Equivalent to (*this)[i] , needs itr to point before or to the added
  //! element. If itr points near the added element, this is faster.
  dangerous_iterator find_create(const dimension_type i,
                                 dangerous_iterator itr);

  //! Equivalent to (*this)[i]=x , needs itr to point before or to the added
  //! element. If itr points near the added element, this is faster.
  dangerous_iterator find_create(const dimension_type i, const Coefficient& x,
                                 dangerous_iterator itr);

  //! Equivalent to (*this)[x.first]=x.second , needs itr to point before or
  //! to the added element. If itr points near the added element, this is
  //! faster.
  dangerous_iterator find_create(const std::pair<dimension_type, Coefficient>&
                                 x, dangerous_iterator itr);

  //! Equivalent to get(), provided for convenience.
  const Coefficient& operator[](const dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    This function is O(n).
  */
  const Coefficient& get(const dimension_type i) const;

  dangerous_iterator begin_dangerous();
  dangerous_iterator end_dangerous();
  iterator begin();
  iterator end();
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
  void for_each_nonzero(const Func& func, const dimension_type n);

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func, const dimension_type n) const;

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
  const_iterator lower_bound(const dimension_type c, const_iterator itr) const;

  //! A faster equivalent of
  //! itr1=find_dangerous(c1); itr2=find_dangerous(c2); .
  void find2_dangerous(const dimension_type c1, const dimension_type c2,
                       dangerous_iterator& itr1, dangerous_iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1, const dimension_type c2,
             iterator& itr1, iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1, const dimension_type c2,
             const_iterator& itr1, const_iterator& itr2) const;

  //! A faster equivalent of p1 = &(get(c1)); p1 = &(get(c2));
  void get2(const dimension_type c1, const dimension_type c2,
            const Coefficient*& p1, const Coefficient*& p2) const;

  bool operator==(const Unlimited_Sparse_Row_Over_Linear_Sequence &x) const;
  bool operator!=(const Unlimited_Sparse_Row_Over_Linear_Sequence &x) const;

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks the invariant.
  bool OK() const;

private:
  //! The std::list that contains the coefficients
  list_t data;
};

} // namespace Parma_Polyhedra_Library

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Unlimited_Sparse_Row_Over_Linear_Sequence */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Unlimited_Sparse_Row_Over_Linear_Sequence& x,
          Parma_Polyhedra_Library::Unlimited_Sparse_Row_Over_Linear_Sequence& y);

} // namespace std


#include "Unlimited_Sparse_Row_Over_Linear_Sequence.templates.hh"
#include "Unlimited_Sparse_Row_Over_Linear_Sequence.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_defs_hh)
