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

#include <vector>

namespace Parma_Polyhedra_Library {

class Unlimited_Sparse_Row {

private:
  typedef Unlimited_Sparse_Row This;

public:

  typedef CO_Tree::value_type value_type;

  Unlimited_Sparse_Row();

  Unlimited_Sparse_Row(const This& x);

  //! Constructs an unlimited row from an std::vector.
  Unlimited_Sparse_Row(const std::vector<Coefficient> &v);

  This& operator=(const This& x);

  void swap(This& x);

  class dangerous_iterator;
  class iterator;
  class const_iterator;

  class unordered_iterator;
  class unordered_const_iterator;

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

  //! Returns an unordered_iterator pointing to the first (key, value) pair,
  //! in the internal order.
  unordered_iterator unordered_begin();
  //! Returns an unordered_iterator pointing after the last pair, in
  //! the internal order.
  unordered_iterator unordered_end();

  //! Returns an unordered_const_iterator pointing to the first (key, value)
  //! pair, in the internal order.
  unordered_const_iterator unordered_begin() const;
  //! Returns an unordered_const_iterator pointing after the last pair, in
  //! the internal order.
  unordered_const_iterator unordered_end() const;

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

  //! Equivalent to itr = find_create(i, x) .
  //! This may be faster in some implementations.
  void find_create_assign(const dimension_type i, const Coefficient& x,
                          iterator& itr);

  //! Equivalent to itr = find_create(x) .
  //! This may be faster in some implementations.
  void find_create_assign(const std::pair<dimension_type, Coefficient>& x,
                          iterator& itr);

  //! Equivalent to itr = find_create(i) .
  //! This may be faster in some implementations.
  void find_create_assign(const dimension_type i, iterator& itr);

  //! Equivalent to itr = find_create(i, x, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const dimension_type i, const Coefficient& x,
                               iterator& itr);

  //! Equivalent to itr = find_create(x, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const std::pair<dimension_type, Coefficient>& x,
                               iterator& itr);

  //! Equivalent to itr = find_create(i, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const dimension_type i, iterator& itr);

  //! Equivalent to itr = find_create(i, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const dimension_type i,
                               dangerous_iterator& itr);

  //! Equivalent to itr = find_create(i, x, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const dimension_type i, const Coefficient& x,
                               dangerous_iterator& itr);

  //! Equivalent to itr = find_create(x, itr) .
  //! This may be faster in some implementations.
  void find_create_hint_assign(const std::pair<dimension_type, Coefficient>& x,
                               dangerous_iterator& itr);

  //! Equivalent to itr = find_dangerous(c).
  void find_assign(const dimension_type c, dangerous_iterator& itr);
  //! Equivalent to itr = find(c).
  void find_assign(const dimension_type c, iterator& itr);
  //! Equivalent to itr = find(c).
  void find_assign(const dimension_type c, const_iterator& itr) const;

  //! Equivalent to itr = lower_bound_dangerous(c)
  void lower_bound_assign(const dimension_type c, dangerous_iterator& itr);
  //! Equivalent to itr = lower_bound(c)
  void lower_bound_assign(const dimension_type c, iterator& itr);
  //! Equivalent to itr = lower_bound(c)
  void lower_bound_assign(const dimension_type c, const_iterator& itr) const;

  //! Equivalent to itr = find_dangerous(c, itr) .
  void find_hint_assign(const dimension_type c, dangerous_iterator& itr);
  //! Equivalent to itr = find(c, itr) .
  void find_hint_assign(const dimension_type c, iterator& itr);
  //! Equivalent to itr = find(c, itr) .
  void find_hint_assign(const dimension_type c, const_iterator& itr) const;

  //! Equivalent to itr = lower_bound_dangerous(c, itr) .
  void lower_bound_hint_assign(const dimension_type c, dangerous_iterator& itr);
  //! Equivalent to itr = lower_bound(c, itr) .
  void lower_bound_hint_assign(const dimension_type c, iterator& itr);
  //! Equivalent to itr = lower_bound(c, itr) .
  void lower_bound_hint_assign(const dimension_type c, const_iterator& itr) const;

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

  bool operator==(const This &x) const;
  bool operator!=(const This &x) const;

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks the invariant.
  bool OK() const;

private:

  void lower_bound_hint_assign(dimension_type i,
                               CO_Tree::inorder_iterator& itr);
  void lower_bound_hint_assign(dimension_type i,
                               CO_Tree::inorder_const_iterator& itr) const;

  void find_hint_assign(dimension_type i, CO_Tree::inorder_iterator& itr);
  void find_hint_assign(dimension_type i,
                        CO_Tree::inorder_const_iterator& itr) const;

  void find_create_assign(dimension_type i, const Coefficient& x,
                          CO_Tree::inorder_iterator& itr);

  void find_create_assign(dimension_type i, CO_Tree::inorder_iterator& itr);

  void find_create_hint_assign(dimension_type i, const Coefficient& x,
                               CO_Tree::inorder_iterator& itr);

  void find_create_hint_assign(dimension_type i,
                               CO_Tree::inorder_iterator& itr);

  //! Equivalent to (*this)[i]=x; itr = tree.lower_bound(i);.
  void find_create(dimension_type i, const Coefficient& x,
                   CO_Tree::inorder_iterator& itr);

  //! Equivalent to (*this)[i]; itr = tree.lower_bound(i);.
  void find_create(dimension_type i, CO_Tree::inorder_iterator& itr);

  CO_Tree tree;
};

class Unlimited_Sparse_Row::iterator {

public:

  typedef std::bidirectional_iterator_tag iterator_category;
  typedef Unlimited_Sparse_Row::value_type value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  class Member_Access_Helper {

  public:
    Member_Access_Helper(dimension_type key, CO_Tree::data_type& data);

    std::pair<dimension_type, Coefficient&>* operator->();

  private:
    std::pair<dimension_type, Coefficient&> my_pair;
  };

  iterator(CO_Tree* x = 0);
  iterator(const iterator& x);
  iterator(const CO_Tree::inorder_iterator& itr);

  iterator& operator=(const iterator& x);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  iterator& operator++();
  iterator& operator--();

  std::pair<dimension_type, Coefficient&> operator*();
  Member_Access_Helper operator->();

  std::pair<dimension_type, const Coefficient&> operator*() const;
  CO_Tree::inorder_iterator::Const_Member_Access_Helper operator->() const;

  operator const_iterator() const;

private:
  CO_Tree::inorder_iterator itr;

  friend class Unlimited_Sparse_Row;
};

class Unlimited_Sparse_Row::dangerous_iterator
  : public Unlimited_Sparse_Row::iterator {

public:

  dangerous_iterator(CO_Tree* x = 0);
  dangerous_iterator(const iterator& itr);
  dangerous_iterator(const CO_Tree::inorder_iterator& itr);
  dangerous_iterator(const dangerous_iterator& x);

  dangerous_iterator& operator=(const dangerous_iterator& x);

  //! Returns a dangerous_iterator pointing to the element after i.
  static dangerous_iterator next(const iterator& i);

  operator const_iterator() const;

private:

  friend class Unlimited_Sparse_Row;
};

class Unlimited_Sparse_Row::const_iterator {

public:

  typedef std::bidirectional_iterator_tag iterator_category;
  typedef const Unlimited_Sparse_Row::value_type
    value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  const_iterator(const CO_Tree* x = 0);
  const_iterator(const const_iterator& x);
  const_iterator(const CO_Tree::inorder_iterator& x);
  const_iterator(const CO_Tree::inorder_const_iterator& x);

  const_iterator& operator=(const const_iterator& x);

  const_iterator& operator++();
  const_iterator& operator--();

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

  std::pair<dimension_type, const Coefficient&> operator*() const;
  CO_Tree::inorder_const_iterator::Const_Member_Access_Helper
    operator->() const;

private:
  CO_Tree::inorder_const_iterator itr;

  friend class Unlimited_Sparse_Row;
};

//! An iterator over the (key, value) pairs, that scans the pairs
//! in the internal order (the pairs are not ordered by key).
class Unlimited_Sparse_Row::unordered_iterator
  : public CO_Tree::unordered_iterator {

private:
  typedef CO_Tree::unordered_iterator Base;

public:

  class Member_Access_Helper {

  public:
    Member_Access_Helper(dimension_type key, Coefficient& data);

    std::pair<dimension_type, Coefficient&>* operator->();

  private:
    std::pair<dimension_type, Coefficient&> my_pair;
  };

  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const Coefficient& data);

    const std::pair<dimension_type, const Coefficient&>* operator->() const;

  private:
    std::pair<dimension_type, const Coefficient&> my_pair;
  };

  unordered_iterator(const Base& itr);

  std::pair<dimension_type, Coefficient&> operator*();
  std::pair<dimension_type, const Coefficient&> operator*() const;

  Member_Access_Helper operator->();
  Const_Member_Access_Helper operator->() const;

  operator unordered_const_iterator() const;
};

//! A const iterator over the (key, value) pairs, that scans the pairs
//! in the internal order (the pairs are not ordered by key).
class Unlimited_Sparse_Row::unordered_const_iterator
  : public CO_Tree::unordered_const_iterator {

private:
  typedef CO_Tree::unordered_const_iterator Base;

public:
  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const Coefficient& data);

    const std::pair<dimension_type, const Coefficient&>* operator->()
      const;

  private:
    std::pair<dimension_type, const Coefficient&> my_pair;
  };

  unordered_const_iterator(const Base& itr);

  std::pair<dimension_type, const Coefficient&> operator*() const;

  Const_Member_Access_Helper operator->() const;
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
