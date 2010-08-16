/* Dense_Row class declaration.
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

#ifndef PPL_Dense_Row_defs_hh
#define PPL_Dense_Row_defs_hh 1

#include "Dense_Row.types.hh"
#include "Row.defs.hh"

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A finite sequence of coefficients.
/*! \ingroup PPL_CXX_interface */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
class Parma_Polyhedra_Library::Dense_Row : public Row {
public:

  class iterator;
  class const_iterator;

  //! Pre-constructs a row: construction must be completed by construct().
  Dense_Row();

  //! Tight constructor: resizing may require reallocation.
  /*!
    Constructs a row with size and capacity \p sz.
  */
  Dense_Row(dimension_type sz);

  //! Sizing constructor with capacity.
  /*!
    \param sz
    The size of the row that will be constructed;

    \param capacity
    The capacity of the row that will be constructed;

    The row that is constructed has storage for \p capacity elements,
    \p sz of which are default-constructed now.
  */
  Dense_Row(dimension_type sz, dimension_type capacity);

  //! Ordinary copy constructor.
  Dense_Row(const Dense_Row& y);

  //! Copy constructor with specified capacity.
  /*!
    It is assumed that \p capacity is greater than or equal to
    the size of \p y.
  */
  Dense_Row(const Dense_Row& y, dimension_type capacity);

  //! Copy constructor with specified size and capacity.
  /*!
    It is assumed that \p sz is greater than or equal to the size of \p y
    and, of course, that \p sz is less than or equal to \p capacity.
  */
  Dense_Row(const Dense_Row& y, dimension_type sz, dimension_type capacity);

  //! Swaps the i-th element with the j-th element.
  //! Provided for compatibility with Sparse_Row
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  //! Provided for compatibility with Sparse_Row
  void swap(iterator i, iterator j);

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  //! Resets the i-th element to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(dimension_type i);

  //! Resets the elements [first,last) to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(dimension_type first, dimension_type last);

  //! Resets the element pointed to by itr to 0.
  //! Provided for compatibility with Sparse_Row.
  iterator reset(iterator itr);

  //! Swaps \p *this with \p y.
  void swap(Dense_Row& y);

  //! Gets the i-th element.
  //! Provided for compatibility with Sparse_Row.
  const Coefficient& get(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator find(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find(iterator itr, dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator lower_bound(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator lower_bound(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator lower_bound(iterator itr, dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(iterator itr, dimension_type i);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
    \param g should take a Coefficient& and a const Coefficient&.
    g(c1, c2) must do nothing if c1 is zero.
    f(c1) must be equivalent to g(c1, 0).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Dense_Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, 0) must do nothing.
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Dense_Row& y,
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
  void combine(const Dense_Row& y,
               const Func1& f, const Func2& g, const Func3& h);
};

class Parma_Polyhedra_Library::Dense_Row::iterator {
public:

  typedef std::pair<const dimension_type,Coefficient&> value_type;
  typedef std::pair<const dimension_type,const Coefficient&> const_type;

private:

  class Member_Access_Helper {
  public:

    Member_Access_Helper(dimension_type index, Coefficient& data);

    value_type* operator->();

  private:
    value_type value;
  };

  class Const_Member_Access_Helper {
  public:

    Const_Member_Access_Helper(dimension_type index,
                               const Coefficient& data);

    const const_type* operator->() const;

  private:
    const_type value;
  };

public:

  iterator();
  iterator(Dense_Row& row1, dimension_type i1);

  value_type operator*();
  const_type operator*() const;

  Member_Access_Helper operator->();
  Const_Member_Access_Helper operator->() const;

  iterator& operator++();
  iterator operator++(int);

  iterator& operator--();
  iterator operator--(int);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  operator const_iterator() const;

  bool OK() const;

private:
  Dense_Row* row;
  dimension_type i;
};

class Parma_Polyhedra_Library::Dense_Row::const_iterator {
public:
  typedef std::pair<const dimension_type, const Coefficient&> const_type;

private:

  class Const_Member_Access_Helper {
  public:

    Const_Member_Access_Helper(dimension_type index,
                               const Coefficient& data);

    const const_type* operator->() const;

  private:
    const_type value;
  };

public:

  const_iterator();
  const_iterator(const Dense_Row& row1, dimension_type i1);

  const_type operator*() const;
  Const_Member_Access_Helper operator->() const;

  const_iterator& operator++();
  const_iterator operator++(int);

  const_iterator& operator--();
  const_iterator operator--(int);

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

  bool OK() const;

private:
  const Dense_Row* row;
  dimension_type i;
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Dense_Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Dense_Row& x,
          Parma_Polyhedra_Library::Dense_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::iter_swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Dense_Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void iter_swap(std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator x,
               std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator y);

} // namespace std


#include "Dense_Row.templates.hh"
#include "Dense_Row.inlines.hh"

#endif // !defined(PPL_Dense_Row_defs_hh)
