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
class Parma_Polyhedra_Library::Dense_Row {
public:

  class iterator;
  class const_iterator;

  //! Pre-constructs a row: construction must be completed by construct().
  Dense_Row();

  //! \name Post-constructors
  //@{
  //! Constructs properly a default-constructed element.
  /*!
    Builds a row with size and capacity \p sz.
  */
  void construct(dimension_type sz);

  //! Constructs properly a default-constructed element.
  /*!
    \param sz
    The size of the row that will be constructed;

    \param capacity
    The capacity of the row that will be constructed;

    The row that is constructed has storage for \p capacity elements,
    \p sz of which are default-constructed now.
  */
  void construct(dimension_type sz, dimension_type capacity);
  //@} // Post-constructors

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

  //! Destructor.
  ~Dense_Row();

  //! Assignment operator.
  Dense_Row& operator=(const Dense_Row& y);

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  //! Resets the i-th element to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(const dimension_type i);

  //! Resets the elements [first,last) to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(const dimension_type first,const dimension_type last);

  //! Swaps \p *this with \p y.
  void swap(Dense_Row& y);

  //! Assigns the implementation of \p y to \p *this.
  /*!
    To be used with extra care, since it may easily cause memory leaks
    or undefined behavior.
  */
  void assign(Dense_Row& y);

  //! Expands the row to size \p new_size.
  /*!
    Adds new positions to the implementation of the row
    obtaining a new row with size \p new_size.
    It is assumed that \p new_size is between the current size
    and capacity of the row.
  */
  void expand_within_capacity(dimension_type new_size);

  //! Shrinks the row by erasing elements at the end.
  /*!
    Destroys elements of the row implementation
    from position \p new_size to the end.
    It is assumed that \p new_size is not greater than the current size.
  */
  void shrink(dimension_type new_size);

  //! Returns the size() of the largest possible Row.
  static dimension_type max_size();

  //! Gives the number of coefficients currently in use.
  dimension_type size() const;

  //! Gets the i-th element.
  //! Provided for compatibility with Sparse_Row.
  const Coefficient& get(const dimension_type i) const;

  //! \name Subscript operators
  //@{
  //! Returns a reference to the element of the row indexed by \p k.
  Coefficient& operator[](dimension_type k);

  //! Returns a constant reference to the element of the row indexed by \p k.
  Coefficient_traits::const_reference operator[](dimension_type k) const;
  //@} // Subscript operators

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  /*! \brief
    Returns a lower bound to the size in bytes of the memory
    managed by \p *this.
  */
  memory_size_type external_memory_in_bytes() const;

  /*! \brief
    Returns the total size in bytes of the memory occupied by \p *this,
    provided the capacity of \p *this is given by \p capacity.
  */
  memory_size_type total_memory_in_bytes(dimension_type capacity) const;

  /*! \brief
    Returns the size in bytes of the memory managed by \p *this,
    provided the capacity of \p *this is given by \p capacity.
  */
  memory_size_type external_memory_in_bytes(dimension_type capacity) const;

  operator Row&();
  operator const Row&() const;

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Unlimited_Sparse_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(Func func,const dimension_type n);

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Unlimited_Sparse_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(Func func,const dimension_type n) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  /*! \brief
    Checks if all the invariants are satisfied and that the actual
    size and capacity match the values provided as arguments.
  */
  bool OK(dimension_type row_size, dimension_type row_capacity) const;

  bool operator==(const Dense_Row& y) const;

private:
  Row row;
};

class Parma_Polyhedra_Library::Dense_Row::iterator {
public:
  typedef std::pair<const dimension_type,Coefficient&> value_type;
  typedef std::pair<const dimension_type,const Coefficient&> const_type;

  iterator(Dense_Row& row1,dimension_type i1);

  value_type operator*();
  const_type operator*() const;

  iterator& operator++();
  iterator operator++(int);

  iterator& operator--();
  iterator operator--(int);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  operator const_iterator() const;

  bool OK() const;

private:
  Dense_Row& row;
  dimension_type i;
};

class Parma_Polyhedra_Library::Dense_Row::const_iterator {
public:

  typedef std::pair<const dimension_type,const Coefficient&> const_type;

  const_iterator(const Dense_Row& row1,dimension_type i1);

  const_type operator*() const;

  const_iterator& operator++();
  const_iterator operator++(int);

  const_iterator& operator--();
  const_iterator operator--(int);

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

  bool OK() const;

private:
  const Dense_Row& row;
  dimension_type i;
};

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates Dense_Row */
bool operator!=(const Dense_Row& x, const Dense_Row& y);

} // namespace Parma_Polyhedra_Library

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


#include "Dense_Row.inlines.hh"

#endif // !defined(PPL_Dense_Row_defs_hh)
