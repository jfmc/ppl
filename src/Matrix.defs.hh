/* Matrix class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Matrix_defs_hh
#define PPL_Matrix_defs_hh 1

#include "Matrix.types.hh"
#include "Row.defs.hh"
#include "Constraint_System.types.hh"
#include "GenSys.types.hh"
#include "Integer.types.hh"
#include <vector>
#include <cstddef>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A 2-dimensional matrix of coefficients.
/*!
  A Matrix object is a sequence of Row objects and is characterized
  by the matrix dimensions (the number of rows and columns).
  All the rows in a matrix, besides having the same size (corresponding
  to the number of columns of the matrix), are also bound to have the
  same capacity.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Matrix {
protected:
  //! Returns the maximum number of rows of a Matrix.
  static dimension_type max_num_rows();

  //! Returns the maximum number of columns of a Matrix.
  static dimension_type max_num_columns();

  //! Builds an empty matrix.
  /*!
    Rows' size and capacity are initialized to \f$0\f$.
  */
  Matrix();

  //! Builds a zero matrix with specified dimensions and flags.
  /*!
    \param n_rows
    The number of rows of the matrix that will be created;

    \param n_columns
    The number of columns of the matrix that will be created.

    \param row_flags
    The flags used to build the rows of the matrix;
    by default, the rows will have all flags unset.
  */
  Matrix(dimension_type n_rows, dimension_type n_columns,
	 Row::Flags row_flags = Row::Flags());

  //! Copy-constructor.
  Matrix(const Matrix& y);

  //! Destructor.
  ~Matrix();

  //! Assignment operator.
  Matrix& operator=(const Matrix& y);

public:
  //! An iterator over a matrix.
  /*!
    A const_iterator is used to provide read-only access
    to each row contained in a Matrix object.
  */
  class const_iterator {
  private:
    typedef std::vector<Row>::const_iterator Iter;
    //! The const iterator on the rows' vector \p rows.
    Iter i;

  public:
    typedef std::forward_iterator_tag iterator_category;
    typedef std::iterator_traits<Iter>::value_type value_type;
    typedef std::iterator_traits<Iter>::difference_type difference_type;
    typedef std::iterator_traits<Iter>::pointer pointer;
    typedef std::iterator_traits<Iter>::reference reference;

    //! Default constructor.
    const_iterator();

    //! \brief
    //! Builds a const iterator on the matrix starting from
    //! an iterator \p b on the elements of the vector \p rows.
    explicit const_iterator(const Iter& b);

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    reference operator*() const;

    //! Indirect member selector.
    pointer operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator==(const const_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator!=(const const_iterator& y) const;
  };

  //! \brief
  //! Returns the const_iterator pointing to the first row,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  // FIXME: the following section must become private.
protected:
  //! Contains the rows of the matrix.
  std::vector<Row> rows;

  //! Size of the initialized part of each row.
  dimension_type row_size;

  //! Capacity allocated for each row.
  dimension_type row_capacity;

public:
  //! Swaps \p *this with \p y.
  void swap(Matrix& y);

  //! Adds to the matrix \p n rows of zeroes with flags set to \p row_flags.
  /*!
    \param n
    The number of rows to be added: must be strictly positive.

    \param row_flags
    Flags for the newly added rows.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times c\f$ matrix \f$M \choose 0\f$.
    The matrix is expanded avoiding reallocation whenever possible.
  */
  void add_zero_rows(dimension_type n, Row::Flags row_flags);

  //! Adds \p n columns of zeroes to the matrix.
  /*!
    \param n
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$r \times (c+n)\f$ matrix \f$(M \, 0)\f$.
    The matrix is expanded avoiding reallocation whenever possible.
  */
  void add_zero_columns(dimension_type n);

  //! Adds \p n rows and \p m columns of zeroes to the matrix.
  /*!
    \param n
    The number of rows to be added: must be strictly positive.

    \param m
    The number of columns to be added: must be strictly positive.

    \param row_flags
    Flags for the newly added rows.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+m)\f$ matrix
    \f$\bigl({M \atop 0}{0 \atop 0}\bigr)\f$.
    The matrix is expanded avoiding reallocation whenever possible.
  */
  void add_zero_rows_and_columns(dimension_type n, dimension_type m,
				 Row::Flags row_flags);

  //! Makes the matrix shrink by removing its \p n trailing columns.
  void remove_trailing_columns(dimension_type n);

  //! Resizes the matrix without worrying about the old contents.
  /*!
    \param new_n_rows
    The number of rows of the resized matrix;

    \param new_n_columns
    The number of columns of the resized matrix.

    \param row_flags
    The flags of the rows eventually added to the matrix.

    The matrix is expanded to the specified dimensions avoiding
    reallocation whenever possible.
    The contents of the original matrix is lost.
  */
  void resize_no_copy(dimension_type new_n_rows, dimension_type new_n_columns,
		      Row::Flags row_flags);

  //! Swaps the columns having indexes \p i and \p j.
  void swap_columns(dimension_type i,  dimension_type j);

  //! Permutes the columns of the matrix.
  /*
    \param cycles
    A vector representing the non-trivial cycles of the permutation
    according to which the columns must be rearranged.

    The \p cycles vector contains, one after the other, the
    non-trivial cycles (i.e., the cycles of length greater than one)
    of a permutation of non-zero column indexes.  Each cycle is
    terminated by zero.  For example, assuming the matrix has 6
    columns, the permutation \f$ \{ 1 \mapsto 3, 2 \mapsto 4,
    3 \mapsto 6, 4 \mapsto 2, 5 \mapsto 5, 6 \mapsto 1 \}\f$ can be
    represented by the non-trivial cycles \f$(1 3 6)(2 4)\f$ that, in
    turn can be represented by a vector of 6 elements containing 1, 3,
    6, 0, 2, 4, 0.
  */
  void permute_columns(const std::vector<dimension_type>& cycles);

  //! \name Accessors
  //@{

  //! \brief
  //! Returns the number of columns of the matrix
  //! (i.e., the size of the rows).
  dimension_type num_columns() const;

  //! Returns the number of rows in the matrix.
  dimension_type num_rows() const;
  //@} // Accessors

  //! \name Subscript operators
  //@{
  //! Returns a reference to the \p k-th row of the matrix.
  Row& operator[](dimension_type k);

  //! Returns a constant reference to the \p k-th row of the matrix.
  const Row& operator[](dimension_type k) const;
  //@} // Subscript operators

  //! Clears the matrix deallocating all its rows.
  void clear();

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! \brief
  //! Erases from the matrix all the rows but those having
  //! an index less than \p first_to_erase.
  void erase_to_end(dimension_type first_to_erase);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Matrix& x,
	  Parma_Polyhedra_Library::Matrix& y);

} // namespace std


namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are identical.
/*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator==(const Matrix& x, const Matrix& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool operator!=(const Matrix& x, const Matrix& y);

} // namespace Parma_Polyhedra_Library

#include "Matrix.inlines.hh"

#endif // !defined(PPL_Matrix_defs_hh)
