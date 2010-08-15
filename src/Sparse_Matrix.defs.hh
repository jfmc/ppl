/* Sparse_Matrix class declaration.
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

#ifndef PPL_Sparse_Matrix_defs_hh
#define PPL_Sparse_Matrix_defs_hh 1

#include "Sparse_Matrix.types.hh"
#include "Sparse_Row.defs.hh"
#include "Unlimited_Sparse_Row.defs.hh"
#include "globals.types.hh"
#include <vector>

//! A sparse matrix of Coefficient.
class Parma_Polyhedra_Library::Sparse_Matrix {

public:
  typedef Sparse_Row_Reference row_reference_type;
  typedef const Unlimited_Sparse_Row& row_const_reference_type;
  typedef const Unlimited_Sparse_Row* row_const_pointer_type;
  typedef Sparse_Row_Reference::iterator row_iterator;
  typedef Sparse_Row_Reference::const_iterator row_const_iterator;
  typedef Unlimited_Sparse_Row::const_iterator const_row_const_iterator;
  typedef Sparse_Row row_copy_type;

  class iterator;
  typedef std::vector<Unlimited_Sparse_Row>::const_iterator const_iterator;

  /*!
    \brief Constructs a square matrix with the given size, filled with
           unstored zeroes.
  */
  explicit Sparse_Matrix(dimension_type n = 0);

  /*!
    \brief Constructs a matrix with the given dimensions, filled with unstored
           zeroes.
  */
  Sparse_Matrix(dimension_type num_rows, dimension_type num_columns);

  //! Swaps (*this) with x.
  void swap(Sparse_Matrix& x);

  //! Returns an iterator pointing to the first row.
  iterator begin();

  //! Returns an iterator pointing after the last row.
  iterator end();

  //! Returns an iterator pointing to the first row.
  const_iterator begin() const;

  //! Returns an iterator pointing after the last row.
  const_iterator end() const;

  //! Returns a reference to the i-th row.
  Sparse_Row_Reference operator[](dimension_type i);

  //! Returns a const reference to the i-th row.
  const Unlimited_Sparse_Row& operator[](dimension_type i) const;

  //! Returns the number of rows in the matrix.
  dimension_type num_rows() const;

  //! Returns the number of columns in the matrix.
  dimension_type num_columns() const;

  //! Removes the i-th from the matrix, shifting other columns to the left.
  /*!
    This operation invalidates existing iterators on rows' elements.

    This method takes $O(k + \sum_{j=1}^{r} log(n_j))$ time, with k the
    number of stored elements with column index greater than i, r the number
    of rows in this matrix and $n_j$ the number of stored elements in row j.
  */
  void remove_column(dimension_type i);

  //! Permutes the columns of the matrix.
  /*!
    This method is provided for compatibilty with Dense_Matrix but it is slow
    and should be avoided if possible.

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

    This method takes $O(k*\sum_{j=1}^{r} log(n_j))$ time, with k the size of
    the \p cycles vector, r the number of rows and $n_j$ the number of stored
    elements in row j.
  */
  void permute_columns(const std::vector<dimension_type>& cycles);

  //! Equivalent to resize(n, n).
  void resize(dimension_type n);

  //! Resizes this matrix to the specified dimensions.
  /*!
    New rows and columns will contain non-stored zeroes.

    Adding n rows takes O(n) time.
    Adding n columns takes O(1) time.
    Removing n rows takes O(n) time.
    Removing n columns takes $O(\sum_{j=1}^{r} k_j*log(n_j))$ time, with r
    the number of rows, $k_j$ the number of stored elements in the columns of
    the j-th row that must be removed and $n_j$ the number of stored
    elements in the j-th row.

    This operation invalidates existing iterators.
  */
  void resize(dimension_type num_rows, dimension_type num_columns);

  //! Equivalent to resize(0,0).
  /*!
    Provided for compatibility with Dense_Matrix.
  */
  void clear();

  //! Adds to the matrix \p n rows of zeroes.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of rows to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times c\f$ matrix \f$\genfrac{(}{)}{0pt}{}{M}{0}\f$.

    This method takes O(n) time.

    This operation invalidates existing iterators.
  */
  void add_zero_rows(dimension_type n);

  //! Adds \p n columns of zeroes to the matrix.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$r \times (c+n)\f$ matrix \f$(M \, 0)\f$.

    This method takes O(1) time.

    This operation invalidates existing iterators.
  */
  void add_zero_columns(dimension_type n);

  //! Adds \p n columns of non-stored zeroes to the matrix before column i.
  /*!
    This method takes $O(\sum_{j=1}^{r} k_j*log(n_j))$ time, with r the
    number of rows, $k_j$ the number of stored elements in the columns of
    the j-th row that must be shifted and $n_j$ the number of stored
    elements in the j-th row.

    This operation invalidates existing iterators.
  */
  void add_zero_columns(dimension_type n, dimension_type i);

  //! Adds \p n rows and \p m columns of zeroes to the matrix.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of rows to be added: must be strictly positive.

    \param m
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+m)\f$ matrix
    \f$\bigl(\genfrac{}{}{0pt}{}{M}{0} \genfrac{}{}{0pt}{}{0}{0}\bigr)\f$.

    This operation invalidates existing iterators.
  */
  void add_zero_rows_and_columns(dimension_type n, dimension_type m);

  //! Adds a copy of the row \p x to the matrix.
  /*!
    This operation invalidates existing iterators.
  */
  void add_row(const Sparse_Row& x);

  //! Adds the row \p x to the matrix.
  /*!
    This operation invalidates existing iterators.
  */
  void add_row(const Sparse_Row_Reference& x);

  //! Adds a copy of the row \p x to the matrix.
  /*!
    This operation invalidates existing iterators.
  */
  void add_row(const Unlimited_Sparse_Row& x);

  //! Shrinks the matrix by removing its \p n trailing columns.
  /*!
    This method is provided for compatibility with Dense_Matrix.

    This operation invalidates existing iterators.
  */
  void remove_trailing_columns(dimension_type n);

  //! Loads the row from an ASCII representation generated using ascii_dump().
  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Calls func on each row.
  /*!
    func should take a Sparse_Row_Reference& argument.
  */
  template <typename Func>
  void for_each_row(const Func& func);

  //! Calls func on each row.
  /*!
    func should take a const Unlimited_Sparse_Row& argument.
  */
  template <typename Func>
  void for_each_row(const Func& func) const;

  /*! \brief
    Erases from the matrix all the rows but those having
    an index less than \p first_to_erase.

    Provided for compatibility with Dense_Row.
    It is equivalent to resize(first_to_erase,num_columns()).
  */
  void erase_to_end(dimension_type first_to_erase);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! The vector that stores the matrix's elements.
  std::vector<Unlimited_Sparse_Row> rows;

  //! The number of columns in this matrix.
  dimension_type num_columns_;
};

//! An iterator over the matrix's rows.
class Parma_Polyhedra_Library::Sparse_Matrix::iterator {

public:
  //! The copy constructor.
  iterator(const iterator&);

  //! Assigns itr into *this.
  iterator& operator=(const iterator& itr);

  //! Compares itr with *this.
  bool operator==(const iterator& itr) const;

  //! Compares itr with *this.
  bool operator!=(const iterator& itr) const;

  //! Returns a reference to the pointed row.
  Sparse_Row_Reference operator*();

  //! Advances to the next row.
  iterator& operator++();

  //! Advances to the next row.
  iterator operator++(int);

private:
  //! The default constructor.
  /*!
    \param size is the number of columns in the matrix.

    This is private so only Sparse_Matrix can access this.
  */
  iterator(std::vector<Unlimited_Sparse_Row>::iterator,
           dimension_type size);

  //! The wrapped iterator.
  std::vector<Unlimited_Sparse_Row>::iterator itr;

  //! The number of columns in the matrix.
  const dimension_type size_;

  friend class Parma_Polyhedra_Library::Sparse_Matrix;
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Sparse_Matrix */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Sparse_Matrix& x,
          Parma_Polyhedra_Library::Sparse_Matrix& y);

} // namespace std


#include "Sparse_Matrix.inlines.hh"

#endif // !defined(PPL_Sparse_Matrix_defs_hh)
