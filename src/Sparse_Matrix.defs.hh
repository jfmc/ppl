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

class Parma_Polyhedra_Library::Sparse_Matrix {

public:
  class iterator;
  typedef std::vector<Unlimited_Sparse_Row>::const_iterator const_iterator;

  //! Constructs a square matrix filled with zeroes with the given size.
  explicit Sparse_Matrix(dimension_type n = 0);

  //! Constructs a matrix filled with zeroes with the given dimensions.
  Sparse_Matrix(dimension_type num_rows, dimension_type num_columns);

  //! Swaps (*this) with x.
  void swap(Sparse_Matrix& x);

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  Sparse_Row_Reference operator[](dimension_type i);
  const Unlimited_Sparse_Row& operator[](dimension_type i) const;

  dimension_type num_rows() const;
  dimension_type num_columns() const;

  //! Removes column i from the matrix, shifting other columns to the left.
  void remove_column(dimension_type i);

  //! Permutes the columns of the matrix.
  /*!
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

  void resize(dimension_type n);
  void resize(dimension_type num_rows, dimension_type num_columns);

  //! Equivalent to resize(0,0). Provided for compatibility with Dense_Matrix.
  void clear();

  //! Adds to the matrix \p n rows of zeroes.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of rows to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times c\f$ matrix \f$\genfrac{(}{)}{0pt}{}{M}{0}\f$.
  */
  void add_zero_rows(dimension_type n);

  //! Adds \p n columns of zeroes to the matrix.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$r \times (c+n)\f$ matrix \f$(M \, 0)\f$.
  */
  void add_zero_columns(dimension_type n);

  //! Adds \p n columns of zeroes to the matrix, starting from column i.
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
  */
  void add_zero_rows_and_columns(dimension_type n, dimension_type m);

  //! Adds the row \p x to the matrix.
  void add_row(const Sparse_Row& x);

  //! Adds the row \p x to the matrix.
  void add_row(const Sparse_Row_Reference& x);

  //! Adds the row \p x to the matrix.
  void add_row(const Unlimited_Sparse_Row& x);

  //! Makes the matrix shrink by removing its \p n trailing columns.
  //! Provided for compatibility with Dense_Matrix.
  void remove_trailing_columns(dimension_type n);

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Calls func on each row. func should take a Sparse_Row_Reference& argument.
  template <typename Func>
  void for_each_row(const Func& func);

  //! Calls func on each row. func should take a const Unlimited_Sparse_Row&
  //! argument.
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
  std::vector<Unlimited_Sparse_Row> rows;
  dimension_type num_columns_;
};

class Parma_Polyhedra_Library::Sparse_Matrix::iterator {

public:
  iterator(const iterator&);

  Sparse_Row_Reference operator*();

  iterator& operator++();
  iterator operator++(int);

private:
  iterator(std::vector<Unlimited_Sparse_Row>::iterator,
           dimension_type size);

  std::vector<Unlimited_Sparse_Row>::iterator itr;
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
