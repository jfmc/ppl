/* Matrix class implementation: inline functions.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Integer.defs.hh"
#include <algorithm>
#include <cassert>

/*!
  Swaps *this with y.
*/
inline void
Parma_Polyhedra_Library::Matrix::swap(Matrix& y) {
  std::swap(rows, y.rows);
  std::swap(row_size, y.row_size);
  std::swap(row_capacity, y.row_capacity);
  std::swap(sorted, y.sorted);
}

/*!
  Specialize <CODE>std::swap</CODE> to use the fast swap that
  is provided as a member function instead of using the default
  algorithm (which creates a temporary and uses assignment).
*/
inline void
std::swap(Parma_Polyhedra_Library::Matrix& x,
	  Parma_Polyhedra_Library::Matrix& y) {
  x.swap(y);
}


/*!
  This is the comparator used for sorting the matrices.
*/
inline bool
Parma_Polyhedra_Library::Matrix::RowCompare::operator ()(const Row& x,
							 const Row& y) const {
  return compare(x, y) < 0;
}


/*!
  \param num_columns   The number of elements we want the rows to contain.

  \return              The actual capacity of the rows.

  Computes the row capacity given the required number of columns.
  Allows speculative allocation aimed at reducing the number of
  reallocations.
*/
inline size_t
Parma_Polyhedra_Library::Matrix::compute_row_capacity(size_t num_columns) {
  return num_columns;
}


/*!
  The default constructor initializes the rows' size and capacity to \f$0\f$.
  FIXME: This "zero-matrix" is temporarily set to be not sorted
         because, when we add some rows, they may not be
         sorted and if the flag had been set to \p true we
         would have to change it to \p false.
	 The fact is that a matrix with no rows is sorted.
*/
inline
Parma_Polyhedra_Library::Matrix::Matrix()
  : rows(),
    row_size(0),
    row_capacity(0),
    sorted(false) {
}


inline
Parma_Polyhedra_Library::Matrix::~Matrix() {
}


/*!
  Returns a reference to the \p k-th row of the matrix.
*/
inline Parma_Polyhedra_Library::Row&
Parma_Polyhedra_Library::Matrix::operator [](size_t k) {
  assert(k < rows.size());
  return rows[k];
}


/*!
  Returns a constant reference to the \p k-th row of the matrix.
*/
inline const Parma_Polyhedra_Library::Row&
Parma_Polyhedra_Library::Matrix::operator [](size_t k) const {
  assert(k < rows.size());
  return rows[k];
}

/*!
  Sets the \p sorted flag of the matrix to the given \p value.
*/
inline void
Parma_Polyhedra_Library::Matrix::set_sorted(bool value) {
  sorted = value;
}


/*!
  Returns the value of the flag \p sorted.
*/
inline bool
Parma_Polyhedra_Library::Matrix::is_sorted() const {
  // Since the flag `sorted' does not really reflect the
  // sort status of a matrix this assertion is used to be sure that the
  // matrix is really sorted when `sorted' value is 'true'.
  assert(!sorted || check_sorted());
  return sorted;
}


/*!
  Returns the number of the columns of the matrix,
  i.e., the size of the rows of the matrix.
*/
inline size_t
Parma_Polyhedra_Library::Matrix::num_columns() const {
  return row_size;
}


/*!
  Returns the number of the rows of the matrix.
*/
inline size_t
Parma_Polyhedra_Library::Matrix::num_rows() const {
  return rows.size();
}


inline bool
Parma_Polyhedra_Library::operator !=(const Matrix& x, const Matrix& y) {
  return !(x == y);
}


/*!
  \param first_to_erase   The row index from which start to erase.

  Erases from the matrix all the rows between the
  \p first_to_erase -th and the last one.
*/
inline void
Parma_Polyhedra_Library::Matrix::erase_to_end(size_t first_to_erase) {
  assert(first_to_erase <= rows.size());
  if (first_to_erase < rows.size())
    rows.erase(rows.begin() + first_to_erase, rows.end());
}

/*!
  Clears the matrix deallocating all its rows.
*/
inline void
Parma_Polyhedra_Library::Matrix::clear() {
  rows.clear();
  row_size = 0;
  row_capacity = 0;
  sorted = true;
}

