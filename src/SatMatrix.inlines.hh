/* SatMatrix class implementation: inline functions.
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

INLINE
Parma_Polyhedra_Library::SatMatrix::SatMatrix()
  : rows(),
    row_size(0) {
}

INLINE
Parma_Polyhedra_Library::SatMatrix::SatMatrix(size_t num_rows,
					      size_t num_columns)
  : rows(num_rows),
    row_size(num_columns) {
}

INLINE
Parma_Polyhedra_Library::SatMatrix::SatMatrix(const SatMatrix& y)
  : rows(y.rows),
    row_size(y.row_size) {
}

INLINE
Parma_Polyhedra_Library::SatMatrix::~SatMatrix() {
}

/*!
  Erases the rows from the \p first_to_erase -th to the last one.
*/
INLINE void
Parma_Polyhedra_Library::SatMatrix::rows_erase_to_end(size_t first_to_erase) {
  assert(OK());
  // The first row to be erased cannot be greater
  // than the actual number of the rows of the matrix.
  assert(first_to_erase <= rows.size());
  if (first_to_erase < rows.size())
    rows.erase(rows.begin() + first_to_erase, rows.end());
  assert(OK());
}

/*!
  Erases the columns from the \p first_to_erase -th to the last one.
*/
INLINE void 
Parma_Polyhedra_Library::SatMatrix::columns_erase_to_end(size_t
							 first_to_erase) {
  assert(OK());
  // The first column to be erased cannot be greater
  // than the actual number of the columns of the matrix.
  assert(first_to_erase <= row_size);
  row_size = first_to_erase;
  assert(OK());
}

/*!
  Swaps \p *this with \p y.
*/
INLINE void
Parma_Polyhedra_Library::SatMatrix::swap(SatMatrix& y) {
  assert(OK());
  std::swap(row_size, y.row_size);
  std::swap(rows, y.rows);
  assert(OK());
}

/*!
  Specializes <CODE>std::swap</CODE> to use the fast swap that 
  is provided as a member function instead of using the default 
  algorithm (which creates a temporary and uses assignment).
*/
INLINE void
std::swap(Parma_Polyhedra_Library::SatMatrix& x,
	  Parma_Polyhedra_Library::SatMatrix& y) {
  x.swap(y);
}

/*!
  Returns a reference to the \p k -th row.
*/
INLINE Parma_Polyhedra_Library::SatRow&
Parma_Polyhedra_Library::SatMatrix::operator [](size_t k) {
  assert(k < rows.size());
  return rows[k];
}

/*!
  Returns a constant reference to the \p k -th row. 
*/
INLINE const Parma_Polyhedra_Library::SatRow&
Parma_Polyhedra_Library::SatMatrix::operator [](size_t k) const {
  assert(k < rows.size());
  return rows[k];
}

INLINE size_t
Parma_Polyhedra_Library::SatMatrix::num_columns() const {
  return row_size;
}

INLINE size_t
Parma_Polyhedra_Library::SatMatrix::num_rows() const {
  return rows.size();
}

INLINE bool
Parma_Polyhedra_Library::operator !=(const SatMatrix& x, const SatMatrix& y) {
  return !(x == y);
}

INLINE bool
Parma_Polyhedra_Library::SatMatrix::RowCompare::
operator ()(const SatRow& x, const SatRow& y) const {
  return compare(x, y) < 0;
}
