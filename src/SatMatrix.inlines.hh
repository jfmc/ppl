/* SatMatrix class implementation: inline functions.
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

#ifndef PPL_SatMatrix_inlines_hh
#define PPL_SatMatrix_inlines_hh 1

#include <algorithm>
#include <cassert>

namespace Parma_Polyhedra_Library {

inline
SatMatrix::SatMatrix()
  : rows(),
    row_size(0) {
}

inline dimension_type 
SatMatrix::max_num_rows() {
  static const dimension_type max_nr = std::vector<SatRow>().max_size();
  return max_nr;
}

inline
SatMatrix::SatMatrix(const dimension_type n_rows,
		     const dimension_type n_columns)
  : rows(n_rows),
    row_size(n_columns) {
}

inline
SatMatrix::SatMatrix(const SatMatrix& y)
  : rows(y.rows),
    row_size(y.row_size) {
}

inline
SatMatrix::~SatMatrix() {
}

inline void
SatMatrix::rows_erase_to_end(const dimension_type first_to_erase) {
  // The first row to be erased cannot be greater
  // than the actual number of the rows of the matrix.
  assert(first_to_erase <= rows.size());
  if (first_to_erase < rows.size())
    rows.erase(rows.begin() + first_to_erase, rows.end());
  assert(OK());
}

inline void
SatMatrix::columns_erase_to_end(const dimension_type first_to_erase) {
  // The first column to be erased cannot be greater
  // than the actual number of the columns of the matrix.
  assert(first_to_erase <= row_size);
  row_size = first_to_erase;
  assert(OK());
}

inline void
SatMatrix::swap(SatMatrix& y) {
  std::swap(row_size, y.row_size);
  std::swap(rows, y.rows);
}

inline SatRow&
SatMatrix::operator[](const dimension_type k) {
  assert(k < rows.size());
  return rows[k];
}

inline const SatRow&
SatMatrix::operator[](const dimension_type k) const {
  assert(k < rows.size());
  return rows[k];
}

inline dimension_type
SatMatrix::num_columns() const {
  return row_size;
}

inline dimension_type
SatMatrix::num_rows() const {
  return rows.size();
}

inline void
SatMatrix::clear() {
  // Clear `rows' and minimize its capacity.
  std::vector<SatRow>().swap(rows);
  row_size = 0;
}

inline memory_size_type
SatMatrix::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline bool
SatMatrix::SatRowLessThan::
operator()(const SatRow& x, const SatRow& y) const {
  return compare(x, y) < 0;
}

inline bool
SatMatrix::sorted_contains(const SatRow& row) const {
  assert(check_sorted());
  return std::binary_search(rows.begin(), rows.end(), row, SatRowLessThan());
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::SatMatrix */
inline void
swap(Parma_Polyhedra_Library::SatMatrix& x,
     Parma_Polyhedra_Library::SatMatrix& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_SatMatrix_inlines_hh)
