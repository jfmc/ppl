/* Matrix class implementation: inline functions.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Matrix_inlines_hh
#define PPL_Matrix_inlines_hh 1

#include <algorithm>
#include <cassert>

namespace Parma_Polyhedra_Library {

inline void
Matrix::swap(Matrix& y) {
  std::swap(rows, y.rows);
  std::swap(row_topology, y.row_topology);
  std::swap(row_size, y.row_size);
  std::swap(row_capacity, y.row_capacity);
  std::swap(index_first_pending, y.index_first_pending);
  std::swap(sorted, y.sorted);
}

inline
Matrix::Matrix(Topology topol)
  : rows(),
    row_topology(topol),
    row_size(0),
    row_capacity(0),
    index_first_pending(0),
    sorted(true) {
}

inline
Matrix::~Matrix() {
}

inline Row&
Matrix::operator[](dimension_type k) {
  assert(k < rows.size());
  return rows[k];
}

inline const Row&
Matrix::operator[](dimension_type k) const {
  assert(k < rows.size());
  return rows[k];
}

inline dimension_type
Matrix::num_rows() const {
  return rows.size();
}

inline dimension_type
Matrix::first_pending_row() const {
  return index_first_pending;
}

inline dimension_type
Matrix::num_pending_rows() const {
  assert(num_rows() >= first_pending_row());
  return num_rows() - first_pending_row();
}

inline void
Matrix::unset_pending_rows() {
  index_first_pending = num_rows();
}

inline void
Matrix::set_index_first_pending_row(dimension_type first_pending) {
  index_first_pending = first_pending;
}

inline void
Matrix::set_necessarily_closed() {
  row_topology = NECESSARILY_CLOSED;
  if (num_rows() > 0)
    set_rows_topology();
}

inline void
Matrix::set_not_necessarily_closed() {
  row_topology = NOT_NECESSARILY_CLOSED;
  if (num_rows() > 0)
    set_rows_topology();
}

inline bool
Matrix::is_necessarily_closed() const {
  return row_topology == NECESSARILY_CLOSED;
}

inline Topology
Matrix::topology() const {
  return row_topology;
}

inline void
Matrix::set_sorted(bool value) {
  sorted = value;
}

inline bool
Matrix::is_sorted() const {
  // Since the flag `sorted' does not really reflect the
  // sort status of a matrix this assertion is used to be sure that the
  // matrix is really sorted when `sorted' value is 'true'.
  assert(!sorted || check_sorted());
  return sorted;
}


inline dimension_type
Matrix::num_columns() const {
  return row_size;
}

inline dimension_type
Matrix::space_dimension() const {
  dimension_type n_columns = num_columns();
  return (n_columns == 0)
    ? 0
    : n_columns - (is_necessarily_closed() ? 1 : 2);
}

/*! \relates Matrix */
inline bool
operator!=(const Matrix& x, const Matrix& y) {
  return !(x == y);
}


inline void
Matrix::add_zero_columns(dimension_type n) {
  assert(n > 0);
  grow(num_rows(), num_columns() + n);
}

inline void
Matrix::erase_to_end(dimension_type first_to_erase) {
  assert(first_to_erase <= rows.size());
  if (first_to_erase < rows.size())
    rows.erase(rows.begin() + first_to_erase, rows.end());
}

inline void
Matrix::clear() {
  // Clear `rows' and minimize its capacity.
  // Note: do NOT modify the value of `row_topology'.
  std::vector<Row>().swap(rows);
  row_size = 0;
  row_capacity = 0;
  index_first_pending = 0;
  sorted = true;
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Matrix */
inline void
swap(Parma_Polyhedra_Library::Matrix& x,
     Parma_Polyhedra_Library::Matrix& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Matrix_inlines_hh)
