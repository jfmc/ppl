/* Dense_Matrix class implementation: inline functions.
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

#ifndef PPL_Dense_Matrix_inlines_hh
#define PPL_Dense_Matrix_inlines_hh 1

#include "globals.defs.hh"
#include <algorithm>
#include "assert.hh"

namespace Parma_Polyhedra_Library {

inline void
Dense_Matrix::remove_rows(iterator first, iterator last) {
  rows.erase(first, last);
}

inline void
Dense_Matrix::reserve_rows(dimension_type n) {
  rows.reserve(n);
}

inline dimension_type
Dense_Matrix::max_num_rows() {
  return std::vector<Dense_Row>().max_size();
}

inline dimension_type
Dense_Matrix::max_num_columns() {
  return Dense_Row::max_size();
}

inline memory_size_type
Dense_Matrix::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline
Dense_Matrix::const_iterator::const_iterator()
  : i() {
}

inline
Dense_Matrix::const_iterator::const_iterator(const Iter& b)
  : i(b) {
}

inline
Dense_Matrix::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

inline Dense_Matrix::const_iterator&
Dense_Matrix::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

inline Dense_Matrix::const_iterator::reference
Dense_Matrix::const_iterator::operator*() const {
  return *i;
}

inline Dense_Matrix::const_iterator::pointer
Dense_Matrix::const_iterator::operator->() const {
  return &*i;
}

inline Dense_Matrix::const_iterator&
Dense_Matrix::const_iterator::operator++() {
  ++i;
  return *this;
}

inline Dense_Matrix::const_iterator
Dense_Matrix::const_iterator::operator++(int) {
  return const_iterator(i++);
}

inline bool
Dense_Matrix::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
Dense_Matrix::const_iterator::operator!=(const const_iterator& y) const {
  return !operator==(y);
}

inline bool
Dense_Matrix::has_no_rows() const {
  return rows.empty();
}

inline Dense_Matrix::iterator
Dense_Matrix::begin() {
  return rows.begin();
}

inline Dense_Matrix::iterator
Dense_Matrix::end() {
  return rows.end();
}

inline Dense_Matrix::const_iterator
Dense_Matrix::begin() const {
  return const_iterator(rows.begin());
}

inline Dense_Matrix::const_iterator
Dense_Matrix::end() const {
  return const_iterator(rows.end());
}

inline void
Dense_Matrix::swap(Dense_Matrix& y) {
  std::swap(rows, y.rows);
  std::swap(row_size, y.row_size);
  std::swap(row_capacity, y.row_capacity);
}

inline
Dense_Matrix::Dense_Matrix()
  : rows(),
    row_size(0),
    row_capacity(0) {
  PPL_ASSERT(OK());
}

inline
Dense_Matrix::Dense_Matrix(const Dense_Matrix& y)
  : rows(y.rows),
    row_size(y.row_size),
    row_capacity(y.row_capacity) {
  PPL_ASSERT(OK());
}

inline
Dense_Matrix::~Dense_Matrix() {
}

inline Dense_Matrix&
Dense_Matrix::operator=(const Dense_Matrix& y) {
  if (this != &y) {
    rows = y.rows;
    row_size = y.row_size;
    row_capacity = y.row_capacity;
  }
  PPL_ASSERT(OK());
  return *this;
}

inline void
Dense_Matrix::add_row(const Dense_Row& y) {
  Dense_Row new_row(y, row_capacity);
  add_recycled_row(new_row);
  PPL_ASSERT(OK());
}

inline Dense_Row&
Dense_Matrix::operator[](const dimension_type k) {
  PPL_ASSERT(k < rows.size());
  return rows[k];
}

inline const Dense_Row&
Dense_Matrix::operator[](const dimension_type k) const {
  PPL_ASSERT(k < rows.size());
  return rows[k];
}

inline dimension_type
Dense_Matrix::num_rows() const {
  return rows.size();
}

inline dimension_type
Dense_Matrix::num_columns() const {
  return row_size;
}

/*! \relates Dense_Matrix */
inline bool
operator!=(const Dense_Matrix& x, const Dense_Matrix& y) {
  return !(x == y);
}

inline void
Dense_Matrix::remove_trailing_rows(const dimension_type n) {
  PPL_ASSERT(n <= rows.size());
  if (n != 0)
    rows.erase(rows.end() - n, rows.end());
  PPL_ASSERT(OK());
}

inline void
Dense_Matrix::clear() {
  // Clear `rows' and minimize its capacity.
  std::vector<Dense_Row>().swap(rows);
  row_size = 0;
  row_capacity = 0;
  PPL_ASSERT(OK());
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Dense_Matrix */
inline void
swap(Parma_Polyhedra_Library::Dense_Matrix& x,
     Parma_Polyhedra_Library::Dense_Matrix& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Dense_Matrix_inlines_hh)
