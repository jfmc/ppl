/* Sparse_Matrix class implementation: inline functions.
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

#ifndef PPL_Sparse_Matrix_inlines_hh
#define PPL_Sparse_Matrix_inlines_hh 1

// FIXME: Remove this.
// It's needed only to please KDevelop4.
#include "Sparse_Matrix.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Sparse_Matrix::Sparse_Matrix(dimension_type n)
  : rows(n), num_columns_(n) {
  PPL_ASSERT(OK());
}

inline
Sparse_Matrix::Sparse_Matrix(dimension_type num_rows,
                             dimension_type num_columns)
  : rows(num_rows), num_columns_(num_columns) {
  PPL_ASSERT(OK());
}

inline void
Sparse_Matrix::swap(Sparse_Matrix& x) {
  std::swap(rows, x.rows);
  std::swap(num_columns_, x.num_columns_);
}

inline Sparse_Matrix::iterator
Sparse_Matrix::begin() {
  return iterator(rows.begin(), num_columns());
}

inline Sparse_Matrix::iterator
Sparse_Matrix::end() {
  return iterator(rows.end(), num_columns());
}

inline Sparse_Matrix::const_iterator
Sparse_Matrix::begin() const {
  return rows.begin();
}

inline Sparse_Matrix::const_iterator
Sparse_Matrix::end() const {
  return rows.end();
}

inline Sparse_Row_Reference
Sparse_Matrix::operator[](dimension_type i) {
  PPL_ASSERT(i < rows.size());
  return Sparse_Row_Reference(rows[i], num_columns());
}

inline const Unlimited_Sparse_Row&
Sparse_Matrix::operator[](dimension_type i) const {
  PPL_ASSERT(i < rows.size());
  return rows[i];
}

inline dimension_type
Sparse_Matrix::num_rows() const {
  return rows.size();
}

inline dimension_type
Sparse_Matrix::num_columns() const {
  return num_columns_;
}

inline void
Sparse_Matrix::resize(dimension_type n) {
  resize(n, n);
}

inline void
Sparse_Matrix::clear() {
  resize(0, 0);
}

inline void
Sparse_Matrix::add_zero_rows(dimension_type n) {
  resize(num_rows() + n, num_columns());
}

inline void
Sparse_Matrix::add_zero_columns(dimension_type n) {
  resize(num_rows(), num_columns() + n);
}

inline void
Sparse_Matrix::add_zero_rows_and_columns(dimension_type n,
                                         dimension_type m) {
  resize(num_rows() + n, num_columns() + m);
}

inline void
Sparse_Matrix::add_row(const Sparse_Row& x) {
  add_zero_rows(1);
  (*this)[num_rows() - 1] = x;
  PPL_ASSERT(OK());
}

inline void
Sparse_Matrix::add_row(const Sparse_Row_Reference& x) {
  add_zero_rows(1);
  (*this)[num_rows() - 1] = x;
  PPL_ASSERT(OK());
}

inline void
Sparse_Matrix::add_row(const Unlimited_Sparse_Row& x) {
  rows.push_back(x);
  PPL_ASSERT(OK());
}

inline void
Sparse_Matrix::remove_trailing_columns(dimension_type n) {
  PPL_ASSERT(n <= num_columns());
  resize(num_rows(), num_columns() - n);
}

inline void
Sparse_Matrix::erase_to_end(dimension_type first_to_erase) {
  resize(first_to_erase, num_columns());
}

inline memory_size_type
Sparse_Matrix::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}


inline
Sparse_Matrix::iterator
::iterator(std::vector<Unlimited_Sparse_Row>::iterator i,
           dimension_type size)
  : itr(i), size_(size) {
}

inline
Sparse_Matrix::iterator::iterator(const iterator& x)
  : itr(x.itr), size_(x.size_) {
}

inline Sparse_Row_Reference
Sparse_Matrix::iterator::operator*() {
  return Sparse_Row_Reference(*itr, size_);
}

inline Sparse_Matrix::iterator&
Sparse_Matrix::iterator::operator++() {
  ++itr;
  return *this;
}

inline Sparse_Matrix::iterator
Sparse_Matrix::iterator::operator++(int) {
  iterator x(*this);
  ++(*this);
  return x;
}


template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(const Func& func) {
  std::for_each(begin(), end(), func);
}

template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(const Func& func) const {
  std::for_each(begin(), end(), func);
}

} // namespace Parma_Polyhedra_Library

namespace std {

inline void
swap(Parma_Polyhedra_Library::Sparse_Matrix& x,
     Parma_Polyhedra_Library::Sparse_Matrix& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Sparse_Matrix_inlines_hh)
