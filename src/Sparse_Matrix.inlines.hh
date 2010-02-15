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

inline Sparse_Matrix::iterator
Sparse_Matrix::begin() {
  return iterator(rows.begin(),num_columns());
}

inline Sparse_Matrix::iterator
Sparse_Matrix::end() {
  return iterator(rows.end(),num_columns());
}

inline Sparse_Matrix::const_iterator
Sparse_Matrix::begin() const {
  return rows.begin();
}

inline Sparse_Matrix::const_iterator
Sparse_Matrix::end() const {
  return rows.end();
}

inline Sparse_Matrix_Row
Sparse_Matrix::operator[](dimension_type i) {
  PPL_ASSERT(i < rows.size());
  return Sparse_Matrix_Row(rows[i],num_columns());
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
  resize(n,n);
}

inline void
Sparse_Matrix::add_zero_rows(const dimension_type n) {
  resize(num_rows()+n,num_columns());
}

inline void
Sparse_Matrix::add_zero_columns(const dimension_type n) {
  resize(num_rows(),num_columns()+n);
}

inline void
Sparse_Matrix::add_zero_rows_and_columns(const dimension_type n,
                                              const dimension_type m) {
  resize(num_rows()+n,num_columns()+m);
}

inline void
Sparse_Matrix::remove_trailing_columns(const dimension_type n) {
  PPL_ASSERT(n <= num_columns());
  resize(num_rows(),num_columns()-n);
}

inline void
Sparse_Matrix::erase_to_end(dimension_type first_to_erase) {
  resize(first_to_erase,num_columns());
}

inline
Sparse_Matrix::iterator::iterator(const iterator& x)
  : itr(x.itr), size_(x.size_) {
}

inline Sparse_Matrix_Row
Sparse_Matrix::iterator::operator*() {
  return Sparse_Matrix_Row(*itr,size_);
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

inline
Sparse_Matrix::iterator::iterator(
  std::vector<Unlimited_Sparse_Row>::iterator i,
  const dimension_type size)
  : itr(i), size_(size) {
}

template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(const Func& func) {
  std::for_each(begin(),end(),func);
}

template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(const Func& func) const {
  std::for_each(begin(),end(),func);
}


inline
Sparse_Matrix_Row::Sparse_Matrix_Row(Unlimited_Sparse_Row& row,
                                          const dimension_type size)
  : row_(row), size_(size) {
  PPL_ASSERT(OK());
}

inline void
Sparse_Matrix_Row::swap(Sparse_Matrix_Row x) {
  PPL_ASSERT(size_ == x.size_);
  row_.swap(x.row_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline dimension_type
Sparse_Matrix_Row::size() const {
  return size_;
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::reset(iterator i) {
  PPL_ASSERT(i != end());
  iterator res = row_.reset(i);
  PPL_ASSERT(OK());
  return res;
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::reset(iterator first,iterator last) {
  iterator res = row_.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Matrix_Row::normalize() {
  row_.normalize();
  PPL_ASSERT(OK());
}

inline Coefficient&
Sparse_Matrix_Row::operator[](const dimension_type i) {
  PPL_ASSERT(i < size_);
  return row_[i];
}

inline const Coefficient&
Sparse_Matrix_Row::operator[](const dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Matrix_Row::get(const dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row_.get(i);
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::begin() {
  return row_.begin();
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::end() {
  return row_.end();
}

inline Sparse_Matrix_Row::const_iterator
Sparse_Matrix_Row::begin() const {
  return row_.begin();
}

inline Sparse_Matrix_Row::const_iterator
Sparse_Matrix_Row::end() const {
  return row_.end();
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::find(const dimension_type c) {
  return row_.find(c);
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::lower_bound(const dimension_type c) {
  return row_.lower_bound(c);
}

inline Sparse_Matrix_Row::iterator
Sparse_Matrix_Row::upper_bound(const dimension_type c) {
  return row_.upper_bound(c);
}

inline Sparse_Matrix_Row::const_iterator
Sparse_Matrix_Row::find(const dimension_type c) const {
  return row_.find(c);
}

inline Sparse_Matrix_Row::const_iterator
Sparse_Matrix_Row::lower_bound(const dimension_type c) const {
  return row_.lower_bound(c);
}

inline Sparse_Matrix_Row::const_iterator
Sparse_Matrix_Row::upper_bound(const dimension_type c) const {
  return row_.upper_bound(c);
}

inline
Sparse_Matrix_Row::operator const Unlimited_Sparse_Row&() const {
  return row_;
}

inline bool
Sparse_Matrix_Row::OK() const {
  if (!row_.OK())
    return false;
  if (row_.begin() == row_.end())
    return true;
  Unlimited_Sparse_Row::const_iterator itr = row_.end();
  --itr;
  return (itr->first < size_);
}

template <typename Func>
inline void
Sparse_Matrix_Row::for_each_nonzero(const Func& func,const dimension_type n) {
  (void)n;
  std::for_each(begin(),end(),apply_to_data(func));
}

template <typename Func>
inline void
Sparse_Matrix_Row::for_each_nonzero(const Func& func,const dimension_type n)
  const {
  (void)n;
  std::for_each(begin(),end(),apply_to_data(func));
}

template <typename Func>
inline
Sparse_Matrix_Row::applier_to_data<Func>::applier_to_data(const Func& func)
  : f(func) {
}

template <typename Func>
inline void
Sparse_Matrix_Row::applier_to_data<Func>::operator()(
  std::pair<dimension_type,Coefficient>& x) const {
  f(x.second);
}

template <typename Func>
inline Sparse_Matrix_Row::applier_to_data<Func>
Sparse_Matrix_Row::apply_to_data(const Func& func) {
  return applier_to_data<Func>(func);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Sparse_Matrix_inlines_hh)
