/* DBMatrix class implementation: inline functions.
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

#ifndef PPL_OR_Matrix_inlines_hh
#define PPL_OR_Matrix_inlines_hh 1

#include "globals.defs.hh"
#include "Checked_Number.defs.hh"
#include "C_Polyhedron.defs.hh"
#include <cassert>
#include <vector>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <algorithm>
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
dimension_type
OR_Matrix<T>::row_first_element_index(dimension_type k) {
  return ((k+1)*(k+1))/2;
}

template <typename T>
dimension_type
OR_Matrix<T>::row_size(dimension_type k) {
  return (k+2) & ~dimension_type(1);
}

#if EXTRA_ROW_DEBUG

template <typename T>
template <typename U>
inline dimension_type
OR_Matrix<T>::Pseudo_Row<U>::size() const {
  return size_;
}

#endif // EXTRA_ROW_DEBUG

template <typename T>
template <typename U>
inline
OR_Matrix<T>::Pseudo_Row<U>::Pseudo_Row()
  : first(0) {
  // FIXME: is zeroing necessary/wanted?
}

template <typename T>
template <typename U>
inline
OR_Matrix<T>::Pseudo_Row<U>::Pseudo_Row(U& y
#if EXTRA_ROW_DEBUG
		, dimension_type s
#endif
		)
  : first(&y)
#if EXTRA_ROW_DEBUG
  , size_(s)
#endif
{
}

template <typename T>
template <typename U>
template <typename V>
inline
OR_Matrix<T>::Pseudo_Row<U>::Pseudo_Row(const Pseudo_Row<V>& y)
  : first(y.first)
#if EXTRA_ROW_DEBUG
    , size_(y.size_)
#endif
{
}

template <typename T>
template <typename U>
inline OR_Matrix<T>::Pseudo_Row<U>&
OR_Matrix<T>::Pseudo_Row<U>::operator=(const Pseudo_Row& y) {
  first = y.first;
#if EXTRA_ROW_DEBUG
  size_ = y.size_;
#endif
  return *this;
}

template <typename T>
template <typename U>
inline
OR_Matrix<T>::Pseudo_Row<U>::~Pseudo_Row() {
}

template <typename T>
template <typename U>
inline U&
OR_Matrix<T>::Pseudo_Row<U>::operator[](dimension_type k) const {
#if EXTRA_ROW_DEBUG
  assert(k < size_);
#endif
  return *(first + k);
}

template <typename T>
template <typename U>
inline
OR_Matrix<T>::any_row_iterator<U>::any_row_iterator(dimension_type n_rows)
  : e(n_rows) {
}

template <typename T>
template <typename U>
inline
OR_Matrix<T>::any_row_iterator<U>::any_row_iterator(U& base)
  :  value(base
#if EXTRA_ROW_DEBUG
	   , OR_Matrix<T>::row_size(0)
#endif
	   ),
     e(0),
     i(0) {
}

template <typename T>
template <typename U>
template <typename V>
inline
OR_Matrix<T>::any_row_iterator<U>::any_row_iterator(const any_row_iterator<V>& y)
  : value(y.value),
    e(y.e),
    i(y.i) {
}

template <typename T>
template <typename U>
template <typename V>
inline typename OR_Matrix<T>::template any_row_iterator<U>&
OR_Matrix<T>::any_row_iterator<U>::operator=(const any_row_iterator<V>& y) {
  value = y.value;
  e = y.e;
  i = y.i;
  return *this;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>::reference
OR_Matrix<T>::any_row_iterator<U>::operator*() const {
  return value;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>::pointer
OR_Matrix<T>::any_row_iterator<U>::operator->() const {
  return &value;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>&
OR_Matrix<T>::any_row_iterator<U>::operator++() {
  ++e;
  dimension_type increment = e;
  if (e % 2) {
    ++increment;
#if EXTRA_ROW_DEBUG
    value.size_ += 2;
#endif
  }
  i += increment;
  value.first += increment;
  return *this;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>
OR_Matrix<T>::any_row_iterator<U>::operator++(int) {
  any_row_iterator old = *this;
  ++(*this);
  return old;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>&
OR_Matrix<T>::any_row_iterator<U>::operator--() {
  dimension_type decrement = e + 1;
  --e;
  if (e % 2) {
    ++decrement;
#if EXTRA_ROW_DEBUG
    value.size_ -= 2;
#endif
  }
  i -= decrement;
  value.first -= decrement;
  return *this;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>
OR_Matrix<T>::any_row_iterator<U>::operator--(int) {
  any_row_iterator old = *this;
  --(*this);
  return old;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>&
OR_Matrix<T>::any_row_iterator<U>::operator+=(difference_type m) {
  difference_type increment = m + m*m/2 + m*e;
  if (e%2 == 0 && m%2 == 1)
    ++increment;
  e += m;
  i += increment;
  value.first += increment;
#if EXTRA_ROW_DEBUG
  // FIXME!!!
  value.size_ = OR_Matrix::row_size(e);
#endif
  return *this;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>&
OR_Matrix<T>::any_row_iterator<U>::operator-=(difference_type m) {
  return *this += -m;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>::difference_type
OR_Matrix<T>::any_row_iterator<U>::operator-(const any_row_iterator<U>& y)
  const {
  return e - y.e;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>
OR_Matrix<T>::any_row_iterator<U>::operator+(difference_type m) const {
  any_row_iterator r = *this;
  r += m;
  return r;
}

template <typename T>
template <typename U>
inline typename OR_Matrix<T>::template any_row_iterator<U>
OR_Matrix<T>::any_row_iterator<U>::operator-(difference_type m) const {
  any_row_iterator r = *this;
  r -= m;
  return r;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator==(const any_row_iterator& y) const {
  return e == y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator!=(const any_row_iterator& y) const {
  return e != y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator<(const any_row_iterator& y) const {
  return e < y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator<=(const any_row_iterator& y) const {
  return e <= y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator>(const any_row_iterator& y) const {
  return e > y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>::operator>=(const any_row_iterator& y) const {
  return e >= y.e;
}

template <typename T>
template <typename U>
inline dimension_type
OR_Matrix<T>::any_row_iterator<U>::row_size() const {
  return (e+2) & ~dimension_type(1);
}

template <typename T>
template <typename U>
inline dimension_type
OR_Matrix<T>::any_row_iterator<U>::index() const {
  return e;
}

template <typename T>
inline typename OR_Matrix<T>::row_iterator
OR_Matrix<T>::row_begin() {
  return num_rows() == 0 ? row_iterator(0) : row_iterator(vec[0]);
}

template <typename T>
inline typename OR_Matrix<T>::row_iterator
OR_Matrix<T>::row_end() {
  return row_iterator(num_rows());
}

template <typename T>
inline typename OR_Matrix<T>::const_row_iterator
OR_Matrix<T>::row_begin() const {
  return num_rows() == 0 ? const_row_iterator(0) : const_row_iterator(vec[0]);
}

template <typename T>
inline typename OR_Matrix<T>::const_row_iterator
OR_Matrix<T>::row_end() const {
  return const_row_iterator(num_rows());
}

template <typename T>
inline typename OR_Matrix<T>::element_iterator
OR_Matrix<T>::element_begin() {
  return vec.begin();
}

template <typename T>
inline typename OR_Matrix<T>::element_iterator
OR_Matrix<T>::element_end() {
  return vec.end();
}

template <typename T>
inline typename OR_Matrix<T>::const_element_iterator
OR_Matrix<T>::element_begin() const {
  return vec.begin();
}

template <typename T>
inline typename OR_Matrix<T>::const_element_iterator
OR_Matrix<T>::element_end() const {
  return vec.end();
}

template <typename T>
inline void
OR_Matrix<T>::swap(OR_Matrix& y) {
  std::swap(vec, y.vec);
  std::swap(num_rows_, y.num_rows_);
  std::swap(vec_capacity, y.vec_capacity);
}

//! Returns the integer square root of \p x.
inline unsigned long
isqrt(unsigned long x)
{
  unsigned long r = 0;
  for (unsigned long t = 0x40000000; t; t >>= 2) {
    unsigned long s = r + t;
    if (s <= x) {
      x -= s;
      r = s + t;
    }
    r >>= 1;
  }
  return r;
}

template <typename T>
inline dimension_type
OR_Matrix<T>::max_num_rows() {
  dimension_type k = isqrt(2*DB_Row<T>::max_size() + 1);
  return (k-1) & ~dimension_type(1);
}

template <typename T>
inline
OR_Matrix<T>::OR_Matrix()
  : vec(),
    num_rows_(0),
    vec_capacity(0) {
}

template <typename T>
inline
OR_Matrix<T>::OR_Matrix(dimension_type nrows)
  : vec(nrows*(nrows/2+1)),
    num_rows_(nrows),
    vec_capacity(vec.size()) {
}


template <typename T>
inline
OR_Matrix<T>::~OR_Matrix() {
}

template <typename T>
inline typename OR_Matrix<T>::row_reference_type
OR_Matrix<T>::operator[](dimension_type k) {
  return row_reference_type(vec[row_first_element_index(k)]
#if EXTRA_ROW_DEBUG
			    , row_size(k)
#endif
			    );
}

template <typename T>
inline typename OR_Matrix<T>::const_row_reference_type
OR_Matrix<T>::operator[](dimension_type k) const {
  return const_row_reference_type(vec[row_first_element_index(k)]
#if EXTRA_ROW_DEBUG
				  , row_size(k)
#endif
				  );
}

template <typename T>
inline dimension_type
OR_Matrix<T>::space_dimension() const {
  return num_rows_/2;
}

template <typename T>
inline dimension_type
OR_Matrix<T>::num_rows() const {
  return num_rows_;
}

template <typename T>
inline void
OR_Matrix<T>::clear() {
  // Clear the matrix.
  OR_Matrix<T>().swap(*this);
}

template <typename T>
inline void
OR_Matrix<T>::erase_to_end(dimension_type first_to_erase) {
  assert(first_to_erase%2 == 1);
  assert(first_to_erase <= num_rows() - 1);
  if (first_to_erase < num_rows() - 2)
    resize_no_copy(first_to_erase);
 }

/*! \relates OR_Matrix */
template <typename T>
inline bool
operator!=(const OR_Matrix<T>& x, const OR_Matrix<T>& y) {
  return !(x == y);
}

template <typename T>
inline
OR_Matrix<T>::OR_Matrix(const OR_Matrix& y)
  : vec(y.vec),
    num_rows_(y.num_rows_),
    vec_capacity(compute_capacity(y.vec.size())) {
}

template <typename T>
inline OR_Matrix<T>&
OR_Matrix<T>::operator=(const OR_Matrix& y) {
  vec = y.vec;
  num_rows_ = y.num_rows_;
  vec_capacity = compute_capacity(y.vec.size());
  return *this;
}

template <typename T>
inline void
OR_Matrix<T>::grow(const dimension_type new_nrows) {
  assert(new_nrows%2 == 0);
  assert(new_nrows >= num_rows_);
  dimension_type new_size = new_nrows*(new_nrows/2 + 1);
  if (new_nrows > num_rows_) {
    if (new_size <= vec_capacity) {
      // We can recycle the old vec.
      //      vec.grow_no_copy(new_size);
      vec.expand_within_capacity(new_size);
      num_rows_ = new_nrows;
    }
    else {
      // We cannot even recycle the old vec.
      OR_Matrix<T> new_matrix(new_nrows);
      //       element_iterator j = new_matrix.element_begin();
      //       for (const_element_iterator i = element_begin(), mend = element_end();
      // 	   i != mend; ++i, ++j)
      // 	*j = *i;
      for (dimension_type i = num_rows_; i-- > 0; )
	for (dimension_type j = row_size(i); j-- > 0; )
	  new_matrix[i][j] = (*this)[i][j];
      swap(new_matrix);
      return;
    }
  }
}

template <typename T>
inline void
OR_Matrix<T>::resize_no_copy(const dimension_type new_nrows) {
  dimension_type old_size = vec.size();
  dimension_type new_size = new_nrows*(new_nrows/2 + 1);

  if (new_size > old_size)
    grow(new_nrows);
  else if (new_size < old_size)
    vec.shrink(new_size);

  num_rows_ = new_nrows;
}

template <typename T>
inline void
OR_Matrix<T>::remove_rows(const dimension_type new_n_rows) {
  assert(new_n_rows < num_rows_);
  // Since we are removing rows, reallocation will
  // not take place and the old contents of the first
  // `new_n_rows' rows will be preserved.
  resize_no_copy(new_n_rows);
}


template <typename T>
inline void
OR_Matrix<T>::ascii_dump(std::ostream& s) const {
  const OR_Matrix<T>& x = *this;
  const char separator = ' ';
  dimension_type nrows = x.num_rows_;
  s << nrows << separator
    << std::endl;
  for (const_row_iterator i = x.row_begin(), xend = x.row_end();
       i != xend; ++i) {
    const_row_reference_type r = *i;
    dimension_type rs = i.row_size();
    for (dimension_type j = 0; j < rs; ++j) {
      using namespace IO_Operators;
      s << r[j] << separator;
    }
    s << std::endl;
  }
}

template <typename T>
inline bool
OR_Matrix<T>::ascii_load(std::istream& s) {
  dimension_type nrows;
  if (!(s >> nrows))
    return false;
  resize_no_copy(nrows);
  OR_Matrix& x = *this;
//   for (const_row_iterator i = x.row_begin(), xend = x.row_end();
//        i != xend; ++i) {
//     const_row_reference_type r = *i;
  for (dimension_type i = 0; i < x.num_rows(); ++i) {
    dimension_type rs = row_size(i);
    for (dimension_type j = 0; j < rs; ++j) {
      Result r = input(x[i][j], s, ROUND_UP);
      // FIXME: V_CVT_STR_UNK is probably not the only possible error.
      if (!s || r == V_CVT_STR_UNK)
	return false;
    }
  }
  assert(OK());
  return true;
}

/*! \relates OR_Matrix */
template <typename T>
inline bool
operator==(const OR_Matrix<T>& x, const OR_Matrix<T>& y) {
  return x.num_rows_ == y.num_rows_ && x.vec == y.vec;
}

template <typename T>
inline void
OR_Matrix<T>::add_rows(dimension_type n) {
  assert(n > 0);
  assert(n%2 == 0);
  grow(num_rows_ + n);
  assert(OK());
}

template <typename T>
inline bool
OR_Matrix<T>::OK() const {
  dimension_type space = space_dimension();
  if (space == 0)
    return vec.size() == 0;
  if (num_rows_%2 == 1)
    return false;
  if (vec.size() != 2*space*(space + 1))
    return false;
  if (!vec.OK(vec.size(), vec_capacity))
    return false;
  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::OR_Matrix */  //FIXME!!
template <typename T>
inline std::ostream&
IO_Operators::operator<<(std::ostream& s, const OR_Matrix<T>& m) {
  dimension_type n_rows = m.num_rows();
  for (dimension_type i = 0; i < n_rows; ++i) {
    dimension_type n_columns = OR_Matrix<T>::row_size(i);
    for (dimension_type j = 0; j < n_columns; ++j) {
      s << m[i][j] << " ";
    }
    s << std::endl;
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::OR_Matrix */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::OR_Matrix<T>& x,
     Parma_Polyhedra_Library::OR_Matrix<T>& y) {
  x.swap(y);
}

} // namespace std


#endif // !defined(PPL_OR_Matrix_inlines_hh)
