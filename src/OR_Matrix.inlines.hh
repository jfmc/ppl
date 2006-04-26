/* OR_Matrix class implementation: inline functions.
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
#include <algorithm>
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
inline dimension_type
OR_Matrix<T>::row_first_element_index(const dimension_type k) {
  return ((k+1)*(k+1))/2;
}

template <typename T>
inline dimension_type
OR_Matrix<T>::row_size(const dimension_type k) {
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
OR_Matrix<T>::Pseudo_Row<U>::operator[](const dimension_type k) const {
#if EXTRA_ROW_DEBUG
  assert(k < size_);
#endif
  return *(first + k);
}

template <typename T>
template <typename U>
inline
OR_Matrix<T>::any_row_iterator<U>
::any_row_iterator(const dimension_type n_rows)
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
OR_Matrix<T>::any_row_iterator<U>
::any_row_iterator(const any_row_iterator<V>& y)
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
OR_Matrix<T>::any_row_iterator<U>::operator-(const difference_type m) const {
  any_row_iterator r = *this;
  r -= m;
  return r;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>
::operator==(const any_row_iterator& y) const {
  return e == y.e;
}

template <typename T>
template <typename U>
inline bool
OR_Matrix<T>::any_row_iterator<U>
::operator!=(const any_row_iterator& y) const {
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
OR_Matrix<T>::any_row_iterator<U>
::operator<=(const any_row_iterator& y) const {
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
OR_Matrix<T>::any_row_iterator<U>
::operator>=(const any_row_iterator& y) const {
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
  std::swap(space_dim, y.space_dim);
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
  // FIXME: this deserves a comment.
  dimension_type k = isqrt(2*DB_Row<T>::max_size() + 1);
  return (k-1) & ~dimension_type(1);
}

template <typename T>
inline
OR_Matrix<T>::OR_Matrix(const dimension_type dim)
  : vec(2*dim*(dim+1)),
    space_dim(dim),
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
  return space_dim;
}

template <typename T>
inline dimension_type
OR_Matrix<T>::num_rows() const {
  return 2*space_dimension();
}

template <typename T>
inline void
OR_Matrix<T>::clear() {
  // Clear the matrix.
  OR_Matrix<T>().swap(*this);
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
    space_dim(y.space_dim),
    vec_capacity(compute_capacity(y.vec.size())) {
}

template <typename T>
inline OR_Matrix<T>&
OR_Matrix<T>::operator=(const OR_Matrix& y) {
  vec = y.vec;
  space_dim = y.space_dim;
  vec_capacity = compute_capacity(y.vec.size());
  return *this;
}

template <typename T>
inline void
OR_Matrix<T>::grow(const dimension_type new_dim) {
  assert(new_dim >= space_dim);
  if (new_dim > space_dim) {
    const dimension_type new_size = 2*new_dim*(new_dim + 1);
    if (new_size <= vec_capacity) {
      // We can recycle the old vec.
      vec.expand_within_capacity(new_size);
      space_dim = new_dim;
    }
    else {
      // We cannot even recycle the old vec.
      OR_Matrix<T> new_matrix(new_dim);
      element_iterator j = new_matrix.element_begin();
      for (element_iterator i = element_begin(),
	     mend = element_end(); i != mend; ++i, ++j)
	// FIXME: this assignment is costly when using mpz_class or
	// mpq_class. Provide a "copy_or_swap()" method that swaps
	// the implementation of coefficients when appropriate.
      	*j = *i;
      swap(new_matrix);
      return;
    }
  }
}

template <typename T>
inline void
OR_Matrix<T>::shrink(const dimension_type new_dim) {
  assert(new_dim <= space_dim);
  const dimension_type new_size = 2*new_dim*(new_dim + 1);
  vec.shrink(new_size);
  space_dim = new_dim;
}

template <typename T>
inline void
OR_Matrix<T>::resize_no_copy(const dimension_type new_dim) {
  if (new_dim > space_dim)
    // FIXME: here we might unnecessarily copy!
    grow(new_dim);
  else if (new_dim < space_dim)
    shrink(new_dim);
}

/*! \relates OR_Matrix */
template <typename T>
inline bool
operator==(const OR_Matrix<T>& x, const OR_Matrix<T>& y) {
  return x.space_dim == y.space_dim && x.vec == y.vec;
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
