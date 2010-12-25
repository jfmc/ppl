/* Dense_Row class implementation: inline functions.
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

#ifndef PPL_Dense_Row_inlines_hh
#define PPL_Dense_Row_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Dense_Row.defs.hh"

#include "assert.hh"
#include <cstddef>
#include <limits>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline dimension_type
Dense_Row::max_size() {
  return std::numeric_limits<size_t>::max() / sizeof(Coefficient);
}

inline dimension_type
Dense_Row::size() const {
  return size_;
}

inline const Row_Flags&
Dense_Row::flags() const {
  return flags_;
}

inline Row_Flags&
Dense_Row::flags() {
  return flags_;
}

inline dimension_type
Dense_Row::capacity() const {
  return capacity_;
}

inline
Dense_Row::Dense_Row()
  : size_(0), capacity_(0), flags_(), vec_(0) {

  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(Flags f)
  : size_(0), capacity_(0), flags_(f), vec_(0) {

  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(const dimension_type sz,
                     const dimension_type capacity,
                     const Flags f)
  : size_(0), capacity_(0), flags_(f), vec_(0) {
  resize(sz, capacity);
  PPL_ASSERT(size() == sz);
  PPL_ASSERT(capacity_ = capacity);
  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(const dimension_type sz, const Flags f)
  : size_(0), capacity_(0), flags_(f), vec_(0) {
  resize(sz);
  PPL_ASSERT(size() == sz);
  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(const Dense_Row& y)
  : size_(0), capacity_(0), flags_(y.flags()), vec_(0) {
  if (y.vec_ != 0) {
    capacity_ = y.capacity();
    vec_ = static_cast<Coefficient*>(
        operator new(sizeof(Coefficient) * capacity_));
    while (size_ != y.size()) {
      new (&vec_[size_]) Coefficient(y[size_]);
      ++size_;
    }
  }
  PPL_ASSERT(size() == y.size());
  PPL_ASSERT(capacity() == y.capacity());
  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
                     const dimension_type capacity)
  : size_(0), capacity_(0), flags_(y.flags()), vec_(0) {
  PPL_ASSERT(y.size() <= capacity);
  PPL_ASSERT(capacity <= max_size());

  vec_ = static_cast<Coefficient*>(
      operator new(sizeof(Coefficient) * capacity));
  capacity_ = capacity;

  if (y.vec_ != 0) {
    while (size_ != y.size()) {
      new (&vec_[size_]) Coefficient(y[size_]);
      ++size_;
    }
  }
  PPL_ASSERT(size() == y.size());
  PPL_ASSERT(capacity_ = capacity);
  PPL_ASSERT(OK());
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
                     const dimension_type sz,
                     const dimension_type capacity)
  : size_(0), capacity_(0), flags_(y.flags()), vec_(0) {
  PPL_ASSERT(y.size() <= sz);
  PPL_ASSERT(sz <= capacity);
  PPL_ASSERT(capacity <= max_size());
  PPL_ASSERT(capacity != 0);
  
  vec_ = static_cast<Coefficient*>(operator new(sizeof(Coefficient) * capacity));
  capacity_ = capacity;
  
  dimension_type n = std::min(sz, y.size());
  while (size_ != n) {
    new (&vec_[size_]) Coefficient(y[size_]);
    ++size_;
  }
  while (size_ != sz) {
    new (&vec_[size_]) Coefficient();
    ++size_;
  }
  PPL_ASSERT(size() == sz);
  PPL_ASSERT(capacity_ = capacity);
  PPL_ASSERT(OK());
}

inline
Dense_Row::~Dense_Row() {
  destroy();
}

inline void
Dense_Row::destroy() {
  resize(0);
  operator delete(vec_);
}

inline void
Dense_Row::swap(Dense_Row& y) {
  std::swap(size_, y.size_);
  std::swap(capacity_, y.capacity_);
  std::swap(flags_, y.flags_);
  std::swap(vec_, y.vec_);
  PPL_ASSERT(OK());
  PPL_ASSERT(y.OK());
}

inline Dense_Row&
Dense_Row::operator=(const Dense_Row& y) {
  
  Dense_Row x(y);
  std::swap(*this, x);

  return *this;
}

inline Coefficient&
Dense_Row::operator[](const dimension_type k) {
  PPL_ASSERT(vec_ != 0);
  PPL_ASSERT(k < size());
  return vec_[k];
}

inline Coefficient_traits::const_reference
Dense_Row::operator[](const dimension_type k) const {
  PPL_ASSERT(vec_ != 0);
  PPL_ASSERT(k < size());
  return vec_[k];
}

inline void
Dense_Row::swap(dimension_type i, dimension_type j) {
  std::swap((*this)[i], (*this)[j]);
}

inline void
Dense_Row::swap(iterator i, iterator j) {
  std::swap(*i, *j);
}

inline void
Dense_Row::reset(dimension_type i) {
  (*this)[i] = 0;
}

inline Dense_Row::iterator
Dense_Row::reset(iterator itr) {
  *itr = 0;
  ++itr;
  return itr;
}

inline Dense_Row::iterator
Dense_Row::begin() {
  return iterator(*this, 0);
}

inline Dense_Row::const_iterator
Dense_Row::begin() const {
  return const_iterator(*this, 0);
}

inline Dense_Row::iterator
Dense_Row::end() {
  return iterator(*this, size());
}

inline Dense_Row::const_iterator
Dense_Row::end() const {
  return const_iterator(*this, size());
}

inline Coefficient_traits::const_reference
Dense_Row::get(dimension_type i) const {
  return (*this)[i];
}

inline Dense_Row::iterator
Dense_Row::find(dimension_type i) {
  return iterator(*this, i);
}

inline Dense_Row::const_iterator
Dense_Row::find(dimension_type i) const {
  return const_iterator(*this, i);
}

inline Dense_Row::iterator
Dense_Row::find(iterator itr, dimension_type i) {
  (void)itr;
  return iterator(*this, i);
}

inline Dense_Row::const_iterator
Dense_Row::find(const_iterator itr, dimension_type i) const {
  (void)itr;
  return const_iterator(*this, i);
}

inline Dense_Row::iterator
Dense_Row::lower_bound(dimension_type i) {
  return find(i);
}

inline Dense_Row::const_iterator
Dense_Row::lower_bound(dimension_type i) const {
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::lower_bound(iterator itr, dimension_type i) {
  return find(itr, i);
}

inline Dense_Row::const_iterator
Dense_Row::lower_bound(const_iterator itr, dimension_type i) const {
  return find(itr, i);
}

inline Dense_Row::iterator
Dense_Row::insert(dimension_type i,
                  Coefficient_traits::const_reference x) {
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::insert(dimension_type i) {
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::insert(iterator itr, dimension_type i,
                  Coefficient_traits::const_reference x) {
  (void)itr;
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::insert(iterator itr, dimension_type i) {
  (void)itr;
  return find(i);
}

inline memory_size_type
Dense_Row::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline memory_size_type
Dense_Row::total_memory_in_bytes(dimension_type capacity) const {
  return sizeof(*this) + external_memory_in_bytes(capacity);
}

/*! \relates Dense_Row */
inline bool
operator!=(const Dense_Row& x, const Dense_Row& y) {
  return !(x == y);
}


inline
Dense_Row::iterator::iterator()
  : row(NULL), i(0) {
  PPL_ASSERT(OK());
}

inline
Dense_Row::iterator::iterator(Dense_Row& row1,dimension_type i1)
  : row(&row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Coefficient&
Dense_Row::iterator::operator*() {
  PPL_ASSERT(i < row->size());
  return (*row)[i];
}

inline Coefficient_traits::const_reference
Dense_Row::iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return (*row)[i];
}

inline dimension_type
Dense_Row::iterator::index() const {
  return i;
}

inline Dense_Row::iterator&
Dense_Row::iterator::operator++() {
  PPL_ASSERT(i < row->size());
  ++i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::iterator
Dense_Row::iterator::operator++(int) {
  iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline Dense_Row::iterator&
Dense_Row::iterator::operator--() {
  PPL_ASSERT(i > 0);
  --i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::iterator
Dense_Row::iterator::operator--(int) {
  iterator tmp(*this);
  --(*this);
  return tmp;
}

inline bool
Dense_Row::iterator::operator==(const iterator& x) const {
  return (row == x.row) && (i == x.i);
}

inline bool
Dense_Row::iterator::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline
Dense_Row::iterator::operator const_iterator() const {
  return const_iterator(*row, i);
}

inline bool
Dense_Row::iterator::OK() const {
  if (row == NULL)
    return true;
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row->size());
}


inline
Dense_Row::const_iterator::const_iterator()
  : row(NULL), i(0) {
  PPL_ASSERT(OK());
}

inline
Dense_Row::const_iterator::const_iterator(const Dense_Row& row1,
                                          dimension_type i1)
  : row(&row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Coefficient_traits::const_reference
Dense_Row::const_iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return (*row)[i];
}

inline dimension_type
Dense_Row::const_iterator::index() const {
  return i;
}

inline Dense_Row::const_iterator&
Dense_Row::const_iterator::operator++() {
  PPL_ASSERT(i < row->size());
  ++i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::const_iterator
Dense_Row::const_iterator::operator++(int) {
  const_iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline Dense_Row::const_iterator&
Dense_Row::const_iterator::operator--() {
  PPL_ASSERT(i > 0);
  --i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::const_iterator
Dense_Row::const_iterator::operator--(int) {
  const_iterator tmp(*this);
  --(*this);
  return tmp;
}

inline bool
Dense_Row::const_iterator::operator==(const const_iterator& x) const {
  return (row == x.row) && (i == x.i);
}

inline bool
Dense_Row::const_iterator::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline bool
Dense_Row::const_iterator::OK() const {
  if (row == NULL)
    return true;
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row->size());
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Dense_Row */
inline void
swap(Parma_Polyhedra_Library::Dense_Row& x,
     Parma_Polyhedra_Library::Dense_Row& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::Dense_Row */
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator x,
          std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_Dense_Row_inlines_hh)
