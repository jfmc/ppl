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

#include "math_utilities.defs.hh"
#include "assert.hh"
#include <cstddef>
#include <limits>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Dense_Row::Flags::Flags()
  : bits(0) {
}

inline
Dense_Row::Flags::Flags(base_type n)
  : bits(n) {
}

inline Dense_Row::Flags::base_type
Dense_Row::Flags::get_bits() const {
  return bits;
}

inline void
Dense_Row::Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Dense_Row::Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Dense_Row::Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline bool
Dense_Row::Flags::operator==(const Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Dense_Row::Flags::operator!=(const Flags& y) const {
  return !operator==(y);
}

inline void*
Dense_Row_Impl_Handler::Impl::operator new(const size_t fixed_size,
                                           const dimension_type capacity) {
#if PPL_CXX_SUPPORTS_FLEXIBLE_ARRAYS
  return ::operator new(fixed_size + capacity*sizeof(Coefficient));
#else
  PPL_ASSERT(capacity >= 1);
  return ::operator new(fixed_size + (capacity-1)*sizeof(Coefficient));
#endif
}

inline void
Dense_Row_Impl_Handler::Impl::operator delete(void* p) {
  ::operator delete(p);
}

inline void
Dense_Row_Impl_Handler::Impl::operator delete(void* p, dimension_type) {
  ::operator delete(p);
}

inline dimension_type
Dense_Row_Impl_Handler::Impl::max_size() {
  return std::numeric_limits<size_t>::max() / sizeof(Coefficient);
}

inline dimension_type
Dense_Row_Impl_Handler::Impl::size() const {
  return size_;
}

inline void
Dense_Row_Impl_Handler::Impl::set_size(const dimension_type new_size) {
  size_ = new_size;
}

inline void
Dense_Row_Impl_Handler::Impl::bump_size() {
  ++size_;
}

inline
Dense_Row_Impl_Handler::Impl::Impl(const Dense_Row::Flags f)
  : size_(0), flags_(f) {
}

inline
Dense_Row_Impl_Handler::Impl::~Impl() {
  shrink(0);
}

inline const Dense_Row::Flags&
Dense_Row_Impl_Handler::Impl::flags() const {
  return flags_;
}

inline Dense_Row::Flags&
Dense_Row_Impl_Handler::Impl::flags() {
  return flags_;
}

inline Coefficient&
Dense_Row_Impl_Handler::Impl::operator[](const dimension_type k) {
  PPL_ASSERT(k < size());
  return vec_[k];
}

inline Coefficient_traits::const_reference
Dense_Row_Impl_Handler::Impl::operator[](const dimension_type k) const {
  PPL_ASSERT(k < size());
  return vec_[k];
}

inline memory_size_type
Dense_Row_Impl_Handler::Impl
::total_memory_in_bytes(dimension_type capacity) const {
  return
    sizeof(*this)
    + capacity*sizeof(Coefficient)
#if !PPL_CXX_SUPPORTS_FLEXIBLE_ARRAYS
    - 1*sizeof(Coefficient)
#endif
    + external_memory_in_bytes();
}

inline memory_size_type
Dense_Row_Impl_Handler::Impl::total_memory_in_bytes() const {
  // In general, this is a lower bound, as the capacity of *this
  // may be strictly greater than `size_'
  return total_memory_in_bytes(size_);
}

inline dimension_type
Dense_Row::max_size() {
  return Impl::max_size();
}

inline dimension_type
Dense_Row::size() const {
  return impl->size();
}

inline const Dense_Row::Flags&
Dense_Row::flags() const {
  return impl->flags();
}

inline Dense_Row::Flags&
Dense_Row::flags() {
  return impl->flags();
}

#if PPL_ROW_EXTRA_DEBUG
inline dimension_type
Dense_Row::capacity() const {
  return capacity_;
}
#endif

inline
Dense_Row_Impl_Handler::Dense_Row_Impl_Handler()
  : impl(0) {
#if PPL_ROW_EXTRA_DEBUG
  capacity_ = 0;
#endif
}

inline
Dense_Row_Impl_Handler::~Dense_Row_Impl_Handler() {
  delete impl;
}

inline
Dense_Row::Dense_Row()
  : Dense_Row_Impl_Handler() {
}

inline void
Dense_Row::allocate(
#if PPL_CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       const
#endif
	       dimension_type capacity,
	       const Flags f) {
  PPL_ASSERT(capacity <= max_size());
#if !PPL_CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  PPL_ASSERT(impl == 0);
  impl = new (capacity) Impl(f);
#if PPL_ROW_EXTRA_DEBUG
  PPL_ASSERT(capacity_ == 0);
  capacity_ = capacity;
#endif
}

inline void
Dense_Row::expand_within_capacity(const dimension_type new_size) {
  PPL_ASSERT(impl);
#if PPL_ROW_EXTRA_DEBUG
  PPL_ASSERT(new_size <= capacity_);
#endif
  impl->expand_within_capacity(new_size);
}

inline void
Dense_Row::copy_construct_coefficients(const Dense_Row& y) {
  PPL_ASSERT(impl && y.impl);
#if PPL_ROW_EXTRA_DEBUG
  PPL_ASSERT(y.size() <= capacity_);
#endif
  impl->copy_construct_coefficients(*(y.impl));
}

inline void
Dense_Row::construct(const dimension_type sz,
                     const dimension_type capacity,
                     const Flags f) {
  PPL_ASSERT(sz <= capacity && capacity <= max_size());
  allocate(capacity, f);
  expand_within_capacity(sz);
}

inline void
Dense_Row::construct(const dimension_type sz, const Flags f) {
  construct(sz, sz, f);
}

inline
Dense_Row::Dense_Row(const dimension_type sz,
                     const dimension_type capacity,
                     const Flags f)
  : Dense_Row_Impl_Handler() {
  construct(sz, capacity, f);
}

inline
Dense_Row::Dense_Row(const dimension_type sz, const Flags f)
  : Dense_Row_Impl_Handler() {
  construct(sz, f);
}

inline
Dense_Row::Dense_Row(const Dense_Row& y)
  : Dense_Row_Impl_Handler() {
  if (y.impl) {
    allocate(compute_capacity(y.size(), max_size()), y.flags());
    copy_construct_coefficients(y);
  }
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
                     const dimension_type capacity)
  : Dense_Row_Impl_Handler() {
  PPL_ASSERT(y.impl);
  PPL_ASSERT(y.size() <= capacity && capacity <= max_size());
  allocate(capacity, y.flags());
  copy_construct_coefficients(y);
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
                     const dimension_type sz,
                     const dimension_type capacity)
  : Dense_Row_Impl_Handler() {
  PPL_ASSERT(y.impl);
  PPL_ASSERT(y.size() <= sz && sz <= capacity && capacity <= max_size());
  allocate(capacity, y.flags());
  copy_construct_coefficients(y);
  expand_within_capacity(sz);
}

inline
Dense_Row::~Dense_Row() {
}

inline void
Dense_Row::shrink(const dimension_type new_size) {
  PPL_ASSERT(impl);
  impl->shrink(new_size);
}

inline void
Dense_Row::swap(Dense_Row& y) {
  std::swap(impl, y.impl);
#if PPL_ROW_EXTRA_DEBUG
  std::swap(capacity_, y.capacity_);
#endif
}

inline void
Dense_Row::assign(Dense_Row& y) {
  impl = y.impl;
#if PPL_ROW_EXTRA_DEBUG
  capacity_ = y.capacity_;
#endif
}

inline Dense_Row&
Dense_Row::operator=(const Dense_Row& y) {
  // Copy-construct `tmp' from `y'.
  Dense_Row tmp(y);
  // Swap the implementation of `*this' with the one of `tmp'.
  swap(tmp);
  // Now `tmp' goes out of scope, so the old `*this' will be destroyed.
  return *this;
}

inline Coefficient&
Dense_Row::operator[](const dimension_type k) {
  PPL_ASSERT(impl);
  return (*impl)[k];
}

inline Coefficient_traits::const_reference
Dense_Row::operator[](const dimension_type k) const {
  PPL_ASSERT(impl);
  return (*impl)[k];
}

inline void
Dense_Row::swap(dimension_type i, dimension_type j) {
  std::swap((*this)[i], (*this)[j]);
}

inline void
Dense_Row::swap(iterator i, iterator j) {
  std::swap(i->second, j->second);
}

inline void
Dense_Row::reset(dimension_type i) {
  (*this)[i] = 0;
}

inline Dense_Row::iterator
Dense_Row::reset(iterator itr) {
  itr->second = 0;
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

inline const Coefficient&
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
Dense_Row::find_create(dimension_type i, const Coefficient& x) {
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(dimension_type i) {
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(iterator itr, dimension_type i, const Coefficient& x) {
  (void)itr;
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(iterator itr, dimension_type i) {
  (void)itr;
  return find(i);
}

inline memory_size_type
Dense_Row::external_memory_in_bytes(dimension_type capacity) const {
  return impl->total_memory_in_bytes(capacity);
}

inline memory_size_type
Dense_Row::total_memory_in_bytes(dimension_type capacity) const {
  return sizeof(*this) + external_memory_in_bytes(capacity);
}

inline memory_size_type
Dense_Row::external_memory_in_bytes() const {
#if PPL_ROW_EXTRA_DEBUG
  return impl->total_memory_in_bytes(capacity_);
#else
  return impl->total_memory_in_bytes();
#endif
}

inline memory_size_type
Dense_Row::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
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

inline Dense_Row::iterator::value_type
Dense_Row::iterator::operator*() {
  PPL_ASSERT(i < row->size());
  return value_type(i, (*row)[i]);
}

inline Dense_Row::iterator::const_type
Dense_Row::iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return const_type(i, (*row)[i]);
}

inline Dense_Row::iterator::Member_Access_Helper
Dense_Row::iterator::operator->() {
  PPL_ASSERT(i < row->size());
  return Member_Access_Helper(i, (*row)[i]);
}

inline Dense_Row::iterator::Const_Member_Access_Helper
Dense_Row::iterator::operator->() const {
  PPL_ASSERT(i < row->size());
  return Const_Member_Access_Helper(i, (*row)[i]);
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
Dense_Row::iterator::Member_Access_Helper
::Member_Access_Helper(dimension_type index, Coefficient& data)
  : value(index, data) {
}

inline Dense_Row::iterator::value_type*
Dense_Row::iterator::Member_Access_Helper::operator->() {
  return &value;
}


inline
Dense_Row::iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type index, const Coefficient& data)
  : value(index, data) {
}

inline const Dense_Row::iterator::const_type*
Dense_Row::iterator::Const_Member_Access_Helper::operator->() const {
  return &value;
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

inline Dense_Row::const_iterator::const_type
Dense_Row::const_iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return const_type(i, (*row)[i]);
}

inline Dense_Row::const_iterator::Const_Member_Access_Helper
Dense_Row::const_iterator::operator->() const {
  PPL_ASSERT(i < row->size());
  return Const_Member_Access_Helper(i, (*row)[i]);
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



inline
Dense_Row::const_iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type index, const Coefficient& data)
  : value(index, data) {
}

inline const Dense_Row::const_iterator::const_type*
Dense_Row::const_iterator::Const_Member_Access_Helper::operator->() const {
  return &value;
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
