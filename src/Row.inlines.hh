/* Row class implementation: inline functions.
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

#ifndef PPL_Row_inlines_hh
#define PPL_Row_inlines_hh 1

#include "globals.defs.hh"
#include <cassert>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Row::Flags::Flags()
  : bits(0) {
}

inline
Row::Flags::Flags(base_type n)
  : bits(n) {
}

inline Row::Flags::base_type
Row::Flags::get_bits() const {
  return bits;
}

inline void
Row::Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Row::Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Row::Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline bool
Row::Flags::operator==(const Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Row::Flags::operator!=(const Flags& y) const {
  return !operator==(y);
}

inline void*
Row::Impl::operator new(const size_t fixed_size,
			const dimension_type capacity) {
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  return ::operator new(fixed_size + capacity*sizeof(Integer));
#else
  assert(capacity >= 1);
  return ::operator new(fixed_size + (capacity-1)*sizeof(Integer));
#endif
}

inline void
Row::Impl::operator delete(void* p) {
  ::operator delete(p);
}

inline void
Row::Impl::operator delete(void* p, dimension_type) {
  ::operator delete(p);
}

inline dimension_type
Row::Impl::max_size() {
  return size_t(-1)/sizeof(Integer);
}

inline dimension_type
Row::Impl::size() const {
  return size_;
}

inline void
Row::Impl::set_size(const dimension_type new_size) {
  size_ = new_size;
}

inline void
Row::Impl::bump_size() {
  ++size_;
}

inline
Row::Impl::Impl(const dimension_type sz, const Flags f)
  : size_(0), flags_(f) {
  expand_within_capacity(sz);
}

inline
Row::Impl::Impl(const Impl& y)
  : size_(0), flags_(y.flags_) {
  copy_construct(y);
}

inline
Row::Impl::Impl(const Impl& y, const dimension_type sz)
  : size_(0), flags_(y.flags_) {
  copy_construct(y);
  expand_within_capacity(sz);
}

inline
Row::Impl::~Impl() {
  shrink(0);
}

inline const Row::Flags&
Row::Impl::flags() const {
  return flags_;
}

inline Row::Flags&
Row::Impl::flags() {
  return flags_;
}

inline Integer&
Row::Impl::operator[](const dimension_type k) {
  assert(k < size());
  return vec_[k];
}

inline Integer_traits::const_reference
Row::Impl::operator[](const dimension_type k) const {
  assert(k < size());
  return vec_[k];
}

inline dimension_type
Row::max_size() {
  return Impl::max_size();
}

inline dimension_type
Row::size() const {
  return impl->size();
}

inline const Row::Flags&
Row::flags() const {
  return impl->flags();
}

inline Row::Flags&
Row::flags() {
  return impl->flags();
}

#if EXTRA_ROW_DEBUG
inline dimension_type
Row::capacity() const {
  return capacity_;
}
#endif

inline
Row::Row()
  : impl(0) {
}

inline void
Row::construct(const dimension_type sz,
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       const
#endif
	       dimension_type capacity,
	       const Flags f) {
  assert(sz <= capacity && capacity <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  impl = new (capacity) Impl(sz, f);
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

inline void
Row::construct(const dimension_type sz, const Flags f) {
  construct(sz, sz, f);
}

inline
Row::Row(const dimension_type sz, const dimension_type capacity,
	 const Flags f) {
  construct(sz, capacity, f);
}

inline
Row::Row(const dimension_type sz, const Flags f) {
  construct(sz, f);
}

inline
Row::Row(const Row& y)
  : impl(y.impl
	 ? new (compute_capacity(y.size(), Row::max_size())) Impl(*y.impl)
	 : 0) {
#if EXTRA_ROW_DEBUG
# if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  capacity_ = y.impl ? compute_capacity(y.size(), Row::max_size()) : 0;
# else
  capacity_ = y.impl ? compute_capacity(y.size(), Row::max_size()) : 1;
# endif
#endif
}

inline
Row::Row(const Row& y,
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
	 const
#endif
	 dimension_type capacity) {
  assert(y.size() <= capacity && capacity <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  impl = y.impl ? new (capacity) Impl(*y.impl) : 0;
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

inline
Row::Row(const Row& y,
	 const dimension_type sz,
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
	 const
#endif
	 dimension_type capacity) {
  assert(y.size() <= sz && sz <= capacity && capacity <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  impl = y.impl ? new (capacity) Impl(*y.impl, sz) : 0;
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

inline
Row::~Row() {
  delete impl;
}

inline void
Row::expand_within_capacity(const dimension_type new_size) {
  assert(impl);
#if EXTRA_ROW_DEBUG
  assert(new_size <= capacity_);
#endif
  impl->expand_within_capacity(new_size);
}

inline void
Row::shrink(const dimension_type new_size) {
  assert(impl);
  impl->shrink(new_size);
}

inline void
Row::swap(Row& y) {
  std::swap(impl, y.impl);
#if EXTRA_ROW_DEBUG
  std::swap(capacity_, y.capacity_);
#endif
}

inline void
Row::assign(Row& y) {
  impl = y.impl;
#if EXTRA_ROW_DEBUG
  capacity_ = y.capacity_;
#endif
}

inline Row&
Row::operator=(const Row& y) {
  // Copy-construct `tmp' from `y'.
  Row tmp(y);
  // Swap the implementation of `*this' with the one of `tmp'.
  swap(tmp);
  // Now `tmp' goes out of scope, so the old `*this' will be destroyed.
  return *this;
}

inline Integer&
Row::operator[](const dimension_type k) {
  return (*impl)[k];
}

inline Integer_traits::const_reference
Row::operator[](const dimension_type k) const {
  return (*impl)[k];
}

/*! \relates Row */ 
inline bool
operator!=(const Row& x, const Row& y) {
  return !(x == y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Row */
inline void
swap(Parma_Polyhedra_Library::Row& x, Parma_Polyhedra_Library::Row& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::Row */
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::Row>::iterator x,
	  std::vector<Parma_Polyhedra_Library::Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_Row_inlines_hh)
