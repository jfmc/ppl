/* Row class implementation: inline functions.
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

#ifndef PPL_Row_inlines_hh
#define PPL_Row_inlines_hh 1

#include "globals.hh"
#include <cassert>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline void*
Row::Impl::operator new(size_t fixed_size, dimension_type capacity) {
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
Row::Impl::size() const {
  return size_;
}

inline void
Row::Impl::set_size(dimension_type new_sz) {
  size_ = new_sz;
}

inline void
Row::Impl::bump_size() {
  ++size_;
}

inline void
Row::Impl::resize_no_copy(dimension_type new_sz) {
  if (new_sz < size())
    shrink(new_sz);
  else
    grow_no_copy(new_sz);
}

inline
Row::Impl::Impl(Type t, dimension_type sz)
  : size_(0), type(t) {
  grow_no_copy(sz);
}

inline
Row::Impl::Impl(const Impl& y)
  : size_(0), type(y.type) {
  copy_construct(y);
}

inline
Row::Impl::Impl(const Impl& y, dimension_type sz)
  : size_(0), type(y.type) {
  copy_construct(y);
  grow_no_copy(sz);
}

inline
Row::Impl::~Impl() {
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  shrink(0);
#else
  shrink(1);
#endif
}

inline Integer&
Row::Impl::operator[](dimension_type k) {
  assert(k < size());
  return vec_[k];
}

inline const Integer&
Row::Impl::operator[](dimension_type k) const {
  assert(k < size());
  return vec_[k];
}

inline
Row::Type::Type()
  : flags(0) {
}

inline
Row::Type::Type(Topology topol, Kind kind)
  : flags(static_cast<flags_t>(topol | (kind << 1))) {
}

inline
Row::Type::Type(flags_t mask)
  : flags(mask) {
}

inline bool
Row::Type::test_all(flags_t mask) const {
  return (flags & mask) == mask;
}

inline void
Row::Type::set(flags_t mask) {
  flags |= mask;
}

inline void
Row::Type::reset(flags_t mask) {
  flags &= ~mask;
}

inline bool
Row::Type::is_ray_or_point_or_inequality() const {
  return test_all(RPI);
}

inline void
Row::Type::set_is_ray_or_point_or_inequality() {
  set(RPI);
}

inline bool
Row::Type::is_line_or_equality() const {
  return !is_ray_or_point_or_inequality();
}

inline void
Row::Type::set_is_line_or_equality() {
  reset(RPI);
}

inline Topology
Row::Type::topology() const {
  return test_all(NNC) ? NOT_NECESSARILY_CLOSED : NECESSARILY_CLOSED;
}

inline bool
Row::Type::is_necessarily_closed() const {
  return !test_all(NNC);
}

inline void
Row::Type::set_necessarily_closed() {
  reset(NNC);
}

inline void
Row::Type::set_not_necessarily_closed() {
  set(NNC);
}

inline dimension_type
Row::size() const {
  return impl->size();
}

inline Row::Type
Row::type() const {
  return impl->type;
}

inline bool
Row::is_necessarily_closed() const {
  return type().is_necessarily_closed();
}

inline dimension_type
Row::space_dimension() const {
  dimension_type sz = size();
  return (sz == 0)
    ? 0
    : sz - (is_necessarily_closed() ? 1 : 2);
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
Row::construct(Type t, dimension_type sz, dimension_type capacity) {
  assert(capacity >= sz);
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  impl = new (capacity) Impl(t, sz);
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

inline void
Row::construct(Type t, dimension_type sz) {
  construct(t, sz, sz);
}

inline
Row::Row(Type t, dimension_type sz, dimension_type capacity) {
  construct(t, sz, capacity);
}

inline
Row::Row(Type t, dimension_type sz) {
  construct(t, sz);
}

inline
Row::Row(const Row& y)
  : impl(y.impl
	 ? new (compute_capacity(y.size())) Impl(*y.impl)
	 : 0) {
#if EXTRA_ROW_DEBUG
 #if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  capacity_ = y.impl ? compute_capacity(y.size()) : 0;
 #else
  capacity_ = y.impl ? compute_capacity(y.size()) : 1;
 #endif
#endif
}

inline
Row::Row(const Row& y, dimension_type capacity) {
  assert(capacity >= y.size());
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
Row::Row(const Row& y, dimension_type sz, dimension_type capacity) {
  assert(capacity >= sz);
  assert(sz >= y.size());
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
Row::resize_no_copy(dimension_type new_sz) {
  assert(impl);
#if EXTRA_ROW_DEBUG
  assert(new_sz <= capacity_);
#endif
  impl->resize_no_copy(new_sz);
}

inline void
Row::grow_no_copy(dimension_type new_sz) {
  assert(impl);
#if EXTRA_ROW_DEBUG
  assert(new_sz <= capacity_);
#endif
  impl->grow_no_copy(new_sz);
}

inline void
Row::shrink(dimension_type new_sz) {
  assert(impl);
  impl->shrink(new_sz);
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

inline bool
Row::is_line_or_equality() const {
  return type().is_line_or_equality();
}

inline bool
Row::is_ray_or_point_or_inequality() const {
  return type().is_ray_or_point_or_inequality();
}

inline Topology
Row::topology() const {
  return type().topology();
}

inline void
Row::set_is_line_or_equality() {
  impl->type.set_is_line_or_equality();
}

inline void
Row::set_is_ray_or_point_or_inequality() {
  impl->type.set_is_ray_or_point_or_inequality();
}

inline void
Row::set_necessarily_closed() {
  impl->type.set_necessarily_closed();
}

inline void
Row::set_not_necessarily_closed() {
  impl->type.set_not_necessarily_closed();
}

inline Integer&
Row::operator[](dimension_type k) {
  return (*impl)[k];
}

inline const Integer&
Row::operator[](dimension_type k) const {
  return (*impl)[k];
}

inline const Integer&
Row::inhomogeneous_term() const {
  return (*this)[0];
}

inline const Integer&
Row::coefficient(dimension_type k) const {
  return (*this)[k+1];
}

/*! \relates Row */
inline bool
operator==(const Row& x, const Row& y) {
  return compare(x, y) == 0;
}

/*! \relates Row */
inline bool
operator!=(const Row& x, const Row& y) {
  return compare(x, y) != 0;
}

/*! \relates Row */
inline bool
operator<=(const Row& x, const Row& y) {
  return compare(x, y) <= 0;
}

/*! \relates Row */
inline bool
operator<(const Row& x, const Row& y) {
  return compare(x, y) < 0;
}

/*! \relates Row */
inline bool
operator>=(const Row& x, const Row& y) {
  return compare(x, y) >= 0;
}

/*! \relates Row */
inline bool
operator>(const Row& x, const Row& y) {
  return compare(x, y) > 0;
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
