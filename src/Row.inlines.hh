/* Row class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include <vector>

namespace Parma_Polyhedra_Library {

/*!
  Allocates a chunk of memory able to contain \p capacity Integer objects
  beyond the specified \p fixed_size and returns a pointer to the new
  allocated memory.
*/
inline void*
Row::Impl::operator new(size_t fixed_size, size_t capacity) {
  return ::operator new(fixed_size + capacity*sizeof(Integer));
}


/*!
  Uses the standard operator delete to free the memory
  \p p points to.
*/
inline void
Row::Impl::operator delete(void* p) {
  ::operator delete(p);
}


/*!
  Placement version:
  uses the standard operator delete to free the memory \p p points to.
*/
inline void
Row::Impl::operator delete(void* p, size_t) {
  ::operator delete(p);
}


/*!
  Returns the actual size of the row \p this points to.
*/
inline size_t
Row::Impl::size() const {
  return size_;
}


/*!
  Sets to \p new_size the actual size of \p *this.
*/
inline void
Row::Impl::set_size(size_t new_sz) {
  size_ = new_sz;
}


/*!
  Increment the size of \p *this by 1.
*/
inline void
Row::Impl::bump_size() {
  ++size_;
}


/*!
  Shrinks the real implementation of the row if
  \p new_size is less than <CODE>size()</CODE> ,
  otherwise it is grown without copying the old contents.
*/
inline void
Row::Impl::resize_no_copy(size_t new_size) {
  if (new_size < size())
    shrink(new_size);
  else
    grow_no_copy(new_size);
}


inline
Row::Impl::Impl(Type t, size_t sz)
  : size_(0), type(t) {
  grow_no_copy(sz);
}


inline
Row::Impl::Impl(const Impl& y)
  : size_(0), type(y.type) {
  copy_construct(y);
}

inline
Row::Impl::Impl(const Impl& y, size_t sz)
  : size_(0), type(y.type) {
  copy_construct(y);
  grow_no_copy(sz);
}


/*!
  Uses <CODE>shrink()</CODE> method with argument \f$0\f$
  to delete all the row elements.
*/
inline
Row::Impl::~Impl() {
  shrink(0);
}


/*!
  Returns a reference to the \p k-th element of \p *this row.
*/
inline Integer&
Row::Impl::operator[](size_t k) {
  assert(k < size());
  return vec_[k];
}


/*!
  Returns a constant reference to the \p k-th element of \p *this row.
*/
inline const Integer&
Row::Impl::operator[](size_t k) const {
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

/*!
  Returns the size of \p *this row.
*/
inline size_t
Row::size() const {
  return impl->size();
}


inline Row::Type
Row::type() const {
  return impl->type;
}


/*!
  Returns <CODE>true</CODE> if \p *this row represent a constraint
  or generator in a necessarily closed polyhedron.
*/
inline bool
Row::is_necessarily_closed() const {
  return type().is_necessarily_closed();
}

inline size_t
Row::space_dimension() const {
  size_t sz = size();
  return (sz == 0)
    ? 0
    : sz - (is_necessarily_closed() ? 1 : 2);
}

#if EXTRA_ROW_DEBUG
inline size_t
Row::capacity() const {
  return capacity_;
}
#endif

/*!
  All elements of the row are initialized to \f$0\f$.
*/
inline
Row::Row()
  : impl(0) {
}


/*!
  \param t          The type of the row that will be constructed.
  \param sz         The size of the row that will be constructed.
  \param capacity   The capacity of the row that will be constructed.

  The row that we are constructing has a fixed capacity, i.e., it can
  contain \p capacity elements; furthermore the actual number of elements
  that has to be considered is \p sz.
*/
inline void
Row::construct(Type t, size_t sz, size_t capacity) {
  assert(capacity >= sz);
  impl = new (capacity) Impl(t, sz);
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}


/*!
  Builds a row having the capacity equal to its \p sz.
*/
inline void
Row::construct(Type t, size_t sz) {
  construct(t, sz, sz);
}

inline
Row::Row(Type t, size_t sz, size_t capacity) {
  construct(t, sz, capacity);
}

inline
Row::Row(Type t, size_t sz) {
  construct(t, sz);
}

inline
Row::Row(const Row& y)
  : impl(y.impl
	 ? new (compute_capacity(y.size())) Impl(*y.impl)
	 : 0) {
#if EXTRA_ROW_DEBUG
  capacity_ = y.impl ? compute_capacity(y.size()) : 0;
#endif
}

/*!
  Allows to specify a capacity,
  provided it is greater than or equal to \p y size.
*/
inline
Row::Row(const Row& y, size_t capacity) {
  assert(capacity >= y.size());
  impl = y.impl ? new (capacity) Impl(*y.impl) : 0;
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

/*!
  Allows to specify size and capacity,
  provided they are both greater then or equal to \p y size.
  Of course, \p sz must also be less than or equal to \p capacity.
*/
inline
Row::Row(const Row& y, size_t sz, size_t capacity) {
  assert(capacity >= y.size());
  impl = y.impl ? new (capacity) Impl(*y.impl, sz) : 0;
#if EXTRA_ROW_DEBUG
  capacity_ = capacity;
#endif
}

inline
Row::~Row() {
  delete impl;
}

/*!
  Shrinks the row if \p new_sz is less than <CODE>size()</CODE> ,
  otherwise grows the row without copying the old contents.
*/
inline void
Row::resize_no_copy(size_t new_sz) {
  assert(impl);
#if EXTRA_ROW_DEBUG
  assert(new_sz <= capacity_);
#endif
  impl->resize_no_copy(new_sz);
}

/*!
  Adds new positions to \p *this row obtaining a new row having
  size \p new_sz.
*/
inline void
Row::grow_no_copy(size_t new_sz) {
  assert(impl);
#if EXTRA_ROW_DEBUG
  assert(new_sz <= capacity_);
#endif
  impl->grow_no_copy(new_sz);
}

/*!
  Delete elements of \p *this row from \p new_sz-th position to
  the end.
*/
inline void
Row::shrink(size_t new_sz) {
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


/*!
  Returns <CODE>true</CODE> if \p *this row represent a line or
  an equality; <CODE>false</CODE> otherwise.
*/
inline bool
Row::is_line_or_equality() const {
  return type().is_line_or_equality();
}

/*!
  Returns <CODE>true</CODE> if the row represent a ray, a point or an
  inequality; <CODE>false</CODE> otherwise.
*/
inline bool
Row::is_ray_or_point_or_inequality() const {
  return type().is_ray_or_point_or_inequality();
}

/*!
  Returns the topological kind of \p *this.
*/
inline Topology
Row::topology() const {
  return type().topology();
}

/*!
  Sets to \p LINE_OR_EQUALITY the type of \p *this row.
*/
inline void
Row::set_is_line_or_equality() {
  impl->type.set_is_line_or_equality();
}


/*!
  Sets to \p RAY_OR_POINT_OR_INEQUALITY the type of \p *this row.
*/
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


/*!
  Returns a reference to the element of the row indexed by \p k.
*/
inline Integer&
Row::operator[](size_t k) {
  return (*impl)[k];
}

/*!
  Returns a constant reference to the element of the row indexed by \p k.
*/
inline const Integer&
Row::operator[](size_t k) const {
  return (*impl)[k];
}

inline const Integer&
Row::inhomogeneous_term() const {
  return (*this)[0];
}

inline const Integer&
Row::coefficient(size_t k) const {
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

/*!
  \relates Parma_Polyhedra_Library::Row
  Specializes <CODE>std::iter_swap</CODE>.
*/
/* FIXME: this was the original comment causing a doxygen bug.
  Specializes <CODE>std::iter_swap</CODE>
  for <CODE>std::vector<Row>::iterator</CODE>.
*/
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::Row>::iterator x,
	  std::vector<Parma_Polyhedra_Library::Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_Row_inlines_hh)
