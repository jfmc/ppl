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
  Returns the type of the row \p this points to.
*/
inline Row::Type
Row::Impl::type() const {
  return type_;
}

/*!
  Returns the type of the row \p this points to.
*/
inline void
Row::Impl::set_type(Type t) {
  type_ = t;
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
Row::Impl::set_size(size_t new_size) {
  size_ = new_size;
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
Row::Impl::Impl(Type type, size_t size)
  : size_(0), type_(type) {
  grow_no_copy(size);
}


inline
Row::Impl::Impl(const Impl& y)
  : size_(0), type_(y.type()) {
  copy_construct(y);
}

inline
Row::Impl::Impl(const Impl& y, size_t size)
  : size_(0), type_(y.type()) {
  copy_construct(y);
  grow_no_copy(size);
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
Row::Impl::operator [](size_t k) {
  assert(k < size());
  return vec_[k];
}


/*!
  Returns a constant reference to the \p k-th element of \p *this row.
*/
inline const Integer&
Row::Impl::operator [](size_t k) const {
  assert(k < size());
  return vec_[k];
}

/*!
  Returns the size of \p *this row.
*/
inline size_t
Row::size() const {
  return impl->size();
}

#ifndef NDEBUG
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
  \param type       The type of the row that will be constructed.
  \param size       The size of the row that will be constructed.
  \param capacity   The capacity of the row that will be constructed.

  The row that we are constructing has a fixed capacity, i.e., it can
  contain \p capacity elements; furthermore the actual number of elements
  that has to be considered is \p size.
*/
inline void
Row::construct(Type type, size_t size, size_t capacity) {
  assert(capacity >= size);
  impl = new (capacity) Impl(type, size);
#ifndef NDEBUG
  capacity_ = capacity;
#endif
}


/*!
  Builds a row having the capacity equal to its \p size.
*/
inline void
Row::construct(Type type, size_t size) {
  construct(type, size, size);
}

inline
Row::Row(Type type, size_t size, size_t capacity) {
  construct(type, size, capacity);
}

inline
Row::Row(Type type, size_t size) {
  construct(type, size);
}

inline
Row::Row(const Row& y)
  : impl(y.impl
	 ? new (compute_capacity(y.size())) Impl(*y.impl)
	 : 0) {
#ifndef NDEBUG
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
#ifndef NDEBUG
  capacity_ = capacity;
#endif
}

/*!
  Allows to specify size and capacity,
  provided they are both greater then or equal to \p y size.
  Of course, \p size must also be less than or equal to \p capacity.
*/
inline
Row::Row(const Row& y, size_t size, size_t capacity) {
  assert(capacity >= y.size());
  impl = y.impl ? new (capacity) Impl(*y.impl, size) : 0;
#ifndef NDEBUG
  capacity_ = capacity;
#endif
}

inline
Row::~Row() {
  delete impl;
}

/*!
  Shrinks the row if \p new_size is less than <CODE>size()</CODE> ,
  otherwise grows the row without copying the old contents.
*/
inline void
Row::resize_no_copy(size_t new_size) {
  assert(impl);
  assert(new_size <= capacity_);
  impl->resize_no_copy(new_size);
}

/*!
  Adds new positions to \p *this row obtaining a new row having
  size \p new_size.
*/
inline void
Row::grow_no_copy(size_t new_size) {
  assert(impl);
  assert(new_size <= capacity_);
  impl->grow_no_copy(new_size);
}

/*!
  Delete elements of \p *this row from \p new_size-th position to
  the end.
*/
inline void
Row::shrink(size_t new_size) {
  assert(impl);
  impl->shrink(new_size);
}

inline void
Row::swap(Row& y) {
  std::swap(impl, y.impl);
#ifndef NDEBUG
  std::swap(capacity_, y.capacity_);
#endif
}

inline void
Row::assign(Row& y) {
  impl = y.impl;
#ifndef NDEBUG
  capacity_ = y.capacity_;
#endif
}


inline Row&
Row::operator =(const Row& y) {
  // Copy-construct `tmp' from `y'.
  Row tmp(y);
  // Swap the implementation of `*this' with the one of `tmp'.
  swap(tmp);
  // Now `tmp' goes out of scope, so the old `*this' will be destroyed.
  return *this;
}


inline Row::Type
Row::type() const {
  return impl->type();
}


/*!
  Returns <CODE>true</CODE> if \p *this row represent a line or
  an equality; <CODE>false</CODE> otherwise.
*/
inline bool
Row::is_line_or_equality() const {
  return type() == LINE_OR_EQUALITY;
}


/*!
  Returns <CODE>true</CODE> if the row represent a ray, a vertex or an
  inequality; <CODE>false</CODE> otherwise.
*/
inline bool
Row::is_ray_or_vertex_or_inequality() const {
  return type() == RAY_OR_VERTEX_OR_INEQUALITY;
}


/*!
  Sets to \p LINE_OR_EQUALITY the type of \p *this row.
*/
inline void
Row::set_is_line_or_equality() {
  impl->set_type(LINE_OR_EQUALITY);
}


/*!
  Sets to \p RAY_OR_VERTEX_OR_INEQUALITY the type of \p *this row.
*/
inline void
Row::set_is_ray_or_vertex_or_inequality() {
  impl->set_type(RAY_OR_VERTEX_OR_INEQUALITY);
}


/*!
  Returns a reference to the element of the row indexed by \p k.
*/
inline Integer&
Row::operator [](size_t k) {
  return (*impl)[k];
}


/*!
  Returns a constant reference to the element of the row indexed by \p k.
*/
inline const Integer&
Row::operator [](size_t k) const {
  return (*impl)[k];
}

inline const Integer&
Row::coefficient() const {
  return (*this)[0];
}

inline const Integer&
Row::coefficient(size_t k) const {
  return (*this)[k+1];
}



inline bool
operator ==(const Row& x, const Row& y) {
  return compare(x, y) == 0;
}

inline bool
operator !=(const Row& x, const Row& y) {
  return compare(x, y) != 0;
}

inline bool
operator <=(const Row& x, const Row& y) {
  return compare(x, y) <= 0;
}

inline bool
operator <(const Row& x, const Row& y) {
  return compare(x, y) < 0;
}

inline bool
operator >=(const Row& x, const Row& y) {
  return compare(x, y) >= 0;
}

inline bool
operator >(const Row& x, const Row& y) {
  return compare(x, y) > 0;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*!
  Specialize <CODE>std::swap</CODE> to use the fast swap that is
  provided as a member function instead of using the default
  algorithm (which creates a temporary and uses assignment).
*/
inline void
swap(Parma_Polyhedra_Library::Row& x, Parma_Polyhedra_Library::Row& y) {
  x.swap(y);
}

/*!
  Specialize <CODE>std::iter_swap</CODE>
  for <CODE>vector<Row>::iterator</CODE>.
*/
inline void
iter_swap(vector<Parma_Polyhedra_Library::Row>::iterator x,
	  vector<Parma_Polyhedra_Library::Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std
