/* Row class implementation: inline functions.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <cassert>
#include <algorithm>

/*!
  Allocates a chunk of memory able to contain \p capacity Integer objects
  beyond the specified \p fixed_size and returns a pointer to the new 
  allocated memory.
*/
INLINE void*
Parma_Polyhedra_Library::Row::Impl::operator
new(size_t fixed_size, size_t capacity) {
  return ::operator new(fixed_size + capacity*sizeof(Integer));
}


/*!
  Uses the standard operator delete to free the memory 
  \p p points to.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::operator delete(void* p) {
  ::operator delete(p);
}


/*!
  Placement version:
  uses the standard operator delete to free the memory \p p points to.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::operator delete(void* p, size_t) {
  ::operator delete(p);
}


/*!
  Returns the type of the row \p this points to. 
*/
INLINE Parma_Polyhedra_Library::Row::Type
Parma_Polyhedra_Library::Row::Impl::type() const {
  return type_;
}

/*!
  Returns the type of the row \p this points to.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::set_type(Type t) {
  type_ = t;
}

/*!
  Returns the actual size of the row \p this points to.
*/
INLINE size_t
Parma_Polyhedra_Library::Row::Impl::size() const {
  return size_;
}


/*!
  Sets to \p new_size the actual size of \p *this.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::set_size(size_t new_size) {
  size_ = new_size;
}


/*!
  Increment the size of \p *this by 1.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::bump_size() {
  ++size_;
}


/*!
  Adds new positions to the real implementation of the row 
  obtaining a new row having size \p new_size.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::grow_no_copy(size_t new_size) {
  assert(size() <= new_size);
  for (size_t i = size(); i < new_size; ++i) {
    new (&vec_[i]) Integer();
    bump_size();
  }
}


/*!
  Delete elements from the real implementation of the row 
  from \p new_size - th position to the end.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::shrink(size_t new_size) {
  assert(new_size <= size());
  // We assume construction was done "forward".
  // We thus perform destruction "backward".
  for (size_t i = size(); i != new_size; ) {
    --i;
    // ~Integer() does not throw exceptions.  So we do.
    vec_[i].~Integer();
  }
  set_size(new_size);
}


/*!
  Shrinks the real implementation of the row if 
  \p new_size is less than <CODE>size()</CODE> , 
  otherwise it is grown without copying the old contents.
*/
INLINE void
Parma_Polyhedra_Library::Row::Impl::resize_no_copy(size_t new_size) {
  if (new_size < size())
    shrink(new_size);
  else
    grow_no_copy(new_size);
}


INLINE
Parma_Polyhedra_Library::Row::Impl::Impl(Type type, size_t size)
  : size_(0), type_(type) {
  grow_no_copy(size);
}


INLINE void
Parma_Polyhedra_Library::Row::Impl::copy_construct(const Impl& y) { 
  size_t y_size = y.size();
  for (size_t i = 0; i < y_size; ++i) {
    new (&vec_[i]) Integer(y.vec_[i]);
    bump_size();
  }
}

INLINE
Parma_Polyhedra_Library::Row::Impl::Impl(const Impl& y)
  : size_(0), type_(y.type()) {
  copy_construct(y);
}

INLINE
Parma_Polyhedra_Library::Row::Impl::Impl(const Impl& y, size_t size)
  : size_(0), type_(y.type()) {
  copy_construct(y);
  grow_no_copy(size);
}


/*!
  Uses <CODE>shrink()</CODE> method with argument \f$0\f$ 
  to delete all the row elements.
*/
INLINE
Parma_Polyhedra_Library::Row::Impl::~Impl() {
  shrink(0);
}


/*!
  Returns a reference to the \p k-th element of \p *this row.
*/
INLINE Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::Impl::operator [](size_t k) {
  assert(k < size());
  return vec_[k];
}


/*!
  Returns a constant reference to the \p k-th element of \p *this row.
*/
INLINE const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::Impl::operator [](size_t k) const {
  assert(k < size());
  return vec_[k];
}


/*!
  All elements of the row are initialized to \f$0\f$.
*/
INLINE
Parma_Polyhedra_Library::Row::Row()
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
INLINE void
Parma_Polyhedra_Library::Row::construct(Type type,
					size_t size,
					size_t capacity) {
  assert(capacity >= size);
  impl = new (capacity) Impl(type, size);
}


/*!
  Builds a row having the capacity equal to its \p size.
*/
INLINE void
Parma_Polyhedra_Library::Row::construct(Type type, size_t size) {
  construct(type, size, size);
}

INLINE
Parma_Polyhedra_Library::Row::Row(Type type, size_t size, size_t capacity) {
  construct(type, size, capacity);
}

INLINE
Parma_Polyhedra_Library::Row::Row(Type type, size_t size) {
  construct(type, size);
}

INLINE
Parma_Polyhedra_Library::Row::Row(const Row& y)
  : impl(y.impl ? new (y.impl->size()) Impl(*y.impl) : 0) {
}


/*!
  Returns the size of \p *this row.
*/
INLINE size_t
Parma_Polyhedra_Library::Row::size() const {
  return impl->size();
}


/*!
  Allows to specify a capacity,
  provided it is greater than or equal to \p y size.
*/
INLINE
Parma_Polyhedra_Library::Row::Row(const Row& y, size_t capacity) {
  assert(capacity >= y.size());
  impl = y.impl ? new (capacity) Impl(*y.impl) : 0;
}

/*!
  Allows to specify size and capacity,
  provided they are both greater then or equal to \p y size.
  Of course, \p size must also be less than or equal to \p capacity.
*/
INLINE
Parma_Polyhedra_Library::Row::Row(const Row& y, size_t size, size_t capacity) {
  assert(capacity >= y.size());
  impl = y.impl ? new (capacity) Impl(*y.impl, size) : 0;
}

INLINE
Parma_Polyhedra_Library::Row::~Row() {
  delete impl;
}

/*!
  Shrinks the row if \p new_size is less than <CODE>size()</CODE> , 
  otherwise grows the row without copying the old contents.
*/
INLINE void
Parma_Polyhedra_Library::Row::resize_no_copy(size_t new_size) {
  assert(impl);
  impl->resize_no_copy(new_size);
}

/*!
  Adds new positions to \p *this row obtaining a new row having 
  size \p new_size.
*/
INLINE void
Parma_Polyhedra_Library::Row::grow_no_copy(size_t new_size) {
  assert(impl);
  impl->grow_no_copy(new_size);
}

/*!
  Delete elements of \p *this row from \p new_size-th position to 
  the end.
*/
INLINE void
Parma_Polyhedra_Library::Row::shrink(size_t new_size) {
  assert(impl);
  impl->shrink(new_size);
}

INLINE void
Parma_Polyhedra_Library::Row::swap(Row& y) {
  std::swap(impl, y.impl);
}

/*!
  Specialize <CODE>std::swap</CODE> to use the fast swap that is 
  provided as a member function instead of using the default 
  algorithm (which creates a temporary and uses assignment).
*/
INLINE void
std::swap(Parma_Polyhedra_Library::Row& x, Parma_Polyhedra_Library::Row& y) {
  x.swap(y);
}

INLINE Parma_Polyhedra_Library::Row&
Parma_Polyhedra_Library::Row::operator =(const Row& y) {
  // Copy-construct `tmp' from `y'.
  Row tmp(y);
  // Swap the implementation of `*this' with the one of `tmp'.
  swap(tmp);
  // Now `tmp' goes out of scope, so the old `*this' will be destroyed.
  return *this;
}


INLINE Parma_Polyhedra_Library::Row::Type
Parma_Polyhedra_Library::Row::type() const {
  return impl->type();
}


/*!
  Returns <CODE>true</CODE> if \p *this row represent a line or 
  an equality; <CODE>false</CODE> otherwise.
*/
INLINE bool
Parma_Polyhedra_Library::Row::is_line_or_equality() const {
  return type() == LINE_OR_EQUALITY;
}


/*!
  Returns <CODE>true</CODE> if the row represent a ray, a vertex or an 
  inequality; <CODE>false</CODE> otherwise.
*/
INLINE bool
Parma_Polyhedra_Library::Row::is_ray_or_vertex_or_inequality() const {
  return type() == RAY_OR_VERTEX_OR_INEQUALITY;
}


/*!
  Sets to \p LINE_OR_EQUALITY the type of \p *this row.
*/
INLINE void
Parma_Polyhedra_Library::Row::set_is_line_or_equality() {
  impl->set_type(LINE_OR_EQUALITY);
}


/*!
  Sets to \p RAY_OR_VERTEX_OR_INEQUALITY the type of \p *this row.
*/
INLINE void
Parma_Polyhedra_Library::Row::set_is_ray_or_vertex_or_inequality() {
  impl->set_type(RAY_OR_VERTEX_OR_INEQUALITY);
}


/*!
  Returns a reference to the element of the row indexed by \p k.
*/
INLINE Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::operator [](size_t k) {
  return (*impl)[k];
}


/*!
  Returns a constant reference to the element of the row indexed by \p k.
*/
INLINE const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::operator [](size_t k) const {
  return (*impl)[k];
}

INLINE int
Parma_Polyhedra_Library::Row::first() const {
  for (size_t i = 1, size_ = size(); i < size_; ++i)
    if ((*this)[i] != 0)
      return i-1;
  return -1;
}

INLINE int
Parma_Polyhedra_Library::Row::next(int p) const {
  assert(p >= 0 && unsigned(p) < size()-1);
  for (size_t i = p+2, size_ = size(); i < size_; ++i)
    if ((*this)[i] != 0)
      return i-1;
  return -1;
}

INLINE int
Parma_Polyhedra_Library::Row::last() const {
  for (size_t i = size()-1; i >= 1; --i)
    if ((*this)[i] != 0)
      return i-1;
  return -1;
}

INLINE int
Parma_Polyhedra_Library::Row::prev(int n) const {
  assert(n >= 0 && unsigned(n) < size()-1);
  for (size_t i = n; i >= 1; --i)
    if ((*this)[i] != 0)
      return i-1;
  return -1;
}

INLINE const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::coefficient() const {
  return (*this)[0];
}

INLINE const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Row::coefficient(size_t k) const {
  return (*this)[k+1];
}



INLINE bool
Parma_Polyhedra_Library::operator ==(const Row& x, const Row& y) {
  return compare(x, y) == 0;
}

INLINE bool
Parma_Polyhedra_Library::operator !=(const Row& x, const Row& y) {
  return compare(x, y) != 0;
}

INLINE bool
Parma_Polyhedra_Library::operator <=(const Row& x, const Row& y) {
  return compare(x, y) <= 0;
}

INLINE bool
Parma_Polyhedra_Library::operator <(const Row& x, const Row& y) {
  return compare(x, y) < 0;
}

INLINE bool
Parma_Polyhedra_Library::operator >=(const Row& x, const Row& y) {
  return compare(x, y) >= 0;
}

INLINE bool
Parma_Polyhedra_Library::operator >(const Row& x, const Row& y) {
  return compare(x, y) > 0;
}
