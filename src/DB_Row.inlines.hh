/* DB_Row class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_DB_Row_inlines_hh
#define PPL_DB_Row_inlines_hh 1

#include <cassert>
#include <algorithm>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename T>
inline void*
DB_Row_Impl_Handler<T>::Impl::operator new(const size_t fixed_size, 
					   const dimension_type capacity) {
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  return ::operator new(fixed_size + capacity*sizeof(T));
#else
  assert(capacity >= 1);
  return ::operator new(fixed_size + (capacity-1)*sizeof(T));
#endif
}

template <typename T>
inline void
DB_Row_Impl_Handler<T>::Impl::operator delete(void* p) {
  ::operator delete(p);
}

template <typename T>
inline void
DB_Row_Impl_Handler<T>::Impl::operator delete(void* p, dimension_type) {
  ::operator delete(p);
}

template <typename T> 
inline dimension_type
DB_Row_Impl_Handler<T>::Impl::max_size() {
  return size_t(-1)/sizeof(T);
}

template <typename T>
inline dimension_type
DB_Row_Impl_Handler<T>::Impl::size() const {
  return size_;
}

template <typename T>
inline void
DB_Row_Impl_Handler<T>::Impl::set_size(const dimension_type new_sz) {
  size_ = new_sz;
}

template <typename T>
inline void
DB_Row_Impl_Handler<T>::Impl::bump_size() {
  ++size_;
}

template <typename T>
inline
DB_Row_Impl_Handler<T>::Impl::Impl()
  : size_(0) {
}

template <typename T>
inline
DB_Row_Impl_Handler<T>::Impl::~Impl() {
  shrink(0);
}

template <typename T>
inline
DB_Row_Impl_Handler<T>::DB_Row_Impl_Handler()
  : impl(0) {
#if EXTRA_ROW_DEBUG
  capacity_ = 0;
#endif
}

template <typename T>
inline
DB_Row_Impl_Handler<T>::~DB_Row_Impl_Handler() {
  delete impl;
}

// template <typename T>
// inline void
// DB_Row<T>::Impl::resize_no_copy(const dimension_type new_sz) {
//   if (new_sz < size())
//     shrink(new_sz);
//   else
//     grow_no_copy(new_sz);
// }

// template <typename T>
// inline
// DB_Row<T>::Impl::Impl(const dimension_type sz)
//   : size_(0) {
//   grow_no_copy(sz);
// }

// template <typename T>
// inline
// DB_Row<T>::Impl::Impl(const Impl& y)
//   : size_(0) {
//   copy_construct(y);
// }

// template <typename T>
// inline
// DB_Row<T>::Impl::Impl(const Impl& y, const dimension_type sz)
//   : size_(0) {
//   copy_construct(y);
//   grow_no_copy(sz);
// }

// template <typename T>
// inline
// DB_Row<T>::Impl::~Impl() {
// #if CXX_SUPPORTS_FLEXIBLE_ARRAYS
//   shrink(0);
// #else
//   shrink(1);
// #endif
// }

template <typename T>
inline T&
DB_Row_Impl_Handler<T>::Impl::operator[](const dimension_type k) {
  assert(k < size());
  return vec_[k];
}

template <typename T>
inline const T&
DB_Row_Impl_Handler<T>::Impl::operator[](const dimension_type k) const {
  assert(k < size());
  return vec_[k];
}

template <typename T>
inline dimension_type
DB_Row<T>::max_size() {
  return DB_Row_Impl_Handler<T>::Impl::max_size();
}

template <typename T>
inline dimension_type
DB_Row<T>::size() const {
  return this->impl->size();
}

#if EXTRA_ROW_DEBUG
template <typename T>
inline dimension_type
DB_Row<T>::capacity() const {
  return this->capacity_;
}
#endif // EXTRA_ROW_DEBUG

template <typename T>
inline
DB_Row<T>::DB_Row()
  : DB_Row_Impl_Handler<T>() {
}

template <typename T>
inline void
DB_Row<T>::allocate(
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       const
#endif
	       dimension_type capacity) {
  DB_Row<T>& x = *this;
  assert(capacity <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity == 0)
    ++capacity;
#endif
  assert(x.impl == 0);
  x.impl = new (capacity) typename DB_Row_Impl_Handler<T>::Impl();
#if EXTRA_ROW_DEBUG
  assert(x.capacity_ == 0);
  x.capacity_ = capacity;
#endif
}

template <typename T>
inline void
DB_Row<T>::expand_within_capacity(const dimension_type new_size) {
  DB_Row<T>& x = *this;
  assert(x.impl);
#if EXTRA_ROW_DEBUG
  assert(new_size <= x.capacity_);
#endif
  x.impl->expand_within_capacity(new_size);
}

template <typename T>
inline void
DB_Row<T>::copy_construct_coefficients(const DB_Row& y) {
  DB_Row<T>& x = *this;
  assert(x.impl && y.impl);
#if EXTRA_ROW_DEBUG
  assert(y.size() <= x.capacity_);
#endif
  x.impl->copy_construct_coefficients(*(y.impl));
}

template <typename T>
inline void
DB_Row<T>::construct(const dimension_type sz, 
		     const dimension_type capacity) {
  assert(sz <= capacity && capacity <= max_size());
  allocate(capacity);
  expand_within_capacity(sz);
}

template <typename T>
inline void
DB_Row<T>::construct(const dimension_type sz) {
  construct(sz, sz);
}

template <typename T>
inline
DB_Row<T>::DB_Row(const dimension_type sz, 
		  const dimension_type capacity)
  : DB_Row_Impl_Handler<T>() {
  construct(sz, capacity);
}

template <typename T>
inline
DB_Row<T>::DB_Row(const dimension_type sz) {
  construct(sz);
}

template <typename T>
inline
DB_Row<T>::DB_Row(const DB_Row& y)
  : DB_Row_Impl_Handler<T>() {
  if (y.impl) {
    allocate(compute_capacity(y.size(), max_size()));
    copy_construct_coefficients(y);
  }
}

template <typename T>
inline
DB_Row<T>::DB_Row(const DB_Row& y, 
		  const	dimension_type capacity)
  : DB_Row_Impl_Handler<T>() {
  assert(y.impl);
  assert(y.size() <= capacity && capacity <= max_size());
  allocate(capacity);
  copy_construct_coefficients(y);
}

template <typename T>
inline
DB_Row<T>::DB_Row(const DB_Row& y, 
		  const dimension_type sz, 
		  const	dimension_type capacity)
  : DB_Row_Impl_Handler<T>() {
  assert(y.impl);
  assert(y.size() <= sz && sz <= capacity && capacity <= max_size());
  allocate(capacity);
  copy_construct_coefficients(y);
  expand_within_capacity(sz);
}

template <typename T>
inline
DB_Row<T>::~DB_Row() {
}

// template <typename T>
// inline void
// DB_Row<T>::resize_no_copy(const dimension_type new_sz) {
//   assert(impl);
// #if EXTRA_ROW_DEBUG
//   assert(new_sz <= capacity_);
// #endif
//   impl->resize_no_copy(new_sz);
// }

// template <typename T>
// inline void
// DB_Row<T>::grow_no_copy(const dimension_type new_sz) {
//   assert(impl);
// #if EXTRA_ROW_DEBUG
//   assert(new_sz <= capacity_);
// #endif
//   impl->grow_no_copy(new_sz);
// }

template <typename T>
inline void
DB_Row<T>::shrink(const dimension_type new_size) {
  DB_Row<T>& x = *this;
  assert(x.impl);
  x.impl->shrink(new_size);
}

template <typename T>
inline void
DB_Row<T>::swap(DB_Row& y) {
  DB_Row<T>& x = *this;
  std::swap(x.impl, y.impl);
#if EXTRA_ROW_DEBUG
  std::swap(x.capacity_, y.capacity_);
#endif
}

template <typename T>
inline void
DB_Row<T>::assign(DB_Row& y) {
  DB_Row<T>& x = *this;
  x.impl = y.impl;
#if EXTRA_ROW_DEBUG
  x.capacity_ = y.capacity_;
#endif
}

template <typename T>
inline DB_Row<T>&
DB_Row<T>::operator=(const DB_Row& y) {
  // Copy-construct `tmp' from `y'.
  DB_Row tmp(y);
  // Swap the implementation of `*this' with the one of `tmp'.
  swap(tmp);
  // Now `tmp' goes out of scope, so the old `*this' will be destroyed.
  return *this;
}

template <typename T>
inline T&
DB_Row<T>::operator[](const dimension_type k) {
  DB_Row<T>& x = *this;
  return (*x.impl)[k];
}

template <typename T>
inline const T&
DB_Row<T>::operator[](const dimension_type k) const {
  const DB_Row<T>& x = *this;
  return (*x.impl)[k];
}

// template <typename T>
// inline void
// DB_Row<T>::Impl::grow_no_copy(const dimension_type new_sz) {
//   assert(new_sz >= size());
// #if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
//   // vec[0] is already constructed.
//   if (size() == 0 && new_sz > 0)
//     bump_size();
// #endif
//   for (dimension_type i = size(); i < new_sz; ++i) {
//     new (&vec_[i]) T();
//     bump_size();
//   }
// }

template <typename T>
inline void
DB_Row_Impl_Handler<T>::
Impl::expand_within_capacity(const dimension_type new_size) {
  assert(size() <= new_size && new_size <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  // vec_[0] is already constructed.
  if (size() == 0 && new_size > 0)
    bump_size();
#endif
  for (dimension_type i = size(); i < new_size; ++i) {
    new (&vec_[i]) T();
    bump_size();
  }
}

template <typename T>
void
DB_Row_Impl_Handler<T>::Impl::shrink(dimension_type new_size) {
  const dimension_type old_size = size();
  assert(new_size <= old_size);
  // Since ~T() does not throw exceptions, nothing here does.
  set_size(new_size);
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  // Make sure we do not try to destroy vec_[0].
  if (new_size == 0)
    ++new_size;
#endif
  // We assume construction was done "forward".
  // We thus perform destruction "backward".
  for (dimension_type i = old_size; i-- > new_size; )
    vec_[i].~T();
}

template <typename T>
void
DB_Row_Impl_Handler<T>::Impl::copy_construct_coefficients(const Impl& y) {
  const dimension_type y_size = y.size();
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  for (dimension_type i = 0; i < y_size; ++i) {
    new (&vec_[i]) T(y.vec_[i]);
    bump_size();
  }
#else
  assert(y_size > 0);
  if (y_size > 0) {
    vec_[0] = y.vec_[0];
    bump_size();
    for (dimension_type i = 1; i < y_size; ++i) {
      new (&vec_[i]) T(y.vec_[i]);
      bump_size();
    }
  }
#endif
}

// template <typename T>
// inline void
// DB_Row<T>::Impl::shrink(const dimension_type new_sz) {
// #if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
//   assert(new_sz > 0);
// #endif
//   assert(new_sz <= size());
//   // We assume construction was done "forward".
//   // We thus perform destruction "backward".
//   for (dimension_type i = size(); i-- > new_sz; )
//     // ~T() does not throw exceptions.  So we do.
//     vec_[i].~T();
//   set_size(new_sz);
// }

// template <typename T>
// inline void
// DB_Row<T>::Impl::copy_construct(const Impl& y) {
//   dimension_type y_size = y.size();
// #if CXX_SUPPORTS_FLEXIBLE_ARRAYS
//   for (dimension_type i = 0; i < y_size; ++i) {
//     new (&vec_[i]) T(y.vec_[i]);
//     bump_size();
//   }
// #else
//   assert(y_size > 0);
//   if (y_size > 0) {
//     vec_[0] = y.vec_[0];
//     bump_size();
//     for (dimension_type i = 1; i < y_size; ++i) {
//       new (&vec_[i]) T(y.vec_[i]);
//       bump_size();
//     }
//   }
// #endif
// }

template <typename T>
typename DB_Row<T>::iterator
DB_Row<T>::begin() {
  DB_Row<T>& x = *this;
  return iterator(x.impl->vec_);
}

template <typename T>
typename DB_Row<T>::iterator
DB_Row<T>::end() {
  DB_Row<T>& x = *this;
  return iterator(x.impl->vec_ + x.impl->size_);
}

template <typename T>
typename DB_Row<T>::const_iterator
DB_Row<T>::begin() const {
  const DB_Row<T>& x = *this;
  return const_iterator(x.impl->vec_);
}

template <typename T>
typename DB_Row<T>::const_iterator
DB_Row<T>::end() const {
  const DB_Row<T>& x = *this;
  return const_iterator(x.impl->vec_ + x.impl->size_);
}

template <typename T>
inline bool
DB_Row<T>::OK(const dimension_type row_size,
	      const dimension_type
#if EXTRA_ROW_DEBUG
	      row_capacity
#endif
	      ) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif
  
  const DB_Row<T>& x = *this;

  bool is_broken = false;
#if EXTRA_ROW_DEBUG
# if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (x.capacity_ == 0) {
    cerr << "Illegal row capacity: is 0, should be at least 1"
	 << endl;
    is_broken = true;
  }
  else if (x.capacity_ == 1 && row_capacity == 0)
    // This is fine.
    ;
  else
# endif
  if (x.capacity_ != row_capacity) {
    cerr << "DB_Row capacity mismatch: is " << x.capacity_
	 << ", should be " << row_capacity << "."
	 << endl;
    is_broken = true;
  }
#endif
  if (x.size() != row_size) {
#ifndef NDEBUG
    cerr << "DB_Row size mismatch: is " << x.size()
	 << ", should be " << row_size << "."
	 << endl;
#endif
    is_broken = true;
  }
#if EXTRA_ROW_DEBUG
  if (x.capacity_ < x.size()) {
#ifndef NDEBUG
    cerr << "DB_Row is completely broken: capacity is " << x.capacity_
	 << ", size is " << x.size() << "."
	 << endl;
#endif
    is_broken = true;
  }
#endif

  for (dimension_type i = x.size(); i-- > 0; ) {
    const T& element = x[i];
    // Not OK is bad.
    // In addition, nans should never occur.
    if (!element.OK() || is_nan(element)) {
      is_broken = true;
      break;
    }
  }

  return !is_broken;
}

/*! \relates DB_Row */
template <typename T>
inline bool
operator==(const DB_Row<T>& x, const DB_Row<T>& y) {
  if (x.size() != y.size())
    return false;
  for (dimension_type i = x.size(); i-- > 0; )
    if (x[i] != y[i])
      return false;
  return true;
}

/*! \relates DB_Row */
template <typename T>
inline bool
operator!=(const DB_Row<T>& x, const DB_Row<T>& y) {
  return !(x == y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::DB_Row */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::DB_Row<T>& x,
     Parma_Polyhedra_Library::DB_Row<T>& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::DB_Row */
template <typename T>
inline void
iter_swap(typename std::vector<Parma_Polyhedra_Library::DB_Row<T> >
	  ::iterator x,
	  typename std::vector<Parma_Polyhedra_Library::DB_Row<T> >
	  ::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_DB_Row_inlines_hh)
