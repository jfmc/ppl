/* Swapping_Vector class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Swapping_Vector_inlines_hh
#define PPL_Swapping_Vector_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Swapping_Vector.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
inline
Swapping_Vector<T>::Swapping_Vector()
  : impl() {
}

template <typename T>
inline
Swapping_Vector<T>::Swapping_Vector(dimension_type i)
  : impl() {
  // NOTE: This is not the same as constructing impl as `impl(i)', because
  // this implementation calls compute_capacity().
  resize(i);
}

template <typename T>
inline
Swapping_Vector<T>::Swapping_Vector(dimension_type new_size, const T& x)
  : impl() {
  resize(new_size, x);
}

template <typename T>
inline void
Swapping_Vector<T>::clear() {
  impl.clear();
}

template <typename T>
inline void
Swapping_Vector<T>::reserve(dimension_type new_capacity) {
  if (impl.capacity() < new_capacity) {
    // Reallocation will take place.
    std::vector<T> new_impl;

    new_impl.reserve(compute_capacity(new_capacity, max_num_rows()));
    new_impl.resize(impl.size());

    // Steal the old rows.
    for (dimension_type i = impl.size(); i-- > 0; )
      std::swap(new_impl[i], impl[i]);

    // Put the new vector into place.
    std::swap(impl, new_impl);
  }
}

template <typename T>
inline void
Swapping_Vector<T>::resize(dimension_type new_size) {
  reserve(new_size);
  impl.resize(new_size);
}

template <typename T>
inline void
Swapping_Vector<T>::resize(dimension_type new_size, const T& x) {
  reserve(new_size);
  impl.resize(new_size, x);
}

template <typename T>
inline dimension_type
Swapping_Vector<T>::size() const {
  return impl.size();
}

template <typename T>
inline dimension_type
Swapping_Vector<T>::capacity() const {
  return impl.capacity();
}

template <typename T>
inline bool
Swapping_Vector<T>::empty() const {
  return impl.empty();
}

template <typename T>
inline void
Swapping_Vector<T>::swap(Swapping_Vector& v) {
  std::swap(impl, v.impl);
}

template <typename T>
inline T&
Swapping_Vector<T>::operator[](dimension_type i) {
  return impl[i];
}

template <typename T>
inline const T&
Swapping_Vector<T>::operator[](dimension_type i) const {
  return impl[i];
}

template <typename T>
inline T&
Swapping_Vector<T>::back() {
  return impl.back();
}

template <typename T>
inline const T&
Swapping_Vector<T>::back() const {
  return impl.back();
}

template <typename T>
inline void
Swapping_Vector<T>::push_back(const T& x) {
  return impl.push_back(x);
}

template <typename T>
inline memory_size_type
Swapping_Vector<T>::external_memory_in_bytes() const {
  // Estimate the size of vector.
  memory_size_type n = impl.capacity() * sizeof(T);
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    n += i->external_memory_in_bytes();
  return n;
}

template <typename T>
inline typename Swapping_Vector<T>::iterator
Swapping_Vector<T>::begin() {
  return impl.begin();
}

template <typename T>
inline typename Swapping_Vector<T>::iterator
Swapping_Vector<T>::end() {
  return impl.end();
}

template <typename T>
inline typename Swapping_Vector<T>::const_iterator
Swapping_Vector<T>::begin() const {
  return impl.begin();
}

template <typename T>
inline typename Swapping_Vector<T>::const_iterator
Swapping_Vector<T>::end() const {
  return impl.end();
}

template <typename T>
inline typename Swapping_Vector<T>::iterator
Swapping_Vector<T>::erase(iterator itr) {
  return impl.erase(itr);
}

template <typename T>
inline typename Swapping_Vector<T>::iterator
Swapping_Vector<T>::erase(iterator first, iterator last) {
  return impl.erase(first, last);
}

template <typename T>
inline dimension_type
Swapping_Vector<T>::max_num_rows() {
  return impl.max_size();
}

} // namespace Parma_Polyhedra_Library


namespace std {

template <typename T>
inline void
swap(Parma_Polyhedra_Library::Swapping_Vector<T>& vec1,
     Parma_Polyhedra_Library::Swapping_Vector<T>& vec2) {
  vec1.swap(vec2);
}

} // namespace std


#endif // !defined(PPL_Swapping_Vector_inlines_hh)
