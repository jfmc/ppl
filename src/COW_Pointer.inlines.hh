/* COW_Pointer class implementation: inline functions.
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


#ifndef PPL_COW_Pointer_inlines_hh
#define PPL_COW_Pointer_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "COW_Pointer.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
inline
COW_Pointer<T>::COW_Pointer() {
  data = new data_struct();
  new ((T*) &(data->value)) T();
  data->n = 1;
}

template <typename T>
template <typename T1>
inline
COW_Pointer<T>::COW_Pointer(T1 x1) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5,
typename T6>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5, x6);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5,
typename T6, typename T7>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5, x6, x7);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5,
typename T6, typename T7, typename T8>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7,
                            T8 x8) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5, x6, x7, x8);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5,
typename T6, typename T7, typename T8, typename T9>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7,
                            T8 x8, T9 x9) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5, x6, x7, x8, x9);
  data->n = 1;
}

template <typename T>
template <typename T1, typename T2, typename T3, typename T4, typename T5,
typename T6, typename T7, typename T8, typename T9, typename T10>
inline
COW_Pointer<T>::COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7,
                            T8 x8, T9 x9, T10 x10) {
  data = new data_struct();
  new ((T*) &(data->value)) T(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
  data->n = 1;
}

template <typename T>
inline
COW_Pointer<T>::COW_Pointer(const COW_Pointer<T>& x) {
  data = x.data;
  ++(data->n);
}

template <typename T>
inline COW_Pointer<T>&
COW_Pointer<T>::operator=(const COW_Pointer<T>& x) {
  if (this != &x) {
    destroy();
    data = x.data;
    ++(data->n);
  }

  return *this;
}

template <typename T>
inline T*
COW_Pointer<T>::operator->() {
  detach();
  return (T*)(data->value);
}

template <typename T>
inline const T*
COW_Pointer<T>::operator->() const {
  return (const T*)(data->value);
}

template <typename T>
inline T&
COW_Pointer<T>::operator*() {
  return *(operator->());
}

template <typename T>
inline const T&
COW_Pointer<T>::operator*() const {
  return *(operator->());
}

template <typename T>
inline
COW_Pointer<T>::~COW_Pointer() {
  destroy();
}

template <typename T>
inline void
COW_Pointer<T>::detach() {
  if (data->n > 1) {
    --(data->n);
    data_struct* new_data = new data_struct();
    new_data->n = 1;
    new ((T*) &(new_data->value)) T(*((T*) (data->value)));
    data = new_data;
  }
}

template <typename T>
inline void
COW_Pointer<T>::destroy() {
  --(data->n);

  if (data->n == 0) {
    ((T*) data->value)->~T();
    delete data;
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_COW_Pointer_inlines_hh)
