/* COW_Pointer class declaration.
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


#ifndef PPL_COW_Pointer_defs_hh
#define PPL_COW_Pointer_defs_hh 1

#include "COW_Pointer.types.hh"

namespace Parma_Polyhedra_Library {

template <typename T>

class COW_Pointer {
public:

  COW_Pointer();

  template <typename T1>
  COW_Pointer(T1 x1);

  template <typename T1, typename T2>
  COW_Pointer(T1 x1, T2 x2);

  template <typename T1, typename T2, typename T3>
  COW_Pointer(T1 x1, T2 x2, T3 x3);

  template <typename T1, typename T2, typename T3, typename T4>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4);

  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5);

  template <typename T1, typename T2, typename T3, typename T4, typename T5,
  typename T6>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6);

  template <typename T1, typename T2, typename T3, typename T4, typename T5,
  typename T6, typename T7>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7);

  template <typename T1, typename T2, typename T3, typename T4, typename T5,
  typename T6, typename T7, typename T8>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7, T8 x8);

  template <typename T1, typename T2, typename T3, typename T4, typename T5,
  typename T6, typename T7, typename T8, typename T9>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7, T8 x8, T9 x9);

  template <typename T1, typename T2, typename T3, typename T4, typename T5,
  typename T6, typename T7, typename T8, typename T9, typename T10>
  COW_Pointer(T1 x1, T2 x2, T3 x3, T4 x4, T5 x5, T6 x6, T7 x7, T8 x8, T9 x9,
              T10 x10);

  COW_Pointer(const COW_Pointer& x);

  COW_Pointer& operator=(const COW_Pointer& x);

  T* operator->();
  const T* operator->() const;
  
  T& operator*();
  const T& operator*() const;

  ~COW_Pointer();

private:

  void detach();
  void destroy();

  struct data_struct {
    char value[sizeof(T)];
    int n;
  };

  data_struct* data;
};

} // namespace Parma_Polyhedra_Library

#include "COW_Pointer.inlines.hh"

#endif // !defined(PPL_COW_Pointer_defs_hh)
