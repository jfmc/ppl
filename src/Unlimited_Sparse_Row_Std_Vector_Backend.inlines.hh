/* Unlimited_Sparse_Row_Std_Vector_Backend class implementation: inline
   functions.
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

#ifndef PPL_Unlimited_Sparse_Row_Std_Vector_Backend_inlines_hh
#define PPL_Unlimited_Sparse_Row_Std_Vector_Backend_inlines_hh 1

// FIXME: remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row_Std_Vector_Backend.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row_Std_Vector_Backend::dangerous_iterator::
  dangerous_iterator()
  : iterator() {
}

inline
Unlimited_Sparse_Row_Std_Vector_Backend::dangerous_iterator::
  dangerous_iterator(iterator i)
  : iterator(i) {
}

inline Unlimited_Sparse_Row_Std_Vector_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_Vector_Backend::dangerous_iterator::next(iterator i) {
  return dangerous_iterator(++i);
}

inline Unlimited_Sparse_Row_Std_Vector_Backend::iterator
Unlimited_Sparse_Row_Std_Vector_Backend::splice(iterator& position,This& x) {
  PPL_ASSERT(this != &x);
  dimension_type i = position - begin();
  dimension_type n = x.size();
  insert(position,x.begin(),x.end());
  x.clear();
  position = begin() + (i+n);
  return begin() + i;
}

inline Unlimited_Sparse_Row_Std_Vector_Backend::iterator
Unlimited_Sparse_Row_Std_Vector_Backend::splice(iterator& position,This& x,
                                                iterator i) {
  return splice(position,x,i,i+1);
}

inline Unlimited_Sparse_Row_Std_Vector_Backend::iterator
Unlimited_Sparse_Row_Std_Vector_Backend::splice(iterator& position,This& x,
                                                iterator first,
                                                iterator last) {
  PPL_ASSERT(last - first >= 0);
  dimension_type i,n,m;
  if (this == &x) {
    if (last - position <= 0) {
      i = first - begin();
      n = last - first;
      m = position - last;
      // first <= last <= position
      //   |       |         |
      //   +-- n --+ -- m ---+
      swap_vector_chunks(*this,i,n,m);
      // position still points to the correct element.
      return begin() + (i+m);
    } else {
      PPL_ASSERT(position - first <= 0);
      i = position - begin();
      n = first - position;
      m = last - first;
      // position <= first <= last
      //    |          |        |
      //    +--- n ----+ -- m --+
      swap_vector_chunks(*this,i,n,m);
      position = begin() + (i+m);
      return begin() + i;
    }
  } else {
    i = first - begin();
    n = last - first;
    insert(position,first,last);
    x.erase(first,last);
    position = begin() + (i+n);
    return begin() + i;
  }
}

inline memory_size_type
Unlimited_Sparse_Row_Std_Vector_Backend::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row_Std_Vector_Backend::OK() const {
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Std_Vector_Backend_inlines_hh)
