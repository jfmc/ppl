/* Unlimited_Sparse_Row_Std_List_Backend class implementation: inline
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

#ifndef PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh
#define PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator::
  dangerous_iterator()
  : iterator() {
}

inline
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator::
  dangerous_iterator(iterator i)
  : iterator(i) {
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator::next(iterator i) {
  return dangerous_iterator(++i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::splice(iterator& position,This& x) {
  bool inserting_at_beginning = (position == begin());
  iterator previous = position;
  if (inserting_at_beginning)
    --previous;
  Base::splice(position,x);
  if (inserting_at_beginning)
    return begin();
  ++previous;
  return previous;
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::splice(iterator& position,This& x,
                                              iterator i) {
  bool inserting_at_beginning = (position == begin());
  iterator previous = position;
  if (inserting_at_beginning)
    --previous;
  Base::splice(position,x,i);
  if (inserting_at_beginning)
    return begin();
  ++previous;
  return previous;
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::splice(iterator& position,This& x,
                                              iterator first,iterator last) {
  bool inserting_at_beginning = (position == begin());
  iterator previous = position;
  if (inserting_at_beginning)
    --previous;
  Base::splice(position,x,first,last);
  if (inserting_at_beginning)
    return begin();
  ++previous;
  return previous;
}

inline memory_size_type
Unlimited_Sparse_Row_Std_List_Backend::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row_Std_List_Backend::OK() const {
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh)
