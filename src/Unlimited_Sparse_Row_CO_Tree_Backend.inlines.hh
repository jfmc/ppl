/* Unlimited_Sparse_Row_CO_Tree_Backend class implementation: inline
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

#ifndef PPL_Unlimited_Sparse_Row_CO_Tree_Backend_inlines_hh
#define PPL_Unlimited_Sparse_Row_CO_Tree_Backend_inlines_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row_CO_Tree_Backend.defs.hh"

namespace Parma_Polyhedra_Library {

inline memory_size_type
Unlimited_Sparse_Row_CO_Tree_Backend::external_memory_in_bytes() const {
  return tree.external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row_CO_Tree_Backend::OK() const {
  return tree.OK();
}

inline
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::iterator(CO_Tree* x)
  : itr(x) {
}

inline bool
Unlimited_Sparse_Row_CO_Tree_Backend::iterator
::operator==(const iterator& x) const {

  return itr == x.itr;
}

inline bool
Unlimited_Sparse_Row_CO_Tree_Backend::iterator
::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::iterator&
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator++() {

  itr.get_next_value();
  return *this;
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::iterator&
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator--() {

  itr.get_previous_value();
  return *this;
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::iterator::value_type&
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator*() {

  return *itr;
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::iterator::value_type*
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator->() {

  return itr.operator->();
}

inline const Unlimited_Sparse_Row_CO_Tree_Backend::iterator::value_type&
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator*() const {

  return *itr;
}

inline const Unlimited_Sparse_Row_CO_Tree_Backend::iterator::value_type*
Unlimited_Sparse_Row_CO_Tree_Backend::iterator::operator->() const {

  return itr.operator->();
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
::dangerous_iterator(CO_Tree* x)
  : iterator(x) {
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
::dangerous_iterator(const iterator& x)
  : iterator(x) {
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
::next(const iterator& i) {
  dangerous_iterator itr(i);
  ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::dangerous_iterator
::operator const_iterator() const {
  return iterator::operator const_iterator();
}


inline Unlimited_Sparse_Row_CO_Tree_Backend::iterator
::operator const_iterator() const {
  return const_iterator(itr);
}


inline
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator
::const_iterator(const CO_Tree* x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator
::const_iterator(const CO_Tree::inorder_iterator& x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator
::const_iterator(const CO_Tree::inorder_const_iterator& x)
  : itr(x) {
}

inline bool
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator
::operator==(const const_iterator& x) const {

  return itr == x.itr;
}

inline bool
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator
::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator&
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::operator++() {

  itr.get_next_value();
  return *this;
}

inline Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator&
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::operator--() {

  itr.get_previous_value();
  return *this;
}

inline const Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::value_type&
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::operator*() const {

  return *itr;
}

inline const Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::value_type*
Unlimited_Sparse_Row_CO_Tree_Backend::const_iterator::operator->() const {

  return itr.operator->();
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_CO_Tree_Backend_inlines_hh)
