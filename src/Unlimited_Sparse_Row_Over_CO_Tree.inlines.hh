/* Unlimited_Sparse_Row_Over_CO_Tree class implementation: inline
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

#ifndef PPL_Unlimited_Sparse_Row_Over_CO_Tree_inlines_hh
#define PPL_Unlimited_Sparse_Row_Over_CO_Tree_inlines_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row_Over_CO_Tree.defs.hh"

namespace Parma_Polyhedra_Library {

inline memory_size_type
Unlimited_Sparse_Row_Over_CO_Tree::external_memory_in_bytes() const {
  return tree.external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row_Over_CO_Tree::OK() const {
  return tree.OK();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::begin_dangerous() {
  dangerous_iterator itr(tree.before_begin());
  ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::end_dangerous() {
  return dangerous_iterator(tree.end());
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::begin() {
  iterator itr(tree.before_begin());
  ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::end() {
  return iterator(tree.end());
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::begin() const {
  const_iterator itr(tree.before_begin());
  ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::end() const {
  return const_iterator(tree.end());
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_dangerous(const dimension_type c) {
  if (tree.empty())
    return end_dangerous();
  dangerous_iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first == c)
    return itr;
  else
    return end_dangerous();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree
::lower_bound_dangerous(const dimension_type c) {
  if (tree.empty())
    return end_dangerous();
  dangerous_iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first < c)
    ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find(const dimension_type c) {
  if (tree.empty())
    return end();
  iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first == c)
    return itr;
  else
    return end();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::lower_bound(const dimension_type c) {
  if (tree.empty())
    return end();
  iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first < c)
    ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find(const dimension_type c) const {
  if (tree.empty())
    return end();
  const_iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first == c)
    return itr;
  else
    return end();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree
::lower_bound(const dimension_type c) const {
  if (tree.empty())
    return end();
  const_iterator itr(&tree);
  tree.lower_bound(itr.itr, c);
  if ((itr.itr)->first < c)
    ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_dangerous(const dimension_type c,
                                                     dangerous_iterator itr) {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return find_dangerous(c);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree
::lower_bound_dangerous(const dimension_type c, dangerous_iterator itr) {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return lower_bound_dangerous(c);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find(const dimension_type c,
                                           iterator itr) {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return find(c);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::lower_bound(const dimension_type c,
                                                  iterator itr) {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return lower_bound(c);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find(const dimension_type c,
                                           const_iterator itr) const {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return find(c);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::lower_bound(const dimension_type c,
                                                  const_iterator itr) const {
  PPL_ASSERT(!itr.itr.is_at_end());
  PPL_ASSERT(itr->first <= c);
#ifdef NDEBUG
  (void) itr;
#endif
  // TODO: consider a faster implementation.
  return lower_bound(c);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree
::find2_dangerous(const dimension_type c1, const dimension_type c2,
                  dangerous_iterator& itr1, dangerous_iterator& itr2) {
  // TODO: consider a faster implementation.
  itr1 = find_dangerous(c1);
  itr2 = find_dangerous(c2);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::find2(const dimension_type c1,
                                            const dimension_type c2,
                                            iterator& itr1, iterator& itr2) {
  // TODO: consider a faster implementation.
  itr1 = find(c1);
  itr2 = find(c2);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::find2(const dimension_type c1,
                                            const dimension_type c2,
                                            const_iterator& itr1,
                                            const_iterator& itr2) const {
  // TODO: consider a faster implementation.
  itr1 = find(c1);
  itr2 = find(c2);
}

/*
//! Inserts x before pos and returns an iterator to the inserted element.
//! This operation invalidates all C::dangerous_iterator objects equal to
//! pos.
inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::insert(dangerous_iterator pos,
                                          const value_type& x) {
  (void) pos;
  dangerous_iterator itr;
  tree.insert(x.first, x.second, itr.itr);
  return itr;
}


//! Inserts the pair (i, x) before pos and returns an iterator to the
//! inserted element.
//! This operation invalidates all dangerous_iterator objects equal to pos.
inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::insert(dangerous_iterator pos,
                                             dimension_type i,
                                             const Coefficient& x) {
  (void) pos;
  dangerous_iterator itr;
  tree.insert(i, x, itr.itr);
  return itr;
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::push_back(const value_type& x) {
  tree.insert(x.first, x.second);
}
*/

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::reset(dangerous_iterator pos) {
  const dimension_type i = pos->first;
  tree.erase(pos.itr);
  return lower_bound_dangerous(i);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::reset(dangerous_iterator first,
                                         dangerous_iterator last) {
  if (first == last)
    return first;
  --last;
  const dimension_type i = first->first;
  const dimension_type j = last->first;
  PPL_ASSERT(i <= j);
  while (first->first <= j) {
    tree.erase(first.itr);
    first = lower_bound_dangerous(i);
  }
  return first;
}


inline
Unlimited_Sparse_Row_Over_CO_Tree::iterator::iterator(CO_Tree* x)
  : itr(x) {
}

inline bool
Unlimited_Sparse_Row_Over_CO_Tree::iterator
::operator==(const iterator& x) const {

  return itr == x.itr;
}

inline bool
Unlimited_Sparse_Row_Over_CO_Tree::iterator
::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator&
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator++() {

  itr.get_next_value();
  return *this;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator&
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator--() {

  itr.get_previous_value();
  return *this;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator::value_type&
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator*() {

  return *itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator::value_type*
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator->() {

  return itr.operator->();
}

inline const Unlimited_Sparse_Row_Over_CO_Tree::iterator::value_type&
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator*() const {

  return *itr;
}

inline const Unlimited_Sparse_Row_Over_CO_Tree::iterator::value_type*
Unlimited_Sparse_Row_Over_CO_Tree::iterator::operator->() const {

  return itr.operator->();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::dangerous_iterator(CO_Tree* x)
  : iterator(x) {
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::dangerous_iterator(const iterator& x)
  : iterator(x) {
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::next(const iterator& i) {
  dangerous_iterator itr(i);
  ++itr;
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::operator const_iterator() const {
  return iterator::operator const_iterator();
}


inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
::operator const_iterator() const {
  return const_iterator(itr);
}


inline
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::const_iterator(const CO_Tree* x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::const_iterator(const CO_Tree::inorder_iterator& x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::const_iterator(const CO_Tree::inorder_const_iterator& x)
  : itr(x) {
}

inline bool
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::operator==(const const_iterator& x) const {

  return itr == x.itr;
}

inline bool
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator&
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::operator++() {

  itr.get_next_value();
  return *this;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::const_iterator&
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::operator--() {

  itr.get_previous_value();
  return *this;
}

inline const Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::value_type&
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::operator*() const {

  return *itr;
}

inline const Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::value_type*
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator::operator->() const {

  return itr.operator->();
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Over_CO_Tree_inlines_hh)
