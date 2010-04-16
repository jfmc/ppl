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

inline
Unlimited_Sparse_Row_Over_CO_Tree::Unlimited_Sparse_Row_Over_CO_Tree()
  : tree() {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree
::Unlimited_Sparse_Row_Over_CO_Tree(const This& x)
  : tree(x.tree) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree
::Unlimited_Sparse_Row_Over_CO_Tree(const std::vector<Coefficient> &v)
  : tree(v) {
}

inline Unlimited_Sparse_Row_Over_CO_Tree&
Unlimited_Sparse_Row_Over_CO_Tree::operator=(const This& x) {
  tree = x.tree;
  return *this;
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::swap(This& x) {
  tree.swap(x.tree);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::swap(dimension_type i, dimension_type j) {
  if (tree.empty())
    return;
  CO_Tree::inorder_iterator itr_i;
  CO_Tree::inorder_iterator itr_j;
  tree.lower_bound(itr_i, i);
  tree.lower_bound(itr_j, j);
  if (itr_i->first == i)
    if (itr_j->first == j)
      // Both elements are in the tree
      std::swap(itr_i->second, itr_j->second);
    else {
      // i is in the tree, j isn't
      Coefficient tmp;
      std::swap(itr_i->second, tmp);
      tree.erase(itr_i);
      tree.insert(j, Coefficient_zero(), itr_j);
      std::swap(itr_j->second, tmp);
    }
  else
    if (itr_j->first == j) {
      // j is in the tree, i isn't
      Coefficient tmp;
      std::swap(itr_j->second, tmp);
      tree.erase(itr_j);
      tree.insert(i, Coefficient_zero(), itr_i);
      std::swap(itr_i->second, tmp);
    } else {
      // Do nothing, elements are both unstored zeroes.
    }
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::swap(iterator i, iterator j) {
  PPL_ASSERT(!i.itr.is_before_begin());
  PPL_ASSERT(!i.itr.is_at_end());
  PPL_ASSERT(!j.itr.is_before_begin());
  PPL_ASSERT(!j.itr.is_at_end());
  std::swap(i->second, j->second);
}

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

inline Unlimited_Sparse_Row_Over_CO_Tree::unordered_iterator
Unlimited_Sparse_Row_Over_CO_Tree::unordered_begin() {
  return tree.unordered_begin();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::unordered_iterator
Unlimited_Sparse_Row_Over_CO_Tree::unordered_end() {
  return tree.unordered_end();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::unordered_const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::unordered_begin() const {
  return tree.unordered_begin();
}

inline Unlimited_Sparse_Row_Over_CO_Tree::unordered_const_iterator
Unlimited_Sparse_Row_Over_CO_Tree::unordered_end() const {
  return tree.unordered_end();
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
  PPL_ASSERT(itr.itr.is_at_end() || itr->first >= c);
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
  PPL_ASSERT(itr.itr.is_at_end() || itr->first >= c);
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
  PPL_ASSERT(itr.itr.is_at_end() || itr->first >= c);
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
  return lower_bound(c);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree
::find2_dangerous(const dimension_type c1, const dimension_type c2,
                  dangerous_iterator& itr1, dangerous_iterator& itr2) {
  if (tree.empty()) {
    itr1 = end_dangerous();
    itr2 = itr1;
    return;
  }

  itr1.itr = CO_Tree::inorder_iterator(&tree);
  while (1) {
    if (itr1.itr->first < c1) {
      if (itr1.itr->first < c2) {
        if (!itr1.itr.get_right_child_value()) {
          ++(itr1.itr);
          itr2.itr = itr1.itr;
          break;
        }
      } else {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      }
    } else {
      if (itr1.itr->first == c1 || itr1.itr->first <= c2) {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      } else {
        if (!itr1.itr.get_left_child_value()) {
          itr2.itr = itr1.itr;
          break;
        }
      }
    }
  }
  if (itr1.itr->first != c1)
    itr1 = end_dangerous();
  if (itr2.itr->first != c2)
    itr2 = end_dangerous();
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::find2(const dimension_type c1,
                                            const dimension_type c2,
                                            iterator& itr1, iterator& itr2) {
  if (tree.empty()) {
    itr1 = end();
    itr2 = itr1;
    return;
  }

  itr1.itr = CO_Tree::inorder_iterator(&tree);
  while (1) {
    if (itr1.itr->first < c1) {
      if (itr1.itr->first < c2) {
        if (!itr1.itr.get_right_child_value()) {
          ++(itr1.itr);
          itr2.itr = itr1.itr;
          break;
        }
      } else {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      }
    } else {
      if (itr1.itr->first == c1 || itr1.itr->first <= c2) {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      } else {
        if (!itr1.itr.get_left_child_value()) {
          itr2.itr = itr1.itr;
          break;
        }
      }
    }
  }
  if (itr1.itr->first != c1)
    itr1 = end();
  if (itr2.itr->first != c2)
    itr2 = end();
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::find2(const dimension_type c1,
                                         const dimension_type c2,
                                         const_iterator& itr1,
                                         const_iterator& itr2) const {
  if (tree.empty()) {
    itr1 = end();
    itr2 = itr1;
    return;
  }

  itr1.itr = CO_Tree::inorder_const_iterator(&tree);
  while (1) {
    if (itr1.itr->first < c1) {
      if (itr1.itr->first < c2) {
        if (!itr1.itr.get_right_child_value()) {
          ++(itr1.itr);
          itr2.itr = itr1.itr;
          break;
        }
      } else {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      }
    } else {
      if (itr1.itr->first == c1 || itr1.itr->first <= c2) {
        itr2.itr = itr1.itr;
        tree.lower_bound(itr1.itr, c1);
        tree.lower_bound(itr2.itr, c2);
        break;
      } else {
        if (!itr1.itr.get_left_child_value()) {
          itr2.itr = itr1.itr;
          break;
        }
      }
    }
  }
  if (itr1.itr->first != c1)
    itr1 = end();
  if (itr2.itr->first != c2)
    itr2 = end();
}

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
  while (!first.itr.is_at_end() && first->first <= j) {
    tree.erase(first.itr);
    first = lower_bound_dangerous(i);
  }
  return first;
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::reset(dimension_type i) {
  tree.erase(i);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::reset(dimension_type i, dimension_type j) {
  dangerous_iterator itr = lower_bound_dangerous(i);

  while (!itr.itr.is_at_end() && itr->first < j) {
    reset(itr);
    itr = lower_bound_dangerous(i);
  }
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::reset_after(dimension_type i) {
  dangerous_iterator itr = lower_bound_dangerous(i);

  while (!itr.itr.is_at_end()) {
    reset(itr);
    itr = lower_bound_dangerous(i);
  }
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree
::delete_element_and_shift(dimension_type i) {
  reset(i);
  for (iterator itr = lower_bound(i); !itr.itr.is_at_end(); ++itr)
    --(itr.itr->first);
  PPL_ASSERT(OK());
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::add_zeroes_and_shift(dimension_type n,
                                                        dimension_type i) {
  CO_Tree::inorder_iterator itr = tree.end();
  itr.get_previous_value();
  for ( ; !itr.is_before_begin() && itr->first >= i; itr.get_previous_value())
    itr->first += n;
  PPL_ASSERT(OK());
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::assign(dimension_type i,
                                          const Coefficient& x) {
  if (tree.empty())
    assign_if_nonzero(i, x);
  else {
    CO_Tree::inorder_iterator itr(&tree);
    tree.lower_bound(itr, i);
    if (itr->first == i)
      itr->second = x;
    else
      if (x != 0)
        tree.insert(i, x, itr);
  }
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::assign_if_nonzero(dimension_type i,
                                                     const Coefficient& x) {
  if (x != 0)
    find_create(i, x);
}

inline Coefficient&
Unlimited_Sparse_Row_Over_CO_Tree::operator[](dimension_type i) {
  CO_Tree::inorder_iterator itr(&tree);
  if (tree.empty())
    tree.insert(i, Coefficient_zero(), itr);
  else {
    tree.lower_bound(itr, i);
    if (itr->first != i)
      tree.insert(i, Coefficient_zero(), itr);
  }
  return itr->second;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i,
                                               const Coefficient& x) {
  iterator itr(&tree);
  find_create(i, x, itr.itr);
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree
::find_create(const std::pair<dimension_type, Coefficient>& x) {
  return find_create(x.first, x.second);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i) {
  iterator itr(&tree);
  find_create(i, itr.itr);
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i,
                                               const Coefficient& x,
                                               iterator itr) {
  // TODO: consider a faster implementation.
  (void) itr;
  return find_create(i, x);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree
::find_create(const std::pair<dimension_type, Coefficient>& x, iterator itr) {
  return find_create(x.first, x.second, itr);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree
::find_create(const dimension_type i, const Coefficient& x,
              CO_Tree::inorder_iterator& itr) {
  if (tree.empty()) {
    tree.insert(i, x, itr);
  } else {
    PPL_ASSERT(!itr.is_at_end());

    if (itr->first <= i)
      while (itr.has_parent() && itr->first < i)
        itr.get_parent();
    else
      while (itr.has_parent() && itr->first > i)
        itr.get_parent();

    tree.lower_bound(itr, i);

    if (itr->first == i)
      itr->second = x;
    else
      tree.insert(i, x, itr);
  }
  PPL_ASSERT(!itr.is_at_end());
  PPL_ASSERT(itr->first == i);
  PPL_ASSERT(itr->second == x);
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree
::find_create(const dimension_type i, CO_Tree::inorder_iterator& itr) {
  if (tree.empty()) {
    tree.insert(i, Coefficient_zero(), itr);
  } else {
    PPL_ASSERT(!itr.is_at_end());

    if (itr->first <= i)
      while (itr.has_parent() && itr->first < i)
        itr.get_parent();
    else
      while (itr.has_parent() && itr->first > i)
        itr.get_parent();

    tree.lower_bound(itr, i);

    if (itr->first != i)
      tree.insert(i, Coefficient_zero(), itr);
  }
  PPL_ASSERT(!itr.is_at_end());
  PPL_ASSERT(itr->first == i);
}

inline Unlimited_Sparse_Row_Over_CO_Tree::iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i,
                                               iterator itr) {
  find_create(i, itr.itr);
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i,
                                               dangerous_iterator itr) {
  find_create(i, itr.itr);
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree::find_create(const dimension_type i,
                                               const Coefficient& x,
                                               dangerous_iterator itr) {
  find_create(i, x, itr.itr);
  return itr;
}

inline Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
Unlimited_Sparse_Row_Over_CO_Tree
::find_create(const std::pair<dimension_type, Coefficient>& x,
              dangerous_iterator itr) {
  find_create(x.first, x.second, itr);
  return itr;
}

inline const Coefficient&
Unlimited_Sparse_Row_Over_CO_Tree::operator[](const dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Unlimited_Sparse_Row_Over_CO_Tree::get(dimension_type i) const {
  if (tree.empty())
    return Coefficient_zero();
  CO_Tree::inorder_const_iterator itr(&tree);
  tree.lower_bound(itr, i);
  if (itr->first == i)
    return itr->second;
  else
    return Coefficient_zero();
}

inline void
Unlimited_Sparse_Row_Over_CO_Tree::get2(const dimension_type c1,
                                        const dimension_type c2,
                                        const Coefficient*& p1,
                                        const Coefficient*& p2) const {
  // TODO: consider a faster impementation.
  p1 = &(get(c1));
  p2 = &(get(c2));
}


inline
Unlimited_Sparse_Row_Over_CO_Tree::iterator::iterator(CO_Tree* x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::iterator
::iterator(const CO_Tree::inorder_iterator& x)
  : itr(x) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::iterator::iterator(const iterator& x)
  : itr(x.itr) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::iterator&
Unlimited_Sparse_Row_Over_CO_Tree::iterator
::operator=(const iterator& x) {
  itr = x.itr;
  return *this;
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

inline
Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::dangerous_iterator(const CO_Tree::inorder_iterator& x)
  : iterator(x) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::dangerous_iterator(const dangerous_iterator& x)
  : iterator(x.itr) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator&
Unlimited_Sparse_Row_Over_CO_Tree::dangerous_iterator
::operator=(const dangerous_iterator& x) {
  itr = x.itr;
  return *this;
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

inline
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::const_iterator(const const_iterator& x)
  : itr(x.itr) {
}

inline
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator&
Unlimited_Sparse_Row_Over_CO_Tree::const_iterator
::operator=(const const_iterator& x) {
  itr = x.itr;
  return *this;
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
