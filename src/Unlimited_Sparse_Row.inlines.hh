/* Unlimited_Sparse_Row class implementation: inline
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

#ifndef PPL_Unlimited_Sparse_Row_inlines_hh
#define PPL_Unlimited_Sparse_Row_inlines_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row.defs.hh"

namespace Parma_Polyhedra_Library {

namespace {

  class Unlimited_Sparse_Row__bisect_helper_functor {
  public:
    Unlimited_Sparse_Row__bisect_helper_functor(dimension_type i1)
      : i(i1) {
    }

    inline int operator()(dimension_type index,
                          const Coefficient& /* data */) const {
      if (index == i)
        return 0;
      else
        if (i < index)
          return -1;
        else
          return 1;
    }

  private:
    dimension_type i;
  };
}

inline
Unlimited_Sparse_Row::Unlimited_Sparse_Row()
  : tree() {
}

inline
Unlimited_Sparse_Row
::Unlimited_Sparse_Row(const This& x)
  : tree(x.tree) {
}

inline
Unlimited_Sparse_Row
::Unlimited_Sparse_Row(const std::vector<Coefficient> &v)
  : tree(v) {
}

inline Unlimited_Sparse_Row&
Unlimited_Sparse_Row::operator=(const This& x) {
  tree = x.tree;
  return *this;
}

inline void
Unlimited_Sparse_Row::swap(This& x) {
  tree.swap(x.tree);
}

inline void
Unlimited_Sparse_Row::swap(dimension_type i, dimension_type j) {
  if (tree.empty())
    return;

  iterator first = begin();
  iterator last = end();
  --last;

  iterator itr_i = tree.bisect_in(first, last,
                                  Unlimited_Sparse_Row__bisect_helper_functor(i));
  iterator itr_j = tree.bisect_in(first, last,
                                  Unlimited_Sparse_Row__bisect_helper_functor(j));
  if (itr_i->first == i)
    if (itr_j->first == j)
      // Both elements are in the tree
      std::swap(itr_i->second, itr_j->second);
    else {
      // i is in the tree, j isn't
      Coefficient tmp;
      std::swap(itr_i->second, tmp);
      tree.erase(itr_i);
      itr_j = tree.insert(itr_j, j, Coefficient_zero());
      std::swap(itr_j->second, tmp);
    }
  else
    if (itr_j->first == j) {
      // j is in the tree, i isn't
      Coefficient tmp;
      std::swap(itr_j->second, tmp);
      tree.erase(itr_j);
      itr_i = tree.insert(itr_i, i, Coefficient_zero());
      std::swap(itr_i->second, tmp);
    } else {
      // Do nothing, elements are both unstored zeroes.
    }
}

inline void
Unlimited_Sparse_Row::swap(iterator i, iterator j) {
  PPL_ASSERT(i != tree.before_begin());
  PPL_ASSERT(i != end());
  PPL_ASSERT(j != tree.before_begin());
  PPL_ASSERT(j != end());
  std::swap(i->second, j->second);
}

inline memory_size_type
Unlimited_Sparse_Row::external_memory_in_bytes() const {
  return tree.external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row::OK() const {
  return tree.OK();
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::begin() {
  return tree.begin();
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::end() {
  return tree.end();
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::begin() const {
  return tree.begin();
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::end() const {
  return tree.end();
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(dimension_type i,
                                  const Coefficient& x) {
  return tree.insert(i, x);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(dimension_type i) {
  return tree.insert(i);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(iterator itr, dimension_type i,
                                  const Coefficient& x) {
  return tree.insert(itr, i, x);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(iterator itr, dimension_type i) {
  return tree.insert(itr, i);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find(dimension_type i) {

  if (tree.empty())
    return end();

  iterator first = begin();
  iterator last = end();
  --last;

  iterator itr = tree.bisect_in(first, last,
                                Unlimited_Sparse_Row__bisect_helper_functor(i));

  if (itr->first != i)
    return end();

  return itr;
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::find(dimension_type i) const {

  if (tree.empty())
    return end();

  const_iterator first = begin();
  const_iterator last = end();
  --last;

  const_iterator itr = tree.bisect_in(first, last,
                                      Unlimited_Sparse_Row__bisect_helper_functor(i));

  if (itr->first != i)
    return end();

  return itr;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::lower_bound(dimension_type i) {
  if (tree.empty())
    return end();

  iterator first = begin();
  iterator last = end();
  --last;

  iterator itr = tree.bisect_in(first, last,
                                Unlimited_Sparse_Row__bisect_helper_functor(i));

  if (itr->first < i)
    ++itr;
  PPL_ASSERT(itr == end() || itr->first >= i);
  return itr;
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::lower_bound(dimension_type i) const {
  if (tree.empty())
    return end();
  const_iterator first = begin();
  const_iterator last = end();
  --last;

  const_iterator itr = tree.bisect_in(first, last,
                                      Unlimited_Sparse_Row__bisect_helper_functor(i));
  if (itr->first < i)
    ++itr;
  PPL_ASSERT(itr == end() || itr->first >= i);
  return itr;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find(iterator hint, dimension_type i) {
  iterator itr = tree.bisect_near(hint,
                                  Unlimited_Sparse_Row__bisect_helper_functor(i));
  if (itr->first != i)
    itr = end();

  return itr;
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::find(const_iterator hint, dimension_type i) const {
  const_iterator itr = tree.bisect_near(hint,
                                        Unlimited_Sparse_Row__bisect_helper_functor(i));
  if (itr->first != i)
    itr = end();

  return itr;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::lower_bound(iterator hint, dimension_type i) {
  PPL_ASSERT(hint != end());

  iterator itr = tree.bisect_near(hint,
                                  Unlimited_Sparse_Row__bisect_helper_functor(i));

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::lower_bound(const_iterator hint, dimension_type i) const {
  PPL_ASSERT(hint != end());
  const_iterator itr = tree.bisect_near(hint,
                                        Unlimited_Sparse_Row__bisect_helper_functor(i));

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::reset(iterator pos) {
  const dimension_type i = pos->first;
  tree.erase(pos);
  return lower_bound(i);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::reset(iterator first, iterator last) {
  if (first == last)
    return first;
  --last;
  const dimension_type i = first->first;
  const dimension_type j = last->first;
  PPL_ASSERT(i <= j);
  while (first != end() && first->first <= j) {
    tree.erase(first);
    first = lower_bound(i);
  }
  return first;
}

inline void
Unlimited_Sparse_Row::reset(dimension_type i) {
  tree.erase(i);
}

inline void
Unlimited_Sparse_Row::reset(dimension_type i, dimension_type j) {
  iterator itr = lower_bound(i);

  while (itr != end() && itr->first < j) {
    reset(itr);
    itr = lower_bound(i);
  }
}

inline void
Unlimited_Sparse_Row::reset_after(dimension_type i) {
  iterator itr = lower_bound(i);

  while (itr != end()) {
    reset(itr);
    itr = lower_bound(i);
  }
}

inline void
Unlimited_Sparse_Row
::delete_element_and_shift(dimension_type i) {
  tree.erase_element_and_shift_left(i);
}

inline void
Unlimited_Sparse_Row::add_zeroes_and_shift(dimension_type n,
                                           dimension_type i) {
  tree.increase_keys_after(i, n);
}

inline void
Unlimited_Sparse_Row::assign(dimension_type i, const Coefficient& x) {
  if (tree.empty())
    assign_if_nonzero(i, x);
  else {
    iterator last = end();
    --last;

    iterator itr = tree.bisect_in(tree.begin(), last,
                                  Unlimited_Sparse_Row__bisect_helper_functor(i));
    if (itr->first == i)
      itr->second = x;
    else
      if (x != 0)
        tree.insert(itr, i, x);
  }
}

inline void
Unlimited_Sparse_Row::assign_if_nonzero(dimension_type i,
                                        const Coefficient& x) {
  if (x != 0)
    find_create(i, x);
}

inline Coefficient&
Unlimited_Sparse_Row::operator[](dimension_type i) {
  iterator itr = find_create(i);
  return itr->second;
}

inline const Coefficient&
Unlimited_Sparse_Row::operator[](dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Unlimited_Sparse_Row::get(dimension_type i) const {
  if (tree.empty())
    return Coefficient_zero();
  const_iterator itr = find(i);
  if (itr != end())
    return itr->second;
  else
    return Coefficient_zero();
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_inlines_hh)
