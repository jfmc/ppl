/* CO_Tree class implementation: inline functions.
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

#ifndef PPL_CO_Tree_inlines_hh
#define PPL_CO_Tree_inlines_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "CO_Tree.defs.hh"

#include <iostream>

namespace Parma_Polyhedra_Library {

inline
CO_Tree::CO_Tree() {

  init(0);

  PPL_ASSERT(OK());
}

inline
CO_Tree::CO_Tree(const CO_Tree& x) {

  PPL_ASSERT(x.OK());

  init(x.reserved_size);

  copy_data_from(x);
}

inline CO_Tree&
CO_Tree::operator=(const CO_Tree& x) {

  if (this != &x) {

    destroy();
    init(x.reserved_size);

    copy_data_from(x);
  }

  return *this;
}

inline
CO_Tree::~CO_Tree() {

  PPL_ASSERT(structure_OK());

  destroy();
}

inline CO_Tree::iterator
CO_Tree::insert(dimension_type key1, const data_type& data1) {
  if (empty()) {
    insert_in_empty_tree(key1, data1);
    tree_iterator itr(*this);
    PPL_ASSERT(itr->first != unused_index);
    return iterator(itr);
  } else {
    tree_iterator itr(*this);
    go_down_searching_key(itr, key1);
    if (itr->first != key1)
      insert_precise(key1, data1, itr);
    else
      itr->second = data1;
    return iterator(itr);
  }
}

inline CO_Tree::iterator
CO_Tree::insert(dimension_type key1) {
  if (empty())
    return insert(key1, Coefficient_zero());
  else {
    tree_iterator itr(*this);
    go_down_searching_key(itr, key1);
    if (itr->first != key1)
      insert_precise(key1, Coefficient_zero(), itr);
    return iterator(itr);
  }
}

namespace {

  class CO_Tree__bisect_helper_functor {
  public:
    CO_Tree__bisect_helper_functor(dimension_type i1)
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

inline CO_Tree::iterator
CO_Tree::insert(iterator itr, dimension_type key1, const data_type& data1) {
  PPL_ASSERT(key1 != unused_index);
  if (!empty()) {
    if (itr == end() || itr == before_begin())
      return insert(key1, data1);
    else {
      iterator candidate1 = bisect_near(itr,
                                        CO_Tree__bisect_helper_functor(key1));
      if (key1 == candidate1->first) {
        candidate1->second = data1;
        return candidate1;
      }
      iterator candidate2(candidate1);
      if (key1 < candidate1->first)
        --candidate2;
      else
        ++candidate2;
      tree_iterator candidate1_node(candidate1, *this);
      if (candidate2 == before_begin() || candidate2 == end()) {
        // Use candidate1
        insert_precise(key1, data1, candidate1_node);
        return iterator(candidate1_node);
      }
      else {
        tree_iterator candidate2_node(candidate2, *this);
        // Adjacent nodes in an in-order visit of a tree always have different
        // heights. This fact can be easily proven by induction on the tree's
        // height, using the definition of the in-order layout.
        PPL_ASSERT(candidate1_node.get_offset() != candidate2_node.get_offset());
        if (candidate1_node.get_offset() < candidate2_node.get_offset()) {
          PPL_ASSERT(candidate1_node.depth() > candidate2_node.depth());
          // candidate1_node is deeper in the tree than candidate2_node, so
          // use candidate1_node.
          insert_precise(key1, data1, candidate1_node);
          return iterator(candidate1_node);
        } else {
          PPL_ASSERT(candidate1_node.depth() < candidate2_node.depth());
          // candidate2_node is deeper in the tree than candidate1_node, so
          // use candidate2_node.
          insert_precise(key1, data1, candidate2_node);
          return iterator(candidate2_node);
        }
      }
    }
  } else {
    insert_in_empty_tree(key1, data1);
    return iterator(*this);
  }
}

inline CO_Tree::iterator
CO_Tree::insert(iterator itr, dimension_type key1) {
  PPL_ASSERT(key1 != unused_index);
  if (!empty()) {
    if (itr == end() || itr == before_begin())
      return insert(key1);
    else {
      iterator candidate1 = bisect_near(itr,
                                        CO_Tree__bisect_helper_functor(key1));
      if (key1 == candidate1->first)
        return candidate1;
      iterator candidate2(candidate1);
      if (key1 < candidate1->first)
        --candidate2;
      else
        ++candidate2;
      tree_iterator candidate1_node(candidate1, *this);
      if (candidate2 == before_begin() || candidate2 == end()) {
        // Use candidate1
        insert_precise(key1, Coefficient_zero(), candidate1_node);
        return iterator(candidate1_node);
      }
      else {
        tree_iterator candidate2_node(candidate2, *this);
        // Adjacent nodes in an in-order visit of a tree always have different
        // heights. This fact can be easily proven by induction on the tree's
        // height, using the definition of the in-order layout.
        PPL_ASSERT(candidate1_node.get_offset() != candidate2_node.get_offset());
        if (candidate1_node.get_offset() < candidate2_node.get_offset()) {
          PPL_ASSERT(candidate1_node.depth() > candidate2_node.depth());
          // candidate1_node is deeper in the tree than candidate2_node, so
          // use candidate1_node.
          insert_precise(key1, Coefficient_zero(), candidate1_node);
          return iterator(candidate1_node);
        } else {
          PPL_ASSERT(candidate1_node.depth() < candidate2_node.depth());
          // candidate2_node is deeper in the tree than candidate1_node, so
          // use candidate2_node.
          insert_precise(key1, Coefficient_zero(), candidate2_node);
          return iterator(candidate2_node);
        }
      }
    }
  } else {
    insert_in_empty_tree(key1, Coefficient_zero());
    return iterator(*this);
  }
}

inline dimension_type
CO_Tree::external_memory_in_bytes() const {
  dimension_type size = 0;
  // Adding the size of data[]
  size += (reserved_size + 1)*sizeof(data[0]);
  // Adding the size of indexes[]
  size += (reserved_size + 2)*sizeof(indexes[0]);
  return size;
}

inline bool
CO_Tree::empty() const {
  return size == 0;
}

inline unsigned
CO_Tree::integer_log2(dimension_type n) {
  PPL_ASSERT(n != 0);
  unsigned result = 0;
  while (n != 1) {
    n /= 2;
    ++result;
  }
  return result;
}

inline void
CO_Tree::dump_tree() const {
  if (empty())
    std::cout << "(empty tree)" << std::endl;
  else
    dump_subtree(tree_iterator(*const_cast<CO_Tree*>(this)));
}

inline void
CO_Tree::dump_subtree(tree_iterator itr) {
  if (!itr.is_leaf()) {
    itr.get_left_child();
    dump_subtree(itr);
    itr.get_parent();
  }
  std::cout << "At depth: " << itr.depth();
  if (itr->first == unused_index)
    std::cout << " (no data)" << std::endl;
  else
    std::cout << " pair (" << itr->first << "," << itr->second << ")" << std::endl;
  if (!itr.is_leaf()) {
    itr.get_right_child();
    dump_subtree(itr);
    itr.get_parent();
  }
}

inline bool
CO_Tree::erase(dimension_type key) {
  PPL_ASSERT(key != unused_index);

  if (size == 0)
    return false;

  tree_iterator itr(*this);
  go_down_searching_key(itr, key);

  if (itr->first != key)
    return false;

  erase(itr);

  return true;
}

inline CO_Tree::iterator
CO_Tree::before_begin() {
  return iterator(*this, 0);
}

inline CO_Tree::iterator
CO_Tree::begin() {
  return iterator(*this);
}

inline CO_Tree::iterator
CO_Tree::end() {
  return iterator(*this, reserved_size + 1);
}

inline CO_Tree::const_iterator
CO_Tree::before_begin() const {
  return const_iterator(*this, 0);
}

inline CO_Tree::const_iterator
CO_Tree::begin() const {
  return const_iterator(*this);
}

inline CO_Tree::const_iterator
CO_Tree::end() const {
  return const_iterator(*this, reserved_size + 1);
}

inline void
CO_Tree::go_down_searching_key(tree_iterator& itr, dimension_type key) {
  // itr points to a node, so the tree is not empty.
  PPL_ASSERT(!empty());
  PPL_ASSERT(key != unused_index);
  PPL_ASSERT(itr->first != unused_index);
  // TODO: Check if the copying back and forth improves or hapers performance.
  tree_iterator itr2(itr);
  while (!itr2.is_leaf()) {
    if (key == itr2->first)
      break;
    if (key < itr2->first) {
      itr2.get_left_child();
      if (itr2->first == unused_index) {
        itr2.get_parent();
        break;
      }
    } else {
      itr2.get_right_child();
      if (itr2->first == unused_index) {
        itr2.get_parent();
        break;
      }
    }
  }
  itr = itr2;
}

inline void
CO_Tree::erase(iterator itr) {
  PPL_ASSERT(itr != before_begin());
  PPL_ASSERT(itr != end());
  erase(tree_iterator(itr, *this));
}

template <typename Func>
inline CO_Tree::iterator
CO_Tree::bisect_in(iterator first, iterator last, const Func &func) {
  PPL_ASSERT(first != before_begin());
  PPL_ASSERT(first != end());
  PPL_ASSERT(last != before_begin());
  PPL_ASSERT(last != end());
  dimension_type index = bisect_in(&(first->second) - data,
                                   &(last->second) - data, func);
  return iterator(*this, index);
}

template <typename Func>
inline CO_Tree::const_iterator
CO_Tree::bisect_in(const_iterator first, const_iterator last,
                   const Func &func) const {
  PPL_ASSERT(first != before_begin());
  PPL_ASSERT(first != end());
  PPL_ASSERT(last != before_begin());
  PPL_ASSERT(last != end());
  dimension_type index = bisect_in(&(first->second) - data,
                                   &(last->second) - data, func);
  return const_iterator(*this, index);
}

template <typename Func>
inline CO_Tree::iterator
CO_Tree::bisect_near(iterator hint, const Func &func) {
  PPL_ASSERT(hint != before_begin());
  PPL_ASSERT(hint != end());
  dimension_type index = bisect_near(&(hint->second) - data, func);
  return iterator(*this, index);
}

template <typename Func>
inline CO_Tree::const_iterator
CO_Tree::bisect_near(const_iterator hint, const Func &func) const {
  PPL_ASSERT(hint != before_begin());
  PPL_ASSERT(hint != end());
  dimension_type index = bisect_near(&(hint->second) - data, func);
  return const_iterator(*this, index);
}

template <typename Func>
inline dimension_type
CO_Tree::bisect_in(dimension_type first, dimension_type last,
                   const Func &func) const {
  PPL_ASSERT(first != 0);
  PPL_ASSERT(last <= reserved_size);
  PPL_ASSERT(first <= last);
  PPL_ASSERT(indexes[first] != unused_index);
  PPL_ASSERT(indexes[last] != unused_index);

  while (first < last) {
    dimension_type half = (first + last) / 2;
    dimension_type new_half = half;

    while (indexes[new_half] == unused_index)
      ++new_half;

    int result = func(indexes[new_half], data[new_half]);

    if (result == 0)
      return new_half;

    if (result < 0) {

      while (indexes[half] == unused_index)
        --half;

      last = half;

    } else {

      ++new_half;
      while (indexes[new_half] == unused_index)
        ++new_half;

      first = new_half;
    }
  }

  // It is important that last is returned instead of first, because first
  // may have increased beyond last, even beyond the original value of last
  // at the beginning of this method.
  return last;
}

template <typename Func>
inline dimension_type
CO_Tree::bisect_near(dimension_type hint, const Func &func) const {
  PPL_ASSERT(hint != 0);
  PPL_ASSERT(hint <= reserved_size);
  PPL_ASSERT(indexes[hint] != unused_index);

  int result = func(indexes[hint], data[hint]);

  if (result == 0)
    return hint;
  else {
    dimension_type new_hint;
    dimension_type offset = 1;

    if (result < 0) {
      // The searched element is before `hint'.

      while (1) {

        if (hint <= offset) {
          // The searched element is in (0,hint).
          new_hint = hint;
          hint = 1;
          // The searched element is in [hint,new_hint).
          while (indexes[hint] == unused_index)
            ++hint;
          result = func(indexes[hint], data[hint]);
          if (result <= 0)
            return hint;
          // The searched element is in (hint,new_hint) and both indexes point
          // to valid elements.
          break;
        } else
          new_hint = hint - offset;

        PPL_ASSERT(new_hint > 0);
        PPL_ASSERT(new_hint <= reserved_size);

        // Find the element at `new_hint' (or the first after it).
        while (indexes[new_hint] == unused_index)
          ++new_hint;

        PPL_ASSERT(new_hint <= hint);

        result = func(indexes[new_hint], data[new_hint]);

        if (result == 0)
          return new_hint;
        else
          if (result > 0) {
            // The searched element is in (new_hint,hint)
            std::swap(hint, new_hint);
            // The searched element is now in (hint,new_hint).
            break;
          }

        hint = new_hint;
        offset *= 2;
      }

    } else {
      // The searched element is after `hint'.
      while (1) {

        if (hint + offset > reserved_size) {
          // The searched element is in (hint,reserved_size+1).
          new_hint = reserved_size;
          // The searched element is in (hint,new_hint].
          while (indexes[new_hint] == unused_index)
            --new_hint;
          result = func(indexes[new_hint], data[new_hint]);
          if (result >= 0)
            return new_hint;
          // The searched element is in (hint,new_hint) and both indexes point
          // to valid elements.
          break;
        } else
          new_hint = hint + offset;

        PPL_ASSERT(new_hint > 0);
        PPL_ASSERT(new_hint <= reserved_size);

        // Find the element at `new_hint' (or the first after it).
        while (indexes[new_hint] == unused_index)
          --new_hint;

        PPL_ASSERT(hint <= new_hint);

        result = func(indexes[new_hint], data[new_hint]);

        if (result == 0)
          return new_hint;
        else
          if (result < 0)
            // The searched element is in (hint,new_hint).
            break;

        hint = new_hint;
        offset *= 2;
      }
    }
    // The searched element is in (hint,new_hint).
    PPL_ASSERT(hint > 0);
    PPL_ASSERT(hint <= new_hint);
    PPL_ASSERT(new_hint <= reserved_size);
    PPL_ASSERT(indexes[hint] != unused_index);
    PPL_ASSERT(indexes[new_hint] != unused_index);

    if (hint == new_hint)
      return hint;

    ++hint;
    while (indexes[hint] == unused_index)
      ++hint;

    if (hint == new_hint)
      return hint;

    --new_hint;
    while (indexes[new_hint] == unused_index)
      --new_hint;

    PPL_ASSERT(hint <= new_hint);
    PPL_ASSERT(indexes[hint] != unused_index);
    PPL_ASSERT(indexes[new_hint] != unused_index);

    return bisect_in(hint, new_hint, func);
  }
}

inline void
CO_Tree::move_data_element(data_type& to, data_type& from) {
  // The following code is equivalent (but slower):
  //
  // new (&to) data_type(from);
  // from.~data_type();

  std::memcpy(&to, &from, sizeof(data_type));
}

inline void
CO_Tree::rebuild_bigger_tree() {
  if (reserved_size == 0)
    init(3);
  else {
    dimension_type new_reserved_size = reserved_size*2 + 1;

    dimension_type* new_indexes = new dimension_type[new_reserved_size + 2];
    data_type* new_data
      = static_cast<data_type*>(operator new(sizeof(data_type)
                                             * (new_reserved_size + 1)));

    new_indexes[1] = unused_index;

    for (dimension_type i = 1, j = 2; i <= reserved_size; ++i, ++j) {
      if (indexes[i] == unused_index)
        new_indexes[j] = unused_index;
      else {
        new_indexes[j] = indexes[i];
        move_data_element(new_data[j], data[i]);
      }
      ++j;
      new_indexes[j] = unused_index;
    }

    // These are used as markers by iterators.
    new_indexes[0] = 0;
    new_indexes[new_reserved_size + 1] = 0;

    delete [] indexes;
    operator delete(data);

    indexes = new_indexes;
    data = new_data;
    reserved_size = new_reserved_size;
    ++max_depth;
  }
  PPL_ASSERT(structure_OK());
}

inline void
CO_Tree::rebuild_smaller_tree() {
  if (reserved_size == 3) {
    destroy();
    init(0);
  } else {
    dimension_type new_reserved_size = reserved_size / 2;
    CO_Tree new_tree;
    new_tree.init(new_reserved_size);
    new_tree.move_data_from(*this);
    swap(new_tree);
    PPL_ASSERT(new_tree.structure_OK());
  }
  PPL_ASSERT(structure_OK());
}

inline void
CO_Tree::swap(CO_Tree& x) {

  std::swap(max_depth, x.max_depth);
  std::swap(indexes, x.indexes);
  std::swap(data, x.data);
  std::swap(reserved_size, x.reserved_size);
  std::swap(size, x.size);
  PPL_ASSERT(structure_OK());
  PPL_ASSERT(x.structure_OK());
}

inline
CO_Tree::iterator::iterator()
  : i(0), tree(0) {
}

inline
CO_Tree::iterator::iterator(CO_Tree& tree1)
  : i(1), tree(&tree1) {
  if (tree->reserved_size != 0)
    while (tree->indexes[i] == unused_index)
      ++i;
}

inline
CO_Tree::iterator::iterator(CO_Tree& tree1, dimension_type i1)
  : i(i1), tree(&tree1) {
  PPL_ASSERT(i <= tree->reserved_size + 1);
  PPL_ASSERT(tree->empty() || tree->indexes[i] != unused_index);
}

inline
CO_Tree::iterator::iterator(const iterator& itr2) {
  (*this) = itr2;
}

inline
CO_Tree::iterator::iterator(const tree_iterator& itr)
  : tree(itr.tree) {
  *this = itr;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator=(const tree_iterator& itr) {
  PPL_ASSERT(tree == itr.tree);
  i = itr.index();
  return *this;
}

inline std::pair<dimension_type&, CO_Tree::data_type&>
CO_Tree::iterator::operator*() {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return std::pair<dimension_type&, data_type&>(tree->indexes[i],
                                                tree->data[i]);
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::iterator::operator*() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return std::pair<const dimension_type&, const data_type&>(tree->indexes[i],
                                                            tree->data[i]);
}

inline CO_Tree::iterator::Member_Access_Helper
CO_Tree::iterator::operator->() {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return Member_Access_Helper(tree->indexes[i], tree->data[i]);
}

inline CO_Tree::iterator::Const_Member_Access_Helper
CO_Tree::iterator::operator->() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return Const_Member_Access_Helper(tree->indexes[i], tree->data[i]);
}

inline bool
CO_Tree::iterator::operator==(const iterator& x) const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree == x.tree);
  return (i == x.i);
}

inline bool
CO_Tree::iterator::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline bool
CO_Tree::iterator::is_at_end() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(i <= tree->reserved_size + 1);
  return i > tree->reserved_size;
}

inline bool
CO_Tree::iterator::is_before_begin() const {
  PPL_ASSERT(tree != 0);
  return i == 0;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  PPL_ASSERT(!tree->empty());
  while (tree->indexes[i] == unused_index)
    ++i;

  return *this;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  PPL_ASSERT(!tree->empty());
  while (tree->indexes[i] == unused_index)
    --i;

  return *this;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator=(const iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0)
    i = itr2.i;

  return *this;
}


inline
CO_Tree::iterator::Member_Access_Helper
::Member_Access_Helper(dimension_type& key, data_type& data)
  : my_pair(key, data) {
}

inline
std::pair<dimension_type&, CO_Tree::data_type&>*
CO_Tree::iterator::Member_Access_Helper::operator->() {
  return &my_pair;
}


inline
CO_Tree::iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type key, const data_type& data)
  : my_pair(key, data) {
}

inline
const std::pair<const dimension_type, const CO_Tree::data_type&>*
CO_Tree::iterator::Const_Member_Access_Helper::operator->() const {
  return &my_pair;
}


inline
CO_Tree::const_iterator::const_iterator()
  : i(0), tree(0) {
}

inline
CO_Tree::const_iterator::const_iterator(const CO_Tree& tree1)
  : i(1), tree(&tree1) {
  if (tree->reserved_size != 0)
    while (tree->indexes[i] == unused_index)
      ++i;
}

inline
CO_Tree::const_iterator::const_iterator(const CO_Tree& tree1,
                                        dimension_type i1)
  : i(i1), tree(&tree1) {
  PPL_ASSERT(i <= tree->reserved_size + 1);
  PPL_ASSERT(tree->empty() || tree->indexes[i] != unused_index);
}

inline
CO_Tree::const_iterator
::const_iterator(const const_iterator& itr2) {
  (*this) = itr2;
}

inline
CO_Tree::const_iterator
::const_iterator(const iterator& itr2) {
  (*this) = itr2;
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::const_iterator::operator*() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return std::pair<const dimension_type&, const data_type&>(tree->indexes[i],
                                                            tree->data[i]);
}

inline CO_Tree::const_iterator::Const_Member_Access_Helper
CO_Tree::const_iterator::operator->() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return Const_Member_Access_Helper(tree->indexes[i], tree->data[i]);
}

inline bool
CO_Tree::const_iterator
::operator==(const const_iterator& x) const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree == x.tree);
  return (i == x.i);
}

inline bool
CO_Tree::const_iterator
::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline bool
CO_Tree::const_iterator::is_at_end() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(i <= tree->reserved_size + 1);
  return i > tree->reserved_size;
}

inline bool
CO_Tree::const_iterator::is_before_begin() const {
  PPL_ASSERT(tree != 0);
  return i == 0;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  PPL_ASSERT(!tree->empty());
  while (tree->indexes[i] == unused_index)
    ++i;
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  PPL_ASSERT(!tree->empty());
  while (tree->indexes[i] == unused_index)
    --i;
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator=(const const_iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0)
    i = itr2.i;
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator=(const iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0)
    i = itr2.i;
  return *this;
}


inline
CO_Tree::const_iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type key, const data_type& data)
  : my_pair(key, data) {
}

inline
const std::pair<const dimension_type, const CO_Tree::data_type&>*
CO_Tree::const_iterator::Const_Member_Access_Helper::operator->() const {
  return &my_pair;
}


inline
CO_Tree::tree_iterator::tree_iterator(CO_Tree& tree1)
  : tree(&tree1) {
  PPL_ASSERT(tree->reserved_size != 0);
  get_root();
  PPL_ASSERT(OK());
}

inline
CO_Tree::tree_iterator::tree_iterator(const iterator& itr, CO_Tree& tree1)
  : tree(&tree1) {
  PPL_ASSERT(tree->reserved_size != 0);
  *this = itr;
  PPL_ASSERT(OK());
}

inline CO_Tree::tree_iterator&
CO_Tree::tree_iterator::operator=(const tree_iterator& itr) {
  PPL_ASSERT(tree == itr.tree);
  i = itr.i;
  offset = itr.offset;
  return *this;
}

inline CO_Tree::tree_iterator&
CO_Tree::tree_iterator::operator=(const iterator& itr) {
  PPL_ASSERT(itr != tree->before_begin());
  PPL_ASSERT(itr != tree->end());
  i = &(itr->second) - tree->data;
  offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  return *this;
}

inline bool
CO_Tree::tree_iterator::operator==(const tree_iterator& itr) const {
  return i == itr.i;
}

inline bool
CO_Tree::tree_iterator::operator!=(const tree_iterator& itr) const {
  return i != itr.i;
}

inline bool
CO_Tree::tree_iterator::operator==(const iterator& itr) const {
  return tree->data + i == &(itr->second);
}

inline bool
CO_Tree::tree_iterator::operator!=(const iterator& itr) const {
  return !(*this == itr);
}

inline void
CO_Tree::tree_iterator::get_root() {
  i = tree->reserved_size / 2 + 1;
  offset = i;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::get_left_child() {
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i -= offset;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::get_right_child() {
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i += offset;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::get_parent() {
  PPL_ASSERT(has_parent());
  PPL_ASSERT(offset != 0);
  i &= ~offset;
  offset *= 2;
  i |= offset;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::follow_left_childs() {
  i -= (offset - 1);
  offset = 1;
  PPL_ASSERT(is_leaf());
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::follow_right_childs() {
  i += (offset - 1);
  offset = 1;
  PPL_ASSERT(is_leaf());
  PPL_ASSERT(OK());
}

inline void
CO_Tree::tree_iterator::follow_left_childs_with_value() {
  PPL_ASSERT((*this)->first != unused_index);
  while (!is_leaf() && (*this)->first != unused_index)
    get_left_child();
  if ((*this)->first == unused_index)
    get_parent();
}

inline void
CO_Tree::tree_iterator::follow_right_childs_with_value() {
  PPL_ASSERT((*this)->first != unused_index);
  while (!is_leaf() && (*this)->first != unused_index)
    get_right_child();
  if ((*this)->first == unused_index)
    get_parent();
}

inline bool
CO_Tree::tree_iterator::get_left_child_value() {
  if (is_leaf())
    return false;
  get_left_child();
  if ((*this)->first == unused_index) {
    get_parent();
    return false;
  }
  return true;
}

inline bool
CO_Tree::tree_iterator::get_right_child_value() {
  if (is_leaf())
    return false;
  get_right_child();
  if ((*this)->first == unused_index) {
    get_parent();
    return false;
  }
  return true;
}

inline bool
CO_Tree::tree_iterator::has_parent() const {
  // This is implied by OK(), it is here for reference only.
  PPL_ASSERT(offset <= (tree->reserved_size / 2 + 1));
  return offset != (tree->reserved_size / 2 + 1);
}

inline bool
CO_Tree::tree_iterator::is_leaf() const {
  return offset == 1;
}

inline bool
CO_Tree::tree_iterator::is_right_child() const {
  if (!has_parent())
    // This is the root node.
    return false;
  return ((i & 2*offset) != 0);
}

inline std::pair<dimension_type&, CO_Tree::data_type&>
CO_Tree::tree_iterator::operator*() {
  return std::pair<dimension_type&, data_type&>(tree->indexes[i],
                                                tree->data[i]);
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::tree_iterator::operator*() const {
  return std::pair<const dimension_type&, const data_type&>(tree->indexes[i],
                                                            tree->data[i]);
}

inline CO_Tree::tree_iterator::Member_Access_Helper
CO_Tree::tree_iterator::operator->() {
  return Member_Access_Helper(tree->indexes[i], tree->data[i]);
}

inline CO_Tree::tree_iterator::Const_Member_Access_Helper
CO_Tree::tree_iterator::operator->() const {
  return Const_Member_Access_Helper(tree->indexes[i], tree->data[i]);
}

inline dimension_type
CO_Tree::tree_iterator::index() const {
  return i;
}

inline dimension_type
CO_Tree::tree_iterator::get_offset() const {
  return offset;
}

inline unsigned
CO_Tree::tree_iterator::depth() const {
  return integer_log2((tree->reserved_size + 1) / offset);
}

inline bool
CO_Tree::tree_iterator::OK() const {
  if (i == 0 || i > tree->reserved_size)
    return false;

  dimension_type correct_offset = i;
  // This assumes two's complement encoding.
  correct_offset &= -i;

  if (offset != correct_offset)
    return false;

  return true;
}


inline
CO_Tree::tree_iterator::Member_Access_Helper
::Member_Access_Helper(dimension_type& key, data_type& data)
  : my_pair(key, data) {
}

inline
std::pair<dimension_type&, CO_Tree::data_type&>*
CO_Tree::tree_iterator::Member_Access_Helper::operator->() {
  return &my_pair;
}


inline
CO_Tree::tree_iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type key, const data_type& data)
  : my_pair(key, data) {
}

inline
const std::pair<const dimension_type, const CO_Tree::data_type&>*
CO_Tree::tree_iterator::Const_Member_Access_Helper::operator->() const {
  return &my_pair;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_CO_Tree_inlines_hh)
