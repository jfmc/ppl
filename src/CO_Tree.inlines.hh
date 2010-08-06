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
  iterator itr(this);
  return insert(itr, key1, data1);
}

inline CO_Tree::iterator
CO_Tree::insert(dimension_type key1) {
  if (empty())
    return insert(key1, Coefficient_zero());
  else {
    iterator itr(this);
    go_down_searching_key(itr, key1);
    if (itr->first != key1)
      insert_precise(key1, Coefficient_zero(), itr);
    return itr;
  }
}

inline CO_Tree::iterator
CO_Tree::insert(iterator itr, dimension_type key1, const data_type& data1) {
  PPL_ASSERT(key1 != unused_index);

  if (!empty()) {
    PPL_ASSERT(!itr.is_at_end());
    PPL_ASSERT(!itr.is_before_begin());
    PPL_ASSERT(itr->first != unused_index);

    if (itr->first != key1) {
      if (itr->first > key1)
        while (itr.has_parent() && itr->first > key1)
          itr.get_parent();
      else
        while (itr.has_parent() && itr->first < key1)
          itr.get_parent();

      go_down_searching_key(itr, key1);

#ifndef NDEBUG
      iterator itr2(this);
      go_down_searching_key(itr2, key1);
      PPL_ASSERT(itr == itr2);
#endif
    }
  }

  insert_precise(key1, data1, itr);

  return itr;
}

inline CO_Tree::iterator
CO_Tree::insert(iterator itr, dimension_type key1) {
  PPL_ASSERT(key1 != unused_index);

  if (empty())
    return insert(itr, key1, Coefficient_zero());
  else {
    PPL_ASSERT(!itr.is_at_end());
    PPL_ASSERT(!itr.is_before_begin());
    PPL_ASSERT(itr->first != unused_index);

    if (itr->first != key1) {
      if (itr->first > key1)
        while (itr.has_parent() && itr->first > key1)
          itr.get_parent();
      else
        while (itr.has_parent() && itr->first < key1)
          itr.get_parent();

      go_down_searching_key(itr, key1);

#ifndef NDEBUG
      iterator itr2(this);
      go_down_searching_key(itr2, key1);
      PPL_ASSERT(itr == itr2);
#endif
    }

    if (itr->first != key1)
      insert_precise(key1, Coefficient_zero(), itr);

    return itr;
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

inline void
CO_Tree::dump_tree() const {
  CO_Tree::const_iterator itr(&*this);
  dump_subtree(itr);
}

inline void
CO_Tree::dump_subtree(iterator& itr) {
  if (itr.get_tree()->empty()) {
    std::cout << "(empty tree)" << std::endl;
    return;
  }
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

inline void
CO_Tree::dump_subtree(const_iterator& itr) {
  if (itr.get_tree()->empty()) {
    std::cout << "(empty tree)" << std::endl;
    return;
  }
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

  iterator itr(&*this);
  go_down_searching_key(itr, key);

  if (itr->first != key)
    return false;

  erase(itr);

  return true;
}

inline CO_Tree::iterator
CO_Tree::before_begin() {
  return iterator::construct_before_begin(*this);
}

inline CO_Tree::iterator
CO_Tree::end() {
  return iterator::construct_end(*this);
}

inline CO_Tree::const_iterator
CO_Tree::before_begin() const {
  return const_iterator::construct_before_begin(*this);
}

inline CO_Tree::const_iterator
CO_Tree::end() const {
  return const_iterator::construct_end(*this);
}

inline void
CO_Tree::go_down_searching_key(iterator& itr, dimension_type key) {
  if (empty())
    return;
  PPL_ASSERT(key != unused_index);
  PPL_ASSERT(itr->first != unused_index);
  PPL_ASSERT(!itr.is_before_begin());
  PPL_ASSERT(!itr.is_at_end());
  iterator itr2(itr);
  dimension_type offset = (itr2.i & - itr2.i) / 2;
  while (offset != 0) {
    if (key == itr2->first)
      break;
    if (key < itr2->first) {
      itr2.i -= offset;
      if (itr2->first == unused_index) {
        itr2.i += offset;
        break;
      }
      offset /= 2;
    } else {
      itr2.i += offset;
      if (itr2->first == unused_index) {
        itr2.i -= offset;
        break;
      }
      offset /= 2;
    }
  }
  itr.i = itr2.i;
}

inline void
CO_Tree::go_down_searching_key(const_iterator& itr,
                               dimension_type key) const {
  if (empty())
    return;
  PPL_ASSERT(key != unused_index);
  PPL_ASSERT(itr->first != unused_index);
  PPL_ASSERT(!itr.is_before_begin());
  PPL_ASSERT(!itr.is_at_end());
  const_iterator itr2(itr);
  dimension_type offset = (itr2.i & - itr2.i) / 2;
  while (offset != 0) {
    if (key == itr2->first)
      break;
    if (key < itr2->first) {
      itr2.i -= offset;
      if (itr2->first == unused_index) {
        itr2.i += offset;
        break;
      }
      offset /= 2;
    } else {
      itr2.i += offset;
      if (itr2->first == unused_index) {
        itr2.i -= offset;
        break;
      }
      offset /= 2;
    }
  }
  itr.i = itr2.i;
}

inline CO_Tree::iterator
CO_Tree::lower_bound(iterator itr, dimension_type key) {
  PPL_ASSERT(!empty());
  PPL_ASSERT(itr.is_at_end() || itr->first <= key);

  if (itr.is_before_begin())
    ++itr;

  PPL_ASSERT(itr.is_at_end() || itr->first <= key);

  if (itr.is_at_end())
    return itr;

  dimension_type low_index = itr.i;
  dimension_type high_index;

  // Logarithmic search of an interval in which the key will be searched.
  // Near (and small) intervals are tried first, to exploit the caches.
  {
    dimension_type hop = 1;
    dimension_type last_low_index = low_index;

    while (1) {

      if (low_index > reserved_size)
        low_index = reserved_size + 1;
      else
        while (indexes[low_index] == unused_index)
          ++low_index;

      PPL_ASSERT(indexes[low_index] != unused_index);

      if (low_index > reserved_size) {
        PPL_ASSERT(low_index <= reserved_size + 1);
        high_index = low_index - 1;
        low_index = last_low_index;
        break;
      }

      if (indexes[low_index] == key) {
        itr.i = low_index;
        return itr;
      }

      if (indexes[low_index] > key) {
        high_index = low_index - 1;
        low_index = last_low_index;
        break;
      }

      PPL_ASSERT(indexes[low_index] < key);

      last_low_index = low_index;
      low_index += hop;
      hop *= 2;
    }
  }

  PPL_ASSERT(low_index > 0);

  while (low_index + 1 < high_index) {

    dimension_type avg = (low_index + high_index) / 2;
    dimension_type index = avg;

    while (indexes[index] == unused_index)
      ++index;

    if (index > high_index || indexes[index] > key) {
      high_index = avg;
    } else {
      low_index = index;
    }
  }

  if (low_index == high_index) {
    if (indexes[low_index] == unused_index || indexes[low_index] < key)
      ++low_index;
    while (indexes[low_index] == unused_index)
      ++low_index;

  } else {
    PPL_ASSERT(low_index + 1 == high_index);
    if (indexes[low_index] == unused_index || indexes[low_index] < key) {
      ++low_index;
      if (indexes[low_index] == unused_index || indexes[low_index] < key)
        ++low_index;
    }
    while (indexes[low_index] == unused_index)
      ++low_index;
  }

  itr.i = low_index;

#ifndef NDEBUG
  iterator itr2 = itr;
  itr2.get_previous_value();
  PPL_ASSERT(itr2.is_before_begin() || itr2->first < key);
  itr2.get_next_value();
  PPL_ASSERT(itr2 == itr);
#endif

  PPL_ASSERT(itr.is_at_end() || itr->first >= key);

  return itr;
}

inline CO_Tree::const_iterator
CO_Tree::lower_bound(const_iterator itr, dimension_type key) const {
  PPL_ASSERT(!empty());
  PPL_ASSERT(itr.is_at_end() || itr->first <= key);

  if (itr.is_before_begin())
    ++itr;

  PPL_ASSERT(itr.is_at_end() || itr->first <= key);

  if (itr.is_at_end())
    return itr;

  dimension_type low_index = itr.i;
  dimension_type high_index;

  // Logarithmic search of an interval in which the key will be searched.
  // Near (and small) intervals are tried first, to exploit the caches.
  {
    dimension_type hop = 1;
    dimension_type last_low_index = low_index;

    while (1) {

      if (low_index > reserved_size)
        low_index = reserved_size + 1;
      else
        while (indexes[low_index] == unused_index)
          ++low_index;

      PPL_ASSERT(indexes[low_index] != unused_index);

      if (low_index > reserved_size) {
        PPL_ASSERT(low_index <= reserved_size + 1);
        high_index = low_index - 1;
        low_index = last_low_index;
        break;
      }

      if (indexes[low_index] == key) {
        itr.i = low_index;
        return itr;
      }

      if (indexes[low_index] > key) {
        high_index = low_index - 1;
        low_index = last_low_index;
        break;
      }

      PPL_ASSERT(indexes[low_index] < key);

      last_low_index = low_index;
      low_index += hop;
      hop *= 2;
    }
  }

  PPL_ASSERT(low_index > 0);

  while (low_index + 1 < high_index) {

    dimension_type avg = (low_index + high_index) / 2;
    dimension_type index = avg;

    while (indexes[index] == unused_index)
      ++index;

    if (index > high_index || indexes[index] > key) {
      high_index = avg;
    } else {
      low_index = index;
    }
  }

  if (low_index == high_index) {
    if (indexes[low_index] == unused_index || indexes[low_index] < key)
      ++low_index;
    while (indexes[low_index] == unused_index)
      ++low_index;

  } else {
    PPL_ASSERT(low_index + 1 == high_index);
    if (indexes[low_index] == unused_index || indexes[low_index] < key) {
      ++low_index;
      if (indexes[low_index] == unused_index || indexes[low_index] < key)
        ++low_index;
    }
    while (indexes[low_index] == unused_index)
      ++low_index;
  }

  itr.i = low_index;

#ifndef NDEBUG
  const_iterator itr2 = itr;
  itr2.get_previous_value();
  PPL_ASSERT(itr2.is_before_begin() || itr2->first < key);
  itr2.get_next_value();
  PPL_ASSERT(itr2 == itr);
#endif

  PPL_ASSERT(itr.is_at_end() || itr->first >= key);

  return itr;
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

    dimension_type* new_indexes
      = static_cast<dimension_type*>(malloc(sizeof(dimension_type)
                                            * (new_reserved_size + 2)));
    data_type* new_data
      = static_cast<data_type*>(malloc(sizeof(data_type)
                                       * (new_reserved_size + 1)));

    new (&(new_indexes[1])) dimension_type(unused_index);

    for (dimension_type i = 1, j = 2; i <= reserved_size; ++i, ++j) {
      if (indexes[i] == unused_index)
        new (&(new_indexes[j])) dimension_type(unused_index);
      else {
        new (&(new_indexes[j])) dimension_type(indexes[i]);
        move_data_element(new_data[j], data[i]);
      }
      indexes[i].~dimension_type();
      ++j;
      new (&(new_indexes[j])) dimension_type(unused_index);
    }

    // These were used as markers by iterators.
    indexes[0].~dimension_type();
    indexes[reserved_size + 1].~dimension_type();

    // These are used as markers by iterators.
    new (&(new_indexes[0])) dimension_type(0);
    new (&(new_indexes[new_reserved_size + 1])) dimension_type(0);

    free(indexes);
    free(data);

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
CO_Tree::iterator::iterator(CO_Tree* tree1)
  : tree(tree1) {
  if (tree != 0)
    get_root();
}

inline
CO_Tree::iterator::iterator(const iterator& itr2) {
  (*this) = itr2;
}

inline CO_Tree::iterator
CO_Tree::iterator::construct_before_begin(CO_Tree& tree) {

  iterator itr(&tree);
  itr.i = 0;
  return itr;
}

inline CO_Tree::iterator
CO_Tree::iterator::construct_end(CO_Tree& tree) {

  iterator itr(&tree);
  itr.i = tree.reserved_size + 1;
  return itr;
}

inline void
CO_Tree::iterator::get_root() {
  PPL_ASSERT(tree != 0);
  i = tree->reserved_size / 2 + 1;
}

inline void
CO_Tree::iterator::get_left_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i -= offset;
}

inline void
CO_Tree::iterator::get_right_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i += offset;
}

inline void
CO_Tree::iterator::get_parent() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(has_parent());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  i &= ~offset;
  offset *= 2;
  i |= offset;
}

inline void
CO_Tree::iterator::follow_left_childs() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  i -= (offset - 1);
  PPL_ASSERT(is_leaf());
}

inline void
CO_Tree::iterator::follow_right_childs() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  i += (offset - 1);
  PPL_ASSERT(is_leaf());
}

inline void
CO_Tree::iterator::follow_left_childs_with_value() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT((*this)->first != unused_index);
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset > 0);
  while (offset != 0 && (*this)->first != unused_index) {
    offset /= 2;
    i -= offset;
  }
  i += offset;
  PPL_ASSERT((*this)->first != unused_index);
}

inline void
CO_Tree::iterator::follow_right_childs_with_value() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT((*this)->first != unused_index);
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset > 0);
  while (offset != 0 && (*this)->first != unused_index) {
    offset /= 2;
    i += offset;
  }
  i -= offset;
  PPL_ASSERT((*this)->first != unused_index);
}

inline bool
CO_Tree::iterator::has_parent() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  offset &= -i;
  PPL_ASSERT(offset != 0);
  return offset < (tree->reserved_size / 2 + 1);
}

inline bool
CO_Tree::iterator::is_right_child() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());

  dimension_type offset = i;
  offset &= -i;
  PPL_ASSERT(offset != 0);
  if (offset == tree->reserved_size / 2 + 1)
    // This is the root node.
    return false;
  PPL_ASSERT(offset < tree->reserved_size / 2 + 1);
  offset *= 2;
  return ((i & offset) != 0);
}

inline bool
CO_Tree::iterator::is_leaf() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return ((i & (dimension_type)0x01U) != 0);
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

inline CO_Tree::height_t
CO_Tree::iterator::depth() const {
  PPL_ASSERT(tree != 0);
  dimension_type n = i;
  height_t d = 0;
  while ((n & (dimension_type)0x01U) == 0) {
    n /= 2;
    ++d;
  }
  d = tree->max_depth - d;
  return d;
}

inline CO_Tree*
CO_Tree::iterator::get_tree() {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline const CO_Tree*
CO_Tree::iterator::get_tree() const {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  return *this;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  return *this;
}

inline bool
CO_Tree::iterator::get_left_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  dimension_type j = i;
  get_left_child();
  if ((*this)->first == unused_index) {
    i = j;
    return false;
  }
  return true;
}

inline bool
CO_Tree::iterator::get_right_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  dimension_type j = i;
  get_right_child();
  if ((*this)->first == unused_index) {
    i = j;
    return false;
  }
  return true;
}

inline void
CO_Tree::iterator::get_next_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  if (!tree->empty())
    while (tree->indexes[i] == unused_index)
      ++i;
}

inline void
CO_Tree::iterator::get_previous_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  if (!tree->empty())
    while (tree->indexes[i] == unused_index)
      --i;
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
CO_Tree::const_iterator::const_iterator(const CO_Tree* tree1)
  : tree(tree1) {
  if (tree != 0)
    get_root();
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

inline CO_Tree::const_iterator
CO_Tree::const_iterator::construct_before_begin(const CO_Tree& tree) {

  const_iterator itr(&tree);
  itr.i = 0;
  return itr;
}

inline CO_Tree::const_iterator
CO_Tree::const_iterator::construct_end(const CO_Tree& tree) {

  const_iterator itr(&tree);
  itr.i = tree.reserved_size + 1;
  return itr;
}

inline void
CO_Tree::const_iterator::get_root() {
  PPL_ASSERT(tree != 0);
  i = tree->reserved_size / 2 + 1;
}

inline void
CO_Tree::const_iterator::get_left_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i -= offset;
}

inline void
CO_Tree::const_iterator::get_right_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  PPL_ASSERT(offset != 1);
  offset /= 2;
  i += offset;
}

inline void
CO_Tree::const_iterator::get_parent() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset != 0);
  i &= ~offset;
  offset *= 2;
  i |= offset;
}

inline void
CO_Tree::const_iterator::follow_left_childs() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  i -= (offset - 1);
  PPL_ASSERT(is_leaf());
}

inline void
CO_Tree::const_iterator::follow_right_childs() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  i += (offset - 1);
  PPL_ASSERT(is_leaf());
}

inline void
CO_Tree::const_iterator::follow_left_childs_with_value() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT((*this)->first != unused_index);
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset > 0);
  while (offset != 0 && (*this)->first != unused_index) {
    offset /= 2;
    i -= offset;
  }
  i += offset;
  PPL_ASSERT((*this)->first != unused_index);
}

inline void
CO_Tree::const_iterator::follow_right_childs_with_value() {
  PPL_ASSERT(!tree->empty());
  PPL_ASSERT(!is_before_begin());
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT((*this)->first != unused_index);
  dimension_type offset = i;
  // This assumes two's complement encoding.
  offset &= -i;
  PPL_ASSERT(offset > 0);
  while (offset != 0 && (*this)->first != unused_index) {
    offset /= 2;
    i += offset;
  }
  i -= offset;
  PPL_ASSERT((*this)->first != unused_index);
}

inline bool
CO_Tree::const_iterator::has_parent() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  offset &= -i;
  PPL_ASSERT(offset != 0);
  return offset < (tree->reserved_size / 2 + 1);
}

inline bool
CO_Tree::const_iterator::is_right_child() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  dimension_type offset = i;
  offset &= -i;
  PPL_ASSERT(offset != 0);
  if (offset == tree->reserved_size / 2 + 1)
    // This is the root node.
    return false;
  PPL_ASSERT(offset < tree->reserved_size / 2 + 1);
  offset *= 2;
  return ((i & offset) != 0);
}

inline bool
CO_Tree::const_iterator::is_leaf() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!is_at_end());
  PPL_ASSERT(!is_before_begin());
  return ((i & (dimension_type)0x01U) != 0);
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

inline CO_Tree::height_t
CO_Tree::const_iterator::depth() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  dimension_type n = i;
  height_t d = 0;
  while ((n & (dimension_type)0x01U) == 0) {
    n /= 2;
    ++d;
  }
  d = tree->max_depth - d;
  return d;
}

inline const CO_Tree*
CO_Tree::const_iterator::get_tree() const {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  return *this;
}

inline bool
CO_Tree::const_iterator::get_left_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  dimension_type j = i;
  get_left_child();
  if ((*this)->first == unused_index) {
    i = j;
    return false;
  }
  return true;
}

inline bool
CO_Tree::const_iterator::get_right_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  dimension_type j = i;
  get_right_child();
  if ((*this)->first == unused_index) {
    i = j;
    return false;
  }
  return true;
}

inline void
CO_Tree::const_iterator::get_next_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_at_end());
  ++i;
  if (!tree->empty())
    while (tree->indexes[i] == unused_index)
      ++i;
}

inline void
CO_Tree::const_iterator::get_previous_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!is_before_begin());
  --i;
  if (!tree->empty())
    while (tree->indexes[i] == unused_index)
      --i;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator
::operator=(const const_iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0)
    i = itr2.i;
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator
::operator=(const iterator& itr2) {
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
CO_Tree::const_iterator::Const_Member_Access_Helper
::operator->() const {
  return &my_pair;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_CO_Tree_inlines_hh)
