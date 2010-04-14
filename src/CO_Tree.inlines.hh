/* Checked_Number class implementation: inline functions.
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

inline dimension_type
CO_Tree::external_memory_in_bytes() const {
  dimension_type size = 0;
  // Adding the size of data[]
  size += (reserved_size + 1)*sizeof(data[0]);
  // Adding the size of level[]
  size += max_depth*sizeof(level[0]);
  return size;
}

inline bool
CO_Tree::empty() const {
  return size == 0;
}

inline void
CO_Tree::dump_tree() const {
  CO_Tree::inorder_const_iterator itr(&*this);
  dump_subtree(itr);
}

inline void
CO_Tree::dump_subtree(inorder_iterator& itr) {
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
CO_Tree::dump_subtree(inorder_const_iterator& itr) {
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
CO_Tree::insert(dimension_type key, const data_type& value) {
  inorder_iterator itr(&*this);
  insert(key, value, itr);
  PPL_ASSERT(OK());
}

inline bool
CO_Tree::erase(dimension_type key) {
  PPL_ASSERT(key != unused_index);

  if (size == 0)
    return false;

  inorder_iterator itr(&*this);
  lower_bound(itr, key);

  if (itr->first != key)
    return false;

  erase(itr);

  return true;
}

inline void
CO_Tree::rebuild_level_data(dimension_type max_depth) {
  level = new level_data[max_depth];
  rebuild_level_data_helper(1, max_depth);
}

inline CO_Tree::inorder_iterator
CO_Tree::before_begin() {
  return inorder_iterator::construct_before_begin(*this);
}

inline CO_Tree::inorder_iterator
CO_Tree::end() {
  return inorder_iterator::construct_end(*this);
}

inline CO_Tree::inorder_const_iterator
CO_Tree::before_begin() const {
  return inorder_const_iterator::construct_before_begin(*this);
}

inline CO_Tree::inorder_const_iterator
CO_Tree::end() const {
  return inorder_const_iterator::construct_end(*this);
}

inline CO_Tree::unordered_iterator
CO_Tree::unordered_begin() {
  if (reserved_size == 0)
    return unordered_iterator(0);
  // The first element of data[] is not used.
  value_type* p = data + 1;
  while (p->first == unused_index)
    ++p;
  return unordered_iterator(p);
}

inline CO_Tree::unordered_iterator
CO_Tree::unordered_end() {
  if (reserved_size == 0)
    return unordered_iterator(0);
  PPL_ASSERT(data[reserved_size + 1].first != unused_index);
  return unordered_iterator(&(data[reserved_size + 1]));
}

inline CO_Tree::unordered_const_iterator
CO_Tree::unordered_begin() const {
  if (reserved_size == 0)
    return unordered_const_iterator(0);
  // The first element of data[] is not used.
  value_type* p = data + 1;
  while (p->first == unused_index)
    ++p;
  return unordered_const_iterator(p);
}

inline CO_Tree::unordered_const_iterator
CO_Tree::unordered_end() const {
  if (reserved_size == 0)
    return unordered_const_iterator(0);
  PPL_ASSERT(data[reserved_size + 1].first != unused_index);
  return unordered_const_iterator(&(data[reserved_size + 1]));
}

inline void
CO_Tree::lower_bound(inorder_iterator& itr, dimension_type key) {
  if (empty())
    return;
  PPL_ASSERT(key != unused_index);
  PPL_ASSERT(itr->first != unused_index);
  PPL_ASSERT(!itr.is_before_begin());
  PPL_ASSERT(!itr.is_at_end());
  while (1) {
    if (key == itr->first)
      return;
    if (key < itr->first) {
      if (!itr.get_left_child_value())
        break;
    } else {
      if (!itr.get_right_child_value())
        break;
    }
  }
}

inline void
CO_Tree::lower_bound(inorder_const_iterator& itr, dimension_type key) const {
  if (empty())
    return;
  PPL_ASSERT(key != unused_index);
  PPL_ASSERT(itr->first != unused_index);
  PPL_ASSERT(!itr.is_before_begin());
  PPL_ASSERT(!itr.is_at_end());
  while (1) {
    if (key == itr->first)
      return;
    if (key < itr->first) {
      if (!itr.get_left_child_value())
        break;
    } else {
      if (!itr.get_right_child_value())
        break;
    }
  }
}

inline void
CO_Tree::move_data_element(data_type& to, data_type& from) {
  std::memcpy(&to, &from, sizeof(data_type));
}

inline void
CO_Tree::rebuild_bigger_tree() {
  if (reserved_size == 0)
    init(3);
  else {
    dimension_type new_reserved_size = reserved_size*2 + 1;
    CO_Tree new_tree;
    new_tree.init(new_reserved_size);
    new_tree.move_data_from(*this);
    swap(new_tree);
    PPL_ASSERT(new_tree.structure_OK());
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
  std::swap(level, x.level);
  std::swap(max_depth, x.max_depth);
  std::swap(data, x.data);
  std::swap(reserved_size, x.reserved_size);
  std::swap(size, x.size);
  PPL_ASSERT(structure_OK());
  PPL_ASSERT(x.structure_OK());
}

inline
CO_Tree::inorder_iterator::inorder_iterator(CO_Tree* tree1)
  : tree(tree1) {
  if (tree != 0) {
    get_root();
    if (tree->reserved_size == 0)
      at_end = true;
  }
}

inline
CO_Tree::inorder_iterator::inorder_iterator(const inorder_iterator& itr2) {
  (*this) = itr2;
}

inline CO_Tree::inorder_iterator
CO_Tree::inorder_iterator::construct_before_begin(CO_Tree& tree) {

  inorder_iterator itr(&tree);
  itr.before_begin = true;
  itr.at_end = false;

  return itr;
}

inline CO_Tree::inorder_iterator
CO_Tree::inorder_iterator::construct_end(CO_Tree& tree) {

  inorder_iterator itr(&tree);
  itr.at_end = true;

  return itr;
}

inline void
CO_Tree::inorder_iterator::get_root() {
  PPL_ASSERT(tree != 0);
  d = 1;
  i = 1;
  pos[1] = 1;
  at_end = false;
  before_begin = false;
}

inline void
CO_Tree::inorder_iterator::get_left_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(tree->max_depth > d);
  i <<= 1;
  const level_data &ld = tree->level[d];
  pos[d+1] = pos[ld.depth_of_root_of_top_tree] + ld.top_tree_size
                + (i & ld.top_tree_size)*ld.bottom_tree_size;
  ++d;
}

inline void
CO_Tree::inorder_iterator::get_right_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(tree->max_depth > d);
  i <<= 1;
  ++i;
  const level_data &ld = tree->level[d];
  pos[d+1] = pos[ld.depth_of_root_of_top_tree] + ld.top_tree_size
                + (i & ld.top_tree_size)*ld.bottom_tree_size;
  ++d;
}

inline void
CO_Tree::inorder_iterator::get_parent() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(d > 1);
  --d;
  i >>= 1;
}

inline bool
CO_Tree::inorder_iterator::has_parent() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return d > 1;
}

inline bool
CO_Tree::inorder_iterator::is_right_child() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return (d > 1) && (i & (dimension_type)0x01);
}

inline bool
CO_Tree::inorder_iterator::is_leaf() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return d == tree->max_depth;
}

inline CO_Tree::value_type&
CO_Tree::inorder_iterator::operator*() {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return tree->data[pos[d]];
}

inline const CO_Tree::value_type&
CO_Tree::inorder_iterator::operator*() const {
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return tree->data[pos[d]];
}

inline CO_Tree::value_type*
CO_Tree::inorder_iterator::operator->() {
  return &(**this);
}

inline const CO_Tree::value_type*
CO_Tree::inorder_iterator::operator->() const {
  return &(**this);
}

inline bool
CO_Tree::inorder_iterator::operator==(const inorder_iterator& x) const {
  PPL_ASSERT(tree != 0);
  if (at_end || x.at_end)
    return at_end == x.at_end;
  if (before_begin || x.before_begin)
    return before_begin == x.before_begin;
  return (tree == x.tree) && (pos[d] == x.pos[x.d]);
}

inline bool
CO_Tree::inorder_iterator::operator!=(const inorder_iterator& x) const {
  return !(*this == x);
}

inline bool
CO_Tree::inorder_iterator::is_at_end() const {
  PPL_ASSERT(tree != 0);
  return at_end;
}

inline bool
CO_Tree::inorder_iterator::is_before_begin() const {
  PPL_ASSERT(tree != 0);
  return before_begin;
}

inline dimension_type
CO_Tree::inorder_iterator::depth() const {
  PPL_ASSERT(tree != 0);
  return d;
}

inline CO_Tree*
CO_Tree::inorder_iterator::get_tree() {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline const CO_Tree*
CO_Tree::inorder_iterator::get_tree() const {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline CO_Tree::inorder_iterator&
CO_Tree::inorder_iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  if (before_begin) {

    get_root();
    if (tree->reserved_size == 0)
      at_end = true;
    else
      while (!is_leaf())
        get_left_child();

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (is_leaf()) {
      while (has_parent() && is_right_child())
        get_parent();
      if (!has_parent())
        at_end = true;
      else
        get_parent();
    } else {
      get_right_child();
      while (!is_leaf())
        get_left_child();
    }
  }

  return *this;
}

inline CO_Tree::inorder_iterator&
CO_Tree::inorder_iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!before_begin);
  if (at_end) {
    get_root();

    if (tree->reserved_size == 0)
      before_begin = true;
    else
      while (!is_leaf())
        get_right_child();

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (is_leaf())
      if (is_right_child())
        get_parent();
      else {
        if (!has_parent())
          before_begin = true;
        else {
          while (has_parent() && !is_right_child())
            get_parent();
          if (has_parent())
            get_parent();
          else
            before_begin = true;
        }
      }
    else {
      get_left_child();
      while (!is_leaf())
        get_right_child();
    }
  }

  return *this;
}

inline bool
CO_Tree::inorder_iterator::get_left_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
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
CO_Tree::inorder_iterator::get_right_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  get_right_child();
  if ((*this)->first == unused_index) {
    get_parent();
    return false;
  }
  return true;
}

inline void
CO_Tree::inorder_iterator::get_next_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  if (before_begin) {
    get_root();

    if (tree->reserved_size == 0)
      at_end = true;
    else {
      while (get_left_child_value())
        ;

      if ((*this)->first == unused_index) {
        PPL_ASSERT(d == 1);
        at_end = true;
      }
    }

    PPL_ASSERT(at_end || (*this)->first != unused_index);

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
#ifndef NDEBUG
    const dimension_type previous_index = (*this)->first;
#endif
    if (get_right_child_value())
      while (get_left_child_value())
        ;
    else {
      while (has_parent() && is_right_child())
        get_parent();
      if (!has_parent())
        at_end = true;
      else
        get_parent();
    }

#ifndef NDEBUG
    if (!at_end)
      // previous_index could be unused_index because we deleted the current
      // node, as we do in move_data_from().
      if (previous_index != unused_index)
      PPL_ASSERT((*this)->first != unused_index
                 && (*this)->first > previous_index);
#endif
  }
}

inline void
CO_Tree::inorder_iterator::get_previous_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!before_begin);
  if (at_end) {
    get_root();

    if (tree->reserved_size == 0)
      before_begin = true;
    else
      while (get_right_child_value())
        ;

    PPL_ASSERT(before_begin || (*this)->first != unused_index);

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (!get_left_child_value())
      if (is_right_child())
        get_parent();
      else {
        if (!has_parent())
          before_begin = true;
        else {
          while (has_parent() && !is_right_child())
            get_parent();
          if (has_parent())
            get_parent();
          else
            before_begin = true;
        }
      }
    else
      while (get_right_child_value())
        ;

    PPL_ASSERT(before_begin || (*this)->first != unused_index);
  }
}

inline CO_Tree::inorder_iterator&
CO_Tree::inorder_iterator::operator=(const inorder_iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0) {
    at_end = itr2.at_end;
    before_begin = itr2.before_begin;
    if (!at_end && !before_begin) {
      d = itr2.d;
      i = itr2.i;
      for (dimension_type i = 1; i <= itr2.d; ++i)
        pos[i] = itr2.pos[i];
    }
  }

  return *this;
}


inline
CO_Tree::inorder_const_iterator::inorder_const_iterator(const CO_Tree* tree1)
  : tree(tree1) {
  if (tree != 0) {
    get_root();
    if (tree->reserved_size == 0)
      at_end = true;
  }
}

inline
CO_Tree::inorder_const_iterator
::inorder_const_iterator(const inorder_const_iterator& itr2) {
  (*this) = itr2;
}

inline
CO_Tree::inorder_const_iterator
::inorder_const_iterator(const inorder_iterator& itr2) {
  (*this) = itr2;
}

inline CO_Tree::inorder_const_iterator
CO_Tree::inorder_const_iterator::construct_before_begin(const CO_Tree& tree) {

  inorder_const_iterator itr(&tree);
  itr.before_begin = true;
  itr.at_end = false;

  return itr;
}

inline CO_Tree::inorder_const_iterator
CO_Tree::inorder_const_iterator::construct_end(const CO_Tree& tree) {

  inorder_const_iterator itr(&tree);
  itr.at_end = true;

  return itr;
}

inline void
CO_Tree::inorder_const_iterator::get_root() {
  PPL_ASSERT(tree != 0);
  d = 1;
  i = 1;
  pos[1] = 1;
  at_end = false;
  before_begin = false;
}

inline void
CO_Tree::inorder_const_iterator::get_left_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(tree->max_depth > d);
  i <<= 1;
  const level_data &ld = tree->level[d];
  pos[d+1] = pos[ld.depth_of_root_of_top_tree] + ld.top_tree_size
                + (i & ld.top_tree_size)*ld.bottom_tree_size;
  ++d;
}

inline void
CO_Tree::inorder_const_iterator::get_right_child() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(tree->max_depth > d);
  i <<= 1;
  ++i;
  const level_data &ld = tree->level[d];
  pos[d+1] = pos[ld.depth_of_root_of_top_tree] + ld.top_tree_size
                + (i & ld.top_tree_size)*ld.bottom_tree_size;
  ++d;
}

inline void
CO_Tree::inorder_const_iterator::get_parent() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  PPL_ASSERT(d > 1);
  --d;
  i >>= 1;
}

inline bool
CO_Tree::inorder_const_iterator::has_parent() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return d > 1;
}

inline bool
CO_Tree::inorder_const_iterator::is_right_child() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return (d > 1) && (i & (dimension_type)0x01);
}

inline bool
CO_Tree::inorder_const_iterator::is_leaf() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return d == tree->max_depth;
}

inline const CO_Tree::value_type&
CO_Tree::inorder_const_iterator::operator*() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  PPL_ASSERT(!at_end);
  PPL_ASSERT(!before_begin);
  return tree->data[pos[d]];
}

inline const CO_Tree::value_type*
CO_Tree::inorder_const_iterator::operator->() const {
  return &(**this);
}

inline bool
CO_Tree::inorder_const_iterator
::operator==(const inorder_const_iterator& x) const {
  PPL_ASSERT(tree != 0);
  if (at_end || x.at_end)
    return at_end == x.at_end;
  if (before_begin || x.before_begin)
    return before_begin == x.before_begin;
  return (tree == x.tree) && (pos[d] == x.pos[x.d]);
}

inline bool
CO_Tree::inorder_const_iterator
::operator!=(const inorder_const_iterator& x) const {
  return !(*this == x);
}

inline bool
CO_Tree::inorder_const_iterator::is_at_end() const {
  PPL_ASSERT(tree != 0);
  return at_end;
}

inline bool
CO_Tree::inorder_const_iterator::is_before_begin() const {
  PPL_ASSERT(tree != 0);
  return before_begin;
}

inline dimension_type
CO_Tree::inorder_const_iterator::depth() const {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  return d;
}

inline const CO_Tree*
CO_Tree::inorder_const_iterator::get_tree() const {
  PPL_ASSERT(tree != 0);
  return tree;
}

inline CO_Tree::inorder_const_iterator&
CO_Tree::inorder_const_iterator::operator++() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  if (before_begin) {

    get_root();

    if (tree->reserved_size == 0)
      at_end = true;
    else
      while (!is_leaf())
        get_left_child();

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (is_leaf()) {
      while (has_parent() && is_right_child())
        get_parent();
      if (!has_parent())
        at_end = true;
      else
        get_parent();
    } else {
      get_right_child();
      while (!is_leaf())
        get_left_child();
    }
  }

  return *this;
}

inline CO_Tree::inorder_const_iterator&
CO_Tree::inorder_const_iterator::operator--() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!before_begin);
  if (at_end) {
    get_root();

    if (tree->reserved_size == 0)
      before_begin = true;
    else
      while (!is_leaf())
        get_right_child();

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (is_leaf())
      if (is_right_child())
        get_parent();
      else {
        if (!has_parent())
          before_begin = true;
        else {
          while (has_parent() && !is_right_child())
            get_parent();
          if (has_parent())
            get_parent();
          else
            before_begin = true;
        }
      }
    else {
      get_left_child();
      while (!is_leaf())
        get_right_child();
    }
  }

  return *this;
}

inline bool
CO_Tree::inorder_const_iterator::get_left_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
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
CO_Tree::inorder_const_iterator::get_right_child_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(tree->reserved_size != 0);
  if (is_leaf())
    return false;
  get_right_child();
  if ((*this)->first == unused_index) {
    get_parent();
    return false;
  }
  return true;
}

inline void
CO_Tree::inorder_const_iterator::get_next_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!at_end);
  if (before_begin) {
    get_root();

    if (tree->reserved_size == 0)
      at_end = true;
    else {
      while (get_left_child_value())
        ;

      if ((*this)->first == unused_index) {
        PPL_ASSERT(d == 1);
        at_end = true;
      }
    }

    PPL_ASSERT(at_end || (*this)->first != unused_index);

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
#ifndef NDEBUG
    const dimension_type previous_index = (*this)->first;
#endif
    PPL_ASSERT(previous_index != unused_index);
    if (get_right_child_value())
      while (get_left_child_value())
        ;
    else {
      while (has_parent() && is_right_child())
        get_parent();
      if (!has_parent())
        at_end = true;
      else
        get_parent();
    }

#ifndef NDEBUG
    if (!at_end)
      PPL_ASSERT((*this)->first != unused_index
                 && (*this)->first > previous_index);
#endif
  }
}

inline void
CO_Tree::inorder_const_iterator::get_previous_value() {
  PPL_ASSERT(tree != 0);
  PPL_ASSERT(!before_begin);
  if (at_end) {
    get_root();

    if (tree->reserved_size == 0)
      before_begin = true;
    else
      while (get_right_child_value())
        ;

    PPL_ASSERT(before_begin || (*this)->first != unused_index);

  } else {
    PPL_ASSERT(tree->reserved_size != 0);
    if (!get_left_child_value())
      if (is_right_child())
        get_parent();
      else {
        if (!has_parent())
          before_begin = true;
        else {
          while (has_parent() && !is_right_child())
            get_parent();
          if (has_parent())
            get_parent();
          else
            before_begin = true;
        }
      }
    else
      while (get_right_child_value())
        ;

    PPL_ASSERT(before_begin || (*this)->first != unused_index);
  }
}

inline CO_Tree::inorder_const_iterator&
CO_Tree::inorder_const_iterator
::operator=(const inorder_const_iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0) {
    at_end = itr2.at_end;
    before_begin = itr2.before_begin;
    if (!at_end && !before_begin) {
      d = itr2.d;
      i = itr2.i;
      for (dimension_type i = 1; i <= itr2.d; ++i)
        pos[i] = itr2.pos[i];
    }
  }

  return *this;
}

inline CO_Tree::inorder_const_iterator&
CO_Tree::inorder_const_iterator
::operator=(const inorder_iterator& itr2) {
  tree = itr2.tree;
  if (tree != 0) {
    at_end = itr2.at_end;
    before_begin = itr2.before_begin;
    if (!at_end && !before_begin) {
      d = itr2.d;
      i = itr2.i;
      for (dimension_type i = 1; i <= itr2.d; ++i)
        pos[i] = itr2.pos[i];
    }
  }

  return *this;
}

inline
CO_Tree::unordered_iterator::unordered_iterator(value_type* p1)
  : p(p1) {
}

inline CO_Tree::value_type&
CO_Tree::unordered_iterator::operator*() {
  return *p;
}

inline const CO_Tree::value_type&
CO_Tree::unordered_iterator::operator*() const {
  return *p;
}

inline CO_Tree::value_type*
CO_Tree::unordered_iterator::operator->() {
  return p;
}

inline const CO_Tree::value_type*
CO_Tree::unordered_iterator::operator->() const {
  return p;
}

inline CO_Tree::unordered_iterator&
CO_Tree::unordered_iterator::operator++() {

  ++p;
  while (p->first == unused_index)
    ++p;

  return *this;
}

inline bool
CO_Tree::unordered_iterator::operator==(const unordered_iterator& x) const {
  return p == x.p;
}

inline bool
CO_Tree::unordered_iterator::operator!=(const unordered_iterator& x) const {
  return !(*this == x);
}


inline
CO_Tree::unordered_const_iterator
::unordered_const_iterator(const value_type* p1)
  : p(p1) {
}

inline
CO_Tree::unordered_const_iterator
::unordered_const_iterator(const unordered_iterator& itr)
  : p(itr.operator->()) {
}

inline const CO_Tree::value_type&
CO_Tree::unordered_const_iterator::operator*() const {
  return *p;
}

inline const CO_Tree::value_type*
CO_Tree::unordered_const_iterator::operator->() const {
  return p;
}

inline CO_Tree::unordered_const_iterator&
CO_Tree::unordered_const_iterator::operator++() {
  ++p;
  while (p->first == unused_index)
    ++p;
  return *this;
}

inline bool
CO_Tree::unordered_const_iterator
::operator==(const unordered_const_iterator& x) const {
  return p == x.p;
}

inline bool
CO_Tree::unordered_const_iterator
::operator!=(const unordered_const_iterator& x) const {
  return !(*this == x);
}


} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_CO_Tree_inlines_hh)
