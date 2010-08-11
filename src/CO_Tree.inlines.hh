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
    tree_iterator itr = go_down_searching_key(tree_iterator(*this), key1);
    if (itr->first != key1)
      return iterator(insert_precise(key1, data1, itr));
    else {
      itr->second = data1;
      return iterator(itr);
    }
  }
}

inline CO_Tree::iterator
CO_Tree::insert(dimension_type key1) {
  if (empty())
    return insert(key1, Coefficient_zero());
  else {
    tree_iterator itr = go_down_searching_key(tree_iterator(*this), key1);
    if (itr->first == key1)
      return iterator(itr);
    else
      return iterator(insert_precise(key1, Coefficient_zero(), itr));
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
  if (empty())
    std::cout << "(empty tree)" << std::endl;
  else
    dump_subtree(tree_iterator(*const_cast<CO_Tree*>(this)));
}

inline CO_Tree::tree_iterator
CO_Tree::least_common_ancestor(tree_iterator itr1, tree_iterator itr2) {
  while (itr1.get_offset() > itr2.get_offset())
    itr2.get_parent();
  while (itr2.get_offset() > itr1.get_offset())
    itr1.get_parent();
  // Now itr1 and itr2 have the same depth.
  PPL_ASSERT(itr1.depth() == itr2.depth());
  while (itr1 != itr2) {
    itr1.get_parent();
    itr2.get_parent();
  }
  return itr1;
}

inline CO_Tree::iterator
CO_Tree::erase(dimension_type key) {
  PPL_ASSERT(key != unused_index);

  if (size == 0)
    return end();

  tree_iterator itr = go_down_searching_key(tree_iterator(*this), key);

  if (itr->first != key) {
    iterator result(itr);
    if (result->first < key)
      ++result;
    PPL_ASSERT(result == end() || result->first > key);
#ifndef NDEBUG
    if (!empty()) {
      iterator last = end();
      --last;
      PPL_ASSERT((result == end()) == (last->first < key));
    }
#endif
    return result;
  }

  return erase(itr);
}

inline const CO_Tree::iterator&
CO_Tree::before_begin() {
  return cached_before_begin;
}

inline CO_Tree::iterator
CO_Tree::begin() {
  return iterator(*this);
}

inline const CO_Tree::iterator&
CO_Tree::end() {
  return cached_end;
}

inline const CO_Tree::const_iterator&
CO_Tree::before_begin() const {
  return cached_const_before_begin;
}

inline CO_Tree::const_iterator
CO_Tree::begin() const {
  return const_iterator(*this);
}

inline const CO_Tree::const_iterator&
CO_Tree::end() const {
  return cached_const_end;
}

inline CO_Tree::iterator
CO_Tree::erase(iterator itr) {
  PPL_ASSERT(itr != before_begin());
  PPL_ASSERT(itr != end());
  return erase(tree_iterator(itr, *this));
}

inline CO_Tree::iterator
CO_Tree::bisect_in(iterator first, iterator last, dimension_type key) {
  PPL_ASSERT(first != before_begin());
  PPL_ASSERT(first != end());
  PPL_ASSERT(last != before_begin());
  PPL_ASSERT(last != end());
  dimension_type index = bisect_in(&(first->second) - data,
                                   &(last->second) - data, key);
  return iterator(*this, index);
}

inline CO_Tree::const_iterator
CO_Tree::bisect_in(const_iterator first, const_iterator last,
                   dimension_type key) const {
  PPL_ASSERT(first != before_begin());
  PPL_ASSERT(first != end());
  PPL_ASSERT(last != before_begin());
  PPL_ASSERT(last != end());
  dimension_type index = bisect_in(&(first->second) - data,
                                   &(last->second) - data, key);
  return const_iterator(*this, index);
}

inline CO_Tree::iterator
CO_Tree::bisect_near(iterator hint, dimension_type key) {
  PPL_ASSERT(hint != before_begin());
  PPL_ASSERT(hint != end());
  dimension_type index = bisect_near(&(hint->second) - data, key);
  return iterator(*this, index);
}

inline CO_Tree::const_iterator
CO_Tree::bisect_near(const_iterator hint, dimension_type key) const {
  PPL_ASSERT(hint != before_begin());
  PPL_ASSERT(hint != end());
  dimension_type index = bisect_near(&(hint->second) - data, key);
  return const_iterator(*this, index);
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
CO_Tree::refresh_cached_iterators() {

  cached_before_begin = iterator(*this, 0);
  cached_end = iterator(*this, reserved_size + 1);
  cached_const_before_begin = const_iterator(*this, 0);
  cached_const_end = const_iterator(*this, reserved_size + 1);
}

inline void
CO_Tree::swap(CO_Tree& x) {

  std::swap(max_depth, x.max_depth);
  std::swap(indexes, x.indexes);
  std::swap(data, x.data);
  std::swap(reserved_size, x.reserved_size);
  std::swap(size, x.size);
  // Cached iterators have been invalidated by the swap, they must be
  // refreshed here.
  refresh_cached_iterators();
  x.refresh_cached_iterators();
  PPL_ASSERT(structure_OK());
  PPL_ASSERT(x.structure_OK());
}

inline
CO_Tree::iterator::iterator()
  : current_index(0), current_data(0) {
#ifndef NDEBUG
  tree = 0;
#endif
  PPL_ASSERT(OK());
}

inline
CO_Tree::iterator::iterator(CO_Tree& tree1)
  : current_index(&(tree1.indexes[1])), current_data(&(tree1.data[1])) {
#ifndef NDEBUG
  tree = &tree1;
#endif
  if (tree1.reserved_size != 0)
    while (*current_index == unused_index) {
      ++current_index;
      ++current_data;
    }
  PPL_ASSERT(OK());
}

inline
CO_Tree::iterator::iterator(CO_Tree& tree1, dimension_type i)
  : current_index(&(tree1.indexes[i])), current_data(&(tree1.data[i])) {
#ifndef NDEBUG
  tree = &tree1;
#endif
  PPL_ASSERT(i <= tree1.reserved_size + 1);
  PPL_ASSERT(tree1.empty() || tree1.indexes[i] != unused_index);
  PPL_ASSERT(OK());
}

inline
CO_Tree::iterator::iterator(const iterator& itr2) {
  (*this) = itr2;
  PPL_ASSERT(OK());
}

inline
CO_Tree::iterator::iterator(const tree_iterator& itr) {
  *this = itr;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::iterator::swap(iterator& itr) {
  std::swap(current_data, itr.current_data);
  std::swap(current_index, itr.current_index);
#ifndef NDEBUG
  std::swap(tree, itr.tree);
#endif
  PPL_ASSERT(OK());
  PPL_ASSERT(itr.OK());
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator=(const tree_iterator& itr) {
  current_index = &(itr.tree.indexes[itr.index()]);
  current_data = &(itr.tree.data[itr.index()]);
#ifndef NDEBUG
  tree = &(itr.tree);
#endif
  PPL_ASSERT(OK());
  return *this;
}

inline std::pair<const dimension_type, CO_Tree::data_type&>
CO_Tree::iterator::operator*() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return std::pair<const dimension_type, data_type&>(*current_index,
                                                     *current_data);
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::iterator::operator*() const {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return std::pair<const dimension_type, const data_type&>(*current_index,
                                                           *current_data);
}

inline CO_Tree::iterator::Member_Access_Helper
CO_Tree::iterator::operator->() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return Member_Access_Helper(*current_index, *current_data);
}

inline CO_Tree::iterator::Const_Member_Access_Helper
CO_Tree::iterator::operator->() const {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return Const_Member_Access_Helper(*current_index, *current_data);
}

inline bool
CO_Tree::iterator::operator==(const iterator& x) const {
  PPL_ASSERT((current_index == x.current_index)
             == (current_data == x.current_data));
  PPL_ASSERT(OK());
  return (current_index == x.current_index);
}

inline bool
CO_Tree::iterator::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator++() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  ++current_index;
  ++current_data;
  while (*current_index == unused_index) {
    ++current_index;
    ++current_data;
  }

  PPL_ASSERT(OK());
  return *this;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator--() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
#endif
  --current_index;
  --current_data;
  while (*current_index == unused_index) {
    --current_index;
    --current_data;
  }

  PPL_ASSERT(OK());
  return *this;
}

inline CO_Tree::iterator&
CO_Tree::iterator::operator=(const iterator& itr2) {
  current_index = itr2.current_index;
  current_data = itr2.current_data;
#ifndef NDEBUG
  tree = itr2.tree;
#endif
  PPL_ASSERT(OK());
  return *this;
}


inline
CO_Tree::iterator::Member_Access_Helper
::Member_Access_Helper(const dimension_type key, data_type& data)
  : my_pair(key, data) {
}

inline
std::pair<const dimension_type, CO_Tree::data_type&>*
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
  : current_index(0), current_data(0) {
#ifndef NDEBUG
  tree = 0;
#endif
  PPL_ASSERT(OK());
}

inline
CO_Tree::const_iterator::const_iterator(const CO_Tree& tree1)
  : current_index(&(tree1.indexes[1])), current_data(&(tree1.data[1])) {
#ifndef NDEBUG
  tree = &tree1;
#endif
  if (tree1.reserved_size != 0)
    while (*current_index == unused_index) {
      ++current_index;
      ++current_data;
    }
  PPL_ASSERT(OK());
}

inline
CO_Tree::const_iterator::const_iterator(const CO_Tree& tree1,
                                        dimension_type i)
  : current_index(&(tree1.indexes[i])), current_data(&(tree1.data[i])) {
#ifndef NDEBUG
  tree = &tree1;
#endif
  PPL_ASSERT(i <= tree1.reserved_size + 1);
  PPL_ASSERT(tree1.empty() || tree1.indexes[i] != unused_index);
  PPL_ASSERT(OK());
}

inline
CO_Tree::const_iterator
::const_iterator(const const_iterator& itr2) {
  (*this) = itr2;
  PPL_ASSERT(OK());
}

inline
CO_Tree::const_iterator
::const_iterator(const iterator& itr2) {
  (*this) = itr2;
  PPL_ASSERT(OK());
}

inline void
CO_Tree::const_iterator::swap(const_iterator& itr) {
  std::swap(current_data, itr.current_data);
  std::swap(current_index, itr.current_index);
#ifndef NDEBUG
  std::swap(tree, itr.tree);
#endif
  PPL_ASSERT(OK());
  PPL_ASSERT(itr.OK());
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::const_iterator::operator*() const {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return std::pair<const dimension_type&, const data_type&>(*current_index,
                                                            *current_data);
}

inline CO_Tree::const_iterator::Const_Member_Access_Helper
CO_Tree::const_iterator::operator->() const {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
  PPL_ASSERT(OK());
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
  PPL_ASSERT(current_index != &(tree->indexes[tree->reserved_size + 1]));
#endif
  return Const_Member_Access_Helper(*current_index, *current_data);
}

inline bool
CO_Tree::const_iterator
::operator==(const const_iterator& x) const {
  PPL_ASSERT((current_index == x.current_index)
             == (current_data == x.current_data));
  PPL_ASSERT(OK());
  return (current_index == x.current_index);
}

inline bool
CO_Tree::const_iterator
::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator++() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
#endif
  ++current_index;
  ++current_data;
  while (*current_index == unused_index) {
    ++current_index;
    ++current_data;
  }
  PPL_ASSERT(OK());
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator--() {
  PPL_ASSERT(current_index != 0);
  PPL_ASSERT(current_data != 0);
#ifndef NDEBUG
  PPL_ASSERT(current_index != &(tree->indexes[0]));
#endif
  --current_index;
  --current_data;
  while (*current_index == unused_index) {
    --current_index;
    --current_data;
  }
  PPL_ASSERT(OK());
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator=(const const_iterator& itr2) {
  current_index = itr2.current_index;
  current_data = itr2.current_data;
#ifndef NDEBUG
  tree = itr2.tree;
#endif
  PPL_ASSERT(OK());
  return *this;
}

inline CO_Tree::const_iterator&
CO_Tree::const_iterator::operator=(const iterator& itr2) {
  current_index = itr2.current_index;
  current_data = itr2.current_data;
#ifndef NDEBUG
  tree = itr2.tree;
#endif
  PPL_ASSERT(OK());
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
  : tree(tree1) {
  PPL_ASSERT(tree.reserved_size != 0);
  get_root();
  PPL_ASSERT(OK());
}

inline
CO_Tree::tree_iterator::tree_iterator(const iterator& itr, CO_Tree& tree1)
  : tree(tree1) {
  PPL_ASSERT(tree.reserved_size != 0);
  *this = itr;
  PPL_ASSERT(OK());
}

inline CO_Tree::tree_iterator&
CO_Tree::tree_iterator::operator=(const tree_iterator& itr) {
  PPL_ASSERT(&tree == &(itr.tree));
  i = itr.i;
  offset = itr.offset;
  return *this;
}

inline CO_Tree::tree_iterator&
CO_Tree::tree_iterator::operator=(const iterator& itr) {
  PPL_ASSERT(itr != tree.before_begin());
  PPL_ASSERT(itr != tree.end());
  i = &(itr->second) - tree.data;
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
  return tree.data + i == &(itr->second);
}

inline bool
CO_Tree::tree_iterator::operator!=(const iterator& itr) const {
  return !(*this == itr);
}

inline void
CO_Tree::tree_iterator::get_root() {
  i = tree.reserved_size / 2 + 1;
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
  PPL_ASSERT(!is_root());
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
  dimension_type* p = tree.indexes;
  p += i;
  p -= (offset - 1);
  while (*p == unused_index)
    ++p;
  i = p - tree.indexes;
  offset = i & -i;
}

inline void
CO_Tree::tree_iterator::follow_right_childs_with_value() {
  PPL_ASSERT((*this)->first != unused_index);
  dimension_type* p = tree.indexes;
  p += i;
  p += (offset - 1);
  while (*p == unused_index)
    --p;
  i = p - tree.indexes;
  offset = i & -i;
}

inline bool
CO_Tree::tree_iterator::is_root() const {
  // This is implied by OK(), it is here for reference only.
  PPL_ASSERT(offset <= (tree.reserved_size / 2 + 1));
  return offset == (tree.reserved_size / 2 + 1);
}

inline bool
CO_Tree::tree_iterator::is_leaf() const {
  return offset == 1;
}

inline bool
CO_Tree::tree_iterator::is_right_child() const {
  if (is_root())
    return false;
  return ((i & 2*offset) != 0);
}

inline std::pair<dimension_type&, CO_Tree::data_type&>
CO_Tree::tree_iterator::operator*() {
  return std::pair<dimension_type&, data_type&>(tree.indexes[i],
                                                tree.data[i]);
}

inline std::pair<const dimension_type, const CO_Tree::data_type&>
CO_Tree::tree_iterator::operator*() const {
  return std::pair<const dimension_type&, const data_type&>(tree.indexes[i],
                                                            tree.data[i]);
}

inline CO_Tree::tree_iterator::Member_Access_Helper
CO_Tree::tree_iterator::operator->() {
  return Member_Access_Helper(tree.indexes[i], tree.data[i]);
}

inline CO_Tree::tree_iterator::Const_Member_Access_Helper
CO_Tree::tree_iterator::operator->() const {
  return Const_Member_Access_Helper(tree.indexes[i], tree.data[i]);
}

inline dimension_type
CO_Tree::tree_iterator::index() const {
  return i;
}

inline dimension_type
CO_Tree::tree_iterator::get_offset() const {
  return offset;
}

inline CO_Tree::height_t
CO_Tree::tree_iterator::depth() const {
  return integer_log2((tree.reserved_size + 1) / offset);
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


namespace std {

inline void
swap(Parma_Polyhedra_Library::CO_Tree& x,
     Parma_Polyhedra_Library::CO_Tree& y) {
  x.swap(y);
}

inline void
swap(Parma_Polyhedra_Library::CO_Tree::const_iterator& x,
     Parma_Polyhedra_Library::CO_Tree::const_iterator& y) {
  x.swap(y);
}

inline void
swap(Parma_Polyhedra_Library::CO_Tree::iterator& x,
     Parma_Polyhedra_Library::CO_Tree::iterator& y) {
  x.swap(y);
}

} // namespace std


#endif // !defined(PPL_CO_Tree_inlines_hh)