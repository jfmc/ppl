/* CO_Tree class implementation
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

#include <ppl-config.h>
#include "CO_Tree.defs.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

PPL::CO_Tree::CO_Tree(const std::vector<data_type>& v) {

  dimension_type n = 0;

  for (dimension_type i = 0; i < v.size(); ++i)
    if (v[i] != 0)
      ++n;

  init(n);

  if (n == 0) {
    PPL_ASSERT(OK());
    return;
  }

  tree_iterator root(*this);
  dimension_type index = 0;

  while (v[index] == 0) {
    ++index;
    PPL_ASSERT(index < v.size());
  }

  // This is static and with static allocation, to improve performance.
  static std::pair<dimension_type,char> stack[5*CHAR_BIT*sizeof(dimension_type)];
  dimension_type stack_first_empty = 0;

  // A pair (n, operation) in the stack means:
  //
  // * Go to the parent, if operation is 0.
  // * Go to the left child, then fill the current tree with n elements, if
  //   operation is 1.
  // * Go to the right child, then visit the current tree with n elements, if
  //   operation is 2.
  // * Fill the current tree with n elements, if operation is 4.

  stack[0].first = n;
  stack[0].second = 4;
  ++stack_first_empty;

  while (stack_first_empty != 0) {

    // top_n         = stack.top().first;
    // top_operation = stack.top().second;
    const dimension_type top_n = stack[stack_first_empty - 1].first;
    const char top_operation = stack[stack_first_empty - 1].second;

    switch (top_operation) {

    case 0:
      root.get_parent();
      --stack_first_empty;
      continue;

    case 1:
      root.get_left_child();
      break;

    case 2:
      root.get_right_child();
      break;
#ifndef NDEBUG
    case 4:
      break;

    default:
      // We should not be here
      PPL_ASSERT(false);
#endif
    }

    // We now visit the current tree

    if (top_n == 0) {
      --stack_first_empty;
    } else {
      if (top_n == 1) {
        PPL_ASSERT(root->first == unused_index);
        PPL_ASSERT(index < v.size());
        root->first = index;
        root->second = v[index];
        ++index;
        while (index < v.size() && v[index] == 0)
          ++index;
        --stack_first_empty;
      } else {
        PPL_ASSERT(stack_first_empty + 3 < sizeof(stack)/sizeof(stack[0]));

        const dimension_type half = (top_n + 1) / 2;
        stack[stack_first_empty - 1].second = 0;
        stack[stack_first_empty    ] = std::make_pair(top_n - half, 2);
        stack[stack_first_empty + 1] = std::make_pair(1, 4);
        stack[stack_first_empty + 2].second = 0;
        stack[stack_first_empty + 3] = std::make_pair(half - 1, 1);
        stack_first_empty += 4;
      }
    }
  }
  size = n;
  PPL_ASSERT(OK());
}

void
PPL::CO_Tree::copy_data_from(const CO_Tree& x) {

  PPL_ASSERT(size == 0);
  PPL_ASSERT(reserved_size == x.reserved_size);

  if (x.size == 0) {
    PPL_ASSERT(OK());
    return;
  }

  for (dimension_type i = x.reserved_size; i > 0; --i)
    if (x.indexes[i] != unused_index) {
      indexes[i] = x.indexes[i];
      new (&(data[i])) data_type(x.data[i]);
    } else
      PPL_ASSERT(indexes[i] == unused_index);

  size = x.size;
  PPL_ASSERT(OK());
}

void
PPL::CO_Tree::init(dimension_type reserved_size1) {

  if (reserved_size1 == 0) {
    indexes = NULL;
    data = NULL;
    size = 0;
    reserved_size = 0;
    max_depth = 0;
    return;
  }

  max_depth = integer_log2(reserved_size1) + 1;

  size = 0;
  reserved_size = ((dimension_type)1 << max_depth) - 1;
  indexes = new dimension_type[reserved_size + 2];
  data = static_cast<data_type*>(operator new(sizeof(data_type)
                                              * (reserved_size + 1)));
  // Mark all pairs as unused.
  for (dimension_type i = 1; i <= reserved_size; ++i)
    indexes[i] = unused_index;

  // These are used as markers by iterators.
  indexes[0] = 0;
  indexes[reserved_size + 1] = 0;

  PPL_ASSERT(structure_OK());
}

void
PPL::CO_Tree::destroy() {

  if (reserved_size != 0) {
    for (dimension_type i = 1; i <= reserved_size; ++i) {
      if (indexes[i] != unused_index)
        data[i].~data_type();
    }

    delete [] indexes;
    operator delete(static_cast<void*>(data));
  }
}

void
PPL::CO_Tree::move_data_from(CO_Tree& tree) {
  PPL_ASSERT(size == 0);
  if (tree.size == 0)
    return;

  tree_iterator root(*this);

  dimension_type source_index = 1;
  while (tree.indexes[source_index] == unused_index)
    ++source_index;

  // This is static and with static allocation, to improve performance.
  static std::pair<dimension_type,char> stack[5*CHAR_BIT*sizeof(dimension_type)];
  dimension_type stack_first_empty = 0;

  // A pair (n, operation) in the stack means:
  //
  // * Go to the parent, if operation is 0.
  // * Go to the left child, then visit the current tree (with size n), if
  //   operation is 1.
  // * Go to the right child, then visit the current tree (with size n), if
  //   operation is 2.
  // * Visit the current tree (with size n), if operation is 4.

  stack[0].first = tree.size;
  stack[0].second = 4;
  ++stack_first_empty;

  while (stack_first_empty != 0) {

    // top_n         = stack.top().first;
    // top_operation = stack.top().second;
    const dimension_type top_n = stack[stack_first_empty - 1].first;
    const char top_operation = stack[stack_first_empty - 1].second;

    switch (top_operation) {

    case 0:
      root.get_parent();
      --stack_first_empty;
      continue;

    case 1:
      root.get_left_child();
      break;

    case 2:
      root.get_right_child();
      break;
#ifndef NDEBUG
    case 4:
      break;

    default:
      // We should not be here
      PPL_ASSERT(false);
#endif
    }

    // We now visit the current tree

    if (top_n == 0) {
      --stack_first_empty;
    } else {
      if (top_n == 1) {
        PPL_ASSERT(root->first == unused_index);
        PPL_ASSERT(tree.indexes[source_index] != unused_index);
        root->first = tree.indexes[source_index];
        tree.indexes[source_index] = unused_index;
        move_data_element(root->second, tree.data[source_index]);
        PPL_ASSERT(source_index <= tree.reserved_size);
        ++source_index;
        while (tree.indexes[source_index] == unused_index)
          ++source_index;
        --stack_first_empty;
      } else {
        PPL_ASSERT(stack_first_empty + 3 < sizeof(stack)/sizeof(stack[0]));

        const dimension_type half = (top_n + 1) / 2;
        stack[stack_first_empty - 1].second = 0;
        stack[stack_first_empty    ] = std::make_pair(top_n - half, 2);
        stack[stack_first_empty + 1] = std::make_pair(1, 4);
        stack[stack_first_empty + 2].second = 0;
        stack[stack_first_empty + 3] = std::make_pair(half - 1, 1);
        stack_first_empty += 4;
      }
    }
  }
  size = tree.size;
  tree.size = 0;
  PPL_ASSERT(tree.structure_OK());
}

bool
PPL::CO_Tree::OK() const {

  if (!structure_OK())
    return false;

  {
    const_iterator itr = before_begin();
    const_iterator itr_end = end();
    dimension_type real_size = 0;

    itr.get_next_value();

    for ( ; itr != itr_end; itr.get_next_value())
        ++real_size;

    if (real_size != size)
      // There are \p real_size elements in the tree, but size is \p size.
      return false;
  }

  if (reserved_size > 0) {
    const float density
      = size / (float) (((dimension_type)1 << max_depth) - 1);
    if (density > max_density && density != 3)
      // Found too high density.
      return false;
    if (density < min_density) {
      // This is the density we could achieve by calling rebuild_bigger_tree.
      const float candidate_density
        = size / (float) (((dimension_type)1 << (max_depth-1)) - 1);
      if (candidate_density <= max_density)
        // Found too low density
        return false;
    }
  }

  return true;
}

bool
PPL::CO_Tree::structure_OK() const {

  if (size > reserved_size)
    return false;

  if (reserved_size == 0) {
    if (indexes != NULL)
      return false;
    if (data != NULL)
      return false;
    if (max_depth != 0)
      return false;

    return true;
  }

  if (reserved_size < 3)
    return false;

  if (data == NULL)
    return false;

  if (indexes == NULL)
    return false;

  if (max_depth == 0)
    return false;

  if (size == 0) {

    // This const_cast could be removed by adding a const_tree_iterator,
    // but it would add much code duplication without a real need.
    tree_iterator itr(*const_cast<CO_Tree*>(this));
    if (itr->first != unused_index)
      return false;

  } else {
    // This const_cast could be removed by adding a const_tree_iterator,
    // but it would add much code duplication without a real need.
    tree_iterator itr(*const_cast<CO_Tree*>(this));
    dimension_type real_size = count_used_in_subtree(itr);
    if (real_size != size)
      // There are \p real_size elements in the tree that are reachable by the
      // root, but size is \p size.
      return false;
  }

  if (size != 0) {
    const_iterator itr = before_begin();
    const_iterator itr_end = end();

    itr.get_next_value();

    if (itr != itr_end) {
      dimension_type last_index = itr->first;
      for (itr.get_next_value(); itr != itr_end; itr.get_next_value()) {
        if (last_index >= itr->first)
          // Found index \p itr->first after index \p last_index.
          return false;
        last_index = itr->first;
      }
    }
  }

  return true;
}

void
PPL::CO_Tree::insert_in_empty_tree(dimension_type key1, const data_type& data1) {
  PPL_ASSERT(empty());
  rebuild_bigger_tree();
  tree_iterator itr(*this);
  PPL_ASSERT(itr->first == unused_index);
  itr->first = key1;
  new (&(itr->second)) data_type(data1);
  size++;

  PPL_ASSERT(OK());
}

void
PPL::CO_Tree::insert_precise(dimension_type key1, const data_type& data1,
                             tree_iterator& itr) {
  PPL_ASSERT(key1 != unused_index);
  PPL_ASSERT(!empty());

#ifndef NDEBUG
  tree_iterator itr2(*this);
  go_down_searching_key(itr2, key1);
  PPL_ASSERT(itr == itr2);
#endif

  if (itr->first == key1) {
    itr->second = data1;
    PPL_ASSERT(OK());
    return;
  }

  if ((size + 1) / (float) (((dimension_type)1 << max_depth) - 1)
    > max_density) {

    rebuild_bigger_tree();

    // itr was invalidated by the rebuild operation
    itr.get_root();
    go_down_searching_key(itr, key1);

    PPL_ASSERT(itr->first != key1);
  }

  PPL_ASSERT((size + 1) / (float) (((dimension_type)1 << max_depth)-1)
         <= max_density);

  if (!itr.is_leaf()) {
    if (key1 < itr->first)
      itr.get_left_child();
    else
      itr.get_right_child();
    PPL_ASSERT(itr->first == unused_index);
    itr->first = key1;
    new (&(itr->second)) data_type(data1);
    size++;

  } else {

    PPL_ASSERT(OK());

    rebalance(itr, key1, data1);

    go_down_searching_key(itr, key1);

    PPL_ASSERT(itr->first == key1);
  }
  PPL_ASSERT(OK());
}

void
PPL::CO_Tree::rebalance(tree_iterator& itr, dimension_type key,
                        const data_type& value) {
#ifndef NDEBUG
  if (itr->first != unused_index) {
    PPL_ASSERT(!itr.get_left_child_value());
    PPL_ASSERT(!itr.get_right_child_value());
  }
#endif
  height_t itr_depth_minus_1 = itr.depth() - 1;
  height_t height = max_depth - itr_depth_minus_1;
  dimension_type subtree_size;
  const bool deleting = itr->first == unused_index;
  PPL_ASSERT(deleting || key != unused_index);
  if (deleting)
    subtree_size = 0;
  else
    // The existing element and the element with index key we want to add.
    subtree_size = 2;

  float density = subtree_size / (float) (((dimension_type)1 << height) - 1);
  const float coeff1 = (1 - max_density)/(max_depth - 1);
  const float coeff2 = (min_density - min_leaf_density)/(max_depth - 1);

  while (density > max_density + itr_depth_minus_1*coeff1
         || density < min_density - itr_depth_minus_1*coeff2) {
    if (itr_depth_minus_1 == 0) {
      // TODO: Check if this is correct
      // We could arrive here because we are using a fake subtree_size.
#ifndef NDEBUG
      dimension_type real_subtree_size = subtree_size;
      if (!deleting)
        --real_subtree_size;
      float real_density
        = real_subtree_size / (float) (((dimension_type)1 << height) - 1);
      PPL_ASSERT(real_density <= max_density);
      if (real_density > min_density) {
        const float candidate_density
          = real_subtree_size
            / (float) (((dimension_type)1 << (height-1)) - 1);
        PPL_ASSERT(candidate_density > max_density);
      }
#endif
      break;
    }
    bool is_right_brother = itr.is_right_child();
    itr.get_parent();
    if (is_right_brother) {
      if (itr.get_left_child_value()) {
        subtree_size += count_used_in_subtree(itr);
        itr.get_parent();
      }
    } else {
      if (itr.get_right_child_value()) {
        subtree_size += count_used_in_subtree(itr);
        itr.get_parent();
      }
    }
    PPL_ASSERT(itr->first != unused_index);
    ++subtree_size;
    ++height;
    --itr_depth_minus_1;
    PPL_ASSERT(itr.depth() - 1 == itr_depth_minus_1);
    density = subtree_size / (float) (((dimension_type)1 << height) - 1);
  };

  redistribute_elements_in_subtree(itr, subtree_size, deleting, key, value);

  PPL_ASSERT(OK());
}

void
PPL::CO_Tree::erase(tree_iterator itr) {
  PPL_ASSERT(itr->first != unused_index);

  PPL_ASSERT(size != 0);

  if (size == 1) {
    // Deleting the only element of this tree, now it is empty.
    destroy();
    init(0);
    return;
  }

  if ((size - 1) / (float) (((dimension_type)1 << max_depth) - 1)
      < min_density
      && (size - 1) / (float) (((dimension_type)1 << (max_depth-1)) - 1)
      <= max_density) {
    const dimension_type key = itr->first;

    PPL_ASSERT(size <= (((dimension_type)1 << max_depth) - 1)*max_density);

    rebuild_smaller_tree();
    itr.get_root();
    go_down_searching_key(itr, key);

    PPL_ASSERT(itr->first == key);
  }

#ifndef NDEBUG
  if (size > 1)
    if ((size - 1) / (float) (((dimension_type)1 << max_depth) - 1)
        < min_density)
      PPL_ASSERT((size - 1)
                 / (float) (((dimension_type)1 << (max_depth-1)) - 1)
                 > max_density);
#endif

  itr->second.~data_type();
  while (1) {
    dimension_type& current_key  = itr->first;
    data_type&      current_data = itr->second;
    if (itr.get_right_child_value()) {
      itr.follow_left_childs_with_value();
    } else
      if (itr.get_left_child_value()) {
        itr.follow_right_childs_with_value();
      } else
        break;
    std::swap(current_key, itr->first);
    move_data_element(current_data, itr->second);
  }

  PPL_ASSERT(itr->first != unused_index);
  itr->first = unused_index;
  --size;

  PPL_ASSERT(OK());

  static const data_type data = data_type();

  rebalance(itr, 0, data);

  PPL_ASSERT(OK());
}

PPL::dimension_type
PPL::CO_Tree::count_used_in_subtree(tree_iterator& itr) {
  PPL_ASSERT(itr->first != unused_index);
  dimension_type n = 0;

  const dimension_type k = itr.get_offset();
  const dimension_type root_index = itr.index();

  // The complete subtree rooted at itr has 2*k - 1 nodes.

  const dimension_type limit = root_index + (k - 1);

  PPL_ASSERT(root_index > (k - 1));

  const dimension_type* indexes = itr.tree->indexes;

  for (dimension_type j = root_index - (k - 1); j <= limit; ++j)
    if (indexes[j] != unused_index)
      ++n;

  return n;
}

void
PPL::CO_Tree::redistribute_elements_in_subtree(tree_iterator& itr,
                                               dimension_type n,
                                               bool deleting,
                                               dimension_type key,
                                               const data_type& value) {
  // Step 1: compact elements of this subtree in the rightmost end, from right
  //         to left.
  dimension_type last_index_in_subtree = itr.index() + itr.get_offset() - 1;

  dimension_type first_unused
    = compact_elements_in_the_rightmost_end(last_index_in_subtree, n, key,
                                            value, !deleting);

  // Step 2: redistribute the elements, from left to right.
  redistribute_elements_in_subtree_helper(itr.index(), n, first_unused + 1,
                                          key, value,
                                          first_unused != last_index_in_subtree - n);

  if (!deleting)
    size++;

  PPL_ASSERT(structure_OK());
}

PPL::dimension_type
PPL::CO_Tree
::compact_elements_in_the_rightmost_end(dimension_type last_in_subtree,
                                        dimension_type subtree_size,
                                        dimension_type key,
                                        const data_type& value,
                                        bool add_element) {

  if (subtree_size == 0)
    return last_in_subtree;

  if (subtree_size == 1 && add_element) {
    // Just add the requested element.
    PPL_ASSERT(indexes[last_in_subtree] == unused_index);
    indexes[last_in_subtree] = key;
    new (&(data[last_in_subtree])) data_type(value);
    return last_in_subtree - 1;
  }

  dimension_type first_unused_index = last_in_subtree;
  while (indexes[last_in_subtree] == unused_index)
    --last_in_subtree;

  // From now on, last_in_subtree points to the rightmost node with a value in
  // the subtree and first_unused_index points to the rightmost unused node in
  // the subtree.

  if (add_element)
    while (subtree_size != 0) {
      --subtree_size;
      if (last_in_subtree == 0 || key > indexes[last_in_subtree]) {
        if (last_in_subtree == 0 || last_in_subtree != first_unused_index) {
          PPL_ASSERT(first_unused_index != 0);
          PPL_ASSERT(indexes[first_unused_index] == unused_index);
          indexes[first_unused_index] = key;
          new (&(data[first_unused_index])) data_type(value);
          --first_unused_index;
        }
        break;
      } else {
        if (last_in_subtree != first_unused_index) {
          PPL_ASSERT(first_unused_index != 0);
          PPL_ASSERT(last_in_subtree != 0);
          PPL_ASSERT(indexes[first_unused_index] == unused_index);
          indexes[first_unused_index] = indexes[last_in_subtree];
          indexes[last_in_subtree] = unused_index;
          move_data_element(data[first_unused_index], data[last_in_subtree]);
        }
        --last_in_subtree;
        while (indexes[last_in_subtree] == unused_index)
          --last_in_subtree;
        --first_unused_index;
      }
    }
  while (subtree_size != 0) {
    if (last_in_subtree != first_unused_index) {
      PPL_ASSERT(first_unused_index != 0);
      PPL_ASSERT(last_in_subtree != 0);
      PPL_ASSERT(indexes[first_unused_index] == unused_index);
      indexes[first_unused_index] = indexes[last_in_subtree];
      indexes[last_in_subtree] = unused_index;
      move_data_element(data[first_unused_index], data[last_in_subtree]);
    }
    --last_in_subtree;
    while (indexes[last_in_subtree] == unused_index)
      --last_in_subtree;
    --first_unused_index;
    --subtree_size;
  }

  return first_unused_index;
}

void
PPL::CO_Tree
::redistribute_elements_in_subtree_helper(dimension_type root_index,
                                          dimension_type subtree_size,
                                          dimension_type last_used,
                                          dimension_type key,
                                          const data_type& value,
                                          bool add_element) {
  // This is static and with static allocation, to improve performance.
  static std::pair<dimension_type,dimension_type> stack[2*CHAR_BIT*sizeof(dimension_type)];
  std::pair<dimension_type,dimension_type>* stack_first_empty = stack;

  // A pair (n, i) in the stack means to visit the subtree with root index i
  // and size n.

  if (subtree_size == 0)
    return;

  stack_first_empty->first  = subtree_size;
  stack_first_empty->second = root_index;
  ++stack_first_empty;

  while (stack_first_empty != stack) {

    --stack_first_empty;

    // top_n = stack.top().first;
    // top_i = stack.top().second;
    const dimension_type top_n = stack_first_empty->first;
    const dimension_type top_i = stack_first_empty->second;

    PPL_ASSERT(top_n != 0);
    if (top_n == 1) {
      if (add_element && (last_used > reserved_size || indexes[last_used] > key)) {
        PPL_ASSERT(last_used != top_i);
        PPL_ASSERT(indexes[top_i] == unused_index);
        add_element = false;
        indexes[top_i] = key;
        new (&(data[top_i])) data_type(value);
      } else {
        if (last_used != top_i) {
          PPL_ASSERT(indexes[top_i] == unused_index);
          indexes[top_i] = indexes[last_used];
          indexes[last_used] = unused_index;
          move_data_element(data[top_i], data[last_used]);
        }
        ++last_used;
      }
    } else {
      PPL_ASSERT(stack_first_empty + 2
                 < stack + sizeof(stack)/sizeof(stack[0]));

      const dimension_type offset = (top_i & -top_i) / 2;
      const dimension_type half = (top_n + 1) / 2;

      PPL_ASSERT(half > 0);

      // Right subtree
      PPL_ASSERT(top_n - half > 0);
      stack_first_empty->first  = top_n - half;
      stack_first_empty->second = top_i + offset;
      ++stack_first_empty;

      // Root of the current subtree
      stack_first_empty->first   = 1;
      stack_first_empty->second  = top_i;
      ++stack_first_empty;

      // Left subtree
      if (half - 1 != 0) {
        stack_first_empty->first   = half - 1;
        stack_first_empty->second  = top_i - offset;
        ++stack_first_empty;
      }
    }
  }

  PPL_ASSERT(!add_element);
}
