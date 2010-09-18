
/* Sparse_Row class implementation: non-inline template functions.
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

#ifndef PPL_CO_Tree_templates_hh
#define PPL_CO_Tree_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename Iterator>
CO_Tree::CO_Tree(Iterator i, Iterator i_end, dimension_type n) {

  init(n);

  if (n == 0) {
    PPL_ASSERT(i == i_end);
    PPL_ASSERT(OK());
    return;
  }

  tree_iterator root(*this);

  // This is static and with static allocation, to improve performance.
  // CHAR_BIT*sizeof(dimension_type) is the maximum k such that 2^k-1 is a
  // dimension_type, so it is the maximum tree height.
  // For each node level, the stack may contain up to 4 elements: two elements
  // with operation 0, one element with operation 2 and one element
  // with operation 3. An additional element with operation 1 can be at the
  // top of the tree.
  static std::pair<dimension_type,char>
    stack[4*CHAR_BIT*sizeof(dimension_type)+1];

  dimension_type stack_first_empty = 0;

  // A pair (n, operation) in the stack means:
  //
  // * Go to the parent, if operation is 0.
  // * Go to the left child, then fill the current tree with n elements, if
  //   operation is 1.
  // * Go to the right child, then fill the current tree with n elements, if
  //   operation is 2.
  // * Fill the current tree with n elements, if operation is 3.

  stack[0].first = n;
  stack[0].second = 3;
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
    case 3:
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
        PPL_ASSERT(root.index() == unused_index);
        PPL_ASSERT(i != i_end);
        root.index() = i.index();
        new (&(*root)) data_type(*i);
        ++i;
        --stack_first_empty;
      } else {
        PPL_ASSERT(stack_first_empty + 3 < sizeof(stack)/sizeof(stack[0]));

        const dimension_type half = (top_n + 1) / 2;
        stack[stack_first_empty - 1].second = 0;
        stack[stack_first_empty    ] = std::make_pair(top_n - half, 2);
        stack[stack_first_empty + 1] = std::make_pair(1, 3);
        stack[stack_first_empty + 2].second = 0;
        stack[stack_first_empty + 3] = std::make_pair(half - 1, 1);
        stack_first_empty += 4;
      }
    }
  }
  size = n;
  PPL_ASSERT(i == i_end);
  PPL_ASSERT(OK());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_CO_Tree_templates_hh)
