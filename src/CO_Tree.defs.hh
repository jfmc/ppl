/* CO_Tree class declaration.
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

#ifndef PPL_CO_Tree_defs_hh
#define PPL_CO_Tree_defs_hh 1

#include "Coefficient.defs.hh"
#include <vector>
#include <stdint.h>

#ifndef USE_PPL_CO_TREE_VEB_LAYOUT
#ifndef USE_PPL_CO_TREE_DFS_LAYOUT
#ifndef USE_PPL_CO_TREE_BFS_LAYOUT

// The default layout is VeB.
#define USE_PPL_CO_TREE_VEB_LAYOUT

#endif // !defined(USE_PPL_CO_TREE_BFS_LAYOUT)
#endif // !defined(USE_PPL_CO_TREE_DFS_LAYOUT)
#endif // !defined(USE_PPL_CO_TREE_VEB_LAYOUT)

#ifdef USE_PPL_CO_TREE_VEB_LAYOUT

#undef USE_PPL_CO_TREE_DFS_LAYOUT
#undef USE_PPL_CO_TREE_BFS_LAYOUT
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT)

#ifdef USE_PPL_CO_TREE_DFS_LAYOUT

#undef USE_PPL_CO_TREE_BFS_LAYOUT
#endif // defined(USE_PPL_CO_TREE_DFS_LAYOUT)

namespace Parma_Polyhedra_Library {

class CO_Tree {

private:
  typedef dimension_type height_t;

public:

  typedef Coefficient data_type;
  typedef std::pair<dimension_type, data_type> value_type;

  class inorder_iterator;
  class inorder_const_iterator;
  class unordered_iterator;
  class unordered_const_iterator;

  CO_Tree();
  CO_Tree(const std::vector<data_type>& v);
  CO_Tree(const CO_Tree& v);

  CO_Tree& operator=(const CO_Tree& x);

  ~CO_Tree();

  //! Returns \p true if the tree has no elements.
  bool empty() const;

  //! Checks the invariants.
  bool OK() const;

  //! Dumps the tree to stdout, for debugging purposes.
  void dump_tree() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  dimension_type external_memory_in_bytes() const;

  //! Inserts the pair (key, data) in the tree.
  void insert(dimension_type key, const data_type& data);

  //! Inserts a pair with key \p key in the tree and modifies \p itr to point
  //! to the inserted pair.
  //! If such a pair already exists, itr is modified to point at that pair.
  void insert(dimension_type key, inorder_iterator& itr);

  //! Inserts the pair (key, data) in the tree.
  //! \p itr is modified to point to the inserted element.
  void insert(dimension_type key, const data_type& data,
              inorder_iterator& itr);

  //! Inserts the pair (key, data) in the tree.
  //! \p itr is used as hint and then modified to point to the inserted
  //! element.
  void insert_hint(dimension_type key, const data_type& data,
                   inorder_iterator& itr);

  //! Inserts a pair with key \p key in the tree.
  //! \p itr is used as hint and then modified to point to the inserted
  //! element.
  void insert_hint(dimension_type key, inorder_iterator& itr);

  //! Inserts the pair (key1, data1) in the tree.
  //! \p itr must be the lower bound of \p key in the tree.
  //! \p itr is modified to point to the inserted element.
  void insert_precise(dimension_type key1, const data_type& data1,
                      inorder_iterator& itr);

  //! Erases the pair with key \p key from the tree.
  //! Returns \p false if there was no pair with key \p key in the tree.
  bool erase(dimension_type key);

  //! Erases from the tree the element pointed to by \p itr .
  //! \p itr is invalidated, it is passed by reference to improve performance.
  void erase(inorder_iterator& itr);

  //! Swaps x with *this.
  void swap(CO_Tree& x);

  //! Returns an iterator that points before the first element.
  inorder_iterator before_begin();

  //! Returns an iterator that points after the last element.
  inorder_iterator end();

  //! Returns an iterator that points before the first element.
  inorder_const_iterator before_begin() const;

  //! Returns an iterator that points after the last element.
  inorder_const_iterator end() const;

  //! Returns an unordered_iterator pointing to the first (used) element in
  //! the tree.
  unordered_iterator unordered_begin();
  //! Returns an unordered_iterator pointing after the last element in
  //! the tree.
  unordered_iterator unordered_end();

  //! Returns an unordered_const_iterator pointing to the first (used) element
  //! in the tree.
  unordered_const_iterator unordered_begin() const;
  //! Returns an unordered_const_iterator pointing after the last element
  //! in the tree.
  unordered_const_iterator unordered_end() const;

  //! Searches for an element with key \p key in the subtree rooted at \p itr.
  //! \p itr is modified to point to the found node (if it exists) or to the
  //! node that would be his parent (otherwise).
  void lower_bound(inorder_iterator& itr, dimension_type key);

  //! Searches for an element with key \p key in the subtree rooted at \p itr.
  //! \p itr is modified to point to the found node (if it exists) or to the
  //! node that would be his parent (otherwise).
  void lower_bound(inorder_const_iterator& itr, dimension_type key) const;

  class inorder_iterator;
  class inorder_const_iterator;

  class unordered_const_iterator;

  class unordered_iterator {

  public:

    typedef std::forward_iterator_tag iterator_category;
    typedef CO_Tree::value_type value_type;
    typedef ptrdiff_t difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;

    class Member_Access_Helper {

    public:
      Member_Access_Helper(dimension_type& key, data_type& data);

      std::pair<dimension_type&, data_type&>* operator->();

    private:
      std::pair<dimension_type&, data_type&> my_pair;
    };

    class Const_Member_Access_Helper {

    public:
      Const_Member_Access_Helper(dimension_type key, const data_type& data);

      const std::pair<const dimension_type, const data_type&>* operator->() const;

    private:
      std::pair<const dimension_type, const data_type&> my_pair;
    };

    unordered_iterator(CO_Tree* p1 = 0, dimension_type i = 0);

    std::pair<dimension_type&, data_type&> operator*();
    std::pair<const dimension_type, const data_type&> operator*() const;

    Member_Access_Helper operator->();
    Const_Member_Access_Helper operator->() const;

    unordered_iterator& operator++();

    bool operator==(const unordered_iterator&) const;
    bool operator!=(const unordered_iterator&) const;

  private:

    CO_Tree* p;
    dimension_type i;

    friend class unordered_const_iterator;
  };

  class unordered_const_iterator {

  public:

    typedef std::forward_iterator_tag iterator_category;
    typedef const CO_Tree::value_type value_type;
    typedef ptrdiff_t difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;

    class Const_Member_Access_Helper {

    public:
      Const_Member_Access_Helper(dimension_type key, const data_type& data);

      const std::pair<const dimension_type, const data_type&>* operator->()
        const;

    private:
      std::pair<const dimension_type, const data_type&> my_pair;
    };

    unordered_const_iterator(const CO_Tree* p1 = 0, dimension_type i = 0);
    unordered_const_iterator(const unordered_iterator& itr);

    std::pair<const dimension_type, const data_type&> operator*() const;

    Const_Member_Access_Helper operator->() const;

    unordered_const_iterator& operator++();

    bool operator==(const unordered_const_iterator&) const;
    bool operator!=(const unordered_const_iterator&) const;

  private:

    const CO_Tree* p;
    dimension_type i;
  };

private:

  struct level_data {
    dimension_type bottom_tree_size;
    dimension_type top_tree_size;
    height_t depth_of_root_of_top_tree;
  };

#ifdef USE_PPL_CO_TREE_VEB_LAYOUT
  class Level_Data_Cache {

  public:

    static const level_data* get_level_data(height_t height);

  private:

    static const height_t max_depth = sizeof(dimension_type)*CHAR_BIT;

    static void fill_level_data(level_data* p, height_t min_depth,
                                height_t max_depth);

    //! cache[0] and cache[1] are not used.
    //! cache[i] contains NULL if get_level_data(i) has not been called yet,
    //! otherwise contains a pointer to an array of i elements containing
    //! the level_data elements for trees of height i (cache[i][0] is not
    //! used).
    //! The level data is shared between CO_Tree objects with the same height.
    //! This memory is never freed.
    //! This is static, so the compiler initializes the pointers with NULLs.
    static level_data* cache[max_depth];
  };
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT)

  //! Initializes a tree with reserved size at least \p n .
  void init(dimension_type n);

  //! Deallocates the tree. After this call, init() can be called again.
  void destroy();

  //! Checks the invariant, but not the densities.
  bool structure_OK() const;

  //! Dumps the subtree rooted at \p itr to stdout, for debugging purposes.
  //! itr is not modified, it is passed by reference to improve performance.
  static void dump_subtree(inorder_iterator& itr);

  //! Dumps the subtree rooted at \p itr to stdout, for debugging purposes.
  //! itr is not modified, it is passed by reference to improve performance.
  static void dump_subtree(inorder_const_iterator& itr);

  //! Increases the tree's reserved size. Called when the density is about to
  //! exceed max_density.
  void rebuild_bigger_tree();

  //! Decreases the tree's reserved size. Called when the density is about to
  //! become less than min_density.
  void rebuild_smaller_tree();

  //! Rebalances the tree after an insertions or a deletion.
  //! \p itr points to the inserted (or deleted) node.
  //! For insertions, it adds the pair (key, value).
  //! After the call, itr is modified so the added node is in the subtree
  //! pointed to by \p itr.
  void rebalance(inorder_iterator& itr, dimension_type key,
                 const data_type& value);

  //! Redistributes the elements in the subtree rooted at the node
  //! pointed to by itr. If \p deleting is not \p false, it adds the pair
  //! (key, value) to the tree.
  //! \p subtree_size is the number of used elements in the subtree at the end
  //! of the call.
  void redistribute_elements_in_subtree(inorder_iterator& itr,
                                        dimension_type n,
                                        bool deleting,
                                        dimension_type key,
                                        const data_type& value);

  //! When called with a node and its rightmost child, this moves all elements
  //! of that node's subtree to the rightmost end.
  //! It tries to add an element with key \p key and value \p value in the
  //! process. If it succeeds, sets inserted_value to true.
  //! A boolean with value \p true has to be passed as can_add_key.
  //! If can_add_key remains \p true and inserted_value remains \p false
  //! , this means that \p key is lower than all keys in the tree.
  //! \p first_unused is updated, but root isn't. The root iterator is passed
  //! by reference to improve performance.
  static void compact_elements_in_the_rightmost_end(
    inorder_iterator& root, inorder_iterator& first_unused,
    dimension_type subtree_size, dimension_type key, const data_type& value,
    bool& added_key, bool& can_add_key);

  //! Redistributes the elements in the subtree rooted at \p root,
  //! with \p subtree_size used elements, after the elements have been
  //! compacted to the rightmost end.
  //! If added_key is false, it tries to add the pair (key, value) to the tree.
  //! \p itr points to the leftmost element to be moved.
  //! \p root is not modified, is only passed by reference to improve
  //! \performance. \p itr is invalidated.
  static void redistribute_elements_in_subtree_helper(
    inorder_iterator& root, dimension_type subtree_size,
    inorder_iterator& itr, dimension_type key, const data_type& value,
    bool added_key);

  //! Moves all data in the tree \p tree in *this.
  //! *this must be empty and big enough to contain all of tree's data
  //! without exceeding max_density.
  void move_data_from(CO_Tree& tree);

  //! Copies all data in the tree \p tree into *this.
  //! *this must be empty and big enough to contain all of tree's data
  //! without exceeding max_density.
  void copy_data_from(const CO_Tree& tree);

  //! Counts the number of used elements in the subtree rooted at the node
  //! pointed to by itr.
  static dimension_type count_used_in_subtree(inorder_iterator& itr);

  //! Counts the number of used elements in the subtree rooted at the node
  //! pointed to by itr.
  static dimension_type count_used_in_subtree(inorder_const_iterator& itr);

  //! Moves the value of \p from in \p to .
  //! The final value of \p from is unspecified.
  static void move_data_element(data_type& to, data_type& from);

  //! The maximum density of used nodes.
  //! Must be greater than or equal to 0.5 and lower than 1.
  static const float max_density = 0.9;

  //! The minimum density of used nodes.
  //! Must be strictly lower than max_density.
  static const float min_density = 0.35;

  //! The minimum density at the leaves' depth.
  //! Must be strictly lower than min_density.
  static const float min_leaf_density = 0.3;

  //! An index used as a marker for unused nodes in the tree.
  //! This must not be used as a key.
  static const dimension_type unused_index = -1;

#ifdef USE_PPL_CO_TREE_VEB_LAYOUT
  // TODO: don't waste the first element.
  //! level[d] contains data about nodes with depth d.
  const level_data* level;
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT)

  //! The depth of the leaves in the static tree.
  height_t max_depth;

  //! The vector that contains the keys in the tree.
  //! If a pair has \p unused_index as first element, it means it is not used.
  //! Its size is reserved_size + 2, because the first and the last elements
  //! are used as markers for iterators.
  dimension_type* indexes;

  //! The vector that contains the data of the keys in the tree.
  //! If index[i] is \p unused_index, data[i] is unused. Otherwise, data[i]
  //! contains the data associated to the indexes[i] key.
  //! Its size is reserved_size + 1, because the first element is not used.
  data_type* data;

  //! The size of the \p data vector minus one. It is one less than a power of
  //! 2.
  //! If this is 0, data, indexes and (for the VeB layout) level are set to
  //! NULL.
  dimension_type reserved_size;

  //! The number of used elements in \p data .
  dimension_type size;
};

class CO_Tree::inorder_const_iterator {
public:

  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const data_type& data);

    const std::pair<const dimension_type, const data_type&>* operator->()
      const;

  private:
    std::pair<const dimension_type, const data_type&> my_pair;
  };

  //! Constructs an iterator pointing to the root node.
  inorder_const_iterator(const CO_Tree* tree = 0);

  inorder_const_iterator(const inorder_const_iterator& itr);
  inorder_const_iterator(const inorder_iterator& itr);

  //! Returns an iterator that points before the first element.
  static inorder_const_iterator construct_before_begin(const CO_Tree& tree);

  //! Returns an iterator that points after the last element.
  static inorder_const_iterator construct_end(const CO_Tree& tree);

  //! Assigns \p itr to *this .
  inorder_const_iterator& operator=(const inorder_const_iterator& itr);

  //! Assigns \p itr to *this .
  inorder_const_iterator& operator=(const inorder_iterator& itr);

  //! Makes the iterator point to the root of \p tree.
  //! The values of all fields (beside root) are overwritten.
  void get_root();

  //! Makes the iterator point to the left child of the current node.
  void get_left_child();

  //! Makes the iterator point to the right child of the current node.
  void get_right_child();

  //! Makes the iterator point to the parent of the current node.
  void get_parent();

  //! Makes the iterator point to the left child of the current node.
  //! Returns false if there is no left child or the left child is unused
  //! (and *this is unchanged).
  bool get_left_child_value();

  //! Makes the iterator point to the right child of the current node.
  //! Returns false if there is no right child or the right child is unused
  //! (and *this is unchanged).
  bool get_right_child_value();

  //! Returns true if the pointed node has a parent.
  bool has_parent() const;

  //! Returns true if the pointed node has a parent and is its right child.
  bool is_right_child() const;

  //! Returns true if the pointed node is a leaf of the complete tree.
  bool is_leaf() const;

  //! Navigates to the next node in the in-order traversal.
  inorder_const_iterator& operator++();

  //! Navigates to the previous node in the in-order traversal.
  inorder_const_iterator& operator--();

  //! Navigates to the next node with a value, in the in-order traversal.
  void get_next_value();

  //! Navigates to the previous node with a value, in the in-order traversal.
  void get_previous_value();

  //! Returns the value_type of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! Compares \p *this with x .
  bool operator==(const inorder_const_iterator& x) const;

  //! Compares \p *this with x .
  bool operator!=(const inorder_const_iterator& x) const;

  //! Returns true if the iterator is an end() iterator.
  bool is_at_end() const;

  //! Returns true if the iterator points before the first element.
  bool is_before_begin() const;

  //! Returns the depth of the current node.
  height_t depth() const;

  const CO_Tree* get_tree() const;

private:
#ifdef USE_PPL_CO_TREE_VEB_LAYOUT
  //! The depth of the current node in the vEB layout.
  height_t d;

  //! Position of the node in a BFS layout.
  dimension_type i;

  //! A vector of size d + 1.
  //! pos[0] is not used.
  //! pos[j], for j>0, is the position in the vEB layout of the node passed
  //! at depth j.
  dimension_type pos[CHAR_BIT*sizeof(dimension_type)];
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT)

#ifdef USE_PPL_CO_TREE_BFS_LAYOUT
  //! The index of the current node in the BFS layout.
  dimension_type i;
#endif // defined(USE_PPL_CO_TREE_BFS_LAYOUT)

#if defined(USE_PPL_CO_TREE_VEB_LAYOUT) || defined(USE_PPL_CO_TREE_BFS_LAYOUT)
  //! When this is true, data fields (except tree) are not valid and
  //! this is an end iterator.
  bool at_end;

  //! When this is true, data fields (except tree) are not valid and
  //! this iterator points to a non-existent node before the beginning.
  bool before_begin;
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT) || defined(USE_PPL_CO_TREE_BFS_LAYOUT)

#ifdef USE_PPL_CO_TREE_DFS_LAYOUT
  //! The index of the current node in the DFS layout.
  dimension_type i;
#endif // defined(USE_PPL_CO_TREE_BFS_LAYOUT)

  //! The tree the iterator points to.
  const CO_Tree* tree;

  friend dimension_type
    CO_Tree::count_used_in_subtree(inorder_const_iterator& itr);
};

class CO_Tree::inorder_iterator {
public:

  class Member_Access_Helper {

  public:
    Member_Access_Helper(dimension_type& key, data_type& data);

    std::pair<dimension_type&, data_type&>* operator->();

  private:
    std::pair<dimension_type&, data_type&> my_pair;
  };

  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const data_type& data);

    const std::pair<const dimension_type, const data_type&>* operator->()
      const;

  private:
    std::pair<const dimension_type, const data_type&> my_pair;
  };

  //! Constructs an iterator pointing to the root node.
  inorder_iterator(CO_Tree* tree = 0);

  inorder_iterator(const inorder_iterator& itr);

  //! Returns an iterator that points before the first element.
  static inorder_iterator construct_before_begin(CO_Tree& tree);

  //! Returns an iterator that points after the last element.
  static inorder_iterator construct_end(CO_Tree& tree);

  //! Assigns \p itr to *this .
  inorder_iterator& operator=(const inorder_iterator& itr);

  //! Makes the iterator point to the root of \p tree.
  //! The values of all fields (beside root) are overwritten.
  void get_root();

  //! Makes the iterator point to the left child of the current node.
  void get_left_child();

  //! Makes the iterator point to the right child of the current node.
  void get_right_child();

  //! Makes the iterator point to the parent of the current node.
  void get_parent();

  //! Makes the iterator point to the left child of the current node.
  //! Returns false if there is no left child or the left child is unused
  //! (and *this is unchanged).
  bool get_left_child_value();

  //! Makes the iterator point to the right child of the current node.
  //! Returns false if there is no right child or the right child is unused
  //! (and *this is unchanged).
  bool get_right_child_value();

  //! Returns true if the pointed node has a parent.
  bool has_parent() const;

  //! Returns true if the pointed node has a parent and is its right child.
  bool is_right_child() const;

  //! Returns true if the pointed node is a leaf of the complete tree.
  bool is_leaf() const;

  //! Navigates to the next node in the in-order traversal.
  inorder_iterator& operator++();

  //! Navigates to the previous node in the in-order traversal.
  inorder_iterator& operator--();

  //! Navigates to the next node with a value, in the in-order traversal.
  void get_next_value();

  //! Navigates to the previous node with a value, in the in-order traversal.
  void get_previous_value();

  //! Returns the value_type of the current node.
  std::pair<dimension_type&, data_type&> operator*();

  //! Returns the value_type of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Member_Access_Helper operator->();

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! Compares \p *this with x .
  bool operator==(const inorder_iterator& x) const;

  //! Compares \p *this with x .
  bool operator!=(const inorder_iterator& x) const;

  //! Returns true if the iterator is an end() iterator.
  bool is_at_end() const;

  //! Returns true if the iterator points before the first element.
  bool is_before_begin() const;

  //! Returns the depth of the current node.
  height_t depth() const;

  CO_Tree* get_tree();

  const CO_Tree* get_tree() const;

private:
#ifdef USE_PPL_CO_TREE_VEB_LAYOUT
  //! The depth of the current node in the vEB layout.
  height_t d;

  //! Position of the node in a BFS layout.
  dimension_type i;

  //! A vector of size d + 1.
  //! pos[0] is not used.
  //! pos[j], for j>0, is the position in the vEB layout of the node passed
  //! at depth j.
  dimension_type pos[CHAR_BIT*sizeof(dimension_type)];
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT)

#ifdef USE_PPL_CO_TREE_BFS_LAYOUT
  //! The index of the current node in the BFS layout.
  dimension_type i;
#endif // defined(USE_PPL_CO_TREE_BFS_LAYOUT)

#ifdef USE_PPL_CO_TREE_DFS_LAYOUT
  //! The index of the current node in the DFS layout.
  dimension_type i;
#endif // defined(USE_PPL_CO_TREE_BFS_LAYOUT)

#if defined(USE_PPL_CO_TREE_VEB_LAYOUT) || defined(USE_PPL_CO_TREE_BFS_LAYOUT)
  //! When this is true, data fields (except tree) are not valid and
  //! this is an end iterator.
  bool at_end;

  //! When this is true, data fields (except tree) are not valid and
  //! this iterator points to a non-existent node before the beginning.
  bool before_begin;
#endif // defined(USE_PPL_CO_TREE_VEB_LAYOUT) || defined(USE_PPL_CO_TREE_BFS_LAYOUT)

  //! The tree the iterator points to.
  CO_Tree* tree;

  friend inorder_const_iterator&
    inorder_const_iterator::operator=(const inorder_iterator&);

  friend dimension_type
    CO_Tree::count_used_in_subtree(inorder_iterator& itr);
};

} // namespace Parma_Polyhedra_Library

#include "CO_Tree.inlines.hh"


#endif // !defined(PPL_CO_Tree_defs_hh)
