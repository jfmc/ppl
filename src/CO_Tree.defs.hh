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

namespace Parma_Polyhedra_Library {

class CO_Tree {

private:
  typedef dimension_type height_t;

public:

  typedef Coefficient data_type;
  typedef std::pair<dimension_type, data_type> value_type;

  class iterator;
  class const_iterator;

  CO_Tree();
  explicit CO_Tree(const std::vector<data_type>& v);
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

  //! Inserts a pair with key \p key in the tree and returns an iterator that
  //! points to the inserted pair.
  //! If such a pair already exists, an iterator to that pair is returned.
  iterator insert(dimension_type key);

  //! Inserts the pair (key, data) in the tree.
  //! Returns an iterator that points to the inserted element.
  //! If the key \p key is already in the tree, its associated value is set to
  //! \p data and an iterator pointing to that pair is returned.
  iterator insert(dimension_type key, const data_type& data);

  //! Inserts the pair (key, data) in the tree.
  //! \p itr is used as hint, this will be faster if \p itr points near to the
  //! place where the new element will be inserted (or where is already stored).
  //! However, the value of \p itr does not affect the correctness of this
  //! method. \p itr may even be before_begin() or end().
  iterator insert(iterator itr, dimension_type key, const data_type& data);

  //! Inserts a pair with key \p key in the tree.
  //! \p itr is used as hint, this will be faster if \p itr points near to the
  //! place where the new element will be inserted (or where is already stored).
  //! However, the value of \p itr does not affect the correctness of this
  //! method. \p itr may even be before_begin() or end().
  iterator insert(iterator itr, dimension_type key);

  //! Erases the pair with key \p key from the tree.
  //! Returns \p false if there was no pair with key \p key in the tree.
  bool erase(dimension_type key);

  //! Swaps x with *this.
  void swap(CO_Tree& x);

  //! Returns an iterator that points before the first element.
  iterator before_begin();

  //! Returns an iterator that points before the first element.
  iterator begin();

  //! Returns an iterator that points after the last element.
  iterator end();

  //! Returns an iterator that points before the first element.
  const_iterator before_begin() const;

  //! Returns an iterator that points before the first element.
  const_iterator begin() const;

  //! Returns an iterator that points after the last element.
  const_iterator end() const;

  //! Erases from the tree the element pointed to by \p itr .
  //! \p itr is invalidated.
  void erase(iterator itr);

  //! Searches in [first, last] using bisection (note that last is included,
  //! too!).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, an iterator pointing to that element is
  //! returned; otherwise, the returned iterator refers to the immediately
  //! preceding or succeeding value.
  //! \p first and \p last must point to existing values.
  template <typename Func>
  iterator bisect_in(iterator first, iterator last, const Func &func);

  //! Searches in [first, last] using bisection (note that last is included,
  //! too!).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, an iterator pointing to that element is
  //! returned; otherwise, the returned iterator refers to the immediately
  //! preceding or succeeding value.
  //! \p first and \p last must point to existing values.
  template <typename Func>
  const_iterator bisect_in(const_iterator first, const_iterator last,
                           const Func &func) const;

  //! Searches near \p hint using a binary progression and then a bisection.
  //! This means this method is always O(log(n)), and it is O(1) if the
  //! distance between the searched position and \p hint is O(1).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, the returned iterator points to that element;
  //! otherwise, it points to the immediately preceding or succeeding value.
  //! \p first and \p last must be indexes of existing values.
  template <typename Func>
  iterator bisect_near(iterator hint, const Func &func);

  //! Searches near \p hint using a binary progression and then a bisection.
  //! This means this method is always O(log(n)), and it is O(1) if the
  //! distance between the searched position and \p hint is O(1).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, the index of that element is returned;
  //! otherwise, the returned index refers to the immediately preceding or
  //! succeeding value.
  //! \p first and \p last must be indexes of existing values.
  template <typename Func>
  const_iterator bisect_near(const_iterator hint, const Func &func) const;

private:

  //! Searches in [first, last] using bisection (note that last is included,
  //! too!).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, the index of that element is returned;
  //! otherwise, the returned index refers to the immediately preceding or
  //! succeeding value.
  //! \p first and \p last must be indexes of existing values.
  template <typename Func>
  dimension_type bisect_in(dimension_type first, dimension_type last,
                           const Func &func) const;

  //! Searches near \p hint using a binary progression and then a bisection.
  //! This means this method is always O(log(n)), and it is O(1) if the
  //! distance between the searched position and \p hint is O(1).
  //! func will be applied to a dimension_type and a data_type and should
  //! return 0 if this is the searched element (ending bisection), a positive
  //! integer if the searched element is after this, and a negative integer if
  //! the searched element is before this.
  //! If the element is found, the index of that element is returned;
  //! otherwise, the returned index refers to the immediately preceding or
  //! succeeding value.
  //! \p first and \p last must be indexes of existing values.
  template <typename Func>
  dimension_type bisect_near(dimension_type hint, const Func &func) const;

  class tree_iterator;

  //! Inserts the pair (key1, data1) in the tree.
  //! \p itr must be the lower bound of \p key in the tree.
  //! \p itr is modified to point to the inserted element.
  void insert_precise(dimension_type key1, const data_type& data1,
                      tree_iterator& itr);

  //! Inserts the pair (key1, data1) in the tree.
  void insert_in_empty_tree(dimension_type key1, const data_type& data1);

  //! Erases from the tree the element pointed to by \p itr .
  //! \p itr is invalidated.
  void erase(tree_iterator itr);

  //! Searches for an element with key \p key in the subtree rooted at \p itr.
  //! \p itr is modified to point to the found node (if it exists) or to the
  //! node that would be his parent (otherwise).
  void go_down_searching_key(tree_iterator& itr, dimension_type key);

  //! Initializes a tree with reserved size at least \p n .
  void init(dimension_type n);

  //! Deallocates the tree. After this call, init() can be called again.
  void destroy();

  //! Checks the invariant, but not the densities.
  bool structure_OK() const;

  //! Returns the floor of the base-2 logarithm of \p n .
  //! \p n must be greater than zero.
  static unsigned integer_log2(dimension_type n);

  //! Dumps the subtree rooted at \p itr to stdout, for debugging purposes.
  static void dump_subtree(tree_iterator itr);

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
  void rebalance(tree_iterator& itr, dimension_type key,
                 const data_type& value);

  //! Redistributes the elements in the subtree rooted at the node
  //! pointed to by itr. If \p deleting is not \p false, it adds the pair
  //! (key, value) to the tree.
  //! \p subtree_size is the number of used elements in the subtree at the end
  //! of the call.
  void redistribute_elements_in_subtree(tree_iterator& itr,
                                        dimension_type n,
                                        bool deleting,
                                        dimension_type key,
                                        const data_type& value);

  //! Moves all elements of a subtree to the rightmost end.
  //! If \p add_element is true, it tries to add an element with key \p key
  //! and value \p value in the process.
  //! \p last_in_subtree is the index of the last element in the subtree.
  //! This returns the index of the rightmost unused node in the subtree
  //! after the process.
  dimension_type compact_elements_in_the_rightmost_end(
    dimension_type last_in_subtree, dimension_type subtree_size,
    dimension_type key, const data_type& value,
    bool add_element);

  //! Redistributes the elements in the subtree rooted at \p root_index,
  //! with \p subtree_size used elements, after the elements have been
  //! compacted to the rightmost end.
  //! If add_element is true, it tries to add the pair (key, value) to the
  //! tree in the process.
  //! \p last_used points to the leftmost element with a value in the subtree.
  void redistribute_elements_in_subtree_helper(
    dimension_type root_index, dimension_type subtree_size,
    dimension_type last_used, dimension_type key, const data_type& value,
    bool add_element);

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
  static dimension_type count_used_in_subtree(tree_iterator& itr);

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
  static const dimension_type unused_index = -(dimension_type)1;

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

class CO_Tree::const_iterator {
public:

  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const data_type& data);

    const std::pair<const dimension_type, const data_type&>* operator->()
      const;

  private:
    std::pair<const dimension_type, const data_type&> my_pair;
  };

  //! Constructs an invalid const_iterator.
  explicit const_iterator();

  //! Constructs an iterator pointing to the root node.
  explicit const_iterator(const CO_Tree& tree);

  //! Constructs a const_iterator pointing to the i-th node.
  //! The i-th node must be before-begin, end or a node with a value.
  const_iterator(const CO_Tree& tree, dimension_type i);

  const_iterator(const const_iterator& itr);
  const_iterator(const iterator& itr);

  //! Assigns \p itr to *this .
  const_iterator& operator=(const const_iterator& itr);

  //! Assigns \p itr to *this .
  const_iterator& operator=(const iterator& itr);

  //! Navigates to the next node with a value.
  const_iterator& operator++();

  //! Navigates to the previous node with a value.
  const_iterator& operator--();

  //! Returns the value_type of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! Compares \p *this with x .
  bool operator==(const const_iterator& x) const;

  //! Compares \p *this with x .
  bool operator!=(const const_iterator& x) const;

private:
  //! A pointer to the corresponding element of the tree's indexes[] array.
  const dimension_type* current_index;

  //! A pointer to the corresponding element of the tree's data[] array.
  const data_type* current_data;
};

class CO_Tree::iterator {
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

  //! Constructs an invalid iterator.
  iterator();

  //! Constructs an iterator pointing to the root node.
  explicit iterator(CO_Tree& tree);

  //! Constructs an iterator pointing to the i-th node.
  //! The i-th node must be before-begin, end or a node with a value.
  iterator(CO_Tree& tree, dimension_type i);

  explicit iterator(const tree_iterator& itr);

  iterator(const iterator& itr);

  //! Assigns \p itr to *this .
  iterator& operator=(const iterator& itr);

  //! Assigns \p itr to *this .
  iterator& operator=(const tree_iterator& itr);

  //! Navigates to the next node with a value.
  iterator& operator++();

  //! Navigates to the previous node with a value.
  iterator& operator--();

  //! Returns the value_type of the current node.
  std::pair<dimension_type&, data_type&> operator*();

  //! Returns the value_type of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Member_Access_Helper operator->();

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! Compares \p *this with x .
  bool operator==(const iterator& x) const;

  //! Compares \p *this with x .
  bool operator!=(const iterator& x) const;

private:
  //! A pointer to the corresponding element of the tree's indexes[] array.
  dimension_type* current_index;

  //! A pointer to the corresponding element of the tree's data[] array.
  data_type* current_data;

  friend const_iterator&
    const_iterator::operator=(const iterator&);
};

class CO_Tree::tree_iterator {

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

  //! Constructs a tree_iterator pointing at the root node of the specified
  //! tree (assuming the tree is not empty).
  explicit tree_iterator(CO_Tree& tree);

  tree_iterator(const iterator& itr, CO_Tree& tree);

  tree_iterator& operator=(const tree_iterator& itr);
  tree_iterator& operator=(const iterator& itr);

  bool operator==(const tree_iterator& itr) const;
  bool operator!=(const tree_iterator& itr) const;

  bool operator==(const iterator& itr) const;
  bool operator!=(const iterator& itr) const;

  //! Makes the iterator point to the root of \p tree.
  //! The values of all fields (beside root) are overwritten.
  void get_root();

  //! Makes the iterator point to the left child of the current node.
  void get_left_child();

  //! Makes the iterator point to the right child of the current node.
  void get_right_child();

  //! Makes the iterator point to the parent of the current node.
  void get_parent();

  //! Follows left childs until it arrives at a leaf.
  void follow_left_childs();

  //! Follows right childs until it arrives at a leaf.
  void follow_right_childs();

  //! Follows left childs with a value, until it arrives at a leaf.
  void follow_left_childs_with_value();

  //! Follows right childs with a value, until it arrives at a leaf.
  void follow_right_childs_with_value();

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

  //! Returns the value_type of the current node.
  std::pair<dimension_type&, data_type&> operator*();

  //! Returns the value_type of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Member_Access_Helper operator->();

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! The tree containing the element pointed to by this iterator.
  CO_Tree* const tree;

  //! Returns the index of the current node in the DFS layout of the complete
  //! tree.
  dimension_type index() const;

  //! Returns 2^h, with h the height of the current node in the tree,
  //! counting from 0. Thus leaves have offset 1.
  dimension_type get_offset() const;

  //! Returns the height of the current node in the complete tree.
  unsigned depth() const;

private:
  bool OK() const;

  //! The index of the current node in the DFS layout of the complete tree.
  dimension_type i;

  //! This is 2^h, with h the height of the current node in the tree,
  //! counting from 0. Thus leaves have offset 1.
  dimension_type offset;
};


} // namespace Parma_Polyhedra_Library

#include "CO_Tree.inlines.hh"


#endif // !defined(PPL_CO_Tree_defs_hh)
