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

namespace Parma_Polyhedra_Library {

//! A cache-oblivious binary search tree of pairs.
/*!
  This class implements a bynary search tree with keys of dimension_type type
  and data of Coefficient type, layed out in a dynamic-sized array.

  The array-based layout saves calls to new/delete (for n inserted elements,
  only O(log(n)) allocations are performed) and, more importantly, is much
  more cache-friendly than a standard (pointer-based) tree, because the
  elements are stored sequentially in memory (leaving some holes to allow
  fast insertion of new elements).
  The downside of this representation is that all iterators are invalidated
  when an element is added or removed, because the array could have been
  enlarged or shrunk. This is partially addressed by providing references to
  internal and end iterators that are updated when needed.

  B-trees are cache-friendly too, but the cache size is fixed (usually at
  compile-time). This raises two problems: firstly the cache size must be
  known in advance and those data structures don't perform well with other
  cache sizes and secondly, even if the cache size is known, the
  optimizations target only one level of cache. This kind of data structures
  are called cache aware. This implementation, instead, is cache oblivious:
  it performs well with every cache size, and thus exploits all of the
  available caches.

  Assuming \p n is the number of elements in the tree and \p B is the number
  of &lt;dimension_type,Coefficient&gt; pairs that fit in cache, the time and
  cache misses complexities are the following:
  -Insertions/Queries/Deletions: O(log(n)) time, O(log(n/B)) cache misses.
  -Tree traversal from begin() to end(), using an iterator: O(n) time, O(n/B)
   cache misses.
  -Queries with a hint: O(log(k)) time and O(log(k/B)), with k the distance
   between the given iterator and the searched element (or the position where
   it would have been).

  The binary search tree is embedded in a (slightly bigger) complete tree,
  that is enlarged and shrunk when needed. The complete tree is layed out
  in an in-order DFS layout in two arrays: one for the keys and one for the
  associated data.
  The indexes and values are stored in different arrays to reduce
  cache-misses during key queries.

  The tree can store up to (-(dimension_type)1)/100 elements.
  This limit allows faster density computations, but could be removed if
  needed.
*/
class CO_Tree {

private:
  //! This is used for node heights and depths in the tree.
  typedef unsigned height_t;

  PPL_COMPILE_TIME_CHECK(-(height_t)1 >= CHAR_BITS*sizeof(dimension_type),
                         "height_t is too small to store depths.");

  class tree_iterator;

public:
  class iterator;
  class const_iterator;

  // This is declared here because it is a friend of iterator.
  bool is_before_begin(const iterator& itr) const;

  // This is declared here because it is a friend of const_iterator.
  bool is_before_begin(const const_iterator& itr) const;

public:

  //! The type of the data elements associated with keys.
  /*!
    If this is changed, occurrences of Coefficient_zero() in the CO_Tree
    implementation have to be replaced with consts of the correct type.
  */
  typedef Coefficient data_type;

  typedef std::pair<const dimension_type, data_type&> value_type;

  class iterator;

  //! A const iterator on the tree elements, ordered by key.
  /*!
    Iterator increment and decrement operations are O(1) amortized time, but
    are not constant time.
    These iterators are invalidated by operations that add or remove elements
    from the tree.
  */
  class const_iterator {
  private:
    //! This is an helper class used by operator->().
    class Const_Member_Access_Helper {

    public:
      Const_Member_Access_Helper(dimension_type key, const data_type& data);

      const std::pair<const dimension_type, const data_type&>* operator->()
        const;

    private:
      std::pair<const dimension_type, const data_type&> my_pair;
    };

  public:

    typedef std::bidirectional_iterator_tag iterator_category;
    typedef std::pair<const dimension_type, const data_type&> value_type;
    typedef ptrdiff_t difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;

    //! Constructs an invalid const_iterator.
    explicit const_iterator();

    //! Constructs an iterator pointing to the first element of the tree.
    explicit const_iterator(const CO_Tree& tree);

    //! Constructs a const_iterator pointing to the i-th node of the tree.
    /*!
      The i-th node must be a node with a value or end().
    */
    const_iterator(const CO_Tree& tree, dimension_type i);

    //! The copy constructor.
    const_iterator(const const_iterator& itr);

    //! Converts an iterator into a const_iterator.
    const_iterator(const iterator& itr);

    //! Swaps itr with *this.
    void swap(const_iterator& itr);

    //! Assigns \p itr to *this .
    const_iterator& operator=(const const_iterator& itr);

    //! Assigns \p itr to *this .
    const_iterator& operator=(const iterator& itr);

    //! Navigates to the next element.
    const_iterator& operator++();

    //! Navigates to the previous element.
    const_iterator& operator--();

    //! Navigates to the next element.
    const_iterator operator++(int);

    //! Navigates to the previous element.
    const_iterator operator--(int);

    //! Returns the current element.
    std::pair<const dimension_type, const data_type&> operator*() const;

    //! Returns a pointer to the value_type of the current node.
    /*!
      This allows using itr->first to access the key of the current
      element and itr->second to access its value.

      This confusing signature is needed because elements are not internally
      stored as pairs in the tree, so it can't just return a pair reference.
    */
    Const_Member_Access_Helper operator->() const;

    //! Compares \p *this with x .
    bool operator==(const const_iterator& x) const;

    //! Compares \p *this with x .
    bool operator!=(const const_iterator& x) const;

  private:
    //! Checks the internal invariants, in debug mode only.
    bool OK() const;

    //! A pointer to the corresponding element of the tree's indexes[] array.
    const dimension_type* current_index;

    //! A pointer to the corresponding element of the tree's data[] array.
    const data_type* current_data;

#ifndef NDEBUG
    //! A pointer to the corresponding tree, used for debug purposes only.
    const CO_Tree* tree;
#endif

    friend bool CO_Tree::is_before_begin(const const_iterator& itr) const;
  };

  //! An iterator on the tree elements, ordered by key.
  /*!
    Iterator increment and decrement operations are O(1) amortized time, but
    are not constant time.
    These iterators are invalidated by operations that add or remove elements
    from the tree.
  */
  class iterator {
  private:

    //! An helper class used by operator->().
    class Member_Access_Helper {

    public:
      Member_Access_Helper(dimension_type key, data_type& data);

      std::pair<const dimension_type, data_type&>* operator->();

    private:
      std::pair<const dimension_type, data_type&> my_pair;
    };

    //! An helper class used by the const version of operator->().
    class Const_Member_Access_Helper {

    public:
      Const_Member_Access_Helper(dimension_type key, const data_type& data);

      const std::pair<const dimension_type, const data_type&>* operator->()
        const;

    private:
      std::pair<const dimension_type, const data_type&> my_pair;
    };

  public:

    typedef std::bidirectional_iterator_tag iterator_category;
    typedef CO_Tree::value_type value_type;
    typedef ptrdiff_t difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;

    //! Constructs an invalid iterator.
    iterator();

    //! Constructs an iterator pointing to first element of the tree.
    explicit iterator(CO_Tree& tree);

    //! Constructs an iterator pointing to the i-th node.
    /*!
      The i-th node must be a node with a value or end().
    */
    iterator(CO_Tree& tree, dimension_type i);

    //! The constructor from a tree_iterator.
    /*!
      This is meant for use by CO_Tree only.
      This is not private to avoid the friend declaration.
    */
    explicit iterator(const tree_iterator& itr);

    //! The copy contructor.
    iterator(const iterator& itr);

    //! Swaps itr with *this.
    void swap(iterator&itr);

    //! Assigns \p itr to *this .
    iterator& operator=(const iterator& itr);

    //! Assigns \p itr to *this .
    iterator& operator=(const tree_iterator& itr);

    //! Navigates to the next element in the tree.
    iterator& operator++();

    //! Navigates to the previous element in the tree.
    iterator& operator--();

    //! Navigates to the next element in the tree.
    iterator operator++(int);

    //! Navigates to the previous element in the tree.
    iterator operator--(int);

    //! Returns the current element.
    std::pair<const dimension_type, data_type&> operator*();

    //! Returns the current element.
    std::pair<const dimension_type, const data_type&> operator*() const;

    //! Returns a pointer to the value_type of the current node.
    /*!
      This allows using itr->first to access the key of the current
      element and itr->second to access its value.
      This confusing signature is needed because elements are not internally
      stored as pairs in the tree, so it can't just return a pair reference.
    */
    Member_Access_Helper operator->();

    //! Returns a pointer to the value_type of the current node.
    /*!
      This allows using itr->first to access the key of the current
      element and itr->second to access its value.
      This confusing signature is needed because elements are not internally
      stored as pairs in the tree, so it can't just return a pair reference.
    */
    Const_Member_Access_Helper operator->() const;

    //! Compares \p *this with x .
    bool operator==(const iterator& x) const;

    //! Compares \p *this with x .
    bool operator!=(const iterator& x) const;

  private:
    //! Checks the internal invariants, in debug mode only.
    bool OK() const;

    //! A pointer to the corresponding element of the tree's indexes[] array.
    const dimension_type* current_index;

    //! A pointer to the corresponding element of the tree's data[] array.
    data_type* current_data;

#ifndef NDEBUG
    //! A pointer to the corresponding tree, used for debug purposes only.
    CO_Tree* tree;
#endif

    friend const_iterator& const_iterator::operator=(const iterator&);
    friend bool CO_Tree::is_before_begin(const iterator& itr) const;
  };

  //! Constructs an empty tree.
  CO_Tree();

  //! The copy constructor.
  CO_Tree(const CO_Tree& v);

  //! The assignment operator.
  CO_Tree& operator=(const CO_Tree& x);

  //! Removes all elements from the tree.
  /*!
    This method takes O(n) time.
  */
  void clear();

  //! The destructor.
  ~CO_Tree();

  //! Returns \p true if the tree has no elements.
  bool empty() const;

  //! Dumps the tree to stdout, for debugging purposes.
  void dump_tree() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  dimension_type external_memory_in_bytes() const;

  //! Inserts an element in the tree.
  /*!
    \returns an iterator that points to the inserted pair.

    If such a pair already exists, an iterator to that pair is returned.

    This operation invalidates existing iterators.
  */
  iterator insert(dimension_type key);

  //! Inserts an element in the tree.
  /*!
    \returns an iterator that points to the inserted element.

    If an element with the specified key already exists, its associated data
    is set to \p data and an iterator pointing to that pair is returned.

    This operation invalidates existing iterators.
  */
  iterator insert(dimension_type key, const data_type& data);

  //! Inserts an element in the tree.
  /*!
    \return an iterator that points to the inserted element.
    \param itr the iterator used as hint

    This will be faster if \p itr points near to the place where the new
    element will be inserted (or where is already stored).
    However, the value of \p itr does not affect the result of this
    method. \p itr may even be end().

    If an element with the specified key already exists, an iterator to that
    pair is returned.

    This operation invalidates existing iterators.
  */
  iterator insert(iterator itr, dimension_type key);

  //! Inserts an element in the tree.
  /*!
    \return an iterator that points to the inserted element.
    \param itr the iterator used as hint

    This will be faster if \p itr points near to the place where the new
    element will be inserted (or where is already stored).
    However, the value of \p itr does not affect the result of this
    method. \p itr may even be end().

    If an element with the specified key already exists, its associated data
    is set to \p data and an iterator pointing to that pair is returned.

    This operation invalidates existing iterators.
  */
  iterator insert(iterator itr, dimension_type key, const data_type& data);

  //! Erases the element with key \p key from the tree.
  /*!
    This operation invalidates existing iterators.

    \returns an iterator to the next element (or end() if there are no
             elements with key greater than \p key ).
  */
  iterator erase(dimension_type key);

  //! Erases the element pointed to by \p itr from the tree.
  /*!
    This operation invalidates existing iterators.

    \returns an iterator to the next element (or end() if there are no
             elements with key greater than \p key ).
  */
  iterator erase(iterator itr);

  /*!
    \brief Removes the element with key \p key (if it exists) and decrements
           by 1 all elements' keys that were greater than \p key.

    This operation invalidates existing iterators.
  */
  void erase_element_and_shift_left(dimension_type key);

  //! Adds \p n to all keys greater than or equal to \p key.
  void increase_keys_after(dimension_type key, dimension_type n);

  //! Swaps x with *this.
  /*!
    This operation invalidates existing iterators.
  */
  void swap(CO_Tree& x);

  //! Returns an iterator that points at the first element.
  iterator begin();

  //! Returns an iterator that points after the last element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is updated at each operation that modifies the structure.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const iterator& end();

  //! Equivalent to cbegin().
  const_iterator begin() const;

  //! Equivalent to cend().
  const const_iterator& end() const;

  //! Returns an iterator that points at the first element.
  const_iterator cbegin() const;

  //! Returns an iterator that points after the last element.
  /*!
    This method always returns a reference to the same internal iterator,
    that is updated at each operation that modifies the structure.
    Client code can keep a const reference to that iterator instead of
    keep updating a local iterator.
  */
  const const_iterator& cend() const;

  //! Searches an element with key \p key using bisection.
  /*!
    If the element is found, an iterator pointing to that element is
    returned; otherwise, the returned iterator refers to the immediately
    preceding or succeeding value.
    If the tree is empty, end() is returned.
  */
  iterator bisect(dimension_type key);

  //! Searches an element with key \p key using bisection.
  /*!
    If the element is found, an iterator pointing to that element is
    returned; otherwise, the returned iterator refers to the immediately
    preceding or succeeding value.
    If the tree is empty, end() is returned.
  */
  const_iterator bisect(dimension_type key) const;

  //! Searches an element with key \p key in [first, last] using bisection.
  /*!
    If the element is found, an iterator pointing to that element is
    returned; otherwise, the returned iterator refers to the immediately
    preceding or succeeding value.
    \p first and \p last must not be end().
    If the tree is empty, end() is returned.

    \note last is included in the search, too.
  */
  iterator bisect_in(iterator first, iterator last, dimension_type key);

  //! Searches an element with key \p key in [first, last] using bisection.
  /*!
    If the element is found, an iterator pointing to that element is
    returned; otherwise, the returned iterator refers to the immediately
    preceding or succeeding value.
    \p first and \p last must not be end().
    If the tree is empty, end() is returned.

    \note last is included in the search, too.
  */
  const_iterator bisect_in(const_iterator first, const_iterator last,
                           dimension_type key) const;

  //! Searches an element with key \p key near \p hint.
  /*!
    If the element is found, the returned iterator points to that element;
    otherwise, it points to the immediately preceding or succeeding value.
    If the tree is empty, end() is returned.

    This uses a binary progression and then a bisection, so this method is
    O(log(n)), and it is O(1) if the distance between the returned position
    and \p hint is O(1).

    \p hint may even be end(), in such cases it is ignored.
  */
  iterator bisect_near(iterator hint, dimension_type key);

  //! Searches an element with key \p key near \p hint.
  /*!
    If the element is found, the returned iterator points to that element;
    otherwise, it points to the immediately preceding or succeeding value.
    If the tree is empty, end() is returned.

    This uses a binary progression and then a bisection, so this method is
    O(log(n)), and it is O(1) if the distance between the returned position
    and \p hint is O(1).

    \p hint may even be end(), in such cases it is ignored.
  */
  const_iterator bisect_near(const_iterator hint, dimension_type key) const;

private:

  //! Searches an element with key \p key in [first, last] using bisection.
  /*!
    If the element is found, an iterator pointing to that element is
    returned; otherwise, the returned iterator refers to the immediately
    preceding or succeeding value.

    \p first and \p last must be indexes of existing values in the indexes[]
    and data[] arrays. They must not be end().
    If the tree is empty, end() is returned.

    \note last is included in the search, too.
  */
  dimension_type bisect_in(dimension_type first, dimension_type last,
                           dimension_type key) const;

  //! Searches an element with key \p key near \p hint.
  /*!
    If the element is found, the returned iterator points to that element;
    otherwise, it points to the immediately preceding or succeeding value.

    This uses a binary progression and then a bisection, so this method is
    O(log(n)), and it is O(1) if the distance between the returned position
    and \p hint is O(1).

    \p hint must be the index of a valid element.
  */
  dimension_type bisect_near(dimension_type hint, dimension_type key) const;

  //! Inserts an element in the tree.
  /*!
    If there is already an element with key \p key in the tree, its
    associated data is set to \p data.

    This operation invalidates existing iterators.

    \param itr must point to the element in the tree with key \p key or, if
               no such element exists, it must point to the node that would
               be his parent.
    \return an iterator that points to the inserted element.
  */
  tree_iterator insert_precise(dimension_type key, const data_type& data,
                               tree_iterator itr);

  //! Inserts an element in the tree.
  /*!
    The tree must be empty.

    This operation invalidates existing iterators.
  */
  void insert_in_empty_tree(dimension_type key1, const data_type& data1);

  //! Erases from the tree the element pointed to by \p itr .
  /*!
    This operation invalidates existing iterators.

    \returns an iterator to the next element (or end() if there are no
             elements with key greater than \p key ).
  */
  iterator erase(tree_iterator itr);

  //! Initializes a tree with reserved size at least \p n .
  void init(dimension_type n);

  //! Deallocates the tree's dynamic arrays.
  /*!
    After this call, the tree fields are uninitialized, so init() must be
    called again before using the tree.
  */
  void destroy();

  //! Checks the internal invariants, but not the densities.
  bool structure_OK() const;

  //! Checks the internal invariants.
  bool OK() const;

  //! Returns the floor of the base-2 logarithm of \p n .
  /*!
    \p n must be greater than zero.
  */
  static unsigned integer_log2(dimension_type n);

  //! Compares the fractions num/den with ratio/100.
  /*!
    \p ratio must be less than or equal to 100.

    \returns true if the fraction num/den is less than the fraction
             ratio/100.
  */
  static bool is_less_than_ratio(dimension_type num, dimension_type den,
                                 dimension_type ratio);

  //! Compares the fractions num/den with ratio/100.
  /*!
    \p ratio must be less than or equal to 100.

    \returns true if the fraction num/den is greater than the fraction
             ratio/100.
  */
  static bool is_greater_than_ratio(dimension_type num, dimension_type den,
                                    dimension_type ratio);

  //! Dumps the subtree rooted at \p itr to stdout, for debugging purposes.
  static void dump_subtree(tree_iterator itr);

  //! Increases the tree's reserved size.
  /*!
    This is called when the density is about to exceed the maximum density
    (specified by max_density_percent).
  */
  void rebuild_bigger_tree();

  //! Decreases the tree's reserved size.
  /*!
    This is called when the density is about to become less than the minimum
    allowed density (specified by min_density_percent).

    \p reserved_size must be greater than 3 (otherwise the tree can just be
    cleared).
  */
  void rebuild_smaller_tree();

  //! Re-initializes the cached iterators.
  /*!
    This method must be called when the indexes[] and data[] vector are
    reallocated.
  */
  void refresh_cached_iterators();

  //! Rebalances the tree after an insertions or a deletion.
  /*!
    \param itr points to the inserted (or deleted) node.

    For insertions, it adds the pair (key, value).

    This operation invalidates existing iterators that point to nodes in the
    rebalanced subtree.

    \returns an iterator pointing to the root of the subtree that was
             rebalanced.
  */
  tree_iterator rebalance(tree_iterator itr, dimension_type key,
                          const data_type& value);

  //! Moves all elements of a subtree to the rightmost end.
  /*!
    If \p add_element is true, it tries to add an element with key \p key and
    value \p value in the process.

    \param last_in_subtree is the index of the last element in the subtree.
    \param subtree_size is the number of valid elements in the subtree.
                        It must be greater than zero.
    \returns the index of the rightmost unused node in the subtree after the
             process.
  */
  dimension_type compact_elements_in_the_rightmost_end(
    dimension_type last_in_subtree, dimension_type subtree_size,
    dimension_type key, const data_type& value,
    bool add_element);

  //! Redistributes the elements in the subtree rooted at \p root_index.
  /*!
    The subtree's elements must be compacted to the rightmost end.

    \param subtree_size the number of used elements in the subtree.
                        It must be greater than zero.
    \param add_element if it is true, it tries to add an element with the
                       specified key and value in the process.
    \param last_used points to the leftmost element with a value in the
                     subtree.
  */
  void redistribute_elements_in_subtree(dimension_type root_index,
                                        dimension_type subtree_size,
                                        dimension_type last_used,
                                        dimension_type key,
                                        const data_type& value,
                                        bool add_element);

  //! Moves all data in the tree \p tree into *this.
  /*!
    *this must be empty and big enough to contain all of tree's data without
    exceeding max_density.
  */
  void move_data_from(CO_Tree& tree);

  //! Copies all data in the tree \p tree into *this.
  /*!
    *this must be empty and big enough to contain all of tree's data without
    exceeding max_density.
  */
  void copy_data_from(const CO_Tree& tree);

  //! Counts the number of used elements in the subtree rooted at itr.
  static dimension_type count_used_in_subtree(tree_iterator itr);

  //! Moves the value of \p from in \p to .
  /*!
    \param from must be a valid value.
    \param to must be a non-constructed chunk of memory.

    After the move, \p from becomes a non-constructed chunk of memory and
    \p to gets the value previously stored by \p from.

    The implementation of this method assumes that data_type values don't
    keep pointers to themselves nor to their fields.
  */
  static void move_data_element(data_type& to, data_type& from);

  //! The maximum density of used nodes.
  /*!
    This must be greater than or equal to 50 and lower than 100.
  */
  static const dimension_type max_density_percent = 90;

  //! The minimum density of used nodes.
  /*!
    Must be strictly lower than max_density_percent.
  */
  static const dimension_type min_density_percent = 35;

  //! The minimum density at the leaves' depth.
  /*!
    Must be strictly lower than min_density_percent..
  */
  static const dimension_type min_leaf_density_percent = 30;

  //! An index used as a marker for unused nodes in the tree.
  /*!
    This must not be used as a key.
  */
  static const dimension_type unused_index = -(dimension_type)1;

  //! The iterator returned by end().
  /*!
    It is updated when needed, to keep it valid.
  */
  iterator cached_end;

  //! The iterator returned by the const version of end().
  /*!
    It is updated when needed, to keep it valid.
  */
  const_iterator cached_const_end;

  //! The depth of the leaves in the complete tree.
  height_t max_depth;

  //! The vector that contains the keys in the tree.
  /*!
    If an element of this vector is \p unused_index , it means that that
    element and the corresponding element of data[] are not used.

    Its size is reserved_size + 2, because the first and the last elements
    are used as markers for iterators.
  */
  dimension_type* indexes;

  //! The vector that contains the data of the keys in the tree.
  /*!
    If index[i] is \p unused_index, data[i] is unused.
    Otherwise, data[i] contains the data associated to the indexes[i] key.

    Its size is reserved_size + 1, because the first element is not used (to
    allow using the same index in both indexes[] and data[] instead of
    adding 1 to access data[]).
  */
  data_type* data;

  //! The number of nodes in the complete tree.
  /*!
    It is one less than a power of 2.
    If this is 0, data and indexes are set to NULL.
  */
  dimension_type reserved_size;

  //! The number of values stored in the tree.
  dimension_type size;
};

class CO_Tree::tree_iterator {

private:

  //! An helper class used by operator->().
  class Member_Access_Helper {

  public:
    Member_Access_Helper(dimension_type& key, data_type& data);

    std::pair<dimension_type&, data_type&>* operator->();

  private:
    std::pair<dimension_type&, data_type&> my_pair;
  };

  //! An helper class used by the const version of operator->().
  class Const_Member_Access_Helper {

  public:
    Const_Member_Access_Helper(dimension_type key, const data_type& data);

    const std::pair<const dimension_type, const data_type&>* operator->()
      const;

  private:
    std::pair<const dimension_type, const data_type&> my_pair;
  };

public:

  /*!
    \brief Constructs a tree_iterator pointing at the root node of the
           specified tree

    \p tree must not be empty.
  */
  explicit tree_iterator(CO_Tree& tree);

  //! Constructs a tree_iterator pointing at the specified node of the tree.
  /*!
    \p tree must not be empty.
  */
  tree_iterator(CO_Tree& tree, dimension_type i);

  //! Constructs a tree_iterator from an iterator.
  /*!
    \p itr must not be end().
  */
  tree_iterator(const iterator& itr, CO_Tree& tree);

  //! The assignment operator.
  tree_iterator& operator=(const tree_iterator& itr);

  //! The assignment operator from an iterator.
  tree_iterator& operator=(const iterator& itr);

  //! Compares *this with \p itr.
  bool operator==(const tree_iterator& itr) const;

  //! Compares *this with \p itr.
  bool operator!=(const tree_iterator& itr) const;

  //! Makes the iterator point to the root of \p tree.
  /*!
    The values of all fields (beside tree) are overwritten.
  */
  void get_root();

  //! Makes the iterator point to the left child of the current node.
  void get_left_child();

  //! Makes the iterator point to the right child of the current node.
  void get_right_child();

  //! Makes the iterator point to the parent of the current node.
  void get_parent();

  /*!
    \brief Searches for an element with key \p key in the subtree rooted at
           \p *this.

    After this method, *this points to the found node (if it exists) or to
    the node that would be his parent (otherwise).
  */
  void go_down_searching_key(dimension_type key);

  /*!
    \brief Follows left childs with a value, until it arrives at a leaf or at
           a node with no value.
  */
  void follow_left_childs_with_value();

  /*!
    \brief Follows right childs with a value, until it arrives at a leaf or at
           a node with no value.
  */
  void follow_right_childs_with_value();

  //! Returns true if the pointed node is the root node.
  bool is_root() const;

  //! Returns true if the pointed node has a parent and is its right child.
  bool is_right_child() const;

  //! Returns true if the pointed node is a leaf of the complete tree.
  bool is_leaf() const;

  //! Returns the key and value of the current node.
  std::pair<dimension_type&, data_type&> operator*();

  //! Returns the key and value of the current node.
  std::pair<const dimension_type, const data_type&> operator*() const;

  //! Returns a pointer to the value_type of the current node.
  Member_Access_Helper operator->();

  //! Returns a pointer to the value_type of the current node.
  Const_Member_Access_Helper operator->() const;

  //! The tree containing the element pointed to by this iterator.
  CO_Tree& tree;

  /*!
    \brief Returns the index of the current node in the DFS layout of the
           complete tree.
  */
  dimension_type index() const;

  /*!
    \brief Returns 2^h, with h the height of the current node in the tree,
           counting from 0.

    Thus leaves have offset 1.
    This is faster than depth(), so it is useful for comparing node depths.
  */
  dimension_type get_offset() const;

  //! Returns the depth of the current node in the complete tree.
  height_t depth() const;

private:
  //! Checks the internal invariant.
  bool OK() const;

  //! The index of the current node in the DFS layout of the complete tree.
  dimension_type i;

  /*!
    \brief This is 2^h, with h the height of the current node in the tree,
           counting from 0.

    Thus leaves have offset 1.
    This is equal to (i & -i), and is stored to increase performance only.
  */
  dimension_type offset;
};

} // namespace Parma_Polyhedra_Library


namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::CO_Tree */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::CO_Tree& x,
          Parma_Polyhedra_Library::CO_Tree& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::CO_Tree::const_iterator */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::CO_Tree::const_iterator& x,
          Parma_Polyhedra_Library::CO_Tree::const_iterator& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::CO_Tree::iterator */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::CO_Tree::iterator& x,
          Parma_Polyhedra_Library::CO_Tree::iterator& y);


} // namespace std

#include "CO_Tree.inlines.hh"


#endif // !defined(PPL_CO_Tree_defs_hh)
