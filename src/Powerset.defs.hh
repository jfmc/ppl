/* Powerset class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Powerset_defs_hh
#define PPL_Powerset_defs_hh

#include "Powerset.types.hh"
#include <iosfwd>
#include <list>
#include <set>

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Powerset */
template <typename CS>
bool
operator==(const Powerset<CS>& x, const Powerset<CS>& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are not equivalent.
/*! \relates Powerset */
template <typename CS>
bool
operator!=(const Powerset<CS>& x, const Powerset<CS>& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename CS>
std::ostream&
operator<<(std::ostream& s, const Powerset<CS>& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


//! The powerset construction on constraint systems.
/*!
  This class offers a generic implementation of <EM>powerset
  constraint systems</EM> as defined in \ref Bag98 "[Bag98]".
  See also the description in Section \ref powerset.
*/
template <typename CS>
class Parma_Polyhedra_Library::Powerset {
public:
  //! \name Constructors and Destructor
  //@{

  //! Default constructor: builds the bottom of the powerset constraint
  //! system (i.e., the empty powerset).
  Powerset();

  //! Copy constructor.
  Powerset(const Powerset& y);

  //! \brief
  //! If \p d is not bottom, builds a powerset containing only \p d.
  //! Builds the empty powerset otherwise.
  explicit Powerset(const CS& d);

  //! Destructor.
  ~Powerset();

  //@} // Constructors and Destructor

  //! \name Member Functions that Do Not Modify the Powerset Element
  //@{

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const Powerset& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is the top
  //! element of the powerset constraint system (i.e., it represents
  //! the universe).
  bool is_top() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is the bottom
  //! element of the powerset constraint system (i.e., it represents
  //! the empty set).
  bool is_bottom() const;

  //! \brief
  //! Returns a lower bound to the total size in bytes of the memory
  //! occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! \brief
  //! Returns a lower bound to the size in bytes of the memory
  //! managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks if all the invariants are satisfied.
  // FIXME: document and perhaps use an enum instead of a bool.
  bool OK(bool disallow_bottom = false) const;

  //@} // Member Functions that Do Not Modify the Powerset Element

protected:
  //! A powerset is implemented as a sequence of elements.
  /*!
    The particular sequence employed must support efficient deletion
    in any position and efficient back insertion.
  */
  typedef std::list<CS> Sequence;
  typedef typename Sequence::iterator Sequence_iterator;
  typedef typename Sequence::const_iterator Sequence_const_iterator;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

  //! If <CODE>true</CODE>, \p *this is omega-reduced.
  mutable bool reduced;

public:
  // Sequence manipulation types, accessors and modifiers
  typedef typename Sequence::size_type size_type;
  typedef typename Sequence::value_type value_type;
  class iterator;
  class const_iterator;
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  //! \name Member Functions for the Direct Inspection of Disjuncts
  //@{

  //! Erase from the sequence of disjuncts all the non-maximal elements.
  /*!
    This method is declared <CODE>const</CODE> because, even though
    omega-reduction may change the syntactic representation of \p *this,
    its semantics will be unchanged.
  */
  void omega_reduce() const;

  //! Returns the number of disjuncts.
  size_type size() const;

  //! Returns <CODE>true</CODE> if and only if there are no disjuncts.
  bool empty() const;

  //! \brief
  //! Returns the iterator pointing to the first disjunct,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end iterator.
  iterator begin();

  //! Returns the past-the-end iterator.
  iterator end();

  //! \brief
  //! Returns the const_iterator pointing to the first disjunct,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  reverse_iterator rbegin();
  reverse_iterator rend();
  const_reverse_iterator rbegin() const;
  const_reverse_iterator rend() const;

  //@} // Member Functions for the Direct Inspection of Disjuncts

  //! \name Member Functions that May Modify the Powerset Element
  //@{

  //! The assignment operator.
  Powerset& operator=(const Powerset& y);

  //! Swaps \p *this with \p y.
  void swap(Powerset& y);

  //! Assigns to \p *this the least upper bound of \p *this and \p y.
  void least_upper_bound_assign(const Powerset& y);

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  /*!
    The result will be the least upper bound of \p *this and \p y.
  */
  void upper_bound_assign(const Powerset& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const Powerset& y);

  //! Adds to \p *this the disjunct \p d.
  void add_disjunct(const CS& d);

  //! \brief
  //! Drops the disjunct pointed to by \p position, returning
  //! an iterator to the disjunct following \p position.
  iterator drop_disjunct(iterator position);

  //! Drops all the disjuncts from \p first to \p last (excluded).
  void drop_disjuncts(iterator first, iterator last);

  //! Drops all the disjuncts, making \p *this an empty powerset.
  void clear();

  //! \brief
  //! If \p *this is not empty (i.e., it is not the bottom element),
  //! it is reduced to a singleton obtained by computing an upper-bound
  //! of all the disjuncts.
  void collapse();

  //@} // Member Functions that May Modify the Powerset element

protected:
  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this does not contain
  //! non-maximal elements.
  bool is_omega_reduced() const;

  //! \brief
  //! Upon return, \p *this will contain \p max_disjuncts elements at most,
  //! by replacing all the exceeding disjuncts, if any, with their upper-bound.
  void collapse(unsigned max_disjuncts);

  //! \brief
  //! Adds to \p *this the disjunct \p d,
  //! assuming \p d is not the bottom element and ensuring
  //! partial omega-reduction.
  /*!
    If \p d is not the bottom element and is not redundant with respect
    to the elements in positions between \p first and \p last,
    adds to \p *this the disjunct \p d, erasing all the elements
    in the above mentioned positions that are made omega-redundant
    by the addition of \p d.
  */
  iterator add_non_bottom_disjunct(const CS& d,
				   iterator first,
				   iterator last);

  //! \brief
  //! Adds to \p *this the disjunct \p d,
  //! assuming \p d is not the bottom element.
  void add_non_bottom_disjunct(const CS& d);

  //! \brief
  //! Assigns to \p *this the result of applying \p op_assign pairwise
  //! to the elements in \p *this and \p y.
  /*!
    The elements of the powerset result are obtained by applying
    \p op_assign to each pair of elements whose components are drawn
    from \p *this and \p y, respectively.
  */
  template <typename Binary_Operator_Assign>
  void pairwise_apply_assign(const Powerset& y,
			     Binary_Operator_Assign op_assign);

private:
  //! \brief
  //! Does the hard work of checking whether \p *this contains non-maximal
  //! elements and returns <CODE>true</CODE> if and only if it does not.
  bool check_omega_reduced() const;

  //! \brief
  //! Replaces the disjunct \p *sink by an upper bound of itself and
  //! all the disjuncts following it.
  void collapse(Sequence_iterator sink);
};

template <typename CS>
class Parma_Polyhedra_Library::Powerset<CS>::const_iterator {
protected:
  //! The type of the underlying const_iterator.
  typedef typename Powerset::Sequence::const_iterator Base;

  //! A shorcut for naming traits.
  typedef typename std::iterator_traits<Base> Traits;

  //! A const_iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level const_iterator.
  const_iterator(const Base& b);

  friend class Powerset;

public:
  // Same traits of the underlying const_iterator.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  const_iterator();

  //! Copy constructor.
  const_iterator(const const_iterator& y);

  //! Constructs from the corresponding non-const iterator.
  const_iterator(const iterator& y);

  //! Dereference operator.
  reference operator*() const;

  //! indirect member selector.
  pointer operator->() const;

  // Prefix increment operator.
  const_iterator& operator++();

  // Postfix increment operator.
  const_iterator operator++(int);

  // Prefix decrement operator.
  const_iterator& operator--();

  // Postfix decrement operator.
  const_iterator operator--(int);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this and \p y are identical.
  bool operator==(const const_iterator& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this and \p y are different.
  bool operator!=(const const_iterator& y) const;
};

template <typename CS>
class Parma_Polyhedra_Library::Powerset<CS>::iterator {
protected:
  //! The type of the underlying mutable iterator.
  typedef typename Powerset::Sequence::iterator Base;

  //! A shorcut for naming traits.
  typedef typename
  std::iterator_traits<typename Powerset::Sequence::const_iterator> Traits;

  //! A (mutable) iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level iterator.
  iterator(const Base& b);

  friend class Powerset;
  friend Powerset<CS>::const_iterator::const_iterator(const iterator& y);

public:
  // Same traits of the const_iterator, therefore
  // forbidding the direct modification of sequence elements.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  iterator();

  //! Copy constructor.
  iterator(const iterator& y);

  //! Dereference operator.
  reference operator*() const;

  //! Indirect access operator.
  pointer operator->() const;

  //! Prefix increment operator.
  iterator& operator++();

  //! Postfix increment operator.
  iterator operator++(int);

  //! Prefix decrement operator.
  iterator& operator--();

  //! Postfix decrement operator.
  iterator operator--(int);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this and \p y are identical.
  bool operator==(const iterator& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this and \p y are different.
  bool operator!=(const iterator& y) const;
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename CS>
void swap(Parma_Polyhedra_Library::Powerset<CS>& x,
	  Parma_Polyhedra_Library::Powerset<CS>& y);

} // namespace std

#include "Powerset.inlines.hh"

#endif // !defined(PPL_Powerset_defs_hh)
