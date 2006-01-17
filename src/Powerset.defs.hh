/* Powerset class declaration.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Powerset_defs_hh
#define PPL_Powerset_defs_hh

#include "Powerset.types.hh"
#include <iosfwd>
#include <iterator>
#include <list>

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Powerset */
template <typename D>
bool
operator==(const Powerset<D>& x, const Powerset<D>& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are not equivalent.
/*! \relates Powerset */
template <typename D>
bool
operator!=(const Powerset<D>& x, const Powerset<D>& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename D>
std::ostream&
operator<<(std::ostream& s, const Powerset<D>& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


//! The powerset construction on a base-level domain.
/*! \ingroup PPL_CXX_interface
  This class offers a generic implementation of a
  <EM>powerset</EM> domain as defined in Section \ref powerset.

  Besides invoking the available methods on the disjuncts of a Powerset,
  this class also provides bidirectional iterators that allow for a
  direct inspection of these disjuncts. For a consistent handling of
  Omega-reduction, all the iterators are <EM>read-only</EM>, meaning
  that the disjuncts cannot be overwritten. Rather, by using the class
  <CODE>iterator</CODE>, it is possible to drop one or more disjuncts
  (possibly so as to later add back modified versions).  As an example
  of iterator usage, the following templatic function drops from
  powerset \p ps all the disjuncts that would have become redundant by
  the addition of an external element \p d.

  \code
template <typename D>
void
drop_subsumed(Powerset<D>& ps, const D& d) {
  for (typename Powerset<D>::iterator i = ps.begin(),
         ps_end = ps.end(), i != ps_end; )
    if (i->definitely_entails(d))
      i = ps.drop_disjunct(i);
    else
      ++i;
}
  \endcode

  The template class D must provide the following methods.
  \code
    memory_size_type total_memory_in_bytes() const
  \endcode
  Returns a lower bound on the total size in bytes of the memory
  occupied by the instance of D.
  \code
    bool is_top() const
  \endcode
  Returns <CODE>true</CODE> if and only if the instance of D is the top
  element of the domain.
  \code
    bool is_bottom() const
  \endcode
  Returns <CODE>true</CODE> if and only if the instance of D is the
  bottom element of the domain.
  \code
    bool definitely_entails(const D& y) const
  \endcode
  Returns <CODE>true</CODE> if the instance of D definitely entails
  <CODE>y</CODE>.  Returns <CODE>false</CODE> if the instance may not
  entail <CODE>y</CODE> (i.e., if the instance does not entail
  <CODE>y</CODE> or if entailment could not be decided).
  \code
    void upper_bound_assign(const D& y)
  \endcode
  Assigns to the instance of D an upper bound of the instance and
  <CODE>y</CODE>.
  \code
    void meet_assign(const D& y)
  \endcode
  Assigns to the instance of D the meet of the instance and
  <CODE>y</CODE>.
  \code
    bool OK() const
  \endcode
  Returns <CODE>true</CODE> if the instance of D is in a consistent
  state, else returns <CODE>false</CODE>.

  The following operators on the template class D must be defined.
  \code
    operator<<(std::ostream& s, const D& x)
  \endcode
  Writes a textual representation of the instance of D on
  <CODE>s</CODE>.
  \code
    operator==(const D& x, const D& y)
  \endcode
  Returns <CODE>true</CODE> if and only if <CODE>x</CODE> and
  <CODE>y</CODE> are equivalent D's.
  \code
    operator!=(const D& x, const D& y)
  \endcode
  Returns <CODE>true</CODE> if and only if <CODE>x</CODE> and
  <CODE>y</CODE> are different D's.
*/
template <typename D>
class Parma_Polyhedra_Library::Powerset {
public:
  //! \name Constructors and Destructor
  //@{

  /*! \brief
    Default constructor: builds the bottom of the powerset constraint
    system (i.e., the empty powerset).
  */
  Powerset();

  //! Copy constructor.
  Powerset(const Powerset& y);

  /*! \brief
    If \p d is not bottom, builds a powerset containing only \p d.
    Builds the empty powerset otherwise.
  */
  explicit Powerset(const D& d);

  //! Destructor.
  ~Powerset();

  //@} // Constructors and Destructor

  //! \name Member Functions that Do Not Modify the Powerset Element
  //@{

  /*! \brief
    Returns <CODE>true</CODE> if \p *this definitely entails \p y.
    Returns <CODE>false</CODE> if \p *this may not entail \p y
    (i.e., if \p *this does not entail \p y or if entailment could
    not be decided).
  */
  bool definitely_entails(const Powerset& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the top
    element of the powerset constraint system (i.e., it represents
    the universe).
  */
  bool is_top() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the bottom
    element of the powerset constraint system (i.e., it represents
    the empty set).
  */
  bool is_bottom() const;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  /*! \brief
    Returns a lower bound to the size in bytes of the memory
    managed by \p *this.
  */
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
  typedef std::list<D> Sequence;

  //! Alias for the low-level iterator on the disjuncts.
  typedef typename Sequence::iterator Sequence_iterator;

  //! Alias for the low-level %const_iterator on the disjuncts.
  typedef typename Sequence::const_iterator Sequence_const_iterator;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

  //! If <CODE>true</CODE>, \p *this is Omega-reduced.
  mutable bool reduced;

public:
  // Sequence manipulation types, accessors and modifiers
  typedef typename Sequence::size_type size_type;
  typedef typename Sequence::value_type value_type;

  class omega_iterator;
  class omega_const_iterator;

  /*! \brief
    Alias for a <EM>read-only</EM> bidirectional %iterator on the
    disjuncts of a Powerset element.

    By using this iterator type, the disjuncts cannot be overwritten,
    but they can be removed using methods
    <CODE>drop_disjunct(iterator position)</CODE> and
    <CODE>drop_disjuncts(iterator first, iterator last)</CODE>,
    while still ensuring a correct handling of Omega-reduction.
  */
  typedef omega_iterator iterator;
  //! A bidirectional %const_iterator on the disjuncts of a Powerset element.
  typedef omega_const_iterator const_iterator;

  //! The reverse iterator type built from Powerset::iterator.
  typedef std::reverse_iterator<iterator> reverse_iterator;
  //! The reverse iterator type built from Powerset::const_iterator.
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  //! \name Member Functions for the Direct Manipulation of Disjuncts
  //@{

  /*! \brief
    Drops from the sequence of disjuncts in \p *this all the
    non-maximal elements so that \p *this is non-redundant.

    This method is declared <CODE>const</CODE> because, even though
    Omega-reduction may change the syntactic representation of \p *this,
    its semantics will be unchanged.
  */
  void omega_reduce() const;

  //! Returns the number of disjuncts.
  size_type size() const;

  //! Returns <CODE>true</CODE> if and only if there are no disjuncts.
  bool empty() const;

  /*! \brief
    Returns an iterator pointing to the first disjunct, if \p *this
    is not empty; otherwise, returns the past-the-end iterator.
  */
  iterator begin();

  //! Returns the past-the-end iterator.
  iterator end();

  /*! \brief
    Returns a const_iterator pointing to the first disjunct, if \p *this
    is not empty; otherwise, returns the past-the-end const_iterator.
  */
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  /*! \brief
    Returns a reverse_iterator pointing to the last disjunct, if \p *this
    is not empty; otherwise, returns the before-the-start reverse_iterator.
  */
  reverse_iterator rbegin();
  //! Returns the before-the-start reverse_iterator.
  reverse_iterator rend();
  /*! \brief
    Returns a const_reverse_iterator pointing to the last disjunct,
    if \p *this is not empty; otherwise, returns the before-the-start
    const_reverse_iterator.
  */
  const_reverse_iterator rbegin() const;
  //! Returns the before-the-start const_reverse_iterator.
  const_reverse_iterator rend() const;

  //! Adds to \p *this the disjunct \p d.
  void add_disjunct(const D& d);

  /*! \brief
    Drops the disjunct in \p *this pointed to by \p position, returning
    an iterator to the disjunct following \p position.
  */
  iterator drop_disjunct(iterator position);

  //! Drops all the disjuncts from \p first to \p last (excluded).
  void drop_disjuncts(iterator first, iterator last);

  //! Drops all the disjuncts, making \p *this an empty powerset.
  void clear();

  //@} // Member Functions for the Direct Manipulation of Disjuncts

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

  /*! \brief
    If \p *this is not empty (i.e., it is not the bottom element),
    it is reduced to a singleton obtained by computing an upper-bound
    of all the disjuncts.
  */
  void collapse();

  //@} // Member Functions that May Modify the Powerset element

protected:
  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this does not contain
    non-maximal elements.
  */
  bool is_omega_reduced() const;

  /*! \brief Upon return, \p *this will contain at most \p
    max_disjuncts elements; the set of disjuncts in positions greater
    than or equal to \p max_disjuncts, will be replaced at that
    position by their upper-bound.
  */
  void collapse(unsigned max_disjuncts);

  /*! \brief
    Adds to \p *this the disjunct \p d,
    assuming \p d is not the bottom element and ensuring
    partial Omega-reduction.

    If \p d is not the bottom element and is not Omega-redundant with
    respect to elements in positions between \p first and \p last, all
    elements in these positions that would be made Omega-redundant by the
    addition of \p d are dropped and \p d is added to the reduced
    sequence.
  */
  iterator add_non_bottom_disjunct(const D& d,
				   iterator first,
				   iterator last);

  /*! \brief
    Adds to \p *this the disjunct \p d,
    assuming \p d is not the bottom element.
  */
  void add_non_bottom_disjunct(const D& d);

  /*! \brief
    Assigns to \p *this the result of applying \p op_assign pairwise
    to the elements in \p *this and \p y.

    The elements of the powerset result are obtained by applying
    \p op_assign to each pair of elements whose components are drawn
    from \p *this and \p y, respectively.
  */
  template <typename Binary_Operator_Assign>
  void pairwise_apply_assign(const Powerset& y,
			     Binary_Operator_Assign op_assign);

private:
  /*! \brief
    Does the hard work of checking whether \p *this contains non-maximal
    elements and returns <CODE>true</CODE> if and only if it does not.
  */
  bool check_omega_reduced() const;

  /*! \brief
    Replaces the disjunct \p *sink by an upper bound of itself and
    all the disjuncts following it.
  */
  void collapse(Sequence_iterator sink);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A %const_iterator on the disjuncts of a Powerset element.
/*! \ingroup PPL_CXX_interface
  This class implements a read-only bidirectional iterator
  on the sequence of disjuncts.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename D>
class Parma_Polyhedra_Library::Powerset<D>::omega_const_iterator {
protected:
  //! The type of the underlying %const_iterator.
  typedef typename Powerset::Sequence::const_iterator Base;

  //! A shortcut for naming traits.
  typedef typename std::iterator_traits<Base> Traits;

  //! A %const_iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level const_iterator.
  omega_const_iterator(const Base& b);

  friend class Powerset;

public:
  // Same traits of the underlying const_iterator.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  omega_const_iterator();

  //! Copy constructor.
  omega_const_iterator(const omega_const_iterator& y);

  //! Constructs from the corresponding non-const iterator.
  omega_const_iterator(const omega_iterator& y);

  //! Dereference operator.
  reference operator*() const;

  //! Indirect member selector.
  pointer operator->() const;

  //! Prefix increment operator.
  omega_const_iterator& operator++();

  //! Postfix increment operator.
  omega_const_iterator operator++(int);

  //! Prefix decrement operator.
  omega_const_iterator& operator--();

  //! Postfix decrement operator.
  omega_const_iterator operator--(int);

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are identical.
  */
  bool operator==(const omega_const_iterator& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are different.
  */
  bool operator!=(const omega_const_iterator& y) const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An iterator on the disjuncts of a Powerset element.
/*! \ingroup PPL_CXX_interface
  This class implements a <EM>read-only</EM> bidirectional iterator
  on the sequence of disjuncts. That is, by using an instance of
  this iterator class it is not possible to overwrite the disjuncts
  contained in a Powerset element. However, using such an instance
  allows for the removal of disjuncts by using methods
  <CODE>Powerset::drop_disjunct(iterator position)</CODE> and
  <CODE>Powerset::drop_disjuncts(iterator first, iterator last)</CODE>.
  Such a policy is needed to allow for a reliable use of the Boolean
  flag <CODE>Powerset::reduced</CODE>.

  \note
  For any developers' need, (low-level) iterators on the sequence of
  disjuncts are still available by accessing the protected member
  <CODE>Powerset::sequence</CODE>.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename D>
class Parma_Polyhedra_Library::Powerset<D>::omega_iterator {
protected:
  //! The type of the underlying mutable iterator.
  typedef typename Powerset::Sequence::iterator Base;

  //! A shortcut for naming the const_iterator traits.
  typedef typename
  std::iterator_traits<typename Powerset::Sequence::const_iterator> Traits;

  //! A (mutable) iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level iterator.
  omega_iterator(const Base& b);

  friend class Powerset;
  friend Powerset<D>::omega_const_iterator
  ::omega_const_iterator(const omega_iterator& y);

public:
  // Same traits of the const_iterator, therefore
  // forbidding the direct modification of sequence elements.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  omega_iterator();

  //! Copy constructor.
  omega_iterator(const omega_iterator& y);

  //! Dereference operator.
  reference operator*() const;

  //! Indirect access operator.
  pointer operator->() const;

  //! Prefix increment operator.
  omega_iterator& operator++();

  //! Postfix increment operator.
  omega_iterator operator++(int);

  //! Prefix decrement operator.
  omega_iterator& operator--();

  //! Postfix decrement operator.
  omega_iterator operator--(int);

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are identical.
  */
  bool operator==(const omega_iterator& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are different.
  */
  bool operator!=(const omega_iterator& y) const;
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Mixed comparison operator: returns <CODE>true</CODE> if and only
  if (the const version of) \p x is identical to \p y.

  \relates Powerset::omega_const_iterator
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename D>
bool
operator==(const typename Powerset<D>::omega_iterator& x,
	   const typename Powerset<D>::omega_const_iterator& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Mixed comparison operator: returns <CODE>true</CODE> if and only
  if (the const version of) \p x is different from \p y.

  \relates Powerset::omega_const_iterator
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename D>
bool
operator!=(const typename Powerset<D>::omega_iterator& x,
	   const typename Powerset<D>::omega_const_iterator& y);

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename D>
void swap(Parma_Polyhedra_Library::Powerset<D>& x,
	  Parma_Polyhedra_Library::Powerset<D>& y);

} // namespace std

#include "Powerset.inlines.hh"

#endif // !defined(PPL_Powerset_defs_hh)
