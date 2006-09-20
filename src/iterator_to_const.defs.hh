/* iterator_to_const and const_iterator_to_const class declarations.
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

#ifndef PPL_iterator_to_const_hh
#define PPL_iterator_to_const_hh 1

#include "iterator_to_const.types.hh"

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
template <typename Container>
class Parma_Polyhedra_Library::iterator_to_const {
  // FIXME
  //protected:
public:
  //! The type of the underlying mutable iterator.
  typedef typename Container::iterator Base;

  //! A shortcut for naming the const_iterator traits.
  typedef typename
  std::iterator_traits<typename Container::const_iterator> Traits;

  //! A (mutable) iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level iterator.
  iterator_to_const(const Base& b);

  friend class const_iterator_to_const<Container>;

public:
  // Same traits of the const_iterator, therefore
  // forbidding the direct modification of sequence elements.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  iterator_to_const();

  //! Copy constructor.
  iterator_to_const(const iterator_to_const& y);

  //! Dereference operator.
  reference operator*() const;

  //! Indirect access operator.
  pointer operator->() const;

  //! Prefix increment operator.
  iterator_to_const& operator++();

  //! Postfix increment operator.
  iterator_to_const operator++(int);

  //! Prefix decrement operator.
  iterator_to_const& operator--();

  //! Postfix decrement operator.
  iterator_to_const operator--(int);

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are identical.
  */
  bool operator==(const iterator_to_const& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are different.
  */
  bool operator!=(const iterator_to_const& y) const;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A %const_iterator on the disjuncts of a Powerset element.
/*! \ingroup PPL_CXX_interface
  This class implements a read-only bidirectional iterator
  on the sequence of disjuncts.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename Container>
class Parma_Polyhedra_Library::const_iterator_to_const {
  // FIXME
  //protected:
public:
  //! The type of the underlying %const_iterator.
  typedef typename Container::const_iterator Base;

  //! A shortcut for naming traits.
  typedef typename std::iterator_traits<Base> Traits;

  //! A %const_iterator on the sequence of elements.
  Base base;

  //! Constructs from the lower-level const_iterator.
  const_iterator_to_const(const Base& b);

  friend class iterator_to_const<Container>;

public:
  // Same traits of the underlying const_iterator.
  typedef typename Traits::iterator_category iterator_category;
  typedef typename Traits::value_type value_type;
  typedef typename Traits::difference_type difference_type;
  typedef typename Traits::pointer pointer;
  typedef typename Traits::reference reference;

  //! Default constructor.
  const_iterator_to_const();

  //! Copy constructor.
  const_iterator_to_const(const const_iterator_to_const& y);

  //! Constructs from the corresponding non-const iterator.
  const_iterator_to_const(const iterator_to_const<Container>& y);

  //! Dereference operator.
  reference operator*() const;

  //! Indirect member selector.
  pointer operator->() const;

  //! Prefix increment operator.
  const_iterator_to_const& operator++();

  //! Postfix increment operator.
  const_iterator_to_const operator++(int);

  //! Prefix decrement operator.
  const_iterator_to_const& operator--();

  //! Postfix decrement operator.
  const_iterator_to_const operator--(int);

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are identical.
  */
  bool operator==(const const_iterator_to_const& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are different.
  */
  bool operator!=(const const_iterator_to_const& y) const;
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Mixed comparison operator: returns <CODE>true</CODE> if and only
  if (the const version of) \p x is identical to \p y.

  \relates const_iterator_to_const
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename Container>
bool
operator==(const iterator_to_const<Container>& x,
	   const const_iterator_to_const<Container>& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Mixed comparison operator: returns <CODE>true</CODE> if and only
  if (the const version of) \p x is different from \p y.

  \relates const_iterator_to_const
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename Container>
bool
operator!=(const iterator_to_const<Container>& x,
	   const const_iterator_to_const<Container>& y);

} // namespace Parma_Polyhedra_Library

#include "iterator_to_const.inlines.hh"

#endif // !defined(PPL_iterator_to_const_hh)
