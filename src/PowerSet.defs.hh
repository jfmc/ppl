/* PowerSet class declaration.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_PowerSet_defs_hh
#define PPL_PowerSet_defs_hh

#include "PowerSet.types.hh"
#include "ConSys.types.hh"
#include "Constraint.types.hh"
#include "Variable.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <list>
#include <set>

namespace Parma_Polyhedra_Library {

//! Returns the lattice upper bound (i.e., the union) of \p x and \p y. 
/*! \relates PowerSet */
template <typename CS>
PowerSet<CS>
operator+(const PowerSet<CS>& x, const PowerSet<CS>& y);

//! Returns the lattice meet (i.e., the intersection) of \p x and \p y. 
/*! \relates PowerSet */
template <typename CS>
PowerSet<CS>
operator*(const PowerSet<CS>& x, const PowerSet<CS>& y);

//! Returns the projection of \p x onto the underlying constraint system.
/*!
  \relates PowerSet
  The result is obtained by computing the <CODE>CS</CODE>-upper-bound
  (i.e., the poly-hull) of all the elements in \p x.
*/
template <typename CS>
CS
project(const PowerSet<CS>& x);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates PowerSet */
template <typename CS>
bool
operator==(const PowerSet<CS>& x, const PowerSet<CS>& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are not equivalent.
/*! \relates PowerSet */
template <typename CS>
bool
operator!=(const PowerSet<CS>& x, const PowerSet<CS>& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::PowerSet */
template <typename CS>
std::ostream&
operator<<(std::ostream& s, const PowerSet<CS>& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


//! The powerset construction on constraint systems.
/*!
  This class offers a generic implementation of <EM>powerset
  constraint systems</EM> as defined in \ref Bag98 "[Bag98]".
*/
template <typename CS>
class Parma_Polyhedra_Library::PowerSet {
public:
  //! Default constructor.
  PowerSet();

  //! Ordinary copy-constructor.
  PowerSet(const PowerSet& y);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  PowerSet& operator=(const PowerSet& y);
  
  //! Creates a PowerSet with the same information contents as \p cs.
  PowerSet(const ConSys& cs);

  //! Adds to \p *this the disjunct \p d.
  PowerSet& add_disjunct(const CS& d);

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const PowerSet& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const PowerSet& y);

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const PowerSet& y) const;

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

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  // TO BE REMOVED.
  friend CS project<>(const PowerSet& x);

protected:
  //! A powerset is implemented as a sequence of elements.
  /*!
    The particular sequence employed must support efficient deletion
    in any position and efficient back insertion.
  */
  typedef std::list<CS> Sequence;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

  void omega_reduction();

  bool definitely_contains(const CS& y) const;

public:
  typedef typename Sequence::size_type size_type;

  size_type size() const;

  typedef typename Sequence::iterator iterator;
  typedef typename Sequence::const_iterator const_iterator;
  typedef typename Sequence::reverse_iterator reverse_iterator;
  typedef typename Sequence::const_reverse_iterator const_reverse_iterator;
  typedef typename Sequence::value_type value_type;

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  reverse_iterator rbegin();
  const_reverse_iterator rbegin() const;

  reverse_iterator rend();
  const_reverse_iterator rend() const;

//protected:
  void push_back(const CS& y);
  void pop_back();
  iterator erase(iterator first, iterator last) {
    return sequence.erase(first, last);
  }
  iterator erase(iterator position) {
    return sequence.erase(position);
  }
};

#include "PowerSet.inlines.hh"

#endif // !defined(PPL_PowerSet_defs_hh)
