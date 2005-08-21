/* Ask_Tell class declaration.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Ask_Tell_defs_hh
#define PPL_Ask_Tell_defs_hh

#include "Ask_Tell.types.hh"
#include "Constraint_System.types.hh"
#include "Constraint.types.hh"
#include "Variable.defs.hh"
#include "globals.defs.hh"
#include <iosfwd>
#include <list>

namespace Parma_Polyhedra_Library {

template <typename CS>
Ask_Tell<CS>
operator+(const Ask_Tell<CS>&, const Ask_Tell<CS>&);

template <typename CS>
Ask_Tell<CS>
operator*(const Ask_Tell<CS>&, const Ask_Tell<CS>&);

template <typename CS>
CS
project(const Ask_Tell<CS>&);

namespace IO_Operators {

template <typename CS>
std::ostream&
operator<<(std::ostream&, const Ask_Tell<CS>&);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A pair of (ask and tell) constraints.
template <typename CS>
class Parma_Polyhedra_Library::Ask_Tell_Pair {
public:
  //! Pair constructor.
  Ask_Tell_Pair(const CS& ask, const CS& tell);

  //! Const accessor to the <EM>ask</EM> component.
  const CS& ask() const;

  //! Non-const accessor to the <EM>ask</EM> component.
  CS& ask();

  //! Const accessor to the <EM>ask</EM> component.
  const CS& tell() const;

  //! Non-const accessor to the <EM>tell</EM> component.
  CS& tell();

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const Ask_Tell_Pair& y) const;

private:
  //! The <EM>ask</EM> component.
  CS a;

  //! The <EM>tell</EM> component.
  CS t;
};

//! The ask and tell construction on constraint systems.
/*!
  This class offers a generic implementation of <EM>ask-and-tell
  constraint systems</EM> as defined in \ref Bag98 "[Bag98]".
*/
template <typename CS>
class Parma_Polyhedra_Library::Ask_Tell {
protected:
  void pair_insert(const CS& a, const CS& t);
  void pair_insert_good(const CS& a, const CS& t);

  bool reduce();
  bool deduce();
  bool absorb();
  void engine();

  bool probe(const CS& tellv, const CS& askv) const;

public:
  //! Builds a universe (top) or empty (bottom) ask-and-tell agent.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the
    ask-and-tell agent;

    \param universe
    If <CODE>true</CODE>, a universe ask-and-tell agent is built; an
    empty agent is built otherwise.
  */
  explicit Ask_Tell(dimension_type num_dimensions = 0,
		   bool universe = true);

  //! Ordinary copy-constructor.
  Ask_Tell(const Ask_Tell& y);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  Ask_Tell& operator=(const Ask_Tell& y);

  //! Swaps \p *this with \p y.
  void swap(Ask_Tell& y);

  //! \brief
  //! Creates an ask-and-tell constraint system with the same
  //! information contents as \p cs.
  Ask_Tell(const Constraint_System& cs);

  //! Adds to \p *this the pair constituted by \p ask and \p tell.
  Ask_Tell& add_pair(const CS& ask, const CS& tell);

  Ask_Tell& bottom();

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const Ask_Tell& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const Ask_Tell& y);

  //! Assigns to \p *this the concatenation of \p *this and \p y.
  /*!
    Seeing an ask-and-tell agent as a set of tuples, this method
    assigns to \p *this all the tuples that can be obtained by
    concatenating, in the order given, a tuple of \p *this with
    a tuple of \p y.
  */
  void concatenate_assign(const Ask_Tell& y);

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const Ask_Tell& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is the top
  //! element of the ask-and-tell constraint system (i.e., it
  //! represents the universe).
  bool is_top() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is the bottom
  //! element of the ask-and-tell constraint system (i.e., it
  //! represents the empty set).
  bool is_bottom() const;

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Intersects \p *this with (a copy of) constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are topology-incompatible
    or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Intersects \p *this with (a copy of) the constraints in \p cs.
  /*!
    \param cs
    Contains the constraints to intersect with.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void add_constraints(const Constraint_System& cs);

  //! \brief
  //! Adds \p m new dimensions to the vector space, embedding
  //! the old polyhedron in the new space.
  void add_space_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new dimensions to the vector space
  //! and does not embed it in the new space.
  void add_space_dimensions_and_project(dimension_type m);

  //! Removes all the specified dimensions from the vector space.
  /*!
    \param to_be_removed
    The set of Variable objects corresponding to the space dimensions
    to be removed.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with one of the
    Variable objects contained in \p to_be_removed.
  */
  void remove_space_dimensions(const Variables_Set& to_be_removed);

  //! \brief
  //! Removes the higher dimensions of the vector space so that
  //! the resulting space will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument
    Thrown if \p new_dimensions is greater than the space dimension of
    \p *this.
  */
  void remove_higher_space_dimensions(dimension_type new_dimension);

  template <typename Partial_Function>
  void map_space_dimensions(const Partial_Function& pfunc);

  void H79_extrapolation_assign(const Ask_Tell& y);

  friend Ask_Tell operator +<>(const Ask_Tell<CS>& x,
			      const Ask_Tell<CS>& y);
  friend Ask_Tell operator *<>(const Ask_Tell& x, const Ask_Tell& y);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  friend CS project<>(const Ask_Tell& x);

private:
  //! An ask-tell agent is implemented as a sequence of ask-tell pairs
  /*!
    The particular sequence employed must support efficient deletion
    in any position and efficient back insertion.
  */
  typedef std::list<Ask_Tell_Pair<CS> > Sequence;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

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
  iterator erase(iterator first, iterator last) {
    return sequence.erase(first, last);
  }
  iterator erase(iterator position) {
    return sequence.erase(position);
  }
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Ask_Tell */
template <typename CS>
void swap(Parma_Polyhedra_Library::Ask_Tell<CS>& x,
	  Parma_Polyhedra_Library::Ask_Tell<CS>& y);

} // namespace std

#include "Ask_Tell.inlines.hh"

#endif // !defined(PPL_Ask_Tell_defs_hh)
