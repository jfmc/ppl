/* AskTell class declaration.
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

#ifndef PPL_AskTell_defs_hh
#define PPL_AskTell_defs_hh

#include "AskTell.types.hh"
#include "ConSys.types.hh"
#include "Constraint.types.hh"
#include "Variable.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <list>

namespace Parma_Polyhedra_Library {

template <typename CS>
AskTell<CS>
operator+(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
AskTell<CS>
operator*(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
CS
project(const AskTell<CS>&);

namespace IO_Operators {

template <typename CS>
std::ostream&
operator<<(std::ostream&, const AskTell<CS>&);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A pair of (ask and tell) constraints.
template <typename CS>
class Parma_Polyhedra_Library::AskTell_Pair {
public:
  //! Pair constructor.
  AskTell_Pair(const CS& ask, const CS& tell);

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
  bool definitely_entails(const AskTell_Pair& y) const;
  
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
class Parma_Polyhedra_Library::AskTell {
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
    \param num_dimensions   The number of dimensions of the vector
                            space enclosing the ask-and-tell agent.
    \param universe         If <CODE>true</CODE>, a universe ask-and-tell
                            agent is built;  an empty agent is built
                            otherwise.
  */
  explicit AskTell(dimension_type num_dimensions = 0,
		   bool universe = true);

  //! Ordinary copy-constructor.
  AskTell(const AskTell& y);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  AskTell& operator=(const AskTell& y);
  
  //! Creates an ask-and-tell constraint system with the same information contents as \p cs.
  AskTell(const ConSys& cs);

  //! Adds to \p *this the pair constituted by \p ask and \p tell.
  AskTell& add_pair(const CS& ask, const CS& tell);

  AskTell& bottom();

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const AskTell& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const AskTell& y);

  //! Assigns to \p *this the concatenation of \p *this and \p y.
  /*!
    Seeing an ask-and-tell agent as a set of tuples, this method
    assigns to \p *this all the tuples that can be obtained by
    concatenating, in the order given, a tuple of \p *this with
    a tuple of \p y.
  */
  void concatenate_assign(const AskTell& y);

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const AskTell& y) const;

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
    \exception std::invalid_argument thrown if \p *this and constraint \p c
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Intersects \p *this with the constraints in \p cs.
  /*!
    \param  cs             The constraints to intersect with.
                           This parameter is not declared
                           <CODE>const</CODE> because  it can be modified.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraints(ConSys& cs);

  //! \brief
  //! Adds \p m new dimensions and embeds the old polyhedron
  //! into the new space.
  void add_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new dimensions to the polyhedron
  //! and does not embed it in the new space.
  void add_dimensions_and_project(dimension_type m);

  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed  The set of Variable objects corresponding
                          to the dimensions to be removed.
    \exception std::invalid_argument thrown if \p *this is
                                     dimension-incompatible with one
				     of the Variable objects contained
				     in \p to_be_removed.
  */
  void remove_dimensions(const Variables_Set& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument thrown if \p new_dimensions is greater
                                     than the space dimension of \p *this.
  */
  void remove_higher_dimensions(dimension_type new_dimension);

  template <typename PartialFunction>
  void map_dimensions(const PartialFunction& pfunc);

  friend AskTell operator +<>(const AskTell<CS>& x,
			      const AskTell<CS>& y);
  friend AskTell operator *<>(const AskTell& x, const AskTell& y);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  friend CS project<>(const AskTell& x);

private:
  //! An ask-tell agent is implemented as a sequence of ask-tell pairs
  /*!
    The particular sequence employed must support efficient deletion
    in any position and efficient back insertion.
  */
  typedef std::list<AskTell_Pair<CS> > Sequence;

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

#include "AskTell.inlines.hh"

#endif // !defined(PPL_AskTell_defs_hh)
