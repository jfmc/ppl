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
#include "Variable.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <list>
#include <set>

namespace Parma_Polyhedra_Library {

/*! \relates PowerSet */
template <typename CS>
PowerSet<CS>
operator+(const PowerSet<CS>&, const PowerSet<CS>&);

/*! \relates PowerSet */
template <typename CS>
PowerSet<CS>
operator*(const PowerSet<CS>&, const PowerSet<CS>&);

/*! \relates PowerSet */
template <typename CS>
CS
project(const PowerSet<CS>&);

/*! \relates PowerSet */
template <typename CS>
bool
operator==(const PowerSet<CS>& x, const PowerSet<CS>& y);

/*! \relates PowerSet */
template <typename CS>
int
lcompare(const PowerSet<CS>&, const PowerSet<CS>&);

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::PowerSet */
template <typename CS>
std::ostream&
operator<<(std::ostream&, const PowerSet<CS>&);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The powerset construction on constraint systems.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename CS>
class Parma_Polyhedra_Library::PowerSet {
public:
  //! Builds a universe (top) or empty (bottom) PowerSet.
  /*!
    \param num_dimensions   The number of dimensions of the vector
                            space enclosing the powerset.
    \param universe         If <CODE>true</CODE>, a universe PowerSet
                            is built;  an empty PowerSet is built otherwise.
  */
  explicit PowerSet(dimension_type num_dimensions = 0,
		    bool universe = true);

  //! Creates a PowerSet with the same information contents as \p cs.
  PowerSet(const ConSys& cs);

  //! Injects \p c into \p *this.
  PowerSet& inject(const CS& c);

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const PowerSet& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const PowerSet& y);

  void concatenate_assign(const PowerSet& y);

  //! \brief
  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const PowerSet& y) const;

  inline bool is_top() const;
  inline bool is_bottom() const;

  friend CS project<>(const PowerSet& x);

  friend int lcompare<>(const PowerSet& x, const PowerSet& y);

  //friend std::ostream& operator <<<>(std::ostream& s, const PowerSet& x);

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

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref H79_widening "H79-widening" between \p *this and \p y.
  /*!
    \param y           A polyhedron that <EM>must</EM>
                       be contained in \p *this.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void H79_extrapolation_assign(const PowerSet& y);

  //! \brief
  //! Limits the \ref H79_widening "H79-widening" computation
  //! between \p *this and \p y by enforcing constraints \p cs
  //! and assigns the result to \p *this.
  /*!
    \param y                 A polyhedron that <EM>must</EM>
                             be contained in \p *this.
    \param cs                The system of constraints that limits
                             the widened polyhedron. It is not
                             declared <CODE>const</CODE>
                             because it can be modified.
    \exception std::invalid_argument thrown if \p *this, \p y and \p cs
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void limited_H79_extrapolation_assign(const PowerSet& y, const ConSys& cs);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  typedef std::list<CS> Sequence;
  typedef typename Sequence::const_reference const_reference;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

  void omega_reduction();

  bool definitely_contains(const CS& y) const;

public:
  typedef typename Sequence::iterator iterator;
  typedef typename Sequence::const_iterator const_iterator;
  typedef typename Sequence::reverse_iterator reverse_iterator;
  typedef typename Sequence::const_reverse_iterator const_reverse_iterator;
  typedef typename Sequence::value_type value_type;

  dimension_type size() const;

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  reverse_iterator rbegin();
  const_reverse_iterator rbegin() const;

  reverse_iterator rend();
  const_reverse_iterator rend() const;
};

#include "PowerSet.inlines.hh"

#endif // !defined(PPL_PowerSet_defs_hh)
