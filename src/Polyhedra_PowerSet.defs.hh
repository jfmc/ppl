/* Polyhedra_PowerSet class declaration.
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

#ifndef PPL_Polyhedra_PowerSet_defs_hh
#define PPL_Polyhedra_PowerSet_defs_hh

#include "Polyhedra_PowerSet.types.hh"
#include "ConSys.types.hh"
#include "Constraint.types.hh"
#include "Polyhedron.defs.hh"
#include "Variable.defs.hh"
#include "Determinate.defs.hh"
#include "PowerSet.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <list>
#include <set>

//! The powerset construction instantiated on PPL polyhedra.
template <typename PH>
class Parma_Polyhedra_Library::Polyhedra_PowerSet
  : public Parma_Polyhedra_Library::PowerSet
<Parma_Polyhedra_Library::Determinate<PH> > {
private:
  typedef Determinate<PH> CS;
  typedef PowerSet<CS> Base;

public:
  //! Builds a universe (top) or empty (bottom) Polyhedra_PowerSet.
  /*!
    \param num_dimensions   The number of dimensions of the vector
                            space enclosing the powerset.
    \param universe         If <CODE>true</CODE>, a universe Polyhedra_PowerSet
                            is built;  an empty Polyhedra_PowerSet is built otherwise.
  */
  explicit
  Polyhedra_PowerSet(dimension_type num_dimensions = 0,
		     Polyhedron::Degenerate_Kind kind = Polyhedron::UNIVERSE);

  //! Ordinary copy-constructor.
  Polyhedra_PowerSet(const Polyhedra_PowerSet& y);

  //! Creates a Polyhedra_PowerSet with the same information contents as \p cs.
  Polyhedra_PowerSet(const ConSys& cs);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  Polyhedra_PowerSet& operator=(const Polyhedra_PowerSet& y);
  
private:
  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

public:
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

  //! Assigns to \p *this the concatenation of \p *this and \p y.
  /*!
    Seeing a powerset as a set of tuples, this method assigns to
    \p *this all the tuples that can be obtained by concatenating,
    in the order given, a tuple of \p *this with a tuple of \p y.
  */
  void concatenate_assign(const Polyhedra_PowerSet& y);

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

  void pairwise_reduce();

private:
  void extrapolation_assign(const Polyhedra_PowerSet& y,
			    void (Polyhedron::*wm)
			    (const Polyhedron&, unsigned*));
  void widening_assign(const Polyhedra_PowerSet& y,
		       void (Polyhedron::*wm)(const Polyhedron&, unsigned*),
		       unsigned max_disjuncts);

public:
  void H79_extrapolation_assign(const Polyhedra_PowerSet& y);
  void BHRZ03_extrapolation_assign(const Polyhedra_PowerSet& y);

  void collapse(unsigned max_disjuncts);

  void H79_widening_assign(const Polyhedra_PowerSet& y,
			   unsigned max_disjuncts);
  void BHRZ03_widening_assign(const Polyhedra_PowerSet& y,
			      unsigned max_disjuncts);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  typedef typename Base::Sequence Sequence;

  typedef typename Sequence::size_type size_type;

  typedef typename Base::iterator iterator;
  typedef typename Base::const_iterator const_iterator;
  typedef typename Base::reverse_iterator reverse_iterator;
  typedef typename Base::const_reverse_iterator const_reverse_iterator;
  typedef typename Base::value_type value_type;
};

#include "Polyhedra_PowerSet.inlines.hh"

#endif // !defined(PPL_Polyhedra_PowerSet_defs_hh)
