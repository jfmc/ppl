/* Determinate class declaration.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Determinate_defs_hh
#define PPL_Determinate_defs_hh

#include "Determinate.types.hh"
#include <iosfwd>
#include <cassert>

namespace Parma_Polyhedra_Library {

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are the same polyhedron.
/*!
  \relates Determinate<PH>
  \exception std::invalid_argument thrown if \p x and \p y
                                          are topology-incompatible
                                          or dimension-incompatible.
*/
template <typename PH>
bool operator==(const Determinate<PH>& x, const Determinate<PH>& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are different polyhedra.
/*!
  \relates Determinate<PH>
  \exception std::invalid_argument thrown if \p x and \p y
  are topology-incompatible
  or dimension-incompatible.
*/
template <typename PH>
bool operator!=(const Determinate<PH>& x, const Determinate<PH>& y);

template <typename PH>
bool
lcompare(const Determinate<PH>& x, const Determinate<PH>& y);

#if 0
template <typename PH>
Determinate<PH>
operator+(const Determinate<PH>& x, const Determinate<PH>& y);

template <typename PH>
Determinate<PH>
operator*(const Determinate<PH>& x, const Determinate<PH>& y);
#endif

template <typename PH>
std::ostream&
operator<<(std::ostream&, const Determinate<PH>&);

} // namespace Parma_Polyhedra_Library

//! Wrap a polyhedron class into a determinate constraint system interface.
template <typename PH>
class Parma_Polyhedra_Library::Determinate {
public:
  explicit
  Determinate(dimension_type num_dimensions = 0,
	      Polyhedron::Degenerate_Kind kind = Polyhedron::UNIVERSE);
  Determinate(const PH& p);
  Determinate(const ConSys& cs);
  Determinate(const Determinate& y);
  ~Determinate();

  Determinate& operator=(const Determinate& y);

  void mutate();

  void upper_bound_assign(const Determinate& y);

  void meet_assign(const Determinate& y);

  void concatenate_assign(const Determinate& y);

  bool definitely_entails(const Determinate& y) const;

  bool is_definitely_equivalent_to(const Determinate& y) const;

  Determinate& operator <<= (dimension_type n);
  Determinate& hide_assign(dimension_type n);

  inline bool is_top() const;
  inline bool is_bottom() const;

  friend bool
  operator==<PH>(const Determinate<PH>& x, const Determinate<PH>& y);
  friend bool
  operator!=<PH>(const Determinate<PH>& x, const Determinate<PH>& y);

#if 0
  friend Determinate operator +<>(const Determinate& x,
				  const Determinate& y);
  friend Determinate operator *<>(const Determinate& x,
				  const Determinate& y);
#endif
  friend bool lcompare<>(const Determinate& x, const Determinate& y);

  friend std::ostream& operator<<<>(std::ostream& s, const Determinate& x);

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the system of constraints.
  const ConSys& constraints() const;

  //! Returns the system of constraints, with no redundant constraint.
  const ConSys& minimized_constraints() const;

  //! Returns the system of generators.
  const GenSys& generators() const;

  //! Returns the system of generators, with no redundant generator.
  const GenSys& minimized_generators() const;

  //! \brief
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

  //! \brief
  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed  The set of Variable objects corresponding
                          to the dimensions to be removed.
    \exception std::invalid_argument thrown if \p *this is
                                     dimension-incompatible with one
				     of the Variable objects contained
				     in \p to_be_removed.
  */
  void remove_dimensions(const std::set<Variable>& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument thrown if \p new_dimensions is greater
                                     than the space dimension of \p *this.
  */
  void remove_higher_dimensions(dimension_type new_dimension);

  template <typename PartialFunction>
  void shuffle_dimensions(const PartialFunction& pfunc);

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
  void H79_widening_assign(const Determinate& y);

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
  void limited_H79_widening_assign(const Determinate& y, ConSys& cs);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  class Rep {
  private:
    //! \brief
    //! Count the number of references:
    //! -   0: leaked, \p pph is non-const;
    //! -   1: one reference, \p pph is non-const;
    //! - > 1: more than one reference, \p pph is const.
    mutable unsigned long references;

    //! Private and unimplemented.
    Rep& operator=(const Rep& y);
    Rep(const Rep& y);
    Rep();

  public:
    //! A polyhedron.
    PH ph;

    Rep(dimension_type num_dimensions, Polyhedron::Degenerate_Kind kind);

    Rep(const PH& p);

    Rep(const ConSys& cs);

    //! Destructor.
    ~Rep();

    //! Register a new reference.
    void new_reference() const;

    //! \brief
    //! Unregister a reference and return true if the representation
    //! has become unreferenced.
    bool del_reference() const;

    //! True if and only if this representation is currently shared.
    bool is_shared() const;
  };

  Rep* prep;
};

#include "Determinate.inlines.hh"

#endif // !defined(PPL_Determinate_defs_hh)
