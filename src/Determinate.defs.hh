/* Determinate class declaration.
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

#ifndef PPL_Determinate_defs_hh
#define PPL_Determinate_defs_hh

#include "Determinate.types.hh"
#include "ConSys.types.hh"
#include "GenSys.types.hh"
#include "Variable.defs.hh"
#include "Polyhedron.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <cassert>

namespace Parma_Polyhedra_Library {

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are the same polyhedron.
/*!
  \relates Determinate
  \exception std::invalid_argument
  Thrown if \p x and \p y are topology-incompatible or
  dimension-incompatible.
*/
template <typename PH>
bool operator==(const Determinate<PH>& x, const Determinate<PH>& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are different polyhedra.
/*!
  \relates Determinate
  \exception std::invalid_argument
  Thrown if \p x and \p y are topology-incompatible or
  dimension-incompatible.
*/
template <typename PH>
bool operator!=(const Determinate<PH>& x, const Determinate<PH>& y);

//! Computes an upper bound of \p x and \p y.
/*! \relates Determinate */
template <typename PH>
Determinate<PH>
operator+(const Determinate<PH>& x, const Determinate<PH>& y);

//! Computes the meet of \p x and \p y.
/*! \relates Determinate */
template <typename PH>
Determinate<PH>
operator*(const Determinate<PH>& x, const Determinate<PH>& y);

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PH>
std::ostream&
operator<<(std::ostream&, const Determinate<PH>&);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! Wraps a polyhedron class into a determinate constraint system interface.
template <typename PH>
class Parma_Polyhedra_Library::Determinate {
public:
  //! \brief
  //! Builds either the top or the bottom of the determinate constraint
  //! system defined on the vector space having \p num_dimensions
  //! dimensions.
  /*!
    The top element, corresponding to the universe polyhedron,
    is built if \p universe is \c true; otherwise the bottom element
    (corresponding to the empty polyhedron) is built. By default,
    the top of a zero-dimension vector space is built.
  */
  explicit
  Determinate(dimension_type num_dimensions = 0, bool universe = true);

  //! \brief
  //! Injection operator: builds the determinate constraint system element
  //! corresponding to polyhedron \p p.
  Determinate(const PH& p);

  //! \brief
  //! Injection operator: builds the determinate constraint system element
  //! corresponding to the polyhedron represented by \p cs.
  Determinate(const ConSys& cs);

  //! Copy constructor.
  Determinate(const Determinate& y);

  //! Destructor.
  ~Determinate();

  //! Assignment operator.
  Determinate& operator=(const Determinate& y);

  //! Swaps \p *this with \p y.
  void swap(Determinate& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! On return from this method, the representation of \p *this
  //! is not shared by different Determinate objects.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void mutate();

  //! \brief
  //! Assigns to \p *this the upper bound (i.e., poly-hull)
  //! of \p *this and \p y.
  void upper_bound_assign(const Determinate& y);

  //! Assigns to \p *this the meet (i.e., intersection) of \p *this and \p y.
  void meet_assign(const Determinate& y);

  //! Assigns to \p *this the concatenation of \p *this with \p y.
  void concatenate_assign(const Determinate& y);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this entails \p y.
  //! (i.e., \p x is contained into \p y).
  bool definitely_entails(const Determinate& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this and \p y
  //! are equivalent.
  bool is_definitely_equivalent_to(const Determinate& y) const;

  Determinate& operator <<= (dimension_type n);
  Determinate& hide_assign(dimension_type n);

  //! Returns a const reference to the embedded polyhedron.
  const PH& polyhedron() const;

  //! Returns a reference to the embedded polyhedron.
  PH& polyhedron();

  //! \brief
  //! Returns \c true if and only if \p *this is the top of the
  //! determinate constraint system (i.e., the universe polyhedron).
  inline bool is_top() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is the bottom
  //! of the determinate constraint system (i.e., the empty polyhedron).
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

  //! Intersects \p *this with (a copy of) constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are topology-incompatible
    or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Intersects \p *this with the constraints in \p cs.
  /*!
    \param cs
    The constraints to intersect with.  This parameter is not declared
    <CODE>const</CODE> because it can be modified.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
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
    \param to_be_removed
    The set of Variable objects corresponding to the dimensions to be
    removed.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with one of the
    Variable objects contained in \p to_be_removed.
  */
  void remove_dimensions(const Variables_Set& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument
    Thrown if \p new_dimensions is greater than the space dimension
    of \p *this.
  */
  void remove_higher_dimensions(dimension_type new_dimension);

  template <typename PartialFunction>
  void map_dimensions(const PartialFunction& pfunc);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref H79_widening "H79-widening" between \p *this and \p y.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void H79_widening_assign(const Determinate& y);

  //! \brief
  //! Limits the \ref H79_widening "H79-widening" computation
  //! between \p *this and \p y by enforcing constraints \p cs
  //! and assigns the result to \p *this.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param cs
    The system of constraints that limits the widened polyhedron. It
    is not declared <CODE>const</CODE> because it can be modified.

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void limited_H79_extrapolation_assign(const Determinate& y, ConSys& cs);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! The possibly shared representation of a Determinate object.
  /*!
    By adopting the <EM>copy-on-write</EM> technique, a single
    representation of the polyhedron may be shared by more than
    one object of the class Determinate.
  */
  class Rep {
  private:
    //! \brief
    //! Count the number of references:
    //! -   0: leaked, \p ph is non-const;
    //! -   1: one reference, \p ph is non-const;
    //! - > 1: more than one reference, \p ph is const.
    mutable unsigned long references;

    //! Private and unimplemented: assignment not allowed.
    Rep& operator=(const Rep& y);

    //! Private and unimplemented: copies not allowed.
    Rep(const Rep& y);

    //! Private and unimplemented: default construction not allowed.
    Rep();

  public:
    //! A possibly shared polyhedron object.
    PH ph;

    //! \brief
    //! Builds a new representation by creating a polyhedron object
    //! of the specified kind, in the specified vector space.
    Rep(dimension_type num_dimensions, Polyhedron::Degenerate_Kind kind);

    //! Builds a new representation by copying polyhedron \p p.
    Rep(const PH& p);

    //! Builds a new representation by copying the constraints in \p cs.
    Rep(const ConSys& cs);

    //! Destructor.
    ~Rep();

    //! Registers a new reference.
    void new_reference() const;

    //! \brief
    //! Unregisters one reference; returns <CODE>true</CODE> if and only if
    //! the representation has become unreferenced.
    bool del_reference() const;

    //! True if and only if this representation is currently shared.
    bool is_shared() const;
  };

  //! A pointer to the possibly shared representation of the polyhedron.
  Rep* prep;
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PH>
void swap(Parma_Polyhedra_Library::Determinate<PH>& x,
	  Parma_Polyhedra_Library::Determinate<PH>& y);

} // namespace std

#include "Determinate.inlines.hh"

#endif // !defined(PPL_Determinate_defs_hh)
