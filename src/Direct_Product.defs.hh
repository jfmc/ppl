/* Direct_Product class declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Direct_Product_defs_hh
#define PPL_Direct_Product_defs_hh 1

#include "Direct_Product.types.hh"
#include "globals.defs.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Congruence.types.hh"
#include "Grid_Generator.types.hh"
#include "Constraint_System.types.hh"
#include "Generator_System.types.hh"
#include "Congruence_System.types.hh"
#include "Grid_Generator_System.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "Variables_Set.types.hh"

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Direct_Product
  Writes a textual representation of \p dp on \p s.
*/
template <typename D1, typename D2>
std::ostream&
operator<<(std::ostream& s, const Direct_Product<D1, D2>& dp);

} // namespace IO_Operators

/*! \brief
  Returns <CODE>true</CODE> if and only if the components of \p x and \p y
  are pairwise equal.

  \relates Direct_Product
  Note that \p x and \p y may be dimension-incompatible: in
  those cases, the value <CODE>false</CODE> is returned.
*/
template <typename D1, typename D2>
bool operator==(const Direct_Product<D1, D2>& x,
		const Direct_Product<D1, D2>& y);

/*! \brief
  Returns <CODE>true</CODE> if and only if the components of \p x and \p y
  are not pairwise equal.

  \relates Direct_Product
  Note that \p x and \p y may be dimension-incompatible: in
  those cases, the value <CODE>true</CODE> is returned.
*/
template <typename D1, typename D2>
bool operator!=(const Direct_Product<D1, D2>& x,
		const Direct_Product<D1, D2>& y);

} // namespace Parma_Polyhedra_Library


//! A Direct Product.
/*! \ingroup PPL_CXX_interface
  An object of the class Direct_Product represents a direct product.

  FIXME: Continue here.
*/

template <typename D1, typename D2>
class Parma_Polyhedra_Library::Direct_Product {
public:
  //! Returns the maximum space dimension this Direct_Product can handle.
  static dimension_type max_space_dimension();

  //! Builds an object having the specified properties.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the pair;

    \param kind
    Specifies whether a universe or an empty pair has to be built.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(dimension_type num_dimensions = 0,
			  Degenerate_Element kind = UNIVERSE);

  //! Builds a pair, copying a system of congruences.
  /*!
    The pair inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(const Congruence_System& cgs);

  //! Builds a pair, recycling a system of congruences.
  /*!
    The pair inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences to be approximates by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(Congruence_System& cgs);

  //! Builds a pair, copying a system of constraints.
  /*!
    The pair inherits the space dimension of the constraint system.

    \param cs
    The system of constraints to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(const Constraint_System& cs);

  //! Builds a pair, recycling a system of constraints.
  /*!
    The pair inherits the space dimension of the constraint system.

    \param cs
    The system of constraints to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(Constraint_System& cs);

  //! Builds a pair, copying a system of grid generators.
  /*!
    The pair inherits the space dimension of the grid generator system.

    \param const_gs
    The system of grid generators to be approximated by the pair.

    \exception std::invalid_argument
    Thrown if the system of grid generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(const Grid_Generator_System& const_gs);

  //! Builds a pair, recycling a system of grid generators.
  /*!
    The pair inherits the space dimension of the grid generator system.

    \param gs
    The system of grid generators to be approximated by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::invalid_argument
    Thrown if the system of grid generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space dimension.
  */
  explicit Direct_Product(Grid_Generator_System& gs);

  //! Builds a pair, copying a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param const_gs
    The system of generators to be approximated by the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Direct_Product(const Generator_System& const_gs);

  //! Builds a pair, recycling a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param gs
    The system of generators to be approximated by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space dimension.
  */
  explicit Direct_Product(Generator_System& gs);

  //! Builds a pair out of a box.
  /*!
    \param box
    The bounding box representing the pair to be built.  The box can
    contain only point and universe intervals;

    \exception std::length_error
    Thrown if the space dimension of \p box exceeds the maximum
    allowed space dimension.

    \exception std::invalid_argument
    FIXME: what???
    Thrown if \p box contains at least one interval with: a
    topologically open bound, a single bound, or two bounds which have
    space between them.
  */
  template <typename Interval>
  Direct_Product(const Box<Interval>& box);

  //! Ordinary copy-constructor.
  Direct_Product(const Direct_Product& y);

  /*! \brief
    The assignment operator.  (\p *this and \p y can be
    dimension-incompatible.)
  */
  Direct_Product& operator=(const Direct_Product& y);

  //! \name Member Functions that Do Not Modify the Direct_Product
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  /*! \brief
    Returns the minimum \ref Affine_Independence_and_Affine_Dimension
    "affine dimension"
    (see also \ref Grid_Affine_Dimension "grid affine dimension")
    of the components of \p *this.
  */
  dimension_type affine_dimension() const;

  //! Returns a constant reference to the first of the pair.
  const D1& domain1() const;

  //! Returns a constant reference to the second of the pair.
  const D2& domain2() const;

  //! Returns a system of constraints which approximates \p *this.
  Constraint_System constraints() const;

  /*! \brief
    Returns a system of constraints which approximates \p *this, in
    reduced form.
  */
  Constraint_System minimized_constraints() const;

  //! Returns a system of congruences which approximates \p *this.
  Congruence_System congruences() const;

  /*! \brief
    Returns a system of congruences which approximates \p *this, in
    reduced form.
  */
  Congruence_System minimized_congruences() const;

  //! Returns a system of generators which approximates \p *this.
  Generator_System generators() const;

  //! Returns the minimized system of generators.
  Generator_System minimized_generators() const;

  //! Returns a system of grid generators which approximates \p *this.
  Grid_Generator_System grid_generators() const;

  /*! \brief
    Returns a system of grid generators which approximates \p *this,
    in reduced form.
  */
  Grid_Generator_System minimized_grid_generators() const;

  // FIXME: Implement relation_with(*).
#if 0
  //! Returns the relations holding between \p *this and \p c.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! Returns the relations holding between \p *this and \p cg.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  Poly_Con_Relation relation_with(const Congruence& cg) const;

  //! Returns the relations holding between \p *this and \p g.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! Returns the relations holding between \p *this and \p g.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Grid_Generator& g) const;
#endif

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is an empty
    product.
  */
  bool is_empty() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a universe
    product.
  */
  bool is_universe() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a
    topologically closed subset of the vector space.
  */
  bool is_topologically_closed() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y are
    disjoint.

    \exception std::invalid_argument
    Thrown if \p x and \p y are dimension-incompatible.
  */
  bool is_disjoint_from(const Direct_Product& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this is discrete.
  /*!
    A grid is discrete if it can be defined by a generator system which
    contains only points and parameters.  This includes the empty grid
    and any grid in dimension zero.
  */
  bool is_discrete() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is bounded.
  bool is_bounded() const;

  // FIXME: Implement these.
#if 0
  //! Returns <CODE>true</CODE> if and only if \p expr is bounded in \p *this.
  /*!
    This method is the same as bounds_from_below.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.
  */
  bool bounds_from_above(const Linear_Expression& expr) const;

  //! Returns <CODE>true</CODE> if and only if \p expr is bounded in \p *this.
  /*!
    This method is the same as bounds_from_above.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.
  */
  bool bounds_from_below(const Linear_Expression& expr) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is not empty and
    \p expr is bounded from above in \p *this, in which case the
    supremum value is computed.

    \param expr
    The linear expression to be maximized subject to \p *this;

    \param sup_n
    The numerator of the supremum value;

    \param sup_d
    The denominator of the supremum value;

    \param maximum
    <CODE>true</CODE> if the supremum value can be reached in \p this.
    Always <CODE>true</CODE> when \p this bounds \p expr.  Present for
    interface compatibility with class Polyhedron, where closure
    points can result in a value of false.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded by \p *this,
    <CODE>false</CODE> is returned and \p sup_n, \p sup_d and \p
    maximum are left untouched.
  */
  bool maximize(const Linear_Expression& expr,
		Coefficient& sup_n, Coefficient& sup_d, bool& maximum) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is not empty and
    \p expr is bounded from above in \p *this, in which case the
    supremum value and a point where \p expr reaches it are computed.

    \param expr
    The linear expression to be maximized subject to \p *this;

    \param sup_n
    The numerator of the supremum value;

    \param sup_d
    The denominator of the supremum value;

    \param maximum
    <CODE>true</CODE> if the supremum value can be reached in \p this.
    Always <CODE>true</CODE> when \p this bounds \p expr.  Present for
    interface compatibility with class Polyhedron, where closure
    points can result in a value of false;

    \param point
    When maximization succeeds, will be assigned a point where \p expr
    reaches its supremum value.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded by \p *this,
    <CODE>false</CODE> is returned and \p sup_n, \p sup_d, \p maximum
    and \p point are left untouched.
  */
  bool maximize(const Linear_Expression& expr,
		Coefficient& sup_n, Coefficient& sup_d, bool& maximum,
		Grid_Generator& point) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is not empty and
    \p expr is bounded from below i \p *this, in which case the
    infimum value is computed.

    \param expr
    The linear expression to be minimized subject to \p *this;

    \param inf_n
    The numerator of the infimum value;

    \param inf_d
    The denominator of the infimum value;

    \param minimum
    <CODE>true</CODE> if the is the infimum value can be reached in \p
    this.  Always <CODE>true</CODE> when \p this bounds \p expr.
    Present for interface compatibility with class Polyhedron, where
    closure points can result in a value of false.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from below,
    <CODE>false</CODE> is returned and \p inf_n, \p inf_d
    and \p minimum are left untouched.
  */
  bool minimize(const Linear_Expression& expr,
		Coefficient& inf_n, Coefficient& inf_d, bool& minimum) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is not empty and
    \p expr is bounded from below in \p *this, in which case the
    infimum value and a point where \p expr reaches it are computed.

    \param expr
    The linear expression to be minimized subject to \p *this;

    \param inf_n
    The numerator of the infimum value;

    \param inf_d
    The denominator of the infimum value;

    \param minimum
    <CODE>true</CODE> if the is the infimum value can be reached in \p
    this.  Always <CODE>true</CODE> when \p this bounds \p expr.
    Present for interface compatibility with class Polyhedron, where
    closure points can result in a value of false;

    \param point
    When minimization succeeds, will be assigned a point where \p expr
    reaches its infimum value.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from below,
    <CODE>false</CODE> is returned and \p inf_n, \p inf_d, \p minimum
    and \p point are left untouched.
  */
  bool minimize(const Linear_Expression& expr,
		Coefficient& inf_n, Coefficient& inf_d, bool& minimum,
		Grid_Generator& point) const;
#endif

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool contains(const Direct_Product& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this strictly
    contains \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool strictly_contains(const Direct_Product& y) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //@} // Member Functions that Do Not Modify the Direct_Product

  //! \name Space Dimension Preserving Member Functions that May Modify the Direct_Product
  //@{

  //! Adds constraint \p c to \p *this.
  /*!
    The addition can only affect \p *this if \p c is an equality.

    \exception std::invalid_argument
    Thrown if \p *this and \p c are dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Adds constraint \p c to \p *this, reducing the result.
  /*!
    The addition can only affect \p *this if \p c is an equality.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p c are dimension-incompatible.
  */
  bool add_constraint_and_minimize(const Constraint& c);

  //! Adds a copy of congruence \p cg to \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are
    dimension-incompatible.
  */
  void add_congruence(const Congruence& cg);

  /*! \brief
    Adds a copy of congruence \p cg to the system of congruences of \p
    *this, reducing the result

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  bool add_congruence_and_minimize(const Congruence& c);

  /*! \brief
    Adds a copy of generator \p g to the system of generators of \p
    *this.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is empty and \p g is not a point.
  */
  void add_generator(const Generator& g);

  /*! \brief
    Adds a copy of generator \p g to the system of generators of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is empty and \p g is not a point.
  */
  bool add_generator_and_minimize(const Generator& g);

  /*! \brief
    Adds a copy of grid generator \p g to the system of grid generators of \p
    *this.

    \exception std::invalid_argument
    Thrown if \p *this and grid generator \p g are dimension-incompatible,
    or if \p *this is empty and \p g is not a grid point.
  */
  void add_grid_generator(const Grid_Generator& g);

  /*! \brief
    Adds a copy of grid generator \p g to the system of grid generators of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and grid generator \p g are dimension-incompatible,
    or if \p *this is empty and \p g is not a point.
  */
  bool add_grid_generator_and_minimize(const Grid_Generator& g);

  //! Adds a copy of each congruence in \p cgs to \p *this.
  /*!
    \param cgs
    Contains the congruences that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  void add_congruences(const Congruence_System& cgs);

  //! Adds the congruences in \p cgs to *this.
  /*!
    \param cgs
    The congruence system that may be recycled, adding its
    congruences to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning
    The only assumption that can be made about \p cgs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  void add_recycled_congruences(Congruence_System& cgs);

  /*! \brief
    Adds a copy of the congruences in \p cgs to the system of
    congruences of \p *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cgs
    Contains the congruences that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  bool add_congruences_and_minimize(const Congruence_System& cgs);

  /*! \brief
    Adds the congruences in \p cgs to the system of congruences of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cgs
    The congruence system that may be recycled, adding its
    congruences to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.

    \warning
    The only assumption that can be made about \p cgs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_congruences_and_minimize(Congruence_System& cgs);

  //! Adds copies of the equality constraints in \p cs to \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.
  */
  void add_constraints(const Constraint_System& cs);

  /*! \brief
    Adds copies of the equality constraints in \p cs to \p *this,
    reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.
  */
  bool add_constraints_and_minimize(const Constraint_System& cs);

  //! Adds the equality constraints in \p cs to \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning
    The only assumption that can be made about \p cs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  void add_recycled_constraints(Constraint_System& cs);

  /*! \brief
    Adds the constraints in \p cs to \p *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning
    The only assumption that can be made about \p cs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_constraints_and_minimize(Constraint_System& cs);

  /*! \brief
    Adds a copy of the grid generators in \p gs to the system of
    grid generators of \p *this.

    \param gs
    Contains the generators that will be added to the system of
    grid generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of grid generators \p gs is not empty,
    but has no grid points.
  */
  void add_grid_generators(const Grid_Generator_System& gs);

  /*! \brief
    Adds the grid generators in \p gs to the system of grid generators of \p
    *this.

    \param gs
    The grid generator system that may be recycled, adding its grid generators
    to the system of grid generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of grid generators \p gs is not empty,
    but has no grid points.

    \warning
    The only assumption that can be made about \p gs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  void add_recycled_grid_generators(Grid_Generator_System& gs);

  /*! \brief
    Adds a copy of the grid generators in \p gs to the system of
    grid generators of \p *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    Contains the grid generators that will be added to the system of
    grid generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if \p
    *this is empty and the system of grid generators \p gs is not empty,
    but has no  grid points.
  */
  bool add_grid_generators_and_minimize(const Grid_Generator_System& gs);

  /*! \brief
    Adds the generators in \p gs to the system of generators of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    The generator system that may be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if \p
    *this is empty and the system of grid generators \p gs is not empty,
    but has no grid points.

    \warning
    The only assumption that can be made about \p gs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_grid_generators_and_minimize(Grid_Generator_System& gs);

  /*! \brief
    Adds a copy of the generators in \p gs to the system of generators
    of \p *this.

    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of generators \p gs is not empty,
    but has no points.
  */
  void add_generators(const Generator_System& gs);

  /*! \brief
    Adds the generators in \p gs to the system of generators of \p
    *this.

    \param gs
    The generator system that may be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of generators \p gs is not empty,
    but has no points.

    \warning
    The only assumption that can be made about \p gs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  void add_recycled_generators(Generator_System& gs);

  /*! \brief
    Adds a copy of the generators in \p gs to the system of generators
    of \p *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if \p
    *this is empty and the system of generators \p gs is not empty,
    but has no points.
  */
  bool add_generators_and_minimize(const Generator_System& gs);

  /*! \brief
    Adds the generators in \p gs to the system of generators of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    The generator system that may be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if \p
    *this is empty and the system of generators \p gs is not empty,
    but has no points.

    \warning
    The only assumption that can be made about \p gs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_generators_and_minimize(Generator_System& gs);

  /*! \brief
    Assigns to \p *this the intersection of \p *this and \p y.  The
    result is not guaranteed to be reduced.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void intersection_assign(const Direct_Product& y);

  /*! \brief
    Assigns to \p *this an upper bound of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void upper_bound_assign(const Direct_Product& y);

  /*! \brief
    If the this of \p *this and \p y is exact it is assigned to \p
    *this and <CODE>true</CODE> is returned, otherwise
    <CODE>false</CODE> is returned.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool upper_bound_assign_if_exact(const Direct_Product& y);

  /*! \brief
    Assigns to \p *this an approximation of the set-theoretic difference
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void difference_assign(const Direct_Product& y);

  // FIXME: Finish these when their interface is cleaned up, taking
  //        into account the extra Grid versions.
#if 0
  /*! \brief
    Assigns to \p *this the \ref Single_Update_Affine_Functions
    "affine image" of \p
    *this under the function mapping variable \p var to the affine
    expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is assigned;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression (optional argument with
    default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of
    \p *this.

  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    Coefficient_traits::const_reference denominator
		    = Coefficient_one());

  /*! \brief
    Assigns to \p *this the \ref  Single_Update_Affine_Functions
    "affine preimage" of
    \p *this under the function mapping variable \p var to the affine
    expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is substituted;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression (optional argument with
    default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p *this.
  */
  void affine_preimage(Variable var,
		       const Linear_Expression& expr,
		       Coefficient_traits::const_reference denominator
		         = Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "generalized affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine relation;

    \param relsym
    The relation symbol;

    \param expr
    The numerator of the right hand side affine expression;

    \param denominator
    The denominator of the right hand side affine expression (optional
    argument with default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p *this
    or if \p *this is a C_Polyhedron and \p relsym is a strict
    relation symbol.
  */
  void generalized_affine_image(Variable var,
				Relation_Symbol relsym,
				const Linear_Expression& expr,
				Coefficient_traits::const_reference denominator
				  = Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to
    the \ref Grid_Generalized_Image "generalized affine relation"
    \f$\mathrm{var}' = \frac{\mathrm{expr}}{\mathrm{denominator}}
    \pmod{\mathrm{modulus}}\f$.

    \param var
    The left hand side variable of the generalized affine relation;

    \param expr
    The numerator of the right hand side affine expression;

    \param denominator
    The denominator of the right hand side affine expression.
    Optional argument with an automatic value of one;

    \param modulus
    The modulus of the congruence lhs %= rhs.  A modulus of zero
    indicates lhs == rhs.  Optional argument with an automatic value
    of one.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p
    *this.
  */
  void generalized_affine_image(Variable var,
				const Linear_Expression& expr,
				Coefficient_traits::const_reference denominator
				= Coefficient_one(),
				Coefficient_traits::const_reference modulus
				= Coefficient_one());

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Affine_Relations "generalized affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine relation;

    \param relsym
    The relation symbol;

    \param expr
    The numerator of the right hand side affine expression;

    \param denominator
    The denominator of the right hand side affine expression (optional
    argument with default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p *this
    or if \p *this is a C_Polyhedron and \p relsym is a strict
    relation symbol.
  */
  void
  generalized_affine_preimage(Variable var,
			      Relation_Symbol relsym,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator
			      = Coefficient_one());

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Grid_Generalized_Image "generalized affine relation"
    \f$\mathrm{var}' = \frac{\mathrm{expr}}{\mathrm{denominator}}
    \pmod{\mathrm{modulus}}\f$.

    \param var
    The left hand side variable of the generalized affine relation;

    \param expr
    The numerator of the right hand side affine expression;

    \param denominator
    The denominator of the right hand side affine expression.
    Optional argument with an automatic value of one;

    \param modulus
    The modulus of the congruence lhs %= rhs.  A modulus of zero
    indicates lhs == rhs.  Optional argument with an automatic value
    of one.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p
    *this.
  */
  void generalized_affine_preimage(Variable var,
				   const Linear_Expression& expr,
				   Coefficient_traits::const_reference denominator
				   = Coefficient_one(),
				   Coefficient_traits::const_reference modulus
				   = Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "generalized affine relation"
    \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
    \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

    \param lhs
    The left hand side affine expression;

    \param relsym
    The relation symbol;

    \param rhs
    The right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p rhs
    or if \p *this is a C_Polyhedron and \p relsym is a strict
    relation symbol.
  */
  void generalized_affine_image(const Linear_Expression& lhs,
				Relation_Symbol relsym,
				const Linear_Expression& rhs);

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to
    the \ref Generalized_Image "generalized affine relation"
    \f$\mathrm{lhs}' = \mathrm{rhs} \pmod{\mathrm{modulus}}\f$.

    \param lhs
    The left hand side affine expression.

    \param rhs
    The right hand side affine expression.

    \param modulus
    The modulus of the congruence lhs %= rhs.  A modulus of zero
    indicates lhs == rhs.  Optional argument with an automatic value
    of one.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p
    rhs.
  */
  void generalized_affine_image(const Linear_Expression& lhs,
				const Linear_Expression& rhs,
				Coefficient_traits::const_reference modulus
				= Coefficient_one());

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Affine_Relations "generalized affine relation"
    \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
    \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

    \param lhs
    The left hand side affine expression;

    \param relsym
    The relation symbol;

    \param rhs
    The right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p rhs
    or if \p *this is a C_Polyhedron and \p relsym is a strict
    relation symbol.
  */
  void generalized_affine_preimage(const Linear_Expression& lhs,
				   Relation_Symbol relsym,
				   const Linear_Expression& rhs);

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Image "generalized affine relation"
    \f$\mathrm{lhs}' = \mathrm{rhs} \pmod{\mathrm{modulus}}\f$.

    \param lhs
    The left hand side affine expression;

    \param rhs
    The right hand side affine expression;

    \param modulus
    The modulus of the congruence lhs %= rhs.  A modulus of zero
    indicates lhs == rhs.  Optional argument with an automatic value
    of one.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p
    rhs.
  */
  void generalized_affine_preimage(const Linear_Expression& lhs,
				   const Linear_Expression& rhs,
				   Coefficient_traits::const_reference modulus
				   = Coefficient_one());
#endif

  /*! \brief
    Assigns to \p *this the result of computing the \ref Time_Elapse_Operator
    "time-elapse" between \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void time_elapse_assign(const Direct_Product& y);

  //! Assigns to \p *this its topological closure.
  void topological_closure_assign();

  // TODO: Add a way to call other widenings.

  /*! \brief
    Assigns to \p *this the result of computing the
    "widening" between \p *this and \p y.

    This widening uses either the congruence or generator systems
    depending on which of the systems describing x and y
    are up to date and minimized.

    \param y
    A direct product that <EM>must</EM> be contained in \p *this;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void widening_assign(const Direct_Product& y, unsigned* tp = NULL);

  //@} // Space Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  /*! \brief
    Adds \p m new space dimensions and embeds the components
    of \p *this in the new vector space.

    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the vector
    space to exceed dimension <CODE>max_space_dimension()</CODE>.
 */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds \p m new space dimensions and does not embed the components
    in the new vector space.

    \param m
    The number of space dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.
  */
  void add_space_dimensions_and_project(dimension_type m);

  /*! \brief
    Assigns to the first (resp., second) component of \p *this
    the "concatenation" of the first (resp., second) components
    of \p *this and \p y, taken in this order.
    See also \ref Concatenating_Polyhedra and \ref Grid_Concatenate.

    \exception std::length_error
    Thrown if the concatenation would cause the vector space
    to exceed dimension <CODE>max_space_dimension()</CODE>.
  */
  void concatenate_assign(const Direct_Product& y);

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

  /*! \brief
    Removes the higher dimensions of the vector space so that the
    resulting space will have dimension \p new_dimension.

    \exception std::invalid_argument
    Thrown if \p new_dimensions is greater than the space dimension of
    \p *this.
  */
  void remove_higher_space_dimensions(dimension_type new_dimension);

  /*! \brief
    Remaps the dimensions of the vector space according to
    a \ref Mapping_the_Dimensions_of_the_Vector_Space "partial function".

    If \p pfunc maps only some of the dimensions of \p *this then the
    rest will be projected away.

    If the highest dimension mapped to by \p pfunc is higher than the
    highest dimension in \p *this then the number of dimensions in \p
    *this will be increased to the highest dimension mapped to by \p
    pfunc.

    \param pfunc
    The partial function specifying the destiny of each space
    dimension.

    The template class Partial_Function must provide the following
    methods.
    \code
      bool has_empty_codomain() const
    \endcode
    returns <CODE>true</CODE> if and only if the represented partial
    function has an empty codomain (i.e., it is always undefined).
    The <CODE>has_empty_codomain()</CODE> method will always be called
    before the methods below.  However, if
    <CODE>has_empty_codomain()</CODE> returns <CODE>true</CODE>, none
    of the functions below will be called.
    \code
      dimension_type max_in_codomain() const
    \endcode
    returns the maximum value that belongs to the codomain of the
    partial function.
    The <CODE>max_in_codomain()</CODE> method is called at most once.
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$k\f$ be the value
    of \p i.  If \f$f\f$ is defined in \f$k\f$, then \f$f(k)\f$ is
    assigned to \p j and <CODE>true</CODE> is returned.  If \f$f\f$ is
    undefined in \f$k\f$, then <CODE>false</CODE> is returned.
    This method is called at most \f$n\f$ times, where \f$n\f$ is the
    dimension of the vector space enclosing \p *this.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in
    \ref Mapping_the_Dimensions_of_the_Vector_Space
    "specification of the mapping operator".
  */
  template <typename Partial_Function>
  void map_space_dimensions(const Partial_Function& pfunc);

  //! Creates \p m copies of the space dimension corresponding to \p var.
  /*!
    \param var
    The variable corresponding to the space dimension to be replicated;

    \param m
    The number of replicas to be created.

    \exception std::invalid_argument
    Thrown if \p var does not correspond to a dimension of the vector
    space.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the vector
    space to exceed dimension <CODE>max_space_dimension()</CODE>.

    If \p *this has space dimension \f$n\f$, with \f$n > 0\f$,
    and <CODE>var</CODE> has space dimension \f$k \leq n\f$,
    then the \f$k\f$-th space dimension is
    \ref Expanding_One_Dimension_of_the_Vector_Space_to_Multiple_Dimensions
    "expanded" to \p m new space dimensions
    \f$n\f$, \f$n+1\f$, \f$\dots\f$, \f$n+m-1\f$.
  */
  void expand_space_dimension(Variable var, dimension_type m);

  //! Folds the space dimensions in \p to_be_folded into \p var.
  /*!
    \param to_be_folded
    The set of Variable objects corresponding to the space dimensions
    to be folded;

    \param var
    The variable corresponding to the space dimension that is the
    destination of the folding operation.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p var or with
    one of the Variable objects contained in \p to_be_folded.  Also
    thrown if \p var is contained in \p to_be_folded.

    If \p *this has space dimension \f$n\f$, with \f$n > 0\f$,
    <CODE>var</CODE> has space dimension \f$k \leq n\f$,
    \p to_be_folded is a set of variables whose maximum space dimension
    is also less than or equal to \f$n\f$, and \p var is not a member
    of \p to_be_folded, then the space dimensions corresponding to
    variables in \p to_be_folded are
    \ref Folding_Multiple_Dimensions_of_the_Vector_Space_into_One_Dimension
    "folded" into the \f$k\f$-th space dimension.
  */
  void fold_space_dimensions(const Variables_Set& to_be_folded, Variable var);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

  friend bool
  Parma_Polyhedra_Library::operator==<>(const Direct_Product<D1, D2>& x,
					const Direct_Product<D1, D2>& y);

#if 0
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::
  operator<<(std::ostream& s, const Direct_Product<D1, D2>& dp);
#endif


  //! \name Miscellaneous Member Functions
  //@{

  //! Destructor.
  ~Direct_Product();

  /*! \brief
    Swaps \p *this with Direct_Product \p y.  (\p *this and \p y can be
    dimension-incompatible.)
  */
  void swap(Direct_Product& y);

  PPL_OUTPUT_DECLARATIONS

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //@} // Miscellaneous Member Functions

protected:
  //! The type of the first component.
  typedef D1 Domain1;

  //! The type of the second component.
  typedef D2 Domain2;

  //! The first component.
  D1 d1;

  //! The second component.
  D2 d2;
};

// FIXME: Move to dedicated file when name of Open_Product is decided.

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
bool standard_reduce (D1& d1, D2& d2);

template <typename D1, typename D2>
bool empty_check_reduce();

template <typename D1, typename D2>
bool propagate_constraints_reduce();

} // namespace Parma_Polyhedra_Library;

template <typename D1, typename D2,
	  bool R(D1&, D2&) = Parma_Polyhedra_Library::standard_reduce>
class Parma_Polyhedra_Library::Open_Product
  : public Parma_Polyhedra_Library::Direct_Product<D1, D2> {
public:
  //! Builds an object having the specified properties.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the pair;

    \param kind
    Specifies whether a universe or an empty pair has to be built.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(dimension_type num_dimensions = 0,
			Degenerate_Element kind = UNIVERSE);

  //! Builds a pair, copying a system of congruences.
  /*!
    The pair inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(const Congruence_System& cgs);

  //! Builds a pair, recycling a system of congruences.
  /*!
    The pair inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences to be approximates by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(Congruence_System& cgs);

  //! Builds a pair, copying a system of constraints.
  /*!
    The pair inherits the space dimension of the constraint system.

    \param cs
    The system of constraints to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(const Constraint_System& cs);

  //! Builds a pair, recycling a system of constraints.
  /*!
    The pair inherits the space dimension of the constraint system.

    \param cs
    The system of constraints to be approximated by the pair.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(Constraint_System& cs);

  //! Builds a pair, copying a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param const_gs
    The system of generators to be approximated by the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(const Grid_Generator_System& const_gs);

  //! Builds a pair, recycling a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param gs
    The system of generators to be approximated by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space dimension.
  */
  explicit Open_Product(Grid_Generator_System& gs);

  //! Builds a pair, copying a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param const_gs
    The system of generators to be approximated by the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space
    dimension.
  */
  explicit Open_Product(const Generator_System& const_gs);

  //! Builds a pair, recycling a system of generators.
  /*!
    The pair inherits the space dimension of the generator system.

    \param gs
    The system of generators to be approximated by the pair.
    Its data-structures may be recycled to build the pair.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.

    \exception std::length_error
    Thrown if \p num_dimensions exceeds the maximum allowed space dimension.
  */
  explicit Open_Product(Generator_System& gs);

  //! Builds a pair out of a box.
  /*!
    \param box
    The bounding box representing the pair to be built.  The box can
    contain only point and universe intervals;

    \exception std::length_error
    Thrown if the space dimension of \p box exceeds the maximum
    allowed space dimension.

    \exception std::invalid_argument
    FIXME: what?!?
    Thrown if \p box contains at least one interval with: a
    topologically open bound, a single bound, or two bounds which have
    space between them.
  */
  template <typename Interval>
  Open_Product(const Box<Interval>& box);

  //! Ordinary copy-constructor.
  Open_Product(const Open_Product& y);

  //! Destructor.
  ~Open_Product();

  /*! \brief
    The assignment operator.  (\p *this and \p y can be
    dimension-incompatible.)
  */
  Open_Product& operator=(const Open_Product& y);

  //! Returns a system of constraints that \p *this satisfies.
  Constraint_System constraints() const;

  /*! \brief
    Returns a system of constraints in reduced form that \p *this
    satisfies.
  */
  Constraint_System minimized_constraints() const;

  //! Returns a system of congruences that \p *this satisfies.
  Congruence_System congruences() const;

  /*! \brief
    Returns a system of congruences in reduced form that \p *this
    satisfies.
  */
  Congruence_System minimized_congruences() const;

  //! Returns the system of generators.
  Generator_System generators() const;

  //! Returns the minimized system of generators.
  Generator_System minimized_generators() const;

  //! Returns the system of grid generators.
  Grid_Generator_System grid_generators() const;

  //! Returns the minimized system of grid generators.
  Grid_Generator_System minimized_grid_generators() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is an empty
    product.
  */
  bool is_empty() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a universe
    product.
  */
  bool is_universe() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a
    topologically closed subset of the vector space.
  */
  bool is_topologically_closed() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y are
    disjoint.

    \exception std::invalid_argument
    Thrown if \p x and \p y are dimension-incompatible.
  */
  bool is_disjoint_from(const Open_Product& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this is discrete.
  /*!
    A grid is discrete if it can be defined by a generator system which
    contains only points and parameters.  This includes the empty grid
    and any grid in dimension zero.
  */
  bool is_discrete() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is bounded.
  bool is_bounded() const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool contains(const Open_Product& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this strictly
    contains \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool strictly_contains(const Open_Product& y) const;

  //! Reduce.
  /*
    \return
    <CODE>true</CODE> if and only if either of the resulting component
    is strictly contained in the respective original.
  */
  bool reduce() const;

protected:
  //! Clears the reduced flag.
  void clear_reduced_flag() const;

  //! Sets the reduced flag.
  void set_reduced_flag() const;

  //! Return <CODE>true</CODE> if and only if the reduced flag is set.
  bool is_reduced() const;

  /*! \brief
    Flag to record whether the components are reduced with respect
    to each other.
  */
  bool reduced;
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
void swap(Parma_Polyhedra_Library::Direct_Product<D1, D2>& x,
	  Parma_Polyhedra_Library::Direct_Product<D1, D2>& y);

} // namespace std

#include "Direct_Product.inlines.hh"

#endif // !defined(PPL_Direct_Product_defs_hh)
