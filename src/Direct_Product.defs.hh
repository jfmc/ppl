/* Direct_Product class declaration.
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


//! A grid.
/*! \ingroup PPL_CXX_interface
  An object of the class Direct_Product represents a direct product.

  FIXME: continue here.
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
			  const Degenerate_Element kind = UNIVERSE);

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
  explicit Direct_Product(const Grid_Generator_System& const_gs);

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
  explicit Direct_Product(Grid_Generator_System& gs);

  //! Builds a pair out of a generic, interval-based bounding box.
  /*!
    \param box
    The bounding box representing the pair to be built.  The box can
    contain only point and universe intervals;

    \param dummy
    A dummy tag to make this constructor syntactically unique.

    \exception std::length_error
    Thrown if the space dimension of \p box exceeds the maximum
    allowed space dimension.

    \exception std::invalid_argument
    Thrown if \p box contains at least one interval with: a
    topologically open bound, a single bound, or two bounds which have
    space between them.

    The template class Box must provide the following methods.
    \code
      dimension_type space_dimension() const
    \endcode
    returns the dimension of the vector space enclosing the pair
    represented by the bounding box.
    \code
      bool is_empty() const
    \endcode
    returns <CODE>true</CODE> if and only if the bounding box
    describes the empty set.
    \code
      bool get_lower_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from below, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the lower boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.  The fraction
    \f$n/d\f$ is in canonical form if and only if \f$n\f$ and \f$d\f$
    have no common factors and \f$d\f$ is positive, \f$0/1\f$ being
    the unique representation for zero.
    \code
      bool get_upper_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from above, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the upper boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.
  */
  template <typename Box>
  Direct_Product(const Box& box, From_Bounding_Box dummy);

  // FIXME: should this be only for grid instantiations?
  //! Builds a grid out of a generic, interval-based covering box.
  /*!
    The covering box is a set of upper and lower values for each
    dimension.  When a covering box is tiled onto empty space the
    corners of the tiles form a rectilinear grid.

    A box interval with only one bound fixes the values of all grid
    points in the dimension associated with the box to the value of
    the bound.  A box interval which has upper and lower bounds of
    equal value allows all grid points with any value in the dimension
    associated with the interval.  The presence of a universe interval
    results in the empty grid.  The empty box produces the empty grid
    of the same dimension as the box.

    \param box
    The covering box representing the grid to be built;

    \param dummy
    A dummy tag to make this constructor syntactically unique.

    \exception std::length_error
    Thrown if the space dimension of \p box exceeds the maximum
    allowed space dimension.

    \exception std::invalid_argument
    Thrown if \p box contains any topologically open bounds.

    The template class Box must provide the following methods.
    \code
      dimension_type space_dimension() const
    \endcode
    returns the dimension of the vector space enclosing the grid
    represented by the covering box.
    \code
      bool is_empty() const
    \endcode
    returns <CODE>true</CODE> if and only if the covering box
    describes the empty set.
    \code
      bool get_lower_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from below, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the lower boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.  The fraction
    \f$n/d\f$ is in canonical form if and only if \f$n\f$ and \f$d\f$
    have no common factors and \f$d\f$ is positive, \f$0/1\f$ being
    the unique representation for zero.
    \code
      bool get_upper_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from above, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the upper boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.
  */
  template <typename Box>
  Direct_Product(const Box& box, From_Covering_Box dummy);

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
    Returns \f$0\f$, if \p *this is empty; otherwise, returns
    the \ref Direct_Product_Affine_Dimension "affine dimension" of \p *this.
  */
  dimension_type affine_dimension() const;

  //! Returns a constant reference to the first of the pair.
  const D1& domain1() const;

  //! Returns a constant reference to the second of the pair.
  const D2& domain2() const;


  //! FIXME maybe these should be something like
  //const pair<D1::con_type&, D2::con_type&> constraining_systems() const;
  //const pair<D1::gen_type&, D2::gen_type&> generating_systems() const;

  //! Returns the system of congruences.
  const Congruence_System& congruences() const;

  //! Returns the system of congruences in reduced form.
  const Congruence_System& minimized_congruences() const;

  //! Returns the system of constraints.
  const Constraint_System& constraints() const;

  //! Returns the system of constraints in reduced form.
  const Constraint_System& minimized_constraints() const;

#if 0
  //! Returns the system of generators.
  const Grid_Generator_System& generators() const;

  //! Returns the minimized system of generators.
  const Grid_Generator_System& minimized_generators() const;
#endif


  //! Returns the relations holding between \p *this and \p cg.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  // FIXME: Poly_Con_Relation seems to encode exactly what we want
  // here.  We must find a new name for that class.  Temporarily,
  // we keep using it without changing the name.
  Poly_Con_Relation relation_with(const Congruence& cg) const;

  //! Returns the relations holding between \p *this and \p g.
  /*
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  // FIXME: see the comment for Poly_Con_Relation above.
  Poly_Gen_Relation
  relation_with(const Grid_Generator& g) const;

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

  //! Reduce the instance of the second domain with the first.
  /*
    \return
    <CODE>true</CODE> if and only if resulting components are strictly
    contained in the originals.
  */
  bool reduce_domain1_with_domain2();

  //! Reduce the instance of the first domain with the second.
  /*
    \return
    <CODE>true</CODE> if and only if resulting components are strictly
    contained in the originals.
  */
  bool reduce_domain2_with_domain1();

  //! Reduce.
  /*
    \return
    <CODE>true</CODE> if and only if either of the resulting component
    is strictly contained in the respective original.
  */
  bool reduce();

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
    \p expr is bounded from below in \p *this, in which case the
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

  //! Uses \p *this to shrink a generic, interval-based bounding box.
  /*!
    \param box
    The bounding box to be shrunk.

    \exception std::invalid_argument
    Thrown if \p *this and \p box are dimension-incompatible, or if \p
    box contains any topologically open bounds.

    The template class Box must provide the following methods
    \code
      dimension_type space_dimension() const
    \endcode
    returns the dimension of the vector space enclosing the grid
    represented by the bounding box.
    \code
      bool get_lower_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from below, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the lower boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.  The fraction
    \f$n/d\f$ is in canonical form if and only if \f$n\f$ and \f$d\f$
    have no common factors and \f$d\f$ is positive, \f$0/1\f$ being
    the unique representation for zero.
    \code
      bool get_upper_bound(dimension_type k, bool closed,
                           Coefficient& n, Coefficient& d) const
    \endcode
    Let \f$I\f$ be the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from above, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the upper boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.
    \code
      set_empty()
    \endcode
    Causes the box to become empty, i.e., to represent the empty set.
    \code
      raise_lower_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension with \f$[n/d, +\infty)\f$.  <CODE>closed</CODE> is
    always passed as <CODE>true</CODE>.
    \code
      lower_upper_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension with \f$(-\infty, n/d]\f$.  <CODE>closed</CODE> is
    always passed as <CODE>true</CODE>.

    The function <CODE>raise_lower_bound(k, closed, n, d)</CODE>
    will be called at most once for each possible value for <CODE>k</CODE>
    and for all such calls the fraction \f$n/d\f$ will be in canonical form,
    that is, \f$n\f$ and \f$d\f$ have no common factors and \f$d\f$
    is positive, \f$0/1\f$ being the unique representation for zero.
    The same guarantee is offered for the function
    <CODE>lower_upper_bound(k, closed, n, d)</CODE>.
  */
  template <typename Box>
  void shrink_bounding_box(Box& box) const;

  //! Writes the covering box for \p *this into \p box.
  /*!
    The covering box is a set of upper and lower values for each
    dimension.  When the covering box written into \p box is tiled
    onto empty space the corners of the tiles form the sparsest
    rectilinear grid that includes \p *this.

    The value of the lower bound of each interval of the resulting \p
    box are as close as possible to the origin, with positive values
    taking preference when the lowest positive value equals the lowest
    negative value.

    If all the points have a single value in a particular dimension of
    the grid then there is only a lower bound on the interval produced
    in \p box, and the lower bound denotes the single value for the
    dimension.  If the coordinates of the points in a particular
    dimension include every value then the upper and lower bounds of
    the associated interval in \p box are set equal.  The empty grid
    produces the empty \p box.  The zero dimension universe grid
    produces the zero dimension universe box.

    \param box
    The Box into which the covering box is written.

    \exception std::invalid_argument
    Thrown if \p *this and \p box are dimension-incompatible.

    The template class Box must provide the following methods
    \code
      Box(dimension_type space_dimension)
    \endcode
    Creates a universe box of space_dimension dimensions.
    \code
      dimension_type space_dimension() const
    \endcode
    returns the dimension of the vector space enclosing the grid
    represented by the covering box.
    \code
      set_empty()
    \endcode
    Causes the box to become empty, i.e., to represent the empty set.
    \code
      raise_lower_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension with \f$[n/d, +\infty)\f$.  <CODE>closed</CODE> is
    always passed as <CODE>true</CODE>.
    \code
      lower_upper_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension with \f$(-\infty, n/d]\f$.  <CODE>closed</CODE> is
    always passed as <CODE>true</CODE>.

    The function <CODE>raise_lower_bound(k, closed, n, d)</CODE>
    will be called at most once for each possible value for <CODE>k</CODE>
    and for all such calls the fraction \f$n/d\f$ will be in canonical form,
    that is, \f$n\f$ and \f$d\f$ have no common factors and \f$d\f$
    is positive, \f$0/1\f$ being the unique representation for zero.
    The same guarantee is offered for the function
    <CODE>lower_upper_bound(k, closed, n, d)</CODE>.
  */
  template <typename Box>
  void get_covering_box(Box& box) const;

  //! Checks if all the invariants are satisfied.
  /*!
    \return
    <CODE>true</CODE> if and only if \p *this satisfies all the
    invariants and either \p check_not_empty is <CODE>false</CODE> or
    \p *this is not empty.

    \param check_not_empty
    <CODE>true</CODE> if and only if, in addition to checking the
    invariants, \p *this must be checked to be not empty.

    The check is performed so as to intrude as little as possible.  If
    the library has been compiled with run-time assertions enabled,
    error messages are written on <CODE>std::cerr</CODE> in case
    invariants are violated. This is useful for the purpose of
    debugging the library.
  */
  bool OK(/*bool check_not_empty = false*/) const;

  //@} // Member Functions that Do Not Modify the Direct_Product

  //! \name Space Dimension Preserving Member Functions that May Modify the Direct_Product
  //@{

  //! Adds a copy of congruence \p cg to \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are
    dimension-incompatible.
  */
  void add_congruence(const Congruence& cg);

  //! Adds constraint \p c to \p *this.
  /*!
    The addition can only affect \p *this if \p c is an equality.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible.
  */
  void add_congruence(const Constraint& c);

  /*! \brief
    Adds a copy of congruence \p cg to the system of congruences of \p
    *this, reducing the result

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  bool add_congruence_and_minimize(const Congruence& c);

  //! Adds a copy of constraint \p c to \p *this, reducing the result.
  /*!
    The addition can only affect \p *this if \p c is an equality.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible.
  */
  bool add_congruence_and_minimize(const Constraint& c);

  /*! \brief
    Adds a copy of generator \p g to the system of generators of \p
    *this.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is an empty grid and \p g is not a point.
  */
  void add_generator(const Grid_Generator& g);

  /*! \brief
    Adds a copy of generator \p g to the system of generators of \p
    *this, reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is an empty grid and \p g is not a point.
  */
  bool add_generator_and_minimize(const Grid_Generator& g);

  //! Adds a copy of each congruence in \p cgs to \p *this.
  /*!
    \param cgs
    Contains the congruences that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  void add_congruences(const Congruence_System& cgs);

  //! Adds a copy of each equality constraint in \p cs to \p *this.
  /*!
    \param cs
    The congruences that will be considered for addition to the system
    of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  void add_congruences(const Constraint_System& cs);

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

  //! Adds the equality constraints in \p cs to \p *this.
  /*!
    \param cs
    The constraint system from which constraints will be considered
    for addition to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning

    The only assumption that can be made about \p cs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  void add_recycled_congruences(Constraint_System& cs);

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
    Adds a copy of each equality constraint in \p cs to \p *this,
    reducing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    Contains the constraints that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.
  */
  bool add_congruences_and_minimize(const Constraint_System& cs);

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

  //! Adds the equalities in \p cs to \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    The constraint system that may be recycled, adding its
    equalities to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning
    The only assumption that can be made about \p cs upon successful
    or exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_congruences_and_minimize(Constraint_System& cs);

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
  void add_generators(const Grid_Generator_System& gs);

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
  void add_recycled_generators(Grid_Generator_System& gs);

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
  bool add_generators_and_minimize(const Grid_Generator_System& gs);

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
  bool add_recycled_generators_and_minimize(Grid_Generator_System& gs);

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

  /*! \brief
    Assigns to \p *this the \ref Direct_Product_Affine_Transformation
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

    \if Include_Implementation_Details

    When considering the generators of a grid, the
    affine transformation
    \f[
      \frac{\sum_{i=0}^{n-1} a_i x_i + b}{\mathrm{denominator}}
    \f]
    is assigned to \p var where \p expr is
    \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
    (\f$b\f$ is the inhomogeneous term).

    If congruences are up-to-date, it uses the specialized function
    affine_preimage() (for the system of congruences)
    and inverse transformation to reach the same result.
    To obtain the inverse transformation we use the following observation.

    Observation:
    -# The affine transformation is invertible if the coefficient
       of \p var in this transformation (i.e., \f$a_\mathrm{var}\f$)
       is different from zero.
    -# If the transformation is invertible, then we can write
       \f[
  	 \mathrm{denominator} * {x'}_\mathrm{var}
	   = \sum_{i = 0}^{n - 1} a_i x_i + b
	   = a_\mathrm{var} x_\mathrm{var}
	     + \sum_{i \neq var} a_i x_i + b,
       \f]
       so that the inverse transformation is
       \f[
	 a_\mathrm{var} x_\mathrm{var}
           = \mathrm{denominator} * {x'}_\mathrm{var}
             - \sum_{i \neq j} a_i x_i - b.
       \f]

    Then, if the transformation is invertible, all the entities that
    were up-to-date remain up-to-date. Otherwise only generators remain
    up-to-date.

    \endif
  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    Coefficient_traits::const_reference denominator
		    = Coefficient_one());

  /*! \brief
    Assigns to \p *this the \ref Direct_Product_Affine_Transformation
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

    \if Include_Implementation_Details

    When considering congruences of a grid, the affine transformation
    \f[
      \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator},
    \f]
    is assigned to \p var where \p expr is
    \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
    (\f$b\f$ is the inhomogeneous term).

    If generators are up-to-date, then the specialized function
    affine_image() is used (for the system of generators)
    and inverse transformation to reach the same result.
    To obtain the inverse transformation, we use the following observation.

    Observation:
    -# The affine transformation is invertible if the coefficient
       of \p var in this transformation (i.e. \f$a_\mathrm{var}\f$)
       is different from zero.
    -# If the transformation is invertible, then we can write
       \f[
  	 \mathrm{denominator} * {x'}_\mathrm{var}
	   = \sum_{i = 0}^{n - 1} a_i x_i + b
           = a_\mathrm{var} x_\mathrm{var}
               + \sum_{i \neq \mathrm{var}} a_i x_i + b,
       \f],
       the inverse transformation is
       \f[
	 a_\mathrm{var} x_\mathrm{var}
           = \mathrm{denominator} * {x'}_\mathrm{var}
               - \sum_{i \neq j} a_i x_i - b.
       \f].

    Then, if the transformation is invertible, all the entities that
    were up-to-date remain up-to-date. Otherwise only congruences remain
    up-to-date.

    \endif
  */
  void affine_preimage(Variable var,
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
    Assigns to \p *this the image of \p *this with respect to
    the \ref Grid_Generalized_Image "generalized affine relation"
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
    \ref Grid_Generalized_Image "generalized affine relation"
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

  /*! \brief
    Assigns to \p *this the result of computing the \ref Direct_Product_Time_Elapse
    "time-elapse" between \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void time_elapse_assign(const Direct_Product& y);

  //! Assigns to \p *this its topological closure.
  void topological_closure_assign();

  /*! \brief
    Assigns to \p *this the result of computing the \ref Direct_Product_Widening
    "Direct_Product widening" between \p *this and \p y using generator systems.

    \param y
    A grid that <EM>must</EM> be contained in \p *this;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Direct_Product_Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void generator_widening_assign(const Direct_Product& y, unsigned* tp = NULL);

  /*! \brief
    Assigns to \p *this the result of computing the \ref Direct_Product_Widening
    "Direct_Product widening" between \p *this and \p y.

    This widening uses either the congruence or generator systems
    depending on which of the systems describing x and y
    are up to date and minimized.

    \param y
    A grid that <EM>must</EM> be contained in \p *this;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Direct_Product_Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void widening_assign(const Direct_Product& y, unsigned* tp = NULL);

  /*! \brief
    Improves the result of the congruence variant of
    \ref Direct_Product_Widening "Direct_Product widening" computation by also enforcing
    those congruences in \p cgs that are satisfied by all the points
    of \p *this.

    \param y
    A grid that <EM>must</EM> be contained in \p *this;

    \param cgs
    The system of congruences used to improve the widened grid;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Direct_Product_Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible.
  */
  void limited_congruence_extrapolation_assign(const Direct_Product& y,
				    const Congruence_System& cgs,
				    unsigned* tp = NULL);

  /*! \brief
    Improves the result of the generator variant of the
    \ref Direct_Product_Widening "Direct_Product widening"
    computation by also enforcing those congruences in \p cgs that are
    satisfied by all the points of \p *this.

    \param y
    A grid that <EM>must</EM> be contained in \p *this;

    \param cgs
    The system of congruences used to improve the widened grid;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Direct_Product_Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible.
  */
  void limited_generator_extrapolation_assign(const Direct_Product& y,
				    const Congruence_System& cgs,
				    unsigned* tp = NULL);

  /*! \brief
    Improves the result of the \ref Direct_Product_Widening "Direct_Product widening"
    computation by also enforcing those congruences in \p cgs that are
    satisfied by all the points of \p *this.

    \param y
    A grid that <EM>must</EM> be contained in \p *this;

    \param cgs
    The system of congruences used to improve the widened grid;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Direct_Product_Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible.
  */
  void limited_extrapolation_assign(const Direct_Product& y,
				    const Congruence_System& cgs,
				    unsigned* tp = NULL);

  //@} // Space Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  /*! \brief
    Adds \p m new space dimensions and embeds the old grid in the new
    vector space.

    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the vector
    space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new grid, which is characterized by a system of congruences
    in which the variables which are the new dimensions can have any
    value.  For instance, when starting from the grid \f$\cL \sseq
    \Rset^2\f$ and adding a third space dimension, the result will be
    the grid
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cL
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds \p m new space dimensions to the grid and does not embed it
    in the new vector space.

    \param m
    The number of space dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new grid, which is characterized by a system of congruences
    in which the variables running through the new dimensions are all
    constrained to be equal to 0.  For instance, when starting from
    the grid \f$\cL \sseq \Rset^2\f$ and adding a third space
    dimension, the result will be the grid
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cL
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_project(dimension_type m);

  /*! \brief
    Assigns to \p *this the \ref Direct_Product_Concatenate "concatenation" of
    \p *this and \p y, taken in this order.

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
    a \ref Direct_Product_Map_Space_Dimensions "partial function".

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
    dimension of the vector space enclosing the grid.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in the
    \ref Direct_Product_Map_Space_Dimensions "specification of the mapping operator".
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
    \ref Direct_Product_Expand_Space_Dimension "expanded" to \p m new space dimensions
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
    variables in \p to_be_folded are \ref Direct_Product_Fold_Space_Dimensions "folded"
    into the \f$k\f$-th space dimension.
  */
  void fold_space_dimensions(const Variables_Set& to_be_folded, Variable var);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

  friend bool
  Parma_Polyhedra_Library::operator==<>(const Direct_Product<D1, D2>& x,
					const Direct_Product<D1, D2>& y);

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::
  operator<<<>(std::ostream& s, const Direct_Product<D1, D2>& dp);


  //! \name Miscellaneous Member Functions
  //@{

  //! Destructor.
  ~Direct_Product();

  /*! \brief
    Swaps \p *this with grid \p y.  (\p *this and \p y can be
    dimension-incompatible.)
  */
  void swap(Direct_Product& y);

  PPL_OUTPUT_DECLARATIONS

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    \ref ascii_dump) and sets \p *this accordingly.

     \return
     <CODE>true</CODE> if successful, else <CODE>false</CODE>.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //@} // Miscellaneous Member Functions

private:
  /*! \brief
    Reduces first component with first, by checking if second is
    empty.

    \return
    <CODE>true</CODE> if and only if resulting components are strictly
    contained in the originals.
  */
  bool empty_reduce_d1_with_d2();

  /*! \brief
    Reduces second component with first, by checking if first is
    empty.

    \return
    <CODE>true</CODE> if and only if resulting components are strictly
    contained in the originals.
  */
  bool empty_reduce_d2_with_d1();

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

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
void swap(Parma_Polyhedra_Library::Direct_Product<D1, D2>& x,
	  Parma_Polyhedra_Library::Direct_Product<D1, D2>& y);

} // namespace std

#include "Direct_Product.inlines.hh"

#endif // !defined(PPL_Direct_Product_defs_hh)
