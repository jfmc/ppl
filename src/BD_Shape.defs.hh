/* BD_Shape class declaration.
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

#ifndef PPL_BD_Shape_defs_hh
#define PPL_BD_Shape_defs_hh 1

#include "BD_Shape.types.hh"
#include "globals.defs.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Linear_Expression.types.hh"
#include "Constraint_System.types.hh"
#include "Generator_System.types.hh"
#include "Poly_Con_Relation.types.hh"
#include "Poly_Gen_Relation.types.hh"
#include "Polyhedron.types.hh"
#include "Variable.defs.hh"
#include "Variables_Set.types.hh"
#include "DB_Matrix.defs.hh"
#include "DB_Row.defs.hh"
#include "Checked_Number.defs.hh"
#include "Bit_Matrix.defs.hh"
#include <cstddef>
#include <iosfwd>
#include <vector>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::BD_Shape
  Writes a textual representation of \p bds on \p s:
  <CODE>false</CODE> is written if \p bds is an empty polyhedron;
  <CODE>true</CODE> is written if \p bds is the universe polyhedron;
  a system of constraints defining \p bds is written otherwise,
  all constraints separated by ", ".
*/
template <typename T>
std::ostream&
operator<<(std::ostream& s, const BD_Shape<T>& bds);

} // namespace IO_Operators

//! Returns <CODE>true</CODE> if and only if \p x and \p y are the same BDS.
/*! \relates BD_Shape
  Note that \p x and \p y may be dimension-incompatible shapes:
  in this case, the value <CODE>false</CODE> is returned.
*/
template <typename T>
bool operator==(const BD_Shape<T>& x, const BD_Shape<T>& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y aren't the same BDS.
/*! \relates BD_Shape
  Note that \p x and \p y may be dimension-incompatible shapes:
  in this case, the value <CODE>true</CODE> is returned.
*/
template <typename T>
bool operator!=(const BD_Shape<T>& x, const BD_Shape<T>& y);

//! Computes the rectilinear (or Manhattan) distance between \p x and \p y.
/*! \relates BD_Shape
  If the rectilinear distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<To, Extended_Number_Policy>.
*/
template <typename To, typename T>
bool rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				 const BD_Shape<T>& x,
				 const BD_Shape<T>& y,
				 const Rounding_Dir dir);

//! Computes the rectilinear (or Manhattan) distance between \p x and \p y.
/*! \relates BD_Shape
  If the rectilinear distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<Temp, Extended_Number_Policy>.
*/
template <typename Temp, typename To, typename T>
bool rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				 const BD_Shape<T>& x,
				 const BD_Shape<T>& y,
				 const Rounding_Dir dir);

//! Computes the rectilinear (or Manhattan) distance between \p x and \p y.
/*! \relates BD_Shape
  If the rectilinear distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using the temporary variables
  \p tmp0, \p tmp1 and \p tmp2.
*/
template <typename Temp, typename To, typename T>
bool rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				 const BD_Shape<T>& x,
				 const BD_Shape<T>& y,
				 const Rounding_Dir dir,
				 Temp& tmp0,
				 Temp& tmp1,
				 Temp& tmp2);

//! Computes the euclidean distance between \p x and \p y.
/*! \relates BD_Shape
  If the euclidean distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<To, Extended_Number_Policy>.
*/
template <typename To, typename T>
bool euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			       const BD_Shape<T>& x,
			       const BD_Shape<T>& y,
			       const Rounding_Dir dir);

//! Computes the euclidean distance between \p x and \p y.
/*! \relates BD_Shape
  If the euclidean distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<Temp, Extended_Number_Policy>.
*/
template <typename Temp, typename To, typename T>
bool euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			       const BD_Shape<T>& x,
			       const BD_Shape<T>& y,
			       const Rounding_Dir dir);

//! Computes the euclidean distance between \p x and \p y.
/*! \relates BD_Shape
  If the euclidean distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using the temporary variables
  \p tmp0, \p tmp1 and \p tmp2.
*/
template <typename Temp, typename To, typename T>
bool euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			       const BD_Shape<T>& x,
			       const BD_Shape<T>& y,
			       const Rounding_Dir dir,
			       Temp& tmp0,
			       Temp& tmp1,
			       Temp& tmp2);

//! Computes the \f$L_\infty\f$ distance between \p x and \p y.
/*! \relates BD_Shape
  If the \f$L_\infty\f$ distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<To, Extended_Number_Policy>.
*/
template <typename To, typename T>
bool l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				const BD_Shape<T>& x,
				const BD_Shape<T>& y,
				const Rounding_Dir dir);

//! Computes the \f$L_\infty\f$ distance between \p x and \p y.
/*! \relates BD_Shape
  If the \f$L_\infty\f$ distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using variables of type
  Checked_Number<Temp, Extended_Number_Policy>.
*/
template <typename Temp, typename To, typename T>
bool l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				const BD_Shape<T>& x,
				const BD_Shape<T>& y,
				const Rounding_Dir dir);

//! Computes the \f$L_\infty\f$ distance between \p x and \p y.
/*! \relates BD_Shape
  If the \f$L_\infty\f$ distance between \p x and \p y is defined,
  stores an approximation of it into \p r and returns <CODE>true</CODE>;
  returns <CODE>false</CODE> otherwise.

  The direction of the approximation is specified by \p dir.

  All computations are performed using the temporary variables
  \p tmp0, \p tmp1 and \p tmp2.
*/
template <typename Temp, typename To, typename T>
bool l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
				const BD_Shape<T>& x,
				const BD_Shape<T>& y,
				const Rounding_Dir dir,
				Temp& tmp0,
				Temp& tmp1,
				Temp& tmp2);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Decodes the constraint \p c as a bounded difference.
/*! \relates BD_Shape
  \return
  <CODE>true</CODE> if the constraint \p c is a
  \ref Bounded_Difference_Shapes "bounded difference";
  <CODE>false</CODE> otherwise.

  \param c
  The constraint to be decoded.

  \param c_space_dim
  The space dimension of the constraint \p c (it is <EM>assumed</EM>
  to match the actual space dimension of \p c).

  \param c_num_vars
  If <CODE>true</CODE> is returned, then it will be set to the number
  of variables having a non-zero coefficient. The only legal values
  will therefore be 0, 1 and 2.

  \param c_first_var
  If <CODE>true</CODE> is returned and if \p c_num_vars is not set to 0,
  then it will be set to the index of the first variable having
  a non-zero coefficient in \p c.

  \param c_second_var
  If <CODE>true</CODE> is returned and if \p c_num_vars is set to 2,
  then it will be set to the index of the second variable having
  a non-zero coefficient in \p c.

  \param c_coeff
  If <CODE>true</CODE> is returned and if \p c_num_vars is not set to 0,
  then it will be set to the value of the first non-zero coefficient
  in \p c.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
bool extract_bounded_difference(const Constraint& c,
				const dimension_type c_space_dim,
				dimension_type& c_num_vars,
				dimension_type& c_first_var,
				dimension_type& c_second_var,
				Coefficient& c_coeff);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Extracts leader indices from the predecessor relation.
/*! \relates BD_Shape */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void compute_leader_indices(const std::vector<dimension_type>& predecessor,
			    std::vector<dimension_type>& indices);

} // namespace Parma_Polyhedra_Library

//! A bounded difference shape.
/*! \ingroup PPL_CXX_interface
  The class template BD_Shape<T> allows for the efficient representation
  of a restricted kind of <EM>topologically closed</EM> convex polyhedra
  called <EM>bounded difference shapes</EM> (BDSs, for short).
  The name comes from the fact that the closed affine half-spaces that
  characterize the polyhedron can be expressed by constraints of the form
  \f$\pm x_i \leq k\f$ or \f$x_i - x_j \leq k\f$, where the inhomogeneous
  term \f$k\f$ is a rational number.

  Based on the class template type parameter \p T, a family of extended
  numbers is built and used to approximate the inhomogeneous term of
  bounded differences. These extended numbers provide a representation
  for the value \f$+\infty\f$, as well as <EM>rounding-aware</EM>
  implementations for several arithmetic functions.
  The value of the type parameter \p T may be one of the following:
    - a bounded precision integer type (e.g., \c int32_t or \c int64_t);
    - a bounded precision floating point type (e.g., \c float or \c double);
    - an unbounded integer or rational type, as provided by GMP
      (i.e., \c mpz_class or \c mpq_class).

  The user interface for BDSs is meant to be as similar as possible to
  the one developed for the polyhedron class C_Polyhedron.  At the
  interface level, bounded differences are specified using objects of
  type Constraint: such a constraint is a bounded difference if it is
  of the form
    \f[
      a_i x_i - a_j x_j \relsym b
    \f]
  where \f$\mathord{\relsym} \in \{ \leq, =, \geq \}\f$ and
  \f$a_i\f$, \f$a_j\f$, \f$b\f$ are integer coefficients such that
  \f$a_i = 0\f$, or \f$a_j = 0\f$, or \f$a_i = a_j\f$.
  The user is warned that the above Constraint object will be mapped
  into a <EM>correct</EM> approximation that, depending on the expressive
  power of the chosen template argument \p T, may loose some precision.
  In particular, constraint objects that do not encode a bounded difference
  will be simply (and safely) ignored.

  For instance, a Constraint object encoding \f$3x - 3y \leq 1\f$ will be
  approximated by:
    - \f$x - y \leq 1\f$,
      if \p T is a (bounded or unbounded) integer type;
    - \f$x - y \leq \frac{1}{3}\f$,
      if \p T is the unbounded rational type \c mpq_class;
    - \f$x - y \leq k\f$, where \f$k > \frac{1}{3}\f$,
      if \p T is a floating point type (having no exact representation
      for \f$\frac{1}{3}\f$).

  On the other hand, a Constraint object encoding \f$3x - y \leq 1\f$
  will be safely ignored in all of the above cases.

  In the following examples it is assumed that the type argument \p T
  is one of the possible instances listed above and that variables
  <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE> are defined
  (where they are used) as follows:
  \code
    Variable x(0);
    Variable y(1);
    Variable z(2);
  \endcode

  \par Example 1
  The following code builds a BDS corresponding to a cube in \f$\Rset^3\f$,
  given as a system of constraints:
  \code
    Constraint_System cs;
    cs.insert(x >= 0);
    cs.insert(x <= 1);
    cs.insert(y >= 0);
    cs.insert(y <= 1);
    cs.insert(z >= 0);
    cs.insert(z <= 1);
    BD_Shape<T> bd(cs);
  \endcode
  Since only those constraints having the syntactic form of a
  <EM>bounded difference</EM> are considered, the following code
  will build the same BDS as above (i.e., the constraints 7, 8, and 9
  are ignored):
  \code
    Constraint_System cs;
    cs.insert(x >= 0);
    cs.insert(x <= 1);
    cs.insert(y >= 0);
    cs.insert(y <= 1);
    cs.insert(z >= 0);
    cs.insert(z <= 1);
    cs.insert(x + y <= 0);      // 7
    cs.insert(x - z + x >= 0);  // 8
    cs.insert(3*z - y <= 1);    // 9
    BD_Shape<T> bd(cs);
  \endcode
*/
template <typename T>
class Parma_Polyhedra_Library::BD_Shape {
private:
  /*! \brief
    The (extended) numeric type of the inhomogeneous term of
    the inequalities defining a BDS.
  */
  typedef Checked_Number<T, Extended_Number_Policy> N;

public:
  //! The numeric base type upon which bounded differences are built.
  typedef T base_type;

  /*! \brief
    The (extended) numeric type of the inhomogeneous term of the
    inequalities defining a BDS.
  */
  typedef N coefficient_type;

  //! Returns the maximum space dimension that a BDS can handle.
  static dimension_type max_space_dimension();

  //! \name Constructors, Assignment, Swap and Destructor
  //@{

  //! Builds a universe or empty BDS of the specified space dimension.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the BDS;

    \param kind
    Specifies whether the universe or the empty BDS has to be built.
  */
  explicit BD_Shape(dimension_type num_dimensions = 0,
		    Degenerate_Element kind = UNIVERSE);

  //! Ordinary copy-constructor.
  BD_Shape(const BD_Shape& y);

  //! Builds a conservative, upward approximation of \p y.
  template <typename U>
  explicit BD_Shape(const BD_Shape<U>& y);

  //! Builds a BDS from the system of constraints \p cs.
  /*!
    The BDS inherits the space dimension of \p cs.

    \param cs
    A system of constraints: constraints that are not
    \ref Bounded_Difference_Shapes "bounded differences"
    are ignored (even though they may have contributed
    to the space dimension).

    \exception std::invalid_argument
    Thrown if the system of constraints \p cs contains strict inequalities.
  */
  explicit BD_Shape(const Constraint_System& cs);

  //! Builds a BDS from the system of generators \p gs.
  /*!
    Builds the smallest BDS containing the polyhedron defined by \p gs.
    The BDS inherits the space dimension of \p gs.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.
  */
  explicit BD_Shape(const Generator_System& gs);

  //! Builds a BDS from the polyhedron \p ph.
  /*!
    Builds a BDS containing \p ph using algorithms whose complexity
    does not exceed the one specified by \p complexity.  If
    \p complexity is \p ANY_COMPLEXITY, then the BDS built is the
    smallest one containing \p ph.
  */
  explicit BD_Shape(const Polyhedron& ph,
		    Complexity_Class complexity = ANY_COMPLEXITY);

  /*! \brief
    The assignment operator
    (\p *this and \p y can be dimension-incompatible).
  */
  BD_Shape& operator=(const BD_Shape& y);

  /*! \brief
    Swaps \p *this with \p y
    (\p *this and \p y can be dimension-incompatible).
  */
  void swap(BD_Shape& y);

  //! Destructor.
  ~BD_Shape();

  //@} Constructors, Assignment, Swap and Destructor

  //! \name Member Functions that Do Not Modify the BD_Shape
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  /*! \brief
    Returns \f$0\f$, if \p *this is empty; otherwise, returns the
    \ref Affine_Independence_and_Affine_Dimension "affine dimension"
    of \p *this.
  */
  dimension_type affine_dimension() const;

  //! Returns a system of constraints defining \p *this.
  Constraint_System constraints() const;

  //! Returns a minimized system of constraints defining \p *this.
  Constraint_System minimized_constraints() const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool contains(const BD_Shape& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this strictly contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool strictly_contains(const BD_Shape& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this and \p y are disjoint.
  /*!
    \exception std::invalid_argument
    Thrown if \p x and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool is_disjoint_from(const BD_Shape& y) const;

  //! Returns the relations holding between \p *this and the constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible
    or if \p c is a strict inequality or if \p c is not a bounded
    difference constraint.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! Returns the relations holding between \p *this and the generator \p g.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! Returns <CODE>true</CODE> if and only if \p *this is an empty BDS.
  bool is_empty() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a universe BDS.
  bool is_universe() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is discrete.
  bool is_discrete() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    is a topologically closed subset of the vector space.
  */
  bool is_topologically_closed() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a bounded BDS.
  bool is_bounded() const;

  /*! \brief
    Uses \p *this to shrink a generic, interval-based bounding box.
    Assigns to \p box the intersection of \p box with the smallest
    bounding box containing \p *this.

    \param box
    The bounding box to be shrunk;

    \param complexity
    The complexity class of the algorithm to be used.

    If the BDS \p *this or \p box is empty, then the empty box
    is returned.

    If \p *this and \p box are non-empty, then, for
    each space dimension \f$k\f$ with variable \f$\mathrm{var}\f$, let
    \f$u\f$ be the upper and \f$l\f$ the lower bound of the smallest
    interval containing \p *this.

    If \f$l\f$ is infinite, then \p box is unaltered; if \f$l\f$ is
    finite, then the \p box interval for space dimension \f$k\f$ is
    (destructively) intersected with \f$[l, +\mathrm{infty})\f$ if a
    point of \p *this satisfies \f$\mathrm{var} == l\f$ and with
    \f$(l, +\mathrm{infty})\f$ otherwise.

    Similarly, if \f$u\f$ is infinite, then \p box is unaltered; if
    \f$u\f$ is finite, then the \p box interval for space dimension
    \f$k\f$ is (destructively) intersected with \f$(-\mathrm{infty},
    u]\f$ if a point of \p *this satisfies \f$\mathrm{var} == u\f$ and
    with \f$(-\mathrm{infty}, u)\f$ otherwise.

    The template class Box must provide the following methods, whose
    return values, if any, are simply ignored.
    \code
      set_empty()
    \endcode
    causes the box to become empty, i.e., to represent the empty set.
    \code
      raise_lower_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension
    with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>false</CODE>.
    \code
      lower_upper_bound(dimension_type k, bool closed,
                        Coefficient_traits::const_reference n,
                        Coefficient_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension
    with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE>
    is <CODE>false</CODE>.

    The function <CODE>raise_lower_bound(k, closed, n, d)</CODE>
    will be called at most once for each possible value for <CODE>k</CODE>
    and for all such calls the fraction \f$n/d\f$ will be in canonical form,
    that is, \f$n\f$ and \f$d\f$ have no common factors and \f$d\f$
    is positive, \f$0/1\f$ being the unique representation for zero.
    The same guarantee is offered for the function
    <CODE>lower_upper_bound(k, closed, n, d)</CODE>.
  */
  template <typename Box>
  void shrink_bounding_box(Box& box,
			   Complexity_Class complexity = ANY_COMPLEXITY) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    contains at least one integer point.
  */
  bool contains_integer_point() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this satisfies
    all its invariants.
  */
  bool OK() const;

  //@} Member Functions that Do Not Modify the BD_Shape

  //! \name Space-Dimension Preserving Member Functions that May Modify the BD_Shape
  //@{

  /*! \brief
    Adds a copy of constraint \p c to the system of bounded differences
    defining \p *this.

    \param c
    The constraint to be added. If it is not a bounded difference, it
    will be simply ignored.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible,
    or if \p c is a strict inequality.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of constraint \p c to the system of bounded differences
    defining \p *this.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param c
    The constraint to be added. If it is not a bounded difference, it
    will be simply ignored.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible,
    or if \p c is a strict inequality.
  */
  bool add_constraint_and_minimize(const Constraint& c);

  /*! \brief
    Adds the constraints in \p cs to the system of bounded differences
    defining \p *this.

    \param  cs
    The constraints that will be added. Constraints that are not bounded
    differences will be simply ignored.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible,
    or if \p cs contains a strict inequality.
  */
  void add_constraints(const Constraint_System& cs);

  /*! \brief
    Adds the constraints in \p cs to the system of constraints
    of \p *this (without minimizing the result).

    \param cs
    The constraint system to be added to \p *this.  The constraints in
    \p cs may be recycled.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.

    \warning
    The only assumption that can be made on \p cs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  void add_recycled_constraints(Constraint_System& cs);

  /*! \brief
    Adds the constraints in \p cs to the system of bounded differences
    defining \p *this.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param  cs
    The constraints that will be added. Constraints that are not bounded
    differences will be simply ignored.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible,
    or if \p cs contains a strict inequality.
  */
  bool add_constraints_and_minimize(const Constraint_System& cs);

  /*! \brief
    Adds the constraints in \p cs to the system of constraints
    of \p *this, minimizing the result.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    The constraint system to be added to \p *this.  The constraints in
    \p cs may be recycled.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.

    \warning
    The only assumption that can be made on \p cs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_constraints_and_minimize(Constraint_System& cs);

  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void intersection_assign(const BD_Shape& y);

  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const BD_Shape& y);

  /*! \brief
    Assigns to \p *this the smallest BDS containing the convex union
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void bds_hull_assign(const BD_Shape& y);

  /*! \brief
    Assigns to \p *this the smallest BDS containing the convex union
    of \p *this and \p y.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool bds_hull_assign_and_minimize(const BD_Shape& y);

  //! Same as bds_hull_assign.
  void upper_bound_assign(const BD_Shape& y);

  /*! \brief
    If the bds-hull of \p *this and \p y is exact, it is assigned
    to \p *this and <CODE>true</CODE> is returned,
    otherwise <CODE>false</CODE> is returned.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool bds_hull_assign_if_exact(const BD_Shape& y);

  //! Same as bds_hull_assign_if_exact.
  bool upper_bound_assign_if_exact(const BD_Shape& y);

  /*! \brief
    Assigns to \p *this
    the \ref Convex_Polyhedral_Difference "poly-difference"
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void bds_difference_assign(const BD_Shape& y);

  //! Same as bds_difference_assign.
  void difference_assign(const BD_Shape& y);

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Affine_Functions "affine image"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is assigned.

    \param expr
    The numerator of the affine expression.

    \param denominator
    The denominator of the affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    Coefficient_traits::const_reference denominator
		    = Coefficient_one());

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Affine_Functions "affine preimage"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is substituted.

    \param expr
    The numerator of the affine expression.

    \param denominator
    The denominator of the affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
  void affine_preimage(Variable var,
		       const Linear_Expression& expr,
		       Coefficient_traits::const_reference denominator
		       = Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine transfer function.

    \param relsym
    The relation symbol.

    \param expr
    The numerator of the right hand side affine expression.

    \param denominator
    The denominator of the right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension
    of \p *this or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_image(Variable var,
				Relation_Symbol relsym,
				const Linear_Expression& expr,
				Coefficient_traits::const_reference denominator
				= Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
    \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

    \param lhs
    The left hand side affine expression.

    \param relsym
    The relation symbol.

    \param rhs
    The right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p rhs
    or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_image(const Linear_Expression& lhs,
				Relation_Symbol relsym,
				const Linear_Expression& rhs);

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine transfer function.

    \param relsym
    The relation symbol.

    \param expr
    The numerator of the right hand side affine expression.

    \param denominator
    The denominator of the right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension
    of \p *this or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_preimage(Variable var,
				   Relation_Symbol relsym,
				   const Linear_Expression& expr,
				   Coefficient_traits::const_reference
				   denominator = Coefficient_one());

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
    \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

    \param lhs
    The left hand side affine expression.

    \param relsym
    The relation symbol.

    \param rhs
    The right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p rhs
    or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_preimage(const Linear_Expression& lhs,
				   Relation_Symbol relsym,
				   const Linear_Expression& rhs);

  /*!
    \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Single_Update_Bounded_Affine_Relations "bounded affine relation"
    \f$\frac{\mathrm{lb\_expr}}{\mathrm{denominator}}
         \leq \mathrm{var}'
           \leq \frac{\mathrm{ub\_expr}}{\mathrm{denominator}}\f$.

    \param var
    The variable updated by the affine relation;

    \param lb_expr
    The numerator of the lower bounding affine expression;

    \param ub_expr
    The numerator of the upper bounding affine expression;

    \param denominator
    The (common) denominator for the lower and upper bounding
    affine expressions (optional argument with default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p lb_expr (resp., \p ub_expr)
    and \p *this are dimension-incompatible or if \p var is not a space
    dimension of \p *this.
  */
  void bounded_affine_image(Variable var,
			    const Linear_Expression& lb_expr,
			    const Linear_Expression& ub_expr,
			    Coefficient_traits::const_reference denominator
			    = Coefficient_one());

  /*!
    \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Single_Update_Bounded_Affine_Relations "bounded affine relation"
    \f$\frac{\mathrm{lb\_expr}}{\mathrm{denominator}}
         \leq \mathrm{var}'
           \leq \frac{\mathrm{ub\_expr}}{\mathrm{denominator}}\f$.

    \param var
    The variable updated by the affine relation;

    \param lb_expr
    The numerator of the lower bounding affine expression;

    \param ub_expr
    The numerator of the upper bounding affine expression;

    \param denominator
    The (common) denominator for the lower and upper bounding
    affine expressions (optional argument with default value 1).

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p lb_expr (resp., \p ub_expr)
    and \p *this are dimension-incompatible or if \p var is not a space
    dimension of \p *this.
  */
  void bounded_affine_preimage(Variable var,
			       const Linear_Expression& lb_expr,
			       const Linear_Expression& ub_expr,
			       Coefficient_traits::const_reference denominator
			       = Coefficient_one());
  /*! \brief
    Assigns to \p *this the result of computing the
    \ref Time_Elapse_Operator "time-elapse" between \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void time_elapse_assign(const BD_Shape& y);

  //! Assigns to \p *this its topological closure.
  void topological_closure_assign();

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref CC76_extrapolation "CC76-extrapolation" between \p *this and \p y.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void CC76_extrapolation_assign(const BD_Shape& y, unsigned* tp = 0);

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref CC76_extrapolation "CC76-extrapolation" between \p *this and \p y.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param first
    An iterator referencing the first stop-point.

    \param last
    An iterator referencing one past the last stop-point.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  template <typename Iterator>
  void CC76_extrapolation_assign(const BD_Shape& y,
				 Iterator first, Iterator last,
				 unsigned* tp = 0);

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref BHMZ05_widening "BHMZ05-widening" of \p *this and \p y.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void BHMZ05_widening_assign(const BD_Shape& y, unsigned* tp = 0);

  /*! \brief
    Improves the result of the \ref BHMZ05_widening "BHMZ05-widening"
    computation by also enforcing those constraints in \p cs that are
    satisfied by all the points of \p *this.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param cs
    The system of constraints used to improve the widened BDS.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible or
    if \p cs contains a strict inequality.
  */
  void limited_BHMZ05_extrapolation_assign(const BD_Shape& y,
					   const Constraint_System& cs,
					   unsigned* tp = 0);

  /*! \brief
    Assigns to \p *this the result of restoring in \p y the constraints
    of \p *this that were lost by
    \ref CC76_extrapolation "CC76-extrapolation" applications.

    \param y
    A BDS that <EM>must</EM> contain \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.

    \note
    As was the case for widening operators, the argument \p y is meant to
    denote the value computed in the previous iteration step, whereas
    \p *this denotes the value computed in the current iteration step
    (in the <EM>decreasing</EM> iteration sequence). Hence, the call
    <CODE>x.CC76_narrowing_assign(y)</CODE> will assign to \p x
    the result of the computation \f$\mathtt{y} \Delta \mathtt{x}\f$.
  */
  void CC76_narrowing_assign(const BD_Shape& y);

  /*! \brief
    Improves the result of the \ref CC76_extrapolation "CC76-extrapolation"
    computation by also enforcing those constraints in \p cs that are
    satisfied by all the points of \p *this.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param cs
    The system of constraints used to improve the widened BDS.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible or
    if \p cs contains a strict inequality.
  */
  void limited_CC76_extrapolation_assign(const BD_Shape& y,
					 const Constraint_System& cs,
					 unsigned* tp = 0);

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref H79_widening "H79-widening" between \p *this and \p y.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void H79_widening_assign(const BD_Shape& y, unsigned* tp = 0);

  /*! \brief
    Improves the result of the \ref H79_widening "H79-widening"
    computation by also enforcing those constraints in \p cs that are
    satisfied by all the points of \p *this.

    \param y
    A BDS that <EM>must</EM> be contained in \p *this.

    \param cs
    The system of constraints used to improve the widened BDS.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible.
  */
  void limited_H79_extrapolation_assign(const BD_Shape& y,
					const Constraint_System& cs,
					unsigned* tp = 0);

  //@} Space-Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! Adds \p m new dimensions and embeds the old BDS into the new space.
  /*!
    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the new
    BDS, which is defined by a system of bounded differences in which the
    variables running through the new dimensions are unconstrained.
    For instance, when starting from the BDS \f$\cB \sseq \Rset^2\f$
    and adding a third dimension, the result will be the BDS
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds \p m new dimensions to the BDS and does not embed it in
    the new vector space.

    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the
    new BDS, which is defined by a system of bounded differences in
    which the variables running through the new dimensions are all
    constrained to be equal to 0.
    For instance, when starting from the BDS \f$\cB \sseq \Rset^2\f$
    and adding a third dimension, the result will be the BDS
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_project(dimension_type m);

  /*! \brief
    Seeing a BDS as a set of tuples (its points),
    assigns to \p *this all the tuples that can be obtained by concatenating,
    in the order given, a tuple of \p *this with a tuple of \p y.

    Let \f$B \sseq \Rset^n\f$ and \f$D \sseq \Rset^m\f$ be the BDSs
    corresponding, on entry, to \p *this and \p y, respectively.
    Upon successful completion, \p *this will represent the BDS
    \f$R \sseq \Rset^{n+m}\f$ such that
    \f[
      R \defeq
          \Bigl\{\,
            (x_1, \ldots, x_n, y_1, \ldots, y_m)^\transpose
          \Bigm|
            (x_1, \ldots, x_n)^\transpose \in B,
            (y_1, \ldots, y_m)^\transpose \in D
          \,\Bigl\}.
    \f]
    Another way of seeing it is as follows: first increases the space
    dimension of \p *this by adding \p y.space_dimension() new
    dimensions; then adds to the system of constraints of \p *this a
    renamed-apart version of the constraints of \p y.
  */
  void concatenate_assign(const BD_Shape& y);

  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed
    The set of Variable objects corresponding to the dimensions to be removed.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with one of the Variable
    objects contained in \p to_be_removed.
  */
  void remove_space_dimensions(const Variables_Set& to_be_removed);

  /*! \brief
    Removes the higher dimensions so that the resulting space
    will have dimension \p new_dimension.

    \exception std::invalid_argument
    Thrown if \p new_dimension is greater than the space dimension
    of \p *this.
  */
  void remove_higher_space_dimensions(dimension_type new_dimension);

  /*! \brief
    Remaps the dimensions of the vector space according to
    a \ref Mapping_the_Dimensions_of_the_Vector_Space "partial function".

    \param pfunc
    The partial function specifying the destiny of each dimension.

    The template class Partial_Function must provide the following
    methods.
    \code
      bool has_empty_codomain() const
    \endcode
    returns <CODE>true</CODE> if and only if the represented partial
    function has an empty co-domain (i.e., it is always undefined).
    The <CODE>has_empty_codomain()</CODE> method will always be called
    before the methods below.  However, if
    <CODE>has_empty_codomain()</CODE> returns <CODE>true</CODE>, none
    of the functions below will be called.
    \code
      dimension_type max_in_codomain() const
    \endcode
    returns the maximum value that belongs to the co-domain
    of the partial function.
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$k\f$ be the value
    of \p i.  If \f$f\f$ is defined in \f$k\f$, then \f$f(k)\f$ is
    assigned to \p j and <CODE>true</CODE> is returned.
    If \f$f\f$ is undefined in \f$k\f$, then <CODE>false</CODE> is
    returned.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in the
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
    Thrown if \p var does not correspond to a dimension of the vector space.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    If \p *this has space dimension \f$n\f$, with \f$n > 0\f$,
    and <CODE>var</CODE> has space dimension \f$k \leq n\f$,
    then the \f$k\f$-th space dimension is
    \ref expand_space_dimension "expanded" to \p m new space dimensions
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
    one of the Variable objects contained in \p to_be_folded.
    Also thrown if \p var is contained in \p to_be_folded.

    If \p *this has space dimension \f$n\f$, with \f$n > 0\f$,
    <CODE>var</CODE> has space dimension \f$k \leq n\f$,
    \p to_be_folded is a set of variables whose maximum space dimension
    is also less than or equal to \f$n\f$, and \p var is not a member
    of \p to_be_folded, then the space dimensions corresponding to
    variables in \p to_be_folded are \ref fold_space_dimensions "folded"
    into the \f$k\f$-th space dimension.
  */
  void fold_space_dimensions(const Variables_Set& to_be_folded, Variable var);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

  PPL_OUTPUT_DECLARATIONS

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  friend bool Parma_Polyhedra_Library::operator==<T>(const BD_Shape<T>& x,
						     const BD_Shape<T>& y);
  template <typename Temp, typename To, typename U>
  friend bool Parma_Polyhedra_Library::rectilinear_distance_assign
  (Checked_Number<To, Extended_Number_Policy>& r,
   const BD_Shape<U>& x, const BD_Shape<U>& y, const Rounding_Dir dir,
   Temp& tmp0, Temp& tmp1, Temp& tmp2);
  template <typename Temp, typename To, typename U>
  friend bool Parma_Polyhedra_Library::euclidean_distance_assign
  (Checked_Number<To, Extended_Number_Policy>& r,
   const BD_Shape<U>& x, const BD_Shape<U>& y, const Rounding_Dir dir,
   Temp& tmp0, Temp& tmp1, Temp& tmp2);
  template <typename Temp, typename To, typename U>
  friend bool Parma_Polyhedra_Library::l_infinity_distance_assign
  (Checked_Number<To, Extended_Number_Policy>& r,
   const BD_Shape<U>& x, const BD_Shape<U>& y, const Rounding_Dir dir,
   Temp& tmp0, Temp& tmp1, Temp& tmp2);

private:
  template <typename U> friend class Parma_Polyhedra_Library::BD_Shape;

  //! The matrix representing the system of bounded differences.
  DB_Matrix<N> dbm;

#define PPL_IN_BD_Shape_CLASS
#include "BDS_Status.idefs.hh"
#undef PPL_IN_BD_Shape_CLASS

  //! The status flags to keep track of the internal state.
  Status status;

  //! A matrix indicating which constraints are redundant.
  Bit_Matrix redundancy_dbm;

  //! Returns <CODE>true</CODE> if the BDS is known to be empty.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-empty.
  */
  bool marked_empty() const;

  /*! \brief
    Returns <CODE>true</CODE> if the system of bounded differences
    is known to be shortest-path closed.

    The return value <CODE>false</CODE> does not necessarily
    implies that <CODE>this->dbm</CODE> is not shortest-path closed.
  */
  bool marked_shortest_path_closed() const;

  /*! \brief
    Returns <CODE>true</CODE> if the system of bounded differences
    is known to be shortest-path reduced.

    The return value <CODE>false</CODE> does not necessarily
    implies that <CODE>this->dbm</CODE> is not shortest-path reduced.
  */
  bool marked_shortest_path_reduced() const;

  //! Turns \p *this into an empty BDS.
  void set_empty();

  //! Turns \p *this into an zero-dimensional universe BDS.
  void set_zero_dim_univ();

  //! Assigns to <CODE>this->dbm</CODE> its shortest-path closure.
  void shortest_path_closure_assign() const;

  /*! \brief
    Assigns to <CODE>this->dbm</CODE> its shortest-path closure and
    records into <CODE>this->redundancy_dbm</CODE> which of the entries
    in <CODE>this->dbm</CODE> are redundant.
  */
  void shortest_path_reduction_assign() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if <CODE>this->dbm</CODE>
    is shortest-path closed and <CODE>this->redundancy_dbm</CODE>
    correctly flags the redundant entries in <CODE>this->dbm</CODE>.
  */
  bool is_shortest_path_reduced() const;

  //! Adds the constraint <CODE>dbm[i][j] \<= k</CODE>.
  void add_dbm_constraint(dimension_type i, dimension_type j, N k);
  //! Adds the constraint <CODE>dbm[i][j] \<= num/den</CODE>.
  void add_dbm_constraint(dimension_type i, dimension_type j,
			  Coefficient_traits::const_reference num,
			  Coefficient_traits::const_reference den);

  //! Removes all the constraints on row/column \p v.
  void forget_all_dbm_constraints(dimension_type v);
  //! Removes all binary constraints on row/column \p v.
  void forget_binary_dbm_constraints(dimension_type v);

  //! An helper function for the computation of affine relations.
  /*!
    For each dbm index \p u (less than or equal to \p last_v and different
    from \p v), deduce constraints of the form <CODE>v - u \<= c</CODE>,
    starting from \p ub_v which is an upper bound for \p v.

    The shortest-path closure is able to deduce the constraint
    <CODE>v - u \<= ub_v - lb_u</CODE>. We can be more precise if variable
    \p u played an active role in the computation of the upper bound for
    \p v, i.e., if the corresponding coefficient
    <CODE>q == sc_expr[u]/sc_den</CODE> is greater than zero. In particular:
      - if <CODE>q \>= 1</CODE>, then <CODE>v - u \<= ub_v - ub_u</CODE>;
      - if <CODE>0 \< q \< 1</CODE>, then
        <CODE>v - u \<= ub_v - (q*ub_u + (1-q)*lb_u)</CODE>.
  */
  void deduce_v_minus_u_bounds(dimension_type v,
			       dimension_type last_v,
			       const Linear_Expression& sc_expr,
			       Coefficient_traits::const_reference sc_den,
			       const N& ub_v);

  //! An helper function for the computation of affine relations.
  /*!
    For each dbm index \p u (less than or equal to \p last_v and different
    from \p v), deduce constraints of the form <CODE>u - v \<= c</CODE>,
    starting from \p minus_lb_v which is a lower bound for \p v.

    The shortest-path closure is able to deduce the constraint
    <CODE>u - v \<= ub_u - lb_v</CODE>. We can be more precise if variable
    \p u played an active role in the computation of the lower bound for
    \p v, i.e., if the corresponding coefficient
    <CODE>q == sc_expr[u]/sc_den</CODE> is greater than zero.
    In particular:
      - if <CODE>q \>= 1</CODE>, then <CODE>u - v \<= lb_u - lb_v</CODE>;
      - if <CODE>0 \< q \< 1</CODE>, then
        <CODE>u - v \<= (q*lb_u + (1-q)*ub_u) - lb_v</CODE>.
  */
  void deduce_u_minus_v_bounds(dimension_type v,
			       dimension_type last_v,
			       const Linear_Expression& sc_expr,
			       Coefficient_traits::const_reference sc_den,
			       const N& minus_lb_v);

  /*! \brief
    Adds to \p limiting_shape the bounded differences in \p cs
    that are satisfied by \p *this.
  */
  void get_limiting_shape(const Constraint_System& cs,
			  BD_Shape& limiting_shape) const;

  //! Compute the (zero-equivalence classes) predecessor relation.
  /*!
    It is assumed that the BDS is not empty and shortest-path closed.
  */
  void compute_predecessors(std::vector<dimension_type>& predecessor) const;

  //! Compute the leaders of zero-equivalence classes.
  /*!
    It is assumed that the BDS is not empty and shortest-path closed.
  */
  void compute_leaders(std::vector<dimension_type>& leaders) const;

#if !defined(__GNUC__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ > 3)
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<<>(std::ostream& s,
						      const BD_Shape<T>& c);
#else
  // This is too lax than wanted.
  template <typename U>
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const BD_Shape<U>& c);
#endif

  //! \name Exception Throwers
  //@{
  void throw_dimension_incompatible(const char* method,
				    const BD_Shape& x) const;

  void throw_dimension_incompatible(const char* method,
				    dimension_type required_dim) const;

  void throw_dimension_incompatible(const char* method,
				    const Constraint& c) const;

  void throw_dimension_incompatible(const char* method,
				    const Generator& g) const;

  void throw_dimension_incompatible(const char* method,
				    const char* name_row,
				    const Linear_Expression& y) const;

  static void throw_constraint_incompatible(const char* method);

  static void throw_expression_too_complex(const char* method,
					   const Linear_Expression& e);

  static void throw_generic(const char* method, const char* reason);
  //@} // Exception Throwers
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
template <typename T>
void swap(Parma_Polyhedra_Library::BD_Shape<T>& x,
	  Parma_Polyhedra_Library::BD_Shape<T>& y);

} // namespace std

#include "BDS_Status.inlines.hh"
#include "BD_Shape.inlines.hh"
#include "BD_Shape.templates.hh"

#endif // !defined(PPL_BD_Shape_defs_hh)
