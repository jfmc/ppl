/* Polyhedron class declaration.
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

#ifndef PPL_Polyhedron_defs_hh
#define PPL_Polyhedron_defs_hh 1

#include "Polyhedron.types.hh"
#include "globals.defs.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Generator_System.defs.hh"
#include "Generator_System.inlines.hh"
#include "Saturation_Matrix.defs.hh"
#include "Generator.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "BHRZ03_Certificate.types.hh"
#include "H79_Certificate.types.hh"
#include <vector>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Polyhedron
  Writes a textual representation of \p ph on \p s:
  <CODE>false</CODE> is written if \p ph is an empty polyhedron;
  <CODE>true</CODE> is written if \p ph is a universe polyhedron;
  a minimized system of constraints defining \p ph is written otherwise,
  all constraints in one row separated by ", ".
*/
std::ostream&
operator<<(std::ostream& s, const Polyhedron& ph);

} // namespace IO_Operators

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are the same polyhedron.
/*!
  \relates Polyhedron
  Note that \p x and \p y may be topology- and/or dimension-incompatible
  polyhedra: in those cases, the value <CODE>false</CODE> is returned.
*/
bool operator==(const Polyhedron& x, const Polyhedron& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are different polyhedra.
/*!
  \relates Polyhedron
  Note that \p x and \p y may be topology- and/or dimension-incompatible
  polyhedra: in those cases, the value <CODE>true</CODE> is returned.
*/
bool operator!=(const Polyhedron& x, const Polyhedron& y);

} // namespace Parma_Polyhedra_Library


//! The base class for convex polyhedra.
/*!
    An object of the class Polyhedron represents a convex polyhedron
    in the vector space \f$\Rset^n\f$.

    A polyhedron can be specified as either a finite system of constraints
    or a finite system of generators (see Section \ref representation)
    and it is always possible to obtain either representation.
    That is, if we know the system of constraints, we can obtain
    from this the system of generators that define the same polyhedron
    and vice versa.
    These systems can contain redundant members: in this case we say
    that they are not in the minimal form.
    Most operators on polyhedra are provided with two implementations:
    one of these, denoted <CODE>\<operator-name\>_and_minimize</CODE>,
    also enforces the minimization of the representations,
    and returns the Boolean value <CODE>false</CODE> whenever
    the resulting polyhedron turns out to be empty.

    Two key attributes of any polyhedron are its topological kind
    (recording whether it is a C_Polyhedron or an NNC_Polyhedron object)
    and its space dimension (the dimension \f$n \in \Nset\f$ of
    the enclosing vector space):

    - all polyhedra, the empty ones included, are endowed with
      a specific topology and space dimension;
    - most operations working on a polyhedron and another object
      (i.e., another polyhedron, a constraint or generator,
      a set of variables, etc.) will throw an exception if
      the polyhedron and the object are not both topology-compatible
      and dimension-compatible (see Section \ref representation);
    - there is no way to change the topology of a polyhedron;
      rather, there are constructors of the two derived classes
      that builds a new polyhedron having a topology when
      provided with the corresponding polyhedron of the other topology;
    - the only ways to change the space dimension of a polyhedron are:
      - <EM>explicit</EM> calls to operators provided for that purpose;
      - standard copy, assignment and swap operators.

    Note that four different polyhedra can be defined on
    the zero-dimension space:
    the empty polyhedron, either closed or NNC,
    and the universe polyhedron \f$R^0\f$, again either closed or NNC.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined (where they are
    used) as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a polyhedron corresponding to
    a square in \f$\Rset^2\f$, given as a system of constraints:
    \code
  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  C_Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from a system of generators specifying
    the four vertices of the square:
    \code
  Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + 3*y));
  gs.insert(point(3*x + 0*y));
  gs.insert(point(3*x + 3*y));
  C_Polyhedron ph(gs);
    \endcode

    \par Example 2
    The following code builds an unbounded polyhedron
    corresponding to a half-strip in \f$\Rset^2\f$,
    given as a system of constraints:
    \code
  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
  C_Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the system of generators specifying
    the two vertices of the polyhedron and one ray:
    \code
  Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + y));
  gs.insert(ray(x - y));
  C_Polyhedron ph(gs);
    \endcode

    \par Example 3
    The following code builds the polyhedron corresponding to
    an half-plane by adding a single constraint
    to the universe polyhedron in \f$\Rset^2\f$:
    \code
  C_Polyhedron ph(2);
  ph.add_constraint(y >= 0);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the empty polyhedron in the space \f$\Rset^2\f$
    and inserting the appropriate generators
    (a point, a ray and a line).
    \code
  C_Polyhedron ph(2, Polyhedron::EMPTY);
  ph.add_generator(point(0*x + 0*y));
  ph.add_generator(ray(y));
  ph.add_generator(line(x));
    \endcode
    Note that, although the above polyhedron has no vertices, we must add
    one point, because otherwise the result of the Minkowsky's sum
    would be an empty polyhedron.
    To avoid subtle errors related to the minimization process,
    it is required that the first generator inserted in an empty
    polyhedron is a point (otherwise, an exception is thrown).

    \par Example 4
    The following code shows the use of the function
    <CODE>add_space_dimensions_and_embed</CODE>:
    \code
  C_Polyhedron ph(1);
  ph.add_constraint(x == 2);
  ph.add_space_dimensions_and_embed(1);
    \endcode
    We build the universe polyhedron in the 1-dimension space \f$\Rset\f$.
    Then we add a single equality constraint,
    thus obtaining the polyhedron corresponding to the singleton set
    \f$\{ 2 \} \sseq \Rset\f$.
    After the last line of code, the resulting polyhedron is
    \f[
      \bigl\{\,
        (2, y)^\transpose \in \Rset^2
      \bigm|
        y \in \Rset
      \,\bigr\}.
    \f]

    \par Example 5
    The following code shows the use of the function
    <CODE>add_space_dimensions_and_project</CODE>:
    \code
  C_Polyhedron ph(1);
  ph.add_constraint(x == 2);
  ph.add_space_dimensions_and_project(1);
    \endcode
    The first two lines of code are the same as in Example 4 for
    <CODE>add_space_dimensions_and_embed</CODE>.
    After the last line of code, the resulting polyhedron is
    the singleton set
    \f$\bigl\{ (2, 0)^\transpose \bigr\} \sseq \Rset^2\f$.

    \par Example 6
    The following code shows the use of the function
    <CODE>affine_image</CODE>:
    \code
  C_Polyhedron ph(2, Polyhedron::EMPTY);
  ph.add_generator(point(0*x + 0*y));
  ph.add_generator(point(0*x + 3*y));
  ph.add_generator(point(3*x + 0*y));
  ph.add_generator(point(3*x + 3*y));
  Linear_Expression coeff = x + 4;
  ph.affine_image(x, coeff);
    \endcode
    In this example the starting polyhedron is a square in
    \f$\Rset^2\f$, the considered variable is \f$x\f$ and the affine
    expression is \f$x+4\f$.  The resulting polyhedron is the same
    square translated to the right.  Moreover, if the affine
    transformation for the same variable \p x is \f$x+y\f$:
    \code
  Linear_Expression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x-y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable; for example, the affine expression \f$y\f$:
    \code
  Linear_Expression coeff = y;
    \endcode
    the resulting polyhedron is a diagonal of the square.

    \par Example 7
    The following code shows the use of the function
    <CODE>affine_preimage</CODE>:
    \code
  C_Polyhedron ph(2);
  ph.add_constraint(x >= 0);
  ph.add_constraint(x <= 3);
  ph.add_constraint(y >= 0);
  ph.add_constraint(y <= 3);
  Linear_Expression coeff = x + 4;
  ph.affine_preimage(x, coeff);
    \endcode
    In this example the starting polyhedron, \p var and the affine
    expression and the denominator are the same as in Example 6,
    while the resulting polyhedron is again the same square,
    but translated to the left.
    Moreover, if the affine transformation for \p x is \f$x+y\f$
    \code
  Linear_Expression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x+y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable \p x, for example, the affine expression \f$y\f$:
    \code
  Linear_Expression coeff = y;
    \endcode
    the resulting polyhedron is a line that corresponds to the \f$y\f$ axis.

    \par Example 8
    For this example we use also the variables:
    \code
  Variable z(2);
  Variable w(3);
    \endcode
    The following code shows the use of the function
    <CODE>remove_space_dimensions</CODE>:
    \code
  Generator_System gs;
  gs.insert(point(3*x + y +0*z + 2*w));
  C_Polyhedron ph(gs);
  set<Variable> to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  ph.remove_space_dimensions(to_be_removed);
    \endcode
    The starting polyhedron is the singleton set
    \f$\bigl\{ (3, 1, 0, 2)^\transpose \bigr\} \sseq \Rset^4\f$, while
    the resulting polyhedron is
    \f$\bigl\{ (3, 2)^\transpose \bigr\} \sseq \Rset^2\f$.
    Be careful when removing space dimensions <EM>incrementally</EM>:
    since dimensions are automatically renamed after each application
    of the <CODE>remove_space_dimensions</CODE> operator, unexpected
    results can be obtained.
    For instance, by using the following code we would obtain
    a different result:
    \code
  set<Variable> to_be_removed1;
  to_be_removed1.insert(y);
  ph.remove_space_dimensions(to_be_removed1);
  set<Variable> to_be_removed2;
  to_be_removed2.insert(z);
  ph.remove_space_dimensions(to_be_removed2);
    \endcode
    In this case, the result is the polyhedron
    \f$\bigl\{(3, 0)^\transpose \bigr\} \sseq \Rset^2\f$:
    when removing the set of dimensions \p to_be_removed2
    we are actually removing variable \f$w\f$ of the original polyhedron.
    For the same reason, the operator \p remove_space_dimensions
    is not idempotent: removing twice the same set of dimensions
    is never a no-op.
*/

class Parma_Polyhedra_Library::Polyhedron {
public:
  //! Returns the maximum space dimension all kinds of Polyhedron can handle.
  static dimension_type max_space_dimension();

  //! Kinds of degenerate polyhedra.
  enum Degenerate_Kind {
    //! The universe polyhedron, i.e., the whole vector space.
    UNIVERSE,
    //! The empty polyhedron, i.e., the empty set.
    EMPTY
  };

protected:
  //! Builds a polyhedron having the specified properties.
  /*!
    \param topol
    The topology of the polyhedron;

    \param num_dimensions
    The number of dimensions of the vector space enclosing the polyhedron;

    \param kind
    Specifies whether the universe or the empty polyhedron has to be built.
  */
  Polyhedron(Topology topol,
	     dimension_type num_dimensions,
	     Degenerate_Kind kind);

  //! Ordinary copy-constructor.
  Polyhedron(const Polyhedron& y);

  //! Builds a polyhedron from a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.

    \param topol
    The topology of the polyhedron;

    \param cs
    The system of constraints defining the polyhedron.

    \exception std::invalid_argument
    Thrown if the topology of \p cs is incompatible with \p topol.
  */
  Polyhedron(Topology topol, const Constraint_System& cs);

  //! Builds a polyhedron recycling a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.

    \param topol
    The topology of the polyhedron;

    \param cs
    The system of constraints defining the polyhedron.  It is not
    declared <CODE>const</CODE> because its data-structures will be
    recycled to build the polyhedron.

    \exception std::invalid_argument
    Thrown if the topology of \p cs is incompatible with \p topol.
  */
  Polyhedron(Topology topol, Constraint_System& cs);

  //! Builds a polyhedron from a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.

    \param topol
    The topology of the polyhedron;

    \param gs
    The system of generators defining the polyhedron.

    \exception std::invalid_argument
    Thrown if if the topology of \p gs is incompatible with \p topol,
    or if the system of generators is not empty but has no points.
  */
  Polyhedron(Topology topol, const Generator_System& gs);

  //! Builds a polyhedron recycling a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.

    \param topol
    The topology of the polyhedron;

    \param gs
    The system of generators defining the polyhedron.  It is not
    declared <CODE>const</CODE> because its data-structures will be
    recycled to build the polyhedron.

    \exception std::invalid_argument
    Thrown if if the topology of \p gs is incompatible with \p topol,
    or if the system of generators is not empty but has no points.
  */
  Polyhedron(Topology topol, Generator_System& gs);

  //! Builds a polyhedron out of a generic, interval-based bounding box.
  /*!
    \param topol
    The topology of the polyhedron;

    \param box
    The bounding box representing the polyhedron to be built.

    \exception std::invalid_argument
    Thrown if \p box has intervals that are incompatible with \p topol.

    The template class Box must provide the following methods.
    \code
      dimension_type space_dimension() const
    \endcode
    returns the dimension of the vector space enclosing the polyhedron
    represented by the bounding box.
    \code
      bool is_empty() const
    \endcode
    returns <CODE>true</CODE> if and only if the bounding box
    describes the empty set.
    The <CODE>is_empty()</CODE> method will always be called before the
    methods below.  However, if <CODE>is_empty()</CODE> returns
    <CODE>true</CODE>, none of the functions below will be called.
    \code
      bool get_lower_bound(dimension_type k, bool closed,
                           Integer& n, Integer& d) const
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from below, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the the lower boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.  The fraction
    \f$n/d\f$ is in canonical form if and only if \f$n\f$ and \f$d\f$
    have no common factors and \f$d\f$ is positive, \f$0/1\f$ being
    the unique representation for zero.
    \code
      bool get_upper_bound(dimension_type k, bool closed,
                           Integer& n, Integer& d) const
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from above, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the the upper boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.
  */
  template <typename Box>
  Polyhedron(Topology topol, const Box& box);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  Polyhedron& operator=(const Polyhedron& y);

public:
  //! \name Member Functions that Do Not Modify the Polyhedron
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Returns \f$0\f$, if \p *this is empty; otherwise, returns the
  //! \ref affine_dimension "affine dimension" of \p *this.
  dimension_type affine_dimension() const;

  //! Returns the system of constraints.
  const Constraint_System& constraints() const;

  //! Returns the system of constraints, with no redundant constraint.
  const Constraint_System& minimized_constraints() const;

  //! Returns the system of generators.
  const Generator_System& generators() const;

  //! Returns the system of generators, with no redundant generator.
  const Generator_System& minimized_generators() const;

  //! \brief
  //! Returns the relations holding between the polyhedron \p *this
  //! and the constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! \brief
  //! Returns the relations holding between the polyhedron \p *this
  //! and the generator \p g.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is
  //! an empty polyhedron.
  bool is_empty() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a universe polyhedron.
  bool is_universe() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a topologically closed subset of the vector space.
  bool is_topologically_closed() const;

  //! Returns <CODE>true</CODE> if and only if \p *this and \p y are disjoint.
  /*!
    \exception std::invalid_argument
    Thrown if \p x and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool is_disjoint_from(const Polyhedron& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a bounded polyhedron.
  bool is_bounded() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p expr is
  //! bounded from above in \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.
  */
  bool bounds_from_above(const Linear_Expression& expr) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p expr is
  //! bounded from below in \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.
  */
  bool bounds_from_below(const Linear_Expression& expr) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is not empty
  //! and \p expr is bounded from above in \p *this, in which case
  //! the supremum value is computed.
  /*!
    \param expr
    The linear expression to be maximized subject to \p *this;

    \param sup_n
    The numerator of the supremum value;

    \param sup_d
    The denominator of the supremum value;

    \param maximum 
    <CODE>true</CODE> if and only if the supremum is also the maximum value.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from above,
    <CODE>false</CODE> is returned and \p sup_n, \p sup_d
    and \p maximum are left untouched.
  */
  bool maximize(const Linear_Expression& expr,
		Integer& sup_n, Integer& sup_d, bool& maximum) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is not empty
  //! and \p expr is bounded from above in \p *this, in which case
  //! the supremum value and a point where \p expr reaches it are computed.
  /*!
    \param expr
    The linear expression to be maximized subject to \p *this;

    \param sup_n
    The numerator of the supremum value;

    \param sup_d
    The denominator of the supremum value;

    \param maximum
    <CODE>true</CODE> if and only if the supremum is also the maximum value;

    \param pppoint
    When nonzero and maximization succeeds, a pointer to a point or
    closure point where \p expr reaches its supremum value will be
    written at this address.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from above,
    <CODE>false</CODE> is returned and \p sup_n, \p sup_d, \p maximum
    and \p pppoint are left untouched.
  */
  bool maximize(const Linear_Expression& expr,
		Integer& sup_n, Integer& sup_d, bool& maximum,
		const Generator** const pppoint) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is not empty
  //! and \p expr is bounded from below in \p *this, in which case
  //! the infimum value is computed.
  /*!
    \param expr
    The linear expression to be minimized subject to \p *this;

    \param inf_n
    The numerator of the infimum value;

    \param inf_d
    The denominator of the infimum value;

    \param minimum
    <CODE>true</CODE> if and only if the infimum is also the minimum value.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from below,
    <CODE>false</CODE> is returned and \p inf_n, \p inf_d
    and \p minimum are left untouched.
  */
  bool minimize(const Linear_Expression& expr,
		Integer& inf_n, Integer& inf_d, bool& minimum) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is not empty
  //! and \p expr is bounded from below in \p *this, in which case
  //! the infimum value and a point where \p expr reaches it are computed.
  /*!
    \param expr
    The linear expression to be minimized subject to \p *this;

    \param inf_n
    The numerator of the infimum value;

    \param inf_d
    The denominator of the infimum value;

    \param minimum
    <CODE>true</CODE> if and only if the infimum is also the minimum value;

    \param pppoint
    When nonzero and minimization succeeds, a pointer to a point or
    closure point where \p expr reaches its infimum value will be
    written at this address.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded from below,
    <CODE>false</CODE> is returned and \p inf_n, \p inf_d, \p minimum
    and \p pppoint are left untouched.
  */
  bool minimize(const Linear_Expression& expr,
		Integer& inf_n, Integer& inf_d, bool& minimum,
		const Generator** const pppoint) const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool contains(const Polyhedron& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this strictly contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool strictly_contains(const Polyhedron& y) const;

  //! Uses \p *this to shrink a generic, interval-based bounding box.
  /*!
    \param box
    The bounding box to be shrunk;

    \param complexity
    The complexity class of the algorithm to be used.

    The template class Box must provide the following
    methods, whose return value, if any, is simply ignored.
    \code
      set_empty()
    \endcode
    causes the box to become empty, i.e., to represent the empty set.
    \code
      raise_lower_bound(dimension_type k, bool closed,
                        Integer_traits::const_reference n,
                        Integer_traits::const_reference d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension
    with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>false</CODE>.
    \code
      lower_upper_bound(dimension_type k, bool closed,
                        Integer_traits::const_reference n,
                        Integer_traits::const_reference d)
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
  bool OK(bool check_not_empty = false) const;

  //@} // Member Functions that Do Not Modify the Polyhedron

  //! \name Space Dimension Preserving Member Functions that May Modify the Polyhedron
  //@{

  //! \brief
  //! Adds a copy of constraint \p c to the system of constraints
  //! of \p *this (without minimizing the result).
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are topology-incompatible
    or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! \brief
  //! Adds a copy of constraint \p c to the system of constraints
  //! of \p *this, minimizing the result
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are topology-incompatible
    or dimension-incompatible.
  */
  bool add_constraint_and_minimize(const Constraint& c);

  //! \brief
  //! Adds a copy of generator \p g to the system of generators
  //! of \p *this (without minimizing the result).
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are topology-incompatible or
    dimension-incompatible, or if \p *this is an empty polyhedron and
    \p g is not a point.
  */
  void add_generator(const Generator& g);

  //! \brief
  //! Adds a copy of generator \p g to the system of generators
  //! of \p *this, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are topology-incompatible or
    dimension-incompatible, or if \p *this is an empty polyhedron and
    \p g is not a point.
  */
  bool add_generator_and_minimize(const Generator& g);

  //! \brief Adds a copy of the constraints in \p cs to the system
  //! of constraints of \p *this (without minimizing the result).
  /*!
    \param cs
    Contains the constraints that will be added to the system of
    constraints of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void add_constraints(const Constraint_System& cs);

  //! \brief Adds the constraints in \p cs to the system of constraints
  //! of \p *this (without minimizing the result).
  /*!
    \param cs
    The constraint system that will be recycled, adding its
    constraints to the system of constraints of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.

    \warning
    The only assumption that can be made on \p cs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  void add_recycled_constraints(Constraint_System& cs);

  //! \brief
  //! Adds a copy of the constraints in \p cs to the system
  //! of constraints of \p *this, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    Contains the constraints that will be added to the system of
    constraints of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  bool add_constraints_and_minimize(const Constraint_System& cs);

  //! \brief
  //! Adds the constraints in \p cs to the system of constraints
  //! of \p *this, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    The constraint system that will be recycled, adding its
    constraints to the system of constraints of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.

    \warning
    The only assumption that can be made on \p cs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_constraints_and_minimize(Constraint_System& cs);

  //! \brief Adds a copy of the generators in \p gs to the system
  //! of generators of \p *this (without minimizing the result).
  /*!
    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are topology-incompatible or
    dimension-incompatible, or if \p *this is empty and the system of
    generators \p gs is not empty, but has no points.
  */
  void add_generators(const Generator_System& gs);

  //! \brief Adds the generators in \p gs to the system of generators
  //! of \p *this (without minimizing the result).
  /*!
    \param gs
    The generator system that will be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are topology-incompatible or
    dimension-incompatible, or if \p *this is empty and the system of
    generators \p gs is not empty, but has no points.

    \warning
    The only assumption that can be made on \p gs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  void add_recycled_generators(Generator_System& gs);

  //! \brief Adds a copy of the generators in \p gs to the system
  //! of generators of \p *this, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are topology-incompatible or
    dimension-incompatible, or if \p *this is empty and the the system
    of generators \p gs is not empty, but has no points.
  */
  bool add_generators_and_minimize(const Generator_System& gs);

  //! \brief Adds the generators in \p gs to the system of generators
  //! of \p *this, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    The generator system that will be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are topology-incompatible or
    dimension-incompatible, or if \p *this is empty and the the system
    of generators \p gs is not empty, but has no points.

    \warning
    The only assumption that can be made on \p gs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_generators_and_minimize(Generator_System& gs);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y.
  //! The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void intersection_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y,
  //! minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the poly-hull of \p *this and \p y.
  //! The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void poly_hull_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the poly-hull of \p *this and \p y,
  //! minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool poly_hull_assign_and_minimize(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the \ref poly_difference "poly-difference" of
  //! \p *this and \p y. The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void poly_difference_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine image"
  //! of \p *this under the function mapping variable \p var to the
  //! affine expression specified by \p expr and \p denominator.
  /*!
    \param var
    The variable to which the affine expression is assigned;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression (optional argument with
    default value 1.)

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of
    \p *this.

    \if Include_Implementation_Details

    When considering the generators of a polyhedron, the
    affine transformation
    \f[
      \frac{\sum_{i=0}^{n-1} a_i x_i + b}{\mathrm{denominator}}
    \f]
    is assigned to \p var where \p expr is
    \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
    (\f$b\f$ is the inhomogeneous term).

    If constraints are up-to-date, it uses the specialized function
    affine_preimage() (for the system of constraints)
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

    In other words, if \f$R\f$ is a \f$m_1 \times n_1\f$ matrix representing
    the rays of the polyhedron, \f$V\f$ is a \f$m_2 \times n_2\f$
    matrix representing the points of the polyhedron and
    \f[
      P = \bigl\{\,
            \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
          \bigm|
            \vect{x} = \vect{\lambda} R + \vect{\mu} V,
	    \vect{\lambda} \in \Rset^{m_1}_+,
	    \vect{\mu} \in \Rset^{m_2}_+,
	    \sum_{i = 0}^{m_1 - 1} \lambda_i = 1
          \,\bigr\}
    \f]
    and \f$T\f$ is the affine transformation to apply to \f$P\f$, then
    the resulting polyhedron is
    \f[
      P' = \bigl\{\,
             (x_0, \ldots, T(x_0, \ldots, x_{n-1}),
                     \ldots, x_{n-1})^\mathrm{T}
           \bigm|
             (x_0, \ldots, x_{n-1})^\mathrm{T} \in P
           \,\bigr\}.
    \f]

    Affine transformations are, for example:
    - translations
    - rotations
    - symmetries.
    \endif
  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    Integer_traits::const_reference denominator
		      = Integer_one());

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine preimage"
  //! of \p *this under the function mapping variable \p var to the
  //! affine expression specified by \p expr and \p denominator.
  /*!
    \param var
    The variable to which the affine expression is substituted;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression (optional argument with
    default value 1.)

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p *this.

    \if Include_Implementation_Details

    When considering constraints of a polyhedron, the affine transformation
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
    were up-to-date remain up-to-date. Otherwise only constraints remain
    up-to-date.

    In other words, if \f$A\f$ is a \f$m \times n\f$ matrix representing
    the constraints of the polyhedron, \f$T\f$ is the affine transformation
    to apply to \f$P\f$ and
    \f[
      P = \bigl\{\,
            \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
          \bigm|
            A\vect{x} \geq \vect{0}
          \,\bigr\}.
    \f]
    The resulting polyhedron is
    \f[
      P' = \bigl\{\,
             \vect{x} = (x_0, \ldots, x_{n-1}))^\mathrm{T}
           \bigm|
             A'\vect{x} \geq \vect{0}
           \,\bigr\},
    \f]
    where \f$A'\f$ is defined as follows:
    \f[
      {a'}_{ij}
        = \begin{cases}
            a_{ij} * \mathrm{denominator} + a_{i\mathrm{var}}*\mathrm{expr}[j]
              \quad \mathrm{for } j \neq \mathrm{var}; \\
            \mathrm{expr}[\mathrm{var}] * a_{i\mathrm{var}},
              \quad \text{for } j = \mathrm{var}.
          \end{cases}
    \f]
    \endif
  */
  void affine_preimage(Variable var,
		       const Linear_Expression& expr,
		       Integer_traits::const_reference denominator
		         = Integer_one());

  //! \brief
  //! Assigns to \p *this the image of \p *this with respect to the
  //! \ref generalized_image "generalized affine transfer function"
  //! \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
  //! where \f$\mathord{\relsym}\f$ is the relation symbol encoded
  //! by \p relsym.
  /*!
    \param var
    The left hand side variable of the generalized affine transfer function;

    \param relsym
    The relation symbol;

    \param expr
    The numerator of the right hand side affine expression;

    \param denominator
    The denominator of the right hand side affine expression (optional
    argument with default value 1.)

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this are
    dimension-incompatible or if \p var is not a space dimension of \p *this
    or if \p *this is a C_Polyhedron and \p relsym is a strict
    relation symbol.
  */
  void generalized_affine_image(Variable var,
				const Relation_Symbol relsym,
				const Linear_Expression& expr,
				Integer_traits::const_reference denominator
				  = Integer_one());

  //! \brief
  //! Assigns to \p *this the image of \p *this with respect to the
  //! \ref generalized_image "generalized affine transfer function"
  //! \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
  //! \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.
  /*!
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
				const Relation_Symbol relsym,
				const Linear_Expression& rhs);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref time_elapse "time-elapse" between \p *this and \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void time_elapse_assign(const Polyhedron& y);

  //! Assigns to \p *this its topological closure.
  void topological_closure_assign();

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref BHRZ03_widening "BHRZ03-widening" between \p *this and \p y.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void BHRZ03_widening_assign(const Polyhedron& y, unsigned* tp = 0);

  //! \brief
  //! Improves the result of the \ref BHRZ03_widening "BHRZ03-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param cs
    The system of constraints used to improve the widened polyhedron;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void limited_BHRZ03_extrapolation_assign(const Polyhedron& y,
					   const Constraint_System& cs,
					   unsigned* tp = 0);

  //! \brief
  //! Improves the result of the \ref BHRZ03_widening "BHRZ03-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this, plus all the constraints
  //! of the form \f$\pm x \leq r\f$ and \f$\pm x < r\f$, with
  //! \f$r \in \Qset\f$, that are satisfied by all the points of \p *this.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param cs
    The system of constraints used to improve the widened polyhedron;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).
    
    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void bounded_BHRZ03_extrapolation_assign(const Polyhedron& y,
					   const Constraint_System& cs,
					   unsigned* tp = 0);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref H79_widening "H79-widening" between \p *this and \p y.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.
  */
  void H79_widening_assign(const Polyhedron& y, unsigned* tp = 0);

  //! \brief
  //! Improves the result of the \ref H79_widening "H79-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param cs
    The system of constraints used to improve the widened polyhedron;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void limited_H79_extrapolation_assign(const Polyhedron& y,
					const Constraint_System& cs,
					unsigned* tp = 0);

  //! \brief
  //! Improves the result of the \ref H79_widening "H79-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this, plus all the constraints
  //! of the form \f$\pm x \leq r\f$ and \f$\pm x < r\f$, with
  //! \f$r \in \Qset\f$, that are satisfied by all the points of \p *this.
  /*!
    \param y
    A polyhedron that <EM>must</EM> be contained in \p *this;

    \param cs
    The system of constraints used to improve the widened polyhedron;

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref widening_with_tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void bounded_H79_extrapolation_assign(const Polyhedron& y,
					const Constraint_System& cs,
					unsigned* tp = 0);

  //@} // Space Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! \brief
  //! Adds \p m new space dimensions and embeds the old polyhedron
  //! in the new vector space.
  /*!
    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new polyhedron, which is characterized by a system
    of constraints in which the variables running through
    the new dimensions are not constrained.
    For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
    and adding a third space dimension, the result will be the polyhedron
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cP
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new space dimensions to the polyhedron
  //! and does not embed it in the new vector space.
  /*!
    \param m
    The number of space dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new polyhedron, which is characterized by a system
    of constraints in which the variables running through
    the new dimensions are all constrained to be equal to 0.
    For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
    and adding a third space dimension, the result will be the polyhedron
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cP
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_project(dimension_type m);

  //! \brief
  //! Assigns to \p *this the \ref concatenate "concatenation"
  //! of \p *this and \p y, taken in this order.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible.

    \exception std::length_error
    Thrown if the concatenation would cause the vector space
    to exceed dimension <CODE>max_space_dimension()</CODE>.
  */
  void concatenate_assign(const Polyhedron& y);

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

  //! \brief
  //! Remaps the dimensions of the vector space
  //! according to a \ref map_space_dimensions "partial function".
  /*!
    \param pfunc
    The partial function specifying the destiny of each space dimension.

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
    returns the maximum value that belongs to the codomain
    of the partial function.
    The <CODE>max_in_codomain()</CODE> method is called at most once.
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$k\f$ be the value
    of \p i.  If \f$f\f$ is defined in \f$k\f$, then \f$f(k)\f$ is
    assigned to \p j and <CODE>true</CODE> is returned.
    If \f$f\f$ is undefined in \f$k\f$, then <CODE>false</CODE> is
    returned.
    This method is called at most \f$n\f$ times, where \f$n\f$ is the
    dimension of the vector space enclosing the polyhedron.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in the
    \ref map_space_dimensions "specification of the mapping operator".
  */
  template <typename Partial_Function>
  void map_space_dimensions(const Partial_Function& pfunc);

  //! Creates \p m copies of the space dimension corresponding to \p var.
  /*!
    \param var
    The variable corresponding to the space dimension to be replicated;

    \param m
    The number of replica to be created.

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

  friend bool Parma_Polyhedra_Library::operator==(const Polyhedron& x,
						  const Polyhedron& y);

  //! \name Miscellaneous Member Functions
  //@{

  //! Destructor.
  ~Polyhedron();

  //! \brief
  //! Swaps \p *this with polyhedron \p y.
  //! (\p *this and \p y can be dimension-incompatible.)
  /*!
    \exception std::invalid_argument
    Thrown if \p x and \p y are topology-incompatible.
  */
  void swap(Polyhedron& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Loads from \p s an ASCII representation (as produced by
  //! \ref ascii_dump) and sets \p *this accordingly.
  //! Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //@} // Miscellaneous Member Functions

private:
  //! The system of constraints.
  Constraint_System con_sys;

  //! The system of generators.
  Generator_System gen_sys;

  //! The saturation matrix having constraints on its columns.
  Saturation_Matrix sat_c;

  //! The saturation matrix having generators on its columns.
  Saturation_Matrix sat_g;

  // Please, do not move the following include directive:
  // `Ph_Status.idefs.hh' must be included exactly at this point.
  // And please do not remove the space separating `#' from `include':
  // this ensures that the directive will not be moved during the
  // procedure that automatically creates the library's include file
  // (see `Makefile.am' in the `src' directory).
# include "Ph_Status.idefs.hh"

  //! The status flags to keep track of the polyhedron's internal state.
  Status status;

  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

  //! Returns the topological kind of the polyhedron.
  Topology topology() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if the polyhedron
  //! is necessarily closed.
  bool is_necessarily_closed() const;

  //! \name Private Verifiers: Verify if Individual Flags are Set
  //@{

  //! Returns <CODE>true</CODE> if the polyhedron is known to be empty.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-empty.
  */
  bool marked_empty() const;

  //! Returns <CODE>true</CODE> if the system of constraints is up-to-date.
  bool constraints_are_up_to_date() const;

  //! Returns <CODE>true</CODE> if the system of generators is up-to-date.
  bool generators_are_up_to_date() const;

  //! Returns <CODE>true</CODE> if the system of constraints is minimized.
  /*!
    Note that only \em weak minimization is entailed, so that
    an NNC polyhedron may still have \f$\epsilon\f$-redundant constraints.
  */
  bool constraints_are_minimized() const;

  //! Returns <CODE>true</CODE> if the system of generators is minimized.
  /*!
    Note that only \em weak minimization is entailed, so that
    an NNC polyhedron may still have \f$\epsilon\f$-redundant generators.
  */
  bool generators_are_minimized() const;

  //! Returns <CODE>true</CODE> if there are pending constraints.
  bool has_pending_constraints() const;

  //! Returns <CODE>true</CODE> if there are pending generators.
  bool has_pending_generators() const;

  //! \brief
  //! Returns <CODE>true</CODE> if there are
  //! either pending constraints or pending generators.
  bool has_something_pending() const;

  //! \brief
  //! Returns <CODE>true</CODE> if the polyhedron can have something
  //! pending.
  bool can_have_something_pending() const;

  //! \brief
  //! Returns <CODE>true</CODE> if the saturation matrix \p sat_c
  //! is up-to-date.
  bool sat_c_is_up_to_date() const;

  //! \brief
  //! Returns <CODE>true</CODE> if the saturation matrix \p sat_g
  //! is up-to-date.
  bool sat_g_is_up_to_date() const;

  //@} // Private Verifiers: Verify if Individual Flags are Set

  //! \name State Flag Setters: Set Only the Specified Flags
  //@{

  //! \brief
  //! Sets \p status to express that the polyhedron
  //! is the universe 0-dimension vector space,
  //! clearing all corresponding matrices.
  void set_zero_dim_univ();

  //! \brief
  //! Sets \p status to express that the polyhedron is empty,
  //! clearing all corresponding matrices.
  void set_empty();

  //! Sets \p status to express that constraints are up-to-date.
  void set_constraints_up_to_date();

  //! Sets \p status to express that generators are up-to-date.
  void set_generators_up_to_date();

  //! Sets \p status to express that constraints are minimized.
  void set_constraints_minimized();

  //! Sets \p status to express that generators are minimized.
  void set_generators_minimized();

  //! Sets \p status to express that constraints are pending.
  void set_constraints_pending();

  //! Sets \p status to express that generators are pending.
  void set_generators_pending();

  //! Sets \p status to express that \p sat_c is up-to-date.
  void set_sat_c_up_to_date();

  //! Sets \p status to express that \p sat_g is up-to-date.
  void set_sat_g_up_to_date();

  //@} // State Flag Setters: Set Only the Specified Flags

  //! \name State Flag Cleaners: Clear Only the Specified Flag
  //@{

  //! Clears the \p status flag indicating that the polyhedron is empty.
  void clear_empty();

  //! Sets \p status to express that constraints are no longer up-to-date.
  /*!
    This also implies that they are neither minimized
    and both saturation matrices are no longer meaningful.
  */
  void clear_constraints_up_to_date();

  //! Sets \p status to express that generators are no longer up-to-date.
  /*!
    This also implies that they are neither minimized
    and both saturation matrices are no longer meaningful.
  */
  void clear_generators_up_to_date();

  //! Sets \p status to express that constraints are no longer minimized.
  void clear_constraints_minimized();

  //! Sets \p status to express that generators are no longer minimized.
  void clear_generators_minimized();

  //! Sets \p status to express that there are no longer pending constraints.
  void clear_pending_constraints();

  //! Sets \p status to express that there are no longer pending generators.
  void clear_pending_generators();

  //! Sets \p status to express that \p sat_c is no longer up-to-date.
  void clear_sat_c_up_to_date();

  //! Sets \p status to express that \p sat_g is no longer up-to-date.
  void clear_sat_g_up_to_date();

  //@} // State Flag Cleaners: Clear Only the Specified Flag

  //! \name The Handling of Pending Rows
  //@{

  //! \brief
  //! Processes the pending rows of either description of the polyhedron
  //! and obtains a minimized polyhedron.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.

    It is assumed that the polyhedron does have some constraints or
    generators pending.
  */
  bool process_pending() const;

  //! Processes the pending constraints and obtains a minimized polyhedron.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.

    It is assumed that the polyhedron does have some pending constraints.
  */
  bool process_pending_constraints() const;

  //! Processes the pending generators and obtains a minimized polyhedron.
  /*!
    It is assumed that the polyhedron does have some pending generators.
  */
  void process_pending_generators() const;

  //! \brief
  //! Lazily integrates the pending descriptions of the polyhedron
  //! to obtain a constraint system without pending rows.
  /*!
    It is assumed that the polyhedron does have some constraints or
    generators pending.
  */
  void remove_pending_to_obtain_constraints() const;

  //! \brief
  //! Lazily integrates the pending descriptions of the polyhedron
  //! to obtain a generator system without pending rows.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.

    It is assumed that the polyhedron does have some constraints or
    generators pending.
  */
  bool remove_pending_to_obtain_generators() const;

  //@} // The Handling of Pending Rows

  //! \name Updating and Sorting Matrices
  //@{

  //! Updates constraints starting from generators and minimizes them.
  /*!
    The resulting system of constraints is only partially sorted:
    the equalities are in the upper part of the matrix,
    while the inequalities in the lower part.
  */
  void update_constraints() const;

  //! Updates generators starting from constraints and minimizes them.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.

    The resulting system of generators is only partially sorted:
    the lines are in the upper part of the matrix,
    while rays and points are in the lower part.
    It is illegal to call this method when the Status field
    already declares the polyhedron to be empty.
  */
  bool update_generators() const;

  //! Updates \p sat_c using the updated constraints and generators.
  /*!
    It is assumed that constraints and generators are up-to-date
    and minimized and that the Status field does not already flag
    \p sat_c to be up-to-date.
    The values of the saturation matrix are computed as follows:
    \f[
      \begin{cases}
        sat\_c[i][j] = 0,
          \quad \text{if } G[i] \cdot C^\mathrm{T}[j] = 0; \\
        sat\_c[i][j] = 1,
          \quad \text{if } G[i] \cdot C^\mathrm{T}[j] > 0.
      \end{cases}
    \f]
  */
  void update_sat_c() const;

  //! Updates \p sat_g using the updated constraints and generators.
  /*!
    It is assumed that constraints and generators are up-to-date
    and minimized and that the Status field does not already flag
    \p sat_g to be up-to-date.
    The values of the saturation matrix are computed as follows:
    \f[
      \begin{cases}
        sat\_g[i][j] = 0,
          \quad \text{if } C[i] \cdot G^\mathrm{T}[j] = 0; \\
        sat\_g[i][j] = 1,
          \quad \text{if } C[i] \cdot G^\mathrm{T}[j] > 0.
      \end{cases}
    \f]
  */
  void update_sat_g() const;

  //! Sorts the matrix of constraints keeping status consistency.
  /*!
    It is assumed that constraints are up-to-date.
    If at least one of the saturation matrices is up-to-date,
    then \p sat_g is kept consistent with the sorted matrix
    of constraints.
    The method is declared \p const because reordering
    the constraints does not modify the polyhedron
    from a \e logical point of view.
  */
  void obtain_sorted_constraints() const;

  //! Sorts the matrix of generators keeping status consistency.
  /*!
    It is assumed that generators are up-to-date.
    If at least one of the saturation matrices is up-to-date,
    then \p sat_c is kept consistent with the sorted matrix
    of generators.
    The method is declared \p const because reordering
    the generators does not modify the polyhedron
    from a \e logical point of view.
  */
  void obtain_sorted_generators() const;

  //! Sorts the matrix of constraints and updates \p sat_c.
  /*!
    It is assumed that both constraints and generators
    are up-to-date and minimized.
    The method is declared \p const because reordering
    the constraints does not modify the polyhedron
    from a \e logical point of view.
  */
  void obtain_sorted_constraints_with_sat_c() const;

  //! Sorts the matrix of generators and updates \p sat_g.
  /*!
    It is assumed that both constraints and generators
    are up-to-date and minimized.
    The method is declared \p const because reordering
    the generators does not modify the polyhedron
    from a \e logical point of view.
  */
  void obtain_sorted_generators_with_sat_g() const;

  //@} // Updating and Sorting Matrices

  //! \name Weak and Strong Minimization of Descriptions
  //@{

  //! Applies (weak) minimization to both the constraints and generators.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.

    Minimization is not attempted if the Status field already declares
    both systems to be minimized.
  */
  bool minimize() const;

  //! Applies strong minimization to the constraints of an NNC polyhedron.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.
  */
  bool strongly_minimize_constraints() const;

  //! Applies strong minimization to the generators of an NNC polyhedron.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty polyhedron.
  */
  bool strongly_minimize_generators() const;

  //@} // Weak and Strong Minimization of Descriptions

  enum Three_Valued_Boolean {
    TVB_TRUE,
    TVB_FALSE,
    TVB_DONT_KNOW
  };

  //! Polynomial but incomplete equivalence test between polyhedra.
  Three_Valued_Boolean quick_equivalence_test(const Polyhedron& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is included in \p y.
  bool is_included_in(const Polyhedron& y) const;

  //! Checks if and how \p expr is bounded in \p *this.
  /*!
    Returns <CODE>true</CODE> if and only if \p from_above is
    <CODE>true</CODE> and \p expr is bounded from above in \p *this,
    or \p from_above is <CODE>false</CODE> and \p expr is bounded
    from below in \p *this.

    \param expr
    The linear expression to test;

    \param from_above
    <CODE>true</CODE> if and only if the boundedness of interest is
    "from above".

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.
  */
  bool bounds(const Linear_Expression& expr, bool from_above) const;

  //! Maximizes or minimizes \p expr subject to \p *this.
  /*!
    \param expr
    The linear expression to be maximized or minimized subject to \p
    *this;

    \param maximize
    <CODE>true</CODE> if maximization is what is wanted;

    \param ext_n
    The numerator of the extremum value;

    \param ext_d
    The denominator of the extremum value;

    \param included
    <CODE>true</CODE> if and only if the extremum of \p expr can
    actually be reached in \p * this;

    \param pppoint
    When nonzero and maximization or minimization succeeds, a pointer
    to a point or closure point where \p expr reaches the
    corresponding extremum value will be written at this address.

    \exception std::invalid_argument
    Thrown if \p expr and \p *this are dimension-incompatible.

    If \p *this is empty or \p expr is not bounded in the appropriate
    direction, <CODE>false</CODE> is returned and \p ext_n, \p ext_d,
    \p included and \p *pppoint are left untouched.
  */
  bool max_min(const Linear_Expression& expr,
	       const bool maximize,
	       Integer& ext_n, Integer& ext_d, bool& included,
	       const Generator** const pppoint = 0) const;

  //! \name Widening- and Extrapolation-Related Functions
  //@{

  //! \brief
  //! Copies to \p cs_selection the constraints of `y' corresponding
  //! to the definition of the CH78-widening of \p *this and \p y.
  void select_CH78_constraints(const Polyhedron& y,
			       Constraint_System& cs_selected) const;

  //! \brief
  //! Splits the constraints of `x' into two subsets, depending on whether
  //! or not they are selected to compute the \ref H79_widening "H79-widening"
  //! of \p *this and \p y.
  void select_H79_constraints(const Polyhedron& y,
			      Constraint_System& cs_selected,
			      Constraint_System& cs_not_selected) const;

  friend class Parma_Polyhedra_Library::BHRZ03_Certificate;
  friend class Parma_Polyhedra_Library::H79_Certificate;

  bool BHRZ03_combining_constraints(const Polyhedron& y,
				    const BHRZ03_Certificate& y_cert,
 				    const Polyhedron& H79,
				    const Constraint_System& x_minus_H79_con_sys);

  bool BHRZ03_evolving_points(const Polyhedron& y,
			      const BHRZ03_Certificate& y_cert,
			      const Polyhedron& H79);

  bool BHRZ03_evolving_rays(const Polyhedron& y,
			    const BHRZ03_Certificate& y_cert,
			    const Polyhedron& H79);

  //@} // Widening- and Extrapolation-Related Functions

  //! Adds the low-level constraints to the constraint system.
  static void add_low_level_constraints(Constraint_System& cs);

  //! Adds new space dimensions to the given matrices.
  /*!
    \param mat1
    The matrix to which columns are added;

    \param mat2
    The matrix to which rows and columns are added;

    \param sat1
    The saturation matrix whose columns are indexed by the rows of
    matrix \p mat1. On entry it is up-to-date;

    \param sat2
    The saturation matrix whose columns are indexed by the rows of \p
    mat2;

    \param add_dim
    The number of space dimensions to add.

    Adds new space dimensions to the vector space modifying the matrices.
    This function is invoked only by
    <CODE>add_space_dimensions_and_embed()</CODE> and
    <CODE>add_space_dimensions_and_project()</CODE>, passing the matrix of
    constraints and that of generators (and the corresponding saturation
    matrices) in different order (see those methods for details).
  */
  static void add_space_dimensions(Linear_System& mat1,
				   Linear_System& mat2,
				   Saturation_Matrix& sat1,
				   Saturation_Matrix& sat2,
				   dimension_type add_dim);

  //! \name Minimization-Related Static Member Functions
  //@{

  //! Builds and simplifies constraints from generators (or vice versa).
  // Detailed Doxygen comment to be found in file minimize.cc.
  static bool minimize(bool con_to_gen,
		       Linear_System& source, Linear_System& dest, Saturation_Matrix& sat);

  //! \brief
  //! Adds given constraints and builds minimized corresponding generators
  //! or vice versa.
  // Detailed Doxygen comment to be found in file minimize.cc.
  static bool add_and_minimize(bool con_to_gen,
			       Linear_System& source1,
			       Linear_System& dest,
			       Saturation_Matrix& sat,
			       const Linear_System& source2);

  //! \brief
  //! Adds given constraints and builds minimized corresponding generators
  //! or vice versa. The given constraints are in \p source.
  // Detailed Doxygen comment to be found in file minimize.cc.
  static bool add_and_minimize(bool con_to_gen,
			       Linear_System& source,
			       Linear_System& dest,
			       Saturation_Matrix& sat);

  //! \brief
  //! Performs the conversion from constraints to generators and vice versa.
  // Detailed Doxygen comment to be found in file conversion.cc.
  static dimension_type conversion(Linear_System& source,
				   dimension_type start,
				   Linear_System& dest,
				   Saturation_Matrix& sat,
				   dimension_type num_lines_or_equalities);

  //! \brief
  //! Uses Gauss' elimination method to simplify the result of
  //! <CODE>conversion()</CODE>.
  // Detailed Doxygen comment to be found in file simplify.cc.
  static int simplify(Linear_System& mat, Saturation_Matrix& sat);

  //@} // Minimization-Related Static Member Functions

  //! \name Exception Throwers
  //@{
protected:
  void throw_runtime_error(const char* method) const;
  void throw_invalid_argument(const char* method, const char* reason) const;

  void throw_topology_incompatible(const char* method,
				   const char* ph_name,
				   const Polyhedron& ph) const;
  void throw_topology_incompatible(const char* method,
				   const char* c_name,
				   const Constraint& c) const;
  void throw_topology_incompatible(const char* method,
				   const char* g_name,
				   const Generator& g) const;
  void throw_topology_incompatible(const char* method,
				   const char* cs_name,
				   const Constraint_System& cs) const;
  void throw_topology_incompatible(const char* method,
				   const char* gs_name,
				   const Generator_System& gs) const;

  void throw_dimension_incompatible(const char* method,
				    const char* other_name,
				    dimension_type other_dim) const;
  void throw_dimension_incompatible(const char* method,
				    const char* ph_name,
				    const Polyhedron& ph) const;
  void throw_dimension_incompatible(const char* method,
				    const char* e_name,
				    const Linear_Expression& e) const;
  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Constraint& c) const;
  void throw_dimension_incompatible(const char* method,
				    const char* g_name,
				    const Generator& g) const;
  void throw_dimension_incompatible(const char* method,
				    const char* cs_name,
				    const Constraint_System& cs) const;
  void throw_dimension_incompatible(const char* method,
				    const char* gs_name,
				    const Generator_System& gs) const;
  void throw_dimension_incompatible(const char* method,
				    const char* var_name,
				    const Variable var) const;
  void throw_dimension_incompatible(const char* method,
				    dimension_type required_space_dim) const;

  // Note: it has to be a static method, because it can be called inside
  // constructors (before actually constructing the polyhedron object).
  static void throw_space_dimension_overflow(Topology topol,
					     const char* method,
					     const char* reason);

  void throw_invalid_generator(const char* method,
			       const char* g_name) const;
  void throw_invalid_generators(const char* method,
				const char* gs_name) const;
  //@} // Exception Throwers

};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polyhedron */
void swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y);

} // namespace std

#include "Ph_Status.inlines.hh"
#include "Polyhedron.inlines.hh"

#endif // !defined(PPL_Polyhedron_defs_hh)
