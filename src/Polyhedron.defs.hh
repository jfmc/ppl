/* Polyhedron class declaration.
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

#ifndef PPL_Polyhedron_defs_hh
#define PPL_Polyhedron_defs_hh 1

#include "Polyhedron.types.hh"
#include "globals.hh"
#include "Variable.defs.hh"
#include "LinExpression.defs.hh"
#include "ConSys.defs.hh"
#include "ConSys.inlines.hh"
#include "GenSys.defs.hh"
#include "GenSys.inlines.hh"
#include "SatMatrix.defs.hh"
#include "Status.defs.hh"
#include "Generator.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <set>

namespace Parma_Polyhedra_Library {

//! Output operator.
/*!
  \relates Polyhedron
  Writes a textual representation of \p ph on \p s:
  <CODE>false</CODE> is written if \p ph is an empty polyhedron;
  <CODE>true</CODE> is written if \p ph is a universe polyhedron;
  a minimized system of constraints defining \p ph is written otherwise,
  all constraints in one row separated by ", ".
*/
std::ostream&
operator<<(std::ostream& s, const Polyhedron& ph);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are the same polyhedron.
/*!
  \relates Polyhedron
  \exception std::invalid_argument thrown if \p x and \p y
  are topology-incompatible
  or dimension-incompatible.
*/
bool operator==(const Polyhedron& x, const Polyhedron& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y are different polyhedra.
/*!
  \relates Polyhedron
  \exception std::invalid_argument thrown if \p x and \p y
  are topology-incompatible
  or dimension-incompatible.
*/
bool operator!=(const Polyhedron& x, const Polyhedron& y);

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x is strictly contained in \p y.
/*!
  \relates Polyhedron
  \exception std::invalid_argument thrown if \p x and \p y
                                   are topology-incompatible
                                   or dimension-incompatible.
*/
bool operator<(const Polyhedron& x, const Polyhedron& y);

//! Returns <CODE>true</CODE> if and only if \p x strictly contains \p y.
/*!
  \relates Polyhedron
  \exception std::invalid_argument thrown if \p x and \p y
                                   are topology-incompatible
                                   or dimension-incompatible.
*/
bool operator>(const Polyhedron& x, const Polyhedron& y);

//! Returns <CODE>true</CODE> if and only if \p x contains \p y.
/*!
  \relates Polyhedron
  \exception std::invalid_argument thrown if \p x and \p y
                                   are topology-incompatible
                                   or dimension-incompatible.
*/
bool operator>=(const Polyhedron& x, const Polyhedron& y);

// Put it in the namespace here to declare it friend later.
bool operator<=(const Polyhedron& x, const Polyhedron& y);

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
    one of these, denoted <CODE><operator-name>_and_minimize</CODE>,
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
  ConSys cs;
  cs.add_constraint(x >= 0);
  cs.add_constraint(x <= 3);
  cs.add_constraint(y >= 0);
  cs.add_constraint(y <= 3);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from a system of generators specifying
    the four vertices of the square:
    \code
  GenSys gs;
  gs.add_generator(point(0*x + 0*y));
  gs.add_generator(point(0*x + 3*y));
  gs.add_generator(point(3*x + 0*y));
  gs.add_generator(point(3*x + 3*y));
  Polyhedron ph(gs);
    \endcode

    \par Example 2
    The following code builds an unbounded polyhedron
    corresponding to a half-strip in \f$\Rset^2\f$,
    given as a system of constraints:
    \code
  ConSys cs;
  cs.add_constraint(x >= 0);
  cs.add_constraint(x - y <= 0);
  cs.add_constraint(x - y + 1 >= 0);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the system of generators specifying
    the two vertices of the polyhedron and one ray:
    \code
  GenSys gs;
  gs.add_generator(point(0*x + 0*y));
  gs.add_generator(point(0*x + y));
  gs.add_generator(ray(x - y));
  Polyhedron ph(gs);
    \endcode

    \par Example 3
    The following code builds the polyhedron corresponding to
    an half-plane by adding a single constraint
    to the universe polyhedron in \f$\Rset^2\f$:
    \code
  Polyhedron ph(2);
  ph.add_constraint(y >= 0);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the empty polyhedron in the space \f$\Rset^2\f$
    and inserting the appropriate generators
    (a point, a ray and a line).
    \code
  Polyhedron ph(2, Polyhedron::EMPTY);
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
    <CODE>add_dimensions_and_embed</CODE>:
    \code
  Polyhedron ph(1);
  ph.add_constraint(x == 2);
  ph.add_dimensions_and_embed(1);
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
    <CODE>add_dimensions_and_project</CODE>:
    \code
  Polyhedron ph(1);
  ph.add_constraint(x == 2);
  ph.add_dimensions_and_project(1);
    \endcode
    The first two lines of code are the same as in Example 4 for
    <CODE>add_dimensions_and_embed</CODE>.
    After the last line of code, the resulting polyhedron is
    the singleton set
    \f$\bigl\{ (2, 0)^\transpose \bigr\} \sseq \Rset^2\f$.

    \par Example 6
    The following code shows the use of the function
    <CODE>affine_image</CODE>:
    \code
  Polyhedron ph(2, Polyhedron::EMPTY);
  ph.add_generator(point(0*x + 0*y));
  ph.add_generator(point(0*x + 3*y));
  ph.add_generator(point(3*x + 0*y));
  ph.add_generator(point(3*x + 3*y));
  LinExpression coeff = x + 4;
  ph.affine_image(x, coeff);
    \endcode
    In this example the starting polyhedron is a square in
    \f$\Rset^2\f$, the considered variable is \f$x\f$ and the affine
    expression is \f$x+4\f$.  The resulting polyhedron is the same
    square translated towards right.  Moreover, if the affine
    transformation for the same variable \p x is \f$x+y\f$:
    \code
  LinExpression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x-y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable; for example, the affine expression \f$y\f$:
    \code
  LinExpression coeff = y;
    \endcode
    the resulting polyhedron is a diagonal of the square.

    \par Example 7
    The following code shows the use of the function
    <CODE>affine_preimage</CODE>:
    \code
  Polyhedron ph(2);
  ph.add_constraint(x >= 0);
  ph.add_constraint(x <= 3);
  ph.add_constraint(y >= 0);
  ph.add_constraint(y <= 3);
  LinExpression coeff = x + 4;
  ph.affine_preimage(x, coeff);
    \endcode
    In this example the starting polyhedron, \p var and the affine
    expression and the denominator are the same as in Example 6,
    while the resulting polyhedron is again the same square,
    but translated towards left.
    Moreover, if the affine transformation for \p x is \f$x+y\f$
    \code
  LinExpression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x+y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable \p x, for example, the affine expression \f$y\f$:
    \code
  LinExpression coeff = y;
    \endcode
    the resulting polyhedron is a line that corresponds to the \f$y\f$ axis.

    \par Example 8
    For this example we use also the variables:
    \code
  Variable z(2);
  Variable w(3);
    \endcode
    The following code shows the use of the function
    <CODE>remove_dimensions</CODE>:
    \code
  GenSys gs;
  gs.add_generator(point(3*x + y +0*z + 2*w));
  Polyhedron ph(gs);
  set<Variable> to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  ph.remove_dimensions(to_be_removed);
    \endcode
    The starting polyhedron is the singleton set
    \f$\bigl\{ (3, 1, 0, 2)^\transpose \bigr\} \sseq \Rset^4\f$, while
    the resulting polyhedron is
    \f$\bigl\{ (3, 2)^\transpose \bigr\} \sseq \Rset^2\f$.
    Be careful when removing dimensions <EM>incrementally</EM>:
    since dimensions are automatically renamed after each application
    of the <CODE>remove_dimensions</CODE> operator, unexpected results
    can be obtained.
    For instance, by using the following code we would obtain
    a different result:
    \code
  set<Variable> to_be_removed1;
  to_be_removed1.insert(y);
  ph.remove_dimensions(to_be_removed1);
  set<Variable> to_be_removed2;
  to_be_removed2.insert(z);
  ph.remove_dimensions(to_be_removed2);
    \endcode
    In this case, the result is the polyhedron
    \f$\bigl\{(3, 0)^\transpose \bigr\} \sseq \Rset^2\f$:
    when removing the set of dimensions \p to_be_removed2
    we are actually removing variable \f$w\f$ of the original polyhedron.
    For the same reason, the operator \p remove_dimensions
    is not idempotent: removing twice the same set of dimensions
    is never a no-op.
*/

class Parma_Polyhedra_Library::Polyhedron {
public:
  //! Kinds of degenerate polyhedra.
  enum Degenerate_Kind {
    //! The universe polyhedron, i.e., the whole vector space.
    UNIVERSE,
    //! The empty polyhedron, i.e., the empty set.
    EMPTY
  };

protected:
  //! Ordinary copy-constructor.
  Polyhedron(const Polyhedron& y);

  //! \brief
  //! Builds a polyhedron having the specified properties.
  /*!
    \param topol          The topology of the polyhedron;
    \param num_dimensions The number of dimensions of the vector space
                          enclosing the polyhedron;
    \param kind           Specifies whether the universe or the empty
                          polyhedron has to be built.
  */
  Polyhedron(Topology topol,
	     dimension_type num_dimensions,
	     Degenerate_Kind kind);

  //! Builds a polyhedron from a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.
    \param topol    The topology of the polyhedron;
    \param cs       The system of constraints defining the polyhedron.
    \exception std::invalid_argument thrown if the topology of \p cs
                                     is incompatible with \p topology.
  */
  Polyhedron(Topology topol, const ConSys& cs);

  //! Builds a polyhedron recycling a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.
    \param topol    The topology of the polyhedron;
    \param cs       The system of constraints defining the polyhedron.
                    It is not declared <CODE>const</CODE> because its
                    data-structures will be recycled to build the polyhedron.
    \exception std::invalid_argument thrown if the topology of \p cs
                                     is incompatible with \p topology.
  */
  Polyhedron(Topology topol, ConSys& cs);

  //! Builds a polyhedron from a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.
    \param topol    The topology of the polyhedron;
    \param gs       The system of generators defining the polyhedron.
    \exception std::invalid_argument thrown if if the topology of \p gs
                                     is incompatible with \p topol,
                                     or if the system of generators
                                     is not empty but has no points.
  */
  Polyhedron(Topology topol, const GenSys& gs);

  //! Builds a polyhedron recycling a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.
    \param topol    The topology of the polyhedron;
    \param gs       The system of generators defining the polyhedron.
                    It is not declared <CODE>const</CODE> because its
                    data-structures will be recycled to build the polyhedron.
    \exception std::invalid_argument thrown if if the topology of \p gs
                                     is incompatible with \p topol,
                                     or if the system of generators
                                     is not empty but has no points.
  */
  Polyhedron(Topology topol, GenSys& gs);

  //! Builds a polyhedron out of a generic, interval-based bounding box.
  /*!
    \param topol    The topology of the polyhedron;
    \param box      The bounding box representing the polyhedron
                    to be built.
    \exception std::invalid_argument thrown if \p box has intervals that
                                     are incompatible with \p topol.

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
    dimension.  If \f$I\f$ is not bounded from below, simply return
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
    dimension.  If \f$I\f$ is not bounded from above, simply return
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
  //! Destructor.
  ~Polyhedron();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y,
  //! minimizing the result.
  /*!
    \return
      <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y.
  //! The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void intersection_assign(const Polyhedron& y);

  //! \brief
  //! Seeing a polyhedron as a set of tuples (its points), assigns
  //! to \p *this all the tuples that can be obtained by concatenating,
  //! in the order given, a tuple of \p *this with a tuple of \p y.
  /*!
    Let \f$P \sseq \Rset^n\f$ and \f$Q \sseq \Rset^m\f$ be the polyhedra
    represented, on entry, by \p *this and \p y, respectively.
    Upon successful completion, \p *this will represent the polyhedron
    \f$R \sseq \Rset^{n+m}\f$ such that
    \f[
      R
        \defeq
          \Bigl\{\,
            (x_1, \ldots, x_n, y_1, \ldots, y_m)^\transpose
          \Bigm|
            (x_1, \ldots, x_n)^\transpose \in P,
            (y_1, \ldots, y_m)^\transpose \in Q
          \,\Bigl\}.
    \f]
    Another way of seeing it is as follows: first increases the space
    dimension of \p *this by adding \p y.space_dimension() new
    dimensions; then adds to the system of constraints of \p *this a
    renamed-apart version of the constraints of \p y.

    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible.
  */
  void concatenate_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the poly-hull of \p *this and \p y,
  //! minimizing the result.
  /*!
    \return       <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  bool poly_hull_assign_and_minimize(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the poly-hull \p *this and \p y.
  //! The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void poly_hull_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the \ref poly_difference "poly-difference" of
  //! \p *this and \p y, minimizing the result.
  /*!
    \return       <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  bool poly_difference_assign_and_minimize(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the \ref poly_difference "poly-difference" of
  //! \p *this and \p y. The result is not guaranteed to be minimized.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void poly_difference_assign(const Polyhedron& y);

  //! \brief
  //! Returns the relations holding between the polyhedron \p *this
  //! and the constraint \p c.
  /*!
    \exception std::invalid_argument thrown if \p *this and constraint
                                     \p c are dimension-incompatible.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! \brief
  //! Returns the relations holding between the polyhedron \p *this
  //! and the generator \p g.
  /*!
    \exception std::invalid_argument thrown if \p *this and generator
                                     \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

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
  void H79_widening_assign(const Polyhedron& y);

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
  void limited_H79_widening_assign(const Polyhedron& y, ConSys& cs);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref BBRZ02_widening "BBRZ02-widening" between \p *this and \p y.
  /*!
    \param y           A polyhedron that <EM>must</EM>
                       be contained in \p *this.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void BBRZ02_widening_assign(const Polyhedron& y);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref time_elapse "time-elapse" between \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void time_elapse_assign(const Polyhedron& y);


  //! Returns the system of constraints.
  const ConSys& constraints() const;

  //! Returns the system of constraints, with no redundant constraint.
  const ConSys& minimized_constraints() const;

  //! Returns the system of generators.
  const GenSys& generators() const;

  //! Returns the system of generators, with no redundant generator.
  const GenSys& minimized_generators() const;

  //! \brief
  //! Adds a copy of constraint \p c to the system of constraints
  //! of \p *this.
  /*!
    \exception std::invalid_argument thrown if \p *this and constraint \p c
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! \brief
  //! Adds a copy of generator \p g to the system of generators
  //! of \p *this.
  /*!
    \exception std::invalid_argument thrown if \p *this and generator \p g
                                     are topology-incompatible
                                     or dimension-incompatible,
                                     or if \p *this is an empty polyhedron
                                     and \p g is not a point.
  */
  void add_generator(const Generator& g);

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine image"
  //! of \p *this under the function mapping variable \p v into the
  //! affine expression specified by \p expr and \p d.
  /*!
    \param var           The variable to which the affine
                         expression is assigned.
    \param expr          The numerator of the affine expression.
    \param denominator   The denominator of the affine expression
                         (optional argument with default value 1.)
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this.
  */
  void affine_image(const Variable& var,
		    const LinExpression& expr,
		    const Integer& denominator = Integer_one());

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine preimage"
  //! of \p *this under the function mapping variable \p v into the
  //! affine expression specified by \p expr and \p d.
  /*!
    \param var           The variable to which the affine expression
                         is substituted.
    \param expr          The numerator of the affine expression.
    \param denominator   The denominator of the affine expression
                         (optional argument with default value 1.)
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this.
  */
  void affine_preimage(const Variable& var,
		       const LinExpression& expr,
		       const Integer& denominator = Integer_one());

  //! \brief
  //! Assigns to \p *this the image of \p *this with respect to
  //! the \ref generalized_affine_transformation generalized affine
  //! transfer function \f$\mathit{var}' \relop \frac{expr}{d}\f$,
  //! where \f$\mathord{\relop}\f$ is the relation operator encoded by
  //! \p relation.
  /*!
    \param var           The variable being compared to the result
                         of the computed affine image.
    \param relation      The relation operator: can be either one of
                         <CODE>"<"</CODE>, <CODE>"<="</CODE>,
			 <CODE>"=="</CODE>, <CODE>">="</CODE>,
			 or <CODE>">"</CODE>.
    \param expr          The numerator of the affine expression.
    \param denominator   The denominator of the affine expression
                         (optional argument with default value 1.)
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this
				     or if \p *this is a C_Polyhedron and
				     \p relation encodes a strict relation
				     operator.
  */
  void generalized_affine_image(const Variable& var,
				const char* relation,
				const LinExpression& expr,
				const Integer& denominator = Integer_one());

  //! Use \p *this to shrink a generic, interval-based bounding box.
  /*!
    \param box    The bounding box to be shrunk.

    The template class Box must provide the following
    methods, whose return value, if any, is simply ignored.
    \code
      set_empty()
    \endcode
    causes the box to become empty, i.e., to represent the empty set.
    \code
      raise_lower_bound(dimension_type k, bool closed,
                        const Integer& n, const Integer& d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th dimension
    with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>false</CODE>.
    The fraction \f$n/d\f$ is in canonical form, that is, \f$n\f$
    and \f$d\f$ have no common factors and \f$d\f$ is positive, \f$0/1\f$
    being the unique representation for zero.
    \code
      lower_upper_bound(dimension_type k, bool closed,
                        const Integer& n, const Integer& d)
    \endcode
    intersects the interval corresponding to the <CODE>k</CODE>-th dimension
    with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE>
    is <CODE>false</CODE>.
    The fraction \f$n/d\f$ is in canonical form.
  */
  template <typename Box>
  void shrink_bounding_box(Box& box) const;

  //! Checks if all the invariants are satisfied.
  /*!
    \param check_not_empty
      <CODE>true</CODE> if and only if, in addition to checking
      the invariants, \p *this must be checked to be not empty.
   
    \return
      <CODE>true</CODE> if and only if \p *this satisfies
      all the invariants and either \p check_not_empty is
      <CODE>false</CODE> or \p *this is not empty.

    The check is performed so as to intrude as little as possible.
    In case invariants are violated error messages are written on
    <CODE>std::cerr</CODE>. This is useful for the purpose of debugging
    the library.
  */
  bool OK(bool check_not_empty = false) const;

  //! \brief
  //! Adds \p m new dimensions and embeds the old polyhedron
  //! into the new space.
  /*!
    \param m      The number of dimensions to add.

    The new dimensions will be those having the highest indexes
    in the new polyhedron, which is characterized by a system
    of constraints in which the variables running through
    the new dimensions are not constrained.
    For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
    and adding a third dimension, the result will be the polyhedron
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cP
      \,\bigr\}.
    \f]
  */
  void add_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new dimensions to the polyhedron
  //! and does not embed it in the new space.
  /*!
    \param m      The number of dimensions to add.

    The new dimensions will be those having the highest indexes
    in the new polyhedron, which is characterized by a system
    of constraints in which the variables running through
    the new dimensions are all constrained to be equal to 0.
    For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
    and adding a third dimension, the result will be the polyhedron
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cP
      \,\bigr\}.
    \f]
  */
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

  //! Shuffles the dimensions of a polyhedron
  //! according to a partial function.
  /*!
    \param pfunc    The partial function specifyng the destiny
                    of each dimension.

    The dimensions for which \p pfunc is undefined are projected away.

    The template class PartialFunction must provide the following methods.
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
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$n\f$ be the value of \p i.
    If \f$f\f$ is defined in \f$n\f$, then \f$f(n)\f$ is assigned to \p j
    and <CODE>true</CODE> is returned.
    If \f$f\f$ is undefined in \f$n\f$, then <CODE>false</CODE> is returned.
  */
  template <typename PartialFunction>
  void shuffle_dimensions(const PartialFunction& pfunc);

  //! \brief
  //! Adds the specified constraints and minimizes the result,
  //! which is assigned to \p *this.
  /*!
    \return
      <CODE>false</CODE> if and only if the result is empty.
    \param  cs            The constraints that will be added to the
                          current system of constraints. This parameter
                          is not declared <CODE>const</CODE> because
                          it can be modified.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  bool add_constraints_and_minimize(ConSys& cs);

  //! Adds the specified constraints without minimizing.
  /*!
    \param  cs             The constraints that will be added to the
                           current system of constraints. This parameter
                           is not declared <CODE>const</CODE> because
                           it can be modified.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraints(ConSys& cs);

  //! \brief
  //! Adds the specified generators and minimizes the result,
  //! which is assigned to \p *this.
  /*!
    \return
      <CODE>false</CODE> if and only if the result is empty.
    \param  gs          The generators that will be added to the
                        current system of generators. The parameter is
                        not declared <CODE>const</CODE> because it
                        can be modified.
    \return             <CODE>false</CODE> if the resulting
                        polyhedron is empty.
    \exception std::invalid_argument thrown if \p *this and \p gs
                                     are topology-incompatible
                                     or dimension-incompatible,
                                     or if \p *this is empty and the
                                     the system of generators \p gs
                                     is not empty, but has no points.
  */
  bool add_generators_and_minimize(GenSys& gs);

  //! \brief
  //! Adds the specified generators without minimizing.
  //! \param  gs             The generators that will be added to the
  //!                        current system of generators. This parameter
  //!                        is not declared <CODE>const</CODE> because
  //!                        it can be modified.
  //! \exception std::invalid_argument thrown if \p *this and \p gs
  //!                                  are topology-incompatible
  //!                                  or dimension-incompatible,
  //!                                  or if \p *this is empty and
  //!                                  the system of generators \p gs
  //!                                  is not empty, but has no points.
  void add_generators(GenSys& gs);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is
  //! an empty polyhedron.
  bool check_empty() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a universe polyhedron.
  bool check_universe() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a bounded polyhedron.
  bool is_bounded() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p expr is
  //! bounded from above in \p *this.
  /*!
    \exception std::invalid_argument thrown if \p expr and \p *this
                                     are dimension-incompatible.
  */
  bool bounds_from_above(const LinExpression& expr) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p expr is
  //! bounded from below in \p *this.
  /*!
    \exception std::invalid_argument thrown if \p expr and \p *this
                                     are dimension-incompatible.
  */
  bool bounds_from_below(const LinExpression& expr) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a topologically closed subset of the vector space.
  bool is_topologically_closed() const;

  //! Assigns to \p *this its topological closure.
  void topological_closure_assign();

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! polyhedron \p x is contained in polyhedron \p y.
  /*!
    \exception std::invalid_argument thrown if \p x and \p y
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  friend bool
  Parma_Polyhedra_Library::operator<=(const Polyhedron& x,
				      const Polyhedron& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ASCII_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ASCII_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ASCII_load(std::istream& s);

  //! \brief
  //! Swaps \p *this with polyhedron \p y.
  //! (\p *this and \p y can be dimension-incompatible.)
  /*!
    \exception std::invalid_argument thrown if \p x and \p y
                                     are topology-incompatible.
  */
  void swap(Polyhedron& y);

private:
  //! The system of constraints.
  ConSys con_sys;

  //! The system of generators.
  GenSys gen_sys;

  //! The saturation matrix having constraints on its columns.
  SatMatrix sat_c;

  //! The saturation matrix having generators on its columns.
  SatMatrix sat_g;

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

  /*! @name Private Verifiers
    Verify if individual flags are set.
  */
  //@{
  //! Returns <CODE>true</CODE> if the polyhedron is known to be empty.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-empty.
  */
  bool is_empty() const;

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

  //! \brief
  //! Returns <CODE>true</CODE> if the saturation matrix \p sat_c
  //! is up-to-date.
  bool sat_c_is_up_to_date() const;

  //! \brief
  //! Returns <CODE>true</CODE> if the saturation matrix \p sat_g
  //! is up-to-date.
  bool sat_g_is_up_to_date() const;
  //@}

  /*! @name State flag setters.
    Set only the specified flags.
  */
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

  //! Sets \p status to express that \p sat_c is up-to-date.
  void set_sat_c_up_to_date();

  //! Sets \p status to express that \p sat_g is up-to-date.
  void set_sat_g_up_to_date();
  //@}

  /*! @name State flag cleaners.
    Clear only the specified flag.
  */
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

  //! Sets \p status to express that \p sat_c is no longer up-to-date.
  void clear_sat_c_up_to_date();

  //! Sets \p status to express that \p sat_g is no longer up-to-date.
  void clear_sat_g_up_to_date();
//@}

  //! Updates constraints starting from generators and minimizes them.
  /*!
    The resulting system of constraints is only partially sorted:
    the equalities are in the upper part of the matrix,
    while the inequalities in the lower part.
  */
  void update_constraints() const;

  //! Updates generators starting from constraints and minimizes them.
  /*!
    \return       <CODE>false</CODE> if and only if \p *this turns out
                  to be an empty polyhedron.

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

  //! Applies (weak) minimization to both the constraints and generators.
  /*!
    \return       <CODE>false</CODE> if and only if \p *this turns out
                  to be an empty polyhedron.

    Minimization is not attempted if the Status field already declares
    both systems to be minimized.
  */
  bool minimize() const;

  //! \brief
  //! Applies strong minimization to both the constraints and generators
  //! of an NNC polyhedron.
  /*!
    \return       <CODE>false</CODE> if and only if \p *this turns out
                  to be an empty polyhedron.
  */
  bool strongly_minimize() const;

  //! Applies strong minimization to the constraints of an NNC polyhedron.
  /*!
    \return       <CODE>false</CODE> if and only if \p *this turns out
                  to be an empty polyhedron.
  */
  bool strongly_minimize_constraints() const;

  //! Applies strong minimization to the generators of an NNC polyhedron.
  /*!
    \return       <CODE>false</CODE> if and only if \p *this turns out
                  to be an empty polyhedron.
  */
  bool strongly_minimize_generators() const;

  //! \brief
  //! Checks if and how \p expr is bounded in \p *this.
  /*!
    Returns <CODE>true</CODE> if and only if \p from_above is
    <CODE>true</CODE> and \p expr is bounded from above in \p *this,
    or \p from_above is <CODE>false</CODE> and \p expr is bounded
    from below in \p *this.
    \param expr        The linear expression to test.
    \param from_above  <CODE>true</CODE> if and only if the boundedness
                       of interest is "from above".
    \exception std::invalid_argument thrown if \p expr and \p *this
                                     are dimension-incompatible.
  */
  bool bounds(const LinExpression& expr, bool from_above) const;

  //! Adds new dimensions to the given matrices.
  /*!
    \param mat1      The matrix to which columns are added.
    \param mat2      The matrix to which rows and columns are added.
    \param sat1      The saturation matrix whose columns are indexed by
                     the rows of matrix \p mat1. On entry it is up-to-date.
    \param sat2      The saturation matrix whose columns are indexed by
                     the rows of \p mat2.
    \param add_dim   The number of dimensions to add.

    Adds new dimensions to the polyhedron modifying the matrices.
    This function is invoked only by <CODE>add_dimensions_and_embed()</CODE>
    and <CODE>add_dimensions_and_project()</CODE>, passing the matrix of
    constraints and that of generators (and the corresponding saturation
    matrices) in different order (see those methods for details).
  */
  static void add_dimensions(Matrix& mat1,
                             Matrix& mat2,
                             SatMatrix& sat1,
                             SatMatrix& sat2,
			     dimension_type add_dim);

  //! Performs the conversion from constraints to generators and vice versa.
  // Detailed Doxygen comment to be found in file conversion.cc.
  static dimension_type conversion(Matrix& entry,
			   dimension_type start,
			   Matrix& result,
			   SatMatrix& sat,
			   dimension_type num_lines_or_equalities);

  //! \brief
  //! Uses Gauss' elimination method to simplify the result of
  //! <CODE>conversion()</CODE>.
  // Detailed Doxygen comment to be found in file simplify.cc.
  static int simplify(Matrix& mat, SatMatrix& sat);

  //! Builds and simplifies constraints from generators (or vice versa).
  // Detailed Doxygen comment to be found in file minimize.cc.
  static bool minimize(bool con_to_gen, Matrix& source, Matrix& dest,
		       SatMatrix& sat);

  //! \brief
  //! Adds given constraints and builds minimized corresponding generators
  //! or vice versa.
  // Detailed Doxygen comment to be found in file minimize.cc.
  static bool add_and_minimize(bool con_to_gen,
			       Matrix& source1, Matrix& dest, SatMatrix& sat,
			       const Matrix& source2);
  
  //! Returns <CODE>true</CODE> if the given polyhedra satisfy
  //! the theorem of BBRZ02.
  /*!
    \param x        The greater polyhedron.
    \param y        The smaller polyhedron.
  */
  static bool is_BBRZ02_stabilizing(const Polyhedron& x, const Polyhedron& y);

  /*! @name Exception throwers
    Throw an exception after having formatted the appropriate
    error message.
   */
  //@{
  void throw_topology_incompatible(const char* method,
				   const Polyhedron& y) const;
  void throw_topology_incompatible(const char* method,
				   const ConSys& ) const;
  void throw_topology_incompatible(const char* method,
				   const Constraint& ) const;
  void throw_topology_incompatible(const char* method,
				   const GenSys& ) const;
  void throw_topology_incompatible(const char* method,
				   const Generator& ) const;

  void throw_dimension_incompatible(const char* method,
				    const Polyhedron& y) const;
  void throw_dimension_incompatible(const char* method,
				    const Matrix& y) const;
  void throw_dimension_incompatible(const char* method,
				    const Row& y) const;
  void throw_dimension_incompatible(const char* method,
				    dimension_type required_dim) const;

  void throw_invalid_generator(const char* method) const;
  void throw_invalid_generators(const char* method) const;

  void throw_generic(const char* method, const char* reason) const;

  static void throw_topology_incompatible(const char* method,
					  const Polyhedron& x,
					  const Polyhedron& y);
  static void throw_dimension_incompatible(const char* method,
					   const Polyhedron& x,
					   const Polyhedron& y);
  //@}

};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polyhedron */
void swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y);

} // namespace std

#include "Polyhedron.inlines.hh"

#endif // !defined(PPL_Polyhedron_defs_hh)
