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

#ifndef _Polyhedron_defs_hh
#define _Polyhedron_defs_hh 1

#include "Polyhedron.types.hh"
#include "PolyBase.defs.hh"
/*
#include "Variable.defs.hh"
#include "LinExpression.defs.hh"
#include "ConSys.defs.hh"
#include "GenSys.defs.hh"
#include "SatMatrix.defs.hh"
#include "Status.defs.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <set>
*/
//! A convex polyhedron.
/*!
    An object of the class Polyhedron represents a convex polyhedron
    in the vector space \f$\Rset^n\f$.

    The dimension \f$n \in \Nset\f$ of the enclosing vector space
    is a key attribute of the polyhedron:
    - all polyhedra, the empty ones included, are endowed with
      a specific space dimension;
    - most operations working on a polyhedron and another object
      (i.e., another polyhedron, a constraint or generator,
      a set of variables, etc.) will throw an exception if
      the polyhedron and the object are dimension-incompatible
      (see the dimension-compatibility rules in the Introduction);
    - the only ways to change the space dimension of a polyhedron are:
      - <EM>explicit</EM> calls to operators provided for that purpose;
      - standard copy, assignment and swap operators.

    Note that two polyhedra can be defined on the zero-dimension space:
    the empty polyhedron and the universe polyhedron \f$R^0\f$.

    A polyhedron can be specified as either a finite system of constraints
    or a finite system of generators
    (see Minkowski's theorem in the Introduction)
    and it is always possible to obtain either representation.
    That is, if we know the system of constraints, we can obtain
    from this the system of generators that define the same polyhedron
    and vice versa.
    These systems can contain redundant members: in this case we say
    that they are not in the minimal form.

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
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from a system of generators specifying
    the four vertices of the square:
    \code
  GenSys gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + 3*y));
  gs.insert(point(3*x + 0*y));
  gs.insert(point(3*x + 3*y));
  Polyhedron ph(gs);
    \endcode

    \par Example 2
    The following code builds an unbounded polyhedron
    corresponding to a half-strip in \f$\Rset^2\f$,
    given as a system of constraints:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the system of generators specifying
    the two vertices of the polyhedron and one ray:
    \code
  GenSys gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + y));
  gs.insert(ray(x - y));
  Polyhedron ph(gs);
    \endcode

    \par Example 3
    The following code builds the polyhedron corresponding to
    an half-plane by adding a single constraint
    to the universe polyhedron in \f$\Rset^2\f$:
    \code
  Polyhedron ph(2);
  ph.insert(y >= 0);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the empty polyhedron in the space \f$\Rset^2\f$
    and inserting the appropriate generators
    (a point, a ray and a line).
    \code
  Polyhedron ph(2, Polyhedron::EMPTY);
  ph.insert(point(0*x + 0*y));
  ph.insert(ray(y));
  ph.insert(line(x));
    \endcode
    Note that, even if the above polyhedron has no vertices, we must add
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
  ph.insert(x == 2);
  ph.add_dimensions_and_embed(1);
    \endcode
    We build the universe polyhedron in the 1-dimension space \f$\Rset\f$.
    Then we add a single equality constraint,
    thus obtaining the polyhedron corresponding to the singleton set
    \f$\{ 2 \} \sseq \Rset\f$.
    After the last line of code, the resulting polyhedron is
    \f[
      \bigl\{\,
        (2, x_1)^\transpose \in \Rset^2
      \bigm|
        x_1 \in \Rset
      \,\bigr\}.
    \f]

    \par Example 5
    The following code shows the use of the function
    <CODE>add_dimensions_and_project</CODE>:
    \code
  Polyhedron ph(1);
  ph.insert(x == 2);
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
  ph.insert(point(0*x + 0*y));
  ph.insert(point(0*x + 3*y));
  ph.insert(point(3*x + 0*y));
  ph.insert(point(3*x + 3*y));
  LinExpression coeff = x + 4;
  ph.affine_image(x, coeff);
    \endcode
    In this example the starting polyhedron is a square in \f$\Rset^2\f$,
    the considered variable is \f$x\f$ and the affine expression is \f$x+4\f$.
    The resulting polyhedron is the same square translated towards right.
    Moreover, if the affine transformation for the same variable \p x
    is \f$x+y\f$:
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
  ph.insert(x >= 0);
  ph.insert(x <= 3);
  ph.insert(y >= 0);
  ph.insert(y <= 3);
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
  gs.insert(point(3*x + y +0*z + 2*w));
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
    For the same reason, the operator\p remove_dimensions
    is not idempotent: removing twice the same set of dimensions
    is never a no-op.
*/

class Parma_Polyhedra_Library::Polyhedron : public PolyBase {
public:

  //! Ordinary copy-constructor.
  Polyhedron(const Polyhedron& y);

  //! Builds either the universe or the empty polyhedron of dimension
  //! \p num_dimensions. Both parameters are optional:
  //! by default, a 0-dimension space universe polyhedron is built.
  explicit Polyhedron(size_t num_dimensions = 0,
		      Degenerate_Kind kind = UNIVERSE);

  //! Builds a polyhedron from a system of constraints.
  //! The polyhedron inherits the space dimension of the constraint system.
  //! \param cs       The system of constraints defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  Polyhedron(ConSys& cs);

  //! Builds a polyhedron from a system of generators.
  //! The polyhedron inherits the space dimension of the generator system.
  //! \param gs       The system of generators defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if the system of generators
  //!                                  is not empty but has no points.
  Polyhedron(GenSys& gs);

  //! The assignment operator.
  //! (Note that \p *this and \p y can be dimension-incompatible.)
  Polyhedron& operator=(const Polyhedron& y);

  // Destructor
  ~Polyhedron();

};

#include "Polyhedron.inlines.hh"

#endif
