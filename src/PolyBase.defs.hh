/* PolyBase class declaration.
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

#ifndef _PolyBase_defs_hh
#define _PolyBase_defs_hh 1

#include "Variable.defs.hh"
#include "LinExpression.defs.hh"
#include "ConSys.defs.hh"
#include "GenSys.defs.hh"
#include "SatMatrix.defs.hh"
#include "Status.defs.hh"
#include "PolyBase.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <set>

namespace Parma_Polyhedra_Library {
  //! Returns <CODE>true</CODE> if and only if
  //! \p x and \p y are the same polyhedron.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  /*! \relates PolyBase */
  bool operator==(const PolyBase& x, const PolyBase& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x and \p y are different polyhedra.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  /*! \relates PolyBase */
  bool operator!=(const PolyBase& x, const PolyBase& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x is strictly contained in \p y.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  /*! \relates PolyBase */
  bool operator<(const PolyBase& x, const PolyBase& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x strictly contains \p y.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  /*! \relates PolyBase */
  bool operator>(const PolyBase& x, const PolyBase& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x contains \p y.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  /*! \relates PolyBase */
  bool operator>=(const PolyBase& x, const PolyBase& y);

  // Put them in the namespace here to declare them friend later.
  bool operator<=(const PolyBase& x, const PolyBase& y);
  std::ostream& operator<<(std::ostream& s, const PolyBase& p);
  std::istream& operator>>(std::istream& s, PolyBase& p);

} // namespace Parma_Polyhedra_Library


//! The base class for convex polyhedra.
/*!
    An object of the class PolyBase represents a convex polyhedron
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

class Parma_Polyhedra_Library::PolyBase {
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
  PolyBase(const PolyBase& y);
  //! Builds either the universe or the empty polyhedron of dimension
  //! \p num_dimensions, either necessarily closed or not.
  explicit PolyBase(Topology topology,
		    size_t num_dimensions,
		    Degenerate_Kind kind);
  //! Builds a polyhedron from a system of constraints.
  //! The polyhedron inherits the space dimension of the constraint system.
  //! \param topology The topology of the polyhedron;
  //! \param cs       The system of constraints defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if the topology of \p cs
  //!                                  is incompatible with \p topology.
  PolyBase(Topology topology, ConSys& cs);
  //! Builds a polyhedron from a system of generators.
  //! The polyhedron inherits the space dimension of the generator system.
  //! \param topology The topology of the polyhedron;
  //! \param gs       The system of generators defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if if the topology of \p gs
  //!                                  is incompatible with \p topology,
  //!                                  or if the system of generators
  //!                                  is not empty but has no points.
  PolyBase(Topology topology, GenSys& gs);
  // Destructor
  ~PolyBase();

  //! The assignment operator.
  //! (Note that \p *this and \p y can be dimension-incompatible.)
  PolyBase& operator=(const PolyBase& y);

public:
  //! Returns the topological kind of the polyhedron.
  Topology topology() const;
  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

  //! Intersects \p *this with polyhedron \p y and assigns the result
  //! to \p *this.   The result is not guaranteed to be minimized.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void intersection_assign_and_minimize(const PolyBase& y);
  //! Intersects \p *this with polyhedron \p y and assigns the result
  //! to \p *this without minimizing the result.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void intersection_assign(const PolyBase& y);
  //! Assigns to \p *this the convex hull of the set-theoretic union
  //! \p *this and \p y, minimizing the result.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void convex_hull_assign_and_minimize(const PolyBase& y);
  //! Assigns to \p *this the convex hull of the set-theoretic union
  //! \p *this and \p y.  The result is not guaranteed to be minimized.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void convex_hull_assign(const PolyBase& y);
  //! Assigns to \p *this the convex hull of the set-theoretic difference
  //! \p *this and \p y, minimizing the result.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void convex_difference_assign_and_minimize(const PolyBase& y);
  //! Assigns to \p *this the convex hull of the set-theoretic difference
  //! \p *this and \p y.  The result is not guaranteed to be minimized.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void convex_difference_assign(const PolyBase& y);

  //! Returns the relations holding between the polyhedron \p *this
  //! and the constraint \p c.
  //! \exception std::invalid_argument thrown if \p *this and constraint
  //!                                  \p c are dimension-incompatible.
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! Returns the relations holding between the polyhedron \p *this
  //! and the generator \p g.
  //! \exception std::invalid_argument thrown if \p *this and generator
  //!                                  \p g are dimension-incompatible.
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! Computes the widening between \p *this and \p y and
  //! assigns the result to \p *this.
  //! \param y           The polyhedron that <EM>must</EM>
  //!                    be contained in \p *this.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  are dimension-incompatible.
  void widening_assign(const PolyBase& y);
  //! Limits the widening between \p *this and \p y by \p cs
  //! and assigns the result to \p *this.
  //! \param y                 The polyhedron that <EM>must</EM>
  //!                          be contained in \p *this.
  //! \param cs                The system of constraints that limits
  //!                          the widened polyhedron. It is not
  //!                          declared <CODE>const</CODE>
  //!                          because it can be modified.
  //! \exception std::invalid_argument thrown if \p *this, \p y and
  //!                                  \p cs are dimension-incompatible
  //!                                  or topology-incompatible.
  void limited_widening_assign(const PolyBase& y, ConSys& cs);

  //! Returns the system of constraints.
  const ConSys& constraints() const;
  //! Returns the system of generators.
  const GenSys& generators() const;

  //! Inserts a copy of constraint \p c into the system of constraints
  //! of \p *this.
  //! \exception std::invalid_argument thrown if \p *this and constraint \p c
  //!                                  are dimension-incompatible
  //!                                  or topology-incompatible.
  void insert(const Constraint& c);

  //! Inserts a copy of generator \p g into the system of generators
  //! of \p *this.
  //! \exception std::invalid_argument thrown if \p *this and generator \p g
  //!                                  are dimension-incompatible
  //!                                  or topology-incompatible,
  //!                                  or if \p *this is an empty polyhedron
  //!                                  and \p g is not a point.
  void insert(const Generator& g);

  //! Transforms the polyhedron \p *this, assigning an affine expression
  //! to the specified variable.
  //! \param var           The variable to which the affine
  //!                      expression is assigned.
  //! \param expr          The numerator of the affine expression.
  //! \param denominator   The denominator of the affine expression
  //!                      (optional argument with default value 1.)
  //! \exception std::invalid_argument thrown if \p denominator is zero
  //!                                  or if \p expr and \p *this
  //!                                  are dimension-incompatible
  //!                                  or if \p var is not a dimension
  //!                                  of \p *this.
  void affine_image(const Variable& var,
		    const LinExpression& expr,
		    const Integer& denominator = Integer_one());
  //! Transforms the polyhedrons \p *this, substituting an affine
  //! expression for the specified variable. (It is the inverse
  //! operation of <CODE>affine_image</CODE>.)
  //! \param var           The variable to which the affine expression
  //!                      is substituted.
  //! \param expr          The numerator of the affine expression.
  //! \param denominator   The denominator of the affine expression
  //!                      (optional argument with default value 1.)
  //! \exception std::invalid_argument thrown if \p denominator is zero
  //!                                  or if \p expr and \p *this
  //!                                  are dimension-incompatible
  //!                                  or if \p var is not a dimension
  //!                                  of \p *this.
  void affine_preimage(const Variable& var,
		       const LinExpression& expr,
		       const Integer& denominator = Integer_one());

  //! Checks if all the invariants are satisfied, doing so in
  //! as non-intrusively as possible.  In case invariants are
  //! violated error messages are written on <CODE>std::cerr</CODE>.
  //! This is useful for the purpose of debugging the library.
  //!
  //! \param check_not_empty
  //!   <CODE>true</CODE> if and only if, in addition to checking
  //!   the invariants, \p *this must be checked to be not empty.
  //!
  //! \return
  //!   <CODE>true</CODE> if and only if \p *this satisfies
  //!   all the invariants and either \p check_not_empty is
  //!   <CODE>false</CODE> or \p *this is not empty.
  bool OK(bool check_not_empty = false) const;

protected:
  //! Minimizes generators and constraints.
  void minimize() const;
  //! Updates constraints starting from generators and minimizes them.
  void update_constraints() const;
  //! Updates generators starting from constraints and minimizes them.
  bool update_generators() const;
  //! Updates \p sat_c using the updated constraints and generators.
  void update_sat_c() const;
  //! Updates \p sat_g using the updated constraints and generators.
  void update_sat_g() const;
  //! Sorts the matrix of constraints keeping status consistency.
  void obtain_sorted_constraints() const;
  //! Sorts the matrix of generators keeping status consistency.
  void obtain_sorted_generators() const;
  //! Sorts the matrix of constraints and updates \p sat_c.
  void obtain_sorted_constraints_with_sat_c() const;
  //! Sorts the matrix of generators and updates \p sat_g.
  void obtain_sorted_generators_with_sat_g() const;

public:
  //! Applies strong minimization to generators and constraints
  //! of an NNC polyhedron.
  void NNC_minimize() const;

public:
  //! Adds new dimensions and embeds the old polyhedron into the new space.
  //! \param dim      The number of dimensions to add.
  void add_dimensions_and_embed(size_t dim);
  //! Adds new dimensions to the polyhedron
  //! and does not embed it in the new space.
  //! \param dim      The number of dimensions to add.
  void add_dimensions_and_project(size_t dim);
  //! Removes the specified dimensions.
  //! \param to_be_removed The set of variables to remove.
  void remove_dimensions(const std::set<Variable>& to_be_removed);
  //! Removes all dimensions higher than a threshold.
  //! \param new_dimension The dimension of the resulting polyhedron
  //!                      after all higher dimensions have been removed.
  void remove_higher_dimensions(size_t new_dimension);
  //! Adds the specified constraints and computes a new polyhedron.
  //! \param  cs            The constraints that will be added to the
  //!                       current system of constraints. This parameter
  //!                       is not declared <CODE>const</CODE> because
  //!                       it can be modified.
  //! \return               <CODE>false</CODE> if the resulting
  //!                       polyhedron is empty.
  //! \exception std::invalid_argument thrown if \p *this and \p cs
  //!                                  are dimension-incompatible or
  //!                                  topology-incompatible.
  bool add_constraints_and_minimize(ConSys& cs);
  //! Adds the specified constraints without minimizing.
  //! \param  cs             The constraints that will be added to the
  //!                        current system of constraints. This parameter
  //!                        is not declared <CODE>const</CODE> because
  //!                        it can be modified.
  //! \exception std::invalid_argument thrown if \p *this and \p cs
  //!                                  are dimension-incompatible or
  //!                                  topology-incompatible.
  void add_constraints(ConSys& cs);

  //! First increases the space dimension of \p *this by adding
  //! \p cs.space_dimension() new dimensions;
  //! then adds to the system of constraints of \p *this
  //! a renamed-apart version of the constraints in \p cs.
  //! \exception std::invalid_argument thrown if \p *this and \p cs
  //!                                  are topology-incompatible.
  void add_dimensions_and_constraints(ConSys& cs);
  //! Adds the specified generators.
  //! \param  gs          The generators that will be added to the
  //!                     current system of generators. The parameter is
  //!                     not declared <CODE>const</CODE> because it
  //!                     can be modified.
  //! \exception std::invalid_argument thrown if \p *this and \p gs
  //!                                  are dimension-incompatible or
  //!                                  topology-incompatible,
  //!                                  or if \p *this is empty and the
  //!                                  the system of generators \p gs
  //!                                  is not empty, but has no points.
  void add_generators_and_minimize(GenSys& gs);
  //! Adds the specified generators without minimizing.
  //! \param  gs             The generators that will be added to the
  //!                        current system of generators. This parameter
  //!                        is not declared <CODE>const</CODE> because
  //!                        it can be modified.
  //! \exception std::invalid_argument thrown if \p *this and \p gs
  //!                                  are dimension-incompatible or
  //!                                  topoly-incompatible, or if
  //!                                  \p *this is empty and the system of
  //!                                  generators \p gs is not empty, but
  //!                                  has no points.
  void add_generators(GenSys& gs);

  //! Returns <CODE>true</CODE> if and only if \p *this is
  //! an empty polyhedron.
  bool check_empty() const;
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a universe polyhedron.
  bool check_universe() const;

  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a bounded polyhedron.
  bool is_bounded() const;

  //! Returns <CODE>true</CODE> if and only if
  //! polyhedron \p x is contained in polyhedron \p y.
  //! \exception std::invalid_argument thrown if \p x and \p y
  //!                                  are dimension-incompatible.
  friend bool
  Parma_Polyhedra_Library::operator<=(const PolyBase& x,
				      const PolyBase& y);

  //! Output operator.
  friend std::ostream&
  Parma_Polyhedra_Library::operator<<(std::ostream& s, const PolyBase& p);

  //! Input operator.
  friend std::istream&
  Parma_Polyhedra_Library::operator>>(std::istream& s, PolyBase& p);

  //! Swaps \p *this with polyhedron \p y.
  //! (Note that \p *this and \p y can be dimension-incompatible.)
  void swap(PolyBase& y);

protected:
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
  size_t space_dim;

  //! Tests if the polyhedron is necessarily closed.
  bool is_necessarily_closed() const;

  /*! @name Protected Verifiers
    Verify if individual flags are set.
  */
  //@{
  bool is_empty() const;
  bool constraints_are_up_to_date() const;
  bool generators_are_up_to_date() const;
  bool constraints_are_minimized() const;
  bool generators_are_minimized() const;
  bool sat_c_is_up_to_date() const;
  bool sat_g_is_up_to_date() const;
  //@}

  /*! @name State flag setters.
    Set only the specified flags.
  */
  //@{
  void set_zero_dim_univ();
  void set_empty();
  void set_constraints_up_to_date();
  void set_generators_up_to_date();
  void set_constraints_minimized();
  void set_generators_minimized();
  void set_sat_c_up_to_date();
  void set_sat_g_up_to_date();
  //@}

  /*! @name State flag cleaners.
    Clear only the specified flag.
  */
  //@{
  void clear_empty();
  void clear_constraints_up_to_date();
  void clear_generators_up_to_date();
  void clear_constraints_minimized();
  void clear_generators_minimized();
  void clear_sat_c_up_to_date();
  void clear_sat_g_up_to_date();
//@}

  //! Adds new dimensions to the given matrices.
  static void add_dimensions(Matrix& mat1,
                             Matrix& mat2,
                             SatMatrix& sat1,
                             SatMatrix& sat2,
			     size_t add_dim);

  //! Performs the conversion from constraints to generators and vice versa.
  static size_t conversion(Matrix& entry,
			   size_t start,
			   Matrix& result,
			   SatMatrix& sat,
			   size_t num_lines_or_equalities);

  //! Uses Gauss' elimination method to simplify the result of
  //! <CODE>conversion()</CODE>.
  static int simplify(Matrix& mat, SatMatrix& sat);

  //! Builds and simplifies constraints from generators (or vice versa).
  static bool minimize(bool con_to_gen, Matrix& source, Matrix& dest,
		       SatMatrix& sat);

  //! Adds given constraints and builds minimized corresponding generators
  //! or vice versa.
  static bool add_and_minimize(bool con_to_gen,
			       Matrix& source1, Matrix& dest, SatMatrix& sat,
			       const Matrix& source2);
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::PolyBase& x,
	  Parma_Polyhedra_Library::PolyBase& y);

} // namespace std

#include "PolyBase.inlines.hh"

#endif
