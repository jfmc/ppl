/* Grid class declaration.
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

#ifndef PPL_Grid_defs_hh
#define PPL_Grid_defs_hh 1

#include "Grid.types.hh"
#include "globals.defs.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include "Congruence_System.defs.hh"
#include "Congruence_System.inlines.hh"
#include "Generator_System.defs.hh"
#include "Generator_System.inlines.hh"
#include "Generator.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include <vector>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Grid Writes a textual
  representation of \p gr on \p s: <CODE>false</CODE> is written if \p
  gr is an empty grid; <CODE>true</CODE> is written if \p gr is a
  universe grid; a reduced system of congruences defining \p gr is
  written otherwise, all congruences in one row separated by ", "s.
*/
std::ostream&
operator<<(std::ostream& s, const Grid& gr);

} // namespace IO_Operators

//! Returns <CODE>true</CODE> if and only if \p x and \p y are the same grid.
/*!
  \relates Grid
  Note that \p x and \p y may be dimension-incompatible grids: in
  those cases, the value <CODE>false</CODE> is returned.
*/
bool operator==(const Grid& x, const Grid& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are different grids.
/*!
  \relates Grid
  Note that \p x and \p y may be dimension-incompatible grids: in
  those cases, the value <CODE>true</CODE> is returned.
*/
bool operator!=(const Grid& x, const Grid& y);

} // namespace Parma_Polyhedra_Library


//! The class of grids.
/*!
    An object of the class Grid represents a convex grid in the vector
    space \f$\Rset^n\f$.

    A grid can be specified as either a finite system of congruences
    or a finite system of generators (see Section \ref representation)
    and it is always possible to obtain either representation.
    That is, if we know the system of congruences, we can obtain
    from this the system of generators that define the same grid
    and vice versa.
    These systems can contain redundant members: in this case we say
    that they are not in the minimal form.
    Most operators on grids are provided with two implementations:
    one of these, denoted <CODE>\<operator-name\>_and_minimize</CODE>,
    also enforces the reduction of the representations,
    and returns the Boolean value <CODE>false</CODE> whenever
    the resulting grid turns out to be empty.

    Two key attributes of any grid are its topological kind
    (recording whether it is a C_Grid or an NNC_Grid object)
    and its space dimension (the dimension \f$n \in \Nset\f$ of
    the enclosing vector space):

    - all grids, the empty ones included, are endowed with
      a specific topology and space dimension;
    - most operations working on a grid and another object
      (i.e., another grid, a congruence or generator,
      a set of variables, etc.) will throw an exception if
      the grid and the object are not both topology-compatible
      and dimension-compatible (see Section \ref representation);
    - the topology of a grid cannot be changed;
      rather, there are constructors for each of the two derived classes
      that will build a new grid with the topology of that class
      from another grid from either class and any topology;
    - the only ways in which the space dimension of a grid can be
      changed are <EM>explicit</EM> calls to operators provided for
      that purpose, and standard copy, assignment and swap operators.

    Note that four different grids can be defined on
    the zero-dimension space:
    the empty grid, either closed or NNC,
    and the universe grid \f$R^0\f$, again either closed or NNC.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined (where they are
    used) as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a grid corresponding to
    a square in \f$\Rset^2\f$, given as a system of congruences:
    \code
  Congruence_System cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  C_Grid gr(cs);
    \endcode
    The following code builds the same grid as above,
    but starting from a system of generators specifying
    the four vertices of the square:
    \code
  Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + 3*y));
  gs.insert(point(3*x + 0*y));
  gs.insert(point(3*x + 3*y));
  C_Grid gr(gs);
    \endcode

    \par Example 2
    The following code builds an unbounded grid
    corresponding to a half-strip in \f$\Rset^2\f$,
    given as a system of congruences:
    \code
  Congruence_System cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
  C_Grid gr(cs);
    \endcode
    The following code builds the same grid as above,
    but starting from the system of generators specifying
    the two vertices of the grid and one ray:
    \code
  Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + y));
  gs.insert(ray(x - y));
  C_Grid gr(gs);
    \endcode

    \par Example 3
    The following code builds the grid corresponding to
    a half-plane by adding a single congruence
    to the universe grid in \f$\Rset^2\f$:
    \code
  C_Grid gr(2);
  gr.add_congruence(y >= 0);
    \endcode
    The following code builds the same grid as above,
    but starting from the empty grid in the space \f$\Rset^2\f$
    and inserting the appropriate generators
    (a point, a ray and a line).
    \code
  C_Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(0*x + 0*y));
  gr.add_generator(ray(y));
  gr.add_generator(line(x));
    \endcode
    Note that, although the above grid has no vertices, we must add
    one point, because otherwise the result of the Minkowsky's sum
    would be an empty grid.
    To avoid subtle errors related to the reduction process,
    it is required that the first generator inserted in an empty
    grid is a point (otherwise, an exception is thrown).

    \par Example 4
    The following code shows the use of the function
    <CODE>add_space_dimensions_and_embed</CODE>:
    \code
  C_Grid gr(1);
  gr.add_congruence(x == 2);
  gr.add_space_dimensions_and_embed(1);
    \endcode
    We build the universe grid in the 1-dimension space \f$\Rset\f$.
    Then we add a single equality congruence,
    thus obtaining the grid corresponding to the singleton set
    \f$\{ 2 \} \sseq \Rset\f$.
    After the last line of code, the resulting grid is
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
  C_Grid gr(1);
  gr.add_congruence(x == 2);
  gr.add_space_dimensions_and_project(1);
    \endcode
    The first two lines of code are the same as in Example 4 for
    <CODE>add_space_dimensions_and_embed</CODE>.
    After the last line of code, the resulting grid is
    the singleton set
    \f$\bigl\{ (2, 0)^\transpose \bigr\} \sseq \Rset^2\f$.

    \par Example 6
    The following code shows the use of the function
    <CODE>affine_image</CODE>:
    \code
  C_Grid gr(2, Grid::EMPTY);
  gr.add_generator(point(0*x + 0*y));
  gr.add_generator(point(0*x + 3*y));
  gr.add_generator(point(3*x + 0*y));
  gr.add_generator(point(3*x + 3*y));
  Linear_Expression coeff = x + 4;
  gr.affine_image(x, coeff);
    \endcode
    In this example the starting grid is a square in
    \f$\Rset^2\f$, the considered variable is \f$x\f$ and the affine
    expression is \f$x+4\f$.  The resulting grid is the same
    square translated to the right.  Moreover, if the affine
    transformation for the same variable \p x is \f$x+y\f$:
    \code
  Linear_Expression coeff = x + y;
    \endcode
    the resulting grid is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x-y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable; for example, the affine expression \f$y\f$:
    \code
  Linear_Expression coeff = y;
    \endcode
    the resulting grid is a diagonal of the square.

    \par Example 7
    The following code shows the use of the function
    <CODE>affine_preimage</CODE>:
    \code
  C_Grid gr(2);
  gr.add_congruence(x >= 0);
  gr.add_congruence(x <= 3);
  gr.add_congruence(y >= 0);
  gr.add_congruence(y <= 3);
  Linear_Expression coeff = x + 4;
  gr.affine_preimage(x, coeff);
    \endcode
    In this example the starting grid, \p var and the affine
    expression and the denominator are the same as in Example 6,
    while the resulting grid is again the same square,
    but translated to the left.
    Moreover, if the affine transformation for \p x is \f$x+y\f$
    \code
  Linear_Expression coeff = x + y;
    \endcode
    the resulting grid is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x+y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable \p x, for example, the affine expression \f$y\f$:
    \code
  Linear_Expression coeff = y;
    \endcode
    the resulting grid is a line that corresponds to the \f$y\f$ axis.

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
  C_Grid gr(gs);
  set<Variable> to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  gr.remove_space_dimensions(to_be_removed);
    \endcode
    The starting grid is the singleton set
    \f$\bigl\{ (3, 1, 0, 2)^\transpose \bigr\} \sseq \Rset^4\f$, while
    the resulting grid is
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
  gr.remove_space_dimensions(to_be_removed1);
  set<Variable> to_be_removed2;
  to_be_removed2.insert(z);
  gr.remove_space_dimensions(to_be_removed2);
    \endcode
    In this case, the result is the grid
    \f$\bigl\{(3, 0)^\transpose \bigr\} \sseq \Rset^2\f$:
    when removing the set of dimensions \p to_be_removed2
    we are actually removing variable \f$w\f$ of the original grid.
    For the same reason, the operator \p remove_space_dimensions
    is not idempotent: removing twice the same non-empty set of dimensions
    is never the same as removing them just once.
*/

class Parma_Polyhedra_Library::Grid {
public:
  //! Returns the maximum space dimension all kinds of Grid can handle.
  static dimension_type max_space_dimension();

  //! Kinds of degenerate grids.
  enum Degenerate_Kind {
    //! The universe grid, i.e., the whole vector space.
    UNIVERSE,
    //! The empty grid, i.e., the empty set.
    EMPTY
  };

  //! Builds a grid having the specified properties.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the grid;

    \param kind
    Specifies whether the universe or the empty grid has to be built.
  */
  Grid(dimension_type num_dimensions = 0,
       const Degenerate_Kind kind = UNIVERSE);

  //! Ordinary copy-constructor.
  Grid(const Grid& y);

  //! Builds a grid from a const system of congruences.
  /*!
    The grid inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences defining the grid.
  */
  Grid(const Congruence_System& cgs);

  //! Builds a grid recycling a system of congruences.
  /*!
    The grid inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences defining the grid.  It is not
    declared <CODE>const</CODE> because its data-structures will be
    recycled to build the grid.
  */
  Grid(Congruence_System& cgs);

  //! Builds a grid from a system of generators.
  /*!
    The grid inherits the space dimension of the generator system.

    \param const_gs
    The system of generators defining the grid.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.
  */
  Grid(const Generator_System& const_gs);

  //! Builds a grid recycling a system of generators.
  /*!
    The grid inherits the space dimension of the generator system.

    \param gs
    The system of generators defining the grid.  It is not
    declared <CODE>const</CODE> because its data-structures will be
    recycled to build the grid.

    \exception std::invalid_argument
    Thrown if the system of generators is not empty but has no points.
  */
  Grid(Generator_System& gs);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  Grid& operator=(const Grid& y);

  //! \name Member Functions that Do Not Modify the Grid
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Returns \f$0\f$, if \p *this is empty; otherwise, returns the
  //! \ref affine_dimension "affine dimension" of \p *this.
  dimension_type affine_dimension() const;

  //! Returns the system of congruences.
  const Congruence_System& congruences() const;

  //! Returns the system of congruences, in reduced form.
  const Congruence_System& minimized_congruences() const;

  //! Returns the system of generators.
  const Generator_System& generators() const;

  //! Returns the system of generators, in reduced form.
  const Generator_System& minimized_generators() const;

  //! \brief
  //! Returns the relations holding between the grid \p *this
  //! and the congruence \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p c are dimension-incompatible.
  */
  // FIXME: Poly_Con_Relation seems to encode exactly what we want
  // here.  We must find a new name for that class.  Temporarily,
  // we keep using it without changing the name.
  Poly_Con_Relation relation_with(const Congruence& c) const;

  //! \brief
  //! Returns the relations holding between the grid \p *this
  //! and the generator \p g.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  // FIXME: see the comment for Poly_Con_Relation above.
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is
  //! an empty grid.
  bool is_empty() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is a universe
  //! grid.
  bool is_universe() const;

  //! Returns <CODE>true</CODE> if and only if \p *this and \p y are disjoint.
  /*!
    \exception std::invalid_argument
    Thrown if \p x and \p y are topology-incompatible or
    dimension-incompatible.
  */
  bool is_disjoint_from(const Grid& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this does not contain lines.
  bool is_pointed() const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool contains(const Grid& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this strictly contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool strictly_contains(const Grid& y) const;

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

  //@} // Member Functions that Do Not Modify the Grid

  //! \name Space Dimension Preserving Member Functions that May Modify the Grid
  //@{

  //! \brief
  //! Adds a copy of congruence \p cg to the system of congruences
  //! of \p *this (without reducing the result).
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  void add_congruence(const Congruence& cg);

  //! \brief
  //! Adds a copy of congruence \p cg to the system of congruences
  //! of \p *this, reducing the result
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and congruence \p cg are dimension-incompatible.
  */
  bool add_congruence_and_minimize(const Congruence& c);

  //! \brief
  //! If \p c is an equality constraint, adds a copy \p c, seen as a
  //! modulo 0 congruence, to the system of congruences of \p *this
  //! (without reducing the result).
  /*!
    \exception std::invalid_argument
    Thrown if \p c is not an equality constraint or if \p *this and
    constraint \p c are dimension-incompatible.
  */
  void add_congruences_and_minimize(const Constraint_System& cs);

  //! \brief
  //! If \p c is an equality constraint, adds a copy \p c, seen as a
  //! modulo 0 congruence, to the system of congruences of \p *this,
  //! reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p c is not an equality constraint or if \p *this and
    constraint \p c are dimension-incompatible.
  */
  bool add_congruence_and_minimize(const Constraint& c);

  //! \brief
  //! Adds a copy of generator \p g to the system of generators
  //! of \p *this (without reducing the result).
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is an empty grid and \p g is not a point.
  */
  void add_generator(const Generator& g);

  //! \brief
  //! Adds a copy of generator \p g to the system of generators
  //! of \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible,
    or if \p *this is an empty grid and \p g is not a point.
  */
  bool add_generator_and_minimize(const Generator& g);

  //! \brief Adds a copy of the congruences in \p cgs to the system
  //! of congruences of \p *this (without reducing the result).
  /*!
    \param cgs
    Contains the congruences that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  void add_congruences(const Congruence_System& cgs);

  // FIX temporary, for test app
  //! \brief Adds a copy of the constraints in \p cgs to the system
  //! of congruences of \p *this (without reducing the result).
  /*!
    \param cs
    Contains the constraints that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  void add_congruence_and_minimize(const Constraint_System& cs);

  //! \brief Adds the congruences in \p cgs to the system of congruences
  //! of \p *this (without reducing the result).
  /*!
    \param cgs
    The congruence system that will be recycled, adding its
    congruences to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.

    \warning
    The only assumption that can be made on \p cgs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  void add_recycled_congruences(Congruence_System& cgs);

  //! \brief
  //! Adds a copy of the congruences in \p cgs to the system
  //! of congruences of \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cgs
    Contains the congruences that will be added to the system of
    congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.
  */
  bool add_congruences_and_minimize(const Congruence_System& cgs);

  //! \brief
  //! Adds the congruences in \p cgs to the system of congruences
  //! of \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cgs
    The congruence system that will be recycled, adding its
    congruences to the system of congruences of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p cgs are dimension-incompatible.

    \warning
    The only assumption that can be made on \p cgs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_congruences_and_minimize(Congruence_System& cgs);

  //! \brief Adds a copy of the generators in \p gs to the system
  //! of generators of \p *this (without reducing the result).
  /*!
    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of generators \p gs is not empty,
    but has no points.
  */
  void add_generators(const Generator_System& gs);

  //! \brief Adds the generators in \p gs to the system of generators
  //! of \p *this (without reducing the result).
  /*!
    \param gs
    The generator system that will be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the system of generators \p gs is not empty,
    but has no points.

    \warning
    The only assumption that can be made on \p gs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  void add_recycled_generators(Generator_System& gs);

  //! \brief Adds a copy of the generators in \p gs to the system
  //! of generators of \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    Contains the generators that will be added to the system of
    generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the the system of generators \p gs is not empty,
    but has no points.
  */
  bool add_generators_and_minimize(const Generator_System& gs);

  //! \brief Adds the generators in \p gs to the system of generators
  //! of \p *this, reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param gs
    The generator system that will be recycled, adding its generators
    to the system of generators of \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p gs are dimension-incompatible, or if
    \p *this is empty and the the system of generators \p gs is not empty,
    but has no points.

    \warning
    The only assumption that can be made on \p gs upon successful or
    exceptional return is that it can be safely destroyed.
  */
  bool add_recycled_generators_and_minimize(Generator_System& gs);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y.
  //! The result is not guaranteed to be reduced.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void intersection_assign(const Grid& y);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y,
  //! reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const Grid& y);

  //! \brief
  //! Assigns to \p *this the join of \p *this and \p y.
  //! The result is not guaranteed to be reduced.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void join_assign(const Grid& y);

  //! \brief
  //! Assigns to \p *this the join of \p *this and \p y,
  //! reducing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool join_assign_and_minimize(const Grid& y);

  //! \brief
  //! Assigns to \p *this the FIX \ref grid_difference
  //! "grid-difference" of \p *this and \p y. The result is not
  //! guaranteed to be reduced.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void grid_difference_assign(const Grid& y);

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

    In other words, if \f$R\f$ is a \f$m_1 \times n_1\f$ matrix representing
    the rays of the grid, \f$V\f$ is a \f$m_2 \times n_2\f$
    matrix representing the points of the grid and
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
    the resulting grid is
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
		    Coefficient_traits::const_reference denominator
		      = Coefficient_one());

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

    In other words, if \f$A\f$ is a \f$m \times n\f$ matrix representing
    the congruences of the grid, \f$T\f$ is the affine transformation
    to apply to \f$P\f$ and
    \f[
      P = \bigl\{\,
            \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
          \bigm|
            A\vect{x} \geq \vect{0}
          \,\bigr\}.
    \f]
    The resulting grid is
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
		       Coefficient_traits::const_reference denominator
		         = Coefficient_one());

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref time_elapse "time-elapse" between \p *this and \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void time_elapse_assign(const Grid& y);

  //@} // Space Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! \brief
  //! Adds \p m new space dimensions and embeds the old grid
  //! in the new vector space.
  /*!
    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the vector
    space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new grid, which is characterized by a system of congruences
    in which the variables which are the new dimensions can have any
    value.  For instance, when starting from the grid \f$\cP \sseq
    \Rset^2\f$ and adding a third space dimension, the result will be
    the grid
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cP
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  //! Adds new space dimensions to the given systems.
  /*!
    \param cgs
    A congruence system, to which columns are added;

    \param gs
    A generator system, to which rows and columns are added;

    \param dims
    The number of space dimensions to add.

    Adds new space dimensions to the vector space modifying the given
    systems.

    This method is invoked only by
    <CODE>add_space_dimensions_and_embed()</CODE>.
  */
  void add_space_dimensions(Congruence_System& cgs,
			    Generator_System& gs,
			    dimension_type dims);

  //! Adds new space dimensions to the given systems.
  /*!
    \param gs
    A generator system, to which columns are added;

    \param cgs
    A congruence system, to which rows and columns are added;

    \param dims
    The number of space dimensions to add.

    Adds new space dimensions to the vector space modifying the given
    systems.

    This method is invoked only by
    <CODE>add_space_dimensions_and_project()</CODE>.
  */
  void add_space_dimensions(Generator_System& gs,
			    Congruence_System& cgs,
			    dimension_type dims);

  //! \brief
  //! Adds \p m new space dimensions to the grid
  //! and does not embed it in the new vector space.
  /*!
    \param m
    The number of space dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new grid, which is characterized by a system
    of congruences in which the variables running through
    the new dimensions are all constrained to be equal to 0.
    For instance, when starting from the grid \f$\cP \sseq \Rset^2\f$
    and adding a third space dimension, the result will be the grid
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
    \exception std::length_error
    Thrown if the concatenation would cause the vector space
    to exceed dimension <CODE>max_space_dimension()</CODE>.
  */
  void concatenate_assign(const Grid& y);

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
    dimension of the vector space enclosing the grid.

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

  friend bool Parma_Polyhedra_Library::operator==(const Grid& x,
						  const Grid& y);

  //! \name Miscellaneous Member Functions
  //@{

  //! Destructor.
  ~Grid();

  //! \brief
  //! Swaps \p *this with grid \p y.
  //! (\p *this and \p y can be dimension-incompatible.)
  /*!
    \exception std::invalid_argument
    Thrown if \p x and \p y are topology-incompatible.
  */
  void swap(Grid& y);

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

  //! If \p sys is lower triangular return true, else return false.
  static bool lower_triangular(const Congruence_System& sys);

  //! If \p sys is upper triangular return true, else return false.
  static bool upper_triangular(const Generator_System& sys);

  //@} // Miscellaneous Member Functions

private:

  //! The system of congruences.
  Congruence_System con_sys;

  //! The system of generators.
  Generator_System gen_sys;

#if 1
  // Please, do not move the following include directive:
  // `Ph_Status.idefs.hh' must be included exactly at this point.
  // And please do not remove the space separating `#' from `include':
  // this ensures that the directive will not be moved during the
  // procedure that automatically creates the library's include file
  // (see `Makefile.am' in the `src' directory).
# include "Grid_Status.idefs.hh"

  //! The status flags to keep track of the grid's internal state.
  Status status;
#endif

  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

  //! Builds a grid from a system of congruences.
  /*!
    The grid inherits the space dimension of the congruence system.

    \param cgs
    The system of congruences defining the grid.
  */
  void construct(const Congruence_System& cgs);

  //! Builds a grid from a system of generators.
  /*!
    The grid inherits the space dimension of the generator system.

    \param gs
    The system of generators defining the grid.
  */
  void construct(const Generator_System& gs);

  //! \name Private Verifiers: Verify if Individual Flags are Set
  //@{

  //! Returns <CODE>true</CODE> if the grid is known to be empty.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-empty.
  */
  bool marked_empty() const;

  //! Returns <CODE>true</CODE> if the system of congruences is up-to-date.
  bool congruences_are_up_to_date() const;

  //! Returns <CODE>true</CODE> if the system of generators is up-to-date.
  bool generators_are_up_to_date() const;

  //! Returns <CODE>true</CODE> if the system of congruences is reduced.
  /*!
    // FIX
    Note that only \em weak minimization is entailed, so that
    an NNC grid may still have \f$\epsilon\f$-redundant congruences.
  */
  bool congruences_are_minimized() const;

  //! Returns <CODE>true</CODE> if the system of generators is minimized.
  /*!
    Note that only \em weak minimization is entailed, so that
    an NNC grid may still have \f$\epsilon\f$-redundant generators.
  */
  bool generators_are_minimized() const;

  //! Returns <CODE>true</CODE> if there are pending congruences.
  bool has_pending_congruences() const;

  //! Returns <CODE>true</CODE> if there are pending generators.
  bool has_pending_generators() const;

  //! \brief
  //! Returns <CODE>true</CODE> if there are either pending
  //! congruences or pending generators.
  bool has_something_pending() const;

  //! \brief
  //! Returns <CODE>true</CODE> if the grid can have something
  //! pending.
  bool can_have_something_pending() const;

  //@} // Private Verifiers: Verify if Individual Flags are Set

  //! \name State Flag Setters: Set Only the Specified Flags
  //@{

  //! \brief
  //! Sets \p status to express that the grid
  //! is the universe 0-dimension vector space,
  //! clearing all corresponding matrices.
  void set_zero_dim_univ();

  //! \brief
  //! Sets \p status to express that the grid is empty,
  //! clearing all corresponding matrices.
  void set_empty();

  //! Sets \p status to express that congruences are up-to-date.
  void set_congruences_up_to_date();

  //! Sets \p status to express that generators are up-to-date.
  void set_generators_up_to_date();

  //! Sets \p status to express that congruences are minimized.
  void set_congruences_minimized();

  //! Sets \p status to express that generators are minimized.
  void set_generators_minimized();

  //! Sets \p status to express that congruences are pending.
  void set_congruences_pending();

  //! Sets \p status to express that generators are pending.
  void set_generators_pending();

  //@} // State Flag Setters: Set Only the Specified Flags

  //! \name State Flag Cleaners: Clear Only the Specified Flag
  //@{

  //! Clears the \p status flag indicating that the grid is empty.
  void clear_empty();

  //! Sets \p status to express that congruences are out of date.
  void clear_congruences_up_to_date();

  //! Sets \p status to express that parameters are out of date.
  void clear_generators_up_to_date();

  //! Sets \p status to express that congruences are no longer reduced.
  void clear_congruences_minimized();

  //! Sets \p status to express that generators are no longer minimized.
  void clear_generators_minimized();

  //! Sets \p status to express that there are no longer pending congruences.
  void clear_pending_congruences();

  //! Sets \p status to express that there are no longer pending generators.
  void clear_pending_generators();

  //@} // State Flag Cleaners: Clear Only the Specified Flag

  //! \name Updating and Sorting Matrices
  //@{

  //! Updates congruences starting from generators and reduces them.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty grid.

    The resulting system of congruences is only partially sorted:
    the equalities are in the upper part of the matrix,
    while the inequalities in the lower part.
  */
  bool update_congruences() const;

  //! Updates generators starting from congruences and minimizes them.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty grid.

    The resulting system of generators is only partially sorted:
    the lines are in the upper part of the matrix,
    while rays and points are in the lower part.
    It is illegal to call this method when the Status field
    already declares the grid to be empty.
  */
  bool update_generators() const;

  //@} // Updating and Sorting Matrices

  //! \name Weak and Strong Minimization of Descriptions
  //@{

  //! Applies (weak) minimization to both the congruences and generators.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty grid.

    Minimization is not attempted if the Status field already declares
    both systems to be minimized.
  */
  bool minimize() const;

  //! Applies strong minimization to the congruences of an NNC grid.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty grid.
  */
  bool strongly_minimize_congruences() const;

  //! Applies strong minimization to the generators of an NNC grid.
  /*!
    \return
    <CODE>false</CODE> if and only if \p *this turns out to be an
    empty grid.
  */
  bool strongly_minimize_generators() const;

  //@} // Weak and Strong Minimization of Descriptions

  enum Three_Valued_Boolean {
    TVB_TRUE,
    TVB_FALSE,
    TVB_DONT_KNOW
  };

  //! Polynomial but incomplete equivalence test between grids.
  Three_Valued_Boolean quick_equivalence_test(const Grid& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is included in \p y.
  bool is_included_in(const Grid& y) const;

  //! Adds the low-level congruences to the congruence system.
  static void add_low_level_congruences(Congruence_System& cgs);

  //! \name Minimization-related Static Member Functions
  //@{

  // FIX params,returns
  // FIX these ret true for empty while minimize() and the grid ops ret false

  //! Builds and simplifies congruences from generators.
  static bool minimize(Congruence_System& source,
		       Linear_System& dest);

  //! Builds and simplifies generators from congruences.
  static bool minimize(Generator_System& dest,
		       Congruence_System& source);

  //! \brief
  //! Adds given congruences and builds minimized corresponding generators,
  //! or vice versa.
  static bool add_and_minimize(Congruence_System& source1,
			       Linear_System& dest,
			       const Congruence_System& source2);

  //! \brief
  //! Adds given congruences and builds minimized corresponding generators
  //! or vice versa. The given congruences are in \p source.
  static bool add_and_minimize(Congruence_System& source,
			       Linear_System& dest);

  //! \brief
  //! Adds given generators and builds reduced corresponding congruences.
  static bool add_and_minimize(Generator_System& source,
			       Congruence_System& dest);

  //! Adds given generators and builds reduced corresponding congruences.
  static bool add_and_minimize(Generator_System& source1,
			       Congruence_System& dest,
			       const Generator_System& source2);

  //! \brief
  //! Converts parameter system \p dest to be equivalent to congruence
  //! system \p source.
  static dimension_type conversion(Congruence_System& source,
				   Linear_System& dest);

  //! \brief
  //! Converts congruence system \p dest to be equivalent to parameter
  //! system \p source.
  static dimension_type conversion(Generator_System& source,
				   Congruence_System& dest);

  //! Normalize the divisors in \p sys.
  /*!
    Convert \p sys to an equivalent representation in which the
    divisors of all the generators are equal.

    \p divisor is an extra divisor to include in the calculation of a
    common divisor.  A value of 0 for \p divisor indicates to leave it
    out of the calculation.
  */
  static Coefficient
  normalize_divisors(Linear_System& sys,
		     Coefficient_traits::const_reference divisor = 0);

  //! Convert the rows in \p sys to parameters.
  /*!
    Represent all points in \p sys relative to row \p reference_row.
    If leave_first is true then the first row in \p sys is left in
    tact (i.e. \p sys is turned into a parameter system).
  */
  // FIX name? relativize?
  static Generator_System& parameterize(Generator_System& sys,
					Generator& reference_row,
					bool leave_first = true);

  //! Convert \p cgs to upper triangular form.
  /*!
    Return true if \p cgs is consistent, else false.
  */
  static bool simplify(Congruence_System& cgs);

  //! Convert \p gs to lower triangular form.
  /*!
    Return true if \p gs is consistent, else false.
  */
  static bool simplify(Generator_System& gs);

  //! Reduce the line \p row using the line \p pivot.
  /*!
    Use the line \p pivot to change the representation of the line \p
    row so that element col of \p row is zero.
  */
  // A member of Grid for access to Matrix::rows.
  static void reduce_line_with_line(Row& row, Row& pivot,
				    dimension_type col);

  //! Reduce the equality \p row using the equality \p pivot.
  /*!
    Use the equality \p pivot to change the representation of the
    equality \p row so that element col of \p row is zero.
  */
  // A member of Grid for access to Matrix::rows.
  static void reduce_equality_with_equality(Congruence& row, Congruence& pivot,
					    dimension_type col);

  //! Reduce \p row using \p pivot.
  /*!
    Use the parameter or congruence at \p pivot to change the
    representation of the parameter or congruence at \p row so that
    element col of \p row is zero.  If \p parameters is true then the
    two rows are taken as parameters, else as congruences.
  */
  // Part of Grid for access to Matrix::rows.
  static void reduce_pc_with_pc(Row& row, Row& pivot, dimension_type col,
				bool parameters = true);

  //! Reduce \p row using \p pivot.
  /*!
    Use the line or equation at \p pivot to change the representation
    of the parameter or congruence at \p row so that element col of \p
    row is zero.
  */
  // A member of Grid for access to Matrix::rows.
  static void reduce_parameter_with_line(Linear_Row& row,
					 Linear_Row& pivot,
					 dimension_type col,
					 Linear_System& sys);

  //! Reduce \p row using \p pivot.
  /*!
    Use the equality \p pivot to change the representation of the
    congruence \p row so that element col of \p row is zero.
  */
  // A member of Grid for access to Matrix::rows.
  static void reduce_congruence_with_equality(Congruence& row,
					      Congruence& pivot,
					      dimension_type col,
					      Congruence_System& sys);

  //@} // Minimization-Related Static Member Functions

  //! \name Exception Throwers
  //@{
protected:
  void throw_runtime_error(const char* method) const;
  void throw_invalid_argument(const char* method, const char* reason) const;

  void throw_topology_incompatible(const char* method,
				   const char* g_name,
				   const Generator& g) const;
  void throw_topology_incompatible(const char* method,
				   const char* gs_name,
				   const Generator_System& gs) const;

  void throw_dimension_incompatible(const char* method,
				    const char* other_name,
				    dimension_type other_dim) const;
  void throw_dimension_incompatible(const char* method,
				    const char* gr_name,
				    const Grid& gr) const;
  void throw_dimension_incompatible(const char* method,
				    const char* e_name,
				    const Linear_Expression& e) const;
  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Congruence& c) const;
  void throw_dimension_incompatible(const char* method,
				    const char* g_name,
				    const Generator& g) const;
  void throw_dimension_incompatible(const char* method,
				    const char* cs_name,
				    const Congruence_System& cs) const;
  void throw_dimension_incompatible(const char* method,
				    const char* gs_name,
				    const Generator_System& gs) const;
  void throw_dimension_incompatible(const char* method,
				    const char* var_name,
				    const Variable var) const;
  void throw_dimension_incompatible(const char* method,
				    dimension_type required_space_dim) const;

  // Note: it has to be a static method, because it can be called inside
  // constructors (before actually constructing the grid object).
  static void throw_space_dimension_overflow(const char* method,
					     const char* reason);

  void throw_invalid_generator(const char* method,
			       const char* g_name) const;
  void throw_invalid_generators(const char* method,
				const char* gs_name) const;
  //@} // Exception Throwers

};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Grid */
void swap(Parma_Polyhedra_Library::Grid& x,
	  Parma_Polyhedra_Library::Grid& y);

} // namespace std

#include "Grid_Status.inlines.hh"
#include "Grid.inlines.hh"

#endif // !defined(PPL_Grid_defs_hh)
