/* Generator class declaration.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Generator_defs_hh
#define _Generator_defs_hh 1

#include "Generator.types.hh"
#include "Row.defs.hh"
#include "LinExpression.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

  //! @name Non-friend operators on objects of the class Generator.
  //@{
  //! Output operator.
  std::ostream&
  operator <<(std::ostream& s, const Generator& g);
  //@}

  // Put them in the namespace here to declare them friend later.
  Generator line(const LinExpression& e);
  Generator ray(const LinExpression& e);
  Generator vertex(const LinExpression& e, const Integer& d);
}

//! A line, ray or vertex.
/*!
  An object of the class Generator is one of the following:

  - a line \f$\vect{l} = (a_0, \ldots, a_{n-1})^\transpose\f$;
	
  - a ray \f$\vect{r} = (a_0, \ldots, a_{n-1})^\transpose\f$;

  - a vertex
    \f$\vect{v} = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$;

  where \f$n\f$ is the dimension of the space.

  \par A note on terminology.
  As observed in the Introduction, there are cases when, in order to
  represent a polyhedron \f$P\f$ using generators, we need to include
  in the finite set \f$V\f$ even points of \f$P\f$ that are <EM>not</EM>
  vertices of \f$P\f$.
  Nonetheless, accordingly to what is now an established terminology,
  we will call <EM>vertex</EM> any element of the set of generators \f$V\f$,
  even though it is not a ``proper'' vertex of \f$P\f$.

  \par How to build a generator.
  Each type of generator is built by applying the corresponding
  function (<CODE>line</CODE>, <CODE>ray</CODE> or <CODE>vertex</CODE>)
  to a linear expression, representing a direction in the space.
  This means that a linear expression used to define a generator
  should be homogeneous and any constant term will be ignored.
  When defining a vertex, an optional Integer argument can be used
  as a common <EM>denominator</EM> for all the coefficients occurring
  in the provided linear expression;
  the default value for this argument is 1.

    \par
    In all the following examples it is assumed that variables
    <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE>
    are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
  Variable z(2);
    \endcode

    \par Example 1
    The following code builds a line with direction \f$x-y-z\f$:
    \code
  Generator l = line(x - y - z);
    \endcode
    As mentioned above, the constant term of the linear expression
    is not relevant. Thus, the following code has the same effect:
    \code
  Generator l = line(x - y - z + 15);
    \endcode

    \par Example 2
    The following code builds a ray with the same direction as the
    line in Example 1:
    \code
  Generator r = ray(x - y - z);
    \endcode
    As is the case for lines, when specifying a ray the constant term
    of the linear expression is not relevant.

    \par Example 3
    The following code builds the vertex
    \f$\vect{v} = (1, 0, 2)^\transpose \in \Rset^3\f$:
    \code
  Generator v = vertex(1*x + 0*y + 2*z);
    \endcode
    The same effect can be obtained by using the following code:
    \code
  Generator v = vertex(x + 2*z);
    \endcode
    Similarly, the origin \f$\vect{0} \in \Rset^3\f$ can be defined
    using either one of the following lines of code:
    \code
  Generator origin1 = vertex(0*x + 0*y + 0*z);
  Generator origin2 = vertex(0*z);
    \endcode
    Note however that the following line would have defined
    a different vertex, namely \f$\vect{0} \in \Rset^2\f$:
    \code
  Generator origin3 = vertex(0*y);
    \endcode
    
    \par Example 4
    The vertex \f$\vect{v}\f$ specified in Example 3 above
    can also be obtained with the following code,
    where we provide a non-default value for the denominator argument:
    \code
  Generator v = vertex(2*x + 0*y + 4*z, 2);
    \endcode
    Obviously, the denominator can be usefully exploited to specify
    vertices having some non-integer (but rational) coordinates.
    For instance, the vertex
    \f$\vect{w} = (-1.5, 3.2, 2.1)^\transpose \in \Rset^3\f$
    can be specified by the following code:
    \code
  Generator w = vertex(-15*x + 32*y + 21*z, 10);
    \endcode
    If a zero denominator is provided, an exception is thrown.
*/

class Parma_Polyhedra_Library::Generator : PPL_INTERNAL Row {
private:
  Generator(LinExpression& e);

  //! Returns the (bidirectional) line of direction \p e.
  friend Generator
  Parma_Polyhedra_Library::line(const LinExpression& e);
  //! Returns the (unidirectional) ray of direction \p e.
  friend Generator
  Parma_Polyhedra_Library::ray(const LinExpression& e);
  //! Returns the vertex at \p e / \p d
  //! (note that \p d is an optional argument with default value 1).
  //! \exception std::invalid_argument thrown if \p d is zero.
  friend Generator
  Parma_Polyhedra_Library::vertex(const LinExpression& e,
				  const Integer& d = 1);

public:
  //! Default constructor.
  Generator();
  //! Ordinary copy-constructor.
  Generator(const Generator& g);
  //! Destructor.
  ~Generator();

  //! The generator type.
  enum Type {
    LINE = Row::LINE_OR_EQUALITY,
    RAY = Row::RAY_OR_VERTEX_OR_INEQUALITY,
    VERTEX = RAY+1
  };

  //! Returns the generator type of \p *this.
  Type type() const;

PPL_INTERNAL:
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a line.
  bool is_line() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is either a ray or a vertex.
  bool is_ray_or_vertex() const;
  //! Sets the type to <CODE>LINE</CODE>.
  void set_is_line();
  //! Sets the type to <CODE>RAY</CODE>.
  void set_is_ray_or_vertex();
};

#include "Generator.inlines.hh"

#endif
