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
  Generator operator |(int, const LinExpression& e);
  Generator operator ^(int, const LinExpression& e);
  Generator operator /=(const LinExpression& e, const Integer& d);
}

//! A line, ray or vertex.
/*!
  An object of the class Generator is one of the following:

  - a line: \f$\vect{l} = (a_0, \ldots, a_{n-1})^\transpose\f$;
	
  - a ray: \f$\vect{r} = (a_0, \ldots, a_{n-1})^\transpose\f$;

  - a vertex:
    \f$\vect{v} = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$;

  where \f$n\f$ is the dimension of the space.

  \par How to build a generator.
  Each type of generator is built by applying a particular operator
  (<CODE>|</CODE> for a line, <CODE>^</CODE> for a ray
  and <CODE>/=</CODE> for a vertex) to a linear expression.
  The linear expression represents a direction in the space.
  This means that a linear expression used to define a vertex, ray or line
  should be homogeneous and any constant term will be ignored.

    \par
    In all the examples it is assumed that variables
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
  Generator line(1 | x - y - z);
    \endcode
    When specifying a line, the actual value of the first argument
    of the operator is not significant.
    Also, as mentioned above, the constant term of the linear expression
    is not relevant.
    Thus, the following code has the same effect:
    \code
  Generator line(18 | x - y - z + 15);
    \endcode

    \par Example 2
    The following code builds a ray with the same direction as the
    line in Example 1:
    \code
  Generator ray(1 ^ x - y - z);
    \endcode
    As is the case for lines, when specifying a ray, the actual value
    of the first argument is not significant.

    \par Example 3
    The following code builds the vertex
    \f$(1, 3, 2)^\transpose \in \Rset^3\f$:
    \code
  Generator vertex(x + 3*y + 2*z /= 1);
    \endcode
    Note that the second argument of the operator is
    the <EM>denominator</EM> for the coefficients
    of the linear expression.
    Therefore, the same vertex as above can also be obtained with the code:
    \code
  Generator vertex(2*x + 6*y + 4*z /= 2);
    \endcode
    Obviously, the denominator can be usefully exploited
    for specifying vertices that have some non-integer
    (but rational) coordinates.
    For instance, the vertex \f$(-1.5, 3.2, 2.1)^\transpose \in \Rset^3\f$
    can be specified by the following code:
    \code
  Generator vertex(-15*x + 32*y + 21*z /= 10);
    \endcode
    If a zero denominator is provided, an exception is thrown.
*/

class Parma_Polyhedra_Library::Generator : PPL_INTERNAL Row {
private:
  Generator(LinExpression& e);

  //! Returns the (bidirectional) line of direction \p e.
  friend Generator
  Parma_Polyhedra_Library::operator |(int, const LinExpression& e);
  //! Returns the (unidirectional) ray of direction \p e.
  friend Generator
  Parma_Polyhedra_Library::operator ^(int, const LinExpression& e);
  //! Returns the vertex at \p e / \p d.
  //! \exception std::invalid_argument thrown if \p d is zero.
  friend Generator
  Parma_Polyhedra_Library::operator /=(const LinExpression& e,
				       const Integer& d);

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

#if !OUTLINE
#include "Generator.inlines.hh"
#endif

#endif
