/* Generator class declaration.
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

#ifndef _Generator_defs_hh
#define _Generator_defs_hh 1

#include "Generator.types.hh"
#include "Row.defs.hh"
#include "Variable.defs.hh"
#include "LinExpression.defs.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

  //! Output operator.
  std::ostream&
  operator<<(std::ostream& s, const Generator& g);

  // Put them in the namespace here to declare them friend later.
  Generator line(const LinExpression& e);
  Generator ray(const LinExpression& e);
  Generator point(const LinExpression& e, const Integer& d);
  Generator closure_point(const LinExpression& e, const Integer& d);

}

//! A line, ray, point or closure point.
/*!
  An object of the class Generator is one of the following:

  - a line \f$\vect{l} = (a_0, \ldots, a_{n-1})^\transpose\f$;
	
  - a ray \f$\vect{r} = (a_0, \ldots, a_{n-1})^\transpose\f$;

  - a point
    \f$\vect{p} = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$;

  - a closure point
    \f$\vect{cp} = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$;

  where \f$n\f$ is the dimension of the space.

  \par A note on terminology.
  As observed in the Introduction, there are cases when, in order to
  represent a polyhedron \f$P\f$ using generators, we need to include
  in the finite set \f$V\f$ even points of \f$P\f$ that are <EM>not</EM>
  vertices of \f$P\f$.

  \par How to build a generator.
  Each type of generator is built by applying the corresponding
  function (<CODE>line</CODE>, <CODE>ray</CODE>, <CODE>point</CODE>
  or <CODE>closure_point</CODE>) to a linear expression,
  representing a direction in the space;
  the space-dimension of the generator is defined as the space-dimension
  of the corresponding linear expression.
  Linear expressions used to define a generator should be homogeneous
  (any constant term will be simply ignored).
  When defining points and closure points, an optional Integer argument
  can be used as a common <EM>divisor</EM> for all the coefficients
  occurring in the provided linear expression;
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
  The following code builds a line with direction \f$x-y-z\f$
  and having space-dimension \f$3\f$:
  \code
  Generator l = line(x - y - z);
  \endcode
  As mentioned above, the constant term of the linear expression
  is not relevant. Thus, the following code has the same effect:
  \code
  Generator l = line(x - y - z + 15);
  \endcode
  By definition, the origin of the space is not a line, so that
  the following code throws an exception:
  \code
  Generator l = line(0*x);
  \endcode

  \par Example 2
  The following code builds a ray with the same direction as the
  line in Example 1:
  \code
  Generator r = ray(x - y - z);
  \endcode
  As is the case for lines, when specifying a ray the constant term
  of the linear expression is not relevant; also, an exception is thrown
  when trying to build a ray from the origin of the space.

  \par Example 3
  The following code builds the point
  \f$\vect{p} = (1, 0, 2)^\transpose \in \Rset^3\f$:
  \code
  Generator p = point(1*x + 0*y + 2*z);
  \endcode
  The same effect can be obtained by using the following code:
  \code
  Generator p = point(x + 2*z);
  \endcode
  Similarly, the origin \f$\vect{0} \in \Rset^3\f$ can be defined
  using either one of the following lines of code:
  \code
  Generator origin3 = point(0*x + 0*y + 0*z);
  Generator origin3_alt = point(0*z);
  \endcode
  Note however that the following code would have defined
  a different point, namely \f$\vect{0} \in \Rset^2\f$:
  \code
  Generator origin2 = point(0*y);
  \endcode
  The following two lines of code both define the only point
  having space-dimension zero, namely \f$\vect{0} \in \Rset^0\f$.
  In the second case we exploit the fact that the first argument
  of the function <CODE>point</CODE> is optional.
  \code
  Generator origin0 = Generator::zero_dim_point();
  Generator origin0_alt = point();
  \endcode

  \par Example 4
  The point \f$\vect{p}\f$ specified in Example 3 above
  can also be obtained with the following code,
  where we provide a non-default value for the second argument
  of the function <CODE>point</CODE> (the divisor):
  \code
  Generator p = point(2*x + 0*y + 4*z, 2);
  \endcode
  Obviously, the divisor can be usefully exploited to specify
  points having some non-integer (but rational) coordinates.
  For instance, the point
  \f$\vect{q} = (-1.5, 3.2, 2.1)^\transpose \in \Rset^3\f$
  can be specified by the following code:
  \code
  Generator q = point(-15*x + 32*y + 21*z, 10);
  \endcode
  If a zero divisor is provided, an exception is thrown.

  \par Example 5
  Closures points are specified in the same way we defined points,
  but invoking their specific constructor function.
  For instance, the closure point
  \f$\vect{cp} = (1, 0, 2)^\transpose \in \Rset^3\f$ is defined by
  \code
  Generator cp = closure_point(1*x + 0*y + 2*z);
  \endcode
  For the particular case of the (only) closure point
  having space-dimension zero, we can use any of the following:
  \code
  Generator closure_origin0 = Generator::zero_dim_closure_point();
  Generator closure_origin0_alt = closure_point();
  \endcode

  \par How to inspect a generator
  Several methods are provided to examine a generator and extract
  all the encoded information: its space-dimension, its type and
  the value of its integer coefficients.

  \par Example 6
  The following code shows how it is possible to access each single
  coefficient of a generator.
  If <CODE>g1</CODE> is a point having coordinates
  \f$(a_0, \ldots, a_{n-1})^\transpose\f$,
  we construct the closure point <CODE>g2</CODE> having coordinates
  \f$(a_0, 2 a_1, \ldots, (i+1)a_i, \ldots, n a_{n-1})^\transpose\f$.
  \code
  if (g1.is_point()) {
    cout << "Point g1: " << g1 << endl;
    LinExpression e;
    for (int i = g1.space_dimension() - 1; i >= 0; i--)
      e += (i + 1) * g1.coefficient(Variable(i)) * Variable(i);
    Generator g2 = closure_point(e, g1.divisor());
    cout << "Closure point g2: " << g2 << endl;
  }
  else
    cout << "Generator g1 is not a point." << endl;
  \endcode
  Therefore, for the point
  \code
  Generator g1 = point(2*x - y + 3*z, 2);
  \endcode
  we would obtain the following output:
  \code
  Point g1: p((2*A - B + 3*C)/2)
  Closure point g2: cp((2*A - 2*B + 9*C)/2)
  \endcode
  When working with (closure) points, be careful not to confuse
  the notion of <EM>coefficient</EM> with the notion of <EM>coordinate</EM>:
  these are equivalent only when the divisor of the (closure) point is 1.
*/

class Parma_Polyhedra_Library::Generator : PPL_HIDDEN Row {
private:
  //! Builds a generator (of unspecified type) stealing
  //! the coefficients from \p e.
  Generator(LinExpression& e);

  //! Throw a <CODE>std::invalid_argument</CODE> exception
  //! containing the appropriate error message.
  //@{
  void
  throw_dimension_incompatible(const char* method, Variable v) const;
  void
  throw_invalid_argument(const char* method, const char* reason) const;
  //@}

  //! Returns the (bidirectional) line of direction \p e.
  //! \exception std::invalid_argument thrown if the homogeneous part
  //!                                  of \p e represents the origin
  //!                                  of the vector space.
  friend Generator
  Parma_Polyhedra_Library::line(const LinExpression& e);
  //! Returns the (unidirectional) ray of direction \p e.
  //! \exception std::invalid_argument thrown if the homogeneous part
  //!                                  of \p e represents the origin
  //!                                  of the vector space.
  friend Generator
  Parma_Polyhedra_Library::ray(const LinExpression& e);
  //! Returns the point at \p e / \p d.
  //! Both \p e and \p d are optional arguments, with default values
  //! LinExpression::zero() and Integer_one(), respectively.
  //! \exception std::invalid_argument thrown if \p d is zero.
  friend Generator
  Parma_Polyhedra_Library::point(const LinExpression& e
				 = LinExpression::zero(),
				 const Integer& d = Integer_one());
  //! Returns the closure point at \p e / \p d.
  //! Both \p e and \p d are optional arguments, with default values
  //! LinExpression::zero() and Integer_one(), respectively.
  //! \exception std::invalid_argument thrown if \p d is zero.
  friend Generator
  Parma_Polyhedra_Library::closure_point(const LinExpression& e
					 = LinExpression::zero(),
					 const Integer& d = Integer_one());

public:
  //! Ordinary copy-constructor.
  Generator(const Generator& g);

  //! Destructor.
  ~Generator();

  //! Assignment operator.
  Generator& operator=(const Generator& g);

  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

  //! The generator type.
  /*! Describes the type of the generator. */
  enum Type {
    /*! The generator is a line. */
    LINE,
    /*! The generator is a ray. */
    RAY,
    /*! The generator is a point. */
    POINT,
    /*! The generator is a closure point. */
    CLOSURE_POINT
  };

  //! Returns the generator type of \p *this.
  Type type() const;

  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a line.
  bool is_line() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a ray.
  bool is_ray() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a point.
  bool is_point() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a closure point.
  bool is_closure_point() const;

  //! If the index of variable \p v is less than the space-dimension
  //! of \p *this, returns the coefficient of \p v in \p *this.
  //! \exception std::invalid_argument thrown if the index of \p v
  //! is greater than or equal to the space-dimension of \p *this.
  const Integer& coefficient(Variable v) const;
  //! If \p *this is either a point or a closure point, returns its divisor.
  //! \exception std::invalid_argument thrown if \p *this is neither a point
  //! nor a closure point.
  const Integer& divisor() const;

  //! Returns the origin of the zero-dimensional space \f$\Rset^0\f$.
  static const Generator& zero_dim_point();
  //! Returns, as a closure point,
  //! the origin of the zero-dimensional space \f$\Rset^0\f$.
  static const Generator& zero_dim_closure_point();

  //! Checks if all the invariants are satisfied.
  bool OK() const;

PPL_INTERNAL:
  //! Copy-constructor with given size.
  Generator(const Generator& g, size_t sz);

  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is not a line.
  bool is_ray_or_point() const;
  //! Sets the Row kind to <CODE>LINE_OR_EQUALITY</CODE>.
  void set_is_line();
  //! Sets the Row kind to <CODE>RAY_OR_POINT_OR_INEQUALITY</CODE>.
  void set_is_ray_or_point();

  //! Returns <CODE>true</CODE> if and only if the closure point
  //! \p *this has the same \e coordinates of the point \p p.
  bool is_matching_closure_point(const Generator& p) const;

private:
  //! Default constructor: private and not implemented.
  Generator();
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::Generator& x,
	  Parma_Polyhedra_Library::Generator& y);

} // namespace std

#include "Generator.inlines.hh"

#endif
