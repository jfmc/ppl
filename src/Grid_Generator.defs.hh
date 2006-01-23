/* Grid_Generator class declaration.
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

#ifndef PPL_Grid_Generator_defs_hh
#define PPL_Grid_Generator_defs_hh 1

#include "Grid_Generator.types.hh"
#include "Grid_Generator_System.defs.hh"
#include "Generator.defs.hh"
#include "Grid.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

// Put these in the namespace here to declare them friend later.

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Grid_Generator */
std::ostream& operator<<(std::ostream& s, const Grid_Generator& g);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Grid_Generator */
void swap(Parma_Polyhedra_Library::Grid_Generator& x,
	  Parma_Polyhedra_Library::Grid_Generator& y);

} // namespace std

// FIXME: Update this to grids.
//! A line, parameter or point.
/*!
  An object of the class Grid_Generator is one of the following:

  - a line \f$\vect{l} = (a_0, \ldots, a_{n-1})^\transpose\f$;

  - a parameter \f$\vect{r} = (a_0, \ldots, a_{n-1})^\transpose\f$;

  - a point
    \f$\vect{p} = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$;

  where \f$n\f$ is the dimension of the space
  and, for points and parameters, \f$d > 0\f$ is the divisor.

  \par A note on terminology.
  As observed in Section \ref representation, there are cases when,
  in order to represent a polyhedron \f$\cP\f$ using the generator system
  \f$\cG = (L, R, P, C)\f$, we need to include in the finite set
  \f$P\f$ even points of \f$\cP\f$ that are <EM>not</EM> vertices
  of \f$\cP\f$.
  This situation is even more frequent when working with NNC polyhedra
  and it is the reason why we prefer to use the word `point'
  where other libraries use the word `vertex'.

  \par How to build a generator.
  Each type of generator is built by applying the corresponding
  function (<CODE>line</CODE>, <CODE>ray</CODE>, <CODE>point</CODE>
  or <CODE>closure_point</CODE>) to a linear expression,
  representing a direction in the space;
  the space dimension of the generator is defined as the space dimension
  of the corresponding linear expression.
  Linear expressions used to define a generator should be homogeneous
  (any constant term will be simply ignored).
  When defining points and closure points, an optional Coefficient argument
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
  and having space dimension \f$3\f$:
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
  having space dimension zero, namely \f$\vect{0} \in \Rset^0\f$.
  In the second case we exploit the fact that the first argument
  of the function <CODE>point</CODE> is optional.
  \code
  Generator origin0 = Generator::zero_dim_point();
  Generator origin0_alt = grid_point();
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
  Closure points are specified in the same way we defined points,
  but invoking their specific constructor function.
  For instance, the closure point
  \f$\vect{c} = (1, 0, 2)^\transpose \in \Rset^3\f$ is defined by
  \code
  Generator c = closure_point(1*x + 0*y + 2*z);
  \endcode
  For the particular case of the (only) closure point
  having space dimension zero, we can use any of the following:
  \code
  Generator closure_origin0 = Generator::zero_dim_closure_point();
  Generator closure_origin0_alt = closure_point();
  \endcode

  \par How to inspect a generator
  Several methods are provided to examine a generator and extract
  all the encoded information: its space dimension, its type and
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
    Linear_Expression e;
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
class Parma_Polyhedra_Library::Grid_Generator : private Generator {
public:
  // FIXME: Add wrappers of any other public Generator methods.

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! The generator type.
  enum Type {
    /*! The generator is a line. */
    LINE,
    /*! The generator is a parameter. */
    PARAMETER,
    /*! The generator is a point. */
    POINT
  };

  //! Returns the generator type of \p *this.
  Type type() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a line.
  bool is_line() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a parameter.
  bool is_parameter() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a line or a parameter.
  bool is_line_or_parameter() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a point.
  bool is_point() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this row represents a
    parameter or a point.
  */
  bool is_parameter_or_point() const;

  //! Returns the line of direction \p e.
  /*!
    \exception std::invalid_argument
    Thrown if the homogeneous part of \p e represents the origin of
    the vector space.
  */
  static Grid_Generator line(const Linear_Expression& e);

  //! Returns the parameter at \p e.
  /*!
    Both \p e and \p d are optional arguments, with default values
    Linear_Expression::zero() and Coefficient_one(), respectively.

    \exception std::invalid_argument
    Thrown if \p d is zero.
  */
  static Grid_Generator parameter(const Linear_Expression& e
				  = Linear_Expression::zero(),
				  Coefficient_traits::const_reference d
				  = Coefficient_one());

  //! Returns the point at \p e / \p d.
  /*!
    Both \p e and \p d are optional arguments, with default values
    Linear_Expression::zero() and Coefficient_one(), respectively.

    \exception std::invalid_argument
    Thrown if \p d is zero.
  */
  static Grid_Generator point(const Linear_Expression& e
			      = Linear_Expression::zero(),
			      Coefficient_traits::const_reference d
			      = Coefficient_one());

  //! Assignment operator.
  Grid_Generator& operator=(const Grid_Generator& g);

  //! Assignment operator.
  Grid_Generator& operator=(const Generator& g);

  //! Returns the divisor of \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this is a line.
  */
  Coefficient_traits::const_reference divisor() const;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y are
    equivalent generators.

    Generators having different space dimensions are not equivalent.
  */
  bool is_equivalent_to(const Grid_Generator& y) const;

  //! Returns <CODE>true</CODE> if \p *this is exactly equal to \p y.
  bool is_equal_to(const Grid_Generator& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if all the homogeneous terms
    of \p *this are \f$0\f$.
  */
  bool all_homogeneous_terms_are_zero() const;

  /*! \brief
    Scales \p *this to be represented with a divisor of \p d (if
    \*this is a parameter or point).

    \exception std::invalid_argument
    Thrown if \p d is zero.
  */
  void scale_to_divisor(Coefficient_traits::const_reference d);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! Swaps \p *this with \p y.
  void swap(Grid_Generator& y);

  /*! \brief
    Swaps \p *this with \p y, leaving \p *this with the original
    capacity.

    All up to and including the last element of the smaller of \p
    *this and \p y are swapped.  The parameter divisor element of \p y
    is swapped with the divisor element of \p *this.
  */
  void coefficient_swap(Grid_Generator& y);

  PPL_OUTPUT_DECLARATIONS;

  //! Uses the ASCII representation from \p s to recreate *this.
  /*!
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE>
    otherwise.  The ASCII representation is as output by \ref
    ascii_dump.
  */
  bool ascii_load(std::istream& s);

private:
  /*! \brief
    Constructs from polyhedron generator \p g, stealing the underlying
    data structures from \p g.

    The last column in \p g becomes the parameter divisor column of
    the new Grid_Generator.
  */
  explicit Grid_Generator(Generator g);

  //! Returns the topological kind of \p *this.
  Topology topology() const;

  //! Returns the actual size of \p this.
  dimension_type size() const;

  //! Negates the elements from index \p start to index \p end.
  void negate(dimension_type start, dimension_type end);

  //! Returns the divisor of \p *this.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this is a line.
  */
  Coefficient& divisor();

  //! Sets the Linear_Row kind to <CODE>LINE_OR_EQUALITY</CODE>.
  void set_is_line();

  //! Sets the Linear_Row kind to <CODE>RAY_OR_POINT_OR_INEQUALITY</CODE>.
  void set_is_parameter();

  /*! \brief
    Strong normalization: ensures that different Grid_Generator
    objects represent different hyperplanes or hyperspaces.

    Applies both Linear_Row::normalize() and Linear_Row::sign_normalize().

    This is simply a wrapper around the Generator::strong_normalize,
    which means applying it to a parameter may change the parameter.
  */
  void strong_normalize();

  //! Returns a reference to the element of the row indexed by \p k.
  Coefficient& operator[](dimension_type k);

  //! Returns a constant reference to the element of the row indexed by \p k.
  Coefficient_traits::const_reference operator[](dimension_type k) const;

  /*! \brief
    Throw a <CODE>std::invalid_argument</CODE> exception containing
    the appropriate error message.
  */
  void
  throw_invalid_argument(const char* method, const char* reason) const;

  friend std::ostream&
  IO_Operators::operator<<(std::ostream& s, const Grid_Generator& g);
  // FIXME: The following friend declaration is for operator[],
  //        divisor() and negate() access in Grid::conversion and
  //        Grid::simplify.
  friend class Grid;

  friend class Grid_Generator_System;
  friend class Grid_Generator_System::const_iterator;
  friend class Congruence_System;
  friend class Scalar_Products;
  friend class Topology_Adjusted_Scalar_Product_Sign;
};


namespace Parma_Polyhedra_Library {

/*! \brief
  Shorthand for Grid_Generator
  Grid_Generator::line(const Linear_Expression& e).
*/
/*! \relates Grid_Generator */
Grid_Generator grid_line(const Linear_Expression& e);

/*! \brief
  Shorthand for Grid_Generator
  Grid_Generator::parameter(const Linear_Expression& e,
  Coefficient_traits::const_reference d).
*/
/*! \relates Grid_Generator */
Grid_Generator
parameter(const Linear_Expression& e = Linear_Expression::zero(),
	  Coefficient_traits::const_reference d = Coefficient_one());

/*! \brief
  Shorthand for Grid_Generator
  Grid_Generator::point(const Linear_Expression& e,
  Coefficient_traits::const_reference d).
*/
/*! \relates Grid_Generator */
Grid_Generator
grid_point(const Linear_Expression& e = Linear_Expression::zero(),
	   Coefficient_traits::const_reference d = Coefficient_one());

//! Returns <CODE>true</CODE> if and only if \p x is equivalent to \p y.
/*! \relates Grid_Generator */
bool operator==(const Grid_Generator& x, const Grid_Generator& y);

//! Returns <CODE>true</CODE> if and only if \p x is not equivalent to \p y.
/*! \relates Grid_Generator */
bool operator!=(const Grid_Generator& x, const Grid_Generator& y);


namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Grid_Generator */
std::ostream& operator<<(std::ostream& s, const Grid_Generator::Type& t);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "Grid_Generator.inlines.hh"

#endif // !defined(PPL_Grid_Generator_defs_hh)
