/* LinExpression class declaration.
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

#ifndef _LinExpression_defs_hh
#define _LinExpression_defs_hh 1

#include "LinExpression.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Row.defs.hh"
#include "Integer.types.hh"
#include "Variable.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Polyhedron.types.hh"
#include <cstddef>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friend later.
  LinExpression operator+(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator+(const Integer& n, const LinExpression& e);
  LinExpression operator+(const LinExpression& e, const Integer& n);

  LinExpression operator-(const LinExpression& e);

  LinExpression operator-(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator-(const Integer& n, const LinExpression& e);
  LinExpression operator-(const LinExpression& e, const Integer& n);

  LinExpression operator*(const Integer& n, const LinExpression& e);
  LinExpression operator*(const LinExpression& e, const Integer& n);

  LinExpression& operator+=(LinExpression& e1, const LinExpression& e2);
  LinExpression& operator+=(LinExpression& e, const Variable& v);
  LinExpression& operator+=(LinExpression& e, const Integer& n);
}

//! A linear expression.
/*!
    An object of the class LinExpression represents the linear expression
    \f[
      \sum_{i=0}^{n-1} a_i x_i + b
    \f]
    where \f$n\f$ is the dimension of the space,
    each \f$a_i\f$ is the integer coefficient
    of the \p i -th variable \f$x_i\f$
    and \f$b\f$ is the integer for the inhomogeneous term.

    \par How to build a linear expression.

    Linear expressions are the basic blocks for defining
    both constraints (i.e., linear equalities or inequalities)
    and generators (i.e., lines, rays, points and closure points).
    A full set of functions is defined to provide a convenient interface
    for building complex linear expressions starting from simpler ones
    and from objects of the classes Variable and Integer:
    available operators include unary negation,
    binary addition and subtraction,
    as well as multiplication by an Integer.
    The space-dimension of a linear expression is defined as the maximum
    space-dimension of the arguments used to build it:
    in particular, the space-dimension of a Variable <CODE>x</CODE>
    is defined as <CODE>x.id()+1</CODE>,
    whereas all the objects of the class Integer have space-dimension zero.

    \par Example
    The following code builds the linear expression \f$4x - 2y - z + 14\f$,
    having space-dimension \f$3\f$:
    \code
  LinExpression e = 4*x - 2*y - z + 14;
    \endcode
    Another way to build the same linear expression is:
    \code
  LinExpression e1 = 4*x;
  LinExpression e2 = 2*y;
  LinExpression e3 = z;
  LinExpression e = LinExpression(14);
  e += e1 - e2 - e3;
    \endcode
    Note that \p e1, \p e2 and \p e3 have space-dimension 1, 2 and 3,
    respectively; also, in the fourth line of code, \p e is created
    with space-dimension zero and then extended to space-dimension 3.
*/

class Parma_Polyhedra_Library::LinExpression : PPL_HIDDEN Row {
public:
  //! Default constructor: returns a copy of LinExpression::zero().
  LinExpression();

  //! Ordinary copy-constructor.
  LinExpression(const LinExpression& e);

  //! Destructor.
  virtual ~LinExpression();

  //! Builds the linear expression corresponding
  //! to the inhomogeneous term \p n.
  explicit LinExpression(const Integer& n);

  //! Builds the linear expression corresponding
  //! to the variable \p v.
  LinExpression(const Variable& v);

  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

  //! Returns the (zero-dimension space) constant 0.
  static const LinExpression& zero();

private:
  friend class Parma_Polyhedra_Library::Constraint;
  friend class Parma_Polyhedra_Library::Generator;
  friend class Parma_Polyhedra_Library::Polyhedron;

  //! Builds the linear expression corresponding to
  //! the constraint \p c.
  explicit LinExpression(const Constraint& c);

  //! Builds the linear expression corresponding to
  //! the coefficients of generator \p g. Note that,
  //! for points and closure points, the divisor is \e not copied.
  explicit LinExpression(const Generator& g);

  //! Copy-constructor with a specified dimension.
  LinExpression(const LinExpression& e, size_t sz);

  //! Implementation sizing constructor.
  //! The bool parameter is just to avoid problems with
  //! the constructor LinExpression(const Integer& n).
  LinExpression(size_t sz, bool);

  //! Returns the linear expression \p e1 + \p e2.
  friend LinExpression
  Parma_Polyhedra_Library::operator+(const LinExpression& e1,
				     const LinExpression& e2);

  //! Returns the linear expression \p n + \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator+(const Integer& n,
				     const LinExpression& e);

  //! Returns the linear expression \p e + \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator+(const LinExpression& e,
				     const Integer& n);

  //! Returns the linear expression - \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator-(const LinExpression& e);

  //! Returns the linear expression \p e1 - \p e2.
  friend LinExpression
  Parma_Polyhedra_Library::operator-(const LinExpression& e1,
				     const LinExpression& e2);

  //! Returns the linear expression \p n - \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator-(const Integer& n,
				     const LinExpression& e);

  //! Returns the linear expression \p e - \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator-(const LinExpression& e,
				     const Integer& n);

  //! Returns the linear expression \p n * \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator*(const Integer& n,
				     const LinExpression& e);

  //! Returns the linear expression \p e * \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator*(const LinExpression& e,
				     const Integer& n);

  //! Returns the linear expression \p e1 + \p e2 and assigns it to \p e1.
  friend LinExpression&
  Parma_Polyhedra_Library::operator+=(LinExpression& e1,
				      const LinExpression& e2);

  //! Returns the linear expression \p e + \p v and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator+=(LinExpression& e,
				      const Variable& v);

  //! Returns the linear expression \p e + \p n and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator+=(LinExpression& e,
				      const Integer& n);
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::LinExpression& x,
	  Parma_Polyhedra_Library::LinExpression& y);

} // namespace std

#include "LinExpression.inlines.hh"

#endif // _LinExpression_defs_hh
