/* LinExpression class declaration.
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

#ifndef _LinExpression_defs_hh
#define _LinExpression_defs_hh 1

#include "LinExpression.types.hh"
#include "Row.defs.hh"
#include "Variable.types.hh"
#include <cstddef>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friend later.

  /*! \addtogroup LinExpression Linear Expressions

    Linear expressions are the blocks used for building both
    constraints (i.e., linear equalities or inequalities)
    and generators (i.e., lines, rays and vertices).
    Linear expressions are recursively built from
    variables and integer terms by applying the usual
    operators, i.e., unary negation, binary addition and subtraction,
    as well as multiplication by an integer.

    @{
  */
  //! Returns the linear expression \p e1 + \p e2.
  LinExpression operator +(const LinExpression& e1, const LinExpression& e2);
  //! Returns the linear expression \p n + \p e.
  LinExpression operator +(const Integer& n, const LinExpression& e);
  //! Returns the linear expression \p e + \p n.
  LinExpression operator +(const LinExpression& e, const Integer& n);

  //! Returns the linear expression - \p e.
  LinExpression operator -(const LinExpression& e);

  //! Returns the linear expression \p e1 - \p e2.
  LinExpression operator -(const LinExpression& e1, const LinExpression& e2);
  //! Returns the linear expression \p n - \p e.
  LinExpression operator -(const Integer& n, const LinExpression& e);
  //! Returns the linear expression \p e - \p n.
  LinExpression operator -(const LinExpression& e, const Integer& n);

  //! Returns the linear expression \p n * \p e.
  LinExpression operator *(const Integer& n, const LinExpression& e);
  //! Returns the linear expression \p e * \p n.
  LinExpression operator *(const LinExpression& e, const Integer& n);

  //! Returns the linear expression \p e1 + \p e2 and assigns it to \p e1.
  LinExpression& operator +=(LinExpression& e1, const LinExpression& e2);
  //! Returns the linear expression \p e + \p v and assigns it to \p e.
  LinExpression& operator +=(LinExpression& e, const Variable& v);
  //! Returns the linear expression \p e + \p n and assigns it to \p e.
  LinExpression& operator +=(LinExpression& e, const Integer& n);

  /*!
    @}
  */
}

//! A linear expression.
/*!
  \ingroup LinExpression
  An object of the class LinExpression represents the linear expression 
  \f[
    \sum_{i=0}^{d-1} a_i x_i + b
  \f]
  where \p d is the dimension of the space,
  each \f$a_i\f$ is the integer coefficient
  of the \p i -th variable \f$x_i\f$
  and \p b is the integer inhomogeneous term.

  \par Example
  The following code builds a linear expression \f$4x-2y-z+14\f$ :
  \code
  Variable x(0);
  Variable y(1);
  Variable z(5);
  LinExpression e = 4 * x - 2 * y - z + 14;
  \endcode
  Another way to build the same linear expression is:
  \code
  Variable x(0);
  Variable y(1);
  Variable z(5);
  LinExpression e = 4 * x;
  LinExpression e1 = -2 * y;
  LinExpression e2 = -z;
  e += e1 + e2 + 14;
  \endcode
  Note that the ``meaning'' of an object of the class Variable
  is completely specified by the integer index provided to its
  constructor:
  be careful not to be mislead by C++ language variable names.
  For instance, in the following example the linear expressions
  <CODE>e1</CODE> and <CODE>e2</CODE> are equivalent,
  since the two variables <CODE>x</CODE> and <CODE>z</CODE> denote
  the same Cartesian axis.
  \code
  Variable x(0);
  Variable y(1);
  Variable z(0);
  LinExpression e1 = x + y;
  LinExpression e2 = y + z;
  \endcode
*/

class Parma_Polyhedra_Library::LinExpression : private Row {
/*!
  \addtogroup LinExpression Linear Expressions
  @{
*/
public:
  //! Default constructor.
  LinExpression();
  //! Ordinary copy-constructor.
  LinExpression(const LinExpression& e);
  //! Destructor.
  virtual ~LinExpression();

  //! Constructor: builds a linear expression with as many as
  //! \p num_dimension variables.
  explicit LinExpression(size_t num_dimensions);
  //! Constructor: builds the linear expression corresponding
  //! to the inhomogeneous term \p n.
  explicit LinExpression(const Integer& n);
  //! Constructor: builds the linear expression corresponding
  //! to the variable \p var.
  LinExpression(const Variable& var);

private:
  //! Implementation sizing constructor: the bool parameter is just to
  //! distinguish it from the public constructor with the space dimension.
  explicit LinExpression(size_t size, bool);
  //! Copy-constructor with a specified dimension.
  LinExpression(const LinExpression& e, size_t size);

  friend class Constraint;
  friend class Generator;

  friend LinExpression
  Parma_Polyhedra_Library::operator +(const LinExpression& e1,
				      const LinExpression& e2);
  friend LinExpression
  Parma_Polyhedra_Library::operator +(const Integer& n,
				      const LinExpression& e);
  friend LinExpression
  Parma_Polyhedra_Library::operator +(const LinExpression& e,
 				      const Integer& n);
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e);
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e1,
				      const LinExpression& e2);
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const Integer& n,
				      const LinExpression& e);
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e,
				      const Integer& n);
  friend LinExpression
  Parma_Polyhedra_Library::operator *(const Integer& n,
				      const LinExpression& e);
  friend LinExpression
  Parma_Polyhedra_Library::operator *(const LinExpression& e,
				      const Integer& n);

  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e1,
				       const LinExpression& e2);
  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e,
				       const Variable& v);
  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e,
                                       const Integer& n);
/*!
  @}
*/
};

#if !OUTLINE
#include "LinExpression.inlines.hh"
#endif

#endif




