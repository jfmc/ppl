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
  LinExpression operator +(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator +(const Integer& n, const LinExpression& e);
  LinExpression operator +(const LinExpression& e, const Integer& n);

  LinExpression operator -(const LinExpression& e);

  LinExpression operator -(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator -(const Integer& n, const LinExpression& e);
  LinExpression operator -(const LinExpression& e, const Integer& n);

  LinExpression operator *(const Integer& n, const LinExpression& e);
  LinExpression operator *(const LinExpression& e, const Integer& n);

  LinExpression& operator +=(LinExpression& e1, const LinExpression& e2);
  LinExpression& operator +=(LinExpression& e, const Variable& v);
  LinExpression& operator +=(LinExpression& e, const Integer& n);
}

//! A linear expression.
/*!
    An object of the class LinExpression represents the linear expression 
    \f[
      \sum_{i=0}^{d-1} a_i x_i + b
    \f]
    where \f$d\f$ is the dimension of the space,
    each \f$a_i\f$ is the integer coefficient
    of the \p i -th variable \f$x_i\f$
    and \f$b\f$ is the integer for the inhomogeneous term.

    Note that the ``meaning'' of an object of the class Variable
    is completely specified by the integer index given to its
    constructor:
    be careful not to be misled by C++ language variable names.
    For instance, in the following example, the linear expressions
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

    \par How to build a linear expression.

    Linear expressions are the basic blocks for defining
    both constraints (i.e., linear equalities or inequalities)
    and generators (i.e., lines, rays and vertices).
    The following functions provide a convenient interface
    for building a complex linear expression starting from simpler ones
    (or even from objects of the classes Variable and Integer).
    Available operators include unary negation,
    binary addition and subtraction,
    as well as multiplication by an Integer.

    \par Example
    The following code builds the linear expression \f$4x - 2y - z + 14\f$:
    \code
  LinExpression e = 4 * x - 2 * y - z + 14;
    \endcode
    Another way to build the same linear expression is:
    \code
  LinExpression e1 = 4 * x;
  LinExpression e2 = 2 * y;
  LinExpression e3 = z;
  LinExpression e = LinExpression(Integer(14));
  e += e1 - e2 - e3;
    \endcode
    Note that, in the second definition of linear expression <CODE>e</CODE>,
    the double type-coercion is necessary. 
*/

class Parma_Polyhedra_Library::LinExpression : PPL_INTERNAL Row {

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

  //! Returns the linear expression \p e1 + \p e2.
  friend LinExpression
  Parma_Polyhedra_Library::operator +(const LinExpression& e1,
				      const LinExpression& e2);
  
  //! Returns the linear expression \p n + \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator +(const Integer& n,
				      const LinExpression& e);
  
  //! Returns the linear expression \p e + \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator +(const LinExpression& e,
 				      const Integer& n);
  
  //! Returns the linear expression - \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e);
  
  //! Returns the linear expression \p e1 - \p e2.
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e1,
				      const LinExpression& e2);
  
  //! Returns the linear expression \p n - \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const Integer& n,
				      const LinExpression& e);
  
  //! Returns the linear expression \p e - \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator -(const LinExpression& e,
				      const Integer& n);

  //! Returns the linear expression \p n * \p e.
  friend LinExpression
  Parma_Polyhedra_Library::operator *(const Integer& n,
				      const LinExpression& e);
  
  //! Returns the linear expression \p e * \p n.
  friend LinExpression
  Parma_Polyhedra_Library::operator *(const LinExpression& e,
				      const Integer& n);

  //! Returns the linear expression \p e1 + \p e2 and assigns it to \p e1.
  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e1,
				       const LinExpression& e2);

  //! Returns the linear expression \p e + \p v and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e,
				       const Variable& v);

  //! Returns the linear expression \p e + \p n and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator +=(LinExpression& e,
                                       const Integer& n);
};

#if !OUTLINE
#include "LinExpression.inlines.hh"
#endif

#endif




