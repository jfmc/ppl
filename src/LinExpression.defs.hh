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

#ifndef PPL_LinExpression_defs_hh
#define PPL_LinExpression_defs_hh 1

#include "LinExpression.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Row.defs.hh"
#include "Integer.types.hh"
#include "Variable.types.hh"
#include "ConSys.types.hh"
#include "GenSys.types.hh"
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

LinExpression& operator-=(LinExpression& e1, const LinExpression& e2);
LinExpression& operator-=(LinExpression& e, const Variable& v);
LinExpression& operator-=(LinExpression& e, const Integer& n);

LinExpression& operator*=(LinExpression& e, const Integer& n);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::LinExpression */
void swap(Parma_Polyhedra_Library::LinExpression& x,
	  Parma_Polyhedra_Library::LinExpression& y);

} // namespace std

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
class Parma_Polyhedra_Library::LinExpression : private Row {
public:
  //! Default constructor: returns a copy of LinExpression::zero().
  LinExpression();

  //! Ordinary copy-constructor.
  LinExpression(const LinExpression& e);

  //! Destructor.
  virtual ~LinExpression();

  //! \brief
  //! Builds the linear expression corresponding
  //! to the inhomogeneous term \p n.
  explicit LinExpression(const Integer& n);

  //! \brief
  //! Builds the linear expression corresponding
  //! to the variable \p v.
  LinExpression(const Variable& v);

  //! Builds the linear expression corresponding to constraint \p c.
  /*!
    Given the constraint
    \f$c = \bigl(\sum_{i=0}^{n-1} a_i x_i + b \relop 0\bigr)\f$,
    where \f$\mathord{\relop} \in \{ =, \geq, > \}\f$,
    builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i + b\f$.
    If \p c is an inequality (resp., equality) constraint, then
    the built linear expression is unique up to a positive
    (resp., non-zero) factor.
  */
  explicit LinExpression(const Constraint& c);

  //! \brief
  //! Builds the linear expression corresponding to generator \p g
  //! (for points and closure points, the divisor is not copied).
  /*!
    Given the generator
    \f$g = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$
    (where, for lines and rays, we have \f$d = 1\f$),
    builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i\f$.
    The inhomogeneous term of the linear expression will always be 0.
    If \p g is a ray, point or closure point (resp., a line), then
    the linear expression is unique up to a positive
    (resp., non-zero) factor.
  */
  explicit LinExpression(const Generator& g);

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the coefficient of \p v in \p *this.
  const Integer& coefficient(Variable v) const;

  //! Returns the inhomogeneous term of \p *this.
  const Integer& inhomogeneous_term() const;

  //! Returns the (zero-dimension space) constant 0.
  static const LinExpression& zero();

private:
  friend class Parma_Polyhedra_Library::Constraint;
  friend class Parma_Polyhedra_Library::Generator;
  friend class Parma_Polyhedra_Library::Polyhedron;

  // FIXME: the following friend declaration is only to grant access to
  // ConSys::affine_preimage().
  friend class Parma_Polyhedra_Library::ConSys;

  // FIXME: the following friend declaration is only to grant access to
  // GenSys::affine_image().
  friend class Parma_Polyhedra_Library::GenSys;

  friend void std::swap(Parma_Polyhedra_Library::LinExpression& x,
		       Parma_Polyhedra_Library::LinExpression& y);

  //! Copy-constructor with a specified dimension.
  LinExpression(const LinExpression& e, dimension_type sz);

  //! Implementation sizing constructor.
  /*!
    The bool parameter is just to avoid problems with
    the constructor LinExpression(const Integer& n).
  */
  LinExpression(dimension_type sz, bool);

  //! Swaps \p *this with \p y.
  void swap(LinExpression& y);

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

  //! Returns the linear expression \p e1 - \p e2 and assigns it to \p e1.
  friend LinExpression&
  Parma_Polyhedra_Library::operator-=(LinExpression& e1,
				      const LinExpression& e2);

  //! Returns the linear expression \p e - \p v and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator-=(LinExpression& e,
				      const Variable& v);

  //! Returns the linear expression \p e - \p n and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator-=(LinExpression& e,
				      const Integer& n);

  //! Returns the linear expression \p n * \p e and assigns it to \p e.
  friend LinExpression&
  Parma_Polyhedra_Library::operator*=(LinExpression& e,
				      const Integer& n);
};

#include "LinExpression.inlines.hh"

#endif // !defined(PPL_LinExpression_defs_hh)
