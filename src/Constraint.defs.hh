/* Constraint class declaration.
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

#ifndef _Constraint_defs_hh
#define _Constraint_defs_hh 1

#include "Constraint.types.hh"
#include "Row.defs.hh"
#include "Variable.defs.hh"
#include "LinExpression.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

  //! @name Non-friend operators on objects of the class Constraint.
  //@{
  //! Output operator.
  std::ostream& operator <<(std::ostream& s, const Constraint& c);
  //@}

  // Put them in the namespace here to declare them friend later.
  Constraint operator ==(const LinExpression& e1, const LinExpression& e2);
  Constraint operator ==(const LinExpression& e, const Integer& n);
  Constraint operator ==(const Integer& n, const LinExpression& e);

  Constraint operator <=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator <=(const LinExpression& e, const Integer& n);
  Constraint operator <=(const Integer& n, const LinExpression& e);

  Constraint operator >=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator >=(const LinExpression& e, const Integer& n);
  Constraint operator >=(const Integer& n, const LinExpression& e);

  Constraint operator >>(const Constraint& c, unsigned int offset);
}

//! A linear equality or inequality.
/*!
  An object of the class Constraint is either:
  - an equality: \f$\sum_{i=0}^{n-1} a_i x_i + b = 0\f$; or
  - an inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b \geq 0\f$;

  where \f$n\f$ is the dimension of the space.

  \par How to build a constraint
  Constraints are typically built by applying a relational operator
  to a pair of linear expressions.
  Available relational operators include equality (<CODE>==</CODE>)
  and non-strict inequalities (<CODE>>=</CODE> and <CODE><=</CODE>).
  Strict inequalities (<CODE><</CODE> and <CODE>></CODE>)
  are not supported.

  \par
  In the following example it is assumed that variables
  <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE>
  are defined as follows:
  \code
  Variable x(0);
  Variable y(1);
  Variable z(2);
  \endcode

  \par Example
  The following code builds the equality \f$3x + 5y - z = 0\f$:
  \code
  Constraint equal(3*x + 5*y - z == 0);
  \endcode
  The following code builds the constraint \f$4x - 2y \geq z - 13\f$:
  \code
  Constraint inequal(4*x - 2*y >= z - 13);
  \endcode
  The unsatisfiable constraint on the zero-dimension space \f$\Rset^0\f$
  can be specified as follows:
  \code
  Constraint c_false(LinExpression::zero == 1);
  \endcode
*/
class Parma_Polyhedra_Library::Constraint : PPL_INTERNAL Row {
private:
  //! Default constructor: private and not implemented.
  Constraint();

  Constraint(LinExpression& e);

  Constraint(Row::Type type, size_t size);

  //! Returns the constraint \p e1 = \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator ==(const LinExpression& e1,
				       const LinExpression& e2);

  //! Returns the constraint \p e = \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator ==(const LinExpression& e,
				       const Integer& n);

  //! Returns the constraint \p n = \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator ==(const Integer& n,
				       const LinExpression& e);

  //! Returns the constraint \p e1 >= \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator >=(const LinExpression& e1,
				       const LinExpression& e2);

  //! Returns the constraint \p e >= \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator >=(const LinExpression& e,
				       const Integer& n);

  //! Returns the constraint \p n >= \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator >=(const Integer& n,
				       const LinExpression& e);

  //! Returns the constraint \p e1 <= \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator <=(const LinExpression& e1,
				       const LinExpression& e2);

  //! Returns the constraint \p e <= \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator <=(const LinExpression& e,
				       const Integer& n);

  //! Returns the constraint \p n <= \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator <=(const Integer& n,
				       const LinExpression& e);

  //! Returns the constraint \p c with variables renamed
  //! by adding \p offset to their Cartesian axis identifier.
  friend Constraint
  Parma_Polyhedra_Library::operator >>(const Constraint& c,
				       unsigned int offset);

public:
  //! Ordinary copy-constructor.
  Constraint(const Constraint& c);
  //! Destructor.
  ~Constraint();

  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an equality constraint.
  bool is_equality() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an inequality constraint.
  bool is_inequality() const;

  //! Returns the last variable in the space of \p *this.
  Variable last_variable() const;
  //! Returns the coefficient of \p v in \p *this.
  const Integer& coefficient(Variable v) const;
  //! Returns the inhomogeneous term of \p *this.
  const Integer& coefficient() const;

  //! The unsatisfiable zero-dim constraint 0 = 1.
  static const Constraint& zero_dim_false();

PPL_INTERNAL:
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a non trivial constraint.
  bool is_nontrivial() const;

  enum Type {
    EQUALITY = Row::LINE_OR_EQUALITY,
    INEQUALITY = Row::RAY_OR_VERTEX_OR_INEQUALITY
  };

  //! Returns the constraint type of \p *this.
  Type type() const;
  //! Sets the constraint type to <CODE>EQUALITY</CODE>.
  void set_is_equality();
  //! Sets the constraint type to <CODE>INEQUALITY</CODE>.
  void set_is_inequality();
};

#include "Constraint.inlines.hh"

#endif



