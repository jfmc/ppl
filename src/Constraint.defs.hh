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
#include "LinExpression.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friend later.

  /*! @name How to build a constraint.
    Constraints are typically built by applying a relational operator
    to a pair of linear expressions.
    Available relational operators include equality (<CODE>==</CODE>)
    and non-strict inequalities (<CODE>>=</CODE> and <CODE><=</CODE>).
    Strict inequalities (<CODE><</CODE> and <CODE>></CODE>)
    are not supported.
    
    \par Example
    The following code builds the equality \f$3x + 5y - z = 0\f$:
    \code
  Constraint equal(3 * x + 5 * y - z == 0);
    \endcode
    The following code builds the constraint \f$4x - 2y \geq z - 13\f$:
    \code
  Constraint inequal(4 * x - 2 * y >= z - 13);
    \endcode

  (This spare line is added just to prevent a doxygen bug).
   */
  //@{
  //! Returns the constraint \p e1 = \p e2.
  Constraint operator ==(const LinExpression& e1, const LinExpression& e2);
  //! Returns the constraint \p e = \p n.
  Constraint operator ==(const LinExpression& e, const Integer& n);
  //! Returns the constraint \p n = \p e.
  Constraint operator ==(const Integer& n, const LinExpression& e);

  //! Returns the constraint \p e1 <= \p e2.
  Constraint operator <=(const LinExpression& e1, const LinExpression& e2);
  //! Returns the constraint \p e <= \p n.
  Constraint operator <=(const LinExpression& e, const Integer& n);
  //! Returns the constraint \p n <= \p e.
  Constraint operator <=(const Integer& n, const LinExpression& e);

  //! Returns the constraint \p e1 >= \p e2.
  Constraint operator >=(const LinExpression& e1, const LinExpression& e2);
  //! Returns the constraint \p e >= \p n.
  Constraint operator >=(const LinExpression& e, const Integer& n);
  //! Returns the constraint \p n >= \p e.
  Constraint operator >=(const Integer& n, const LinExpression& e);

  //! Returns the constraint \p c with variables renamed by \p offset.
  Constraint operator >>(const Constraint& c, unsigned int offset);

  //! Output operator.
  std::ostream& operator <<(std::ostream& s, const Constraint& c);
  //@}
}

//! A linear equality or inequality.
/*! An object of the class Constraint is either:
      - an equality: \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$;
      - an inequality: \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$;

    where \f$d\f$ is the dimension of the space.
*/
class Parma_Polyhedra_Library::Constraint : public Row {
private:
  Constraint(LinExpression& e);

  friend Constraint
  Parma_Polyhedra_Library::operator ==(const LinExpression& e1,
				       const LinExpression& e2);
  friend Constraint
  Parma_Polyhedra_Library::operator ==(const LinExpression& e,
				       const Integer& n);
  friend Constraint
  Parma_Polyhedra_Library::operator ==(const Integer& n,
				       const LinExpression& e);

  friend Constraint
  Parma_Polyhedra_Library::operator >=(const LinExpression& e1,
				       const LinExpression& e2);
  friend Constraint
  Parma_Polyhedra_Library::operator >=(const LinExpression& e,
				       const Integer& n); 
  friend Constraint
  Parma_Polyhedra_Library::operator >=(const Integer& n,
				       const LinExpression& e);

  friend Constraint
  Parma_Polyhedra_Library::operator <=(const LinExpression& e1,
				       const LinExpression& e2);
  friend Constraint
  Parma_Polyhedra_Library::operator <=(const LinExpression& e,
				       const Integer& n);
  friend Constraint
  Parma_Polyhedra_Library::operator <=(const Integer& n,
				       const LinExpression& e);

  friend Constraint
  Parma_Polyhedra_Library::operator >>(const Constraint& c, unsigned int p);

  Constraint(Row::Type type, size_t size);

public:
  //! Default constructor.
  Constraint();
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

#if !OUTLINE
#include "Constraint.inlines.hh"
#endif

#endif



