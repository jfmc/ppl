/* Constraint class declaration.
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

#ifndef _Constraint_defs_hh
#define _Constraint_defs_hh 1

#include "Constraint.types.hh"
#include "Row.defs.hh"
#include "Variable.defs.hh"
#include "LinExpression.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

  //! Output operator.
  std::ostream& operator<<(std::ostream& s, const Constraint& c);

  // Put them in the namespace here to declare them friend later.
  Constraint operator==(const LinExpression& e1, const LinExpression& e2);
  Constraint operator==(const LinExpression& e, const Integer& n);
  Constraint operator==(const Integer& n, const LinExpression& e);

  Constraint operator<=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator<=(const LinExpression& e, const Integer& n);
  Constraint operator<=(const Integer& n, const LinExpression& e);

  Constraint operator>=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator>=(const LinExpression& e, const Integer& n);
  Constraint operator>=(const Integer& n, const LinExpression& e);

  Constraint operator<(const LinExpression& e1, const LinExpression& e2);
  Constraint operator<(const LinExpression& e, const Integer& n);
  Constraint operator<(const Integer& n, const LinExpression& e);

  Constraint operator>(const LinExpression& e1, const LinExpression& e2);
  Constraint operator>(const LinExpression& e, const Integer& n);
  Constraint operator>(const Integer& n, const LinExpression& e);

  Constraint operator>>(const Constraint& c, unsigned int offset);
}

//! A linear equality or inequality.
/*!
  An object of the class Constraint is either:
  - an equality: \f$\sum_{i=0}^{n-1} a_i x_i + b = 0\f$; or
  - a non-strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b \geq 0\f$;
  - a strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b > 0\f$;

  where \f$n\f$ is the dimension of the space,
  \f$a_i\f$ is the integer coefficient of variable \f$x_i\f$
  and \f$b\f$ is the integer inhomogeneous term.

  \par How to build a constraint
  Constraints are typically built by applying a relational operator
  to a pair of linear expressions.
  Available relational operators include equality (<CODE>==</CODE>),
  non-strict inequalities (<CODE>>=</CODE> and <CODE><=</CODE>) and
  strict inequalities (<CODE><</CODE> and <CODE>></CODE>).
  The space-dimension of a constraint is defined as the maximum
  space-dimension of the arguments of its constructor.

  \par
  In the following examples it is assumed that variables
  <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE>
  are defined as follows:
  \code
  Variable x(0);
  Variable y(1);
  Variable z(2);
  \endcode

  \par Example 1
  The following code builds the equality constraint
  \f$3x + 5y - z = 0\f$, having space-dimension \f$3\f$:
  \code
  Constraint eq_c(3*x + 5*y - z == 0);
  \endcode
  The following code builds the (non-strict) inequality constraint
  \f$4x \geq 2y - 13\f$, having space-dimension \f$2\f$:
  \code
  Constraint ineq_c(4*x >= 2*y - 13);
  \endcode
  The corresponding strict inequality constraint
  \f$4x > 2y - 13\f$ is obtained as follows:
  \code
  Constraint strict_ineq_c(4*x > 2*y - 13);
  \endcode
  An unsatisfiable constraint on the zero-dimension space \f$\Rset^0\f$
  can be specified as follows:
  \code
  Constraint false_c = Constraint::zero_dim_false();
  \endcode
  Equivalent, but more involved ways are the following:
  \code
  Constraint false_c1(LinExpression::zero() == 1);
  Constraint false_c2(LinExpression::zero() >= 1);
  Constraint false_c3(LinExpression::zero() > 0);
  \endcode
  In constrast, the following code defines an unsatisfiable constraint
  having space-dimension \f$3\f$:
  \code
  Constraint false_c(0*z == 1);
  \endcode

  \par How to inspect a constraint
  Several methods are provided to examine a constraint and extract
  all the encoded information: its space-dimension, its type
  (equality, non-strict inequality, strict inequality) and
  the value of its integer coefficients.

  \par Example 2
  The following code shows how it is possible to access each single
  coefficient of a constraint. Given an inequality constraint
  (in this case \f$x - 5y + 3z <= 4\f$), we construct a new constraint
  corresponding to its complement (thus, in this case we want to obtain
  the strict inequality constraint \f$x - 5y + 3z > 4\f$).
  \code
  Constraint c1(x - 5*y + 3*z <= 4);
  cout << "Constraint c1: " << c1 << endl;
  if (c1.is_equality())
    cout << "Constraint c1 is not an inequality." << endl;
  else { 
    LinExpression e;
    for (int i = c1.space_dimension() - 1; i >= 0; i--)
      e += c1.coefficient(Variable(i)) * Variable(i);
    e += c1.inhomogeneous_term();
    Constraint c2 = c1.is_strict_inequality() ? (e <= 0) : (e < 0);
    cout << "Complement c2: " << c2 << endl;
  }
  \endcode
  The actual output is the following:
  \code
  Constraint c1: -A + 5*B - 3*C >= -4
  Complement c2: A - 5*B + 3*C > 4
  \endcode
  Note that, in general, the particular output obtained can be
  syntactically different from the (semantically equivalent)
  constraint considered.
*/
class Parma_Polyhedra_Library::Constraint : PPL_HIDDEN Row {
private:
  //! Default constructor: private and not implemented.
  Constraint();

  //! Builds a constraint (of unspecified type) stealing
  //! the coefficients from \p e.
  Constraint(LinExpression& e);

  //! Builds a constraint, having type \p type, which is able
  //! to store \p sz coefficients, whose values are left unspecified.
  Constraint(Row::Type t, size_t sz);

  //! Throws a <CODE>std::invalid_argument</CODE> exception
  //! containing the appropriate error message.
  void
  throw_dimension_incompatible(const char* method, Variable v) const;

  //! Returns the constraint \p e1 = \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator==(const LinExpression& e1,
				      const LinExpression& e2);

  //! Returns the constraint \p e = \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator==(const LinExpression& e,
				      const Integer& n);

  //! Returns the constraint \p n = \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator==(const Integer& n,
				      const LinExpression& e);

  //! Returns the constraint \p e1 >= \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator>=(const LinExpression& e1,
				      const LinExpression& e2);

  //! Returns the constraint \p e >= \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator>=(const LinExpression& e,
				      const Integer& n);

  //! Returns the constraint \p n >= \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator>=(const Integer& n,
				      const LinExpression& e);

  //! Returns the constraint \p e1 <= \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator<=(const LinExpression& e1,
				      const LinExpression& e2);

  //! Returns the constraint \p e <= \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator<=(const LinExpression& e,
				      const Integer& n);

  //! Returns the constraint \p n <= \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator<=(const Integer& n,
				      const LinExpression& e);

  //! Returns the constraint \p e1 > \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator>(const LinExpression& e1,
				     const LinExpression& e2);

  //! Returns the constraint \p e > \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator>(const LinExpression& e,
				     const Integer& n);

  //! Returns the constraint \p n > \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator>(const Integer& n,
				     const LinExpression& e);

  //! Returns the constraint \p e1 < \p e2.
  friend Constraint
  Parma_Polyhedra_Library::operator<(const LinExpression& e1,
				     const LinExpression& e2);

  //! Returns the constraint \p e < \p n.
  friend Constraint
  Parma_Polyhedra_Library::operator<(const LinExpression& e,
				     const Integer& n);

  //! Returns the constraint \p n < \p e.
  friend Constraint
  Parma_Polyhedra_Library::operator<(const Integer& n,
				     const LinExpression& e);

  //! Returns the constraint \p c with variables renamed
  //! by adding \p offset to their Cartesian axis identifier.
  friend Constraint
  Parma_Polyhedra_Library::operator>>(const Constraint& c,
				      unsigned int offset);

public:
  //! Ordinary copy-constructor.
  Constraint(const Constraint& c);

  //! Destructor.
  ~Constraint();

  //! Assignment operator.
  Constraint& operator=(const Constraint& c);

  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

  //! The constraint type.
  /*! Describes the type of the constraint. */
  enum Type {
    /*! The constraint is an equality. */
    EQUALITY,
    /*! The constraint is a non-strict inequality. */
    NONSTRICT_INEQUALITY,
    /*! The constraint is a strict inequality. */
    STRICT_INEQUALITY
  };

  //! Returns the constraint type of \p *this.
  Type type() const;

  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an equality constraint.
  bool is_equality() const;
  
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an inequality constraint (either strict or non-strict).
  bool is_inequality() const;
  
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a non-strict inequality constraint.
  bool is_nonstrict_inequality() const;
  
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a strict inequality constraint.
  bool is_strict_inequality() const;

  //! If the index of variable \p v is less than the space-dimension
  //! of \p *this, returns the coefficient of \p v in \p *this.
  //! \exception std::invalid_argument thrown if the index of \p v
  //! is greater than or equal to the space-dimension of \p *this.
  const Integer& coefficient(Variable v) const;
  
  //! Returns the inhomogeneous term of \p *this.
  const Integer& inhomogeneous_term() const;

  //! The unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$.
  static const Constraint& zero_dim_false();

  //! The true (zero-dimension space) constraint \f$0 \leq 1\f$,
  //! also known as <EM>positivity constraint</EM>.
  static const Constraint& zero_dim_positivity();

  //! Checks if all the invariants are satisfied.
  bool OK() const;

PPL_INTERNAL:
  //! Copy-constructor with given size.
  Constraint(const Constraint& c, size_t sz);

  static Constraint construct_epsilon_geq_zero();

// FIXME: this is a kludge
public:
  //! The zero-dimension space constraint \f$\epsilon \geq 0\f$
  //! (used to implement non-necessarily closed polyhedra).
  static const Constraint& epsilon_geq_zero();

  //! The zero-dimension space constraint \f$\epsilon \leq 1\f$
  //! (used to implement non-necessarily closed polyhedra).
  static const Constraint& epsilon_leq_one();

// FIXME: this is a kludge
PPL_INTERNAL:
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is the trivially true constraint \f$0 <= n\f$,
  //! where \f$n \geq 0\f$.
  bool is_trivial_true() const;

  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is the trivially false constraint
  //! (i.e., either \f$0 >= n\f$, where \f$n > 0\f$
  //! or \f$0 = n\f$, where \f$n \neq 0\f$.
  bool is_trivial_false() const;

  //! Sets the constraint type to <CODE>EQUALITY</CODE>.
  void set_is_equality();
 
  //! Sets the constraint type to <CODE>INEQUALITY</CODE>.
  void set_is_inequality();
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::Constraint& x,
	  Parma_Polyhedra_Library::Constraint& y);

} // namespace std


#include "Constraint.inlines.hh"

#endif
