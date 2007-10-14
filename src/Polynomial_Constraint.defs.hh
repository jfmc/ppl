/* Polynomial_Constraint class declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Polynomial_Constraint_defs_hh
#define PPL_Polynomial_Constraint_defs_hh 1

#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Polynomial_Constraint.types.hh"
#include "Polynomial.defs.hh"
#include "Polynomial_Constraint_System.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friend later.

//! Returns <CODE>true</CODE> if and only if \p x is equivalent to \p y.
/*! \relates Polynomial_Constraint */
bool
operator==(const Polynomial_Constraint& x, const Polynomial_Constraint& y);

//! Returns <CODE>true</CODE> if and only if \p x is not equivalent to \p y.
/*! \relates Polynomial_Constraint */
bool
operator!=(const Polynomial_Constraint& x, const Polynomial_Constraint& y);

//! Returns the constraint \p p1 = \p p2.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator==(const Polynomial& p1, const Polynomial& p2);
//! Returns the constraint \p p = \p n.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator==(const Polynomial& p, Coefficient_traits::const_reference n);
//! Returns the constraint \p n = \p p.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator==(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the constraint \p p1 \<= \p p2.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<=(const Polynomial& p1, const Polynomial& p2);
//! Returns the constraint \p p \<= \p n.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<=(const Polynomial& p, Coefficient_traits::const_reference n);
//! Returns the constraint \p n \<= \p p.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<=(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the constraint \p p1 \>= \p p2.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>=(const Polynomial& p1, const Polynomial& p2);
//! Returns the constraint \p p \>= \p n.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>=(const Polynomial& p, Coefficient_traits::const_reference n);
//! Returns the constraint \p n \>= \p p.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>=(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the constraint \p p1 \< \p p2.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<(const Polynomial& p1, const Polynomial& p2);
//! Returns the constraint \p p \< \p n.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<(const Polynomial& p, Coefficient_traits::const_reference n);
//! Returns the constraint \p n \< \p p.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator<(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the constraint \p p1 \> \p p2.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>(const Polynomial& p1, const Polynomial& p2);
//! Returns the constraint \p p \> \p n.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>(const Polynomial& p, Coefficient_traits::const_reference n);
//! Returns the constraint \p n \> \p p.
/*! \relates Polynomial_Constraint */
Polynomial_Constraint
operator>(Coefficient_traits::const_reference n, const Polynomial& p);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
std::ostream& operator<<(std::ostream& s, const Polynomial_Constraint& c);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
void swap(Parma_Polyhedra_Library::Polynomial_Constraint& x,
	  Parma_Polyhedra_Library::Polynomial_Constraint& y);

} // namespace std

//! A polynomial equality or inequality.
/*! \ingroup PPL_CXX_interface
  An object of the class Polynomial_Constraint is either:
  - an equality: \f$\sum_{i=0}^{n-1} a_i x_i + b = 0\f$;
  - a non-strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b \geq 0\f$; or
  - a strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b > 0\f$;

  where \f$n\f$ is the dimension of the space,
  \f$a_i\f$ is the integer coefficient of variable \f$x_i\f$
  and \f$b\f$ is the integer constant term.

  \par How to build a constraint
  Polynomial_Constraints are typically built by applying a relation symbol
  to a pair of polynomials.
  Available relation symbols are equality (<CODE>==</CODE>),
  non-strict inequalities (<CODE>\>=</CODE> and <CODE>\<=</CODE>) and
  strict inequalities (<CODE>\<</CODE> and <CODE>\></CODE>).
  The space dimension of a constraint is defined as the maximum
  space dimension of the arguments of its constructor.

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
  \f$3x + 5y - z = 0\f$, having space dimension \f$3\f$:
  \code
  Polynomial_Constraint eq_c(3*x + 5*y - z == 0);
  \endcode
  The following code builds the (non-strict) inequality constraint
  \f$4x \geq 2y - 13\f$, having space dimension \f$2\f$:
  \code
  Polynomial_Constraint ineq_c(4*x >= 2*y - 13);
  \endcode
  The corresponding strict inequality constraint
  \f$4x > 2y - 13\f$ is obtained as follows:
  \code
  Polynomial_Constraint strict_ineq_c(4*x > 2*y - 13);
  \endcode
  An unsatisfiable constraint on the zero-dimension space \f$\Rset^0\f$
  can be specified as follows:
  \code
  Polynomial_Constraint false_c = Polynomial_Constraint::zero_dim_false();
  \endcode
  Equivalent, but more involved ways are the following:
  \code
  Polynomial_Constraint false_c1(Polynomial::zero() == 1);
  Polynomial_Constraint false_c2(Polynomial::zero() >= 1);
  Polynomial_Constraint false_c3(Polynomial::zero() > 0);
  \endcode
  In contrast, the following code defines an unsatisfiable constraint
  having space dimension \f$3\f$:
  \code
  Polynomial_Constraint false_c(0*z == 1);
  \endcode

  \par How to inspect a constraint
  Several methods are provided to examine a constraint and extract
  all the encoded information: its space dimension, its type
  (equality, non-strict inequality, strict inequality) and
  the value of its integer coefficients.

  \par Example 2
  The following code shows how it is possible to access each single
  coefficient of a constraint. Given an inequality constraint
  (in this case \f$x - 5y + 3z \leq 4\f$), we construct a new constraint
  corresponding to its complement (thus, in this case we want to obtain
  the strict inequality constraint \f$x - 5y + 3z > 4\f$).
  \code
  Polynomial_Constraint c1(x - 5*y + 3*z <= 4);
  cout << "Polynomial_Constraint c1: " << c1 << endl;
  if (c1.is_equality())
    cout << "Polynomial_Constraint c1 is not an inequality." << endl;
  else {
    Polynomial e;
    for (int i = c1.space_dimension() - 1; i >= 0; i--)
      e += c1.coefficient(Variable(i)) * Variable(i);
    e += c1.constant_term_coefficient();
    Polynomial_Constraint c2 = c1.is_strict_inequality() ? (e <= 0) : (e < 0);
    cout << "Complement c2: " << c2 << endl;
  }
  \endcode
  The actual output is the following:
  \code
  Polynomial_Constraint c1: -A + 5*B - 3*C >= -4
  Complement c2: A - 5*B + 3*C > 4
  \endcode
  Note that, in general, the particular output obtained can be
  syntactically different from the (semantically equivalent)
  constraint considered.
*/
class Parma_Polyhedra_Library::Polynomial_Constraint {
public:
  //! Ordinary copy-constructor.
  Polynomial_Constraint(const Polynomial_Constraint& c);

  //! Destructor.
  ~Polynomial_Constraint();

  //! Assignment operator.
  Polynomial_Constraint& operator=(const Polynomial_Constraint& c);

  //! Returns the maximum space dimension a Polynomial_Constraint can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the polynomial corresponding to \p *this.
  /*!
    If \p *this encodes the polynomial constraint \f$(p \relsym 0)\f$,
    where \f$\mathord{\relsym} \in \{ =, \geq, > \}\f$,
    the method returns the polynomial \f$p\f$.
    If \p *this is an inequality (resp., equality) constraint, then the
    returned polynomial is unique up to a positive (resp., non-zero) factor.
  */
  const Polynomial& polynomial() const;

  //! The constraint type.
  enum Relation {
    /*! The constraint is an equality. */
    EQUALITY,
    /*! The constraint is a non-strict inequality. */
    NONSTRICT_INEQUALITY,
    /*! The constraint is a strict inequality. */
    STRICT_INEQUALITY
  };

  //! Returns the constraint type of \p *this.
  Relation relation() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an equality constraint.
  bool is_equality() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an inequality constraint (either strict or non-strict).
  bool is_inequality() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a non-strict inequality constraint.
  bool is_nonstrict_inequality() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a strict inequality constraint.
  bool is_strict_inequality() const;

  //! Returns the coefficient of \p m in \p *this.
  /*!
    \exception std::invalid_argument thrown if the space dimension of \p m
    is greater than or equal to the space dimension of \p *this.
  */
  Coefficient_traits::const_reference coefficient(const Term& t) const;

  //! Returns the constant term of \p *this.
  Coefficient_traits::const_reference constant_term_coefficient() const;

  //! The unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$.
  static const Polynomial_Constraint& zero_dim_false();

  //! \brief
  //! The true (zero-dimension space) constraint \f$0 \leq 1\f$,
  //! also known as <EM>positivity constraint</EM>.
  static const Polynomial_Constraint& zero_dim_positivity();

  //! \brief
  //! Returns a lower bound to the total size in bytes of the memory
  //! occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a tautology (i.e., an always true constraint).
  /*!
    A tautology can have either one of the following forms:
    - an equality: \f$\sum_{i=0}^{n-1} 0 x_i + 0 = 0\f$; or
    - a non-strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b \geq 0\f$,
      where \f$b \geq 0\f$; or
    - a strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b > 0\f$,
      where \f$b > 0\f$.
  */
  bool is_tautological() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is inconsistent (i.e., an always false constraint).
  /*!
    An inconsistent constraint can have either one of the following forms:
    - an equality: \f$\sum_{i=0}^{n-1} 0 x_i + b = 0\f$,
      where \f$b \neq 0\f$; or
    - a non-strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b \geq 0\f$,
      where \f$b < 0\f$; or
    - a strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b > 0\f$,
      where \f$b \leq 0\f$.
  */
  bool is_inconsistent() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y
    are equivalent constraints.

    Polynomial_Constraints having different space dimensions
    are not equivalent. Note that constraints having different relations
    may nonetheless be equivalent, if they both are tautologies or
    inconsistent.
  */
  bool is_equivalent_to(const Polynomial_Constraint& y) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! Swaps \p *this with \p y.
  void swap(Polynomial_Constraint& y);

private:
  //! The polynomial expression defining this constraint.
  Polynomial poly;

  //! The type of this constraint.
  Relation rel;

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  /*! \brief
    Normalizes the sign of the coefficients so that the first non-zero
    (homogeneous) coefficient of an equality constraint is positive.
  */
  void sign_normalize();

  /*! \brief
    Strong normalization: ensures that different Linear_Row objects
    represent different hyperplanes or hyperspaces.

    Applies both Linear_Row::normalize() and Linear_Row::sign_normalize().
  */
  void strong_normalize();

  friend class Parma_Polyhedra_Library::Polynomial_Constraint_System;
  //friend class Parma_Polyhedra_Library::Polynomial_Constraint_System::const_iterator;

  //friend
  //Parma_Polyhedra_Library
  //::Polynomial::Polynomial(const Polynomial_Constraint& c);

  //! Default constructor: private and not implemented.
  Polynomial_Constraint();

  //! Builds a constraint of type \p r stealing the polynimial \p p.
  Polynomial_Constraint(Polynomial& p, Relation r);

  //! \brief
  //! Throws a <CODE>std::invalid_argument</CODE> exception
  //! containing the appropriate error message.
  void
  throw_dimension_incompatible(const char* method,
			       const char* name_var,
			       const Term& t) const;

  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator==(const Polynomial& p1,
				      const Polynomial& p2);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator==(const Polynomial& p,
				      Coefficient_traits::const_reference n);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator==(Coefficient_traits::const_reference n,
				      const Polynomial& p);

  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>=(const Polynomial& p1,
				      const Polynomial& p2);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>=(const Polynomial& p,
				      Coefficient_traits::const_reference n);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>=(Coefficient_traits::const_reference n,
				      const Polynomial& p);

  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<=(const Polynomial& p1,
				      const Polynomial& p2);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<=(const Polynomial& p,
				      Coefficient_traits::const_reference n);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<=(Coefficient_traits::const_reference n,
				      const Polynomial& p);

  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>(const Polynomial& p1,
				     const Polynomial& p2);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>(const Polynomial& p,
				     Coefficient_traits::const_reference n);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator>(Coefficient_traits::const_reference n,
				     const Polynomial& p);

  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<(const Polynomial& p1,
				     const Polynomial& p2);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<(const Polynomial& p,
				     Coefficient_traits::const_reference n);
  friend Polynomial_Constraint
  Parma_Polyhedra_Library::operator<(Coefficient_traits::const_reference n,
				     const Polynomial& p);

  friend bool
  Parma_Polyhedra_Library::operator==(const Polynomial_Constraint& x,
				      const Polynomial_Constraint& y);

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators
  ::operator<<(std::ostream& s, const Polynomial_Constraint& c);

  //! Sets the constraint relation to <CODE>EQUALITY</CODE>.
  void set_is_equality();

  //! Sets the constraint relation to <CODE>NONSTRICT_INEQUALITY</CODE>.
  void set_is_nonstrict_inequality();

  //! Sets the constraint relation to <CODE>STRICT_INEQUALITY</CODE>.
  void set_is_strict_inequality();
};

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
std::ostream& operator<<(std::ostream& s,
			 const Polynomial_Constraint::Relation& r);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "Polynomial.inlines.hh"
#include "Polynomial_Constraint.inlines.hh"

#endif // !defined(PPL_Polynomial_Constraint_defs_hh)
