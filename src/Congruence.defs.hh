/* Congruence class declaration.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Congruence_defs_hh
#define PPL_Congruence_defs_hh 1

#include "Congruence.types.hh"
#include "Row.defs.hh"
#include "Grid.types.hh"
#include "Variable.defs.hh"
#include "Constraint.types.hh"
#include "Linear_Expression.types.hh"
#include "Linear_Row.types.hh"
#include "Congruence_System.defs.hh"
#include <iosfwd>

namespace PPL = Parma_Polyhedra_Library;

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Congruence */
std::ostream&
operator<<(std::ostream& s, const Congruence& c);

} // namespace IO_Operators

// Put these in the namespace here to declare them friend later.

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Congruence */
bool
operator==(const Congruence& x, const Congruence& y);

//! Returns <CODE>false</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Congruence */
bool
operator!=(const Congruence& x, const Congruence& y);

//! Returns the congruence \f$e1 = e2 \pmod{1}\f$.
/*! \relates Congruence */
Congruence
operator%=(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the congruence \f$e = n \pmod{1}\f$.
/*! \relates Congruence */
Congruence
operator%=(const Linear_Expression& e,
	   const Coefficient_traits::const_reference n);

//! Returns a copy of \p cg, multiplying \p k into the copy's modulus.
/*!
  If \p cg is said to represent the congruence \f$ e_1 = e_2
  \pmod{m}\f$, then the returned copy can be said to represent the
  congruence \f$ e_1 = e_2 \pmod{mk}\f$.
  \relates Congruence
*/
Congruence
operator/(const Congruence& cg,
	  const Coefficient_traits::const_reference k);

//! Creates a congruence from \p c, with \p m as the modulus.
/*! \relates Congruence */
Congruence
operator/(const Constraint& c,
	  const Coefficient_traits::const_reference m);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Computes the scalar product of \p x and \p y and assigns it to \p z.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
scalar_product_assign(Coefficient& z,
		      const Linear_Row& x, const Congruence& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the scalar product of \p x (FIX relative to \p ref) and
//! \p y and assigns it to \p z.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
scalar_product_assign(Coefficient& z,
		      const Linear_Row& x, const Congruence& y,
		      const Linear_Row& ref);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e reduced scalar product of \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored,
//! and assigns the result to \p z.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
reduced_scalar_product_assign(Coefficient& z,
			      const Linear_Row& x, const Congruence& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e reduced scalar product of \p x (FIX relative to \p
//! ref) and \p y, where the \f$\epsilon\f$ coefficient of \p x is
//! ignored, and assigns the result to \p z.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
reduced_scalar_product_assign(Coefficient& z,
			      const Linear_Row& x, const Congruence& y,
			      const Linear_Row& ref);

// FIX will this ever be used?
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e homogeneous scalar product of \p x and \p y,
//! where the inhomogeneous terms are ignored,
//! and assigns the result to \p z.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void
homogeneous_scalar_product_assign(Coefficient& z,
				  const Linear_Row& x,
				  const Congruence& y);

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Congruence */
void
swap(Parma_Polyhedra_Library::Congruence& x,
     Parma_Polyhedra_Library::Congruence& y);

} // namespace std

//! A linear congruence.
/*!
  An object of the class Congruence is either:
  - a congruence \f$\pmod{1}\f$: \f$\sum_{i=0}^{n-1} a_i x_i + b \%= 0\f$; or
  - a congruence \f$\pmod{m}\f$: \f$\sum_{i=0}^{n-1} a_i x_i + b \%= 0 / m\f$;

  where \f$n\f$ is the dimension of the space,
  \f$a_i\f$ is the integer coefficient of variable \f$x_i\f$
  and \f$b\f$ is the integer inhomogeneous term.

  \par How to build a congruence
  Congruences \f$\pmod{1}\f$ are typically built by applying the congruence symbol
  to a pair of linear expressions.
  The congruence symbol is (<CODE>\%=</CODE>).
  Congruences \f$\pmod{m}\f$, for an integer \p m
  are typically built by first building a congruence \f$\pmod{1}\f$
  using the given pair of linear expressions
  and then adding the modulus \p m.
  The modulus symbol is (<CODE>/</CODE>).

  The space dimension of a congruence is defined as the maximum
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
  The following code builds the equality congruence
  \f$3x + 5y - z = 0\f$, having space dimension \f$3\f$:
  \code
  Congruence eq_cg((3*x + 5*y - z %= 0) / 0);
  \endcode
  The following code builds the congruence
  \f$4x = 2y - 13 \pmod{1}\f$, having space dimension \f$2\f$:
  \code
  Congruence mod1_cg(4*x %= 2*y - 13);
  \endcode
  The following code builds the congruence
  \f$4x = 2y - 13 \pmod{2}\f$, having space dimension \f$2\f$:
  \code
  Congruence mod2_cg((4*x %= 2*y - 13) / 2);
  \endcode
  An unsatisfiable congruence on the zero-dimension space \f$\Rset^0\f$
  can be specified as follows:
  \code
  Congruence false_cg = Congruence::zero_dim_false();
  \endcode
  Equivalent, but more involved ways are the following:
  \code
  Congruence false_cg1((Linear_Expression::zero() %= 1) / 0);
  Congruence false_cg2((Linear_Expression::zero() %= 1) / 2);
  \endcode
  In contrast, the following code defines an unsatisfiable congruence
  having space dimension \f$3\f$:
  \code
  Congruence false_cg3((0*z %= 1) / 0);
  \endcode

  \par How to inspect a congruence
  Several methods are provided to examine a congruence and extract
  all the encoded information: its space dimension, its modulus
  and the value of its integer coefficients.

  \par Example 2
  The following code shows how it is possible to access the modulus
  as well as each of the coefficients.
  Given a congruence with linear expression \p e and modulus \p m
  (in this case \f$x - 5y + 3z = 4 \pmod{5}\f$), we construct a new
  congruence with the same modulus \p m but where the linear
  expression is \f$-e\f$ (\f$-x + 5y - 3z = -4 \pmod{5}\f$).
  \code
  Congruence cg1((x - 5*y + 3*z %= 4) / 5);
  cout << "Congruence cg1: " << cg1 << endl;
  Coefficient m = cg1.modulus();
  if (m == 0)
    cout << "Congruence cg1 is an equality." << endl;
  else {
    Linear_Expression e;
    for (int i = cg1.space_dimension() - 1; i >= 0; i--)
      e -= cg1.coefficient(Variable(i)) * Variable(i);
      e -= cg1.inhomogeneous_term();
    Congruence cg2((e %= 0) / m);
    cout << "Congruence cg2: " << cg2 << endl;
  }
  \endcode
  The actual output could be the following:
  \code
  Congruence cg1: -A + 5*B - 3*C %= 4 / 5
  Congruence cg2: A - 5*B + 3*C %= -4 / 5
  \endcode
  Note that, in general, the particular output obtained can be
  syntactically different from the (semantically equivalent)
  equality congruence considered.
*/
class Parma_Polyhedra_Library::Congruence : private Row {
public:
  //! Ordinary copy-constructor.
  Congruence(const Congruence& cg);

  //! Copy-constructs (modulo 0) from equality constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p c is a relation.
  */
  explicit Congruence(const Constraint& c);

  //! Destructor.
  ~Congruence();

  //! Assignment operator.
  Congruence& operator=(const Congruence& cg);

  //! Returns the maximum space dimension a Congruence can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the coefficient of \p v in \p *this.
  /*!
    \exception std::invalid_argument thrown if the index of \p v
    is greater than or equal to the space dimension of \p *this.
  */
  Coefficient_traits::const_reference coefficient(Variable v) const;

  //! Returns the inhomogeneous term of \p *this.
  Coefficient_traits::const_reference inhomogeneous_term() const;

  //! Returns a reference to the modulus of \p *this.
  Coefficient& modulus();

  //! Returns a const reference to the modulus of \p *this.
  const Coefficient& modulus() const;

  //! Multiplies \p k into the modulus of \p *this.
  /*!
    If \p *this is said to represent the congruence \f$ e_1 = e_2
    \pmod{m}\f$, then it can be said that *this will be left
    representing the congruence \f$ e_1 = e_2 \pmod{mk}\f$.
  */
  Congruence&
  operator/=(const Coefficient_traits::const_reference k);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is a trivially
  //! true congruence.
  /*!
    Trivially true congruences have either one of the following two forms:
    - a congruence: \f$\sum_{i=0}^{n-1} 0 x_i + 0 \%= 0 / m\f$; or
    - a congruence: \f$\sum_{i=0}^{n-1} 0 x_i + b \%= 0\f$.
  */
  bool is_trivial_true() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is a trivially
  //! false congruence.
  /*!
    Trivially false congruences have one of the following two forms:
    - a congruence: \f$\sum_{i=0}^{n-1} 0 x_i + 0 \%= b / 0\f$; or
    - a congruence: \f$\sum_{i=0}^{n-1} 0 x_i + b \%= 0 / 0\f$,
      where \f$b b \neq 0\f$.
  */
  bool is_trivial_false() const;

  //! Returns <CODE>true</CODE> if the modulus is greater than zero.
  /*!
    A congruence with a modulus of 0 is a linear equality.
  */
  bool is_proper_congruence() const;

  //! Returns <CODE>true</CODE> if \p *this is an equality.
  /*!
    A modulus of zero denotes a linear equality.
  */
  bool is_equality() const;

  //! \brief
  //! Returns a reference to the true (zero-dimension space)
  //! congruence \f$0 = 1 \pmod{1}\f$, also known as the
  //! <EM>integrality congruence</EM>.
  static const Congruence& zero_dim_integrality();

  //! \brief
  //! Returns a reference to the false (zero-dimension space)
  //! congruence \f$0 = 1 \pmod{0}\f$.
  static const Congruence& zero_dim_false();

  //! \brief
  //! Returns a lower bound to the total size in bytes of the memory
  //! occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Writes to std::cerr an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump() const;

  //! \brief
  //! Loads from \p s an ASCII representation of the internal
  //! representation of \p *this.
  bool ascii_load(std::istream& s);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

protected:

  //! Normalizes the signs.
  /*!
    The signs of the coefficients and the inhomogeneous term are
    normalized, leaving the first non-zero homogeneous coefficient
    positive.
  */
  void sign_normalize();

  //! Normalizes signs and then the inhomogeneous term.
  /*!
    Applies sign_normalize, then reduces the inhomogeneous term to the
    smallest possible positive number.
  */
  void normalize();

  //! Calls normalize, then divides out common factors.
  /*!
    Strongly normalized Congruences which have different syntaxes (as
    output by operator<<) are guaranteed to have different semantics.
  */
  void strong_normalize();

private:

  //! Mark this congruence as a linear equality.
  void set_is_equality();

  //! Default constructor: private and not implemented.
  Congruence();

  //! Copy-constructs with specified size and capacity.
  Congruence(const Congruence& cg,
	     dimension_type sz,
	     dimension_type capacity);

  //! Copy-constructs from \p cg, multiplying \p k into the modulus.
  /*!
    If \p cg is said to represent the congruence \f$ e_1 = e_2
    \pmod{m}\f$, then the result can be said to represent the
    congruence \f$ e_1 = e_2 \pmod{mk}\f$.
  */
  Congruence(const Congruence& cg, Coefficient_traits::const_reference k);

  //! Constructs from Linear_Expression \p le, using modulus \p m.
  /*!
     Builds a congruence with modulus \p m, stealing the coefficients
     from \p le.  Assumes that \p le has spare capacity of at least
     one element, for the modulus.
  */
  Congruence(Linear_Expression& le,
	     Coefficient_traits::const_reference m);

  //! Swaps \p *this with \p y.
  void swap(Congruence& y);

  //! \brief
  //! Throws a <CODE>std::invalid_argument</CODE> exception containing
  //! the appropriate error message.
  void
  throw_dimension_incompatible(const char* method,
			       const char* v_name,
			       Variable v) const;

  friend Congruence
  PPL::operator%=(const Linear_Expression& e1, const Linear_Expression& e2);

  friend Congruence
  PPL::operator%=(const Linear_Expression& e,
		  const Coefficient_traits::const_reference n);

  friend Congruence
  PPL::operator/(const Congruence& cg,
		 const Coefficient_traits::const_reference k);

  friend Congruence
  PPL::operator/(const Constraint& c,
		 const Coefficient_traits::const_reference m);

  friend bool
  PPL::operator==(const Congruence& x, const Congruence& y);

  friend bool
  PPL::operator!=(const Congruence& x, const Congruence& y);

  friend class PPL::Congruence_System;
  friend class PPL::Congruence_System::const_iterator;
  friend class PPL::Grid;

  friend void
  std::swap(PPL::Congruence& x, PPL::Congruence& y);

  // FIX check which used
  friend void
  PPL::scalar_product_assign(Coefficient& z,
			     const Linear_Row& x,
			     const Congruence& y);
  friend void
  PPL::scalar_product_assign(Coefficient& z,
			     const Linear_Row& x,
			     const Congruence& y,
			     const Linear_Row& ref);
  friend void
  PPL::reduced_scalar_product_assign(Coefficient& z,
				     const Linear_Row& x,
				     const Congruence& y);
  friend void
  PPL::reduced_scalar_product_assign(Coefficient& z,
				     const Linear_Row& x,
				     const Congruence& y,
				     const Linear_Row& ref);
  friend void
  PPL::homogeneous_scalar_product_assign(Coefficient& z,
					 const Linear_Row& x,
					 const Congruence& y);
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns the sign of the scalar product between \p x and \p y.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int
scalar_product_sign(const Linear_Row& x, const Congruence& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the sign of the \e reduced scalar product of \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored.
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int
reduced_scalar_product_sign(const Linear_Row& x, const Congruence& y);

// FIX will this ever be used?
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the sign of the \e homogeneous scalar product of \p x and \p y,
//! where the inhomogeneous terms are ignored,
/*! \relates Linear_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int
homogeneous_scalar_product_sign(const Linear_Row& x, const Congruence& y);

}

#include "Congruence.inlines.hh"

#endif // !defined(PPL_Congruence_defs_hh)
