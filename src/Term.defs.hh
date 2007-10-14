/* Term class declaration.
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

#ifndef PPL_Term_defs_hh
#define PPL_Term_defs_hh 1

#include "Term.types.hh"
#include "globals.types.hh"
#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Variables_Set.types.hh"
#include "Polynomial.types.hh"
#include <vector>

namespace Parma_Polyhedra_Library {
// Put them in the namespace here to declare them friend later.

//! Returns the term <CODE>t1 * t2</CODE>.
/*! \relates Term */
Term
operator*(const Term& t1, const Term& t2);

//! Returns the term <CODE>v * t</CODE>.
/*! \relates Term */
Term
operator*(Variable v, const Term& t);

//! Returns the term <CODE>t * v</CODE>.
/*! \relates Term */
Term
operator*(const Term& t, Variable v);

//! Returns the term <CODE>v * w</CODE>..
/*! \relates Term */
Term
operator*(Variable v, Variable w);

//! Returns the term <CODE>t1 * t2</CODE> and assigns it to \p t1.
/*! \relates Term */
Term&
operator*=(Term& t1, const Term& t2);

//! Returns the term <CODE>t * v</CODE> and assigns it to \p t.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.
 */
Term&
operator*=(Term& t, const Variable v);

//! Returns the term <CODE>t1/t2</CODE>.
/*! \relates Term
  \exception std::invalid_argument
  Thrown if \p t2 does not divide \p t1.
*/
Term
operator/(const Term& t1, const Term& t2);

//! Returns the term <CODE>t/v</CODE>.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.

  \exception std::invalid_argument
  Thrown if \p v does not divide \p t.
*/
Term
operator/(const Term& t, Variable v);

//! Returns the term <CODE>t1/t2</CODE> and assigns it to \p t1.
/*! \relates Term
  \exception std::invalid_argument
  Thrown if \p t2 does not divide \p t1.
*/
Term&
operator/=(Term& t1, const Term& t2);

//! Returns the term <CODE>t/v</CODE> and assigns it to \p t.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.

  \exception std::invalid_argument
  Thrown if \p v does not divide \p t.
*/
Term&
operator/=(Term& t, Variable v);

//! Returns the term <CODE>t * v</CODE> and assigns it to \p t.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.
 */
Term&
operator*=(Term& t, const Variable v);

//! Returns the term <CODE>v^n</CODE>.
/*! \relates Term */
Term pow(Variable v, exponent_type n);

//! Returns the term <CODE>t^n</CODE>.
/*! \relates Term */
Term pow(const Term& t, exponent_type n);

//! Returns the term <CODE>t^n</CODE> and assigns it to \p t.
/*! \relates Term */
Term& pow_assign(Term& t, exponent_type n);

//! Returns the term <CODE>t * (v^n)</CODE>.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.
*/
Term mul_pow(const Term& t, Variable v, exponent_type n);

//! Returns the term <CODE>t * (v^n)</CODE> and assigns it to \p t.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.
*/
Term& mul_pow_assign(Term& t, Variable v, exponent_type n);

//! Returns the term <CODE>t/(v^n)</CODE>.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.

  \exception std::invalid_argument
  Thrown if <CODE>v^n</CODE> does not divide \p t.
*/
Term div_pow(const Term& t, Variable v, exponent_type n);

//! Returns the term <CODE>t/(v^n)</CODE> and assigns it to \p t.
/*! \relates Term
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Term::max_space_dimension()</CODE>.

  \exception std::invalid_argument
  Thrown if <CODE>v^n</CODE> does not divide \p t.
*/
Term& div_pow_assign(Term& t, Variable v, exponent_type n);

/*! \brief
  Returns the term <CODE>t/v</CODE> and assigns it to \p t,
  <EM>assuming</EM> that \p v divides \p t.

  \relates Term
  The behavior is undefined if \p v does not divide \p t.
*/
Term& exact_div_assign(Term& t, Variable v);

/*! \brief
  Returns the term <CODE>t1/t2</CODE> and assigns it to \p t1,
  <EM>assuming</EM> that \p t2 divides \p t1.

  \relates Term
  The behavior is undefined if \p t2 does not divide \p t1.
*/
Term& exact_div_assign(Term& t1, const Term& t2);


//! Returns \c true if and only if \p x and \p y are the same term.
bool operator==(const Term& x, const Term& y);

//! Returns \c true if and only if \p x and \p y are different terms.
bool operator!=(const Term& x, const Term& y);

//! Defines a lexicographical ordering on terms.
/*! \relates Term */
bool lexicographic_less(const Term& x, const Term& y);

//! Defines a graded lexicographical ordering on terms.
/*! \relates Term */
bool graded_lexicographic_less(const Term& x, const Term& y);


namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Term */
std::ostream& operator<<(std::ostream& s, const Term& t);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Term */
void swap(Parma_Polyhedra_Library::Term& x,
	  Parma_Polyhedra_Library::Term& y);

} // namespace std

//! A term.
/*! \ingroup PPL_CXX_interface
  An object of the class Term represents the term
  \f[
    \prod_{i=0}^{n-1} x_i^{e_i}
  \f]
  where \f$n\f$ is the dimension of the vector space,
  each \f$e_i\f$ is the non-negative exponent of
  the \p i -th variable \f$x_i\f$ and \f$e_{n-1} \neq 0\f$ holds.

  \par How to build a term.

  Terms are (recursively) build starting from objects of class Variable,
  typically by using the multiplication operator or by computing
  non-negative powers of variables or other terms.

  \par Example
  The following code builds the term \f$x^4 y^2 z\f$,
  having space dimension \f$3\f$:
  \code
  Term t = x * x * x * x * y * y * z;
  \endcode
  Another way to build the same term is:
  \code
  Term t1 = pow(x, 4);
  Term t2 = pow(y, 2);
  Term t = t1 * t2 * z;
  \endcode
  Note that \p t1 and \p t2 have space dimension 1 and 2, respectively.

  Also available are operators and functions dividing a term by a variable
  or another term. These will only work provided such a division is exact.
  In particular, the division operator performs the divisibility check,
  throwing an exception if the check fails; in contrast, the function
  \c exact_div_assign performs no check at all and will result in undefined
  behavior if the second input argument does not divide the first one.
*/
class Parma_Polyhedra_Library::Term {
public:
  //! Default constructor: constructs the term \f$1\f$.
  Term();

  //! Ordinary copy-constructor.
  Term(const Term& t);

  //! Destructor.
  ~Term();

  //! Builds the term corresponding to the variable \p v.
  /*!
    \exception std::length_error
    Thrown if the space dimension of \p v exceeds
    <CODE>Term::max_space_dimension()</CODE>.
  */
  Term(const Variable v);

  //! Returns the maximum space dimension a Term can handle.
  static dimension_type max_space_dimension();

  //! Returns the maximum exponent a Term can handle.
  static dimension_type max_exponent();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the exponent of \p v in \p *this.
  exponent_type exponent(Variable v) const;

  //! Returns the degree of \p *this.
  degree_type degree() const;

  //! Returns the degree of \p v in \p *this.
  degree_type degree(Variable v) const;

  //! Returns the (zero-dimension space) constant term \f$1\f$.
  static const Term& one();

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  PPL_OUTPUT_DECLARATIONS

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    \ref ascii_dump) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
#endif
  bool ascii_load(std::istream& s);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! Swaps \p *this with \p y.
  void swap(Term& y);

  //! Binary predicate defining the total ordering on terms.
  struct Compare {
    //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
    bool operator()(const Term& x, const Term& y) const;
  };

private:
  struct Checked_Exponent_Type_Policy {
    const_bool_nodef(check_overflow, true);
    const_bool_nodef(check_inf_add_inf, false);
    const_bool_nodef(check_inf_sub_inf, false);
    const_bool_nodef(check_inf_mul_zero, false);
    const_bool_nodef(check_div_zero, false);
    const_bool_nodef(check_inf_div_inf, false);
    const_bool_nodef(check_inf_mod, false);
    const_bool_nodef(check_sqrt_neg, false);
    const_bool_nodef(has_nan, true);
    const_bool_nodef(has_infinity, false);
    const_bool_nodef(convertible, true);
    const_bool_nodef(fpu_check_inexact, false);
    const_bool_nodef(check_nan_result, false);
    static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_NATIVE;
    static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_NATIVE;
    static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_NATIVE;
    static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_NATIVE;
    static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_NATIVE;
    static void handle_result(Result r);
  };

  //! The checked exponent type.
  typedef Checked_Number<exponent_type, Checked_Exponent_Type_Policy>
  Checked_Exponent_Type;

  //! The type of the implementation vector.
  typedef std::vector<Checked_Exponent_Type> Vector;

  //! The implementation vector.
  Vector vec;

  //! Implementation sizing constructor.
  explicit Term(dimension_type sz);

  //! Copy-constructor with a specified space dimension.
  Term(const Term& t, dimension_type sz);

  //! Copy-constructor with permutation of space dimensions.
  /*!
    The behavior is undefined if the parameter \p perm does not encode
    a permutation of \f$\{0, \ldots, m-1\}\f$, where
    <CODE>m == perm.size()</CODE> and <CODE>m >= t.space_dimension()</CODE>.
  */
  Term(const Term& t, const std::vector<dimension_type>& perm);

  //! \name Subscript operators
  //@{
  //! Returns a reference to the exponent of dimension \p k.
  Checked_Exponent_Type& operator[](dimension_type k);

  //! Returns the exponent of dimension \p k.
  Checked_Exponent_Type operator[](dimension_type k) const;
  //@} // Subscript operators

  /*! \brief
    Shifts space dimensions towards zero, overwriting the <EM>unused</EM>
    space dimensions represented by the variables in \p unused.

    The behavior is undefined if any of the variables in \p unused has a
    positive degree in \p *this.
    Suppose <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE> correspond
    to dimensions 0, 1 and 2, respectively.  Then, if <CODE>unused</CODE>
    contains only <CODE>y</CODE>, applying the method to
    <CODE>x*pow(z, 2)</CODE> gives <CODE>x*pow(y, 2)</CODE> whereas
    applying the method to <CODE>x*y</CODE> results in undefined behavior.
  */
  void shift_space_dimensions(const Variables_Set& unused);

  friend class Parma_Polyhedra_Library::Polynomial;

  friend Term operator*(const Term& t1, const Term& t2);
  friend Term operator*(Variable v, const Term& t);
  friend Term operator*(Variable v, Variable w);

  friend Term& operator*=(Term& t1, const Term& t2);

  friend Term mul_pow(const Term& t, Variable v, exponent_type n);
  friend Term& mul_pow_assign(Term& t, const Variable v, exponent_type n);

  friend Term& pow_assign(Term& t, exponent_type n);

  friend Term operator/(const Term& t1, const Term& t2);

  friend Term& operator/=(Term& t1, const Term& t2);

  friend Term div_pow(const Term& t, const Variable v, exponent_type n);
  friend Term& div_pow_assign(Term& t, const Variable v, exponent_type n);

  friend Term& exact_div_assign(Term& t, Variable v);
  friend Term& exact_div_assign(Term& t1, const Term& t2);

  friend bool operator==(const Term& x, const Term& y);

  friend bool lexicographic_less(const Term& x, const Term& y);
  friend bool graded_lexicographic_less(const Term& x, const Term& y);

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const Term& t);
};

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
std::ostream&
operator<<(std::ostream& s, exponent_type e);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "Term.inlines.hh"

#endif // !defined(PPL_Term_defs_hh)
