/* Monomial class declaration.
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

#ifndef PPL_Monomial_defs_hh
#define PPL_Monomial_defs_hh 1

#include "Monomial.types.hh"
#include "globals.types.hh"
#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Term.defs.hh"
#include "Polynomial.defs.hh"
#include <utility>

namespace Parma_Polyhedra_Library {

//! Returns the monomial <CODE>n * m</CODE>.
/*! \relates Monomial */
Monomial
operator*(Coefficient_traits::const_reference n, const Monomial& m);

//! Returns the monomial <CODE>n * m</CODE>.
/*! \relates Monomial */
Monomial
operator*(const Monomial& m, Coefficient_traits::const_reference n);

//! Returns the monomial <CODE>m * v</CODE>.
/*! \relates Monomial
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Monomial::max_space_dimension()</CODE>.
*/
Monomial
operator*(const Monomial& m, Variable v);

//! Returns the monomial <CODE>m * v</CODE>.
/*! \relates Monomial
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Monomial::max_space_dimension()</CODE>.
*/
Monomial
operator*(Variable v, const Monomial& m);

//! Returns the monomial <CODE>m * t</CODE>.
/*! \relates Monomial */
Monomial
operator*(const Monomial& m, const Term& t);

//! Returns the monomial <CODE>m * t</CODE>..
/*! \relates Monomial */
Monomial
operator*(const Term& t, const Monomial& m);

//! Returns the monomial <CODE>m1 * m2</CODE>.
/*! \relates Monomial */
Monomial
operator*(const Monomial& m1, const Monomial& m2);

//! Returns the monomial <CODE>n * m</CODE> and assigns it to \p m.
/*! \relates Monomial */
Monomial&
operator*=(Monomial& m, Coefficient_traits::const_reference n);

//! Returns the monomial <CODE>m * v</CODE> and assigns it to \p m.
/*! \relates Monomial
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Monomial::max_space_dimension()</CODE>.
*/
Monomial&
operator*=(Monomial& m, const Variable v);

//! Returns the monomial <CODE>m * t</CODE> and assigns it to \p m.
/*! \relates Monomial */
Monomial&
operator*=(Monomial& m, const Term& t);

//! Returns the monomial <CODE>m1 * m2</CODE> and assigns it to \p m1.
/*! \relates Monomial */
Monomial&
operator*=(Monomial& m1, const Monomial& m2);

//! Returns the monomial <CODE>m^n</CODE>.
/*! \relates Monomial */
Monomial pow(const Monomial& m, dimension_type n);

//! Assigns the monomial <CODE>m^n</CODE> to \p m.
/*! \relates Monomial */
void pow_assign(Monomial& m, dimension_type n);

/*! \brief
  Returns the monomial <CODE>m/n</CODE> and assigns it to \p m,
  <EM>assuming</EM> that \p n divides \p m.

  \relates Monomial
  The behavior is undefined if \p n does not divide \p m.
*/
Monomial& exact_div_assign(Monomial& m,
			   Coefficient_traits::const_reference n);

/*! \brief
  Returns the monomial <CODE>m/v</CODE> and assigns it to \p m,
  <EM>assuming</EM> that \p v divides \p m.

  \relates Monomial
  The behavior is undefined if \p v does not divide \p m.
*/
Monomial& exact_div_assign(Monomial& m, Variable v);

/*! \brief
  Returns the monomial <CODE>m/t</CODE> and assigns it to \p m,
  <EM>assuming</EM> that \p t divides \p m.

  \relates Monomial
  The behavior is undefined if \p t does not divide \p m.
*/
Monomial& exact_div_assign(Monomial& m, const Term& t);

/*! \brief
  Returns the monomial <CODE>m1/m2</CODE> and assigns it to \p m1,
  <EM>assuming</EM> that \p m2 divides \p m1.

  \relates Monomial
  The behavior is undefined if \p m2 does not divide \p m1.
*/
Monomial& exact_div_assign(Monomial& m1, const Monomial& m2);

//! Returns \c true if and only if \p x and \p y are the same monomial.
bool operator==(const Monomial& x, const Monomial& y);

//! Returns \c true if and only if \p x and \p y are different monomials.
bool operator!=(const Monomial& x, const Monomial& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Monomial */
std::ostream& operator<<(std::ostream& s, const Monomial& m);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Monomial */
void swap(Parma_Polyhedra_Library::Monomial& x,
	  Parma_Polyhedra_Library::Monomial& y);

} // namespace std

//! A monomial.
/*! \ingroup PPL_CXX_interface
  An object of class Monomial is made up of a Term and a multiplying
  Coefficient. Monomials inherit their space dimension and degree ...
*/
class Parma_Polyhedra_Library::Monomial
  : private std::pair<const Term, Coefficient> {
private:
  //! Base type of the implementation.
  typedef std::pair<const Term, Coefficient> Base;

public:
  /*! \brief
    Default constructor: constructs the (zero-dimension space)
    constant monomial \f$0\f$.
  */
  Monomial();

  //! Ordinary copy-constructor.
  Monomial(const Monomial& y);

  //! Constructs the monomial \p n.
  explicit Monomial(Coefficient_traits::const_reference n);

  //! Constructs the monomial <CODE>n * t</CODE>.
  Monomial(const Term& t, Coefficient_traits::const_reference n);

  //! Constructs the monomial <CODE>n * t</CODE>.
  Monomial(Coefficient_traits::const_reference n, const Term& t);

  //! Constructs the monomial \p t.
  explicit Monomial(const Term& t);

  //! Builds the monomial corresponding to the variable \p v.
  /*! \relates Monomial
    \exception std::length_error
    Thrown if the space dimension of \p v exceeds
    <CODE>Monomial::max_space_dimension()</CODE>.
  */
  explicit Monomial(const Variable v);

  //! Destructor.
  ~Monomial();

  //! Returns a const reference to the term component.
  const Term& term() const;

  //! Returns a const reference to the coefficient component.
  const Coefficient& coefficient() const;

  //! Returns the maximum space dimension a Monomial can handle.
  static dimension_type max_space_dimension();

  //! Returns the maximum exponent a Monomial can handle.
  static dimension_type max_exponent();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the exponent of \p v in \p *this.
  exponent_type exponent(Variable v) const;

  /*! \brief
    If \p *this is not identically zero, returns the degree of \p *this;
    returns \f$0\f$ otherwise.

    \warning
    It is customary to define the degree of \f$0\f$ as \f$-\infty\f$.
    This method, however, does not implement that definition.
  */
  degree_type degree() const;

  /*! \brief
    If \p *this is not identically zero, returns the degree of \p v
    in \p *this; returns \f$0\f$ otherwise.

    \warning
    It is customary to define the degree of \f$0\f$ as \f$-\infty\f$.
    This method, however, does not implement that definition.
  */
  degree_type degree(Variable v) const;

  //! Returns the (zero-dimension space) constant monomial \f$1\f$.
  static const Monomial& one();

  //! Returns the (zero-dimension space) constant monomial \f$0\f$.
  static const Monomial& zero();

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  PPL_OUTPUT_DECLARATIONS;

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
  void swap(Monomial& y);

  //! Binary predicate defining the total ordering on monomials.
  struct Compare {
    //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
    bool operator()(const Monomial& x, const Monomial& y) const;
  };

private:
  //! Returns a reference to the term component.
  Term& term_ref();

  //! Returns a reference to the coefficient component.
  Coefficient& coefficient_ref();

  friend Monomial&
  Parma_Polyhedra_Library::operator*=(Monomial& m,
				      Coefficient_traits::const_reference n);

  friend Monomial&
  Parma_Polyhedra_Library::operator*=(Monomial& m, Variable v);

  friend Monomial&
  Parma_Polyhedra_Library::operator*=(Monomial& m, const Term& t);

  friend Monomial&
  Parma_Polyhedra_Library::operator*=(Monomial& m1, const Monomial& m2);

  friend void
  Parma_Polyhedra_Library::pow_assign(Monomial& m, dimension_type n);

  friend Monomial&
  Parma_Polyhedra_Library
  ::exact_div_assign(Monomial& m,
		     Coefficient_traits::const_reference n);
  friend Monomial&
  Parma_Polyhedra_Library::exact_div_assign(Monomial& m,
					    Variable v);
  friend Monomial&
  Parma_Polyhedra_Library::exact_div_assign(Monomial& m,
					    const Term& t);
  friend Monomial&
  Parma_Polyhedra_Library::exact_div_assign(Monomial& m1,
					    const Monomial& m2);

  friend class Parma_Polyhedra_Library::Polynomial;
  friend class Parma_Polyhedra_Library::Polynomial::const_iterator;

};

#include "Monomial.inlines.hh"

#endif // !defined(PPL_Monomial_defs_hh)
