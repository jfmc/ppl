/* Polynomial class declaration.
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

#ifndef PPL_Polynomial_defs_hh
#define PPL_Polynomial_defs_hh 1

#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.types.hh"
#include "Polynomial.types.hh"
#include "Monomial.types.hh"
#include "Polynomial_Constraint.types.hh"
#include "Polynomial_Space.types.hh"
#include "Term.defs.hh"
#include <iterator>
#include <map>

namespace Parma_Polyhedra_Library {
// Put them in the namespace here to declare them friend later.

//! Returns the polynomial <CODE>p1 + p2</CODE>.
/*! \relates Polynomial */
Polynomial
operator+(const Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>n + p</CODE>.
/*! \relates Polynomial */
Polynomial
operator+(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the polynomial <CODE>p + n</CODE>..
/*! \relates Polynomial */
Polynomial
operator+(const Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>v + p</CODE>.
/*! \relates Polynomial */
Polynomial
operator+(Variable v, const Polynomial& p);

//! Returns the polynomial <CODE>p + v</CODE>.
/*! \relates Polynomial */
Polynomial
operator+(const Polynomial& p, Variable v);

//! Returns the polynomial \p p.
/*! \relates Polynomial */
Polynomial
operator+(const Polynomial& p);

//! Returns the polynomial <CODE>-p</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(const Polynomial& p);

//! Returns the polynomial <CODE>p1 - p2</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(const Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>n - p</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the polynomial <CODE>p - n</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(const Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>v - p</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(Variable v, const Polynomial& p);

//! Returns the polynomial <CODE>p - v</CODE>.
/*! \relates Polynomial */
Polynomial
operator-(const Polynomial& p, Variable v);

//! Returns the polynomial <CODE>p1 * p2</CODE>.
/*! \relates Polynomial */
Polynomial
operator*(const Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>n * p</CODE>.
/*! \relates Polynomial */
Polynomial
operator*(Coefficient_traits::const_reference n, const Polynomial& p);

//! Returns the polynomial <CODE>p * n</CODE>.
/*! \relates Polynomial */
Polynomial
operator*(const Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>v * p</CODE>.
/*! \relates Polynomial */
Polynomial
operator*(Variable v, const Polynomial& p);

//! Returns the polynomial <CODE>p * v</CODE>.
/*! \relates Polynomial */
Polynomial
operator*(const Polynomial& p, Variable v);

//! Returns the polynomial <CODE>p1 + p2</CODE> and assigns it to \p p1.
/*! \relates Polynomial */
Polynomial&
operator+=(Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>p + v</CODE> and assigns it to \p p.
/*! \relates Polynomial
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Polynomial::max_space_dimension()</CODE>.
 */
Polynomial&
operator+=(Polynomial& p, Variable v);

//! Returns the polynomial <CODE>p + n</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator+=(Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>p1 - p2</CODE> and assigns it to \p p1.
/*! \relates Polynomial */
Polynomial&
operator-=(Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>p - v</CODE> and assigns it to \p p.
/*! \relates Polynomial
  \exception std::length_error
  Thrown if the space dimension of \p v exceeds
  <CODE>Polynomial::max_space_dimension()</CODE>.
 */
Polynomial&
operator-=(Polynomial& p, Variable v);

//! Returns the polynomial <CODE>p - n</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator-=(Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>p * n</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator*=(Polynomial& p, Coefficient_traits::const_reference n);

//! Returns the polynomial <CODE>p * v</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator*=(Polynomial& p, Variable v);

//! Returns the polynomial <CODE>p * t</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator*=(Polynomial& p, const Term& t);

//! Returns the polynomial <CODE>p * m</CODE> and assigns it to \p p.
/*! \relates Polynomial */
Polynomial&
operator*=(Polynomial& p, const Monomial& m);

//! Returns the polynomial <CODE>p1 * p2</CODE> and assigns it to \p p1.
/*! \relates Polynomial */
Polynomial&
operator*=(Polynomial& p1, const Polynomial& p2);

//! Returns the polynomial <CODE>p^n</CODE>.
/*! \relates Polynomial */
Polynomial
pow(const Polynomial& p, dimension_type n);

//! Assigns to \p p the polynomial <CODE>p^n</CODE>.
/*! \relates Polynomial */
void
pow_assign(Polynomial& p, dimension_type n);

/*! \brief
  Returns the term <CODE>p/n</CODE> and assigns it to \p p,
  <EM>assuming</EM> that \p n divides \p p.

  \relates Polynomial
  The behavior is undefined if \p n does not divide \p p.
*/
Polynomial&
exact_div_assign(Polynomial& p, Coefficient_traits::const_reference n);

/*! \brief
  Returns the term <CODE>p/v</CODE> and assigns it to \p p,
  <EM>assuming</EM> that \p v divides \p p.

  \relates Polynomial
  The behavior is undefined if \p v does not divide \p p.
*/
Polynomial&
exact_div_assign(Polynomial& p, Variable v);

/*! \brief
  Returns the term <CODE>p/t</CODE> and assigns it to \p p,
  <EM>assuming</EM> that \p t divides \p p.

  \relates Polynomial
  The behavior is undefined if \p t does not divide \p p.
*/
Polynomial&
exact_div_assign(Polynomial& p, const Term& t);

/*! \brief
  Returns the term <CODE>p/m</CODE> and assigns it to \p p,
  <EM>assuming</EM> that \p m divides \p p.

  \relates Polynomial
  The behavior is undefined if \p m does not divide \p p.
*/
Polynomial&
exact_div_assign(Polynomial& p, const Monomial& m);

/*! \brief
  Returns the term <CODE>p1/p2</CODE> and assigns it to \p p1,
  <EM>assuming</EM> that \p p2 divides \p p1.

  \relates Polynomial
  The behavior is undefined if \p p2 does not divide \p p1.
*/
Polynomial&
exact_div_assign(Polynomial& p1, const Polynomial& p2);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial */
std::ostream& operator<<(std::ostream& s, const Polynomial& p);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polynomial */
void swap(Parma_Polyhedra_Library::Polynomial& x,
	  Parma_Polyhedra_Library::Polynomial& y);

} // namespace std

//! A polynomial.
/*! \ingroup PPL_CXX_interface
    An object of the class Polynomial represents the polynomial
    \f[
      \sum_{i=0}^k a_i m_i
    \f]
    where, for each \f$i\f$, \f$a_i\f$ is the integer coefficient
    of the \f$i\f$-th term \f$m_i\f$,
    \f$m_0\f$ being the unit term \f$1\f$.
    With the provision that the \f$m_i\f$'s are all distinct,
    a Polynomial can be seen as a finite set of monomials.

    // FIXME: the following should be rewritten.
    \par How to build a polynomial.

    Polynomials are the basic blocks for defining
    both constraints (i.e., linear equalities or inequalities)
    and generators (i.e., lines, rays, points and closure points).
    A full set of functions is defined to provide a convenient interface
    for building complex polynomials starting from simpler ones
    and from objects of the classes Variable and Coefficient:
    available operators include unary negation,
    binary addition and subtraction,
    as well as multiplication by a Coefficient.
    The space dimension of a polynomial is defined as the maximum
    space dimension of the arguments used to build it:
    in particular, the space dimension of a Variable <CODE>x</CODE>
    is defined as <CODE>x.id()+1</CODE>,
    whereas all the objects of the class Coefficient have space dimension zero.

    \par Example
    The following code builds the polynomial \f$4x - 2y - z + 14\f$,
    having space dimension \f$3\f$:
    \code
  Polynomial e = 4*x - 2*y - z + 14;
    \endcode
    Another way to build the same polynomial is:
    \code
  Polynomial e1 = 4*x;
  Polynomial e2 = 2*y;
  Polynomial e3 = z;
  Polynomial e = Polynomial(14);
  e += e1 - e2 - e3;
    \endcode
    Note that \p e1, \p e2 and \p e3 have space dimension 1, 2 and 3,
    respectively; also, in the fourth line of code, \p e is created
    with space dimension zero and then extended to space dimension 3
    in the fifth line.
*/
class Parma_Polyhedra_Library::Polynomial {
public:
  //! Default constructor: returns a copy of Polynomial::zero().
  Polynomial();

  //! Ordinary copy-constructor.
  Polynomial(const Polynomial& p);

  //! Constructs a polynomial equivalent to \p e.
  Polynomial(const Linear_Expression& e);

  //! Destructor.
  ~Polynomial();

  //! Builds the polynomial corresponding to the constant term \p n.
  explicit Polynomial(Coefficient_traits::const_reference n);

  //! Builds the polynomial corresponding to the variable \p v.
  /*!
    \exception std::length_error
    Thrown if the space dimension of \p v exceeds
    <CODE>Polynomial::max_space_dimension()</CODE>.
  */
  explicit Polynomial(Variable v);

  //! Builds the polynomial corresponding to the term \p t.
  Polynomial(const Term& t);

  //! Builds the polynomial corresponding to the monomial \p m.
  Polynomial(const Monomial& m);

  //! Returns the maximum space dimension a Polynomial can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the coefficient of \p t in \p *this.
  Coefficient_traits::const_reference coefficient(const Term& t) const;

  //! Returns the constant term of \p *this.
  Coefficient_traits::const_reference constant_term_coefficient() const;

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

  //! Returns the (zero-dimension space) constant 0.
  static const Polynomial& zero();

  //! Returns <CODE>true</CODE> if and only if \p x is equal to \p y.
  bool is_equal_to(const Polynomial& y) const;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  /*! \brief
    Assigns to \p *this its
    \ref Terms_Monomials_and_Polynomials "primitive form".
  */
  void primitive_form_assign();

  /*! \brief
    Returns <CODE>true</CODE> if and only if the polynomial is in
    \ref Terms_Monomials_and_Polynomials "primitive form".
  */
  bool check_primitive_form() const;

  /*! \brief
    Assigns to \p *this the
    \ref Terms_Monomials_and_Polynomials "primitive form"
    of the linear combination
    <CODE>y.coefficient(t) * (*this) - this->coefficient(t) * y</CODE>.

    The behavior is undefined if
    <CODE>this->coefficient(t) == 0</CODE> or
    <CODE>y.coefficient(t) == 0</CODE>.
  */
  void linear_combine(const Polynomial& y, const Term& t);

  /*! \brief
    Assigns to \p *this the primitive form of the linear combination
    <CODE>y_t * (*this) - this->coefficient(t) * y</CODE>.

    The behavior is undefined if
    <CODE>this->coefficient(t) == 0</CODE> or
    <CODE>y_t != y.coefficient(t)</CODE> or
    <CODE>y_t == 0</CODE>.
  */
  void linear_combine(const Polynomial& y, const Term& t,
		      Coefficient_traits::const_reference y_t);

  /*! \brief
    Assigns to \p *this the primitive form of the linear combination
    <CODE>y_t * (*this) - x_t * y</CODE>.

    The behavior is undefined if
    <CODE>x_t != this->coefficient(t)</CODE> or
    <CODE>x_t == 0</CODE> or
    <CODE>y_t != y.coefficient(t)</CODE> or
    <CODE>y_t == 0</CODE>.
  */
  void linear_combine(const Polynomial& y, const Term& t,
		      Coefficient_traits::const_reference x_t,
		      Coefficient_traits::const_reference y_t);

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
  void swap(Polynomial& y);

private:
  //! The type of the implementation vector.
  typedef std::map<Term, Coefficient, Term::Compare> Map;

public:
  //! A const iterator over the monomials of a polynomial.
  /*!
    A const_iterator is used to provide read-only access
    to each monomial contained in a Polynomial object.
  */
  class const_iterator
    : public std::iterator<Map::const_iterator::iterator_category,
			   Monomial,
			   Map::const_iterator::difference_type,
			   const Monomial*,
			   const Monomial&> {
  public:
    //! Default constructor.
    const_iterator();

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Destructor.
    ~const_iterator();

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    const Monomial& operator*() const;

    //! Indirect member selector.
    const Monomial* operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    //! Prefix decrement operator.
    const_iterator& operator--();

    //! Postfix decrement operator.
    const_iterator operator--(int);

    /*! \brief
      Returns <CODE>true</CODE> if and only if
      \p *this and \p y are identical.
    */
    bool operator==(const const_iterator& y) const;

    /*! \brief
      Returns <CODE>true</CODE> if and only if
      \p *this and \p y are different.
    */
    bool operator!=(const const_iterator& y) const;

  private:
    //! The implementation const iterator.
    Map::const_iterator i;

    //! Constructor.
    const_iterator(const Map::const_iterator& iter);

    friend class Polynomial;
  };

  /*! \brief
    Returns the const_iterator pointing to the first
    monomial, if \p *this is not empty; otherwise,
    returns the past-the-end const_iterator.
  */
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  //! The reverse iterator type built from Polynomial::const_iterator.
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  //! Returns a const_reverse_iterator pointing to the last monomial.
  const_reverse_iterator rbegin() const;

  //! Returns the before-the-start const_reverse_iterator.
  const_reverse_iterator rend() const;

private:
  //! The implementation vector.
  Map map;

  //! Construct a polynomial stealing the contents of \p m.
  Polynomial(Map& m);

  //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
  /*!
    The ordering is the one induced by Term::Compare.
  */
  static bool less(const Map::value_type& x, const Map::value_type& y);


  //! Implementation sizing constructor.
  /*!
    The bool parameter is just to avoid problems with
    the constructor Polynomial(Coefficient_traits::const_reference n).
  */
  Polynomial(dimension_type sz, bool);

  // TODO: write doc.
  void normalize();
  void sign_normalize();

  /*! \brief
    Shifts space dimensions towards zero, overwriting the <EM>unused</EM>
    space dimensions represented by the variables in \p unused.

    The behavior is undefined if any of the variables in \p unused has a
    positive degree in any of the terms of \p *this.
    Suppose <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE> correspond
    to dimensions 0, 1 and 2, respectively.  Then, if <CODE>unused</CODE>
    contains only <CODE>y</CODE>, applying the method to
    <CODE>z + x*pow(z, 2)</CODE> gives <CODE>y + x*pow(y, 2)</CODE> whereas
    applying the method to <CODE>z + x*y</CODE> results in undefined behavior.
  */
  void shift_space_dimensions(const Variables_Set& unused);

  /*! \brief
    Permutes the space dimensions as specified by \p perm.

    The behavior is undefined if the parameter \p perm does not encode
    a permutation of \f$\{0, \ldots, m-1\}\f$, where \f$m\f$ is equal to
    <CODE>perm.size()</CODE>.
  */
  void permute_space_dimensions(const std::vector<dimension_type>& perm);

  friend class Parma_Polyhedra_Library::Polynomial_Constraint;

  template <Parma_Polyhedra_Library::degree_type>
  friend class Parma_Polyhedra_Library::Polynomial_Space;

  friend Polynomial operator+(const Polynomial& p1, const Polynomial& p2);
  friend Polynomial operator+(Coefficient_traits::const_reference n,
			      const Polynomial& p);
  friend Polynomial operator+(const Polynomial& p,
			      Coefficient_traits::const_reference n);

  friend Polynomial& operator+=(Polynomial& p1, const Polynomial& p2);
  friend Polynomial& operator+=(Polynomial& p, Variable v);
  friend Polynomial& operator+=(Polynomial& p,
				Coefficient_traits::const_reference n);

  friend Polynomial operator-(const Polynomial& p);

  friend Polynomial operator-(const Polynomial& p1, const Polynomial& p2);
  friend Polynomial operator-(Coefficient_traits::const_reference n,
			      const Polynomial& p);
  friend Polynomial operator-(const Polynomial& p,
			      Coefficient_traits::const_reference n);

  friend Polynomial& operator-=(Polynomial& p1, const Polynomial& p2);
  friend Polynomial& operator-=(Polynomial& p, Variable v);
  friend Polynomial& operator-=(Polynomial& p,
				Coefficient_traits::const_reference n);

  friend Polynomial operator*(const Polynomial& p1, const Polynomial& p2);

  friend Polynomial operator*(Coefficient_traits::const_reference n,
			      const Polynomial& p);
  friend Polynomial operator*(const Polynomial& p,
			      Coefficient_traits::const_reference n);

  friend Polynomial& operator*=(Polynomial& p1, const Polynomial& p2);
  friend Polynomial& operator*=(Polynomial& p, const Monomial& m);
  friend Polynomial& operator*=(Polynomial& p, const Term& t);
  friend Polynomial& operator*=(Polynomial& p, Variable v);
  friend Polynomial& operator*=(Polynomial& p,
				Coefficient_traits::const_reference n);

  friend void pow_assign(Polynomial& p, dimension_type n);

  friend Polynomial& exact_div_assign(Polynomial& p,
				      Coefficient_traits::const_reference n);
  friend Polynomial& exact_div_assign(Polynomial& p, Variable v);
  friend Polynomial& exact_div_assign(Polynomial& p, const Term& t);
  friend Polynomial& exact_div_assign(Polynomial& p, const Monomial& m);
  friend Polynomial& exact_div_assign(Polynomial& p1, const Polynomial& p2);

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const Polynomial& p);
};

// Polynomial.inlines.hh is not included here on purpose.

#endif // !defined(PPL_Polynomial_defs_hh)
