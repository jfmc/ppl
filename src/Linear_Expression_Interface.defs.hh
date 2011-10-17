/* Linear_Expression_Interface class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Linear_Expression_Interface_defs_hh
#define PPL_Linear_Expression_Interface_defs_hh 1

#include "Linear_Expression_Interface.types.hh"
#include "Coefficient.defs.hh"
#include "Variable.types.hh"
#include "Variables_Set.types.hh"
#include <vector>

//! A linear expression.
/*! \ingroup PPL_CXX_interface
  An object of a class implementing Linear_Expression_Interface represents a
  linear expression
  \f[
    \sum_{i=0}^{n-1} a_i x_i + b
  \f]
  where \f$n\f$ is the dimension of the vector space,
  each \f$a_i\f$ is the integer coefficient
  of the \f$i\f$-th variable \f$x_i\f$
  and \f$b\f$ is the integer for the inhomogeneous term.
*/
class Parma_Polyhedra_Library::Linear_Expression_Interface {
public:
  virtual ~Linear_Expression_Interface();

  //! Returns the dimension of the vector space enclosing \p *this.
  virtual dimension_type space_dimension() const = 0;

  //! Sets the dimension of the vector space enclosing \p *this to \p n .
  virtual void set_space_dimension(dimension_type n) = 0;

  //! Returns the coefficient of \p v in \p *this.
  virtual Coefficient_traits::const_reference coefficient(Variable v) const = 0;

  //! Sets the coefficient of \p v in \p *this to \p n.
  virtual void set_coefficient(Variable v,
                               Coefficient_traits::const_reference n) = 0;

  //! Returns the inhomogeneous term of \p *this.
  virtual Coefficient_traits::const_reference inhomogeneous_term() const = 0;

  //! Sets the inhomogeneous term of \p *this to \p n.
  virtual void set_inhomogeneous_term(Coefficient_traits::const_reference n) = 0;

  //! Linearly combines \p *this with \p y so that the coefficient of \p v
  //! is 0.
  /*!
    \param y
    The expression that will be combined with \p *this object;

    \param v
    The variable whose coefficient has to become \f$0\f$.

    Computes a linear combination of \p *this and \p y having
    the coefficient of variable \p v equal to \f$0\f$. Then it assigns
    the resulting expression to \p *this.

    \p *this and \p y must have the same space dimension.
  */
  virtual void linear_combine(const Linear_Expression_Interface& y, Variable v) = 0;

  //! Equivalent to <CODE>*this = *this * c1 + y * c2</CODE>, but assumes that
  //! \p *this and \p y have the same space dimension.
  virtual void linear_combine(const Linear_Expression_Interface& y,
                              Coefficient_traits::const_reference c1,
                              Coefficient_traits::const_reference c2) = 0;

  //! Swaps the coefficients of the variables \p v1 and \p v2 .
  virtual void swap_space_dimensions(Variable v1, Variable v2) = 0;

  //! Removes all the specified dimensions from the expression.
  /*!
    The space dimension of the variable with the highest space
    dimension in \p vars must be at most the space dimension
    of \p this.
  */
  virtual void remove_space_dimensions(const Variables_Set& vars) = 0;

  //! Shift by \p n positions the coefficients of variables, starting from
  //! the coefficient of \p v. This increases the space dimension by \p n.
  virtual void shift_space_dimensions(Variable v, dimension_type n) = 0;

  //! Permutes the space dimensions of the expression.
  /*!
    \param cycle
    A vector representing a cycle of the permutation according to which the
    space dimensions must be rearranged.

    The \p cycle vector represents a cycle of a permutation of space
    dimensions.
    For example, the permutation
    \f$ \{ x_1 \mapsto x_2, x_2 \mapsto x_3, x_3 \mapsto x_1 \}\f$ can be
    represented by the vector containing \f$ x_1, x_2, x_3 \f$.
  */
  virtual void permute_space_dimensions(const std::vector<Variable>& cycle) = 0;
  
  //! Returns <CODE>true</CODE> if and only if \p *this is \f$0\f$.
  virtual bool is_zero() const = 0;

  /*! \brief
    Returns <CODE>true</CODE> if and only if all the homogeneous
    terms of \p *this are \f$0\f$.
  */
  virtual bool all_homogeneous_terms_are_zero() const = 0;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  virtual memory_size_type total_memory_in_bytes() const = 0;

  //! Returns the size in bytes of the memory managed by \p *this.
  virtual memory_size_type external_memory_in_bytes() const = 0;

  //! Writes to \p s an ASCII representation of \p *this.
  virtual void ascii_dump(std::ostream& s) const = 0;

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  virtual bool ascii_load(std::istream& s) = 0;

  //! Swaps \p *this with \p y.
  virtual void swap(Linear_Expression_Interface& y) = 0;

  //! Returns \p true if *this is equal to \p x.
  //! Note that (*this == x) has a completely different meaning.
  virtual bool is_equal_to(const Linear_Expression_Interface& x) const = 0;

  //! Normalizes the modulo of the coefficients and of the inhomogeneous term
  //! so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the coefficients
    and the inhomogeneous term and normalizes them by the GCD itself.
  */
  virtual void normalize() = 0;

  //! Ensures that the first nonzero homogeneous coefficient is positive,
  //! by negating the row if necessary.
  virtual void sign_normalize() = 0;

  /*! \brief
    Negates the elements from index \p first (included)
    to index \p last (excluded).
  */
  virtual void negate(dimension_type first, dimension_type last) = 0;

  virtual Linear_Expression_Interface& operator+=(Coefficient_traits::const_reference n) = 0;
  virtual Linear_Expression_Interface& operator-=(Coefficient_traits::const_reference n) = 0;

  //! The basic comparison function.
  /*! \relates Linear_Expression_Interface

    \returns -1 or -2 if x is less than y, 0 if they are equal and 1 or 2 is y
            is greater. The absolute value of the result is 1 if the difference
            is only in the inhomogeneous terms, 2 otherwise

    The order is a lexicographic. It starts comparing the variables' coefficient,
    starting from Variable(0), and at the end it compares the inhomogeneous
    terms.
  */
  virtual int compare(const Linear_Expression_Interface& y) const = 0;

  virtual Linear_Expression_Interface& operator+=(const Linear_Expression_Interface& e2) = 0;
  virtual Linear_Expression_Interface& operator+=(const Variable v) = 0;
  virtual Linear_Expression_Interface& operator-=(const Linear_Expression_Interface& e2) = 0;
  virtual Linear_Expression_Interface& operator-=(const Variable v) = 0;
  virtual Linear_Expression_Interface& operator*=(Coefficient_traits::const_reference n) = 0;
  virtual Linear_Expression_Interface& operator/=(Coefficient_traits::const_reference n) = 0;

  virtual void negate() = 0;

  virtual Linear_Expression_Interface& add_mul_assign(Coefficient_traits::const_reference n,
                                                      const Variable v) = 0;

  virtual Linear_Expression_Interface& sub_mul_assign(Coefficient_traits::const_reference n,
                                                      const Variable v) = 0;

  virtual Linear_Expression_Interface& sub_mul_assign(Coefficient_traits::const_reference n,
                                                      const Linear_Expression_Interface& y,
                                                      dimension_type start, dimension_type end) = 0;

  virtual void add_mul_assign(Coefficient_traits::const_reference factor,
                              const Linear_Expression_Interface& e2) = 0;

  virtual void sub_mul_assign(Coefficient_traits::const_reference factor,
                              const Linear_Expression_Interface& e2) = 0;

  //! NOTE: This can be used as <CODE>(*this) << s</CODE>, but it actually
  //! means <CODE>s << *this</CODE>. This strange syntax is needed because
  //! this must be a method and can't be a free function.
  virtual std::ostream& operator<<(std::ostream& s) const = 0;

  /*! \brief
    Returns <CODE>true</CODE> if the coefficient of each variable in
    \p vars[i] is \f$0\f$.
  */
  virtual bool all_zeroes(const Variables_Set& vars) const = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual Coefficient& operator[](dimension_type i) = 0;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual const Coefficient& operator[](dimension_type i) const = 0;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Equivalent to the const version of operator[].
  virtual const Coefficient& get(dimension_type i) const = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns <CODE>true</CODE> if (*this)[i] is \f$0\f$, for each i in
    [start, end).
  */
  virtual bool all_zeroes(dimension_type start, dimension_type end) const = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns the number of zero coefficient in [start, end).
  */
  virtual dimension_type num_zeroes(dimension_type start, dimension_type end) const = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns the gcd of the nonzero coefficients in [start,end). If all the
    coefficients in this range are 0 returns 0.
  */
  virtual Coefficient gcd(dimension_type start, dimension_type end) const = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual void exact_div_assign(Coefficient_traits::const_reference c,
                                dimension_type start, dimension_type end) = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Linearly combines \p *this with \p y so that the coefficient of \p v
  //! is 0.
  /*!
    \param y
    The expression that will be combined with \p *this object;

    \param i
    The index of the coefficient that has to become \f$0\f$.

    Computes a linear combination of \p *this and \p y having
    the i-th coefficient equal to \f$0\f$. Then it assigns
    the resulting expression to \p *this.

    \p *this and \p y must have the same space dimension.
  */
  virtual void linear_combine(const Linear_Expression_Interface& y, dimension_type i) = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Equivalent to <CODE>(*this)[i] = (*this)[i] * c1 + y[i] * c2</CODE>,
  //! for each i in [start, end).
  virtual void linear_combine(const Linear_Expression_Interface& y,
                              Coefficient_traits::const_reference c1,
                              Coefficient_traits::const_reference c2,
                              dimension_type start, dimension_type end) = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Modify `new_ray' according to the evolution of `x_g' with
  //! respect to `y_g'. This method is a code fragment used by Polyhedron.
  //! Read the method implementation for more details.
  virtual void modify_according_to_evolution(const Linear_Expression_Interface& x,
                                             const Linear_Expression_Interface& y) = 0;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Returns the index of the last nonzero element, or 0 if there are no
  //! nonzero elements.
  virtual dimension_type last_nonzero() const = 0;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns <CODE>true</CODE> if each coefficient in [start,end) is *not* in
    \f$0\f$, disregarding coefficients of variables in \p vars.
  */
  virtual bool all_zeroes_except(const Variables_Set& vars, dimension_type start, dimension_type end) const = 0;
};

#endif // !defined(PPL_Linear_Expression_Interface_defs_hh)
