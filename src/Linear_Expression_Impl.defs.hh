/* Linear_Expression_Impl class declaration.
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

#ifndef PPL_Linear_Expression_Impl_defs_hh
#define PPL_Linear_Expression_Impl_defs_hh 1

#include "Linear_Expression_Impl.types.hh"
#include "Generator.types.hh"
#include "Grid_Generator.types.hh"
#include "Constraint.types.hh"
#include "Congruence.types.hh"
#include "Variable.defs.hh"
#include "Variables_Set.defs.hh"
#include <cstddef>
#include "Linear_Expression_Interface.defs.hh"

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
std::ostream& operator<<(std::ostream& s, const Linear_Expression_Impl<Row>& e);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A linear expression.
/*! \ingroup PPL_CXX_interface
  An object of the class Linear_Expression_Impl represents the linear expression
  \f[
    \sum_{i=0}^{n-1} a_i x_i + b
  \f]
  where \f$n\f$ is the dimension of the vector space,
  each \f$a_i\f$ is the integer coefficient
  of the \f$i\f$-th variable \f$x_i\f$
  and \f$b\f$ is the integer for the inhomogeneous term.

  \par How to build a linear expression.

  Linear expressions are the basic blocks for defining
  both constraints (i.e., linear equalities or inequalities)
  and generators (i.e., lines, rays, points and closure points).
  A full set of functions is defined to provide a convenient interface
  for building complex linear expressions starting from simpler ones
  and from objects of the classes Variable and Coefficient:
  available operators include unary negation,
  binary addition and subtraction,
  as well as multiplication by a Coefficient.
  The space dimension of a linear expression is defined as the maximum
  space dimension of the arguments used to build it:
  in particular, the space dimension of a Variable <CODE>x</CODE>
  is defined as <CODE>x.id()+1</CODE>,
  whereas all the objects of the class Coefficient have space dimension zero.

  \par Example
  The following code builds the linear expression \f$4x - 2y - z + 14\f$,
  having space dimension \f$3\f$:
  \code
  Linear_Expression_Impl e = 4*x - 2*y - z + 14;
  \endcode
  Another way to build the same linear expression is:
  \code
  Linear_Expression_Impl e1 = 4*x;
  Linear_Expression_Impl e2 = 2*y;
  Linear_Expression_Impl e3 = z;
  Linear_Expression_Impl e = Linear_Expression_Impl(14);
  e += e1 - e2 - e3;
  \endcode
  Note that \p e1, \p e2 and \p e3 have space dimension 1, 2 and 3,
  respectively; also, in the fourth line of code, \p e is created
  with space dimension zero and then extended to space dimension 3
  in the fifth line.
*/
template <typename Row>
class Parma_Polyhedra_Library::Linear_Expression_Impl : public Linear_Expression_Interface {
public:
  //! Default constructor: returns a copy of Linear_Expression_Impl::zero().
  Linear_Expression_Impl();

  //! Ordinary copy constructor.
  Linear_Expression_Impl(const Linear_Expression_Impl& e);

  //! Copy constructor for other row types.
  template <typename Row2>
  Linear_Expression_Impl(const Linear_Expression_Impl<Row2>& e);

  //! Copy constructor from any implementation of Linear_Expression_Interface.
  Linear_Expression_Impl(const Linear_Expression_Interface& e);
  
  //! Destructor.
  virtual ~Linear_Expression_Impl();

  /*! \brief
    Builds the linear expression corresponding
    to the inhomogeneous term \p n.
  */
  explicit Linear_Expression_Impl(Coefficient_traits::const_reference n);

  //! Builds the linear expression corresponding to the variable \p v.
  /*!
    \exception std::length_error
    Thrown if the space dimension of \p v exceeds
    <CODE>Linear_Expression_Impl::max_space_dimension()</CODE>.
  */
  Linear_Expression_Impl(Variable v);

  //! Builds the linear expression corresponding to constraint \p c.
  /*!
    Given the constraint
    \f$c = \bigl(\sum_{i=0}^{n-1} a_i x_i + b \relsym 0\bigr)\f$,
    where \f$\mathord{\relsym} \in \{ =, \geq, > \}\f$,
    this builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i + b\f$.
    If \p c is an inequality (resp., equality) constraint, then
    the built linear expression is unique up to a positive
    (resp., non-zero) factor.
  */
  explicit Linear_Expression_Impl(const Constraint& c);

  /*! \brief
    Builds the linear expression corresponding to generator \p g
    (for points and closure points, the divisor is not copied).

    Given the generator
    \f$g = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$
    (where, for lines and rays, we have \f$d = 1\f$),
    this builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i\f$.
    The inhomogeneous term of the linear expression will always be 0.
    If \p g is a ray, point or closure point (resp., a line), then
    the linear expression is unique up to a positive
    (resp., non-zero) factor.
  */
  explicit Linear_Expression_Impl(const Generator& g);

  /*! \brief
    Builds the linear expression corresponding to grid generator \p g
    (for points, parameters and lines the divisor is not copied).

    Given the grid generator
    \f$g = (\frac{a_0}{d}, \ldots, \frac{a_{n-1}}{d})^\transpose\f$
    this builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i\f$.
    The inhomogeneous term of the linear expression is always 0.
  */
  explicit Linear_Expression_Impl(const Grid_Generator& g);

  //! Builds the linear expression corresponding to congruence \p cg.
  /*!
    Given the congruence
    \f$cg = \bigl(\sum_{i=0}^{n-1} a_i x_i + b = 0 \pmod{m}\bigr)\f$,
    this builds the linear expression \f$\sum_{i=0}^{n-1} a_i x_i + b\f$.
  */
  explicit Linear_Expression_Impl(const Congruence& cg);

  //! Returns the maximum space dimension a Linear_Expression_Impl can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  virtual dimension_type space_dimension() const;

  //! Sets the dimension of the vector space enclosing \p *this to \p n .
  virtual void set_space_dimension(dimension_type n);

  //! Returns the coefficient of \p v in \p *this.
  virtual Coefficient_traits::const_reference coefficient(Variable v) const;

  //! Sets the coefficient of \p v in \p *this to \p n.
  virtual void set_coefficient(Variable v,
                               Coefficient_traits::const_reference n);

  //! Returns the inhomogeneous term of \p *this.
  virtual Coefficient_traits::const_reference inhomogeneous_term() const;

  //! Sets the inhomogeneous term of \p *this to \p n.
  virtual void set_inhomogeneous_term(Coefficient_traits::const_reference n);

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
  virtual void linear_combine(const Linear_Expression_Interface& y, Variable v);

  //! Equivalent to <CODE>*this = *this * c1 + y * c2</CODE>, but assumes that
  //! \p *this and \p y have the same space dimension.
  virtual void linear_combine(const Linear_Expression_Interface& y,
                              Coefficient_traits::const_reference c1,
                              Coefficient_traits::const_reference c2);

  //! Swaps the coefficients of the variables \p v1 and \p v2 .
  virtual void swap_space_dimensions(Variable v1, Variable v2);

  //! Removes all the specified dimensions from the expression.
  /*!
    The space dimension of the variable with the highest space
    dimension in \p vars must be at most the space dimension
    of \p this.
  */
  virtual void remove_space_dimensions(const Variables_Set& vars);

  //! Shift by \p n positions the coefficients of variables, starting from
  //! the coefficient of \p v. This increases the space dimension by \p n.
  virtual void shift_space_dimensions(Variable v, dimension_type n);

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
  virtual void permute_space_dimensions(const std::vector<Variable>& cycle);
  
  //! Returns <CODE>true</CODE> if and only if \p *this is \f$0\f$.
  virtual bool is_zero() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if all the homogeneous
    terms of \p *this are \f$0\f$.
  */
  virtual bool all_homogeneous_terms_are_zero() const;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  virtual memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  virtual memory_size_type external_memory_in_bytes() const;

  //! Writes to \p s an ASCII representation of \p *this.
  virtual void ascii_dump(std::ostream& s) const;
  
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  virtual bool ascii_load(std::istream& s);

  //! Swaps \p *this with \p y.
  virtual void swap(Linear_Expression_Interface& y);

  // TODO: Make this private.
  //! Copy constructor with a specified space dimension.
  Linear_Expression_Impl(const Linear_Expression_Interface& e, dimension_type sz);

  //! Returns \p true if *this is equal to \p x.
  //! Note that (*this == x) has a completely different meaning.
  virtual bool is_equal_to(const Linear_Expression_Interface& x) const;

  //! Normalizes the modulo of the coefficients and of the inhomogeneous term
  //! so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the coefficients
    and the inhomogeneous term and normalizes them by the GCD itself.
  */
  virtual void normalize();

  //! Ensures that the first nonzero homogeneous coefficient is positive,
  //! by negating the row if necessary.
  virtual void sign_normalize();

  /*! \brief
    Negates the elements from index \p first (included)
    to index \p last (excluded).
  */
  virtual void negate(dimension_type first, dimension_type last);

  virtual Linear_Expression_Impl& operator+=(Coefficient_traits::const_reference n);
  virtual Linear_Expression_Impl& operator-=(Coefficient_traits::const_reference n);

  //! The basic comparison function.
  /*! \relates Linear_Expression_Impl

    \returns -1 or -2 if x is less than y, 0 if they are equal and 1 or 2 is y
            is greater. The absolute value of the result is 1 if the difference
            is only in the inhomogeneous terms, 2 otherwise

    The order is a lexicographic. It starts comparing the variables' coefficient,
    starting from Variable(0), and at the end it compares the inhomogeneous
    terms.
  */
  virtual int compare(const Linear_Expression_Interface& y) const;

  virtual Linear_Expression_Impl& operator+=(const Linear_Expression_Interface& e2);
  virtual Linear_Expression_Impl& operator+=(const Variable v);
  virtual Linear_Expression_Impl& operator-=(const Linear_Expression_Interface& e2);
  virtual Linear_Expression_Impl& operator-=(const Variable v);
  virtual Linear_Expression_Impl& operator*=(Coefficient_traits::const_reference n);
  virtual Linear_Expression_Impl& operator/=(Coefficient_traits::const_reference n);

  virtual void negate();

  virtual Linear_Expression_Impl& add_mul_assign(Coefficient_traits::const_reference n,
                                                 const Variable v);

  virtual Linear_Expression_Impl& sub_mul_assign(Coefficient_traits::const_reference n,
                                                 const Variable v);

  virtual Linear_Expression_Impl& sub_mul_assign(Coefficient_traits::const_reference n,
                                                 const Linear_Expression_Interface& y,
                                                 dimension_type start, dimension_type end);

  virtual void add_mul_assign(Coefficient_traits::const_reference factor,
                              const Linear_Expression_Interface& e2);

  virtual void sub_mul_assign(Coefficient_traits::const_reference factor,
                              const Linear_Expression_Interface& e2);

  //! NOTE: This can be used as <CODE>(*this) << s</CODE>, but it actually
  //! means <CODE>s << *this</CODE>. This strange syntax is needed because
  //! this must be a method and can't be a free function.
  virtual std::ostream& operator<<(std::ostream& s) const;

  /*! \brief
    Returns <CODE>true</CODE> if the coefficient of each variable in
    \p vars[i] is \f$0\f$.
  */
  virtual bool all_zeroes(const Variables_Set& vars) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual Coefficient& operator[](dimension_type i);
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual const Coefficient& operator[](dimension_type i) const;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Equivalent to the const version of operator[].
  virtual const Coefficient& get(dimension_type i) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns <CODE>true</CODE> if (*this)[i] is \f$0\f$, for each i in
    [start, end).
  */
  virtual bool all_zeroes(dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns the number of zero coefficient in [start, end).
  */
  virtual dimension_type num_zeroes(dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns the gcd of the nonzero coefficients in [start,end). If all the
    coefficients in this range are 0 returns 0.
  */
  virtual Coefficient gcd(dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  virtual void exact_div_assign(Coefficient_traits::const_reference c,
                                dimension_type start, dimension_type end);

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
  virtual void linear_combine(const Linear_Expression_Interface& y, dimension_type i);

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Equivalent to <CODE>(*this)[i] = (*this)[i] * c1 + y[i] * c2</CODE>,
  //! for each i in [start, end).
  virtual void linear_combine(const Linear_Expression_Interface& y,
                              Coefficient_traits::const_reference c1,
                              Coefficient_traits::const_reference c2,
                              dimension_type start, dimension_type end);

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Modify `new_ray' according to the evolution of `x_g' with
  //! respect to `y_g'. This method is a code fragment used by Polyhedron.
  //! Read the method implementation for more details.
  virtual void modify_according_to_evolution(const Linear_Expression_Interface& x,
                                             const Linear_Expression_Interface& y);

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Returns the index of the last nonzero element, or 0 if there are no
  //! nonzero elements.
  virtual dimension_type last_nonzero() const;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  /*! \brief
    Returns <CODE>true</CODE> if each coefficient in [start,end) is *not* in
    \f$0\f$, disregarding coefficients of variables in \p vars.
  */
  virtual bool all_zeroes_except(const Variables_Set& vars, dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Sets results to the sum of (*this)[i]*y[i], for each i in [start,end).
  virtual void scalar_product_assign(Coefficient& result, const Linear_Expression_Interface& y,
                                     dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Computes the sign of the sum of (*this)[i]*y[i], for each i in [start,end).
  virtual int scalar_product_sign(const Linear_Expression_Interface& y,
                                  dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Returns the index of the first nonzero element, or \p last if there are no
  //! nonzero elements, considering only elements in [first,last).
  virtual dimension_type first_nonzero(dimension_type first, dimension_type last) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Removes from the set x all the indexes of nonzero elements of *this.
  virtual void has_a_free_dimension_helper(std::set<dimension_type>& x) const;
  
  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Returns \p true if (*this)[i] is equal to x[i], for each i in [start,end).
  virtual bool is_equal_to(const Linear_Expression_Interface& x,
                           dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Returns \p true if (*this)[i]*c1 is equal to x[i]*c2, for each i in
  //! [start,end).
  virtual bool is_equal_to(const Linear_Expression_Interface& x,
                           Coefficient_traits::const_reference c1,
                           Coefficient_traits::const_reference c2,
                           dimension_type start, dimension_type end) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Sets `row' to a copy of the row that implements *this.
  virtual void get_row(Dense_Row& row) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Sets `row' to a copy of the row that implements *this.
  virtual void get_row(Sparse_Row& row) const;

  // NOTE: This method is public, but it's not exposed in Linear_Expression,
  // so that it can be used internally in the PPL, by friends of
  // Linear_Expression.
  //! Checks if *this, interpreted as an objective function of a linear
  //! problem, is unbounded.
  virtual bool is_unbounded_obj_function(
    const std::vector<std::pair<dimension_type, dimension_type> >& mapping,
    Optimization_Mode optimization_mode) const;

  //! Implementation sizing constructor.
  /*!
    The bool parameter is just to avoid problems with
    the constructor Linear_Expression_Impl(Coefficient_traits::const_reference n).
  */
  Linear_Expression_Impl(dimension_type sz, bool);

  //! Builds the linear expression corresponding to congruence \p cg, and
  //! with the specified size.
  /*!
    Given the congruence
    \f$cg = \bigl(\sum_{i=0}^{n-1} a_i x_i + b = 0 \pmod{m}\bigr)\f$,
    this builds the linear expression \f$\sum_{i=0}^{sz-1} a_i x_i + b\f$.
  */
  Linear_Expression_Impl(const Congruence& cg, dimension_type sz);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

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
  template <typename Row2>
  void linear_combine(const Linear_Expression_Impl<Row2>& y, Variable v);

  //! Equivalent to <CODE>*this = *this * c1 + y * c2</CODE>, but assumes that
  //! \p *this and \p y have the same space dimension.
  template <typename Row2>
  void linear_combine(const Linear_Expression_Impl<Row2>& y,
                      Coefficient_traits::const_reference c1,
                      Coefficient_traits::const_reference c2);

  //! Swaps \p *this with \p y.
  template <typename Row2>
  void swap(Linear_Expression_Impl<Row2>& y);

  //! Returns \p true if *this is equal to \p x.
  //! Note that (*this == x) has a completely different meaning.
  template <typename Row2>
  bool is_equal_to(const Linear_Expression_Impl<Row2>& x) const;

  template <typename Row2>
  Linear_Expression_Impl& operator+=(const Linear_Expression_Impl<Row2>& e2);
  template <typename Row2>
  Linear_Expression_Impl& operator-=(const Linear_Expression_Impl<Row2>& e2);

  template <typename Row2>
  Linear_Expression_Impl& sub_mul_assign(Coefficient_traits::const_reference n,
                                         const Linear_Expression_Impl<Row2>& y,
                                         dimension_type start, dimension_type end);

  template <typename Row2>
  void add_mul_assign(Coefficient_traits::const_reference factor,
                      const Linear_Expression_Impl<Row2>& e2);

  template <typename Row2>
  void sub_mul_assign(Coefficient_traits::const_reference factor,
                      const Linear_Expression_Impl<Row2>& e2);

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
  template <typename Row2>
  void linear_combine(const Linear_Expression_Impl<Row2>& y, dimension_type i);

  //! Equivalent to <CODE>(*this)[i] = (*this)[i] * c1 + y[i] * c2</CODE>,
  //! for each i in [start, end).
  template <typename Row2>
  void linear_combine(const Linear_Expression_Impl<Row2>& y,
                      Coefficient_traits::const_reference c1,
                      Coefficient_traits::const_reference c2,
                      dimension_type start, dimension_type end);

  //! Modify `new_ray' according to the evolution of `x_g' with
  //! respect to `y_g'. This method is a code fragment used by Polyhedron.
  //! Read the method implementation for more details.
  template <typename Row2, typename Row3>
  void modify_according_to_evolution(const Linear_Expression_Impl<Row2>& x,
                                     const Linear_Expression_Impl<Row3>& y);
  
  //! The basic comparison function.
  /*! \relates Linear_Expression_Impl

    \returns -1 or -2 if x is less than y, 0 if they are equal and 1 or 2 is y
            is greater. The absolute value of the result is 1 if the difference
            is only in the inhomogeneous terms, 2 otherwise

    The order is a lexicographic. It starts comparing the variables' coefficient,
    starting from Variable(0), and at the end it compares the inhomogeneous
    terms.
  */
  template <typename Row2>
  int compare(const Linear_Expression_Impl<Row2>& y) const;
  
  //! Sets results to the sum of (*this)[i]*y[i], for each i in [start,end).
  template <typename Row2>
  void scalar_product_assign(Coefficient& result, const Linear_Expression_Impl<Row2>& y,
                             dimension_type start, dimension_type end) const;

  //! Computes the sign of the sum of (*this)[i]*y[i], for each i in [start,end).
  template <typename Row2>
  int scalar_product_sign(const Linear_Expression_Impl<Row2>& y,
                          dimension_type start, dimension_type end) const;

  //! Returns \p true if (*this)[i] is equal to x[i], for each i in [start,end).
  template <typename Row2>
  bool is_equal_to(const Linear_Expression_Impl<Row2>& x,
                   dimension_type start, dimension_type end) const;

  //! Returns \p true if (*this)[i]*c1 is equal to x[i]*c2, for each i in
  //! [start,end).
  template <typename Row2>
  bool is_equal_to(const Linear_Expression_Impl<Row2>& x,
                   Coefficient_traits::const_reference c1,
                   Coefficient_traits::const_reference c2,
                   dimension_type start, dimension_type end) const;
private:

  void construct(const Linear_Expression_Interface& e);
  void construct(const Linear_Expression_Interface& e, dimension_type sz);

  template <typename Row2>
  void construct(const Linear_Expression_Impl<Row2>& e);
  template <typename Row2>
  void construct(const Linear_Expression_Impl<Row2>& e, dimension_type sz);

  Row row;

  template <typename Row2>
  friend class Linear_Expression_Impl;
};

#include "Linear_Expression_Impl.inlines.hh"
#include "Linear_Expression_Impl.templates.hh"

#endif // !defined(PPL_Linear_Expression_Impl_defs_hh)
