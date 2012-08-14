/* Expression_Adapter class declaration.
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#ifndef PPL_Expression_Adapter_defs_hh
#define PPL_Expression_Adapter_defs_hh 1

#include "Expression_Adapter.types.hh"
#include "Variable.types.hh"
#include "Variables_Set.types.hh"
#include "Dense_Row.defs.hh"
#include "Sparse_Row.defs.hh"

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An adapter for Linear_Expression-like objects.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T>
class Parma_Polyhedra_Library::Expression_Adapter {
private:
  // NOTE: this class is meant to have no constructors at all.
  // Private and not implemented: copy and assignment are not allowed.
  Expression_Adapter(const Expression_Adapter&);
  Expression_Adapter& operator=(const Expression_Adapter&);

public:
  //! \name Typedefs meant be only used internally.
  //@{
  //! The type we are adapting (could be itself an adapter).
  typedef T obj_type;
  //! The type of expressions as provided by \c obj_type.
  typedef typename T::expr_type obj_expr_type;
  //! The type of expressions as provided by this adapter.
  typedef Expression_Adapter<T> expr_type;
  //@} // Typedefs meant be only used internally.

  //! The type of const iterators on wrapped coefficients.
  typedef typename obj_expr_type::const_iterator const_iterator;

  //! Returns the current representation of *this.
  Representation representation() const;

  //! Returns an iterator that points to the first nonzero coefficient in the
  //! expression.
  const_iterator begin() const;

  //! Returns an iterator that points to the last nonzero coefficient in the
  //! expression.
  const_iterator end() const;

  //! Returns an iterator that points to the first nonzero coefficient of a
  //! variable bigger than or equal to \p v.
  const_iterator lower_bound(Variable v) const;

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the coefficient of \p v in \p *this.
  Coefficient_traits::const_reference coefficient(Variable v) const;

  //! Returns the inhomogeneous term of \p *this.
  Coefficient_traits::const_reference inhomogeneous_term() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is \f$0\f$.
  bool is_zero() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if all the homogeneous
    terms of \p *this are \f$0\f$.
  */
  bool all_homogeneous_terms_are_zero() const;

  //! Returns \p true if *this is equal to \p x.
  //! Note that (*this == x) has a completely different meaning.
  template <typename Expression2>
  bool is_equal_to(const Expression2& x) const;

  /*! \brief
    Returns <CODE>true</CODE> if the coefficient of each variable in
    \p vars[i] is \f$0\f$.
  */
  bool all_zeroes(const Variables_Set& vars) const;

  //! \name Member functions meant to be used internally.
  //@{

  //! Helper for accessing the wrapped object.
  const obj_type& obj() const;

  //! Helper for accessing the expression inside the wrapped object.
  const obj_expr_type& obj_expr() const;

  //! Return \c true if it would make sense to hide last coefficient.
  //! Note: this is queried by derived class Expression_Hide_Last.
  bool hiding_last() const;

  //! Returns the i-th coefficient.
  Coefficient_traits::const_reference get(dimension_type i) const;

  //! Returns the coefficient of v.
  Coefficient_traits::const_reference get(Variable v) const;

  /*! \brief
    Returns <CODE>true</CODE> if (*this)[i] is \f$0\f$, for each i in
    [start, end).
  */
  bool all_zeroes(dimension_type start, dimension_type end) const;

  /*! \brief
    Returns the number of zero coefficient in [start, end).
  */
  dimension_type num_zeroes(dimension_type start, dimension_type end) const;

  /*! \brief
    Returns the gcd of the nonzero coefficients in [start,end).
    Returns 0 if all the coefficients in the range are 0.
  */
  Coefficient gcd(dimension_type start, dimension_type end) const;

  //! Returns the index of the last nonzero element, or 0 if there are no
  //! nonzero elements.
  dimension_type last_nonzero() const;

  //! Returns the index of the last nonzero element in [first,last), or last
  //! if there are no nonzero elements.
  dimension_type last_nonzero(dimension_type first, dimension_type last) const;

  //! Returns the index of the first nonzero element, or \p last if there are no
  //! nonzero elements, considering only elements in [first,last).
  dimension_type first_nonzero(dimension_type first, dimension_type last) const;

  /*! \brief
    Returns <CODE>true</CODE> if each coefficient in [start,end) is *not* in
    \f$0\f$, disregarding coefficients of variables in \p vars.
  */
  bool all_zeroes_except(const Variables_Set& vars,
                         dimension_type start, dimension_type end) const;

  //! Removes from the set x all the indexes of nonzero elements of *this.
  void has_a_free_dimension_helper(std::set<dimension_type>& x) const;

  //! Returns \p true if (*this)[i] is equal to x[i], for each i in [start,end).
  template <typename Expression2>
  bool is_equal_to(const Expression2& x,
                   dimension_type start, dimension_type end) const;

  //! Returns \p true if (*this)[i]*c1 is equal to x[i]*c2, for each i in
  //! [start,end).
  template <typename Expression2>
  bool is_equal_to(const Expression2& x,
                   Coefficient_traits::const_reference c1,
                   Coefficient_traits::const_reference c2,
                   dimension_type start, dimension_type end) const;

  //! Sets `row' to a copy of the row that implements *this.
  void get_row(Dense_Row& row) const;

  //! Sets `row' to a copy of the row that implements *this.
  void get_row(Sparse_Row& row) const;

  //! Returns true if there is a variable in [first,last) whose coefficient
  //! is nonzero in both *this and x.
  template <typename Expression2>
  bool have_a_common_variable(const Expression2& x,
                              Variable first, Variable last) const;

  //@} // Member functions meant to be used internally.
};

namespace Parma_Polyhedra_Library {

#define PPL_DECLARE_EXPR_ADAPTER_MEMBER_SPEC(Class) \
template <> \
Expression_Adapter<Class>::obj_expr_type const& \
Expression_Adapter<Class>::obj_expr() const; \
template <> \
bool Expression_Adapter<Class>::hiding_last() const;

} // namespace Parma_Polyhedra_Library

#include "Expression_Adapter.inlines.hh"

#endif // !defined(PPL_Expression_Adapter_defs_hh)
