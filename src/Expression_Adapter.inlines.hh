/* Expression_Adapter class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
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

#ifndef PPL_Expression_Adapter_inlines_hh
#define PPL_Expression_Adapter_inlines_hh 1

#include "Variables_Set.defs.hh"
#include "Linear_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
inline typename Expression_Adapter<T>::obj_type const&
Expression_Adapter<T>::obj() const {
  return reinterpret_cast<const obj_type&>(*this);
}

template <typename T>
inline typename Expression_Adapter<T>::obj_expr_type const&
Expression_Adapter<T>::obj_expr() const {
  return obj();
}

template <typename T>
inline bool
Expression_Adapter<T>::hiding_last() const {
  return obj().hiding_last();
}

template <typename T>
inline Representation
Expression_Adapter<T>::representation() const {
  return obj_expr().representation();
}

template <typename T>
inline typename Expression_Adapter<T>::const_iterator
Expression_Adapter<T>::begin() const {
  return obj_expr().begin();
}

template <typename T>
inline typename Expression_Adapter<T>::const_iterator
Expression_Adapter<T>::end() const {
  return obj_expr().end();
}

template <typename T>
inline typename Expression_Adapter<T>::const_iterator
Expression_Adapter<T>::lower_bound(Variable v) const {
  return obj_expr().lower_bound(v);
}

template <typename T>
inline dimension_type
Expression_Adapter<T>::space_dimension() const {
  return obj_expr().space_dimension();
}

template <typename T>
inline Coefficient_traits::const_reference
Expression_Adapter<T>::coefficient(Variable v) const {
  return obj_expr().coefficient(v);
}

template <typename T>
inline Coefficient_traits::const_reference
Expression_Adapter<T>::inhomogeneous_term() const {
  return obj_expr().inhomogeneous_term();
}

template <typename T>
inline bool
Expression_Adapter<T>::is_zero() const {
  return obj_expr().is_zero();
}

template <typename T>
inline bool
Expression_Adapter<T>::all_homogeneous_terms_are_zero() const {
  return obj_expr().all_homogeneous_terms_are_zero();
}

template <typename T>
template <typename Expression>
inline bool
Expression_Adapter<T>::is_equal_to(const Expression& y) const {
  return obj_expr().is_equal_to(y);
}

template <typename T>
inline bool
Expression_Adapter<T>
::all_zeroes(const Variables_Set& vars) const {
  return obj_expr().all_zeroes(vars);
}

template <typename T>
inline Coefficient_traits::const_reference
Expression_Adapter<T>::get(dimension_type i) const {
  return obj_expr().get(i);
}

template <typename T>
inline Coefficient_traits::const_reference
Expression_Adapter<T>::get(Variable v) const {
  return obj_expr().get(v);
}

template <typename T>
inline bool
Expression_Adapter<T>::all_zeroes(dimension_type start,
                                  dimension_type end) const {
  return obj_expr().all_zeroes(start, end);
}

template <typename T>
inline dimension_type
Expression_Adapter<T>::num_zeroes(dimension_type start,
                                  dimension_type end) const {
  return obj_expr().num_zeroes(start, end);
}

template <typename T>
inline Coefficient
Expression_Adapter<T>::gcd(dimension_type start,
                           dimension_type end) const {
  return obj_expr().gcd(start, end);
}

template <typename T>
inline dimension_type
Expression_Adapter<T>::last_nonzero() const {
  return obj_expr().last_nonzero();
}

template <typename T>
inline dimension_type
Expression_Adapter<T>::last_nonzero(dimension_type first,
                                    dimension_type last) const {
  return obj_expr().last_nonzero(first, last);
}

template <typename T>
inline dimension_type
Expression_Adapter<T>::first_nonzero(dimension_type first,
                                     dimension_type last) const {
  return obj_expr().first_nonzero(first, last);
}

template <typename T>
inline bool
Expression_Adapter<T>
::all_zeroes_except(const Variables_Set& vars,
                    dimension_type start, dimension_type end) const {
  return obj_expr().all_zeroes_except(vars, start, end);
}

template <typename T>
inline void
Expression_Adapter<T>
::has_a_free_dimension_helper(std::set<dimension_type>& x) const {
  obj_expr().has_a_free_dimension_helper(x);
}

template <typename T>
template <typename Expression>
inline bool
Expression_Adapter<T>
::is_equal_to(const Expression& y,
              dimension_type start, dimension_type end) const {
  return obj_expr().is_equal_to(y, start, end);
}

template <typename T>
template <typename Expression>
inline bool
Expression_Adapter<T>
::is_equal_to(const Expression& y,
              Coefficient_traits::const_reference c1,
              Coefficient_traits::const_reference c2,
              dimension_type start, dimension_type end) const {
  return obj_expr().is_equal_to(y, c1, c2, start, end);
}

template <typename T>
inline void
Expression_Adapter<T>::get_row(Dense_Row& row) const {
  obj_expr().get_row(row);
}

template <typename T>
inline void
Expression_Adapter<T>::get_row(Sparse_Row& row) const {
  obj_expr().get_row(row);
}

template <typename T>
template <typename Expression>
inline bool
Expression_Adapter<T>
::have_a_common_variable(const Expression& y,
                         Variable first, Variable last) const {
  return obj_expr().have_a_common_variable(y, first, last);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Expression_Adapter_inlines_hh)
