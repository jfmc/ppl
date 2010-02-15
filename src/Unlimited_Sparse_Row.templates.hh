/* Unlimited_Sparse_Row class implementation: non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Unlimited_Sparse_Row_templates_hh
#define PPL_Unlimited_Sparse_Row_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename Func>
void
Unlimited_Sparse_Row::for_each_nonzero(const Func& func,const dimension_type n) {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Func>
void
Unlimited_Sparse_Row::for_each_nonzero(const Func& func,const dimension_type n)
  const {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Compare>
Unlimited_Sparse_Row::value_key_comparison<Compare>
Unlimited_Sparse_Row::value_key_compare(const Compare& comp) {
  return value_key_comparison<Compare>(comp);
}

template <typename Compare>
Unlimited_Sparse_Row::value_key_comparison<Compare>::
  value_key_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
bool
Unlimited_Sparse_Row::value_key_comparison<Compare>::operator()(
  const Unlimited_Sparse_Row::value_type& x,
  const dimension_type y) const {
  return comp_(x.first,y);
}

template <typename Compare>
Unlimited_Sparse_Row::key_value_comparison<Compare>
Unlimited_Sparse_Row::key_value_compare(const Compare& comp) {
  return key_value_comparison<Compare>(comp);
}

template <typename Compare>
Unlimited_Sparse_Row::key_value_comparison<Compare>::
  key_value_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
bool
Unlimited_Sparse_Row::key_value_comparison<Compare>::operator()(
  const dimension_type x,
  const Unlimited_Sparse_Row::value_type& y) const {
  return comp_(x,y.first);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_templates_hh)
