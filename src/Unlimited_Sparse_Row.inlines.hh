/* Unlimited_Sparse_Row class implementation: inline functions.
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

#ifndef PPL_Unlimited_Sparse_Row_inlines_hh
#define PPL_Unlimited_Sparse_Row_inlines_hh 1

#include "math_utilities.defs.hh"
#include "assert.hh"
#include <cstddef>
#include <limits>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row::Unlimited_Sparse_Row()
  : data() {
  PPL_ASSERT(OK());
}

inline void
Unlimited_Sparse_Row::swap(Unlimited_Sparse_Row& x) {
  data.swap(x.data);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Unlimited_Sparse_Row::swap(dimension_type i, dimension_type j) {
  if (i == j)
    return;
  iterator i_itr = lower_bound(i);
  iterator j_itr = lower_bound(j);
  iterator itr_end = end();
  if (i_itr != itr_end && i_itr->first == i)
    if (j_itr != itr_end && j_itr->first == j) {
      // Both i and j are in the list.
      Unlimited_Sparse_Row::swap(i_itr,j_itr);
    } else {
      if (i_itr != j_itr) {
        // i is in the list, j isn't
        data.splice(j_itr,data,i_itr);
        // j_itr is no longer valid.
        // i_itr is valid because i_itr != j_itr.
        --j_itr;
        j_itr->first = j;
      } else {
        j_itr->first=j;
      }
    }
  else
    if (j_itr != itr_end && j_itr->first == j) {
      if (i_itr != j_itr) {
        // j is in the list, i isn't
        data.splice(i_itr,data,j_itr);
        // j_itr is no longer valid.
        // i_itr is valid because i_itr != j_itr.
        --i_itr;
        i_itr->first = i;
      } else {
        i_itr->first = j;
      }
    } else {
      // Do nothing
    }
  assert(OK());
}

inline void
Unlimited_Sparse_Row::swap(iterator i, iterator j) {
  PPL_ASSERT(i != data.end());
  PPL_ASSERT(j != data.end());
  std::swap(i->second,j->second);
  PPL_ASSERT(OK());
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::reset(iterator i) {
  iterator res = data.erase(i);
  PPL_ASSERT(OK());
  return res;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::reset(iterator first,iterator last) {
  iterator res = data.erase(first,last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Unlimited_Sparse_Row::reset(const dimension_type i) {
  iterator itr = find(i);
  if (itr != end())
    reset(itr);
  PPL_ASSERT(OK());
}

inline void
Unlimited_Sparse_Row::reset_after(dimension_type i) {
  data.erase(lower_bound(i),end());
  PPL_ASSERT(OK());
}

inline Coefficient&
Unlimited_Sparse_Row::operator[](const dimension_type i) {
  iterator itr = lower_bound(i);
  if (itr != end())
    if (itr->first == i)
      return itr->second;

  data.insert(itr,std::make_pair(i,Coefficient(0)));
  --itr;
  return itr->second;
}

inline const Coefficient&
Unlimited_Sparse_Row::operator[](const dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Unlimited_Sparse_Row::get(const dimension_type i) const {
  static const Coefficient zero = 0;

  const_iterator itr = find(i);
  if (itr == end())
    return zero;
  else {
    PPL_ASSERT(itr->first == i);
    return itr->second;
  }
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::begin() {
  return data.begin();
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::end() {
  return data.end();
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::begin() const {
  return data.begin();
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::end() const {
  return data.end();
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find(const dimension_type k) {
  iterator itr = lower_bound(k);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::lower_bound(const dimension_type k) {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::upper_bound(const dimension_type k) {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::find(const dimension_type k) const {
  const_iterator itr = lower_bound(k);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::lower_bound(const dimension_type k) const {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::upper_bound(const dimension_type k) const {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<dimension_type>()));
}

inline bool
Unlimited_Sparse_Row::operator!=(const Unlimited_Sparse_Row &x) const {
  return !((*this) == x);
}

template <typename Func>
inline void
Unlimited_Sparse_Row::for_each_nonzero(const Func& func,const dimension_type n) {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Func>
inline void
Unlimited_Sparse_Row::for_each_nonzero(const Func& func,const dimension_type n)
  const {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Compare>
inline Unlimited_Sparse_Row::value_key_comparison<Compare>
Unlimited_Sparse_Row::value_key_compare(const Compare& comp) {
  return value_key_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row::value_key_comparison<Compare>::
  value_key_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row::value_key_comparison<Compare>::operator()(
  const Unlimited_Sparse_Row::value_type& x,
  const dimension_type y) const {
  return comp_(x.first,y);
}

template <typename Compare>
inline Unlimited_Sparse_Row::key_value_comparison<Compare>
Unlimited_Sparse_Row::key_value_compare(const Compare& comp) {
  return key_value_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row::key_value_comparison<Compare>::
  key_value_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row::key_value_comparison<Compare>::operator()(
  const dimension_type x,
  const Unlimited_Sparse_Row::value_type& y) const {
  return comp_(x,y.first);
}

} // namespace Parma_Polyhedra_Library


namespace std {

inline void
swap(Parma_Polyhedra_Library::Unlimited_Sparse_Row& x,
     Parma_Polyhedra_Library::Unlimited_Sparse_Row& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Unlimited_Sparse_Row_inlines_hh)
