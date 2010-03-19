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

// FIXME: Remove this.
// It's needed only to please KDevelop4.
#include "Unlimited_Sparse_Row.defs.hh"

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
  dangerous_iterator i_itr = lower_bound_dangerous(i);
  dangerous_iterator j_itr = lower_bound_dangerous(j);
  dangerous_iterator itr_end = end_dangerous();
  if (i_itr != itr_end && i_itr->first == i)
    if (j_itr != itr_end && j_itr->first == j) {
      // Both i and j are in the list.
      Unlimited_Sparse_Row::swap(i_itr, j_itr);
    } else {
      if (i_itr != j_itr) {
        // i is in the list, j isn't
        i_itr = data.splice(j_itr, data, i_itr);
        // i_itr was invalidated.
        // Now i_itr points to the moved element.
        i_itr->first = j;
      } else {
        j_itr->first=j;
      }
    }
  else
    if (j_itr != itr_end && j_itr->first == j) {
      if (i_itr != j_itr) {
        // j is in the list, i isn't
        j_itr = data.splice(i_itr, data, j_itr);
        // j_itr was invalidated.
        // Now j_itr points to the moved element.
        j_itr->first = i;
      } else
        i_itr->first = j;
    } else {
      // Do nothing
    }
  assert(OK());
}

inline void
Unlimited_Sparse_Row::swap(iterator i, iterator j) {
  PPL_ASSERT(i != data.end());
  PPL_ASSERT(j != data.end());
  std::swap(i->second, j->second);
  PPL_ASSERT(OK());
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row::reset(dangerous_iterator i) {
  dangerous_iterator res = data.erase(i);
  PPL_ASSERT(OK());
  return res;
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::reset(dangerous_iterator first, dangerous_iterator last) {
  dangerous_iterator res = data.erase(first, last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Unlimited_Sparse_Row::reset(const dimension_type i) {
  dangerous_iterator itr = find_dangerous(i);
  if (itr != end_dangerous())
    reset(itr);
  PPL_ASSERT(OK());
}

inline void
Unlimited_Sparse_Row::reset_after(dimension_type i) {
  data.erase(lower_bound_dangerous(i), end_dangerous());
  PPL_ASSERT(OK());
}

inline Coefficient&
Unlimited_Sparse_Row::operator[](const dimension_type i) {
  dangerous_iterator itr = lower_bound_dangerous(i);
  if (itr != end_dangerous())
    if (itr->first == i)
      return itr->second;

  itr = data.insert(itr, std::make_pair(i, Coefficient_zero()));
  return itr->second;
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row
::find_create(const dimension_type i, const Coefficient& x) {
  if (begin() == end())
    return data.insert(end_dangerous(), std::make_pair(i, x));
  dangerous_iterator itr = begin_dangerous();
  if ((*itr).first > i)
    return data.insert(itr, std::make_pair(i, x));
  // Now we can call find_create(i, x, itr) without triggering asserts.
  return find_create(i, x, itr);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row
::find_create(const std::pair<dimension_type, Coefficient>& x) {
  if (begin() == end())
    return data.insert(end_dangerous(), x);
  dangerous_iterator itr = begin_dangerous();
  if ((*itr).first > x.first)
    return data.insert(itr, x);
  // Now we can call find_create(x, itr) without triggering asserts.
  return find_create(x, itr);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(const dimension_type i) {
  if (begin() == end())
    return data.insert(end_dangerous(),
                       std::make_pair(i, Coefficient_zero()));
  dangerous_iterator itr = begin_dangerous();
  if ((*itr).first > i)
    return data.insert(itr, std::make_pair(i, Coefficient_zero()));
  // Now we can call find_create(i, itr) without triggering asserts.
  return find_create(i, itr);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row
::find_create(const dimension_type i, const Coefficient& x, iterator itr) {
  PPL_ASSERT(itr != end());
  PPL_ASSERT((*itr).first <= i);
  if ((*itr).first == i) {
    (*itr).second = x;
    return itr;
  }
  return find_create(i, x, dangerous_iterator::next(itr));
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row
::find_create(const std::pair<dimension_type, Coefficient>& x, iterator itr) {
  PPL_ASSERT(itr != end());
  PPL_ASSERT((*itr).first <= x.first);
  if ((*itr).first == x.first) {
    (*itr).second = x.second;
    return itr;
  }
  return find_create(x, dangerous_iterator::next(itr));
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find_create(const dimension_type i, iterator itr) {
  PPL_ASSERT(itr != end());
  PPL_ASSERT((*itr).first <= i);
  if ((*itr).first == i) {
    return itr;
  }
  return find_create(i, dangerous_iterator::next(itr));
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::find_create(const dimension_type i, dangerous_iterator itr) {
  // The check is needed to avoid triggering assertions in lower_bound().
  if (itr != end_dangerous() && (*itr).first < i)
    itr = lower_bound_dangerous(i, itr);
  if (itr != end_dangerous() && (*itr).first == i)
    return itr;
  itr = data.insert(itr, std::make_pair(i, Coefficient_zero()));
  return itr;
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::find_create(const dimension_type i, const Coefficient& x,
              dangerous_iterator itr) {
  // The check is needed to avoid triggering assertions in lower_bound().
  if (itr != end_dangerous() && (*itr).first < i)
    itr = lower_bound_dangerous(i, itr);
  if (itr != end_dangerous() && (*itr).first == i) {
    (*itr).second = x;
    return itr;
  }
  itr = data.insert(itr, std::make_pair(i, x));
  return itr;
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::find_create(const std::pair<dimension_type, Coefficient>& x,
              dangerous_iterator itr) {
  // The check is needed to avoid triggering assertions in lower_bound().
  if (itr != end_dangerous() && (*itr).first < x.first)
    itr = lower_bound_dangerous(x.first, itr);
  if (itr != end_dangerous() && (*itr).first == x.first) {
    (*itr).second = x.second;
    return itr;
  }
  itr = data.insert(itr, x);
  return itr;
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

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row::begin_dangerous() {
  return data.begin_dangerous();
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row::end_dangerous() {
  return data.end_dangerous();
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

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row::find_dangerous(const dimension_type k) {
  return data.find_dangerous(k);
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row::lower_bound_dangerous(const dimension_type k) {
  return data.lower_bound_dangerous(k);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find(const dimension_type k) {
  return data.find(k);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::lower_bound(const dimension_type k) {
  return data.lower_bound(k);
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::find(const dimension_type k) const {
  return data.find(k);
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::lower_bound(const dimension_type k) const {
  return data.lower_bound(k);
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::find_dangerous(const dimension_type k, dangerous_iterator itr1) {
  return data.find_dangerous(k, itr1);
}

inline Unlimited_Sparse_Row::dangerous_iterator
Unlimited_Sparse_Row
::lower_bound_dangerous(const dimension_type k, dangerous_iterator itr) {
  return data.lower_bound_dangerous(k, itr);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::find(const dimension_type k, iterator itr1) {
  return data.find(k, itr1);
}

inline Unlimited_Sparse_Row::iterator
Unlimited_Sparse_Row::lower_bound(const dimension_type k, iterator itr) {
  return data.lower_bound(k, itr);
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row::find(const dimension_type k, const_iterator itr1) const {
  return data.find(k, itr1);
}

inline Unlimited_Sparse_Row::const_iterator
Unlimited_Sparse_Row
::lower_bound(const dimension_type k const_iterator itr1) const {
  return data.lower_bound(k, itr1);
}

inline void
Unlimited_Sparse_Row
::find2_dangerous(const dimension_type c1, const dimension_type c2,
                  dangerous_iterator& itr1, dangerous_iterator& itr2) {
  data.find2_dangerous(c1, c2, itr1, itr2);
}

inline void
Unlimited_Sparse_Row::find2(const dimension_type c1, const dimension_type c2,
                            iterator& itr1, iterator& itr2) {
  data.find2(c1, c2, itr1, itr2);
}

inline void
Unlimited_Sparse_Row::find2(const dimension_type c1,const dimension_type c2,
                            const_iterator& itr1,const_iterator& itr2) const {
  data.find2(c1,c2,itr1,itr2);
}

inline void
Unlimited_Sparse_Row
::get2(const dimension_type c1, const dimension_type c2,
       const Coefficient*& p1, const Coefficient*& p2) const {
  const_iterator i1;
  const_iterator i2;
  find2(c1, c2, i1, i2);
  if (i1 == end())
    p1 = &(Coefficient_zero());
  else {
    PPL_ASSERT((*i1).first == c1);
    p1 = &((*i1).second);
  }
  if (i2 == end())
    p2 = &(Coefficient_zero());
  else {
    PPL_ASSERT((*i2).first == c2);
    p2 = &((*i2).second);
  }
}

inline bool
Unlimited_Sparse_Row::operator!=(const Unlimited_Sparse_Row &x) const {
  return !((*this) == x);
}

template <typename Func>
inline void
Unlimited_Sparse_Row
::for_each_nonzero(const Func& func, const dimension_type /* n */) {
  std::for_each(begin(), end(), func);
}

template <typename Func>
inline void
Unlimited_Sparse_Row
::for_each_nonzero(const Func& func,const dimension_type /* n */) const {
  std::for_each(begin(), end(), func);
}

inline memory_size_type
Unlimited_Sparse_Row::external_memory_in_bytes() const {
  return data.external_memory_in_bytes();
}

inline memory_size_type
Unlimited_Sparse_Row::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
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
