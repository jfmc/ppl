/* Unlimited_Sparse_Row_Std_List_Backend class implementation: inline
   functions.
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

#ifndef PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh
#define PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
::dangerous_iterator()
  : iterator() {
}

inline
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
::dangerous_iterator(iterator i)
  : iterator(i) {
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend
::insert(dangerous_iterator pos,dimension_type i,const Coefficient& x) {
  return insert(pos,std::make_pair(i,x));
}


inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator::next(iterator i) {
  return dangerous_iterator(++i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend::begin_dangerous() {
  return begin();
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend::end_dangerous() {
  return end();
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend
::find_dangerous(const dimension_type k) {
  if (begin() == end())
    return end_dangerous();
  dangerous_iterator i = begin_dangerous();
  if ((*i).first > k)
    return end_dangerous();
  // Now we can call find(k,i) without triggering asserts.
  return find_dangerous(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend
::lower_bound_dangerous(const dimension_type k) {
  if (begin() == end())
    return end_dangerous();
  dangerous_iterator i = begin_dangerous();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound_dangerous(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::find(const dimension_type k) {
  if (begin() == end())
    return end();
  iterator i = begin();
  if ((*i).first > k)
    return end();
  // Now we can call find(k,i) without triggering asserts.
  return find(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::lower_bound(const dimension_type k) {
  if (begin() == end())
    return end();
  iterator i = begin();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::const_iterator
Unlimited_Sparse_Row_Std_List_Backend::find(const dimension_type k) const {
  if (begin() == end())
    return end();
  const_iterator i = begin();
  if ((*i).first > k)
    return end();
  // Now we can call find(k,i) without triggering asserts.
  return find(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::const_iterator
Unlimited_Sparse_Row_Std_List_Backend
::lower_bound(const dimension_type k) const {
  if (begin() == end())
    return end();
  const_iterator i = begin();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound(k,i);
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend
::find_dangerous(const dimension_type k,dangerous_iterator itr1) {
  PPL_ASSERT(itr1 == end_dangerous() || (*itr1).first <= k);
  dangerous_iterator itr = lower_bound_dangerous(k,itr1);
  if (itr != end_dangerous())
    if (itr->first != k)
      return end_dangerous();
  return itr;
}

inline Unlimited_Sparse_Row_Std_List_Backend::dangerous_iterator
Unlimited_Sparse_Row_Std_List_Backend
::lower_bound_dangerous(const dimension_type k,dangerous_iterator itr) {
  PPL_ASSERT(itr == end_dangerous() || (*itr).first <= k);
  return std::lower_bound(itr,end_dangerous(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend
::find(const dimension_type k,iterator itr1) {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  iterator itr = lower_bound(k,itr1);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend
::lower_bound(const dimension_type k, iterator itr) {
  PPL_ASSERT(itr == end() || (*itr).first <= k);
  return std::lower_bound(itr,end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row_Std_List_Backend::const_iterator
Unlimited_Sparse_Row_Std_List_Backend
::find(const dimension_type k,const_iterator itr1) const {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  const_iterator itr = lower_bound(k,itr1);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row_Std_List_Backend::const_iterator
Unlimited_Sparse_Row_Std_List_Backend
::lower_bound(const dimension_type k,const_iterator itr1) const {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  return std::lower_bound(itr1,end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline void
Unlimited_Sparse_Row_Std_List_Backend
::find2_dangerous(const dimension_type c1,const dimension_type c2,
                  dangerous_iterator& itr1,dangerous_iterator& itr2) {
  if (c1 > c2) {
    find2_dangerous(c2,c1,itr2,itr1);
    return;
  }
  itr1 = lower_bound_dangerous(c1);
  itr2 = lower_bound_dangerous(c2,itr1);
  if (itr1 != end_dangerous())
    if (itr1->first != c1)
      itr1 = end_dangerous();
  if (itr2 != end_dangerous())
    if (itr2->first != c2)
      itr2 = end_dangerous();
}

inline void
Unlimited_Sparse_Row_Std_List_Backend
::find2(const dimension_type c1,const dimension_type c2,
        iterator& itr1,iterator& itr2) {
  iterator i1;
  iterator i2;
  find2(c1,c2,i1,i2);
  itr1 = i1;
  itr2 = i2;
}

inline void
Unlimited_Sparse_Row_Std_List_Backend
::find2(const dimension_type c1,const dimension_type c2,
        const_iterator& itr1,const_iterator& itr2) const {
  if (c1 > c2) {
    find2(c2,c1,itr2,itr1);
    return;
  }
  itr1 = lower_bound(c1);
  itr2 = lower_bound(c2,itr1);
  if (itr1 != end())
    if (itr1->first != c1)
      itr1 = end();
  if (itr2 != end())
    if (itr2->first != c2)
      itr2 = end();
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend::splice(iterator& position,This& x) {
  PPL_ASSERT(this != &x);
  if (position == begin()) {
    Base::splice(position,x);
    return begin();
  }
  iterator previous = position;
  --previous;
  Base::splice(position,x);
  ++previous;
  return previous;
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend
::splice(iterator& position,This& x,iterator i) {
  if (position == begin()) {
    Base::splice(position,x,i);
    return begin();
  }
  iterator previous = position;
  --previous;
  if (previous == i) {
    Base::splice(position,x,i);
    // previous is no longer valid because it was equal to i.
    previous = position;
    --previous;
  } else {
    Base::splice(position,x,i);
    ++previous;
  }
  return previous;
}

inline Unlimited_Sparse_Row_Std_List_Backend::iterator
Unlimited_Sparse_Row_Std_List_Backend
::splice(iterator& position,This& x,iterator first,iterator last) {
  if (position == begin()) {
    Base::splice(position,x,first,last);
    return begin();
  }
  if (first == last)
    return position;
  if (last == position)
    return first;
  iterator previous = position;
  --previous;
  // previous is not in [first,last) because position is not in [first,last)
  // and position!=last.
  Base::splice(position,x,first,last);
  ++previous;
  return previous;
}

inline memory_size_type
Unlimited_Sparse_Row_Std_List_Backend::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline bool
Unlimited_Sparse_Row_Std_List_Backend::OK() const {
  return true;
}

template <typename Compare>
inline Unlimited_Sparse_Row_Std_List_Backend::value_key_comparison<Compare>
Unlimited_Sparse_Row_Std_List_Backend
::value_key_compare(const Compare& comp) {
  return value_key_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row_Std_List_Backend::value_key_comparison<Compare>
::value_key_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row_Std_List_Backend::value_key_comparison<Compare>
::operator()(const Unlimited_Sparse_Row_Std_List_Backend::value_type& x,
             const dimension_type y) const {
  return comp_(x.first,y);
}

template <typename Compare>
inline Unlimited_Sparse_Row_Std_List_Backend::key_value_comparison<Compare>
Unlimited_Sparse_Row_Std_List_Backend
::key_value_compare(const Compare& comp) {
  return key_value_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row_Std_List_Backend::key_value_comparison<Compare>
::key_value_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row_Std_List_Backend::key_value_comparison<Compare>
::operator()(const dimension_type x,
             const Unlimited_Sparse_Row_Std_List_Backend::value_type& y
             ) const {
  return comp_(x,y.first);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Std_List_Backend_inlines_hh)
