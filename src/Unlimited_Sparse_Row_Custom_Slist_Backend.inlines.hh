/* Unlimited_Sparse_Row_Custom_Slist_Backend class implementation: inline
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

#ifndef PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_inlines_hh
#define PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_inlines_hh 1

// FIXME: Remove this.
// It's needed only to please KDevelop4.
#include "Unlimited_Sparse_Row_Custom_Slist_Backend.defs.hh"

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row_Custom_Slist_Backend
::Unlimited_Sparse_Row_Custom_Slist_Backend()
  : first(0), last(&first) {
  PPL_ASSERT(OK());
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend
::Unlimited_Sparse_Row_Custom_Slist_Backend(const This& x)
  : first(0), last(&first) {
  PPL_ASSERT(OK());
  (*this) = x;
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend
::~Unlimited_Sparse_Row_Custom_Slist_Backend() {
  PPL_ASSERT(OK());
  clear();
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::empty() const {
  return (first == 0);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::push_front(const value_type& x) {
  insert(begin_dangerous(),x);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::push_back(const value_type& x) {
  insert(end_dangerous(),x);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::pop_front() {
  PPL_ASSERT(!empty());
  PPL_ASSERT(OK());
  list_elem* p = first;
  first = first->next;
  if (last == &(p->next))
    // We deleted the only element left in the list.
    last = &first;
  delete p;
  PPL_ASSERT(OK());
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::begin_dangerous() {
  return dangerous_iterator(&first);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::end_dangerous() {
  return dangerous_iterator(last);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::begin() {
  return iterator(first);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::end() {
  return iterator(0);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::begin() const {
  return const_iterator(first);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::end() const {
  return const_iterator(0);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::find_dangerous(const dimension_type k) {
  if (begin() == end())
    return end_dangerous();
  dangerous_iterator i = begin_dangerous();
  if ((*i).first > k)
    return end_dangerous();
  // Now we can call find(k,i) without triggering asserts.
  return find_dangerous(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::lower_bound_dangerous(const dimension_type k) {
  if (begin() == end())
    return end_dangerous();
  dangerous_iterator i = begin_dangerous();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound_dangerous(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::find(const dimension_type k) {
  if (begin() == end())
    return end();
  iterator i = begin();
  if ((*i).first > k)
    return end();
  // Now we can call find(k,i) without triggering asserts.
  return find(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::lower_bound(const dimension_type k) {
  if (begin() == end())
    return end();
  iterator i = begin();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::find(const dimension_type k) const {
  if (begin() == end())
    return end();
  const_iterator i = begin();
  if ((*i).first > k)
    return end();
  // Now we can call find(k,i) without triggering asserts.
  return find(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::lower_bound(const dimension_type k) const {
  if (begin() == end())
    return end();
  const_iterator i = begin();
  if ((*i).first > k)
    return i;
  // Now we can call lower_bound(k,i) without triggering asserts.
  return lower_bound(k,i);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::find_dangerous(const dimension_type k,dangerous_iterator itr1) {
  PPL_ASSERT(itr1 == end_dangerous() || (*itr1).first <= k);
  dangerous_iterator itr = lower_bound_dangerous(k,itr1);
  if (itr != end_dangerous())
    if (itr->first != k)
      return end_dangerous();
  return itr;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::lower_bound_dangerous(const dimension_type k,dangerous_iterator itr) {
  PPL_ASSERT(itr == end_dangerous() || (*itr).first <= k);
  return std::lower_bound(itr,end_dangerous(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::find(const dimension_type k,iterator itr1) {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  iterator itr = lower_bound(k,itr1);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::lower_bound(const dimension_type k, iterator itr) {
  PPL_ASSERT(itr == end() || (*itr).first <= k);
  return std::lower_bound(itr,end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::find(const dimension_type k,const_iterator itr1) const {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  const_iterator itr = lower_bound(k,itr1);
  if (itr != end())
    if (itr->first != k)
      return end();
  return itr;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::lower_bound(const dimension_type k,const_iterator itr1) const {
  PPL_ASSERT(itr1 == end() || (*itr1).first <= k);
  return std::lower_bound(itr1,end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend
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
Unlimited_Sparse_Row_Custom_Slist_Backend
::find2(const dimension_type c1,const dimension_type c2,
        iterator& itr1,iterator& itr2) {
  iterator i1;
  iterator i2;
  find2(c1,c2,i1,i2);
  itr1 = i1;
  itr2 = i2;
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend
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

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::insert(dangerous_iterator pos,dimension_type i,const Coefficient& x) {
  PPL_ASSERT(pos.OK());
  PPL_ASSERT(OK());
  list_elem* elem_after = *(pos.p);
  list_elem* new_elem = new list_elem(i,x,elem_after);
  *(pos.p) = new_elem;
  // No change needed to pos.p
#ifndef NDEBUG
  pos.q = *(pos.p);
#endif
  if (elem_after == 0)
    // We are inserting at end(), so last changed.
    last = &(new_elem->next);
  PPL_ASSERT(OK());
  PPL_ASSERT(pos.OK());
  return pos;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::insert(dangerous_iterator pos,
         const std::pair<dimension_type,Coefficient>& x) {
  return insert(pos,x.first,x.second);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::erase(dangerous_iterator pos) {
  PPL_ASSERT(pos.OK());
  PPL_ASSERT(OK());
  // We must not be at end().
  PPL_ASSERT(*(pos.p) != 0);
  list_elem* to_erase = *(pos.p);
  *(pos.p) = to_erase->next;
  // No change needed to pos.p
#ifndef NDEBUG
  pos.q = *(pos.p);
#endif
  if (to_erase->next == 0)
    // We are erasing the last element, so last changed
    last = pos.p;
  delete to_erase;
  PPL_ASSERT(OK());
  PPL_ASSERT(pos.OK());
  return pos;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::erase(dangerous_iterator first,dangerous_iterator last) {
  PPL_ASSERT(first.OK());
  PPL_ASSERT(last.OK());
  PPL_ASSERT(OK());
  if (first == last)
    return first;
  dangerous_iterator next = first;
  ++next;
  for ( ; next!=last; ++next) {
    // Next was invalidated by erase() so we must assign it a value.
    next = first = erase(first);
  }
  // Invalidates next, last
  first = erase(first);
  PPL_ASSERT(OK());
  PPL_ASSERT(first.OK());
  return first;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::splice(dangerous_iterator& position,This& x) {
  return splice(position,x,x.begin_dangerous(),x.end_dangerous());
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::splice(dangerous_iterator& position,This& x,dangerous_iterator i) {
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
  PPL_ASSERT(position.OK());
  PPL_ASSERT(i.OK());
  PPL_ASSERT(i.p != 0);
  list_elem* to_move = *(i.p);
  PPL_ASSERT(to_move != 0);
  *(i.p) = to_move->next;
  if (to_move->next == 0)
    // We moved the last element of x, so `x.last' must be updated.
    x.last = i.p;
  to_move->next = *(position.p);
  *(position.p) = to_move;
  dangerous_iterator i_itr = position;
#ifndef NDEBUG
  i_itr.q = *(i_itr.p);
#endif
  // i_itr points to the moved element.
  position.p = &(to_move->next);
#ifndef NDEBUG
  position.q = *(position.p);
#endif
  if (to_move->next == 0)
    last = position.p;
  PPL_ASSERT(i_itr.OK());
  PPL_ASSERT(position.OK());
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
  return i_itr;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend
::splice(dangerous_iterator& position,This& x,
         dangerous_iterator first1,dangerous_iterator last1) {

  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
  PPL_ASSERT(position.OK());
  PPL_ASSERT(first1.OK());
  PPL_ASSERT(last1.OK());
  PPL_ASSERT(position.p != 0);
  PPL_ASSERT(first1.p != 0);
  PPL_ASSERT(last1.p != 0);
  if (first1 == last1)
    return position;
  PPL_ASSERT(*(first1.p) != 0);
  list_elem* tail = *(position.p);
  *(position.p) = *(first1.p);
  *(first1.p) = *(last1.p);
  if (x.last == last1.p)
    // We moved some elements from the end of x, so x.last must be updated.
    x.last = first1.p;
  *(last1.p) = tail;
  dangerous_iterator first_itr = position;
#ifndef NDEBUG
  first_itr.q = *(first_itr.p);
#endif
  // first_itr points to the first added element
  position.p = last1.p;
#ifndef NDEBUG
  position.q = *(position.p);
#endif
  if (tail == 0)
    // We moved some elements to the end of *this, so `last' must be updated.
    last = position.p;
  PPL_ASSERT(first_itr.OK());
  PPL_ASSERT(position.OK());
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
  return first_itr;
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::swap(This& x) {
  std::swap(first,x.first);
  if (last == &first)
    if (x.last == &x.first) {
      x.last = &x.first;
      last = &first;
    } else {
      last = x.last;
      x.last = &x.first;
    }
  else
    if (x.last == &x.first) {
      x.last = last;
      last = &first;
    } else
      std::swap(last,x.last);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::operator!=(const This& x) const {
  return !(*this == x);
}

inline memory_size_type
Unlimited_Sparse_Row_Custom_Slist_Backend::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::list_elem
::list_elem()
  : data(), next(0) {
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::list_elem
::list_elem(dimension_type i,const Coefficient& x,list_elem* next1)
  : data(i,x), next(next1) {
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::list_elem
::list_elem(const value_type& x,list_elem* next1)
  : data(x), next(next1) {
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::dangerous_iterator(list_elem** p1)
  : p(p1) {
#ifndef NDEBUG
  if (p != 0)
    q = *p;
#endif
  PPL_ASSERT(OK());
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::next(iterator i) {
  return dangerous_iterator(&(i.p->next));
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::value_type&
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator*() {
  return *operator->();
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::value_type*
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator->() {
  PPL_ASSERT(p != 0);
  PPL_ASSERT(*p != 0);
  PPL_ASSERT(OK());
  return &((*p)->data);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator&
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator++() {
  // This iterator has been initialized (for example with begin() ).
  PPL_ASSERT(p != 0);
  // (*this) != end()
  PPL_ASSERT(*p != 0);
  PPL_ASSERT(OK());
  p = &((*p)->next);
#ifndef NDEBUG
  q = *p;
#endif
  PPL_ASSERT(OK());
  return *this;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::operator++(int) {
  dangerous_iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::operator==(const dangerous_iterator& x) const {
  PPL_ASSERT(OK());
  return p == x.p;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::operator!=(const dangerous_iterator& x) const {
  return !((*this) == x);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::operator iterator() {
  PPL_ASSERT(OK());
  if (p == 0)
    // This iterator has been default-contructed and not initialized.
    return iterator();
  else
    return iterator(*p);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
::operator const_iterator() const {
  PPL_ASSERT(OK());
  if (p == 0)
    // This iterator has been default-contructed and not initialized.
    return const_iterator();
  else
    return const_iterator(*p);
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::OK() const {
#ifndef NDEBUG
  if (p != 0 && *p != q)
    // This iterator was invalidated by adding an element after it or removing
    // the element before it.
    return false;
#endif
  return true;
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::iterator(list_elem* p1)
  : p(p1) {
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::value_type&
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator*() {
  return *operator->();
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::value_type*
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator->() {
  PPL_ASSERT(p != 0);
  return &(p->data);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator&
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator++() {
  // (*this) != end()
  PPL_ASSERT(p != 0);
  p = p->next;
  return *this;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
::operator++(int) {
  iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
::operator==(const iterator& x) const {
  return p == x.p;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
::operator!=(const iterator& x) const {
  return !((*this) == x);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator
::operator const_iterator() const {
  return const_iterator(p);
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
::const_iterator(list_elem* const p1)
  : p(p1) {
}

inline const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type&
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::operator*() const {
  return *operator->();
}

inline const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type*
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
::operator->() const {
  PPL_ASSERT(p != 0);
  return &(p->data);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator&
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::operator++() {
  PPL_ASSERT(p != 0);
  p = p->next;
  return *this;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::operator++(int) {
  const_iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::
  operator==(const const_iterator& x) const {
  return p == x.p;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::
  operator!=(const const_iterator& x) const {
  return !(*this == x);
}

template <typename Compare>
inline Unlimited_Sparse_Row_Custom_Slist_Backend::value_key_comparison<Compare>
Unlimited_Sparse_Row_Custom_Slist_Backend
::value_key_compare(const Compare& comp) {
  return value_key_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row_Custom_Slist_Backend::value_key_comparison<Compare>
::value_key_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::value_key_comparison<Compare>
::operator()(const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type& x,
             const dimension_type y) const {
  return comp_(x.first,y);
}

template <typename Compare>
inline
Unlimited_Sparse_Row_Custom_Slist_Backend::key_value_comparison<Compare>
Unlimited_Sparse_Row_Custom_Slist_Backend
::key_value_compare(const Compare& comp) {
  return key_value_comparison<Compare>(comp);
}

template <typename Compare>
inline
Unlimited_Sparse_Row_Custom_Slist_Backend::key_value_comparison<Compare>
::key_value_comparison(const Compare& comp)
  : comp_(comp) {
}

template <typename Compare>
inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::key_value_comparison<Compare>
::operator()(const dimension_type x,
             const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type& y
             ) const {
  return comp_(x,y.first);
}

} // namespace Parma_Polyhedra_Library

namespace std {

inline void
swap(Parma_Polyhedra_Library::Unlimited_Sparse_Row_Custom_Slist_Backend& x,
     Parma_Polyhedra_Library::Unlimited_Sparse_Row_Custom_Slist_Backend& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_inlines_hh)
