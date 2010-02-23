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
#include "Unlimited_Sparse_Row_Custom_Slist_Backend.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::
  Unlimited_Sparse_Row_Custom_Slist_Backend()
  : first(0), last(&first) {
  PPL_ASSERT(OK());
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::
  Unlimited_Sparse_Row_Custom_Slist_Backend(const This& x)
  : first(0), last(&first) {
  PPL_ASSERT(OK());
  (*this) = x;
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::
  ~Unlimited_Sparse_Row_Custom_Slist_Backend() {
  clear();
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::empty() const {
  return (first == 0);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::push_front(const value_type& x) {
  insert(begin(),x);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::push_back(const value_type& x) {
  insert(end(),x);
}

inline void
Unlimited_Sparse_Row_Custom_Slist_Backend::pop_front() {
  PPL_ASSERT(!empty());
  list_elem* p = first;
  first = first->next;
  delete p;
  PPL_ASSERT(OK());
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::begin() {
  return dangerous_iterator(&first);
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::end() {
  return dangerous_iterator(last);
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
Unlimited_Sparse_Row_Custom_Slist_Backend::insert(dangerous_iterator pos,
                                                  const value_type& x) {
  list_elem* elem_after = *(pos.p);
  list_elem* new_elem = new list_elem(x,elem_after);
  *(pos.p) = new_elem;
  // No change needed to pos.p
  if (elem_after == 0)
    // We are inserting at end(), so last changed.
    last = &(new_elem->next);
  PPL_ASSERT(OK());
  PPL_ASSERT(pos.OK());
  return pos;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::erase(dangerous_iterator pos) {
  // We must not be at end().
  PPL_ASSERT(*(pos.p) != 0);
  list_elem* to_erase = *(pos.p);
  *(pos.p) = to_erase->next;
  // No change needed to pos.p
  if (to_erase->next == 0)
    // We are erasing the last element, so last changed
    last = pos.p;
  delete to_erase;
  PPL_ASSERT(OK());
  PPL_ASSERT(pos.OK());
  return pos;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::erase(dangerous_iterator first,
                                                 dangerous_iterator last) {
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
Unlimited_Sparse_Row_Custom_Slist_Backend::splice(
  dangerous_iterator& position,This& x) {
  return splice(position,x,x.begin(),x.end());
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::splice(
  dangerous_iterator& position,This& x,dangerous_iterator i) {
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
  // i_itr points to the moved element.
  dangerous_iterator i_itr = position;
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
Unlimited_Sparse_Row_Custom_Slist_Backend::splice(
  dangerous_iterator& position,This& x,
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
  // first_itr points to the first added element
  dangerous_iterator first_itr = position;
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
  std::swap(last,x.last);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::operator!=(const This& x) const {
  return !(*this == x);
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::list_elem::
  list_elem()
  : data(), next(0) {
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::list_elem::
  list_elem(const value_type& data1,list_elem* next1)
  : data(data1), next(next1) {
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::
  dangerous_iterator(list_elem** p1)
  : p(p1) {
#ifndef NDEBUG
  if (p != 0)
    q = *p;
#endif
  PPL_ASSERT(OK());
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
  q = *p;
  PPL_ASSERT(OK());
  return *this;
}

inline Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::
  operator++(int) {
  dangerous_iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator==(
  const dangerous_iterator& x) const {
  PPL_ASSERT(OK());
  return p == x.p;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator!=(
  const dangerous_iterator& x) const {
  return !((*this) == x);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator
  iterator() {
  PPL_ASSERT(OK());
  if (p == 0)
    // This iterator has been default-contructed and not initialized.
    return iterator();
  else
    return iterator(*p);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::dangerous_iterator::operator
  const_iterator() const {
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
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::
  operator++(int) {
  iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator==(
  const iterator& x) const {
  return p == x.p;
}

inline bool
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator!=(
  const iterator& x) const {
  return !((*this) == x);
}

inline
Unlimited_Sparse_Row_Custom_Slist_Backend::iterator::operator
  const_iterator() const {
  return const_iterator(p);
}


inline
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::
  const_iterator(list_elem* const p1)
  : p(p1) {
}

inline const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type&
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::operator*() const {
  return *operator->();
}

inline const Unlimited_Sparse_Row_Custom_Slist_Backend::value_type*
Unlimited_Sparse_Row_Custom_Slist_Backend::const_iterator::operator->()
  const {
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

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Custom_Slist_Backend_inlines_hh)
