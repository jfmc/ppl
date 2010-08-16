/* Dense_Row class implementation: inline functions.
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

#ifndef PPL_Dense_Row_inlines_hh
#define PPL_Dense_Row_inlines_hh 1

// FIXME: Remove this.
// Added to please KDevelop4.
#include "Dense_Row.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Dense_Row::Dense_Row()
  : Row() {
}

inline void
Dense_Row::swap(dimension_type i, dimension_type j) {
  std::swap((*this)[i], (*this)[j]);
}

inline void
Dense_Row::swap(iterator i, iterator j) {
  std::swap(i->second, j->second);
}

inline void
Dense_Row::reset(dimension_type i) {
  (*this)[i] = 0;
}

inline Dense_Row::iterator
Dense_Row::reset(iterator itr) {
  itr->second = 0;
  ++itr;
  return itr;
}

inline
Dense_Row::Dense_Row(dimension_type sz, dimension_type capacity)
  : Row(sz, capacity, Row::Flags()) {
}

inline
Dense_Row::Dense_Row(dimension_type sz)
  : Row(sz, Row::Flags()) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y)
  : Row(y) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y, dimension_type capacity)
  : Row(y, capacity) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y, dimension_type sz,
                     dimension_type capacity)
  : Row(y, sz, capacity) {
}

inline Dense_Row::iterator
Dense_Row::begin() {
  return iterator(*this, 0);
}

inline Dense_Row::const_iterator
Dense_Row::begin() const {
  return const_iterator(*this, 0);
}

inline Dense_Row::iterator
Dense_Row::end() {
  return iterator(*this, size());
}

inline Dense_Row::const_iterator
Dense_Row::end() const {
  return const_iterator(*this, size());
}

inline void
Dense_Row::swap(Dense_Row& y) {
  Row::swap(y);
}

inline const Coefficient&
Dense_Row::get(dimension_type i) const {
  return (*this)[i];
}

inline Dense_Row::iterator
Dense_Row::find(dimension_type i) {
  return iterator(*this, i);
}

inline Dense_Row::const_iterator
Dense_Row::find(dimension_type i) const {
  return const_iterator(*this, i);
}

inline Dense_Row::iterator
Dense_Row::find(iterator itr, dimension_type i) {
  (void)itr;
  return iterator(*this, i);
}

inline Dense_Row::const_iterator
Dense_Row::find(const_iterator itr, dimension_type i) const {
  (void)itr;
  return const_iterator(*this, i);
}

inline Dense_Row::iterator
Dense_Row::lower_bound(dimension_type i) {
  return find(i);
}

inline Dense_Row::const_iterator
Dense_Row::lower_bound(dimension_type i) const {
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::lower_bound(iterator itr, dimension_type i) {
  return find(itr, i);
}

inline Dense_Row::const_iterator
Dense_Row::lower_bound(const_iterator itr, dimension_type i) const {
  return find(itr, i);
}

inline Dense_Row::iterator
Dense_Row::find_create(dimension_type i, const Coefficient& x) {
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(dimension_type i) {
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(iterator itr, dimension_type i, const Coefficient& x) {
  (void)itr;
  (*this)[i] = x;
  return find(i);
}

inline Dense_Row::iterator
Dense_Row::find_create(iterator itr, dimension_type i) {
  (void)itr;
  return find(i);
}


inline
Dense_Row::iterator::iterator()
  : row(NULL), i(0) {
  PPL_ASSERT(OK());
}

inline
Dense_Row::iterator::iterator(Dense_Row& row1,dimension_type i1)
  : row(&row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Dense_Row::iterator::value_type
Dense_Row::iterator::operator*() {
  PPL_ASSERT(i < row->size());
  return value_type(i, (*row)[i]);
}

inline Dense_Row::iterator::const_type
Dense_Row::iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return const_type(i, (*row)[i]);
}

inline Dense_Row::iterator::Member_Access_Helper
Dense_Row::iterator::operator->() {
  PPL_ASSERT(i < row->size());
  return Member_Access_Helper(i, (*row)[i]);
}

inline Dense_Row::iterator::Const_Member_Access_Helper
Dense_Row::iterator::operator->() const {
  PPL_ASSERT(i < row->size());
  return Const_Member_Access_Helper(i, (*row)[i]);
}

inline Dense_Row::iterator&
Dense_Row::iterator::operator++() {
  PPL_ASSERT(i < row->size());
  ++i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::iterator
Dense_Row::iterator::operator++(int) {
  iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline Dense_Row::iterator&
Dense_Row::iterator::operator--() {
  PPL_ASSERT(i > 0);
  --i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::iterator
Dense_Row::iterator::operator--(int) {
  iterator tmp(*this);
  --(*this);
  return tmp;
}

inline bool
Dense_Row::iterator::operator==(const iterator& x) const {
  return (row == x.row) && (i == x.i);
}

inline bool
Dense_Row::iterator::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline
Dense_Row::iterator::operator const_iterator() const {
  return const_iterator(*row, i);
}

inline bool
Dense_Row::iterator::OK() const {
  if (row == NULL)
    return true;
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row->size());
}


inline
Dense_Row::iterator::Member_Access_Helper
::Member_Access_Helper(dimension_type index, Coefficient& data)
  : value(index, data) {
}

inline Dense_Row::iterator::value_type*
Dense_Row::iterator::Member_Access_Helper::operator->() {
  return &value;
}


inline
Dense_Row::iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type index, const Coefficient& data)
  : value(index, data) {
}

inline const Dense_Row::iterator::const_type*
Dense_Row::iterator::Const_Member_Access_Helper::operator->() const {
  return &value;
}


inline
Dense_Row::const_iterator::const_iterator()
  : row(NULL), i(0) {
  PPL_ASSERT(OK());
}

inline
Dense_Row::const_iterator::const_iterator(const Dense_Row& row1,
                                          dimension_type i1)
  : row(&row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Dense_Row::const_iterator::const_type
Dense_Row::const_iterator::operator*() const {
  PPL_ASSERT(i < row->size());
  return const_type(i, (*row)[i]);
}

inline Dense_Row::const_iterator::Const_Member_Access_Helper
Dense_Row::const_iterator::operator->() const {
  PPL_ASSERT(i < row->size());
  return Const_Member_Access_Helper(i, (*row)[i]);
}

inline Dense_Row::const_iterator&
Dense_Row::const_iterator::operator++() {
  PPL_ASSERT(i < row->size());
  ++i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::const_iterator
Dense_Row::const_iterator::operator++(int) {
  const_iterator tmp(*this);
  ++(*this);
  return tmp;
}

inline Dense_Row::const_iterator&
Dense_Row::const_iterator::operator--() {
  PPL_ASSERT(i > 0);
  --i;
  PPL_ASSERT(OK());
  return *this;
}

inline Dense_Row::const_iterator
Dense_Row::const_iterator::operator--(int) {
  const_iterator tmp(*this);
  --(*this);
  return tmp;
}

inline bool
Dense_Row::const_iterator::operator==(const const_iterator& x) const {
  return (row == x.row) && (i == x.i);
}

inline bool
Dense_Row::const_iterator::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline bool
Dense_Row::const_iterator::OK() const {
  if (row == NULL)
    return true;
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row->size());
}



inline
Dense_Row::const_iterator::Const_Member_Access_Helper
::Const_Member_Access_Helper(dimension_type index, const Coefficient& data)
  : value(index, data) {
}

inline const Dense_Row::const_iterator::const_type*
Dense_Row::const_iterator::Const_Member_Access_Helper::operator->() const {
  return &value;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Dense_Row */
inline void
swap(Parma_Polyhedra_Library::Dense_Row& x,
     Parma_Polyhedra_Library::Dense_Row& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::Dense_Row */
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator x,
          std::vector<Parma_Polyhedra_Library::Dense_Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_Dense_Row_inlines_hh)
