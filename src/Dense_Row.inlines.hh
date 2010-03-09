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

inline dimension_type
Dense_Row::max_size() {
  return Row::max_size();
}

inline dimension_type
Dense_Row::size() const {
  return row.size();
}

inline
Dense_Row::Dense_Row()
  : row() {
}

inline void
Dense_Row::swap(dimension_type i, dimension_type j) {
  std::swap((*this)[i],(*this)[j]);
}

inline void
Dense_Row::swap(iterator i, iterator j) {
  std::swap((*i).second,(*j).second);
}

inline void
Dense_Row::reset(const dimension_type i) {
  (*this)[i] = 0;
}

inline void
Dense_Row::construct(const dimension_type sz,
         const dimension_type capacity) {
  row.construct(sz,capacity,Row::Flags());
}

inline void
Dense_Row::construct(const dimension_type sz) {
  row.construct(sz,Row::Flags());
}

inline
Dense_Row::Dense_Row(const dimension_type sz,
	 const dimension_type capacity)
  : row(sz,capacity,Row::Flags()) {
}

inline
Dense_Row::Dense_Row(const dimension_type sz)
  : row(sz,Row::Flags()) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y)
  : row(y.row) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
	 const dimension_type capacity)
  : row(y.row,capacity) {
}

inline
Dense_Row::Dense_Row(const Dense_Row& y,
	 const dimension_type sz,
	 const dimension_type capacity)
  : row(y.row,sz,capacity) {
}

inline
Dense_Row::~Dense_Row() {
}

inline Dense_Row::iterator
Dense_Row::begin() {
  return iterator(*this,0);
}

inline Dense_Row::const_iterator
Dense_Row::begin() const {
  return const_iterator(*this,0);
}

inline Dense_Row::iterator
Dense_Row::end() {
  return iterator(*this,size());
}

inline Dense_Row::const_iterator
Dense_Row::end() const {
  return const_iterator(*this,size());
}

inline void
Dense_Row::expand_within_capacity(dimension_type new_size) {
  row.expand_within_capacity(new_size);
}

inline void
Dense_Row::shrink(const dimension_type new_size) {
  row.shrink(new_size);
}

inline void
Dense_Row::swap(Dense_Row& y) {
  row.swap(y.row);
}

inline void
Dense_Row::assign(Dense_Row& y) {
  row.assign(y.row);
}

inline Dense_Row&
Dense_Row::operator=(const Dense_Row& y) {
  row = y.row;
  return *this;
}

inline const Coefficient&
Dense_Row::get(const dimension_type i) const {
  return (*this)[i];
}

inline void
Dense_Row::get2(const dimension_type c1,const dimension_type c2,
                const Coefficient*& p1,const Coefficient*& p2) const {
  p1 = &(get(c1));
  p2 = &(get(c2));
}

inline Coefficient&
Dense_Row::operator[](const dimension_type k) {
  return row[k];
}

inline Coefficient_traits::const_reference
Dense_Row::operator[](const dimension_type k) const {
  return row[k];
}

/*! \relates Dense_Row */
inline bool
operator!=(const Dense_Row& x, const Dense_Row& y) {
  return !(x == y);
}

inline void
Dense_Row::normalize() {
  row.normalize();
}

inline void
Dense_Row::ascii_dump(std::ostream& s) const {
  row.ascii_dump(s);
}


inline bool
Dense_Row::ascii_load(std::istream& s) {
  return row.ascii_load(s);
}

inline memory_size_type
Dense_Row::external_memory_in_bytes() const {
  return row.external_memory_in_bytes();
}

inline memory_size_type
Dense_Row::external_memory_in_bytes(dimension_type capacity) const {
  return row.external_memory_in_bytes(capacity);
}

template <typename Func>
void
Dense_Row::for_each_nonzero(Func func,const dimension_type n) {
  for (dimension_type j = n; j-- > 0; )
    func((*this)[j]);
}

template <typename Func>
void
Dense_Row::for_each_nonzero(Func func,const dimension_type n) const {
  for (dimension_type j = n; j-- > 0; )
    func((*this)[j]);
}

inline bool
Dense_Row::OK() const {
  return row.OK();
}

inline bool
Dense_Row::OK(const dimension_type row_size,
                   const dimension_type row_capacity) const {
  return row.OK(row_size,row_capacity);
}

inline bool
Dense_Row::operator==(const Dense_Row& y) const {
  return row == y.row;
}

inline
Dense_Row::operator Row&() {
  return row;
}

inline
Dense_Row::operator const Row&() const {
  return row;
}


inline
Dense_Row::iterator::iterator(Dense_Row& row1,dimension_type i1)
  : row(row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Dense_Row::iterator::value_type
Dense_Row::iterator::operator*() {
  PPL_ASSERT(i < row.size());
  return value_type(i,row[i]);
}

inline Dense_Row::iterator::const_type
Dense_Row::iterator::operator*() const {
  PPL_ASSERT(i < row.size());
  return const_type(i,row[i]);
}

inline Dense_Row::iterator&
Dense_Row::iterator::operator++() {
  PPL_ASSERT(i < row.size());
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
  return (&row == &(x.row)) && (i == x.i);
}

inline bool
Dense_Row::iterator::operator!=(const iterator& x) const {
  return !(*this == x);
}

inline
Dense_Row::iterator::operator const_iterator() const {
  return const_iterator(row,i);
}

inline bool
Dense_Row::iterator::OK() const {
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row.size());
}


inline
Dense_Row::const_iterator::const_iterator(const Dense_Row& row1,dimension_type i1)
  : row(row1), i(i1) {
  PPL_ASSERT(OK());
}

inline Dense_Row::const_iterator::const_type
Dense_Row::const_iterator::operator*() const {
  PPL_ASSERT(i < row.size());
  return const_type(i,row[i]);
}

inline Dense_Row::const_iterator&
Dense_Row::const_iterator::operator++() {
  PPL_ASSERT(i < row.size());
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
  return (&row == &(x.row)) && (i == x.i);
}

inline bool
Dense_Row::const_iterator::operator!=(const const_iterator& x) const {
  return !(*this == x);
}

inline bool
Dense_Row::const_iterator::OK() const {
  // i can be equal to row.size() for past-the-end iterators
  return (i <= row.size());
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
