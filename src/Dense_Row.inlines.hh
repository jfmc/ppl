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
