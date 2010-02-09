/* Dense_Matrix class implementation: inline functions.
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

#ifndef PPL_Dense_Matrix_inlines_hh
#define PPL_Dense_Matrix_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline dimension_type
Dense_Matrix::max_num_rows() {
  return Matrix::max_num_rows();
}

inline dimension_type
Dense_Matrix::max_num_columns() {
  return Matrix::max_num_columns();
}

inline memory_size_type
Dense_Matrix::total_memory_in_bytes() const {
  return m.total_memory_in_bytes();
}

inline bool
Dense_Matrix::has_no_rows() const {
  return m.has_no_rows();
}

inline Matrix::const_iterator
Dense_Matrix::begin() const {
  return m.begin();
}

inline Dense_Matrix::const_iterator
Dense_Matrix::end() const {
  return m.end();
}

inline void
Dense_Matrix::swap(Dense_Matrix& y) {
  m.swap(y.m);
}

inline
Dense_Matrix::Dense_Matrix()
  : m() {
}

inline
Dense_Matrix::Dense_Matrix(const Dense_Matrix& y)
  :m(y.m) {
}

inline
Dense_Matrix::~Dense_Matrix() {
}

inline Dense_Matrix&
Dense_Matrix::operator=(const Dense_Matrix& y) {
  m = y.m;
  return *this;
}

inline void
Dense_Matrix::add_row(const Row& y) {
  m.add_row(y);
}

inline Row&
Dense_Matrix::operator[](const dimension_type k) {
  return m[k];
}

inline const Row&
Dense_Matrix::operator[](const dimension_type k) const {
  return m[k];
}

inline dimension_type
Dense_Matrix::num_rows() const {
  return m.num_rows();
}

inline dimension_type
Dense_Matrix::num_columns() const {
  return m.num_columns();
}

/*! \relates Dense_Matrix */
inline bool
operator!=(const Dense_Matrix& x, const Dense_Matrix& y) {
  return !(x == y);
}

inline void
Dense_Matrix::erase_to_end(const dimension_type first_to_erase) {
  m.erase_to_end(first_to_erase);
}

inline void
Dense_Matrix::clear() {
  m.clear();
}

inline Dense_Matrix::Dense_Matrix(const dimension_type n_rows,
        const dimension_type n_columns,
        Row::Flags row_flags)
  : m(n_rows,n_columns,row_flags) {
}

inline void
Dense_Matrix::add_zero_rows(const dimension_type n, Row::Flags row_flags) {
  m.add_zero_rows(n,row_flags);
}

inline void
Dense_Matrix::add_zero_columns(const dimension_type n) {
  m.add_zero_columns(n);
}

inline void
Dense_Matrix::add_zero_rows_and_columns(const dimension_type n,
               const dimension_type m1,
               Row::Flags row_flags) {
  m.add_zero_rows_and_columns(n,m1,row_flags);
}

inline void
Dense_Matrix::add_recycled_row(Row& y) {
  m.add_recycled_row(y);
}

inline void
Dense_Matrix::resize_no_copy(const dimension_type new_n_rows,
          const dimension_type new_n_columns,
          Row::Flags row_flags) {
  m.resize_no_copy(new_n_rows,new_n_columns,row_flags);
}

inline void
Dense_Matrix::ascii_dump(std::ostream& s) const {
  m.ascii_dump(s);
}

inline bool
Dense_Matrix::ascii_load(std::istream& s) {
  return m.ascii_load(s);
}

inline void
Dense_Matrix::swap_columns(const dimension_type i, const dimension_type j) {
  m.swap_columns(i,j);
}

inline void
Dense_Matrix::remove_trailing_columns(const dimension_type n) {
  m.remove_trailing_columns(n);
}

inline void
Dense_Matrix::permute_columns(const std::vector<dimension_type>& cycles) {
  m.permute_columns(cycles);
}

/*! \relates Parma_Polyhedra_Library::Dense_Matrix */
inline bool
operator==(const Dense_Matrix& x, const Dense_Matrix& y) {
  return (x.m == y.m);
}

inline memory_size_type
Dense_Matrix::external_memory_in_bytes() const {
  return m.external_memory_in_bytes();
}

inline bool
Dense_Matrix::OK() const {
  return m.OK();
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Dense_Matrix */
inline void
swap(Parma_Polyhedra_Library::Dense_Matrix& x,
     Parma_Polyhedra_Library::Dense_Matrix& y) {
  x.swap(y);
}

} // namespace std


#endif // !defined(PPL_Dense_Matrix_inlines_hh)
