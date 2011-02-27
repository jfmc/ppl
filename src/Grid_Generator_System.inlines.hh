/* Grid_Generator_System class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Grid_Generator_System_inlines_hh
#define PPL_Grid_Generator_System_inlines_hh 1

#include "Grid_Generator.defs.hh"

namespace Parma_Polyhedra_Library {

inline void
Grid_Generator_System::set_sorted(bool b) {
  Linear_System<Linear_Row>::set_sorted(b);
}

inline void
Grid_Generator_System::unset_pending_rows() {
  Linear_System<Linear_Row>::unset_pending_rows();
}

inline void
Grid_Generator_System::set_index_first_pending_row(const dimension_type i) {
  Linear_System<Linear_Row>::set_index_first_pending_row(i);
}

inline void
Grid_Generator_System::resize_no_copy(const dimension_type new_num_rows,
				      const dimension_type new_num_columns) {
  Linear_System<Linear_Row>::resize_no_copy(new_num_rows, new_num_columns);
}

inline dimension_type
Grid_Generator_System::num_columns() const {
  return Linear_System<Linear_Row>::num_columns();
}

inline void
Grid_Generator_System
::permute_columns(const std::vector<dimension_type>& cycles) {
  return Linear_System<Linear_Row>::permute_columns(cycles);
}

inline bool
Grid_Generator_System::is_equal_to(const Grid_Generator_System& y) const {
  return operator==(static_cast<const Linear_System<Linear_Row>&>(*this),
                    static_cast<const Linear_System<Linear_Row>&>(y));
}

inline
Grid_Generator_System::Grid_Generator_System()
  : Linear_System<Linear_Row>(NECESSARILY_CLOSED) {
  // For grid generators, two extra columns are needed, but one has already
  // been added by Linear_System's constructor.
  add_zero_columns(1);
  set_sorted(false);
  PPL_ASSERT(space_dimension() == 0);
}

inline
Grid_Generator_System::Grid_Generator_System(const Grid_Generator_System& gs)
  : Linear_System<Linear_Row>(gs) {
}

inline
Grid_Generator_System::Grid_Generator_System(dimension_type dim)
  : Linear_System<Linear_Row>(NECESSARILY_CLOSED) {
  // For grid generators, two extra columns are needed, but one has already
  // been added by the Linear_System's constructor.
  add_zero_columns(dim + 1);
  set_sorted(false);
  PPL_ASSERT(space_dimension() == dim);
}

inline
Grid_Generator_System::Grid_Generator_System(const Grid_Generator& g)
  : Linear_System<Linear_Row>(NECESSARILY_CLOSED) {
  Linear_System<Linear_Row>::insert(g);
  set_sorted(false);
}

inline
Grid_Generator_System::~Grid_Generator_System() {
}

inline Grid_Generator_System&
Grid_Generator_System::operator=(const Grid_Generator_System& y) {
  Linear_System<Linear_Row>::operator=(y);
  return *this;
}

inline dimension_type
Grid_Generator_System::max_space_dimension() {
  // Grid generators use an extra column for the parameter divisor.
  return Linear_System<Linear_Row>::max_space_dimension() - 1;
}

inline dimension_type
Grid_Generator_System::space_dimension() const {
  PPL_ASSERT(Linear_System<Linear_Row>::space_dimension() > 0);
  // Grid generators use an extra column for the parameter divisor.
  return Linear_System<Linear_Row>::space_dimension() - 1;
}

inline const Grid_Generator_System&
Grid_Generator_System::zero_dim_univ() {
  PPL_ASSERT(zero_dim_univ_p != 0);
  return *zero_dim_univ_p;
}

inline void
Grid_Generator_System::clear() {
  Linear_System<Linear_Row>::clear();
  // For grid generators, two extra columns are needed, but one has already
  // been added by Linear_System<Linear_Row>::clear().
  add_zero_columns(1);
  set_sorted(false);
  unset_pending_rows();
  PPL_ASSERT(space_dimension() == 0);
}

inline void
Grid_Generator_System::swap(Grid_Generator_System& y) {
  Linear_System<Linear_Row>::swap(y);
}

inline memory_size_type
Grid_Generator_System::external_memory_in_bytes() const {
  return Linear_System<Linear_Row>::external_memory_in_bytes();
}

inline memory_size_type
Grid_Generator_System::total_memory_in_bytes() const {
  return Linear_System<Linear_Row>::total_memory_in_bytes();
}

inline dimension_type
Grid_Generator_System::num_rows() const {
  return Linear_System<Linear_Row>::num_rows();
}

inline
Grid_Generator_System::const_iterator::const_iterator()
  : i() {
}

inline
Grid_Generator_System::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

inline
Grid_Generator_System::const_iterator::~const_iterator() {
}

inline Grid_Generator_System::const_iterator&
Grid_Generator_System::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

inline const Grid_Generator&
Grid_Generator_System::const_iterator::operator*() const {
  return static_cast<const Grid_Generator&>(*i);
}

inline const Grid_Generator*
Grid_Generator_System::const_iterator::operator->() const {
  return static_cast<const Grid_Generator*>(i.operator->());
}

inline Grid_Generator_System::const_iterator&
Grid_Generator_System::const_iterator::operator++() {
  ++i;
  return *this;
}

inline Grid_Generator_System::const_iterator
Grid_Generator_System::const_iterator::operator++(int) {
  const const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline bool
Grid_Generator_System
::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
Grid_Generator_System
::const_iterator::operator!=(const const_iterator& y) const {
  return i != y.i;
}

inline bool
Grid_Generator_System::empty() const {
  return Linear_System<Linear_Row>::has_no_rows();
}

inline
Grid_Generator_System::const_iterator
::const_iterator(const Linear_System<Linear_Row>::const_iterator& y)
  : i(y) {
}

inline Grid_Generator_System::const_iterator
Grid_Generator_System::begin() const {
  return static_cast<Grid_Generator_System::const_iterator>
    (Linear_System<Linear_Row>::begin());
}

inline Grid_Generator_System::const_iterator
Grid_Generator_System::end() const {
  return static_cast<Grid_Generator_System::const_iterator>
    (Linear_System<Linear_Row>::end());
}

inline const Grid_Generator&
Grid_Generator_System::operator[](const dimension_type k) const {
  return static_cast<const Grid_Generator&>(Linear_System<Linear_Row>::operator[](k));
}

/*! \relates Grid_Generator_System */
inline bool
operator==(const Grid_Generator_System& x,
	   const Grid_Generator_System& y) {
  return x.is_equal_to(y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Constraint_System */
inline void
swap(Parma_Polyhedra_Library::Grid_Generator_System& x,
     Parma_Polyhedra_Library::Grid_Generator_System& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Grid_Generator_System_inlines_hh)
