/* Linear_Expression_Impl class implementation: inline functions.
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

#ifndef PPL_Linear_Expression_Impl_inlines_hh
#define PPL_Linear_Expression_Impl_inlines_hh 1

#include "Linear_Expression_Impl.defs.hh"

#include "Variable.defs.hh"
#include "Coefficient.defs.hh"
#include "math_utilities.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <typename Row>
inline dimension_type
Linear_Expression_Impl<Row>::max_space_dimension() {
  return Row::max_size() - 1;
}

template <typename Row>
inline
Linear_Expression_Impl<Row>::Linear_Expression_Impl()
  : row(1) {
  PPL_ASSERT(OK());
}

template <typename Row>
inline
Linear_Expression_Impl<Row>::Linear_Expression_Impl(dimension_type sz, bool)
  : row(sz) {
  PPL_ASSERT(OK());
}

template <typename Row>
inline
Linear_Expression_Impl<Row>::~Linear_Expression_Impl() {
}

template <typename Row>
inline
Linear_Expression_Impl<Row>::Linear_Expression_Impl(Coefficient_traits::const_reference n)
  : row(1) {
  row[0] = n;
  PPL_ASSERT(OK());
}

template <typename Row>
inline dimension_type
Linear_Expression_Impl<Row>::space_dimension() const {
  return row.size() - 1;
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>::set_space_dimension(dimension_type n) {
  row.resize(n + 1);
  PPL_ASSERT(OK());
}

template <typename Row>
inline Coefficient_traits::const_reference
Linear_Expression_Impl<Row>::coefficient(Variable v) const {
  if (v.space_dimension() > space_dimension())
    return Coefficient_zero();
  return row[v.id() + 1];
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>
::set_coefficient(Variable v, Coefficient_traits::const_reference n) {
  PPL_ASSERT(v.space_dimension() <= space_dimension());
  row[v.id() + 1] = n;
  PPL_ASSERT(OK());
}

template <typename Row>
inline Coefficient_traits::const_reference
Linear_Expression_Impl<Row>::inhomogeneous_term() const {
  return row[0];
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>
::set_inhomogeneous_term(Coefficient_traits::const_reference n) {
  row[0] = n;
  PPL_ASSERT(OK());
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>::swap_space_dimensions(Variable v1, Variable v2) {
  row.swap(v1.space_dimension(), v2.space_dimension());
  PPL_ASSERT(OK());
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>::shift_space_dimensions(Variable v, dimension_type n) {
  row.add_zeroes_and_shift(n, v.space_dimension());
  PPL_ASSERT(OK());
}

template <typename Row>
inline bool
Linear_Expression_Impl<Row>::is_zero() const {
  for (typename Row::const_iterator i = row.begin(), i_end = row.end();
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

template <typename Row>
inline bool
Linear_Expression_Impl<Row>::all_homogeneous_terms_are_zero() const {
  for (typename Row::const_iterator i = row.lower_bound(1), i_end = row.end();
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

template <typename Row>
inline memory_size_type
Linear_Expression_Impl<Row>::external_memory_in_bytes() const {
  return row.external_memory_in_bytes();
}

template <typename Row>
inline memory_size_type
Linear_Expression_Impl<Row>::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

template <typename Row>
inline Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator+=(Coefficient_traits::const_reference n) {
  row[0] += n;
  return *this;
}

template <typename Row>
inline Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator-=(Coefficient_traits::const_reference n) {
  row[0] -= n;
  return *this;
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>::normalize() {
  row.normalize();
}

template <typename Row>
inline void
Linear_Expression_Impl<Row>::ascii_dump(std::ostream& s) const {
  s << "size " << (space_dimension() + 1) << " ";
  for (dimension_type j = 0; j < row.size(); ++j) {
    s << row[j];
    if (j != row.size() - 1)
      s << ' ';
  }
}

template <typename Row>
inline bool
Linear_Expression_Impl<Row>::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str))
    return false;
  if (str != "size")
    return false;

  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  row.resize(new_size);

  for (dimension_type j = 0; j < new_size; ++j)
    if (!(s >> row[j]))
      return false;

  PPL_ASSERT(OK());
  return true;
}

namespace IO_Operators {

template <typename Row>
inline std::ostream&
operator<<(std::ostream& s, const Linear_Expression_Impl<Row>& e) {
  return e << s;
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Linear_Expression_Impl_inlines_hh)
