/* Linear_Expression class implementation: inline functions.
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

#ifndef PPL_Linear_Expression_inlines_hh
#define PPL_Linear_Expression_inlines_hh 1

#include "Variable.defs.hh"
#include "Coefficient.defs.hh"
#include "math_utilities.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

inline dimension_type
Linear_Expression::max_space_dimension() {
  return Dense_Row::max_size() - 1;
}

inline
Linear_Expression::Linear_Expression()
  : row(1) {
}

inline
Linear_Expression::Linear_Expression(dimension_type sz, bool)
  : row(sz) {
}

inline
Linear_Expression::Linear_Expression(const Linear_Expression& e)
  : row(e.row) {
}

inline
Linear_Expression::~Linear_Expression() {
}

inline
Linear_Expression::Linear_Expression(const Linear_Expression& e,
				     dimension_type sz)
  : row(e.row, sz, sz) {
}

inline
Linear_Expression::Linear_Expression(Coefficient_traits::const_reference n)
  : row(1) {
  row[0] = n;
}

inline dimension_type
Linear_Expression::space_dimension() const {
  return row.size() - 1;
}

inline void
Linear_Expression::set_space_dimension(dimension_type n) {
  row.resize(n + 1);
}

inline Coefficient_traits::const_reference
Linear_Expression::coefficient(Variable v) const {
  if (v.space_dimension() > space_dimension())
    return Coefficient_zero();
  return row[v.id() + 1];
}

inline void
Linear_Expression
::set_coefficient(Variable v, Coefficient_traits::const_reference n) {
  PPL_ASSERT(v.space_dimension() <= space_dimension());
  row[v.id() + 1] = n;
}

inline Coefficient_traits::const_reference
Linear_Expression::inhomogeneous_term() const {
  return row[0];
}

inline void
Linear_Expression
::set_inhomogeneous_term(Coefficient_traits::const_reference n) {
  row[0] = n;
}

inline void
Linear_Expression
::linear_combine(const Linear_Expression& y, dimension_type i) {
  Linear_Expression& x = *this;
  // We can combine only vector of the same dimension.
  PPL_ASSERT(x.space_dimension() == y.space_dimension());
  PPL_ASSERT(x.row[i] != 0);
  PPL_ASSERT(y.row[i] != 0);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_v);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_y_v);
  normalize2(x.row[i], y.row[i], normalized_x_v, normalized_y_v);
  neg_assign(normalized_x_v);
  x.row.linear_combine(y.row, normalized_y_v, normalized_x_v);
  assert(x.row[i] == 0);
}

inline void
Linear_Expression
::linear_combine(const Linear_Expression& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2) {
  row.linear_combine(y.row, c1, c2);
}

inline void
Linear_Expression::swap_space_dimensions(Variable v1, Variable v2) {
  row.swap(v1.space_dimension(), v2.space_dimension());
}

inline void
Linear_Expression::shift_space_dimensions(Variable v, dimension_type n) {
  row.add_zeroes_and_shift(n, v.space_dimension());
}

inline bool
Linear_Expression::is_zero() const {
  for (Dense_Row::const_iterator i = row.begin(), i_end = row.end();
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

inline bool
Linear_Expression::all_homogeneous_terms_are_zero() const {
  for (Dense_Row::const_iterator i = row.lower_bound(1), i_end = row.end();
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

inline const Linear_Expression&
Linear_Expression::zero() {
  PPL_ASSERT(zero_p != 0);
  return *zero_p;
}

inline memory_size_type
Linear_Expression::external_memory_in_bytes() const {
  return row.external_memory_in_bytes();
}

inline memory_size_type
Linear_Expression::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e) {
  return e;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n + e;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e, const Variable v) {
  return v + e;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator-(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return -n + e;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator-(const Variable v, const Variable w) {
  return Linear_Expression(v, w);
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator*(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n * e;
}

/*! \relates Linear_Expression */
inline Linear_Expression&
operator+=(Linear_Expression& e, Coefficient_traits::const_reference n) {
  e.row[0] += n;
  return e;
}

/*! \relates Linear_Expression */
inline Linear_Expression&
operator-=(Linear_Expression& e, Coefficient_traits::const_reference n) {
  e.row[0] -= n;
  return e;
}

inline void
Linear_Expression::swap(Linear_Expression& y) {
  row.swap(y.row);
}

inline void
Linear_Expression::normalize() {
  row.normalize();
}

inline void
Linear_Expression::ascii_dump(std::ostream& s) const {
  s << "size " << (space_dimension() + 1) << " ";
  for (dimension_type j = 0; j < row.size(); ++j) {
    s << row[j];
    if (j != row.size() - 1)
      s << ' ';
  }
}

inline bool
Linear_Expression::ascii_load(std::istream& s) {
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

  return true;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
inline void
swap(Parma_Polyhedra_Library::Linear_Expression& x,
     Parma_Polyhedra_Library::Linear_Expression& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Linear_Expression_inlines_hh)
