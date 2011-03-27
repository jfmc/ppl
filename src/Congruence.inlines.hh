/* Congruence class implementation: inline functions.
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

#ifndef PPL_Congruence_inlines_hh
#define PPL_Congruence_inlines_hh 1

#include "Linear_Expression.defs.hh"
#include "Constraint.defs.hh"

#include <sstream>

namespace Parma_Polyhedra_Library {

inline
Congruence::Congruence(dimension_type n)
  : row(n + 2, n + 2) {
  PPL_ASSERT(OK());
}

inline
Congruence::Congruence(const Congruence& cg)
  : row(cg.row) {
}

inline
Congruence::Congruence(const Congruence& cg,
		       dimension_type new_space_dimension)
  : row(cg.row, new_space_dimension + 2, new_space_dimension + 2) {
  if (new_space_dimension >= cg.space_dimension())
    // Swap the modulus to the correct place.
    swap(new_space_dimension + 1, cg.space_dimension() + 1);
  else
    // The modulus has not been copied yet, so do it.
    row[new_space_dimension + 1] = cg.modulus();
  PPL_ASSERT(OK());
}

inline Dense_Row&
Congruence::get_row() {
  return row;
}

inline const Dense_Row&
Congruence::get_row() const {
  return row;
}

inline Congruence&
Congruence::expression() {
  return *this;
}

inline const Congruence&
Congruence::expression() const {
  return *this;
}

inline void
Congruence::add_space_dimensions(dimension_type n) {
  const dimension_type old_size = row.size();
  const dimension_type new_size = old_size + n;
  row.resize(new_size);
  row.swap(old_size - 1, new_size - 1);
}

inline void
Congruence::remove_space_dimensions(dimension_type n) {
  PPL_ASSERT(row.size() >= n + 2);
  const dimension_type old_size = row.size();
  const dimension_type new_size = row.size() - n;
  row.swap(old_size - 1, new_size - 1);
  row.resize(new_size);
}

inline void
Congruence::set_space_dimension(dimension_type n) {
  if (n > space_dimension())
    add_space_dimensions(n - space_dimension());
  else
    remove_space_dimensions(space_dimension() - n);
}

inline void
Congruence::shift_coefficients(dimension_type n, dimension_type i) {
  row.add_zeroes_and_shift(n, i + 1);
}

inline
Congruence::~Congruence() {
}

inline
Congruence::Congruence(Linear_Expression& le,
		       Coefficient_traits::const_reference m) {
  row.swap(static_cast<Dense_Row&>(le.get_row()));
  PPL_ASSERT(m >= 0);
  row[row.size()-1] = m;
}

inline Congruence
Congruence::create(const Linear_Expression& e,
		   Coefficient_traits::const_reference n) {
  // Ensure that diff has capacity for the modulus.
  Linear_Expression diff(e, e.space_dimension() + 2);
  diff -= n;
  Congruence cg(diff, 1);
  return cg;
}

inline Congruence
Congruence::create(Coefficient_traits::const_reference n,
		   const Linear_Expression& e) {
  // Ensure that diff has capacity for the modulus.
  Linear_Expression diff(e, e.space_dimension() + 2);
  diff -= n;
  Congruence cg(diff, 1);
  return cg;
}

/*! \relates Parma_Polyhedra_Library::Congruence */
inline Congruence
operator%=(const Linear_Expression& e1, const Linear_Expression& e2) {
  return Congruence::create(e1, e2);
}

/*! \relates Parma_Polyhedra_Library::Congruence */
inline Congruence
operator%=(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return Congruence::create(e, n);
}

/*! \relates Parma_Polyhedra_Library::Congruence */
inline Congruence
operator/(const Congruence& cg, Coefficient_traits::const_reference k) {
  Congruence ret = cg;
  Coefficient m = ret.modulus();
  m *= k;
  if (m < 0)
    neg_assign(m);
  ret.set_modulus(m);
  return ret;
}

inline const Congruence&
Congruence::zero_dim_integrality() {
  return *zero_dim_integrality_p;
}

inline const Congruence&
Congruence::zero_dim_false() {
  return *zero_dim_false_p;
}

inline Congruence&
Congruence::operator=(const Congruence& c) {
  row = c.row;
  return *this;
}

/*! \relates Congruence */
inline Congruence
operator/(const Constraint& c, Coefficient_traits::const_reference m) {
  Congruence ret(c);
  return ret / m;
}

inline Congruence&
Congruence::operator/=(Coefficient_traits::const_reference k) {
  if (k >= 0)
    row[row.size() - 1] *= k;
  else
    row[row.size() - 1] *= -k;
  return *this;
}

/*! \relates Congruence */
inline bool
operator==(const Congruence& x, const Congruence& y) {
  Congruence x_temp(x);
  Congruence y_temp(y);
  x_temp.strong_normalize();
  y_temp.strong_normalize();
  return x_temp.row == y_temp.row;
}

/*! \relates Congruence */
inline bool
operator!=(const Congruence& x, const Congruence& y) {
  return !(x == y);
}

inline dimension_type
Congruence::max_space_dimension() {
  // The first coefficient holds the inhomogeneous term, while
  // the last coefficient is for the modulus.
  return Dense_Row::max_size() - 2;
}

inline dimension_type
Congruence::space_dimension() const {
  return row.size() - 2;
}

inline Coefficient_traits::const_reference
Congruence::operator[](dimension_type i) const {
  return row[i];
}

inline Coefficient&
Congruence::operator[](dimension_type i) {
  return row[i];
}

inline Coefficient_traits::const_reference
Congruence::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return row[v.id() + 1];
}

inline void
Congruence::permute_dimensions(const std::vector<dimension_type>& cycles) {
  PPL_ASSERT(cycles[cycles.size() - 1] == 0);
#ifndef NDEBUG
  // Check that no permutation involves the modulus.
  for (dimension_type i = cycles.size(); i-- > 0; )
    PPL_ASSERT(cycles[i] <= space_dimension());
#endif
  PPL_DIRTY_TEMP_COEFFICIENT(tmp);
  for (dimension_type i = 0, j = 0; i < cycles.size(); i = ++j) {
    // Make `j' be the index of the next cycle terminator.
    while (cycles[j] != 0)
      ++j;
    // Cycles of length less than 2 are not allowed.
    PPL_ASSERT(j - i >= 2);
    if (j - i == 2)
      // For cycles of length 2 no temporary is needed, just a swap.
      row.swap(cycles[i], cycles[i + 1]);
    else {
      // Longer cycles need a temporary.
      tmp = row.get(cycles[j - 1]);
      for (dimension_type l = (j - 1); l > i; --l)
        row.swap(cycles[l-1], cycles[l]);
      if (tmp == 0)
        row.reset(cycles[i]);
      else
        std::swap(tmp, row[cycles[i]]);
    }
  }
}

inline Coefficient_traits::const_reference
Congruence::inhomogeneous_term() const {
  return row[0];
}

inline void
Congruence::set_inhomogeneous_term(Coefficient_traits::const_reference c) {
  row[0] = c;
}

inline Coefficient_traits::const_reference
Congruence::modulus() const {
  PPL_ASSERT(row.size() > 1);
  return row[row.size()-1];
}

inline void
Congruence::set_modulus(Coefficient_traits::const_reference m) {
  row[row.size()-1] = m;
  PPL_ASSERT(OK());
}

inline bool
Congruence::is_proper_congruence() const {
  return modulus() > 0;
}

inline bool
Congruence::is_equality() const {
  return modulus() == 0;
}

inline bool
Congruence::is_equal_at_dimension(dimension_type dim,
				  const Congruence& cg) const {
  return row[dim] * cg.modulus() == cg.row[dim] * modulus();
}

inline void
Congruence::set_is_equality() {
  row[row.size() - 1] = 0;
}

inline void
Congruence::negate(dimension_type first, dimension_type last) {
  for ( ; first != last; ++first)
    neg_assign(row[first]);
}

inline memory_size_type
Congruence::external_memory_in_bytes() const {
  return row.external_memory_in_bytes();
}

inline memory_size_type
Congruence::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

inline void
Congruence::swap(Congruence& y) {
  std::swap(row, y.row);
}

inline void
Congruence::swap(dimension_type i, dimension_type j) {
  row.swap(i, j);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Congruence */
inline void
swap(Parma_Polyhedra_Library::Congruence& x,
     Parma_Polyhedra_Library::Congruence& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Congruence_inlines_hh)
