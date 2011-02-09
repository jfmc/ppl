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
Congruence::Congruence()
  : Dense_Row() {
  resize(2);
  PPL_ASSERT(OK());
}

inline
Congruence::Congruence(const Congruence& cg)
  : Dense_Row(cg) {
}

inline
Congruence::Congruence(const Congruence& cg,
		       dimension_type sz, dimension_type capacity)
  : Dense_Row(cg, sz, capacity) {
}

inline void
Congruence::add_space_dimensions(dimension_type n) {
  const dimension_type old_size = size();
  const dimension_type new_size = old_size + n;
  resize(new_size);
  Dense_Row::swap(old_size - 1, new_size - 1);
}

inline void
Congruence::remove_space_dimensions(dimension_type n) {
  PPL_ASSERT(size() >= n + 2);
  const dimension_type old_size = size();
  const dimension_type new_size = size() - n;
  Dense_Row::swap(old_size - 1, new_size - 1);
  resize(new_size);
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
  Dense_Row::add_zeroes_and_shift(n, i + 1);
}

inline
Congruence::~Congruence() {
}

inline
Congruence::Congruence(Linear_Expression& le,
		       Coefficient_traits::const_reference m) {
  Dense_Row::swap(static_cast<Dense_Row&>(le));
  PPL_ASSERT(m >= 0);
  (*this)[size()-1] = m;
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
  Dense_Row::operator=(c);
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
    (*this)[size()-1] *= k;
  else
    (*this)[size()-1] *= -k;
  return *this;
}

/*! \relates Congruence */
inline bool
operator==(const Congruence& x, const Congruence& y) {
  Congruence x_temp(x);
  Congruence y_temp(y);
  x_temp.strong_normalize();
  y_temp.strong_normalize();
  return static_cast<const Dense_Row&>(x_temp)
         == static_cast<const Dense_Row&>(y_temp);
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
  return max_size() - 2;
}

inline dimension_type
Congruence::space_dimension() const {
  return size() - 2;
}

inline Coefficient_traits::const_reference
Congruence::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return (*this)[v.id()+1];
}

inline void
Congruence::set_coefficient(const Variable v,
                            Coefficient_traits::const_reference c) {
  PPL_ASSERT(v.space_dimension() <= space_dimension());
  (*this)[v.id() + 1] = c;
}

inline Coefficient_traits::const_reference
Congruence::inhomogeneous_term() const {
  return (*this)[0];
}

inline Coefficient_traits::const_reference
Congruence::modulus() const {
  PPL_ASSERT(size() > 1);
  return (*this)[size()-1];
}

inline void
Congruence::set_modulus(Coefficient_traits::const_reference m) {
  (*this)[size()-1] = m;
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
  return operator[](dim) * cg.modulus() == cg[dim] * modulus();
}

inline void
Congruence::set_is_equality() {
  (*this)[size()-1] = 0;
}

inline void
Congruence::negate(dimension_type first, dimension_type last) {
  Congruence& x = *this;
  for ( ; first != last; ++first)
    neg_assign(x[first]);
}

inline memory_size_type
Congruence::external_memory_in_bytes() const {
  return Dense_Row::external_memory_in_bytes();
}

inline memory_size_type
Congruence::total_memory_in_bytes() const {
  return Dense_Row::total_memory_in_bytes();
}

inline void
Congruence::swap(Congruence& y) {
  Dense_Row::swap(y);
}

inline void
Congruence::swap(dimension_type i, dimension_type j) {
  Dense_Row::swap(i, j);
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
