/* Congruence class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Congruence_inlines_hh
#define PPL_Congruence_inlines_hh 1

#include "Constraint.defs.hh"
#include "Linear_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Congruence::Congruence(const Congruence& cg)
  : Row(cg) {
}

inline
Congruence::Congruence(const Congruence& cg,
		       dimension_type sz, dimension_type capacity)
  : Row(cg, sz, capacity) {
}

inline
Congruence::Congruence(const Congruence& cg,
		       Coefficient_traits::const_reference k)
  : Row(cg) {
  if (k >= 0)
    (*this)[size()-1] *= k;
  else
    (*this)[size()-1] *= -k;
}

inline
Congruence::~Congruence() {
}

inline const Congruence&
Congruence::zero_dim_integrality() {
  static const Congruence zdi(Linear_Expression::zero() %= Coefficient_one());
  return zdi;
}

inline Congruence&
Congruence::operator=(const Congruence& c) {
  Row::operator=(c);
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Congruence */
inline Congruence
operator%=(const Linear_Expression& e,
	   const Coefficient_traits::const_reference n) {
  // Ensure that diff has capacity for the modulo.
  dimension_type e_dim = e.space_dimension();
  Linear_Expression diff(e, e_dim + 1, e_dim + 2);
  diff -= n;
  Congruence cg(diff, 1);
  return cg;
}

/*! \relates Parma_Polyhedra_Library::Congruence */
inline Congruence
operator/(const Congruence& cg,
	  const Coefficient_traits::const_reference k) {
  Congruence ret (cg, k);
  return ret;
}

/*! \relates Congruence */
inline Congruence
operator/(const Constraint& c,
	  const Coefficient_traits::const_reference m) {
  Congruence ret (c);
  return ret / m;
}

inline Congruence&
Congruence::operator/=(const Coefficient_traits::const_reference k) {
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
  return static_cast<const Row&>(x_temp) == static_cast<const Row&>(y_temp);
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
  return (*this)[v.id()+1];}

inline Coefficient_traits::const_reference
Congruence::inhomogeneous_term() const {
  return (*this)[0];
}

inline Coefficient_traits::const_reference
Congruence::modulus() const {
  assert(size() > 0);
  return (*this)[size()-1];
}

inline bool
Congruence::is_trivial_true() const {
  if (modulus() == 1
      || inhomogeneous_term() == 0) {
    for (unsigned i = 1; i <= space_dimension(); i++)
      if ((*this)[i] != 0)
	return false;
    return true;
  }
  return false;
}

inline bool
Congruence::is_trivial_false() const {
  if (inhomogeneous_term() == 0
      || modulus() != 0)
    return false;
  for (unsigned i = 1; i <= space_dimension(); i++)
    if ((*this)[i] != 0)
      return false;
  return true;
}

inline bool
Congruence::is_equality() const {
  return modulus() == 0;
}

inline void
Congruence::set_is_equality() {
  // FIX modulus() = 0?
  assert(size() > 0);
  (*this)[size()-1] = 0;
}

inline bool
Congruence::is_virtual() const {
  return modulus() == -1;
}

inline void
Congruence::set_is_virtual() {
  // FIX modulus() = -1?
  assert(size() > 0);
  (*this)[size()-1] = -1;
}

inline memory_size_type
Congruence::external_memory_in_bytes() const {
  return Row::external_memory_in_bytes();
}

inline memory_size_type
Congruence::total_memory_in_bytes() const {
  return Row::total_memory_in_bytes();
}

inline
Congruence::Congruence(const Constraint& c)
  : Row(c, c.space_dimension() + 1, c.space_dimension() + 2) {
  Row::expand_within_capacity(size()+1);
  (*this)[size()-1] = 1;
}

inline
Congruence::Congruence(Linear_Expression& le,
		       Coefficient_traits::const_reference m) {
  Row::swap(static_cast<Row&>(le));
  Row::expand_within_capacity(size()+1);
  if (m >= 0)
    (*this)[size()-1] = m;
  else
    (*this)[size()-1] = -m;
}

inline void
Congruence::swap(Congruence& y) {
  Row::swap(y);
}

/*! \relates Congruence */
inline int
scalar_product_sign(const Linear_Row& x, const Congruence& y) {
  TEMP_INTEGER(z);
  scalar_product_assign(z, x, y);
  return sgn(z);
}

/*! \relates Congruence */
inline int
reduced_scalar_product_sign(const Linear_Row& x, const Congruence& y) {
  TEMP_INTEGER(z);
  reduced_scalar_product_assign(z, x, y);
  return sgn(z);
}

/*! \relates Congruence */
inline int
homogeneous_scalar_product_sign(const Linear_Row& x, const Congruence& y) {
  TEMP_INTEGER(z);
  homogeneous_scalar_product_assign(z, x, y);
  return sgn(z);
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
