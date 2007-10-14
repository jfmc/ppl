/* Polynomial_Constraint class implementation: inline functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Polynomial_Constraint_inlines_hh
#define PPL_Polynomial_Constraint_inlines_hh 1

#include "Polynomial.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Polynomial_Constraint::Polynomial_Constraint(Polynomial& p, Relation r)
  : rel(r) {
  std::swap(poly, p);
}

inline
Polynomial_Constraint::Polynomial_Constraint(const Polynomial_Constraint& c)
  : poly(c.poly), rel(c.rel) {
}

inline
Polynomial_Constraint::~Polynomial_Constraint() {
}

inline Polynomial_Constraint&
Polynomial_Constraint::operator=(const Polynomial_Constraint& c) {
  poly = c.poly;
  rel = c.rel;
  return *this;
}

inline dimension_type
Polynomial_Constraint::max_space_dimension() {
  return Polynomial::max_space_dimension();
}

inline dimension_type
Polynomial_Constraint::space_dimension() const {
  return poly.space_dimension();
}

inline const Polynomial&
Polynomial_Constraint::polynomial() const {
  return poly;
}

inline Polynomial_Constraint::Relation
Polynomial_Constraint::relation() const {
  return rel;
}

inline bool
Polynomial_Constraint::is_equality() const {
  return relation() == EQUALITY;
}

inline bool
Polynomial_Constraint::is_nonstrict_inequality() const {
  return relation() == NONSTRICT_INEQUALITY;
}

inline bool
Polynomial_Constraint::is_strict_inequality() const {
  return relation() == STRICT_INEQUALITY;
}

inline void
Polynomial_Constraint::set_is_equality() {
  rel = EQUALITY;
}

inline void
Polynomial_Constraint::set_is_nonstrict_inequality() {
  rel = NONSTRICT_INEQUALITY;
}

inline void
Polynomial_Constraint::set_is_strict_inequality() {
  rel = STRICT_INEQUALITY;
}

inline Coefficient_traits::const_reference
Polynomial_Constraint::coefficient(const Term& t) const {
  if (t.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(t)", "t", t);
  return poly.coefficient(t);
}

inline Coefficient_traits::const_reference
Polynomial_Constraint::constant_term_coefficient() const {
  return poly.constant_term_coefficient();
}

inline memory_size_type
Polynomial_Constraint::external_memory_in_bytes() const {
  return poly.external_memory_in_bytes();
}

inline memory_size_type
Polynomial_Constraint::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline void
Polynomial_Constraint::normalize() {
  poly.normalize();
}

inline void
Polynomial_Constraint::sign_normalize() {
  if (is_equality())
    poly.sign_normalize();
}

inline void
Polynomial_Constraint::strong_normalize() {
  normalize();
  sign_normalize();
}

/*! \relates Polynomial_Constraint */
inline bool
operator==(const Polynomial_Constraint& x, const Polynomial_Constraint& y) {
  return x.rel == y.rel && x.poly.is_equal_to(y.poly);
}

/*! \relates Polynomial_Constraint */
inline bool
operator!=(const Polynomial_Constraint& x, const Polynomial_Constraint& y) {
  return !(x == y);
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator==(const Polynomial& p1, const Polynomial& p2) {
  Polynomial diff = p1 - p2;
  Polynomial_Constraint c(diff, Polynomial_Constraint::EQUALITY);
  // Enforce normalization.
  c.strong_normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>=(const Polynomial& p1, const Polynomial& p2) {
  Polynomial diff = p1 - p2;
  Polynomial_Constraint c(diff, Polynomial_Constraint::NONSTRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>(const Polynomial& p1, const Polynomial& p2) {
  Polynomial diff = p1 - p2;
  Polynomial_Constraint c(diff, Polynomial_Constraint::STRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator==(Coefficient_traits::const_reference n, const Polynomial& p) {
  Polynomial diff = n - p;
  Polynomial_Constraint c(diff, Polynomial_Constraint::EQUALITY);
  // Enforce normalization.
  c.strong_normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>=(Coefficient_traits::const_reference n, const Polynomial& p) {
  Polynomial diff = n - p;
  Polynomial_Constraint c(diff, Polynomial_Constraint::NONSTRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>(Coefficient_traits::const_reference n, const Polynomial& p) {
  Polynomial diff = n - p;
  Polynomial_Constraint c(diff, Polynomial_Constraint::STRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator==(const Polynomial& p, Coefficient_traits::const_reference n) {
  Polynomial diff = p - n;
  Polynomial_Constraint c(diff, Polynomial_Constraint::EQUALITY);
  // Enforce normalization.
  c.strong_normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>=(const Polynomial& p, Coefficient_traits::const_reference n) {
  Polynomial diff = p - n;
  Polynomial_Constraint c(diff, Polynomial_Constraint::NONSTRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator>(const Polynomial& p, Coefficient_traits::const_reference n) {
  Polynomial diff = p - n;
  Polynomial_Constraint c(diff, Polynomial_Constraint::STRICT_INEQUALITY);
  // Enforce normalization.
  c.normalize();
  return c;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<=(const Polynomial& p1, const Polynomial& p2) {
  return p2 >= p1;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<=(Coefficient_traits::const_reference n, const Polynomial& p) {
  return p >= n;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<=(const Polynomial& p, Coefficient_traits::const_reference n) {
  return n >= p;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<(const Polynomial& p1, const Polynomial& p2) {
  return p2 > p1;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<(Coefficient_traits::const_reference n, const Polynomial& p) {
  return p > n;
}

/*! \relates Polynomial_Constraint */
inline Polynomial_Constraint
operator<(const Polynomial& p, Coefficient_traits::const_reference n) {
  return n > p;
}

inline const Polynomial_Constraint&
Polynomial_Constraint::zero_dim_false() {
  static const Polynomial_Constraint
    zdf(Polynomial::zero() == Coefficient_one());
  return zdf;
}

inline void
Polynomial_Constraint::swap(Polynomial_Constraint& y) {
  std::swap(poly, y.poly);
  std::swap(rel, y.rel);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
inline void
swap(Parma_Polyhedra_Library::Polynomial_Constraint& x,
     Parma_Polyhedra_Library::Polynomial_Constraint& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polynomial_Constraint_inlines_hh)
