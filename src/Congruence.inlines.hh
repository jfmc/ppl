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

#include "Congruence.defs.hh"

#include "Constraint.defs.hh"

#include <sstream>

namespace Parma_Polyhedra_Library {

inline
Congruence::Congruence(dimension_type n) {
  expr.set_space_dimension(n + 1);
  PPL_ASSERT(OK());
}

inline
Congruence::Congruence(const Congruence& cg)
  : expr(cg.expr) {
}

inline
Congruence::Congruence(const Congruence& cg,
		       dimension_type new_space_dimension)
  : expr(cg.expr, new_space_dimension + 2) {
  if (new_space_dimension >= cg.space_dimension())
    // Swap the modulus to the correct place.
    swap_space_dimensions(Variable(new_space_dimension), Variable(cg.space_dimension()));
  else
    set_modulus(cg.modulus());
  PPL_ASSERT(OK());
}

inline Linear_Expression&
Congruence::expression() {
  return expr;
}

inline const Linear_Expression&
Congruence::expression() const {
  return expr;
}

inline void
Congruence::add_space_dimensions(dimension_type n) {
  const dimension_type old_space_dim = space_dimension();
  const dimension_type new_space_dim = old_space_dim + n;
  expr.set_space_dimension(new_space_dim + 1);
  expr.swap_space_dimensions(Variable(old_space_dim), Variable(new_space_dim));
  PPL_ASSERT(OK());
}

inline void
Congruence::remove_space_dimensions(dimension_type n) {
  PPL_ASSERT(space_dimension() >= n);
  const dimension_type old_space_dim = space_dimension();
  const dimension_type new_space_dim = old_space_dim - n;
  expr.swap_space_dimensions(Variable(old_space_dim), Variable(new_space_dim));
  expr.set_space_dimension(new_space_dim + 1);
  PPL_ASSERT(OK());
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
  expr.get_row().add_zeroes_and_shift(n, i + 1);
}

inline
Congruence::~Congruence() {
}

inline
Congruence::Congruence(Linear_Expression& le,
		       Coefficient_traits::const_reference m) {
  expr.swap(le);
  PPL_ASSERT(m >= 0);
  set_modulus(m);
  PPL_ASSERT(OK());
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
  expr = c.expr;
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
    modulus() *= k;
  else
    modulus() *= -k;
  return *this;
}

/*! \relates Congruence */
inline bool
operator==(const Congruence& x, const Congruence& y) {
  Congruence x_temp(x);
  Congruence y_temp(y);
  x_temp.strong_normalize();
  y_temp.strong_normalize();
  return x_temp.expr.is_equal_to(y_temp.expr);
}

/*! \relates Congruence */
inline bool
operator!=(const Congruence& x, const Congruence& y) {
  return !(x == y);
}

inline dimension_type
Congruence::max_space_dimension() {
  // The last coefficient holds the modulus.
  return Linear_Expression::max_space_dimension() - 1;
}

inline dimension_type
Congruence::space_dimension() const {
  return expr.space_dimension() - 1;
}

inline Coefficient_traits::const_reference
Congruence::operator[](dimension_type i) const {
  return expr[i];
}

inline Coefficient&
Congruence::operator[](dimension_type i) {
  return expr[i];
}

inline Coefficient_traits::const_reference
Congruence::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return expr.coefficient(v);
}

inline void
Congruence::permute_space_dimensions(const std::vector<Variable>& cycles) {
  expr.permute_space_dimensions(cycles);
}

inline Coefficient_traits::const_reference
Congruence::inhomogeneous_term() const {
  return expr.inhomogeneous_term();
}

inline void
Congruence::set_inhomogeneous_term(Coefficient_traits::const_reference c) {
  expr.set_inhomogeneous_term(c);
}

inline Coefficient_traits::const_reference
Congruence::modulus() const {
  PPL_ASSERT(expr.space_dimension() > 0);
  return expr[expr.space_dimension()];
}

inline Coefficient&
Congruence::modulus() {
  PPL_ASSERT(expr.space_dimension() > 0);
  return expr[expr.space_dimension()];
}

inline void
Congruence::set_modulus(Coefficient_traits::const_reference m) {
  modulus() = m;
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
  return expr[dim] * cg.modulus() == cg.expr[dim] * modulus();
}

inline void
Congruence::set_is_equality() {
  modulus() = 0;
}

inline void
Congruence::negate(dimension_type first, dimension_type last) {
  for ( ; first != last; ++first)
    neg_assign(expr[first]);
}

inline memory_size_type
Congruence::external_memory_in_bytes() const {
  return expr.external_memory_in_bytes();
}

inline memory_size_type
Congruence::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

inline void
Congruence::swap(Congruence& y) {
  std::swap(expr, y.expr);
}

inline void
Congruence::swap_space_dimensions(Variable v1, Variable v2) {
  expr.swap_space_dimensions(v1, v2);
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
