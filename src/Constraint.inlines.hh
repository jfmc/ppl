/* Constraint class implementation: inline functions.
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

#ifndef PPL_Constraint_inlines_hh
#define PPL_Constraint_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Constraint.defs.hh"

#include "Linear_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

inline bool
Constraint::is_necessarily_closed() const {
  return (topology_ == NECESSARILY_CLOSED);
}

inline bool
Constraint::is_not_necessarily_closed() const {
  return !is_necessarily_closed();
}

inline dimension_type
Constraint::space_dimension() const {
  if (is_necessarily_closed())
    return expr.space_dimension();
  else {
    assert(expr.space_dimension() != 0);
    return expr.space_dimension() - 1;
  }
}

inline bool
Constraint::is_line_or_equality() const {
  return (kind_ == LINE_OR_EQUALITY);
}

inline bool
Constraint::is_ray_or_point_or_inequality() const {
  return (kind_ == RAY_OR_POINT_OR_INEQUALITY);
}

inline Topology
Constraint::topology() const {
  return topology_;
}

inline void
Constraint::set_is_line_or_equality() {
  kind_ = LINE_OR_EQUALITY;
}

inline void
Constraint::set_is_ray_or_point_or_inequality() {
  kind_ = RAY_OR_POINT_OR_INEQUALITY;
}

inline void
Constraint::set_topology(Topology x) {
  if (topology() == x)
    return;
  if (topology() == NECESSARILY_CLOSED)
    // Add a column for the epsilon dimension.
    expr.set_space_dimension(expr.space_dimension() + 1);
  else {
    PPL_ASSERT(expr.space_dimension() != 0);
    expr.set_space_dimension(expr.space_dimension() - 1);
  }
  topology_ = x;
}

inline void
Constraint::mark_as_necessarily_closed() {
  PPL_ASSERT(is_not_necessarily_closed());
  topology_ = NECESSARILY_CLOSED;
}

inline void
Constraint::mark_as_not_necessarily_closed() {
  PPL_ASSERT(is_necessarily_closed());
  topology_ = NOT_NECESSARILY_CLOSED;
}

inline void
Constraint::set_necessarily_closed() {
  set_topology(NECESSARILY_CLOSED);
}

inline void
Constraint::set_not_necessarily_closed() {
  set_topology(NOT_NECESSARILY_CLOSED);
}

inline
Constraint::Constraint(dimension_type sz)
  : expr(),
    kind_(RAY_OR_POINT_OR_INEQUALITY),
    topology_(NOT_NECESSARILY_CLOSED) {
  PPL_ASSERT(sz != 0);
  expr.set_space_dimension(sz - 1);
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(dimension_type sz, Kind kind, Topology topology)
  : expr(), kind_(kind), topology_(topology) {
  PPL_ASSERT(sz != 0);
  expr.set_space_dimension(sz - 1);
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(Linear_Expression& e, Type type, Topology topology)
  : topology_(topology) {
  PPL_ASSERT(type != STRICT_INEQUALITY || topology == NOT_NECESSARILY_CLOSED);
  expr.swap(e);
  if (type == EQUALITY)
    kind_ = LINE_OR_EQUALITY;
  else
    kind_ = RAY_OR_POINT_OR_INEQUALITY;
  strong_normalize();
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(const Constraint& c)
  : expr(c.expr), kind_(c.kind_), topology_(c.topology_) {
  // NOTE: This does not call PPL_ASSERT(OK()) because this is called by OK().
}

inline
Constraint::Constraint(const Constraint& c, const dimension_type sz)
  : expr(c.expr, sz), kind_(c.kind_), topology_(c.topology_) {
  PPL_ASSERT(OK());
}

inline
Constraint::~Constraint() {
}

inline Constraint&
Constraint::operator=(const Constraint& c) {
  expr = c.expr;
  kind_ = c.kind_;
  topology_ = c.topology_;
  PPL_ASSERT(OK());
  
  return *this;
}

inline Linear_Expression&
Constraint::expression() {
  return expr;
}

inline const Linear_Expression&
Constraint::expression() const {
  return expr;
}

inline dimension_type
Constraint::max_space_dimension() {
  return Linear_Expression::max_space_dimension();
}

inline void
Constraint::set_space_dimension(dimension_type space_dim) {
  if (topology() == NECESSARILY_CLOSED) {
    expr.set_space_dimension(space_dim);
  } else {
    const dimension_type old_space_dim = space_dimension();
    if (space_dim > old_space_dim) {
      expr.set_space_dimension(space_dim + 1);
      expr.swap_space_dimensions(Variable(space_dim), Variable(old_space_dim));
    } else {
      expr.swap_space_dimensions(Variable(space_dim), Variable(old_space_dim));
      expr.set_space_dimension(space_dim + 1);
    }
  }
  PPL_ASSERT(space_dimension() == space_dim);
}

inline bool
Constraint::remove_space_dimensions(const Variables_Set& vars) {
  expr.remove_space_dimensions(vars);
  return true;
}

inline bool
Constraint::is_equality() const {
  return is_line_or_equality();
}

inline bool
Constraint::is_inequality() const {
  return is_ray_or_point_or_inequality();
}

inline Constraint::Type
Constraint::type() const {
  if (is_equality())
    return EQUALITY;
  if (is_necessarily_closed())
    return NONSTRICT_INEQUALITY;
  else
    return (expr.coefficient(Variable(expr.space_dimension() - 1)) < 0)
      ? STRICT_INEQUALITY
      : NONSTRICT_INEQUALITY;
}

inline bool
Constraint::is_nonstrict_inequality() const {
  return type() == NONSTRICT_INEQUALITY;
}

inline bool
Constraint::is_strict_inequality() const {
  return type() == STRICT_INEQUALITY;
}

inline void
Constraint::set_is_equality() {
  set_is_line_or_equality();
}

inline void
Constraint::set_is_inequality() {
  set_is_ray_or_point_or_inequality();
}

inline Coefficient_traits::const_reference
Constraint::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return expr.coefficient(v);
}

inline Coefficient_traits::const_reference
Constraint::inhomogeneous_term() const {
  return expr.inhomogeneous_term();
}

inline memory_size_type
Constraint::external_memory_in_bytes() const {
  return expr.external_memory_in_bytes();
}

inline memory_size_type
Constraint::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline void
Constraint::strong_normalize() {
  expr.normalize();
  sign_normalize();
}

/*! \relates Constraint */
inline bool
operator==(const Constraint& x, const Constraint& y) {
  return x.is_equivalent_to(y);
}

/*! \relates Constraint */
inline bool
operator!=(const Constraint& x, const Constraint& y) {
  return !x.is_equivalent_to(y);
}

/*! \relates Constraint */
inline Constraint
operator==(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff = e1 - e2;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(const Variable v1, const Variable v2) {
  Linear_Expression diff
    = (v1.space_dimension() < v2.space_dimension()) ? v1-v2 : v2-v1;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff = e1 - e2;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Variable v1, const Variable v2) {
  Linear_Expression diff = v1-v2;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  const dimension_type e1_dim = e1.space_dimension();
  const dimension_type e2_dim = e2.space_dimension();
  if (e1_dim > e2_dim)
    diff -= Variable(e1_dim);
  else
    diff -= Variable(e2_dim);
  diff += e1;
  diff -= e2;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Variable v1, const Variable v2) {
  Linear_Expression diff = v1-v2;
  diff -= Variable(std::max(v1.space_dimension(), v2.space_dimension()));
  return Constraint(diff,
                    Constraint::STRICT_INEQUALITY,
                    NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff = n - e;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff = n - e;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += n;
  diff -= e;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff = e - n;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff = e - n;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += e;
  diff -= n;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator<=(const Linear_Expression& e1, const Linear_Expression& e2) {
  return e2 >= e1;
}

/*! \relates Constraint */
inline Constraint
operator<=(const Variable v1, const Variable v2) {
  return v2 >= v1;
}

/*! \relates Constraint */
inline Constraint
operator<=(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  return e >= n;
}

/*! \relates Constraint */
inline Constraint
operator<=(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n >= e;
}

/*! \relates Constraint */
inline Constraint
operator<(const Linear_Expression& e1, const Linear_Expression& e2) {
  return e2 > e1;
}

/*! \relates Constraint */
inline Constraint
operator<(const Variable v1, const Variable v2) {
  return v2 > v1;
}

/*! \relates Constraint */
inline Constraint
operator<(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  return e > n;
}

/*! \relates Constraint */
inline Constraint
operator<(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n > e;
}

inline const Constraint&
Constraint::zero_dim_false() {
  PPL_ASSERT(zero_dim_false_p != 0);
  return *zero_dim_false_p;
}

inline const Constraint&
Constraint::zero_dim_positivity() {
  PPL_ASSERT(zero_dim_positivity_p != 0);
  return *zero_dim_positivity_p;
}

inline const Constraint&
Constraint::epsilon_geq_zero() {
  PPL_ASSERT(epsilon_geq_zero_p != 0);
  return *epsilon_geq_zero_p;
}

inline const Constraint&
Constraint::epsilon_leq_one() {
  PPL_ASSERT(epsilon_leq_one_p != 0);
  return *epsilon_leq_one_p;
}

inline void
Constraint::swap(Constraint& y) {
  expr.swap(y.expr);
  std::swap(kind_, y.kind_);
  std::swap(topology_, y.topology_);
}

inline Coefficient_traits::const_reference
Constraint::epsilon_coefficient() const {
  PPL_ASSERT(is_not_necessarily_closed());
  return expr.coefficient(Variable(expr.space_dimension() - 1));
}

inline void
Constraint::set_epsilon_coefficient(Coefficient_traits::const_reference n) {
  PPL_ASSERT(is_not_necessarily_closed());
  expr.set_coefficient(Variable(expr.space_dimension() - 1), n);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Constraint */
template <>
inline void
swap(Parma_Polyhedra_Library::Constraint& x,
     Parma_Polyhedra_Library::Constraint& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Constraint_inlines_hh)
