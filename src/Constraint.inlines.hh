/* Constraint class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Constraint_inlines_hh
#define _Constraint_inlines_hh 1

#include "LinExpression.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Constraint::Constraint(LinExpression& e) {
  Row::swap(e);
}

inline
Constraint::Constraint(const Constraint& c)
  : Row(c) {
}

inline
Constraint::Constraint(const Constraint& c, size_t sz)
  : Row(c, sz, sz) {
}

inline
Constraint::Constraint(Row::Type t, size_t sz)
  : Row(t, sz) {
}

inline
Constraint::~Constraint() {
}

inline Constraint&
Constraint::operator=(const Constraint& c) {
  Row::operator=(c);
  return *this;
}

inline size_t
Constraint::space_dimension() const {
  return Row::space_dimension();
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
    return ((*this)[size() - 1] < 0)
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

inline const Integer&
Constraint::coefficient(Variable v) const {
  if (v.id() >= space_dimension())
    throw_dimension_incompatible("PPL::Constraint::coefficient(v)", v);
  return Row::coefficient(v.id());
}

inline const Integer&
Constraint::inhomogeneous_term() const {
  return Row::inhomogeneous_term();
}

inline Constraint
operator==(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_equality();
  // Enforcing normalization.
#if EXTRA_NORMALIZATION
  c.strongly_normalize();
#else
  c.normalize();
#endif
  return c;
}

inline Constraint
operator>=(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_inequality();
  // Enforcing normalization.
  c.normalize();
  return c;
}

inline Constraint
operator>(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  size_t e1_dim = e1.space_dimension();
  size_t e2_dim = e2.space_dimension();
  if (e1_dim > e2_dim)
    diff -= Variable(e1_dim);
  else
    diff -= Variable(e2_dim);
  diff += e1;
  diff -= e2;

  Constraint c(diff);
  c.set_not_necessarily_closed();
  c.set_is_inequality();
  return c;
}

inline Constraint
operator==(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_equality();
  // Enforcing normalization.
#if EXTRA_NORMALIZATION
  c.strongly_normalize();
#else
  c.normalize();
#endif
  return c;
}

inline Constraint
operator>=(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_inequality();
  // Enforcing normalization.
  c.normalize();
  return c;
}

inline Constraint
operator>(const Integer& n, const LinExpression& e) {
  LinExpression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += n;
  diff -= e;

  Constraint c(diff);
  c.set_not_necessarily_closed();
  c.set_is_inequality();
  return c;
}

inline Constraint
operator==(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_equality();
  // Enforcing normalization.
#if EXTRA_NORMALIZATION
  c.strongly_normalize();
#else
  c.normalize();
#endif
  return c;
}

inline Constraint
operator>=(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_inequality();
  // Enforcing normalization.
  c.normalize();
  return c;
}

inline Constraint
operator>(const LinExpression& e, const Integer& n) {
  LinExpression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += e;
  diff -= n;

  Constraint c(diff);
  c.set_not_necessarily_closed();
  c.set_is_inequality();
  return c;
}

inline Constraint
operator<=(const LinExpression& e1, const LinExpression& e2) {
  return e2 >= e1;
}

inline Constraint
operator<=(const Integer& n, const LinExpression& e) {
  return e >= n;
}

inline Constraint
operator<=(const LinExpression& e, const Integer& n) {
  return n >= e;
}

inline Constraint
operator<(const LinExpression& e1, const LinExpression& e2) {
  return e2 > e1;
}

inline Constraint
operator<(const Integer& n, const LinExpression& e) {
  return e > n;
}

inline Constraint
operator<(const LinExpression& e, const Integer& n) {
  return n > e;
}

inline const Constraint&
Constraint::zero_dim_false() {
  static Constraint zdf(LinExpression::zero() == Integer_one());
  return zdf;
}

inline const Constraint&
Constraint::zero_dim_positivity() {
  static Constraint zdp(LinExpression::zero() <= Integer_one());
  return zdp;
}

inline const Constraint&
Constraint::epsilon_leq_one() {
  static Constraint eps_leq_one(LinExpression::zero() < Integer_one());
  return eps_leq_one;
}

inline void
Constraint::swap(Constraint& y) {
  Row::swap(y);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*!
  \relates Parma_Polyhedra_Library::Constraint
*/
inline void
swap(Parma_Polyhedra_Library::Constraint& x,
     Parma_Polyhedra_Library::Constraint& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(_Constraint_inlines_hh)
