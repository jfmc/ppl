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


#include "LinExpression.defs.hh"

namespace Parma_Polyhedra_Library {

  // CHECK ME.
inline
Constraint::Constraint(LinExpression& e) {
  swap(e);
}

inline
Constraint::Constraint(const Constraint& c)
  : Row(c) {
}

inline
Constraint::Constraint(const Constraint& c, size_t size)
  : Row(c, size, size) {
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

inline Constraint::Type
Constraint::type() const {
  if (is_equality())
    return EQUALITY;
  if (is_necessarily_closed())
    return NONSTRICT_INEQUALITY;
  else
    return ((*this)[size()-1] == 0)
      ? NONSTRICT_INEQUALITY
      : STRICT_INEQUALITY;
}

inline bool
Constraint::is_equality() const {
  return is_line_or_equality();
}

inline bool
Constraint::is_inequality() const {
  return is_ray_or_point_or_inequality();
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
Constraint::coefficient() const {
  return Row::coefficient();
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
Constraint::epsilon_geq_zero() {
  static Constraint eps_geq_zero = construct_epsilon_geq_zero();
  return eps_geq_zero;
}

inline const Constraint&
Constraint::epsilon_leq_one() {
  static Constraint eps_leq_one(LinExpression::zero() < Integer_one());
  return eps_leq_one;
}

inline Constraint
operator==(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator>=(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator>(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  // Setting the \epsilon coefficient to -1.
  diff += - Variable(diff.space_dimension());
  Constraint c(diff);
  // FIXME: provide a single istruction for setting both at once.
  c.set_non_necessarily_closed();
  c.set_is_inequality();
  return c;
}

inline Constraint
operator==(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator>=(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator>(const Integer& n, const LinExpression& e) {
  // Setting the \epsilon coefficient to -1.
  LinExpression diff = n - e - Variable(e.space_dimension());
  Constraint c(diff);
  // FIXME: provide a single istruction for setting both at once.
  c.set_non_necessarily_closed();
  c.set_is_inequality();
  return c;
}

inline Constraint
operator==(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator>=(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator>(const LinExpression& e, const Integer& n) {
  // Setting the \epsilon coefficient to -1.
  LinExpression diff = e - n - Variable(e.space_dimension());
  Constraint c(diff);
  // FIXME: provide a single istruction for setting both at once.
  c.set_non_necessarily_closed();
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

} // namespace Parma_Polyhedra_Library

namespace std {

/*!
  Specialize <CODE>std::swap</CODE> to use the fast swap that is
  provided as a member function instead of using the default
  algorithm (which creates a temporary and uses assignment).
*/
inline void
swap(Parma_Polyhedra_Library::Constraint& x,
     Parma_Polyhedra_Library::Constraint& y) {
  x.swap(y);
}

} // namespace std
