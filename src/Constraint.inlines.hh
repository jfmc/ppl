/* Constraint class implementation: inline functions.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

INLINE
Parma_Polyhedra_Library::Constraint::Constraint()
  : Row() {
}

INLINE
Parma_Polyhedra_Library::Constraint::Constraint(LinExpression& e) {
  swap(e);
}

INLINE
Parma_Polyhedra_Library::Constraint::Constraint(const Constraint& c)
  : Row(c) {
}

INLINE
Parma_Polyhedra_Library::Constraint::Constraint(Row::Type type, size_t size)
  : Row(type, size) {
}

INLINE
Parma_Polyhedra_Library::Constraint::~Constraint() {
}

INLINE bool
Parma_Polyhedra_Library::Constraint::is_equality() const {
  return is_line_or_equality();
}

INLINE Parma_Polyhedra_Library::Constraint::Type
Parma_Polyhedra_Library::Constraint::type() const {
  return is_equality() ? EQUALITY : INEQUALITY;
}

INLINE bool
Parma_Polyhedra_Library::Constraint::is_inequality() const {
  return is_ray_or_vertex_or_inequality();
}

INLINE void
Parma_Polyhedra_Library::Constraint::set_is_equality() {
  set_is_line_or_equality();
}

INLINE void
Parma_Polyhedra_Library::Constraint::set_is_inequality() {
  set_is_ray_or_vertex_or_inequality();
}

namespace Parma_Polyhedra_Library {

INLINE Constraint
operator >>(const Constraint& y, unsigned int p) {
  size_t y_size = y.size();
  Constraint x(y.Row::type(), y_size+p);
  x[0] = y[0];
  for (size_t i = 1; i < y_size; ++i)
    x[i+p] = y[i];
  return x;
}

INLINE Constraint
operator ==(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

INLINE Constraint
operator >=(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator >(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  // e1 > e2 is the same as e1-e2 > 0,
  // which is the same as e1-e2+1 >= 0.
  diff += Integer::one;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator ==(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

INLINE Constraint
operator >=(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator >(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  // n > e is the same as n-e > 0,
  // which is the same as n-e+1 >= 0.
  diff += Integer::one;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator ==(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

INLINE Constraint
operator >=(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator >(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  // e > n is the same as e-n > 0,
  // which is the same as e-n+1 >= 0.
  diff += Integer::one;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

INLINE Constraint
operator <=(const LinExpression& e1, const LinExpression& e2) {
  return e2 >= e1;
}

INLINE Constraint
operator <(const LinExpression& e1, const LinExpression& e2) {
  return e2 > e1;
}

INLINE Constraint
operator <=(const Integer& n, const LinExpression& e) {
  return e >= n;
}

INLINE Constraint
operator <(const Integer& n, const LinExpression& e) {
  return e > n;
}

INLINE Constraint
operator <=(const LinExpression& e, const Integer& n) {
  return n >= e;
}

INLINE Constraint
operator <(const LinExpression& e, const Integer& n) {
  return n > e;
}

} // namespace Parma_Polyhedra_Library
