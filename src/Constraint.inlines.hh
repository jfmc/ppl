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

inline
Parma_Polyhedra_Library::Constraint::Constraint()
  : Row() {
}

inline
Parma_Polyhedra_Library::Constraint::Constraint(LinExpression& e) {
  swap(e);
}

inline
Parma_Polyhedra_Library::Constraint::Constraint(const Constraint& c)
  : Row(c) {
}

inline
Parma_Polyhedra_Library::Constraint::Constraint(Row::Type type, size_t size)
  : Row(type, size) {
}

inline
Parma_Polyhedra_Library::Constraint::~Constraint() {
}

inline bool
Parma_Polyhedra_Library::Constraint::is_equality() const {
  return is_line_or_equality();
}

inline Parma_Polyhedra_Library::Constraint::Type
Parma_Polyhedra_Library::Constraint::type() const {
  return is_equality() ? EQUALITY : INEQUALITY;
}

inline bool
Parma_Polyhedra_Library::Constraint::is_inequality() const {
  return is_ray_or_vertex_or_inequality();
}

inline void
Parma_Polyhedra_Library::Constraint::set_is_equality() {
  set_is_line_or_equality();
}

inline void
Parma_Polyhedra_Library::Constraint::set_is_inequality() {
  set_is_ray_or_vertex_or_inequality();
}

namespace Parma_Polyhedra_Library {

inline Constraint
operator ==(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator >=(const LinExpression& e1, const LinExpression& e2) {
  LinExpression diff = e1 - e2;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator ==(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator >=(const Integer& n, const LinExpression& e) {
  LinExpression diff = n - e;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator ==(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_equality();
  return c;
}

inline Constraint
operator >=(const LinExpression& e, const Integer& n) {
  LinExpression diff = e - n;
  Constraint c(diff);
  c.set_is_inequality();
  return c;
}

inline Constraint
operator <=(const LinExpression& e1, const LinExpression& e2) {
  return e2 >= e1;
}

inline Constraint
operator <=(const Integer& n, const LinExpression& e) {
  return e >= n;
}

inline Constraint
operator <=(const LinExpression& e, const Integer& n) {
  return n >= e;
}

} // namespace Parma_Polyhedra_Library
