/* Generator class implementation: inline functions.
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
Parma_Polyhedra_Library::Generator::Generator()
  : Row() {
}

INLINE
Parma_Polyhedra_Library::Generator::Generator(LinExpression& e) {
  swap(e);
}

INLINE
Parma_Polyhedra_Library::Generator::Generator(const Generator& g)
  : Row(g) {
}

INLINE
Parma_Polyhedra_Library::Generator::~Generator() {
}

INLINE bool
Parma_Polyhedra_Library::Generator::is_line() const {
  return is_line_or_equality();
}

INLINE Parma_Polyhedra_Library::Generator::Type
Parma_Polyhedra_Library::Generator::type() const {
  return is_line() ? LINE : (((*this)[0] == 0) ? RAY : VERTEX);
}

INLINE bool
Parma_Polyhedra_Library::Generator::is_ray_or_vertex() const {
  return is_ray_or_vertex_or_inequality();
}

INLINE void
Parma_Polyhedra_Library::Generator::set_is_line() {
  set_is_line_or_equality();
}

INLINE void
Parma_Polyhedra_Library::Generator::set_is_ray_or_vertex() {
  set_is_ray_or_vertex_or_inequality();
}

namespace Parma_Polyhedra_Library {

INLINE Generator
operator |(int, const LinExpression& e) {
  LinExpression ec = e;
  Generator g(ec);
  g.set_is_line();
  return g;
}

INLINE Generator
operator ^(int, const LinExpression& e) {
  LinExpression ec = e;
  Generator g(ec);
  g[0] = 0;
  g.set_is_ray_or_vertex();
  return g;
}

INLINE Generator
operator /=(const LinExpression& e, const Integer& n) {
  LinExpression ec = e;
  Generator g(ec);
  g[0] = n;
  g.set_is_ray_or_vertex();
  return g;
}

} // namespace Parma_Polyhedra_Library








