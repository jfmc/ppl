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
#include <stdexcept>

inline
Parma_Polyhedra_Library::Generator::Generator(LinExpression& e) {
  swap(e);
}

inline
Parma_Polyhedra_Library::Generator::Generator(const Generator& g)
  : Row(g) {
}

inline
Parma_Polyhedra_Library::Generator::~Generator() {
}

inline bool
Parma_Polyhedra_Library::Generator::is_line() const {
  return is_line_or_equality();
}

inline Parma_Polyhedra_Library::Generator::Type
Parma_Polyhedra_Library::Generator::type() const {
  return is_line() ? LINE : (((*this)[0] == 0) ? RAY : VERTEX);
}

inline bool
Parma_Polyhedra_Library::Generator::is_ray_or_vertex() const {
  return is_ray_or_vertex_or_inequality();
}

inline void
Parma_Polyhedra_Library::Generator::set_is_line() {
  set_is_line_or_equality();
}

inline void
Parma_Polyhedra_Library::Generator::set_is_ray_or_vertex() {
  set_is_ray_or_vertex_or_inequality();
}

inline Parma_Polyhedra_Library::Variable
Parma_Polyhedra_Library::Generator::last_variable() const {
  assert(Row::size() >= 2);
  return Variable(size()-2);
}

inline const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Generator::coefficient(Variable v) const {
  return Row::coefficient(v.id());
}

inline const Parma_Polyhedra_Library::Integer&
Parma_Polyhedra_Library::Generator::divisor() const {
  const Integer& d = Row::coefficient();
  if (!is_ray_or_vertex() || d == 0)
    throw std::invalid_argument("PPL::Generator::divisor(): "
				"*this is not a vertex");
  return d;
}


namespace Parma_Polyhedra_Library {

inline Generator
line(const LinExpression& e) {
  LinExpression ec = e;
  Generator g(ec);
  g[0] = 0;
  g.set_is_line();
  return g;
}

inline Generator
ray(const LinExpression& e) {
  LinExpression ec = e;
  Generator g(ec);
  g[0] = 0;
  g.set_is_ray_or_vertex();
  return g;
}

} // namespace Parma_Polyhedra_Library








