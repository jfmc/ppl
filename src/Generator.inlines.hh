/* Generator class implementation: inline functions.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Generator_inlines_hh
#define PPL_Generator_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Generator::Generator(Linear_Expression& e, Type type, Topology topology) {
  assert(type != CLOSURE_POINT || topology == NOT_NECESSARILY_CLOSED);
  Linear_Row::swap(e);
  flags() = Flags(topology, (type == LINE
			     ? LINE_OR_EQUALITY
			     : RAY_OR_POINT_OR_INEQUALITY));
}

inline
Generator::Generator(const Generator& g)
  : Linear_Row(g) {
}

inline
Generator::Generator(const Generator& g, dimension_type dimension)
  : Linear_Row(g, dimension, dimension) {
}

inline
Generator::~Generator() {
}

inline Generator&
Generator::operator=(const Generator& g) {
  Linear_Row::operator=(g);
  return *this;
}

inline dimension_type
Generator::max_space_dimension() {
  return Linear_Row::max_space_dimension();
}

inline dimension_type
Generator::space_dimension() const {
  return Linear_Row::space_dimension();
}

inline bool
Generator::is_line() const {
  return is_line_or_equality();
}

inline bool
Generator::is_ray_or_point() const {
  return is_ray_or_point_or_inequality();
}

inline bool
Generator::is_ray() const {
  return is_ray_or_point() && ((*this)[0] == 0);
}

inline Generator::Type
Generator::type() const {
  if (is_line())
    return LINE;
  const Generator& g = *this;
  if (g[0] == 0)
    return RAY;
  if (is_necessarily_closed())
    return POINT;
  else
    // Checking the value of the epsilon coefficient.
    return (g[size() - 1] == 0) ? CLOSURE_POINT : POINT;
}

inline bool
Generator::is_point() const {
  return type() == POINT;
}

inline bool
Generator::is_closure_point() const {
  return type() == CLOSURE_POINT;
}

inline void
Generator::set_is_line() {
  set_is_line_or_equality();
}

inline void
Generator::set_is_ray_or_point() {
  set_is_ray_or_point_or_inequality();
}

inline Coefficient_traits::const_reference
Generator::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return Linear_Row::coefficient(v.id());
}

inline Coefficient_traits::const_reference
Generator::divisor() const {
  Coefficient_traits::const_reference d = Linear_Row::inhomogeneous_term();
  if (!is_ray_or_point() || d == 0)
    throw_invalid_argument("divisor()",
			   "*this is neither a point nor a closure point");
  return d;
}

inline memory_size_type
Generator::external_memory_in_bytes() const {
  return Linear_Row::external_memory_in_bytes();
}

inline memory_size_type
Generator::total_memory_in_bytes() const {
  return Linear_Row::total_memory_in_bytes();
}

inline const Generator&
Generator::zero_dim_point() {
  static const Generator zdp = point();
  return zdp;
}

inline const Generator&
Generator::zero_dim_closure_point() {
  static const Generator zdcp = closure_point();
  return zdcp;
}

/*! \relates Generator */
inline Generator
line(const Linear_Expression& e) {
  return Generator::line(e);
}

/*! \relates Generator */
inline Generator
ray(const Linear_Expression& e) {
  return Generator::ray(e);
}

/*! \relates Generator */
inline Generator
point(const Linear_Expression& e, Coefficient_traits::const_reference d) {
  return Generator::point(e, d);
}

/*! \relates Generator */
inline Generator
closure_point(const Linear_Expression& e, Coefficient_traits::const_reference d) {
  return Generator::closure_point(e, d);
}

inline void
Generator::swap(Generator& y) {
  Linear_Row::swap(y);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Generator */
inline void
swap(Parma_Polyhedra_Library::Generator& x,
     Parma_Polyhedra_Library::Generator& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Generator_inlines_hh)
