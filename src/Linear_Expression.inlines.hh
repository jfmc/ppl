/* Linear_Expression class implementation: inline functions.
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

#ifndef PPL_Linear_Expression_inlines_hh
#define PPL_Linear_Expression_inlines_hh 1

#include "Linear_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

inline dimension_type
Linear_Expression::max_space_dimension() {
  return Dense_Row::max_size() - 1;
}

inline
Linear_Expression::Linear_Expression()
  : impl(new Linear_Expression_Impl()) {
}

inline
Linear_Expression::Linear_Expression(dimension_type sz, bool x)
  : impl(new Linear_Expression_Impl(sz, x)) {
}

inline
Linear_Expression::Linear_Expression(const Linear_Expression& e)
  : impl(new Linear_Expression_Impl(*e.impl)) {
}

inline Linear_Expression&
Linear_Expression::operator=(const Linear_Expression& e) {
  Linear_Expression tmp = e;
  swap(tmp);
  return *this;
}

inline
Linear_Expression::~Linear_Expression() {
  delete impl;
}

inline
Linear_Expression::Linear_Expression(const Linear_Expression& e,
				     dimension_type sz)
  : impl(new Linear_Expression_Impl(*e.impl, sz)) {
}

inline
Linear_Expression::Linear_Expression(Coefficient_traits::const_reference n)
  : impl(new Linear_Expression_Impl(n)) {
}

inline dimension_type
Linear_Expression::space_dimension() const {
  return impl->space_dimension();
}

inline void
Linear_Expression::set_space_dimension(dimension_type n) {
  impl->set_space_dimension(n);
}

inline Coefficient_traits::const_reference
Linear_Expression::coefficient(Variable v) const {
  return impl->coefficient(v);
}

inline void
Linear_Expression
::set_coefficient(Variable v, Coefficient_traits::const_reference n) {
  impl->set_coefficient(v, n);
}

inline Coefficient_traits::const_reference
Linear_Expression::inhomogeneous_term() const {
  return impl->inhomogeneous_term();
}

inline void
Linear_Expression
::set_inhomogeneous_term(Coefficient_traits::const_reference n) {
  impl->set_inhomogeneous_term(n);
}

inline void
Linear_Expression
::linear_combine(const Linear_Expression& y, dimension_type i) {
  impl->linear_combine(*y.impl, i);
}

inline void
Linear_Expression
::linear_combine(const Linear_Expression& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2) {
  impl->linear_combine(*y.impl, c1, c2);
}

inline void
Linear_Expression::swap_space_dimensions(Variable v1, Variable v2) {
  impl->swap_space_dimensions(v1, v2);
}

inline void
Linear_Expression::shift_space_dimensions(Variable v, dimension_type n) {
  impl->shift_space_dimensions(v, n);
}

inline bool
Linear_Expression::is_zero() const {
  return impl->is_zero();
}

inline bool
Linear_Expression::all_homogeneous_terms_are_zero() const {
  return impl->all_homogeneous_terms_are_zero();
}

inline const Linear_Expression&
Linear_Expression::zero() {
  PPL_ASSERT(zero_p != 0);
  return *zero_p;
}

inline memory_size_type
Linear_Expression::external_memory_in_bytes() const {
  return impl->total_memory_in_bytes();
}

inline memory_size_type
Linear_Expression::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e) {
  return e;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression x = e;
  x += n;
  return x;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator+(const Linear_Expression& e, const Variable v) {
  Linear_Expression x = e;
  x += v;
  return x;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator-(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression x = e;
  x -= n;
  return x;
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator-(const Variable v, const Variable w) {
  const dimension_type v_space_dim = v.space_dimension();
  const dimension_type w_space_dim = w.space_dimension();
  const dimension_type space_dim = std::max(v_space_dim, w_space_dim);
  if (space_dim > Linear_Expression::max_space_dimension())
    throw std::length_error("Linear_Expression "
                            "PPL::operator+(v, w):\n"
                            "v or w exceed the maximum allowed "
                            "space dimension.");
  if (v_space_dim >= w_space_dim) {
    Linear_Expression e(v);
    e -= w;
    return e;
  } else {
    Linear_Expression e(w.space_dimension(), true);
    e -= w;
    e += v;
    return e;
  }
}

/*! \relates Linear_Expression */
inline Linear_Expression
operator*(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression x = e;
  x *= n;
  return x;
}

/*! \relates Linear_Expression */
inline Linear_Expression&
operator+=(Linear_Expression& e, Coefficient_traits::const_reference n) {
  *e.impl += n;
  return e;
}

/*! \relates Linear_Expression */
inline Linear_Expression&
operator-=(Linear_Expression& e, Coefficient_traits::const_reference n) {
  *e.impl -= n;
  return e;
}

inline void
Linear_Expression::swap(Linear_Expression& y) {
  std::swap(impl, y.impl);
}

inline void
Linear_Expression::normalize() {
  impl->normalize();
}

inline void
Linear_Expression::ascii_dump(std::ostream& s) const {
  impl->ascii_dump(s);
}

inline bool
Linear_Expression::ascii_load(std::istream& s) {
  return impl->ascii_load(s);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
inline void
swap(Parma_Polyhedra_Library::Linear_Expression& x,
     Parma_Polyhedra_Library::Linear_Expression& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Linear_Expression_inlines_hh)
