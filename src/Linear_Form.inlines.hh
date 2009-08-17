/* Linear_Form class implementation: inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Linear_Form_inlines_hh
#define PPL_Linear_Form_inlines_hh 1

#include "Variable.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <typename C>
inline dimension_type
Linear_Form<C>::max_space_dimension() {
  return vec_type().max_size() - 1;
}

template <typename C>
inline
Linear_Form<C>::Linear_Form()
  : vec(1, zero) {
  vec.reserve(compute_capacity(1, vec_type::max_size()));
}

template <typename C>
inline
Linear_Form<C>::Linear_Form(dimension_type sz, bool)
  : vec(sz, zero) {
  vec.reserve(compute_capacity(sz, vec_type().max_size()));
}

template <typename C>
inline
Linear_Form<C>::Linear_Form(const Linear_Form& f)
  : vec(f.vec) {
}

template <typename C>
inline
Linear_Form<C>::~Linear_Form() {
}

template <typename C>
inline dimension_type
Linear_Form<C>::size() const {
  return vec.size();
}

template <typename C>
inline void
Linear_Form<C>::extend(dimension_type sz) {
  assert(sz > size());
  vec.reserve(compute_capacity(sz, vec_type().max_size()));
  vec.resize(sz, zero);
}

template <typename C>
inline
Linear_Form<C>::Linear_Form(const Linear_Form& f, dimension_type sz)
  : vec(f.vec) {
  extend(sz);
}

template <typename C>
inline
Linear_Form<C>::Linear_Form(const C& n)
  : vec(1, n) {
  vec.reserve(compute_capacity(1, vec_type::max_size()));
}

template <typename C>
inline dimension_type
Linear_Form<C>::space_dimension() const {
  return size() - 1;
}

template <typename C>
inline const C&
Linear_Form<C>::coefficient(Variable v) const {
  if (v.space_dimension() > space_dimension())
    return zero;
  return vec[v.id()+1];
}

template <typename C>
inline C&
Linear_Form<C>::operator[](dimension_type i) {
  assert(i < size());
  return vec[i];
}

template <typename C>
inline const C&
Linear_Form<C>::operator[](dimension_type i) const {
  assert(i < size());
  return vec[i];
}

template <typename C>
inline const C&
Linear_Form<C>::inhomogeneous_term() const {
  return vec[0];
}

template <typename C>
inline memory_size_type
Linear_Form<C>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator+(const Linear_Form<C>& f) {
  return f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator+(const Linear_Form<C>& f, const C& n) {
  return n + f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator+(const Linear_Form<C>& f, const Variable v) {
  return v + f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator-(const Linear_Form<C>& f, const C& n) {
  return -n + f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator-(const Variable v, const Variable w) {
  return Linear_Form<C>(v, w);
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>
operator*(const Linear_Form<C>& f, const C& n) {
  return n * f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>&
operator+=(Linear_Form<C>& f, const C& n) {
  f[0] += n;
  return f;
}

/*! \relates Linear_Form */
template <typename C>
inline Linear_Form<C>&
operator-=(Linear_Form<C>& f, const C& n) {
  f[0] -= n;
  return f;
}

/*! \relates Linear_Form */
template <typename C>
inline bool
operator!=(const Linear_Form<C>& x, const Linear_Form<C>& y) {
  return !(x == y);
}

template <typename C>
inline void
Linear_Form<C>::swap(Linear_Form& y) {
  std::swap(vec, y.vec);
}

template <typename C>
inline void
Linear_Form<C>::ascii_dump(std::ostream& s) const {
  // FIXME: to be written.
}

template <typename C>
inline bool
Linear_Form<C>::ascii_load(std::istream& s) {
  // FIXME: to be written.
  return false;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Linear_Form */
template <typename C>
inline void
swap(Parma_Polyhedra_Library::Linear_Form<C>& x,
     Parma_Polyhedra_Library::Linear_Form<C>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Linear_Form_inlines_hh)
