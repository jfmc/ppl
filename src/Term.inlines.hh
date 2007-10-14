/* Term class implementation: inline functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Term_inlines_hh
#define PPL_Term_inlines_hh 1

//#include "Variable.defs.hh"
#include <stdexcept>
#include <limits>
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline void
Term::Checked_Exponent_Type_Policy::handle_result(Result r) {
  if (is_special(r))
    throw_result_exception(r);
}

inline dimension_type
Term::max_space_dimension() {
  degree_type max_for_degree_computations
    = std::numeric_limits<degree_type>::max()
    / std::numeric_limits<exponent_type>::max();
  Vector::size_type max_for_vector = Vector().max_size();
  if (max_for_degree_computations <= max_for_vector)
    return max_for_degree_computations;
  else
    return max_for_vector;
}

inline dimension_type
Term::max_exponent() {
  return std::numeric_limits<Checked_Exponent_Type>::max();
}

inline
Term::Term()
  : vec() {
}

inline
Term::Term(const dimension_type sz)
  : vec(sz, 0) {
}

inline
Term::Term(const Term& t, const dimension_type sz)
  : vec(sz, 0) {
  for (dimension_type i = std::min(sz, t.vec.size()); i-- > 0; )
    vec[i] = t.vec[i];
}

inline
Term::Term(const Variable v)
  : vec(v.space_dimension() <= max_space_dimension()
	? v.id() + 1
	: (throw std::length_error("PPL::Term::"
				   "Term(v):\n"
				   "v exceeds the maximum allowed "
				   "space dimension."),
	   v.id() + 1)) {
  vec[v.id()] = 1;
}

inline
Term::Term(const Term& e)
  : vec(e.vec) {
}

inline
Term::~Term() {
}

inline Term::Checked_Exponent_Type&
Term::operator[](const dimension_type k) {
  assert(k < vec.size());
  return vec[k];
}

inline Term::Checked_Exponent_Type
Term::operator[](const dimension_type k) const {
  assert(k < vec.size());
  return vec[k];
}

inline exponent_type
Term::exponent(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    return 0;
  return vec[v.id()];
}

inline degree_type
Term::degree(const Variable v) const {
  return exponent(v);
}

inline degree_type
Term::degree() const {
  degree_type d = 0;
  for (dimension_type i = vec.size(); i-- > 0; )
    d += vec[i];
  return d;
}

inline const Term&
Term::one() {
  static Term z;
  return z;
}

inline memory_size_type
Term::external_memory_in_bytes() const {
  return vec.capacity() * sizeof(exponent_type);
}

inline memory_size_type
Term::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

/*! \relates Term */
inline Term
operator*(const Term& t, const Variable v) {
  return mul_pow(t, v, 1);
}

/*! \relates Term */
inline Term
operator*(Variable v, const Term& t) {
  return t * v;
}

/*! \relates Term */
inline Term&
operator*=(Term& t, const Variable v) {
  return mul_pow_assign(t, v, 1);
}

/*! \relates Term */
inline Term&
operator/=(Term& t, const Variable v) {
  return div_pow_assign(t, v, 1);
}

/*! \relates Term */
inline Term
operator/(const Term& t, const Variable v) {
  return div_pow(t, v, 1);
}

/*! \relates Term */
inline Term
pow(const Variable v, const exponent_type n) {
  Term t(v);
  pow_assign(t, n);
  return t;
}

/*! \relates Term */
inline Term
pow(const Term& t, const exponent_type n) {
  Term new_t(t);
  pow_assign(new_t, n);
  return new_t;
}

/*! \relates Term */
inline Term&
exact_div_assign(Term& t, const Variable v) {
  assert(v.space_dimension() <= t.space_dimension());
  assert(t[v.id()] > 0);
  --t[v.id()];
  return t;
}

/*! \relates Term */
inline bool
operator!=(const Term& x, const Term& y) {
  return !(x == y);
}

inline void
Term::swap(Term& y) {
  std::swap(vec, y.vec);
}

inline bool
Term::Compare::operator()(const Term& x, const Term& y) const {
#if 0
  return lexicographic_less(x, y);
#else
  return graded_lexicographic_less(x, y);
#endif
}

/*! \relates Term */
inline std::ostream&
IO_Operators::operator<<(std::ostream& s, exponent_type e) {
  s << static_cast<unsigned long>(e);
  return s;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Term */
inline void
swap(Parma_Polyhedra_Library::Term& x, Parma_Polyhedra_Library::Term& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Term_inlines_hh)
