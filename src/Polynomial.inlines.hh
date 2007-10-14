/* Polynomial class implementation: inline functions.
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

#ifndef PPL_Polynomial_inlines_hh
#define PPL_Polynomial_inlines_hh 1

#include "Monomial.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

inline
Polynomial::const_iterator
::const_iterator(const Map::const_iterator& iter)
  : i(iter) {
}

inline
Polynomial::const_iterator::const_iterator()
  : i() {
}

inline
Polynomial::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

inline
Polynomial::const_iterator::~const_iterator() {
}

inline Polynomial::const_iterator&
Polynomial::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

inline const Monomial&
Polynomial::const_iterator::operator*() const {
  return static_cast<const Monomial&>(*i);
}

inline const Monomial*
Polynomial::const_iterator::operator->() const {
  return static_cast<const Monomial*>(i.operator->());
}

inline Polynomial::const_iterator&
Polynomial::const_iterator::operator++() {
  ++i;
  return *this;
}

inline Polynomial::const_iterator
Polynomial::const_iterator::operator++(int) {
  const const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline Polynomial::const_iterator&
Polynomial::const_iterator::operator--() {
  --i;
  return *this;
}

inline Polynomial::const_iterator
Polynomial::const_iterator::operator--(int) {
  const const_iterator tmp = *this;
  operator--();
  return tmp;
}

inline bool
Polynomial::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
Polynomial::const_iterator::operator!=(const const_iterator& y) const {
  return i != y.i;
}

inline Polynomial::const_iterator
Polynomial::begin() const {
  return const_iterator(map.begin());
}

inline Polynomial::const_iterator
Polynomial::end() const {
  return const_iterator(map.end());
}

inline Polynomial::const_reverse_iterator
Polynomial::rbegin() const {
  return const_reverse_iterator(end());
}

inline Polynomial::const_reverse_iterator
Polynomial::rend() const {
  return const_reverse_iterator(begin());
}

inline dimension_type
Polynomial::max_space_dimension() {
  return Term::max_space_dimension();
}

inline
Polynomial::Polynomial()
  : map() {
  assert(OK());
}

inline
Polynomial::Polynomial(const Variable v)
  : map() {
  map.insert(Map::value_type(Term(v), Coefficient_one()));
  assert(OK());
}

inline
Polynomial::Polynomial(const Term& t)
  : map() {
  map.insert(Map::value_type(t, Coefficient_one()));
  assert(OK());
}

inline
Polynomial::Polynomial(const Monomial& m)
  : map() {
  if (m.coefficient() != 0)
    map.insert(m);
  assert(OK());
}

inline
Polynomial::Polynomial(const Polynomial& p)
  : map(p.map) {
}

inline
Polynomial::Polynomial(Map& m)
  : map() {
  std::swap(map, m);
}

inline
Polynomial::~Polynomial() {
}

inline
Polynomial::Polynomial(Coefficient_traits::const_reference n)
  : map() {
  if (n != 0)
    map.insert(Map::value_type(Term::one(), n));
  assert(OK());
}

inline Coefficient_traits::const_reference
Polynomial::coefficient(const Term& t) const {
  Map::const_iterator i = map.find(t);
  if (i == map.end())
    return Coefficient_zero();
  else {
    assert(i->second != 0);
    return i->second;
  }
}

inline Coefficient_traits::const_reference
Polynomial::constant_term_coefficient() const {
  Map::const_iterator map_begin = map.begin();
  if (map_begin != map.end() && map_begin->first == Term::one())
    return map_begin->second;
  else
    return Coefficient_zero();
}

inline const Polynomial&
Polynomial::zero() {
  static Polynomial z;
  return z;
}

inline memory_size_type
Polynomial::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline bool
Polynomial::is_equal_to(const Polynomial& y) const {
  return map == y.map;
}

inline void
Polynomial::primitive_form_assign() {
  normalize();
  sign_normalize();
}

inline bool
Polynomial::check_primitive_form() const {
  Polynomial tmp = *this;
  tmp.primitive_form_assign();
  return is_equal_to(tmp);
}

inline void
Polynomial::linear_combine(const Polynomial& y,
			   const Term& t,
			   Coefficient_traits::const_reference y_t) {
  linear_combine(y, t, coefficient(t), y_t);
}

inline void
Polynomial::linear_combine(const Polynomial& y, const Term& t) {
  linear_combine(y, t, y.coefficient(t));
}

/*! \relates Polynomial */
inline Polynomial
operator+(const Polynomial& p) {
  return p;
}

/*! \relates Polynomial */
inline Polynomial
operator+(const Polynomial& p, Coefficient_traits::const_reference n) {
  return n + p;
}

/*! \relates Polynomial */
inline Polynomial
operator+(const Variable v, const Polynomial& p) {
  return Term(v) + p;
}

/*! \relates Polynomial */
inline Polynomial
operator+(const Polynomial& p, const Variable v) {
  return v + p;
}

/*! \relates Polynomial */
inline Polynomial
operator-(const Polynomial& p, Coefficient_traits::const_reference n) {
  TEMP_INTEGER(m);
  neg_assign(m, n);
  return m + p;
}

/*! \relates Polynomial */
inline Polynomial
operator-(const Variable v, const Polynomial& p) {
  return Term(v) - p;
}

/*! \relates Polynomial */
inline Polynomial
operator-(const Polynomial& p, const Variable v) {
  return p - Term(v);
}

/*! \relates Polynomial */
inline Polynomial
operator*(const Polynomial& p, Coefficient_traits::const_reference n) {
  return n * p;
}

/*! \relates Polynomial */
inline Polynomial
operator*(const Variable v, const Polynomial& p) {
  Polynomial q = p;
  q *= v;
  return q;
}

/*! \relates Polynomial */
inline Polynomial
operator*(const Polynomial& p, const Variable v) {
  return v * p;
}

/*! \relates Polynomial */
inline Polynomial&
operator+=(Polynomial& p, Coefficient_traits::const_reference n) {
  std::pair<Polynomial::Map::iterator, bool>
    r = p.map.insert(Polynomial::Map::value_type(Term::one(), n));
  if (!r.second) {
    const Coefficient& sum = r.first->second += n;
    if (sum == 0)
      p.map.erase(r.first);
  }
  assert(p.OK());
  return p;
}

/*! \relates Polynomial */
inline Polynomial&
operator-=(Polynomial& p, Coefficient_traits::const_reference n) {
  std::pair<Polynomial::Map::iterator, bool>
    r = p.map.insert(Polynomial::Map::value_type(Term::one(), n));
  if (r.second)
    neg_assign(r.first->second);
  else {
    const Coefficient& diff = r.first->second -= n;
    if (diff == 0)
      p.map.erase(r.first);
  }
  assert(p.OK());
  return p;
}

/*! \relates Polynomial */
inline Polynomial
pow(const Polynomial& p, const dimension_type n) {
  Polynomial result(p);
  pow_assign(result, n);
  return result;
}

inline void
Polynomial::swap(Polynomial& y) {
  std::swap(map, y.map);
}

inline bool
Polynomial::less(const Map::value_type& x, const Map::value_type& y) {
  return Parma_Polyhedra_Library::Term::Compare()(x.first, y.first);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Polynomial */
inline void
swap(Parma_Polyhedra_Library::Polynomial& x,
     Parma_Polyhedra_Library::Polynomial& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polynomial_inlines_hh)
