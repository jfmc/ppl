/* Monomial class implementation: inline functions.
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

#ifndef PPL_Monomial_inlines_hh
#define PPL_Monomial_inlines_hh 1

//#include "Variable.defs.hh"
#include <stdexcept>
#include <limits>

namespace Parma_Polyhedra_Library {

inline const Term&
Monomial::term() const {
  return first;
}

inline const Coefficient&
Monomial::coefficient() const {
  return second;
}

inline Term&
Monomial::term_ref() {
  return const_cast<Term&>(first);
}

inline Coefficient&
Monomial::coefficient_ref() {
  return second;
}

inline dimension_type
Monomial::space_dimension() const {
  if (coefficient() == 0)
    return 0;
  else
    return term().space_dimension();
}

inline dimension_type
Monomial::max_space_dimension() {
  return Term::max_space_dimension();
}

inline dimension_type
Monomial::max_exponent() {
  return Term::max_exponent();
}

inline
Monomial::Monomial()
  : Base(Term::one(), Coefficient_zero()) {
}

inline
Monomial::Monomial(const Monomial& e)
  : Base(e) {
}

inline
Monomial::Monomial(Coefficient_traits::const_reference n)
  : Base(Term::one(), n) {
}

inline
Monomial::Monomial(const Term& t, Coefficient_traits::const_reference n)
  : Base((n == 0 ? Term::one() : t), n) {
}

inline
Monomial::Monomial(Coefficient_traits::const_reference n, const Term& t)
  : Base((n == 0 ? Term::one() : t), n) {
}

inline
Monomial::Monomial(const Term& t)
  : Base(t, Coefficient_one()) {
}

inline
Monomial::Monomial(const Variable v)
  : Base(v, Coefficient_one()) {
}

inline
Monomial::~Monomial() {
}

inline exponent_type
Monomial::exponent(const Variable v) const {
  return term().exponent(v);
}

inline degree_type
Monomial::degree() const {
  return term().degree();
}

inline degree_type
Monomial::degree(const Variable v) const {
  return term().degree(v);
}

inline const Monomial&
Monomial::zero() {
  static Monomial z(Term::one(), Coefficient_zero());
  return z;
}

inline const Monomial&
Monomial::one() {
  static Monomial o(Term::one(), Coefficient_one());
  return o;
}

inline memory_size_type
Monomial::external_memory_in_bytes() const {
  return term().external_memory_in_bytes()
    + Parma_Polyhedra_Library::external_memory_in_bytes(coefficient());
}

inline memory_size_type
Monomial::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

/*! \relates Monomial */
inline Monomial
operator*(const Monomial& m, const Term& t) {
  Monomial new_m(m.coefficient() == 0 ? Term::one() : m.term() * t,
		 m.coefficient());
  return new_m;
}

/*! \relates Monomial */
inline Monomial
operator*(const Term& t, const Monomial& m) {
  return m * t;
}

/*! \relates Monomial */
inline Monomial
operator*(const Monomial& m1, const Monomial& m2) {
  Monomial m(m1.coefficient() == 0 || m2.coefficient() == 0
	      ? Term::one()
	      : m1.term() * m2.term(),
	     m1.coefficient() * m2.coefficient());
  return m;
}

/*! \relates Monomial */
inline Monomial&
operator*=(Monomial& m, const Term& t) {
  if (m.coefficient() != 0)
    m.term_ref() *= t;
  return m;
}

/*! \relates Monomial */
inline Monomial&
operator*=(Monomial& m1, const Monomial& m2) {
  if (m1.coefficient() != 0) {
    if (m2.coefficient() != 0) {
      m1.term_ref() *= m2.term();
      m1.coefficient_ref() *= m2.coefficient();
    }
    else {
      Monomial z;
      m1.swap(z);
    }
  }
  return m1;
}

/*! \relates Monomial */
inline Monomial&
operator*=(Monomial& m, const Variable v) {
  if (m.coefficient() != 0)
    m.term_ref() *= v;
  return m;
}

/*! \relates Monomial */
inline Monomial
operator*(const Monomial& m, const Variable v) {
  Monomial new_m(m);
  new_m *= v;
  return new_m;
}

/*! \relates Monomial */
inline Monomial
operator*(Variable v, const Monomial& m) {
  return m * v;
}

/*! \relates Monomial */
inline Monomial&
operator*=(Monomial& m, Coefficient_traits::const_reference n) {
  if (n == 0) {
    Monomial z;
    m.swap(z);
  }
  else
    m.coefficient_ref() *= n;
  return m;
}

/*! \relates Monomial */
inline Monomial
operator*(const Monomial& m, Coefficient_traits::const_reference n) {
  Monomial new_m(m);
  new_m *= n;
  return new_m;
}

/*! \relates Monomial */
inline Monomial
operator*(Coefficient_traits::const_reference n, const Monomial& m) {
  return m * n;
}

/*! \relates Monomial */
inline Monomial
pow(const Monomial& m, const dimension_type n) {
  Monomial new_m(m);
  pow_assign(new_m, n);
  return new_m;
}

/*! \relates Monomial */
inline Monomial&
exact_div_assign(Monomial& m, Coefficient_traits::const_reference n) {
  Coefficient& m_coeff = m.coefficient_ref();
  exact_div_assign(m_coeff, m_coeff, n);
  return m;
}

/*! \relates Monomial */
inline Monomial&
exact_div_assign(Monomial& m, Variable v) {
  if (m.coefficient() != 0)
    exact_div_assign(m.term_ref(), v);
  return m;
}

/*! \relates Monomial */
inline Monomial&
exact_div_assign(Monomial& m, const Term& t) {
  if (m.coefficient() != 0)
    exact_div_assign(m.term_ref(), t);
  return m;
}

/*! \relates Monomial */
inline Monomial&
exact_div_assign(Monomial& m1, const Monomial& m2) {
  if (m1.coefficient() != 0) {
    exact_div_assign(m1, m2.coefficient());
    exact_div_assign(m1, m2.term());
  }
  return m1;
}

/*! \relates Monomial */
inline bool
operator==(const Monomial& x, const Monomial& y) {
  return x.coefficient() == y.coefficient() && x.term() == y.term();
}

/*! \relates Monomial */
inline bool
operator!=(const Monomial& x, const Monomial& y) {
  return !(x == y);
}

inline void
Monomial::swap(Monomial& y) {
  std::swap(term_ref(), y.term_ref());
  std::swap(coefficient_ref(), y.coefficient_ref());
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Monomial */
inline void
swap(Parma_Polyhedra_Library::Monomial& x,
     Parma_Polyhedra_Library::Monomial& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Monomial_inlines_hh)
