/* LinExpression class implementation: inline functions.
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

#ifndef PPL_LinExpression_inlines_hh
#define PPL_LinExpression_inlines_hh 1

#include "Variable.defs.hh"
#include "Integer.defs.hh"

namespace Parma_Polyhedra_Library {

inline
LinExpression::LinExpression()
  : Row(Row::Type(), 1) {
}

inline
LinExpression::LinExpression(dimension_type sz, bool)
  : Row(Row::Type(), sz) {
}

inline
LinExpression::LinExpression(const Variable v)
  : Row(Row::Type(), v.id() + 2) {
  (*this)[v.id() + 1] = 1;
}

inline
LinExpression::LinExpression(const LinExpression& e)
  : Row(e) {
}

inline
LinExpression::~LinExpression() {
}

inline
LinExpression::LinExpression(const LinExpression& e, dimension_type sz)
  : Row(e, sz, sz) {
}

inline
LinExpression::LinExpression(const Integer& n)
  : Row(Row::Type(), 1) {
  (*this)[0] = n;
}

inline dimension_type
LinExpression::space_dimension() const {
  return size() - 1;
}

inline const Integer&
LinExpression::coefficient(Variable v) const {
  if (v.id() >= space_dimension())
    return Integer_zero();
  return Row::coefficient(v.id());
}

inline const Integer&
LinExpression::inhomogeneous_term() const {
  return Row::inhomogeneous_term();
}

inline const LinExpression&
LinExpression::zero() {
  static LinExpression z = LinExpression(Integer_zero());
  return z;
}

/*! \relates LinExpression */
inline LinExpression
operator+(const LinExpression& e) {
  return e;
}

/*! \relates LinExpression */
inline LinExpression
operator+(const LinExpression& e, const Integer& n) {
  return n + e;
}

/*! \relates LinExpression */
inline LinExpression
operator-(const LinExpression& e, const Integer& n) {
  return -n + e;
}

/*! \relates LinExpression */
inline LinExpression
operator*(const LinExpression& e, const Integer& n) {
  return n * e;
}

/*! \relates LinExpression */
inline LinExpression&
operator+=(LinExpression& e, const Integer& n) {
  e[0] += n;
  return e;
}

/*! \relates LinExpression */
inline LinExpression&
operator-=(LinExpression& e, const Integer& n) {
  e[0] -= n;
  return e;
}

inline void
LinExpression::swap(LinExpression& y) {
  Row::swap(y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::LinExpression */
inline void
swap(Parma_Polyhedra_Library::LinExpression& x,
     Parma_Polyhedra_Library::LinExpression& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_LinExpression_inlines_hh)
