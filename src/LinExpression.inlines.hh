/* LinExpression class implementation: inline functions.
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


#include "Variable.defs.hh"
#include <cassert>
#include "Integer.defs.hh"

namespace Parma_Polyhedra_Library {

inline
LinExpression::LinExpression()
  : Row(Row::LINE_OR_EQUALITY, 1) {
}

inline
LinExpression::LinExpression(size_t size, bool)
  : Row(Row::LINE_OR_EQUALITY, size) {
}

inline
LinExpression::LinExpression(const Variable& v)
  : Row(Row::LINE_OR_EQUALITY, v.id() + 2) {
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
LinExpression::LinExpression(const LinExpression& e, size_t size)
  : Row(e, size, size) {
}

inline
LinExpression::LinExpression(const Integer& n)
  : Row(Row::LINE_OR_EQUALITY, 1) {
  (*this)[0] = n;
}

inline size_t
LinExpression::space_dimension() const {
  return size() - 1;
}

inline const LinExpression&
LinExpression::zero() {
  static LinExpression z = LinExpression(Integer_zero());
  return z;
}

inline LinExpression
operator +(const LinExpression& e, const Integer& n) {
  return n + e;
}

inline LinExpression
operator -(const LinExpression& e, const Integer& n) {
  return -n + e;
}

inline LinExpression
operator *(const LinExpression& e, const Integer& n) {
  return n * e;
}

inline LinExpression&
operator +=(LinExpression& e, const Integer& n) {
  e[0] += n;
  return e;
}

} // namespace Parma_Polyhedra_Library












