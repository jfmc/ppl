/* ConSys class implementation: inline functions.
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

#include "Constraint.defs.hh"

INLINE
Parma_Polyhedra_Library::ConSys::ConSys()
  : Matrix() {
}

INLINE
Parma_Polyhedra_Library::ConSys::ConSys(const ConSys& cs)
  : Matrix(cs) {
}

INLINE
Parma_Polyhedra_Library::ConSys::ConSys(size_t num_rows, size_t num_columns)
  : Matrix(num_rows, num_columns) {
}

INLINE
Parma_Polyhedra_Library::ConSys::~ConSys() {
}

INLINE Parma_Polyhedra_Library::Constraint&
Parma_Polyhedra_Library::ConSys::operator [](size_t k) {
  return
    static_cast<Parma_Polyhedra_Library::Constraint&>
    (Matrix::operator[](k));
}

INLINE const Parma_Polyhedra_Library::Constraint&
Parma_Polyhedra_Library::ConSys::operator [](size_t k) const {
  return
    static_cast<const Parma_Polyhedra_Library::Constraint&>
    (Matrix::operator[](k));
}

INLINE void
Parma_Polyhedra_Library::ConSys::insert(const Constraint& c) {
  Matrix::insert(c);
}

INLINE void
Parma_Polyhedra_Library::ConSys::swap(ConSys& y) {
  Matrix::swap(y);
}

INLINE
Parma_Polyhedra_Library::
ConSys::const_iterator::const_iterator(const Matrix::const_iterator& iter,
				       const ConSys& csys)
  : i(iter), csp(&csys) {
}

INLINE
Parma_Polyhedra_Library::ConSys::const_iterator::const_iterator()
  : i(), csp(0) {
}

INLINE
Parma_Polyhedra_Library::
ConSys::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i), csp(y.csp) {
}

INLINE
Parma_Polyhedra_Library::
ConSys::const_iterator::~const_iterator() {
}

INLINE
Parma_Polyhedra_Library::ConSys::const_iterator&
Parma_Polyhedra_Library::
ConSys::const_iterator::operator =(const const_iterator& y) {
  i = y.i;
  csp = y.csp;
  return *this;
}

INLINE const Parma_Polyhedra_Library::Constraint&
Parma_Polyhedra_Library::ConSys::const_iterator::operator *() const {
  return static_cast<const Constraint&>(*i);
}

INLINE const Parma_Polyhedra_Library::Constraint*
Parma_Polyhedra_Library::ConSys::const_iterator::operator ->() const {
  return static_cast<const Constraint*>(i.operator ->());
}

INLINE Parma_Polyhedra_Library::ConSys::const_iterator&
Parma_Polyhedra_Library::ConSys::const_iterator::operator ++() {
  ++i;
  skip_forward();
  return *this;
}

INLINE Parma_Polyhedra_Library::ConSys::const_iterator
Parma_Polyhedra_Library::ConSys::const_iterator::operator ++(int) {
  const_iterator tmp = *this;
  operator ++();
  return tmp;
}

INLINE bool
Parma_Polyhedra_Library::ConSys::
const_iterator::operator ==(const const_iterator& y) const {
  return i == y.i;
}

INLINE bool
Parma_Polyhedra_Library::ConSys::
const_iterator::operator !=(const const_iterator& y) const {
  return i != y.i;
}

INLINE Parma_Polyhedra_Library::ConSys::const_iterator
Parma_Polyhedra_Library::ConSys::begin() const {
  const_iterator i(Matrix::begin(), *this);
  i.skip_forward();
  return i;
}

INLINE Parma_Polyhedra_Library::ConSys::const_iterator
Parma_Polyhedra_Library::ConSys::end() const {
  const_iterator i(Matrix::end(), *this);
  return i;
}








