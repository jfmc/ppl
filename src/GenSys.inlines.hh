/* GenSys class implementation: inline functions.
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

#include "Generator.defs.hh"

INLINE
Parma_Polyhedra_Library::GenSys::GenSys()
  : Matrix() {
}

INLINE
Parma_Polyhedra_Library::GenSys::GenSys(const GenSys& gs)
  : Matrix(gs) {
}

INLINE
Parma_Polyhedra_Library::GenSys::GenSys(size_t num_rows, size_t num_columns)
  : Matrix(num_rows, num_columns) {
}

INLINE
Parma_Polyhedra_Library::GenSys::~GenSys() {
}

INLINE void
Parma_Polyhedra_Library::GenSys::insert(const Generator& g) {
  Matrix::insert(g);
}

INLINE void
Parma_Polyhedra_Library::GenSys::swap(GenSys& y) {
  Matrix::swap(y);
}

INLINE void
std::swap(Parma_Polyhedra_Library::GenSys& x,
	  Parma_Polyhedra_Library::GenSys& y) {
  x.swap(y);
}

INLINE Parma_Polyhedra_Library::Generator&
Parma_Polyhedra_Library::GenSys::operator [](size_t k) {
  return static_cast<Generator&>(Matrix::operator[](k));
}

INLINE const Parma_Polyhedra_Library::Generator&
Parma_Polyhedra_Library::GenSys::operator [](size_t k) const {
  return static_cast<const Generator&>(Matrix::operator[](k));
}

INLINE
Parma_Polyhedra_Library::
GenSys::const_iterator::const_iterator(const Matrix::const_iterator& iter)
  : i(iter) {
}

INLINE
Parma_Polyhedra_Library::GenSys::const_iterator::const_iterator()
  : i() {
}

INLINE
Parma_Polyhedra_Library::
GenSys::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

INLINE
Parma_Polyhedra_Library::
GenSys::const_iterator::~const_iterator() {
}

INLINE
Parma_Polyhedra_Library::GenSys::const_iterator&
Parma_Polyhedra_Library::
GenSys::const_iterator::operator =(const const_iterator& y) {
  i = y.i;
  return *this;
}

INLINE const Parma_Polyhedra_Library::Generator&
Parma_Polyhedra_Library::GenSys::const_iterator::operator *() const {
  return static_cast<const Generator&>(*i);
}

INLINE const Parma_Polyhedra_Library::Generator*
Parma_Polyhedra_Library::GenSys::const_iterator::operator ->() const {
  return static_cast<const Generator*>(i.operator ->());
}

INLINE Parma_Polyhedra_Library::GenSys::const_iterator&
Parma_Polyhedra_Library::GenSys::const_iterator::operator ++() {
  ++i;
  return *this;
}

INLINE Parma_Polyhedra_Library::GenSys::const_iterator
Parma_Polyhedra_Library::GenSys::const_iterator::operator ++(int) {
  const_iterator tmp = *this;
  operator ++();
  return tmp;
}

INLINE bool
Parma_Polyhedra_Library::GenSys::
const_iterator::operator ==(const const_iterator& y) const {
  return i == y.i;
}

INLINE bool
Parma_Polyhedra_Library::GenSys::
const_iterator::operator !=(const const_iterator& y) const {
  return i != y.i;
}

INLINE Parma_Polyhedra_Library::GenSys::const_iterator
Parma_Polyhedra_Library::GenSys::begin() const {
  return const_iterator(Matrix::begin());
}

INLINE Parma_Polyhedra_Library::GenSys::const_iterator
Parma_Polyhedra_Library::GenSys::end() const {
  return const_iterator(Matrix::end());
}
