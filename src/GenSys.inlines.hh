/* GenSys class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace Parma_Polyhedra_Library {

inline
GenSys::GenSys()
  : Matrix(NECESSARILY_CLOSED) {
}

inline
GenSys::GenSys(const Generator& g)
  : Matrix(g.topology()) {
  Matrix::insert(g);
}

inline
GenSys::GenSys(const GenSys& gs)
  : Matrix(gs) {
}

inline
GenSys::GenSys(Topology topol)
  : Matrix(topol) {
}

inline
GenSys::GenSys(Topology topol, size_t n_rows, size_t n_columns)
  : Matrix(topol, n_rows, n_columns) {
}

inline
GenSys::~GenSys() {
}

inline GenSys&
GenSys::operator=(const GenSys& y) {
  Matrix::operator=(y);
  return *this;
}

inline size_t
GenSys::space_dimension() const {
  return Matrix::space_dimension();
}

inline Generator&
GenSys::operator[](size_t k) {
  return static_cast<Generator&>(Matrix::operator[](k));
}

inline const Generator&
GenSys::operator[](size_t k) const {
  return static_cast<const Generator&>(Matrix::operator[](k));
}

inline
GenSys::const_iterator::const_iterator(const Matrix::const_iterator& iter)
  : i(iter) {
}

inline
GenSys::const_iterator::const_iterator()
  : i() {
}

inline
GenSys::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

inline
GenSys::const_iterator::~const_iterator() {
}

inline
GenSys::const_iterator&
GenSys::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

inline const Generator&
GenSys::const_iterator::operator*() const {
  return static_cast<const Generator&>(*i);
}

inline const Generator*
GenSys::const_iterator::operator->() const {
  return static_cast<const Generator*>(i.operator->());
}

inline GenSys::const_iterator&
GenSys::const_iterator::operator++() {
  ++i;
  return *this;
}

inline GenSys::const_iterator
GenSys::const_iterator::operator++(int) {
  const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline bool
GenSys::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
GenSys::const_iterator::operator!=(const const_iterator& y) const {
  return i != y.i;
}

inline GenSys::const_iterator
GenSys::begin() const {
  return const_iterator(Matrix::begin());
}

inline GenSys::const_iterator
GenSys::end() const {
  return const_iterator(Matrix::end());
}

inline const GenSys&
GenSys::zero_dim_univ() {
  static GenSys zdu(Generator::zero_dim_point());
  return zdu;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*!
  Specializes <CODE>std::swap</CODE> to use the fast swap that is
  provided as a member function instead of using the default
  algorithm (which creates a temporary and uses assignment).
*/
inline void
swap(Parma_Polyhedra_Library::GenSys& x,
     Parma_Polyhedra_Library::GenSys& y) {
  x.swap(y);
}

} // namespace std
