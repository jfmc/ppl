/* GenSys class implementation: inline functions.
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

#ifndef PPL_GenSys_inlines_hh
#define PPL_GenSys_inlines_hh 1

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
GenSys::GenSys(const Topology topol)
  : Matrix(topol) {
}

inline
GenSys::GenSys(const Topology topol,
	       const dimension_type n_rows, const dimension_type n_columns)
  : Matrix(topol, n_rows, n_columns) {
}

inline
GenSys::GenSys(GenSys& y, const dimension_type first_stolen)
  : Matrix(y, first_stolen) {
}

inline
GenSys::~GenSys() {
}

inline GenSys&
GenSys::operator=(const GenSys& y) {
  Matrix::operator=(y);
  return *this;
}

inline dimension_type
GenSys::space_dimension() const {
  return Matrix::space_dimension();
}

inline void
GenSys::clear() {
  Matrix::clear();
}

inline Generator&
GenSys::operator[](const dimension_type k) {
  return static_cast<Generator&>(Matrix::operator[](k));
}

inline const Generator&
GenSys::operator[](const dimension_type k) const {
  return static_cast<const Generator&>(Matrix::operator[](k));
}

inline
GenSys::const_iterator::const_iterator(const Matrix::const_iterator& iter,
				       const GenSys& gsys)
  : i(iter), gsp(&gsys) {
}

inline
GenSys::const_iterator::const_iterator()
  : i(), gsp(0) {
}

inline
GenSys::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i), gsp(y.gsp) {
}

inline
GenSys::const_iterator::~const_iterator() {
}

inline
GenSys::const_iterator&
GenSys::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  gsp = y.gsp;
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
  if (!gsp->is_necessarily_closed())
    skip_forward();
  return *this;
}

inline GenSys::const_iterator
GenSys::const_iterator::operator++(int) {
  const const_iterator tmp = *this;
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
  const_iterator i(Matrix::begin(), *this);
  if (!is_necessarily_closed())
    i.skip_forward();
  return i;
}

inline GenSys::const_iterator
GenSys::end() const {
  const const_iterator i(Matrix::end(), *this);
  return i;
}

inline const GenSys&
GenSys::zero_dim_univ() {
  static const GenSys zdu(Generator::zero_dim_point());
  return zdu;
}

inline void
GenSys::swap(GenSys& y) {
  Matrix::swap(y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::ConSys */
inline void
swap(Parma_Polyhedra_Library::GenSys& x,
     Parma_Polyhedra_Library::GenSys& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_GenSys_inlines_hh)
