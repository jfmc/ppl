/* ConSys class implementation: inline functions.
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

#ifndef PPL_ConSys_inlines_hh
#define PPL_ConSys_inlines_hh 1

#include "Constraint.defs.hh"

namespace Parma_Polyhedra_Library {

inline
ConSys::ConSys()
  : Matrix(NECESSARILY_CLOSED) {
}

inline
ConSys::ConSys(const Constraint& c)
  : Matrix(c.topology()) {
  Matrix::insert(c);
}

inline
ConSys::ConSys(const ConSys& cs)
  : Matrix(cs) {
}

inline
ConSys::ConSys(Topology topol)
  : Matrix(topol) {
}

inline
ConSys::ConSys(Topology topol,
	       dimension_type n_rows, dimension_type n_columns)
  : Matrix(topol, n_rows, n_columns) {
}

inline
ConSys::~ConSys() {
}

inline ConSys&
ConSys::operator=(const ConSys& y) {
  Matrix::operator=(y);
  return *this;
}

inline Constraint&
ConSys::operator[](dimension_type k) {
  return static_cast<Constraint&> (Matrix::operator[](k));
}

inline const Constraint&
ConSys::operator[](dimension_type k) const {
  return static_cast<const Constraint&> (Matrix::operator[](k));
}

inline dimension_type
ConSys::space_dimension() const {
  return Matrix::space_dimension();
}

inline void
ConSys::clear() {
  Matrix::clear();
}

inline const ConSys&
ConSys::zero_dim_empty() {
  static ConSys zdf(Constraint::zero_dim_false());
  return zdf;
}

inline
ConSys::const_iterator::
const_iterator(const Parma_Polyhedra_Library::Matrix::const_iterator& iter,
	       const ConSys& csys)
  : i(iter), csp(&csys) {
}

inline
ConSys::const_iterator::const_iterator()
  : i(), csp(0) {
}

inline
ConSys::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i), csp(y.csp) {
}

inline
ConSys::const_iterator::~const_iterator() {
}

inline ConSys::const_iterator&
ConSys::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  csp = y.csp;
  return *this;
}

inline const Constraint&
ConSys::const_iterator::operator*() const {
  return static_cast<const Constraint&>(*i);
}

inline const Constraint*
ConSys::const_iterator::operator->() const {
  return static_cast<const Constraint*>(i.operator->());
}

inline ConSys::const_iterator&
ConSys::const_iterator::operator++() {
  ++i;
  skip_forward();
  return *this;
}

inline ConSys::const_iterator
ConSys::const_iterator::operator++(int) {
  const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline bool
ConSys::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
ConSys::const_iterator::operator!=(const const_iterator& y) const {
  return i != y.i;
}

inline ConSys::const_iterator
ConSys::begin() const {
  const_iterator i(Matrix::begin(), *this);
  i.skip_forward();
  return i;
}

inline ConSys::const_iterator
ConSys::end() const {
  const_iterator i(Matrix::end(), *this);
  return i;
}

inline void
ConSys::swap(ConSys& y) {
  Matrix::swap(y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::ConSys */
inline void
swap(Parma_Polyhedra_Library::ConSys& x,
     Parma_Polyhedra_Library::ConSys& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_ConSys_inlines_hh)
