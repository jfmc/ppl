/* ConSys class implementation: inline functions.
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

#ifndef PPL_ConSys_inlines_hh
#define PPL_ConSys_inlines_hh 1

#include "Constraint.defs.hh"

namespace Parma_Polyhedra_Library {

inline
ConSys::ConSys()
  : Linear_System(NECESSARILY_CLOSED) {
}

inline
ConSys::ConSys(const Constraint& c)
  : Linear_System(c.topology()) {
  Linear_System::insert(c);
}

inline
ConSys::ConSys(const ConSys& cs)
  : Linear_System(cs) {
}

inline
ConSys::ConSys(const Topology topol)
  : Linear_System(topol) {
}

inline
ConSys::ConSys(const Topology topol,
	       const dimension_type n_rows, const dimension_type n_columns)
  : Linear_System(topol, n_rows, n_columns) {
}

inline
ConSys::~ConSys() {
}

inline ConSys&
ConSys::operator=(const ConSys& y) {
  Linear_System::operator=(y);
  return *this;
}

inline Constraint&
ConSys::operator[](const dimension_type k) {
  return static_cast<Constraint&>(Linear_System::operator[](k));
}

inline const Constraint&
ConSys::operator[](const dimension_type k) const {
  return static_cast<const Constraint&>(Linear_System::operator[](k));
}

inline dimension_type
ConSys::max_space_dimension() {
  return Linear_System::max_space_dimension();
}

inline dimension_type
ConSys::space_dimension() const {
  return Linear_System::space_dimension();
}

inline void
ConSys::clear() {
  Linear_System::clear();
}

inline const ConSys&
ConSys::zero_dim_empty() {
  static const ConSys zdf(Constraint::zero_dim_false());
  return zdf;
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
  const const_iterator tmp = *this;
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

inline
ConSys::const_iterator::
const_iterator(const Linear_System::const_iterator& iter,
	       const ConSys& csys)
  : i(iter), csp(&csys) {
}

inline ConSys::const_iterator
ConSys::begin() const {
  const_iterator i(Linear_System::begin(), *this);
  i.skip_forward();
  return i;
}

inline ConSys::const_iterator
ConSys::end() const {
  const const_iterator i(Linear_System::end(), *this);
  return i;
}

inline void
ConSys::swap(ConSys& y) {
  Linear_System::swap(y);
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
