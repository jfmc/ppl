/* Polynomial_Constraint_System class implementation: inline functions.
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

#ifndef PPL_Polynomial_Constraint_System_inlines_hh
#define PPL_Polynomial_Constraint_System_inlines_hh 1

#include "Polynomial_Constraint.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Polynomial_Constraint_System::Polynomial_Constraint_System()
  : vec() {
}

inline
Polynomial_Constraint_System
::Polynomial_Constraint_System(const Polynomial_Constraint& pc)
  : vec() {
  vec.push_back(pc);
}

inline
Polynomial_Constraint_System
::Polynomial_Constraint_System(const Polynomial_Constraint_System& y)
  : vec(y.vec) {
}

inline
Polynomial_Constraint_System::~Polynomial_Constraint_System() {
}

inline Polynomial_Constraint_System&
Polynomial_Constraint_System
::operator=(const Polynomial_Constraint_System& y) {
  vec = y.vec;
  return *this;
}

inline void
Polynomial_Constraint_System::insert(const Polynomial_Constraint& pc) {
  vec.push_back(pc);
  assert(OK());
}

inline dimension_type
Polynomial_Constraint_System::max_space_dimension() {
  return Polynomial_Constraint::max_space_dimension();
}

inline dimension_type
Polynomial_Constraint_System::num_constraints() const {
  return vec.size();
}

inline dimension_type
Polynomial_Constraint_System::num_equalities() const {
  return num_constraints() - num_inequalities();
}

inline void
Polynomial_Constraint_System::clear() {
  vec.clear();
}

inline const Polynomial_Constraint_System&
Polynomial_Constraint_System::zero_dim_empty() {
  static const Polynomial_Constraint_System
    zdf(Polynomial_Constraint::zero_dim_false());
  return zdf;
}

inline
Polynomial_Constraint_System::const_iterator::const_iterator()
  : i() {
}

inline
Polynomial_Constraint_System
::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

inline
Polynomial_Constraint_System::const_iterator::~const_iterator() {
}

inline Polynomial_Constraint_System::const_iterator&
Polynomial_Constraint_System
::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

inline const Polynomial_Constraint&
Polynomial_Constraint_System::const_iterator::operator*() const {
  return *i;
}

inline const Polynomial_Constraint*
Polynomial_Constraint_System::const_iterator::operator->() const {
  return static_cast<const Polynomial_Constraint*>(i.operator->());
}

inline Polynomial_Constraint_System::const_iterator&
Polynomial_Constraint_System::const_iterator::operator++() {
  ++i;
  return *this;
}

inline Polynomial_Constraint_System::const_iterator
Polynomial_Constraint_System::const_iterator::operator++(int) {
  const const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline bool
Polynomial_Constraint_System
::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

inline bool
Polynomial_Constraint_System
::const_iterator::operator!=(const const_iterator& y) const {
  return i != y.i;
}

inline
Polynomial_Constraint_System::const_iterator::
const_iterator(const Vec::const_iterator& iter)
  : i(iter) {
}

inline Polynomial_Constraint_System::const_iterator
Polynomial_Constraint_System::begin() const {
  return vec.begin();
}

inline Polynomial_Constraint_System::const_iterator
Polynomial_Constraint_System::end() const {
  return vec.end();
}

inline void
Polynomial_Constraint_System::swap(Polynomial_Constraint_System& y) {
  std::swap(vec, y.vec);
}

inline memory_size_type
Polynomial_Constraint_System::external_memory_in_bytes() const {
  // FIXME!!!
  abort();
  return 0;
}

inline memory_size_type
Polynomial_Constraint_System::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint_System */
inline void
swap(Parma_Polyhedra_Library::Polynomial_Constraint_System& x,
     Parma_Polyhedra_Library::Polynomial_Constraint_System& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polynomial_Constraint_System_inlines_hh)
