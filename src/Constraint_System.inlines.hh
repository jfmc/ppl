/* Constraint_System class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Constraint_System_inlines_hh
#define PPL_Constraint_System_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Constraint_System.defs.hh"

#include "Constraint.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Constraint_System::Constraint_System()
  : sys(NECESSARILY_CLOSED) {
}

inline
Constraint_System::Constraint_System(const Constraint& c)
  : sys(c.topology()) {
  sys.insert(c);
}

inline
Constraint_System::Constraint_System(const Constraint_System& cs)
  : sys(cs.sys) {
}

inline
Constraint_System::Constraint_System(const Topology topol)
  : sys(topol) {
}

inline
Constraint_System::Constraint_System(const Topology topol,
				     const dimension_type space_dim)
  : sys(topol, space_dim) {
}

inline
Constraint_System::~Constraint_System() {
}

inline Constraint_System&
Constraint_System::operator=(const Constraint_System& y) {
  sys = y.sys;
  return *this;
}

inline const Constraint&
Constraint_System::operator[](const dimension_type k) const {
  return static_cast<const Constraint&>(sys[k]);
}

inline dimension_type
Constraint_System::max_space_dimension() {
  return Linear_System<Constraint>::max_space_dimension();
}

inline dimension_type
Constraint_System::space_dimension() const {
  return sys.space_dimension();
}

inline void
Constraint_System::set_space_dimension(dimension_type space_dim) {
  return sys.set_space_dimension(space_dim);
}

inline void
Constraint_System::clear() {
  sys.clear();
}

inline const Constraint_System&
Constraint_System::zero_dim_empty() {
  PPL_ASSERT(zero_dim_empty_p != 0);
  return *zero_dim_empty_p;
}

inline
Constraint_System_const_iterator::Constraint_System_const_iterator()
  : i(), csp(0) {
}

inline
Constraint_System_const_iterator::Constraint_System_const_iterator(const Constraint_System_const_iterator& y)
  : i(y.i), csp(y.csp) {
}

inline
Constraint_System_const_iterator::~Constraint_System_const_iterator() {
}

inline Constraint_System_const_iterator&
Constraint_System_const_iterator::operator=(const Constraint_System_const_iterator& y) {
  i = y.i;
  csp = y.csp;
  return *this;
}

inline const Constraint&
Constraint_System_const_iterator::operator*() const {
  return static_cast<const Constraint&>(*i);
}

inline const Constraint*
Constraint_System_const_iterator::operator->() const {
  return static_cast<const Constraint*>(i.operator->());
}

inline Constraint_System_const_iterator&
Constraint_System_const_iterator::operator++() {
  ++i;
  skip_forward();
  return *this;
}

inline Constraint_System_const_iterator
Constraint_System_const_iterator::operator++(int) {
  const Constraint_System_const_iterator tmp = *this;
  operator++();
  return tmp;
}

inline bool
Constraint_System_const_iterator::operator==(const Constraint_System_const_iterator& y) const {
  return i == y.i;
}

inline bool
Constraint_System_const_iterator::operator!=(const Constraint_System_const_iterator& y) const {
  return i != y.i;
}

inline
Constraint_System_const_iterator::
Constraint_System_const_iterator(const Linear_System<Constraint>::const_iterator& iter,
	       const Constraint_System& csys)
  : i(iter), csp(&csys.sys) {
}

inline Constraint_System_const_iterator
Constraint_System::begin() const {
  const_iterator i(sys.begin(), *this);
  i.skip_forward();
  return i;
}

inline Constraint_System_const_iterator
Constraint_System::end() const {
  const Constraint_System_const_iterator i(sys.end(), *this);
  return i;
}

inline bool
Constraint_System::empty() const {
  return begin() == end();
}

inline void
Constraint_System::add_low_level_constraints() {
  if (sys.is_necessarily_closed())
    // The positivity constraint.
    insert(Constraint::zero_dim_positivity());
  else {
    // Add the epsilon constraints.
    insert(Constraint::epsilon_leq_one());
    insert(Constraint::epsilon_geq_zero());
  }
}

inline void
Constraint_System::swap(Constraint_System& y) {
  std::swap(sys, y.sys);
}

inline memory_size_type
Constraint_System::external_memory_in_bytes() const {
  return sys.external_memory_in_bytes();
}

inline memory_size_type
Constraint_System::total_memory_in_bytes() const {
  return external_memory_in_bytes() + sizeof(*this);
}

inline void
Constraint_System::simplify() {
  sys.simplify();
}

inline Topology
Constraint_System::topology() const {
  return sys.topology();
}

inline dimension_type
Constraint_System::num_rows() const {
  return sys.num_rows();
}

inline void
Constraint_System::swap_rows(dimension_type i, dimension_type j) {
  sys.swap_rows(i, j);
}

inline bool
Constraint_System::is_necessarily_closed() const {
  return sys.is_necessarily_closed();
}

inline dimension_type
Constraint_System::num_pending_rows() const {
  return sys.num_pending_rows();
}

inline dimension_type
Constraint_System::first_pending_row() const {
  return sys.first_pending_row();
}

inline bool
Constraint_System::is_sorted() const {
  return sys.is_sorted();
}

inline void
Constraint_System::unset_pending_rows() {
  sys.unset_pending_rows();
}

inline void
Constraint_System::set_index_first_pending_row(dimension_type i) {
  sys.set_index_first_pending_row(i);
}

inline void
Constraint_System::set_sorted(bool b) {
  sys.set_sorted(b);
}

inline void
Constraint_System::remove_trailing_rows(dimension_type n) {
  sys.remove_trailing_rows(n);
}

inline void
Constraint_System
::remove_space_dimensions(const Variables_Set& vars) {
  sys.remove_space_dimensions(vars);
}

inline void
Constraint_System
::permute_space_dimensions(const std::vector<Variable>& cycle) {
  sys.permute_space_dimensions(cycle);
}

inline void
Constraint_System
::swap_space_dimensions(Variable v1, Variable v2) {
  sys.swap_space_dimensions(v1, v2);
}

inline bool
Constraint_System::has_no_rows() const {
  return sys.has_no_rows();
}

inline void
Constraint_System::strong_normalize() {
  sys.strong_normalize();
}

inline void
Constraint_System::sort_rows() {
  sys.sort_rows();
}

inline void
Constraint_System::insert_pending_recycled(Constraint& r) {
  sys.insert_pending_recycled(r);
}

inline void
Constraint_System::insert_pending_recycled(Constraint_System& r) {
  sys.insert_pending_recycled(r.sys);
}

inline void
Constraint_System::insert_recycled(Constraint& r) {
  sys.insert_recycled(r);
}

inline void
Constraint_System::insert_recycled(Constraint_System& r) {
  sys.insert_recycled(r.sys);
}

inline void
Constraint_System::insert_pending(const Constraint_System& r) {
  sys.insert_pending(r.sys);
}

inline void
Constraint_System::merge_rows_assign(const Constraint_System& y) {
  sys.merge_rows_assign(y.sys);
}

inline void
Constraint_System::insert(const Constraint_System& y) {
  sys.insert(y.sys);
}

inline void
Constraint_System::mark_as_necessarily_closed() {
  sys.mark_as_necessarily_closed();
}

inline void
Constraint_System::mark_as_not_necessarily_closed() {
  sys.mark_as_not_necessarily_closed();
}

inline void
Constraint_System::release_row(Constraint& row) {
  sys.release_row(row);
}

inline void
Constraint_System::release_rows(Swapping_Vector<Constraint>& v) {
  sys.release_rows(v);
}

inline void
Constraint_System::take_ownership_of_rows(Swapping_Vector<Constraint>& v) {
  sys.take_ownership_of_rows(v);
}

inline void
Constraint_System::resize_no_copy(dimension_type new_n_rows,
                                  dimension_type new_space_dim) {
  sys.resize_no_copy(new_n_rows, new_space_dim);
}

inline dimension_type
Constraint_System::gauss(dimension_type n_lines_or_equalities) {
  return sys.gauss(n_lines_or_equalities);
}

inline void
Constraint_System::back_substitute(dimension_type n_lines_or_equalities) {
  sys.back_substitute(n_lines_or_equalities);
}

inline void
Constraint_System::assign_with_pending(const Constraint_System& y) {
  sys.assign_with_pending(y.sys);
}

inline void
Constraint_System::sort_pending_and_remove_duplicates() {
  sys.sort_pending_and_remove_duplicates();
}

inline void
Constraint_System::sort_and_remove_with_sat(Bit_Matrix& sat) {
  sys.sort_and_remove_with_sat(sat);
}

inline bool
Constraint_System::check_sorted() const {
  return sys.check_sorted();
}

inline dimension_type
Constraint_System::num_lines_or_equalities() const {
  return sys.num_lines_or_equalities();
}

inline void
Constraint_System::add_universe_rows_and_columns(dimension_type n) {
  sys.add_universe_rows_and_columns(n);
}

inline bool
operator==(const Constraint_System& x, const Constraint_System& y) {
  return x.sys == y.sys;
}

inline bool
operator!=(const Constraint_System& x, const Constraint_System& y) {
  return !(x == y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Constraint_System */
inline void
swap(Parma_Polyhedra_Library::Constraint_System& x,
     Parma_Polyhedra_Library::Constraint_System& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Constraint_System_inlines_hh)
