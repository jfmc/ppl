/* COW_Pointset class implementation: inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_COW_Pointset_inlines_hh
#define PPL_COW_Pointset_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PSET>
inline
COW_Pointset<PSET>::Rep::Rep(dimension_type num_dimensions,
			  Degenerate_Element kind)
  : references(0), ph(num_dimensions, kind) {
}

template <typename PSET>
inline
COW_Pointset<PSET>::Rep::Rep(const PSET& p)
  : references(0), ph(p) {
}

template <typename PSET>
inline
COW_Pointset<PSET>::Rep::Rep(const Constraint_System& cs)
  : references(0), ph(cs) {
}

template <typename PSET>
inline
COW_Pointset<PSET>::Rep::Rep(const Congruence_System& cgs)
  : references(0), ph(cgs) {
}

template <typename PSET>
inline
COW_Pointset<PSET>::Rep::~Rep() {
  assert(references == 0);
}

template <typename PSET>
inline void
COW_Pointset<PSET>::Rep::new_reference() const {
  ++references;
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::Rep::del_reference() const {
  return --references == 0;
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::Rep::is_shared() const {
  return references > 1;
}

template <typename PSET>
inline memory_size_type
COW_Pointset<PSET>::Rep::external_memory_in_bytes() const {
  return ph.external_memory_in_bytes();
}

template <typename PSET>
inline memory_size_type
COW_Pointset<PSET>::Rep::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename PSET>
inline
COW_Pointset<PSET>::COW_Pointset(const PSET& ph)
  : prep(new Rep(ph)) {
  prep->new_reference();
}

template <typename PSET>
inline
COW_Pointset<PSET>::COW_Pointset(const Constraint_System& cs)
  : prep(new Rep(cs)) {
  prep->new_reference();
}

template <typename PSET>
inline
COW_Pointset<PSET>::COW_Pointset(const Congruence_System& cgs)
  : prep(new Rep(cgs)) {
  prep->new_reference();
}

template <typename PSET>
inline
COW_Pointset<PSET>::COW_Pointset(const COW_Pointset& y)
  : prep(y.prep) {
  prep->new_reference();
}

template <typename PSET>
inline
COW_Pointset<PSET>::~COW_Pointset() {
  if (prep->del_reference())
    delete prep;
}

template <typename PSET>
inline COW_Pointset<PSET>&
COW_Pointset<PSET>::operator=(const COW_Pointset& y) {
  y.prep->new_reference();
  if (prep->del_reference())
    delete prep;
  prep = y.prep;
  return *this;
}

template <typename PSET>
inline void
COW_Pointset<PSET>::swap(COW_Pointset& y) {
  std::swap(prep, y.prep);
}

template <typename PSET>
inline void
COW_Pointset<PSET>::mutate() {
  if (prep->is_shared()) {
    Rep* new_prep = new Rep(prep->ph);
    (void) prep->del_reference();
    new_prep->new_reference();
    prep = new_prep;
  }
}

template <typename PSET>
inline const PSET&
COW_Pointset<PSET>::element() const {
  return prep->ph;
}

template <typename PSET>
inline PSET&
COW_Pointset<PSET>::element() {
  mutate();
  return prep->ph;
}

template <typename PSET>
inline void
COW_Pointset<PSET>::upper_bound_assign(const COW_Pointset& y) {
  element().upper_bound_assign(y.element());
}

template <typename PSET>
inline void
COW_Pointset<PSET>::meet_assign(const COW_Pointset& y) {
  element().intersection_assign(y.element());
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::has_nontrivial_weakening() {
  // FIXME: the following should be turned into a query to PSET.  This
  // can be postponed until the time the ask-and-tell construction is
  // revived.
  return false;
}

template <typename PSET>
inline void
COW_Pointset<PSET>::weakening_assign(const COW_Pointset& y) {
  // FIXME: the following should be turned into a proper
  // implementation.  This can be postponed until the time the
  // ask-and-tell construction is revived.
  element().difference_assign(y.element());
}

template <typename PSET>
inline void
COW_Pointset<PSET>::concatenate_assign(const COW_Pointset& y) {
  element().concatenate_assign(y.element());
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::definitely_entails(const COW_Pointset& y) const {
  return prep == y.prep || y.prep->ph.contains(prep->ph);
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::is_definitely_equivalent_to(const COW_Pointset& y) const {
  return prep == y.prep || prep->ph == y.prep->ph;
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::is_top() const {
  return prep->ph.is_universe();
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::is_bottom() const {
  return prep->ph.is_empty();
}

template <typename PSET>
inline memory_size_type
COW_Pointset<PSET>::external_memory_in_bytes() const {
  return prep->total_memory_in_bytes();
}

template <typename PSET>
inline memory_size_type
COW_Pointset<PSET>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename PSET>
inline bool
COW_Pointset<PSET>::OK() const {
  return prep->ph.OK();
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::COW_Pointset */
template <typename PSET>
inline std::ostream&
operator<<(std::ostream& s, const COW_Pointset<PSET>& x) {
  s << x.element();
  return s;
}

} // namespace IO_Operators

/*! \relates COW_Pointset */
template <typename PSET>
inline bool
operator==(const COW_Pointset<PSET>& x, const COW_Pointset<PSET>& y) {
  return x.prep == y.prep || x.prep->ph == y.prep->ph;
}

/*! \relates COW_Pointset */
template <typename PSET>
inline bool
operator!=(const COW_Pointset<PSET>& x, const COW_Pointset<PSET>& y) {
  return x.prep != y.prep && x.prep->ph != y.prep->ph;
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline
COW_Pointset<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>::
Binary_Operator_Assign_Lifter(Binary_Operator_Assign op_assign)
  : op_assign_(op_assign) {
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline void
COW_Pointset<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>::
operator()(COW_Pointset& x, const COW_Pointset& y) const {
  op_assign_(x.element(), y.element());
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline
COW_Pointset<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>
COW_Pointset<PSET>::lift_op_assign(Binary_Operator_Assign op_assign) {
  return Binary_Operator_Assign_Lifter<Binary_Operator_Assign>(op_assign);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::COW_Pointset */
template <typename PSET>
inline void
swap(Parma_Polyhedra_Library::COW_Pointset<PSET>& x,
     Parma_Polyhedra_Library::COW_Pointset<PSET>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_COW_Pointset_inlines_hh)
