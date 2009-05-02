/* Determinate class implementation: inline functions.
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

#ifndef PPL_Determinate_inlines_hh
#define PPL_Determinate_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PSET>
inline
Determinate<PSET>::Rep::Rep(dimension_type num_dimensions,
			  Degenerate_Element kind)
  : references(0), ph(num_dimensions, kind) {
}

template <typename PSET>
inline
Determinate<PSET>::Rep::Rep(const PSET& p)
  : references(0), ph(p) {
}

template <typename PSET>
inline
Determinate<PSET>::Rep::Rep(const Constraint_System& cs)
  : references(0), ph(cs) {
}

template <typename PSET>
inline
Determinate<PSET>::Rep::Rep(const Congruence_System& cgs)
  : references(0), ph(cgs) {
}

template <typename PSET>
inline
Determinate<PSET>::Rep::~Rep() {
  assert(references == 0);
}

template <typename PSET>
inline void
Determinate<PSET>::Rep::new_reference() const {
  ++references;
}

template <typename PSET>
inline bool
Determinate<PSET>::Rep::del_reference() const {
  return --references == 0;
}

template <typename PSET>
inline bool
Determinate<PSET>::Rep::is_shared() const {
  return references > 1;
}

template <typename PSET>
inline memory_size_type
Determinate<PSET>::Rep::external_memory_in_bytes() const {
  return ph.external_memory_in_bytes();
}

template <typename PSET>
inline memory_size_type
Determinate<PSET>::Rep::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename PSET>
inline
Determinate<PSET>::Determinate(const PSET& ph)
  : prep(new Rep(ph)) {
  prep->new_reference();
}

template <typename PSET>
inline
Determinate<PSET>::Determinate(const Constraint_System& cs)
  : prep(new Rep(cs)) {
  prep->new_reference();
}

template <typename PSET>
inline
Determinate<PSET>::Determinate(const Congruence_System& cgs)
  : prep(new Rep(cgs)) {
  prep->new_reference();
}

template <typename PSET>
inline
Determinate<PSET>::Determinate(const Determinate& y)
  : prep(y.prep) {
  prep->new_reference();
}

template <typename PSET>
inline
Determinate<PSET>::~Determinate() {
  if (prep->del_reference())
    delete prep;
}

template <typename PSET>
inline Determinate<PSET>&
Determinate<PSET>::operator=(const Determinate& y) {
  y.prep->new_reference();
  if (prep->del_reference())
    delete prep;
  prep = y.prep;
  return *this;
}

template <typename PSET>
inline void
Determinate<PSET>::swap(Determinate& y) {
  std::swap(prep, y.prep);
}

template <typename PSET>
inline void
Determinate<PSET>::mutate() {
  if (prep->is_shared()) {
    Rep* new_prep = new Rep(prep->ph);
    (void) prep->del_reference();
    new_prep->new_reference();
    prep = new_prep;
  }
}

template <typename PSET>
inline const PSET&
Determinate<PSET>::element() const {
  return prep->ph;
}

template <typename PSET>
inline PSET&
Determinate<PSET>::element() {
  mutate();
  return prep->ph;
}

template <typename PSET>
inline void
Determinate<PSET>::upper_bound_assign(const Determinate& y) {
  element().upper_bound_assign(y.element());
}

template <typename PSET>
inline void
Determinate<PSET>::meet_assign(const Determinate& y) {
  element().intersection_assign(y.element());
}

template <typename PSET>
inline bool
Determinate<PSET>::has_nontrivial_weakening() {
  // FIXME: the following should be turned into a query to PSET.  This
  // can be postponed until the time the ask-and-tell construction is
  // revived.
  return false;
}

template <typename PSET>
inline void
Determinate<PSET>::weakening_assign(const Determinate& y) {
  // FIXME: the following should be turned into a proper
  // implementation.  This can be postponed until the time the
  // ask-and-tell construction is revived.
  element().difference_assign(y.element());
}

template <typename PSET>
inline void
Determinate<PSET>::concatenate_assign(const Determinate& y) {
  element().concatenate_assign(y.element());
}

template <typename PSET>
inline bool
Determinate<PSET>::definitely_entails(const Determinate& y) const {
  return prep == y.prep || y.prep->ph.contains(prep->ph);
}

template <typename PSET>
inline bool
Determinate<PSET>::is_definitely_equivalent_to(const Determinate& y) const {
  return prep == y.prep || prep->ph == y.prep->ph;
}

template <typename PSET>
inline bool
Determinate<PSET>::is_top() const {
  return prep->ph.is_universe();
}

template <typename PSET>
inline bool
Determinate<PSET>::is_bottom() const {
  return prep->ph.is_empty();
}

template <typename PSET>
inline memory_size_type
Determinate<PSET>::external_memory_in_bytes() const {
  return prep->total_memory_in_bytes();
}

template <typename PSET>
inline memory_size_type
Determinate<PSET>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename PSET>
inline bool
Determinate<PSET>::OK() const {
  return prep->ph.OK();
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PSET>
inline std::ostream&
operator<<(std::ostream& s, const Determinate<PSET>& x) {
  s << x.element();
  return s;
}

} // namespace IO_Operators

/*! \relates Determinate */
template <typename PSET>
inline bool
operator==(const Determinate<PSET>& x, const Determinate<PSET>& y) {
  return x.prep == y.prep || x.prep->ph == y.prep->ph;
}

/*! \relates Determinate */
template <typename PSET>
inline bool
operator!=(const Determinate<PSET>& x, const Determinate<PSET>& y) {
  return x.prep != y.prep && x.prep->ph != y.prep->ph;
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline
Determinate<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>::
Binary_Operator_Assign_Lifter(Binary_Operator_Assign op_assign)
  : op_assign_(op_assign) {
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline void
Determinate<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>::
operator()(Determinate& x, const Determinate& y) const {
  op_assign_(x.element(), y.element());
}

template <typename PSET>
template <typename Binary_Operator_Assign>
inline
Determinate<PSET>::Binary_Operator_Assign_Lifter<Binary_Operator_Assign>
Determinate<PSET>::lift_op_assign(Binary_Operator_Assign op_assign) {
  return Binary_Operator_Assign_Lifter<Binary_Operator_Assign>(op_assign);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PSET>
inline void
swap(Parma_Polyhedra_Library::Determinate<PSET>& x,
     Parma_Polyhedra_Library::Determinate<PSET>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Determinate_inlines_hh)
