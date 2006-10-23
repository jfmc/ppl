/* Pointset_Powerset class implementation: inline functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Pointset_Powerset_inlines_hh
#define PPL_Pointset_Powerset_inlines_hh 1

#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Congruence.defs.hh"
#include "Congruence_System.defs.hh"
#include "Congruence_System.inlines.hh"
#include "C_Polyhedron.defs.hh"
#include "NNC_Polyhedron.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

template <typename PH>
inline dimension_type
Pointset_Powerset<PH>::space_dimension() const {
  return space_dim;
}

template <typename PH>
inline dimension_type
Pointset_Powerset<PH>::max_space_dimension() {
  return PH::max_space_dimension();
}

template <typename PH>
inline
Pointset_Powerset<PH>::Pointset_Powerset(dimension_type num_dimensions,
					 Degenerate_Element kind)
  : Base(), space_dim(num_dimensions) {
  Pointset_Powerset& x = *this;
  if (kind == UNIVERSE)
    x.sequence.push_back(Determinate<PH>(PH(num_dimensions, kind)));
  assert(x.OK());
}

template <typename PH>
inline
Pointset_Powerset<PH>::Pointset_Powerset(const Pointset_Powerset& y)
  : Base(y), space_dim(y.space_dim) {
}

template <typename PH>
inline
Pointset_Powerset<PH>::Pointset_Powerset(const PH& ph)
  : Base(ph), space_dim(ph.space_dimension()) {
}

// FIXME: This full specialization is declared inline and placed here
// just as a workaround to a bug in GCC 3.3.3. In principle, it should
// not be declared inline and moved in Pointset_Powerset.cc.
// See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=13635.
template <>
template <>
inline
Pointset_Powerset<NNC_Polyhedron>
::Pointset_Powerset(const Pointset_Powerset<C_Polyhedron>& y)
  : Base(), space_dim(y.space_dimension()) {
  Pointset_Powerset& x = *this;
  for (Pointset_Powerset<C_Polyhedron>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i)
    x.sequence.push_back(Determinate<NNC_Polyhedron>
			 (NNC_Polyhedron(i->element())));
  x.reduced = y.reduced;
  assert(x.OK());
}

// FIXME: This full specialization is declared inline and placed here
// just as a workaround to a bug in GCC 3.3.3. In principle, it should
// not be declared inline and moved in Pointset_Powerset.cc.
// See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=13635.
template <>
template <>
inline
Pointset_Powerset<C_Polyhedron>
::Pointset_Powerset(const Pointset_Powerset<NNC_Polyhedron>& y)
  : Base(), space_dim(y.space_dimension()) {
  Pointset_Powerset& x = *this;
  for (Pointset_Powerset<NNC_Polyhedron>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i)
    x.sequence.push_back(Determinate<C_Polyhedron>
			 (C_Polyhedron(i->element())));

  // Note: this might be non-reduced even when `y' is known to be
  // omega-reduced, because the constructor of C_Polyhedron, by
  // enforcing topological closure, may have made different elements
  // comparable.
  x.reduced = false;
  assert(x.OK());
}

template <typename PH>
inline
Pointset_Powerset<PH>::Pointset_Powerset(const Constraint_System& cs)
  : Base(Determinate<PH>(cs)), space_dim(cs.space_dimension()) {
  assert(OK());
}

template <typename PH>
inline
Pointset_Powerset<PH>::Pointset_Powerset(const Congruence_System& cgs)
  : Base(Determinate<PH>(cgs)), space_dim(cgs.space_dimension()) {
  assert(OK());
}

template <typename PH>
inline Pointset_Powerset<PH>&
Pointset_Powerset<PH>::operator=(const Pointset_Powerset& y) {
  Pointset_Powerset& x = *this;
  x.Base::operator=(y);
  x.space_dim = y.space_dim;
  return x;
}

template <typename PH>
inline void
Pointset_Powerset<PH>::swap(Pointset_Powerset& y) {
  Pointset_Powerset& x = *this;
  x.Base::swap(y);
  std::swap(x.space_dim, y.space_dim);
}

template <typename PH>
template <typename QH>
inline Pointset_Powerset<PH>&
Pointset_Powerset<PH>::operator=(const Pointset_Powerset<QH>& y) {
  Pointset_Powerset& x = *this;
  Pointset_Powerset<PH> pps(y);
  x.swap(pps);
  return x;
}

template <typename PH>
inline void
Pointset_Powerset<PH>::intersection_assign(const Pointset_Powerset& y) {
  Pointset_Powerset& x = *this;
  x.pairwise_apply_assign
    (y, CS::lift_op_assign(std::mem_fun_ref(&PH::intersection_assign)));
}

template <typename PH>
inline void
Pointset_Powerset<PH>::time_elapse_assign(const Pointset_Powerset& y) {
  Pointset_Powerset& x = *this;
  x.pairwise_apply_assign
    (y, CS::lift_op_assign(std::mem_fun_ref(&PH::time_elapse_assign)));
}

template <typename PH>
inline bool
Pointset_Powerset<PH>
::geometrically_covers(const Pointset_Powerset& y) const {
  const Pointset_Powerset<NNC_Polyhedron> xx(*this);
  const Pointset_Powerset<NNC_Polyhedron> yy(y);
  return xx.geometrically_covers(yy);
}

template <typename PH>
inline bool
Pointset_Powerset<PH>
::geometrically_equals(const Pointset_Powerset& y) const {
  const Pointset_Powerset<NNC_Polyhedron> xx(*this);
  const Pointset_Powerset<NNC_Polyhedron> yy(y);
  return xx.geometrically_covers(yy) && yy.geometrically_covers(xx);
}

template <>
inline bool
Pointset_Powerset<NNC_Polyhedron>
::geometrically_equals(const Pointset_Powerset& y) const {
  const Pointset_Powerset& x = *this;
  return x.geometrically_covers(y) && y.geometrically_covers(x);
}

template <typename PH>
inline memory_size_type
Pointset_Powerset<PH>::external_memory_in_bytes() const {
  return Base::external_memory_in_bytes();
}

template <typename PH>
inline memory_size_type
Pointset_Powerset<PH>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename PH>
inline void
Pointset_Powerset<PH>
::poly_difference_assign(const Pointset_Powerset& y) {
  // FIXME: can this become more accurate for, e.g., Grid instances?
  Pointset_Powerset<NNC_Polyhedron> nnc_this(*this);
  Pointset_Powerset<NNC_Polyhedron> nnc_y(y);
  nnc_this.poly_difference_assign(nnc_y);
  *this = nnc_this;
}

/*! \relates Pointset_Powerset */
template <typename PH>
inline bool
check_containment(const PH& ph, const Pointset_Powerset<PH>& ps) {
  const NNC_Polyhedron pph = NNC_Polyhedron(ph.constraints());
  const Pointset_Powerset<NNC_Polyhedron> pps(ps);
  return check_containment(pph, pps);
}

/*! \relates Pointset_Powerset */
template <>
inline bool
check_containment(const C_Polyhedron& ph,
		  const Pointset_Powerset<C_Polyhedron>& ps) {
  return check_containment(NNC_Polyhedron(ph),
			   Pointset_Powerset<NNC_Polyhedron>(ps));
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Pointset_Powerset */
template <typename PH>
inline void
swap(Parma_Polyhedra_Library::Pointset_Powerset<PH>& x,
     Parma_Polyhedra_Library::Pointset_Powerset<PH>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Pointset_Powerset_inlines_hh)
