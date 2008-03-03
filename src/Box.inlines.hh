/* Box class implementation: inline functions.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Box_inlines_hh
#define PPL_Box_inlines_hh 1

#include "Boundary.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Congruence_System.defs.hh"
#include "Congruence_System.inlines.hh"
#include "distances.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline bool
Box<Interval>::marked_empty() const {
  return status.test_empty_up_to_date() && status.test_empty();
}

template <typename Interval>
inline void
Box<Interval>::set_empty() {
  status.set_empty();
  status.set_empty_up_to_date();
}

template <typename Interval>
inline void
Box<Interval>::set_nonempty() {
  status.reset_empty();
  status.set_empty_up_to_date();
}

template <typename Interval>
inline void
Box<Interval>::set_empty_up_to_date() {
  status.set_empty_up_to_date();
}

template <typename Interval>
inline void
Box<Interval>::reset_empty_up_to_date() {
  return status.reset_empty_up_to_date();
}

template <typename Interval>
inline
Box<Interval>::Box(const Box& y)
  : seq(y.seq), status(y.status) {
}

template <typename Interval>
inline Box<Interval>&
Box<Interval>::operator=(const Box& y) {
  seq = y.seq;
  status = y.status;
  return *this;
}

template <typename Interval>
inline void
Box<Interval>::swap(Box& y) {
  Box& x = *this;
  std::swap(x.seq, y.seq);
  std::swap(x.status, y.status);
}

template <typename Interval>
inline
Box<Interval>::Box(const Constraint_System& cs, Recycle_Input) {
  // Recycling is useless: just delegate.
  Box<Interval> tmp(cs);
  this->swap(tmp);
}

template <typename Interval>
inline
Box<Interval>::Box(const Generator_System& gs, Recycle_Input) {
  // Recycling is useless: just delegate.
  Box<Interval> tmp(gs);
  this->swap(tmp);
}

template <typename Interval>
inline
Box<Interval>::Box(const Congruence_System& cgs, Recycle_Input) {
  // Recycling is useless: just delegate.
  Box<Interval> tmp(cgs);
  this->swap(tmp);
}

template <typename Interval>
inline memory_size_type
Box<Interval>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename Interval>
inline dimension_type
Box<Interval>::space_dimension() const {
  return seq.size();
}

template <typename Interval>
inline dimension_type
Box<Interval>::max_space_dimension() {
  // One dimension is reserved to have a value of type dimension_type
  // that does not represent a legal dimension.
  return Sequence().max_size() - 1;
}

template <typename Interval>
inline const Interval&
Box<Interval>::operator[](const dimension_type k) const {
  assert(k < seq.size());
  return seq[k];
}

template <typename Interval>
inline const Interval&
Box<Interval>::get_interval(const Variable var) const {
  if (space_dimension() < var.space_dimension())
    throw_dimension_incompatible("get_interval(v)", "v", var);

  if (is_empty()) {
    static Interval empty_interval(EMPTY);
    return empty_interval;
  }

  return seq[var.id()];
}

template <typename Interval>
inline void
Box<Interval>::set_interval(const Variable var, const Interval& i) {
  const dimension_type space_dim = space_dimension();
  if (space_dim < var.space_dimension())
    throw_dimension_incompatible("set_interval(v, i)", "v", var);

  if (is_empty() && space_dim >= 2)
    // If the box is empty, and has dimension >= 2, setting only one
    // interval will not make it non-empty.
    return;

  seq[var.id()] = i;
  reset_empty_up_to_date();

  assert(OK());
}

template <typename Interval>
inline bool
Box<Interval>::is_empty() const {
  return marked_empty() || check_empty();
}

template <typename Interval>
inline bool
Box<Interval>::bounds_from_above(const Linear_Expression& expr) const {
  return bounds(expr, true);
}

template <typename Interval>
inline bool
Box<Interval>::bounds_from_below(const Linear_Expression& expr) const {
  return bounds(expr, false);
}

template <typename Interval>
inline bool
Box<Interval>::maximize(const Linear_Expression& expr,
			Coefficient& sup_n, Coefficient& sup_d,
			bool& maximum) const {
  return max_min(expr, true, sup_n, sup_d, maximum);
}

template <typename Interval>
inline bool
Box<Interval>::maximize(const Linear_Expression& expr,
			Coefficient& sup_n, Coefficient& sup_d, bool& maximum,
			Generator& g) const {
  return max_min(expr, true, sup_n, sup_d, maximum, g);
}

template <typename Interval>
inline bool
Box<Interval>::minimize(const Linear_Expression& expr,
			Coefficient& inf_n, Coefficient& inf_d,
			bool& minimum) const {
  return max_min(expr, false, inf_n, inf_d, minimum);
}

template <typename Interval>
inline bool
Box<Interval>::minimize(const Linear_Expression& expr,
			Coefficient& inf_n, Coefficient& inf_d, bool& minimum,
			Generator& g) const {
  return max_min(expr, false, inf_n, inf_d, minimum, g);
}

template <typename Interval>
inline bool
Box<Interval>::strictly_contains(const Box& y) const {
  const Box& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename Interval>
inline bool
Box<Interval>::intersection_assign_and_minimize(const Box& y) {
  intersection_assign(y);
  return !is_empty();
}

template <typename Interval>
inline void
Box<Interval>::upper_bound_assign(const Box& y) {
  Box& x = *this;
  x.box_hull_assign(y);
}

template <typename Interval>
inline bool
Box<Interval>::box_hull_assign_if_exact(const Box&) {
  // TODO: this must be properly implemented.
  return false;
}

template <typename Interval>
inline bool
Box<Interval>::upper_bound_assign_if_exact(const Box& y) {
  Box& x = *this;
  return x.box_hull_assign_if_exact(y);
}

template <typename Interval>
inline void
Box<Interval>::expand_space_dimension(const Variable var,
				      const dimension_type m) {
  const dimension_type space_dim = space_dimension();
  // `var' should be one of the dimensions of the vector space.
  if (var.space_dimension() > space_dim)
    throw_dimension_incompatible("expand_space_dimension(v, m)", "v", var);

  // The space dimension of the resulting Box should not
  // overflow the maximum allowed space dimension.
  if (m > max_space_dimension() - space_dim)
    throw_generic("expand_dimension(v, m)",
		  "adding m new space dimensions exceeds "
		  "the maximum allowed space dimension");

  // To expand the space dimension corresponding to variable `var',
  // we append to the box `m' copies of the corresponding interval.
  seq.insert(seq.end(), m, seq[var.id()]);
  assert(OK());
}

template <typename Interval>
inline bool
operator!=(const Box<Interval>& x, const Box<Interval>& y) {
  return !(x == y);
}

template <typename Interval>
inline bool
Box<Interval>::get_lower_bound(const dimension_type k, bool& closed,
			       Coefficient& n, Coefficient& d) const {
  assert(k < seq.size());
  const Interval& seq_k = seq[k];

  if (seq_k.lower_is_unbounded())
    return false;

  closed = !seq_k.lower_is_open();

  DIRTY_TEMP0(mpq_class, lr);
  assign_r(lr, seq_k.lower(), ROUND_NOT_NEEDED);
  n = lr.get_num();
  d = lr.get_den();

  return true;
}

template <typename Interval>
inline bool
Box<Interval>::get_upper_bound(const dimension_type k, bool& closed,
			       Coefficient& n, Coefficient& d) const {
  assert(k < seq.size());
  const Interval& seq_k = seq[k];

  if (seq_k.upper_is_unbounded())
    return false;

  closed = !seq_k.upper_is_open();

  DIRTY_TEMP0(mpq_class, ur);
  assign_r(ur, seq_k.upper(), ROUND_NOT_NEEDED);
  n = ur.get_num();
  d = ur.get_den();

  return true;
}

template <typename Interval>
inline void
Box<Interval>::difference_assign(const Box& y) {
  Box& x = *this;
  x.box_difference_assign(y);
}

template <typename Interval>
inline void
Box<Interval>::add_constraint(const Constraint& c) {
  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > space_dimension())
    throw_dimension_incompatible("add_constraint(c)", c);

  // If the box is already empty, there is nothing left to do.
  if (marked_empty())
    return;

  add_constraint_no_check(c);
}

template <typename T>
inline bool
Box<T>::add_constraint_and_minimize(const Constraint& c) {
  add_constraint(c);
  return !is_empty();
}

template <typename Interval>
inline void
Box<Interval>::add_constraints(const Constraint_System& cs) {
  // Dimension-compatibility check.
  if (cs.space_dimension() > space_dimension())
    throw_dimension_incompatible("add_constraints(cs)", cs);

  add_constraints_no_check(cs);
}

template <typename T>
inline bool
Box<T>::add_constraints_and_minimize(const Constraint_System& cs) {
  add_constraints(cs);
  return !is_empty();
}

template <typename T>
inline void
Box<T>::add_recycled_constraints(Constraint_System& cs) {
  add_constraints(cs);
}

template <typename T>
inline bool
Box<T>::add_recycled_constraints_and_minimize(Constraint_System& cs) {
  add_constraints(cs);
  return !is_empty();
}

template <typename Interval>
inline void
Box<Interval>::add_congruence(const Congruence& cg) {
  const dimension_type cg_space_dim = cg.space_dimension();
  // Dimension-compatibility check.
  if (cg_space_dim > space_dimension())
    throw_dimension_incompatible("add_congruence(cg)", cg);

  // If the box is already empty, there is nothing left to do.
  if (marked_empty())
    return;

  add_congruence_no_check(cg);
}

template <typename T>
inline bool
Box<T>::add_congruence_and_minimize(const Congruence& cg) {
  add_congruence(cg);
  return !is_empty();
}

template <typename Interval>
inline void
Box<Interval>::add_congruences(const Congruence_System& cgs) {
  if (cgs.space_dimension() > space_dimension())
    throw_dimension_incompatible("add_congruences(cgs)", cgs);
  add_congruences_no_check(cgs);
}

template <typename T>
inline bool
Box<T>::add_congruences_and_minimize(const Congruence_System& cgs) {
  add_congruences(cgs);
  return !is_empty();
}

template <typename T>
inline void
Box<T>::add_recycled_congruences(Congruence_System& cgs) {
  add_congruences(cgs);
}

template <typename T>
inline bool
Box<T>::add_recycled_congruences_and_minimize(Congruence_System& cgs) {
  add_congruences(cgs);
  return !is_empty();
}

template <typename T>
inline bool
Box<T>::can_recycle_constraint_systems() {
  return false;
}

template <typename T>
inline bool
Box<T>::can_recycle_congruence_systems() {
  return false;
}

template <typename T>
inline void
Box<T>::widening_assign(const Box& y, unsigned* tp) {
  CC76_widening_assign(y, tp);
}

template <typename Interval>
inline Congruence_System
Box<Interval>::minimized_congruences() const {
  // Only equalities can be congruences and these are already minimized.
  return congruences();
}

template <typename Interval>
inline void
Box<Interval>::refine(const Constraint& c) {
  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > space_dimension())
    throw_dimension_incompatible("add_constraint(c)", c);

  // If the box is already empty, there is nothing left to do.
  if (marked_empty())
    return;

  refine_no_check(c);
}

template <typename Interval>
inline void
Box<Interval>::refine(const Constraint_System& cs) {
  // Dimension-compatibility check.
  if (cs.space_dimension() > space_dimension())
    throw_dimension_incompatible("add_constraints(cs)", cs);

  // If the box is already empty, there is nothing left to do.
  if (marked_empty())
    return;

  refine_no_check(cs);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Box<Interval>& x,
			    const Box<Interval>& y,
			    const Rounding_Dir dir,
			    Temp& tmp0,
			    Temp& tmp1,
			    Temp& tmp2) {
  return l_m_distance_assign<Rectilinear_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Box<Interval>& x,
			    const Box<Interval>& y,
			    const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  DIRTY_TEMP(Checked_Temp, tmp0);
  DIRTY_TEMP(Checked_Temp, tmp1);
  DIRTY_TEMP(Checked_Temp, tmp2);
  return rectilinear_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename To, typename Interval>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Box<Interval>& x,
			    const Box<Interval>& y,
			    const Rounding_Dir dir) {
  return rectilinear_distance_assign<To, To, Interval>(r, x, y, dir);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Box<Interval>& x,
			  const Box<Interval>& y,
			  const Rounding_Dir dir,
			  Temp& tmp0,
			  Temp& tmp1,
			  Temp& tmp2) {
  return l_m_distance_assign<Euclidean_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Box<Interval>& x,
			  const Box<Interval>& y,
			  const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  DIRTY_TEMP(Checked_Temp, tmp0);
  DIRTY_TEMP(Checked_Temp, tmp1);
  DIRTY_TEMP(Checked_Temp, tmp2);
  return euclidean_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename To, typename Interval>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Box<Interval>& x,
			  const Box<Interval>& y,
			  const Rounding_Dir dir) {
  return euclidean_distance_assign<To, To, Interval>(r, x, y, dir);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Box<Interval>& x,
			   const Box<Interval>& y,
			   const Rounding_Dir dir,
			   Temp& tmp0,
			   Temp& tmp1,
			   Temp& tmp2) {
  return l_m_distance_assign<L_Infinity_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename Temp, typename To, typename Interval>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Box<Interval>& x,
			   const Box<Interval>& y,
			   const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  DIRTY_TEMP(Checked_Temp, tmp0);
  DIRTY_TEMP(Checked_Temp, tmp1);
  DIRTY_TEMP(Checked_Temp, tmp2);
  return l_infinity_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Box */
template <typename To, typename Interval>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Box<Interval>& x,
			   const Box<Interval>& y,
			   const Rounding_Dir dir) {
  return l_infinity_distance_assign<To, To, Interval>(r, x, y, dir);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_inlines_hh)
