/* Box class implementation: inline functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Box_inlines_hh
#define PPL_Box_inlines_hh 1

#include "Boundary.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline
Box<Interval>::Box(dimension_type num_dimensions, Degenerate_Element kind)
  : seq(num_dimensions), empty(kind == EMPTY), empty_up_to_date(true) {
  // FIXME: temporary. To be removed as soon as the default
  // constructor of Interval will do the right thing.
  Box& x = *this;
  if (kind == UNIVERSE)
    for (dimension_type i = num_dimensions; i-- > 0; )
      x.seq[i].set_universe();
  // END OF FIXME.
  assert(this->OK());
}

template <typename Interval>
inline
Box<Interval>::Box(const Box& y)
  : seq(y.seq), empty(y.empty), empty_up_to_date(y.empty_up_to_date) {
}

template <typename Interval>
inline
Box<Interval>::Box(const Constraint_System& cs)
  : seq(cs.space_dimension()), empty_up_to_date(false) {
  Box& x = *this;
  // FIXME: temporary. To be removed as soon as the default
  // constructor of Interval will do the right thing.
  for (dimension_type i = x.seq.size(); i-- > 0; )
    x.seq[i].set_universe();
  // END OF FIXME.
  x.add_constraints(cs);
}

template <typename Interval>
inline void
Box<Interval>::swap(Box& y) {
  Box& x = *this;
  std::swap(x.seq, y.seq);
  std::swap(x.empty, y.empty);
  std::swap(x.empty_up_to_date, y.empty_up_to_date);
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
inline dimension_type
Box<Interval>::space_dimension() const {
  const Box& x = *this;
  return x.seq.size();
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
  const Box& x = *this;
  assert(k < x.seq.size());
  return x.seq[k];
}

template <typename Interval>
inline bool
Box<Interval>::marked_empty() const {
  const Box& x = *this;
  return x.empty_up_to_date && x.empty;
}

template <typename Interval>
inline bool
Box<Interval>::is_empty() const {
  const Box& x = *this;
  return x.empty_up_to_date ? x.empty : x.check_empty();
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
bool
Box<Interval>::strictly_contains(const Box<Interval>& y) const {
  const Box& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename Interval>
inline void
Box<Interval>::upper_bound_assign(const Box& y) {
  Box& x = *this;
  x.box_hull_assign(y);
}

template <typename Interval>
Constraint_System
Box<Interval>::minimized_constraints() const {
  const Box& x = *this;
  return x.constraints();
}

template <typename Interval>
inline void
Box<Interval>::add_space_dimensions_and_embed(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  Box& x = *this;
  // To embed an n-dimension space box in a (n+m)-dimension space,
  // we just add `m' new (universe) elements to the sequence.
  x.seq.insert(x.seq.end(), m, Interval());
  // FIXME: temporary. To be removed as soon as the default
  // constructor of Interval will do the right thing.
  for (dimension_type sz = x.seq.size(), i = sz - m; i < sz; ++i)
    x.seq[i].set_universe();
  // END OF FIXME.
  assert(x.OK());
}

template <typename Interval>
inline void
Box<Interval>::expand_space_dimension(const Variable var,
				      const dimension_type m) {
  Box& x = *this;
  const dimension_type space_dim = x.space_dimension();
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
  x.seq.insert(x.seq.end(), m, x.seq[var.id()]);
  assert(x.OK());
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
  const Box& x = *this;
  assert(k < x.seq.size());
  const Interval& seq_k = x.seq[k];

  if (seq_k.lower_is_unbounded())
    return false;

  closed = !seq_k.lower_is_open();

  mpq_class lr;
  assign_r(lr, seq_k.lower(), ROUND_NOT_NEEDED);
  n = lr.get_num();
  d = lr.get_den();

  return true;
}

template <typename Interval>
inline bool
Box<Interval>::get_upper_bound(const dimension_type k, bool& closed,
			       Coefficient& n, Coefficient& d) const {
  const Box& x = *this;
  assert(k < x.seq.size());
  const Interval& seq_k = x.seq[k];

  if (seq_k.upper_is_unbounded())
    return false;

  closed = !seq_k.upper_is_open();

  mpq_class ur;
  assign_r(ur, seq_k.upper(), ROUND_NOT_NEEDED);
  n = ur.get_num();
  d = ur.get_den();

  return true;
}

template <typename Interval>
inline void
Box<Interval>::set_empty() {
  Box& x = *this;
  x.empty = x.empty_up_to_date = true;
}

template <typename Interval>
void
Box<Interval>::difference_assign(const Box& y) {
  Box& x = *this;
  x.box_difference_assign(y);
}

template <typename Interval>
inline void
Box<Interval>::raise_lower_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  Box& x = *this;
  assert(k < x.seq.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  I_Result r = refine_existential(x.seq[k], (closed ? GREATER_OR_EQUAL : GREATER_THAN), q);
  // FIXME: r is a mask I_EMPTY may be or'ed with I_SINGULARITIES
  if (r == I_EMPTY)
    x.set_empty();
  else if (r & I_MAYBE_EMPTY)
    x.empty_up_to_date = false;
}

template <typename Interval>
inline void
Box<Interval>::lower_upper_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  Box& x = *this;
  assert(k < x.seq.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  I_Result r = refine_existential(x.seq[k], (closed ? LESS_OR_EQUAL : LESS_THAN), q);
  // FIXME: r is a mask I_EMPTY may be or'ed with I_SINGULARITIES
  if (r == I_EMPTY)
    x.set_empty();
  else if (r & I_MAYBE_EMPTY)
    x.empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_inlines_hh)
