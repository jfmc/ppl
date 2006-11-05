/* Box class implementation: inline functions.
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
  add_constraints(cs);
}

template <typename Interval>
inline dimension_type
Box<Interval>::space_dimension() const {
  return seq.size();
}

template <typename Interval>
inline const Interval&
Box<Interval>::operator[](const dimension_type k) const {
  assert(k < seq.size());
  return seq[k];
}

template <typename Interval>
inline bool
Box<Interval>::marked_empty() const {
  return empty_up_to_date && empty;
}

template <typename Interval>
inline bool
Box<Interval>::is_empty() const {
  return marked_empty() || check_empty();
}

template <typename Interval>
inline void
Box<Interval>::upper_bound_assign(const Box& y) {
  box_hull_assign(y);
}

template <typename Interval>
Constraint_System
Box<Interval>::minimized_constraints() const {
  return constraints();
}

template <typename Interval>
inline void
Box<Interval>::add_space_dimensions_and_embed(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  // To embed an n-dimension space box in a (n+m)-dimension space,
  // we just add `m' new (universe) elements to the sequence.
  seq.insert(seq.end(), m, Interval());
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

  if (seq_k.info().test_boundary_property(Boundary::LOWER,
					  Boundary::UNBOUNDED))
    return false;

  closed = !seq_k.info().test_boundary_property(Boundary::LOWER,
						Boundary::OPEN);

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
  assert(k < seq.size());
  const Interval& seq_k = seq[k];

  if (seq_k.info().test_boundary_property(Boundary::UPPER,
					  Boundary::UNBOUNDED))
    return false;

  closed = !seq_k.info().test_boundary_property(Boundary::UPPER,
						Boundary::OPEN);

  mpq_class ur;
  assign_r(ur, seq_k.upper(), ROUND_NOT_NEEDED);
  n = ur.get_num();
  d = ur.get_den();

  return true;
}

template <typename Interval>
inline void
Box<Interval>::set_empty() {
  empty = empty_up_to_date = true;
}

template <typename Interval>
inline void
Box<Interval>::raise_lower_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  assert(k < seq.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // FIXME: can exploit the return type?
  refine(seq[k], (closed ? GREATER_THAN_OR_EQUAL : GREATER_THAN), q);
  empty_up_to_date = false;
}

template <typename Interval>
inline void
Box<Interval>::lower_upper_bound(const dimension_type k, const bool closed,
				 Coefficient_traits::const_reference n,
				 Coefficient_traits::const_reference d) {
  assert(k < seq.size());
  assert(d != 0);
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // FIXME: can exploit the return type?
  refine(seq[k], (closed ? LESS_THAN_OR_EQUAL : LESS_THAN), q);
  empty_up_to_date = false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_inlines_hh)
