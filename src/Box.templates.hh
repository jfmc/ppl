/* Box class implementation: non-inline template functions.
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

#ifndef PPL_Box_templates_hh
#define PPL_Box_templates_hh 1

#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"

// This is only to access Implementation::BD_Shapes::div_round_up.
#include "BD_Shape.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline bool
Box<Interval>::OK() const {
  // FIXME: check that the empty flag and the intervals are consistent?
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].OK())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::check_empty() const {
  assert(!empty_up_to_date);
  empty_up_to_date = true;
  for (dimension_type k = seq.size(); k-- > 0; )
    if (seq[k].is_empty()) {
      empty = true;
      return true;
    }
  empty = false;
  return false;
}

template <typename Interval>
bool
Box<Interval>::is_universe() const {
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].is_universe())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_topologically_closed() const {
  if (is_empty())
    return true;
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].topologicaly_closed())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_bounded() const {
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].is_bounded())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::contains_integer_point() const {
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].contains_integer_point())
      return false;
  return true;
}

template <typename Interval>
void
Box<Interval>::intersection_assign(const Box& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("intersection_assign(y)", y);

  // If one of the two boxes is empty, the intersection is empty.
  if (marked_empty())
    return;
  if (y.marked_empty()) {
    set_empty();
    return;
  }

  for (dimension_type k = seq.size(); k-- > 0; )
    intersect_assign(seq[k], y.seq[k]);

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::box_hull_assign(const Box& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("box_hull_assign(y)", y);

  // The hull of a box with an empty box is equal to the first box.
  if (y.marked_empty())
    return;
  if (marked_empty()) {
    *this = y;
    return;
  }

  for (dimension_type k = seq.size(); k-- > 0; )
    convex_hull_assign(seq[k], y.seq[k]);

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::concatenate_assign(const Box& y) {
  Box& x = *this;

  const dimension_type x_space_dim = x.space_dimension();
  const dimension_type y_space_dim = y.space_dimension();

  // If `y' is an empty 0-dim space box let `*this' become empty.
  if (y_space_dim == 0 && y.marked_empty()) {
    set_empty();
    return;
  }

  // If `x' is an empty 0-dim space box, then it is sufficient to adjust
  // the dimension of the vector space.
  if (x_space_dim == 0 && marked_empty()) {
    x.seq.insert(x.seq.end(), y_space_dim);
    assert(OK());
    return;
  }

  seq.reserve(x_space_dim + y_space_dim);
  std::copy(y.seq.begin(), y.seq.end(),
	    std::back_insert_iterator<Sequence>(x.seq));

  if (x.marked_empty() && !y.marked_empty())
    x.empty_up_to_date = false;

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::add_constraint(const Constraint& c) {
  using Implementation::BD_Shapes::div_round_up;

  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > space_dimension())
    throw_dimension_incompatible("add_constraint(c)", c);

  dimension_type c_num_vars = 0;
  dimension_type c_only_var = 0;
  TEMP_INTEGER(c_coeff);

  // Constraints that are not interval constraints are ignored.
  if (!extract_interval_constraint(c, c_space_dim,
				   c_num_vars, c_only_var, c_coeff))
    return;

  if (c_num_vars == 0) {
    // Dealing with a trivial constraint.
    if (c.inhomogeneous_term() < 0)
      set_empty();
    return;
  }

#if 0
  // Select the cell to be modified for the "<=" part of the constraint,
  // and set `coeff' to the absolute value of itself.
  N& x = (coeff < 0) ? dbm[i][j] : dbm[j][i];
  N& y = (coeff < 0) ? dbm[j][i] : dbm[i][j];
  if (coeff < 0)
    coeff = -coeff;

  bool changed = false;
  // Compute the bound for `x', rounding towards plus infinity.
  N d;
  div_round_up(d, c.inhomogeneous_term(), coeff);
  if (x > d) {
    x = d;
    changed = true;
  }

  if (c.is_strict_inequality())
    ;


  if (c.is_equality()) {
    // Also compute the bound for `y', rounding towards plus infinity.
    div_round_up(d, -c.inhomogeneous_term(), coeff);
    if (y > d) {
      y = d;
      changed = true;
    }
  }

  // In general, adding a constraint does not preserve the shortest-path
  // closure or reduction of the bounded difference shape.
  if (changed && marked_shortest_path_closed())
    status.reset_shortest_path_closed();
#endif

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::add_constraints(const Constraint_System& cs) {
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i)
    add_constraint(*i);
  assert(OK());
}

template <typename Interval>
template <typename Iterator>
void
Box<Interval>::CC76_widening_assign(const Box& y,
				    Iterator first, Iterator last) {
  for (dimension_type i = seq.size(); i-- > 0; ) {
    Interval& x_seq_i = seq[i];
    const Interval& y_seq_i = y.seq[i];

    // Upper bound.
    typename Interval::boundary_type& x_ub = x_seq_i.upper();
    const typename Interval::boundary_type& y_ub = y_seq_i.upper();
    assert(y_ub <= x_ub);
    if (y_ub < x_ub) {
      Iterator k = std::lower_bound(first, last, x_ub);
      if (k != last) {
	if (x_ub < *k)
	  x_ub = *k;
      }
      else
	x_seq_i.set_upper_unbounded();
    }

    // Lower bound.
    typename Interval::boundary_type& x_lb = x_seq_i.upper();
    const typename Interval::boundary_type& y_lb = y_seq_i.upper();
    assert(y_lb >= x_lb);
    if (y_lb > x_lb) {
      Iterator k = std::lower_bound(first, last, x_lb);
      if (k != last) {
	if (x_lb < *k)
	  if (k != first)
	    x_lb = *--k;
	  else
	    x_seq_i.set_lower_unbounded();
      }
      else
	x_lb = *--k;
    }
  }
  assert(OK());
}

template <typename Interval>
void
Box<Interval>::CC76_widening_assign(const Box& y) {
  static typename Interval::boundary_type stop_points[] = {
    typename Interval::boundary_type(-2),
    typename Interval::boundary_type(-1),
    typename Interval::boundary_type(0),
    typename Interval::boundary_type(1),
    typename Interval::boundary_type(2)
  };
  CC76_widening_assign(y,
		       stop_points,
		       stop_points
		       + sizeof(stop_points)/sizeof(stop_points[0]));
}

template <typename Interval>
Constraint_System
Box<Interval>::constraints() const {
  Constraint_System cs;
  dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    if (is_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (is_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    for (dimension_type k = 0; k < space_dim; ++k) {
      bool closed = false;
      Coefficient n;
      Coefficient d;
      if (get_lower_bound(k, closed, n, d)) {
	if (closed)
	  cs.insert(d*Variable(k) >= n);
	else
	  cs.insert(d*Variable(k) > n);
      }
      if (get_upper_bound(k, closed, n, d)) {
	if (closed)
	  cs.insert(d*Variable(k) <= n);
	else
	  cs.insert(d*Variable(k) < n);
      }
    }
  }
  return cs;
}

/*! \relates Parma_Polyhedra_Library::Box */
template <typename Interval>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Box<Interval>& box) {
  if (box.is_empty()) {
    s << "empty";
    return s;
  }
  const dimension_type dimension = box.space_dimension();
  for (dimension_type k = 0; k < dimension; ++k) {
    bool closed = false;
    Coefficient n;
    Coefficient d;
    if (box.get_lower_bound(k, closed, n, d)) {
      s << (closed ? "[" : "(")
	<< n;
      if (d != 1)
	s << "/" << d;
      s << ", ";
    }
    else
      s << "(-inf, ";
    if (box.get_upper_bound(k, closed, n, d)) {
      s << n;
      if (d != 1)
	s << "/" << d;
      s << (closed ? "]" : ")");
    }
    else
      s << "+inf)";
    s << "\n";
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_templates_hh)
