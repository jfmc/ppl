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

#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename Interval>
template <typename Other_Interval>
inline
Box<Interval>::Box(const Box<Other_Interval>& y)
  : seq(y.space_dimension()) {
  Box& x = *this;
  if (y.marked_empty())
    x.set_empty();
  else {
    x.empty_up_to_date = false;
    for (dimension_type k = y.space_dimension(); k-- > 0; )
      assign(x.seq[k], y.seq[k]);
  }
  assert(x.OK());
}

template <typename Interval>
Box<Interval>::Box(const Generator_System& gs)
  : seq(gs.space_dimension()), empty(false), empty_up_to_date(true) {
  const Generator_System::const_iterator gs_begin = gs.begin();
  const Generator_System::const_iterator gs_end = gs.end();
  if (gs_begin == gs_end) {
    // An empty generator system defines the empty box.
    set_empty();
    return;
  }

  const dimension_type space_dim = space_dimension();
  mpq_class q;

  bool point_seen = false;
  // Going through all the points.
  for (Generator_System::const_iterator
	 gs_i = gs_begin; gs_i != gs_end; ++gs_i) {
    const Generator& g = *gs_i;
    if (g.is_point()) {
      const Coefficient& d = g.divisor();
      if (point_seen) {
	// This is not the first point: `seq' already contains valid values.
	for (dimension_type i = space_dim; i-- > 0; ) {
	  assign_r(q.get_num(), g.coefficient(Variable(i)), ROUND_NOT_NEEDED);
	  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
	  q.canonicalize();
	  join_assign(seq[i], q);
	}
      }
      else {
	// This is the first point seen: initialize `seq'.
	point_seen = true;
	for (dimension_type i = space_dim; i-- > 0; ) {
	  assign_r(q.get_num(), g.coefficient(Variable(i)), ROUND_NOT_NEEDED);
	  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
	  q.canonicalize();
	  assign(seq[i], q);
	}
      }
    }
  }

  if (!point_seen)
    // The generator system is not empty, but contains no points.
    throw std::invalid_argument("PPL::Box<Interval>::Box(gs):\n"
				"the non-empty generator system gs "
				"contains no points.");

  // Going through all the lines, rays and closure points.
  Interval q_interval;
  for (Generator_System::const_iterator gs_i = gs_begin;
       gs_i != gs_end; ++gs_i) {
    const Generator& g = *gs_i;
    switch (g.type()) {
    case Generator::LINE:
      for (dimension_type i = space_dim; i-- > 0; )
	if (g.coefficient(Variable(i)) != 0)
	  seq[i].set_universe();
      break;
    case Generator::RAY:
      for (dimension_type i = space_dim; i-- > 0; )
	switch (sgn(g.coefficient(Variable(i)))) {
	case 1:
	  seq[i].upper_set_unbounded();
	  break;
	case -1:
	  seq[i].lower_set_unbounded();
	  break;
	default:
	  break;
	}
      break;
    case Generator::CLOSURE_POINT:
      {
	const Coefficient& d = g.divisor();
	for (dimension_type i = space_dim; i-- > 0; ) {
	  assign_r(q.get_num(), g.coefficient(Variable(i)), ROUND_NOT_NEEDED);
	  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
	  q.canonicalize();
	  Interval& seq_i = seq[i];
	  if (!seq_i.upper_is_unbounded()) {
	    const typename Interval::boundary_type& upper_i = seq_i.upper();
	    q_interval.set_universe();
	    refine_existential(q_interval, LESS_THAN, q);
	    if (upper_i < upper(q_interval)) {
	      refine_existential(q_interval, GREATER_THAN_OR_EQUAL, upper_i);
	      join_assign(seq_i, q_interval);
	    }
	  }
	  if (!seq_i.lower_is_unbounded()) {
	    const typename Interval::boundary_type& lower_i = seq_i.lower();
	    q_interval.set_universe();
	    refine_existential(q_interval, GREATER_THAN, q);
	    if (lower_i > lower(q_interval)) {
	      refine_existential(q_interval, LESS_THAN_OR_EQUAL, lower_i);
	      join_assign(seq_i, q_interval);
	    }
	  }
	}
      }
      break;
    default:
      // Points already dealt with.
      break;
    }
  }
  assert(OK());
}

template <typename Interval>
bool
operator==(const Box<Interval>& x, const Box<Interval>& y) {
  const dimension_type x_space_dim = x.space_dimension();
  if (x_space_dim != y.space_dimension())
    return false;

  if (x.is_empty())
    return y.is_empty();

  if (y.is_empty())
    return x.is_empty();

  for (dimension_type k = x_space_dim; k-- > 0; )
    if (x.seq[k] != y.seq[k])
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::contains(const Box<Interval>& y) const {
  const Box& x = *this;
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    x.throw_dimension_incompatible("contains(y)", y);

  // If `y' is empty, then `x' contains `y'.
  if (y.is_empty())
    return true;

  for (dimension_type k = x.seq.size(); k-- > 0; )
    // FIXME: fix this name qualification issue.
    if (!Parma_Polyhedra_Library::contains(x.seq[k], y.seq[k]))
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::OK() const {
  const Box& x = *this;
  if (x.empty_up_to_date && !x.empty) {
    Box tmp = x;
    tmp.empty_up_to_date = false;
    if (tmp.check_empty()) {
#ifndef NDEBUG
      std::cerr << "The box is empty, but it is marked as non-empty."
		<< std::endl;
#endif // NDEBUG
      return false;
    }
  }
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (!x.seq[k].OK())
      return false;
  return true;
}

template <typename Interval>
dimension_type
Box<Interval>::affine_dimension() const {
  const Box& x = *this;
  dimension_type d = x.space_dimension();
  // A zero-space-dim box always has affine dimension zero.
  if (d == 0)
    return 0;

  // An empty box has affine dimension zero.
  if (x.is_empty())
    return 0;

  for (dimension_type k = d; k-- > 0; )
    if (x.seq[k].is_singleton())
      --d;

  return d;
}

template <typename Interval>
bool
Box<Interval>::check_empty() const {
  const Box& x = *this;
  assert(!x.empty_up_to_date);
  x.empty_up_to_date = true;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (x.seq[k].is_empty()) {
      x.empty = true;
      return true;
    }
  x.empty = false;
  return false;
}

template <typename Interval>
bool
Box<Interval>::is_universe() const {
  const Box& x = *this;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (!x.seq[k].is_universe())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_topologically_closed() const {
  const Box& x = *this;
  if (x.is_empty())
    return true;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (!x.seq[k].topologically_closed())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_discrete() const {
  const Box& x = *this;
  if (x.is_empty())
    return true;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (!x.seq[k].is_singleton())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_bounded() const {
  const Box& x = *this;
  if (x.is_empty())
    return true;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (x.seq[k].is_unbounded())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::contains_integer_point() const {
  const Box& x = *this;
  for (dimension_type k = x.seq.size(); k-- > 0; )
    if (!x.seq[k].contains_integer_point())
      return false;
  return true;
}

template <typename Interval>
void
Box<Interval>::intersection_assign(const Box& y) {
  Box& x = *this;
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    x.throw_dimension_incompatible("intersection_assign(y)", y);

  // If one of the two boxes is empty, the intersection is empty.
  if (x.marked_empty())
    return;
  if (y.marked_empty()) {
    x.set_empty();
    return;
  }

  for (dimension_type k = x.seq.size(); k-- > 0; )
    intersect_assign(x.seq[k], y.seq[k]);

  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::box_hull_assign(const Box& y) {
  Box& x = *this;
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    x.throw_dimension_incompatible("box_hull_assign(y)", y);

  // The hull of a box with an empty box is equal to the first box.
  if (y.marked_empty())
    return;
  if (x.marked_empty()) {
    x = y;
    return;
  }

  for (dimension_type k = x.seq.size(); k-- > 0; )
    join_assign(x.seq[k], y.seq[k]);

  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::concatenate_assign(const Box& y) {
  Box& x = *this;
  const dimension_type x_space_dim = x.space_dimension();
  const dimension_type y_space_dim = y.space_dimension();

  // If `y' is an empty 0-dim space box let `*this' become empty.
  if (y_space_dim == 0 && y.marked_empty()) {
    x.set_empty();
    return;
  }

  // If `x' is an empty 0-dim space box, then it is sufficient to adjust
  // the dimension of the vector space.
  if (x_space_dim == 0 && x.marked_empty()) {
    x.seq.insert(x.seq.end(), y_space_dim, Interval());
    assert(x.OK());
    return;
  }

  x.seq.reserve(x_space_dim + y_space_dim);
  std::copy(y.seq.begin(), y.seq.end(),
	    std::back_insert_iterator<Sequence>(x.seq));

  if (x.marked_empty() && !y.marked_empty())
    x.empty_up_to_date = false;

  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::box_difference_assign(const Box& y) {
  const dimension_type space_dim = space_dimension();
  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("box_difference_assign(y)", y);

  Box new_box(space_dim, EMPTY);

  Box& x = *this;
  if (x.is_empty() || y.is_empty())
    return;

  // If `x' is zero-dimensional, then at this point both `x' and `y'
  // are th euniverse box, so that their difference is empty.
  if (space_dim == 0) {
    x.set_empty();
    return;
  }

  dimension_type index_non_contained = space_dim;
  dimension_type number_non_contained = 0;
  for (dimension_type i = space_dim; i-- > 0; )
    if (!Parma_Polyhedra_Library::contains(y.seq[i], x.seq[i]))
      if (++number_non_contained == 1)
	index_non_contained = i;
      else
	break;

  switch (number_non_contained) {
  case 0:
    // `y' covers `x': the difference is empty.
    x.set_empty();
    break;
  case 1:
//     Parma_Polyhedra_Library::difference_assign(x.seq[index_non_contained],
// 					       y.seq[index_non_contained]);
    break;
  default:
    // Nothing to do: the difference is `x'.
    break;
  }
  assert(OK());
}

template <typename Interval>
inline void
Box<Interval>::add_space_dimensions_and_project(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  Box& x = *this;
  const dimension_type old_space_dim = x.space_dimension();
  x.add_space_dimensions_and_embed(m);
  for (dimension_type k = m; k-- > 0; )
    assign(x.seq[old_space_dim+k], 0);

  assert(x.OK());
}

template <typename Interval>
inline void
Box<Interval>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  Box& x = *this;
  // The removal of no dimensions from any box is a no-op.
  // Note that this case also captures the only legal removal of
  // space dimensions from a box in a zero-dimensional space.
  if (to_be_removed.empty()) {
    assert(x.OK());
    return;
  }

  const dimension_type old_space_dim = x.space_dimension();

  // Dimension-compatibility check.
  const dimension_type tbr_space_dim = to_be_removed.space_dimension();
  if (old_space_dim < tbr_space_dim)
    x.throw_dimension_incompatible("remove_space_dimensions(vs)",
				   tbr_space_dim);

  const dimension_type new_space_dim = old_space_dim - to_be_removed.size();

  // If the box is empty (this must be detected), then resizing is all
  // what is needed.  If it is not empty and we are removing _all_ the
  // dimensions then, again, resizing suffices.
  if (x.is_empty() || new_space_dim == 0) {
    x.seq.resize(new_space_dim);
    assert(x.OK());
    return;
  }

  // For each variable to be removed, we fill the corresponding interval
  // by shifting left those intervals that will not be removed.
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst = *tbr + 1;
  dimension_type src = dst + 1;
  for (++tbr; tbr != tbr_end; ++tbr) {
    dimension_type tbr_next = *tbr + 1;
    // All intervals in between are moved to the left.
    while (src < tbr_next)
      x.seq[dst++].swap(x.seq[src++]);
    ++src;
  }
  // Moving the remaining intervals.
  while (src < old_space_dim)
    x.seq[dst++].swap(x.seq[src++]);

  assert(dst == new_space_dim);
  x.seq.resize(new_space_dim);

  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::remove_higher_space_dimensions(const dimension_type new_dim) {
  Box& x = *this;
  // Dimension-compatibility check: the variable having
  // maximum index is the one occurring last in the set.
  const dimension_type old_dim = x.space_dimension();
  if (new_dim > old_dim)
    x.throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				   new_dim);

  // The removal of no dimensions from any box is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a zero-dim space box.
  if (new_dim == old_dim) {
    assert(x.OK());
    return;
  }

  x.seq.erase(x.seq.begin() + new_dim, x.seq.end());
  assert(x.OK());
}

template <typename Interval>
template <typename Partial_Function>
void
Box<Interval>::map_space_dimensions(const Partial_Function& pfunc) {
  Box& x = *this;
  const dimension_type space_dim = x.space_dimension();
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the Box becomes zero_dimensional.
    x.remove_higher_space_dimensions(0);
    return;
  }

  const dimension_type new_space_dim = pfunc.max_in_codomain() + 1;
  // If the Box is empty, then simply adjust the space dimension.
  if (x.is_empty()) {
    x.remove_higher_space_dimensions(new_space_dim);
    return;
  }

  // We create a new Box with the new space dimension.
  Box<Interval> new_x(new_space_dim);
  // Map the intervals, exchanging the indexes.
  for (dimension_type i = 0; i < space_dim; ++i) {
    dimension_type new_i;
    if (pfunc.maps(i, new_i))
      x.seq[i].swap(new_x.seq[new_i]);
  }
  x.swap(new_x);
  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::add_constraint(const Constraint& c) {
  using Implementation::BD_Shapes::div_round_up;

  Box& x = *this;
  const dimension_type c_space_dim = c.space_dimension();
  // Dimension-compatibility check.
  if (c_space_dim > x.space_dimension())
    x.throw_dimension_incompatible("add_constraint(c)", c);

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
      x.set_empty();
    return;
  }

  assert(c_num_vars == 1);
  const Coefficient& d = c.coefficient(Variable(c_only_var-1));
  const Coefficient& n = c.inhomogeneous_term();
  // The constraint `c' is of the form
  // `Variable(c_only_var) + n / d rel 0', where
  // `rel' is either the relation `==', `>=', or `>'.
  // For the purpose of refining intervals, this is
  // (morally) turned into `Variable(c_only_var) rel -n/d'.
  mpq_class q;
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // Turn `n/d' into `-n/d'.
  q = -q;

  Interval& seq_c = x.seq[c_only_var-1];
  const Constraint::Type c_type = c.type();
  switch (c_type) {
  case Constraint::EQUALITY:
    refine_existential(seq_c, EQUAL, q);
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    refine_existential(seq_c, (d > 0) ? GREATER_THAN_OR_EQUAL : LESS_THAN_OR_EQUAL, q);
    // FIXME: this assertion fails due to a bug in refine.
    assert(seq_c.OK());
    break;
  case Constraint::STRICT_INEQUALITY:
    refine_existential(seq_c, (d > 0) ? GREATER_THAN : LESS_THAN, q);
    break;
  }
  // FIXME: do check the value returned by `refine' and
  // set `empty' and `empty_up_to_date' as appropriate.
  x.empty_up_to_date = false;
  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::add_constraints(const Constraint_System& cs) {
  Box& x = *this;
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i)
    x.add_constraint(*i);
  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::affine_image(const Variable var,
			    const Linear_Expression& expr,
			    Coefficient_traits::const_reference denominator) {
  Box& x = *this;
  // The denominator cannot be zero.
  if (denominator == 0)
    x.throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  const dimension_type x_space_dim = x.space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (x_space_dim < expr_space_dim)
    x.throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type var_space_dim = var.space_dimension();
  if (x_space_dim < var_space_dim)
    x.throw_dimension_incompatible("affine_image(v, e, d)", "v", var);

  if (x.is_empty())
    return;

  Interval expr_value;
  assign(expr_value, expr.inhomogeneous_term());
  Interval temp;
  for (dimension_type i = expr_space_dim; i-- > 0; ) {
    const Coefficient& coeff = expr.coefficient(Variable(i));
    if (coeff != 0) {
      assign(temp, coeff);
      mul_assign(temp, temp, x.seq[i]);
      add_assign(expr_value, expr_value, temp);
    }
  }
  if (denominator != 1) {
    assign(temp, denominator);
    div_assign(expr_value, expr_value, temp);
  }
  std::swap(x.seq[var.id()], expr_value);

  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::affine_preimage(const Variable var,
			       const Linear_Expression& expr,
			       Coefficient_traits::const_reference
			       denominator) {
  Box& x = *this;
  // The denominator cannot be zero.
  if (denominator == 0)
    x.throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  const dimension_type x_space_dim = x.space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (x_space_dim < expr_space_dim)
    x.throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type var_space_dim = var.space_dimension();
  if (x_space_dim < var_space_dim)
    x.throw_dimension_incompatible("affine_preimage(v, e, d)", "v", var);

  if (x.is_empty())
    return;

  const Coefficient& expr_v = expr.coefficient(var);
  const bool invertible = (expr_v != 0);
  if (!invertible) {
    Interval expr_value;
    assign(expr_value, expr.inhomogeneous_term());
    Interval temp;
    for (dimension_type i = expr_space_dim; i-- > 0; ) {
      const Coefficient& coeff = expr.coefficient(Variable(i));
      if (coeff != 0) {
	assign(temp, coeff);
	mul_assign(temp, temp, x.seq[i]);
	add_assign(expr_value, expr_value, temp);
      }
    }
    if (denominator != 1) {
      assign(temp, denominator);
      div_assign(expr_value, expr_value, temp);
    }
    Interval& x_seq_v = x.seq[var.id()];
    intersect_assign(temp, x_seq_v);
    if (temp.is_empty())
      x.set_empty();
    else {
      x_seq_v.upper_set_unbounded();
      x_seq_v.lower_set_unbounded();
    }
  }
  else {
    // The affine transformation is invertible.
    // CHECKME: for efficiency, would it be meaningful to avoid
    // the computation of inverse by partially evaluating the call
    // to affine_image?
    Linear_Expression inverse;
    inverse -= expr;
    inverse += (expr_v + denominator) * var;
    x.affine_image(var, inverse, expr_v);
  }
  assert(x.OK());
}

template <typename Interval>
template <typename Iterator>
void
Box<Interval>::CC76_widening_assign(const Box& y,
				    Iterator first, Iterator last) {
  Box& x = *this;
  for (dimension_type i = x.seq.size(); i-- > 0; ) {
    Interval& x_seq_i = x.seq[i];
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
	x_seq_i.upper_set_unbounded();
    }

    // Lower bound.
    typename Interval::boundary_type& x_lb = x_seq_i.lower();
    const typename Interval::boundary_type& y_lb = y_seq_i.lower();
    assert(y_lb >= x_lb);
    if (y_lb > x_lb) {
      Iterator k = std::lower_bound(first, last, x_lb);
      if (k != last) {
	if (x_lb < *k)
	  if (k != first)
	    x_lb = *--k;
	  else
	    x_seq_i.lower_set_unbounded();
      }
      else
	x_lb = *--k;
    }
  }
  assert(x.OK());
}

template <typename Interval>
void
Box<Interval>::CC76_widening_assign(const Box& y, unsigned* tp) {
  static typename Interval::boundary_type stop_points[] = {
    typename Interval::boundary_type(-2),
    typename Interval::boundary_type(-1),
    typename Interval::boundary_type(0),
    typename Interval::boundary_type(1),
    typename Interval::boundary_type(2)
  };

  Box& x = *this;
  // If there are tokens available, work on a temporary copy.
  if (tp != 0 && *tp > 0) {
    Box<Interval> x_tmp(x);
    x_tmp.CC76_widening_assign(y, 0);
    // If the widening was not precise, use one of the available tokens.
    if (!x.contains(x_tmp))
      --(*tp);
    return;
  }
  x.CC76_widening_assign(y,
			 stop_points,
			 stop_points
			 + sizeof(stop_points)/sizeof(stop_points[0]));
}

template <typename Interval>
Constraint_System
Box<Interval>::constraints() const {
  const Box& x = *this;
  Constraint_System cs;
  const dimension_type space_dim = x.space_dimension();
  if (space_dim == 0) {
    if (x.is_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (x.is_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    for (dimension_type k = 0; k < space_dim; ++k) {
      bool closed = false;
      Coefficient n;
      Coefficient d;
      if (x.get_lower_bound(k, closed, n, d)) {
	if (closed)
	  cs.insert(d*Variable(k) >= n);
	else
	  cs.insert(d*Variable(k) > n);
      }
      if (x.get_upper_bound(k, closed, n, d)) {
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
    s << "false";
    return s;
  }
  for (dimension_type k = 0,
	 space_dim = box.space_dimension(); k < space_dim; ) {
    s << Variable(k) << " in " << box[k];
    ++k;
    if (k < space_dim)
      s << ", ";
    else
      break;
  }
  return s;
}

template <typename Interval>
void
Box<Interval>::ascii_dump(std::ostream& s) const {
  const Box& x = *this;
  const char separator = ' ';
  s << "empty" << separator << x.empty;
  s << separator;
  s << "empty_up_to_date" << separator << x.empty_up_to_date;
  s << separator;
  const dimension_type space_dim = x.space_dimension();
  s << "space_dim" << separator << space_dim;
  s << "\n";
  for (dimension_type i = 0; i < space_dim;  ++i)
    x.seq[i].ascii_dump(s);
}

PPL_OUTPUT_TEMPLATE_DEFINITIONS(Interval, Box<Interval>)

template <typename Interval>
bool
Box<Interval>::ascii_load(std::istream& s) {
  Box& x = *this;
  std::string str;

  bool flag;
  if (!(s >> str) || str != "empty")
    return false;
  if (!(s >> flag))
    return false;
  x.empty = flag;
  if (!(s >> str) || str != "empty_up_to_date")
    return false;
  if (!(s >> flag))
    return false;
  x.empty_up_to_date = flag;

  dimension_type space_dim;
  if (!(s >> str) || str != "space_dim")
    return false;
  if (!(s >> space_dim))
    return false;

  seq.clear();
  Interval seq_i;
  for (dimension_type i = 0; i < space_dim;  ++i) {
    if (seq_i.ascii_load(s))
      seq.push_back(seq_i);
    else
      return false;
  }

  // Check invariants.
  assert(OK());
  return true;
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Box& y) const {
  const Box& x = *this;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>
::throw_dimension_incompatible(const char* method,
			       dimension_type required_dim) const {
  const Box& x = *this;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Constraint& c) const {
  const Box& x = *this;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", c->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Generator& g) const {
  const Box& x = *this;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", g->space_dimension == " << g.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_constraint_incompatible(const char* method) {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "the constraint is incompatible.";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_expression_too_complex(const char* method,
					    const Linear_Expression& e) {
  using namespace IO_Operators;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << e << " is too complex.";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const char* name_row,
					    const Linear_Expression& y) const {
  const Box& x = *this;
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_generic(const char* method, const char* reason) {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << reason;
  throw std::invalid_argument(s.str());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_templates_hh)
