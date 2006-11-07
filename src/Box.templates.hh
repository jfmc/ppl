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
template <typename Other_Interval>
inline
Box<Interval>::Box(const Box<Other_Interval>& y)
  : seq(y.space_dimension()) {
  if (y.marked_empty())
    set_empty();
  else {
    empty_up_to_date = false;
    for (dimension_type k = y.space_dimension(); k-- > 0; )
      assign(seq[k], y.seq[k]);
  }
  assert(OK());
}

template <typename Interval>
bool
operator==(const Box<Interval>& x, const Box<Interval>& y) {
  const dimension_type x_space_dim = x.space_dimension();
  if (x_space_dim != y.space_dimension())
    return false;

  for (dimension_type k = x_space_dim; k-- > 0; )
    if (x.seq[k] != y.seq[k])
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::OK() const {
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].OK())
      return false;
  return true;
}

template <typename Interval>
dimension_type
Box<Interval>::affine_dimension() const {
  dimension_type d = space_dimension();
  // A zero-space-dim box always has affine dimension zero.
  if (d == 0)
    return 0;

  // An empty box has affine dimension zero.
  if (is_empty())
    return 0;

  for (dimension_type k = d; k-- > 0; )
    if (seq[k].is_singleton())
      --d;

  return d;
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
    x.seq.insert(x.seq.end(), y_space_dim, Interval());
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
inline void
Box<Interval>::add_space_dimensions_and_project(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  const dimension_type old_space_dim = space_dimension();
  add_space_dimensions_and_embed(m);
  for (dimension_type k = m; k-- > 0; )
      assign(seq[old_space_dim+k], 0);

  assert(OK());
}

template <typename Interval>
inline void
Box<Interval>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any box is a no-op.
  // Note that this case also captures the only legal removal of
  // space dimensions from a box in a zero-dimensional space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  const dimension_type old_space_dim = space_dimension();

  // Dimension-compatibility check.
  const dimension_type tbr_space_dim = to_be_removed.space_dimension();
  if (old_space_dim < tbr_space_dim)
    throw_dimension_incompatible("remove_space_dimensions(vs)", tbr_space_dim);

  const dimension_type new_space_dim = old_space_dim - to_be_removed.size();

  // If the box is empty (this must be detected), then resizing is all
  // what is needed.  If it is not empty and we are removing _all_ the
  // dimensions then, again, resizing suffices.
  if (is_empty() || new_space_dim == 0) {
    seq.resize(new_space_dim);
    assert(OK());
    return;
  }

  // For each variable to be removed, we fill the corresponding interval
  // by shifting left those intervals that will not be removed.
  Variables_Set::const_iterator i = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst = i->id();
  dimension_type src = dst + 1;
  for (++i; i != tbr_end; ++i) {
    dimension_type next = i->id();
    // All intervals in between are moved to the left.
    while (src < next)
      seq[dst++].swap(seq[src++]);
    ++src;
  }
  // Moving the remaining intervals.
  while (src < old_space_dim)
    seq[dst++].swap(seq[src++]);

  assert(dst == new_space_dim);
  seq.resize(new_space_dim);

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::remove_higher_space_dimensions(const dimension_type new_dim) {
  // Dimension-compatibility check: the variable having
  // maximum index is the one occurring last in the set.
  if (new_dim > space_dimension())
    throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				 new_dim);

  // The removal of no dimensions from any box is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a zero-dim space box.
  if (new_dim == space_dimension()) {
    assert(OK());
    return;
  }

  seq.erase(seq.begin() + new_dim, seq.end());
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

  Interval& seq_c = seq[c_only_var-1];
  const Constraint::Type c_type = c.type();
  switch (c_type) {
  case Constraint::EQUALITY:
    refine(seq_c, EQUAL, q);
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    refine(seq_c, (d > 0) ? GREATER_THAN_OR_EQUAL : LESS_THAN_OR_EQUAL, q);
    break;
  case Constraint::STRICT_INEQUALITY:
    refine(seq_c, (d > 0) ? GREATER_THAN : LESS_THAN, q);
    break;
  }
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
	x_seq_i.upper_set_unbounded();
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
	    x_seq_i.lower_set_unbounded();
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
  const dimension_type space_dim = space_dimension();
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
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Box& y) const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    dimension_type required_dim)
  const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Constraint& c) const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", c->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>::throw_dimension_incompatible(const char* method,
					    const Generator& g) const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
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
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
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
