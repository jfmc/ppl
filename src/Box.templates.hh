/* Box class implementation: non-inline template functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Box_templates_hh
#define PPL_Box_templates_hh 1

#include "Variables_Set.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Generator_System.defs.hh"
#include "Generator_System.inlines.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "Polyhedron.defs.hh"
#include "Grid.defs.hh"
#include "BD_Shape.defs.hh"
#include "Octagonal_Shape.defs.hh"
#include "MIP_Problem.defs.hh"
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline
Box<Interval>::Box(dimension_type num_dimensions, Degenerate_Element kind)
  : seq(num_dimensions <= max_space_dimension()
	? num_dimensions
	: (throw_space_dimension_overflow("Box(n, k)",
					  "n exceeds the maximum "
					  "allowed space dimension"),
	   num_dimensions)),
	empty(kind == EMPTY), empty_up_to_date(true) {
  // In a box that is marked empty the intervals are completely
  // meaningless: we exploit this by avoiding their initializion.
  if (kind == UNIVERSE)
    for (dimension_type i = num_dimensions; i-- > 0; )
      seq[i].assign(UNIVERSE);
  assert(OK());
}

template <typename Interval>
inline
Box<Interval>::Box(const Constraint_System& cs)
  : seq(cs.space_dimension() <= max_space_dimension()
	? cs.space_dimension()
	: (throw_space_dimension_overflow("Box(cs)",
					  "cs exceeds the maximum "
					  "allowed space dimension"),
	   cs.space_dimension())),
    empty_up_to_date(false) {
  // FIXME: check whether we can avoid the double initialization.
  for (dimension_type i = cs.space_dimension(); i-- > 0; )
    seq[i].assign(UNIVERSE);
  add_constraints_no_check(cs);
}

template <typename Interval>
template <typename Other_Interval>
inline
Box<Interval>::Box(const Box<Other_Interval>& y)
  : seq(y.space_dimension()),
    empty(y.empty),
    empty_up_to_date(y.empty_up_to_date) {
  if (!y.marked_empty())
    for (dimension_type k = y.space_dimension(); k-- > 0; )
      seq[k].assign(y.seq[k]);
  assert(OK());
}

template <typename Interval>
Box<Interval>::Box(const Generator_System& gs)
  : seq(gs.space_dimension() <= max_space_dimension()
	? gs.space_dimension()
	: (throw_space_dimension_overflow("Box(gs)",
					  "gs exceeds the maximum "
					  "allowed space dimension"),
	   gs.space_dimension())),
    empty(false),
    empty_up_to_date(true) {
  const Generator_System::const_iterator gs_begin = gs.begin();
  const Generator_System::const_iterator gs_end = gs.end();
  if (gs_begin == gs_end) {
    // An empty generator system defines the empty box.
    set_empty();
    return;
  }

  const dimension_type space_dim = space_dimension();
  DIRTY_TEMP0(mpq_class, q);
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
	  seq[i].join_assign(q);
	}
      }
      else {
	// This is the first point seen: initialize `seq'.
	point_seen = true;
	for (dimension_type i = space_dim; i-- > 0; ) {
	  assign_r(q.get_num(), g.coefficient(Variable(i)), ROUND_NOT_NEEDED);
	  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
	  q.canonicalize();
	  seq[i].assign(q);
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
	  seq[i].assign(UNIVERSE);
      break;
    case Generator::RAY:
      for (dimension_type i = space_dim; i-- > 0; )
	switch (sgn(g.coefficient(Variable(i)))) {
	case 1:
	  seq[i].upper_set(UNBOUNDED);
	  break;
	case -1:
	  seq[i].lower_set(UNBOUNDED);
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
	  seq_i.lower_widen(q, true);
	  seq_i.upper_widen(q, true);
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
template <typename T>
Box<Interval>::Box(const BD_Shape<T>& bds, Complexity_Class)
  : seq(bds.space_dimension() <= max_space_dimension()
	? bds.space_dimension()
	: (throw_space_dimension_overflow("Box(bds)",
					  "bds exceeds the maximum "
					  "allowed space dimension"),
	   bds.space_dimension())),
    empty(false),
    empty_up_to_date(true) {
  // Expose all the interval constraints.
  bds.shortest_path_closure_assign();
  if (bds.marked_empty()) {
    set_empty();
    assert(OK());
    return;
  }

  const dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    assert(OK());
    return;
  }

  DIRTY_TEMP(typename BD_Shape<T>::coefficient_type, tmp);
  const DB_Row<typename BD_Shape<T>::coefficient_type>& dbm_0 = bds.dbm[0];
  for (dimension_type i = space_dim; i-- > 0; ) {
    Interval& seq_i = seq[i];
    // Set the upper bound.
    const typename BD_Shape<T>::coefficient_type& u = dbm_0[i+1];
    if (is_plus_infinity(u))
      seq_i.upper_set_uninit(UNBOUNDED);
    else
      seq_i.upper_set_uninit(u);

    // Set the lower bound.
    const typename BD_Shape<T>::coefficient_type& negated_l = bds.dbm[i+1][0];
    if (is_plus_infinity(negated_l))
      seq_i.lower_set_uninit(UNBOUNDED);
    else {
      neg_assign_r(tmp, negated_l, ROUND_DOWN);
      seq_i.lower_set_uninit(tmp);
    }

    // Complete the interval initialization.
    seq_i.complete_init();
  }
  assert(OK());
}

template <typename Interval>
template <typename T>
Box<Interval>::Box(const Octagonal_Shape<T>& oct, Complexity_Class)
  : seq(oct.space_dimension() <= max_space_dimension()
	? oct.space_dimension()
	: (throw_space_dimension_overflow("Box(oct)",
					  "oct exceeds the maximum "
					  "allowed space dimension"),
	   oct.space_dimension())),
    empty(false),
    empty_up_to_date(true) {
  // Expose all the interval constraints.
  oct.strong_closure_assign();
  if (oct.marked_empty()) {
    set_empty();
    return;
  }

  const dimension_type space_dim = space_dimension();
  if (space_dim == 0)
    return;

  DIRTY_TEMP0(mpq_class, bound);
  for (dimension_type i = space_dim; i-- > 0; ) {
    Interval& seq_i = seq[i];
    const dimension_type ii = 2*i;
    const dimension_type cii = ii + 1;

    // Set the upper bound.
    const typename Octagonal_Shape<T>::coefficient_type& twice_ub
      = oct.matrix[cii][ii];
    if (!is_plus_infinity(twice_ub)) {
      assign_r(bound, twice_ub, ROUND_NOT_NEEDED);
      div2exp_assign_r(bound, bound, 1, ROUND_NOT_NEEDED);
      seq_i.upper_set_uninit(bound);
    }
    else
      seq_i.upper_set_uninit(UNBOUNDED);

    // Set the lower bound.
    const typename Octagonal_Shape<T>::coefficient_type& twice_lb
      = oct.matrix[ii][cii];
    if (!is_plus_infinity(twice_lb)) {
      assign_r(bound, twice_lb, ROUND_NOT_NEEDED);
      neg_assign_r(bound, bound, ROUND_NOT_NEEDED);
      div2exp_assign_r(bound, bound, 1, ROUND_NOT_NEEDED);
      seq_i.lower_set_uninit(bound);
    }
    else
      seq_i.lower_set_uninit(UNBOUNDED);
    seq_i.complete_init();
  }
}

template <typename Interval>
Box<Interval>::Box(const Polyhedron& ph, Complexity_Class complexity)
  : seq(ph.space_dimension() <= max_space_dimension()
	? ph.space_dimension()
	: (throw_space_dimension_overflow("Box(ph)",
					  "ph exceeds the maximum "
					  "allowed space dimension"),
	   ph.space_dimension())),
    empty(false),
    empty_up_to_date(true) {
  // We do not need to bother about `complexity' if:
  // a) the polyhedron is already marked empty; or ...
  if (ph.marked_empty()) {
    set_empty();
    return;
  }

  // b) the polyhedron is zero-dimensional; or ...
  const dimension_type space_dim = ph.space_dimension();
  if (space_dim == 0)
    return;

  // c) the polyhedron is already described by a generator system.
  if (ph.generators_are_up_to_date() && !ph.has_pending_constraints()) {
    Box tmp(ph.generators());
    swap(tmp);
    return;
  }

  // Here generators are not up-to-date or there are pending constraints.
  assert(ph.constraints_are_up_to_date());

  if (complexity == POLYNOMIAL_COMPLEXITY) {
    // Extract easy-to-find bounds from constraints.
    Box tmp(ph.simplified_constraints(), Recycle_Input());
    swap(tmp);
  }
  else if (complexity == SIMPLEX_COMPLEXITY) {
    MIP_Problem lp(space_dim);
    const Constraint_System& ph_cs = ph.constraints();
    if (!ph_cs.has_strict_inequalities())
      lp.add_constraints(ph_cs);
    else
      // Adding to `lp' a topologically closed version of `ph_cs'.
      for (Constraint_System::const_iterator i = ph_cs.begin(),
	     ph_cs_end = ph_cs.end(); i != ph_cs_end; ++i) {
	const Constraint& c = *i;
	if (c.is_strict_inequality())
	  lp.add_constraint(Linear_Expression(c) >= 0);
	else
	  lp.add_constraint(c);
      }
    // Check for unsatisfiability.
    if (!lp.is_satisfiable()) {
      set_empty();
      return;
    }
    // Get all the bounds for the space dimensions.
    Generator g(point());
    DIRTY_TEMP0(mpq_class, bound);
    DIRTY_TEMP(Coefficient, bound_num);
    DIRTY_TEMP(Coefficient, bound_den);
    for (dimension_type i = space_dim; i-- > 0; ) {
      Interval& seq_i = seq[i];
      lp.set_objective_function(Variable(i));
      // Evaluate upper bound.
      lp.set_optimization_mode(MAXIMIZATION);
      if (lp.solve() == OPTIMIZED_MIP_PROBLEM) {
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, bound_num, bound_den);
	assign_r(bound.get_num(), bound_num, ROUND_NOT_NEEDED);
	assign_r(bound.get_den(), bound_den, ROUND_NOT_NEEDED);
	assert(is_canonical(bound));
	seq_i.upper_set_uninit(bound);
      }
      else
	seq_i.upper_set_uninit(UNBOUNDED);
      // Evaluate optimal lower bound.
      lp.set_optimization_mode(MINIMIZATION);
      if (lp.solve() == OPTIMIZED_MIP_PROBLEM) {
	g = lp.optimizing_point();
	lp.evaluate_objective_function(g, bound_num, bound_den);
	assign_r(bound.get_num(), bound_num, ROUND_NOT_NEEDED);
	assign_r(bound.get_den(), bound_den, ROUND_NOT_NEEDED);
	assert(is_canonical(bound));
	seq_i.lower_set_uninit(bound);
      }
      else
	seq_i.lower_set_uninit(UNBOUNDED);
      seq_i.complete_init();
    }
  }
  else {
    assert(complexity == ANY_COMPLEXITY);
    if (ph.is_empty())
      set_empty();
    else {
      Box tmp(ph.generators());
      swap(tmp);
    }
  }
}

template <typename Interval>
Box<Interval>::Box(const Grid& gr, Complexity_Class)
  : seq(gr.space_dimension() <= max_space_dimension()
	? gr.space_dimension()
	: (throw_space_dimension_overflow("Box(gr)",
					  "gr exceeds the maximum "
					  "allowed space dimension"),
	   gr.space_dimension())),
    empty(false),
    empty_up_to_date(true) {

  // FIXME: here we are not taking advantage of intervals with restrictions!

  if (gr.marked_empty()) {
    set_empty();
    return;
  }

  dimension_type space_dim = gr.space_dimension();

  if (space_dim == 0)
    return;

  if (!gr.generators_are_up_to_date() && !gr.update_generators()) {
    // Updating found the grid empty.
    set_empty();
    return;
  }

  assert(!gr.gen_sys.empty());

  // Create a vector to record which dimensions are bounded.
  std::vector<bool> bounded_interval(space_dim, true);

  const Grid_Generator *first_point = 0;
  // Clear the bound flag in `bounded_interval' for all dimensions in
  // which a line or sequence of points extends away from a single
  // value in the dimension.
  // FIXME: this computation should be provided by the Grid class.
  // FIXME: remove the declaration making Box a friend of Grid_Generator
  //        when this is done.
  for (Grid_Generator_System::const_iterator gs_i = gr.gen_sys.begin(),
	 gs_end = gr.gen_sys.end(); gs_i != gs_end; ++gs_i) {
    Grid_Generator& g = const_cast<Grid_Generator&>(*gs_i);
    if (g.is_point()) {
      if (first_point == 0) {
	first_point = &g;
	continue;
      }
      const Grid_Generator& point = *first_point;
      // Convert the point `g' to a parameter.
      for (dimension_type dim = space_dim; dim-- > 0; )
	g[dim] -= point[dim];
      g.set_divisor(point.divisor());
    }
    for (dimension_type col = space_dim; col > 0; )
      if (g[col--] != 0)
	bounded_interval[col] = false;
  }

  // For each dimension that is bounded by the grid, set both bounds
  // of the interval to the value of the associated coefficient in a
  // generator point.
  assert(first_point != 0);
  const Grid_Generator& point = *first_point;
  DIRTY_TEMP0(mpq_class, bound);
  const Coefficient& divisor = point.divisor();
  for (dimension_type i = space_dim; i-- > 0; ) {
    Interval& seq_i = seq[i];
    if (bounded_interval[i]) {
      assign_r(bound.get_num(), point[i+1], ROUND_NOT_NEEDED);
      assign_r(bound.get_den(), divisor, ROUND_NOT_NEEDED);
      bound.canonicalize();
      seq_i.assign(bound);
    }
    else
      seq_i.assign(UNIVERSE);
  }
}

template <typename Interval>
template <typename D1, typename D2, typename R>
Box<Interval>::Box(const Partially_Reduced_Product<D1, D2, R>& dp,
		   Complexity_Class complexity)
  : seq(dp.space_dimension() <= max_space_dimension()
	? dp.space_dimension()
	: (throw_space_dimension_overflow("Box(dp)",
					  "dp exceeds the maximum "
					  "allowed space dimension"),
	   dp.space_dimension())),
    empty(false),
    empty_up_to_date(true) {
  for (dimension_type i = dp.space_dimension(); i-- > 0; )
    seq[i].assign(UNIVERSE);
  {
    Box tmp(dp.domain1(), complexity);
    intersection_assign(tmp);
  }
  {
    Box tmp(dp.domain2(), complexity);
    intersection_assign(tmp);
  }
}

template <typename Interval>
inline void
Box<Interval>::add_space_dimensions_and_embed(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  // To embed an n-dimension space box in a (n+m)-dimension space,
  // we just add `m' new universe elements to the sequence.
  seq.insert(seq.end(), m, Interval());
  for (dimension_type sz = seq.size(), i = sz - m; i < sz; ++i)
    seq[i].assign(UNIVERSE);
  assert(OK());
}

template <typename Interval>
inline void
Box<Interval>::add_space_dimensions_and_project(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  // A add `m' new zero elements to the sequence.
  seq.insert(seq.end(), m, Interval());
  for (dimension_type sz = seq.size(), i = sz - m; i < sz; ++i)
    seq[i].assign(0);

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
Box<Interval>::bounds(const Linear_Expression& expr,
		      const bool from_above) const {
  // `expr' should be dimension-compatible with `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  const dimension_type space_dim = space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((from_above
				  ? "bounds_from_above(e)"
				  : "bounds_from_below(e)"), "e", expr);
  // A zero-dimensional or empty Box bounds everything.
  if (space_dim == 0 || is_empty())
    return true;

  const int from_above_sign = from_above ? 1 : -1;
  for (dimension_type i = expr_space_dim; i-- > 0; )
    switch (sgn(expr.coefficient(Variable(i))) * from_above_sign) {
    case 1:
      if (seq[i].upper_is_unbounded())
	return false;
      break;
    case 0:
      // Nothing to do.
      break;
    case -1:
      if (seq[i].lower_is_unbounded())
	return false;
      break;
    }
  return true;
}

template <typename Interval>
Poly_Con_Relation
Box<Interval>::relation_with(const Constraint& c) const {
  const dimension_type c_space_dim = c.space_dimension();
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (c_space_dim > space_dim)
    throw_dimension_incompatible("relation_with(c)", c);

  dimension_type c_num_vars = 0;
  dimension_type c_only_var = 0;
  // Constraints that are not interval constraints are illegal.
  if (!extract_interval_constraint(c, c_space_dim, c_num_vars, c_only_var))
    throw_constraint_incompatible("relation_with(c)");

  if (is_empty())
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0) {
    if ((c.is_equality() && c.inhomogeneous_term() != 0)
	|| (c.is_inequality() && c.inhomogeneous_term() < 0))
      return Poly_Con_Relation::is_disjoint();
    else if (c.is_strict_inequality() && c.inhomogeneous_term() == 0)
      // The constraint 0 > 0 implicitly defines the hyperplane 0 = 0;
      // thus, the zero-dimensional point also saturates it.
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_disjoint();
    else if (c.is_equality() || c.inhomogeneous_term() == 0)
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included();
    else
      // The zero-dimensional point saturates
      // neither the positivity constraint 1 >= 0,
      // nor the strict positivity constraint 1 > 0.
      return Poly_Con_Relation::is_included();
  }

  if (c_num_vars == 0) {
    // Dealing with a trivial constraint.
    switch (sgn(c.inhomogeneous_term())) {
    case -1:
      return Poly_Con_Relation::is_disjoint();
    case 0:
      if (c.is_strict_inequality())
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_disjoint();
      else
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_included();
    case 1:
      return Poly_Con_Relation::is_included();
    }
  }

  // Here constraint `c' is a non-trivial interval constraint
  // and the box is not empty.
  assert(c_num_vars == 1);
  const Interval& seq_var = seq[c_only_var];
  if (seq_var.is_universe())
    return Poly_Con_Relation::strictly_intersects();

  DIRTY_TEMP0(mpq_class, c_bound);
  assign_r(c_bound.get_num(), c.inhomogeneous_term(), ROUND_NOT_NEEDED);
  const Coefficient& d = c.coefficient(Variable(c_only_var));
  assign_r(c_bound.get_den(), d, ROUND_NOT_NEEDED);
  c_bound.canonicalize();
  neg_assign_r(c_bound, c_bound, ROUND_NOT_NEEDED);
  const bool c_is_lower_bound = (d > 0);

  DIRTY_TEMP0(mpq_class, bound_diff);
  if (c.is_equality()) {
    if (seq_var.lower_is_unbounded()) {
      assert(!seq_var.upper_is_unbounded());
      assign_r(bound_diff, seq_var.upper(), ROUND_NOT_NEEDED);
      sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
      switch (sgn(bound_diff)) {
      case 1:
	return Poly_Con_Relation::strictly_intersects();
      case 0:
	return seq_var.upper_is_open()
	  ? Poly_Con_Relation::is_disjoint()
	  : Poly_Con_Relation::strictly_intersects();
      case -1:
	return Poly_Con_Relation::is_disjoint();
      }
    }
    else {
      assign_r(bound_diff, seq_var.lower(), ROUND_NOT_NEEDED);
      sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
      switch (sgn(bound_diff)) {
      case 1:
	return Poly_Con_Relation::is_disjoint();
      case 0:
	if (seq_var.lower_is_open())
	  return Poly_Con_Relation::is_disjoint();
	else {
	  Poly_Con_Relation result = Poly_Con_Relation::is_included();
	  if (seq_var.is_singleton())
	    result = result && Poly_Con_Relation::saturates();
	  return result;
	}
      case -1:
	if (seq_var.upper_is_unbounded())
	  return Poly_Con_Relation::is_included();
	else {
	  assign_r(bound_diff, seq_var.upper(), ROUND_NOT_NEEDED);
	  sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
	  switch (sgn(bound_diff)) {
	  case 1:
	    return Poly_Con_Relation::strictly_intersects();
	  case 0:
	    if (seq_var.upper_is_open())
	      return Poly_Con_Relation::is_disjoint();
	    else
	      return Poly_Con_Relation::strictly_intersects();
	  case -1:
	    return Poly_Con_Relation::is_disjoint();
	  }
	}
      }
    }
  }

  assert(!c.is_equality());
  if (c_is_lower_bound) {
    if (seq_var.lower_is_unbounded()) {
      assert(!seq_var.upper_is_unbounded());
      assign_r(bound_diff, seq_var.upper(), ROUND_NOT_NEEDED);
      sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
      switch (sgn(bound_diff)) {
      case 1:
	return Poly_Con_Relation::strictly_intersects();
      case 0:
	if (c.is_strict_inequality() || seq_var.upper_is_open())
	  return Poly_Con_Relation::is_disjoint();
	else
	  return Poly_Con_Relation::strictly_intersects();
      case -1:
	return Poly_Con_Relation::is_disjoint();
      }
    }
    else {
      assign_r(bound_diff, seq_var.lower(), ROUND_NOT_NEEDED);
      sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
      switch (sgn(bound_diff)) {
      case 1:
	return Poly_Con_Relation::is_included();
      case 0:
	if (c.is_nonstrict_inequality() || seq_var.lower_is_open()) {
	  Poly_Con_Relation result = Poly_Con_Relation::is_included();
	  if (seq_var.is_singleton())
	    result = result && Poly_Con_Relation::saturates();
	  return result;
	}
	else {
	  assert(c.is_strict_inequality() && !seq_var.lower_is_open());
	  if (seq_var.is_singleton())
	    return Poly_Con_Relation::is_disjoint()
	      && Poly_Con_Relation::saturates();
	  else
	    return Poly_Con_Relation::strictly_intersects();
	}
	break;
      case -1:
	if (seq_var.upper_is_unbounded())
	  return Poly_Con_Relation::strictly_intersects();
	else {
	  assign_r(bound_diff, seq_var.upper(), ROUND_NOT_NEEDED);
	  sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
	  switch (sgn(bound_diff)) {
	  case 1:
	    return Poly_Con_Relation::strictly_intersects();
	  case 0:
	    if (c.is_strict_inequality() || seq_var.upper_is_open())
	      return Poly_Con_Relation::is_disjoint();
	    else
	      return Poly_Con_Relation::strictly_intersects();
	  case -1:
	    return Poly_Con_Relation::is_disjoint();
	  }
	}
      }
    }
  }
  else {
    // `c' is an upper bound.
    if (seq_var.upper_is_unbounded())
      return Poly_Con_Relation::strictly_intersects();
    else {
      assign_r(bound_diff, seq_var.upper(), ROUND_NOT_NEEDED);
      sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
      switch (sgn(bound_diff)) {
      case -1:
	return Poly_Con_Relation::is_included();
      case 0:
	if (c.is_nonstrict_inequality() || seq_var.upper_is_open()) {
	  Poly_Con_Relation result = Poly_Con_Relation::is_included();
	  if (seq_var.is_singleton())
	    result = result && Poly_Con_Relation::saturates();
	  return result;
	}
	else {
	  assert(c.is_strict_inequality() && !seq_var.upper_is_open());
	  if (seq_var.is_singleton())
	    return Poly_Con_Relation::is_disjoint()
	      && Poly_Con_Relation::saturates();
	  else
	    return Poly_Con_Relation::strictly_intersects();
	}
	break;
      case 1:
	if (seq_var.lower_is_unbounded())
	  return Poly_Con_Relation::strictly_intersects();
	else {
	  assign_r(bound_diff, seq_var.lower(), ROUND_NOT_NEEDED);
	  sub_assign_r(bound_diff, bound_diff, c_bound, ROUND_NOT_NEEDED);
	  switch (sgn(bound_diff)) {
	  case -1:
	    return Poly_Con_Relation::strictly_intersects();
	  case 0:
	    if (c.is_strict_inequality() || seq_var.lower_is_open())
	      return Poly_Con_Relation::is_disjoint();
	    else
	      return Poly_Con_Relation::strictly_intersects();
	  case 1:
	    return Poly_Con_Relation::is_disjoint();
	  }
	}
      }
    }
  }
  // Quiet a compiler warning: this program point is unreachable.
  throw std::runtime_error("PPL internal error");
}

template <typename Interval>
Poly_Gen_Relation
Box<Interval>::relation_with(const Generator& g) const {
  const dimension_type space_dim = space_dimension();
  const dimension_type g_space_dim = g.space_dimension();

  // Dimension-compatibility check.
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("relation_with(g)", g);

  // The empty box cannot subsume a generator.
  if (is_empty())
    return Poly_Gen_Relation::nothing();

  // A universe box in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();

  if (g.is_line_or_ray()) {
    if (g.is_line()) {
      for (dimension_type i = g_space_dim; i-- > 0; )
	if (g.coefficient(Variable(i)) != 0 && !seq[i].is_universe())
	  return Poly_Gen_Relation::nothing();
      return Poly_Gen_Relation::subsumes();
    }
    else {
      assert(g.is_ray());
      for (dimension_type i = g_space_dim; i-- > 0; )
	switch (sgn(g.coefficient(Variable(i)))) {
	case 1:
	  if (!seq[i].upper_is_unbounded())
	    return Poly_Gen_Relation::nothing();
	  break;
	case 0:
	  break;
	case -1:
	  if (!seq[i].lower_is_unbounded())
	    return Poly_Gen_Relation::nothing();
	  break;
	}
      return Poly_Gen_Relation::subsumes();
    }
  }

  // Here `g' is a point or closure point.
  const Coefficient& g_divisor = g.divisor();
  DIRTY_TEMP0(mpq_class, g_coord);
  DIRTY_TEMP0(mpq_class, bound);
  for (dimension_type i = g_space_dim; i-- > 0; ) {
    const Interval& seq_i = seq[i];
    if (seq_i.is_universe())
      continue;
    assign_r(g_coord.get_num(), g.coefficient(Variable(i)), ROUND_NOT_NEEDED);
    assign_r(g_coord.get_den(), g_divisor, ROUND_NOT_NEEDED);
    g_coord.canonicalize();
    // Check lower bound.
    if (!seq_i.lower_is_unbounded()) {
      assign_r(bound, seq_i.lower(), ROUND_NOT_NEEDED);
      if (g_coord <= bound) {
	if (seq_i.lower_is_open()) {
	  if (g.is_point() || g_coord != bound)
	    return Poly_Gen_Relation::nothing();
	}
	else if (g_coord != bound)
	  return Poly_Gen_Relation::nothing();
      }
    }
    // Check upper bound.
    if (!seq_i.upper_is_unbounded()) {
      assign_r(bound, seq_i.upper(), ROUND_NOT_NEEDED);
      if (g_coord >= bound) {
	if (seq_i.upper_is_open()) {
	  if (g.is_point() || g_coord != bound)
	    return Poly_Gen_Relation::nothing();
	}
	else if (g_coord != bound)
	  return Poly_Gen_Relation::nothing();
      }
    }
  }
  return Poly_Gen_Relation::subsumes();
}


template <typename Interval>
bool
Box<Interval>::max_min(const Linear_Expression& expr,
		       const bool maximize,
		       Coefficient& ext_n, Coefficient& ext_d,
		       bool& included) const {
  // `expr' should be dimension-compatible with `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((maximize
				  ? "maximize(e, ...)"
				  : "minimize(e, ...)"), "e", expr);
  // Deal with zero-dim Box first.
  if (space_dim == 0)
    if (marked_empty())
      return false;
    else {
      ext_n = expr.inhomogeneous_term();
      ext_d = 1;
      included = true;
      return true;
    }

  // For an empty Box we simply return false.
  if (is_empty())
    return false;

  DIRTY_TEMP0(mpq_class, result);
  assign_r(result, expr.inhomogeneous_term(), ROUND_NOT_NEEDED);
  bool is_included = true;
  const int maximize_sign = maximize ? 1 : -1;
  DIRTY_TEMP0(mpq_class, bound_i);
  DIRTY_TEMP0(mpq_class, expr_i);
  for (dimension_type i = expr_space_dim; i-- > 0; ) {
    const Interval& seq_i = seq[i];
    assign_r(expr_i, expr.coefficient(Variable(i)), ROUND_NOT_NEEDED);
    switch (sgn(expr_i) * maximize_sign) {
    case 1:
      if (seq_i.upper_is_unbounded())
	return false;
      assign_r(bound_i, seq_i.upper(), ROUND_NOT_NEEDED);
      add_mul_assign_r(result, bound_i, expr_i, ROUND_NOT_NEEDED);
      if (seq_i.upper_is_open())
	is_included = false;
      break;
    case 0:
      // Nothing to do.
      break;
    case -1:
      if (seq_i.lower_is_unbounded())
	return false;
      assign_r(bound_i, seq_i.lower(), ROUND_NOT_NEEDED);
      add_mul_assign_r(result, bound_i, expr_i, ROUND_NOT_NEEDED);
      if (seq_i.lower_is_open())
	is_included = false;
      break;
    }
  }
  // Extract output info.
  assert(is_canonical(result));
  ext_n = result.get_num();
  ext_d = result.get_den();
  included = is_included;
  return true;
}

template <typename Interval>
bool
Box<Interval>::max_min(const Linear_Expression& expr,
		       const bool maximize,
		       Coefficient& ext_n, Coefficient& ext_d,
		       bool& included,
		       Generator& g) const {
  if (!max_min(expr, maximize, ext_n, ext_d, included))
    return false;

  // Compute generator `g'.
  Linear_Expression g_expr;
  DIRTY_TEMP(Coefficient, g_divisor);
  g_divisor = 1;
  const int maximize_sign = maximize ? 1 : -1;
  DIRTY_TEMP0(mpq_class, g_coord);
  DIRTY_TEMP(Coefficient, num);
  DIRTY_TEMP(Coefficient, den);
  DIRTY_TEMP(Coefficient, lcm);
  DIRTY_TEMP(Coefficient, factor);
  for (dimension_type i = space_dimension(); i-- > 0; ) {
    const Interval& seq_i = seq[i];
    switch (sgn(expr.coefficient(Variable(i))) * maximize_sign) {
    case 1:
      assign_r(g_coord, seq_i.upper(), ROUND_NOT_NEEDED);
      break;
    case 0:
      // If 0 belongs to the interval, choose it
      // (and directly proceed to the next iteration).
      // FIXME: name qualification issue.
      if (seq_i.contains(0))
	continue;
      if (!seq_i.lower_is_unbounded())
	if (seq_i.lower_is_open())
	  if (!seq_i.upper_is_unbounded())
	    if (seq_i.upper_is_open()) {
	      // Bounded and open interval: compute middle point.
	      assign_r(g_coord, seq_i.lower(), ROUND_NOT_NEEDED);
	      DIRTY_TEMP0(mpq_class, q_seq_i_upper);
	      assign_r(q_seq_i_upper, seq_i.upper(), ROUND_NOT_NEEDED);
	      g_coord += q_seq_i_upper;
	      g_coord /= 2;
	    }
	    else
	      // The upper bound is in the interval.
	      assign_r(g_coord, seq_i.upper(), ROUND_NOT_NEEDED);
	  else {
	    // Lower is open, upper is unbounded.
	    assign_r(g_coord, seq_i.lower(), ROUND_NOT_NEEDED);
	    ++g_coord;
	  }
	else
	  // The lower bound is in the interval.
	  assign_r(g_coord, seq_i.lower(), ROUND_NOT_NEEDED);
      else {
	// Lower is unbounded, hence upper is bounded
	// (since we know that 0 does not belong to the interval).
	assert(!seq_i.upper_is_unbounded());
	assign_r(g_coord, seq_i.upper(), ROUND_NOT_NEEDED);
	if (seq_i.upper_is_open())
	  --g_coord;
      }
      break;
    case -1:
      assign_r(g_coord, seq_i.lower(), ROUND_NOT_NEEDED);
      break;
    }
    // Add g_coord * Variable(i) to the generator.
    assign_r(den, g_coord.get_den(), ROUND_NOT_NEEDED);
    lcm_assign(lcm, g_divisor, den);
    exact_div_assign(factor, lcm, g_divisor);
    g_expr *= factor;
    exact_div_assign(factor, lcm, den);
    assign_r(num, g_coord.get_num(), ROUND_NOT_NEEDED);
    num *= factor;
    g_expr += num * Variable(i);
    g_divisor = lcm;
  }
  g = Generator::point(g_expr, g_divisor);
  return true;
}

template <typename Interval>
bool
Box<Interval>::contains(const Box& y) const {
  const Box& x = *this;
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    x.throw_dimension_incompatible("contains(y)", y);

  // If `y' is empty, then `x' contains `y'.
  if (y.is_empty())
    return true;

  // If `x' is empty, then `x' cannot contain `y'.
  if (x.is_empty())
    return false;

  for (dimension_type k = x.seq.size(); k-- > 0; )
    // FIXME: fix this name qualification issue.
    if (!x.seq[k].contains(y.seq[k]))
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_disjoint_from(const Box& y) const {
  const Box& x = *this;
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    x.throw_dimension_incompatible("is_disjoint_from(y)", y);

  // If any of `x' or `y' is marked empty, then they are disjoint.
  // Note: no need to use `is_empty', as the following loop is anyway correct.
  if (x.marked_empty() || y.marked_empty())
    return true;

  for (dimension_type k = x.seq.size(); k-- > 0; )
    // FIXME: fix this name qualification issue.
    if (x.seq[k].is_disjoint_from(y.seq[k]))
      return true;
  return false;
}

template <typename Interval>
bool
Box<Interval>::OK() const {
  if (empty_up_to_date && !empty) {
    Box tmp = *this;
    tmp.empty_up_to_date = false;
    if (tmp.check_empty()) {
#ifndef NDEBUG
      std::cerr << "The box is empty, but it is marked as non-empty."
		<< std::endl;
#endif // NDEBUG
      return false;
    }
  }

  // A box that is not marked empty must have meaningful intervals.
  if (!empty_up_to_date || !empty) {
    for (dimension_type k = seq.size(); k-- > 0; )
      if (!seq[k].OK())
	return false;
  }

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
  if (marked_empty())
    return false;
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
    if (!seq[k].topologically_closed())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_discrete() const {
  if (is_empty())
    return true;
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].is_singleton())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::is_bounded() const {
  if (is_empty())
    return true;
  for (dimension_type k = seq.size(); k-- > 0; )
    if (seq[k].is_unbounded())
      return false;
  return true;
}

template <typename Interval>
bool
Box<Interval>::contains_integer_point() const {
  if (marked_empty())
    return false;
  for (dimension_type k = seq.size(); k-- > 0; )
    if (!seq[k].contains_integer_point())
      return false;
  return true;
}

template <typename Interval>
void
Box<Interval>::intersection_assign(const Box& y) {
  Box& x = *this;
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    x.throw_dimension_incompatible("intersection_assign(y)", y);

  // If one of the two boxes is empty, the intersection is empty.
  if (x.marked_empty())
    return;
  if (y.marked_empty()) {
    x.set_empty();
    return;
  }

  // If both boxes are zero-dimensional, then at this point they are
  // necessarily non-empty, so that their intersection is non-empty too.
  if (space_dim == 0)
    return;

  // FIXME: here we may conditionally exploit a capability of the
  // underlying Interval to eagerly detect empty results.
  empty_up_to_date = false;

  for (dimension_type k = space_dim; k-- > 0; )
    x.seq[k].intersect_assign(y.seq[k]);

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
    x.seq[k].join_assign(y.seq[k]);

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

  Box& x = *this;
  if (x.is_empty() || y.is_empty())
    return;

  // If `x' is zero-dimensional, then at this point both `x' and `y'
  // are the universe box, so that their difference is empty.
  if (space_dim == 0) {
    x.set_empty();
    return;
  }

  dimension_type index_non_contained = space_dim;
  dimension_type number_non_contained = 0;
  for (dimension_type i = space_dim; i-- > 0; )
    if (!y.seq[i].contains(x.seq[i]))
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
void
Box<Interval>::time_elapse_assign(const Box& y) {
  Box& x = *this;
  const dimension_type x_space_dim = x.space_dimension();

  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    x.throw_dimension_incompatible("time_elapse_assign(y)", y);

  // Dealing with the zero-dimensional case.
  if (x_space_dim == 0) {
    if (y.marked_empty())
      x.set_empty();
    return;
  }

  // If either one of `x' or `y' is empty, the result is empty too.
  // Note: if possible, avoid cost of checking for emptiness.
  if (x.marked_empty() || y.marked_empty()
      || x.is_empty() || y.is_empty()) {
    x.set_empty();
    return;
  }

  for (dimension_type i = x_space_dim; i-- > 0; ) {
    Interval& x_seq_i = x.seq[i];
    const Interval& y_seq_i = y.seq[i];
    if (!x_seq_i.lower_is_unbounded())
      if (y_seq_i.lower_is_unbounded() || y_seq_i.lower() < 0)
	x_seq_i.lower_set(UNBOUNDED);
    if (!x_seq_i.upper_is_unbounded())
      if (y_seq_i.upper_is_unbounded() || y_seq_i.upper() > 0)
	x_seq_i.upper_set(UNBOUNDED);
  }
  assert(x.OK());
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
    throw_dimension_incompatible("remove_space_dimensions(vs)",
				 tbr_space_dim);

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
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst = *tbr;
  dimension_type src = dst + 1;
  for (++tbr; tbr != tbr_end; ++tbr) {
    const dimension_type tbr_next = *tbr;
    // All intervals in between are moved to the left.
    while (src < tbr_next)
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
  const dimension_type old_dim = space_dimension();
  if (new_dim > old_dim)
    throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				 new_dim);

  // The removal of no dimensions from any box is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a zero-dim space box.
  if (new_dim == old_dim) {
    assert(OK());
    return;
  }

  seq.erase(seq.begin() + new_dim, seq.end());
  assert(OK());
}

template <typename Interval>
template <typename Partial_Function>
void
Box<Interval>::map_space_dimensions(const Partial_Function& pfunc) {
  const dimension_type space_dim = space_dimension();
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the box becomes zero_dimensional.
    remove_higher_space_dimensions(0);
    return;
  }

  const dimension_type new_space_dim = pfunc.max_in_codomain() + 1;
  // If the box is empty, then simply adjust the space dimension.
  if (is_empty()) {
    remove_higher_space_dimensions(new_space_dim);
    return;
  }

  // We create a new Box with the new space dimension.
  Box<Interval> tmp(new_space_dim);
  // Map the intervals, exchanging the indexes.
  for (dimension_type i = 0; i < space_dim; ++i) {
    dimension_type new_i;
    if (pfunc.maps(i, new_i))
      seq[i].swap(tmp.seq[new_i]);
  }
  swap(tmp);
  assert(OK());
}

template <typename Interval>
void
Box<Interval>::fold_space_dimensions(const Variables_Set& to_be_folded,
				     const Variable var) {
  const dimension_type space_dim = space_dimension();
  // `var' should be one of the dimensions of the box.
  if (var.space_dimension() > space_dim)
    throw_dimension_incompatible("fold_space_dimensions(tbf, v)", "v", var);

  // The folding of no dimensions is a no-op.
  if (to_be_folded.empty())
    return;

  // All variables in `to_be_folded' should be dimensions of the box.
  if (to_be_folded.space_dimension() > space_dim)
    throw_dimension_incompatible("fold_space_dimensions(tbf, ...)",
				 to_be_folded.space_dimension());

  // Moreover, `var.id()' should not occur in `to_be_folded'.
  if (to_be_folded.find(var.id()) != to_be_folded.end())
    throw_generic("fold_space_dimensions(tbf, v)",
		  "v should not occur in tbf");

  // Note: the check for emptiness is needed for correctness.
  if (!is_empty()) {
    // Join the interval corresponding to variable `var' with the intervals
    // corresponding to the variables in `to_be_folded'.
    Interval& seq_v = seq[var.id()];
    for (Variables_Set::const_iterator i = to_be_folded.begin(),
	   tbf_end = to_be_folded.end(); i != tbf_end; ++i)
      seq_v.join_assign(seq[*i]);
  }
  remove_space_dimensions(to_be_folded);
}

template <typename Interval>
void
Box<Interval>::add_constraint_no_check(const Constraint& c) {
  const dimension_type c_space_dim = c.space_dimension();
  assert(c_space_dim <= space_dimension());

  dimension_type c_num_vars = 0;
  dimension_type c_only_var = 0;
  // Constraints that are not interval constraints are ignored.
  if (!extract_interval_constraint(c, c_space_dim, c_num_vars, c_only_var))
    return;

  if (c_num_vars == 0) {
    // Dealing with a trivial constraint.
    if ((c.is_equality() && c.inhomogeneous_term() != 0)
	|| c.inhomogeneous_term() < 0)
      set_empty();
    return;
  }

  assert(c_num_vars == 1);
  const Coefficient& d = c.coefficient(Variable(c_only_var));
  const Coefficient& n = c.inhomogeneous_term();
  // The constraint `c' is of the form
  // `Variable(c_only_var-1) + n / d rel 0', where
  // `rel' is either the relation `==', `>=', or `>'.
  // For the purpose of refining intervals, this is
  // (morally) turned into `Variable(c_only_var-1) rel -n/d'.
  DIRTY_TEMP0(mpq_class, q);
  assign_r(q.get_num(), n, ROUND_NOT_NEEDED);
  assign_r(q.get_den(), d, ROUND_NOT_NEEDED);
  q.canonicalize();
  // Turn `n/d' into `-n/d'.
  q = -q;

  Interval& seq_c = seq[c_only_var];
  const Constraint::Type c_type = c.type();
  switch (c_type) {
  case Constraint::EQUALITY:
    seq_c.refine_existential(EQUAL, q);
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    seq_c.refine_existential((d > 0) ? GREATER_OR_EQUAL : LESS_OR_EQUAL, q);
    // FIXME: this assertion fails due to a bug in refine.
    assert(seq_c.OK());
    break;
  case Constraint::STRICT_INEQUALITY:
    seq_c.refine_existential((d > 0) ? GREATER_THAN : LESS_THAN, q);
    break;
  }
  // FIXME: do check the value returned by `refine' and
  // set `empty' and `empty_up_to_date' as appropriate.
  empty_up_to_date = false;
  assert(OK());
}

template <typename Interval>
void
Box<Interval>::add_constraints_no_check(const Constraint_System& cs) {
  assert(cs.space_dimension() <= space_dimension());
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i) {
    add_constraint_no_check(*i);
    if (is_empty())
      return;
  }
  assert(OK());
}

#if 1
namespace {

inline bool
refine_no_check_check_result(Result r, Ternary& open) {
  switch (r) {
  case V_NEG_OVERFLOW:
  case V_POS_OVERFLOW:
  case V_UNKNOWN_NEG_OVERFLOW:
  case V_UNKNOWN_POS_OVERFLOW:
    return true;
  case V_LT:
  case V_GT:
    open = T_YES;
    return false;
  case V_LE:
  case V_GE:
    if (open == T_NO)
      open = T_MAYBE;
    return false;
  case V_EQ:
    return false;
  default:
    assert(false);
    return true;
  }
}

} // namespace

template <typename Interval>
void
Box<Interval>::refine_no_check(const Constraint& c) {
  assert(c.space_dimension() <= space_dimension());

  typedef
    typename Select_Temp_Boundary_Type<typename Interval::boundary_type>::type
    Temp_Boundary_Type;

  dimension_type c_space_dim = c.space_dimension();
  Constraint::Type c_type = c.type();
  const Coefficient& c_inhomogeneous_term = c.inhomogeneous_term();
  Result r;
  Temp_Boundary_Type t_bound;
  Temp_Boundary_Type t_a;
  Temp_Boundary_Type t_x;
  Ternary open;
  for (dimension_type k = c_space_dim; k-- > 0; ) {
    const Coefficient& a_k = c.coefficient(Variable(k));
    int sgn_a_k = sgn(a_k);
    if (sgn_a_k == 0)
      continue;
    if (sgn_a_k > 0) {
      open = (c_type == Constraint::STRICT_INEQUALITY) ? T_YES : T_NO;
      if (open == T_NO)
	maybe_reset_fpu_inexact<Temp_Boundary_Type>();
      r = assign_r(t_bound, c_inhomogeneous_term, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_1;
      r = neg_assign_r(t_bound, t_bound, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_1;
      for (dimension_type i = c_space_dim; i-- > 0; ) {
	if (i == k)
	  continue;
	const Coefficient& a_i = c.coefficient(Variable(i));
	int sgn_a_i = sgn(a_i);
	if (sgn_a_i == 0)
	  continue;
	Interval& x_i = seq[i];
	if (sgn_a_i < 0) {
	  if (x_i.lower_is_unbounded())
	    goto maybe_refine_upper_1;
	  r = assign_r(t_a, a_i, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	  r = assign_r(t_x, x_i.lower(), ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	  if (x_i.lower_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	}
	else {
	  assert(sgn_a_i > 0);
	  if (x_i.upper_is_unbounded())
	    goto maybe_refine_upper_1;
	  r = assign_r(t_a, a_i, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	  r = assign_r(t_x, x_i.upper(), ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	  if (x_i.upper_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_1;
	}
      }
      r = assign_r(t_a, a_k, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_1;
      r = div_assign_r(t_bound, t_bound, t_a, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_1;

      // Refine the lower bound of `seq[k]' with `t_bound'.
      if (open == T_MAYBE
	  && maybe_check_fpu_inexact<Temp_Boundary_Type>() == 1)
	open = T_YES;
      seq[k].lower_narrow(t_bound, open == T_YES);
      empty_up_to_date = false;
    maybe_refine_upper_1:
      if (c_type != Constraint::EQUALITY)
	continue;
      open = T_NO;
      maybe_reset_fpu_inexact<Temp_Boundary_Type>();
      r = assign_r(t_bound, c_inhomogeneous_term, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      r = neg_assign_r(t_bound, t_bound, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      for (dimension_type i = c_space_dim; i-- > 0; ) {
	if (i == k)
	  continue;
	const Coefficient& a_i = c.coefficient(Variable(i));
	int sgn_a_i = sgn(a_i);
	if (sgn_a_i == 0)
	  continue;
	Interval& x_i = seq[i];
	if (sgn_a_i < 0) {
	  if (x_i.upper_is_unbounded())
	    goto next_k;
	  r = assign_r(t_a, a_i, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  r = assign_r(t_x, x_i.upper(), ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  if (x_i.upper_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	}
	else {
	  assert(sgn_a_i > 0);
	  if (x_i.lower_is_unbounded())
	    goto next_k;
	  r = assign_r(t_a, a_i, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  r = assign_r(t_x, x_i.lower(), ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  if (x_i.lower_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	}
      }
      r = assign_r(t_a, a_k, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      r = div_assign_r(t_bound, t_bound, t_a, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto next_k;

      // Refine the upper bound of seq[k] with t_bound.
      if (open == T_MAYBE
	  && maybe_check_fpu_inexact<Temp_Boundary_Type>() == 1)
	open = T_YES;
      seq[k].upper_narrow(t_bound, open == T_YES);
      empty_up_to_date = false;
    }
    else {
      assert(sgn_a_k < 0);
      open = (c_type == Constraint::STRICT_INEQUALITY) ? T_YES : T_NO;
      if (open == T_NO)
	maybe_reset_fpu_inexact<Temp_Boundary_Type>();
      r = assign_r(t_bound, c_inhomogeneous_term, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_2;
      r = neg_assign_r(t_bound, t_bound, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_2;
      for (dimension_type i = c_space_dim; i-- > 0; ) {
	if (i == k)
	  continue;
	const Coefficient& a_i = c.coefficient(Variable(i));
	int sgn_a_i = sgn(a_i);
	if (sgn_a_i == 0)
	  continue;
	Interval& x_i = seq[i];
	if (sgn_a_i < 0) {
	  if (x_i.lower_is_unbounded())
	    goto maybe_refine_upper_2;
	  r = assign_r(t_a, a_i, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	  r = assign_r(t_x, x_i.lower(), ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	  if (x_i.lower_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	}
	else {
	  assert(sgn_a_i > 0);
	  if (x_i.upper_is_unbounded())
	    goto maybe_refine_upper_2;
	  r = assign_r(t_a, a_i, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	  r = assign_r(t_x, x_i.upper(), ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	  if (x_i.upper_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto maybe_refine_upper_2;
	}
      }
      r = assign_r(t_a, a_k, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_2;
      r = div_assign_r(t_bound, t_bound, t_a, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto maybe_refine_upper_2;

      // Refine the upper bound of seq[k] with t_bound.
      if (open == T_MAYBE
	  && maybe_check_fpu_inexact<Temp_Boundary_Type>() == 1)
	open = T_YES;
      seq[k].upper_narrow(t_bound, open == T_YES);
      empty_up_to_date = false;
    maybe_refine_upper_2:
      if (c_type != Constraint::EQUALITY)
	continue;
      open = T_NO;
      maybe_reset_fpu_inexact<Temp_Boundary_Type>();
      r = assign_r(t_bound, c_inhomogeneous_term, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      r = neg_assign_r(t_bound, t_bound, ROUND_UP);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      for (dimension_type i = c_space_dim; i-- > 0; ) {
	if (i == k)
	  continue;
	const Coefficient& a_i = c.coefficient(Variable(i));
	int sgn_a_i = sgn(a_i);
	if (sgn_a_i == 0)
	  continue;
	Interval& x_i = seq[i];
	if (sgn_a_i < 0) {
	  if (x_i.upper_is_unbounded())
	    goto next_k;
	  r = assign_r(t_a, a_i, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  r = assign_r(t_x, x_i.upper(), ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  if (x_i.upper_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	}
	else {
	  assert(sgn_a_i > 0);
	  if (x_i.lower_is_unbounded())
	    goto next_k;
	  r = assign_r(t_a, a_i, ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  r = assign_r(t_x, x_i.lower(), ROUND_DOWN);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	  if (x_i.lower_is_open())
	    open = T_YES;
	  r = sub_mul_assign_r(t_bound, t_a, t_x, ROUND_UP);
	  if (refine_no_check_check_result(r, open))
	    goto next_k;
	}
      }
      r = assign_r(t_a, a_k, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto next_k;
      r = div_assign_r(t_bound, t_bound, t_a, ROUND_DOWN);
      if (refine_no_check_check_result(r, open))
	goto next_k;

      // Refine the lower bound of seq[k] with t_bound.
      if (open == T_MAYBE
	  && maybe_check_fpu_inexact<Temp_Boundary_Type>() == 1)
	open = T_YES;
      seq[k].lower_narrow(t_bound, open == T_YES);
      empty_up_to_date = false;
    }
  next_k:
    ;
  }
}

#else

template <typename Interval>
void
Box<Interval>::refine_no_check(const Constraint& c) {
  assert(c.space_dimension() <= space_dimension());

  dimension_type c_space_dim = c.space_dimension();
  Interval k[c_space_dim];
  Interval p[c_space_dim];
  for (dimension_type i = c_space_dim; i-- > 0; ) {
    k[i] = c.coefficient(Variable(i));
    Interval& p_i = p[i];
    p_i = seq[i];
    p_i.mul_assign(p_i, k[i]);
  }
  const Coefficient& inhomogeneous_term = c.inhomogeneous_term();
  for (dimension_type i = c_space_dim; i-- > 0; ) {
    int sgn_coefficient_i = sgn(c.coefficient(Variable(i)));
    if (sgn_coefficient_i == 0)
      continue;
    Interval q(inhomogeneous_term);
    for (dimension_type j = c_space_dim; j-- > 0; ) {
      if (i == j)
	continue;
      q.add_assign(q, p[j]);
    }
    q.div_assign(q, k[i]);
    q.neg_assign(q);
    Relation_Symbol rel;
    switch (c.type()) {
    case Constraint::EQUALITY:
      rel = EQUAL;
      break;
    case Constraint::NONSTRICT_INEQUALITY:
      rel = (sgn_coefficient_i > 0) ? GREATER_OR_EQUAL : LESS_OR_EQUAL;
      break;
    case Constraint::STRICT_INEQUALITY:
      rel = (sgn_coefficient_i > 0) ? GREATER_THAN : LESS_THAN;
      break;
    }
    seq[i].refine_existential(rel, q);
    // FIXME: could/should we exploit the return value of refine_existential,
    //        in case it is available?
    // FIMXE: should we instead be lazy and do not even bother about
    //        the possibility the interval becomes empty apart from setting
    //        empty_up_to_date = false?
    if (seq[i].is_empty()) {
      set_empty();
      break;
    }
  }

  assert(OK());
}

#endif

template <typename Interval>
void
Box<Interval>::refine_no_check(const Constraint_System& cs) {
  assert(cs.space_dimension() <= space_dimension());

  bool changed;
  do {
    Sequence copy(seq);
    for (Constraint_System::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i)
      refine_no_check(*i);

    // Check if the client has requested abandoning all expensive
    // computations.  If so, the exception specified by the client
    // is thrown now.
    maybe_abandon();

    changed = (copy != seq);
  } while (changed);
}

template <typename Interval>
void
Box<Interval>::affine_image(const Variable var,
			    const Linear_Expression& expr,
			    Coefficient_traits::const_reference denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "v", var);

  if (is_empty())
    return;

  Tmp_Interval_Type expr_value, temp0, temp1;
  expr_value.assign(expr.inhomogeneous_term());
  for (dimension_type i = expr_space_dim; i-- > 0; ) {
    const Coefficient& coeff = expr.coefficient(Variable(i));
    if (coeff != 0) {
      temp0.assign(coeff);
      temp1.assign(seq[i]);
      temp0.mul_assign(temp0, temp1);
      expr_value.add_assign(expr_value, temp0);
    }
  }
  if (denominator != 1) {
    temp0.assign(denominator);
    expr_value.div_assign(expr_value, temp0);
  }
  seq[var.id()].assign(expr_value);

  assert(OK());
}

template <typename Interval>
void
Box<Interval>::affine_preimage(const Variable var,
			       const Linear_Expression& expr,
			       Coefficient_traits::const_reference
			       denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  const dimension_type x_space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (x_space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type var_space_dim = var.space_dimension();
  if (x_space_dim < var_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "v", var);

  if (is_empty())
    return;

  const Coefficient& expr_v = expr.coefficient(var);
  const bool invertible = (expr_v != 0);
  if (!invertible) {
    Tmp_Interval_Type expr_value, temp0, temp1;
    expr_value.assign(expr.inhomogeneous_term());
    for (dimension_type i = expr_space_dim; i-- > 0; ) {
      const Coefficient& coeff = expr.coefficient(Variable(i));
      if (coeff != 0) {
	temp0.assign(coeff);
	temp1.assign(seq[i]);
	temp0.mul_assign(temp0, temp1);
	expr_value.add_assign(expr_value, temp0);
      }
    }
    if (denominator != 1) {
      temp0.assign(denominator);
      expr_value.div_assign(expr_value, temp0);
    }
    Interval& x_seq_v = seq[var.id()];
    expr_value.intersect_assign(x_seq_v);
    if (expr_value.is_empty())
      set_empty();
    else
      x_seq_v.assign(UNIVERSE);
  }
  else {
    // The affine transformation is invertible.
    // CHECKME: for efficiency, would it be meaningful to avoid
    // the computation of inverse by partially evaluating the call
    // to affine_image?
    Linear_Expression inverse;
    inverse -= expr;
    inverse += (expr_v + denominator) * var;
    affine_image(var, inverse, expr_v);
  }
  assert(OK());
}

template <typename Interval>
template <typename Iterator>
void
Box<Interval>::CC76_widening_assign(const Box& y,
				    Iterator first, Iterator last) {
  if (y.is_empty())
    return;

  Box& x = *this;
  for (dimension_type i = x.seq.size(); i-- > 0; ) {
    Interval& x_seq_i = x.seq[i];
    const Interval& y_seq_i = y.seq[i];

    // Upper bound.
    if (!x_seq_i.upper_is_unbounded()) {
      typename Interval::boundary_type& x_ub = x_seq_i.upper();
      const typename Interval::boundary_type& y_ub = y_seq_i.upper();
      assert(!y_seq_i.upper_is_unbounded() && y_ub <= x_ub);
      if (y_ub < x_ub) {
	Iterator k = std::lower_bound(first, last, x_ub);
	if (k != last) {
	  if (x_ub < *k)
	    x_ub = *k;
	}
	else
	  x_seq_i.upper_set(UNBOUNDED);
      }
    }

    // Lower bound.
    if (!x_seq_i.lower_is_unbounded()) {
      typename Interval::boundary_type& x_lb = x_seq_i.lower();
      const typename Interval::boundary_type& y_lb = y_seq_i.lower();
      assert(!y_seq_i.lower_is_unbounded() && y_lb >= x_lb);
      if (y_lb > x_lb) {
	Iterator k = std::lower_bound(first, last, x_lb);
	if (k != last) {
	  if (x_lb < *k)
	    if (k != first)
	      x_lb = *--k;
	    else
	      x_seq_i.lower_set(UNBOUNDED);
	}
	else
	  x_lb = *--k;
      }
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
void
Box<Interval>::limited_CC76_extrapolation_assign(const Box& y,
						 const Constraint_System& cs,
						 unsigned* tp) {
  // FIXME: should take into account cs.
  Box& x = *this;
  x.CC76_widening_assign(y, tp);
}

template <typename Interval>
void
Box<Interval>::CC76_narrowing_assign(const Box& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("CC76_narrowing_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that `*this' is contained in or equal to `y'.
    const Box x_copy = *this;
    const Box y_copy = y;
    assert(y_copy.contains(x_copy));
  }
#endif

  // If both boxes are zero-dimensional,
  // since `y' contains `*this', we simply return `*this'.
  if (space_dim == 0)
    return;

  // If `y' is empty, since `y' contains `this', `*this' is empty too.
  if (y.is_empty())
    return;
  // If `*this' is empty, we return.
  if (is_empty())
    return;

  // Replace each constraint in `*this' by the corresponding constraint
  // in `y' if the corresponding inhomogeneous terms are both finite.
  for (dimension_type i = space_dim; i-- > 0; ) {
    Interval& x_i = seq[i];
    const Interval& y_i = y.seq[i];
    if (!x_i.lower_is_unbounded()
	&& !y_i.lower_is_unbounded()
	&& x_i.lower() != y_i.lower())
      x_i.lower() = y_i.lower();
    if (!x_i.upper_is_unbounded()
	&& !y_i.upper_is_unbounded()
	&& x_i.upper() != y_i.upper())
      x_i.upper() = y_i.upper();
  }
  assert(OK());
}

template <typename Interval>
Constraint_System
Box<Interval>::constraints() const {
  Constraint_System cs;
  const dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    if (marked_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (marked_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    for (dimension_type k = 0; k < space_dim; ++k) {
      bool closed = false;
      DIRTY_TEMP(Coefficient, n);
      DIRTY_TEMP(Coefficient, d);
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

template <typename Interval>
Constraint_System
Box<Interval>::minimized_constraints() const {
  Constraint_System cs;
  const dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    if (marked_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  // Make sure emptyness is detected.
  else if (is_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    for (dimension_type k = 0; k < space_dim; ++k) {
      bool closed = false;
      DIRTY_TEMP(Coefficient, n);
      DIRTY_TEMP(Coefficient, d);
      if (get_lower_bound(k, closed, n, d)) {
	if (closed)
	  // Make sure equality constraints are detected.
	  if (seq[k].is_singleton()) {
	    cs.insert(d*Variable(k) == n);
	    continue;
	  }
	  else
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
  if (box.is_empty())
    s << "false";
  else if (box.is_universe())
    s << "true";
  else
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
  const char separator = ' ';
  s << "empty" << separator << (empty ? '1' : '0');
  s << separator;
  s << "empty_up_to_date" << separator << (empty_up_to_date ? '1' : '0');
  s << separator;
  const dimension_type space_dim = space_dimension();
  s << "space_dim" << separator << space_dim;
  s << "\n";
  for (dimension_type i = 0; i < space_dim;  ++i)
    seq[i].ascii_dump(s);
}

PPL_OUTPUT_TEMPLATE_DEFINITIONS(Interval, Box<Interval>)

template <typename Interval>
bool
Box<Interval>::ascii_load(std::istream& s) {
  std::string str;

  bool flag;
  if (!(s >> str) || str != "empty")
    return false;
  if (!(s >> flag))
    return false;
  empty = flag;
  if (!(s >> str) || str != "empty_up_to_date")
    return false;
  if (!(s >> flag))
    return false;
  empty_up_to_date = flag;

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
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << this->space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename Interval>
void
Box<Interval>
::throw_dimension_incompatible(const char* method,
			       dimension_type required_dim) const {
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
Box<Interval>
::throw_dimension_incompatible(const char* method,
			       const Constraint_System& cs) const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", cs->space_dimension == " << cs.space_dimension() << ".";
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
					    const Linear_Expression& e) const {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << e.space_dimension() << ".";
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

template <typename Interval>
void
Box<Interval>::throw_space_dimension_overflow(const char* method,
					      const char* reason) {
  std::ostringstream s;
  s << "PPL::Box::" << method << ":" << std::endl
    << reason;
  throw std::length_error(s.str());
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \relates Box */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename Specialization,
	  typename Temp, typename To, typename Interval>
bool
l_m_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
		    const Box<Interval>& x, const Box<Interval>& y,
		    const Rounding_Dir dir,
		    Temp& tmp0, Temp& tmp1, Temp& tmp2) {
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // Zero-dim boxes are equal if and only if they are both empty or universe.
  if (x_space_dim == 0) {
    if (x.marked_empty() == y.marked_empty())
      assign_r(r, 0, ROUND_NOT_NEEDED);
    else
      assign_r(r, PLUS_INFINITY, ROUND_NOT_NEEDED);
    return true;
  }

  // The distance computation requires a check for emptiness.
  (void) x.is_empty();
  (void) y.is_empty();
  // If one of two boxes is empty, then they are equal if and only if
  // the other box is empty too.
  if (x.marked_empty() || y.marked_empty())
    if (x.marked_empty() == y.marked_empty()) {
      assign_r(r, 0, ROUND_NOT_NEEDED);
      return true;
    }
    else
      goto pinf;

  assign_r(tmp0, 0, ROUND_NOT_NEEDED);
  for (dimension_type i = x_space_dim; i-- > 0; ) {
    const Interval& x_i = x.seq[i];
    const Interval& y_i = y.seq[i];
    // Dealing with the lower bounds.
    if (x_i.lower_is_unbounded())
      if (y_i.lower_is_unbounded())
	continue;
      else
	goto pinf;
    else if (y_i.lower_is_unbounded())
      goto pinf;
    else {
      const Temp* tmp1p;
      const Temp* tmp2p;
      if (x_i.lower() > y_i.lower()) {
	maybe_assign(tmp1p, tmp1, x_i.lower(), dir);
	maybe_assign(tmp2p, tmp2, y_i.lower(), inverse(dir));
      }
      else {
	maybe_assign(tmp1p, tmp1, y_i.lower(), dir);
	maybe_assign(tmp2p, tmp2, x_i.lower(), inverse(dir));
      }
      sub_assign_r(tmp1, *tmp1p, *tmp2p, dir);
      assert(sgn(tmp1) >= 0);
      Specialization::combine(tmp0, tmp1, dir);
    }
    // Dealing with the lower bounds.
    if (x_i.upper_is_unbounded())
      if (y_i.upper_is_unbounded())
	continue;
      else
	goto pinf;
    else if (y_i.upper_is_unbounded())
      goto pinf;
    else {
      const Temp* tmp1p;
      const Temp* tmp2p;
      if (x_i.upper() > y_i.upper()) {
	maybe_assign(tmp1p, tmp1, x_i.upper(), dir);
	maybe_assign(tmp2p, tmp2, y_i.upper(), inverse(dir));
      }
      else {
	maybe_assign(tmp1p, tmp1, y_i.upper(), dir);
	maybe_assign(tmp2p, tmp2, x_i.upper(), inverse(dir));
      }
      sub_assign_r(tmp1, *tmp1p, *tmp2p, dir);
      assert(sgn(tmp1) >= 0);
      Specialization::combine(tmp0, tmp1, dir);
    }
  }
  Specialization::finalize(tmp0, dir);
  assign_r(r, tmp0, dir);
  return true;

 pinf:
  assign_r(r, PLUS_INFINITY, ROUND_NOT_NEEDED);
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_templates_hh)
