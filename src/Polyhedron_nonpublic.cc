/* Polyhedron class implementation
   (non-inline private or protected functions).
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "Polyhedron.defs.hh"

#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

#define BE_LAZY

namespace PPL = Parma_Polyhedra_Library;

PPL::Polyhedron::Polyhedron(Topology topol,
			    dimension_type num_dimensions,
			    Degenerate_Kind kind)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  if (kind == EMPTY)
    status.set_empty();
  else
    if (num_dimensions > 0) {
      add_low_level_constraints(con_sys);
      con_sys.adjust_topology_and_dimension(topol, num_dimensions);
      // CHECK ME.
      // The constraint system is in the minimal form.
      set_constraints_minimized();
    }
  space_dim = num_dimensions;
  assert(OK());
}

PPL::Polyhedron::Polyhedron(const Polyhedron& y)
  : con_sys(y.topology()),
    gen_sys(y.topology()),
    status(y.status),
    space_dim(y.space_dim) {
  // Being a protected method, we simply assert that topologies do match.
  assert(topology() == y.topology());
  if (y.constraints_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
      sat_g = y.sat_g;
}

PPL::Polyhedron::Polyhedron(Topology topol, const ConSys& ccs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // FIXME: this implementation is just an executable specification.
  ConSys cs = ccs;

  // Try to adapt `cs' to the required topology.
  dimension_type cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_dimension(topol, cs_space_dim))
    throw_topology_incompatible("Polyhedron(cs)", cs);

  if (cs.num_rows() > 0 && cs_space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (con_sys.num_pending_rows() > 0) {
      // Even though `cs' has pending constraints, since the generators
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending constraints. By integrating the pending part
      // of `con_sys' we may loose sortedness.
      con_sys.unset_pending_rows();
      con_sys.set_sorted(false);
    }
    add_low_level_constraints(con_sys);
    set_constraints_up_to_date();

    // Set the space dimension.
    space_dim = cs_space_dim;
    assert(OK());
    return;
  }

  // Here `cs.num_rows == 0' or `cs_space_dim == 0'.
  space_dim = 0;
  if (cs.num_columns() > 0)
    // See if an inconsistent constraint has been passed.
    for (dimension_type i = cs.num_rows(); i-- > 0; )
      if (cs[i].is_trivial_false()) {
	// Inconsistent constraint found: the polyhedron is empty.
	set_empty();
	return;
      }
}

PPL::Polyhedron::Polyhedron(Topology topol, ConSys& cs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Try to adapt `cs' to the required topology.
  dimension_type cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_dimension(topol, cs_space_dim))
    throw_topology_incompatible("Polyhedron(cs)", cs);
  
  if (cs.num_rows() > 0 && cs_space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (con_sys.num_pending_rows() > 0) {
      // Even though `cs' has pending constraints, since the generators
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending constraints. By integrating the pending part
      // of `con_sys' we may loose sortedness.
      con_sys.unset_pending_rows();
      con_sys.set_sorted(false);
    }
    add_low_level_constraints(con_sys);
    set_constraints_up_to_date();
    
    // Set the space dimension.
    space_dim = cs_space_dim;
    assert(OK());
    return;
  }

  // Here `cs.num_rows == 0' or `cs_space_dim == 0'.
  space_dim = 0;
  if (cs.num_columns() > 0)
    // See if an inconsistent constraint has been passed.
    for (dimension_type i = cs.num_rows(); i-- > 0; )
      if (cs[i].is_trivial_false()) {
	// Inconsistent constraint found: the polyhedron is empty.
	set_empty();
	return;
      }
}

PPL::Polyhedron::Polyhedron(Topology topol, const GenSys& cgs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // FIXME: this implementation is just an executable specification.
  GenSys gs = cgs;

  // An empty set of generators defines the empty polyhedron.
  if (gs.num_rows() == 0) {
    space_dim = 0;
    status.set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators("Polyhedron(gs)");

  dimension_type gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_dimension(topol, gs_space_dim))
    throw_topology_incompatible("Polyhedron(gs)", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // for each point we must also have the corresponding closure point.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.add_corresponding_closure_points();
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the constraints
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending generators. By integrating the pending part
      // of `gen_sys' we may loose sortedness.
      gen_sys.unset_pending_rows();
      gen_sys.set_sorted(false);
    }
    // Generators are now up-to-date.
    set_generators_up_to_date();

    // Set the space dimension.
    space_dim = gs_space_dim;
    assert(OK());
    return;
  }

  // Here `gs.num_rows > 0' and `gs_space_dim == 0':
  // we already checked for both the topology-compatibility
  // and the supporting point.
  space_dim = 0;
}

PPL::Polyhedron::Polyhedron(Topology topol, GenSys& gs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // An empty set of generators defines the empty polyhedron.
  if (gs.num_rows() == 0) {
    space_dim = 0;
    status.set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators("Polyhedron(gs)");

  dimension_type gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_dimension(topol, gs_space_dim))
    throw_topology_incompatible("Polyhedron(gs)", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // for each point we must also have the corresponding closure point.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.add_corresponding_closure_points();
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the constraints
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending generators. By integrating the pending part
      // of `gen_sys' we may loose sortedness.
      gen_sys.unset_pending_rows();
      gen_sys.set_sorted(false);
    }
    // Generators are now up-to-date.
    set_generators_up_to_date();

    // Set the space dimension.
    space_dim = gs_space_dim;
    assert(OK());
    return;
  }

  // Here `gs.num_rows > 0' and `gs_space_dim == 0':
  // we already checked for both the topology-compatibility
  // and the supporting point.
  space_dim = 0;
}

PPL::Polyhedron&
PPL::Polyhedron::operator=(const Polyhedron& y) {
  // Being a protected method, we simply assert that topologies do match.
  assert(topology() == y.topology());
  space_dim = y.space_dim;
  if (y.is_empty())
    set_empty();
  else if (space_dim == 0)
    set_zero_dim_univ();
  else {
    status = y.status;
    if (y.constraints_are_up_to_date())
      con_sys = y.con_sys;
    if (y.generators_are_up_to_date())
      gen_sys = y.gen_sys;
    if (y.sat_c_is_up_to_date())
      sat_c = y.sat_c;
    if (y.sat_g_is_up_to_date())
      sat_g = y.sat_g;
  }
  return *this;
}

PPL::Polyhedron::Three_Valued_Boolean
PPL::Polyhedron::quick_equivalence_test(const Polyhedron& y) const {
  // Private method: the caller must ensure the following.
  assert(topology() == y.topology());
  assert(space_dimension() == y.space_dimension());
  assert(!is_empty() && !y.is_empty() && space_dimension() > 0);

  const Polyhedron& x = *this;

  if (x.is_necessarily_closed()) {
    if (!x.has_something_pending() && !y.has_something_pending()) {
      bool css_normalized = false;
      if (x.constraints_are_minimized() && y.constraints_are_minimized()) {
	// Equivalent minimized constraint systems have:
	//  - the same number of constraints; ...
	if (x.con_sys.num_rows() != y.con_sys.num_rows())
	  return Polyhedron::TVB_FALSE;
	//  - the same number of equalities; ...
	dimension_type x_num_equalities = x.con_sys.num_equalities();
	if (x_num_equalities != y.con_sys.num_equalities())
	  return Polyhedron::TVB_FALSE;
	//  - if there are no equalities, they have the same constraints.
	//    Delay this test: try cheaper tests on generators first.
	css_normalized = (x_num_equalities == 0);
      }

      if (x.generators_are_minimized() && y.generators_are_minimized()) {
	// Equivalent minimized generator systems have:
	//  - the same number of generators; ...
	if (x.gen_sys.num_rows() != y.gen_sys.num_rows())
	  return Polyhedron::TVB_FALSE;
	//  - the same number of lines; ...
	dimension_type x_num_lines = x.gen_sys.num_lines();
	if (x_num_lines != y.gen_sys.num_lines())
	  return Polyhedron::TVB_FALSE;
	//  - if there are no lines, they have the same generators.
	if (x_num_lines == 0) {
	  // Sort the two systems and check for syntactic identity.
	  x.obtain_sorted_generators();
	  y.obtain_sorted_generators();
	  if (x.gen_sys == y.gen_sys)
	    return Polyhedron::TVB_TRUE;
	  else
	    return Polyhedron::TVB_FALSE;
	}
      }
      
      if (css_normalized) {
	// Sort the two systems and check for identity.
	x.obtain_sorted_constraints();
	y.obtain_sorted_constraints();
	if (x.con_sys == y.con_sys)
	    return Polyhedron::TVB_TRUE;
	  else
	    return Polyhedron::TVB_FALSE;
      }
    }
  }
  return Polyhedron::TVB_DONT_KNOW;
}

bool
PPL::Polyhedron::is_included(const Polyhedron& y) const {
  // Private method: the caller must ensure the following.
  assert(topology() == y.topology());
  assert(space_dimension() == y.space_dimension());
  assert(!is_empty() && !y.is_empty() && space_dimension() > 0);

  const Polyhedron& x = *this;

  // `x' cannot have pending constraints, because we need its generators.
  if (x.has_pending_constraints() && !x.process_pending_constraints())
    return true;
  // `y' cannot have pending generators, because we need its constraints.
  if (y.has_pending_generators())
    y.process_pending_generators();

#ifdef BE_LAZY
  if (!x.generators_are_up_to_date() && !x.update_generators())
    return true;
  if (!y.constraints_are_up_to_date())
    y.update_constraints();
#else
  if (!x.generators_are_minimized())
    x.minimize();
  if (!y.constraints_are_minimized())
    y.minimize();
#endif

  assert(x.OK());
  assert(y.OK());
  
  const GenSys& gs = x.gen_sys;
  const ConSys& cs = y.con_sys;
  
  if (x.is_necessarily_closed())
    // When working with necessarily closed polyhedra,
    // `x' is contained in `y' if and only if all the generators of `x'
    // satisfy all the inequalities and saturate all the equalities of `y'.
    // This comes from the definition of a polyhedron as the set of
    // vectors satisfying a constraint system and the fact that all
    // vectors in `x' can be obtained by suitably combining its generators.
    for (dimension_type i = cs.num_rows(); i-- > 0; ) {
      const Constraint& c = cs[i];
      if (c.is_inequality()) {
	for (dimension_type j = gs.num_rows(); j-- > 0; ) {
	  const Generator& g = gs[j];
	  int sp_sign = sgn(c * g);
	  if (g.is_line()) {
	    if (sp_sign != 0)
	      return false;
	  }
	  else
	    // `g' is a ray or a point.
	    if (sp_sign < 0)
	      return false;
	}
      }
      else {
	// `c' is an equality.
	for (dimension_type j = gs.num_rows(); j-- > 0; )
	  if (c * gs[j] != 0)
	    return false;
      }
    }
  else {
    // Here we have a NON-necessarily closed polyhedron: using the
    // reduced scalar product, which ignores the epsilon coefficient.
    dimension_type eps_index = x.space_dimension() + 1;
    for (dimension_type i = cs.num_rows(); i-- > 0; ) {
      const Constraint& c = cs[i];
      switch (c.type()) {
      case Constraint::NONSTRICT_INEQUALITY:
	for (dimension_type j = gs.num_rows(); j-- > 0; ) {
	  const Generator& g = gs[j];
	  int sp_sign = sgn(reduced_scalar_product(c, g));
	  if (g.is_line()) {
	    if (sp_sign != 0)
	      return false;
	  }
	  else
	    // `g' is a ray or a point or a closure point.
	    if (sp_sign < 0)
	      return false;
	}
	break;
      case Constraint::EQUALITY:
	for (dimension_type j = gs.num_rows(); j-- > 0; )
	  if (reduced_scalar_product(c, gs[j]) != 0)
	    return false;
	break;
      case Constraint::STRICT_INEQUALITY:
	for (dimension_type j = gs.num_rows(); j-- > 0; ) {
	  const Generator& g = gs[j];
	  int sp_sign = sgn(reduced_scalar_product(c, g));
	  if (g[eps_index] > 0) {
	    // Generator `g' is a point.
	    // If a point violates or saturates a strict inequality
	    // (when ignoring the epsilon coefficients) then it is
	    // not included in the polyhedron.
	    if (sp_sign <= 0)
	      return false;
	  }
	  else if (g.is_line()) {
	    // Lines have to saturate all constraints.
	    if (sp_sign != 0)
	      return false;
	  }
	  else 
	    // The generator is a ray or closure point: usual test.
	    if (sp_sign < 0)
	      return false;
	}
	break;
      }
    }
  }

  // Inclusion holds.
  return true;
}

bool
PPL::Polyhedron::bounds(const LinExpression& expr, bool from_above) const {
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((from_above
				  ? "bounds_from_above(e)"
				  : "bounds_from_below(e)"), "e", expr);

  // A zero-dimensional or empty polyhedron bounds everything.
  if (space_dim == 0
      || is_empty()
      || (has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators()))
    return true;
  
  // The polyhedron has updated, possibly pending generators.
  for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
    const Generator& g = gen_sys[i];
    // Only lines and rays in `*this' can cause `expr' to be unbounded.
    if (g[0] == 0) {
      // Compute the scalar product between `g' and `expr'.
      tmp_Integer[0] = 0;
      for (dimension_type j = expr.size(); j-- > 0; ) {
	// The following two lines optimize the computation
	// of tmp_Integer[0] += g[j] * expr[j].
	tmp_Integer[1] = g[j] * expr[j];
	tmp_Integer[0] += tmp_Integer[1];
      }
      int sign = sgn(tmp_Integer[0]);
      if (sign != 0
	  && (g.is_line()
	      || (from_above && sign > 0)
	      || (!from_above && sign < 0)))
	// `*this' does not bound `expr'.
	return false;
    }
  }
  // No sources of unboundedness have been found for `expr'
  // in the given direction.
  return true;
}

void
PPL::Polyhedron::set_zero_dim_univ() {
  status.set_zero_dim_univ();
  space_dim = 0;
  con_sys.clear();
  gen_sys.clear();
}

void
PPL::Polyhedron::set_empty() {
  status.set_empty();
  // The polyhedron is empty: we can thus throw away everything.
  con_sys.clear();
  gen_sys.clear();
  sat_c.clear();
  sat_g.clear();
}

bool
PPL::Polyhedron::process_pending_constraints() const {
  assert(space_dim > 0 && !is_empty());
  assert(has_pending_constraints() && !has_pending_generators());

  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // Integrate the pending part of the system of constraints and minimize.
  // We need `sat_c' up-to-date and `con_sys' sorted (together with `sat_c').
  if (!x.sat_c_is_up_to_date())
    x.sat_c.transpose_assign(x.sat_g);
  if (!x.con_sys.is_sorted())
    x.obtain_sorted_constraints_with_sat_c();
  // We sort in place the pending constraints, erasing those constraints
  // that also occur in the non-pending part of `con_sys'.
  x.con_sys.sort_pending_and_remove_duplicates();
  if (x.con_sys.num_pending_rows() == 0) {
    // All pending constraints were duplicates.
    x.clear_pending_constraints();
    assert(OK(true));
    return true;
  }

  bool empty = add_and_minimize(true, x.con_sys, x.gen_sys, x.sat_c);
  assert(x.con_sys.num_pending_rows() == 0);
  
  if (empty)
    x.set_empty();
  else {
    x.clear_pending_constraints();
    x.clear_sat_g_up_to_date();
    x.set_sat_c_up_to_date();
  }
  assert(OK(!empty));
  return !empty;
}

void
PPL::Polyhedron::process_pending_generators() const {
  assert(space_dim > 0 && !is_empty());
  assert(has_pending_generators() && !has_pending_constraints());

  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // Integrate the pending part of the system of generators and minimize.
  // We need `sat_g' up-to-date and `gen_sys' sorted (together with `sat_g').
  if (!x.sat_g_is_up_to_date())
    x.sat_g.transpose_assign(x.sat_c);
  if (!x.gen_sys.is_sorted())
    x.obtain_sorted_generators_with_sat_g();
  // We sort in place the pending generators, erasing those generators
  // that also occur in the non-pending part of `gen_sys'.
  x.gen_sys.sort_pending_and_remove_duplicates();
  if (x.gen_sys.num_pending_rows() == 0) {
    // All pending generators were duplicates.
    x.clear_pending_generators();
    assert(OK(true));
    return;
  }

  add_and_minimize(false, x.gen_sys, x.con_sys, x.sat_g); 
  assert(x.gen_sys.num_pending_rows() == 0);

  x.clear_pending_generators();
  x.clear_sat_c_up_to_date();
  x.set_sat_g_up_to_date();
}

void
PPL::Polyhedron::remove_pending_to_obtain_constraints() const {
  assert(has_something_pending());

  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // If the polyhedron has pending constraints, simply unset them.
  if (x.has_pending_constraints()) {
    // Integrate the pending constraints, which are possibly not sorted.
    x.con_sys.unset_pending_rows();
    x.con_sys.set_sorted(false);
    x.clear_pending_constraints();
    x.clear_constraints_minimized();
    x.clear_generators_up_to_date();
  }
  else {
    assert(x.has_pending_generators());
    // We must process the pending generators and obtain the
    // corresponding system of constraints.
    x.process_pending_generators();
  }
  assert(OK(true));
}

bool
PPL::Polyhedron::remove_pending_to_obtain_generators() const {
  assert(has_something_pending());

  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // If the polyhedron has pending generators, simply unset them.
  if (x.has_pending_generators()) {
    // Integrate the pending generators, which are possibly not sorted.
    x.gen_sys.unset_pending_rows();
    x.gen_sys.set_sorted(false);
    x.clear_pending_generators();
    x.clear_generators_minimized();
    x.clear_constraints_up_to_date();
    assert(OK(true));
    return true;
  }
  else {
    assert(x.has_pending_constraints());
    // We must integrate the pending constraints and obtain the
    // corresponding system of generators.
    return x.process_pending_constraints();
  }
}

void
PPL::Polyhedron::update_constraints() const {
  assert(space_dim > 0);
  assert(!is_empty());
  assert(generators_are_up_to_date());
  // We assume the polyhedron has no pending constraints or generators.
  assert(!has_something_pending());

  Polyhedron& x = const_cast<Polyhedron&>(*this);
  minimize(false, x.gen_sys, x.con_sys, x.sat_c);
  // `sat_c' is the only saturation matrix up-to-date.
  x.set_sat_c_up_to_date();
  x.clear_sat_g_up_to_date();
  // The system of constraints and the system of generators
  // are minimized.
  x.set_constraints_minimized();
  x.set_generators_minimized();
}

bool
PPL::Polyhedron::update_generators() const {
  assert(space_dim > 0);
  assert(!is_empty());
  assert(constraints_are_up_to_date());
  // We assume the polyhedron has no pending constraints or generators.
  assert(!has_something_pending());

  Polyhedron& x = const_cast<Polyhedron&>(*this);
  // If the system of constraints is not consistent the
  // polyhedron is empty.
  bool empty = minimize(true, x.con_sys, x.gen_sys, x.sat_g);
  if (empty)
    x.set_empty();
  else {
    // `sat_g' is the only saturation matrix up-to-date.
    x.set_sat_g_up_to_date();
    x.clear_sat_c_up_to_date();
    // The system of constraints and the system of generators
    // are minimized.
    x.set_constraints_minimized();
    x.set_generators_minimized();
  }
  return !empty;
}

void
PPL::Polyhedron::update_sat_c() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_c_is_up_to_date());

  // We only consider non-pending rows.
  dimension_type csr = con_sys.first_pending_row();
  dimension_type gsr = gen_sys.first_pending_row();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // The columns of `sat_c' represent the constraints and
  // its rows represent the generators: resize accordingly.
  x.sat_c.resize(gsr, csr);
  for (dimension_type i = gsr; i-- > 0; )
    for (dimension_type j = csr; j-- > 0; ) {
      int sp_sign = sgn(con_sys[j] * gen_sys[i]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[i]' violates the constraint
      // `con_sys[j]' and it is not possible because both generators
      // and constraints are up-to-date.
      assert(sp_sign >= 0);
      if (sp_sign > 0)
	// `gen_sys[i]' satisfies (without saturate) `con_sys[j]'.
	x.sat_c[i].set(j);
      else
	// `gen_sys[i]' saturates `con_sys[j]'.
	x.sat_c[i].clear(j);
    }
  x.set_sat_c_up_to_date();
}

void
PPL::Polyhedron::update_sat_g() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_g_is_up_to_date());
  
  // We only consider non-pending rows.
  dimension_type csr = con_sys.first_pending_row();
  dimension_type gsr = gen_sys.first_pending_row();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // The columns of `sat_g' represent generators and its
  // rows represent the constraints: resize accordingly.
  x.sat_g.resize(csr, gsr);
  for (dimension_type i = csr; i-- > 0; )
    for (dimension_type j = gsr; j-- > 0; ) {
      int sp_sign = sgn(con_sys[i] * gen_sys[j]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[j]' violates the constraint
      // `con_sys[i]' and it is not possible because both generators
      // and constraints are up-to-date.
      assert(sp_sign >= 0);
      if (sp_sign > 0)
	// `gen_sys[j]' satisfies (without saturate) `con_sys[i]'.
	x.sat_g[i].set(j);
      else
	// `gen_sys[j]' saturates `con_sys[i]'.
	x.sat_g[i].clear(j);
    }
  x.set_sat_g_up_to_date();
}

void
PPL::Polyhedron::obtain_sorted_constraints() const {
  assert(constraints_are_up_to_date());
  // `con_sys' will be sorted up to `index_first_pending'.
  Polyhedron& x = const_cast<Polyhedron&>(*this);
  if (!x.con_sys.is_sorted())
    if (x.sat_g_is_up_to_date()) {
      // Sorting constraints keeping `sat_g' consistent.
      x.con_sys.sort_and_remove_with_sat(x.sat_g);
      // `sat_c' is not up-to-date anymore.
      x.clear_sat_c_up_to_date();
    }
    else if (x.sat_c_is_up_to_date()) {
      // Using `sat_c' to obtain `sat_g', then it is like previous case.
      x.sat_g.transpose_assign(x.sat_c);
      x.con_sys.sort_and_remove_with_sat(x.sat_g);
      x.set_sat_g_up_to_date();
      x.clear_sat_c_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date,
      // we just sort the constraints.
      x.con_sys.sort_rows();

  assert(con_sys.check_sorted());
}

void
PPL::Polyhedron::obtain_sorted_generators() const {
  assert(generators_are_up_to_date());
  // `gen_sys' will be sorted up to `index_first_pending'.
  Polyhedron& x = const_cast<Polyhedron&>(*this);
  if (!x.gen_sys.is_sorted())
    if (x.sat_c_is_up_to_date()) {
      // Sorting generators keeping 'sat_c' consistent.
      x.gen_sys.sort_and_remove_with_sat(x.sat_c);
      // `sat_g' is not up-to-date anymore.
      x.clear_sat_g_up_to_date();
    }
    else if (x.sat_g_is_up_to_date()) {
      // Obtaining `sat_c' from `sat_g' and proceeding like previous case.
      x.sat_c.transpose_assign(x.sat_g);
      x.gen_sys.sort_and_remove_with_sat(x.sat_c);
      x.set_sat_c_up_to_date();
      x.clear_sat_g_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
      // the generators.
      x.gen_sys.sort_rows();

  assert(gen_sys.check_sorted());
}

void
PPL::Polyhedron::obtain_sorted_constraints_with_sat_c() const {
  assert(constraints_are_up_to_date());
  assert(constraints_are_minimized());
  // `con_sys' will be sorted up to `index_first_pending'.
  Polyhedron& x = const_cast<Polyhedron&>(*this);
  // At least one of the saturation matrices must be up-to-date.
  if (!x.sat_c_is_up_to_date() && !x.sat_g_is_up_to_date())
    x.update_sat_c();

  if (x.con_sys.is_sorted()) {
    if (x.sat_c_is_up_to_date())
      // If constraints are already sorted and sat_c is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!x.sat_g_is_up_to_date()) {
      // If constraints are not sorted and sat_g is not up-to-date
      // we obtain sat_g from sat_c (that has to be up-to-date) ...
      x.sat_g.transpose_assign(x.sat_c);
      x.set_sat_g_up_to_date();
    }
    // ...and sort it together with constraints.
    x.con_sys.sort_and_remove_with_sat(x.sat_g);
  }
  // Obtaining sat_c from sat_g.
  x.sat_c.transpose_assign(x.sat_g);
  x.set_sat_c_up_to_date();
  // Constraints are sorted now.
  x.con_sys.set_sorted(true);

  assert(con_sys.check_sorted());
}

void
PPL::Polyhedron::obtain_sorted_generators_with_sat_g() const {
  assert(generators_are_up_to_date());
  // `gen_sys' will be sorted up to `index_first_pending'.
  Polyhedron& x = const_cast<Polyhedron&>(*this);
  // At least one of the saturation matrices must be up-to-date.
  if (!x.sat_c_is_up_to_date() && !x.sat_g_is_up_to_date())
    x.update_sat_g();

  if (x.gen_sys.is_sorted()) {
    if (x.sat_g_is_up_to_date())
      // If generators are already sorted and sat_g is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!x.sat_c_is_up_to_date()) {
      // If generators are not sorted and sat_c is not up-to-date
      // we obtain sat_c from sat_g (that has to be up-to-date) ...
      x.sat_c.transpose_assign(x.sat_g);
      x.set_sat_c_up_to_date();
    }
    // ...and sort it together with generators.
    x.gen_sys.sort_and_remove_with_sat(x.sat_c);
  }
  // Obtaining sat_g from sat_c.
  x.sat_g.transpose_assign(sat_c);
  x.set_sat_g_up_to_date();
  // Generators are sorted now.
  x.gen_sys.set_sorted(true);

  assert(gen_sys.check_sorted());
}

bool
PPL::Polyhedron::minimize() const {
  // 0-dim space or empty polyhedra are already minimized.
  if (is_empty())
    return false;
  if (space_dim == 0)
    return true;

  // If the polyhedron has something pending, process it.
  if (has_something_pending()) {
    bool ret = process_pending();
    assert(OK());
    return ret;
  }

  // Here there are no pending constraints or generators.
  // Is the polyhedron already minimized?
  if (constraints_are_minimized() && generators_are_minimized())
    return true;   

  // If constraints or generators are up-to-date, invoking
  // update_generators() or update_constraints(), respectively,
  // minimizes both constraints and generators.
  // If both are up-to-date it does not matter whether we use
  // update_generators() or update_constraints():
  // both minimize constraints and generators.
  if (constraints_are_up_to_date()) {
    // We may discover here that `*this' is empty.
    bool ret = update_generators();
    assert(OK());
    return ret;
  }
  else {
    assert(generators_are_up_to_date());
    update_constraints();
    assert(OK());
    return true;
  }
}

bool
PPL::Polyhedron::strongly_minimize_constraints() const {
  assert(!is_necessarily_closed());

  // From the user perspective, the polyhedron will not change.
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // We need `con_sys' (weakly) minimized and `gen_sys' up-to-date.
  // `minimize()' will process any pending constraints or generators.
  if (!minimize())
    return false;
  
  // If the polyhedron `*this' is zero-dimensional
  // at this point it must be a universe polyhedron.
  if (x.space_dim == 0)
    return true;

  // We also need `sat_g' up-to-date.
  if (!sat_g_is_up_to_date()) {
    assert(sat_c_is_up_to_date());
    x.sat_g.transpose_assign(sat_c);
  }

  // These SatRow's will be later used as masks in order to check
  // saturation conditions restricted to particular subsets of
  // the generator system.
  SatRow sat_all_but_rays;
  SatRow sat_all_but_points;
  SatRow sat_all_but_closure_points;

  dimension_type gs_rows = gen_sys.num_rows();
  dimension_type n_lines = gen_sys.num_lines();
  for (dimension_type i = gs_rows; i-- > n_lines; )
    switch (gen_sys[i].type()) {
    case Generator::RAY:
      sat_all_but_rays.set(i);
      break;
    case Generator::POINT:
      sat_all_but_points.set(i);
      break;
    case Generator::CLOSURE_POINT:
      sat_all_but_closure_points.set(i);
      break;
    default:
      // Found a line with index i >= n_lines.
      throw std::runtime_error("PPL internal error: "
			       "strongly_minimize_constraints.");
    }
  SatRow sat_lines_and_rays;
  set_union(sat_all_but_points, sat_all_but_closure_points,
	    sat_lines_and_rays);
  SatRow sat_lines_and_closure_points;
  set_union(sat_all_but_rays, sat_all_but_points,
	    sat_lines_and_closure_points);
  SatRow sat_lines;
  set_union(sat_lines_and_rays, sat_lines_and_closure_points,
	    sat_lines);

  // These flags are maintained to later decide
  // if we have to add the eps_leq_one constraint
  // and whether or not the constraint system was changed.
  bool changed = false;
  bool found_eps_leq_one = false;

  // For all the strict inequalities in `con_sys',
  // check for eps-redundancy and eventually move them
  // to the bottom part of the system.
  ConSys& cs = x.con_sys;
  SatMatrix& sat = x.sat_g;
  dimension_type cs_rows = cs.num_rows();
  dimension_type eps_index = cs.num_columns() - 1;
  for (dimension_type i = 0; i < cs_rows; )
    if (cs[i].is_strict_inequality()) {
      // First, check if it is saturated by no closure points
      SatRow sat_ci;
      set_union(sat[i], sat_lines_and_closure_points, sat_ci);
      if (sat_ci == sat_lines) {
	// It is saturated by no closure points.
	if (!found_eps_leq_one) {
	  // Check if it is the eps_leq_one constraint.
	  const Constraint& c = cs[i];
	  bool all_zeros = true;
	  for (dimension_type k = eps_index; k-- > 1; )
	    if (c[k] != 0) {
	      all_zeros = false;
	      break;
	    }
	  if (all_zeros && (c[0] + c[eps_index] == 0)) {
	    // We found the eps_leq_one constraint.
	    found_eps_leq_one = true;
	    // Consider next constraint.
	    ++i;
	    continue;
	  }
	}
	// Here `cs[i]' is not the eps_leq_one constraint,
	// so it is eps-redundant.
	// Move it to the bottom of the constraint system,
	// while keeping `sat_g' consistent.
	--cs_rows;
	std::swap(cs[i], cs[cs_rows]);
	std::swap(sat[i], sat[cs_rows]);
	// The constraint system is changed.
	changed = true;
	// Continue by considering next constraint,
	// which is already in place due to the swap.
	continue;
      }
      // Now we check if there exists another strict inequality
      // constraint having a superset of its saturators,
      // when disregarding points.
      sat_ci.clear();
      set_union(sat[i], sat_all_but_points, sat_ci);
      bool eps_redundant = false;
      for (dimension_type j = 0; j < cs_rows; ++j)
	if (i != j && cs[j].is_strict_inequality()
	    && subset_or_equal(sat[j], sat_ci)) {
	  // Constraint `cs[i]' is eps-redundant:
	  // move it to the bottom of the constraint system,
	  // while keeping `sat_g' consistent.
	  --cs_rows;
	  std::swap(cs[i], cs[cs_rows]);
	  std::swap(sat[i], sat[cs_rows]);
	  eps_redundant = true;
	  // The constraint system is changed.
	  changed = true;
	  break;
	}
      // Continue with next constraint, which is already in place
      // due to the swap if we have found an eps-redundant constraint.
      if (!eps_redundant)
	++i;
    }
    else
      // `cs[i]' is not a strict inequality: consider next constraint.
      ++i;

  if (changed) {
    // If the constraint system has been changed and we haven't found the
    // eps_leq_one constraint, insert it to force an upper bound on epsilon.
    if (!found_eps_leq_one) {
      // Note: we overwrite the first of the eps-redundant constraints found.
      assert(cs_rows < cs.num_rows());
      Constraint& eps_leq_one = cs[cs_rows];
      eps_leq_one[0] = 1;
      eps_leq_one[eps_index] = -1;
      for (dimension_type k = eps_index; k-- > 1; )
	eps_leq_one[k] = 0;      
      // Bump number of rows.
      ++cs_rows;
    }
    // Erase the eps-redundant constraints, if there are any
    // (the remaining constraints are no pending).
    if (cs_rows < cs.num_rows()) {
      cs.erase_to_end(cs_rows);
      cs.unset_pending_rows();
    }
    // The constraint system is no longer sorted.
    cs.set_sorted(false);
    // The generator system is no longer up-to-date.
    x.clear_generators_up_to_date();
  }

  assert(OK());
  return true;
}

bool
PPL::Polyhedron::strongly_minimize_generators() const {
  assert(!is_necessarily_closed());

  // From the user perspective, the polyhedron will not change.
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // We need `gen_sys' (weakly) minimized and `con_sys' up-to-date.
  // `minimize()' will process any pending constraints or generators.
  if (!minimize())
    return false;

  // If the polyhedron `*this' is zero-dimensional
  // at this point it must be a universe polyhedron.
  if (x.space_dim == 0)
    return true;

  // We also need `sat_c' up-to-date.
  if (!sat_c_is_up_to_date()) {
    assert(sat_g_is_up_to_date());
    x.sat_c.transpose_assign(sat_g);
  }

  // This SatRow will have all and only the indexes
  // of strict inequalities set to 1.
  SatRow sat_all_but_strict_ineq;
  dimension_type cs_rows = con_sys.num_rows();
  dimension_type n_equals = con_sys.num_equalities();
  for (dimension_type i = cs_rows; i-- > n_equals; )
    if (con_sys[i].is_strict_inequality())
      sat_all_but_strict_ineq.set(i);

  // Will record whether or not we changed the generator system.
  bool changed = false;

  // For all points in the generator system, check for eps-redundancy
  // and eventually move them to the bottom part of the system.
  GenSys& gs = const_cast<GenSys&>(gen_sys);
  SatMatrix& sat = const_cast<SatMatrix&>(sat_c);
  dimension_type gs_rows = gs.num_rows();
  dimension_type n_lines = gs.num_lines();
  dimension_type eps_index = gs.num_columns() - 1;
  for (dimension_type i = n_lines; i < gs_rows; )
    if (gs[i].is_point()) {
      // Compute the SatRow corresponding to the candidate point
      // when strict inequality constraints are ignored.
      SatRow sat_gi;
      set_union(sat[i], sat_all_but_strict_ineq, sat_gi);
      // Check if the candidate point is actually eps-redundant:
      // namely, if there exists another point that saturates
      // all the non-strict inequalities saturated by the candidate.
      bool eps_redundant = false;
      for (dimension_type j = n_lines; j < gs_rows; ++j)
	if (i != j && gs[j].is_point() && subset_or_equal(sat[j], sat_gi)) {
	  // Point `gs[i]' is eps-redundant:
	  // move it to the bottom of the generator system,
	  // while keeping `sat_c' consistent.
	  --gs_rows;
	  std::swap(gs[i], gs[gs_rows]);
	  std::swap(sat[i], sat[gs_rows]);
	  eps_redundant = true;
	  changed = true;
	  break;
	}
      if (!eps_redundant) {
	// Let all point encodings have epsilon coordinate 1.
	Generator& gi = gs[i];
	if (gi[eps_index] != gi[0]) {
	  gi[eps_index] = gi[0];
	  // Enforce normalization.
	  gi.normalize();
	  changed = true;
	}
	// Consider next generator.
	++i;
      }
    }
    else
      // Consider next generator.
      ++i;

  // If needed, erase the eps-redundant generators
  // (also updating `index_first_pending').
  if (gs_rows < gs.num_rows()) {
    gs.erase_to_end(gs_rows);
    gs.set_index_first_pending_row(gs_rows);
  }
  
  if (changed) {
    // The generator system is no longer sorted.
    x.gen_sys.set_sorted(false);
    // The constraint system is no longer up-to-date.
    x.clear_constraints_up_to_date();
  }

  assert(OK());
  return true;
}

void
PPL::Polyhedron::throw_runtime_error(const char* method) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << std::endl;
  throw std::runtime_error(s.str());
}

void
PPL::Polyhedron::throw_invalid_argument(const char* method,
					const char* reason) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << reason;
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const Polyhedron& y) const {
  std::ostringstream s;
  s << "Topology mismatch in PPL::Polyhedron::" << method << ":"
    << std::endl
    << "*this is a ";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron," << std::endl
    << "y is a ";
  if (y.is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron." << std::endl;
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const Constraint& ) const {
  assert(is_necessarily_closed());
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "the constraint is a strict inequality.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const Generator& ) const {
  assert(is_necessarily_closed());
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "the generator is a closure point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const ConSys& ) const {
  assert(is_necessarily_closed());
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "constraint system contains strict inequalities.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const GenSys& ) const {
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "generator system contains closure points.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_dimension_incompatible(const char* method,
					      const Polyhedron& y) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_dimension_incompatible(const char* method,
					      const char* name_row,
					      const Row& y) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_dimension_incompatible(const char* method,
					      const char* name_system,
					      const Matrix& y) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_system << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::
throw_dimension_incompatible(const char* method,
			     dimension_type required_dim) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_invalid_generator(const char* method) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "polyhedron is empty and generator is not a point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_invalid_generators(const char* method) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "non-empty generator system contains no points.";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_topology_incompatible(const char* method,
					     const Polyhedron& x,
					     const Polyhedron& y) {
  std::ostringstream s;
  s << "Topology mismatch in PPL::" << method << ":"
    << std::endl
    << "x is a ";
  if (x.is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron," << std::endl
    << "y is a ";
  if (y.is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron." << std::endl;
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_dimension_incompatible(const char* method,
					      const Polyhedron& x,
					      const Polyhedron& y) {
  std::ostringstream s;
  s << "PPL::";
  if (x.is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "x->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}
