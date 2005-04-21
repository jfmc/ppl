/* Grid class implementation
   (non-inline private or protected functions).
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Grid.defs.hh"

#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_defines
  \brief
  Controls the laziness level of the implementation.

  Temporarily used in a few of the function implementations to
  switch to an even more lazy algorithm. To be removed as soon as
  we collect enough information to decide which is the better
  implementation alternative.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define BE_LAZY 1

namespace PPL = Parma_Polyhedra_Library;

// FIX add  void construct(Congruence_System& cgs)?
void
PPL::Grid::construct(const Congruence_System& ccgs) {
  // Protecting against space dimension overflow is up to the caller.
  assert(ccgs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Congruence_System cgs = ccgs;

  const dimension_type cgs_space_dim = cgs.space_dimension();
#if 0
  // Try to adapt `cgs' to the required topology.
  // FIX need adjust_space_dimension? think for general constr's
  if (!cgs.adjust_topology_and_space_dimension(topol, cgs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(cgs)"
				: "NNC_Polyhedron(cgs)", "cgs", cgs);
#endif

  // Set the space dimension.
  space_dim = cgs_space_dim;

  if (space_dim > 0) {
    // Stealing the rows from `cgs'.
    std::swap(con_sys, cgs);
    simplify(con_sys, sat_c);
#if 0 // FIX
    if (con_sys.num_pending_rows() > 0) {
      // Even though `cgs' has pending constraints, since the generators
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending constraints. By integrating the pending part
      // of `con_sys' we may loose sortedness.
      con_sys.unset_pending_rows();
      con_sys.set_sorted(false);
    }
#endif
    add_low_level_congruences(con_sys);
    set_congruences_up_to_date();
  }
  else {
    // Here `space_dim == 0'.
    if (cgs.num_columns() > 0)
      // See if an inconsistent congruence has been passed.
      for (dimension_type i = cgs.num_rows(); i-- > 0; )
	if (cgs[i].is_trivial_false()) {
	  // Inconsistent constraint found: the polyhedron is empty.
	  set_empty();
	  break;
	}
  }
  assert(OK());
}

// FIX add  void construct(Generator_System& cgs)?
void
PPL::Grid::construct(const Generator_System& cgs) {
  // Protecting against space dimension overflow is up to the caller.
  assert(cgs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Generator_System gs = cgs;

  // An empty set of generators defines the empty grid.
  if (gs.num_rows() == 0) {
    space_dim = gs.space_dimension();
    status.set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators("Grid(gs)" /* FIX correct? */, "gs");

  const dimension_type gs_space_dim = gs.space_dimension();
#if 0
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_space_dimension(topol, gs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(gs)"
				: "NNC_Polyhedron(gs)", "gs", gs);
#endif

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    simplify(gen_sys, sat_g);
#if 0
    // FIX how in grid?
    // In a generator system describing a NNC polyhedron,
    // for each point we must also have the corresponding closure point.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.add_corresponding_closure_points();
#endif
    if (gen_sys.num_pending_rows() > 0) {
      // FIX
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

  // Here `gs.num_rows > 0' and `gs_space_dim == 0': we have already
  // checked for both the topology-compatibility and the supporting
  // point.
  space_dim = 0;
}

PPL::Coefficient
PPL::Grid::normalize_divisors(Linear_System& sys,
			      Coefficient_traits::const_reference divisor) {
  std::cout << "Normalizing divisors..." << std::endl;
  // FIX ~normalize divisor. (?)
  TEMP_INTEGER(lcm);
  lcm = divisor;
  if (sys.num_columns()) {
    dimension_type row = 0;
    dimension_type num_rows = sys.num_rows();

    // Move to the first point.
    while (sys[row].is_line_or_equality())
      if (++row == num_rows)
	return divisor;		// All rows are lines.

    if (lcm == 0) {
      lcm = sys[row][0];
      ++row;
    }

    // Calculate the LCM of the divisors.
    while (row < num_rows) {
      if (sys[row].is_ray_or_point_or_inequality())
	lcm_assign(lcm, sys[row][0]);
      ++row;
    }

    // Represent each point using the LCM as the divisor.
    for (dimension_type row = 0; row < num_rows; ++row) {
      Linear_Row& gen = sys[row];
      if (gen.is_ray_or_point_or_inequality()) {
	TEMP_INTEGER(divisor);
	divisor = gen[0];
	// FIX skip if divisor == lcm
	for (dimension_type col = 1; col < gen.size(); ++col)
	  gen[col] = (lcm * gen[col]) / divisor;
	gen[0] = lcm;
      }
    }

    sys.set_sorted(false);	// FIX
  }
  std::cout << "Normalizing divisors... done." << std::endl;
  return lcm;
}

PPL::Generator_System&
PPL::Grid::parameterize(Generator_System& sys, Generator& reference_row,
			bool leave_first) {
  // FIX use as ref pnt first pnt which has a value in the first col?
  //         better result? then need to search for it

  dimension_type num_rows = sys.num_rows();
  if (num_rows > (leave_first ? 1 : 0)) {
    dimension_type row_len = sys.num_columns();
    for (dimension_type row = (leave_first ? 1 : 0); row < num_rows; ++row)
      if (sys[row].is_ray_or_point_or_inequality())
	for (dimension_type col = 0; col < row_len; ++col)
	  sys[row][col] -= reference_row[col];
    sys.set_sorted(false);	// FIX
  }

  return sys;
}

#if 0

PPL::Grid::Grid(const Grid& y)
  : con_sys(y.topology()),
    gen_sys(y.topology()),
    status(y.status),
    space_dim(y.space_dim) {
  // Being a protected method, we simply assert that topologies do match.
  assert(topology() == y.topology());
  if (y.congruences_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
    sat_g = y.sat_g;
}

PPL::Grid::Grid(const Topology topol, const Congruence_System& ccs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Protecting against space dimension overflow is up to the caller.
  assert(ccs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Congruence_System cs = ccs;

  // Try to adapt `cs' to the required topology.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_space_dimension(topol, cs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(cs)"
				: "NNC_Polyhedron(cs)", "cs", cs);

  // Set the space dimension.
  space_dim = cs_space_dim;

  if (space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (con_sys.num_pending_rows() > 0) {
      // Even though `cs' has pending congruences, since the generators
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending congruences. By integrating the pending part
      // of `con_sys' we may loose sortedness.
      con_sys.unset_pending_rows();
      con_sys.set_sorted(false);
    }
    add_low_level_congruences(con_sys);
    set_congruences_up_to_date();
  }
  else {
    // Here `space_dim == 0'.
    if (cs.num_columns() > 0)
      // See if an inconsistent congruence has been passed.
      for (dimension_type i = cs.num_rows(); i-- > 0; )
	if (cs[i].is_trivial_false()) {
	  // Inconsistent congruence found: the polyhedron is empty.
	  set_empty();
	  break;
	}
  }
  assert(OK());
}

PPL::Grid::Grid(const Topology topol, Congruence_System& cs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Protecting against space dimension overflow is up to the caller.
  assert(cs.space_dimension() <= max_space_dimension());

  // Try to adapt `cs' to the required topology.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_space_dimension(topol, cs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(cs)"
				: "NNC_Polyhedron(cs)", "cs", cs);

  // Set the space dimension.
  space_dim = cs_space_dim;

  if (space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (con_sys.num_pending_rows() > 0) {
      // Even though `cs' has pending congruences, since the generators
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending congruences. By integrating the pending part
      // of `con_sys' we may loose sortedness.
      con_sys.unset_pending_rows();
      con_sys.set_sorted(false);
    }
    add_low_level_congruences(con_sys);
    set_congruences_up_to_date();
  }
  else {
    // Here `space_dim == 0'.
    if (cs.num_columns() > 0)
      // See if an inconsistent congruence has been passed.
      for (dimension_type i = cs.num_rows(); i-- > 0; )
	if (cs[i].is_trivial_false()) {
	  // Inconsistent congruence found: the polyhedron is empty.
	  set_empty();
	  break;
	}
  }
  assert(OK());
}

PPL::Grid::Grid(const Topology topol, const Generator_System& cgs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Protecting against space dimension overflow is up to the caller.
  assert(cgs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Generator_System gs = cgs;

  // An empty set of generators defines the empty polyhedron.
  if (gs.num_rows() == 0) {
    space_dim = gs.space_dimension();
    status.set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators((topol == NECESSARILY_CLOSED)
			     ? "C_Polyhedron(gs)"
			     : "NNC_Polyhedron(gs)", "gs");

  const dimension_type gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_space_dimension(topol, gs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(gs)"
				: "NNC_Polyhedron(gs)", "gs", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // for each point we must also have the corresponding closure point.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.add_corresponding_closure_points();
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the congruences
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

PPL::Grid::Grid(const Topology topol, Generator_System& gs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Protecting against space dimension overflow is up to the caller.
  assert(gs.space_dimension() <= max_space_dimension());

  // An empty set of generators defines the empty polyhedron.
  if (gs.num_rows() == 0) {
    space_dim = gs.space_dimension();
    status.set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators((topol == NECESSARILY_CLOSED)
			     ? "C_Polyhedron(gs)"
			     : "NNC_Polyhedron(gs)", "gs");

  const dimension_type gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_space_dimension(topol, gs_space_dim))
    throw_topology_incompatible((topol == NECESSARILY_CLOSED)
				? "C_Polyhedron(gs)"
				: "NNC_Polyhedron(gs)", "gs", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // for each point we must also have the corresponding closure point.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.add_corresponding_closure_points();
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the congruences
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

PPL::Grid&
PPL::Grid::operator=(const Grid& y) {
  // Being a protected method, we simply assert that topologies do match.
  assert(topology() == y.topology());
  space_dim = y.space_dim;
  if (y.marked_empty())
    set_empty();
  else if (space_dim == 0)
    set_zero_dim_univ();
  else {
    status = y.status;
    if (y.congruences_are_up_to_date())
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
#endif
PPL::Grid::Three_Valued_Boolean
PPL::Grid::quick_equivalence_test(const Grid& y) const {
  std::cout << "quick_equivalence_test" << std::endl;
  // Private method: the caller must ensure the following.
  //assert(topology() == y.topology()); // FIX
  assert(space_dim == y.space_dim);
  assert(!marked_empty() && !y.marked_empty() && space_dim > 0);

  const Grid& x = *this;

  //if (x.is_necessarily_closed()) {
  { // FIX
    if (x.has_something_pending() || y.has_something_pending())
      return Grid::TVB_DONT_KNOW;

    bool css_normalized = false;

    if (x.congruences_are_minimized() && y.congruences_are_minimized()) {
      // Equivalent minimized congruence systems have:
      //  - the same number of congruences; ...
      if (x.con_sys.num_rows() != y.con_sys.num_rows())
	return Grid::TVB_FALSE;
      //  - the same number of equalities; ...
      dimension_type x_num_equalities = x.con_sys.num_equalities();
      if (x_num_equalities != y.con_sys.num_equalities())
	return Grid::TVB_FALSE;
      //  - and if there are no equalities, the same congruences.
      //    Delay this test: try cheaper tests on generators first.
      css_normalized = (x_num_equalities == 0);
    }

    if (x.generators_are_minimized() && y.generators_are_minimized()) {
      // Equivalent minimized generator systems have:
      //  - the same number of generators; ...
      if (x.gen_sys.num_rows() != y.gen_sys.num_rows())
	return Grid::TVB_FALSE;
      //  - the same number of lines; ...
      const dimension_type x_num_lines = x.gen_sys.num_lines();
      if (x_num_lines != y.gen_sys.num_lines())
	return Grid::TVB_FALSE;
#if 0 // FIX requires canonical form?
      //  - and if there are no lines, the same generators.
      if (x_num_lines == 0) {
	// Sort the two systems and check for syntactic identity.
	x.obtain_sorted_generators();
	y.obtain_sorted_generators();
	if (x.gen_sys == y.gen_sys)
	  return Grid::TVB_TRUE;
	else
	  return Grid::TVB_FALSE;
      }
#endif
    }

    if (css_normalized) {
      // Sort the two systems and check for identity.
#if 0
      x.obtain_sorted_congruences();
      y.obtain_sorted_congruences();
#endif
#if 0
      // FIX requires sorting or a canonical form?
      if (x.con_sys == y.con_sys)
	return Grid::TVB_TRUE;
      else
	return Grid::TVB_FALSE;
#endif
    }
  }

  return Grid::TVB_DONT_KNOW;
}

bool
PPL::Grid::is_included_in(const Grid& y) const {
  // Private method: the caller must ensure the following.
  //assert(topology() == y.topology()); // FIX
  std::cout << "is_included_in..." << std::endl;
  assert(space_dim == y.space_dim);
  assert(!marked_empty() && !y.marked_empty() && space_dim > 0);

  const Grid& x = *this;

  // `x' cannot have pending congruences, because we need its generators.
  if (x.has_pending_congruences() && !x.process_pending_congruences())
    return true;
  // `y' cannot have pending generators, because we need its congruences.
  if (y.has_pending_generators())
    y.process_pending_generators();

#if BE_LAZY
  if (!x.generators_are_up_to_date() && !x.update_generators())
    return true;
  if (!y.congruences_are_up_to_date())
    y.update_congruences();
#else
  if (!x.generators_are_minimized())
    x.minimize();
  if (!y.congruences_are_minimized())
    y.minimize();
#endif

  assert(x.OK());
  assert(y.OK());

  const Generator_System& gs = x.gen_sys;
  const Congruence_System& cgs = y.con_sys;

  dimension_type num_rows = gs.num_rows();
  // FIX Generator& gen = gs[0];
  if (num_rows && virtual_row(gs[0]) == false)
    // FIX could this be any different for NNC ph's? (?)
    if (cgs.satisfies_all_congruences(gs[0]) == false) {
      std::cout << "is_included_in... done (false 0)." << std::endl;
      return false;
    }
  for (dimension_type i = num_rows; i-- > 1; )
    // FIX gen = gs[i];
    if (virtual_row(gs[i]) == false)
      // FIX could this be any different for NNC ph's? (?)
      if ((gs[i].is_ray_or_point_or_inequality()
	   // FIX is adjusting the param relative to gs[0] necessary?
	   && cgs.satisfies_all_congruences(gs[i], gs[0]) == false)
	  || cgs.satisfies_all_congruences(gs[i]) == false) {
	std::cout << "is_included_in... done (false i = " << i << ")." << std::endl;
	return false;
      }

  std::cout << "is_included_in... done." << std::endl;
  // Inclusion holds.
  return true;
}
#if 0
bool
PPL::Grid::bounds(const Linear_Expression& expr,
			const bool from_above) const {
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((from_above
				  ? "bounds_from_above(e)"
				  : "bounds_from_below(e)"), "e", expr);

  // A zero-dimensional or empty polyhedron bounds everything.
  if (space_dim == 0
      || marked_empty()
      || (has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !update_generators()))
    return true;

  // The polyhedron has updated, possibly pending generators.
  for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
    const Generator& g = gen_sys[i];
    // Only lines and rays in `*this' can cause `expr' to be unbounded.
    if (g[0] == 0) {
      const int sp_sign = homogeneous_scalar_product_sign(expr, g);
      if (sp_sign != 0
	  && (g.is_line()
	      || (from_above && sp_sign > 0)
	      || (!from_above && sp_sign < 0)))
	// `*this' does not bound `expr'.
	return false;
    }
  }
  // No sources of unboundedness have been found for `expr'
  // in the given direction.
  return true;
}

bool
PPL::Grid::max_min(const Linear_Expression& expr,
			 const bool maximize,
			 Coefficient& ext_n, Coefficient& ext_d, bool& included,
			 const Generator** const pppoint) const {
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((maximize
				  ? "maximize(e, ...)"
				  : "minimize(e, ...)"), "e", expr);

  // For an empty polyhedron we simply return false.
  if (marked_empty()
      || (has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !update_generators()))
    return false;

  // The polyhedron has updated, possibly pending generators.
  // The following loop will iterate through the generator
  // to find the extremum.
  mpq_class extremum;

  // True if we have no other candidate extremum to compare with.
  bool first_candidate = true;

  // To store the position of the current candidate extremum.
  // Initialized only to avoid a compiler warning.
  dimension_type ext_position = 0;

  // Whether the current candidate extremum is included or not.
  // Initialized only to avoid a compiler warning.
  bool ext_included = false;

  TEMP_INTEGER(sp);
  for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
    const Generator& g = gen_sys[i];
    homogeneous_scalar_product_assign(sp, expr, g);
    // Lines and rays in `*this' can cause `expr' to be unbounded.
    if (g[0] == 0) {
      const int sp_sign = sgn(sp);
      if (sp_sign != 0
	  && (g.is_line()
	      || (maximize && sp_sign > 0)
	      || (!maximize && sp_sign < 0)))
	// `expr' is unbounded in `*this'.
	return false;
    }
    else {
      // We have a point or a closure point.
      assert(g.is_point() || g.is_closure_point());
      // Notice that we are ignoring the constant term in `expr' here.
      // We will add it to the extremum as soon as we find it.
      mpq_class candidate;
      Checked::assign<Check_Overflow_Policy>(candidate.get_num(),
					     raw_value(sp));
      Checked::assign<Check_Overflow_Policy>(candidate.get_den(),
					     raw_value(g[0]));
      candidate.canonicalize();
      const bool g_is_point = g.is_point();
      if (first_candidate
	  || (maximize
	      && (candidate > extremum
		  || (g_is_point
		      && !ext_included
		      && candidate == extremum)))
	  || (!maximize
	      && (candidate < extremum
		  || (g_is_point
		      && !ext_included
		      && candidate == extremum)))) {
	// We have a (new) candidate extremum.
	first_candidate = false;
	extremum = candidate;
	ext_position = i;
	ext_included = g_is_point;
      }
    }
  }

  // Add in the constant term in `expr'.
  mpz_class n;
  Checked::assign<Check_Overflow_Policy>(n, raw_value(expr[0]));
  extremum += n;;

  // The polyhedron is bounded in the right direction and we have
  // computed the extremum: write the result into the caller's structures.
  assert(!first_candidate);
  ext_n = Coefficient(extremum.get_num());
  ext_d = Coefficient(extremum.get_den());
  included = ext_included;
  if (pppoint != 0)
    *pppoint = &gen_sys[ext_position];

  return true;
}

#endif

void
PPL::Grid::set_zero_dim_univ() {
  status.set_zero_dim_univ();
  space_dim = 0;
  con_sys.clear();
  gen_sys.clear();
}

void
PPL::Grid::set_empty() {
  status.set_empty();
  // The polyhedron is empty: we can thus throw away everything.
  con_sys.clear();
  gen_sys.clear();
#if 0
  sat_c.clear();
  sat_g.clear();
#endif
}

bool
PPL::Grid::process_pending_congruences() const {
#if 0 // FIX
  assert(space_dim > 0 && !marked_empty());
  assert(has_pending_congruences() && !has_pending_generators());

  Grid& x = const_cast<Grid&>(*this);

  // Integrate the pending part of the system of congruences and minimize.
  // We need `sat_c' up-to-date and `con_sys' sorted (together with `sat_c').
  if (!x.sat_c_is_up_to_date())
    x.sat_c.transpose_assign(x.sat_g);
  if (!x.con_sys.is_sorted())
    x.obtain_sorted_congruences_with_sat_c();
  // We sort in place the pending congruences, erasing those congruences
  // that also occur in the non-pending part of `con_sys'.
  x.con_sys.sort_pending_and_remove_duplicates();
  if (x.con_sys.num_pending_rows() == 0) {
    // All pending congruences were duplicates.
    x.clear_pending_congruences();
    assert(OK(true));
    return true;
  }

  const bool empty = add_and_minimize(true, x.con_sys, x.gen_sys, x.sat_c);
  assert(x.con_sys.num_pending_rows() == 0);

  if (empty)
    x.set_empty();
  else {
    x.clear_pending_congruences();
    x.clear_sat_g_up_to_date();
    x.set_sat_c_up_to_date();
  }
  assert(OK(!empty));
  return !empty;
#endif
  return true;
}

void
PPL::Grid::process_pending_generators() const {
  assert(space_dim > 0 && !marked_empty());
  assert(has_pending_generators() && !has_pending_congruences());

  Grid& x = const_cast<Grid&>(*this);

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

  // FIX could this return true (ie that the grid is empty)
  // FIX if so set_empty
  add_and_minimize(x.gen_sys, x.con_sys, x.sat_g);
  assert(x.gen_sys.num_pending_rows() == 0);

  x.clear_pending_generators();
  x.clear_sat_c_up_to_date();
  x.set_sat_g_up_to_date();
}
#if 0
void
PPL::Grid::remove_pending_to_obtain_congruences() const {
  assert(has_something_pending());

  Grid& x = const_cast<Grid&>(*this);

  // If the polyhedron has pending congruences, simply unset them.
  if (x.has_pending_congruences()) {
    // Integrate the pending congruences, which are possibly not sorted.
    x.con_sys.unset_pending_rows();
    x.con_sys.set_sorted(false);
    x.clear_pending_congruences();
    x.clear_congruences_minimized();
    x.clear_generators_up_to_date();
  }
  else {
    assert(x.has_pending_generators());
    // We must process the pending generators and obtain the
    // corresponding system of congruences.
    x.process_pending_generators();
  }
  assert(OK(true));
}

bool
PPL::Grid::remove_pending_to_obtain_generators() const {
  assert(has_something_pending());

  Grid& x = const_cast<Grid&>(*this);

  // If the polyhedron has pending generators, simply unset them.
  if (x.has_pending_generators()) {
    // Integrate the pending generators, which are possibly not sorted.
    x.gen_sys.unset_pending_rows();
    x.gen_sys.set_sorted(false);
    x.clear_pending_generators();
    x.clear_generators_minimized();
    x.clear_congruences_up_to_date();
    assert(OK(true));
    return true;
  }
  else {
    assert(x.has_pending_congruences());
    // We must integrate the pending congruences and obtain the
    // corresponding system of generators.
    return x.process_pending_congruences();
  }
}
#endif

bool
PPL::Grid::update_congruences() const {
  assert(space_dim > 0);
  assert(!marked_empty());
  assert(generators_are_up_to_date());
  // We assume the polyhedron has no pending congruences or generators.
  assert(!has_something_pending());
  std::cout << "update_congruences" << std::endl; // FIX

  Grid& gr = const_cast<Grid&>(*this);
  if (minimize(false, gr.gen_sys, gr.con_sys, gr.sat_c)) {
    gr.set_empty();
    return false;
  }
  // `sat_c' is the only saturation matrix up-to-date.
  gr.set_sat_c_up_to_date();
  gr.clear_sat_g_up_to_date();
  // The system of congruences and the system of generators are
  // minimized.
  gr.set_congruences_minimized();
  gr.set_generators_minimized();
  return true;
}

bool
PPL::Grid::update_generators() const {
  assert(space_dim > 0);
  assert(!marked_empty());
  assert(congruences_are_up_to_date());
  // We assume the polyhedron has no pending congruences or generators.
  assert(!has_something_pending());
  std::cout << "update_generators" << std::endl; // FIX

  Grid& x = const_cast<Grid&>(*this);
  // If the system of congruences is not consistent the polyhedron is
  // empty.
  if (minimize(true, x.con_sys, x.gen_sys, x.sat_g)) {
    x.set_empty();
    return false;
  }
  // `sat_g' is the only saturation matrix up-to-date.
  x.set_sat_g_up_to_date();
  x.clear_sat_c_up_to_date();
  // The system of congruences and the system of generators are
  // minimized.
  x.set_congruences_minimized();
  x.set_generators_minimized();
  return true;
}

#if 0
void
PPL::Grid::update_sat_c() const {
  assert(congruences_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_c_is_up_to_date());

  // We only consider non-pending rows.
  const dimension_type csr = con_sys.first_pending_row();
  const dimension_type gsr = gen_sys.first_pending_row();
  Grid& x = const_cast<Grid&>(*this);

  // The columns of `sat_c' represent the congruences and
  // its rows represent the generators: resize accordingly.
  x.sat_c.resize(gsr, csr);
  for (dimension_type i = gsr; i-- > 0; )
    for (dimension_type j = csr; j-- > 0; ) {
      const int sp_sign = scalar_product_sign(con_sys[j], gen_sys[i]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[i]' violates the congruence
      // `con_sys[j]' and it is not possible because both generators
      // and congruences are up-to-date.
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
#endif
void
PPL::Grid::update_sat_g() const {
  assert(congruences_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_g_is_up_to_date());

  // We only consider non-pending rows.
  //const dimension_type csr = con_sys.first_pending_row();
  const dimension_type csr = 0;
  const dimension_type gsr = gen_sys.first_pending_row();
  Grid& x = const_cast<Grid&>(*this);

  // The columns of `sat_g' represent generators and its
  // rows represent the congruences: resize accordingly.
  x.sat_g.resize(csr, gsr);
  for (dimension_type i = csr; i-- > 0; )
    for (dimension_type j = gsr; j-- > 0; ) {
      const int sp_sign = scalar_product_sign(gen_sys[i], con_sys[j]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[j]' violates the congruence
      // `con_sys[i]' and it is not possible because both generators
      // and congruences are up-to-date.
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
#if 0
void
PPL::Grid::obtain_sorted_congruences() const {
  assert(congruences_are_up_to_date());
  // `con_sys' will be sorted up to `index_first_pending'.
  Grid& x = const_cast<Grid&>(*this);
  if (!x.con_sys.is_sorted())
    if (x.sat_g_is_up_to_date()) {
      // Sorting congruences keeping `sat_g' consistent.
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
      // we just sort the congruences.
      x.con_sys.sort_rows();

  assert(con_sys.check_sorted());
}
#endif
void
PPL::Grid::obtain_sorted_generators() const {
  assert(generators_are_up_to_date());

  if (gen_sys.is_sorted())
    return;

  // `gen_sys' will be sorted up to `index_first_pending'.

  Grid& x = const_cast<Grid&>(*this);

  if (x.sat_c_is_up_to_date()) {
    // Sorting generators keeping 'sat_c' consistent.
    x.gen_sys.sort_and_remove_with_sat(x.sat_c);
    // `sat_g' is not up-to-date anymore.
    x.clear_sat_g_up_to_date();
  }
  else if (x.sat_g_is_up_to_date()) {
    // Obtaining `sat_c' from `sat_g' and proceeding like previous case.
#if 0 // FIX for assertion
    x.sat_c.transpose_assign(x.sat_g);
    x.gen_sys.sort_and_remove_with_sat(x.sat_c);
#else
    x.gen_sys.sort_rows();
#endif
    x.set_sat_c_up_to_date();
    x.clear_sat_g_up_to_date();
  }
  else
    // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
    // the generators.
    x.gen_sys.sort_rows();

  assert(gen_sys.check_sorted());
}
#if 0
void
PPL::Grid::obtain_sorted_congruences_with_sat_c() const {
  assert(congruences_are_up_to_date());
  assert(congruences_are_minimized());
  // `con_sys' will be sorted up to `index_first_pending'.
  Grid& x = const_cast<Grid&>(*this);
  // At least one of the saturation matrices must be up-to-date.
  if (!x.sat_c_is_up_to_date() && !x.sat_g_is_up_to_date())
    x.update_sat_c();

  if (x.con_sys.is_sorted()) {
    if (x.sat_c_is_up_to_date())
      // If congruences are already sorted and sat_c is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!x.sat_g_is_up_to_date()) {
      // If congruences are not sorted and sat_g is not up-to-date
      // we obtain sat_g from sat_c (that has to be up-to-date)...
      x.sat_g.transpose_assign(x.sat_c);
      x.set_sat_g_up_to_date();
    }
    // ... and sort it together with congruences.
    x.con_sys.sort_and_remove_with_sat(x.sat_g);
  }
  // Obtaining sat_c from sat_g.
  x.sat_c.transpose_assign(x.sat_g);
  x.set_sat_c_up_to_date();
  // Congruences are sorted now.
  x.con_sys.set_sorted(true);

  assert(con_sys.check_sorted());
}
#endif
void
PPL::Grid::obtain_sorted_generators_with_sat_g() const {
  assert(generators_are_up_to_date());
  // `gen_sys' will be sorted up to `index_first_pending'.
  Grid& x = const_cast<Grid&>(*this);
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
      // we obtain sat_c from sat_g (that has to be up-to-date)...
      x.sat_c.transpose_assign(x.sat_g);
      x.set_sat_c_up_to_date();
    }
    // ... and sort it together with generators.
    //x.gen_sys.sort_and_remove_with_sat(x.sat_c); // FIX
  }
  // Obtaining sat_g from sat_c.
  x.sat_g.transpose_assign(sat_c);
  x.set_sat_g_up_to_date();
  // Generators are sorted now.
  x.gen_sys.set_sorted(true);

  assert(gen_sys.check_sorted());
}

bool
PPL::Grid::minimize() const {
  std::cout << "minimize()" << std::endl;
  // 0-dim space and empty polyhedra are already minimized.
  // FIX should they return the same val?
  if (marked_empty())
    return false;
  if (space_dim == 0)
    return true;

#if 0
  // If the polyhedron has something pending, process it.
  if (has_something_pending()) {
    const bool not_empty = process_pending();
    assert(OK());
    return not_empty;
  }
#endif

  // Here there are no pending congruences or generators.
  // Is the polyhedron already minimized?
  if (congruences_are_minimized() && generators_are_minimized())
    return true;

  // If congruences or generators are up-to-date, invoking
  // update_generators() or update_congruences(), respectively, FIX
  // both of which minimize both congruences and generators.  If
  // congruences and generators are up-to-date then either function
  // can be called.
  if (congruences_are_up_to_date()) {
    // We may discover here that `*this' is empty.
    const bool ret = update_generators();
    assert(OK());
    return ret;
  }
  else {
    assert(generators_are_up_to_date());
    update_congruences();
    assert(OK());
    return true;
  }
}

bool
PPL::Grid::strongly_minimize_congruences() const {
  //assert(!is_necessarily_closed()); // FIX

  // From the user perspective, the polyhedron will not change.
  Grid& x = const_cast<Grid&>(*this);

  // We need `con_sys' (weakly) minimized and `gen_sys' up-to-date.
  // `minimize()' will process any pending congruences or generators.
  if (!minimize())
    return false;

  // If the grid `*this' is zero-dimensional
  // at this point it must be a universe polyhedron.
  if (x.space_dim == 0)
    return true;

#if 0
  // We also need `sat_g' up-to-date.
  if (!sat_g_is_up_to_date()) {
    assert(sat_c_is_up_to_date());
    x.sat_g.transpose_assign(sat_c);
  }

  // These Saturation_Row's will be later used as masks in order to
  // check saturation conditions restricted to particular subsets of
  // the generator system.
  Saturation_Row sat_all_but_rays;
  Saturation_Row sat_all_but_points;
  Saturation_Row sat_all_but_closure_points;

  const dimension_type gs_rows = gen_sys.num_rows();
  const dimension_type n_lines = gen_sys.num_lines();
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
			       "strongly_minimize_congruences.");
    }
  Saturation_Row sat_lines_and_rays;
  set_union(sat_all_but_points, sat_all_but_closure_points,
	    sat_lines_and_rays);
  Saturation_Row sat_lines_and_closure_points;
  set_union(sat_all_but_rays, sat_all_but_points,
	    sat_lines_and_closure_points);
  Saturation_Row sat_lines;
  set_union(sat_lines_and_rays, sat_lines_and_closure_points,
	    sat_lines);

  // These flags are maintained to later decide if we have to add the
  // eps_leq_one congruence and whether or not the congruence system
  // was changed.
  bool changed = false;
  bool found_eps_leq_one = false;

  // For all the strict inequalities in `con_sys', check for
  // eps-redundancy and eventually move them to the bottom part of the
  // system.
  Congruence_System& cs = x.con_sys;
  Saturation_Matrix& sat = x.sat_g;
  dimension_type cs_rows = cs.num_rows();
  const dimension_type eps_index = cs.num_columns() - 1;
  for (dimension_type i = 0; i < cs_rows; )
    if (cs[i].is_strict_inequality()) {
      // First, check if it is saturated by no closure points
      Saturation_Row sat_ci;
      set_union(sat[i], sat_lines_and_closure_points, sat_ci);
      if (sat_ci == sat_lines) {
	// It is saturated by no closure points.
	if (!found_eps_leq_one) {
	  // Check if it is the eps_leq_one congruence.
	  const Congruence& c = cs[i];
	  bool all_zeroes = true;
	  for (dimension_type k = eps_index; k-- > 1; )
	    if (c[k] != 0) {
	      all_zeroes = false;
	      break;
	    }
	  if (all_zeroes && (c[0] + c[eps_index] == 0)) {
	    // We found the eps_leq_one congruence.
	    found_eps_leq_one = true;
	    // Consider next congruence.
	    ++i;
	    continue;
	  }
	}
	// Here `cs[i]' is not the eps_leq_one congruence,
	// so it is eps-redundant.
	// Move it to the bottom of the congruence system,
	// while keeping `sat_g' consistent.
	--cs_rows;
	std::swap(cs[i], cs[cs_rows]);
	std::swap(sat[i], sat[cs_rows]);
	// The congruence system is changed.
	changed = true;
	// Continue by considering next congruence,
	// which is already in place due to the swap.
	continue;
      }
      // Now we check if there exists another strict inequality
      // congruence having a superset of its saturators,
      // when disregarding points.
      sat_ci.clear();
      set_union(sat[i], sat_all_but_points, sat_ci);
      bool eps_redundant = false;
      for (dimension_type j = 0; j < cs_rows; ++j)
	if (i != j && cs[j].is_strict_inequality()
	    && subset_or_equal(sat[j], sat_ci)) {
	  // Congruence `cs[i]' is eps-redundant:
	  // move it to the bottom of the congruence system,
	  // while keeping `sat_g' consistent.
	  --cs_rows;
	  std::swap(cs[i], cs[cs_rows]);
	  std::swap(sat[i], sat[cs_rows]);
	  eps_redundant = true;
	  // The congruence system is changed.
	  changed = true;
	  break;
	}
      // Continue with next congruence, which is already in place
      // due to the swap if we have found an eps-redundant congruence.
      if (!eps_redundant)
	++i;
    }
    else
      // `cs[i]' is not a strict inequality: consider next congruence.
      ++i;

  if (changed) {
    // If the congruence system has been changed and we haven't found the
    // eps_leq_one congruence, insert it to force an upper bound on epsilon.
    if (!found_eps_leq_one) {
      // Note: we overwrite the first of the eps-redundant congruences found.
      assert(cs_rows < cs.num_rows());
      Congruence& eps_leq_one = cs[cs_rows];
      eps_leq_one[0] = 1;
      eps_leq_one[eps_index] = -1;
      for (dimension_type k = eps_index; k-- > 1; )
	eps_leq_one[k] = 0;
      // Bump number of rows.
      ++cs_rows;
    }
    // Erase the eps-redundant congruences, if there are any (the
    // remaining congruences are not pending).
    if (cs_rows < cs.num_rows()) {
      cs.erase_to_end(cs_rows);
      cs.unset_pending_rows();
    }
    // The congruence system is no longer sorted.
    cs.set_sorted(false);
    // The generator system is no longer up-to-date.
    x.clear_generators_up_to_date();
  }
#endif

  assert(OK());
  return true;
}

bool
PPL::Grid::strongly_minimize_generators() const {
  //assert(!is_necessarily_closed()); // FIX

  // From the user perspective, the polyhedron will not change.
  Grid& x = const_cast<Grid&>(*this);

  // We need `gen_sys' (weakly) minimized and `con_sys' up-to-date.
  // `minimize()' will process any pending congruences or generators.
  if (!minimize())
    return false;

  // If the polyhedron `*this' is zero-dimensional
  // at this point it must be a universe polyhedron.
  if (x.space_dim == 0)
    return true;

#if 0
  // We also need `sat_c' up-to-date.
  if (!sat_c_is_up_to_date()) {
    assert(sat_g_is_up_to_date());
    x.sat_c.transpose_assign(sat_g);
  }
#endif

#if 0
  // This Saturation_Row will have all and only the indexes
  // of strict inequalities set to 1.
  Saturation_Row sat_all_but_strict_ineq;
  const dimension_type cs_rows = con_sys.num_rows();
  const dimension_type n_equals = con_sys.num_equalities();
  for (dimension_type i = cs_rows; i-- > n_equals; )
    if (con_sys[i].is_strict_inequality())
      sat_all_but_strict_ineq.set(i);

  // Will record whether or not we changed the generator system.
  bool changed = false;

  // For all points in the generator system, check for eps-redundancy
  // and eventually move them to the bottom part of the system.
  Generator_System& gs = const_cast<Generator_System&>(gen_sys);
  Saturation_Matrix& sat = const_cast<Saturation_Matrix&>(sat_c);
  dimension_type gs_rows = gs.num_rows();
  const dimension_type n_lines = gs.num_lines();
  const dimension_type eps_index = gs.num_columns() - 1;
  for (dimension_type i = n_lines; i < gs_rows; )
    if (gs[i].is_point()) {
      // Compute the Saturation_Row corresponding to the candidate point
      // when strict inequality congruences are ignored.
      Saturation_Row sat_gi;
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

  // If needed, erase the eps-redundant generators (also updating
  // `index_first_pending').
  if (gs_rows < gs.num_rows()) {
    gs.erase_to_end(gs_rows);
    gs.unset_pending_rows();
  }

  if (changed) {
    // The generator system is no longer sorted.
    x.gen_sys.set_sorted(false);
    // The congruence system is no longer up-to-date.
    x.clear_congruences_up_to_date();
  }
#endif

  assert(OK());
  return true;
}

void
PPL::Grid::throw_runtime_error(const char* method) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << "." << std::endl;
  throw std::runtime_error(s.str());
}

void
PPL::Grid::throw_invalid_argument(const char* method,
					const char* reason) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << reason << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
				       const char* ph_name,
				       const Grid& ph) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << ph_name << " is a ";
#if 0
  if (ph.is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid." << std::endl;
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* c_name,
					     const Congruence&) const {
  //assert(is_necessarily_closed());  // FIX
  std::ostringstream s;
  s << "PPL::C_Grid::" << method << ":" << std::endl
    << c_name << " is a strict inequality.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* g_name,
					     const Generator&) const {
  //assert(is_necessarily_closed()); // FIX
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << g_name << " is a closure point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* cs_name,
					     const Congruence_System&) const {
  //assert(is_necessarily_closed()); // FIX
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << cs_name << " contains strict inequalities.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* gs_name,
					     const Generator_System&) const {
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << gs_name << " contains closure points.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* other_name,
					      dimension_type other_dim) const {
  std::ostringstream s;
  s << "PPL::"
    //<< (is_necessarily_closed() ? "C_" : "NNC_") // FIX
    << "Grid::" << method << ":\n"
    << "this->space_dimension() == " << space_dimension() << ", "
    << other_name << ".space_dimension() == " << other_dim << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* ph_name,
					      const Grid& ph) const {
  throw_dimension_incompatible(method, ph_name, ph.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* e_name,
					      const Linear_Expression& e) const {
  throw_dimension_incompatible(method, e_name, e.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* c_name,
					      const Congruence& c) const {
  throw_dimension_incompatible(method, c_name, c.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* g_name,
					      const Generator& g) const {
  throw_dimension_incompatible(method, g_name, g.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* cs_name,
					      const Congruence_System& cs) const {
  throw_dimension_incompatible(method, cs_name, cs.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* gs_name,
					      const Generator_System& gs) const {
  throw_dimension_incompatible(method, gs_name, gs.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					      const char* var_name,
					      const Variable var) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << var_name << ".space_dimension() == " << var.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::
throw_dimension_incompatible(const char* method,
			     dimension_type required_space_dim) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required space dimension == " << required_space_dim << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_space_dimension_overflow(const Topology topol,
					  const char* method,
					  const char* reason) {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (topol == NECESSARILY_CLOSED)
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << reason << ".";
  throw std::length_error(s.str());
}

void
PPL::Grid::throw_invalid_generator(const char* method,
					 const char* g_name) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << "*this is an empty polyhedron and "
    << g_name << " is not a point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_invalid_generators(const char* method,
					  const char* gs_name) const {
  std::ostringstream s;
  s << "PPL::";
#if 0
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
#endif
  s << "Grid::" << method << ":" << std::endl
    << "*this is an empty polyhedron and" << std::endl
    << "the non-empty generator system " << gs_name << " contains no points.";
  throw std::invalid_argument(s.str());
}
