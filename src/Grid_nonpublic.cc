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
    simplify(con_sys);
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
    simplify(gen_sys);
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
}

PPL::Grid::Grid(const Topology topol, const Congruence_System& ccs)
  : con_sys(topol),
    gen_sys(topol) {
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
    gen_sys(topol) {
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
    gen_sys(topol) {
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
    gen_sys(topol) {
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
  if (num_rows && (gs[0].is_virtual() == false))
    // FIX could this be any different for NNC ph's? (?)
    if (cgs.satisfies_all_congruences(gs[0]) == false) {
      std::cout << "is_included_in... done (false 0)." << std::endl;
      return false;
    }
  for (dimension_type i = num_rows; i-- > 1; )
    // FIX gen = gs[i];
    if (gs[i].is_virtual() == false)
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
  // FIX The grid is empty, so throw away the descriptions.
  con_sys.clear();
  gen_sys.clear();
}

bool
PPL::Grid::process_pending_congruences() const {
#if 0 // FIX
  assert(space_dim > 0 && !marked_empty());
  assert(has_pending_congruences() && !has_pending_generators());

  Grid& x = const_cast<Grid&>(*this);

  // Integrate the pending part of the system of congruences and minimize.
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
#if 0
  if (!x.gen_sys.is_sorted())
    x.obtain_sorted_generators_with_sat_g();
#endif
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
  add_and_minimize(x.gen_sys, x.con_sys);
  assert(x.gen_sys.num_pending_rows() == 0);

  x.clear_pending_generators();
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

  Grid& gr = const_cast<Grid&>(*this);
  if (minimize(gr.gen_sys, gr.con_sys)) {
    gr.set_empty();
    return false;
  }
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

  Grid& x = const_cast<Grid&>(*this);
  // If the system of congruences is not consistent the polyhedron is
  // empty.
  if (minimize(x.con_sys, x.gen_sys)) {
    x.set_empty();
    return false;
  }
  // The system of congruences and the system of generators are
  // minimized.
  x.set_congruences_minimized();
  x.set_generators_minimized();
  return true;
}

bool
PPL::Grid::minimize() const {
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

  // FIX is this method necessary? perhaps use for strong reduc

  // From the user perspective, the polyhedron stays the same.
  Grid& x = const_cast<Grid&>(*this);

  // We need `con_sys' (weakly) minimized and `gen_sys' up-to-date.
  // `minimize()' will process any pending congruences or generators.
  if (!minimize())
    return false;

  // If the grid `*this' is zero-dimensional at this point it must be
  // a universe polyhedron.
  if (x.space_dim == 0)
    return true;

  assert(OK());
  return true;
}

bool
PPL::Grid::strongly_minimize_generators() const {

  // FIX is this method necessary? perhaps use for strong reduc

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

  assert(OK());
  return true;
}

void
PPL::Grid::throw_runtime_error(const char* method) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << "." << std::endl;
  throw std::runtime_error(s.str());
}

void
PPL::Grid::throw_invalid_argument(const char* method,
					const char* reason) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << reason << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
				       const char* ph_name,
				       const Grid& ph) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << ph_name << " is a Grid." << std::endl;
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* c_name,
					     const Congruence&) const {
  std::ostringstream s;
  s << "PPL::C_Grid::" << method << ":" << std::endl
    << c_name << " is a strict inequality.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* g_name,
					     const Generator&) const {
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << g_name << " is a closure point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_topology_incompatible(const char* method,
					     const char* cs_name,
					     const Congruence_System&) const {
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
  s << "PPL::Grid::" << method << ":\n"
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
  s << "PPL::Grid::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << var_name << ".space_dimension() == " << var.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::
throw_dimension_incompatible(const char* method,
			     dimension_type required_space_dim) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required space dimension == " << required_space_dim << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_space_dimension_overflow(const Topology topol,
					  const char* method,
					  const char* reason) {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << reason << ".";
  throw std::length_error(s.str());
}

void
PPL::Grid::throw_invalid_generator(const char* method,
					 const char* g_name) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << "*this is an empty polyhedron and "
    << g_name << " is not a point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_invalid_generators(const char* method,
					  const char* gs_name) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << "*this is an empty polyhedron and" << std::endl
    << "the non-empty generator system " << gs_name << " contains no points.";
  throw std::invalid_argument(s.str());
}
