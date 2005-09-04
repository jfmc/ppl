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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

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

void
PPL::Grid::construct(const Congruence_System& ccgs) {
  // Protecting against space dimension overflow is up to the caller.
  assert(ccgs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Congruence_System cgs = ccgs;

  // Set the space dimension.
  space_dim = cgs.space_dimension();

  if (space_dim > 0) {
    // Stealing the rows from `cgs'.
    std::swap(con_sys, cgs);
    con_sys.normalize_moduli();
    set_congruences_up_to_date();
  }
  else
    // Here `space_dim == 0'.
    if (cgs.num_columns() > 0)
      // See if an inconsistent congruence has been passed.
      for (dimension_type i = cgs.num_rows(); i-- > 0; )
	if (cgs[i].is_trivial_false()) {
	  // Inconsistent congruence found: the grid is empty.
	  set_empty();
	  break;
	}

  gen_sys.set_sorted(false);

  assert(OK());
}

void
PPL::Grid::construct(const Generator_System& const_gs,
		     const bool convert_rays_to_lines) {
  // Protecting against space dimension overflow is up to the caller.
  assert(const_gs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Generator_System gs = const_gs;

  // An empty set of generators defines the empty grid.
  if (gs.num_rows() == 0) {
    space_dim = gs.space_dimension();
    status.set_empty();
    gen_sys.set_sorted(false);
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators("Grid(const_gs)", "gs");

  const dimension_type gs_space_dim = gs.space_dimension();

  if (!gs.is_necessarily_closed()) {
    // A NOT_NECESSARILY_CLOSED generator system can be converted in
    // to a NECESSARILY_CLOSED one only if it does not contain closure
    // points.
    if (gs.has_closure_points())
      throw_topology_incompatible("Grid(const_gs)", "const_gs", gs);
    gs.remove_trailing_columns(1); // The epsilon coefficient column.
    gs.set_necessarily_closed();
  }

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // FIX for now convert rays to lines
    if (convert_rays_to_lines)
      for (dimension_type row = 0; row < gen_sys.num_rows(); ++row) {
	Generator& g = gen_sys[row];
	if (g.is_ray()) {
	  g.set_is_line();
	  g.strong_normalize();
	}
      }
    normalize_divisors(gen_sys);
    gen_sys.unset_pending_rows();
    gen_sys.set_sorted(false);

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
  gen_sys.set_sorted(false);
}

PPL::Grid::Three_Valued_Boolean
PPL::Grid::quick_equivalence_test(const Grid& y) const {
  // Private method: the caller must ensure the following.
  //assert(topology() == y.topology()); // FIX
  assert(space_dim == y.space_dim);
  assert(!marked_empty() && !y.marked_empty() && space_dim > 0);

  const Grid& x = *this;

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

  return Grid::TVB_DONT_KNOW;
}

bool
PPL::Grid::is_included_in(const Grid& y) const {
  // Private method: the caller must ensure the following.
  //assert(topology() == y.topology()); // FIX
  assert(space_dim == y.space_dim);
  assert(!marked_empty() && !y.marked_empty() && space_dim > 0);

  const Grid& x = *this;

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

#if BE_LAZY
  dimension_type num_rows = gs.num_rows();
  if (x.generators_are_minimized()) {
    TEMP_INTEGER(divisor);
    divisor = gs[0][0];
    for (dimension_type i = num_rows; i-- > 0; )
      if (!cgs.satisfies_all_congruences(gs[i], divisor))
	return false;
  }
  else
    for (dimension_type i = num_rows; i-- > 0; ) {
      const Generator& g = gs[i];
      if (!cgs.satisfies_all_congruences(g, g[0]))
	return false;
    }
#else
  TEMP_INTEGER(divisor);
  divisor = gs[0][0];
  dimension_type num_rows = gs.num_rows();
  for (dimension_type i = num_rows; i-- > 0; )
    if (!cgs.satisfies_all_congruences(gs[i], divisor))
      return false;
#endif

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

  // A zero-dimensional or empty grid bounds everything.
  if (space_dim == 0
      || marked_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return true;

  // The grid has updated generators.
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

  // For an empty grid we simply return false.
  if (marked_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return false;

  // The grid has updated generators.  The following loop will iterate
  // through the generator (FIX generators?) to find the extremum.
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

  // The grid is bounded in the right direction and we have
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
  gen_sys.set_sorted(false);
}

void
PPL::Grid::set_empty() {
  status.set_empty();
  // FIX The grid is empty, so clear the descriptions.
  con_sys.clear();
  gen_sys.clear();
  gen_sys.set_sorted(false);
}

bool
PPL::Grid::update_congruences() const {
  assert(space_dim > 0);
  assert(!marked_empty());
  assert(generators_are_up_to_date());

  Grid& gr = const_cast<Grid&>(*this);
  if (minimize(gr.gen_sys, gr.con_sys, gr.dim_kinds)) {
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

  Grid& x = const_cast<Grid&>(*this);
  // Either the system of congruences is consistent, or the grid is
  // empty.
  if (minimize(x.con_sys, x.gen_sys, x.dim_kinds)) {
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
  // 0-dimension and empty grids are already minimized.
  if (marked_empty())
    return false;
  if (space_dim == 0)
    return true;

  // Is the grid already minimized?
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

  // FIX is this (private) method necessary? perhaps use for strong reduc

  // From the user perspective, the grid stays the same.
  Grid& x = const_cast<Grid&>(*this);

  // We need `con_sys' (weakly) minimized and `gen_sys' up-to-date.
  if (!minimize())
    return false;

  // If the grid `*this' is zero-dimensional at this point then it
  // must be a universe grid.
  if (x.space_dim == 0)
    return true;

  assert(OK());
  return true;
}

bool
PPL::Grid::strongly_minimize_generators() const {

  // FIX is this method necessary? perhaps use for strong reduc

  // From the user perspective, the grid stays the same.
  Grid& x = const_cast<Grid&>(*this);

  // We need `gen_sys' (weakly) minimized and `con_sys' up-to-date.
  if (!minimize())
    return false;

  // If the grid `*this' is zero-dimensional at this point it must be
  // a universe grid.
  if (x.space_dim == 0)
    return true;

  assert(OK());
  return true;
}

void
PPL::Grid::normalize_divisors(Generator_System& sys,
			      Generator_System& gen_sys) {
  dimension_type row = 0;
  dimension_type num_rows = gen_sys.num_rows();
  // Find first point in gen_sys.
  while (gen_sys[row].is_line_or_equality())
    if (++row == num_rows)
      // All rows are lines; generators should always contain a point.
      //abort(); // FIX instead?
      throw std::runtime_error("PPL::Grid::normalize_divisors(sys, gen_sys).");
  TEMP_INTEGER(gen_sys_divisor);
  TEMP_INTEGER(divisor);
  gen_sys_divisor = gen_sys[row].divisor();
  divisor = normalize_divisors(sys, gen_sys_divisor);
  if (divisor != gen_sys_divisor)
    // FIX here normalize_divisors can skip the lcm calc
    normalize_divisors(gen_sys, divisor);
}

PPL::Coefficient
PPL::Grid::normalize_divisors(Generator_System& sys,
			      Coefficient_traits::const_reference divisor) {
  TEMP_INTEGER(lcm);
  lcm = divisor;
  if (sys.num_columns()) {
    dimension_type row = 0;
    dimension_type num_rows = sys.num_rows();

    // Move to the first point.
    while (!sys[row].is_point())
      if (++row == num_rows)
	return divisor;		// All rows are lines.

    if (lcm == 0) {
      lcm = sys[row][0];
      ++row;
    }

    // Calculate the LCM of the divisors of the points.
    while (row < num_rows) {
      Generator& g = sys[row];
      if (g.is_point())
	lcm_assign(lcm, g[0]);
      ++row;
    }

    // Represent each point using the LCM as the divisor.
    for (dimension_type row = 0; row < num_rows; ++row) {
      Generator& gen = sys[row];
      if (gen.is_ray_or_point_or_inequality()) {
	TEMP_INTEGER(factor);
	if (gen.is_point()) {
	  factor = lcm / gen[0];
	  gen[0] = lcm;
	}
	else
	  factor = lcm / sys[0][0];
	// FIX skip if divisor == lcm
	for (dimension_type col = 1; col < gen.size(); ++col)
	  gen[col] *= factor;
      }
    }
  }
  return lcm;
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
				       const char* g_name,
				       const Generator&) const {
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << g_name << " is a closure point.";
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
					const char* cg_name,
					const Congruence& cg) const {
  throw_dimension_incompatible(method, cg_name, cg.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					const char* c_name,
					const Constraint& c) const {
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
					const char* cgs_name,
					const Congruence_System& cgs) const {
  throw_dimension_incompatible(method, cgs_name, cgs.space_dimension());
}

void
PPL::Grid::throw_dimension_incompatible(const char* method,
					const char* cs_name,
					const Constraint_System& cs) const {
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
PPL::Grid::throw_space_dimension_overflow(const char* method,
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
    << "*this is an empty grid and "
    << g_name << " is not a point.";
  throw std::invalid_argument(s.str());
}

void
PPL::Grid::throw_invalid_generators(const char* method,
				    const char* gs_name) const {
  std::ostringstream s;
  s << "PPL::Grid::" << method << ":" << std::endl
    << "*this is an empty grid and" << std::endl
    << "the non-empty generator system " << gs_name << " contains no points.";
  throw std::invalid_argument(s.str());
}
