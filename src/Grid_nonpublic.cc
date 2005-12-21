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
#include "Grid_Generator.defs.hh"
#include "Scalar_Products.defs.hh"
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
  else {
    // Here `space_dim == 0'.
    if (cgs.num_columns() > 1)
      // See if an inconsistent congruence has been passed.
      for (dimension_type i = cgs.num_rows(); i-- > 0; )
	if (cgs[i].is_trivial_false()) {
	  // Inconsistent congruence found: the grid is empty.
	  set_empty();
	  assert(OK());
	  return;
	}
    set_zero_dim_univ();
  }

  assert(OK());
}

void
PPL::Grid::construct(const Grid_Generator_System& const_gs) {
  // Protecting against space dimension overflow is up to the caller.
  assert(const_gs.space_dimension() <= max_space_dimension());

  // TODO: this implementation is just an executable specification.
  Grid_Generator_System gs = const_gs;

  // Set the space dimension.
  space_dim = gs.space_dimension();

  // An empty set of generators defines the empty grid.
  if (gs.num_rows() == 0) {
    set_empty();
    return;
  }

  // Non-empty valid generator systems have a supporting point, at least.
  if (!gs.has_points())
    throw_invalid_generators("Grid(const_gs)", "gs");

  if (space_dim > 0) {
    // Steal the rows from `gs'.
    std::swap(gen_sys, gs);
    normalize_divisors(gen_sys);

    // Generators are now up-to-date.
    set_generators_up_to_date();
  }
  else
    set_zero_dim_univ();

  assert(OK());
}

PPL::Grid::Three_Valued_Boolean
PPL::Grid::quick_equivalence_test(const Grid& y) const {
  // Private method: the caller must ensure the following.
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
      //  - and if there are no lines, the same generators.
    if (x_num_lines == 0) {
      // Check for syntactic identity.
      if (x.gen_sys == y.gen_sys)
	return Grid::TVB_TRUE;
      else
	return Grid::TVB_FALSE;
    }
  }

  // TODO: Consider minimizing the systems and re-performing these
  //       checks.

  if (css_normalized)
    if (x.con_sys == y.con_sys)
      return Grid::TVB_TRUE;
    else
      return Grid::TVB_FALSE;

  return Grid::TVB_DONT_KNOW;
}

bool
PPL::Grid::is_included_in(const Grid& y) const {
  // Private method: the caller must ensure the following.
  assert(space_dim == y.space_dim);
  assert(!marked_empty() && !y.marked_empty() && space_dim > 0);

  const Grid& x = *this;

#if BE_LAZY
  if (!x.generators_are_up_to_date() && !x.update_generators())
    // Updating found `x' empty.
    return true;
  y.congruences_are_up_to_date() || y.update_congruences();
#else
  if (!x.generators_are_minimized() && !x.minimize())
    // Minimizing found `x' empty.
    return true;
  y.congruences_are_minimized() || y.minimize();
#endif

  assert(x.OK());
  assert(y.OK());

  const Grid_Generator_System& gs = x.gen_sys;
  const Congruence_System& cgs = y.con_sys;

  dimension_type num_rows = gs.num_rows();
  for (dimension_type i = num_rows; i-- > 0; )
    if (!cgs.satisfies_all_congruences(gs[i]))
      return false;

  // Inclusion holds.
  return true;
}

bool
PPL::Grid::bounds(const Linear_Expression& expr,
		  const char* method_call) const {
  // The dimension of `expr' must be at most the dimension of *this.
  if (space_dim < expr.space_dimension())
    throw_dimension_incompatible(method_call, "e", expr);

  // A zero-dimensional or empty grid bounds everything.
  if (space_dim == 0
      || marked_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return true;

  // The generators are up to date.
  for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
    const Grid_Generator& g = gen_sys[i];
    // Only lines and parameters in `*this' can cause `expr' to be
    // unbounded.
    if (g.is_line_or_parameter()) {
      const int sp_sign = Scalar_Products::homogeneous_sign(expr, g);
      if (sp_sign != 0)
	// `*this' does not bound `expr'.
	return false;
    }
  }
  return true;
}

bool
PPL::Grid::max_min(const Linear_Expression& expr,
		   char* method_call,
		   Coefficient& ext_n, Coefficient& ext_d, bool& included,
		   Grid_Generator* point) const {
  if (bounds(expr, method_call)) {
    if (marked_empty())
      return false;
    if (space_dim == 0) {
      ext_n = 0;
      ext_d = 1;
      included = true;
      if (point)
	*point = Grid_Generator::point();
      return true;
    }
    if (!generators_are_minimized()) {
      // Minimize the generator system.
      Grid& gr = const_cast<Grid&>(*this);
      gr.simplify(gr.gen_sys, gr.dim_kinds);
      gr.set_generators_minimized();
    }

    const Grid_Generator& gen = gen_sys[0];
    Scalar_Products::homogeneous_assign(ext_n, expr, gen);
    ext_n += expr.inhomogeneous_term();
    ext_d = gen.divisor();
    // Reduce ext_n and ext_d.
    TEMP_INTEGER(gcd);
    gcd_assign(gcd, ext_n, ext_d);
    exact_div_assign(ext_n, gcd);
    exact_div_assign(ext_d, gcd);

    included = true;
    if (point) {
      *point = gen;
      point->strong_normalize();
    }
    return true;
  }
  return false;
}

void
PPL::Grid::set_zero_dim_univ() {
  status.set_zero_dim_univ();
  space_dim = 0;
  con_sys.clear();
  gen_sys.clear();
  gen_sys.insert(Grid_Generator::point());
}

void
PPL::Grid::set_empty() {
  status.set_empty();

  // Replace gen_sys with an empty system of the right dimension.
  Grid_Generator_System gs(space_dim);
  gen_sys.swap(gs);

  // Extend the zero dim false congruence system to the appropriate
  // dimension and then swap it with `con_sys'.
  Congruence_System cgs(Congruence::zero_dim_false());
  cgs.increase_space_dimension(space_dim);
  const_cast<Congruence_System&>(con_sys).swap(cgs);
}

bool
PPL::Grid::update_congruences() const {
  // The caller must ensure that the generators are up to date.
  assert(space_dim > 0);
  assert(!marked_empty());
  assert(gen_sys.num_rows() > 0);
  assert(gen_sys.space_dimension() > 0);

  Grid& gr = const_cast<Grid&>(*this);

  if (!generators_are_minimized())
    gr.simplify(gr.gen_sys, gr.dim_kinds);

  // `gen_sys' contained rows before being reduced, so it should
  // contain at least a single point afterwards.
  assert(gen_sys.num_rows() > 0);

  // Populate `con_sys' with congruences characterizing the grid
  // described by `gen_sys'.
  gr.conversion(gr.gen_sys, gr.con_sys, gr.dim_kinds);

  // Both systems are minimized.
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

  if (!congruences_are_minimized())
    // Either the system of congruences is consistent, or the grid is
    // empty.
    if (simplify(x.con_sys, x.dim_kinds)) {
      x.set_empty();
      return false;
    }

  // Populate gen_sys with generators characterizing the grid
  // described by con_sys.
  conversion(x.con_sys, x.gen_sys, x.dim_kinds);

  // Both systems are minimized.
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

  // Are both systems already minimized?
  if (congruences_are_minimized() && generators_are_minimized())
    return true;

  // Invoke update_generators, update_congruences or simplify,
  // depending on the state of the systems.
  if (congruences_are_up_to_date()) {
    if (generators_are_up_to_date()) {
      Grid& gr = const_cast<Grid&>(*this);
      // Only one of the systems can be minimized here.
      if (congruences_are_minimized()) {
	// Minimize the generator system.
	gr.simplify(gr.gen_sys, gr.dim_kinds);
	gr.set_generators_minimized();
      }
      else {
	// Minimize the congruence system.
	if (simplify(gr.con_sys, gr.dim_kinds)) {
	  gr.set_empty();
	  assert(OK());
	  return false;
	}
	gr.set_congruences_minimized();
	if (!generators_are_minimized()) {
	  // Minimize the generator system.
	  gr.simplify(gr.gen_sys, gr.dim_kinds);
	  gr.set_generators_minimized();
	}
      }
    }
    else {
      // Updating the generators may reveal that `*this' is empty.
      const bool ret = update_generators();
      assert(OK());
      return ret;
    }
  }
  else {
    assert(generators_are_up_to_date());
    update_congruences();
  }
  assert(OK());
  return true;
}

void
PPL::Grid::normalize_divisors(Grid_Generator_System& sys,
			      Grid_Generator_System& gen_sys) {
  dimension_type row = 0;
  dimension_type num_rows = gen_sys.num_rows();
  // Find first point in gen_sys.
  while (gen_sys[row].is_line_or_parameter())
    if (++row == num_rows)
      // All rows are lines or parameters; generators should always
      // contain a point.
      throw std::runtime_error("PPL::Grid::normalize_divisors(sys, gen_sys).");
  Grid_Generator& first_point = gen_sys[row];
  Coefficient_traits::const_reference gen_sys_divisor = first_point.divisor();
  Coefficient divisor = normalize_divisors(sys, gen_sys_divisor);
  if (divisor != gen_sys_divisor)
    // The divisors of the points in gen_sys are always the same, so
    // the new divisor will be the LCM of this value and `divisor',
    // hence the third argument.
    normalize_divisors(gen_sys, divisor, &first_point);
}

PPL::Coefficient
PPL::Grid::normalize_divisors(Grid_Generator_System& sys,
			      Coefficient_traits::const_reference divisor,
			      Grid_Generator* first_point) {
  assert(divisor >= 0);
  if (sys.space_dimension() > 0 && divisor > 0) {
    TEMP_INTEGER(lcm);
    lcm = divisor;

    dimension_type row = 0;
    dimension_type num_rows = sys.num_rows();

    if (first_point)
      lcm_assign(lcm, (*first_point).divisor());
    else {
      // Move to the first point or parameter.
      while (sys[row].is_line())
	if (++row == num_rows)
	  // All rows are lines.
	  return divisor;

      // Calculate the LCM of `divisor' and the divisor of every
      // point or parameter.
      while (row < num_rows) {
	Grid_Generator& g = sys[row];
	if (g.is_parameter_or_point())
	  lcm_assign(lcm, g.divisor());
	++row;
      }
    }

    // Represent every point and every parameter using the LCM as the
    // divisor.
    for (dimension_type row = 0; row < num_rows; ++row)
      sys[row].scale_to_divisor(lcm);

    return lcm;
  }
  return divisor;
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
					const Grid_Generator& g) const {
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
					const Grid_Generator_System& gs) const {
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
