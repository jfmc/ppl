/* Polyhedron class implementation (non-inline functions).
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Generator.defs.hh"
#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

#define BE_LAZY

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Polyhedron::throw_generic(const char* method,
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
					      const Matrix& y) const {
  std::ostringstream s;
  s << "PPL::";
  if (is_necessarily_closed())
    s << "C_";
  else
    s << "NNC_";
  s << "Polyhedron::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", system->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Polyhedron::throw_dimension_incompatible(const char* method,
					      const Row& y) const {
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
					      size_t required_dim) const {
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
					     const ConSys& ) const {
  assert(is_necessarily_closed());
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "constraint system contains strict inequalities.";
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
					     const GenSys& ) const {
  std::ostringstream s;
  s << "PPL::C_Polyhedron::" << method << ":" << std::endl
    << "generator system contains closure points.";
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

const PPL::ConSys&
PPL::Polyhedron::constraints() const {
  if (is_empty()) {
    // We want `con_sys' to only contain the unsatisfiable constraint
    // of the appropriate dimension.
    if (con_sys.num_rows() == 0) {
      // The 0-dim unsatisfiable constraint is extended to
      // the appropriate dimension and then stored in `con_sys'.
      ConSys unsat_cs = ConSys::zero_dim_empty();
      unsat_cs.adjust_topology_and_dimension(topology(), space_dim);
      const_cast<ConSys&>(con_sys).swap(unsat_cs);
    }
    else {
      // Checking that `con_sys' contains the right thing.
      assert(con_sys.space_dimension() == space_dim);
      assert(con_sys.num_rows() == 1);
      assert(con_sys[0].is_trivial_false());
    }
    return con_sys;
  }

  if (space_dim == 0) {
    // zero-dimensional universe.
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    return con_sys;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // We insist in returning a sorted system of constraints:
  // this is needed so that the const_iterator on ConSys
  // could correctly filter out the matched non-strict inequalities
  // in the case of an NNC polyhedron. 
  obtain_sorted_constraints();
  return con_sys;
}

const PPL::ConSys&
PPL::Polyhedron::minimized_constraints() const {
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_constraints();
  // Note: calling constraints() also ensure sortedness,
  // which is required to correctly filter the output
  // of an NNC constraint system.
  return constraints();
}

const PPL::GenSys&
PPL::Polyhedron::generators() const {

  if (is_empty()) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return gen_sys;
  }

  if (space_dim == 0) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return GenSys::zero_dim_univ();
  }

  if (!generators_are_up_to_date() && !update_generators()) {
    // We have just discovered that `*this' is empty.
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return gen_sys;
  }

  // We insist in returning a sorted system of generators.
  obtain_sorted_generators();
  return gen_sys;
}

const PPL::GenSys&
PPL::Polyhedron::minimized_generators() const {
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_generators();
  return generators();
}


PPL::Polyhedron::Polyhedron(Topology topol,
			    size_t num_dimensions,
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


PPL::Polyhedron::Polyhedron(Topology topol, ConSys& cs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Try to adapt `cs' to the required topology.
  size_t cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_dimension(topol, cs_space_dim))
    throw_topology_incompatible("Polyhedron(cs)", cs);

  if (cs.num_rows() > 0 && cs_space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (topol == NOT_NECESSARILY_CLOSED)
      // For each strict inequality we must also have
      // the corresponding non-strict inequality.
      con_sys.add_corresponding_nonstrict_inequalities();
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
    for (size_t i = cs.num_rows(); i-- > 0; )
      if (cs[i].is_trivial_false()) {
	// Inconsistent constraint found: the polyhedron is empty.
	set_empty();
	return;
      }
}

PPL::Polyhedron::Polyhedron(Topology topol, const ConSys& ccs)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // FIXME: this implementation is just an executable specification.
  ConSys cs = ccs;

  // Try to adapt `cs' to the required topology.
  size_t cs_space_dim = cs.space_dimension();
  if (!cs.adjust_topology_and_dimension(topol, cs_space_dim))
    throw_topology_incompatible("Polyhedron(cs)", cs);

  if (cs.num_rows() > 0 && cs_space_dim > 0) {
    // Stealing the rows from `cs'.
    std::swap(con_sys, cs);
    if (topol == NOT_NECESSARILY_CLOSED)
      // For each strict inequality we must also have
      // the corresponding non-strict inequality.
      con_sys.add_corresponding_nonstrict_inequalities();
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
    for (size_t i = cs.num_rows(); i-- > 0; )
      if (cs[i].is_trivial_false()) {
	// Inconsistent constraint found: the polyhedron is empty.
	set_empty();
	return;
      }
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

  size_t gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_dimension(topol, gs_space_dim))
    throw_topology_incompatible("Polyhedron(gs)", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // we must have the minus_epsilon_ray.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.insert(Generator::zero_dim_minus_epsilon_ray());
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

  size_t gs_space_dim = gs.space_dimension();
  // Try to adapt `gs' to the required topology.
  if (!gs.adjust_topology_and_dimension(topol, gs_space_dim))
    throw_topology_incompatible("Polyhedron(gs)", gs);

  if (gs_space_dim > 0) {
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);
    // In a generator system describing a NNC polyhedron,
    // we must have the minus_epsilon_ray.
    if (topol == NOT_NECESSARILY_CLOSED)
      gen_sys.insert(Generator::zero_dim_minus_epsilon_ray());
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


PPL::Polyhedron::~Polyhedron() {
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

void
PPL::Polyhedron::set_zero_dim_univ() {
  status.set_zero_dim_univ();
  space_dim = 0;
  con_sys.clear();
  gen_sys.clear();
}

void
PPL::Polyhedron::update_constraints() const {
  assert(space_dim > 0);
  assert(!is_empty());
  assert(generators_are_up_to_date());

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
    // are minimized
    x.set_constraints_minimized();
    x.set_generators_minimized();
  }
  return !empty;
}


bool
PPL::Polyhedron::minimize() const {
  // 0-dim space or empty polyhedra are already minimized.
  if (is_empty())
    return false;
  else if (space_dim == 0
	   || (constraints_are_minimized() && generators_are_minimized()))
    return true;
  // If constraints or generators are up-to-date, invoking
  // update_generators() or update_constraints(), respectively,
  // minimizes both constraints and generators.
  // If both are up-to-date it does not matter whether we use
  // update_generators() or update_constraints():
  // both minimize constraints and generators.
  else if (constraints_are_up_to_date()) {
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
PPL::Polyhedron::strongly_minimize_generators() const {
  assert(!is_necessarily_closed());

  // From the user perspective, the polyhedron will not change.
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // We need `gen_sys' (weakly) minimized and `con_sys' up-to-date.
  if (!minimize())
    return false;

  // We also need `sat_c' up-to-date.
  if (!sat_c_is_up_to_date()) {
    assert(sat_g_is_up_to_date());
    x.sat_c.transpose_assign(sat_g);
  }

  // This SatRow will have all and only the indexes
  // of strict inequalities set to 1.
  SatRow sat_all_but_strict_ineq;
  size_t cs_rows = con_sys.num_rows();
  size_t n_equals = con_sys.num_equalities();
  for (size_t i = cs_rows; i-- > n_equals; )
    if (con_sys[i].is_strict_inequality())
      sat_all_but_strict_ineq.set(i);

  // Will record whether or not we changed the generator system.
  bool changed = false;

  // For all points in the generator system, check for eps-redundancy
  // and eventually move them to the bottom part of the system.
  GenSys& gs = const_cast<GenSys&>(gen_sys);
  SatMatrix& sat = const_cast<SatMatrix&>(sat_c);
  size_t gs_rows = gs.num_rows();
  size_t n_lines = gs.num_lines();
  size_t eps_index = gs.num_columns() - 1;
  for (size_t i = n_lines; i < gs_rows; )
    if (gs[i].is_point()) {
      // Compute the SatRow corresponding to the candidate point
      // when strict inequality constraints are ignored.
      SatRow sat_gi;
      set_union(sat[i], sat_all_but_strict_ineq, sat_gi);
      // Check if the candidate point is actually eps-redundant:
      // namely, if there exists another point that saturates
      // all the non-strict inequalities saturated by the candidate.
      bool eps_redundant = false;
      for (size_t j = n_lines; j < gs_rows; ++j)
	if (i != j && gs[j].is_point() && sat[j] <= sat_gi) {
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

  // If needed, erase the eps-redundant generators.
  if (gs_rows < gs.num_rows())
    gs.erase_to_end(gs_rows);

  if (changed) {
    // The generator system is no longer sorted.
    x.gen_sys.set_sorted(false);
    // The constraint system is no longer up-to-date.
    x.clear_constraints_up_to_date();
  }

  assert(OK());
  return true;
}


bool
PPL::Polyhedron::strongly_minimize_constraints() const {
  assert(!is_necessarily_closed());

  // From the user perspective, the polyhedron will not change.
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // We need `con_sys' (weakly) minimized and `gen_sys' up-to-date.
  if (!minimize())
    return false;

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

  size_t gs_rows = gen_sys.num_rows();
  size_t n_lines = gen_sys.num_lines();
  for (size_t i = gs_rows; i-- > n_lines; )
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
  // if we have to add back the eps_leq_one constraint
  // and whether or not the constraint system is changed.
  SatRow eps_leq_one_saturators;
  bool topologically_closed = true;
  bool strict_inequals_saturate_all_rays = true;
  bool eps_leq_one_removed = false;
  bool changed = false;

  // For all the strict inequalities in `con_sys',
  // check for eps-redundancy and eventually move them
  // to the bottom part of the system.
  ConSys& cs = x.con_sys;
  SatMatrix& sat = x.sat_g;
  size_t cs_rows = cs.num_rows();
  size_t n_equals = cs.num_equalities();
  size_t eps_index = cs.num_columns() - 1;
  for (size_t i = n_equals; i < cs_rows; )
    if (cs[i].is_strict_inequality()) {
      // First, check if it is saturated by no closure points
      SatRow sat_ci;
      set_union(sat[i], sat_lines_and_closure_points, sat_ci);
      if (sat_ci == sat_lines) {
	// Constraint `cs[i]' is eps-redundant.
	// move it to the bottom of the constraint system,
	// while keeping `sat_g' consistent.
	--cs_rows;
	std::swap(cs[i], cs[cs_rows]);
	std::swap(sat[i], sat[cs_rows]);
	// Check if it was the eps_leq_one constraint.
	const Constraint& c = cs[cs_rows];
	bool all_zeros = true;
	for (size_t k = eps_index; k-- > 1; )
	  if (c[k] != 0) {
	    all_zeros = false;
	    break;
	  }
	if (all_zeros && (c[0] + c[eps_index] == 0)) {
	  // We removed the eps_leq_one constraint.
	  eps_leq_one_removed = true;
	  // Remembering it to eventually restore it later.
	  eps_leq_one_saturators = sat[cs_rows];
	}
	else
	  // We removed another constraint.
	  changed = true;
	// Continue considering next constraint,
	// which is already in place due to the swap.
	continue;
      }
      // Now we check if there exists another strict inequality
      // constraint having a superset of its saturators,
      // when disregarding points.
      sat_ci.clear();
      set_union(sat[i], sat_all_but_points, sat_ci);
      bool eps_redundant = false;
      for (size_t j = n_equals; j < cs_rows; ++j)
	if (i != j && cs[j].is_strict_inequality() && sat[j] <= sat_ci) {
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
      if (!eps_redundant) {
	// The constraint is not eps-redudnant.
	// Maintain boolean flags to later check
	// if the eps_leq_one constraint is needed.
	topologically_closed = false;
	if (strict_inequals_saturate_all_rays)
	  strict_inequals_saturate_all_rays = (sat[i] <= sat_lines_and_rays);
	// Continue with next constraint.
	++i;
      }
    }
    else
      // `cs[i]' is not a strict inequality: consider next constraint.
      ++i;

  // Now insert the eps_leq_one constraint, if it is needed.
  // It is needed if either the polyhedron is topologically closed
  // or there exists a strict inequality encoding that is not
  // saturated by one of the rays.
  if (topologically_closed || !strict_inequals_saturate_all_rays) {
    assert(cs_rows < cs.num_rows());
    // Note: `eps_leq_one' is already normalized.
    Constraint& eps_leq_one = cs[cs_rows];
    eps_leq_one[0] = 1;
    eps_leq_one[eps_index] = -1;
    for (size_t k = eps_index; k-- > 1; )
      eps_leq_one[k] = 0;
    // If this is the only change performed to the constraint system,
    // maybe we can keep things consistent.
    if (!changed) {
      if (eps_leq_one_removed) {
	// The constraint system is no longer sorted.
	cs.set_sorted(false);
	// Restore the corresponding saturation row.
	sat[cs_rows] = eps_leq_one_saturators;
	// `sat_c' is no longer up-to-date.
	x.clear_sat_c_up_to_date();
      }
      else
	changed = true;
    }
    // Bump number of rows.
    cs_rows++;
  }
  else
    // The eps_leq_one constraint is not needed:
    // if we previously removed it from the input constraint system,
    // then the constraint system has changed.
    if (eps_leq_one_removed)
      changed = true;

  if (changed) {
    // Erase the eps-redundant constraints, if there are any.
    if (cs_rows < cs.num_rows())
      cs.erase_to_end(cs_rows);
    // The constraint system is no longer sorted.
    cs.set_sorted(false);
    // The generator system is no longer up-to-date.
    x.clear_generators_up_to_date();
  }

  assert(OK());
  return true;
}


bool
PPL::Polyhedron::strongly_minimize() const {
  assert(!is_necessarily_closed());

  // We need `gen_sys' strongly minimized,
  // `con_sys' (weakly) minimized
  // and `sat_g' up-to-date.
  if (!strongly_minimize_generators())
    return false;
  minimize();
  if (!sat_g_is_up_to_date()) {
    assert(sat_c_is_up_to_date());
    // From the user perspective, the polyhedron will not change.
    SatMatrix& sat = const_cast<SatMatrix&>(sat_g);
    sat.transpose_assign(sat_c);
  }

  // Now applying a restricted form of strong minimization to `con_sys',
  // which will preserve `gen_sys' strongly minimized too.

  // Computing mask SatRow's.
  SatRow sat_all_but_rays;
  SatRow sat_all_but_points;
  SatRow sat_all_but_closure_points;
  size_t gs_rows = gen_sys.num_rows();
  size_t n_lines = gen_sys.num_lines();
  for (size_t i = gs_rows; i-- > n_lines; )
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
			       "strongly_minimize.");
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

  // Find, if it exists, the strict inequality in `con_sys'
  // that saturates no closure point encodings
  // (there exists at most one such a strict inequality).
  size_t cs_rows = con_sys.num_rows();
  size_t eps_index = con_sys.num_columns() - 1;
  for (size_t i = con_sys.num_equalities(); i < cs_rows; ++i) {
    const Constraint& ci = con_sys[i];
    if (ci.is_strict_inequality()) {
      // Check if it is the eps_leq_one constraint.
      bool all_zeros = true;
      for (size_t k = eps_index; k-- > 1; )
	if (ci[k] != 0) {
	  all_zeros = false;
	  break;
	}
      if (all_zeros && (ci[0] + ci[eps_index] == 0)) {
	// It is the eps_leq_one constraint:
	// the constraint system was already in strong minimal form.
	assert(OK());
	return true;
      }

      // Check if `ci' is saturated by no closure points.
      SatRow sat_ci;
      set_union(sat_g[i], sat_lines_and_closure_points, sat_ci);
      if (sat_ci == sat_lines) {
	// Replace it by the eps_leq_one constraint
	// (`gen_sys' and `sat_g' are not affected by this change).
	// Note: `eps_leq_one' is already normalized.
	Constraint& eps_leq_one = const_cast<Constraint&>(ci);
	eps_leq_one[0] = 1;
	eps_leq_one[eps_index] = -1;
	for (size_t k = eps_index; k-- > 1; )
	  eps_leq_one[k] = 0;
	// `con_sys' is no longer sorted.
	ConSys& cs = const_cast<ConSys&>(con_sys);
	cs.set_sorted(false);
	// `con_sys' is now in strong minimal form.
	assert(OK());
	return true;
      }
    }
  }
  // There was no such a strict inequality:
  // the constraint system was already in strong minimal form.
  assert(OK());
  return true;
}

void
PPL::Polyhedron::obtain_sorted_constraints() const {
  assert(constraints_are_up_to_date());

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
      // we obtain sat_g from sat_c (that has to be up-to-date)...
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

void
PPL::Polyhedron::update_sat_c() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_c_is_up_to_date());

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // The columns of `sat_c' represent the constraints and
  // its rows represent the generators: resize accordingly.
  x.sat_c.resize(gsr, csr);
  for (size_t i = gsr; i-- > 0; )
    for (size_t j = csr; j-- > 0; ) {
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

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // The columns of `sat_g' represent generators and its
  // rows represent the constraints: resize accordingly.
  x.sat_g.resize(csr, gsr);
  for (size_t i = csr; i-- > 0; )
    for (size_t j = gsr; j-- > 0; ) {
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


bool
PPL::operator<=(const Polyhedron& x, const Polyhedron& y) {
  // Topology compatibility check.
  if (x.topology() != y.topology())
    Polyhedron::throw_topology_incompatible("operator<=("
					    "const Polyhedron& x, "
					    "const Polyhedron& y)", x, y);
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    Polyhedron::throw_dimension_incompatible("operator<=("
					     "const Polyhedron& x, "
					     "const Polyhedron& y)", x, y);
  if (x.is_empty())
    return true;
  else if (y.is_empty())
    return x.check_empty();
  else if (x_space_dim == 0)
    return true;

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
    for (size_t i = cs.num_rows(); i-- > 0; ) {
      const Constraint& c = cs[i];
      if (c.is_inequality()) {
	for (size_t j = gs.num_rows(); j-- > 0; )
	  if (c * gs[j] < 0)
	    return false;
      }
      else
	// `c' is an equality.
	for (size_t j = gs.num_rows(); j-- > 0; )
	  if (c * gs[j] != 0)
	    return false;
    }
  else {
    // Here we have a NOT necessarily closed polyhedron: using the
    // reduced scalar product, which ignores the epsilon coefficient.
    size_t eps_index = x_space_dim + 1;
    for (size_t i = cs.num_rows(); i-- > 0; ) {
      const Constraint& c = cs[i];
      switch (c.type()) {
      case Constraint::NONSTRICT_INEQUALITY:
	for (size_t j = gs.num_rows(); j-- > 0; )
	  if (reduced_scalar_product(c, gs[j]) < 0)
	    return false;
	break;
      case Constraint::EQUALITY:
	for (size_t j = gs.num_rows(); j-- > 0; )
	  if (reduced_scalar_product(c, gs[j]) != 0)
	    return false;
	break;
      case Constraint::STRICT_INEQUALITY:
	for (size_t j = gs.num_rows(); j-- > 0; ) {
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
	  else
	    // The generator is a line, ray or closure point: usual test.
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
PPL::Polyhedron::intersection_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("inters_assign_and_min(y)", y);
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("inters_assign_and_min(y)", y);

  // If one of the two polyhedra is empty, the intersection is empty.
  if (x.is_empty())
    return false;
  if (y.is_empty()) {
    x.set_empty();
    return false;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (x_space_dim == 0)
    return true;

  // add_and_minimize() requires x to be up-to-date
  // and to have sorted constraints...
  if (!x.minimize())
    // We have just discovered that `x' is empty.
    return false;
  // ... and y to have updated and sorted constraints.
  if (!y.constraints_are_up_to_date())
    y.update_constraints();

  // After minimize(), `x.con_sys' is not necessarily sorted.
  x.obtain_sorted_constraints_with_sat_c();
  // After update_constraint(), `y.con_sys' is not necessarily sorted.
  y.obtain_sorted_constraints();

  bool empty = add_and_minimize(true,
				x.con_sys, x.gen_sys, x.sat_c,
				y.con_sys);

  if (empty)
    x.set_empty();
  else {
    // On exit of the function intersection_assign_and_minimize()
    // the polyhedron is up-to-date and sat_c is meaningful.
    x.set_sat_c_up_to_date();
    x.clear_sat_g_up_to_date();
  }
  assert(x.OK());
  return !empty;
}

void
PPL::Polyhedron::intersection_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("inters_assign(y)", y);
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("inters_assign(y)", y);

  // If one of the two polyhedra is empty, the intersection is empty.
  if (x.is_empty())
    return;
  if (y.is_empty()) {
    x.set_empty();
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (x_space_dim == 0)
    return;

  // We need the system of contraints of both the polyhedra up-to-date.
  if(!x.constraints_are_up_to_date())
    x.update_constraints();
  if(!y.constraints_are_up_to_date())
    y.update_constraints();

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  x.obtain_sorted_constraints();
  y.obtain_sorted_constraints();

  x.con_sys.merge_rows_assign(y.con_sys);
  // After adding new constraints, generators are no longer up-to-date.
  x.clear_generators_up_to_date();
  // It does not minimize the system of constraints.
  x.clear_constraints_minimized();

  assert(x.OK() && y.OK());
}

void
PPL::Polyhedron::concatenate_assign(const Polyhedron& y) {
  if (topology() != y.topology())
    throw_topology_incompatible("concatenate_assign(y)", y);

  size_t added_columns = y.space_dimension();

  // If `*this' or `y' are empty polyhedra, it is sufficient to adjust
  // the dimension of the space.
  if (is_empty() || y.is_empty()) {
    space_dim += added_columns;
    set_empty();
    return;
  }

  // If `y' is a non-empty 0-dim space polyhedron, the result is `*this'.
  if (added_columns == 0)
    return;

  // If `*this' is a non-empty 0-dim space polyhedron, the result is `y'.
  if (space_dim == 0) {
    *this = y;
    return;
  }

  // FIXME: this implementation is just an executable specification.
  ConSys cs = y.constraints();

  if (!constraints_are_up_to_date())
    update_constraints();

  // The matrix for the new system of constraints is obtained
  // by leaving the old system of constraints in the upper left-hand side
  // and placing the constraints of `cs' in the lower right-hand side.
  // NOTE: here topologies agree, whereas dimensions may not agree.
  size_t old_num_rows = con_sys.num_rows();
  size_t old_num_columns = con_sys.num_columns();
  size_t added_rows = cs.num_rows();

  con_sys.grow(old_num_rows + added_rows, old_num_columns + added_columns);

  // Move the epsilon coefficient to the last column, if needed.
  if (!is_necessarily_closed() && added_columns > 0)
    con_sys.swap_columns(old_num_columns - 1,
			 old_num_columns - 1 + added_columns);
  // Steal the constraints from `cs' and put them in `con_sys'
  // using the right displacement for coefficients.
  size_t cs_num_columns = cs.num_columns();
  for (size_t i = added_rows; i-- > 0; ) {
    Constraint& c_old = cs[i];
    Constraint& c_new = con_sys[old_num_rows + i];
    // Method `grow', by default, added inequalities.
    if (c_old.is_equality())
      c_new.set_is_equality();
    // The inhomogeneous term is not displaced.
    std::swap(c_new[0], c_old[0]);
    // All homogeneous terms (included the epsilon coefficient,
    // if present) are displaced by `space_dim' columns.
    for (size_t j = 1; j < cs_num_columns; ++j)
      std::swap(c_old[j], c_new[space_dim + j]);
  }
  // Update space dimension.
  space_dim += added_columns;

#ifdef BE_LAZY
  con_sys.set_sorted(false);
#else
  con_sys.sort_rows();
#endif
  clear_constraints_minimized();
  clear_generators_up_to_date();
  clear_sat_g_up_to_date();
  clear_sat_c_up_to_date();

  // Note: the system of constraints may be unsatisfiable, thus we do
  // not check for satisfiability.
  assert(OK());
}

bool
PPL::Polyhedron::poly_hull_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_hull_assign_and_min(y)", y);    
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("poly_hull_assign_and_min(y)", y);

  // The poly-hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.is_empty())
    return minimize();
  if (x.is_empty()) {
    x = y;
    return minimize();
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their poly-hull is the universe polyhedron too.
  if (x_space_dim == 0)
    return true;

  // The function add_and_minimize() requires `x' to have both
  // the constraints and the generators systems up-to-date and
  // to have sorted generators ...
  if (!x.minimize()) {
    // We have just discovered that `x' is empty.
    x = y;
    return minimize();
  }
  x.obtain_sorted_generators_with_sat_g();
  // ...and `y' to have updated and sorted generators.
  if (!y.generators_are_up_to_date() && !y.update_generators())
    // We have just discovered that `y' is empty
    // (and we know that `x' is NOT empty).
    return true;
  y.obtain_sorted_generators();

  // This call to `add_and_minimize(...)' cannot return `true'.
  add_and_minimize(false,
		   x.gen_sys, x.con_sys, x.sat_g,
		   y.gen_sys);

  x.set_sat_g_up_to_date();
  x.clear_sat_c_up_to_date();

  assert(x.OK(true) && y.OK());
  return true;
}

void
PPL::Polyhedron::poly_hull_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_hull_assign(y)", y);    
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("poly_hull_assign(y)", y);

  // The poly-hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  if (x.is_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their poly-hull is the universe polyhedron too.
  if (x_space_dim == 0)
    return;

  if (!y.generators_are_up_to_date() && !y.update_generators())
    // Discovered `y' empty when updating generators.
    return;
  if (!x.generators_are_up_to_date() && !x.update_generators()) {
    // Discovered `x' empty when updating generators.
    x = y;
    return;
  }

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  x.obtain_sorted_generators();
  y.obtain_sorted_generators();

  x.gen_sys.merge_rows_assign(y.gen_sys);

  // After adding new generators, constraints are no longer up-to-date.
  x.clear_constraints_up_to_date();
  // It does not minimize the system of generators.
  x.clear_generators_minimized();

  // At this point both `x' and `y' are not empty.
  assert(x.OK(true) && y.OK(true));
}

void
PPL::Polyhedron::poly_difference_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_difference_assign(y)", y);    
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("poly_difference_assign(y)", y);

  // The difference of a polyhedron `p' and an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  // The difference of an empty polyhedron and of a polyhedron `p' is empty.
  if (x.is_empty())
    return;

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their difference is empty.
  if (x_space_dim == 0) {
    x.set_empty();
    return;
  }

  // TODO: This is just an executable specification.
  //       Have to find a more efficient method.
  if (x <= y) {
    x.set_empty();
    return;
  }

  Polyhedron new_polyhedron(topology(), x_space_dim, EMPTY);

  const ConSys& x_cs = x.constraints();
  const ConSys& y_cs = y.constraints();
  for (ConSys::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    ConSys z_cs = x_cs;
    const Constraint& c = *i;
    assert(!c.is_trivial_true());
    assert(!c.is_trivial_false());
    LinExpression e = LinExpression(c);
    switch (c.type()) {
    case Constraint::NONSTRICT_INEQUALITY:
      if (is_necessarily_closed())
	z_cs.insert(e <= 0);
      else
	z_cs.insert(e < 0);
      break;
    case Constraint::STRICT_INEQUALITY:
      z_cs.insert(e <= 0);
      break;
    case Constraint::EQUALITY:
      if (is_necessarily_closed())
	// We have already filtered out the case
	// when `x' is included in `y': the result is `x'.
	return;
      else {
	ConSys w_cs = x_cs;
	w_cs.insert(e < 0);
	new_polyhedron.poly_hull_assign(Polyhedron(topology(), w_cs));
	z_cs.insert(e > 0);
      }
      break;
    }
    new_polyhedron.poly_hull_assign(Polyhedron(topology(), z_cs));
  }
  *this = new_polyhedron;

  assert(OK());
}

bool
PPL::Polyhedron::poly_difference_assign_and_minimize(const Polyhedron& y) {
  // Topology and dimensions compatibility check are done
  // inside poly_difference_assign(y).
  poly_difference_assign(y);
  bool not_empty = minimize();
  assert(OK(true));
  return not_empty;
}

void
PPL::Polyhedron::add_dimensions(Matrix& mat1,
				Matrix& mat2,
				SatMatrix& sat1,
				SatMatrix& sat2,
				size_t add_dim) {
  assert(mat1.topology() == mat2.topology());
  assert(mat1.num_columns() == mat2.num_columns());
  assert(add_dim != 0);

  mat1.add_zero_columns(add_dim);
  mat2.add_rows_and_columns(add_dim);
  // The resulting saturation matrix will be the follow:
  // from row    0    to      add_dim-1       : only zeroes
  //          add_dim     add_dim+num_rows-1  : old saturation matrix

  // In fact all the old generators saturate all the new constraints
  // because the polyhedron has not been embedded in the new space.
  sat1.resize(sat1.num_rows() + add_dim, sat1.num_columns());
  // The old matrix is copied at the end of the new matrix.
  for (size_t i = sat1.num_rows() - add_dim; i-- > 0; )
    std::swap(sat1[i], sat1[i+add_dim]);
  // Computes the "sat_c", too.
  sat2.transpose_assign(sat1);

  if (!mat1.is_necessarily_closed()) {
    // Moving the epsilon coefficients in the last column.
    size_t new_eps_index = mat1.num_columns() - 1;
    size_t old_eps_index = new_eps_index - add_dim;
    // This swap preserves sortedness of `mat1'.
    mat1.swap_columns(old_eps_index, new_eps_index);

    // Try to preserve sortedness of `mat2'.
    if (!mat2.is_sorted())
      mat2.swap_columns(old_eps_index, new_eps_index);
    else {
      for (size_t i = mat2.num_rows(); i-- > add_dim; ) {
	Row& r = mat2[i];
	std::swap(r[old_eps_index], r[new_eps_index]);
      }
      // The upper-right corner of `mat2' contains the J matrix:
      // swap coefficients to preserve sortedness.
      for (size_t i = add_dim; i-- > 0; ++old_eps_index) {
	Row& r = mat2[i];
	std::swap(r[old_eps_index], r[old_eps_index + 1]);
      }
    }
    // NOTE: since we swapped columns in both `mat1' and `mat2',
    // no swapping is required for `sat1' and `sat2'.
  }
}


void
PPL::Polyhedron::add_dimensions_and_embed(size_t m) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (m == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained by adjusting
  // `space_dim' and clearing `con_sys' (since it can contain the
  // unsatisfiable constraint system of the wrong dimension).
  if (is_empty()) {
    space_dim += m;
    con_sys.clear();
    return;
  }

  // The case of a zero-dimensional space polyhedron.
  if (space_dim == 0) {
    // Since it is not empty, it has to be the universe polyhedron.
    assert(status.test_zero_dim_univ());
    // We swap `*this' with a newly created
    // universe polyhedron of dimension `m'.
    Polyhedron ph(topology(), m, UNIVERSE);
    swap(ph);
    return;
  }

  // To embed an n-dimension space polyhedron in a (n+m)-dimension space,
  // we just add `m' zero-columns to the rows in the matrix of constraints;
  // in contrast, the matrix of generators needs additional rows,
  // corresponding to the vectors of the canonical basis
  // for the added dimensions. That is, for each new dimension `x[k]'
  // we add the line having that direction. This is done by invoking
  // the function add_dimensions() giving the matrix of generators
  // as the second argument.
  if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // `sat_c' must be up to date for add_dimensions(...).
    if (!sat_c_is_up_to_date())
      update_sat_c();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(con_sys, gen_sys, sat_c, sat_g, m);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: we do not need to modify generators.
    con_sys.add_zero_columns(m);
    // If the polyhedron is NON-necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed())
      con_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
  }
  else {
    // Only generators are up-to-date: we do not need to modify constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_rows_and_columns(m);

    // If the polyhedron is NON-necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed()) {
      // Try to preserve sortedness of `gen_sys'.
      if (!gen_sys.is_sorted())
	gen_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
      else {
	size_t old_eps_index = space_dim + 1;
	size_t new_eps_index = old_eps_index + m;
	for (size_t i = gen_sys.num_rows(); i-- > m; ) {
	  Row& r = gen_sys[i];
	  std::swap(r[old_eps_index], r[new_eps_index]);
	}
	// The upper-right corner of `gen_sys' contains the J matrix:
	// swap coefficients to preserve sortedness.
	for (size_t i = m; i-- > 0; ++old_eps_index) {
	  Row& r = gen_sys[i];
	  std::swap(r[old_eps_index], r[old_eps_index + 1]);
	}
      }
    }
  }
  // Update the space dimension.
  space_dim += m;

  // Note: we do not check for satisfiability, because the system of
  // constraints may be unsatisfiable.
  assert(OK());
}

void
PPL::Polyhedron::add_dimensions_and_project(size_t m) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (m == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained
  // by merely adjusting `space_dim'.
  if (is_empty()) {
    space_dim += m;
    con_sys.clear();
    return;
  }

  if (space_dim == 0) {
    assert(status.test_zero_dim_univ() && gen_sys.num_rows() == 0);
    // The system of generators for this polyhedron has only
    // the origin as a point.
    // In a non-empty NNC polyhedron, we have the minus_eps_ray.
    if (!is_necessarily_closed())
      gen_sys.insert(Generator::zero_dim_minus_epsilon_ray());
    gen_sys.insert(Generator::zero_dim_point());
    gen_sys.adjust_topology_and_dimension(topology(), m);
    set_generators_minimized();
    space_dim = m;
    assert(OK());
    return;
  }

  // To project an n-dimension space polyhedron in a (n+m)-dimension space,
  // we just add to the matrix of generators `m' zero-columns;
  // In contrast, in the matrix of constraints, new rows are needed
  // in order to avoid embedding the old polyhedron in the new space.
  // Thus, for each new dimensions `x[k]', we add the constraint
  // x[k] = 0; this is done by invoking the function add_dimensions()
  // giving the matrix of constraints as the second argument.
  if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // `sat_g' must be up to date for add_dimensions(...).
    if (!sat_g_is_up_to_date())
      update_sat_g();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(gen_sys, con_sys, sat_g, sat_c, m);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: no need to modify the generators.
    con_sys.add_rows_and_columns(m);
    // If the polyhedron is NON-necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed()) {
      // Try to preserve sortedness of `con_sys'.
      if (!con_sys.is_sorted())
	con_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
      else {
	size_t old_eps_index = space_dim + 1;
	size_t new_eps_index = old_eps_index + m;
	for (size_t i = con_sys.num_rows(); i-- > m; ) {
	  Row& r = con_sys[i];
	  std::swap(r[old_eps_index], r[new_eps_index]);
	}
	// The upper-right corner of `con_sys' contains the J matrix:
	// swap coefficients to preserve sortedness.
	for (size_t i = m; i-- > 0; ++old_eps_index) {
	  Row& r = con_sys[i];
	  std::swap(r[old_eps_index], r[old_eps_index + 1]);
	}
      }
    }
  }
  else {
    // Only generators are up-to-date: no need to modify the constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_zero_columns(m);
    if (!is_necessarily_closed())
      gen_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
  }
  // Now we update the space dimension.
  space_dim += m;

  // Note: we do not check for satisfiability, because the system of
  // constraints may be unsatisfiable.
  assert(OK());
}

void
PPL::Polyhedron::remove_dimensions(const std::set<Variable>& to_be_removed) {
  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  unsigned int max_dim_to_be_removed = to_be_removed.rbegin()->id();
  if (max_dim_to_be_removed >= space_dim)
    throw_dimension_incompatible("remove_dimensions(vs)",
				 max_dim_to_be_removed);

  size_t new_space_dim = space_dim - to_be_removed.size();

  if (is_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Removing dimensions from the empty polyhedron:
    // we clear `con_sys' since it could have contained the
    // unsatisfiable constraint of the wrong dimension.
    con_sys.clear();
    // Update the space dimension.
    space_dim = new_space_dim;
    assert(OK());
    return;
  }

  // When removing _all_ dimensions from a non-empty polyhedron,
  // we obtain the zero-dimensional universe polyhedron.
  if (new_space_dim == 0) {
    set_zero_dim_univ();
    return;
  }

  // For each variable to be removed, we fill the corresponding column
  // by shifting left those columns that will not be removed.
  std::set<Variable>::const_iterator tbr = to_be_removed.begin();
  std::set<Variable>::const_iterator tbr_end = to_be_removed.end();
  size_t dst_col = tbr->id() + 1;
  size_t src_col = dst_col + 1;
  size_t nrows = gen_sys.num_rows();
  for (++tbr; tbr != tbr_end; ++tbr) {
    size_t tbr_col = tbr->id() + 1;
    // All columns in between are moved to the left.
    while (src_col < tbr_col) {
      for (size_t r = nrows; r-- > 0; )
	std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
      ++dst_col;
      ++src_col;
    }
    ++src_col;
  }
  // Moving the remaining columns.
  size_t ncols = gen_sys.num_columns();
  while (src_col < ncols) {
    for (size_t r = nrows; r-- > 0; )
      std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
    ++src_col;
    ++dst_col;
  }
  // The number of remaining columns is `dst_col'.
  // Note that resizing also calls `set_sorted(false)'.
  gen_sys.resize_no_copy(nrows, dst_col);
  // We may have invalid line and rays now.
  gen_sys.remove_invalid_lines_and_rays();

  // Constraints are not up-to-date.
  clear_constraints_up_to_date();
  // Generators are no longer guaranteed to be minimized.
  clear_generators_minimized();

  // Update the space dimension.
  space_dim = new_space_dim;

  assert(OK(true));
}

void
PPL::Polyhedron::remove_higher_dimensions(size_t new_dimension) {
  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  if (new_dimension > space_dim)
    throw_dimension_incompatible("remove_higher_dimensions(nd)",
				 new_dimension);

  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (new_dimension == space_dim) {
    assert(OK());
    return;
  }

  if (is_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Removing dimensions from the empty polyhedron:
    // just updates the space dimension.
    space_dim = new_dimension;
    con_sys.clear();
    assert(OK());
    return;
  }

  if (new_dimension == 0) {
    // Removing all dimensions from a non-empty polyhedron:
    // just return the zero-dimensional universe polyhedron.
    set_zero_dim_univ();
    return;
  }

  size_t new_num_cols = new_dimension + 1;
  if (!is_necessarily_closed()) {
    // The polyhedron is NOT necessarily closed: move the column
    // of the epsilon coefficients to its new place.
    gen_sys.swap_columns(gen_sys.num_columns() - 1, new_num_cols);
    // The number of remaining columns is `new_dimension + 2'.
    ++new_num_cols;
  }
  // Note that resizing also calls `set_sorted(false)'.
  gen_sys.resize_no_copy(gen_sys.num_rows(), new_num_cols);
  // We may have invalid line and rays now.
  gen_sys.remove_invalid_lines_and_rays();
  // Constraints are not up-to-date.
  clear_constraints_up_to_date();
  // Generators are no longer guaranteed to be minimized.
  clear_generators_minimized();
  // Update the space dimension.
  space_dim = new_dimension;

  assert(OK(true));
}

bool
PPL::Polyhedron::add_constraints_and_minimize(ConSys& cs) {
  // Topology-compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw_topology_incompatible("add_constraints_and_min(cs)", cs);
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  size_t cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_constraints_and_min(cs)", cs);

  // Adding no constraints: just minimize.
  if (cs.num_rows() == 0) {
    assert(cs.num_columns() == 0);
    return minimize();
  }

  // Dealing with zero-dimensional space polyhedra first.
  if (space_dim == 0) {
    // In a 0-dimensional space the constraints are
    // trivial (e.g., 0 == 0 or 1 >= 0 or 1 > 0) or
    // inconsistent (e.g., 1 == 0 or -1 >= 0 or 0 > 0).
    // In a system of constraints `begin()' and `end()' are equal
    // if and only if the system contains trivial constraints only.
    if (cs.begin() == cs.end())
      return true;
    // There is a constraint, it must be inconsistent,
    // the polyhedron is empty.
    status.set_empty();
    return false;
  }

  // We need both the system of generators and constraints minimal.
  if (!minimize())
    return false;

  // For NNC polyhedra, each strict inequality must be matched by
  // the corresponding non-strict inequality.
  if (!cs.is_necessarily_closed())
    cs.add_corresponding_nonstrict_inequalities();
  // Polyhedron::add_and_minimize() requires that
  // the matrix of constraints to be added is sorted.
  if (!cs.is_sorted())
    cs.sort_rows();
  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);

  obtain_sorted_constraints_with_sat_c();

  bool empty = add_and_minimize(true, con_sys, gen_sys,
				sat_c, cs);
  if (empty)
    set_empty();
  else {
    // On exit of the function add_and_minimize(),
    // `sat_c' is up-to-date, while `sat_g' is no longer up-to-date.
    set_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
  assert(OK());

  return !empty;
}


void
PPL::Polyhedron::add_constraint(const Constraint& c) {
  // Topology-compatibility check.
  if (c.is_strict_inequality() && is_necessarily_closed())
    throw_topology_incompatible("add_constraint(c)", c);
  // Dimension-compatibility check:
  // the dimension of `c' can not be greater than space_dim.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("add_constraint(c)", c);

  // Adding a new constraint to an empty polyhedron
  // results in an empty polyhedron.
  if (is_empty())
    return;

  // Dealing with a zero-dimensional space polyhedron first.
  if (space_dim == 0) {
    if (!c.is_trivial_true())
      set_empty();
    return;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // Here we know that the system of constraints has at least a row,
  // so that the space dimension of the inserted constraint
  // is automatically adjusted.
  if (c.is_necessarily_closed())
    // Topology automatically adjusted.
    con_sys.insert(c);
  else if (!is_necessarily_closed()) {
    // Topology automatically adjusted.
    con_sys.insert(c);
    if (c.is_strict_inequality()) {
      // In the NNC topology, each strict inequality has to be
      // matched by a corresponding non-strict inequality:
      // turn the just inserted strict inequality into the corresponding
      // (normalized) non-strict inequality.
      Constraint& nonstrict = con_sys[con_sys.num_rows() - 1];
      nonstrict[space_dim + 1] = 0;
      nonstrict.normalize();
      // Re-insert the strict inequality (which is already normalized).
      con_sys.insert(c);
    }
  }
  else {
    // Note: here we have a _legal_ topology mismatch, because
    // `c' is NOT a strict inequality.
    // However, by barely invoking `con_sys.insert(c)' we would
    // cause a change in the topology of `con_sys', which is wrong.
    // Thus, we insert a "topology corrected" copy of `c'.
    LinExpression nc_expr = LinExpression(c);
    if (c.is_equality())
      con_sys.insert(nc_expr == 0);
    else
      con_sys.insert(nc_expr >= 0);
  }

  // After adding new constraints, generators are no longer up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // Note: the constraint system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

void
PPL::Polyhedron::add_generator(const Generator& g) {
  // Topology-compatibility check.
  if (g.is_closure_point() && is_necessarily_closed())
    throw_topology_incompatible("add_generator(g)", g);
  // Dimension-compatibility check:
  // the dimension of `g' can not be greater than space_dim.
  size_t g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("add_generator(g)", g);

  // Dealing with a zero-dimensional space polyhedron first.
  if (space_dim == 0) {
    // It is not possible to create 0-dim rays or lines.
    assert(g.is_point() || g.is_closure_point());
    // Closure points can only be inserted in non-empty polyhedra.
    if (is_empty())
      if (g.type() != Generator::POINT)
	throw_invalid_generator("add_generator(g)");
      else
	status.set_zero_dim_univ();
    assert(OK());
    return;
  }

  if (generators_are_up_to_date() || !check_empty()) {
    // Since `gen_sys' is not empty, the space dimension
    // of the inserted generator is automatically adjusted.
    if (g.is_necessarily_closed() || !is_necessarily_closed())
      // Topology automatically adjusted.
      gen_sys.insert(g);
    else {
      assert(!g.is_closure_point());
      // Note: here we have a _legal_ topology mismatch, because
      // `g' is NOT a closure point.
      // However, by barely invoking `gen_sys.insert(g)' we would
      // cause a change in the topology of `gen_sys', which is wrong.
      // Thus, we insert a "topology corrected" copy of `g'.
      LinExpression nc_expr = LinExpression(g);
      switch (g.type()) {
      case Generator::LINE:
	gen_sys.insert(Generator::line(nc_expr));
	break;
      case Generator::RAY:
	gen_sys.insert(Generator::ray(nc_expr));
	break;
      case Generator::POINT:
	gen_sys.insert(Generator::point(nc_expr, g.divisor()));
	break;
      default:
	throw std::runtime_error("PPL::C_Polyhedron::add_generator"
				 "(const Generator& g)");
      }
    }
    // After adding the new generator,
    // constraints are no longer up-to-date.
    clear_generators_minimized();
    clear_constraints_up_to_date();
  }
  else {
    // Here the polyhedron is empty:
    // the specification says we can only insert a point.
    if (!g.is_point())
      throw_invalid_generator("add_generator(g)");
    if (g.is_necessarily_closed() || !is_necessarily_closed()) {
      gen_sys.insert(g);
      // Since `gen_sys' was empty, after inserting `g' we have to resize
      // the system of generators to have the right dimension.
      gen_sys.adjust_topology_and_dimension(topology(), space_dim);
      if (!is_necessarily_closed())
	// In the NNC topology, we have to add the epsilon-ray.
	gen_sys.insert(Generator::zero_dim_minus_epsilon_ray());
    }
    else {
      // Note: here we have a _legal_ topology mismatch,
      // because `g' is NOT a closure point (it is a point!)
      // However, by barely invoking `gen_sys.insert(g)' we would
      // cause a change in the topology of `gen_sys', which is wrong.
      // Thus, we insert a "topology corrected" copy of `g'.
      LinExpression nc_expr = LinExpression(g);
      gen_sys.insert(Generator::point(nc_expr, g.divisor()));
      // Since `gen_sys' was empty, after inserting `g' we have to resize
      // the system of generators to have the right dimension.
      gen_sys.adjust_topology_and_dimension(topology(), space_dim);
    }
    // No longer empty, generators up-to-date and minimized.
    clear_empty();
    set_generators_minimized();
  }
  assert(OK());
}


void
PPL::Polyhedron::add_constraints(ConSys& cs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw_topology_incompatible("add_constraints(cs)", cs);
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  size_t cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_constraints(cs)", cs);

  // Adding no constraints is a no-op.
  if (cs.num_rows() == 0) {
    assert(cs.num_columns() == 0);
    return;
  }

  if (space_dim == 0) {
    // In a 0-dimensional space the constraints are
    // trivial (e.g., 0 == 0 or 1 >= 0 or 1 > 0) or
    // inconsistent (e.g., 1 == 0 or -1 >= 0 or 0 > 0).
    // In a system of constraints `begin()' and `end()' are equal
    // if and only if the system contains trivial constraints only.
    if (cs.begin() != cs.end())
      // There is a constraint, it must be inconsistent,
      // the polyhedron is empty.
      status.set_empty();
    return;
  }

  // We only need that the system of constraints is up-to-date.
  if (is_empty() || !constraints_are_up_to_date() && !minimize())
    // We have just discovered that `*this' is empty, and adding
    // constraints to an empty polyhedron is a no-op.
    return;

  // TODO: the following instruction breaks all the
  // performance-keeping effort of the rows include in #ifdef BE_LAZY.
  // If it is worth, do perform this topology adjustement inside the
  // loop of the #ifdef BE_LAZY branch.

  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);

  // For NNC polyhedra, each strict inequality must be matched by
  // the corresponding non-strict inequality.
  if (!is_necessarily_closed())
    cs.add_corresponding_nonstrict_inequalities();

#ifdef BE_LAZY
  // Here we do not require `con_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `cs'
  // (which is not a const).
  // In contrast, in the non-BE_LAZY version, by using the method
  // Matrix::merge_rows_assign() we force `con_sys' to be sorted
  // and we _copy_ `cs'.
  size_t old_num_rows = con_sys.num_rows();
  size_t cs_num_rows = cs.num_rows();
  size_t cs_num_columns = cs.num_columns();
  con_sys.grow(old_num_rows + cs_num_rows, con_sys.num_columns());
  for (size_t i = cs_num_rows; i-- > 0; ) {
    // NOTE: we cannot directly swap the rows, since they might have
    // different capacities (besides possibly having different sizes):
    // thus, we steal one coefficient at a time.
    Constraint& c_new = con_sys[old_num_rows + i];
    Constraint& c_old = cs[i];
    if (c_old.is_equality())
      c_new.set_is_equality();
    for (size_t j = cs_num_columns; j-- > 0; )
      std::swap(c_new[j], c_old[j]);
  }
  // The new constraints have been simply appended.
  con_sys.set_sorted(false);

#else // !defined(BE_LAZY)

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  if (!con_sys.is_sorted())
    con_sys.sort_rows();

  if (!cs.is_sorted())
    cs.sort_rows();

  // The function `merge_row_assign' automatically resizes
  // the system `cs' if the dimension of the space of `cs'
  // is smaller then the dimension of the space of the polyhedron.
  con_sys.merge_rows_assign(cs);
#endif // !defined(BE_LAZY)

  // After adding new constraints, generators are no longer up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // Note: the constraint system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

bool
PPL::Polyhedron::add_generators_and_minimize(GenSys& gs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && gs.has_closure_points())
    throw_topology_incompatible("add_generators_and_min(gs)", gs);
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dimension().
  size_t gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_generators_and_min(gs)", gs);

  // Adding no generators is equivalent to just requiring minimization.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return minimize();
  }

  // Adding valid generators to a zero-dimensional polyhedron
  // transform it in the zero-dimensional universe polyhedron.
  if (space_dim == 0) {
    if (is_empty() && !gs.has_points())
      throw_invalid_generators("add_generators_and_min(gs)");
    status.set_zero_dim_univ();
    assert(OK(true));
    return true;
  }

  // Adjust `gs' to the right topology.
  // NOTE: we already checked for topology compatibility;
  // also, we do NOT adjust dimensions now, so that we will
  // spend less time to sort rows.
  gs.adjust_topology_and_dimension(topology(), gs_space_dim);
  // For NNC polyhedra, we have to add the minus_epsilon_ray.
  if (!is_necessarily_closed())
    gs.insert(Generator::zero_dim_minus_epsilon_ray());
  // Sorting.
  if (!gs.is_sorted())
    gs.sort_rows();
  // Now adjusting dimensions (topology already adjusted).
  // NOTE: sortedness is preserved.
  gs.adjust_topology_and_dimension(topology(), space_dim);

  // We use `check_empty()' because we want the flag EMPTY
  // to precisely represents the status of the polyhedron
  // (i.e., if it is false the polyhedron is really NOT empty)
  // and because, for a non-empty polyhedron, we need both
  // the system of generators and constraints minimal.
  if (check_empty()) {
    // Checking if the system of generators contains a point.
    if (!gs.has_points())
      throw_invalid_generators("add_generators_and_min(gs)");
    // If the system of generators has a point, the polyhedron is no
    // longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    clear_empty();
    set_generators_up_to_date();
    // This call to `minimize()' cannot return `false'.
    minimize();
  }
  else {
    obtain_sorted_generators_with_sat_g();
    // This call to `add_and_minimize(...)' cannot return `false'.
    add_and_minimize(false, gen_sys, con_sys, sat_g, gs);
    clear_sat_c_up_to_date();
  }
  assert(OK(true));
  return true;
}


void
PPL::Polyhedron::add_generators(GenSys& gs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && gs.has_closure_points())
    throw_topology_incompatible("add_generators(gs)", gs);
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dim.
  size_t gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_generators(gs)", gs);

  // Adding no generators is a no-op.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return;
  }

  // Adding valid generators to a zero-dimensional polyhedron
  // transform it in the zero-dimensional universe polyhedron.
  if (space_dim == 0) {
    if (is_empty() && !gs.has_points())
      throw_invalid_generators("add_generators(gs)");
    status.set_zero_dim_univ();
    assert(OK(true));
    return;
  }

  // Adjust `gs' to the right topology and dimensions.
  // NOTE: we already checked for topology compatibility.
  gs.adjust_topology_and_dimension(topology(), space_dim);
  // For NNC polyhedra, the minus_epsilon_ray is needed.
  if (!is_necessarily_closed())
    gs.insert(Generator::zero_dim_minus_epsilon_ray());

  // We only need that the system of generators is up-to-date.
  if (!generators_are_up_to_date() && !minimize()) {
    // We have just discovered that `*this' is empty.
    // So `gs' must contain at least one point.
    if (!gs.has_points())
      throw_invalid_generators("add_generators(gs)");
    // The polyhedron is no longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    set_generators_up_to_date();
    clear_empty();
    assert(OK());
    return;
  }

  // Matrix::merge_row_assign() requires both matrices to be sorted.
  if (!gen_sys.is_sorted())
    gen_sys.sort_rows();
  if (!gs.is_sorted())
    gs.sort_rows();
  gen_sys.merge_rows_assign(gs);

  // After adding new generators, constraints are no longer up-to-date.
  clear_generators_minimized();
  clear_constraints_up_to_date();

  assert(OK(true));
}


std::ostream&
PPL::operator<<(std::ostream& s, const Polyhedron& p) {
  using std::endl;

  s << "space_dim "
    << p.space_dimension()
    << endl
    << p.status
    << endl
    << "con_sys ("
    << (p.constraints_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl
    << p.con_sys
    << endl
    << "gen_sys ("
    << (p.generators_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl
    << p.gen_sys
    << endl
    << "sat_c"
    << endl
    << p.sat_c
    << endl
    << "sat_g"
    << endl
    << p.sat_g
    << endl;

  return s;
}

std::istream&
PPL::operator>>(std::istream& s, Polyhedron& p) {
  std::string str;

  s >> str;
  assert(str == "space_dim");
  s >> p.space_dim;

  s >> p.status;

  s >> str;
  assert(str == "con_sys");
  s >> str;
  assert(str == "(not_up-to-date)" || str == "(up-to-date)");
  s >> p.con_sys;
  s >> str;
  assert(str == "gen_sys");
  s >> str;
  assert(str == "(not_up-to-date)" || str == "(up-to-date)");
  s >> p.gen_sys;
  s >> str;
  assert(str == "sat_c");
  s >> p.sat_c;
  s >> str;
  assert(str == "sat_g");
  s >> p.sat_g;

  return s;
}


/*!
  When considering the generators of a polyhedron, the
  affine transformation
  \f[
    \frac{\sum_{i=0}^{n-1} a_i x_i + b}{\mathrm{denominator}}
  \f]
  is assigned to \p var where \p expr is
  \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
  (\f$b\f$ is the inhomogeneous term).

  If constraints are up-to-date, it uses the specialized function
  affine_preimage() (for the system of constraints)
  and inverse transformation to reach the same result.
  To obtain the inverse transformation we use the following observation.

  Observation:
  -# The affine transformation is invertible if the coefficient
     of \p var in this transformation (i.e., \f$a_\mathrm{var}\f$)
     is different from zero.
  -# If the transformation is invertible, then we can write
     \f[
  	\mathrm{denominator} * {x'}_\mathrm{var}
	  = \sum_{i = 0}^{n - 1} a_i x_i + b
	  = a_\mathrm{var} x_\mathrm{var}
	      + \sum_{i \neq var} a_i x_i + b,
     \f]
     so that the inverse transformation is
     \f[
	a_\mathrm{var} x_\mathrm{var}
          = \mathrm{denominator} * {x'}_\mathrm{var}
              - \sum_{i \neq j} a_i x_i - b.
     \f]

  Then, if the transformation is invertible, all the entities that
  were up-to-date remain up-to-date. Otherwise only generators remain
  up-to-date.

  In other words, if \f$R\f$ is a \f$m_1 \times n_1\f$ matrix representing
  the rays of the polyhedron, \f$V\f$ is a \f$m_2 \times n_2\f$
  matrix representing the points of the polyhedron and
  \f[
    P = \bigl\{\,
          \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
        \bigm|
          \vect{x} = \vect{\lambda} R + \vect{\mu} V,
          \vect{\lambda} \in \Rset^{m_1}_+,
          \vect{\mu} \in \Rset^{m_2}_+,
	  \sum_{i = 0}^{m_1 - 1} \lambda_i = 1
        \,\bigr\}
  \f]
  and \f$T\f$ is the affine transformation to apply to \f$P\f$, then
  the resulting polyhedron is
  \f[
    P' = \bigl\{\,
           (x_0, \ldots, T(x_0, \ldots, x_{n-1}),
                   \ldots, x_{n-1})^\mathrm{T}
         \bigm|
           (x_0, \ldots, x_{n-1})^\mathrm{T} \in P
         \,\bigr\}.
  \f]

  Affine transformations are, for example:
  - translations
  - rotations
  - symmetries.
*/
void
PPL::Polyhedron::affine_image(const Variable& var,
			      const LinExpression& expr,
			      const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  size_t expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", expr);
  // `var' should be one of the dimensions of the polyhedron.
  size_t num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());

  if (is_empty())
    return;

  if (num_var <= expr_space_dim && expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (generators_are_up_to_date())
      gen_sys.affine_image(num_var, expr, denominator);
    if (constraints_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      con_sys.affine_preimage(num_var, inverse, expr[num_var]);
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of generators.
    if (!generators_are_up_to_date())
      minimize();
    if (!is_empty()) {
      gen_sys.affine_image(num_var, expr, denominator);
      clear_constraints_up_to_date();
      clear_generators_minimized();
      clear_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
  }
  assert(OK());
}



/*!
  When considering constraints of a polyhedron, the affine transformation
  \f[
  \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator},
  \f]
  is assigned to \p var where \p expr is
  \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
  (\f$b\f$ is the inhomogeneous term).

  If generators are up-to-date, then the specialized function
  affine_image() is used (for the system of generators)
  and inverse transformation to reach the same result.
  To obtain the inverse transformation, we use the following observation.

  Observation:
  -# The affine transformation is invertible if the coefficient
     of \p var in this transformation (i.e. \f$a_\mathrm{var}\f$)
     is different from zero.
  -# If the transformation is invertible, then we can write
     \f[
  	\mathrm{denominator} * {x'}_\mathrm{var}
	  = \sum_{i = 0}^{n - 1} a_i x_i + b
          = a_\mathrm{var} x_\mathrm{var}
              + \sum_{i \neq \mathrm{var}} a_i x_i + b,
     \f],
     the inverse transformation is
     \f[
	a_\mathrm{var} x_\mathrm{var}
          = \mathrm{denominator} * {x'}_\mathrm{var}
              - \sum_{i \neq j} a_i x_i - b.
     \f].

  Then, if the transformation is invertible, all the entities that
  were up-to-date remain up-to-date. Otherwise only constraints remain
  up-to-date.

  In other words, if \f$A\f$ is a \f$m \times n\f$ matrix representing
  the constraints of the polyhedron, \f$T\f$ is the affine transformation
  to apply to \f$P\f$ and
  \f[
    P = \bigl\{\,
          \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
        \bigm|
          A\vect{x} \geq \vect{0}
        \,\bigr\}.
  \f]
  The resulting polyhedron is
  \f[
    P' = \bigl\{\,
           \vect{x} = (x_0, \ldots, x_{n-1}))^\mathrm{T}
         \bigm|
           A'\vect{x} \geq \vect{0}
         \,\bigr\},
  \f]
  where \f$A'\f$ is defined as follows:
  \f[
    {a'}_{ij}
      = \begin{cases}
          a_{ij} * \mathrm{denominator} + a_{i\mathrm{var}} * \mathrm{expr}[j]
            \quad \mathrm{for } j \neq \mathrm{var}; \\
          \mathrm{expr}[\mathrm{var}] * a_{i\mathrm{var}},
            \quad \text{for } j = \mathrm{var}.
        \end{cases}
  \f]
*/
void
PPL::Polyhedron::affine_preimage(const Variable& var,
				 const LinExpression& expr,
				 const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  size_t expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", expr);
  // `var' should be one of the dimensions of the polyhedron.
  size_t num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  if (is_empty())
    return;

  if (num_var <= expr_space_dim && expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (constraints_are_up_to_date())
      con_sys.affine_preimage(num_var, expr, denominator);
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      gen_sys.affine_image(num_var, inverse, expr[num_var]);
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of constraints.
    if (!constraints_are_up_to_date())
      minimize();
    con_sys.affine_preimage(num_var, expr, denominator);
    clear_generators_up_to_date();
    clear_constraints_minimized();
    clear_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
  assert(OK());
}

PPL::Poly_Con_Relation
PPL::Polyhedron::relation_with(const Constraint& c) const {
  // Dimension-compatibility check.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("relation_with(c)", c);

  if (is_empty())
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0)
    if (c.is_trivial_false())
      if (c.is_strict_inequality() && c[0] == 0)
	// The constraint 0 > 0 implicitly defines the hyperplane 0 = 0;
	// thus, the zero-dimensional point also saturates it.
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_disjoint();
      else
	return Poly_Con_Relation::is_disjoint();
    else
      if (c.is_equality() || c[0] == 0)
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_included();
      else
	// The zero-dimensional point saturates
	// neither the positivity constraint 1 >= 0,
	// nor the strict positivity constraint 1 > 0. 
	return Poly_Con_Relation::is_included();

  if (!generators_are_up_to_date() && !update_generators())
    // The polyhedron is empty.
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  return gen_sys.relation_with(c);
}

PPL::Poly_Gen_Relation
PPL::Polyhedron::relation_with(const Generator& g) const {
  // Dimension-compatibility check.
  if (space_dim < g.space_dimension())
    throw_dimension_incompatible("relation_with(g)", g);

  // The empty polyhedron cannot subsume a generator.
  if (is_empty())
    return Poly_Gen_Relation::nothing();

  // A universe polyhedron in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();

  if (!constraints_are_up_to_date())
    update_constraints();
  return
    con_sys.satisfies_all_constraints(g)
    ? Poly_Gen_Relation::subsumes()
    : Poly_Gen_Relation::nothing();
}

void
PPL::Polyhedron::H79_widening_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("H79_widening_assign(y)", y);
  // Dimension-compatibility check.
  size_t x_space_dim = x.space_dim;
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("H79_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    Polyhedron x_copy = x;
    Polyhedron y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  // If any argument is zero-dimensional or empty,
  // the H79-widening behaves as the identity function.
  if (x_space_dim == 0 || x.is_empty() || y.is_empty())
    return;

  // `y.gen_sys' should be in minimal form and
  // `y.sat_g' should be up-to-date.
  if (y.is_necessarily_closed()) {
    if (!y.minimize())
      // `y' is empty: the result is `x'.
      return;
  }
  else {
    // Dealing with a NNC polyhedron.
    // To obtain a correct reasoning when comparing
    // the constraints of `x' with the generators of `y',
    // we enforce the inclusion relation holding between
    // the two NNC polyhedra `x' and `y' (i.e., `y <= x')
    // to also hold for the corresponding eps-representations:
    // this is obtained by intersecting the two eps-representations.
    Polyhedron& yy = const_cast<Polyhedron&>(y);
    if (!yy.intersection_assign_and_minimize(x))
      // `y' is empty: the result is `x'.
      return;
  }

  // `x.con_sys' is just required to be up-to-date, because:
  // - if `x.con_sys' is unsatisfiable, then also `y' is empty
  //   and so the resulting polyhedron is `x';
  // - redundant constraints in `x.con_sys' do not influence the
  //   computation of the widened polyhedron. This is because
  //     CHECK ME: are the following motivations correct?
  //   if a constraint is a combination of other two constraints,
  //   it can be also in the resulting system, if the two constraints
  //   are common to both polyhedron. The redundant constraints
  //   is `redundant' also in the new polyhedron.
  //   If otherwise a constraint is redundant in the sense that it does not
  //   satisfy the saturation rule (see Section \ref prelims), it can not
  //   be put into the new system, because of the way that we use to
  //   choose the constraints.
  if (!x.constraints_are_up_to_date())
    x.update_constraints();
  // This function requires the saturation matrix `sat_g' of
  // the polyhedron `y' to choose which constraints of the
  // polyhedron `x' must be also constraints of the widened
  // polyhedron.
  if (!y.sat_g_is_up_to_date())
    y.update_sat_g();
  // `y.sat_g' is copied in a temporary one, so that
  // it can be sorted without affecting the constant polyhedron `y'.
  SatMatrix tmp_sat_g = y.sat_g;
  tmp_sat_g.sort_rows();

  // Start bulding the system of constraints of the widened polyhedron.
  ConSys new_con_sys;
  add_low_level_constraints(new_con_sys);
  new_con_sys.adjust_topology_and_dimension(topology(), x_space_dim);

  // The size of `buffer' will reach sat.num_columns() bit.
  SatRow buffer;
  // A constraint in `x.con_sys' is placed in the new constraint
  // system if its behavior with respect to `y.gen_sys' is the same
  // as that of a constraint of `y.con_sys'.
  // Namely, we check whether the saturation row `buffer'
  // (built starting from the given constraint and `y.gen_sys')
  // is a row of the saturation matrix `tmp_sat_g'.
  // Note: if the considered constraint of `x.con_sys' does not
  // satisfy the saturation rule (see Section \ref prelims), then
  // it will not appear in the resulting constraint system,
  // because `tmp_sat_g' is built starting from a minimized polyhedron.
  size_t n_constraints = x.con_sys.num_rows();
  // Note: the loop index `i' goes upwards to avoid reversing
  // the ordering of the chosen constraints.
  for (size_t i = 0; i < n_constraints; ++i) {
    buffer.clear();
    // The saturation row `buffer' is built considering the `i'-th
    // constraint of the polyhedron `x' and the generators of the
    // polyhedron `y'.
    for (size_t j = y.gen_sys.num_rows(); j-- > 0; ) {
      int sp_sgn = sgn(y.gen_sys[j] * x.con_sys[i]);
      // We are assuming that y <= x.
      assert(sp_sgn >= 0);
      if (sp_sgn > 0)
	buffer.set(j);
    }
    // We verify if `buffer' is a row of the saturation matrix
    // `sat_g' of the polyhedron `y': to do this check, we use
    // the saturation matrix `tmp_sat_g' (that is sorted)
    // in order to have faster comparisons.
    if (tmp_sat_g.sorted_contains(buffer))
      new_con_sys.add_row(x.con_sys[i]);
  }
  // CHECK ME: is this really required?
  if (!is_necessarily_closed())
    new_con_sys.add_corresponding_nonstrict_inequalities();

  // Let `new_con_sys' be the constraint system of `x'
  // and update the status of `x'.
  std::swap(x.con_sys, new_con_sys);
  x.set_constraints_up_to_date();
  x.clear_constraints_minimized();
  x.clear_generators_up_to_date();

  assert(x.OK(true));
}


void
PPL::Polyhedron::limited_H79_widening_assign(const Polyhedron& y, ConSys& cs) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.is_necessarily_closed()) {
    if (!y.is_necessarily_closed())
      throw_topology_incompatible("limited_H79_widening_assign(y, cs)", y);
    if (cs.has_strict_inequalities())
      throw_topology_incompatible("limited_H79_widening_assign(y, cs)", cs);
  }
  else if (y.is_necessarily_closed())
    throw_topology_incompatible("limited_H79_widening_assign(y, cs)", y);

  // Dimension-compatibility check.
  size_t x_space_dim = x.space_dim;
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("limited_H79_widening_assign(y, cs)", y);
  // `cs' must be dimension-compatible with the two polyhedra.
  size_t cs_space_dim = cs.space_dimension();
  if (x_space_dim < cs_space_dim)
    throw_dimension_incompatible("limited_H79_widening_assign(y, cs)", cs);

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    Polyhedron x_copy = x;
    Polyhedron y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  if (y.is_empty())
    return;
  if (x.is_empty())
    return;

  // The limited H79-widening between two polyhedra in a
  // zero-dimensional space is a polyhedron in a zero-dimensional
  // space, too.
  if (x_space_dim == 0)
    return;

  if (!y.minimize())
    // We have just discovered that `y' is empty.
    return;

  // Update the generators of `x': these are used to select,
  // from the constraints in `cs', those that must be added
  // to the resulting polyhedron.
  if (!x.generators_are_up_to_date() && !x.update_generators())
    // We have just discovered that `x' is empty.
    return;

  size_t new_cs_num_rows = 0;
  for (size_t i = 0, cs_num_rows = cs.num_rows(); i < cs_num_rows; ++i) {
    // The constraints to be added must be saturated by both `x' and `y'.
    // We only consider the generators of the greater polyhedron `x',
    // because the generators of `y' can be obtained by combining
    // those of `x' (since `y' is contained in `x').
    Poly_Con_Relation relation = x.gen_sys.relation_with(cs[i]);
    if (relation.implies(Poly_Con_Relation::is_included()))
      // The chosen constraints are put at the top of the
      // matrix \p cs.
      std::swap(cs[new_cs_num_rows], cs[i]);
    ++new_cs_num_rows;
  }
  // We erase the constraints that are not saturated or satisfied
  // by the generators of `x' and `y' and that have been put to
  // the end of the matrix \p cs.
  cs.erase_to_end(new_cs_num_rows);

  x.H79_widening_assign(y);
#if 1
  // TODO : merge_rows_assign (in the #else branch below)
  // does not automatically adjust the topology of cs,
  // so that the #else branch, as it is, is not correct.
  // However, by simply calling add_constraints() we are going
  // to duplicate a big number of constraints.
  // Would it be worth to provide a topology-adjusting
  // merge_rows_assign method?
  x.add_constraints(cs);
#else
  // The system of constraints of the resulting polyhedron is
  // composed by the constraints of the widened polyhedron `x'
  // and by those of the new `cs'.
  // The function `merge_row_assign' automatically resizes
  // the system `cs' if the dimension of the space of `cs'
  // is smaller then the dimension of the space of the polyhedron.
  cs.sort_rows();
  x.con_sys.sort_rows();
  x.con_sys.merge_rows_assign(cs);
#endif

  // Only the system of constraints is up-to-date.
  x.set_constraints_up_to_date();
  x.clear_constraints_minimized();
  x.clear_generators_up_to_date();

  assert(OK());
}


void
PPL::Polyhedron::time_elapse_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("time_elapse_assign(y)", y);
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility checks.
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("time_elapse_assign(y)", y);
  
  // Dealing with the zero-dimensional case.
  if (x_space_dim == 0) {
    if (y.is_empty())
      x.set_empty();
    return;
  }
  
  // If either one of `x' or `y' is empty, the result is empty too.
  if (x.is_empty() || y.is_empty()
      || (!x.generators_are_up_to_date() && !x.update_generators())
      || (!y.generators_are_up_to_date() && !y.update_generators())) {
    x.set_empty();
    return;
  }

  // At this point the generator systems of both polyhedra are up-to-date.
  GenSys gs = y.gen_sys;
  size_t gs_num_rows = gs.num_rows();

  if (!x.is_necessarily_closed()) {
    // `x' and `y' are NNC polyhedra.
    size_t eps_index = space_dim + 1;
    for (size_t i = gs_num_rows; i-- > 0; )
      switch (gs[i].type()) {
      case Generator::POINT:
	// Transform it into (and then treat it as) a closure point.
	gs[i][eps_index] = 0;
	// Intentionally fall through.
      case Generator::CLOSURE_POINT:
	{
	  Generator& cp = gs[i];
	  // If it is the origin, erase it.
	  if (cp.all_homogeneous_terms_are_zero()) {
	    --gs_num_rows;
	    std::swap(cp, gs[gs_num_rows]);
	  }
	  // Otherwise, transform the closure point into a ray.
	  else {
	    cp[0] = 0;
	    // Enforce normalization.
	    cp.normalize();
	  }
	}
	break;
      default:
	// For rays and lines, nothing to be done.
	break;
      }
  }
  else
    // `x' and `y' are C polyhedra.
    for (size_t i = gs_num_rows; i-- > 0; )
      switch (gs[i].type()) {
      case Generator::POINT:
	{
	  Generator& p = gs[i];
	  // If it is the origin, erase it.
	  if (p.all_homogeneous_terms_are_zero()) {
	    --gs_num_rows;
	    std::swap(p, gs[gs_num_rows]);
	  }
	  // Otherwise, transform the point into a ray.
	  else {
	    p[0] = 0;
	    // Enforce normalization.
	    p.normalize();
	  }
	}
	break;
      default:
	// For rays and lines, nothing to be done.
	break;
      }
  // If it was present, erase the origin point or closure point,
  // which cannot be tranformed into a valid ray or line.
  gs.erase_to_end(gs_num_rows);
  
  // `gs' may now have no rows.
  // Namely, this happens when `y' was the singleton polyehdron
  // having the origin as the one and only point.
  // In such a case, the resulting polyhedron is equal to `x'.
  if (gs_num_rows == 0)
    return;

  // Otherwise, the two systems are merged.
  // `Matrix::merge_row_assign()' requires both matrices to be ordered.
  if (!x.gen_sys.is_sorted())
    x.gen_sys.sort_rows();
  // We have changed `gs': it must be sorted.
  gs.sort_rows();
  x.gen_sys.merge_rows_assign(gs);
  // Only the system of generators is up-to-date.
  x.clear_constraints_up_to_date();
  x.clear_generators_minimized();

  assert(x.OK(true) && y.OK(true));
}


bool
PPL::Polyhedron::check_universe() const {
  if (space_dim == 0)
    return !is_empty();

  if (!constraints_are_minimized())
    minimize();
  if (is_necessarily_closed())
    return (con_sys.num_rows() == 1
	    && con_sys[0].is_inequality()
	    && con_sys[0][0] > 0
	    && con_sys[0].all_homogeneous_terms_are_zero());
  else {
    // Polyhedron NOT-necessarily closed.
    if (con_sys.num_rows() != 2
	|| con_sys[0].is_equality()
	|| con_sys[1].is_equality())
      return false;
    obtain_sorted_constraints();
    const Constraint& eps_leq_one = con_sys[0]; 
    const Constraint& positivity = con_sys[1]; 
    size_t eps_index = con_sys.num_columns() - 1;
    if (eps_leq_one[0] <= 0
	|| eps_leq_one[eps_index] >= 0
	|| positivity[0] <= 0
	|| positivity[eps_index] != 0)
      return false;
    for (size_t i = eps_index; i-- > 1; )
      if (eps_leq_one[i] != 0 || positivity[i] != 0)
	return false;
    return true;
  }
}

bool
PPL::Polyhedron::is_bounded() const {
  // A zero-dimensional or empty polyhedron is bounded.
  if (space_dim == 0
      || is_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return true;
  
  for (size_t i = gen_sys.num_rows(); i-- > 0; )
    if (gen_sys[i][0] == 0)
      // A line or a ray has been found.
      // If the polyhedron is necessarily closed or
      // if it is NNC but the ray is NOT the minus_epsilon_ray,
      // then the polyhedron is not bounded.
      if (is_necessarily_closed() || gen_sys[i][space_dim + 1] == 0)
	return false;

  // The system of generators is composed only by
  // points and closure points: the polyhedron is bounded.
  return true;
}


bool
PPL::Polyhedron::bounds(const LinExpression& expr, bool from_above) const {
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  size_t expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible((from_above
				  ? "bounds_from_above(e)"
				  : "bounds_from_below(e)"), expr);

  // A zero-dimensional or empty polyhedron bounds everything.
  if (space_dim == 0
      || is_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return true;
  
  for (size_t i = gen_sys.num_rows(); i-- > 0; ) {
    const Generator& g = gen_sys[i];
    // Only lines and rays in `*this' can cause `expr' to be unbounded.
    if (g[0] == 0) {
      // Compute the scalar product between `g' and `expr'.
      tmp_Integer[0] = 0;
      for (size_t j = expr.size(); j-- > 0; ) {
	// The following two lines optimize the computation
	// of tmp_Integer[0] += g[j] * expr[j].
	tmp_Integer[1] = g[j] * expr[j];
	tmp_Integer[0] += tmp_Integer[1];
      }
      int sp_sign = sgn(tmp_Integer[0]);
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
PPL::Polyhedron::is_topologically_closed() const {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return true;
  // Any empty or zero-dimensional polyhedron is closed.
  if (is_empty() || space_dimension() == 0)
    return true;

  // A polyhedron is closed if its (weakly) minimized
  // generator system has no closure points.
  if (!generators_are_minimized())
    if (!minimize())
      // Found empty: it is topologically closed.
      return true;
  // Here the polyhedron is not empty and
  // generators are weakly minimized.
  return !gen_sys.has_closure_points();
}

void
PPL::Polyhedron::topological_closure_assign() {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return;
  // Any empty or zero-dimensional polyhedron is closed.
  if (is_empty() || space_dimension() == 0)
    return;

  size_t eps_index = space_dim + 1;
  bool changed = false;

  if (constraints_are_up_to_date()) {
    // Remove all strict inequality constraints:
    // the corresponding non-strict inequality constraints
    // are already (either explicitly or implicitly) in the
    // constraint system and will be left there.
    size_t cs_rows = con_sys.num_rows();
    for (size_t i = cs_rows; i-- > 0; ) {
      Constraint& c = con_sys[i];
      if (c[eps_index] < 0 && !c.is_trivial_true()) {
	--cs_rows;
	std::swap(c, con_sys[cs_rows]);
	changed = true;
      }
    }
    if (changed) {
      con_sys.erase_to_end(cs_rows);
      // Constraints preserve minimization,
      // but they may be no longer sorted.
      con_sys.set_sorted(false);
      // The generators are no longer up-to-date.
      clear_generators_up_to_date();
    }
  }
  else {
    assert(generators_are_up_to_date());
    // Transform all closure points into points.
    for (size_t i = gen_sys.num_rows(); i-- > 0; ) {
      Generator& g = gen_sys[i];
      if (g[0] > 0 && g[eps_index] == 0) {
	// Make it a point (normalization is preserved).
	g[eps_index] = g[0];
	changed = true;
      }
    }
    if (changed) {
      // Generators are no longer minimized
      // (but their sortedness is preserved).
      clear_generators_minimized();
      // Constraints are no longer up-to-date.
      clear_constraints_up_to_date();
    }
  }
}


/*!
  Checks if \p *this is really a polyhedron, i.e., excludes all the extreme
  cases.

  For this purpose we check several things and in particular we check
  whether
  - the system of constraints and the system of generators satisfy the
    dimensional rules,
  - the system of constraints and the system of generators are really
    minimized, when they are declared minimal.
*/
// CHECK ME.
bool
PPL::Polyhedron::OK(bool check_not_empty) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // The expected number of columns in the constraint and generator
  // systems, if they are not empty. 
  size_t poly_num_columns = space_dim + (is_necessarily_closed() ? 1 : 2); 

  // Check whether the topologies of `con_sys' and `gen_sys' agree.
  if (con_sys.topology() != gen_sys.topology()) {
#ifndef NDEBUG
    cerr << "Constraints and generators have different topologies!"
	 << endl;
#endif
    goto bomb;
  }

  // Check whether the saturation matrices are well-formed.
  if (!sat_c.OK())
    goto bomb;
  if (!sat_g.OK())
    goto bomb;

  // Checks the possible meaningful status combinations.
  if (!status.OK()) {
#ifndef NDEBUG
    cerr << "Wrong status!" << endl;
#endif
    goto bomb;
  }

  if (is_empty()) {
    if (check_not_empty) {
      // The caller does not want the polyhedron to be empty.
#ifndef NDEBUG
      cerr << "Empty polyhedron!" << endl;
#endif
      goto bomb;
    }

    // An empty polyhedron is allowed if the system of constraints
    // either has no rows or only contains an unsatisfiable constraint.
    if (con_sys.num_rows() == 0)
      return true;
    else {
      if (con_sys.space_dimension() != space_dim) {
#ifndef NDEBUG
	cerr << "The polyhedron is in a space of dimension "
	     << space_dim
	     << " while the system of constraints is in a space of dimension "
	     << con_sys.space_dimension()
	     << endl;
#endif
	goto bomb;
      }
      if (con_sys.num_rows() != 1) {
#ifndef NDEBUG
	cerr << "The system of constraints for an empty polyhedron "
	     << "has more then one row"
	     << endl;
#endif
	goto bomb;
      }
      else
	if (!con_sys[0].is_trivial_false()) {
#ifndef NDEBUG
	  cerr << "Empty polyhedron with a satisfiable system of constraints"
	       << endl;
#endif
	  goto bomb;
	}
    }
  }

  // A zero-dimensional, non-empty polyhedron is legal only if the
  // system of constraint `con_sys' and the system of generators
  // `gen_sys' have no rows.
  if (space_dim == 0)
    if (con_sys.num_rows() != 0 || gen_sys.num_rows() != 0) {
#ifndef NDEBUG
      cerr << "Zero-dimensional polyhedron with a non-empty"
	   << endl
	   << "system of constraints or generators."
	   << endl;
#endif
      goto bomb;
    }
    else
      return true;

  // A polyhedron is defined by a system of constraints
  // or a system of generators: at least one of them must be up to date.
  if (!constraints_are_up_to_date() && !generators_are_up_to_date()) {
#ifndef NDEBUG
    cerr << "Polyhedron not empty, not zero-dimensional"
	 << endl
	 << "and with neither constraints nor generators up-to-date!"
	 << endl;
#endif
    goto bomb;
  }

  // Here we check if the size of the matrices is consistent.
  // Let us suppose that all the matrices are up-to-date; this means:
  // `con_sys' : number of constraints x poly_num_columns
  // `gen_sys' : number of generators  x poly_num_columns
  // `sat_c'   : number of generators  x number of constraints
  // `sat_g'   : number of constraints x number of generators.
  if (constraints_are_up_to_date()) {
    if (con_sys.num_columns() != poly_num_columns) {
#ifndef NDEBUG
      cerr << "Incompatible size! (con_sys and space_dim)"
	   << endl;
#endif
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (con_sys.num_rows() != sat_c.num_columns()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (con_sys and sat_c)"
	     << endl;
#endif
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (con_sys.num_rows() != sat_g.num_rows()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (con_sys and sat_g)"
	     << endl;
#endif
	goto bomb;
      }
    if (generators_are_up_to_date())
      if (con_sys.num_columns() != gen_sys.num_columns()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (con_sys and gen_sys)"
	     << endl;
#endif
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    if (gen_sys.num_columns() != poly_num_columns) {
#ifndef NDEBUG
      cerr << "Incompatible size! (gen_sys and space_dim)"
	   << endl;
#endif
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (gen_sys.num_rows() != sat_c.num_rows()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (gen_sys and sat_c)"
	     << endl;
#endif
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (gen_sys.num_rows() != sat_g.num_columns()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (gen_sys and sat_g)"
	     << endl;
#endif
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    // Check if the system of generators is well-formed.
    if (!gen_sys.OK())
      goto bomb;

    // A non_empty system of generators describing a polyhedron
    // is valid iff it contains a point.
    if (gen_sys.num_rows() > 0 && !gen_sys.has_points()) {
#ifndef NDEBUG
      cerr << "Non-empty generator system declared up-to-date "
	   << "has no points!"
	   << endl;
#endif
      goto bomb;
    }

    if (generators_are_minimized()) {
      // If the system of generators is minimized, the number of lines,
      // rays and points of the polyhedron must be the same
      // of the temporary minimized one. If it does not happen
      // the polyhedron is not ok.
      ConSys new_con_sys(topology());
      GenSys copy_of_gen_sys = gen_sys;
      SatMatrix new_sat_c;
      minimize(false, copy_of_gen_sys, new_con_sys, new_sat_c);
      size_t copy_num_lines = copy_of_gen_sys.num_lines();
      if (gen_sys.num_rows() != copy_of_gen_sys.num_rows()
	  || gen_sys.num_lines() != copy_num_lines
	  || gen_sys.num_rays() != copy_of_gen_sys.num_rays()) {
#ifndef NDEBUG
	cerr << "Generators are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the generators:"
	     << endl
	     << copy_of_gen_sys
	     << endl;
#endif
	goto bomb;
      }

      // CHECKME : the following observation is not formally true
      //           for a NNC_Polyhedron. But it may be true for its
      //           representation ...

      // If the corresponding polyhedral cone is _pointed_, then
      // a minimal system of generators is unique up to positive scaling.
      // We thus verify if the cone is pointed (i.e., there are no lines)
      // and, after normalizing and sorting a copy of the matrix `gen_sys'
      // of the polyhedron (we use a copy not to modify the polyhedron's
      // matrix) and the matrix `copy_of_gen_sys' that has been just
      // minimized, we check if the two matrices are identical.  If
      // they are different it means that the generators of the
      // polyhedron are declared minimized, but they are not.
      if (copy_num_lines == 0) {
	copy_of_gen_sys.strong_normalize();
	copy_of_gen_sys.sort_rows();
	GenSys tmp_gen = gen_sys;
	tmp_gen.strong_normalize();
	tmp_gen.sort_rows();
	if (copy_of_gen_sys != tmp_gen) {
#ifndef NDEBUG
	  cerr << "Generators are declared minimized, but they are not!"
	       << endl
	       << "(we are in the case:"
	       << endl
	       << "dimension of lineality space equal to 0)"
	       << endl
	       << "Here is the minimized form of the generators:"
	       << endl
	       << copy_of_gen_sys
	       << endl;
#endif
	    goto bomb;
	}
      }
    }
  }

  if (constraints_are_up_to_date()) {
    // Check if the system of constraints is well-formed.
    if (!con_sys.OK())
      goto bomb;

    // A non-empty system of constraints describing a polyhedron
    // must contain a constraint with a non-zero inhomogeneous term;
    // such a constraint corresponds to (a combination of other
    // constraints with):
    // -* the positivity constraint, for necessarily closed polyhedra;
    // -* the epsilon <= 1 constraint, for NNC polyhedra.
    bool no_positivity_constraint = true;
    for (size_t i = con_sys.num_rows(); i-- > 0; )
      if (con_sys[i][0] != 0) {
	no_positivity_constraint = false;
	break;
      }
    if (no_positivity_constraint) {
#ifndef NDEBUG
      cerr << "Non-empty constraint system has no positivity constraint"
	   << endl;
#endif
      goto bomb;
    }

    ConSys copy_of_con_sys = con_sys;
    GenSys new_gen_sys(topology());
    SatMatrix new_sat_g;

    if (minimize(true, copy_of_con_sys, new_gen_sys, new_sat_g)) {
      if (check_not_empty) {
	// Want to know the satisfiability of the constraints.
#ifndef NDEBUG
	cerr << "Insoluble system of constraints!"
	     << endl;
#endif
	goto bomb;
      }
      else
	// The polyhedron is empty, there is nothing else to check.
	return true;
    }

    if (constraints_are_minimized()) {
      // If the constraints are minimized, the number of equalities
      // and of inequalities of the system of the polyhedron must be
      // the same of the temporary minimized one.
      // If it does not happen, the polyhedron is not ok.
      if (con_sys.num_rows() != copy_of_con_sys.num_rows()
	  || con_sys.num_equalities() != copy_of_con_sys.num_equalities()) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
#endif
	goto bomb;
      }
      // The matrix `copy_of_con_sys' has the form that is obtained
      // after the functions gauss() and back_substitute().
      // A system of constraints can be minimal even if it does not
      // have this form. So, to verify if the polyhedron is correct,
      // we copy the matrix `con_sys' in a temporary one that then
      // is modified using the functions gauss() and back_substitute().
      // If the temporary matrix and `copy_of_con_sys' are different,
      // the polyhedron is not ok.
      copy_of_con_sys.strong_normalize();
      copy_of_con_sys.sort_rows();
      ConSys tmp_con = con_sys;
      tmp_con.sort_rows();
      tmp_con.back_substitute(tmp_con.gauss());
      tmp_con.strong_normalize();
      tmp_con.sort_rows();
      if (tmp_con != copy_of_con_sys) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
#endif
	goto bomb;
      }
    }
  }

  if (sat_c_is_up_to_date())
    for (size_t i = sat_c.num_rows(); i-- > 0; ) {
      Generator tmp_gen = gen_sys[i];
      SatRow tmp_sat = sat_c[i];
      for (size_t j = sat_c.num_columns(); j-- > 0; )
	if (sgn(tmp_gen * con_sys[j]) != tmp_sat[j]) {
#ifndef NDEBUG
	  cerr << "sat_c is declared up-to-date, but it is not!"
	       << endl;
#endif
	  goto bomb;
	}
    }

  if (sat_g_is_up_to_date())
    for (size_t i = sat_g.num_rows(); i-- > 0; ) {
      Constraint tmp_con = con_sys[i];
      SatRow tmp_sat = sat_g[i];
      for (size_t j = sat_g.num_columns(); j-- > 0; )
	if (sgn(tmp_con * gen_sys[j]) != tmp_sat[j]) {
#ifndef NDEBUG
	  cerr << "sat_g is declared up-to-date, but it is not!"
	       << endl;
#endif
	  goto bomb;
	}
    }

  return true;

 bomb:
#ifndef NDEBUG
  cerr << "Here is the guilty polyhedron:"
       << endl
       << *this
       << endl;
#endif
  return false;
}
