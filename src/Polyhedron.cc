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
#include <deque>

#define BE_LAZY

#ifndef NEW_LIMITED_GROWTH_ORDERING
#define NEW_LIMITED_GROWTH_ORDERING 1
#endif

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

  // If the polyhedron has pending generators, we process them to obtain
  // the constraints. No processing is needed if the polyhedron has
  // pending constraints.
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();
  
  // We insist in returning a sorted system of constraints.
  obtain_sorted_constraints();
  return con_sys;
}

const PPL::ConSys&
PPL::Polyhedron::minimized_constraints() const {
  // `minimize()' or `strongly_minimize_constraints()'
  // will process any pending constraints or generators.
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_constraints();
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

  // If the polyhedron has pending constraints, we process them to obtain
  // the generators (we may discover that the polyhedron is empty).
  // No processing is needed if the polyhedron has pending generators.
  if ((has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators())) {
    // We have just discovered that `*this' is empty.
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return gen_sys;
  }
  
  // We insist in returning a sorted system of generators:
  // this is needed so that the const_iterator on GenSys
  // could correctly filter out the matched closure points
  // in the case of a NNC polyhedron. 
  obtain_sorted_generators();
  return gen_sys;
}

const PPL::GenSys&
PPL::Polyhedron::minimized_generators() const {
  // `minimize()' or `strongly_minimize_generators()'
  // will process any pending constraints or generators.
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_generators();
  // Note: calling generators() also ensure sortedness,
  // which is required to correctly filter the output
  // of an NNC generator system.
  return generators();
}


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
    // are minimized
    x.set_constraints_minimized();
    x.set_generators_minimized();
  }
  return !empty;
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

bool
PPL::operator<=(const Polyhedron& x, const Polyhedron& y) {
  // Topology compatibility check.
  if (x.topology() != y.topology())
    Polyhedron::throw_topology_incompatible("operator<=("
					    "const Polyhedron& x, "
					    "const Polyhedron& y)", x, y);
  dimension_type x_space_dim = x.space_dim;
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
    dimension_type eps_index = x_space_dim + 1;
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
PPL::are_disjoint(const Polyhedron& x, const Polyhedron& y) {
  Polyhedron z = x;
  z.intersection_assign_and_minimize(y);
  return z.check_empty();
}

bool
PPL::Polyhedron::intersection_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("inters_assign_and_min(y)", y);
  dimension_type x_space_dim = x.space_dim;
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

  // `x' must be minimized and have sorted constraints.
  // `minimize()' will process any pending constraints or generators.
  if (!x.minimize())
    // We have just discovered that `x' is empty.
    return false;
  x.obtain_sorted_constraints_with_sat_c();

  // `y' must have updated, possibly pending constraints.
  if (y.has_pending_generators())
    y.process_pending_generators();
  else if (!y.constraints_are_up_to_date())
    y.update_constraints();

  bool empty;
  if (y.con_sys.num_pending_rows() > 0) {
    // Integrate `y.con_sys' as pending constraints of `x',
    // sort them in place and then call `add_and_minimize()'.
    x.con_sys.add_pending_rows(y.con_sys);
    x.con_sys.sort_pending_and_remove_duplicates();
    if (x.con_sys.num_pending_rows() == 0) {
      // All pending constraints were duplicates.
      x.clear_pending_constraints();
      assert(OK(true));
      return true;
    }
    empty = add_and_minimize(true, x.con_sys, x.gen_sys, x.sat_c);
  }
  else {
    y.obtain_sorted_constraints();
    empty = add_and_minimize(true, x.con_sys, x.gen_sys, x.sat_c, y.con_sys);
  }

  if (empty)
    x.set_empty();
  else {
    // On exit of the function `intersection_assign_and_minimize()'
    // the polyhedron is up-to-date and `sat_c' is meaningful.
    x.set_sat_c_up_to_date();
    x.clear_sat_g_up_to_date();
  }
  assert(x.OK(!empty));
  return !empty;
}

void
PPL::Polyhedron::intersection_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("inters_assign(y)", y);
  dimension_type x_space_dim = x.space_dim;
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

  // Both systems of constraints have to be up-to-date,
  // possibly having pending constraints.
  if (x.has_pending_generators())
    x.process_pending_generators();
  else if (!x.constraints_are_up_to_date())
    x.update_constraints();

  if (y.has_pending_generators())
    y.process_pending_generators();
  else if (!y.constraints_are_up_to_date())
    y.update_constraints();

  // Here both systems are up-to-date and possibly have pending constraints
  // (but they cannot have pending generators).
  assert(!x.has_pending_generators() && x.constraints_are_up_to_date());
  assert(!y.has_pending_generators() && y.constraints_are_up_to_date());

  // If `x' can support pending constraints,
  // the constraints of `y' are added as pending constraints of `x'.
  if (x.can_have_something_pending()) {
    x.con_sys.add_pending_rows(y.con_sys);
    x.set_constraints_pending();
  }
  else {
    // `x' cannot support pending constraints.
    if (y.has_pending_constraints())
      x.con_sys.add_rows(y.con_sys);
    else {
      // Neither `x' nor `y' have pending constraints.
      x.obtain_sorted_constraints();
      y.obtain_sorted_constraints();
      x.con_sys.merge_rows_assign(y.con_sys);
    }
    // Generators are no longer up-to-date
    // and constraints are no longer minimized.
    x.clear_generators_up_to_date();
    x.clear_constraints_minimized();
  }
  assert(x.OK() && y.OK());
}

void
PPL::Polyhedron::concatenate_assign(const Polyhedron& y) {
  if (topology() != y.topology())
    throw_topology_incompatible("concatenate_assign(y)", y);

  dimension_type added_columns = y.space_dimension();

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

  // The constraints of `x' (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();

  // The matrix for the new system of constraints is obtained
  // by leaving the old system of constraints in the upper left-hand side
  // and placing the constraints of `cs' in the lower right-hand side.
  // NOTE: here topologies agree, whereas dimensions may not agree.
  dimension_type old_num_rows = con_sys.num_rows();
  dimension_type old_num_columns = con_sys.num_columns();
  dimension_type added_rows = cs.num_rows();

  // We already dealt with the cases of an empty or zero-dim `y' polyhedron;
  // also, `cs' contains the low-level constraints, at least.
  assert(added_rows > 0 && added_columns > 0);

  con_sys.grow(old_num_rows + added_rows, old_num_columns + added_columns);
  // Move the epsilon coefficient to the last column, if needed.
  if (!is_necessarily_closed())
    con_sys.swap_columns(old_num_columns - 1,
			 old_num_columns - 1 + added_columns);
  dimension_type cs_num_columns = cs.num_columns();
  // Steal the constraints from `cs' and put them in `con_sys'
  // using the right displacement for coefficients.
  for (dimension_type i = added_rows; i-- > 0; ) {
    Constraint& c_old = cs[i];
    Constraint& c_new = con_sys[old_num_rows + i];
    // Method `grow', by default, added inequalities.
    if (c_old.is_equality())
      c_new.set_is_equality();
    // The inhomogeneous term is not displaced.
    std::swap(c_new[0], c_old[0]);
    // All homogeneous terms (included the epsilon coefficient,
    // if present) are displaced by `space_dim' columns.
    for (dimension_type j = 1; j < cs_num_columns; ++j)
      std::swap(c_old[j], c_new[space_dim + j]);
  }
  
  if (can_have_something_pending()) {
    // If `*this' can support pending constraints, then, since we have
    // resized the system of constraints, we must also add to the generator
    // system those lines corresponding to the newly added dimensions,
    // because the non-pending parts of `con_sys' and `gen_sys' must still
    // be a DD pair in minimal form.
    gen_sys.add_rows_and_columns(added_columns);
    gen_sys.set_sorted(false);
    if (!is_necessarily_closed())
      gen_sys.swap_columns(old_num_columns - 1,
			   old_num_columns - 1 + added_columns);
    // The added lines are not pending.
    gen_sys.unset_pending_rows();
    // Since we added new lines at the beginning of `x.gen_sys',
    // we also have to adjust the saturation matrix `sat_c'.
    // FIXME: if `sat_c' is not up-to-date, couldn't we directly update
    // `sat_g' by resizing it and shifting its columns?
    if (!sat_c_is_up_to_date()) {
      sat_c.transpose_assign(sat_g);
      clear_sat_g_up_to_date();
      set_sat_c_up_to_date();
    }
    sat_c.resize(sat_c.num_rows() + added_columns, sat_c.num_columns());
    // The old saturation rows are copied at the end of the matrix.
    // The newly introduced lines saturate all the non-pending constraints,
    // thus their saturations rows are made of zeroes.
    for (dimension_type i = sat_c.num_rows() - added_columns; i-- > 0; )
      std::swap(sat_c[i], sat_c[i+added_columns]);
    // Since `added_rows > 0', we now have pending constraints.
    set_constraints_pending();
  }
  else {
    // The polyhedron cannot have pending constraints.
    con_sys.unset_pending_rows();
#ifdef BE_LAZY
    con_sys.set_sorted(false);
#else
    con_sys.sort_rows();
#endif
    clear_constraints_minimized();
    clear_generators_up_to_date();
    clear_sat_g_up_to_date();
    clear_sat_c_up_to_date();
  }
  // Update space dimension.
  space_dim += added_columns;

  // The system of constraints may be unsatisfiable,
  // thus we do not check for satisfiability.
  assert(OK());
}

bool
PPL::Polyhedron::poly_hull_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_hull_assign_and_min(y)", y);    
  dimension_type x_space_dim = x.space_dim;
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

  // `x' must have minimized constraints and generators.
  // `minimize()' will process any pending constraints or generators.
  if (!x.minimize()) {
    // We have just discovered that `x' is empty.
    x = y;
    return minimize();
  }
  // x must have `sat_g' up-to-date and sorted generators.
  x.obtain_sorted_generators_with_sat_g();

  // `y' must have updated, possibly pending generators.
  if ((y.has_pending_constraints() && !y.process_pending_constraints())
      || (!y.generators_are_up_to_date() && !y.update_generators()))
    // We have just discovered that `y' is empty
    // (and we know that `x' is not empty).
    return true;

  if (y.gen_sys.num_pending_rows() > 0) {
    // Integrate `y.gen_sys' as pending generators of `x',
    // sort them in place and then call `add_and_minimize()'.
    x.gen_sys.add_pending_rows(y.gen_sys);
    x.gen_sys.sort_pending_and_remove_duplicates();
    if (x.gen_sys.num_pending_rows() == 0) {
      // All pending generators were duplicates.
      x.clear_pending_generators();
      assert(OK(true) && y.OK());
      return true;
    }
    add_and_minimize(true, x.gen_sys, x.con_sys, x.sat_g);
  }
  else {
    y.obtain_sorted_generators();
    add_and_minimize(true, x.gen_sys, x.con_sys, x.sat_g, y.gen_sys);
  }
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
  dimension_type x_space_dim = x.space_dim;
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

  // Both systems of generators have to be up-to-date,
  // possibly having pending generators.
  if ((x.has_pending_constraints() && !x.process_pending_constraints())
      || (!x.generators_are_up_to_date() && !x.update_generators())) {
    // Discovered `x' empty when updating generators.
    x = y;
    return;
  }
  if ((y.has_pending_constraints() && !y.process_pending_constraints())
      || (!y.generators_are_up_to_date() && !y.update_generators()))
    // Discovered `y' empty when updating generators.
    return;

  // Here both systems are up-to-date and possibly have pending generators
  // (but they cannot have pending constraints).
  assert(!x.has_pending_constraints() && x.generators_are_up_to_date());
  assert(!y.has_pending_constraints() && y.generators_are_up_to_date());

  // If `x' can support pending generators,
  // the generators of `y' are added as pending generators of `x'.
  if (x.can_have_something_pending()) {
    x.gen_sys.add_pending_rows(y.gen_sys);
    x.set_generators_pending();
  }
  else {
    // `x' cannot support pending generators.
    if (y.has_pending_generators())
      x.gen_sys.add_rows(y.gen_sys);
    else {
      // Neither `x' nor `y' have pending generators.
      x.obtain_sorted_generators();
      y.obtain_sorted_generators();
      x.gen_sys.merge_rows_assign(y.gen_sys);
    }
    // Constraints are no longer up-to-date
    // and generators are no longer minimized.
    x.clear_constraints_up_to_date();
    x.clear_generators_minimized();    
  }
  // At this point both `x' and `y' are not empty.
  assert(x.OK(true) && y.OK(true));
}

void
PPL::Polyhedron::poly_difference_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_difference_assign(y)", y);    
  dimension_type x_space_dim = x.space_dim;
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

  // Being lazy here is only harmful.
  // `minimize()' will process any pending constraints or generators.
  x.minimize();
  y.minimize();

  const ConSys& y_cs = y.constraints();
  for (ConSys::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    Polyhedron z = x;
    const Constraint& c = *i;
    assert(!c.is_trivial_true());
    assert(!c.is_trivial_false());
    LinExpression e = LinExpression(c);
    switch (c.type()) {
    case Constraint::NONSTRICT_INEQUALITY:
      if (is_necessarily_closed())
	z.add_constraint(e <= 0);
      else
	z.add_constraint(e < 0);
      break;
    case Constraint::STRICT_INEQUALITY:
      z.add_constraint(e <= 0);
      break;
    case Constraint::EQUALITY:
      if (is_necessarily_closed())
	// We have already filtered out the case
	// when `x' is included in `y': the result is `x'.
	return;
      else {
	Polyhedron w = x;
	w.add_constraint(e < 0);
	new_polyhedron.poly_hull_assign(w);
	z.add_constraint(e > 0);
      }
      break;
    }
    new_polyhedron.poly_hull_assign(z);
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
  assert(OK(not_empty));
  return not_empty;
}

void
PPL::Polyhedron::add_dimensions(Matrix& mat1,
				Matrix& mat2,
				SatMatrix& sat1,
				SatMatrix& sat2,
				dimension_type add_dim) {
  assert(mat1.topology() == mat2.topology());
  assert(mat1.num_columns() == mat2.num_columns());
  assert(add_dim != 0);

  mat1.add_zero_columns(add_dim);
  dimension_type old_index = mat2.first_pending_row();
  mat2.add_rows_and_columns(add_dim);
  // The added rows are in the non-pending part.
  mat2.set_index_first_pending_row(old_index + add_dim);

  // The resulting saturation matrix will be the follow:
  // from row    0    to      add_dim-1       : only zeroes
  //          add_dim     add_dim+num_rows-1  : old saturation matrix

  // In fact all the old generators saturate all the new constraints
  // because the polyhedron has not been embedded in the new space.
  sat1.resize(sat1.num_rows() + add_dim, sat1.num_columns());
  // The old matrix is copied at the end of the new matrix.
  for (dimension_type i = sat1.num_rows() - add_dim; i-- > 0; )
    std::swap(sat1[i], sat1[i+add_dim]);
  // Computes the "sat_c", too.
  sat2.transpose_assign(sat1);

  if (!mat1.is_necessarily_closed()) {
    // Moving the epsilon coefficients in the last column.
    dimension_type new_eps_index = mat1.num_columns() - 1;
    dimension_type old_eps_index = new_eps_index - add_dim;
    // This swap preserves sortedness of `mat1'.
    mat1.swap_columns(old_eps_index, new_eps_index);

    // Try to preserve sortedness of `mat2'.
    if (!mat2.is_sorted())
      mat2.swap_columns(old_eps_index, new_eps_index);
    else {
      for (dimension_type i = mat2.num_rows(); i-- > add_dim; ) {
	Row& r = mat2[i];
	std::swap(r[old_eps_index], r[new_eps_index]);
      }
      // The upper-right corner of `mat2' contains the J matrix:
      // swap coefficients to preserve sortedness.
      for (dimension_type i = add_dim; i-- > 0; ++old_eps_index) {
	Row& r = mat2[i];
	std::swap(r[old_eps_index], r[old_eps_index + 1]);
      }
    }
    // NOTE: since we swapped columns in both `mat1' and `mat2',
    // no swapping is required for `sat1' and `sat2'.
  }
}


void
PPL::Polyhedron::add_dimensions_and_embed(dimension_type m) {
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
    // Adds rows and/or columns to both matrices.
    // `add_dimensions' correctly handles pending constraints or generators.
    add_dimensions(con_sys, gen_sys, sat_c, sat_g, m);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: no need to modify the generators.
    con_sys.add_zero_columns(m);
    // If the polyhedron is not necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed())
      con_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
  }
  else {
    // Only generators are up-to-date: no need to modify the constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_rows_and_columns(m);
    // The polyhedron does not support pending generators.
    gen_sys.unset_pending_rows();
    // If the polyhedron is not necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed()) {
      // Try to preserve sortedness of `gen_sys'.
      if (!gen_sys.is_sorted())
	gen_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
      else {
	dimension_type old_eps_index = space_dim + 1;
	dimension_type new_eps_index = old_eps_index + m;
	for (dimension_type i = gen_sys.num_rows(); i-- > m; ) {
	  Row& r = gen_sys[i];
	  std::swap(r[old_eps_index], r[new_eps_index]);
	}
	// The upper-right corner of `gen_sys' contains the J matrix:
	// swap coefficients to preserve sortedness.
	for (dimension_type i = m; i-- > 0; ++old_eps_index) {
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
PPL::Polyhedron::add_dimensions_and_project(dimension_type m) {
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
    // In a non-necessarily closed polyhedron, all points
    // have to be accompanied by the corresponding closure points
    // (this time, dimensions are automatically adjusted).
    if (!is_necessarily_closed())
      gen_sys.insert(Generator::zero_dim_closure_point());
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
    // Adds rows and/or columns to both matrices.
    // `add_dimensions' correctly handles pending constraints or generators.
    add_dimensions(gen_sys, con_sys, sat_g, sat_c, m);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: no need to modify the generators.
    con_sys.add_rows_and_columns(m);
    // The polyhedron does not support pending constraints.
    con_sys.unset_pending_rows();
    // If the polyhedron is not necessarily closed,
    // move the epsilon coefficients to the last column.
    if (!is_necessarily_closed()) {
      // Try to preserve sortedness of `con_sys'.
      if (!con_sys.is_sorted())
	con_sys.swap_columns(space_dim + 1, space_dim + 1 + m);
      else {
	dimension_type old_eps_index = space_dim + 1;
	dimension_type new_eps_index = old_eps_index + m;
	for (dimension_type i = con_sys.num_rows(); i-- > m; ) {
	  Row& r = con_sys[i];
	  std::swap(r[old_eps_index], r[new_eps_index]);
	}
	// The upper-right corner of `con_sys' contains the J matrix:
	// swap coefficients to preserve sortedness.
	for (dimension_type i = m; i-- > 0; ++old_eps_index) {
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
    // If the polyhedron is not necessarily closed,
    // move the epsilon coefficients to the last column.
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
PPL::Polyhedron::remove_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  dimension_type max_dim_to_be_removed = to_be_removed.rbegin()->id();
  if (max_dim_to_be_removed >= space_dim)
    throw_dimension_incompatible("remove_dimensions(vs)",
				 max_dim_to_be_removed);

  dimension_type new_space_dim = space_dim - to_be_removed.size();

  // We need updated generators; note that keeping pending generators
  // is useless because constraints will be dropped anyway.
  if (is_empty()
      || (has_something_pending() && !remove_pending_to_obtain_generators())
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
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst_col = tbr->id() + 1;
  dimension_type src_col = dst_col + 1;
  dimension_type nrows = gen_sys.num_rows();
  for (++tbr; tbr != tbr_end; ++tbr) {
    dimension_type tbr_col = tbr->id() + 1;
    // All columns in between are moved to the left.
    while (src_col < tbr_col) {
      for (dimension_type r = nrows; r-- > 0; )
	std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
      ++dst_col;
      ++src_col;
    }
    ++src_col;
  }
  // Moving the remaining columns.
  dimension_type ncols = gen_sys.num_columns();
  while (src_col < ncols) {
    for (dimension_type r = nrows; r-- > 0; )
      std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
    ++src_col;
    ++dst_col;
  }
  // The number of remaining columns is `dst_col'.
  // Note that resizing also calls `set_sorted(false)'.
  gen_sys.resize_no_copy(nrows, dst_col);
  // We may have invalid line and rays now.
  gen_sys.remove_invalid_lines_and_rays();

  // Constraints are not up-to-date and generators are not minimized.
  clear_constraints_up_to_date();
  clear_generators_minimized();

  // Update the space dimension.
  space_dim = new_space_dim;

  assert(OK(true));
}

void
PPL::Polyhedron::remove_higher_dimensions(dimension_type new_dimension) {
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

  // We need updated generators; note that keeping pending generators
  // is useless because constraints will be dropped anyway.
  if (is_empty()
      || (has_something_pending() && !remove_pending_to_obtain_generators())
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

  dimension_type new_num_cols = new_dimension + 1;
  if (!is_necessarily_closed()) {
    // The polyhedron is not necessarily closed: move the column
    // of the epsilon coefficients to its new place.
    gen_sys.swap_columns(gen_sys.num_columns() - 1, new_num_cols);
    // The number of remaining columns is `new_dimension + 2'.
    ++new_num_cols;
  }
  // Note that resizing also calls `set_sorted(false)'.
  gen_sys.resize_no_copy(gen_sys.num_rows(), new_num_cols);
  // We may have invalid line and rays now.
  gen_sys.remove_invalid_lines_and_rays();

  // Constraints are not up-to-date and generators are not minimized.
  clear_constraints_up_to_date();
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
  dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_constraints_and_min(cs)", "cs", cs);

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

  // The polyhedron must be minimized and have sorted constraints.
  // `minimize()' will process any pending constraints or generators.
  if (!minimize())
    // We have just discovered that `x' is empty.
    return false;
  obtain_sorted_constraints_with_sat_c();

  // Fully sort the matrix of constraints to be added
  // (before adjusting dimensions in order to save time).
  if (cs.num_pending_rows() > 0) {
    cs.unset_pending_rows();
    cs.sort_rows();
  }
  else if (!cs.is_sorted())
    cs.sort_rows();
  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);

  bool empty = add_and_minimize(true, con_sys, gen_sys, sat_c, cs);

  if (empty)
    set_empty();
  else {
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
    throw_dimension_incompatible("add_constraint(c)", "c", c);

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
  
  // The constraints (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();

  bool adding_pending = can_have_something_pending();

  // Here we know that the system of constraints has at least a row.
  if (c.is_necessarily_closed() || !is_necessarily_closed())
    // Since `con_sys' is not empty, the topology and space dimension
    // of the inserted constraint are automatically adjusted.
    if (adding_pending)
      con_sys.insert_pending(c);
    else
      con_sys.insert(c);
  else {
    // Note: here we have a _legal_ topology mismatch, because
    // `c' is NOT a strict inequality.
    // However, by barely invoking `con_sys.insert(c)' we would
    // cause a change in the topology of `con_sys', which is wrong.
    // Thus, we insert a "topology corrected" copy of `c'.
    LinExpression nc_expr = LinExpression(c);
    if (c.is_equality())
      if (adding_pending)
	con_sys.insert_pending(nc_expr == 0);
      else
	con_sys.insert(nc_expr == 0);
    else
      if (adding_pending)
	con_sys.insert_pending(nc_expr >= 0);
      else
	con_sys.insert(nc_expr >= 0);
  }
  
  if (adding_pending)
    set_constraints_pending();
  else {
    // Constraints are not minimized and generators are not up-to-date.
    clear_constraints_minimized();
    clear_generators_up_to_date();
  } 
  // Note: the constraint system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

bool
PPL::Polyhedron::add_constraint_and_minimize(const Constraint& c) {
  // FIXME: this is just an executable specification.
  ConSys cs(c);
  return add_constraints_and_minimize(cs);
}

void
PPL::Polyhedron::add_generator(const Generator& g) {
  // Topology-compatibility check.
  if (g.is_closure_point() && is_necessarily_closed())
    throw_topology_incompatible("add_generator(g)", g);
  // Dimension-compatibility check:
  // the dimension of `g' can not be greater than space_dim.
  dimension_type g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("add_generator(g)", "g", g);

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

  if (is_empty()
      || (has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators())) {
    // Here the polyhedron is empty:
    // the specification says we can only insert a point.
    if (!g.is_point())
      throw_invalid_generator("add_generator(g)");
    if (g.is_necessarily_closed() || !is_necessarily_closed()) {
      gen_sys.insert(g);
      // Since `gen_sys' was empty, after inserting `g' we have to resize
      // the system of generators to have the right dimension.
      gen_sys.adjust_topology_and_dimension(topology(), space_dim);
      if (!is_necessarily_closed()) {
	// In the NNC topology, each point has to be matched by
	// a corresponding closure point:
	// turn the just inserted point into the corresponding
	// (normalized) closure point.
	Generator& cp = gen_sys[gen_sys.num_rows() - 1];
	cp[space_dim + 1] = 0;
	cp.normalize();
	// Re-insert the point (which is already normalized).
	gen_sys.insert(g);
      }
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
  else {
    assert(generators_are_up_to_date());
    bool has_pending = can_have_something_pending();
    if (g.is_necessarily_closed() || !is_necessarily_closed()) {
      // Since `gen_sys' is not empty, the topology and space dimension
      // of the inserted generator are automatically adjusted.
      if (has_pending)
	gen_sys.insert_pending(g);
      else
	gen_sys.insert(g);
      if (!is_necessarily_closed() && g.is_point()) {
	// In the NNC topology, each point has to be matched by
	// a corresponding closure point:
	// turn the just inserted point into the corresponding
	// (normalized) closure point.
	Generator& cp = gen_sys[gen_sys.num_rows() - 1];
	cp[space_dim + 1] = 0;
	cp.normalize();
	// Re-insert the point (which is already normalized).
	if (has_pending)
	  gen_sys.insert_pending(g);
	else
	  gen_sys.insert(g);
      }
    }
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
	if (has_pending)
	  gen_sys.insert_pending(Generator::line(nc_expr));
	else
	  gen_sys.insert(Generator::line(nc_expr));
	break;
      case Generator::RAY:
	if (has_pending)
	  gen_sys.insert_pending(Generator::ray(nc_expr));
	else
	  gen_sys.insert(Generator::ray(nc_expr));
	break;
      case Generator::POINT:
	if (has_pending)
	  gen_sys.insert_pending(Generator::point(nc_expr, g.divisor()));
	else
	  gen_sys.insert(Generator::point(nc_expr, g.divisor()));
	break;
      default:
	throw std::runtime_error("PPL::C_Polyhedron::add_generator"
				 "(const Generator& g)");
      }
    }
    
    if (has_pending)
      set_generators_pending();
    else {
      // After adding the new generator,
      // constraints are no longer up-to-date.
      clear_generators_minimized();
      clear_constraints_up_to_date();
    }
  }
  assert(OK());
}

bool
PPL::Polyhedron::add_generator_and_minimize(const Generator& g) {
  // FIXME: this is just an executable specification.
  GenSys gs(g);
  return add_generators_and_minimize(gs);
}

void
PPL::Polyhedron::add_constraints(ConSys& cs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw_topology_incompatible("add_constraints(cs)", cs);
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_constraints(cs)", "cs", cs);

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
  
  if (is_empty())
    return;

  // The constraints (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();

  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);

  bool adding_pending = can_have_something_pending();

  // Here we do not require `con_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `cs'
  // (which is not a const).
  dimension_type old_num_rows = con_sys.num_rows();
  dimension_type cs_num_rows = cs.num_rows();
  dimension_type cs_num_columns = cs.num_columns();
  con_sys.grow(old_num_rows + cs_num_rows, con_sys.num_columns());
  for (dimension_type i = cs_num_rows; i-- > 0; ) {
    // NOTE: we cannot directly swap the rows, since they might have
    // different capacities (besides possibly having different sizes):
    // thus, we steal one coefficient at a time.
    Constraint& c_new = con_sys[old_num_rows + i];
    Constraint& c_old = cs[i];
    if (c_old.is_equality())
      c_new.set_is_equality();
    for (dimension_type j = cs_num_columns; j-- > 0; )
      std::swap(c_new[j], c_old[j]);
  }

  if (adding_pending)
    set_constraints_pending();
  else {
    // The newly added ones are not pending constraints.
    con_sys.unset_pending_rows();
    // They have been simply appended.
    con_sys.set_sorted(false);
    // Constraints are not minimized and generators are not up-to-date.
    clear_constraints_minimized();
    clear_generators_up_to_date();
  }
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
  dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_generators_and_min(gs)", "gs", gs);

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

  // For NNC polyhedra, each point must be matched by
  // the corresponding closure point.
  if (!is_necessarily_closed())
    gs.add_corresponding_closure_points();
  
  // `gs' has to be fully sorted, thus it cannot have pending rows.
  if (gs.num_pending_rows() > 0) {
    gs.unset_pending_rows();
    gs.sort_rows();
  }
  else if (!gs.is_sorted())
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
  dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_generators(gs)", "gs", gs);

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
  // For NNC polyhedra, each point must be matched by
  // the corresponding closure point.
  if (!is_necessarily_closed())
    gs.add_corresponding_closure_points();

  // The generators (possibly with pending rows) are required.
  if ((has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !minimize())) {
    // We have just discovered that `*this' is empty.
    // So `gs' must contain at least one point.
    if (!gs.has_points())
      throw_invalid_generators("add_generators(gs)");
    // The polyhedron is no longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    gen_sys.unset_pending_rows();
    set_generators_up_to_date();
    clear_empty();
    assert(OK());
    return;
  }
  
  bool adding_pending = can_have_something_pending();

  // Here we do not require `gen_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `gs'
  // (which is not a const).
  dimension_type old_num_rows = gen_sys.num_rows();
  dimension_type gs_num_rows = gs.num_rows();
  dimension_type gs_num_columns = gs.num_columns();
  gen_sys.grow(old_num_rows + gs_num_rows, gen_sys.num_columns());
  for (dimension_type i = gs_num_rows; i-- > 0; ) {
    // NOTE: we cannot directly swap the rows, since they might have
    // different capacities (besides possibly having different sizes):
    // thus, we steal one coefficient at a time.
    Generator& g_new = gen_sys[old_num_rows + i];
    Generator& g_old = gs[i];
    if (g_old.is_line())
      g_new.set_is_line();
    for (dimension_type j = gs_num_columns; j-- > 0; )
      std::swap(g_new[j], g_old[j]);
  }

  if (adding_pending)
    set_generators_pending();
  else {
    // The newly added ones are not pending generators.
    gen_sys.unset_pending_rows();
    // They have been simply appended.
    gen_sys.set_sorted(false);
    // Constraints are not up-to-date and generators are not minimized.
    clear_constraints_up_to_date();
    clear_generators_minimized();
  }
  assert(OK(true));
}

void
PPL::Polyhedron::ascii_dump(std::ostream& s) const {
  using std::endl;

  s << "space_dim "
    << space_dimension()
    << endl;
  status.ascii_dump(s);
  s << endl
    << "con_sys ("
    << (constraints_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  con_sys.ascii_dump(s);
  s << endl
    << "gen_sys ("
    << (generators_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  gen_sys.ascii_dump(s);
  s << endl
    << "sat_c"
    << endl;
  sat_c.ascii_dump(s);
  s << endl
    << "sat_g"
    << endl;
  sat_g.ascii_dump(s);
  s << endl;
}

bool
PPL::Polyhedron::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> space_dim))
    return false;

  if (!status.ascii_load(s))
    return false;

  if (!(s >> str) || str != "con_sys")
    return false;

  if (!(s >> str) || (str != "(not_up-to-date)" && str != "(up-to-date)"))
    return false;

  if (!con_sys.ascii_load(s))
    return false;

  if (!(s >> str) || str != "gen_sys")
    return false;

  if (!(s >> str) || (str != "(not_up-to-date)" && str != "(up-to-date)"))
    return false;

  if (!gen_sys.ascii_load(s))
    return false;

  if (!(s >> str) || str != "sat_c")
    return false;

  if (!sat_c.ascii_load(s))
    return false;

  if (!(s >> str) || str != "sat_g")
    return false;

  if (!sat_g.ascii_load(s))
    return false;

  // Check for well-formedness.
  assert(OK());
  return true;
}

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
  dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  dimension_type num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());
  
  if (is_empty())
    return;
  
  if (num_var <= expr_space_dim && expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (generators_are_up_to_date()) {
      // GenSys::affine_image() requires the third argument
      // to be a positive Integer.
      if (denominator > 0)
	gen_sys.affine_image(num_var, expr, denominator);
      else
	gen_sys.affine_image(num_var, -expr, -denominator);
    }
    if (constraints_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse;
      if (expr[num_var] > 0) {
	inverse = -expr;
	inverse[num_var] = denominator;
	con_sys.affine_preimage(num_var, inverse, expr[num_var]);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as ConSys::affine_preimage()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[num_var] = denominator;
	negate(inverse[num_var]);
	con_sys.affine_preimage(num_var, inverse, -expr[num_var]);
      }
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of generators.
    if (has_something_pending())
      remove_pending_to_obtain_generators();
    else if (!generators_are_up_to_date())
      minimize();
    if (!is_empty()) {
      // GenSys::affine_image() requires the third argument
      // to be a positive Integer.
      if (denominator > 0)
	gen_sys.affine_image(num_var, expr, denominator);
      else
	gen_sys.affine_image(num_var, -expr, -denominator);
      
      clear_constraints_up_to_date();
      clear_generators_minimized();
      clear_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
  }
  assert(OK());
}


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
  dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  dimension_type num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  if (is_empty())
    return;

  if (num_var <= expr_space_dim && expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (constraints_are_up_to_date()) {
      // ConSys::affine_preimage() requires the third argument
      // to be a positive Integer.
      if (denominator > 0)
	con_sys.affine_preimage(num_var, expr, denominator);
      else
	con_sys.affine_preimage(num_var, -expr, -denominator);
    }
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse;
      if (expr[num_var] > 0) {
	inverse = -expr;
	inverse[num_var] = denominator;
	gen_sys.affine_image(num_var, inverse, expr[num_var]);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as GenSys::affine_image()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[num_var] = denominator;
	negate(inverse[num_var]);
	gen_sys.affine_image(num_var, inverse, -expr[num_var]);
      }
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of constraints.
    if (has_something_pending())
      remove_pending_to_obtain_constraints();
    else if (!constraints_are_up_to_date())
      minimize();
    // ConSys::affine_preimage() requires the third argument
    // to be a positive Integer.
    if (denominator > 0)
      con_sys.affine_preimage(num_var, expr, denominator);
    else
      con_sys.affine_preimage(num_var, -expr, -denominator);

    clear_generators_up_to_date();
    clear_constraints_minimized();
    clear_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
  assert(OK());
}

void
PPL::Polyhedron::generalized_affine_image(const Variable& var,
					  const Relation_Operator relop,
					  const LinExpression& expr,
					  const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_image(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  dimension_type num_var = var.id() + 1;
  if (num_var > space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 var.id());

  // Strict relation operators are only admitted for NNC polyhedra.
  if (is_necessarily_closed() && (relop == PPL_LT || relop == PPL_GT))
    throw_generic("generalized_affine_image(v, r, e, d)",
		  "r is a strict relation operator and "
		  "*this is a C_Polyhedron");

  // Any image of an empty polyhedron is empty.
  if (is_empty())
    return;

  // First compute the affine image.
  affine_image(var, expr, denominator);
  switch (relop) {
  case PPL_LE:
    add_generator(ray(-var));
    break;
  case PPL_EQ:
    // The relation operator is "==":
    // this is just an affine image computation.
    break;
  case PPL_GE:
    add_generator(ray(var));
    break;
  case PPL_LT:
  // Intentionally fall through.
  case PPL_GT:
    {
      // The relation operator is strict.
      assert(!is_necessarily_closed());
      // While adding the ray, we minimize the generators
      // in order to avoid adding too many redundant generators later.
      GenSys gs;
      gs.insert(ray(relop == PPL_GT ? var : -var));
      add_generators_and_minimize(gs);
      // We split each point of the generator system into two generators:
      // a closure point, having the same coordinates of the given point,
      // and another point, having the same coordinates for all but the
      // `var' dimension, which is displaced along the direction of the
      // newly introduced ray.
      dimension_type eps_index = space_dimension() + 1;
      for (dimension_type i =  gen_sys.num_rows(); i-- > 0; )
	if (gen_sys[i].is_point()) {
	  Generator& g = gen_sys[i];
	  // Add a `var'-displaced copy of `g' to the generator system.
	  gen_sys.add_row(g);
	  if (relop == PPL_GT)
	    gen_sys[gen_sys.num_rows()-1][num_var]++;
	  else
	    gen_sys[gen_sys.num_rows()-1][num_var]--;
	  // Transform `g' into a closure point.
	  g[eps_index] = 0;
	}
      clear_constraints_up_to_date();
      clear_generators_minimized();
      gen_sys.set_sorted(false);
      clear_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
  }
  assert(OK());
}

void
PPL::Polyhedron::generalized_affine_image(const LinExpression& lhs,
					  const Relation_Operator relop,
					  const LinExpression& rhs) {
  // Dimension-compatibility checks.
  // The dimension of `lhs' should not be greater than the dimension
  // of `*this'.
  dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e1", lhs);
  // The dimension of `rhs' should not be greater than the dimension
  // of `*this'.
  dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  // Strict relation operators are only admitted for NNC polyhedra.
  if (is_necessarily_closed() && (relop == PPL_LT || relop == PPL_GT))
    throw_generic("generalized_affine_image(e1, r, e2)",
		  "r is a strict relation operator and "
		  "*this is a C_Polyhedron");

  // Any image of an empty polyhedron is empty.
  if (is_empty())
    return;

  // Compute the actual space dimension of `lhs',
  // i.e., the highest dimension having a non-zero coefficient in `lhs'.
  for ( ; lhs_space_dim > 0; lhs_space_dim--)
    if (lhs.coefficient(Variable(lhs_space_dim - 1)) != 0)
      break;
  // If all variables have a zero coefficient, then `lhs' is a constant:
  // we can simply add the constraint `lhs relop rhs'.
  if (lhs_space_dim == 0) {
    switch (relop) {
    case PPL_LT:
      add_constraint(lhs < rhs);
      break;
    case PPL_LE:
      add_constraint(lhs <= rhs);
      break;
    case PPL_EQ:
      add_constraint(lhs == rhs);
      break;
    case PPL_GE:
      add_constraint(lhs >= rhs);
      break;
    case PPL_GT:
      add_constraint(lhs > rhs);
      break;
    }
    return;
  }

  // Gather in `new_gs' the collections of all the lines having
  // the direction of variables occurring in `lhs'.
  // While at it, check whether or not there exists a variable
  // occurring in both `lhs' and `rhs'.
  GenSys new_lines;
  bool lhs_vars_intersects_rhs_vars = false;
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      new_lines.insert(line(Variable(i)));
      if (rhs.coefficient(Variable(i)) != 0)
	lhs_vars_intersects_rhs_vars = true;
    }

  if (lhs_vars_intersects_rhs_vars) {
    // Some variables in `lhs' also occur in `rhs'.
    // To ease the computation, we add and additional dimension.
    Variable new_var = Variable(space_dimension());
    add_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to the right hand side.
    // (we force minimization because we will need the generators).
    ConSys new_cs;
    new_cs.insert(new_var == rhs);
    add_constraints_and_minimize(new_cs);
    
    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the constraints).
    add_generators_and_minimize(new_lines);
    
    // Constrain the new dimension so that it is related to
    // the left hand side as dictated by `relop'
    // (we force minimization because we will need the generators).
    new_cs.clear();
    switch (relop) {
    case PPL_LT:
      new_cs.insert(lhs < new_var);
      break;
    case PPL_LE:
      new_cs.insert(lhs <= new_var);
      break;
    case PPL_EQ:
      new_cs.insert(lhs == new_var);
      break;
    case PPL_GE:
      new_cs.insert(lhs >= new_var);
      break;
    case PPL_GT:
      new_cs.insert(lhs > new_var);
      break;
    }
    add_constraints_and_minimize(new_cs);

    // Remove the temporarily added dimension.
    remove_higher_dimensions(space_dimension()-1);
  }
  else {
    // `lhs' and `rhs' variables are disjoint:
    // there is no need to add a further dimension.

    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the constraints).
    add_generators_and_minimize(new_lines);

    // Constrain the left hand side expression so that it is related to
    // the right hand side expression as dictated by `relop'.
    switch (relop) {
    case PPL_LT:
      add_constraint(lhs < rhs);
      break;
    case PPL_LE:
      add_constraint(lhs <= rhs);
      break;
    case PPL_EQ:
      add_constraint(lhs == rhs);
      break;
    case PPL_GE:
      add_constraint(lhs >= rhs);
      break;
    case PPL_GT:
      add_constraint(lhs > rhs);
      break;
    }
  }

  assert(OK());
}


PPL::Poly_Con_Relation
PPL::Polyhedron::relation_with(const Constraint& c) const {
  // Dimension-compatibility check.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("relation_with(c)", "c", c);

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
  
  if ((has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators()))
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
    throw_dimension_incompatible("relation_with(g)", "g", g);

  // The empty polyhedron cannot subsume a generator.
  if (is_empty())
    return Poly_Gen_Relation::nothing();

  // A universe polyhedron in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();
  
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();
  
  return
    con_sys.satisfies_all_constraints(g)
    ? Poly_Gen_Relation::subsumes()
    : Poly_Gen_Relation::nothing();
}


void
PPL::Polyhedron::select_H79_constraints(const Polyhedron& y,
					ConSys& cs_selection) const {
  // Private method: the caller must ensure the following conditions.
  assert(topology() == y.topology()
	 && topology() == cs_selection.topology()
	 && space_dimension() == y.space_dimension());
  assert(!is_empty() && constraints_are_up_to_date());
  assert(!y.is_empty()
	 && y.constraints_are_minimized()
	 && y.generators_are_minimized());
  assert(!has_something_pending() && !y.has_something_pending());
  
  // Add low-level constraints.
  add_low_level_constraints(cs_selection);
  // Now adjust dimensions, if needed.
  cs_selection.adjust_topology_and_dimension(topology(), space_dimension());

  // Obtain a sorted copy of `y.sat_g'.
  if (!y.sat_g_is_up_to_date())
    y.update_sat_g();
  SatMatrix tmp_sat_g = y.sat_g;
  tmp_sat_g.sort_rows();
  // The size of `buffer' will reach sat.num_columns() bit.
  SatRow buffer;

  // A constraint in `con_sys' is placed in `cs_selection'
  // if its behavior with respect to `y.gen_sys' is the same
  // as that of another constraint in `y.con_sys'.
  // Namely, we check whether the saturation row `buffer'
  // (built starting from the given constraint and `y.gen_sys')
  // is a row of the saturation matrix `tmp_sat_g'.
  // Note: if the considered constraint of `con_sys' does not
  // satisfy the saturation rule (see Section \ref prelims), then
  // it will not appear in the resulting constraint system,
  // because `tmp_sat_g' is built starting from a minimized polyhedron.

  // Note: the loop index `i' goes upwards to avoid reversing
  // the ordering of the chosen constraints.
  for (dimension_type i = 0, iend = con_sys.num_rows(); i < iend; ++i) {
    buffer.clear();
    // The saturation row `buffer' is built considering
    // the `i'-th constraint of the polyhedron `x' and
    // all the generators of the polyhedron `y'.
    for (dimension_type j = y.gen_sys.num_rows(); j-- > 0; ) {
      int sp_sgn = sgn(y.gen_sys[j] * con_sys[i]);
      // We are assuming that `y <= x'.
      assert(sp_sgn >= 0);
      if (sp_sgn > 0)
	buffer.set(j);
    }
    // We check whether `buffer' is a row of `tmp_sat_g',
    // exploiting its sortedness in order to have faster comparisons.
    if (tmp_sat_g.sorted_contains(buffer))
      cs_selection.add_row(con_sys[i]);
  }
}


void
PPL::Polyhedron::H79_widening_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("H79_widening_assign(y)", y);
  // Dimension-compatibility check.
  dimension_type x_space_dim = x.space_dim;
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
  if (has_something_pending())
    remove_pending_to_obtain_constraints();
  else if (!x.constraints_are_up_to_date())
    x.update_constraints();

  // Copy into `H79_con_sys' the constraints that are common
  // to `x' and `y', according to the definition of the H79 widening.
  ConSys H79_con_sys(x.topology());
  x.select_H79_constraints(y, H79_con_sys);

  // Let `H79_con_sys' be the constraint system of `x'
  // and update the status of `x'.
  std::swap(x.con_sys, H79_con_sys);
  x.set_constraints_up_to_date();
  x.clear_constraints_minimized();
  x.clear_generators_up_to_date();

  assert(x.OK(true));
}

void
PPL::Polyhedron::limited_H79_widening_assign(const Polyhedron& y,
					     ConSys& cs) {
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
  dimension_type x_space_dim = x.space_dim;
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("limited_H79_widening_assign(y, cs)", y);
  // `cs' must be dimension-compatible with the two polyhedra.
  dimension_type cs_space_dim = cs.space_dimension();
  if (x_space_dim < cs_space_dim)
    throw_dimension_incompatible("limited_H79_widening_assign(y, cs)", "cs", cs);

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
  if ((x.has_something_pending() && !x.remove_pending_to_obtain_generators())
      || (!x.generators_are_up_to_date() && !x.update_generators()))
    // We have just discovered that `x' is empty.
    return;

  dimension_type new_cs_num_rows = 0;
  for (dimension_type
	 i = 0, cs_num_rows = cs.num_rows(); i < cs_num_rows; ++i) {
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
  // NOTE: here `cs' has no pending constraints.
  cs.erase_to_end(new_cs_num_rows);
  cs.unset_pending_rows();

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

bool
PPL::Polyhedron::is_BHRZ03_stabilizing(const Polyhedron& x,
				       const Polyhedron& y) {
  assert(x.space_dimension() == y.space_dimension());
  assert(x.constraints_are_minimized());
  assert(y.constraints_are_minimized());
  assert(x.generators_are_minimized());
  assert(y.generators_are_minimized());
  assert(!x.has_something_pending());
  assert(!y.has_something_pending());

  // If the dimension of `x' is greater than the dimension of `y',
  // the chain is stabilizing.
  // Since the constraint systems are minimized, the dimension of
  // the polyhedra is obtained by subtracting the number of
  // equalities from the space dimension.
  dimension_type x_dimension =
    x.space_dimension() - x.con_sys.num_equalities();
  dimension_type y_dimension =
    y.space_dimension() - y.con_sys.num_equalities();
  if (x_dimension > y_dimension) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03_stabilizing: number of dimensions" << std::endl;
#endif
    return true;
  }

  // Since `y' is assumed to be included into `x',
  // at this point the two polyhedra must have the same dimension.
  assert(x_dimension == y_dimension);

  // If the dimension of the lineality space of `x' is greater than
  // the dimension of the lineality space of `y', then the chain
  // is stabilizing. Since both generator systems are minimized,
  // the dimension of the lineality space is equal to the number of lines.
  dimension_type x_num_lines = x.gen_sys.num_lines();
  dimension_type y_num_lines = y.gen_sys.num_lines();
  if (x_num_lines > y_num_lines) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03_stabilizing: lineality space" << std::endl;
#endif
    return true;
  }

  // Since `y' is assumed to be included into `x', at this point
  // the lineality space of the two polyhedra must have the same dimension.
  assert (x_num_lines == y_num_lines);


#if 1
  // Just for debugging.
  bool stabilizing = true;
#endif

#if NEW_LIMITED_GROWTH_ORDERING
  // If the number of constraints of `x' is smaller than the number
  // of constraints of `y', then the chain is stabilizing. If it is
  // bigger, the chain is not stabilizing. If they are equal, further
  // investigation is needed.
  dimension_type x_con_sys_num_rows = x.con_sys.num_rows();
  dimension_type y_con_sys_num_rows = y.con_sys.num_rows();
  if (x_con_sys_num_rows < y_con_sys_num_rows)
    return true;
  if (x_con_sys_num_rows > y_con_sys_num_rows)
#if 1
    stabilizing = false;
#else
    return false;
  assert(x_con_sys_num_rows == y_con_sys_num_rows);
#endif
#endif // #if NEW_LIMITED_GROWTH_ORDERING

  dimension_type x_gen_sys_num_rows = x.gen_sys.num_rows();
  dimension_type y_gen_sys_num_rows = y.gen_sys.num_rows();
  if (x.is_necessarily_closed()) {
    // If the number of points of `x' is smaller than the number
    // of points of `y', then the chain is stabilizing.
    dimension_type x_num_points = x_gen_sys_num_rows
      - x_num_lines - x.gen_sys.num_rays();
    dimension_type y_num_points = y_gen_sys_num_rows
      - y_num_lines - y.gen_sys.num_rays();
    if (x_num_points < y_num_points) {
#if 0 //#ifndef NDEBUG
      std::cout << "BHRZ03_stabilizing: number of points" << std::endl;
#endif
#if 1
      if (!stabilizing)
	std::cerr << "*";
      return stabilizing;
#endif
      return true;
    }
    else
      // If the number of points of `y' is smaller than the number of
      // points of `x', then the chain is not stabilizing.
      if (x_num_points > y_num_points)
	return false;
  }
  else {
    // The polyhedra are NNC.
    dimension_type x_num_closure_points = 0;
    for (dimension_type i = x_gen_sys_num_rows; i-- > 0; )
      if (x.gen_sys[i].is_closure_point())
	++x_num_closure_points;
    dimension_type y_num_closure_points = 0;
    for (dimension_type i = y_gen_sys_num_rows; i-- > 0; )
      if (y.gen_sys[i].is_closure_point())
	++y_num_closure_points;
    // If the number of closure points of `x' is smaller than
    // the number of closure points of `y', the chain is stabilizing.
    if (x_num_closure_points < y_num_closure_points) {
#if 0 //#ifndef NDEBUG
      std::cout << "BHRZ03_stabilizing: number of closure points"
		<< std::endl;
#endif
#if 1
      if (!stabilizing)
	std::cerr << "*";
      return stabilizing;
#endif
      return true;
    }
    else
      // If the number of closure points of `y' is smaller than the
      // number of closure points of `x', the chain is not stabilizing.
      if (x_num_closure_points > y_num_closure_points)
	return false;
  }

  
  // For each i such that 0 <= i < x.space_dim, let x_num_rays[i] be
  // the number of rays in x.gen_sys
  // having exactly `i' coordinates equal to 0.
  std::vector<dimension_type> x_num_rays(x.space_dimension());
  for (dimension_type i = x.space_dimension(); i-- > 0; )
    x_num_rays[i] = 0;
  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; )
    if (x.gen_sys[i].is_ray()) {
      const Generator& r = x.gen_sys[i];
      dimension_type num_zeroes = 0;
      for (dimension_type j = x.space_dimension(); j >= 1; j--)
	if (r[j] == 0)
	  num_zeroes++;
      x_num_rays[num_zeroes]++;
    }
  // The same as above, this time for `y'.
  std::vector<dimension_type> y_num_rays(y.space_dimension());
  for (dimension_type i = y.space_dimension(); i-- > 0; )
    y_num_rays[i] = 0;
  for (dimension_type i = y_gen_sys_num_rows; i-- > 0; )
    if (y.gen_sys[i].is_ray()) {
      const Generator& r = y.gen_sys[i];
      dimension_type num_zeroes = 0;
      for (dimension_type j = y.space_dimension(); j >= 1; j--)
	if (r[j] == 0)
	  num_zeroes++;
      y_num_rays[num_zeroes]++;
    }
  // Compare (lexicographically) the two vectors:
  // if x_num_rays < y_num_rays the chain is stabilizing.
  for (dimension_type i = 0; i < x.space_dimension(); i++) {
    if (x_num_rays[i] > y_num_rays[i])
      // Not stabilizing.
      break;
    if (x_num_rays[i] < y_num_rays[i]) {
#if 0 //#ifndef NDEBUG
      std::cout << "BHRZ03_stabilizing: zero-coord rays" << std::endl;
#endif
#if 1
      if (!stabilizing)
	std::cerr << "*";
      return stabilizing;
#endif
      return true;
    }
  }

  // Hey, wait a minute! Are they equal?
  if (x == y) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03_stabilizing: same polyhedra" << std::endl;
#endif
#if 1
    if (!stabilizing)
      std::cerr << "*";
    return stabilizing;
#endif
    return true;
  }

  // The chain is not stabilizing.
  return false;
}

void
PPL::Polyhedron::BHRZ03_widening_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("BHRZ03_widening_assign(y)", y);
  // Dimension-compatibility check.
  dimension_type x_space_dim = x.space_dim;
  if (x_space_dim != y.space_dim)
    throw_dimension_incompatible("BHRZ03_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    Polyhedron x_copy = x;
    Polyhedron y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  // If any argument is zero-dimensional or empty,
  // the BHRZ03-widening behaves as the identity function.
  if (x_space_dim == 0 || x.is_empty() || y.is_empty())
    return;

  // `x.con_sys' and `x.gen_sys' should be in minimal form.
  x.minimize();

  // `y.con_sys' and `y.gen_sys' should be in minimal form.
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
  
  // If the iteration is stabilizing, the resulting polyhedron is `x'.
  if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03: immediately stabilizing" << std::endl;
#endif
    assert(OK());
    return;
  }

  // Copy into `H79_con_sys' the constraints that are common
  // to `x' and `y', according to the definition of the H79 widening.
  ConSys H79_con_sys(x.topology());
  x.select_H79_constraints(y, H79_con_sys);
  // CHECK ME: why should it be sorted?
  H79_con_sys.sort_rows();

  // The following heuristics are intrusive: to avoid problems,
  // we backup the current value of `x'.
  Polyhedron x_backup = x;

  // ****************
  // First technique.
  // ****************

  // We must choose the constraints of `x' that do not belong to
  // the system of constraints of `y'.
  // To choose this constraints we use `x.sat_g'
  if (!x.sat_g_is_up_to_date())
    x.update_sat_g();
  dimension_type H79_con_sys_num_rows = H79_con_sys.num_rows();
  dimension_type x_gen_sys_num_rows = x.gen_sys.num_rows();
  // We built a temporary saturation matrix that contains the
  // relations between the constraints of `H79_con_sys' and
  // the generators of `x'.
  SatMatrix common_sat_g(H79_con_sys_num_rows, x_gen_sys_num_rows);
  for (dimension_type i = H79_con_sys_num_rows; i-- > 0; ) {
    const Constraint& c = H79_con_sys[i];
    for (dimension_type j = x_gen_sys_num_rows; j-- > 0; ) {
      Generator& g = x.gen_sys[j];
      if (sgn(c * g) != 0)
	common_sat_g[i].set(j);
    }
  }
  common_sat_g.sort_rows();

  // The system of constraints `x_con_sys_minus_y_con_sys' contains
  // the constraints of `x' that do not belong to `y'.
  ConSys x_con_sys_minus_y_con_sys;
  dimension_type x_con_sys_num_rows = x.con_sys.num_rows();
  for (dimension_type i = x_con_sys_num_rows; i-- > 0; )
    if (!x.con_sys[i].is_equality())
      if (!common_sat_g.sorted_contains(x.sat_g[i]))
	x_con_sys_minus_y_con_sys.insert(x.con_sys[i]);

  // The system of constraints of the resulting polyhedron
  // contains the constraints of `H79_con_sys'.
  ConSys new_con_sys = H79_con_sys;
  // We must choose a point (if the polyhedra are necessarily closed)
  // or a closure point (if the polyhedra are not necessarily closed)
  // that belong to `x' and `y'.  In the case of not necessarily
  // closed polyhedra, we can consider only the closure points,
  // because the role of the points can be played by the closure
  // points.
  dimension_type y_gen_sys_num_rows = y.gen_sys.num_rows();
  for (dimension_type i = y_gen_sys_num_rows; i-- > 0; ) {
    const Generator& g = y.gen_sys[i];
    if ((g.is_point() && x.is_necessarily_closed())
	|| (g.is_closure_point() && !x.is_necessarily_closed())) {
      // We choose a constraint of `x' that saturates the point `g'
      // and that belongs to `x_con_sys_minus_y_con_sys' and we put
      // these constraints in a temporary system of constraints.
      ConSys tmp_con_sys(x.topology(), 0, x.con_sys.num_columns());
      for (dimension_type j = x_con_sys_minus_y_con_sys.num_rows();
	   j-- > 0; ) {
	Constraint& c = x_con_sys_minus_y_con_sys[j];
	if (c * g == 0)
	  tmp_con_sys.insert(c);
      }
      // We build the new constraint that is
      // obtained adding all the chosen normalized constraint.
      if (tmp_con_sys.num_rows() != 0) {
	if (tmp_con_sys.num_rows() == 1)
	  // If we have chosen only a constraint, we add it to the
	  // new system.
	  new_con_sys.insert(tmp_con_sys[0]);
	else {
	  // The number of the chosen constraints is greather than 1.
	  dimension_type tmp_con_sys_num_rows = tmp_con_sys.num_rows();
	  // We compute the norms of the vectors composed by the
	  // homogeneous terms of the chosen constraints and 
	  // we put it into the vector `norms'.
	  // NOTE: Actually, the coefficients of `norms' are the
	  // truncated integer part of the square roots of the norms
	  // of the vectors.
	  std::vector<Integer> norms(tmp_con_sys_num_rows);
	  for (dimension_type h = tmp_con_sys_num_rows; h-- > 0; ) {
	    Constraint& tmp_c = tmp_con_sys[h];
	    for (dimension_type k = tmp_con_sys.num_columns(); k-- > 1; )
	      norms[h] += tmp_c[k] * tmp_c[k];
	    sqrt_assign(norms[h]);
	  }
	  
	  // In `lcm_norm' we put the least common multiple of the
	  // coefficients of the vector `norms'.
	  Integer lcm_norm = norms[0];
	  for (dimension_type h = 0; h < tmp_con_sys_num_rows; ++h)
	    lcm_assign(lcm_norm, norms[h]);
	  
	  // The new constraints is equal to `e op b', where `op' is
	  // the symbol of strict inequality if in the system
	  // `tmp_con_sys' there is a strict inequality or otherwise
	  // the symbol of not strict inequality; `e' is equal to the
	  // sum of all vectors that are obtained from the constraints
	  // of `tmp_con_sys' erasing the non-homogeneous term and
	  // modifying them so that they have the same length; `b' is
	  // equal to `e * g'.
	  // NOTE: The real thing that we do is an approximation of
	  // what we have just written. We are still working on this
	  // problem.
	  LinExpression e(0);
	  bool strict_inequality = false;
	  for (dimension_type h = tmp_con_sys_num_rows; h-- > 0; ) {
	    LinExpression tmp(tmp_con_sys[h]);
	    tmp -= tmp[0];
	    for (dimension_type t = tmp.size(); t-- > 1; )
	      tmp[t] = tmp[t] * lcm_norm / norms[h];
	    e += tmp;
	    if (tmp_con_sys[h].is_strict_inequality())
	      strict_inequality = true;
	  }
	  Integer tmp = 0;
	  for (size_t t = e.size(); t-- > 1; )
	    tmp+= e[t] * g[t];
	  e -= tmp;
	  e.normalize();
	  
	  // If there is a strict inequality in the chosen
	  // constraints, the new constraint is a strict inequality,
	  // too. Otherwise it is a non-strict inequality.
	  if (!e.all_homogeneous_terms_are_zero())
	    if (strict_inequality)
	      new_con_sys.insert(e > 0);
	    else
	      new_con_sys.insert(e >= 0);
	  
	}
      }
    }
  }
  std::swap(new_con_sys, x.con_sys);
  // The resulting polyhedron has only
  // the system of constraints up to date.
  x.clear_generators_up_to_date();
  x.clear_constraints_minimized();

  // Check for stabilization.
  x.minimize();
  if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03: stabilizing on 1st technique" << std::endl;
#endif
    assert(OK(true));
    return;
  }
  
  // *****************
  // Second technique.
  // *****************

  // The first tecnique did not succeeded, possibly modifying `x'.
  // Thus, we recover the backup copy of `x'.
  x = x_backup;
  
  // For each point in `x.gen_sys' that is not included in `y',
  // this technique identifies a set of rays that subsume this point
  // and do not violate the constraints in `H79_con_sys'.
  // All such rays are kept in `valid_rays'.
  GenSys valid_rays;

  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; ) {
    Generator& g1 = x.gen_sys[i];
    // For C polyhedra, we choose a point of `x.gen_sys'
    // that is not included in `y'.
    // In the case of NNC polyhedra, we can restrict attention to
    // closure points (considering also points will only add redundancy).
    if ((g1.is_point() && x.is_necessarily_closed())
	|| (g1.is_closure_point() && !x.is_necessarily_closed())) {
      Poly_Gen_Relation relation = y.relation_with(g1);
      if (relation == Poly_Gen_Relation::nothing()) {
	// Candidate rays are kept in `new_rays'.
	GenSys new_rays;
	// For each point (resp., closure point) `g2' in `y.gen_sys',
	// where `g1' and `g2' are different,
	// we built the ray `g1 - g2' and put it into `new_rays'.
	for (dimension_type j = y_gen_sys_num_rows; j-- > 0; ) {
	  const Generator& g2 = y.gen_sys[j];
	  if ((g2.is_point() && y.is_necessarily_closed())
	      || (g2.is_closure_point() && !y.is_necessarily_closed())) {
	    // Check that `g1' and `g2' are different.
	    if (compare(g1, g2) == 0)
	      continue;
	    Generator ray_from_g2_to_g1 = g1;
	    ray_from_g2_to_g1.linear_combine(g2, 0);
	    new_rays.insert(ray_from_g2_to_g1);
	  }
	}
	// Similarly, for each point (resp., closure point) `g2'
	// in `x.gen_sys', where `g1' and `g2' are different,
	// we built the ray `g1 - g2' and put it into `new_rays'.
	for (dimension_type j = x_gen_sys_num_rows; j-- > 0; ) {
	  // Check that `g1' and `g2' are different:
	  // since they both belong to `x.gen_sys', which is minimized,
	  // they are equal if and only if their indexes are the same.
	  if (i == j)
	    continue;
	  const Generator& g2 = x.gen_sys[j];
	  if ((g2.is_point() && x.is_necessarily_closed())
	      || (g2.is_closure_point() && !x.is_necessarily_closed())) {
	    Generator ray_from_g2_to_g1(g1);
	    ray_from_g2_to_g1.linear_combine(g2, 0);
	    new_rays.insert(ray_from_g2_to_g1);
	  }
	}
	if (new_rays.num_rows() == 1) {
	  // `new_rays' contains one ray only: it is a valid ray
	  // if it satisfies all of the constraints in `H79_con_sys'.
	  const Generator& new_ray = new_rays[0];
	  bool is_valid_ray = true;
	  for (dimension_type j = H79_con_sys.num_rows(); j-- > 0; )
	    if (new_ray * H79_con_sys[j] < 0) {
	      is_valid_ray = false;
	      break;
	    }
	  if (is_valid_ray)
	    valid_rays.insert(new_ray);
	}
	else
	  if (new_rays.num_rows() > 1) {
	    // `new_rays' contains more than one candidate ray.
	    // After adding a point of `x' to `new_rays',
	    // we build the corresponding polyhedron
	    // (a polyhedral cone having the point as apex).
	    // We compute the intersection of this polyhedron
	    // with the polyhedron generated by `H79_con_sys':
	    // the valid rays are those belonging to this intersection.
	    dimension_type k = x_gen_sys_num_rows - 1;
	    while (!x.gen_sys[k].is_point())
	      --k;
	    // Insert the point.
	    new_rays.insert(x.gen_sys[k]);
	    Polyhedron ph(x.topology(), new_rays);
	    // Have to take a copy, because `H79_con_sys'
	    // may be needed later.
	    ConSys H79_con_sys_copy = H79_con_sys;
	    ph.add_constraints_and_minimize(H79_con_sys_copy);
	    const GenSys& ph_gs = ph.generators();
	    // Copy the rays of `ph' into `valid_rays'.
	    for (dimension_type j = ph_gs.num_rows(); j-- > 0; ) {
	      const Generator& g = ph_gs[j];
	      if (g.is_ray())
		valid_rays.insert(g);
	    }
	  }
      }
    }
  }

  // We "average" the directions of all the rays that belong to
  // `valid_rays' and then we add the new ray to the system of
  // generators of `x'.
  // NOTE: it is useless copy the polyhedron `x' in a temporary
  // polyhedron, because the ray that is obtained averaging the
  // directions of the rays of `valid_rays' is redundant in the system
  // `valid_rays'.
  LinExpression e(0);
  for (dimension_type i = valid_rays.num_rows(); i-- > 0; )
    e += LinExpression(valid_rays[i]);
  e.normalize();
  if (!e.all_homogeneous_terms_are_zero()) {
    GenSys avg_ray;
    avg_ray.insert(ray(e));
    x.add_generators_and_minimize(avg_ray);
  
    // Check for stabilization.
    if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
      std::cout << "BHRZ03: stabilizing on the first case of 2nd technique"
		<< std::endl;
#endif
      assert(OK(true));
      return;
    }
  }

  // At this point there is not stabilization adding only a ray
  // or the ray that we have obtained has all homogeneous terms
  // equal to zero, we add all the valid rays to `x' and
  // check for stabilization (which requires minimization).
  x.add_generators_and_minimize(valid_rays);

  // Check for stabilization.
  if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03: stabilizing on the second case of 2nd technique"
	      << std::endl;
#endif
    assert(OK(true));
    return;
  }

  // ****************
  // Third technique.
  // ****************

  // The second tecnique did not succeeded, possibly modifying `x'.
  // Thus, we recover the backup copy of `x'.
  x = x_backup;

  if (!x.sat_c_is_up_to_date())
    x.sat_c.transpose_assign(x.sat_g);

  // We build a temporary saturation matrix in which we put the relations
  // between the constraints of `x' and the generators of `y'.
  SatMatrix tmp_sat(y_gen_sys_num_rows, x_con_sys_num_rows);
  for (dimension_type i = y_gen_sys_num_rows; i-- > 0; )
    for (dimension_type j = x_con_sys_num_rows; j-- > 0; )
      if (x.con_sys[j] * y.gen_sys[i] > 0)
        tmp_sat[i].set(j);

  // We built a temporary system of generators in which we put
  // the new rays.
  GenSys modified_rays;
  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; ) {
    const Generator& x_g = x.gen_sys[i];
    // We choose a ray of `x' that does not belong to `y' and
    // "evolved" since a ray of `y'.
    if (x_g.is_ray() && y.relation_with(x_g) == Poly_Gen_Relation::nothing()) {
      for (dimension_type j = y_gen_sys_num_rows; j-- > 0; ) {
	const Generator& y_g = y.gen_sys[j];
	if (y_g.is_ray() && strict_subset(x.sat_c[i], tmp_sat[j])) {
	  Generator new_ray(x_g);
	  std::deque<bool> considered(x.space_dim + 1);
	  Integer tmp_1;
	  Integer tmp_2;
	  // We modify the ray `new_ray' according to how `x_g'
	  // evolve since the ray of `y'.
	  for (dimension_type k = 1; k < x.space_dim; ++k)
	    if (!considered[k])
	      for (dimension_type h = k + 1; h <= x.space_dim; ++h)
		if (!considered[h]) {
		  tmp_1 = x_g[k] * y_g[h];
		  tmp_2 = x_g[h] * y_g[k];
		  int ratio_sign = sgn(x_g[k]) * sgn(x_g[h]);
		  if (tmp_1 != tmp_2)
		    if ((tmp_1 >= 0 && tmp_2 > tmp_1)
			|| (tmp_2 < tmp_1 && tmp_1 <= 0)
			|| (tmp_2 > 0 && tmp_1 < 0 && ratio_sign > 0)
			|| (tmp_1 < 0 && tmp_2 > 0 && ratio_sign > 0)
			|| (tmp_2 < 0 && tmp_1 > 0 && ratio_sign < 0)) {
		      new_ray[k] = 0;
		      considered[k] = true;
		    }
		    else {
		      new_ray[h] = 0;
		      considered[h] = true;
		    }
		}
	  new_ray.normalize();
	  // In `modified_rays', we put the new ray `new_ray' and `y_g'.
	  modified_rays.insert(y_g);
	  modified_rays.insert(new_ray);
	}
      }
    }
  }
  GenSys valid_modified_rays;
  // If `modified_rays' has rows, we know that it has more than
  // one row.
  if (modified_rays.num_rows() != 0) {
    // `modified_rays' contains more than one candidate ray.
    // After adding a point of `x' to `modified_rays',
    // we build the corresponding polyhedron
    // (a polyhedral cone having the point as apex).
    // We compute the intersection of this polyhedron
    // with the polyhedron generated by `H79_con_sys':
    // the valid rays are those belonging to this intersection.
    dimension_type k = x_gen_sys_num_rows - 1;
    while (!x.gen_sys[k].is_point())
      --k;
    // Insert the point.
    modified_rays.insert(x.gen_sys[k]);
    Polyhedron ph(x.topology(), modified_rays);
    // Have to take a copy, because `H79_con_sys'
    // may be needed later.
    ConSys H79_con_sys_copy = H79_con_sys;
    ph.add_constraints_and_minimize(H79_con_sys_copy);
    const GenSys& ph_gs = ph.generators();
    // Copy the rays of `ph' into `valid_modified_rays'.
    for (dimension_type j = ph_gs.num_rows(); j-- > 0; ) {
      const Generator& g = ph_gs[j];
      if (g.is_ray() || g.is_line())
	valid_modified_rays.insert(g);
    }
  }

  // We add the new system of generators `valid_modified_rays'
  // to the polyhedron `x'.
  x.add_generators_and_minimize(valid_modified_rays);
  
  // Check for stabilization.
  if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03: stabilizing on 3rd technique" << std::endl;
#endif
    assert(OK(true));
    return;
  }

  // ****************
  // Fourth technique.
  // ****************
  
  // Try applying the H79 widening.
  Polyhedron ph(x.topology(), H79_con_sys);
  std::swap(x, ph);
  // Check for stabilization.
  x.minimize();
  if (is_BHRZ03_stabilizing(x, y)) {
#if 0 //#ifndef NDEBUG
    std::cout << "BHRZ03: stabilizing on H79 widening" << std::endl;
#endif
    assert(OK(true));
    return;
  }

#if 0 //ifndef NDEBUG
  std::cout << "BHRZ03: NOT stabilizing!" << std::endl;
#endif
#if NEW_LIMITED_GROWTH_ORDERING
  std::cout << "BHRZ03 is not a widening!" << std::endl;
  assert(false);
  abort;
#else
  // FIXME: here we should abort the computation, because we have
  // found a chain that is not stabilizing under the BHRZ03 widening.
  // Since we are still developing and debugging this operator,
  // for the moment we simply return the input polyhedron `x'.
  x = x_backup;
  assert(OK(true));
#endif //#if NEW_LIMITED_GROWTH_ORDERING
}

void
PPL::Polyhedron::limited_BHRZ03_widening_assign(const Polyhedron& y,
						ConSys& cs) {
  BHRZ03_widening_assign(y);
  add_constraints(cs);
}

void
PPL::Polyhedron::time_elapse_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("time_elapse_assign(y)", y);
  dimension_type x_space_dim = x.space_dim;
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
      || (x.has_pending_constraints() && !x.process_pending_constraints())
      || (!x.generators_are_up_to_date() && !x.update_generators())
      || (y.has_pending_constraints() && !y.process_pending_constraints())
      || (!y.generators_are_up_to_date() && !y.update_generators())) {
    x.set_empty();
    return;
  }
      
  // At this point both generator systems are up-to-date,
  // possibly containing pending generators.
  GenSys gs = y.gen_sys;
  dimension_type gs_num_rows = gs.num_rows();

  if (!x.is_necessarily_closed())
    // `x' and `y' are NNC polyhedra.
    for (dimension_type i = gs_num_rows; i-- > 0; )
      switch (gs[i].type()) {
      case Generator::POINT:
	// The points of `gs' can be erased,
	// since their role can be played by closure points.
	--gs_num_rows;
	std::swap(gs[i], gs[gs_num_rows]);
	break;
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
  else
    // `x' and `y' are C polyhedra.
    for (dimension_type i = gs_num_rows; i-- > 0; )
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
  // For NNC polyhedra, also erase all the points of `gs',
  // whose role can be payed by the closure points.
  // These have been previously moved to the end of `gs'.
  gs.erase_to_end(gs_num_rows);
  gs.unset_pending_rows();
  
  // `gs' may now have no rows.
  // Namely, this happens when `y' was the singleton polyhedron
  // having the origin as the one and only point.
  // In such a case, the resulting polyhedron is equal to `x'.
  if (gs_num_rows == 0)
    return;

  // If the polyhedron can have something pending, we add `gs'
  // to `gen_sys' as pending rows
  if (x.can_have_something_pending()) {
    x.gen_sys.add_pending_rows(gs);
    x.set_generators_pending();
  }
  // Otherwise, the two systems are merged.
  // `Matrix::merge_row_assign()' requires both matrices to be ordered.
  else {
    if (!x.gen_sys.is_sorted())
      x.gen_sys.sort_rows();
    gs.sort_rows();
    x.gen_sys.merge_rows_assign(gs);
    // Only the system of generators is up-to-date.
    x.clear_constraints_up_to_date();
    x.clear_generators_minimized();
  }
  assert(x.OK(true) && y.OK(true));
}


bool
PPL::Polyhedron::check_universe() const {
  if (space_dim == 0)
    return !is_empty();

  // We need the polyhedron in minimal form.
  if (has_something_pending())
    process_pending();
  else if (!constraints_are_minimized())
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
    else {
#ifndef NDEBUG
      // If the system of constraints contains two rows that
      // are not equalities, we are sure that they are
      // epsilon constraints: in this case we know that
      // the polyhedron is universe.
      obtain_sorted_constraints();
      const Constraint& eps_leq_one = con_sys[0]; 
      const Constraint& eps_geq_zero = con_sys[1]; 
      dimension_type eps_index = con_sys.num_columns() - 1;
      assert(eps_leq_one[0] > 0 && eps_leq_one[eps_index] < 0
	     && eps_geq_zero[0] == 0 && eps_geq_zero[eps_index] > 0);
      for (dimension_type i = 1; i < eps_index; ++i)
	assert(eps_leq_one[i] == 0 && eps_geq_zero[i] == 0);
#endif
      return true;
    }
  }
}

bool
PPL::Polyhedron::is_bounded() const {
  // A zero-dimensional or empty polyhedron is bounded.
  if (space_dim == 0
      || is_empty()
      || (has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators()))
    return true;
  
   for (dimension_type i = gen_sys.num_rows(); i-- > 0; )
    if (gen_sys[i][0] == 0)
      // A line or a ray is found: the polyhedron is not bounded.
      return false;
  
  // The system of generators is composed only by
  // points and closure points: the polyhedron is bounded.
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


bool
PPL::Polyhedron::is_topologically_closed() const {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return true;
  // Any empty or zero-dimensional polyhedron is closed.
  if (is_empty()
      || space_dimension() == 0
      || (has_something_pending() && !process_pending()))
     return true;

  // At this point there are no pending constraints or generators.
  assert(!has_something_pending());

  if (generators_are_minimized()) {
    // A polyhedron is closed iff all of its (non-redundant)
    // closure points are matched by a corresponding point.
    dimension_type n_rows = gen_sys.num_rows();
    dimension_type n_lines = gen_sys.num_lines();
    for (dimension_type i = n_rows; i-- > n_lines; ) {
      const Generator& gi = gen_sys[i];
      if (gi.is_closure_point()) {
	bool gi_has_no_matching_point = true;
	for (dimension_type j = n_rows; j-- > n_lines; ) {
	  const Generator& gj = gen_sys[j];
	  if (i != j
	      && gj.is_point()
	      && gi.is_matching_closure_point(gj)) {
	    gi_has_no_matching_point = false;
	    break;
	  }
	}
	if (gi_has_no_matching_point)
	  return false;
      }
    }
    // All closure points are matched.
    return true;
  }

  // A polyhedron is closed if, after strong minimization
  // of its constraint system, it has no strict inequalities.
  strongly_minimize_constraints();
  return is_empty() || !con_sys.has_strict_inequalities();
}


void
PPL::Polyhedron::topological_closure_assign() {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return;
  // Any empty or zero-dimensional polyhedron is closed.
  if (is_empty() || space_dimension() == 0)
    return;

  // The computation can be done using constraints or generators.
  // If we use constraints, we will change them, so that having pending
  // constraints would be useless. If we use generators, we add generators,
  // so that having pending generators still makes sense.

  // Process any pending constraints.
  if (has_pending_constraints() && !process_pending_constraints())
    return;

  // Use constraints only if they are available and
  // there are no pending genreators.
  if (!has_pending_generators() && constraints_are_up_to_date()) {
    dimension_type eps_index = space_dim + 1;
    bool changed = false;
    // Transform all strict inequalities into non-strict ones.
    for (dimension_type i = con_sys.num_rows(); i-- > 0; ) {
      Constraint& c = con_sys[i];
      if (c[eps_index] < 0 && !c.is_trivial_true()) {
	c[eps_index] = 0;
	// Enforce normalization.
	c.normalize();
	changed = true;
      }
    }
    if (changed) {
      con_sys.insert(Constraint::epsilon_leq_one());
      con_sys.set_sorted(false);
      // After changing the system of constraints, the generators
      // are no longer up-to-date and the constraints are no longer
      // minimized.
      clear_generators_up_to_date();
      clear_constraints_minimized();
    }
  }
  else {
    // Here we use generators, possibly keeping constraints.
    assert(generators_are_up_to_date());
    // Add the corresponding point to each closure point.
    gen_sys.add_corresponding_points();
    if (can_have_something_pending())
      set_generators_pending();
    else {
      // We cannot have pending generators; this also implies
      // that generators may have lost their sortedness.
      gen_sys.unset_pending_rows();
      gen_sys.set_sorted(false);
      // Constraints are not up-to-date and generators are not minimized.
      clear_constraints_up_to_date();
      clear_generators_minimized();
    }
  }
  assert(OK());
}


bool
PPL::Polyhedron::OK(bool check_not_empty) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // The expected number of columns in the constraint and generator
  // systems, if they are not empty. 
  dimension_type poly_num_columns
    = space_dim + (is_necessarily_closed() ? 1 : 2); 

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
    // either has no rows or only contains an unsatisfiable constraint
    // and if it has no pending constraints or generators.
    if (has_something_pending()) {
#ifndef NDEBUG
      cerr << "The polyhedron is empty, "
	   << "but it has something pending" << endl;
#endif
      goto bomb;
    }
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
      if (!con_sys[0].is_trivial_false()) {
#ifndef NDEBUG
	cerr << "Empty polyhedron with a satisfiable system of constraints"
	     << endl;
#endif
	goto bomb;
      }
      // Here we have only one, trivially false constraint.
      return true;
    }
  }

  // A zero-dimensional, non-empty polyhedron is legal only if the
  // system of constraint `con_sys' and the system of generators
  // `gen_sys' have no rows.
  if (space_dim == 0) {
    if (has_something_pending()) {
#ifndef NDEBUG
      cerr << "Zero-dimensional polyhedron with something pending"
	   << endl;
#endif
      goto bomb;
    }
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
  }

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
      if (con_sys.first_pending_row() != sat_c.num_columns()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (con_sys and sat_c)"
	     << endl;
#endif
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (con_sys.first_pending_row() != sat_g.num_rows()) {
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
      if (gen_sys.first_pending_row() != sat_c.num_rows()) {
#ifndef NDEBUG
	cerr << "Incompatible size! (gen_sys and sat_c)"
	     << endl;
#endif
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (gen_sys.first_pending_row() != sat_g.num_columns()) {
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

    if (gen_sys.first_pending_row() == 0) {
#ifndef NDEBUG
      cerr << "Up-to-date generator system with all rows pending!"
	   << endl;
#endif
      goto bomb;
    }

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

#if 0
    //=================================================
    // TODO: this test is wrong in the general case.
    // However, such an invariant does hold for a
    // strongly-minimized GenSys.
    // We will activate this test as soon as the Status
    // flags will be able to remember if a system is
    // strongly minimized.

    // Checking that the number of closure points is always
    // grater than the number of points.
    if (!is_necessarily_closed()) {
      dimension_type num_points = 0;
      dimension_type num_closure_points = 0;
      dimension_type eps_index = gen_sys.num_columns() - 1;
      for (dimension_type i = gen_sys.num_rows(); i-- > 0; )
	if (gen_sys[i][0] != 0)
	  if (gen_sys[i][eps_index] > 0)
	    ++num_points;
	  else
	    ++num_closure_points;
      if (num_points > num_closure_points) {
#ifndef NDEBUG
	cerr << "# POINTS > # CLOSURE_POINTS" << endl;
#endif
	goto bomb;
      }
    }
    //=================================================
#endif

    if (generators_are_minimized()) {
      // If the system of generators is minimized, the number of lines,
      // rays and points of the polyhedron must be the same
      // of the temporary minimized one. If it does not happen
      // the polyhedron is not ok.
      ConSys new_con_sys(topology());
      GenSys gs_without_pending = gen_sys;
      // NOTE: We can avoid to update `index_first_pending'
      // of `gs_without_pending', because it is equal to the
      // new number of rows of `gs_without_pending'.
      gs_without_pending.erase_to_end(gen_sys.first_pending_row());
      GenSys copy_of_gen_sys = gs_without_pending;
      SatMatrix new_sat_c;
      minimize(false, copy_of_gen_sys, new_con_sys, new_sat_c);
      dimension_type copy_num_lines = copy_of_gen_sys.num_lines();
      if (gs_without_pending.num_rows() != copy_of_gen_sys.num_rows()
	  || gs_without_pending.num_lines() != copy_num_lines
	  || gs_without_pending.num_rays() != copy_of_gen_sys.num_rays()) {
#ifndef NDEBUG
	cerr << "Generators are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the generators:"
	     << endl;
	copy_of_gen_sys.ascii_dump(cerr);
	cerr << endl;
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
	gs_without_pending.strong_normalize();
	gs_without_pending.sort_rows();
	if (copy_of_gen_sys != gs_without_pending) {
#ifndef NDEBUG
	  cerr << "Generators are declared minimized, but they are not!"
	       << endl
	       << "(we are in the case:"
	       << endl
	       << "dimension of lineality space equal to 0)"
	       << endl
	       << "Here is the minimized form of the generators:"
	       << endl;
	  copy_of_gen_sys.ascii_dump(cerr);
	  cerr << endl;
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

    if (con_sys.first_pending_row() == 0) {
#ifndef NDEBUG
      cerr << "Up-to-date constraint system with all rows pending!"
	   << endl;
#endif
      goto bomb;
    }

    // A non-empty system of constraints describing a polyhedron
    // must contain a constraint with a non-zero inhomogeneous term;
    // such a constraint corresponds to (a combination of other
    // constraints with):
    // -* the positivity constraint, for necessarily closed polyhedra;
    // -* the epsilon <= 1 constraint, for NNC polyhedra.
    bool no_positivity_constraint = true;
    for (dimension_type i = con_sys.num_rows(); i-- > 0; )
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

    if (!is_necessarily_closed()) {
      // A non-empty system of constraints describing a NNC polyhedron
      // must also contain a (combination of) the constraint epsilon >= 0,
      // i.e., a constraint with a positive epsilon coefficient.
      bool no_epsilon_geq_zero = true;
      dimension_type eps_index = con_sys.num_columns() - 1;
      for (dimension_type i = con_sys.num_rows(); i-- > 0; )
	if (con_sys[i][eps_index] > 0) {
	  no_epsilon_geq_zero = false;
	  break;
	}
      if (no_epsilon_geq_zero) {
#ifndef NDEBUG
	cerr << "Non-empty constraint system for NNC polyhedron "
	     << "has no epsilon >= 0 constraint"
	     << endl;
#endif
	goto bomb;
      }
    }

    ConSys cs_without_pending = con_sys;
    // NOTE: We can avoid to update `index_first_pending'
    // of `cs_without_pending', because it is equal to the
    // new number of rows of `cs_without_pending'.
    cs_without_pending.erase_to_end(con_sys.first_pending_row());
    ConSys copy_of_con_sys = cs_without_pending;
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
      if (cs_without_pending.num_rows() != copy_of_con_sys.num_rows()
	  || cs_without_pending.num_equalities()
	  != copy_of_con_sys.num_equalities()) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl;
	copy_of_con_sys.ascii_dump(cerr);
	cerr << endl;
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
      cs_without_pending.sort_rows();
      cs_without_pending.back_substitute(cs_without_pending.gauss());
      cs_without_pending.strong_normalize();
      cs_without_pending.sort_rows();
      if (cs_without_pending != copy_of_con_sys) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl;
	copy_of_con_sys.ascii_dump(cerr);
	cerr << endl;
#endif
	goto bomb;
      }
    }
  }

  if (sat_c_is_up_to_date())
    for (dimension_type i = sat_c.num_rows(); i-- > 0; ) {
      Generator tmp_gen = gen_sys[i];
      SatRow tmp_sat = sat_c[i];
      for (dimension_type j = sat_c.num_columns(); j-- > 0; )
	if (sgn(tmp_gen * con_sys[j]) != tmp_sat[j]) {
#ifndef NDEBUG
	  cerr << "sat_c is declared up-to-date, but it is not!"
	       << endl;
#endif
	  goto bomb;
	}
    }

  if (sat_g_is_up_to_date())
    for (dimension_type i = sat_g.num_rows(); i-- > 0; ) {
      Constraint tmp_con = con_sys[i];
      SatRow tmp_sat = sat_g[i];
      for (dimension_type j = sat_g.num_columns(); j-- > 0; )
	if (sgn(tmp_con * gen_sys[j]) != tmp_sat[j]) {
#ifndef NDEBUG
	  cerr << "sat_g is declared up-to-date, but it is not!"
	       << endl;
#endif
	  goto bomb;
	}
    }
  
  if (has_pending_constraints()) {
    if (con_sys.num_pending_rows() == 0) {
#ifndef NDEBUG
      cerr << "polyhedron is deecleared to have pending constraints, "
	   << "but it has no pending rows!"
	   << endl;
#endif
      goto bomb;
    }
  }
  
  if (has_pending_generators()) {
    if (gen_sys.num_pending_rows() == 0) {
#ifndef NDEBUG
      cerr << "polyhedron is deecleared to have pending generators, "
	   << "but it has no pending rows!"
	   << endl;
#endif
      goto bomb;
    }
  }
  
  return true;
  
 bomb:
#ifndef NDEBUG
  cerr << "Here is the guilty polyhedron:"
       << endl;
  ascii_dump(cerr);
#endif
  return false;
}

/*! \relates Parma_Polyhedra_Library::Polyhedron */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Polyhedron& ph) {
  if (ph.check_empty())
    s << "false";
  else
    s << ph.minimized_constraints();
  return s;
}
