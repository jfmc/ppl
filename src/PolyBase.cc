/* PolyBase class implementation (non-inline functions).
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

#include "PolyBase.defs.hh"

#include "Generator.defs.hh"
#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

#define BE_LAZY

namespace PPL = Parma_Polyhedra_Library;

/*!
  Updates the constraints as necessary, then returns a constant
  reference to the system of constraints.
*/
const PPL::ConSys&
PPL::PolyBase::constraints() const {
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
    // Zero-dim universe.
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    return con_sys;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // We insist in returning a sorted system of constraints.
  if (!con_sys.is_sorted()) {
    if (sat_c_is_up_to_date()) {
      const_cast<PolyBase&>(*this).obtain_sorted_constraints_with_sat_c();
      if (sat_g_is_up_to_date())
	const_cast<SatMatrix&>(sat_g).transpose_assign(sat_c);
    }
    else {
      const_cast<ConSys&>(con_sys).sort_rows();
      if (sat_g_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<PolyBase&>(*this).clear_sat_g_up_to_date();
#endif
	const_cast<PolyBase&>(*this).update_sat_g();
      }
    }
  }
  return con_sys;
}


/*!
  Updates the generators as necessary, then returns a constant
  reference to the system of generators.
*/
const PPL::GenSys&
PPL::PolyBase::generators() const {

  if (is_empty()) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return gen_sys;
  }

  if (space_dim == 0) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    // CHECK ME: do we need to provide a zero_dim_univ polyhedron
    // of the same topology ?
    return GenSys::zero_dim_univ();
  }

  if (!generators_are_up_to_date())
    update_generators();

  // We insist in returning a sorted system of generators.
  if (!gen_sys.is_sorted()) {
    if (sat_g_is_up_to_date()) {
      const_cast<PolyBase&>(*this).obtain_sorted_generators_with_sat_g();
      if (sat_c_is_up_to_date())
	const_cast<SatMatrix&>(sat_c).transpose_assign(sat_g);
    }
    else {
      const_cast<GenSys&>(gen_sys).sort_rows();
      if (sat_c_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<PolyBase&>(*this).clear_sat_c_up_to_date();
#endif
	const_cast<PolyBase&>(*this).update_sat_c();
      }
    }
  }
  return gen_sys;
}


PPL::PolyBase::PolyBase(Topology topology,
			size_t num_dimensions,
			Degenerate_Kind kind)
  : con_sys(topology),
    gen_sys(topology),
    sat_c(),
    sat_g() {
  if (kind == EMPTY)
    status.set_empty();
  else
    if (num_dimensions > 0) {
      if (topology == NECESSARILY_CLOSED)
	// The only constraint is the positivity one.
	con_sys.insert(Constraint::zero_dim_positivity());
      else {
	// Polyhedron NON-necessarily closed: the only constraints
	// are the ones regarding the \epsilon dimension.
	con_sys.insert(Constraint::epsilon_geq_zero());
	con_sys.insert(Constraint::epsilon_leq_one());
      }
      con_sys.adjust_topology_and_dimension(topology, num_dimensions);
      // In both cases (positivity or epsilon constraints)
      // the constraint system is in the minimal form.
      set_constraints_minimized();
    }
  space_dim = num_dimensions;
}


PPL::PolyBase::PolyBase(const PolyBase& y)
  : con_sys(y.topology()),
    gen_sys(y.topology()),
    status(y.status),
    space_dim(y.space_dim) {
  if (y.constraints_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
      sat_g = y.sat_g;
}

/*!
  Builds a polyhedron satisfying the given system of constraints \p cs.
*/
PPL::PolyBase::PolyBase(Topology topology, ConSys& cs)
  : con_sys(topology),
    gen_sys(topology),
    sat_c(),
    sat_g() {
  size_t cs_space_dim = cs.space_dimension();
  if (cs.num_rows() > 0 && cs_space_dim > 0) {
    // Try to adapt `cs' to the required topology.
    // The possible topology change _and_ the following swap operation
    // will modify the given argument `cs';
    // that is why the formal parameter is not declared const.
    if (!cs.adjust_topology_and_dimension(topology, cs_space_dim))
      throw std::invalid_argument("PPL::Polyhedron::Polyhedron(cs): "
				  "cs contains strict inequalities");
    std::swap(con_sys, cs);
    if (topology == NECESSARILY_CLOSED)
      // Add the positivity constraint.
      con_sys.insert(Constraint::zero_dim_positivity());
    else {
      // Add the \epsilon constraints.
      con_sys.insert(Constraint::epsilon_geq_zero());
      con_sys.insert(Constraint::epsilon_leq_one());
    }
    set_constraints_up_to_date();
    // Set the space dimension.
    space_dim = cs_space_dim;
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


/*!
  Builds a polyhedron generated by the given system of generators \p gs.
*/
PPL::PolyBase::PolyBase(Topology topology, GenSys& gs)
  : con_sys(topology),
    gen_sys(topology),
    sat_c(),
    sat_g() {

  size_t gs_space_dim = gs.space_dimension();
  if (gs.num_rows() > 0 && gs_space_dim > 0) {
    // Valid generator systems need a supporting point, at least.
    if (!gs.has_points()) {
      std::ostringstream s;
      std::ostringstream prefix;
      if (topology == NECESSARILY_CLOSED)
	prefix << "NNC_";
      s << "PPL::"
	<< prefix.str() << "Polyhedron::"
	<< prefix.str() << "Polyhedron(gs): gs contains no points";
      throw std::invalid_argument(s.str());
    }
    // Try to adapt `gs' to the required topology.
    // The possible topology change _and_ the following swap operation
    // will modify the given argument `gs';
    // that is why the formal parameter is not declared const.
    if (!gs.adjust_topology_and_dimension(topology, gs_space_dim))
      throw std::invalid_argument("PPL::Polyhedron::Polyhedron(gs): "
				  "gs contains closure points");
    // Stealing the rows from `gs'.
    std::swap(gen_sys, gs);

    if (topology == NON_NECESSARILY_CLOSED) {
      // In a generator system describing a NNC polyhedron,
      // for each point we must also have the corresponding closure point.
      size_t n_rows = gen_sys.num_rows();
      size_t eps_index = gs_space_dim + 1;
      for (size_t i = n_rows; i-- > 0; ) {
	const Generator& g = gen_sys[i];
	if (g[eps_index] != 0) {
	  // `g' is a point: adding the closure point.
	  gen_sys.add_row(g);
	  gen_sys[n_rows][eps_index] = 0;
	  ++n_rows;
	}
      }
    }
    set_generators_up_to_date();
    // Set the space dimension.
    space_dim = gs_space_dim;
    return;
  }

  // Here `gs.num_rows == 0' or `gs_space_dim == 0',
  // so that we have a zero-dim space polyhedron.
  space_dim = 0;
  if (gs.num_rows() == 0)
    status.set_empty();
  else
    // Valid generator systems need a supporting point, at least.
    if (!gs.has_points()) {
      std::ostringstream s;
      std::ostringstream prefix;
      if (topology == NECESSARILY_CLOSED)
	prefix << "NNC_";
      s << "PPL::"
	<< prefix.str() << "Polyhedron::"
	<< prefix.str() << "Polyhedron(gs): gs contains no points";
      throw std::invalid_argument(s.str());
    }
}


PPL::PolyBase&
PPL::PolyBase::operator=(const PolyBase& y) {
  assert(topology() == y.topology());
  space_dim = y.space_dim;
  status = y.status;
  if (y.constraints_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
    sat_g = y.sat_g;

  return *this;
}


//! Destructor.
PPL::PolyBase::~PolyBase() {
}


/*!
  Clears the polyhedron since it is empty.
*/
void
PPL::PolyBase::set_empty() {
  status.set_empty();
  // The polyhedron is empty: we can thus throw away everything.
  con_sys.clear();
  gen_sys.clear();
  sat_c.clear();
  sat_g.clear();
}

/*!
  A universe zero-dimensional polyhedron is in a 0-dimensional space and
  must have both `con_sys' and `gen_sys' with no rows.
*/
void
PPL::PolyBase::set_zero_dim_univ() {
  status.set_zero_dim_univ();
  space_dim = 0;
  con_sys.clear();
  gen_sys.clear();
}

/*!
  Updates constraints starting from the system of generators
  and minimizes them. The resulting system of constraints will not
  be sorted: we only know that the equalities are in the upper
  part of the matrix and the inequalities in the lower one.
*/
void
PPL::PolyBase::update_constraints() const {
  assert(space_dim > 0);
  assert(!is_empty());
  assert(generators_are_up_to_date());

  PolyBase& x = const_cast<PolyBase&>(*this);
  minimize(false, x.gen_sys, x.con_sys, x.sat_c);
  // `sat_c' is the only saturation matrix up-to-date.
  x.set_sat_c_up_to_date();
  x.clear_sat_g_up_to_date();
  // The system of constraints and the system of generators
  // are minimized.
  x.set_constraints_minimized();
  x.set_generators_minimized();
}

/*!
  Updates generators starting from the system of constraints
  and minimizes them. The resulting system of generators will not
  be sorted: we only know that the lines are in the upper part of
  the matrix and the rays and the points are in the lower one.
*/
bool
PPL::PolyBase::update_generators() const {
  assert(space_dim > 0);
  assert(!is_empty());
  assert(constraints_are_up_to_date());

  PolyBase& x = const_cast<PolyBase&>(*this);
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


/*!
  If required, updates generators from constraints or vice versa
  and minimizes them.
*/
void
PPL::PolyBase::minimize() const {

  // 0-dim space or empty polyhedra are already minimized.
  if (space_dim == 0
      || is_empty()
      || (constraints_are_minimized() && generators_are_minimized()))
    return;
  // If constraints or generators are up-to-date, invoking
  // update_generators() or update_constraints(), respectively,
  // minimizes both constraints and generators.
  // If both are up-to-date it does not matter whether we use
  // update_generators() or update_constraints():
  // both minimize constraints and generators.
  else if (constraints_are_up_to_date())
    update_generators();
  else {
    assert(generators_are_up_to_date());
    update_constraints();
  }

  assert(OK());
}

/*!
  Sorts the matrix of constraints keeping \p sat_g consistent;
  if \p sat_g is not up-to-date it will be obtained from \p sat_c
  and then sorted together with \p con_sys.
*/
void
PPL::PolyBase::obtain_sorted_constraints() {
  assert(constraints_are_up_to_date());

  if (!con_sys.is_sorted())
    if (sat_g_is_up_to_date()) {
      // Sorting constraints keeping `sat_g' consistent.
      con_sys.sort_and_remove_with_sat(sat_g);
      // `sat_c' is not up-to-date anymore.
      clear_sat_c_up_to_date();
    }
    else if (sat_c_is_up_to_date()) {
      // Using `sat_c' to obtain `sat_g', then it is like previous case.
      sat_g.transpose_assign(sat_c);
      con_sys.sort_and_remove_with_sat(sat_g);
      set_sat_g_up_to_date();
      clear_sat_c_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
      // the constraints.
      con_sys.sort_rows();
  assert(con_sys.check_sorted());
}

/*!
  Sorts the matrix of generators keeping \p sat_c consistent;
  if \p sat_c is not up-to-date it will be obtained from \p sat_g
  and then sorted together with \p gen_sys.
*/
void
PPL::PolyBase::obtain_sorted_generators() {
  assert(generators_are_up_to_date());
  if (!gen_sys.is_sorted())
    if (sat_c_is_up_to_date()) {
      // Sorting generators keeping 'sat_c' consistent.
      gen_sys.sort_and_remove_with_sat(sat_c);
      // `sat_g' is not up-to-date anymore.
      clear_sat_g_up_to_date();
    }
    else if (sat_g_is_up_to_date()) {
      // Obtaining `sat_c' from `sat_g' and proceeding like previous case.
      sat_c.transpose_assign(sat_g);
      gen_sys.sort_and_remove_with_sat(sat_c);
      set_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
      // the generators.
      gen_sys.sort_rows();
  assert(gen_sys.check_sorted());
}


/*!
  Sorts the matrix of constraints keeping \p sat_g consistent and then
  obtains \p sat_c from \p sat_g.
*/
void
PPL::PolyBase::obtain_sorted_constraints_with_sat_c() {
  assert(constraints_are_up_to_date());

  assert(constraints_are_minimized());

  // At least one of the saturation matrices must be up-to-date.
  if (!sat_c_is_up_to_date() && !sat_g_is_up_to_date())
    update_sat_c();

  if (con_sys.is_sorted()) {
    if (sat_c_is_up_to_date())
      // If constraints are already sorted and sat_c is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!sat_g_is_up_to_date()) {
      // If constraints are not sorted and sat_g is not up-to-date
      // we obtain sat_g from sat_c (that has to be up-to-date)...
      sat_g.transpose_assign(sat_c);
      set_sat_g_up_to_date();
    }
    // ...and sort it together with constraints.
    con_sys.sort_and_remove_with_sat(sat_g);
  }
  // Obtaining sat_c from sat_g.
  sat_c.transpose_assign(sat_g);
  set_sat_c_up_to_date();
  // Constraints are sorted now.
  con_sys.set_sorted(true);
  assert(con_sys.check_sorted());
}


/*!
  Sorts the matrix of generators keeping \p sat_c consistent and then
  obtains \p sat_g from \p sat_c.
*/
void
PPL::PolyBase::obtain_sorted_generators_with_sat_g() {
  assert(generators_are_up_to_date());

  // At least one of the saturation matrices must be up-to-date.
  if (!sat_c_is_up_to_date() && !sat_g_is_up_to_date())
    update_sat_g();

  if (gen_sys.is_sorted()) {
    if (sat_g_is_up_to_date())
      // If generators are already sorted and sat_g is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!sat_c_is_up_to_date()) {
      // If generators are not sorted and sat_c is not up-to-date
      // we obtain sat_c from sat_g (that has to be up-to-date) ...
      sat_c.transpose_assign(sat_g);
      set_sat_c_up_to_date();
    }
    // ...and sort it together with generators.
    gen_sys.sort_and_remove_with_sat(sat_c);
  }
  // Obtaining sat_g from sat_c.
  sat_g.transpose_assign(sat_c);
  set_sat_g_up_to_date();
  // Generators are sorted now.
  gen_sys.set_sorted(true);
  assert(gen_sys.check_sorted());
}

/*!
  Computes \p sat_c starting from the systems of generators
  and constraints and assuming that both of them are up-to-date:
  \f[
    \begin{cases}
    sat\_c[i][j] = 0, \quad \text{if } G[i] \cdot C^\mathrm{T}[j] = 0; \\
    sat\_c[i][j] = 1, \quad \text{if } G[i] \cdot C^\mathrm{T}[j] > 0.
    \end{cases}
  \f]
*/
void
PPL::PolyBase::update_sat_c() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_c_is_up_to_date());

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  PolyBase& x = const_cast<PolyBase&>(*this);

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

/*!
  Computes \p sat_g starting from the systems of generators
  and constraints and assuming both of them are up-to-date:
  \f[
  \begin{cases}
  sat\_g[i][j] = 0, \quad \text{if } C[i] \cdot G^\mathrm{T}[j] = 0; \\
  sat\_g[i][j] = 1, \quad \text{if } C[i] \cdot G^\mathrm{T}[j] > 0.
  \end{cases}
  \f]
*/
void
PPL::PolyBase::update_sat_g() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_g_is_up_to_date());

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  PolyBase& x = const_cast<PolyBase&>(*this);

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

static void
throw_different_dimensions(const char* method,
			   const PPL::PolyBase& x,
			   const PPL::PolyBase& y) {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "x->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.space_dimension();
  throw std::invalid_argument(s.str());
}

/*!
  Returns <CODE>true</CODE> if and only if
  \p x is contained in \p y.
*/
bool
PPL::operator<=(const PolyBase& x, const PolyBase& y) {
  assert(x.topology() == y.topology());
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::operator<=(x, y)", x, y);
  if (x.is_empty())
    return true;
  else if (y.is_empty())
    return x.check_empty();
  else if (x_space_dim == 0)
    return true;
#ifdef BE_LAZY
  if (!x.generators_are_up_to_date())
    x.update_generators();
  if (!y.constraints_are_up_to_date())
    y.update_constraints();
#else
  if (!x.generators_are_minimized())
    x.minimize();
  if (!y.constraints_are_minimized())
    y.minimize();
#endif


  if (!x.is_necessarily_closed()) {
    // FIXME.
    // For non-necessarily closed polyhedra it is not that easy!
    // We have to check that:
    //  - closure(x) <= closure(y); and
    //  - all _points_ of `x.gen_sys', when projected onto
    //    the hyper-plane \epsilon == 0, are included in `y'.
    throw std::invalid_argument("PPL::operator<=(x, y) not (yet) "
				"implemented for NNC_Polyhedron");
  }

  // We have that `x' is contained in `y' if and only if all the
  // generators of `x' satisfy or saturate all the inequalities and
  // saturate all the equalities of `y'.  This comes from the
  // definition of a polyhedron as the set of vectors that satisfy a
  // given system of constraints and the fact that all vectors in `x'
  // can be obtained as a combination of its generators.
  for (size_t i = x.gen_sys.num_rows(); i-- > 0; ) {
    const Generator& gx = x.gen_sys[i];
    for (size_t j = y.con_sys.num_rows(); j-- > 0; ) {
      const Constraint& cy = y.con_sys[j];
      int sgn_gx_scalar_cy = sgn(gx * cy);
      if (cy.is_inequality()) {
	if (sgn_gx_scalar_cy < 0)
	  return false;
      }
      else if (sgn_gx_scalar_cy != 0)
	return false;
    }
  }
  return true;
}


/*!
  The intersection of \p *this with \p y (that are assumed to
  have the same dimension) is assigned to \p *this.
  The result is minimized.
*/
void
PPL::PolyBase::intersection_assign_and_minimize(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::Polyhedron::inters_assign_and_min(y)",
			       x, y);

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

  // add_and_minimize() requires x to be up-to-date
  // and to have sorted constraints...
  x.minimize();
  // ... and y to have updated and sorted constraints.
  if (!y.constraints_are_up_to_date())
    y.update_constraints();

  // After minimize(), x.con_sys is not necessarily sorted.
  x.obtain_sorted_constraints_with_sat_c();
  // After update_constraint() y.con_sys is not necessarily sorted.
  const_cast<PolyBase&>(y).obtain_sorted_constraints();

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
}

/*!
  The intersection of \p *this with \p y (that are assumed to
  have the same dimension) is assigned to \p *this.
  The result is not minimized.
*/
void
PPL::PolyBase::intersection_assign(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::Polyhedron::inters_assign_and_min(y)",
			       x, y);

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
  if (!x.con_sys.is_sorted())
    x.con_sys.sort_rows();
  if (!y.con_sys.is_sorted())
    const_cast<PolyBase&>(y).con_sys.sort_rows();

  x.con_sys.merge_rows_assign(y.con_sys);
  // After adding new constraints, generators are no longer up-to-date.
  x.clear_generators_up_to_date();
  // It does not minimize the system of constraints.
  x.clear_constraints_minimized();
}

/*!
  The convex hull of the set-theoretic union of \p *this and \p y
  (that are assumed to have the same dimension) is assigned to \p *this.
  The result is minimized.
*/
void
PPL::PolyBase::convex_hull_assign_and_minimize(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::Polyhedron::con_hull_assign_and_min(y)",
			       x, y);

  // Convex hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  if (x.is_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their convex-hull is the universe polyhedron too.
  if (x_space_dim == 0)
    return;

  // The function add_and_minimize() requires `x' to be up-to-date and
  // to have sorted generators...
  x.minimize();
  // ...and `y' to have updated and sorted generators.
  if (!y.generators_are_up_to_date())
    y.update_generators();

  x.obtain_sorted_generators_with_sat_g();

  const_cast<PolyBase&>(y).obtain_sorted_generators();

  add_and_minimize(false,
		   x.gen_sys, x.con_sys, x.sat_g,
		   y.gen_sys);

  x.set_sat_g_up_to_date();
  x.clear_sat_c_up_to_date();

  assert(OK());
}

/*!
  The convex hull of the set-theoretic union of \p *this and \p y
  (that are assumed to have the same dimension) is assigned to \p *this.
  The result is not minimized.
*/
void
PPL::PolyBase::convex_hull_assign(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::Polyhedron::convex_hull_assign(y)",
			       x, y);

  // Convex hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  if (x.is_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their convex-hull is the universe polyhedron too.
  if (x_space_dim == 0)
    return;

  if (!x.generators_are_up_to_date())
    x.update_generators();
  if (!y.generators_are_up_to_date())
    y.update_generators();

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  if (!x.gen_sys.is_sorted())
    x.gen_sys.sort_rows();
  if (!y.gen_sys.is_sorted())
    const_cast<PolyBase&>(y).gen_sys.sort_rows();

  x.gen_sys.merge_rows_assign(y.gen_sys);

  // After adding new generators, constraints are no longer up-to-date.
  x.clear_constraints_up_to_date();
  // It does not minimize the system of generators.
  x.clear_generators_minimized();

  assert(OK());
}

/*!
  The convex hull of the set-theoretic difference of \p *this and \p y
  (that are assumed to have the same dimension) is assigned to \p *this.
  The result is not minimized.
*/
void
PPL::PolyBase::convex_difference_assign(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;
  size_t x_space_dim = x.space_dim;
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dim)
    throw_different_dimensions("PPL::Polyhedron::convex_difference_assign(y)",
			       x, y);

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

  // FIXME: This is just an executable specification.
  if (x <= y) {
    x.set_empty();
    return;
  }

  PolyBase new_polyhedron(topology(), x_space_dim, EMPTY);

  const ConSys& x_cs = x.constraints();
  const ConSys& y_cs = y.constraints();
  for (ConSys::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    ConSys z_cs = x_cs;
    const Constraint& c = *i;
    assert(!c.is_trivial_true());
    assert(!c.is_trivial_false());
    if (c.is_inequality()) {
      LinExpression e(0 * Variable(x_space_dim-1));
      for (int varid = x_space_dim-1; varid >= 0; --varid) {
	const Integer& n = c.coefficient(Variable(varid));
	if (n != 0)
	  e += n * Variable(varid);
      }
      if (is_necessarily_closed())
	z_cs.insert(e <= -c.coefficient());
      else
	if (c.is_strict_inequality())
	  z_cs.insert(e <= -c.coefficient());
	else
	  z_cs.insert(e < -c.coefficient());
      new_polyhedron.convex_hull_assign(PolyBase(topology(), z_cs));
    }
  }
  *this = new_polyhedron;

  assert(OK());
}

/*!
  The convex hull of the set-theoretic difference of \p *this and \p y
  (that are assumed to have the same dimension) is assigned to \p *this.
  The result is minimized.
*/
void
PPL::PolyBase::convex_difference_assign_and_minimize(const PolyBase& y) {
  assert(topology() == y.topology());
  convex_difference_assign(y);
  minimize();
  assert(OK());
}

/*!
  \param mat1      The matrix to which columns are added.
  \param mat2      The matrix to which rows and columns are added.
  \param sat1      The saturation matrix whose columns represent
                   constraints or generators if \p mat1 is a matrix
		   of constraints or generators respectively.
		   On entry it is up-to-date.
  \param sat2      The saturation matrix which columns represent
                   constraints or generators if \p mat2 is a matrix
		   of constraints or generators respectively.
  \param add_dim   The number of dimensions to add.

  Adds new dimensions to the polyhedron modifying the matrices.
  This function is invoked only by <CODE>add_dimensions_and_embed()</CODE>
  and <CODE>add_dimensions_and_project()</CODE> passing the matrix of
  constraints and that of generators (and the corresponding saturation
  matrices) in different order (see those methods for details).
  Also, this method is invoked only when the polyhedron has at
  least the system of constraints or generators up-to-date.
*/
void
PPL::PolyBase::add_dimensions(Matrix& mat1,
			      Matrix& mat2,
			      SatMatrix& sat1,
			      SatMatrix& sat2,
			      size_t add_dim) {
  assert(mat1.topology() == mat2.topology());
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
    // Moving the \epsilon coefficients in the last column.
    size_t new_eps_index = mat1.num_columns() - 1;
    size_t old_eps_index = new_eps_index - add_dim;
    mat1.swap_columns(old_eps_index, new_eps_index);
    mat2.swap_columns(old_eps_index, new_eps_index);
    // CHECK ME: since we swapped columns in both
    // `mat1' and `mat2', no swapping is required
    // for `sat1' and `sat2'.
  }
}


/*!
  Adds \p dim new dimensions and embeds the old polyhedron
  in the new space. The new dimensions are the last and the corresponding
  coefficients are in the last columns of the new matrices.
  The new polyhedron is characterized by
  a system of constraints in which the variables running through
  the new dimensions are not constrained, and by a system of generators
  containing the old one and the lines that are parallel to the axes
  corresponding to the new dimensions.
  For example if the old polyhedron is a line lying on the \f$xy\f$ plane and
  we add a third dimension \f$z\f$, then the operation results in
  a plane parallel to the \f$z\f$ axis that contains the line fixed on
  the \f$xy\f$ plane.
*/
void
PPL::PolyBase::add_dimensions_and_embed(size_t dim) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (dim == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained
  // by adjusting `space_dim' and clearing con_sys
  // (since it can contain the unsatisfiable constraint system
  //  of the wrong dimension).
  if (is_empty()) {
    space_dim += dim;
    con_sys.clear();
    return;
  }

  // The case of a zero-dim space polyhedron.
  if (space_dim == 0) {
    // Since it is not empty, it has to be the universe polyhedron.
    assert(status.test_zero_dim_univ());
    // We swap *this with a newly created
    // universe polyhedron of dimension `dim'.
    PolyBase ph(topology(), dim, UNIVERSE);
    swap(ph);
    return;
  }

  // To embed an n-dimension space polyhedron in a (n+dim)-dimension space,
  // we just add `dim' zero-columns to the rows in the matrix of constraints;
  // in contrast, the matrix of generators needs additional rows,
  // corresponding to the vectors of the canonical basis
  // for the added dimensions. That is, for each new dimension `x[k]'
  // we add the line having that direction. This is done by invoking
  // the function add_dimensions() giving the matrix of generators
  // as the second argument.
  if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // sat_c is necessary in add_dimensions(...).
    if (!sat_c_is_up_to_date())
      update_sat_c();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(con_sys, gen_sys, sat_c, sat_g, dim);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: we do not need to modify generators.
    con_sys.add_zero_columns(dim);
    // If the polyhedron is NON-necessarily closed,
    // move the \epsilon coefficients to the last column.
    if (!is_necessarily_closed())
      con_sys.swap_columns(space_dim + 1, space_dim + dim + 1);
  }
  else {
    // Only generators are up-to-date: we do not need to modify constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_rows_and_columns(dim);
    // If the polyhedron is NON-necessarily closed,
    // move the \epsilon coefficients to the last column.
    if (!is_necessarily_closed())
      gen_sys.swap_columns(space_dim + 1, space_dim + dim + 1);
  }
  // Update the space dimension.
  space_dim += dim;

  // Do not check for satisfiability, because the system of constraints
  // may be unsatisfiable.
  assert(OK(false));
}

/*!
  Adds \p dim new dimensions to the old polyhedron and projects
  it onto the old space.  The new dimensions are the last and the
  corresponding coefficients are in the last columns of the new matrices.
  The new polyhedron is characterized by a system of constraints
  containing the old one and other \p dim equalities that constrain
  the new variable to be zero. So the new matrix of generators is made
  by adding \p dim zero-columns to the old matrix.
  For example if we have a line lying on the plane \f$xy\f$, we
  add a third dimension \f$z\f$ and project the line in \f$\Rset^3\f$,
  we still obtain the line on the plane \f$xy\f$ but now the new
  considered space has one more dimension.
*/
void
PPL::PolyBase::add_dimensions_and_project(size_t dim) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (dim == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained
  // by merely adjusting `space_dim'.
  if (is_empty()) {
    space_dim += dim;
    con_sys.clear();
    return;
  }

  if (space_dim == 0) {
    assert(status.test_zero_dim_univ() && gen_sys.num_rows() == 0);
    // The system of generators for this polyhedron has only
    // the origin as a point.
    gen_sys.insert(Generator::zero_dim_point());
    // FIXME: the following call performs a few redundant tests
    //        on the topology of gen_sys.
    gen_sys.adjust_topology_and_dimension(topology(), dim);
    // In a non-necessarily closed polyhedron, all points
    // have to be accompanied by the corresponding closure points.
    if (!is_necessarily_closed())
      gen_sys.insert(Generator::zero_dim_closure_point());
    set_generators_minimized();
    space_dim = dim;
    return;
  }

  // To project an n-dimension space polyhedron in a (n+dim)-dimension space,
  // we just add to the matrix of generators `dim' zero-columns;
  // In contrast, in the matrix of constraints, new rows are needed
  // in order to avoid embedding the old polyhedron in the new space.
  // Thus, for each new dimensions `x[k]', we add the constraint
  // x[k] = 0; this is done by invoking the function add_dimensions()
  // giving the matrix of constraints as the second argument.
  if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // sat_g is necessary in add_dimensions(...).
    if (!sat_g_is_up_to_date())
      update_sat_g();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(gen_sys, con_sys, sat_g, sat_c, dim);
  }
  else if (constraints_are_up_to_date()) {
    // Only constraints are up-to-date: no need to modify the generators.
    con_sys.add_rows_and_columns(dim);
    // If the polyhedron is NON-necessarily closed,
    // move the \epsilon coefficients to the last column.
    if (!is_necessarily_closed())
      con_sys.swap_columns(space_dim + 1, space_dim + dim + 1);
  }
  else {
    // Only generators are up-to-date: no need to modify the constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_zero_columns(dim);
    if (!is_necessarily_closed())
      gen_sys.swap_columns(space_dim + 1, space_dim + dim + 1);
  }
  // Now we update the space dimension.
  space_dim += dim;

  // Do not check for satisfiability, because the system of constraints
  // may be unsatisfiable.
  assert(OK(false));
}

static void
throw_dimension_incompatible(const char* method,
			     const PPL::PolyBase& x,
			     size_t requested_dimension) {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", requested dimension == " << requested_dimension;
  throw std::invalid_argument(s.str());
}

/*!
  Removes the dimensions corresponding to the elements of
  the set \p dims_to_remove.
*/
void
PPL::PolyBase::remove_dimensions(const std::set<Variable>& to_be_removed) {
  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK(false));
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  unsigned int max_dim_to_be_removed = to_be_removed.rbegin()->id();
  if (max_dim_to_be_removed >= space_dim)
    throw_dimension_incompatible("void PPL::Polyhedron::remove_dimensions(vs)",
				 *this, max_dim_to_be_removed);

  if (is_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Removing dimensions from the empty polyhedron:
    // just updates the space dimension.
    space_dim -= to_be_removed.size();
    con_sys.clear();
    assert(OK(false));
    return;
  }

  // FIXME : is there an efficient way to check if removing
  //         ALL the dimensions ?

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

  // Update the space dimension.
  space_dim = dst_col - (is_necessarily_closed() ? 1 : 2);
  if (space_dim == 0)
    // If zero `true' Cartesian axes have been left,
    // the resulting polyhedron is the zero-dimension universe.
    set_zero_dim_univ();
  else {
    // Constraints are not up-to-date.
    clear_constraints_up_to_date();
    // Generators are no longer guaranteed to be minimized.
    clear_generators_minimized();
  }
  assert(OK());
}

/*!
  Removes the higher dimensions so that the resulting space
  is of dimension \p new_dimension.
*/
void
PPL::PolyBase::remove_higher_dimensions(size_t new_dimension) {
  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (new_dimension == space_dim) {
    assert(OK(false));
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  if (new_dimension > space_dim)
    throw_dimension_incompatible("void PPL::Polyhedron::"
				 "remove_higher_dimensions(nd)",
				 *this, new_dimension);

  if (is_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Removing dimensions from the empty polyhedron:
    // just updates the space dimension.
    space_dim = new_dimension;
    con_sys.clear();
    assert(OK(false));
    return;
  }

  if (new_dimension == 0) {
    // Removing all dimensions from a non-empty polyhedron:
    // just return the zero-dim universe polyhedron.
    set_zero_dim_univ();
    return;
  }

  size_t new_num_cols = new_dimension + 1;
  if (!is_necessarily_closed()) {
    // The polyhedron is NOT necessarily closed: move the column
    // of the \epsilon coefficients to its new place.
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

  assert(OK());
}

static void
throw_different_dimensions(const char* method,
			   const PPL::PolyBase& x,
			   const PPL::Matrix& y) {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", system->space_dimension() == " << y.space_dimension();
  throw std::invalid_argument(s.str());
}


/*!
  Adds further constraints to a polyhedron and computes the new polyhedron
  satisfying all the constraints.
*/
bool
PPL::PolyBase::add_constraints_and_minimize(ConSys& cs) {
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  size_t cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_different_dimensions("PPL::Polyhedron::add_constraints_and_min(cs)",
			       *this, cs);
  // Topology compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::Polyhedron::"
				"add_constraints_and_min(cs): "
				"cs contains strict inequalities");

  // Adding no constraints is the same as checking for emptyness.
  if (cs.num_rows() == 0) {
    assert(cs.num_columns() == 0);
    return !check_empty();
  }

  // Dealing with zero-dim space polyhedra first.
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

  // We use `check_empty()' because we want the flag EMPTY
  // to precisely represents the status of the polyhedron
  // (i.e., if it is false the polyhedron is really NOT empty)
  // and because, for a non-empty polyhedron, we need both
  // the system of generators and constraints minimal.
  if (check_empty())
    return false;

  // PolyBase::add_and_minimize() requires that
  // the matrix of constraints to add is sorted.
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
    // On exit of the function add_and_minimize() `sat_c' is up-to-date
    // while `sat_g' is not up-to-date anymore.
    set_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
  assert(OK());

  return !empty;
}


static void
throw_different_dimensions(const char* method,
			   const PPL::PolyBase& x,
			   const PPL::Row& y) {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.space_dimension();
  throw std::invalid_argument(s.str());
}

/*!
  This function inserts a new constraint \p c into the system of
  constraints of the polyhedron \p *this.
*/
// FIXME.
void
PPL::PolyBase::insert(const Constraint& c) {
  // Dimension-compatibility check:
  // the dimension of `c' can not be greater than space_dim.
  if (space_dim < c.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::insert(c)",
			       *this, c);
  // Topology-compatibility check.
  if (c.is_strict_inequality() && is_necessarily_closed())
    throw std::invalid_argument("PPL::Polyhedron::insert(c): "
				"c is a strict inequality");

  // Adding a new constraint to an empty polyhedron
  // results in an empty polyhedron.
  if (is_empty())
    return;

  // Dealing with a zero-dim space polyhedron first.
  if (space_dim == 0) {
    if (!c.is_trivial_true())
      set_empty();
    return;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // Here we know that the system of constraints has at least a row.
  if (is_necessarily_closed() && !c.is_necessarily_closed())
    // FIXME : here we have a (legal) topology mismatch.
    //         `ConSys::insert(c)' would change the topology
    // of `con_sys', which is wrong. Should we provide a new method
    //     ConSys::topology_and_dimension_preserving_insert(c)
    // which can only modify the constraint ?
    throw std::invalid_argument("PPL::Polyhedron::insert(c): "
				"INTERNAL ERROR (not implemented yet).\n"
				"[c is a _legal_ NNC constraint, "
				"since it is not a strict inequality.]");
  else
    // Since `con_sys' is not empty, the topology and space dimension
    // of the inserted constraint are automatically adjusted.
    con_sys.insert(c);

  // After adding new constraints, generators are no longer up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // The constraint system may have become unsatisfiable.
  // Do not check for satisfiability.
  assert(OK(false));
}

/*!
  This function inserts a new generator \p g into the system of
  generators of the polyhedron \p *this. The new generator
  can be redundant.
*/
// FIXME.
void
PPL::PolyBase::insert(const Generator& g) {
  // Dimension-compatibility check:
  // the dimension of `g' can not be greater than space_dim.
  size_t g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_different_dimensions("PPL::Polyhedron::insert(g)",
			       *this, g);
  // Topology-compatibility check.
  if (g.is_closure_point() && is_necessarily_closed())
    throw std::invalid_argument("PPL::Polyhedron::insert(g): "
				"g is a closure point");

  // Dealing with a zero-dim space polyhedron first.
  if (space_dim == 0) {
    // It is not possible to create 0-dim rays or lines.
    assert(g.is_point() || g.is_closure_point());
    // Closure points can only be inserted in non-empty polyhedra.
    if (is_empty())
      if (g.type() != Generator::POINT)
	throw std::invalid_argument("PPL::NCC_Polyhedron::insert(g): "
				    "*this is empty and g is not a point");
      else
	status.set_zero_dim_univ();
    return;
  }

  if (generators_are_up_to_date()) {
    if (is_necessarily_closed() && !g.is_necessarily_closed())
      // FIXME : here we have a (legal) topology mismatch.
      //         `GenSys::insert(g)' would change the topology
      // of `gen_sys', which is wrong. Should we provide a new method
      //     GenSys::topology_and_dimension_preserving_insert(g)
      // which can only modify the generator ?
      throw std::invalid_argument("PPL::Polyhedron::insert(g): "
				  "INTERNAL ERROR (not implemented yet).\n"
				  "[g is a _legal_ NNC generator, "
				  "since it is not a closure point.]");
    else
      // Since `gen_sys' is not empty, the topology and space dimension
      // of the inserted generator are automatically adjusted.
      gen_sys.insert(g);

    // After adding the new generator, constraints are no longer up-to-date.
    clear_generators_minimized();
    clear_constraints_up_to_date();
    return;
  }

  // Generators are not up-to-date. Checking for emptyness and
  // computing the generators at the same time.
  if (check_empty()) {
    // Polyhedron is empty:
    // the specification says we can only insert a point.
    if (g.type() != Generator::POINT)
      throw std::invalid_argument("void PPL::Polyhedron::insert(g): "
				  "*this is empty and g is not a point");

    if (is_necessarily_closed() && !g.is_necessarily_closed())
      // FIXME : here we have a (legal) topology mismatch.
      //         `GenSys::insert(g)' would change the topology
      // of `gen_sys', which is wrong. Should we provide a new method
      //     GenSys::topology_and_dimension_preserving_insert(g)
      // which can only modify the generator ?
      throw std::invalid_argument("PPL::Polyhedron::insert(g): "
				  "INTERNAL ERROR (not implemented yet).\n"
				  "[g is a _legal_ NNC generator, "
				  "since it is not a closure point.]");
    else
      gen_sys.insert(g);

    // `gen_sys' was empty: after inserting `g' we have to resize
    // the system of generators to have the right dimension.
    gen_sys.adjust_topology_and_dimension(topology(), space_dim);
    // No longer empty, generators up-to-date and minimized.
    clear_empty();
    set_generators_minimized();
    return;
  }

  // Here the polyhedron is NOT empty and with generators up-to-date.
  if (is_necessarily_closed() && !g.is_necessarily_closed())
    // FIXME : here we have a (legal) topology mismatch.
    //         `GenSys::insert(g)' would change the topology
    // of `gen_sys', which is wrong. Should we provide a new method
    //     GenSys::topology_and_dimension_preserving_insert(g)
    // which can only modify the generator ?
    throw std::invalid_argument("PPL::Polyhedron::insert(g): "
				"INTERNAL ERROR (not implemented yet).\n"
				"[g is a _legal_ NNC generator, "
				"since it is not a closure point.]");
  else
    // Since `gen_sys' is not empty, the topology and space dimension
    // of the inserted generator are automatically adjusted.
    gen_sys.insert(g);
  // Generators no longer minimized, constraints no longer up-to-date.
  clear_generators_minimized();
  clear_constraints_up_to_date();
}


/*!
  Adds specified constraints to a polyhedron without minimizing.
*/
void
PPL::PolyBase::add_constraints(ConSys& cs) {
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  size_t cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_different_dimensions("PPL::Polyhedron::add_constraints(cs)",
			       *this, cs);
  // Topology compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::Polyhedron::"
				"add_constraints(cs): "
				"cs contains strict inequalities");

  // Adding no constraints is a no-op.
  if (cs.num_rows() == 0)
    return;

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
  if (!constraints_are_up_to_date())
    minimize();

  // Adding constraints to an empty polyhedron is a no-op.
  if (is_empty())
    return;

  // FIXME: the following instruction breaks all the
  // performance-keeping effort of the rows include in #ifdef BE_LAZY.

  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);


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

#else

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  if (!con_sys.is_sorted())
    con_sys.sort_rows();

  if (!cs.is_sorted())
    cs.sort_rows();

  // The function `merge_row_assign' automatically resizes
  // the system `cs' if the dimension of the space of `cs'
  // is smaller then the dimension of the space of the polyhedron.
  con_sys.merge_rows_assign(cs);
#endif //#ifdef BE_LAZY

  // After adding new constraints, generators are no longer up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // The constraint system may have become unsatisfiable.
  // Do not check for satisfiability.
  assert(OK(false));
}


void
PPL::PolyBase::add_dimensions_and_constraints(ConSys& cs) {
  size_t added_columns = cs.space_dimension();
  // Topology compatibility check: we do NOT adjust dimensions.
  if (!cs.adjust_topology_and_dimension(topology(), added_columns))
    throw std::invalid_argument("PPL::Polyhedron::"
				"add_dimensionss_and_constraints(cs): "
				"cs contains strict inequalities");

  // For an empty polyhedron, it is sufficient to adjust
  // the dimension of the space.
  if (is_empty()) {
    space_dim += added_columns;
    con_sys.clear();
    return;
  }

  // For a non-empty 0-dim space polyhedron,
  // the result is the polyhedron defined by `cs'.
  if (space_dim == 0) {
    PolyBase y(topology(), cs);
    swap(y);
    return;
  }

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
  // Move the \epsilon coefficient to the last column, if needed.
  if (!is_necessarily_closed())
    con_sys.swap_columns(old_num_columns - 1,
			 old_num_columns - 1 + added_columns);
  for (size_t i = added_rows; i-- > 0; ) {
    Constraint& c_new = con_sys[old_num_rows + i];
    Constraint& c_old = cs[i];
    // Method `grow', by defaults, adds inequalities.
    if (c_old.is_equality())
      c_new.set_is_equality();
    std::swap(c_new[0], c_old[0]);
    for (size_t j = added_columns; j-- > 1; )
      std::swap(c_new[old_num_columns - 1 + j], c_old[j]);
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

  // The system of constraints may be unsatisfiable.
  // Do not check for satisfiability.
  assert(OK(false));
}

/*!
  Adds further generators to a polyhedron.
*/
void
PPL::PolyBase::add_generators_and_minimize(GenSys& gs) {
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dimension().
  size_t gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_different_dimensions("PPL::Polyhedron::add_generators_and_min(g)",
			       *this, gs);

  // Adding no generators is a no-op.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return;
  }

  if (space_dim == 0) {
    // Since `gs' is 0-dim and non-empty,
    // it has to contain only points.
    assert(gs[0].type() == Generator::POINT);
    status.set_zero_dim_univ();
    return;
  }

  if (!gs.is_sorted())
    gs.sort_rows();

  // If needed, we extend `gs' to the right space dimension.
  if (space_dim > gs_space_dim)
    gs.add_zero_columns(space_dim - gs_space_dim);

  // We use `check_empty()' because we want the flag EMPTY
  // to precisely represents the status of the polyhedron
  // (i.e., if it is false the polyhedron is really NOT empty)
  // and because, for a non-empty polyhedron, we need both
  // the system of generators and constraints minimal.
  if (!check_empty()) {
    obtain_sorted_generators_with_sat_g();
    add_and_minimize(false, gen_sys, con_sys, sat_g, gs);
    clear_sat_c_up_to_date();
  }
  else {
    // Checking if the system of generators contains a point.
    size_t i = 0;
    size_t iend = gs.num_rows();
    for ( ; i < iend; ++i) {
      if (gs[i][0] > 0)
	break;
    }
    if (i == iend)
      throw std::invalid_argument("PPL::Polyhedron::add_generators_and_min"
				  "(gs): non-empty gs with no points");

    // If the system of generators has a point, the polyhedron is no
    // longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    clear_empty();
    set_generators_up_to_date();
    minimize();
  }
}

/*!
  Adds specified generators to a polyhedron without minimizing.
*/
void
PPL::PolyBase::add_generators(GenSys& gs) {
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dim.
  size_t gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_different_dimensions("PPL::Polyhedron::add_generators(g)",
			       *this, gs);

  // Adding no generators is a no-op.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return;
  }

  if (space_dim == 0) {
    // Since `gs' is 0-dim and non-empty,
    // it has to contain only points.
    assert(gs[0].type() == Generator::POINT);
    status.set_zero_dim_univ();
    return;
  }

  // We only need that the system of generators is up-to-date.
  if (!generators_are_up_to_date())
    minimize();


  if (is_empty()) {
    // Checking if the system of generators contains a point.
    size_t i = 0;
    size_t iend = gs.num_rows();
    for ( ; i < iend; ++i) {
      if (gs[i][0] > 0)
	break;
    }
    if (i == iend)
      throw std::invalid_argument("PPL::Polyhedron::add_generators(gs): "
				  "non-empty gs with no points");

    // The system of generators contains at least a point.
    // If needed, we extend `gs' to the right space dimension.
    if (space_dim > gs_space_dim)
      gs.add_zero_columns(space_dim - gs_space_dim);

    // The polyhedron is no longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    set_generators_up_to_date();
    clear_empty();
    return;
  }

  // Matrix::merge_row_assign() requires both matrices to be sorted.
  if (!gen_sys.is_sorted())
    gen_sys.sort_rows();

  if (!gs.is_sorted())
    gs.sort_rows();

  // The function `merge_row_assign' automatically resizes
  // the system `gs' if the dimension of the space of `gs'
  // is smaller then the dimension of the space of the polyhedron.
  gen_sys.merge_rows_assign(gs);

  // After adding new generators, generators are no longer up-to-date.
  clear_generators_minimized();
  clear_constraints_up_to_date();

  assert(OK());
}

std::ostream&
PPL::operator<<(std::ostream& s, const PolyBase& p) {
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
PPL::operator>>(std::istream& s, PolyBase& p) {
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
    \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator}
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
  	\text{denominator} * {x'}_\mathrm{var}
	  = \sum_{i = 0}^{n - 1} a_i x_i + b
	  = a_\mathrm{var} x_\mathrm{var}
	      + \sum_{i \neq var} a_i x_i + b,
     \f]
     so that the inverse transformation is
     \f[
	a_\mathrm{var} x_\mathrm{var}
          = \text{denominator} * {x'}_\mathrm{var}
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
PPL::PolyBase::affine_image(const Variable& var,
			    const LinExpression& expr,
			    const Integer& denominator) {
  if (denominator == 0)
    throw std::invalid_argument("void PPL::Polyhedron::affine_image"
				"(v, e, d): d == 0");
  PolyBase& x = *this;
  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension of `x'.
  size_t x_space_dim = x.space_dim;
  size_t expr_space_dim = expr.space_dimension();
  if (x_space_dim < expr_space_dim)
    throw_different_dimensions("PPL::Polyhedron::affine_image(v, e, d)",
			       x, expr);
  // `var' should be one of the dimensions of the polyhedron.
  size_t num_var = var.id() + 1;
  if (num_var >= x_space_dim + 1)
    throw_dimension_incompatible("PPL::Polyhedron::affine_image(v, e, d)",
				 x, var.id());

  if (x.is_empty())
    return;

  if (x_space_dim > expr_space_dim) {
    LinExpression copy(expr, x_space_dim + 1);
    const_cast<LinExpression&>(expr).swap(copy);
  }
  if (expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (generators_are_up_to_date())
      x.gen_sys.affine_image(num_var, expr, denominator);
    if (constraints_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      x.con_sys.affine_preimage(num_var, inverse, expr[num_var]);
    }
  }
  // The transformation is not invertible.
  else {
    // We need that the system of generators is up-to-date.
    if (!generators_are_up_to_date())
      x.minimize();
    if (!is_empty()) {
      x.gen_sys.affine_image(num_var, expr, denominator);
      x.clear_constraints_up_to_date();
      x.clear_generators_minimized();
      clear_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
  }
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
  	\text{denominator} * {x'}_\mathrm{var}
	  = \sum_{i = 0}^{n - 1} a_i x_i + b
          = a_\mathrm{var} x_\mathrm{var}
              + \sum_{i \neq \text{var}} a_i x_i + b,
     \f],
     the inverse transformation is
     \f[
	a_\mathrm{var} x_\mathrm{var}
          = \text{denominator} * {x'}_\mathrm{var}
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
          a_{ij} * \text{denominator} + a_{i\text{var}} * \text{expr}[j]
            \quad \text{for } j \neq \text{var}; \\
          \text{expr}[\text{var}] * a_{i\text{var}},
            \quad \text{for } j = \text{var}.
        \end{cases}
  \f]
*/
void
PPL::PolyBase::affine_preimage(const Variable& var,
			       const LinExpression& expr,
			       const Integer& denominator) {
  if (denominator == 0)
    throw std::invalid_argument("void PPL::Polyhedron::affine_preimage"
				"(v, e, d): d == 0");

  PolyBase& x = *this;
  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension of `x'.
  size_t x_space_dim = x.space_dim;
  size_t expr_space_dim = expr.space_dimension();
  if (x_space_dim < expr_space_dim)
    throw_different_dimensions("PPL::Polyhedron::affine_preimage(v, e, d)",
			       x, expr);
  // `var' should be one of the dimensions of the polyhedron.
  size_t num_var = var.id() + 1;
  if (num_var >= x_space_dim + 1)
    throw_dimension_incompatible("PPL::Polyhedron::affine_preimage(v, e, d)",
				 x, var.id());

  if (x.is_empty())
    return;

  if (x_space_dim > expr_space_dim) {
    LinExpression copy(expr, x_space_dim + 1);
    const_cast<LinExpression&>(expr).swap(copy);
  }

  if (expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (constraints_are_up_to_date())
      x.con_sys.affine_preimage(num_var, expr, denominator);
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      x.gen_sys.affine_image(num_var, inverse, expr[num_var]);
    }
  }
  else {
    // The transformation is not invertible.
    if (!constraints_are_up_to_date())
      x.minimize();
    x.con_sys.affine_preimage(num_var, expr, denominator);
    x.clear_generators_up_to_date();
    x.clear_constraints_minimized();
    clear_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
}

/*!
  Returns the relations holding between the polyhedron \p *this and
  the constraint \p c.
*/
PPL::Poly_Con_Relation
PPL::PolyBase::relation_with(const Constraint& c) {
  // Dimension-compatibility check.
  if (space_dim < c.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::relation_with(c)",
			       *this, c);
  if (is_empty())
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0)
    if (c.is_trivial_false())
      return Poly_Con_Relation::is_disjoint();
    else
      if (c.is_equality() || c[0] == 0)
	return Poly_Con_Relation::saturates()
	  && Poly_Con_Relation::is_included();
      else
	// The zero-dim point does not saturate
	// the positivity constraint. 
	return Poly_Con_Relation::is_included();

  if (!generators_are_up_to_date())
    if (!update_generators())
      // The polyhedron is empty.
      return Poly_Con_Relation::saturates()
	&& Poly_Con_Relation::is_included()
	&& Poly_Con_Relation::is_disjoint();

  return gen_sys.relation_with(c);
}

/*!
  Returns the relations holding between the polyhedron \p *this and
  the generator \p g.
*/
PPL::Poly_Gen_Relation
PPL::PolyBase::relation_with(const Generator& g) {
  // Dimension-compatibility check.
  if (space_dim < g.space_dimension())
     throw_different_dimensions("PPL::Polyhedron::relation_with(g)",
				 *this, g);

  // Any generators is not subsumed by the empty-polyhedron.
  if (is_empty())
    return Poly_Gen_Relation::nothing();

  // A universe polyhedron in a zero-dimensional space subsumes
  // all the generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();
  else {
    if (!constraints_are_up_to_date())
      update_constraints();
    return
      con_sys.satisfies_all_constraints(g)
      ? Poly_Gen_Relation::subsumes()
      : Poly_Gen_Relation::nothing();
  }
}

/*!
  This function computes the widening between \p *this and
  \p y and the result is assigned to the polyhedron \p *this.
  The resulting polyhedron has a system of constraints
  which is up-to-date for just those constraints that are common
  to both the polyhedra.
  For this function, it is required that the polyhedron \p y is contained
  in \p *this.
*/
void
PPL::PolyBase::widening_assign(const PolyBase& y) {
  assert(topology() == y.topology());
  PolyBase& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    PolyBase x_copy = x;
    PolyBase y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  // Dimension-compatibility check.
  size_t x_space_dim = x.space_dim;
  size_t y_space_dim = y.space_dim;
  if (x_space_dim != y_space_dim)
    throw_different_dimensions("PPL::Polyhedron::widening_assign(y)",
			       *this, y);

  // The widening between two polyhedra in a zero-dimensional space
  // is a polyhedron iin a zero-dimensional space, too.
  if (x_space_dim == 0)
    return;

  if (y.is_empty())
    return;
  if (x.is_empty())
    return;

  // For this function, we need the minimal system of generators and
  // the saturation matrix `sat_g' of the polyhedron `y'. To obtain
  // the saturation matrix, the minimal system of constraints and
  // the minimal system of generators are necessary.
  y.minimize();
  // After the minimize, a polyhedron can become empty if the system
  // of constraints is unsatisfiable.
  // If the polyhedron `y' is empty, the widened polyhedron is `x'.
  if (y.is_empty())
    return;
  // We only need that the system of constraints is up-to-date
  // because
  // - if the system of constraints is insoluble, this means that
  //   also `y' is empty and so the resulting polyhedron is `x';
  // - if some constraints are redundant, these do not influence the
  //   resulting polyhedron.
  //   If a constraint is a combination of other two, it can be also in
  //   the resulting system, if the two constraints are common to both
  //   polyhedron. The redundant constraints is `redundant' also in
  //   the new polyhedron.
  //   If a constraint is redundant in the sense that it does not
  //   satisfy the saturation rule (see in the Introduction), it can not
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
  // The saturation matrix sat_g is copied in a temporary one:
  // in this way, the new saturation matrix can be sorted
  // without modifying the constant polyhedron `y'.
  SatMatrix tmp_sat_g = y.sat_g;
  tmp_sat_g.sort_rows();
  // Now, we start to build the system of constraints of the
  // widened polyhedron. The first constraint is the positivity
  // one: its presence is necessary to have a correct polyhedron.
  ConSys new_con_sys(topology(), 1, x.con_sys.num_columns());
  // Hand-made positivity constraint.
  new_con_sys[0][0] = 1;
  new_con_sys[0].set_is_inequality();
  // A one-row ConSys is sorted.
  new_con_sys.set_sorted(true);
  // The size of `buffer' will reach sat.num_columns() bit.
  SatRow buffer;
  // We choose a constraint of `x' if its behavior with the
  // generators of `y' is the same that a constraints of `y' has.
  // This means that we verify if the saturation row built starting
  // from a constraint of `x' and all the generators of `y' is
  // a row of the saturation matrix `sat_g' of `y'(if it happens,
  // the constraint of `x' is also a constraint of `y').
  // In this way a constraint of `x' that does not verify the
  // saturation rule (see in the Introduction) can be put into the
  // resulting polyhedron, because `sat_g' is built staring from
  // a minimized polyhedron.
  for (size_t i = x.con_sys.num_rows(); i-- > 0; ) {
    buffer.clear();
    // The saturation row `buffer' is built considering the `i'-th
    // constraint of the polyhedron `x' and the generators of the
    // polyhedron `y'.
    for (size_t j = tmp_sat_g.num_columns(); j-- > 0; ) {
      int sp_sgn = sgn(y.gen_sys[j] * x.con_sys[i]);
      // We are assuming that y <= x.
      assert(sp_sgn >= 0);
      if (sp_sgn > 0)
	buffer.set(j);
    }
    // We verify if `buffer' is a row of the saturation matrix
    // `sat_g' of the polyhedron `y': to do this check, we use
    // the saturation matrix `tmp_sat_g' (that is sorted) in order to have
    // faster comparisons.
    if (tmp_sat_g.sorted_contains(buffer))
      new_con_sys.add_row(x.con_sys[i]);
  }

  std::swap(x.con_sys, new_con_sys);

  // Update the status of x.
  x.set_constraints_up_to_date();
  x.clear_constraints_minimized();
  x.clear_generators_up_to_date();

  assert(OK());
}


/*!
  This function adds to the widened polyhedron \p *this (obtained
  starting from \p *this and \p y) the constraints of the matrix
  \p cs that are satisfied by both the polyhedron \p y and
  the initial polyhedron \p *this.
  Returns <CODE>true</CODE> if the widened polyhedron \p *this is
  not empty.
*/
void
PPL::PolyBase::limited_widening_assign(const PolyBase& y, ConSys& cs) {
  assert(topology() == y.topology());
  PolyBase& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    PolyBase x_copy = x;
    PolyBase y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  // Dimension-compatibility check.
  size_t x_space_dim = x.space_dim;
  size_t y_space_dim = y.space_dim;
  if (x_space_dim != y_space_dim)
    throw_different_dimensions("PPL::Polyhedron::limited_widening_assign(y,c)",
			       *this, y);
  // `cs' must be dimension-compatible with the two polyhedra.
  size_t cs_space_dim = cs.space_dimension();
  if (x_space_dim < cs_space_dim)
    throw_different_dimensions("PPL::Polyhedron::limited_widening_assign(y,c)",
			       *this, cs);

  if (y.is_empty())
    return;
  if (x.is_empty())
    return;

  // The limited_widening between two polyhedra in a zero-dimensional space
  // is a polyhedron in a zero-dimensional space, too.
  if (x_space_dim == 0)
    return;

  y.minimize();
  // This function needs that the generators of `x' are up-to-date,
  // because we use these to choose which constraints of the matrix
  // `cs' must be added to the resulting polyhedron.
  if (!x.generators_are_up_to_date())
    x.update_generators();
  // After the minimize, a polyhedron can become empty if the system
  // of constraints is unsatisfiable.
  if (!y.is_empty()) {
    size_t new_cs_num_rows = 0;
    for (size_t i = 0, cs_num_rows = cs.num_rows(); i < cs_num_rows; ++i) {
      // The constraints to add must be saturated by both the
      // polyhedrons. To choose them, we only use the generators
      // of the greater polyhedron `x', because those of `y'
      // are points also of `x' (`y' is contained in `x') and
      // so they satisfy the chosen constraints, too.
      Poly_Con_Relation relation = x.gen_sys.relation_with(cs[i]);
      if (relation == Poly_Con_Relation::saturates()
	  || relation == Poly_Con_Relation::is_included())
	// The chosen constraints are put at the top of the
	// matrix \p cs.
	std::swap(cs[new_cs_num_rows], cs[i]);
      ++new_cs_num_rows;
    }
    x.widening_assign(y);
    // We erase the constraints that are not saturated or satisfied
    // by the generators of `x' and `y' and that are put at the end
    // of the matrix \p cs.
    cs.erase_to_end(new_cs_num_rows);

    // The system of constraints of the resulting polyhedron is
    // composed by the constraints of the widened polyhedron `x'
    // and by those of the new `cs'.
    // The function `merge_row_assign' automatically resizes
    // the system `cs' if the dimension of the space of `cs'
    // is smaller then the dimension of the space of the polyhedron.
    cs.sort_rows();
    x.con_sys.sort_rows();
    x.con_sys.merge_rows_assign(cs);

    // Only the system of constraints is up-to-date.
    x.set_constraints_up_to_date();
    x.clear_constraints_minimized();
    x.clear_generators_up_to_date();
  }
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this represents
  the universal polyhedron: to be universal, \p this must contain
  just the positivity constraint.
*/
bool
PPL::PolyBase::check_universe() const {
  if (!constraints_are_minimized())
    minimize();
  if (con_sys.num_rows() != 1)
    return false;
  if (!con_sys[0].is_inequality())
    return false;
  if (con_sys[0][0] <= 0)
    return false;
  for (size_t i = con_sys.num_columns(); i-- > 1; )
    if (con_sys[0][i] != 0)
      return false;
  return true;
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this is a bounded
  polyhedron: to be bounded a polyhedron must have only points
  in its system of generators.
*/
bool
PPL::PolyBase::is_bounded() const {
  if (!generators_are_up_to_date())
    // We use the function `minimize()', because if the polyhedro is
    // empty or zero-dimensional, this function does nothing.
    minimize();
  if (is_empty() || space_dim == 0)
    // An empty or a zero-dimensional polyhedron is bounded.
    return true;
  
  for (size_t i = gen_sys.num_rows(); i-- > 0; )
    if (gen_sys[i][0] == 0)
      // A line or a ray is found: the polyhedron is not bounded.
      return false;
  // The system of generators is composed only by points:
  // the polyhedron is bounded.
  return true;
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
bool
PPL::PolyBase::OK(bool check_not_empty) const {
  using std::endl;
  using std::cerr;

  // Check whether the saturation matrices are well-formed.
  if (!sat_c.OK())
    goto bomb;
  if (!sat_g.OK())
    goto bomb;

  // Checks the possible meaningful status combinations.
  if (!status.OK()) {
    cerr << "Wrong status!" << endl;
    goto bomb;
  }
  // An empty polyhedron is allowed if the system of constraints has
  // no rows or if the system of constraints contains only an
  // unsatisfiable constraint.
  if (is_empty())
    if (con_sys.num_rows() == 0)
      return true;
    else {
      if (con_sys.space_dimension() != space_dim) {
	cerr << "The polyhedron is in a space of dimension "
	     << space_dim
	     << " while the system of constraints is in a space of dimension "
	     << con_sys.space_dimension()
	     << endl;
	goto bomb;
      }
      if (con_sys.num_rows() != 1) {
	cerr << "The system of constraints has more then one row "
	     << endl;
	goto bomb;
      }
      else
	if (!con_sys[0].is_trivial_false()) {
	  cerr << "Empty polyhedron with a satisfiable system of constraints"
	       << endl;
	  goto bomb;
	}
    }

  // A zero-dimensional, non-empty polyhedron is legal only if the
  // system of constraint `con_sys' and the system of generators
  // `gen_sys' have no rows.
  if (space_dim == 0)
    if (con_sys.num_rows() != 0 || gen_sys.num_rows() != 0) {
      cerr << "Zero-dimensional polyhedron with a non-empty"
	   << endl
	   << "system of constraints or generators."
	   << endl;
      goto bomb;
    }
    else
      return true;

  // A polyhedron is defined by a system of constraints
  // or a system of generators: at least one of them must be up to date.
  if (!constraints_are_up_to_date() && !generators_are_up_to_date()) {
    cerr << "Polyhedron not empty, not zero-dimensional"
	 << endl
	 << "and with neither constraints nor generators up-to-date!"
	 << endl;
    goto bomb;
  }
  // Here we check if the size of the matrices is consistent.
  // Let us suppose that all the matrices are up-to-date; this means:
  // `con_sys' : number of constraints x space_dimension() + 1
  // `gen_sys' : number of generators  x space_dimension() + 1
  // `sat_c'   : number of generators  x number of constraints
  // `sat_g'   : number of constraints x number of generators
  if (constraints_are_up_to_date()) {
    if (con_sys.num_columns() != space_dim + 1) {
      cerr << "Incompatible size! (con_sys and space_dim)"
	   << endl;
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (con_sys.num_rows() != sat_c.num_columns()) {
	cerr << "Incompatible size! (con_sys and sat_c)"
	     << endl;
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (con_sys.num_rows() != sat_g.num_rows()) {
	cerr << "Incompatible size! (con_sys and sat_g)"
	     << endl;
	goto bomb;
      }
    if (generators_are_up_to_date())
      if (con_sys.num_columns() != gen_sys.num_columns()) {
	cerr << "Incompatible size! (con_sys and gen_sys)"
	     << endl;
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    if (gen_sys.num_columns() != space_dim + 1) {
      cerr << "Incompatible size! (gen_sys and space_dim)"
	   << endl;
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (gen_sys.num_rows() != sat_c.num_rows()) {
	cerr << "Incompatible size! (gen_sys and sat_c)"
	     << endl;
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (gen_sys.num_rows() != sat_g.num_columns()) {
	cerr << "Incompatible size! (gen_sys and sat_g)"
	     << endl;
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    // Check if the system of generators is well-formed.
    if (!gen_sys.OK())
      goto bomb;

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
	cerr << "Generators are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the generators:"
	     << endl
	     << copy_of_gen_sys
	     << endl;
	goto bomb;
      }
      // A minimal system of generators is unique up to positive
      // scaling, if the cone is pointed.  We thus verify if the cone
      // is pointed (i.e., if there are no lines) and, after
      // normalizing and sorting a copy of the matrix `gen_sys' of the
      // polyhedron (we use a copy not to modify the polyhedron's
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
	    goto bomb;
	}
      }
    }
  }

  if (constraints_are_up_to_date()) {
    // Check if the system of constraints is well-formed.
    if (!con_sys.OK())
      goto bomb;

    ConSys copy_of_con_sys = con_sys;
    GenSys new_gen_sys(topology());
    SatMatrix new_sat_g;

    if (minimize(true, copy_of_con_sys, new_gen_sys, new_sat_g)) {
      if (check_not_empty) {
	// Want to know the satisfiability of the constraints.
	cerr << "Insoluble system of constraints!"
	     << endl;
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
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
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
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
	goto bomb;
      }
    }
  }

  // If the polyhedron has both the system of constraints and
  // the system of generators, they must have the same number of columns.
  if (constraints_are_up_to_date() && generators_are_up_to_date()
      && con_sys.num_columns() != gen_sys.num_columns()) {
    cerr << "Constraints and generators are dimension-incompatible:" << endl
	 << con_sys.num_columns()-1 << " and " << gen_sys.num_columns()-1
	 << endl;
    goto bomb;
  }

  if (sat_c_is_up_to_date())
    for (size_t i = sat_c.num_rows(); i-- > 0; ) {
      Generator tmp_gen = gen_sys[i];
      SatRow tmp_sat = sat_c[i];
      for (size_t j = sat_c.num_columns(); j-- > 0; )
	if (sgn(tmp_gen * con_sys[j]) != tmp_sat[j]) {
	  cerr << "sat_c is declared up-to-date, but it is not!"
	       << endl;
	  goto bomb;
	}
    }

  if (sat_g_is_up_to_date())
    for (size_t i = sat_g.num_rows(); i-- > 0; ) {
      Constraint tmp_con = con_sys[i];
      SatRow tmp_sat = sat_g[i];
      for (size_t j = sat_g.num_columns(); j-- > 0; )
	if (sgn(tmp_con * gen_sys[j]) != tmp_sat[j]) {
	  cerr << "sat_g is declared up-to-date, but it is not!"
	       << endl;
	  goto bomb;
	}
    }

  return true;

 bomb:
  cerr << "Here is the guilty polyhedron:"
       << endl
       << *this
       << endl;
  return false;
}
