/* Polyhedron class implementation (non-inline public functions).
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

#include "Polyhedron.defs.hh"

#include <cassert>
#include <iostream>

#ifndef GRAM_SHMIDT
#define GRAM_SHMIDT 0
#endif

namespace PPL = Parma_Polyhedra_Library;

PPL::dimension_type
PPL::Polyhedron::dimension() const {
  // FIXME: this implementation can be made more efficient.
  if (is_empty())
    return 0;

  const ConSys& cs = minimized_constraints();
  dimension_type d = space_dim;
  for (ConSys::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i)
    if (i->is_equality())
      --d;
  return d;
}

const PPL::ConSys&
PPL::Polyhedron::constraints() const {
  if (marked_empty()) {
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
  if (marked_empty()) {
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

PPL::Poly_Con_Relation
PPL::Polyhedron::relation_with(const Constraint& c) const {
  // Dimension-compatibility check.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("relation_with(c)", "c", c);

  if (marked_empty())
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
    else if (c.is_equality() || c[0] == 0)
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
  if (marked_empty())
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

bool
PPL::Polyhedron::is_universe() const {
  if (marked_empty())
    return false;

  if (space_dim == 0)
    return true;

  if (!has_pending_generators() && constraints_are_up_to_date()) {
    // Search for a constraint that is not trivially true.
    for (dimension_type i = con_sys.num_rows(); i-- > 0; )
      if (!con_sys[i].is_trivial_true())
	return false;
    // All the constraints are trivially true.
    return true;
  }

  assert(!has_pending_constraints() && generators_are_up_to_date());
  // Try a fast-fail test: the universe polyhedron has `space_dim' lines.
  // If there are too few lines and rays we can return `false'
  // without performing minimization.
  const dimension_type num_rays_required = 2*space_dim;

  dimension_type num_lines = 0;
  dimension_type num_rays = 0;
  const dimension_type first_pending = gen_sys.first_pending_row();
  for (dimension_type i = first_pending; i-- > 0; )
    switch (gen_sys[i].type()) {
    case Generator::RAY:
      ++num_rays;
      break;
    case Generator::LINE:
      ++num_lines;
      break;
    default:
      break;
    }

  if (has_pending_generators()) {
    // The seen part of `gen_sys' was minimized.
    if (num_rays == 0 && num_lines == space_dim)
      return true;
    // Now scan the pending generators.
    const dimension_type gs_num_rows = gen_sys.num_rows();
    for (dimension_type i = first_pending; i < gs_num_rows; i++)
      switch (gen_sys[i].type()) {
      case Generator::RAY:
	++num_rays;
	break;
      case Generator::LINE:
	++num_lines;
	break;
      default:
	break;
      }
    if (2*num_lines + num_rays < num_rays_required)
      return false;
  }
  else {
    // There is nothing pending.
    if (generators_are_minimized())
      // The exact test is possible.
      return (num_rays == 0 && num_lines == space_dim);
    else if (2*num_lines + num_rays < num_rays_required)
      return false;
  }

  // We need the polyhedron in minimal form.
  if (has_pending_generators())
    process_pending_generators();
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
      const dimension_type eps_index = con_sys.num_columns() - 1;
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
      || marked_empty()
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
PPL::Polyhedron::is_topologically_closed() const {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return true;
  // Any empty or zero-dimensional polyhedron is closed.
  if (marked_empty()
      || space_dim == 0
      || (has_something_pending() && !process_pending()))
     return true;

  // At this point there are no pending constraints or generators.
  assert(!has_something_pending());

  if (generators_are_minimized()) {
    // A polyhedron is closed iff all of its (non-redundant)
    // closure points are matched by a corresponding point.
    const dimension_type n_rows = gen_sys.num_rows();
    const dimension_type n_lines = gen_sys.num_lines();
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
  return marked_empty() || !con_sys.has_strict_inequalities();
}

bool
PPL::Polyhedron::OK(bool check_not_empty) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // The expected number of columns in the constraint and generator
  // systems, if they are not empty.
  const dimension_type poly_num_columns
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

  // Check whether the status information is legal.
  if (!status.OK())
    goto bomb;

  if (marked_empty()) {
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
      // the polyhedron is not OK.
      ConSys new_con_sys(topology());
      GenSys gs_without_pending = gen_sys;
      // NOTE: We can avoid to update `index_first_pending'
      // of `gs_without_pending', because it is equal to the
      // new number of rows of `gs_without_pending'.
      gs_without_pending.erase_to_end(gen_sys.first_pending_row());
      GenSys copy_of_gen_sys = gs_without_pending;
      SatMatrix new_sat_c;
      minimize(false, copy_of_gen_sys, new_con_sys, new_sat_c);
      const dimension_type copy_num_lines = copy_of_gen_sys.num_lines();
      if (gs_without_pending.num_rows() != copy_of_gen_sys.num_rows()
	  || gs_without_pending.num_lines() != copy_num_lines
	  || gs_without_pending.num_rays() != copy_of_gen_sys.num_rays()) {
#ifndef NDEBUG
	cerr << "Generators are declared minimized, but they are not!\n"
	     << "Here is the minimized form of the generators:\n";
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
	  cerr << "Generators are declared minimized, but they are not!\n"
	       << "(we are in the case:\n"
	       << "dimension of lineality space equal to 0)\n"
	       << "Here is the minimized form of the generators:\n";
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
      const dimension_type eps_index = con_sys.num_columns() - 1;
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
      // If it does not happen, the polyhedron is not OK.
      if (cs_without_pending.num_rows() != copy_of_con_sys.num_rows()
	  || cs_without_pending.num_equalities()
	  != copy_of_con_sys.num_equalities()) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!\n"
	     << "Here is the minimized form of the constraints:\n";
	copy_of_con_sys.ascii_dump(cerr);
	cerr << endl;
#endif
	goto bomb;
      }
#if !GRAM_SHMIDT
      // The matrix `copy_of_con_sys' has the form that is obtained
      // after the functions gauss() and back_substitute().
      // A system of constraints can be minimal even if it does not
      // have this form. So, to verify if the polyhedron is correct,
      // we copy the matrix `con_sys' in a temporary one that then
      // is modified using the functions gauss() and back_substitute().
      // If the temporary matrix and `copy_of_con_sys' are different,
      // the polyhedron is not OK.
      copy_of_con_sys.strong_normalize();
      copy_of_con_sys.sort_rows();
      cs_without_pending.sort_rows();
      cs_without_pending.back_substitute(cs_without_pending.gauss());
      cs_without_pending.strong_normalize();
      cs_without_pending.sort_rows();
      if (cs_without_pending != copy_of_con_sys) {
#ifndef NDEBUG
	cerr << "Constraints are declared minimized, but they are not!\n"
	     << "Here is the minimized form of the constraints:\n";
	copy_of_con_sys.ascii_dump(cerr);
	cerr << endl;
#endif
	goto bomb;
      }
#endif // !GRAM_SHMIDT
    }
  }

  if (sat_c_is_up_to_date())
    for (dimension_type i = sat_c.num_rows(); i-- > 0; ) {
      const Generator tmp_gen = gen_sys[i];
      const SatRow tmp_sat = sat_c[i];
      for (dimension_type j = sat_c.num_columns(); j-- > 0; )
	if (scalar_product_sign(tmp_gen, con_sys[j]) != tmp_sat[j]) {
#ifndef NDEBUG
	  cerr << "sat_c is declared up-to-date, but it is not!"
	       << endl;
#endif
	  goto bomb;
	}
    }

  if (sat_g_is_up_to_date())
    for (dimension_type i = sat_g.num_rows(); i-- > 0; ) {
      const Constraint tmp_con = con_sys[i];
      const SatRow tmp_sat = sat_g[i];
      for (dimension_type j = sat_g.num_columns(); j-- > 0; )
	if (scalar_product_sign(tmp_con, gen_sys[j]) != tmp_sat[j]) {
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
      cerr << "The polyhedron is declared to have pending constraints, "
	   << "but con_sys has no pending rows!"
	   << endl;
#endif
      goto bomb;
    }
  }

  if (has_pending_generators()) {
    if (gen_sys.num_pending_rows() == 0) {
#ifndef NDEBUG
      cerr << "The polyhedron is declared to have pending generators, "
	   << "but gen_sys has no pending rows!"
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

void
PPL::Polyhedron::add_constraint(const Constraint& c) {
  // Topology-compatibility check.
  if (c.is_strict_inequality() && is_necessarily_closed())
    throw_topology_incompatible("add_constraint(c)", "c", c);
  // Dimension-compatibility check:
  // the dimension of `c' can not be greater than space_dim.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("add_constraint(c)", "c", c);

  // Adding a new constraint to an empty polyhedron
  // results in an empty polyhedron.
  if (marked_empty())
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

  const bool adding_pending = can_have_something_pending();

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
  // TODO: this is just an executable specification.
  ConSys cs(c);
  return add_recycled_constraints_and_minimize(cs);
}

void
PPL::Polyhedron::add_generator(const Generator& g) {
  // Topology-compatibility check.
  if (g.is_closure_point() && is_necessarily_closed())
    throw_topology_incompatible("add_generator(g)", "g", g);
  // Dimension-compatibility check:
  // the dimension of `g' can not be greater than space_dim.
  const dimension_type g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("add_generator(g)", "g", g);

  // Dealing with a zero-dimensional space polyhedron first.
  if (space_dim == 0) {
    // It is not possible to create 0-dim rays or lines.
    assert(g.is_point() || g.is_closure_point());
    // Closure points can only be inserted in non-empty polyhedra.
    if (marked_empty())
      if (g.type() != Generator::POINT)
	throw_invalid_generator("add_generator(g)", "g");
      else
	status.set_zero_dim_univ();
    assert(OK());
    return;
  }

  if (marked_empty()
      || (has_pending_constraints() && !process_pending_constraints())
      || (!generators_are_up_to_date() && !update_generators())) {
    // Here the polyhedron is empty:
    // the specification says we can only insert a point.
    if (!g.is_point())
      throw_invalid_generator("add_generator(g)", "g");
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
      const LinExpression nc_expr = LinExpression(g);
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
    const bool has_pending = can_have_something_pending();
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
      const LinExpression nc_expr = LinExpression(g);
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
	throw_runtime_error("add_generator(const Generator& g)");
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
  // TODO: this is just an executable specification.
  GenSys gs(g);
  return add_recycled_generators_and_minimize(gs);
}

void
PPL::Polyhedron::add_recycled_constraints(ConSys& cs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw_topology_incompatible("add_constraints(cs)", "cs", cs);
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_recycled_constraints(cs)", "cs", cs);

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

  if (marked_empty())
    return;

  // The constraints (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (!constraints_are_up_to_date())
    update_constraints();

  // Adjust `cs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  cs.adjust_topology_and_dimension(topology(), space_dim);

  const bool adding_pending = can_have_something_pending();

  // Here we do not require `con_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `cs'
  // (which is not a const).
  const dimension_type old_num_rows = con_sys.num_rows();
  const dimension_type cs_num_rows = cs.num_rows();
  const dimension_type cs_num_columns = cs.num_columns();
  con_sys.grow(old_num_rows + cs_num_rows, con_sys.num_columns());
  for (dimension_type i = cs_num_rows; i-- > 0; ) {
    // NOTE: we cannot directly swap the rows, since they might have
    // different capacities (besides possibly having different sizes):
    // thus, we steal one coefficient at a time.
    Constraint& new_c = con_sys[old_num_rows + i];
    Constraint& old_c = cs[i];
    if (old_c.is_equality())
      new_c.set_is_equality();
    for (dimension_type j = cs_num_columns; j-- > 0; )
      std::swap(new_c[j], old_c[j]);
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

void
PPL::Polyhedron::add_constraints(const ConSys& cs) {
  // TODO: this is just an executable specification.
  ConSys cs_copy = cs;
  add_recycled_constraints(cs_copy);
}

bool
PPL::Polyhedron::add_recycled_constraints_and_minimize(ConSys& cs) {
  // Topology-compatibility check.
  if (is_necessarily_closed() && cs.has_strict_inequalities())
    throw_topology_incompatible("add_recycled_constraints_and_minimize(cs)",
				"cs", cs);
  // Dimension-compatibility check:
  // the dimension of `cs' can not be greater than space_dim.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (space_dim < cs_space_dim)
    throw_dimension_incompatible("add_recycled_constraints_and_minimize(cs)",
				 "cs", cs);

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

  const bool empty = add_and_minimize(true, con_sys, gen_sys, sat_c, cs);

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

bool
PPL::Polyhedron::add_constraints_and_minimize(const ConSys& cs) {
  // TODO: this is just an executable specification.
  ConSys cs_copy = cs;
  return add_recycled_constraints_and_minimize(cs_copy);
}

void
PPL::Polyhedron::add_recycled_generators(GenSys& gs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && gs.has_closure_points())
    throw_topology_incompatible("add_recycled_generators(gs)", "gs", gs);
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dim.
  const dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_recycled_generators(gs)", "gs", gs);

  // Adding no generators is a no-op.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return;
  }

  // Adding valid generators to a zero-dimensional polyhedron
  // transform it in the zero-dimensional universe polyhedron.
  if (space_dim == 0) {
    if (marked_empty() && !gs.has_points())
      throw_invalid_generators("add_recycled_generators(gs)", "gs");
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
      throw_invalid_generators("add_recycled_generators(gs)", "gs");
    // The polyhedron is no longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the constraints
      // of the polyhedron are not up-to-date, the polyhedron cannot
      // have pending generators. By integrating the pending part
      // of `gen_sys' we may loose sortedness.
      gen_sys.unset_pending_rows();
      gen_sys.set_sorted(false);
    }
    set_generators_up_to_date();
    clear_empty();
    assert(OK());
    return;
  }

  const bool adding_pending = can_have_something_pending();

  // Here we do not require `gen_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `gs'
  // (which is not a const).
  const dimension_type old_num_rows = gen_sys.num_rows();
  const dimension_type gs_num_rows = gs.num_rows();
  const dimension_type gs_num_columns = gs.num_columns();
  gen_sys.grow(old_num_rows + gs_num_rows, gen_sys.num_columns());
  for (dimension_type i = gs_num_rows; i-- > 0; ) {
    // NOTE: we cannot directly swap the rows, since they might have
    // different capacities (besides possibly having different sizes):
    // thus, we steal one coefficient at a time.
    Generator& new_g = gen_sys[old_num_rows + i];
    Generator& old_g = gs[i];
    if (old_g.is_line())
      new_g.set_is_line();
    for (dimension_type j = gs_num_columns; j-- > 0; )
      std::swap(new_g[j], old_g[j]);
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
PPL::Polyhedron::add_generators(const GenSys& gs) {
  // TODO: this is just an executable specification.
  GenSys gs_copy = gs;
  add_recycled_generators(gs_copy);
}

bool
PPL::Polyhedron::add_recycled_generators_and_minimize(GenSys& gs) {
  // Topology compatibility check.
  if (is_necessarily_closed() && gs.has_closure_points())
    throw_topology_incompatible("add_recycled_generators_and_minimize(gs)",
				"gs", gs);
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dim.
  const dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_recycled_generators_and_minimize(gs)",
				 "gs", gs);

  // Adding no generators is equivalent to just requiring minimization.
  if (gs.num_rows() == 0) {
    assert(gs.num_columns() == 0);
    return minimize();
  }

  // Adding valid generators to a zero-dimensional polyhedron
  // transform it in the zero-dimensional universe polyhedron.
  if (space_dim == 0) {
    if (marked_empty() && !gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
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

  if (minimize()) {
    obtain_sorted_generators_with_sat_g();
    // This call to `add_and_minimize(...)' cannot return `false'.
    add_and_minimize(false, gen_sys, con_sys, sat_g, gs);
    clear_sat_c_up_to_date();
  }
  else {
    // The polyhedron was empty: check if `gs' contains a point.
    if (!gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
    // `gs' has a point: the polyhedron is no longer empty
    // and generators are up-to-date.
    std::swap(gen_sys, gs);
    clear_empty();
    set_generators_up_to_date();
    // This call to `minimize()' cannot return `false'.
    minimize();
  }
  assert(OK(true));
  return true;
}

bool
PPL::Polyhedron::add_generators_and_minimize(const GenSys& gs) {
  // TODO: this is just an executable specification.
  GenSys gs_copy = gs;
  return add_recycled_generators_and_minimize(gs_copy);
}

void
PPL::Polyhedron::intersection_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("intersection_assign(y)", "y", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("intersection_assign(y)", "y", y);

  // If one of the two polyhedra is empty, the intersection is empty.
  if (x.marked_empty())
    return;
  if (y.marked_empty()) {
    x.set_empty();
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (x.space_dim == 0)
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

bool
PPL::Polyhedron::intersection_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("intersection_assign_and_minimize(y)",
				"y", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("intersection_assign_and_minimize(y)",
				 "y", y);

  // If one of the two polyhedra is empty, the intersection is empty.
  if (x.marked_empty())
    return false;
  if (y.marked_empty()) {
    x.set_empty();
    return false;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (x.space_dim == 0)
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
PPL::Polyhedron::poly_hull_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_hull_assign(y)", "y", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("poly_hull_assign(y)", "y", y);

  // The poly-hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.marked_empty())
    return;
  if (x.marked_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their poly-hull is the universe polyhedron too.
  if (x.space_dim == 0)
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

bool
PPL::Polyhedron::poly_hull_assign_and_minimize(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_hull_assign_and_minimize(y)", "y", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("poly_hull_assign_and_minimize(y)", "y", y);

  // The poly-hull of a polyhedron `p' with an empty polyhedron is `p'.
  if (y.marked_empty())
    return minimize();
  if (x.marked_empty()) {
    x = y;
    return minimize();
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their poly-hull is the universe polyhedron too.
  if (x.space_dim == 0)
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
    add_and_minimize(false, x.gen_sys, x.con_sys, x.sat_g);
  }
  else {
    y.obtain_sorted_generators();
    add_and_minimize(false, x.gen_sys, x.con_sys, x.sat_g, y.gen_sys);
  }
  x.clear_sat_c_up_to_date();

  assert(x.OK(true) && y.OK());
  return true;
}

void
PPL::Polyhedron::poly_difference_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("poly_difference_assign(y)", "y", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("poly_difference_assign(y)", "y", y);

  // The difference of a polyhedron `p' and an empty polyhedron is `p'.
  if (y.marked_empty())
    return;
  // The difference of an empty polyhedron and of a polyhedron `p' is empty.
  if (x.marked_empty())
    return;

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their difference is empty.
  if (x.space_dim == 0) {
    x.set_empty();
    return;
  }

  // TODO: This is just an executable specification.
  //       Have to find a more efficient method.

  if (y.contains(x)) {
    x.set_empty();
    return;
  }

  Polyhedron new_polyhedron(topology(), x.space_dim, EMPTY);

  // Being lazy here is only harmful.
  // `minimize()' will process any pending constraints or generators.
  x.minimize();
  y.minimize();

  const ConSys& y_cs = y.constraints();
  for (ConSys::const_iterator i = y_cs.begin(),
	 y_cs_end = y_cs.end(); i != y_cs_end; ++i) {
    const Constraint& c = *i;
    assert(!c.is_trivial_true());
    assert(!c.is_trivial_false());
    // If the polyhedron `x' is included in the polyhedron defined by
    // `c', then `c' can be skipped, as adding its complement to `x'
    // would result in the empty polyhedron.  Moreover, if we operate
    // on C-polyhedra and `c' is a non-strict inequality, c _must_ be
    // skipped for otherwise we would obtain a result that is less
    // precise than the poly-difference.
    if (x.relation_with(c).implies(Poly_Con_Relation::is_included()))
      continue;
    Polyhedron z = x;
    const LinExpression e = LinExpression(c);
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

void
PPL::Polyhedron::affine_image(const Variable var,
			      const LinExpression& expr,
			      const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_invalid_argument("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type num_var = var.id() + 1;
  if (space_dim < num_var)
    throw_dimension_incompatible("affine_image(v, e, d)", "v", var);

  if (marked_empty())
    return;

  if (num_var <= expr_space_dim && expr[num_var] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved, so that
    // pending rows, if present, are correctly handled.
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
    if (!marked_empty()) {
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
PPL::Polyhedron::affine_preimage(const Variable var,
				 const LinExpression& expr,
				 const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_invalid_argument("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type num_var = var.id() + 1;
  if (space_dim < num_var)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "v", var);

  if (marked_empty())
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
PPL::Polyhedron::generalized_affine_image(const Variable var,
					  const Relation_Symbol relsym,
					  const LinExpression& expr,
					  const Integer& denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_invalid_argument("generalized_affine_image(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "e", expr);
  // `var' should be one of the dimensions of the polyhedron.
  const dimension_type num_var = var.id() + 1;
  if (space_dim < num_var)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "v", var);

  // Strict relation symbols are only admitted for NNC polyhedra.
  if (is_necessarily_closed()
      && (relsym == LESS_THAN || relsym == GREATER_THAN))
    throw_invalid_argument("generalized_affine_image(v, r, e, d)",
			   "r is a strict relation symbol");

  // Any image of an empty polyhedron is empty.
  if (marked_empty())
    return;

  // First compute the affine image.
  affine_image(var, expr, denominator);
  switch (relsym) {
  case LESS_THAN_OR_EQUAL:
    add_generator(ray(-var));
    break;
  case EQUAL:
    // The relation symbol is "==":
    // this is just an affine image computation.
    break;
  case GREATER_THAN_OR_EQUAL:
    add_generator(ray(var));
    break;
  case LESS_THAN:
  // Intentionally fall through.
  case GREATER_THAN:
    {
      // The relation symbol is strict.
      assert(!is_necessarily_closed());
      // While adding the ray, we minimize the generators
      // in order to avoid adding too many redundant generators later.
      // FIXME: why not using add_generator_and_minimize() here?
      GenSys gs;
      gs.insert(ray(relsym == GREATER_THAN ? var : -var));
      add_recycled_generators_and_minimize(gs);
      // We split each point of the generator system into two generators:
      // a closure point, having the same coordinates of the given point,
      // and another point, having the same coordinates for all but the
      // `var' dimension, which is displaced along the direction of the
      // newly introduced ray.
      const dimension_type eps_index = space_dim + 1;
      for (dimension_type i =  gen_sys.num_rows(); i-- > 0; )
	if (gen_sys[i].is_point()) {
	  Generator& g = gen_sys[i];
	  // Add a `var'-displaced copy of `g' to the generator system.
	  gen_sys.add_row(g);
	  if (relsym == GREATER_THAN)
	    ++gen_sys[gen_sys.num_rows()-1][num_var];
	  else
	    --gen_sys[gen_sys.num_rows()-1][num_var];
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
					  const Relation_Symbol relsym,
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
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  // Strict relation symbols are only admitted for NNC polyhedra.
  if (is_necessarily_closed()
      && (relsym == LESS_THAN || relsym == GREATER_THAN))
    throw_invalid_argument("generalized_affine_image(e1, r, e2)",
			   "r is a strict relation symbol");

  // Any image of an empty polyhedron is empty.
  if (marked_empty())
    return;

  // Compute the actual space dimension of `lhs',
  // i.e., the highest dimension having a non-zero coefficient in `lhs'.
  for ( ; lhs_space_dim > 0; lhs_space_dim--)
    if (lhs.coefficient(Variable(lhs_space_dim - 1)) != 0)
      break;
  // If all variables have a zero coefficient, then `lhs' is a constant:
  // we can simply add the constraint `lhs relsym rhs'.
  if (lhs_space_dim == 0) {
    switch (relsym) {
    case LESS_THAN:
      add_constraint(lhs < rhs);
      break;
    case LESS_THAN_OR_EQUAL:
      add_constraint(lhs <= rhs);
      break;
    case EQUAL:
      add_constraint(lhs == rhs);
      break;
    case GREATER_THAN_OR_EQUAL:
      add_constraint(lhs >= rhs);
      break;
    case GREATER_THAN:
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
    const Variable new_var = Variable(space_dim);
    add_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to the right hand side.
    // (we force minimization because we will need the generators).
    // FIXME: why not use add_constraint_and_minimize() here?
    ConSys new_cs1;
    new_cs1.insert(new_var == rhs);
    add_recycled_constraints_and_minimize(new_cs1);

    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the constraints).
    add_recycled_generators_and_minimize(new_lines);

    // Constrain the new dimension so that it is related to
    // the left hand side as dictated by `relsym'
    // (we force minimization because we will need the generators).
    // FIXME: why not use add_constraint_and_minimize() here?
    ConSys new_cs2;
    switch (relsym) {
    case LESS_THAN:
      new_cs2.insert(lhs < new_var);
      break;
    case LESS_THAN_OR_EQUAL:
      new_cs2.insert(lhs <= new_var);
      break;
    case EQUAL:
      new_cs2.insert(lhs == new_var);
      break;
    case GREATER_THAN_OR_EQUAL:
      new_cs2.insert(lhs >= new_var);
      break;
    case GREATER_THAN:
      new_cs2.insert(lhs > new_var);
      break;
    }
    add_recycled_constraints_and_minimize(new_cs2);

    // Remove the temporarily added dimension.
    remove_higher_dimensions(space_dim-1);
  }
  else {
    // `lhs' and `rhs' variables are disjoint:
    // there is no need to add a further dimension.

    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the constraints).
    add_recycled_generators_and_minimize(new_lines);

    // Constrain the left hand side expression so that it is related to
    // the right hand side expression as dictated by `relsym'.
    switch (relsym) {
    case LESS_THAN:
      add_constraint(lhs < rhs);
      break;
    case LESS_THAN_OR_EQUAL:
      add_constraint(lhs <= rhs);
      break;
    case EQUAL:
      add_constraint(lhs == rhs);
      break;
    case GREATER_THAN_OR_EQUAL:
      add_constraint(lhs >= rhs);
      break;
    case GREATER_THAN:
      add_constraint(lhs > rhs);
      break;
    }
  }

  assert(OK());
}

void
PPL::Polyhedron::time_elapse_assign(const Polyhedron& y) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("time_elapse_assign(y)", "y", y);
  // Dimension-compatibility checks.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("time_elapse_assign(y)", "y", y);

  // Dealing with the zero-dimensional case.
  if (x.space_dim == 0) {
    if (y.marked_empty())
      x.set_empty();
    return;
  }

  // If either one of `x' or `y' is empty, the result is empty too.
  if (x.marked_empty() || y.marked_empty()
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
  // which cannot be transformed into a valid ray or line.
  // For NNC polyhedra, also erase all the points of `gs',
  // whose role can be played by the closure points.
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

void
PPL::Polyhedron::topological_closure_assign() {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return;
  // Any empty or zero-dimensional polyhedron is closed.
  if (marked_empty() || space_dim == 0)
    return;

  // The computation can be done using constraints or generators.
  // If we use constraints, we will change them, so that having pending
  // constraints would be useless. If we use generators, we add generators,
  // so that having pending generators still makes sense.

  // Process any pending constraints.
  if (has_pending_constraints() && !process_pending_constraints())
    return;

  // Use constraints only if they are available and
  // there are no pending generators.
  if (!has_pending_generators() && constraints_are_up_to_date()) {
    const dimension_type eps_index = space_dim + 1;
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

/*! \relates Parma_Polyhedra_Library::Polyhedron */
bool
PPL::operator==(const Polyhedron& x, const Polyhedron& y) {
  // If the two polyhedra are topology-incompatible or dimension-incompatible,
  // then they cannot be the same polyhedron.
  if (x.topology() != y.topology() || x.space_dim != y.space_dim)
    return false;

  if (x.marked_empty())
    return y.is_empty();
  else if (y.marked_empty())
    return x.is_empty();
  else if (x.space_dim == 0)
    return true;

  switch (x.quick_equivalence_test(y)) {
  case Polyhedron::TVB_TRUE:
    return true;

  case Polyhedron::TVB_FALSE:
    return false;

  default:
    if (x.is_included_in(y))
      if (x.marked_empty())
	return y.is_empty();
      else
	return y.is_included_in(x);
    else
      return false;
  }
}

/*! \relates Parma_Polyhedra_Library::Polyhedron */
bool
PPL::Polyhedron::contains(const Polyhedron& y) const {
  const Polyhedron& x = *this;

  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("contains(y)", "y", y);

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("contains(y)", "y", y);

  if (y.marked_empty())
    return true;
  else if (x.marked_empty())
    return y.is_empty();
  else if (y.space_dim == 0)
    return true;
  else if (x.quick_equivalence_test(y) == Polyhedron::TVB_TRUE)
    return true;
  else
    return y.is_included_in(x);
}

bool
PPL::Polyhedron::is_disjoint_from(const Polyhedron& y) const {
  Polyhedron z = *this;
  z.intersection_assign_and_minimize(y);
  return z.is_empty();
}

void
PPL::Polyhedron::ascii_dump(std::ostream& s) const {
  using std::endl;

  s << "space_dim "
    << space_dim
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

/*! \relates Parma_Polyhedra_Library::Polyhedron */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Polyhedron& ph) {
  if (ph.is_empty())
    s << "false";
  else
    s << ph.minimized_constraints();
  return s;
}
