/* Grid class implementation (non-inline public functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "Topology.hh"

#include <cassert>
#include <iostream>

#ifndef ENSURE_SORTEDNESS
#define ENSURE_SORTEDNESS 0
#endif

namespace PPL = Parma_Polyhedra_Library;

PPL::Grid::Grid(dimension_type num_dimensions,
		const Degenerate_Kind kind) {
  if (num_dimensions > max_space_dimension())
    throw_space_dimension_overflow("Grid(n, k)",
				   "n exceeds the maximum "
				   "allowed space dimension");

  // FIX is this ok if oom/excptn?
  con_sys = Congruence_System();
  gen_sys = Generator_System(NECESSARILY_CLOSED);

  if (kind == EMPTY)
    status.set_empty();
  space_dim = num_dimensions;
  if (num_dimensions > 0) {
    add_low_level_congruences(con_sys);
    con_sys.adjust_space_dimension(num_dimensions);
    // FIX where will gen_sys space dim be adjusted?
    if (kind == UNIVERSE) {
      // Initialise both systems to universe representations.
      set_congruences_minimized();
      set_congruences_up_to_date();
      // FIX add gs::adjust_space_dimension?
      //gen_sys.adjust_space_dimension(num_dimensions);
      gen_sys.adjust_topology_and_space_dimension(NECESSARILY_CLOSED,
						  num_dimensions);
      set_generators_minimized();
      con_sys.add_zero_rows(num_dimensions + 1, Row::Flags());
      gen_sys.add_zero_rows(num_dimensions + 1,
			    Linear_Row::Flags(NECESSARILY_CLOSED,
					      Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
      Generator& first_g = gen_sys[0];
      first_g[0] = 1;
      first_g.set_is_ray_or_point();
      Congruence& first_cg = con_sys[0];
      first_cg[0] = 1;
      first_cg[num_dimensions + 1] = 1; // Modulus.
      do {
	Generator& g = gen_sys[num_dimensions];
	g[num_dimensions] = 1;
	g.set_is_line();
	Congruence& cg = con_sys[num_dimensions];
	cg[num_dimensions] = 1;
	cg.set_is_virtual();
      }
      while (num_dimensions-- > 1);
      gen_sys.unset_pending_rows();
      gen_sys.set_sorted(false);
    }
  }
  assert(OK());
}

PPL::Grid::Grid(const Grid& y)
  : con_sys(),
    gen_sys(y.gen_sys.topology()),
    status(y.status),
    space_dim(y.space_dim) {
  if (y.congruences_are_up_to_date())
    //con_sys.assign_with_pending(y.con_sys); // FIX
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys.assign_with_pending(y.gen_sys);
}

#if 0

PPL::dimension_type
PPL::Grid::affine_dimension() const {
  if (is_empty())
    return 0;

  const Congruence_System& cs = minimized_congruences();
  dimension_type d = space_dim;
  for (Congruence_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i)
    if (i->is_equality())
      --d;
  return d;
}

#endif

const PPL::Congruence_System&
PPL::Grid::congruences() const {
  if (marked_empty())
    return con_sys;

  if (space_dim == 0) {
    // zero-dimensional universe.
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    return con_sys;
  }

  // FIX
  // If the polyhedron has pending generators, we process them to obtain
  // the congruences. No processing is needed if the polyhedron has
  // pending congruences.
#if 0
  if (has_pending_generators())
    process_pending_generators();
  else
#endif
    if (!congruences_are_up_to_date())
      update_congruences();

  return con_sys;
}

const PPL::Congruence_System&
PPL::Grid::minimized_congruences() const {
  // `minimize()' or `strongly_minimize_congruences()'
  // will process any pending congruences or generators.
  minimize();
#if 0
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_congruences();
#endif
  return congruences();
}

const PPL::Generator_System&
PPL::Grid::generators() const {
  if (marked_empty()) {
    assert(gen_sys.num_rows() == 0);
    // We want `gen_sys' to have the appropriate space dimension,
    // even though it is an empty generator system.
    if (gen_sys.space_dimension() != space_dim) {
      Generator_System gs;
      //gs.adjust_topology_and_space_dimension(topology(), space_dim); // FIX
      gs.adjust_topology_and_space_dimension(gs.topology(), space_dim);
      const_cast<Generator_System&>(gen_sys).swap(gs);
    }
    return gen_sys;
  }

  if (space_dim == 0) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return Generator_System::zero_dim_univ();
  }

#if 0
  // If the polyhedron has pending congruences, we process them to obtain
  // the generators (we may discover that the polyhedron is empty).
  // No processing is needed if the polyhedron has pending generators.
  if ((has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !update_generators())) {
    // We have just discovered that `*this' is empty.
    assert(gen_sys.num_rows() == 0);
    // We want `gen_sys' to have the appropriate space dimension,
    // even though it is an empty generator system.
    if (gen_sys.space_dimension() != space_dim) {
      Generator_System gs;
      gs.adjust_topology_and_space_dimension(topology(), space_dim);
      const_cast<Generator_System&>(gen_sys).swap(gs);
    }
    return gen_sys;
  }
#endif

  return gen_sys;
}

const PPL::Generator_System&
PPL::Grid::minimized_generators() const {
#if 0
  // `minimize()' or `strongly_minimize_generators()'
  // will process any pending congruences or generators.
  if (is_necessarily_closed())
    minimize();
  else
    strongly_minimize_generators();
  // Note: calling generators() on a strongly minimized NNC generator
  // system will also ensure sortedness, which is required to correctly
  // filter away the matched closure points.
#endif
  minimize();
  return generators();
}
#if 0
PPL::Poly_Con_Relation
PPL::Grid::relation_with(const Congruence& c) const {
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
	// The congruence 0 > 0 implicitly defines the hyperplane 0 = 0;
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
      // neither the positivity congruence 1 >= 0,
      // nor the strict positivity congruence 1 > 0.
      return Poly_Con_Relation::is_included();

  if ((has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !update_generators()))
    // The polyhedron is empty.
    return Poly_Con_Relation::saturates()
      && Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

    return gen_sys.relation_with(c);
}

PPL::Poly_Gen_Relation
PPL::Grid::relation_with(const Generator& g) const {
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
  else if (!congruences_are_up_to_date())
    update_congruences();

  return
    con_sys.satisfies_all_congruences(g)
    ? Poly_Gen_Relation::subsumes()
    : Poly_Gen_Relation::nothing();
}

bool
PPL::Grid::is_universe() const {
  if (marked_empty())
    return false;

  // FIX correct?
  if (space_dim == 0)
    return true;

  if (!has_pending_generators() && congruences_are_up_to_date()) {
    if (has_pending_congruences()) {
      Grid& x = const_cast<Grid&>(*this);
      if (x.simplify(x.con_sys)) {
	x.set_empty();
	return false;
      }
      x.set_congruences_minimized();
    }

    // Compare congruences to the universe representation.

    dimension_type size = con_sys.size();
    if (size == 0)
      return false;
    // Check if the first row has 1 in the first and last elements,
    // and 0 in the others.
    Congruence& cg = con_sys[0];
    if (cg[0] == 1 && cg[size] == 1) {
      for (dimension_type col = 1; col < size - 1; ++col)
	if (col != 0)
	  return false;
    }
    else
      return false;
    // Check if all subsequent rows are virtual.
    Congruence_System::const_iterator row = con_sys.begin();
    while (row != con_sys.end())
      if (row++.is_virtual() == false)
	return false;
    return true;
  }

  assert(!has_pending_congruences() && generators_are_up_to_date());

  if (has_pending_generators()) {
    Grid& x = const_cast<Grid&>(*this);
    // FIX check return and set empty?
    x.simplify(x.gen_sys);
    x.set_generators_minimized();
  }

  // Compare generators to the universe representation.

  dimension_type size = gen_sys.size();
  if (size == 0)
    return false;
  // Check the first row.
  Generator& g = gen_sys[0];
  if (g[0] == 1 && g[size] == 1) {
    for (dimension_type col = 1; col < size - 1; ++col)
      if (col != 0)
	return false;
  }
  else
    return false;
  // Check if all subsequent rows are virtual.
  Generator_System::const_iterator row = gen_sys.begin();
  while (row != con_sys.end())
    if (row++.is_virtual() == false)
      return false;
  return true;
}

bool
PPL::Grid::is_bounded() const {
  // A zero-dimensional or empty polyhedron is bounded.
  if (space_dim == 0
      || marked_empty()
      || (has_pending_congruences() && !process_pending_congruences())
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
PPL::Grid::is_topologically_closed() const {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return true;
  // Any empty or zero-dimensional polyhedron is closed.
  if (marked_empty()
      || space_dim == 0
      || (has_something_pending() && !process_pending()))
     return true;

  // At this point there are no pending congruences or generators.
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
  // of its congruence system, it has no strict inequalities.
  strongly_minimize_congruences();
  return marked_empty() || !con_sys.has_strict_inequalities();
}
#endif

bool
PPL::Grid::OK(bool check_not_empty) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // Check whether the topologies of `con_sys' and `gen_sys' agree.
  if (gen_sys.topology() == NOT_NECESSARILY_CLOSED) {
#ifndef NDEBUG
    cerr << "Generator system should be necessarily closed." << endl;
#endif
    goto fail;
  }

  // Check whether the status information is legal.
  if (status.OK() == false)
    goto fail;

  if (marked_empty()) {
    if (check_not_empty) {
      // The caller does not want the grid to be empty.
#ifndef NDEBUG
      cerr << "Empty polyhedron!" << endl;
#endif
      goto fail;
    }

#if 0 // FIX pending

    // An empty polyhedron is allowed if the system of congruences
    // either has no rows or only contains an unsatisfiable congruence
    // and if it has no pending congruences or generators.
    if (has_something_pending()) {
#ifndef NDEBUG
      cerr << "The polyhedron is empty, "
	   << "but it has something pending" << endl;
#endif
      goto fail;
    }
#endif // 0 FIX pending
    if (con_sys.num_rows() != 0 // FIX check
	&& con_sys.space_dimension() != space_dim) {
#ifndef NDEBUG
      cerr << "The grid is in a space of dimension " << space_dim
	   << " while the system of congruences is in a space of dimension "
	   << con_sys.space_dimension()
	   << endl;
#endif
      goto fail;
    }
    return true;
  }

  // A zero-dimensional, non-empty grid is legal only if the system of
  // congruence `con_sys' and the system of generators `gen_sys' have
  // no rows.
  if (space_dim == 0) {
    if (has_something_pending()) {
#ifndef NDEBUG
      cerr << "Zero-dimensional polyhedron with something pending"
	   << endl;
#endif
      goto fail;
    }
    if (con_sys.num_rows() != 0 || gen_sys.num_rows() != 0) {
#ifndef NDEBUG
      cerr << "Zero-dimensional grid with a non-empty" << endl
	   << "system of congruences or generators." << endl;
#endif
      goto fail;
    }
    return true;
  }

  // A grid is defined by a system of congruences or a system of
  // generators.  At least one of them must be up to date.
  if (!congruences_are_up_to_date() && !generators_are_up_to_date()) {
#ifndef NDEBUG
    cerr << "Grid not empty, not zero-dimensional"
	 << endl
	 << "and with neither congruences nor generators up-to-date!"
	 << endl;
#endif
    goto fail;
  }

  // The expected number of columns in the congruence and generator
  // systems, if they are not empty.
  const dimension_type num_columns = space_dim + 1;

  // Here we check if the size of the matrices is consistent.
  // Let us suppose that all the matrices are up-to-date; this means:
  // `con_sys' : number of congruences x poly_num_columns
  // `gen_sys' : number of generators  x poly_num_columns
  if (congruences_are_up_to_date())
    if (con_sys.num_columns() != num_columns + 1 /* moduli */) {
#ifndef NDEBUG
      cerr << "Incompatible size! (con_sys and space_dim)"
	   << endl;
#endif
      goto fail;
    }

  if (generators_are_up_to_date()) {
    if (gen_sys.num_columns() != num_columns) {
#ifndef NDEBUG
      cerr << "Incompatible size! (gen_sys and space_dim)"
	   << endl;
#endif
      goto fail;
    }

    // Check if the system of generators is well-formed.  Check by
    // hand, as many valid characteristics of a parameter system will
    // fail Generator_System::OK.
    if (gen_sys.Linear_System::OK(false) == false) {
#ifndef NDEBUG
      cerr << "gen_sys Linear_System::OK failed." << endl;
#endif
      goto fail;
    }
    // Check each generator in the system.
    for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
      const Generator& g = gen_sys[i];

      if (g.is_necessarily_closed() == false) {
#ifndef NDEBUG
	cerr << "Parameter should be necessarily closed."
	     << endl;
#endif
	goto fail;
      }

      if (g.size() < 1) {
#ifndef NDEBUG
	cerr << "Parameter should have coefficients." << endl;
#endif
	goto fail;
      }
    }

    // FIX gen_sys.f_p_r == size instead?
    if (gen_sys.first_pending_row() == 0) {
#ifndef NDEBUG
      cerr << "Up-to-date generator system with all rows pending!"
	   << endl;
#endif
      goto fail;
    }

    // A non-empty system of generators describing a grid is valid iff
    // it contains a point.
    if (gen_sys.num_rows() > 0 && !gen_sys.has_points()) {
#ifndef NDEBUG
      cerr << "Non-empty generator system declared up-to-date "
	   << "has no points!"
	   << endl;
#endif
      goto fail;
    }

    if (generators_are_minimized()) {
      Generator_System gs = gen_sys;
      // Leave `index_first_pending' as it is in `gs', because it is
      // equal to the new number of rows in `gs'.
      gs.erase_to_end(gen_sys.first_pending_row());

      // A reduced parameter system must be upper triangular.
      if (upper_triangular(gs) == false) {
#ifndef NDEBUG
	cerr << "Reduced parameters should be upper triangular." << endl;
#endif
	goto fail;
      }

      // A reduced parameter system must be the same as a temporary
      // reduced copy.
      gs.unset_pending_rows();
      Generator_System gs_copy = gs;
      simplify(gs);
      for (dimension_type row = 0; row < gs_copy.num_rows(); ++row) {
	Generator& g = gs[row];
	Generator& g_copy = gs_copy[row];
	dimension_type col = gs_copy.num_columns();
	if (g.type() != g_copy.type())
	  goto message_fail;
	while (col--) {
	  if (g[col] == g_copy[col])
	    continue;
	message_fail:
#ifndef NDEBUG
	  cerr << "Parameters are declared minimized, but they change under reduction.\n"
	       << "Here is the parameter system:\n";
	  gs_copy.ascii_dump(cerr);
	  cerr << "and here is the minimized form of the temporary copy:\n";
	  gs.ascii_dump(cerr);
#endif
	  goto fail;
	}
      }
    }
  }

  if (congruences_are_up_to_date()) {
    // Check if the system of congruences is well-formed.
    if (!con_sys.OK()) {
#ifndef NDEBUG
      cerr << "con_sys OK failed." << endl;
#endif
      goto fail;
    }

#if 0 // FIX pending
    if (con_sys.first_pending_row() == 0) {
#ifndef NDEBUG
      cerr << "Up-to-date congruence system with all rows pending!"
	   << endl;
#endif
      goto fail;
    }
#endif

    Congruence_System cs = con_sys;
#if 0 // FIX pending
    // NOTE: We can avoid to update `index_first_pending'
    // of `cs_without_pending', because it is equal to the
    // new number of rows of `cs_without_pending'.
    cs.erase_to_end(con_sys.first_pending_row());
#endif
    Congruence_System cs_copy = cs;
    Generator_System new_gen_sys(NECESSARILY_CLOSED);

    if (minimize(cs_copy, new_gen_sys)) {
      if (check_not_empty) {
	// Want to know the satisfiability of the congruences.
#ifndef NDEBUG
	cerr << "Insoluble system of congruences!"
	     << endl;
#endif
	goto fail;
      }
      // The grid is empty, all checks are done.
      return true;
    }

    if (congruences_are_minimized()) {
      // FIX pending  (both below) check minimized part of gs copy

      // A reduced congruence system must be lower triangular.
      if (lower_triangular(con_sys) == false) {
#ifndef NDEBUG
	cerr << "Reduced congruences should be lower triangular." << endl;
#endif
	goto fail;
      }

      // If the congruences are minimized, all the elements in the
      // congruence system must be the same as those in the temporary,
      // minimized system `cs_copy'.
      for (dimension_type row = 0; row < cs_copy.num_rows(); ++row)
	for (dimension_type col = 0; col < cs_copy.num_columns(); ++col) {
	  if (cs[row][col] == cs_copy[row][col])
	    continue;
#ifndef NDEBUG
	  cerr << "Generators are declared minimized, but they change under reduction!"
	       << endl
	       << "Here is the minimized form of the generator system:"
	       << endl;
	  cs.ascii_dump(cerr);
	  cerr << endl;
#endif
	  goto fail;
	}
    }
  }

#if 0 // FIX pending
  if (has_pending_congruences()
      && con_sys.num_pending_rows() == 0) {
#ifndef NDEBUG
      cerr << "The polyhedron is declared to have pending congruences, "
	   << "but con_sys has no pending rows!"
	   << endl;
#endif
      goto fail;
    }
#endif

  if (has_pending_generators()
      && gen_sys.num_pending_rows() == 0) {
#ifndef NDEBUG
      cerr << "The polyhedron is declared to have pending generators, "
	   << "but gen_sys has no pending rows!"
	   << endl;
#endif
    }
  else
    return true;

 fail:
#ifndef NDEBUG
  cerr << "Here is the grid under check:" << endl;
  ascii_dump(cerr);
#endif
  return false;
}

void
PPL::Grid::add_congruence(const Congruence& cg) {
  // Dimension-compatibility check: the dimension of `cg' can not be
  // greater than space_dim.
  if (space_dim < cg.space_dimension())
    throw_dimension_incompatible("add_congruence(cg)", "cg", cg);

  // Adding a new congruence to an empty polyhedron results in an
  // empty polyhedron.
  if (marked_empty())
    return;

  // Dealing with a zero-dimensional space polyhedron first.
  if (space_dim == 0) {
    if (!cg.is_trivial_true())
      set_empty();
    return;
  }

  // The congruences (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (!congruences_are_up_to_date())
    update_congruences();

  con_sys.insert(cg);

#if 0 // FIX pending
  const bool adding_pending = can_have_something_pending();

  // Here we know that the system of congruences has at least a row.
  if (cg.is_necessarily_closed() || !is_necessarily_closed())
    // Since `con_sys' is not empty, the topology and space dimension
    // of the inserted congruence are automatically adjusted.
    if (adding_pending)
      con_sys.insert_pending(cg);
    else
      con_sys.insert(cg);
  else {
    // Note: here we have a _legal_ topology mismatch, because
    // `cg' is NOT a strict inequality.
    // However, by invoking `con_sys.insert(cg)' we would
    // cause a change in the topology of `con_sys', which is wrong.
    // Thus, we insert a "topology corrected" copy of `cg'.
    Linear_Expression nc_expr = Linear_Expression(cg);
    if (cg.is_equality())
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
    set_congruences_pending();
  else {
    // Congruences are not minimized and generators are not up-to-date.
    clear_congruences_minimized();
    clear_generators_up_to_date();
  }
#endif
  // Note: the congruence system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

bool
PPL::Grid::add_congruence_and_minimize(const Congruence& cg) {
  // TODO: this is just an executable specification.
  Congruence_System cgs(cg);
  return add_recycled_congruences_and_minimize(cgs);
}

#if 0
void
PPL::Grid::add_generator(const Generator& g) {
  // Topology-compatibility check.
  if (g.is_closure_point())
    throw_topology_incompatible("add_generator(g)", "g", g);
  // FIX handle nnc g
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
      || (has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !update_generators())) {
    // Here the polyhedron is empty:
    // the specification says we can only insert a point.
    if (!g.is_point())
      throw_invalid_generator("add_generator(g)", "g");
    if (g.is_necessarily_closed() || !is_necessarily_closed()) {
      gen_sys.insert(g);
      // Since `gen_sys' was empty, after inserting `g' we have to resize
      // the system of generators to have the right dimension.
      gen_sys.adjust_topology_and_space_dimension(topology(), space_dim);
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
      const Linear_Expression nc_expr = Linear_Expression(g);
      gen_sys.insert(Generator::point(nc_expr, g.divisor()));
      // Since `gen_sys' was empty, after inserting `g' we have to resize
      // the system of generators to have the right dimension.
      gen_sys.adjust_topology_and_space_dimension(topology(), space_dim);
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
      const Linear_Expression nc_expr = Linear_Expression(g);
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
      // congruences are no longer up-to-date.
      clear_generators_minimized();
      clear_congruences_up_to_date();
    }
  }
  assert(OK());
}
#endif
bool
PPL::Grid::add_generator_and_minimize(const Generator& g) {
  // TODO: this is just an executable specification.
  Generator_System gs(g);
  return add_recycled_generators_and_minimize(gs);
}

void
PPL::Grid::add_recycled_congruences(Congruence_System& cgs) {
  // Dimension-compatibility check: the dimension of `cgs' can not be
  // greater than space_dim.
  const dimension_type cgs_space_dim = cgs.space_dimension();
  if (space_dim < cgs_space_dim)
    throw_dimension_incompatible("add_recycled_congruences(cgs)", "cgs", cgs);

  if (cgs.num_rows() == 0)
    return;

  if (space_dim == 0) {
    // FIX
    // In a 0-dimensional space the congruences are trivial (e.g., 0
    // == 0 or 1 >= 0 or 1 > 0) or inconsistent (e.g., 1 == 0 or -1 >=
    // 0 or 0 > 0).  In a system of congruences `begin()' and `end()'
    // are equal if and only if the system contains trivial
    // congruences only.
    if (cgs.begin() != cgs.end())
      // There is a congruence, it must be inconsistent, the
      // polyhedron is empty.
      status.set_empty();
    return;
  }

#if 0
  if (marked_empty())
    return;
#endif

  // The congruences (possibly with pending rows) are required.
  if (has_pending_generators())
    process_pending_generators();
  else if (marked_empty() == false
	   && !congruences_are_up_to_date())
    update_congruences();

  // FIX Added for Grid.
  if (marked_empty()) {
    clear_empty();
    set_congruences_up_to_date();
  }

  // Adjust `cgs' to the right topology and space dimension.
  // NOTE: we have already checked for topology compatibility.
  //cgs.adjust_topology_and_space_dimension(topology(), space_dim); // FIX
  cgs.adjust_space_dimension(space_dim);

  //const bool adding_pending = can_have_something_pending();
  const bool adding_pending = false;

  // _Swap_ (instead of copying) the coefficients of `cgs' (which is
  // not a const).
  const dimension_type old_num_rows = con_sys.num_rows();
  const dimension_type cgs_num_rows = cgs.num_rows();
  const dimension_type cgs_num_columns = cgs.num_columns();
  con_sys.add_zero_rows(cgs_num_rows, Row::Flags());
  for (dimension_type i = cgs_num_rows; i-- > 0; ) {
    // Steal one coefficient at a time, since the rows might have
    // different capacities (besides possibly having different sizes)
    Congruence& new_cg = con_sys[old_num_rows + i];
    Congruence& old_cg = cgs[i];
    for (dimension_type j = cgs_num_columns; j-- > 0; )
      std::swap(new_cg[j], old_cg[j]);
  }

  if (adding_pending)
    set_congruences_pending();
  else {
#if 0
    // The newly added ones are not pending congruences.
    con_sys.unset_pending_rows();
    // They have been simply appended.
    con_sys.set_sorted(false);
#endif
    // Congruences are not minimized and generators are not up-to-date.
    clear_congruences_minimized();
    clear_generators_up_to_date();
  }
  // Note: the congruence system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

void
PPL::Grid::add_congruences(const Congruence_System& cgs) {
  // TODO: this is just an executable specification.
  Congruence_System cgs_copy = cgs;
  add_recycled_congruences(cgs_copy);
}

bool
PPL::Grid::add_recycled_congruences_and_minimize(Congruence_System& cgs) {
  // Dimension-compatibility check: the dimension of `cgs' can not be
  // greater than space_dim.
  const dimension_type cgs_space_dim = cgs.space_dimension();
  if (space_dim < cgs_space_dim)
    throw_dimension_incompatible("add_recycled_congruences_and_minimize(cgs)",
				 "cgs", cgs);

  // Adding no congruences: just minimize.
  if (cgs.num_rows() == 0)
    return minimize();

  // Dealing with zero-dimensional space polyhedra first.
  if (space_dim == 0) {
    // In a 0-dimensional space the congruences are
    // trivial (e.g., 0 == 0 or 1 >= 0 or 1 > 0) or
    // inconsistent (e.g., 1 == 0 or -1 >= 0 or 0 > 0).
    // In a system of congruences `begin()' and `end()' are equal
    // if and only if the system contains trivial congruences only.
    if (cgs.begin() == cgs.end())
      return true;
    // There is a congruence, it must be inconsistent, the polyhedron
    // is empty.
    status.set_empty();
    return false;
  }

#if 0
  // The grid must be minimized and have sorted congruences.
  // `minimize()' will process any pending congruences or generators.
  if (!minimize())
    // We have just discovered that `x' is empty.
    return false;

  // Fully sort the system of congruences to be added (before
  // adjusting dimensions, in order to save time).
  if (cgs.num_pending_rows() > 0) {
    cgs.unset_pending_rows();
    cgs.sort_rows();
  }
  else if (!cgs.is_sorted())
    cgs.sort_rows();
#endif

  // Adjust `cgs' to the right topology and space dimension.
  // NOTE: we already checked for topology compatibility.
  //cgs.adjust_topology_and_space_dimension(topology(), space_dim); // FIX
  cgs.adjust_space_dimension(space_dim); // FIX

  if (add_and_minimize(con_sys, gen_sys, cgs)) {
    set_empty();

    assert(OK());
    return false;
  }

  // FIX added for grid
  set_congruences_up_to_date();
  clear_empty();

  assert(OK());
  return true;
}

bool
PPL::Grid::add_congruences_and_minimize(const Congruence_System& cgs) {
  // TODO: this is just an executable specification.
  Congruence_System cgs_copy = cgs;
  return add_recycled_congruences_and_minimize(cgs_copy);
}

void
PPL::Grid::add_recycled_generators(Generator_System& gs) {
  // Topology compatibility check.
  if (gs.has_closure_points())
    throw_topology_incompatible("add_recycled_generators(gs)", "gs", gs);
  // FIX handle nnc gs
  // Dimension-compatibility check:
  // the dimension of `gs' can not be greater than space_dim.
  const dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_recycled_generators(gs)", "gs", gs);

  // Adding no generators is a no-op.
  if (gs.num_rows() == 0)
    return;

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
  //gs.adjust_topology_and_space_dimension(topology(), space_dim);
  gs.adjust_topology_and_space_dimension(gs.topology(), space_dim); // FIX
#if 0
  // For NNC polyhedra, each point must be matched by
  // the corresponding closure point.
  if (!is_necessarily_closed())
    gs.add_corresponding_closure_points();
#endif

  // The generators (possibly with pending rows) are required.
  if ((has_pending_congruences() && !process_pending_congruences())
      || (!generators_are_up_to_date() && !minimize())) {
    // We have just discovered that `*this' is empty.
    // So `gs' must contain at least one point.
    if (!gs.has_points())
      throw_invalid_generators("add_recycled_generators(gs)", "gs");
    // The polyhedron is no longer empty and generators are up-to-date.
    normalize_divisors(gs);
    //std::swap(gen_sys, parameterize(gs, gs[0])); // FIX
    std::swap(gen_sys, gs);
    if (gen_sys.num_pending_rows() > 0) {
      // Even though `gs' has pending generators, since the congruences
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

#if 0
  // FIX this is messy; duplicated below
  dimension_type row = 0;
  TEMP_INTEGER(gen_sys_divisor);
  dimension_type num_rows = gen_sys.num_rows();
  // Find first point in gen_sys.
  while (gen_sys[row].is_line_or_equality())
    if (++row == num_rows) {
      // All rows are lines.
      gen_sys_divisor = 0;
      goto normalize;
    }
  gen_sys_divisor = gen_sys[row].divisor();
 normalize:
  TEMP_INTEGER(divisor);
  divisor = normalize_divisors(gs, gen_sys_divisor);
  if (divisor != gen_sys_divisor)
    // FIX this call can skip the lcm calc
    normalize_divisors(gen_sys, divisor);
  parameterize(gs, gen_sys[row], false);
#endif

  const bool adding_pending = can_have_something_pending();

  // Here we do not require `gen_sys' to be sorted.
  // also, we _swap_ (instead of copying) the coefficients of `gs'
  // (which is not a const).
  const dimension_type old_num_rows = gen_sys.num_rows();
  const dimension_type gs_num_rows = gs.num_rows();
  const dimension_type gs_num_columns = gs.num_columns();
  gen_sys.add_zero_rows(gs_num_rows,
			Linear_Row::Flags(gs.topology(),
					  Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
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
    // Congruences are not up-to-date and generators are not minimized.
    clear_congruences_up_to_date();
    clear_generators_minimized();
  }
  //clear_congruences_minimized(); // FIX?
  assert(OK(true));
}

void
PPL::Grid::add_generators(const Generator_System& gs) {
  // TODO: this is just an executable specification.
  Generator_System gs_copy = gs;
  add_recycled_generators(gs_copy);
}

bool
PPL::Grid::add_recycled_generators_and_minimize(Generator_System& gs) {
  // Topology compatibility check.
  if (gs.has_closure_points())
    throw_topology_incompatible("add_recycled_generators_and_minimize(gs)",
				"gs", gs);
  // FIX handle nnc gs
  // Dimension-compatibility check: the dimension of `gs' must be less
  // than or equal to that of space_dim.
  const dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_recycled_generators_and_minimize(gs)",
				 "gs", gs);

  // Adding no generators is equivalent to just requiring reduction.
  if (gs.num_rows() == 0)
    return minimize();

  // FIX
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

#if 0
  // Adjust `gs' to the right topology.
  // NOTE: we already checked for topology compatibility;
  // also, we do NOT adjust dimensions now, so that we will
  // spend less time to sort rows.
  gs.adjust_topology_and_space_dimension(topology(), gs_space_dim);

  // For NNC polyhedra, each point must be matched by
  // the corresponding closure point.
  if (!is_necessarily_closed())
    gs.add_corresponding_closure_points();
#endif

  // `gs' has to be fully sorted, thus it cannot have pending rows.
  if (gs.num_pending_rows() > 0) {
    gs.unset_pending_rows();
    gs.sort_rows();
  }
  else if (!gs.is_sorted())
    gs.sort_rows();

  // Now adjusting dimensions (topology already adjusted).
  // NOTE: sortedness is preserved.
  //gs.adjust_topology_and_space_dimension(topology(), space_dim); // FIX
  gs.adjust_topology_and_space_dimension(gs.topology(), space_dim);

#if 0
  // FIX this is messy; duplicated above
  dimension_type row = 0;
  TEMP_INTEGER(gen_sys_divisor);
  dimension_type num_rows = gen_sys.num_rows();
  // Find first point in gen_sys.
  while (gen_sys[row].is_line_or_equality())
    if (++row == num_rows) {
      // All rows are lines.
      gen_sys_divisor = 0;
      goto normalize;
    }
  gen_sys_divisor = gen_sys[row].divisor();
 normalize:
  TEMP_INTEGER(divisor);
  divisor = normalize_divisors(gs, gen_sys_divisor);
  if (divisor != gen_sys_divisor)
    // FIX this call can skip the lcm calc
    normalize_divisors(gen_sys, divisor);
  parameterize(gs, gen_sys[row], false);
#endif

  if (minimize())
    // This call to `add_and_minimize(...)' returns `true'.
    add_and_minimize(gen_sys, con_sys, gs);
  else {
    // The grid was empty: check if `gs' contains a point.
    if (!gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
    // `gs' has a point: the grid is no longer empty and
    // generators are up-to-date.
    std::swap(gen_sys, gs);
    clear_empty();
    set_generators_up_to_date();
    // This call to `minimize()' returns `true'.
    minimize();
  }
  assert(OK(true));
  return true;
}

bool
PPL::Grid::add_generators_and_minimize(const Generator_System& gs) {
  // TODO: this is just an executable specification.
  Generator_System gs_copy = gs;
  return add_recycled_generators_and_minimize(gs_copy);
}

void
PPL::Grid::intersection_assign(const Grid& y) {
  Grid& x = *this;
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("intersection_assign(y)", "y", y);

  // If one of the two grids is empty, the intersection is empty.
  if (x.marked_empty())
    return;
  if (y.marked_empty()) {
    x.set_empty();
    return;
  }

  // If both polyhedra are zero-dimensional, then at this point they
  // are necessarily non-empty, so that their intersection is
  // non-empty too.
  if (x.space_dim == 0)
    return;

  // Both systems of congruences have to be up-to-date, possibly
  // having pending congruences.
  if (x.has_pending_generators())
    x.process_pending_generators();
  else if (!x.congruences_are_up_to_date())
    x.update_congruences();

  if (y.has_pending_generators())
    y.process_pending_generators();
  else if (!y.congruences_are_up_to_date())
    y.update_congruences();

  // Here both systems are up-to-date and possibly have pending congruences
  // (but they cannot have pending generators).
  assert(!x.has_pending_generators() && x.congruences_are_up_to_date());
  assert(!y.has_pending_generators() && y.congruences_are_up_to_date());

#if 0 // FIX pending
  // If `x' can support pending congruences, the congruences of `y'
  // are added as pending congruences of `x'.
  if (x.can_have_something_pending()) {
    x.con_sys.add_pending_rows(y.con_sys);
    x.set_congruences_pending();
  }
#endif // else
  {
    // `x' cannot support pending congruences.
    x.con_sys.add_rows(y.con_sys);
    // Generators are no longer up-to-date and congruences are no
    // longer minimized.
    x.clear_generators_up_to_date();
    x.clear_congruences_minimized();
  }
  assert(x.OK() && y.OK());
}
#if 0
bool
PPL::Grid::intersection_assign_and_minimize(const Grid& y) {
  Grid& x = *this;
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

  // `x' must be minimized and have sorted congruences.
  // `minimize()' will process any pending congruences or generators.
  if (!x.minimize())
    // We have just discovered that `x' is empty.
    return false;

  // `y' must have updated, possibly pending congruences.
  if (y.has_pending_generators())
    y.process_pending_generators();
  else if (!y.congruences_are_up_to_date())
    y.update_congruences();

  bool empty;
  if (y.con_sys.num_pending_rows() > 0) {
    // Integrate `y.con_sys' as pending congruences of `x',
    // sort them in place and then call `add_and_minimize()'.
    x.con_sys.add_pending_rows(y.con_sys);
    x.con_sys.sort_pending_and_remove_duplicates();
    if (x.con_sys.num_pending_rows() == 0) {
      // All pending congruences were duplicates.
      x.clear_pending_congruences();
      assert(OK(true));
      return true;
    }
    empty = add_and_minimize(x.con_sys, x.gen_sys);
  }
  else
    empty = add_and_minimize(x.con_sys, x.gen_sys, y.con_sys);

  if (empty) {
    x.set_empty();
    assert(x.OK(false));
    return false;
  }
  assert(x.OK(true));
  return true;
}

void
PPL::Grid::poly_hull_assign(const Grid& y) {
  Grid& x = *this;
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
  if ((x.has_pending_congruences() && !x.process_pending_congruences())
      || (!x.generators_are_up_to_date() && !x.update_generators())) {
    // Discovered `x' empty when updating generators.
    x = y;
    return;
  }
  if ((y.has_pending_congruences() && !y.process_pending_congruences())
      || (!y.generators_are_up_to_date() && !y.update_generators()))
    // Discovered `y' empty when updating generators.
    return;

  // Here both systems are up-to-date and possibly have pending generators
  // (but they cannot have pending congruences).
  assert(!x.has_pending_congruences() && x.generators_are_up_to_date());
  assert(!y.has_pending_congruences() && y.generators_are_up_to_date());

  // If `x' can support pending generators,
  // the generators of `y' are added as pending generators of `x'.
  if (x.can_have_something_pending()) {
    x.gen_sys.add_pending_rows(y.gen_sys);
    x.set_generators_pending();
  }
  else {
    // `x' cannot support pending generators.
    // If both generator systems are (fully) sorted, then we can merge
    // them; otherwise we simply adds the second to the first.
    if (x.gen_sys.is_sorted()
	&& y.gen_sys.is_sorted() && !y.has_pending_generators())
      x.gen_sys.merge_rows_assign(y.gen_sys);
    else
      x.gen_sys.add_rows(y.gen_sys);
    // Congruences are no longer up-to-date
    // and generators are no longer minimized.
    x.clear_congruences_up_to_date();
    x.clear_generators_minimized();
  }
  // At this point both `x' and `y' are not empty.
  assert(x.OK(true) && y.OK(true));
}

bool
PPL::Grid::poly_hull_assign_and_minimize(const Grid& y) {
  Grid& x = *this;
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

  // `x' must have minimized congruences and generators.
  // `minimize()' will process any pending congruences or generators.
  if (!x.minimize()) {
    // We have just discovered that `x' is empty.
    x = y;
    return minimize();
  }
  // x must have `sat_g' up-to-date and sorted generators.
  //x.obtain_sorted_generators_with_sat_g();

  // `y' must have updated, possibly pending generators.
  if ((y.has_pending_congruences() && !y.process_pending_congruences())
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
    add_and_minimize(x.gen_sys, x.con_sys);
  }
  else
    add_and_minimize(x.gen_sys, x.con_sys, y.gen_sys);

  assert(x.OK(true) && y.OK());
  return true;
}

void
PPL::Grid::poly_difference_assign(const Grid& y) {
  Grid& x = *this;
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

  Grid new_polyhedron(topology(), x.space_dim, EMPTY);

  // Being lazy here is only harmful.
  // `minimize()' will process any pending congruences or generators.
  x.minimize();
  y.minimize();

  const Congruence_System& y_cgs = y.congruences();
  for (Congruence_System::const_iterator i = y_cgs.begin(),
	 y_cgs_end = y_cgs.end(); i != y_cgs_end; ++i) {
    const Congruence& c = *i;
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
    Grid z = x;
    const Linear_Expression e = Linear_Expression(c);
    switch (c.type()) {
    case Congruence::NONSTRICT_INEQUALITY:
      if (is_necessarily_closed())
	z.add_congruence(e <= 0);
      else
	z.add_congruence(e < 0);
      break;
    case Congruence::STRICT_INEQUALITY:
      z.add_congruence(e <= 0);
      break;
    case Congruence::EQUALITY:
      if (is_necessarily_closed())
	// We have already filtered out the case
	// when `x' is included in `y': the result is `x'.
	return;
      else {
	Grid w = x;
	w.add_congruence(e < 0);
	new_polyhedron.poly_hull_assign(w);
	z.add_congruence(e > 0);
      }
      break;
    }
    new_polyhedron.poly_hull_assign(z);
  }
  *this = new_polyhedron;

  assert(OK());
}

void
PPL::Grid::affine_image(const Variable var,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator) {
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
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "v", var);

  if (marked_empty())
    return;

  if (var_space_dim <= expr_space_dim && expr[var_space_dim] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved, so that
    // pending rows, if present, are correctly handled.
    if (generators_are_up_to_date()) {
      // Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator);
    }
    if (congruences_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[var_space_dim]' and `denominator'.
      Linear_Expression inverse;
      if (expr[var_space_dim] > 0) {
	inverse = -expr;
	inverse[var_space_dim] = denominator;
	con_sys.affine_preimage(var_space_dim, inverse, expr[var_space_dim]);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as Congruence_System::affine_preimage()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[var_space_dim] = denominator;
	negate(inverse[var_space_dim]);
	con_sys.affine_preimage(var_space_dim, inverse, -expr[var_space_dim]);
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
      // Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator);

      clear_congruences_up_to_date();
      clear_generators_minimized();
    }
  }
  assert(OK());
}


void
PPL::Grid::
affine_preimage(const Variable var,
		const Linear_Expression& expr,
		Coefficient_traits::const_reference denominator) {
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
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "v", var);

  if (marked_empty())
    return;

  if (var_space_dim <= expr_space_dim && expr[var_space_dim] != 0) {
    // The transformation is invertible:
    // minimality and saturators are preserved.
    if (congruences_are_up_to_date()) {
      // Congruence_System::affine_preimage() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	con_sys.affine_preimage(var_space_dim, expr, denominator);
      else
	con_sys.affine_preimage(var_space_dim, -expr, -denominator);
    }
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[var_space_dim]' and `denominator'.
      Linear_Expression inverse;
      if (expr[var_space_dim] > 0) {
	inverse = -expr;
	inverse[var_space_dim] = denominator;
	gen_sys.affine_image(var_space_dim, inverse, expr[var_space_dim]);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as Generator_System::affine_image()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[var_space_dim] = denominator;
	negate(inverse[var_space_dim]);
	gen_sys.affine_image(var_space_dim, inverse, -expr[var_space_dim]);
      }
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of congruences.
    if (has_something_pending())
      remove_pending_to_obtain_congruences();
    else if (!congruences_are_up_to_date())
      minimize();
    // Congruence_System::affine_preimage() requires the third argument
    // to be a positive Coefficient.
    if (denominator > 0)
      con_sys.affine_preimage(var_space_dim, expr, denominator);
    else
      con_sys.affine_preimage(var_space_dim, -expr, -denominator);

    clear_generators_up_to_date();
    clear_congruences_minimized();
  }
  assert(OK());
}

void
PPL::Grid::
generalized_affine_image(const Variable var,
			 const Relation_Symbol relsym,
			 const Linear_Expression& expr,
			 Coefficient_traits::const_reference denominator) {
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
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
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
      Generator_System gs;
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
	    ++gen_sys[gen_sys.num_rows()-1][var_space_dim];
	  else
	    --gen_sys[gen_sys.num_rows()-1][var_space_dim];
	  // Transform `g' into a closure point.
	  g[eps_index] = 0;
	}
      clear_congruences_up_to_date();
      clear_generators_minimized();
      gen_sys.set_sorted(false);
    }
  }
  assert(OK());
}

void
PPL::Grid::generalized_affine_image(const Linear_Expression& lhs,
					  const Relation_Symbol relsym,
					  const Linear_Expression& rhs) {
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
  // we can simply add the congruence `lhs relsym rhs'.
  if (lhs_space_dim == 0) {
    switch (relsym) {
    case LESS_THAN:
      add_congruence(lhs < rhs);
      break;
    case LESS_THAN_OR_EQUAL:
      add_congruence(lhs <= rhs);
      break;
    case EQUAL:
      add_congruence(lhs == rhs);
      break;
    case GREATER_THAN_OR_EQUAL:
      add_congruence(lhs >= rhs);
      break;
    case GREATER_THAN:
      add_congruence(lhs > rhs);
      break;
    }
    return;
  }

  // Gather in `new_gs' the collections of all the lines having
  // the direction of variables occurring in `lhs'.
  // While at it, check whether or not there exists a variable
  // occurring in both `lhs' and `rhs'.
  Generator_System new_lines;
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
    add_space_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to the right hand side.
    // (we force minimization because we will need the generators).
    // FIXME: why not use add_congruence_and_minimize() here?
    Congruence_System new_cgs1;
    new_cgs1.insert(new_var == rhs);
    add_recycled_congruences_and_minimize(new_cgs1);

    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the congruences).
    add_recycled_generators_and_minimize(new_lines);

    // Constrain the new dimension so that it is related to
    // the left hand side as dictated by `relsym'
    // (we force minimization because we will need the generators).
    // FIXME: why not use add_congruence_and_minimize() here?
    Congruence_System new_cgs2;
    switch (relsym) {
    case LESS_THAN:
      new_cgs2.insert(lhs < new_var);
      break;
    case LESS_THAN_OR_EQUAL:
      new_cgs2.insert(lhs <= new_var);
      break;
    case EQUAL:
      new_cgs2.insert(lhs == new_var);
      break;
    case GREATER_THAN_OR_EQUAL:
      new_cgs2.insert(lhs >= new_var);
      break;
    case GREATER_THAN:
      new_cgs2.insert(lhs > new_var);
      break;
    }
    add_recycled_congruences_and_minimize(new_cgs2);

    // Remove the temporarily added dimension.
    remove_higher_space_dimensions(space_dim-1);
  }
  else {
    // `lhs' and `rhs' variables are disjoint:
    // there is no need to add a further dimension.

    // Cylindrificate on all the variables occurring in the left hand side
    // (we force minimization because we will need the congruences).
    add_recycled_generators_and_minimize(new_lines);

    // Constrain the left hand side expression so that it is related to
    // the right hand side expression as dictated by `relsym'.
    switch (relsym) {
    case LESS_THAN:
      add_congruence(lhs < rhs);
      break;
    case LESS_THAN_OR_EQUAL:
      add_congruence(lhs <= rhs);
      break;
    case EQUAL:
      add_congruence(lhs == rhs);
      break;
    case GREATER_THAN_OR_EQUAL:
      add_congruence(lhs >= rhs);
      break;
    case GREATER_THAN:
      add_congruence(lhs > rhs);
      break;
    }
  }

  assert(OK());
}

void
PPL::Grid::time_elapse_assign(const Grid& y) {
  Grid& x = *this;
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
      || (x.has_pending_congruences() && !x.process_pending_congruences())
      || (!x.generators_are_up_to_date() && !x.update_generators())
      || (y.has_pending_congruences() && !y.process_pending_congruences())
      || (!y.generators_are_up_to_date() && !y.update_generators())) {
    x.set_empty();
    return;
  }

  // At this point both generator systems are up-to-date,
  // possibly containing pending generators.
  Generator_System gs = y.gen_sys;
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
  // `Linear_System::merge_rows_assign()' requires both systems to be sorted.
  else {
    if (!x.gen_sys.is_sorted())
      x.gen_sys.sort_rows();
    gs.sort_rows();
    x.gen_sys.merge_rows_assign(gs);
    // Only the system of generators is up-to-date.
    x.clear_congruences_up_to_date();
    x.clear_generators_minimized();
  }
  assert(x.OK(true) && y.OK(true));
}

void
PPL::Grid::topological_closure_assign() {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return;
  // Any empty or zero-dimensional polyhedron is closed.
  if (marked_empty() || space_dim == 0)
    return;

  // The computation can be done using congruences or generators.
  // If we use congruences, we will change them, so that having pending
  // congruences would be useless. If we use generators, we add generators,
  // so that having pending generators still makes sense.

  // Process any pending congruences.
  if (has_pending_congruences() && !process_pending_congruences())
    return;

  // Use congruences only if they are available and
  // there are no pending generators.
  if (!has_pending_generators() && congruences_are_up_to_date()) {
    const dimension_type eps_index = space_dim + 1;
    bool changed = false;
    // Transform all strict inequalities into non-strict ones.
    for (dimension_type i = con_sys.num_rows(); i-- > 0; ) {
      Congruence& c = con_sys[i];
      if (c[eps_index] < 0 && !c.is_trivial_true()) {
	c[eps_index] = 0;
	// Enforce normalization.
	c.normalize();
	changed = true;
      }
    }
    if (changed) {
      con_sys.insert(Congruence::epsilon_leq_one());
      con_sys.set_sorted(false);
      // After changing the system of congruences, the generators
      // are no longer up-to-date and the congruences are no longer
      // minimized.
      clear_generators_up_to_date();
      clear_congruences_minimized();
    }
  }
  else {
    // Here we use generators, possibly keeping congruences.
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
      // Congruences are not up-to-date and generators are not minimized.
      clear_congruences_up_to_date();
      clear_generators_minimized();
    }
  }
  assert(OK());
}
#endif

/*! \relates Parma_Polyhedra_Library::Grid */
bool
PPL::operator==(const Grid& x, const Grid& y) {
  if (x.space_dim != y.space_dim)
    return false;

  if (x.marked_empty())
    return y.is_empty();
  if (y.marked_empty())
    return x.is_empty();
  if (x.space_dim == 0)
    return true;
#if 0 // FIX?
  if (y.is_universe())
    return x.is_universe();
#endif

  switch (x.quick_equivalence_test(y)) {
  case Grid::TVB_TRUE:
    return true;

  case Grid::TVB_FALSE:
    return false;

  default:
    if (x.is_included_in(y)) {
      if (x.marked_empty())
	return y.is_empty();
      return y.is_included_in(x);
    }
    return false;
  }
}

PPL::Grid&
PPL::Grid::operator=(const Grid& y) {
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

#if 0
/*! \relates Parma_Polyhedra_Library::Grid */
bool
PPL::Grid::contains(const Grid& y) const {
  const Grid& x = *this;

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("contains(y)", "y", y);

  if (y.marked_empty())
    return true;
  else if (x.marked_empty())
    return y.is_empty();
  else if (y.space_dim == 0)
    return true;
  else if (x.quick_equivalence_test(y) == Grid::TVB_TRUE)
    return true;
  else
    return y.is_included_in(x);
}

bool
PPL::Grid::is_disjoint_from(const Grid& y) const {
  Grid z = *this;
  z.intersection_assign_and_minimize(y);
  return z.is_empty();
}
#endif

void
PPL::Grid::ascii_dump(std::ostream& s) const {
  using std::endl;

  s << "space_dim "
    << space_dim
    << endl;
  //status.ascii_dump(s); // FIX
  s //<< endl // FIX
    << "con_sys ("
    << (congruences_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  con_sys.ascii_dump(s);
  s << "gen_sys ("
    << (generators_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  gen_sys.ascii_dump(s);
}

bool
PPL::Grid::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> space_dim))
    return false;

#if 0
  if (!status.ascii_load(s))
    return false;
#endif

  if (!(s >> str) || str != "con_sys")
    return false;

  if (!(s >> str) || (str != "(not_up-to-date)" && str != "(up-to-date)"))
    return false;

  if (!con_sys.ascii_load(s))
    return false;

  // FIX also add to ph?
  if (str == "(up-to-date)")
    set_congruences_up_to_date();

  if (!(s >> str) || str != "gen_sys")
    return false;

  if (!(s >> str) || (str != "(not_up-to-date)" && str != "(up-to-date)"))
    return false;

  if (!gen_sys.ascii_load(s))
    return false;

  // FIX also add to ph?
  if (str == "(up-to-date)")
    set_generators_up_to_date();

  // Check for well-formedness.
  assert(OK());
  return true;
}

PPL::memory_size_type
PPL::Grid::external_memory_in_bytes() const {
  return
    con_sys.external_memory_in_bytes()
    + gen_sys.external_memory_in_bytes();
}

/*! \relates Parma_Polyhedra_Library::Grid */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Grid& gr) {
  if (gr.is_empty())
    s << "false";
  else
    // FIX ph only prints min'd constraints
    s << gr.minimized_congruences() << std::endl
      << gr.minimized_generators();
  return s;
}
