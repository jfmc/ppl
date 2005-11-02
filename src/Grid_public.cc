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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "Grid.defs.hh"
#include "Topology.hh"

#include <cassert>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

PPL::Grid::Grid(dimension_type num_dimensions,
		const Degenerate_Element kind)
  : con_sys(),
    gen_sys(NECESSARILY_CLOSED) {
  if (num_dimensions > max_space_dimension())
    throw_space_dimension_overflow("Grid(n, k)",
				   "n exceeds the maximum "
				   "allowed space dimension");

  if (kind == EMPTY)
    status.set_empty();
  space_dim = num_dimensions;
  con_sys.increase_space_dimension(num_dimensions);
  // FIX where will gen_sys space dim be adjusted?
  if (num_dimensions > 0 && kind == UNIVERSE) {
    // Initialise both systems to universe representations.
    set_congruences_minimized();
    set_congruences_up_to_date();
    dim_kinds.resize(num_dimensions + 1);
    gen_sys.adjust_topology_and_space_dimension(NECESSARILY_CLOSED,
						num_dimensions);
    set_generators_minimized();
    con_sys.add_zero_rows(1, Row::Flags());
    gen_sys.add_zero_rows(num_dimensions + 1,
			  Linear_Row::Flags(NECESSARILY_CLOSED,
					    Linear_Row::LINE_OR_EQUALITY));
    // Trivially true parameter.
    Generator& first_g = gen_sys[0];
    first_g[0] = 1;
    first_g.set_is_ray_or_point();
    // Integrality congruence.
    Congruence& first_cg = con_sys[0];
    first_cg[0] = 1;
    first_cg[num_dimensions + 1] = 1; // Modulus.
    dim_kinds[0] = PROPER_CONGRUENCE;
    // The rest.
    while (num_dimensions > 0) {
      gen_sys[num_dimensions][num_dimensions] = 1;
      dim_kinds[num_dimensions--] = CON_VIRTUAL;
    }
    gen_sys.unset_pending_rows();
  }
  gen_sys.set_sorted(false);
  assert(OK());
}

PPL::Grid::Grid(const Grid& y)
  : con_sys(),
    gen_sys(y.gen_sys.topology()),
    status(y.status),
    space_dim(y.space_dim),
    dim_kinds(y.dim_kinds) {
  // FIX check con,gen_sys dims correctly handled if out of date
  if (y.congruences_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  else
    gen_sys.set_sorted(false);
}

PPL::Grid::Grid(const Constraint_System& ccs) {
  if (ccs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(ccs)",
				   "the space dimension of ccs "
				   "exceeds the maximum allowed "
				   "space dimension");
  Congruence_System cgs;
  cgs.insert(0*Variable(ccs.space_dimension() - 1) %= 1);
  for (Constraint_System::const_iterator i = ccs.begin(),
         ccs_end = ccs.end(); i != ccs_end; ++i)
    if (i->is_equality())
      cgs.insert(*i);
  construct(cgs);
}

PPL::dimension_type
PPL::Grid::affine_dimension() const {
  if (is_empty() || space_dim == 0)
    return 0;

  // FIXME: Use the minimized congruence system, or the generator
  //        system in any form.

  const Congruence_System& cgs = minimized_congruences();
  dimension_type d = space_dim;
  for (dimension_type i = cgs.num_rows(); i-- > 0; )
    if (cgs[i].is_equality())
      --d;
  return d;
}

const PPL::Congruence_System&
PPL::Grid::congruences() const {
  if (marked_empty())
    return con_sys;

  if (space_dim == 0) {
    // Zero-dimensional universe.
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    return con_sys;
  }

  if (!congruences_are_up_to_date())
    update_congruences();

  return con_sys;
}

const PPL::Congruence_System&
PPL::Grid::minimized_congruences() const {
  if (congruences_are_up_to_date() && !congruences_are_minimized()) {
    // Minimize the congruences.
    Grid& gr = const_cast<Grid&>(*this);
    gr.con_sys.normalize_moduli();
    if (gr.simplify(gr.con_sys, gr.dim_kinds))
      gr.set_empty();
    else
      gr.set_congruences_minimized();
  }
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
      gs.adjust_topology_and_space_dimension(NECESSARILY_CLOSED,
					     space_dim);
      const_cast<Generator_System&>(gen_sys).swap(gs);
    }
    return gen_sys;
  }

  if (space_dim == 0) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return Generator_System::zero_dim_univ();
  }

  return gen_sys;
}

const PPL::Generator_System&
PPL::Grid::minimized_generators() const {
  if (!marked_empty()
      && generators_are_up_to_date()
      && !generators_are_minimized()) {
    // Minimize the generators.
    Grid& gr = const_cast<Grid&>(*this);
    gr.simplify(gr.gen_sys, gr.dim_kinds);
    gr.set_generators_minimized();
  }
  return generators();
}

PPL::Poly_Con_Relation
PPL::Grid::relation_with(const Congruence& cg) const {
  // Dimension-compatibility check.
  if (space_dim < cg.space_dimension())
    throw_dimension_incompatible("relation_with(cg)", "cg", cg);

  if (marked_empty())
    return Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  if (space_dim == 0)
    if (cg.is_trivial_false())
      // FIX
      // The congruence 0 %= 1 mod 0 implicitly defines the hyperplane
      // 0 = 0; thus, the zero-dimensional point also saturates it.
      return Poly_Con_Relation::is_disjoint();
    else if (cg.is_equality() || cg[0] == 0)
      return Poly_Con_Relation::is_included();
    else if (cg[0] % cg.modulus() == 0)
      return Poly_Con_Relation::is_included();
    else
      // cg is false.
      return Poly_Con_Relation::is_disjoint();

  if (!generators_are_up_to_date() && !update_generators())
    // The grid is empty.
    return Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  // Return one of the relations
  // 'strictly_intersects'   share at least one point
  // 'is_included'	     share all points (FIX of grid?)
  // 'is_disjoint'	     the other case, intersection fails

  // There is always a point.

  // Scalar product of the congruence and the first point that
  // satisfies the congruence.
  TEMP_INTEGER(point_sp);
  point_sp = 0;

  TEMP_INTEGER(modulus);
  modulus = cg.modulus();

  TEMP_INTEGER(div);
  div = 0;

  bool known_to_intersect = false;

  for (Generator_System::const_iterator g = gen_sys.begin(),
         gen_sys_end = gen_sys.end(); g != gen_sys_end; ++g) {
    TEMP_INTEGER(sp);
    PPL::scalar_product_assign(sp, cg, *g);

    switch (g->type()) {

    case Generator::POINT:
      if (cg.is_proper_congruence())
	// FIX include the divisor?
	sp %= modulus;
      if (sp == 0)
	// The point satisfies the congruence.
	if (point_sp == 0)
	  // Any previous points satisfied the congruence.
	  known_to_intersect = true;
	else
	  return Poly_Con_Relation::strictly_intersects();
      else
	if (point_sp == 0) {
	  if (known_to_intersect)
	    return Poly_Con_Relation::strictly_intersects();
	  // Assign `sp' to `point_sp' as `sp' is the scalar product
	  // of cg and a point g and is non-zero.
	  point_sp = sp;
	}
	else {
	  // A previously considered point p failed to satisfy cg such that
	  // `point_sp' = `scalar_prod(p,cg)'
	  // so, if we consider the parameter g-p instead of g, we have
	  // scalar_prod(g-p, cg) = scalar_prod(g,cg) - scalar_prod(p,cg)
	  //                      = sp - point_sp
	  sp -= point_sp;

	  if (sp != 0) {
	    // Find the GCD between sp and the previous GCD.
	    gcd_assign(div, sp);
	    if (point_sp % div == 0)
	      // There is a point in the grid satisfying cg.
	      return Poly_Con_Relation::strictly_intersects();
	  }
	}
      break;

    case Generator::RAY:	// PARAMETER
      if (cg.is_proper_congruence())
	// FIX include the divisor?
	sp %= modulus;
      if (sp == 0)
	// Parameter g satisfies the cg so the relation depends
	// entirely on the other generators.
	break;

      if (known_to_intersect)
	// At least one point satisfies cg.  However, the sum of such
	// a point and the parameter g fails to satisfy cg (due to g).
	return Poly_Con_Relation::strictly_intersects();

      // Find the GCD between sp and the previous GCD.
      gcd_assign(div, sp);
      if (point_sp != 0)
	// At least one of any previously encountered points fails to
	// satisfy cg.
	if (point_sp == div)
	  // There is also a grid point that satisfies cg.
	  return Poly_Con_Relation::strictly_intersects();

      break;

    case Generator::LINE:
      if (sp == 0)
	// Line g satisfies the cg so the relation depends entirely on
	// the other generators.
	break;

      // Line g intersects the congruence.
      //
      // There is a point p in the grid.  Suppose <p*cg> = p_sp.  Then
      // (-p_sp/sp)*g + p is a point that satisfies cg: <((-p_sp/sp)*g
      // + p).cg> = -(p_sp/sp)*sp + p_sp) = 0.  If p does not satisfy
      // `cg' and hence is not in the grid defined by `cg', the grid
      // `*this' strictly intersects the `cg' grid.  On the other
      // hand, if `p' is in the grid defined by `cg' so that p_sp = 0,
      // then <p+g.cg> = p_sp + sp != 0; thus `p+g' is a point in
      // *this that does not satisfy `cg' and hence `p+g' is a point
      // in *this not in the grid defined by `cg'; therefore `*this'
      // strictly intersects the `cg' grid.

      return Poly_Con_Relation::strictly_intersects();

    case Generator::CLOSURE_POINT:
      // FIX
      break;
    }
  }

  if (point_sp == 0)
    // Every generator satisfied the cg.
    return Poly_Con_Relation::is_included();

  assert(!known_to_intersect);
  return Poly_Con_Relation::is_disjoint();
}

PPL::Poly_Gen_Relation
PPL::Grid::relation_with(const Generator& g,
			 Coefficient_traits::const_reference divisor) const {
  // Dimension-compatibility check.
  if (space_dim < g.space_dimension())
    throw_dimension_incompatible("relation_with(g)", "g", g);

  // The empty grid cannot subsume a generator.
  if (marked_empty())
    return Poly_Gen_Relation::nothing();

  // A universe grid in a zero-dimensional space subsumes all the
  // generators of a zero-dimensional space.
  if (space_dim == 0)
    return Poly_Gen_Relation::subsumes();

  congruences_are_up_to_date() || update_congruences();

  return
    con_sys.satisfies_all_congruences(g, divisor == 0 ? g.divisor() : divisor)
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

  if (generators_are_up_to_date() && gen_sys.num_columns() == 0)
    return false;

  if (!congruences_are_up_to_date() && !update_congruences())
    // Grid is empty.
    return false;

  if (con_sys.num_columns() == 0)
    return false;

  // Test gen_sys's inclusion in a universe generator system.
  // FIX if min cmp to single integrality cong
  // FIX create single g'tor and modify for each iteration below
  Grid gr(space_dim);
  Generator_System& gs = gr.gen_sys;
  for (dimension_type i = gs.num_rows(); i-- > 0; ) {
    const Generator& g = gs[i];
    if (!con_sys.satisfies_all_congruences(g, g[0]))
      return false;
  }
  return true;
}
#if 0
bool
PPL::Grid::is_bounded() const {
  // A zero-dimensional or empty polyhedron is bounded.
  if (space_dim == 0
      || marked_empty()
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
#endif
bool
PPL::Grid::is_pointed() const {
  // A zero-dimensional or empty grid is pointed.
  if (space_dim == 0 || marked_empty())
    return true;

  if (generators_are_minimized()) {
  line_search:
    // Search for lines in the minimized generator system.
    for (dimension_type row = gen_sys.num_rows(); row-- > 1; )
      if (gen_sys[row].is_line())
	return false;
    return true;
  }

  if (congruences_are_minimized()) {
  free_dim_check:
    // Search for a dimension that is free of any congruence or
    // equality constraint from the minimized congruence system.
    std::vector<bool> free_dim(space_dim, true);
    dimension_type free_dims = space_dim;
    for (dimension_type row = con_sys.num_rows(); row-- > 0; ) {
      const Congruence& cg = con_sys[row];
      for (dimension_type dim = 0; dim < space_dim; ++dim)
	if (free_dim[dim] && cg[dim+1] != 0) {
	  if (--free_dims == 0) {
	    // All dimensions are constrained.
#ifndef NDEBUG
	    free_dim[dim] = false;
	    // Check that there are free_dims dimensions marked free
	    // in free_dim.
	    dimension_type count = 0;
	    for (dimension_type dim = 0; dim < space_dim; ++dim)
	      count += free_dim[dim];
	    assert(count == free_dims);
#endif
	    return true;
	  }
	  free_dim[dim] = false;
	}
    }
    // At least one dimension is free of constraint.
    return false;
  }

  Grid& gr = const_cast<Grid&>(*this);
  if (generators_are_up_to_date()) {
    // Minimize the generator system.
    gr.simplify(gr.gen_sys, gr.dim_kinds);
    gr.set_generators_minimized();

    goto line_search;
  }

  // Generators are out of date.

  // Minimize the congruence system to find out whether it is empty.
  gr.con_sys.normalize_moduli();
  if (gr.simplify(gr.con_sys, gr.dim_kinds)) {
    // The congruence system reduced to the empty grid.
    gr.set_empty();
    return true;
  }
  gr.set_congruences_minimized();

  goto free_dim_check;
}

bool
PPL::Grid::is_topologically_closed() const {
  // Any empty or zero-dimensional grid is closed.
  if (marked_empty() || space_dim == 0)
    return true;

  if (generators_are_minimized()) {
  param_search:
    // Search for a parameter in the minimized generator system.
    for (dimension_type row = gen_sys.num_rows(); row-- > 1; )
      if (gen_sys[row].is_ray())
	return false;
    return true;
  }

  if (congruences_are_minimized()) {
  proper_cg_search:
    // Search for a proper congruence following the integrality
    // congruence, in the minimized congruence system.
    for (dimension_type row = con_sys.num_rows() - 1; row-- > 0; )
      if (con_sys[row].is_proper_congruence())
	return false;
    return true;
  }

  Grid& gr = const_cast<Grid&>(*this);
  if (generators_are_up_to_date()) {
    gr.simplify(gr.gen_sys, gr.dim_kinds);
    gr.set_generators_minimized();
    goto param_search;
  }

  // Minimize the congruence system.
  gr.con_sys.normalize_moduli();
  if (gr.simplify(gr.con_sys, gr.dim_kinds)) {
    // The congruence system reduced to the empty grid.
    gr.set_empty();
    return true;
  }
  gr.set_congruences_minimized();
  goto proper_cg_search;
}

bool
PPL::Grid::OK(bool check_not_empty) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // Check the topology of `gen_sys'.
  if (gen_sys.topology() == NOT_NECESSARILY_CLOSED) {
#ifndef NDEBUG
    cerr << "Generator system should be necessarily closed." << endl;
#endif
    goto fail;
  }

  // Check the sortedness of `gen_sys'.
  if (gen_sys.is_sorted()) {
#ifndef NDEBUG
    cerr << "Generator system is marked as sorted." << endl;
#endif
    goto fail;
  }

  // Check whether the status information is legal.
  if (!status.OK())
    goto fail;

  if (marked_empty()) {
    if (check_not_empty) {
      // The caller does not want the grid to be empty.
#ifndef NDEBUG
      cerr << "Empty grid!" << endl;
#endif
      goto fail;
    }

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
    cerr << "Grid not empty, not zero-dimensional" << endl
	 << "and with neither congruences nor generators up-to-date!"
	 << endl;
#endif
    goto fail;
  }

  if (has_something_pending()) {
#ifndef NDEBUG
    cerr << "Grid with congruences and/or generators pending." << endl;
#endif
    goto fail;
  }

  {
    // This block is to limit the scope of num_columns, at least for
    // GCC < 3.4.

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
      if (!gen_sys.Linear_System::OK(false)) {
#ifndef NDEBUG
	cerr << "gen_sys Linear_System::OK failed." << endl;
#endif
	goto fail;
      }
      // Check each generator in the system.
      for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
	const Generator& g = gen_sys[i];

	if (!g.is_necessarily_closed()) {
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

	if (dim_kinds.size() != num_columns) {
#ifndef NDEBUG
	  cerr << "Size of dim_kinds should equal the number of columns."
	       << endl;
#endif
	  goto fail;
	}

	if (!upper_triangular(gs, dim_kinds)) {
#ifndef NDEBUG
	  cerr << "Reduced generators should be upper triangular."
	       << endl;
#endif
	  goto fail;
	}

	// Check that dim_kinds corresponds to the row kinds in gen_sys.
	for (dimension_type dim = 0, row = 0;
	     dim < space_dim + 1;
	     ++dim, assert(row <= dim)) {
	  if (dim_kinds[dim] == GEN_VIRTUAL
	      || (gen_sys[row++].is_ray_or_point_or_inequality()
		  && dim_kinds[dim] == PARAMETER)
	      || (assert(gen_sys[row-1].is_line()), dim_kinds[dim] == LINE))
	    continue;
#ifndef NDEBUG
	  cerr << "Kinds in dim_kinds should match those in gen_sys."
	       << endl;
#endif
	  goto fail;
	}

	// A reduced generator system must be the same as a temporary
	// reduced copy.
	Dimension_Kinds d = dim_kinds;
	// `gs' is minimized and marked_empty returned false, so `gs'
	// should contain rows.
	assert(gs.num_rows() > 0);
	simplify(gs, d);
	// gs contained rows before being reduced, so it should
	// contain at least a single point afterwards.
	assert(gs.num_rows() > 0);
	for (dimension_type row = 0; row < gen_sys.num_rows(); ++row) {
	  Generator& g = gs[row];
	  const Generator& g_copy = gen_sys[row];
	  dimension_type col = gen_sys.num_columns();
	  if (g.type() != g_copy.type())
	    goto message_fail;
	  while (col--) {
	    if (g[col] == g_copy[col])
	      continue;
	  message_fail:
#ifndef NDEBUG
	    cerr << "Generators are declared minimized, but they change under reduction.\n"
		 << "Here is the generator system:\n";
	    gen_sys.ascii_dump(cerr);
	    cerr << "and here is the minimized form of the temporary copy:\n";
	    gs.ascii_dump(cerr);
#endif
	    goto fail;
	  }
	}
      }

    } // if (congruences_are_up_to_date())
  } // scope block

  if (congruences_are_up_to_date()) {
    // Check if the system of congruences is well-formed.
    if (!con_sys.OK()) {
#ifndef NDEBUG
      cerr << "con_sys OK failed." << endl;
#endif
      goto fail;
    }

    Congruence_System cs_copy = con_sys;
    Generator_System tem_gen_sys(NECESSARILY_CLOSED);
    Dimension_Kinds d = dim_kinds;

    if (minimize(cs_copy, tem_gen_sys, d)) {
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
      // A reduced congruence system must be lower triangular.
      if (!lower_triangular(con_sys, dim_kinds)) {
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
	  if (con_sys[row][col] == cs_copy[row][col])
	    continue;
#ifndef NDEBUG
	  cerr << "Congruences are declared minimized, but they change under reduction!"
	       << endl
	       << "Here is the minimized form of the congruence system:"
	       << endl;
	  cs_copy.ascii_dump(cerr);
	  cerr << endl;
#endif
	  goto fail;
	}

      if (dim_kinds.size() != con_sys.num_columns() - 1 /* modulus */) {
#ifndef NDEBUG
	cerr << "Size of dim_kinds should equal the number of columns."
	     << endl;
#endif
	goto fail;
      }

      // Check that dim_kinds corresponds to the row kinds in con_sys.
      for (dimension_type dim = 0, row = con_sys.num_rows() - 1;
	   dim < space_dim + 1;
	   ++dim) {
	if (dim_kinds[dim] == CON_VIRTUAL
	    || (con_sys[row--].is_proper_congruence()
		&& dim_kinds[dim] == PROPER_CONGRUENCE)
	    || (assert(con_sys[row+1].is_equality()),
		dim_kinds[dim] == EQUALITY))
	  continue;
#ifndef NDEBUG
	cerr << "Kinds in dim_kinds should match those in con_sys." << endl;
#endif
	goto fail;
      }
    }
  }

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

  if (!congruences_are_up_to_date())
    update_congruences();

  con_sys.insert(cg);

  clear_congruences_minimized();
  set_congruences_up_to_date();
  clear_generators_up_to_date();

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

void
PPL::Grid::add_congruence(const Constraint& c) {
  // TODO: this is just an executable specification.
  Congruence_System cgs(c);
  return add_recycled_congruences(cgs);
}

bool
PPL::Grid::add_congruence_and_minimize(const Constraint& c) {
  // TODO: this is just an executable specification.
  Congruence_System cgs(c);
  return add_recycled_congruences_and_minimize(cgs);
}

void
PPL::Grid::add_generator(const Generator& g) {
  // Topology-compatibility check.
  if (g.is_closure_point())
    throw_topology_incompatible("add_generator(g)", "g", g);

  // The dimension of `g' must be at most space_dim.
  const dimension_type g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("add_generator(g)", "g", g);

  // Deal with zero-dimension case first.
  if (space_dim == 0) {
    // In dimension zero it is only possible to create points.
    assert(g.is_point());
    if (marked_empty())
      status.set_zero_dim_univ();
    assert(OK());
    return;
  }

  if (marked_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Here the grid is empty: the specification says we can only
    // insert a point.
    if (!g.is_point())
      throw_invalid_generator("add_generator(g)", "g");
    if (g.is_necessarily_closed())
      gen_sys.insert(g, false);
    else {
      // Note: here we have a _legal_ topology mismatch,
      // because `g' is NOT a closure point (it is a point!)
      // However, by barely invoking `gen_sys.insert(g)' we would
      // cause a change in the topology of `gen_sys', which is wrong.
      // Thus, we insert a "topology corrected" copy of `g'.
      const Linear_Expression nc_expr = Linear_Expression(g);
      gen_sys.insert(Generator::point(nc_expr, g.divisor()), false);
    }
    // Since `gen_sys' was empty, resize the system of generators to
    // the right dimension.
    gen_sys.adjust_topology_and_space_dimension(NECESSARILY_CLOSED,
						space_dim);
    clear_empty();
  }
  else {
    assert(generators_are_up_to_date());
    // Copy g, in all cases, in case the divisor needs to be changed
    // when g is a point.
    // FIX only copy when point
    Generator tem(g);
    Generator& g = tem;
    // g is now a reference to the copy.
    TEMP_INTEGER(divisor);
    TEMP_INTEGER(gen_sys_divisor);
    if (g.is_point()) {
      // Ensure that the divisors of gen_sys and g are the same.
      divisor = g.divisor();
      gen_sys_divisor = normalize_divisors(gen_sys, divisor);
    }
    if (g.is_necessarily_closed()) {
      // Since `gen_sys' is not empty, the topology and space dimension
      // of the inserted generator are automatically adjusted.
      // FIX convert rays to lines for now
      if (g.is_ray())
	gen_sys.insert(Generator::line(Linear_Expression(g)), false);
      else {
	gen_sys.insert(g, false);
	if (g.is_point())
	check_divisor:
	  if (divisor != gen_sys_divisor) {
	    // Multiply the inserted point to match the gen_sys
	    // divisor.  Done after the insert so that a normalized g
	    // is passed to insert.
	    Generator& inserted_g = gen_sys[gen_sys.num_rows()-1];
	    inserted_g[0] = gen_sys_divisor;
	    gen_sys_divisor /= divisor;
	    for (dimension_type col = 1; col < inserted_g.size(); ++col)
	      inserted_g[col] *= gen_sys_divisor;
	  }
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
	gen_sys.insert(Generator::line(nc_expr), false);
	break;
      case Generator::RAY:
	// FIX should input parameter?
	gen_sys.insert(Generator::line(nc_expr), false);
	break;
      case Generator::POINT:
	gen_sys.insert(Generator::point(nc_expr, g.divisor()), false);
	goto check_divisor;
      default:
	//abort(); // FIX instead?
	throw_runtime_error("add_generator(const Generator& g)");
      }
    }
  }

  // With the added generator, congruences are out of date.
  clear_congruences_up_to_date();

  clear_generators_minimized();
  set_generators_up_to_date();
  assert(OK());
}

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

  if (marked_empty()) {
    assert(OK());
    return;
  }

  // The congruences are required.
  if (!congruences_are_up_to_date() && !update_congruences()) {
    set_empty();
    assert(OK());
    return;
  }

  // Adjust `cgs' to the right space dimension.
  cgs.increase_space_dimension(space_dim);

  // Swap (instead of copying) the coefficients of `cgs' (which is
  // writable).
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

  // Congruences may not be minimized and generators are out of date.
  clear_congruences_minimized();
  clear_generators_up_to_date();
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

  // Dealing with zero-dimensional space grids first.
  if (space_dim == 0) {
    // In a 0-dimensional space the congruences are
    // trivial (e.g., 0 == 0 or 1 >= 0 or 1 > 0) or
    // inconsistent (e.g., 1 == 0 or -1 >= 0 or 0 > 0).
    // In a system of congruences `begin()' and `end()' are equal
    // if and only if the system contains trivial congruences only.
    if (cgs.begin() == cgs.end())
      return true;
    // There is a congruence, it must be inconsistent, the grid is
    // empty.
    status.set_empty();
    return false;
  }

  if (marked_empty())
    return false;

  if (!congruences_are_up_to_date() && !update_congruences()) {
    set_empty();
    assert(OK());
    return false;
  }

  // Adjust `cgs' to the current space dimension.
  cgs.increase_space_dimension(space_dim); // FIX (?)

  for (dimension_type row = 0; row < cgs.num_rows(); ++row)
    con_sys.add_row(cgs[row]);

  if (minimize(con_sys, gen_sys, dim_kinds)) {
    set_empty();
    assert(OK());
    return false;
  }

  clear_empty();
  set_congruences_up_to_date();

  assert(OK());
  return true;
}

bool
PPL::Grid::add_congruences_and_minimize(const Congruence_System& cgs) {
  // TODO: this is just an executable specification.
  Congruence_System cgs_copy = cgs;
  return add_recycled_congruences_and_minimize(cgs_copy);
}

bool
PPL::Grid::add_congruences_and_minimize(const Constraint_System& cs) {
  // TODO: this is just an executable specification.
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_congruences_and_minimize(cs)", "cs", cs);
  Congruence_System cgs;
  bool cgs_is_empty = true;
  for (Constraint_System::const_iterator i = cs.begin(),
         cs_end = cs.end(); i != cs_end; ++i)
    if (i->is_equality()) {
      Congruence cg(*i / 0);
      cgs.insert(cg);
      //cgs.insert(*i);
      cgs_is_empty = false;
    }
  if (cgs_is_empty)
    return minimize();
  return add_congruences_and_minimize(cgs);
}

void
PPL::Grid::add_constraint(const Constraint& c) {
  // The dimension of `c' must be at most `space_dim'.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("add_constraint(c)", "c", c);
  if (c.is_equality()) {
    Congruence cg(c);
    add_congruence(cg);
  }
}

void
PPL::Grid::add_constraints(const Constraint_System& cs) {
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_constraints(cs)", "cs", cs);
  Congruence_System cgs;
  bool inserted = false;
  for (Constraint_System::const_iterator i = cs.begin(),
         cs_end = cs.end(); i != cs_end; ++i)
    if (i->is_equality()) {
      cgs.insert(*i);
      inserted = true;
    }
  // Only add cgs if congruences were inserted into cgs, as the
  // dimension of cgs must be at most that of the grid.
  if (inserted)
    add_congruences(cgs);
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

  // Adding no generators leaves the grid the same.
  if (gs.num_rows() == 0)
    return;

  // Adding valid generators to a zero-dimensional grid transforms it
  // to the zero-dimensional universe grid.
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

  // The generators are required.
  if (!generators_are_up_to_date() && !minimize()) {
    // `minimize' has shown that `*this' is empty.
    // `gs' must contain at least one point.
    if (!gs.has_points())
      // FIX should this return an empty flag?
      throw_invalid_generators("add_recycled_generators(gs)", "gs");
    gs.unset_pending_rows();
    std::swap(gen_sys, gs);
    gen_sys.set_sorted(false);
    // FIX for now convert rays to lines
    for (dimension_type row = 0; row < gen_sys.num_rows(); ++row) {
      Generator& g = gen_sys[row];
      if (g.is_ray()) {
	g.set_is_line();
	g.strong_normalize();
      }
    }
    normalize_divisors(gen_sys);
    // The grid is no longer empty and generators are up-to-date.
    set_generators_up_to_date();
    clear_empty();
    assert(OK());
    return;
  }

  normalize_divisors(gs, gen_sys);

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
    if (old_g.is_ray()) {
      new_g.set_is_line();
      new_g.strong_normalize();
    }
    for (dimension_type j = gs_num_columns; j-- > 0; )
      std::swap(new_g[j], old_g[j]);
  }

  // Congruences are out of date and generators are not minimized.
  clear_congruences_up_to_date();
  clear_generators_minimized();

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
  // transforms it into the zero-dimensional universe polyhedron.
  if (space_dim == 0) {
    if (marked_empty() && !gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
    status.set_zero_dim_univ();
    assert(OK(true));
    return true;
  }

  // Now adjusting dimensions (topology already adjusted).
  // NOTE: sortedness is preserved.
  //gs.adjust_topology_and_space_dimension(topology(), space_dim); // FIX
  gs.adjust_topology_and_space_dimension(gs.topology(), space_dim);

  if (minimize()) {   // FIX why does this minimize (incls cgs) first?
    normalize_divisors(gs, gen_sys);

    for (dimension_type row = 0; row < gs.num_rows(); ++row) {
      const Generator& g = gs[row];
      // FIX for now convert rays to lines
      if (g.is_ray())
	gen_sys.insert(Generator::line(Linear_Expression(g)), false);
      else
	gen_sys.add_row(g);
    }

    // This call to minimize returns true.
    // FIX minimize copies the generators (check cgs version)
    minimize(gen_sys, con_sys, dim_kinds);
  }
  else {
    // The grid was empty: check if `gs' contains a point.
    if (!gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
    gs.unset_pending_rows();
    // `gs' has a point: the grid is no longer empty and generators
    // are up-to-date.
    std::swap(gen_sys, gs);
    gen_sys.set_sorted(false);
    // FIX for now convert rays to lines
    for (dimension_type row = 0; row < gen_sys.num_rows(); ++row) {
      Generator& g = gen_sys[row];
      if (g.is_ray()) {
	g.set_is_line();
	g.strong_normalize();
      }
    }
    normalize_divisors(gen_sys);
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

  // If both grids are zero-dimensional, then at this point they are
  // necessarily non-empty, so that their intersection is non-empty
  // too.
  if (x.space_dim == 0)
    return;

  // The congruences must be up-to-date.
  if (!x.congruences_are_up_to_date() && !x.update_congruences())
    // Discovered `x' empty when updating congruences.
    return;
  if (!y.congruences_are_up_to_date() && !y.update_congruences()) {
    // Discovered `y' empty when updating congruences.
    x.set_empty();
    return;
  }

  x.con_sys.add_rows(y.con_sys);
  // Generators may be out of date and congruences may have changed
  // from minimal form.
  x.clear_generators_up_to_date();
  x.clear_congruences_minimized();

  // At this point both `x' and `y' are not empty.
  assert(x.OK(true) && y.OK(true));
}

bool
PPL::Grid::intersection_assign_and_minimize(const Grid& y) {
  intersection_assign(y);
  return minimize();
}

void
PPL::Grid::join_assign(const Grid& y) {
  Grid& x = *this;
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("join_assign(y)", "y", y);

  // The join of a grid `gr' with an empty grid is `gr'.
  if (y.marked_empty())
    return;
  if (x.marked_empty()) {
    x = y;
    return;
  }

  // If both grids are zero-dimensional, then they are necessarily
  // universe grids, and so is their join.
  if (x.space_dim == 0)
    return;

  // The generators must be up-to-date.
  if (!x.generators_are_up_to_date() && !x.update_generators()) {
    // Discovered `x' empty when updating generators.
    x = y;
    return;
  }
  if (!y.generators_are_up_to_date() && !y.update_generators())
    // Discovered `y' empty when updating generators.
    return;

  // Match the divisors of the x and y generator systems.
  // FIX this makes a copy of which add_rows makes a copy
  Generator_System gs(y.gen_sys);
  normalize_divisors(x.gen_sys, gs);
  x.gen_sys.add_rows(gs);
  // Congruences may be out of date and generators may have lost
  // minimal form.
  x.clear_congruences_up_to_date();
  x.clear_generators_minimized();

  // At this point both `x' and `y' are not empty.
  assert(x.OK(true) && y.OK(true));
}

bool
PPL::Grid::join_assign_and_minimize(const Grid& y) {
  join_assign(y);
  return minimize();
}

bool
PPL::Grid::join_assign_if_exact(const Grid& y) {
  Grid& x = *this;

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("join_assign_if_exact(y)", "y", y);

  if (x.marked_empty()
      || y.marked_empty()
      || x.space_dim == 0
      || x.is_included_in(y)
      || y.is_included_in(x)) {
    join_assign(y);
    return true;
  }

  Grid x_copy = x;
  x_copy.join_assign(y);
  x_copy.grid_difference_assign(y);
  if (x_copy.is_included_in(x)) {
    join_assign(y);
    return true;
  }

  return false;
}

void
PPL::Grid::grid_difference_assign(const Grid& y) {
  Grid& x = *this;
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("poly_difference_assign(y)", "y", y);

  if (y.marked_empty() || x.marked_empty())
    return;

  // If both grids are zero-dimensional, then they are necessarily
  // universe grids, so the result is empty.
  if (x.space_dim == 0) {
    x.set_empty();
    return;
  }

  if (y.contains(x)) {
    x.set_empty();
    return;
  }

  Grid new_grid(x.space_dim, EMPTY);

  const Congruence_System& y_cgs = y.congruences();
  for (Congruence_System::const_iterator i = y_cgs.begin(),
	 y_cgs_end = y_cgs.end(); i != y_cgs_end; ++i) {
    const Congruence& cg = *i;

    // The 2-complement cg2 of cg = ((e %= 0) / m) is the congruence
    // defining the sets of points exactly half-way between successive
    // hyperplanes e = km and e = (k+1)m, for any integer k; that is,
    // the hyperplanes defined by 2e = (2k + 1)m, for any integer k.
    // Thus `cg2' is the congruence ((2e %= m) / 2m).

    // As the grid difference must be a grid, only add the
    // 2-complement congruence to x if the resulting grid includes all
    // the points in x that did not satisfy `cg'.

    // The 2-complement of cg can be included in the result only if x
    // holds points other than those in cg.
    if (x.relation_with(cg).implies(Poly_Con_Relation::is_included()))
      continue;

    if (cg.is_proper_congruence()) {
      const Linear_Expression e = Linear_Expression(cg);
      // Congruence cg is ((e %= 0) / m).
      Coefficient_traits::const_reference m = cg.modulus();
      // If x is included in the grid defined by the congruences cg
      // and its 2-complement (i.e. the grid defined by the congruence
      // (2e %= 0) / m) then add the 2-complement to the potential
      // result.
      if (x.relation_with((2*e %= 0) / m).implies(Poly_Con_Relation::is_included())) {
	Grid z = x;
	z.add_congruence((2*e %= m) / (2*m));
	new_grid.join_assign(z);
	continue;
      }
    }
    return;
  }

  *this = new_grid;

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
  // The dimension of `expr' must be at most the dimension of `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);
  // `var' must be one of the dimensions of the grid.
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "v", var);

  if (marked_empty())
    return;

  if (var_space_dim <= expr_space_dim && expr[var_space_dim] != 0) {
    // The transformation is invertible.
    if (generators_are_up_to_date()) {
      // Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator, true);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator, true);
      clear_generators_minimized();
      // Strong normalization in gs::affine_image may have modified
      // divisors.
      normalize_divisors(gen_sys);
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
      clear_congruences_minimized();
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of generators.
    if (!generators_are_up_to_date())
      minimize();
    if (!marked_empty()) {
      // Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator, true);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator, true);

      clear_congruences_up_to_date();
      clear_generators_minimized();
      // Strong normalization in gs::affine_image may have modified
      // divisors.
      normalize_divisors(gen_sys);
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
    // The transformation is invertible.
    if (congruences_are_up_to_date()) {
      // Congruence_System::affine_preimage() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	con_sys.affine_preimage(var_space_dim, expr, denominator);
      else
	con_sys.affine_preimage(var_space_dim, -expr, -denominator);
      clear_congruences_minimized();
    }
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[var_space_dim]' and `denominator'.
      Linear_Expression inverse;
      if (expr[var_space_dim] > 0) {
	inverse = -expr;
	inverse[var_space_dim] = denominator;
	gen_sys.affine_image(var_space_dim, inverse, expr[var_space_dim], true);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as Generator_System::affine_image()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[var_space_dim] = denominator;
	negate(inverse[var_space_dim]);
	gen_sys.affine_image(var_space_dim, inverse, -expr[var_space_dim], true);
      }
      clear_generators_minimized();
    }
  }
  else {
    // The transformation is not invertible.
    // We need an up-to-date system of congruences.
    if (!congruences_are_up_to_date())
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
			 const Linear_Expression& expr,
			 Coefficient_traits::const_reference denominator,
			 Coefficient_traits::const_reference modulus) {
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
  // `var' should be one of the dimensions of the grid.
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "v", var);

  // Any image of an empty grid is empty.
  if (marked_empty())
    return;

  affine_image(var, expr, denominator);

  if (modulus == 0)
    return;

  // Modulate dimension `var' according to `modulus'.

  generators_are_up_to_date() || minimize();

  // Test if minimization, possibly in affine_image, found an empty
  // grid.
  if (marked_empty())
    return;

  // Insert a strongly-normalized point that will pass assertions.
  gen_sys.insert(point(), false);
  // Update the inserted point to the required parameter.
  Generator& inserted_g = gen_sys[gen_sys.num_rows()-1];
  inserted_g[0] = 0;		// Set inserted_g to a ray.
  if (modulus < 0)
    inserted_g[var.space_dimension()] = -modulus;
  else
    inserted_g[var.space_dimension()] = modulus;

  clear_generators_minimized();
  clear_congruences_up_to_date();

  assert(OK());
}

void PPL::Grid::
generalized_affine_preimage(const Variable var,
			    const Linear_Expression& expr,
			    Coefficient_traits::const_reference denominator,
			    Coefficient_traits::const_reference modulus) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_invalid_argument("generalized_affine_preimage(v, e, d, m)",
			   "d == 0");

  // The dimension of `expr' should be at most the dimension of
  // `*this'.
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, e, d, m)",
				 "e", expr);
  // `var' should be one of the dimensions of the grid.
  const dimension_type var_space_dim = var.space_dimension();
  if (space_dim < var_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, e, d, m)",
				 "v", var);

  // Check whether the affine relation is an affine function.
  if (modulus == 0) {
    affine_preimage(var, expr, denominator);
    return;
  }

  // Check whether the preimage of this affine relation can be easily
  // computed as the image of its inverse relation.
  Coefficient_traits::const_reference var_coefficient = expr.coefficient(var);
  if (var_space_dim <= expr_space_dim && var_coefficient != 0) {
    Linear_Expression inverse_expr
      = expr - (denominator + var_coefficient) * var;
    Coefficient inverse_denominator = - var_coefficient;
    // FIX include modulus in the inverse properly
    if (modulus < 0)
      generalized_affine_image(var, inverse_expr, inverse_denominator,
			       - modulus);
    else
      generalized_affine_image(var, inverse_expr, inverse_denominator,
			       modulus);
    return;
  }

  // Here `var_coefficient == 0', so that the preimage cannot be
  // easily computed by inverting the affine relation.  Add the
  // congruence induced by the affine relation.
  if (modulus < 0)
    add_congruence((denominator*var %= expr) / denominator /= - modulus);
  else
    add_congruence((denominator*var %= expr) / denominator /= modulus);

  // If the resulting grid is empty, its preimage is empty too.
  // Note: DO check for emptyness here, as we will later add a line.
  if (is_empty())
    return;
  add_generator(line(var));
  assert(OK());
}

void
PPL::Grid::
generalized_affine_image(const Linear_Expression& lhs,
			 const Linear_Expression& rhs,
			 Coefficient_traits::const_reference modulus) {
  // Dimension-compatibility checks.
  // The dimension of `lhs' should be at most the dimension of
  // `*this'.
  dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e1", lhs);
  // The dimension of `rhs' should be at most the dimension of
  // `*this'.
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  // Any image of an empty grid is empty.
  if (marked_empty())
    return;

  TEMP_INTEGER(mod);
  if (modulus < 0)
    mod = -modulus;
  else
    mod = modulus;

  // Compute the actual space dimension of `lhs',
  // i.e., the highest dimension having a non-zero coefficient in `lhs'.
  for ( ; lhs_space_dim > 0; lhs_space_dim--)
    if (lhs.coefficient(Variable(lhs_space_dim - 1)) != 0)
      break;
  if (lhs_space_dim == 0) {
    // All variables have zero coefficients, so `lhs' is a constant.
    add_congruence((lhs %= rhs) / mod);
    return;
  }

  // Gather in `new_lines' the collections of all the lines having the
  // direction of variables occurring in `lhs'.  While at it, check
  // whether there exists a variable occurring in both `lhs' and
  // `rhs'.
  Generator_System new_lines;
  bool lhs_vars_intersect_rhs_vars = false;
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      new_lines.insert(line(Variable(i)), false);
      if (rhs.coefficient(Variable(i)) != 0)
	lhs_vars_intersect_rhs_vars = true;
    }

  if (lhs_vars_intersect_rhs_vars) {
    // Some variables in `lhs' also occur in `rhs'.
    // To ease the computation, add an additional dimension.
    const Variable new_var = Variable(space_dim);
    add_space_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to the right hand side.
    // TODO: use add_congruence_and_minimize() when it has been updated
    Congruence_System new_cgs1(new_var == rhs);
    if (add_recycled_congruences_and_minimize(new_cgs1)) {
      // The grid still contains points.

      // Cylindrificate on all the variables occurring in the left
      // hand side expression.
      // FIX this will do a redundant minimized check
      add_recycled_generators(new_lines);

      // Constrain the new dimension so that it is congruent to the left
      // hand side expression modulo `mod'.
      // TODO: use add_congruence() when it has been updated
      Congruence_System new_cgs2((lhs %= new_var) / mod);
      add_recycled_congruences(new_cgs2);
    }

    // Remove the temporarily added dimension.
    remove_higher_space_dimensions(space_dim-1);
  }
  else {
    // `lhs' and `rhs' variables are disjoint:
    // there is no need to add a further dimension.

    // Only add the lines and congruence if there are points.
    if (is_empty())
      return;

    // Cylindrificate on all the variables occurring in the left hand
    // side expression.
    add_recycled_generators(new_lines);

    // Constrain the left hand side expression so that it is congruent to
    // the right hand side expression modulo `mod'.
    add_congruence((lhs %= rhs) / mod);
  }

  assert(OK());
}

void PPL::Grid::
generalized_affine_preimage(const Linear_Expression& lhs,
			    const Linear_Expression& rhs,
			    Coefficient_traits::const_reference modulus) {
  // The dimension of `lhs' must be at most the dimension of `*this'.
  dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(e1, e2, m)",
				 "lhs", lhs);
  // The dimension of `rhs' must be at most the dimension of `*this'.
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(e1, e2, m)",
				 "e2", rhs);

  // Any preimage of an empty polyhedron is empty.
  if (marked_empty())
    return;

  // Compute the actual space dimension of `lhs',
  // i.e., the highest dimension having a non-zero coefficient in `lhs'.
  for ( ; lhs_space_dim > 0; lhs_space_dim--)
    if (lhs.coefficient(Variable(lhs_space_dim - 1)) != 0)
      break;

  TEMP_INTEGER(mod);
  if (modulus < 0)
    mod = -modulus;
  else
    mod = modulus;

  // If all variables have a zero coefficient, then `lhs' is a constant:
  // in this case, preimage and image happen to be the same.
  // FIX really?
  if (lhs_space_dim == 0) {
    generalized_affine_image(lhs, rhs, mod);
    return;
  }

  // Gather in `new_lines' the collections of all the lines having
  // the direction of variables occurring in `lhs'.
  // While at it, check whether or not there exists a variable
  // occurring in both `lhs' and `rhs'.
  Generator_System new_lines;
  bool lhs_vars_intersect_rhs_vars = false;
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      new_lines.insert(line(Variable(i)), false);
      if (rhs.coefficient(Variable(i)) != 0)
	lhs_vars_intersect_rhs_vars = true;
    }

  if (lhs_vars_intersect_rhs_vars) {
    // FIX this case is identical to same case in gen_affine_image
    // Some variables in `lhs' also occur in `rhs'.
    // To ease the computation, add an additional dimension.
    const Variable new_var = Variable(space_dim);
    add_space_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to `lhs'
    // TODO: use add_congruence_and_minimize() when it has been updated
    Congruence_System new_cgs1(new_var == rhs);
    if (add_recycled_congruences_and_minimize(new_cgs1)) {
      // The grid still contains points.

      // Cylindrificate on all the variables occurring in the left hand side
      add_recycled_generators(new_lines);

      // Constrain the new dimension so that it is related to
      // the right hand side modulo `mod'.
      // TODO: use add_congruence() when it has been updated
      Congruence_System new_cgs2((lhs %= new_var) / mod);
      add_recycled_congruences(new_cgs2);
    }

    // Remove the temporarily added dimension.
    remove_higher_space_dimensions(space_dim-1);
  }
  else {
    // `lhs' and `rhs' variables are disjoint:
    // there is no need to add a further dimension.

    // Constrain the left hand side expression so that it is congruent to
    // the right hand side expression modulo `mod'.
    add_congruence((lhs %= rhs) / mod);

    // Any image of an empty grid is empty.
    if (is_empty())
      return;

    // FIX why does this follow the add_congruence, whereas in the
    //     branch above (and in affine_image) it comes first
    // Cylindrificate on all the variables occurring in `lhs'.
    add_recycled_generators(new_lines);
  }
  assert(OK());
}

void
PPL::Grid::time_elapse_assign(const Grid& y) {
  Grid& x = *this;
  // Check dimension-compatibility.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("time_elapse_assign(y)", "y", y);

  // Deal with the zero-dimensional case.
  if (x.space_dim == 0) {
    if (y.marked_empty())
      x.set_empty();
    return;
  }

  // If either one of `x' or `y' is empty, the result is empty too.
  if (x.marked_empty())
    return;
  if (y.marked_empty()
      || (!x.generators_are_up_to_date() && !x.update_generators())
      || (!y.generators_are_up_to_date() && !y.update_generators())) {
    x.set_empty();
    return;
  }

  // At this point both generator systems are up-to-date.
  Generator_System gs = y.gen_sys;
  dimension_type gs_num_rows = gs.num_rows();

  normalize_divisors(gs, gen_sys);

  for (dimension_type i = gs_num_rows; i-- > 0; ) {
    Generator& g = gs[i];
    if (g.is_point())
      // Either erase the origin.
      if (g.all_homogeneous_terms_are_zero()) {
	--gs_num_rows;
	std::swap(g, gs[gs_num_rows]);
      }
      // Or transform the point into a parameter.
      else
	g[0] = 0;
  }

  if (gs_num_rows == 0)
    // `y' was the grid containing a single point at the origin, so
    // the result is `x'.
    return;

  // If it is present, erase the origin point.
  gs.erase_to_end(gs_num_rows);

  // Append `gs' to the generators of `x'.

  const dimension_type old_num_rows = gen_sys.num_rows();
  const dimension_type gs_num_columns = gs.num_columns();
  gen_sys.add_zero_rows(gs_num_rows,
			Linear_Row::Flags(gs.topology(),
					  Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
  for (dimension_type i = gs_num_rows; i-- > 0; ) {
    // Steal one coefficient at a time, to ensure that the row
    // capacities are all the same.
    Generator& new_g = gen_sys[old_num_rows + i];
    Generator& old_g = gs[i];
    if (old_g.is_line())
      new_g.set_is_line();
    for (dimension_type j = gs_num_columns; j-- > 0; )
      std::swap(new_g[j], old_g[j]);
  }

  x.clear_congruences_up_to_date();
  x.clear_generators_minimized();

  assert(x.OK(true) && y.OK(true));
}
#if 0
void
PPL::Grid::topological_closure_assign() {
  // Necessarily closed polyhedra are trivially closed.
  if (is_necessarily_closed())
    return;
  // Any empty or zero-dimensional polyhedron is closed.
  if (marked_empty() || space_dim == 0)
    return;

  // The computation can be done using congruences or generators.

  // Use congruences only if they are available.
  if (congruences_are_up_to_date()) {
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
    // FIX adds pending
    gen_sys.add_corresponding_points();
#if 0
    // We cannot have pending generators; this also implies
    // that generators may have lost their sortedness.
    gen_sys.unset_pending_rows();
#endif
    gen_sys.set_sorted(false);
    // Congruences are not up-to-date and generators are not minimized.
    clear_congruences_up_to_date();
    clear_generators_minimized();
  }
  assert(OK());
}
#endif

/*! \relates Parma_Polyhedra_Library:Grid */
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
  dim_kinds = y.dim_kinds;
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

bool
PPL::Grid::contains(const Grid& y) const {
  const Grid& x = *this;

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("contains(y)", "y", y);

  if (y.marked_empty())
    return true;
  if (x.marked_empty())
    return y.is_empty();
  if (y.space_dim == 0)
    return true;
  if (x.quick_equivalence_test(y) == Grid::TVB_TRUE)
    return true;
  return y.is_included_in(x);
}
#if 0
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
  status.ascii_dump(s);
  s << "con_sys ("
    << (congruences_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  con_sys.ascii_dump(s);
  s << "gen_sys ("
    << (generators_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl;
  gen_sys.ascii_dump(s);
  s << "dimension_kinds";
  if ((generators_are_up_to_date() && generators_are_minimized())
      || (congruences_are_up_to_date() && congruences_are_minimized()))
    for (Dimension_Kinds::const_iterator i = dim_kinds.begin();
	 i != dim_kinds.end();
	 ++i)
      s << " " << *i;
  s << endl;
}

void
PPL::Grid::ascii_dump() const {
  ascii_dump(std::cerr);
}

bool
PPL::Grid::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> space_dim))
    return false;

  if (!status.ascii_load(s))
    return false;

  if (!(s >> str) || str != "con_sys")
    return false;

  if (s >> str) {
    if (str == "(up-to-date)")
      set_congruences_up_to_date();
    else if (str != "(not_up-to-date)")
      return false;
  }
  else
    return false;

  if (!con_sys.ascii_load(s))
    return false;

  if (!(s >> str) || str != "gen_sys")
    return false;

  if (s >> str) {
    if (str == "(up-to-date)")
      set_generators_up_to_date();
    else if (str != "(not_up-to-date)")
      return false;
  }
  else
    return false;

  if (!gen_sys.ascii_load(s))
    return false;


  // FIX Move to follow status above, when status is fixed.

  if (!(s >> str) || str != "dimension_kinds")
    return false;

  if (!marked_empty()
      && ((generators_are_up_to_date() && generators_are_minimized())
	  || (congruences_are_up_to_date() && congruences_are_minimized()))) {
    dim_kinds.resize(space_dim + 1);
    for (Dimension_Kinds::size_type dim = 0; dim <= space_dim; ++dim) {
      // FIX read directly into dim_kinds[dim]?
      unsigned int dim_kind;
      if (!(s >> dim_kind) || (dim_kind > GEN_VIRTUAL))
	return false;
      switch(dim_kind) {
      case 0: dim_kinds[dim] = PARAMETER; break;
      case 1: dim_kinds[dim] = LINE; break;
      case 2: dim_kinds[dim] = GEN_VIRTUAL; break;
      default: return false;
      }
    }
  }


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
  else if (gr.is_universe())
    s << "true";
  else
    s << gr.minimized_congruences();
  return s;
}
