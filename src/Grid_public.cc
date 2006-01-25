/* Grid class implementation (non-inline public functions).
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "Scalar_Products.defs.hh"

#include <cassert>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

PPL::Grid::Grid(dimension_type num_dimensions,
		const Degenerate_Element kind)
  : con_sys(),
    gen_sys(num_dimensions > max_space_dimension()
	    ? (throw_space_dimension_overflow("Grid(n, k)",
					      "n exceeds the maximum "
					      "allowed space dimension"),
	       0)
	    : num_dimensions) {

  space_dim = num_dimensions;

  if (kind == EMPTY) {
    // Set emptiness directly instead of with set_empty, as gen_sys is
    // already correctly initialized.

    status.set_empty();

    // Extend the zero dim false congruence system to the appropriate
    // dimension and then store it in `con_sys'.
    Congruence_System cgs(Congruence::zero_dim_false());
    cgs.increase_space_dimension(space_dim);
    const_cast<Congruence_System&>(con_sys).swap(cgs);

    assert(OK());
    return;
  }

  if (num_dimensions > 0) {
    con_sys.increase_space_dimension(num_dimensions);

    // Initialise both systems to universe representations.

    set_congruences_minimized();
    set_generators_minimized();
    dim_kinds.resize(num_dimensions + 1);

    // Extend the zero dim integrality congruence system to the
    // appropriate dimension and then store it in `con_sys'.
    Congruence_System cgs(Congruence::zero_dim_integrality());
    cgs.increase_space_dimension(space_dim);
    cgs[0][0] = 1; // Recover minimal form after cgs(zdi) normalization.
    con_sys.swap(cgs);

    dim_kinds[0] = PROPER_CONGRUENCE /* a.k.a. PARAMETER */;

    // Trivially true point.
    gen_sys.insert(grid_point(0*(Variable(0))));

    // A line for each dimension.
    dimension_type dim = 0;
    while (dim < num_dimensions) {
      gen_sys.insert(grid_line(Variable(dim++)));
      dim_kinds[dim] = CON_VIRTUAL /* a.k.a. LINE */;
    }
  }
  else
    set_zero_dim_univ();

  assert(OK());
}

PPL::Grid::Grid(const Grid& y)
  : con_sys(),
    gen_sys(),
    status(y.status),
    space_dim(y.space_dim),
    dim_kinds(y.dim_kinds) {
  if (space_dim == 0) {
    con_sys = y.con_sys;
    gen_sys = y.gen_sys;
  }
  else {
    if (y.congruences_are_up_to_date())
      con_sys = y.con_sys;
    else
      con_sys.increase_space_dimension(space_dim);
    if (y.generators_are_up_to_date())
      gen_sys = y.gen_sys;
    else
      gen_sys = Grid_Generator_System(y.space_dim);
  }
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

PPL::Grid::Grid(Constraint_System& cs) {
  if (cs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(cs)",
				   "the space dimension of cs "
				   "exceeds the maximum allowed "
				   "space dimension");
  // FIXME: Adapt and use cs instead of using a copy.
  Congruence_System cgs;
  cgs.insert(0*Variable(cs.space_dimension() - 1) %= 1);
  for (Constraint_System::const_iterator i = cs.begin(),
         cs_end = cs.end(); i != cs_end; ++i)
    if (i->is_equality())
      cgs.insert(*i);
  construct(cgs);
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

PPL::dimension_type
PPL::Grid::affine_dimension() const {
  if (space_dim == 0 || is_empty())
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
    assert(con_sys.num_columns() == 2 && con_sys.num_rows() == 0);
    return con_sys;
  }

  congruences_are_up_to_date() || update_congruences();

  return con_sys;
}

const PPL::Congruence_System&
PPL::Grid::minimized_congruences() const {
  if (space_dim == 0) {
    if (!marked_empty()) {
      // Ensure the congruences are minimal by extending a zero dim
      // universe congruence system to the appropriate dimension and
      // then storing it in `con_sys'.
      Congruence_System cgs(Congruence::zero_dim_integrality());
      cgs.increase_space_dimension(space_dim);
      cgs[0][0] = 1; // Recover minimal form after cgs(zdi) normalization.
      const_cast<Congruence_System&>(con_sys).swap(cgs);
    }
    return con_sys;
  }
  if (congruences_are_up_to_date() && !congruences_are_minimized()) {
    // Minimize the congruences.
    Grid& gr = const_cast<Grid&>(*this);
    if (gr.simplify(gr.con_sys, gr.dim_kinds))
      gr.set_empty();
    else
      gr.set_congruences_minimized();
  }
  return congruences();
}

const PPL::Grid_Generator_System&
PPL::Grid::generators() const {
  if (space_dim == 0) {
    assert(gen_sys.space_dimension() == 0
	   && gen_sys.num_rows() == (marked_empty() ? 0 : 1));
    return gen_sys;
  }

  if (marked_empty()) {
    assert(gen_sys.num_rows() == 0);
    return gen_sys;
  }

  if (!generators_are_up_to_date() && !update_generators()) {
    // Updating found the grid empty.
    const_cast<Grid&>(*this).set_empty();
    return gen_sys;
  }

  return gen_sys;
}

const PPL::Grid_Generator_System&
PPL::Grid::minimized_generators() const {
  if (space_dim == 0) {
    assert(gen_sys.space_dimension() == 0
	   && gen_sys.num_rows() == (marked_empty() ? 0 : 1));
    return gen_sys;
  }

  if (marked_empty()) {
    assert(gen_sys.num_rows() == 0);
    return gen_sys;
  }

  if (generators_are_up_to_date()) {
    if (!generators_are_minimized()) {
      // Minimize the generators.
      Grid& gr = const_cast<Grid&>(*this);
      gr.simplify(gr.gen_sys, gr.dim_kinds);
      gr.set_generators_minimized();
    }
  }
  else if (!update_generators()) {
    // Updating found the grid empty.
    const_cast<Grid&>(*this).set_empty();
    return gen_sys;
  }

  return gen_sys;
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
    // FIXME: Confirm that these are correct, especially the first.
    if (cg.is_trivial_false())
      return Poly_Con_Relation::is_disjoint();
    else if (cg.is_equality() || cg.inhomogeneous_term() == 0)
      return Poly_Con_Relation::is_included();
    else if (cg.inhomogeneous_term() % cg.modulus() == 0)
      return Poly_Con_Relation::is_included();
    else
      // cg is false.
      return Poly_Con_Relation::is_disjoint();

  if (!generators_are_up_to_date() && !update_generators())
    // Updating found the grid empty.
    return Poly_Con_Relation::is_included()
      && Poly_Con_Relation::is_disjoint();

  // Return one of the relations
  // 'strictly_intersects'   a strict subset of the grid points satisfy cg
  // 'is_included'	     every grid point satisfies cg
  // 'is_disjoint'	     cg and the grid occupy seperate spaces.

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

  for (Grid_Generator_System::const_iterator g = gen_sys.begin(),
         gen_sys_end = gen_sys.end(); g != gen_sys_end; ++g) {
    TEMP_INTEGER(sp);
    Scalar_Products::assign(sp, cg, *g);

    switch (g->type()) {

    case Grid_Generator::POINT:
      if (cg.is_proper_congruence())
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
	    gcd_assign(div, div, sp);
	    if (point_sp % div == 0)
	      // There is a point in the grid satisfying cg.
	      return Poly_Con_Relation::strictly_intersects();
	  }
	}
      break;

    case Grid_Generator::PARAMETER:
      if (cg.is_proper_congruence())
	sp %= (modulus * g->divisor());
      if (sp == 0)
	// Parameter g satisfies the cg so the relation depends
	// entirely on the other generators.
	break;

      if (known_to_intersect)
	// At least one point satisfies cg.  However, the sum of such
	// a point and the parameter g fails to satisfy cg (due to g).
	return Poly_Con_Relation::strictly_intersects();

      // Find the GCD between sp and the previous GCD.
      gcd_assign(div, div, sp);
      if (point_sp != 0)
	// At least one of any previously encountered points fails to
	// satisfy cg.
	if (point_sp == div)
	  // There is also a grid point that satisfies cg.
	  return Poly_Con_Relation::strictly_intersects();

      break;

    case Grid_Generator::LINE:
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
    }
  }

  if (point_sp == 0)
    // Every generator satisfied the cg.
    return Poly_Con_Relation::is_included();

  assert(!known_to_intersect);
  return Poly_Con_Relation::is_disjoint();
}

PPL::Poly_Gen_Relation
PPL::Grid::relation_with(const Grid_Generator& g) const {
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
    con_sys.satisfies_all_congruences(g)
    ? Poly_Gen_Relation::subsumes()
    : Poly_Gen_Relation::nothing();
}

inline bool
PPL::Grid::is_empty() const {
  if (marked_empty())
    return true;
  // Try a fast-fail test: if generators are up-to-date then the
  // generator system (since it is well formed) contains a point.
  if (generators_are_up_to_date())
    return false;
  if (space_dim == 0)
    return false;
  if (congruences_are_minimized())
    // If the grid was empty it would be marked empty.
    return false;
  // Minimize the congruences to check if the grid is empty.
  Grid& gr = const_cast<Grid&>(*this);
  if (gr.simplify(gr.con_sys, gr.dim_kinds)) {
    gr.set_empty();
    return true;
  }
  gr.set_congruences_minimized();
  return false;
}

bool
PPL::Grid::is_universe() const {
  if (marked_empty())
    return false;

  if (space_dim == 0)
    return true;

  if (congruences_are_up_to_date()) {
    if (congruences_are_minimized())
      // The mininimized universe congruence system has only one row,
      // the integrality congruence.
      return con_sys.num_rows() == 1 && con_sys[0].is_trivial_true();
  }
  else {
    update_congruences();
    return con_sys.num_rows() == 1 && con_sys[0].is_trivial_true();
  }

  // Test con_sys's inclusion in a universe generator system.

  // The zero dimension cases are handled above.
  Variable var(space_dim - 1);
  for (dimension_type i = space_dim; i-- > 0; )
    if (!con_sys.satisfies_all_congruences(grid_line(Variable(i) + var)))
      return false;
  if (con_sys.satisfies_all_congruences(grid_point(0*var)))
    return true;
  return false;
}

bool
PPL::Grid::is_bounded() const {
  // A zero-dimensional or empty grid is bounded.
  if (space_dim == 0
      || marked_empty()
      || (!generators_are_up_to_date() && !update_generators()))
    return true;

  // TODO: Consider using con_sys when gen_sys is out of date.

  if (gen_sys.num_rows() > 1) {
    // Check if all generators are the same point.
    const Grid_Generator& first_point = gen_sys[0];
    if (first_point.is_line_or_parameter())
      return false;
    for (dimension_type row = gen_sys.num_rows(); row-- > 0; ) {
      const Grid_Generator& gen = gen_sys[row];
      if (gen.is_line_or_parameter() || gen != first_point)
	return false;
    }
  }
  return true;
}

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

  if (congruences_are_minimized())
    return con_sys.has_a_free_dimension();

  Grid& gr = const_cast<Grid&>(*this);
  if (generators_are_up_to_date()) {
    // Minimize the generator system.
    gr.simplify(gr.gen_sys, gr.dim_kinds);
    gr.set_generators_minimized();

    goto line_search;
  }

  // Generators are out of date.

  // Minimize the congruence system to find out whether it is empty.
  if (gr.simplify(gr.con_sys, gr.dim_kinds)) {
    // The congruence system reduced to the empty grid.
    gr.set_empty();
    return true;
  }
  gr.set_congruences_minimized();

  return gr.con_sys.has_a_free_dimension();
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
      if (gen_sys[row].is_parameter())
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

    if (con_sys.space_dimension() != space_dim) {
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

  // A zero-dimensional universe grid is legal only if the system of
  // congruences `con_sys' is empty, and the generator system contains
  // one point.
  if (space_dim == 0) {
    if (con_sys.num_rows() == 0)
      if (gen_sys.num_rows() == 1 && gen_sys[0].is_point())
	return true;
#ifndef NDEBUG
    cerr << "Zero-dimensional grid should have an empty congruence" << endl
	 << "system and a generator system of a single point." << endl;
#endif
    goto fail;
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
      if (gen_sys.space_dimension() + 1 != num_columns) {
#ifndef NDEBUG
	cerr << "Incompatible size! (gen_sys and space_dim)"
	     << endl;
#endif
	goto fail;
      }

      // Check if the system of generators is well-formed.
      if (!gen_sys.OK()) {
#ifndef NDEBUG
	cerr << "gen_sys OK failed." << endl;
#endif
	goto fail;
      }
      // Check each generator in the system.
      for (dimension_type i = gen_sys.num_rows(); i-- > 0; ) {
	const Grid_Generator& g = gen_sys[i];

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
	Grid_Generator_System gs = gen_sys;

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
	      || (gen_sys[row++].is_parameter_or_point()
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
	Dimension_Kinds dk = dim_kinds;
	// `gs' is minimized and marked_empty returned false, so `gs'
	// should contain rows.
	assert(gs.num_rows() > 0);
	simplify(gs, dk);
	// gs contained rows before being reduced, so it should
	// contain at least a single point afterwards.
	assert(gs.num_rows() > 0);
	for (dimension_type row = 0; row < gen_sys.num_rows(); ++row) {
	  Grid_Generator& g = gs[row];
	  const Grid_Generator& g_copy = gen_sys[row];
	  if (g.is_equal_to(g_copy))
	    continue;
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

    Grid tem_gr = *this;
    Congruence_System cs_copy = tem_gr.con_sys;

    // Clear the generators in tem_gr.
    Grid_Generator_System gs(space_dim);
    std::swap(tem_gr.gen_sys, gs);
    tem_gr.clear_generators_up_to_date();

    if (!tem_gr.update_generators()) {
      if (check_not_empty) {
	// Want to know the satisfiability of the congruences.
#ifndef NDEBUG
	cerr << "Unsatisfiable system of congruences!"
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
      if (!con_sys.is_equal_to(cs_copy)) {
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

  // Adding a new congruence to an empty grid results in an empty
  // grid.
  if (marked_empty())
    return;

  // Dealing with a zero-dimensional space grid first.
  if (space_dim == 0) {
    if (!cg.is_trivial_true())
      set_empty();
    return;
  }

  congruences_are_up_to_date() || update_congruences();

  con_sys.insert(cg);

  clear_congruences_minimized();
  set_congruences_up_to_date();
  clear_generators_up_to_date();

  // Note: the congruence system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

void
PPL::Grid::add_congruence(const Constraint& c) {
  // TODO: this is just an executable specification.
  if (c.is_equality()) {
    Congruence_System cgs(c);
    add_recycled_congruences(cgs);
  }
}

bool
PPL::Grid::add_congruence_and_minimize(const Congruence& cg) {
  // TODO: this is just an executable specification.
  Congruence_System cgs(cg);
  return add_recycled_congruences_and_minimize(cgs);
}

bool
PPL::Grid::add_congruence_and_minimize(const Constraint& c) {
  // TODO: this is just an executable specification.
  if (c.is_equality()) {
    Congruence_System cgs(c);
    return add_recycled_congruences_and_minimize(cgs);
  }
  return minimize();
}

void
PPL::Grid::add_generator(const Grid_Generator& g) {
  // The dimension of `g' must be at most space_dim.
  const dimension_type g_space_dim = g.space_dimension();
  if (space_dim < g_space_dim)
    throw_dimension_incompatible("add_generator(g)", "g", g);

  // Deal with zero-dimension case first.
  if (space_dim == 0) {
    // Points are the only zero-dimension generators that can be
    // created.
    assert(g.is_point());
    if (marked_empty())
      set_zero_dim_univ();
    assert(OK());
    return;
  }

  if (marked_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Here the grid is empty: the specification says we can only
    // insert a point.
    if (g.is_line_or_parameter())
      throw_invalid_generator("add_generator(g)", "g");
    gen_sys.insert(g);
    clear_empty();
  }
  else {
    assert(generators_are_up_to_date());
    gen_sys.insert(g);
    if (g.is_parameter_or_point())
      normalize_divisors(gen_sys);
  }

  // With the added generator, congruences are out of date.
  clear_congruences_up_to_date();

  clear_generators_minimized();
  set_generators_up_to_date();
  assert(OK());
}

bool
PPL::Grid::add_generator_and_minimize(const Grid_Generator& g) {
  // TODO: this is just an executable specification.
  Grid_Generator_System gs(g);
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
    // In a 0-dimensional space the congruences are trivial (e.g., 0
    // == 0 or 1 %= 0) or false (e.g., 1 == 0).  In a system of
    // congruences `begin()' and `end()' are equal if and only if the
    // system contains only trivial congruences.
    if (cgs.begin() == cgs.end()) {
      if (marked_empty())
	set_zero_dim_univ();
      return;
    }
    // There is a congruence, it must be false, the grid is empty.
    if (status.test_zero_dim_univ())
      set_empty();
    return;
  }

  if (marked_empty()) {
    assert(OK());
    return;
  }

  // The congruences are required.
  congruences_are_up_to_date() || update_congruences();

  // Swap (instead of copying) the coefficients of `cgs' (which is
  // writable).
  con_sys.recycling_insert(cgs);

  // Congruences may not be minimized and generators are out of date.
  clear_congruences_minimized();
  clear_generators_up_to_date();
  // Note: the congruence system may have become unsatisfiable, thus
  // we do not check for satisfiability.
  assert(OK());
}

void
PPL::Grid::add_recycled_congruences(Constraint_System& cs) {
  // TODO: this is just an executable specification.
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_recycled_congruences(cs)", "cs", cs);
  Congruence_System cgs(cs);
  add_recycled_congruences(cgs);
}

void
PPL::Grid::add_congruences(const Congruence_System& cgs) {
  // TODO: this is just an executable specification.
  // The dimension of `cgs' must be at most `space_dim'.
  if (space_dim < cgs.space_dimension())
    throw_dimension_incompatible("add_congruences(cgs)", "cgs", cgs);
  Congruence_System cgs_copy = cgs;
  add_recycled_congruences(cgs_copy);
}

void
PPL::Grid::add_congruences(const Constraint_System& cs) {
  // TODO: this is just an executable specification.
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_congruences(cs)", "cs", cs);
  Congruence_System cgs(cs);
  add_recycled_congruences(cgs);
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
    // In a 0-dimensional space the congruences are trivial (e.g., 0
    // == 0 or 1 %= 0) or false (e.g., 1 == 0).  In a system of
    // congruences `begin()' and `end()' are equal if and only if the
    // system contains only trivial congruences.
    if (cgs.begin() == cgs.end()) {
      if (marked_empty())
	set_zero_dim_univ();
      return true;
    }
    // There is a congruence, it must be false, the grid is empty.
    if (status.test_zero_dim_univ())
      set_empty();
    return false;
  }

  if (marked_empty())
    return false;

  congruences_are_up_to_date() || update_congruences();

  con_sys.insert(cgs);

  clear_congruences_minimized();

#ifndef NDEBUG
  bool ret = update_generators();
  assert(OK());
  return ret;
#else
  return update_generators();
#endif
}

bool
PPL::Grid::add_recycled_congruences_and_minimize(Constraint_System& cs) {
  // TODO: this is just an executable specification.
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_recycled_congruences_and_minimize(cs)",
				 "cs", cs);
  Congruence_System cgs(cs);
  return add_recycled_congruences_and_minimize(cgs);
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
  Congruence_System cgs(cs);
  return add_recycled_congruences_and_minimize(cgs);
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

bool
PPL::Grid::add_constraint_and_minimize(const Constraint& c) {
  // The dimension of `c' must be at most `space_dim'.
  if (space_dim < c.space_dimension())
    throw_dimension_incompatible("add_constraint_and_minimize(c)", "c", c);
  if (c.is_equality()) {
    Congruence cg(c);
    return add_congruence_and_minimize(cg);
  }
  return minimize();
}

void
PPL::Grid::add_constraints(const Constraint_System& cs) {
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_constraints(cs)", "cs", cs);
  Congruence_System cgs(cs);
  add_recycled_congruences(cgs);
}

bool
PPL::Grid::add_constraints_and_minimize(const Constraint_System& cs) {
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_constraints_and_minimize(cs)",
				 "cs", cs);
  Congruence_System cgs(cs);
  return add_recycled_congruences_and_minimize(cgs);
}

void
PPL::Grid::add_recycled_constraints(Constraint_System& cs) {
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_recycled_constraints(cs)",
				 "cs", cs);
  Congruence_System cgs(cs);
  add_recycled_congruences(cgs);
}

bool
PPL::Grid::add_recycled_constraints_and_minimize(Constraint_System& cs) {
  // The dimension of `cs' must be at most `space_dim'.
  if (space_dim < cs.space_dimension())
    throw_dimension_incompatible("add_recycled_constraints_and_minimize(cs)",
				 "cs", cs);
  Congruence_System cgs(cs);
  return add_recycled_congruences_and_minimize(cgs);
}

void
PPL::Grid::add_recycled_generators(Grid_Generator_System& gs) {
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
    if (marked_empty())
      if (gs.has_points())
	set_zero_dim_univ();
      else
	throw_invalid_generators("add_recycled_generators(gs)", "gs");
    assert(OK(true));
    return;
  }

  if (!marked_empty()
      && (generators_are_up_to_date() || update_generators())) {
    // The grid contains at least one point.

    normalize_divisors(gs, gen_sys);

    gen_sys.recycling_insert(gs);

    // Congruences are out of date and generators are not minimized.
    clear_congruences_up_to_date();
    clear_generators_minimized();

    assert(OK(true));
    return;
  }

  // The grid is empty.

  // `gs' must contain at least one point.
  if (!gs.has_points())
    throw_invalid_generators("add_recycled_generators(gs)", "gs");

  // Adjust `gs' to the right dimension.
  gs.insert(parameter(0*Variable(space_dim-1)));

  std::swap(gen_sys, gs);

  normalize_divisors(gen_sys);

  // The grid is no longer empty and generators are up-to-date.
  set_generators_up_to_date();
  clear_empty();

  assert(OK());
}

void
PPL::Grid::add_generators(const Grid_Generator_System& gs) {
  // TODO: this is just an executable specification.
  Grid_Generator_System gs_copy = gs;
  add_recycled_generators(gs_copy);
}

bool
PPL::Grid::add_recycled_generators_and_minimize(Grid_Generator_System& gs) {
  // Dimension-compatibility check: the dimension of `gs' must be less
  // than or equal to that of space_dim.
  const dimension_type gs_space_dim = gs.space_dimension();
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("add_recycled_generators_and_minimize(gs)",
				 "gs", gs);

  // Adding no generators is equivalent to just requiring reduction.
  if (gs.num_rows() == 0)
    return minimize();

  // Adding valid generators to a zero-dimensional grid produces the
  // zero-dimensional universe grid.
  if (space_dim == 0) {
    if (marked_empty())
      if (gs.has_points())
	set_zero_dim_univ();
      else
	throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
				 "gs");
    assert(OK(true));
    return true;
  }

  // Adjust `gs' to the right dimension.
  gs.insert(parameter(0*Variable(space_dim-1)));

  if (!marked_empty()
      && (generators_are_up_to_date() || update_generators())) {
    // The grid contains at least one point.
    normalize_divisors(gs, gen_sys);

    for (dimension_type row = 0; row < gs.num_rows(); ++row)
      gen_sys.recycling_insert(gs[row]);
  }
  else {
    // The grid is empty: check if `gs' contains a point.
    if (!gs.has_points())
      throw_invalid_generators("add_recycled_generators_and_minimize(gs)",
			       "gs");
    std::swap(gen_sys, gs);
    normalize_divisors(gen_sys);
    clear_empty();
  }
  clear_generators_minimized();
  update_congruences();

  assert(OK(true));
  return true;
}

bool
PPL::Grid::add_generators_and_minimize(const Grid_Generator_System& gs) {
  // TODO: this is just an executable specification.
  Grid_Generator_System gs_copy = gs;
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
  // necessarily universe, so the intersection is also universe.
  if (x.space_dim == 0)
    return;

  // The congruences must be up-to-date.
  x.congruences_are_up_to_date() || x.update_congruences();
  y.congruences_are_up_to_date() || y.update_congruences();

  if (y.con_sys.num_rows() > 0 ) {
    x.con_sys.insert(y.con_sys);
    // Grid_Generators may be out of date and congruences may have changed
    // from minimal form.
    x.clear_generators_up_to_date();
    x.clear_congruences_minimized();
  }

  // `y' should still contain a point.
  assert(x.OK() && y.OK(true));
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
  Grid_Generator_System gs(y.gen_sys);
  normalize_divisors(x.gen_sys, gs);
  x.gen_sys.recycling_insert(gs);
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
      // Grid_Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator);
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
	neg_assign(inverse[var_space_dim]);
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
      // Grid_Generator_System::affine_image() requires the third argument
      // to be a positive Coefficient.
      if (denominator > 0)
	gen_sys.affine_image(var_space_dim, expr, denominator);
      else
	gen_sys.affine_image(var_space_dim, -expr, -denominator);

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
	gen_sys.affine_image(var_space_dim, inverse, expr[var_space_dim]);
      }
      else {
	// The new denominator is negative:
	// we negate everything once more, as Grid_Generator_System::affine_image()
	// requires the third argument to be positive.
	inverse = expr;
	inverse[var_space_dim] = denominator;
	neg_assign(inverse[var_space_dim]);
	gen_sys.affine_image(var_space_dim, inverse, -expr[var_space_dim]);
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

  if (modulus < 0)
    gen_sys.insert(parameter(-modulus * var));
  else
    gen_sys.insert(parameter(modulus * var));

  normalize_divisors(gen_sys);

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
  add_generator(grid_line(var));
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
  Grid_Generator_System new_lines;
  bool lhs_vars_intersect_rhs_vars = false;
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      new_lines.insert(Grid_Generator::line(Variable(i)));
      if (rhs.coefficient(Variable(i)) != 0)
	lhs_vars_intersect_rhs_vars = true;
    }

  if (lhs_vars_intersect_rhs_vars) {
    // Some variables in `lhs' also occur in `rhs'.
    // To ease the computation, add an additional dimension.
    const Variable new_var = Variable(space_dim);
    add_space_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to the right hand side.
    // TODO: Use add_congruence_and_minimize() when it has been updated.
    Congruence_System new_cgs1(new_var == rhs);
    if (add_recycled_congruences_and_minimize(new_cgs1)) {
      // The grid still contains points.

      // Cylindrificate on all the variables occurring in the left
      // hand side expression.

      // Ajust `new_lines' to the right dimension.
      new_lines.insert(parameter(0*Variable(space_dim-1)));
      // Add the lines to `gen_sys'.
      gen_sys.recycling_insert(new_lines);
      normalize_divisors(gen_sys);
      // Update the flags.
      clear_congruences_up_to_date();
      clear_generators_minimized();

      // Constrain the new dimension so that it is congruent to the left
      // hand side expression modulo `mod'.
      // TODO: Use add_congruence() when it has been updated.
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
  for ( ; lhs_space_dim > 0; --lhs_space_dim)
    if (lhs.coefficient(Variable(lhs_space_dim - 1)) != 0)
      break;

  TEMP_INTEGER(mod);
  if (modulus < 0)
    mod = -modulus;
  else
    mod = modulus;

  // If all variables have zero coefficients, then `lhs' is a
  // constant: in this case, preimage and image happen to be the same.
  if (lhs_space_dim == 0) {
    add_congruence((lhs %= rhs) / mod);
    return;
  }

  // Gather in `new_lines' the collections of all the lines having
  // the direction of variables occurring in `lhs'.
  // While at it, check whether or not there exists a variable
  // occurring in both `lhs' and `rhs'.
  Grid_Generator_System new_lines;
  bool lhs_vars_intersect_rhs_vars = false;
  for (dimension_type i = lhs_space_dim; i-- > 0; )
    if (lhs.coefficient(Variable(i)) != 0) {
      new_lines.insert(Grid_Generator::line(Variable(i)));
      if (rhs.coefficient(Variable(i)) != 0)
	lhs_vars_intersect_rhs_vars = true;
    }

  if (lhs_vars_intersect_rhs_vars) {
    // Some variables in `lhs' also occur in `rhs'.
    // To ease the computation, add an additional dimension.
    const Variable new_var = Variable(space_dim);
    add_space_dimensions_and_embed(1);

    // Constrain the new dimension to be equal to `lhs'
    // TODO: Use add_congruence_and_minimize() when it has been updated.
    Congruence_System new_cgs1(new_var == lhs);
    if (add_recycled_congruences_and_minimize(new_cgs1)) {
      // The grid still contains points.

      // Cylindrificate on all the variables occurring in the left
      // hand side

      // Ajust `new_lines' to the right dimension.
      new_lines.insert(parameter(0*Variable(space_dim-1)));
      // Add the lines to `gen_sys'.
      gen_sys.recycling_insert(new_lines);
      normalize_divisors(gen_sys);
      // Update the flags.
      clear_congruences_up_to_date();
      clear_generators_minimized();

      // Constrain the new dimension so that it is related to
      // the right hand side modulo `mod'.
      // TODO: Use add_congruence() when it has been updated.
      Congruence_System new_cgs2((rhs %= new_var) / mod);
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
  Grid_Generator_System gs = y.gen_sys;
  dimension_type gs_num_rows = gs.num_rows();

  normalize_divisors(gs, gen_sys);

  for (dimension_type i = gs_num_rows; i-- > 0; ) {
    Grid_Generator& g = gs[i];
    if (g.is_point()) {
      // Transform the point into a parameter.
      TEMP_INTEGER(div);
      div = g.divisor();
      g.divisor() = 0;
      g.divisor() = div;
    }
  }

  if (gs_num_rows == 0)
    // `y' was the grid containing a single point at the origin, so
    // the result is `x'.
    return;

  // Append `gs' to the generators of `x'.

  gen_sys.recycling_insert(gs);

  x.clear_congruences_up_to_date();
  x.clear_generators_minimized();

  assert(x.OK(true) && y.OK(true));
}

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

bool
PPL::Grid::is_disjoint_from(const Grid& y) const {
  // Dimension-compatibility check.
  if (space_dim != y.space_dim)
    throw_dimension_incompatible("is_disjoint_from(y)", "y", y);
  Grid z = *this;
  return !z.intersection_assign_and_minimize(y);
}

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

PPL_OUTPUT_DEFINITIONS(Grid);

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

  if (!(s >> str) || str != "dimension_kinds")
    return false;

  if (!marked_empty()
      && ((generators_are_up_to_date() && generators_are_minimized())
	  || (congruences_are_up_to_date() && congruences_are_minimized()))) {
    dim_kinds.resize(space_dim + 1);
    for (Dimension_Kinds::size_type dim = 0; dim <= space_dim; ++dim) {
      short unsigned int dim_kind;
      if (!(s >> dim_kind))
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
