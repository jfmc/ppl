/* Grid class implementation: inline functions.
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

#ifndef PPL_Grid_templates_hh
#define PPL_Grid_templates_hh 1

#include "Interval.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Grid_Generator_System.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

template <typename Box>
Grid::Grid(const Box& box, From_Bounding_Box dummy)
  : con_sys(),
    gen_sys(NECESSARILY_CLOSED) {
  used(dummy);

  if (box.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(box, from_bounding_box)",
				   "the space dimension of box "
				   "exceeds the maximum allowed "
				   "space dimension");

  space_dim = box.space_dimension();

  TEMP_INTEGER(l_n);
  TEMP_INTEGER(l_d);

  // Check that all bounds are closed.  This must be done before the
  // empty check below, in case an open bound makes the grid empty.
  for (dimension_type k = space_dim; k-- > 0; ) {
    bool closed;
    // FIXME: Perhaps introduce box::is_bounded_and_closed.
    if (box.get_lower_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("Grid(box, from_bounding_box)", "box");
    if (box.get_upper_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("Grid(box, from_bounding_box)", "box");
  }

  if (box.is_empty()) {
    // Empty grid.
    set_empty();
    assert(OK());
    return;
  }

  if (space_dim == 0)
    set_zero_dim_univ();
  else {
    // Initialize the space dimension as indicated by the box.
    con_sys.increase_space_dimension(space_dim);
    // Add congruences according to `box'.
    TEMP_INTEGER(u_n);
    TEMP_INTEGER(u_d);
    for (dimension_type k = space_dim; k-- > 0; ) {
      bool closed;
      // TODO: Consider producing the system(s) in minimized form.
      // FIXME: Also create the generator system.
      if (box.get_lower_bound(k, closed, l_n, l_d)) {
	if (box.get_upper_bound(k, closed, u_n, u_d))
	  if (l_n * u_d == u_n * l_d) {
	    // A point interval sets dimension k of every point to a
	    // single value.
	    con_sys.insert(l_d * Variable(k) == l_n);
	    continue;
	  }
	// The only valid bounded interval is a point interval.
	throw_invalid_argument("Grid(box, from_bounding_box)", "box");
      }
      else if (box.get_upper_bound(k, closed, u_n, u_d))
	// An interval can only be a point or the universe.
	throw_invalid_argument("Grid(box, from_covering_box)",
			       "box");
      // A universe interval allows any value in dimension k.
    }
    set_congruences_up_to_date();
    gen_sys.unset_pending_rows();
    gen_sys.set_sorted(false);
  }

  assert(OK());
}

template <typename Box>
Grid::Grid(const Box& box, From_Covering_Box dummy)
  : con_sys(),
    gen_sys(NECESSARILY_CLOSED) {
  used(dummy);

  if (box.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(box, from_covering_box)",
				   "the space dimension of box "
				   "exceeds the maximum allowed "
				   "space dimension");

  space_dim = box.space_dimension();

  TEMP_INTEGER(l_n);
  TEMP_INTEGER(l_d);

  // Check that all bounds are closed.  This must be done before the
  // empty check below, in case an open bound makes the grid empty.
  for (dimension_type k = space_dim; k-- > 0; ) {
    bool closed;
    // FIXME: Perhaps introduce box::is_bounded_and_closed.
    if (box.get_lower_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("Grid(box, from_covering_box)", "box");
    if (box.get_upper_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("Grid(box, from_covering_box)", "box");
  }

  if (box.is_empty()) {
    // Empty grid.
    set_empty();
    assert(OK());
    return;
  }

  if (space_dim == 0)
    set_zero_dim_univ();
  else {
    // Initialize the space dimension as indicated by the box.
    con_sys.increase_space_dimension(space_dim);
    // Add congruences according to `box'.
    TEMP_INTEGER(u_n);
    TEMP_INTEGER(u_d);
    TEMP_INTEGER(d);
    for (dimension_type k = space_dim; k-- > 0; ) {
      bool closed;
      // TODO: Consider producing the system(s) in minimized form.
      // FIXME: Also create the generator system.
      if (box.get_lower_bound(k, closed, l_n, l_d)) {
	if (box.get_upper_bound(k, closed, u_n, u_d)) {
	  if (l_n * u_d == u_n * l_d)
	    // A point interval allows any point along the dimension
	    // k axis.
	    continue;
	  gcd_assign(d, l_d, u_d);
	  // `d' is the gcd of the divisors.
	  l_n *= (u_d / d);
	  d = l_d / d;
	  // `d' is now the smallest integer expression of the size
	  // of l_d relative to u_d.  `d * u_d' is the lcm of the
	  // divisors.
	  con_sys.insert((d * u_d * Variable(k) %= l_n) / ((u_n * d) - l_n));
	}
	else
	  // An interval bounded only from below produces an
	  // equality.
	  con_sys.insert(l_d * Variable(k) == l_n);
      }
      else
	if (box.get_upper_bound(k, closed, u_n, u_d))
	  // An interval bounded only from above produces an equality.
	  con_sys.insert(u_d * Variable(k) == u_n);
	else {
	  // Any universe interval produces an empty grid.
	  set_empty();
	  assert(OK());
	  return;
	}
    }
    set_congruences_up_to_date();
    gen_sys.set_sorted(false);
    gen_sys.unset_pending_rows();
  }

  assert(OK());
}

template <typename Box>
void
Grid::shrink_bounding_box(Box& box) const {
  // Dimension-compatibility check.
  if (space_dim > box.space_dimension())
    throw_dimension_incompatible("shrink_bounding_box(box)", "box",
				 box.space_dimension());

  TEMP_INTEGER(l_n);
  TEMP_INTEGER(l_d);

  // Check that all bounds are closed.
  for (dimension_type k = space_dim; k-- > 0; ) {
    bool closed;
    // FIXME: Perhaps introduce box::is_bounded_and_closed.
    if (box.get_lower_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("shrink_bounding_box(box)", "box");
    if (box.get_upper_bound(k, closed, l_n, l_d) && !closed)
      throw_invalid_argument("shrink_bounding_box(box)", "box");
  }

  if (marked_empty()) {
    box.set_empty();
    return;
  }
  if (space_dim == 0)
    return;
  if (!generators_are_up_to_date() && !update_generators()) {
    // Updating found the grid empty.
    box.set_empty();
    return;
  }

  assert(gen_sys.num_generators() > 0);

  dimension_type num_dims = gen_sys.num_columns() - 2 /* parameter divisor */;
  dimension_type num_rows = gen_sys.num_generators();

  // Create a vector to record which dimensions are bounded.
  std::vector<bool> bounded_interval(num_dims, true);

  const Grid_Generator *first_point = NULL;
  // Clear the bound flag in `bounded_interval' for all dimensions in
  // which a line or sequence of points extends away from a single
  // value in the dimension.
  for (dimension_type row = 0; row < num_rows; ++row) {
    Grid_Generator& gen = const_cast<Grid_Generator&>(gen_sys[row]);
    if (gen.is_point()) {
      if (first_point == NULL) {
	first_point = &gen_sys[row];
	continue;
      }
      const Grid_Generator& point = *first_point;
      // Convert the point `gen' to a parameter.
      for (dimension_type dim = 0; dim < num_dims; ++dim)
	gen[dim] -= point[dim];
      gen.divisor() = point.divisor();
    }
    for (dimension_type col = num_dims; col > 0; )
      if (gen[col--] != 0)
	bounded_interval[col] = false;
  }

  // Attempt to set both bounds of each boundable interval to the
  // value of the associated coefficient in the point.
  const Grid_Generator& point = *first_point;
  TEMP_INTEGER(divisor);
  TEMP_INTEGER(gcd);
  TEMP_INTEGER(bound);
  TEMP_INTEGER(reduced_divisor);
  divisor = point.divisor();
  for (dimension_type dim = 0; dim < num_dims; ++dim)
    if (bounded_interval[dim]) {
      // Reduce the bound fraction first.
      gcd_assign(gcd, point[dim+1], divisor);
      exact_div_assign(bound, point[dim+1], gcd);
      exact_div_assign(reduced_divisor, divisor, gcd);
      box.raise_lower_bound(dim, true, bound, reduced_divisor);
      box.lower_upper_bound(dim, true, bound, reduced_divisor);
    }
}

template <typename Box>
void
Grid::get_covering_box(Box& box) const {
  // Dimension-compatibility check.
  if (space_dim > box.space_dimension())
    throw_dimension_incompatible("get_covering_box(box)", "box",
				 box.space_dimension());

  Box new_box(box.space_dimension());

  if (marked_empty()) {
    box = new_box;
    box.set_empty();
    return;
  }
  if (space_dim == 0) {
    return;
  }
  if (!generators_are_up_to_date() && !update_generators()) {
    // Updating found the grid empty.
    box = new_box;
    box.set_empty();
    return;
  }

  assert(gen_sys.num_generators() > 0);

  dimension_type num_dims = gen_sys.num_columns() - 2 /* parameter divisor */;
  dimension_type num_rows = gen_sys.num_generators();

  TEMP_INTEGER(divisor);
  TEMP_INTEGER(gcd);
  TEMP_INTEGER(bound);
  TEMP_INTEGER(reduced_divisor);

  if (num_rows > 1) {
    Row interval_sizes(num_dims, Row::Flags());
    std::vector<bool> interval_emptiness(num_dims, false);

    // Store in `interval_sizes', for each column (that is, for each
    // dimension), the GCD of all the values in that column where the
    // row is of type parameter.

    for (dimension_type dim = num_dims; dim-- > 0; )
      interval_sizes[dim] = 0;
    const Grid_Generator *first_point = NULL;
    for (dimension_type row = 0; row < num_rows; ++row) {
      Grid_Generator& gen = const_cast<Grid_Generator&>(gen_sys[row]);
      if (gen.is_line()) {
	for (dimension_type dim = 0; dim < num_dims; ++dim)
	  if (!interval_emptiness[dim] && gen[dim+1] != 0) {
	    // Empty interval, set both bounds for associated
      	    // dimension to zero.
	    new_box.lower_upper_bound(dim, true, 0, 1);
	    new_box.raise_lower_bound(dim, true, 0, 1);
	    interval_emptiness[dim] = true;
	  }
	continue;
      }
      if (gen.is_point()) {
	if (first_point == NULL) {
	  first_point = &gen_sys[row];
	  continue;
	}
	const Grid_Generator& point = *first_point;
	// Convert the point `gen' to a parameter.
	for (dimension_type dim = 0; dim <= num_dims; ++dim)
	  gen[dim] -= point[dim];
	gen.divisor() = point.divisor();
      }
      for (dimension_type dim = 0; dim < num_dims; ++dim)
	if (!interval_emptiness[dim])
	  gcd_assign(interval_sizes[dim], interval_sizes[dim], gen[dim+1]);
    }

    // For each dimension set the lower bound of the interval to the
    // grid value closest to the origin, and the upper bound to the
    // addition of the lower bound and the shortest distance in the
    // given dimension between any two grid points.
    const Grid_Generator& point = *first_point;
    divisor = point.divisor();
    TEMP_INTEGER(lower_bound);
    for (dimension_type dim = 0; dim < num_dims; ++dim) {
      if (interval_emptiness[dim])
	continue;

      lower_bound = point[dim+1];

      // If the interval size is zero then all points have the same
      // value in this dimension, so set only the lower bound.
      if (interval_sizes[dim] != 0) {
	// Make the lower bound as close as possible to the origin,
	// leaving the sign the same.
	lower_bound %= interval_sizes[dim];
	// Check if the lowest value the other side of the origin is
	// closer to the origin, prefering the lowest positive if they
	// are equal.
	if (lower_bound > 0) {
	  if (interval_sizes[dim] - lower_bound < lower_bound)
	    lower_bound -= interval_sizes[dim];
	}
	else if (lower_bound < 0
		 && interval_sizes[dim] + lower_bound < - lower_bound)
	  lower_bound += interval_sizes[dim];

	// Reduce the bound fraction first.
	bound = interval_sizes[dim] + lower_bound;
	gcd_assign(gcd, bound, divisor);
	exact_div_assign(bound, bound, gcd);
	exact_div_assign(reduced_divisor, divisor, gcd);
	new_box.lower_upper_bound(dim, true, bound, reduced_divisor);
      }

      // Reduce the bound fraction first.
      gcd_assign(gcd, lower_bound, divisor);
      exact_div_assign(lower_bound, lower_bound, gcd);
      exact_div_assign(reduced_divisor, divisor, gcd);
      new_box.raise_lower_bound(dim, true, lower_bound, reduced_divisor);
    }
  }
  else {
    const Grid_Generator& point = gen_sys[0];
    divisor = point.divisor();
    // The covering box of a single point has only lower bounds.
    for (dimension_type dim = 0; dim < num_dims; ++dim) {
      // Reduce the bound fraction first.
      gcd_assign(gcd, point[dim+1], divisor);
      exact_div_assign(bound, point[dim+1], gcd);
      exact_div_assign(reduced_divisor, divisor, gcd);
      new_box.raise_lower_bound(dim, true, bound, reduced_divisor);
    }
  }

  box = new_box;
}

template <typename Partial_Function>
void
Grid::map_space_dimensions(const Partial_Function& pfunc) {
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the grid becomes zero_dimensional.
    if (marked_empty()
	|| (!generators_are_up_to_date() && !update_generators())) {
      // Removing all dimensions from the empty grid.
      space_dim = 0;
      set_empty();
    }
    else
      // Removing all dimensions from a non-empty grid.
      set_zero_dim_univ();

    assert(OK());
    return;
  }

  dimension_type new_space_dimension = pfunc.max_in_codomain() + 1;

  if (new_space_dimension == space_dim) {
    // The partial function `pfunc' is indeed total and thus specifies
    // a permutation, that is, a renaming of the dimensions.  For
    // maximum efficiency, we will simply permute the columns of the
    // constraint system and/or the generator system.

    // We first compute suitable permutation cycles for the columns of
    // the `con_sys' and `gen_sys' matrices.  We will represent them
    // with a linear array, using 0 as a terminator for each cycle
    // (notice that the columns with index 0 of `con_sys' and
    // `gen_sys' represent the inhomogeneous terms, and thus are
    // unaffected by the permutation of dimensions).
    // Cycles of length 1 will be omitted so that, in the worst case,
    // we will have `space_dim' elements organized in `space_dim/2'
    // cycles, which means we will have at most `space_dim/2'
    // terminators.
    std::vector<dimension_type> cycles;
    cycles.reserve(space_dim + space_dim/2);

    // Used to mark elements as soon as they are inserted in a cycle.
    std::deque<bool> visited(space_dim);

    for (dimension_type i = space_dim; i-- > 0; ) {
      if (!visited[i]) {
	dimension_type j = i;
	do {
	  visited[j] = true;
	  dimension_type k;
	  (void) pfunc.maps(j, k);
	  if (k == j)
	    // Cycle of length 1: skip it.
	    goto skip;

	  cycles.push_back(j+1);
	  // Go along the cycle.
	  j = k;
	} while (!visited[j]);
	// End of cycle: mark it.
	cycles.push_back(0);
      skip:
	;
      }
    }

    // If `cycles' is empty then `pfunc' is the identity.
    if (cycles.empty())
      return;

    // Permute all that is up-to-date.
    if (congruences_are_up_to_date()) {
      con_sys.permute_columns(cycles);
      clear_congruences_minimized();
    }

    if (generators_are_up_to_date()) {
      gen_sys.permute_columns(cycles);
      clear_generators_minimized();
    }

    assert(OK());
    return;
  }

  // If control gets here, then `pfunc' is not a permutation and some
  // dimensions must be projected away.

  const Grid_Generator_System& old_gensys = generators();

  if (old_gensys.num_generators() == 0) {
    // The grid is empty.
    Grid new_grid(new_space_dimension, EMPTY);
    std::swap(*this, new_grid);
    assert(OK());
    return;
  }

  // Make a local copy of the partial function.
  std::vector<dimension_type> pfunc_maps(space_dim, not_a_dimension());
  for (dimension_type j = space_dim; j-- > 0; ) {
    dimension_type pfunc_j;
    if (pfunc.maps(j, pfunc_j))
      pfunc_maps[j] = pfunc_j;
  }

  Grid_Generator_System new_gensys;
  // Set sortedness, for the assertion met via gs::insert.
  new_gensys.set_sorted(false);
  // Get the divisor of the first point.
  Grid_Generator_System::const_iterator i;
  Grid_Generator_System::const_iterator old_gensys_end = old_gensys.end();
  for (i = old_gensys.begin(); i != old_gensys_end; ++i)
    if (i->is_point())
      break;
  assert(i != old_gensys_end);
  Coefficient_traits::const_reference system_divisor = i->divisor();
  for (Grid_Generator_System::const_iterator i = old_gensys.begin();
       i != old_gensys_end;
       ++i) {
    const Grid_Generator& old_g = *i;
    Linear_Expression e(0 * Variable(new_space_dimension-1));
    bool all_zeroes = true;
    for (dimension_type j = space_dim; j-- > 0; ) {
      if (old_g.coefficient(Variable(j)) != 0
	  && pfunc_maps[j] != not_a_dimension()) {
	e += Variable(pfunc_maps[j]) * old_g.coefficient(Variable(j));
	all_zeroes = false;
      }
    }
    switch (old_g.type()) {
    case Grid_Generator::LINE:
      if (!all_zeroes)
	new_gensys.insert(grid_line(e));
      break;
    case Grid_Generator::PARAMETER:
      if (!all_zeroes)
	new_gensys.insert(parameter(e, system_divisor));
      break;
    case Grid_Generator::POINT:
      new_gensys.insert(grid_point(e, old_g.divisor()));
      break;
    case Grid_Generator::CLOSURE_POINT:
    default:
      assert(0);
    }
  }

  Grid new_grid(new_gensys);
  std::swap(*this, new_grid);

  assert(OK(true));
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Grid_templates_hh)
