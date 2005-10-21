/* Grid class implementation: inline functions.
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

#ifndef PPL_Grid_inlines_hh
#define PPL_Grid_inlines_hh 1

#include "Interval.defs.hh"
#include "Generator.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

inline dimension_type
Grid::max_space_dimension() {
  using std::min;
  // One dimension is reserved to have a value of type dimension_type
  // that does not represent a legal dimension.
  return min(std::numeric_limits<dimension_type>::max() - 1,
	     min(Congruence_System::max_space_dimension(),
		 Generator_System::max_space_dimension()
		 )
	     );
}

inline void
Grid::set_congruences_up_to_date() {
  status.set_c_up_to_date();
}

inline
Grid::Grid(const Congruence_System& ccgs) {
  if (ccgs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(ccgs)",
				   "the space dimension of ccgs "
				   "exceeds the maximum allowed "
				   "space dimension");
  construct(ccgs);
}

inline
Grid::Grid(Congruence_System& cgs) {
  if (cgs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(cgs)",
				   "the space dimension of cgs "
				   "exceeds the maximum allowed "
				   "space dimension");
  construct(cgs);
}

inline
Grid::Grid(const Generator_System& gs) {
  if (gs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(gs)",
				   "the space dimension of gs "
				   "exceeds the maximum allowed "
				   "space dimension");
  construct(gs);
}

inline
Grid::Grid(Generator_System& gs,
	   const bool convert_rays_to_lines) {
  if (gs.space_dimension() > max_space_dimension())
    throw_space_dimension_overflow("Grid(gs)",
				   "the space dimension of gs "
				   "exceeds the maximum allowed "
				   "space dimension");
  construct(gs, convert_rays_to_lines);
}

inline
Grid::~Grid() {
}

#if 0
inline memory_size_type
Grid::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline Topology
Grid::topology() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.topology();
}

inline bool
Grid::is_necessarily_closed() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.is_necessarily_closed();
}
#endif

inline dimension_type
Grid::space_dimension() const {
  return space_dim;
}

inline void
Grid::upper_bound_assign(const Grid& y) {
  join_assign(y);
}

inline bool
Grid::upper_bound_assign_if_exact(const Grid& y) {
  return join_assign_if_exact(y);
}

inline void
PPL::Grid::difference_assign(const Grid& y) {
  grid_difference_assign(y);
}

inline void
Grid::swap(Grid& y) {
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
  std::swap(status, y.status);
  std::swap(space_dim, y.space_dim);
  std::swap(dim_kinds, y.dim_kinds);
}

} // namespace Parma_Polyhedra_Library

/*! \relates Parma_Polyhedra_Library::Grid */
inline void
std::swap(Parma_Polyhedra_Library::Grid& x,
	  Parma_Polyhedra_Library::Grid& y) {
  x.swap(y);
}

namespace Parma_Polyhedra_Library {

inline bool
Grid::marked_empty() const {
  return status.test_empty();
}

inline bool
Grid::congruences_are_up_to_date() const {
  return status.test_c_up_to_date();
}

inline bool
Grid::generators_are_up_to_date() const {
  return status.test_g_up_to_date();
}

inline bool
Grid::congruences_are_minimized() const {
  return status.test_c_minimized();
}

inline bool
Grid::generators_are_minimized() const {
  return status.test_g_minimized();
}

inline bool
Grid::has_pending_congruences() const {
  return status.test_c_pending();
}

inline bool
Grid::has_pending_generators() const {
  return status.test_g_pending();
}

inline bool
Grid::has_something_pending() const {
  return status.test_c_pending() || status.test_g_pending();
}

inline bool
Grid::can_have_something_pending() const {
  return congruences_are_minimized()
    && generators_are_minimized();
}

inline void
Grid::set_generators_up_to_date() {
  status.set_g_up_to_date();
}

inline void
Grid::set_congruences_minimized() {
  set_congruences_up_to_date();
  status.set_c_minimized();
}

inline void
Grid::set_generators_minimized() {
  set_generators_up_to_date();
  status.set_g_minimized();
}

inline void
Grid::set_congruences_pending() {
  status.set_c_pending();
}

inline void
Grid::set_generators_pending() {
  status.set_g_pending();
}

inline void
Grid::clear_empty() {
  status.reset_empty();
}

inline void
Grid::clear_congruences_minimized() {
  status.reset_c_minimized();
}

inline void
Grid::clear_generators_minimized() {
  status.reset_g_minimized();
}

inline void
Grid::clear_pending_congruences() {
  status.reset_c_pending();
}

inline void
Grid::clear_pending_generators() {
  status.reset_g_pending();
}

inline void
Grid::clear_congruences_up_to_date() {
  clear_pending_congruences();
  clear_congruences_minimized();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}

inline void
Grid::clear_generators_up_to_date() {
  clear_pending_generators();
  clear_generators_minimized();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

inline bool
Grid::is_empty() const {
  if (marked_empty())
    return true;
  // Try a fast-fail test: if generators are up-to-date then the
  // generator system (since it is well formed) contains a point.
  if (generators_are_up_to_date())
    return false;
  return !minimize();
}

#if 0
inline bool
Grid::bounds_from_above(const Linear_Expression& expr) const {
  return bounds(expr, true);
}

inline bool
Grid::bounds_from_below(const Linear_Expression& expr) const {
  return bounds(expr, false);
}

inline bool
Grid::maximize(const Linear_Expression& expr,
	       Coefficient& sup_n, Coefficient& sup_d, bool& maximum) const {
  return max_min(expr, true, sup_n, sup_d, maximum);
}

inline bool
Grid::maximize(const Linear_Expression& expr,
	       Coefficient& sup_n, Coefficient& sup_d, bool& maximum,
	       const Generator** const pppoint) const {
  return max_min(expr, true, sup_n, sup_d, maximum, pppoint);
}

inline bool
Grid::minimize(const Linear_Expression& expr,
	       Coefficient& inf_n, Coefficient& inf_d, bool& minimum) const {
  return max_min(expr, false, inf_n, inf_d, minimum);
}

inline bool
Grid::minimize(const Linear_Expression& expr,
	       Coefficient& inf_n, Coefficient& inf_d, bool& minimum,
	       const Generator** const pppoint) const {
  return max_min(expr, false, inf_n, inf_d, minimum, pppoint);
}
#endif // 0 FIX
/*! \relates Grid */
inline bool
operator!=(const Grid& x, const Grid& y) {
  return !(x == y);
}

inline bool
Grid::strictly_contains(const Grid& y) const {
  const Grid& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename Box>
Grid::Grid(const Box& box, From_Bounding_Box dummy)
  : con_sys(),
    gen_sys(NECESSARILY_CLOSED) {
  used(dummy);

  // Initialize the space dimension as indicated by the box.
  space_dim = box.space_dimension();

  con_sys.increase_space_dimension(space_dim);

  if (box.is_empty()) {
    // Empty grid.
    status.set_empty();
    if (space_dim == 0)
      status.set_zero_dim_univ();
  }
  else
    if (space_dim == 0)
      status.set_zero_dim_univ();
    else {
      // Add integrality congruence.
      con_sys.insert(Linear_Expression::zero() %= 1);
      // Add congruences according to `box'.
      TEMP_INTEGER(l_n);
      TEMP_INTEGER(l_d);
      TEMP_INTEGER(u_n);
      TEMP_INTEGER(u_d);
      TEMP_INTEGER(d);
      for (dimension_type k = space_dim; k-- > 0; ) {
	bool closed;
	// FIX also add generators
	if (box.get_lower_bound(k, closed, l_n, l_d)
	    && box.get_upper_bound(k, closed, u_n, u_d)) {
	  lcm_assign(d, l_d, u_d);
	  // FIX division by l_d and u_d repeats some of lcm_assign (use gcd instd)
	  l_n *= (d / l_d);
	  con_sys.insert((d * Variable(k) %= l_n) / ((u_n * (d / u_d)) - l_n));
	}
      }
      set_congruences_up_to_date();
    }

  gen_sys.set_sorted(false);
  gen_sys.unset_pending_rows();

  assert(OK());
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
    return;
  }
  if (generators_are_up_to_date()) {
    if (gen_sys.num_rows() == 0) {
      box = new_box;
      return;
    }
    Grid& gr = const_cast<Grid&>(*this);
    // Ensure generators are minimized.
    if (!generators_are_minimized()) {
      if (simplify(gr.gen_sys, gr.dim_kinds)) {
	// FIX empty result possible?
	gr.set_empty();
	box = new_box;
	return;
      }
      gr.set_generators_minimized();
    }
  }
  else if (!update_generators()) {
    // Updating found the grid empty.
    box = new_box;
    return;
  }

  assert(gen_sys.num_rows() > 0);

  dimension_type num_dims = gen_sys.num_columns() - 1;
  dimension_type num_rows = gen_sys.num_rows();
  const Generator& point = gen_sys[0];
  TEMP_INTEGER(divisor);
  divisor = point[0];

  // The covering box of a single point has only lower bounds.
  if (num_rows > 1) {
    Row gcds(num_dims, Row::Flags());
    dimension_type& last_index = num_rows;
    last_index--;

    // Store in `gcds', for each column (that is, for each dimension),
    // the GCD of the coefficients that are in parameter rows.

    const Generator& last_gen = gen_sys[last_index];
    // Initialise `gcds' according to the final parameter row.
    dimension_type dim = num_dims - 1;
    if (last_gen.is_line_or_equality()) {
      if (last_gen[num_dims] != 0) {
	// Empty interval, set both bounds for dimension `dim' to
	// zero.
	new_box.lower_upper_bound(dim, false, 0, 1);
	new_box.raise_lower_bound(dim, false, 0, 1);
      }
      gcds[dim] = 0;
    }
    else
      gcds[dim] = last_gen[num_dims];
    // Coefficients before the last are zero, due to minimal form.
    while (dim-- > 0)
      gcds[dim] = 0;
    last_index--;
    // Include the preceding parameter rows into `gcds'.
    for (dimension_type dim = num_dims; dim-- > 1; ) {
      // `dim_kinds' is indexed one higher than `gcds', as it starts
      // with the generator system's point.
      switch (dim_kinds[dim]) {
      case PARAMETER:
	{
	  const Generator& gen = gen_sys[last_index];
	  for (dimension_type col = dim; col <= num_dims; ++col)
	    gcd_assign(gcds[col-1], gen[col]);
	  last_index--;
	}
	break;
      case LINE:
	{
	  const Generator& gen = gen_sys[last_index];
	  for (dimension_type col = dim; col <= num_dims; ++col) {
	    if (gen[col] != 0) {
	      // Empty interval, set both bounds for dimension `dim' to
	      // zero.
	      new_box.lower_upper_bound(col-1, false, 0, 1);
	      new_box.raise_lower_bound(col-1, false, 0, 1);
	    }
	  }
	  last_index--;
	}
	break;
      case GEN_VIRTUAL:
	break;
      }
    }

    // Set each interval of the upper bound to the addition of the
    // point and GCD values associated with the interval's dimension.
    for (dimension_type dim = 0; dim < num_dims; ++dim)
      new_box.lower_upper_bound(dim, false, gcds[dim] + point[dim+1], divisor);
  }

  // Make the point the lower bound.
  for (dimension_type dim = 0; dim < num_dims; ++dim)
    new_box.raise_lower_bound(dim, false, point[dim+1], divisor);

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
      con_sys.clear();
      gen_sys.clear();
      gen_sys.set_sorted(false);
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

  const Generator_System& old_gensys = generators();

  if (old_gensys.num_rows() == 0) {
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

  Generator_System new_gensys;
  // Set sortedness, for the assertion met via gs::insert.
  new_gensys.set_sorted(false);
  for (Generator_System::const_iterator i = old_gensys.begin(),
	 old_gensys_end = old_gensys.end(); i != old_gensys_end; ++i) {
    const Generator& old_g = *i;
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
    case Generator::LINE:
      if (!all_zeroes)
	new_gensys.insert(line(e), false);
      break;
    case Generator::RAY:
      if (!all_zeroes) {
	// Inserting a point is safe, even with the normalization in
	// `point', as the divisor is 1.
	new_gensys.insert(point(e), false);
	new_gensys[new_gensys.num_rows()-1][0] = 0;
      }
      break;
    case Generator::POINT:
      // A point in the origin has all zero homogeneous coefficients.
      // Inserting the point is safe, even with the normalization in
      // `point', as the divisor is 1.
      new_gensys.insert(point(e), false);
      new_gensys[new_gensys.num_rows()-1][0] = old_g.divisor();
      break;
    case Generator::CLOSURE_POINT:
    default:
      assert(0);
    }
  }

  Grid new_grid(new_gensys, false);
  std::swap(*this, new_grid);

  assert(OK(true));
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Grid_inlines_hh)
