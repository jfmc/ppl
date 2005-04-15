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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Grid_inlines_hh
#define PPL_Grid_inlines_hh 1

#include "Interval.defs.hh"
#include "Generator.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

// FIX next 4: assume elements?, row.size() call may be saveable
//     in context

// FIX
// Virtual rows currently "take precedence" to the other kinds.  I.e
// the flags for the other kinds may be set at the same time as the
// virtual flag.  So the virtual flag must always be checked first.
// This is due to the quick Linear_Row implementation of the virtual
// flag.

inline bool
Grid::virtual_row(const Linear_Row& row) {
  //return row[0] == -1;
  return row.is_virtual();
}

inline bool
Grid::virtual_row(const Generator& row) {
  //return row[0] == -1;
  return row.is_virtual();
}

inline bool
Grid::virtual_row(const Congruence& row) {
  return row[row.size() - 1] == -1;
}

inline Linear_Row&
Grid::mark_virtual(Linear_Row& row) {
  //row[0] = -1;
  row.set_is_virtual();
  return row;
}

inline Congruence&
Grid::mark_virtual(Congruence& row) {
  row[row.size() - 1] = -1;
  return row;
}

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
  construct(ccgs);
}

inline
Grid::Grid(Congruence_System& cgs) {
  construct(cgs);
}

inline
Grid::Grid(const Generator_System& gs) {
  construct(gs);
}

inline
Grid::Grid(Generator_System& gs) {
  construct(gs);
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
Grid::swap(Grid& y) {
#if 0
  if (topology() != y.topology())
    throw_topology_incompatible("swap(y)", "y", y);
#endif
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
#if 0
  std::swap(sat_c, y.sat_c);
  std::swap(sat_g, y.sat_g);
#endif
  std::swap(status, y.status);
  std::swap(space_dim, y.space_dim);
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
Grid::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

inline bool
Grid::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
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
    && generators_are_minimized()
    && (sat_c_is_up_to_date() || sat_g_is_up_to_date());
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
Grid::set_sat_c_up_to_date() {
  status.set_sat_c_up_to_date();
}

inline void
Grid::set_sat_g_up_to_date() {
  status.set_sat_g_up_to_date();
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
Grid::clear_sat_c_up_to_date() {
  status.reset_sat_c_up_to_date();
  // Can get rid of sat_c here.
}

inline void
Grid::clear_sat_g_up_to_date() {
  status.reset_sat_g_up_to_date();
  // Can get rid of sat_g here.
}

inline void
Grid::clear_congruences_up_to_date() {
  clear_pending_congruences();
  clear_congruences_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}

inline void
Grid::clear_generators_up_to_date() {
  clear_pending_generators();
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

#if 0
inline bool
Grid::process_pending() const {
  assert(space_dim > 0 && !marked_empty());
  assert(has_something_pending());

  Grid& x = const_cast<Grid&>(*this);

  if (x.has_pending_congruences())
    return x.process_pending_congruences();

  assert(x.has_pending_generators());
  x.process_pending_generators();
  return true;
}
#endif

inline void
Grid::add_low_level_congruences(Congruence_System& cgs) {
  // FIX
  //cgs.insert(Congruence::zero_dim_integrality());
}

inline bool
Grid::is_empty() const {
  if (marked_empty())
    return true;
  // Try a fast-fail test: if generators are up-to-date and
  // there are no pending congruences, then the generator system
  // (since it is well formed) contains a point.
  if (generators_are_up_to_date() && !has_pending_congruences())
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
#endif
/*! \relates Grid */
inline bool
operator!=(const Grid& x, const Grid& y) {
  return !(x == y);
}
#if 0
inline bool
Grid::strictly_contains(const Grid& y) const {
  const Grid& x = *this;
  return x.contains(y) && !y.contains(x);
}

template <typename Box>
Grid::Grid(Topology topol, const Box& box)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Initialize the space dimension as indicated by the box.
  space_dim = box.space_dimension();

  // Check for emptiness.
  if (box.is_empty()) {
    set_empty();
    return;
  }

  // Zero-dim universe polyhedron.
  if (space_dim == 0) {
    set_zero_dim_univ();
    return;
  }

  // Insert a dummy constraint of the highest dimension to avoid the
  // need of resizing the matrix of congruences later; this constraint
  // will be removed at the end.
  con_sys.insert(Variable(space_dim - 1) >= 0);

  for (dimension_type k = space_dim; k-- > 0; ) {
    // See if we have a valid lower bound.
    bool l_closed = false;
    Coefficient l_n, l_d;
    bool l_bounded = box.get_lower_bound(k, l_closed, l_n, l_d);
    if (l_bounded && topol == NECESSARILY_CLOSED && !l_closed)
      throw_invalid_argument("C_Grid(const Box& box)",
			     "box has an open lower bound");
    // See if we have a valid upper bound.
    bool u_closed = false;
    Coefficient u_n, u_d;
    bool u_bounded = box.get_upper_bound(k, u_closed, u_n, u_d);
    if (u_bounded && topol == NECESSARILY_CLOSED && !u_closed)
      throw_invalid_argument("C_Grid(const Box& box)",
			     "box has an open upper bound");

    // See if we have an implicit equality constraint.
    if (l_bounded && u_bounded
	&& l_closed && u_closed
	&& l_n == u_n && l_d == u_d) {
      // Add the constraint `l_d*v_k == l_n'.
      con_sys.insert(l_d * Variable(k) == l_n);
    }
    else {
      // Check if a lower bound constraint is required.
      if (l_bounded) {
       if (l_closed)
	 // Add the constraint `l_d*v_k >= l_n'.
	 con_sys.insert(l_d * Variable(k) >= l_n);
       else
	 // Add the constraint `l_d*v_k > l_n'.
	 con_sys.insert(l_d * Variable(k) > l_n);
      }
      // Check if an upper bound constraint is required.
      if (u_bounded) {
       if (u_closed)
	 // Add the constraint `u_d*v_k <= u_n'.
	 con_sys.insert(u_d * Variable(k) <= u_n);
       else
	 // Add the constraint `u_d*v_k < u_n'.
	 con_sys.insert(u_d * Variable(k) < u_n);
      }
    }
  }

  // Adding the low-level congruences.
  add_low_level_congruences(con_sys);
  // Now removing the dummy constraint inserted before.
  dimension_type n_rows = con_sys.num_rows() - 1;
  con_sys[0].swap(con_sys[n_rows]);
  con_sys.set_sorted(false);
  // NOTE: here there are no pending congruences.
  con_sys.set_index_first_pending_row(n_rows);
  con_sys.erase_to_end(n_rows);

  // Congruences are up-to-date.
  set_congruences_up_to_date();
  assert(OK());
}

template <typename Box>
void
Grid::shrink_bounding_box(Box& box, Complexity_Class complexity) const {
  bool polynomial = (complexity != ANY_COMPLEXITY);
  if ((polynomial && !has_something_pending()
       && congruences_are_minimized()) || !polynomial) {
    // If the constraint system is minimized, the test `is_universe()'
    // is not exponential.
    if (is_universe())
      return;
  }
  if (polynomial) {
    if (marked_empty() ||
	(generators_are_up_to_date() && gen_sys.num_rows() == 0)) {
      box.set_empty();
      return;
    }
    if (congruences_are_up_to_date()) {
      for (Constraint_System::const_iterator i
	     = con_sys.begin(); i != con_sys.end(); ++i)
	if ((*i).is_trivial_false()){
	  box.set_empty();
	  return;
	}
    }
  }
  else
    // The flag `polynomial' is `false'.
    // Note that the test `is_empty()' is exponential in the worst case.
    if (is_empty()) {
      box.set_empty();
      return;
    }

  if (space_dim == 0)
    return;

  // To record the lower and upper bound for each dimension.
  // Lower bounds are initialized to open plus infinity.
  std::vector<LBoundary>
    lower_bound(space_dim, LBoundary(ERational('+'), LBoundary::OPEN));
  // Upper bounds are initialized to open minus infinity.
  std::vector<UBoundary>
    upper_bound(space_dim, UBoundary(ERational('-'), UBoundary::OPEN));

  if (!polynomial && has_something_pending())
    process_pending();

  if (polynomial &&
       (!generators_are_up_to_date() || has_pending_congruences())) {
    // Extract easy-to-find bounds from congruences.
    assert(congruences_are_up_to_date());

    // We must copy `con_sys' to a temporary matrix,
    // because we must apply gauss() and back_substitute()
    // to all the matrix and not only to the non-pending part.
    Constraint_System cs(con_sys);
    if (cs.num_pending_rows() > 0) {
      cs.unset_pending_rows();
      cs.sort_rows();
    }
    else if (!cs.is_sorted())
      cs.sort_rows();

    if (has_pending_congruences() || !congruences_are_minimized())
      cs.back_substitute(cs.gauss());

    const Constraint_System::const_iterator cs_begin = cs.begin();
    const Constraint_System::const_iterator cs_end = cs.end();

    for (Constraint_System::const_iterator i = cs_begin; i != cs_end; ++i) {
      dimension_type varid = space_dim;
      const Constraint& c = *i;
      // After `gauss()' and `back_substitute()' some congruences can
      // be trivially false.
      for (dimension_type j = space_dim; j-- > 0; ) {
	if (c.is_trivial_false()) {
	  box.set_empty();
	  return;
	}
	// We look for congruences of the form `Variable(j) == k',
	// `Variable(j) >= k', and `Variable(j) > k'.
	if (c.coefficient(Variable(j)) != 0)
	  if (varid != space_dim) {
	    varid = space_dim;
	    break;
	  }
	  else
	    varid = j;
      }
      if (varid != space_dim) {
	Coefficient_traits::const_reference d = c.coefficient(Variable(varid));
	Coefficient_traits::const_reference n = c.inhomogeneous_term();
	// The constraint `c' is of the form
	// `Variable(varid) + n / d rel 0', where
	// `rel' is either the relation `==', `>=', or `>'.
	// For the purpose of shrinking intervals, this is
	// (morally) turned into `Variable(varid) rel  -n/d'.
	ERational r(-n, d);
	Constraint::Type c_type = c.type();
	switch (c_type) {
	case Constraint::EQUALITY:
	  lower_bound[varid] = LBoundary(r, LBoundary::CLOSED);
	  upper_bound[varid] = UBoundary(r, UBoundary::CLOSED);
	  break;
	case Constraint::NONSTRICT_INEQUALITY:
	case Constraint::STRICT_INEQUALITY:
	  if (d > 0)
	  // If `d' is strictly positive, we have a constraint of the
	  // form `Variable(varid) >= k' or `Variable(varid) > k'.
	    lower_bound[varid]
	      = LBoundary(r, (c_type == Constraint::NONSTRICT_INEQUALITY
			      ? LBoundary::CLOSED
			      : LBoundary::OPEN));
	  else {
	    // Otherwise, we are sure that `d' is strictly negative
	    // and, in this case, we have a constraint of the form
	    // `Variable(varid) <= k' or `Variable(varid) < k'.
	    assert(d < 0);
	    upper_bound[varid]
	      = UBoundary(r, (c_type == Constraint::NONSTRICT_INEQUALITY
			      ? UBoundary::CLOSED
			      : UBoundary::OPEN));
	  }
	  break;
	}
      }
    }
  }
  else {
    // We are in the case where either the generators are up-to-date
    // or polynomial execution time is not required.
    // Get the generators for *this.

    // We have not to copy `gen_sys', because in this case
    // we only read the generators.
    const Generator_System& gs = gen_sys;
    // Using the iterator, we read also the pending part of the matrix.
    const Generator_System::const_iterator gs_begin = gs.begin();
    const Generator_System::const_iterator gs_end = gs.end();

    // We first need to identify those axes that are unbounded
    // below and/or above.
    for (Generator_System::const_iterator i = gs_begin; i != gs_end; ++i) {
      const Generator& g = *i;
      Generator::Type g_type = g.type();
      switch (g_type) {
      case Generator::LINE:
	// Any axes `j' in which the coefficient is non-zero is unbounded
	// both below and above.
	for (dimension_type j = space_dim; j-- > 0; )
	  if (g.coefficient(Variable(j)) != 0) {
	    lower_bound[j] = LBoundary(ERational('-'), LBoundary::OPEN);
	    upper_bound[j] = UBoundary(ERational('+'), UBoundary::OPEN);
	  }
	break;
      case Generator::RAY:
	// Axes in which the coefficient is negative are unbounded below.
	// Axes in which the coefficient is positive are unbounded above.
	for (dimension_type j = space_dim; j-- > 0; ) {
	  int sign = sgn(g.coefficient(Variable(j)));
	  if (sign < 0)
	    lower_bound[j] = LBoundary(ERational('-'), LBoundary::OPEN);
	  else if (sign > 0)
	    upper_bound[j] = UBoundary(ERational('+'), UBoundary::OPEN);
	}
	break;
      case Generator::POINT:
      case Generator::CLOSURE_POINT:
	{
	  Coefficient_traits::const_reference d = g.divisor();
	  for (dimension_type j = space_dim; j-- > 0; ) {
	    Coefficient_traits::const_reference n = g.coefficient(Variable(j));
	    ERational r(n, d);
	    LBoundary lb(r,(g_type == Generator::CLOSURE_POINT
			    ? LBoundary::OPEN
			    : LBoundary::CLOSED));
	    if (lb < lower_bound[j])
	      lower_bound[j] = lb;
	    UBoundary ub(r, (g_type == Generator::CLOSURE_POINT
			     ? UBoundary::OPEN
			     : UBoundary::CLOSED));
	    if (ub > upper_bound[j])
	      upper_bound[j] = ub;
	  }
	}
	break;
      }
    }
  }

  TEMP_INTEGER(n);
  TEMP_INTEGER(d);

  // Now shrink the bounded axes.
  for (dimension_type j = space_dim; j-- > 0; ) {
    // Lower bound.
    const LBoundary& lb = lower_bound[j];
    const ERational& lr = lb.bound();
    if (lr.direction_of_infinity() == 0) {
      lr.numerator(n);
      lr.denominator(d);
      box.raise_lower_bound(j, lb.is_closed(), n, d);
    }

    // Upper bound.
    const UBoundary& ub = upper_bound[j];
    const ERational& ur = ub.bound();
    if (ur.direction_of_infinity() == 0) {
      ur.numerator(n);
      ur.denominator(d);
      box.lower_upper_bound(j, ub.is_closed(), n, d);
    }
  }
}

template <typename Partial_Function>
void
Grid::map_space_dimensions(const Partial_Function& pfunc) {
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the polyhedron becomes zero_dimensional.
    if (marked_empty()
	|| (has_pending_congruences()
	    && !remove_pending_to_obtain_generators())
	|| (!generators_are_up_to_date() && !update_generators())) {
      // Removing all dimensions from the empty polyhedron.
      space_dim = 0;
      con_sys.clear();
    }
    else
      // Removing all dimensions from a non-empty polyhedron.
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

    // Permute all that is up-to-date.  Notice that the contents of
    // the saturation matrices is unaffected by the permutation of
    // columns: they remain valid, if they were so.
    if (congruences_are_up_to_date())
      con_sys.permute_columns(cycles);

    if (generators_are_up_to_date())
      gen_sys.permute_columns(cycles);

    assert(OK());
    return;
  }

  // If control gets here, then `pfunc' is not a permutation and some
  // dimensions must be projected away.

  // If there are pending congruences, using `generators()' we process them.
  const Generator_System& old_gensys = generators();

  if (old_gensys.num_rows() == 0) {
    // The polyhedron is empty.
    Grid new_polyhedron(topology(), new_space_dimension, EMPTY);
    std::swap(*this, new_polyhedron);
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
	new_gensys.insert(line(e));
      break;
    case Generator::RAY:
      if (!all_zeroes)
	new_gensys.insert(ray(e));
      break;
    case Generator::POINT:
      // A point in the origin has all zero homogeneous coefficients.
      new_gensys.insert(point(e, old_g.divisor()));
      break;
    case Generator::CLOSURE_POINT:
      // A closure point in the origin has all zero homogeneous coefficients.
      new_gensys.insert(closure_point(e, old_g.divisor()));
      break;
    }
  }
  Grid new_polyhedron(topology(), new_gensys);
  std::swap(*this, new_polyhedron);
  assert(OK(true));
}

#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Grid_inlines_hh)
