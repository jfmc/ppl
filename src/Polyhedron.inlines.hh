/* Polyhedron class implementation: inline functions.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Polyhedron_inlines_hh
#define PPL_Polyhedron_inlines_hh 1

#include "Interval.defs.hh"
#include "Generator.defs.hh"
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Polyhedron::~Polyhedron() {
}

inline Topology
Polyhedron::topology() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.topology();
}

inline void
Polyhedron::swap(Polyhedron& y) {
  if (topology() != y.topology())
    throw_topology_incompatible("swap(y)", y);
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
  std::swap(sat_c, y.sat_c);
  std::swap(sat_g, y.sat_g);
  std::swap(status, y.status);
  std::swap(space_dim, y.space_dim);
}

} // namespace Parma_Polyhedra_Library

/*! \relates Parma_Polyhedra_Library::Polyhedron */
inline void
std::swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y) {
  x.swap(y);
}

namespace Parma_Polyhedra_Library {

inline bool
Polyhedron::is_necessarily_closed() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.is_necessarily_closed();
}

inline bool
Polyhedron::is_empty() const {
  return status.test_empty();
}

inline bool
Polyhedron::constraints_are_up_to_date() const {
  return status.test_c_up_to_date();
}

inline bool
Polyhedron::generators_are_up_to_date() const {
  return status.test_g_up_to_date();
}

inline bool
Polyhedron::constraints_are_minimized() const {
  return status.test_c_minimized();
}

inline bool
Polyhedron::generators_are_minimized() const {
  return status.test_g_minimized();
}

inline bool
Polyhedron::has_pending_constraints() const {
  return status.test_c_pending();
}

inline bool
Polyhedron::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

inline bool
Polyhedron::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
}

inline bool
Polyhedron::can_have_something_pending() const {
  return constraints_are_minimized()
    && generators_are_minimized()
    && (sat_c_is_up_to_date() || sat_g_is_up_to_date());
}

inline bool
Polyhedron::has_pending_generators() const {
  return status.test_g_pending();
}

inline bool
Polyhedron::has_something_pending() const {
  return status.test_c_pending() || status.test_g_pending();
}

inline void
Polyhedron::set_constraints_up_to_date() {
  status.set_c_up_to_date();
}

inline void
Polyhedron::set_generators_up_to_date() {
  status.set_g_up_to_date();
}

inline void
Polyhedron::set_constraints_minimized() {
  set_constraints_up_to_date();
  status.set_c_minimized();
}

inline void
Polyhedron::set_generators_minimized() {
  set_generators_up_to_date();
  status.set_g_minimized();
}

inline void
Polyhedron::set_constraints_pending() {
  status.set_c_pending();
}

inline void
Polyhedron::set_generators_pending() {
  status.set_g_pending();
}

inline void
Polyhedron::set_sat_c_up_to_date() {
  status.set_sat_c_up_to_date();
}

inline void
Polyhedron::set_sat_g_up_to_date() {
  status.set_sat_g_up_to_date();
}

inline void
Polyhedron::clear_empty() {
  status.reset_empty();
}

inline void
Polyhedron::clear_sat_c_up_to_date() {
  status.reset_sat_c_up_to_date();
  // Can get rid of sat_c here.
}

inline void
Polyhedron::clear_sat_g_up_to_date() {
  status.reset_sat_g_up_to_date();
  // Can get rid of sat_g here.
}

inline void
Polyhedron::clear_constraints_minimized() {
  status.reset_c_minimized();
}

inline void
Polyhedron::clear_generators_minimized() {
  status.reset_g_minimized();
}

inline void
Polyhedron::clear_pending_constraints() {
  status.reset_c_pending();
}

inline void
Polyhedron::clear_pending_generators() {
  status.reset_g_pending();
}

inline void
Polyhedron::clear_constraints_up_to_date() {
  clear_pending_constraints();
  clear_constraints_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}

inline void
Polyhedron::clear_generators_up_to_date() {
  clear_pending_generators();
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

/*! \relates Polyhedron */
inline bool
operator!=(const Polyhedron& x, const Polyhedron& y) {
  return !(x == y);
}

/*! \relates Polyhedron */
inline bool
operator>=(const Polyhedron& x, const Polyhedron& y) {
  return y <= x;
}

/*! \relates Polyhedron */
inline bool
operator<(const Polyhedron& x, const Polyhedron& y) {
  return x <= y && !(x >= y);
}

/*! \relates Polyhedron */
inline bool
operator>(const Polyhedron& x, const Polyhedron& y) {
  return y < x;
}


inline dimension_type
Polyhedron::space_dimension() const {
  return space_dim;
}

inline bool
Polyhedron::check_empty() const {
  if (is_empty())
    return true;
  // Try a fast-fail test: if generators are up-to-date and
  // there are no pending constraints, then the generator system
  // (since it is well formed) contains a point.
  if (generators_are_up_to_date() && !has_pending_constraints())
    return false;
  return !minimize();
}

inline bool
Polyhedron::bounds_from_above(const LinExpression& expr) const {
  return bounds(expr, true);
}

inline bool
Polyhedron::bounds_from_below(const LinExpression& expr) const {
  return bounds(expr, false);
}

inline void
Polyhedron::add_low_level_constraints(ConSys& cs) {
  if (cs.is_necessarily_closed())
    // The positivity constraint.
    cs.insert(Constraint::zero_dim_positivity());
  else {
    // Add the epsilon constraints.
    cs.insert(Constraint::epsilon_leq_one());
    cs.insert(Constraint::epsilon_geq_zero());
  }
}

inline bool
Polyhedron::process_pending() const {
  assert(space_dim > 0 && !is_empty());
  assert(has_something_pending());

  Polyhedron& x = const_cast<Polyhedron&>(*this);

  if (x.has_pending_constraints())
    return x.process_pending_constraints();

  assert(x.has_pending_generators());
  x.process_pending_generators();
  return true;
}

template <typename Box>
Polyhedron::Polyhedron(Topology topol, const Box& box)
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
  // need of resizing the matrix of constraints later;
  // this constraint will be removed at the end.
  con_sys.insert(Variable(space_dim - 1) >= 0);

  for (dimension_type k = space_dim; k-- > 0; ) {
    // See if we have a valid lower bound.
    bool l_closed = false;
    Integer l_n, l_d;
    bool l_bounded = box.get_lower_bound(k, l_closed, l_n, l_d);
    if (l_bounded && topol == NECESSARILY_CLOSED && !l_closed)
      throw_invalid_argument("C_Polyhedron(const Box& box)",
			     "box has an open lower bound");
    // See if we have a valid upper bound.
    bool u_closed = false;
    Integer u_n, u_d;
    bool u_bounded = box.get_upper_bound(k, u_closed, u_n, u_d);
    if (u_bounded && topol == NECESSARILY_CLOSED && !u_closed)
      throw_invalid_argument("C_Polyhedron(const Box& box)",
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

  if (topol == NECESSARILY_CLOSED)
    // Adding the positivity constraint.
    con_sys.insert(Constraint::zero_dim_positivity());
  else {
    // Polyhedron NOT-necessarily closed:
    // adding the epsilon dimension constraints.
    con_sys.insert(Constraint::epsilon_leq_one());
    con_sys.insert(Constraint::epsilon_geq_zero());
  }
  
  // Now removing the dummy constraint inserted before.
  dimension_type n_rows = con_sys.num_rows() - 1;
  con_sys[0].swap(con_sys[n_rows]);
  // NOTE: here there are no pending constraints.
  con_sys.set_index_first_pending_row(n_rows);
  con_sys.erase_to_end(n_rows);

  // Constraints are up-to-date.
  set_constraints_up_to_date();
  assert(OK());
}

template <typename Box>
void
Polyhedron::shrink_bounding_box(Box& box, Complexity_Class complexity) const {
  bool polynomial = (complexity != ANY);
  if ((polynomial && !has_something_pending()
       && constraints_are_minimized()) || !polynomial) {
    // If the constraint system is minimized, the check
    // `check_universe()' is not exponential.
    if (check_universe())
      return;

  }
  if (polynomial) {
    if (is_empty() ||
	(generators_are_up_to_date() && gen_sys.num_rows() == 0)) {
      box.set_empty();
      return;
    }
    if (constraints_are_up_to_date()) {
      for (ConSys::const_iterator i = con_sys.begin();
	   i != con_sys.end(); ++i)
	if ((*i).is_trivial_false()){
	  box.set_empty();
	  return;
	}
    }
  }
  else
    // The flag `polynomial' is `false'.
    // Note that the check `check_empty()' is exponential!!!
    if (check_empty()) {
      box.set_empty();
      return;
    }

  if (space_dim == 0)
    return;
  
  // To record the lower and upper bound for each dimension.
  std::vector<LBoundary> lower_bound(space_dim);
  std::vector<UBoundary> upper_bound(space_dim);

  for (dimension_type j = space_dim; j-- > 0; ) {
    // Lower bounds are initialized to (open) plus infinity;
    lower_bound[j] = LBoundary(ExtendedRational('+'), LBoundary::OPEN);
    // Upper bounds are initialized to (open) minus infinity;
    upper_bound[j] = UBoundary(ExtendedRational('-'), UBoundary::OPEN);
  }
  
  if (!polynomial && has_something_pending())
    process_pending();

  if (polynomial &&
       (!generators_are_up_to_date() || has_pending_constraints())) {
    // Extract easy-to-find bounds from constraints.
    assert(constraints_are_up_to_date());
    
    // We must copy `con_sys' into a temporary matrix,
    // because we must apply gauss() and back_substitute()
    // to all the matrix and not only to the non-pending part.
    ConSys cs(con_sys);
    if (cs.num_pending_rows() > 0) {
      cs.unset_pending_rows();
      cs.sort_rows();
    }
    else if (!cs.is_sorted())
      cs.sort_rows();

    if (has_pending_constraints() || !constraints_are_minimized())
      cs.back_substitute(cs.gauss());
    
    const ConSys::const_iterator cs_begin = cs.begin();
    const ConSys::const_iterator cs_end = cs.end();
    
    for (ConSys::const_iterator i = cs_begin; i != cs_end; ++i) {
      dimension_type varid = space_dim;
      const Constraint& c = *i;
      // After `gauss()' and `back_substitute()' some constraints can
      // be trivially false.
      for (dimension_type j = space_dim; j-- > 0; ) {
	if (c.is_trivial_false()) {
	  box.set_empty();
	  return;
	}
	// We look foe constraints of the form `Variable(j) == k',
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
	const Integer& d = c.coefficient(Variable(varid));
	const Integer& n = c.inhomogeneous_term();
	// The constraint `c' is of the form
	// `Variable(varid) + n / d rel 0', where
	// `rel' is either the relation `==', `>=', or `>'.
	// For the purpose of shrinking intervals, this is
	// (morally) turned into `Variable(varid) rel  -n/d'.
	ExtendedRational r(-n, d);
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
    const GenSys& gs = gen_sys;
    // Using the iterator, we read also the pending part of the matrix.
    const GenSys::const_iterator gs_begin = gs.begin();
    const GenSys::const_iterator gs_end = gs.end();

    // We first need to identify those axes that are unbounded
    // below and/or above.
    for (GenSys::const_iterator i = gs_begin; i != gs_end; ++i) {
      const Generator& g = *i;
      Generator::Type g_type = g.type();
      switch (g_type) {
      case Generator::LINE:
	// Any axes `j' in which the coefficient is non-zero is unbounded
	// both below and above.
	for (dimension_type j = space_dim; j-- > 0; )
	  if (g.coefficient(Variable(j)) != 0) {
	    lower_bound[j] = LBoundary(ExtendedRational('-'), LBoundary::OPEN);
	    upper_bound[j] = UBoundary(ExtendedRational('+'), UBoundary::OPEN);
	  }
	break;
      case Generator::RAY:
	// Axes in which the coefficient is negative are unbounded below.
	// Axes in which the coefficient is positive are unbounded above.
	for (dimension_type j = space_dim; j-- > 0; ) {
	  int sign = sgn(g.coefficient(Variable(j)));
	  if (sign < 0)
	    lower_bound[j] = LBoundary(ExtendedRational('-'), LBoundary::OPEN);
	  else if (sign > 0)
	    upper_bound[j] = UBoundary(ExtendedRational('+'), UBoundary::OPEN);
	}
	break;
      case Generator::POINT:
      case Generator::CLOSURE_POINT:
	{
	  const Integer& d = g.divisor();
	  for (dimension_type j = space_dim; j-- > 0; ) {
	    const Integer& n = g.coefficient(Variable(j));
	    ExtendedRational r(n, d);
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
  
  // Now shrink the bounded axes.
  for (dimension_type j = space_dim; j-- > 0; ) {
    // Lower bound.
    const LBoundary& lb = lower_bound[j];
    const ExtendedRational& lr = lb.bound();
    if (lr.direction_of_infinity() == 0)
      box.raise_lower_bound(j, lb.is_closed(),
			    lr.numerator(), lr.denominator());

    // Upper bound.
    const UBoundary& ub = upper_bound[j];
    const ExtendedRational& ur = ub.bound();
    if (ur.direction_of_infinity() == 0)
      box.lower_upper_bound(j, ub.is_closed(),
			    ur.numerator(), ur.denominator());
  }
}

template <typename PartialFunction>
void
Polyhedron::rename_dimensions(const PartialFunction& pifunc) {
  // FIXME: this implementation is just an executable specification.
  if (space_dim == 0)
    return;

  if (pifunc.has_empty_codomain()) {
    // All dimensions vanish: the polyhedron becomes zero_dimensional.
    if (is_empty()
	|| (has_pending_constraints()
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

  dimension_type new_space_dimension = pifunc.max_in_codomain() + 1;
  // If there is something pending, using `generators()' we erase it.
  const GenSys& old_gensys = generators();

  if (old_gensys.num_rows() == 0) {
    // The polyhedron is empty.
    Polyhedron new_polyhedron(topology(), new_space_dimension, EMPTY);
    std::swap(*this, new_polyhedron);
    assert(OK());
    return;
  } 

  GenSys new_gensys;
  for (GenSys::const_iterator i = old_gensys.begin(),
	 old_gensys_end = old_gensys.end(); i != old_gensys_end; ++i) {
    const Generator& old_g = *i;
    LinExpression e(0 * Variable(new_space_dimension-1));
    bool all_zeroes = true;
    for (dimension_type index = 0; index < space_dim; ++index) {
      dimension_type new_index;
      if (old_g.coefficient(Variable(index)) != 0
	  && pifunc.maps(index, new_index)) {
	e += Variable(new_index)
	  * old_g.coefficient(Variable(index));
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
      // Note: a point in the origin has all zero homogeneous coefficients.
      new_gensys.insert(point(e, old_g.divisor()));
      break;
    case Generator::CLOSURE_POINT:
      // Note: a point in the origin has all zero homogeneous coefficients.
      new_gensys.insert(point(e, old_g.divisor()));
      break;
    }
  }
  Polyhedron new_polyhedron(topology(), new_gensys);
  std::swap(*this, new_polyhedron);
  assert(OK(true));
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Polyhedron_inlines_hh)
