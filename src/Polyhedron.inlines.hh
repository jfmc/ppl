/* Polyhedron class implementation: inline functions.
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

#ifndef _Polyhedron_inlines_hh
#define _Polyhedron_inlines_hh 1

#include "Interval.defs.hh"
#include "Generator.defs.hh"
#include <algorithm>

namespace Parma_Polyhedra_Library {

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
Polyhedron::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

inline bool
Polyhedron::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
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
Polyhedron::clear_constraints_up_to_date() {
  clear_constraints_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}

inline void
Polyhedron::clear_generators_up_to_date() {
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

/*! \relates Polyhedron */
inline bool
operator==(const Polyhedron& x, const Polyhedron& y) {
  return x <= y && y <= x;
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


inline size_t
Polyhedron::space_dimension() const {
  return space_dim;
}

inline bool
Polyhedron::check_empty() const {
  minimize();
  return is_empty();
}

inline bool
Polyhedron::bounds_from_above(const LinExpression& expr) const {
  return bounds(expr, true);
}

inline bool
Polyhedron::bounds_from_below(const LinExpression& expr) const {
  return bounds(expr, false);
}

template <typename Box>
Polyhedron::Polyhedron(Topology topol, const Box& box)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Initialize the space dimension as indicated by the box.
  space_dim = box.space_dimension();

  // Check for emptyness.
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

  for (size_t k = space_dim; k-- > 0; ) {
    // See if we have a valid lower bound.
    bool l_closed = false;
    Integer l_n, l_d;
    bool l_bounded = box.get_lower_bound(k, l_closed, l_n, l_d);
    if (l_bounded && topol == NECESSARILY_CLOSED && !l_closed)
      throw_generic("C_Polyhedron(const Box& box)",
		    "box has an open lower bound");
    // See if we have a valid upper bound.
    bool u_closed = false;
    Integer u_n, u_d;
    bool u_bounded = box.get_upper_bound(k, u_closed, u_n, u_d);
    if (u_bounded && topol == NECESSARILY_CLOSED && !u_closed)
      throw_generic("C_Polyhedron(const Box& box)",
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
  size_t n_rows = con_sys.num_rows() - 1;
  con_sys[0].swap(con_sys[n_rows]);
  con_sys.erase_to_end(n_rows);

  // Constraints are up-to-date.
  set_constraints_up_to_date();
  assert(OK());
}

template <typename Box>
void
Polyhedron::shrink_bounding_box(Box& box) const {
  if (check_universe())
    return;

  if (check_empty()) {
    box.set_empty();
    return;
  }

  if (space_dim == 0)
    return;

  // To record the lower and upper bound for each dimension.
  std::vector<LBoundary> lower_bound(space_dim);
  std::vector<UBoundary> upper_bound(space_dim);

  for (size_t j = space_dim; j-- > 0; ) {
    // Lower bounds are initialized to (open) plus infinity;
    lower_bound[j] = LBoundary(ExtendedRational('+'), LBoundary::OPEN);
    // Upper bounds are initialized to (open) minus infinity;
    upper_bound[j] = UBoundary(ExtendedRational('-'), UBoundary::OPEN);
  }

  // Get the generators for *this.
  const GenSys& gs = generators();
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
      for (size_t j = space_dim; j-- > 0; )
        if (g.coefficient(Variable(j)) != 0) {
	  lower_bound[j] = LBoundary(ExtendedRational('-'), LBoundary::OPEN);
	  upper_bound[j] = UBoundary(ExtendedRational('+'), UBoundary::OPEN);
	}
    break;
    case Generator::RAY:
      // Axes in which the coefficient is negative are unbounded below.
      // Axes in which the coefficient is positive are unbounded above.
      for (size_t j = space_dim; j-- > 0; ) {
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
	for (size_t j = space_dim; j-- > 0; ) {
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

  // Now shrink the bounded axes.
  for (size_t j = space_dim; j-- > 0; ) {
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

} // namespace Parma_Polyhedra_Library


/*! \relates Parma_Polyhedra_Library::Polyhedron */
inline void
std::swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y) {
  x.swap(y);
}

#endif // !defined(_Polyhedron_inlines_hh)
