/* Polyhedron class implementation
   (non-inline widening-related member functions).
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

#include "BHRZ03_Certificate.defs.hh"
#include "statistics.hh"
#include <cassert>
#include <iostream>
#include <stdexcept>
#include <deque>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Polyhedron::select_CH78_constraints(const Polyhedron& y,
					 ConSys& cs_selection) const {
  // Private method: the caller must ensure the following conditions.
  assert(topology() == y.topology()
	 && topology() == cs_selection.topology()
	 && space_dim == y.space_dim);
  assert(!marked_empty()
	 && !has_pending_constraints()
	 && generators_are_up_to_date());
  assert(!y.marked_empty()
	 && !y.has_something_pending()
	 && y.constraints_are_minimized());

  // A constraint in `y.con_sys' is copied into `cs_selection'
  // if it is satisfied by all the generators of `gen_sys'.

  // Note: the loop index `i' goes upward to avoid reversing
  // the ordering of the chosen constraints.
  for (dimension_type i = 0, iend = y.con_sys.num_rows(); i < iend; ++i) {
    const Constraint& c = y.con_sys[i];
    if (gen_sys.satisfied_by_all_generators(c))
      cs_selection.insert(c);
  }
}

void
PPL::Polyhedron::select_H79_constraints(const Polyhedron& y,
					ConSys& cs_selected,
					ConSys& cs_not_selected) const {
  // Private method: the caller must ensure the following conditions
  // (beside the inclusion `y <= x').
  assert(topology() == y.topology()
	 && topology() == cs_selected.topology()
	 && topology() == cs_not_selected.topology());
  assert(space_dim == y.space_dim);
  assert(!marked_empty()
	 && !has_pending_generators()
	 && constraints_are_up_to_date());
  assert(!y.marked_empty()
	 && !y.has_something_pending()
	 && y.constraints_are_minimized()
	 && y.generators_are_up_to_date());

  // Obtain a sorted copy of `y.sat_g'.
  if (!y.sat_g_is_up_to_date())
    y.update_sat_g();
  SatMatrix tmp_sat_g = y.sat_g;
  tmp_sat_g.sort_rows();

  // A constraint in `con_sys' is copied into `cs_selected'
  // if its behavior with respect to `y.gen_sys' is the same
  // as that of another constraint in `y.con_sys'.
  // otherwise it is copied into `cs_not_selected'.
  // Namely, we check whether the saturation row `buffer'
  // (built starting from the given constraint and `y.gen_sys')
  // is a row of the saturation matrix `tmp_sat_g'.

  // CHECK ME: the following comment is only applicable when `y.gen_sys'
  // is minimized. In that case, the comment suggests that it would be
  // possible to use a fast (but incomplete) redundancy test based on
  // the number of saturators in `buffer'.
  // NOTE: If the considered constraint of `con_sys' does not
  // satisfy the saturation rule (see Section \ref prelims), then
  // it will not appear in the resulting constraint system,
  // because `tmp_sat_g' is built starting from a minimized polyhedron.

  // The size of `buffer' will reach sat.num_columns() bit.
  SatRow buffer;
  // Note: the loop index `i' goes upward to avoid reversing
  // the ordering of the chosen constraints.
  for (dimension_type i = 0, iend = con_sys.num_rows(); i < iend; ++i) {
    const Constraint& ci = con_sys[i];
    // The saturation row `buffer' is built considering
    // the `i'-th constraint of the polyhedron `x' and
    // all the generators of the polyhedron `y'.
    buffer.clear();
    for (dimension_type j = y.gen_sys.num_rows(); j-- > 0; ) {
      const int sp_sgn = sgn(y.gen_sys[j] * ci);
      // We are assuming that `y <= x'.
      assert(sp_sgn >= 0);
      if (sp_sgn > 0)
	buffer.set(j);
    }
    // We check whether `buffer' is a row of `tmp_sat_g',
    // exploiting its sortedness in order to have faster comparisons.
    if (tmp_sat_g.sorted_contains(buffer))
      cs_selected.insert(ci);
    else
      cs_not_selected.insert(ci);
  }
}

void
PPL::Polyhedron::H79_widening_assign(const Polyhedron& y, unsigned* tp) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  const Topology tpl = x.topology();
  if (tpl != y.topology())
    throw_topology_incompatible("H79_widening_assign(y)", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("H79_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that y is contained in or equal to x.
    const Polyhedron x_copy = x;
    const Polyhedron y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If any argument is zero-dimensional or empty,
  // the H79-widening behaves as the identity function.
  if (x.space_dim == 0 || x.marked_empty() || y.marked_empty())
    return;

  // `y.gen_sys' should be in minimal form and
  // `y.sat_g' should be up-to-date.
  if (y.is_necessarily_closed()) {
    if (!y.minimize())
      // `y' is empty: the result is `x'.
      return;
  }
  else {
    // Dealing with a NNC polyhedron.
    // To obtain a correct reasoning when comparing
    // the constraints of `x' with the generators of `y',
    // we enforce the inclusion relation holding between
    // the two NNC polyhedra `x' and `y' (i.e., `y <= x')
    // to also hold for the corresponding eps-representations:
    // this is obtained by intersecting the two eps-representations.
    Polyhedron& yy = const_cast<Polyhedron&>(y);
    if (!yy.intersection_assign_and_minimize(x))
      // `y' is empty: the result is `x'.
      return;
  }

  // If we only have the generators of `x' and the dimensions of
  // the two polyhedra are the same, we can compute the standard
  // widening by using the specification in CousotH78, therefore
  // avoiding converting from generators to constraints.
  if (x.has_pending_generators() || !x.constraints_are_up_to_date()) {
    ConSys CH78_cs(tpl);
    x.select_CH78_constraints(y, CH78_cs);

    if (CH78_cs.num_rows() == y.con_sys.num_rows()) {
      // Having selected all the constraints, the result is `y'.
      x = y;
      return;
    }
    // Otherwise, check if `x' and `y' have the same dimension.
    // Note that `y.con_sys' is minimized and `CH78_cs' has no redundant
    // constraints, since it is a subset of the former.
    else if (CH78_cs.num_equalities() == y.con_sys.num_equalities()) {
      // Let `x' be defined by the constraints in `CH78_cs'.
      Polyhedron CH78(tpl, x.space_dim, UNIVERSE);
      CH78.add_recycled_constraints(CH78_cs);

      // Check whether we are using the widening-with-tokens technique
      // and there still are tokens available.
      if (tp != 0 && *tp > 0) {
	// There are tokens available. If `CH78' is not a subset of `x',
	// then it is less precise and we use one of the available tokens.
	if (!x.contains(CH78))
	  --(*tp);
      }
      else
	// No tokens.
	std::swap(x, CH78);
      assert(x.OK(true));
      return;
    }
  }

  // As the dimension of `x' is strictly greater than the dimension of `y',
  // we have to compute the standard widening by selecting a subset of
  // the constraints of `x'.
  // `x.con_sys' is just required to be up-to-date, because:
  // - if `x.con_sys' is unsatisfiable, then by assumption
  //   also `y' is empty, so that the resulting polyhedron is `x';
  // - redundant constraints in `x.con_sys' do not affect the result
  //   of the widening, because if they are selected they will be
  //   redundant even in the result.
  if (has_pending_generators())
    process_pending_generators();
  else if (!x.constraints_are_up_to_date())
    x.update_constraints();

  // Copy into `H79_con_sys' the constraints of `x' that are common to `y',
  // according to the definition of the H79 widening.
  ConSys H79_cs(tpl);
  ConSys x_minus_H79_cs(tpl);
  x.select_H79_constraints(y, H79_cs, x_minus_H79_cs);

  if (x_minus_H79_cs.num_rows() == 0)
    // We selected all of the constraints of `x',
    // thus the result of the widening is `x'.
    return;
  else {
    // We selected a strict subset of the constraints of `x'.
    // NOTE: as `x.con_sys' was not necessarily in minimal form,
    // this does not imply that the result strictly includes `x'.
    // Let `H79' be defined by the constraints in `H79_cs'.
    Polyhedron H79(tpl, x.space_dim, UNIVERSE);
    H79.add_recycled_constraints(H79_cs);

    // Check whether we are using the widening-with-tokens technique
    // and there still are tokens available.
    if (tp != 0 && *tp > 0) {
      // There are tokens available. If `H79' is not a subset of `x',
      // then it is less precise and we use one of the available tokens.
      if (!x.contains(H79))
	--(*tp);
    }
    else
      // No tokens.
      std::swap(x, H79);
    assert(x.OK(true));
  }
}

void
PPL::Polyhedron::limited_H79_extrapolation_assign(const Polyhedron& y,
						  const ConSys& cs,
						  unsigned* tp) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.is_necessarily_closed()) {
    if (!y.is_necessarily_closed())
      throw_topology_incompatible("limited_H79_extrapolation_assign(y, cs)",
				  y);
    if (cs.has_strict_inequalities())
      throw_topology_incompatible("limited_H79_extrapolation_assign(y, cs)",
				  cs);
  }
  else if (y.is_necessarily_closed())
    throw_topology_incompatible("limited_H79_extrapolation_assign(y, cs)",
				y);

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("limited_H79_extrapolation_assign(y, cs)",
				 y);
  // `cs' must be dimension-compatible with the two polyhedra.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (x.space_dim < cs_space_dim)
    throw_dimension_incompatible("limited_H79_extrapolation_assign(y, cs)",
				 "cs", cs);

#ifndef NDEBUG
  {
    // We assume that y is contained in or equal to x.
    const Polyhedron x_copy = x;
    const Polyhedron y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  if (y.marked_empty())
    return;
  if (x.marked_empty())
    return;

  // The limited H79-widening between two polyhedra in a
  // zero-dimensional space is a polyhedron in a zero-dimensional
  // space, too.
  if (x.space_dim == 0)
    return;

  if (!y.minimize())
    // We have just discovered that `y' is empty.
    return;

  // Update the generators of `x': these are used to select,
  // from the constraints in `cs', those that must be added
  // to the resulting polyhedron.
  if ((x.has_pending_constraints() && !x.process_pending_constraints())
      || (!x.generators_are_up_to_date() && !x.update_generators()))
    // We have just discovered that `x' is empty.
    return;

  ConSys new_cs;
  // The constraints to be added must be satisfied by all the
  // generators of `x'. We can disregard `y' because `y <= x'.
  const GenSys& x_gs = x.gen_sys;
  // FIXME: why iterating upwards here?
  // FIXME: why repeating the call to cs[i]?
  for (dimension_type i = 0, cs_num_rows = cs.num_rows(); i < cs_num_rows; ++i)
    if (x_gs.satisfied_by_all_generators(cs[i]))
      new_cs.insert(cs[i]);

  x.H79_widening_assign(y, tp);
  x.add_constraints(new_cs);
  assert(OK());
}

namespace {

using namespace PPL;

class BW_Box {
private:
  ConSys& con_sys;

public:

  BW_Box(ConSys& cs)
    : con_sys(cs) {
  }

  void set_empty() {
    throw std::runtime_error("PPL internal error");
  }

  void raise_lower_bound(const dimension_type k, const bool closed,
			 Integer_traits::const_reference n,
			 Integer_traits::const_reference d) {
    if (closed)
      con_sys.insert(d*Variable(k) >= n);
    else
      con_sys.insert(d*Variable(k) > n);
  }

  void lower_upper_bound(const dimension_type k, const bool closed,
			 Integer_traits::const_reference n,
			 Integer_traits::const_reference d) {
    if (closed)
      con_sys.insert(d*Variable(k) <= n);
    else
      con_sys.insert(d*Variable(k) < n);
  }
};

} // namespace

void
PPL::Polyhedron::bounded_H79_extrapolation_assign(const Polyhedron& y,
						  const ConSys& cs,
						  unsigned* tp) {
  ConSys bounding_cs;
  BW_Box box(bounding_cs);
  shrink_bounding_box(box, ANY);
  limited_H79_extrapolation_assign(y, cs, tp);
  add_recycled_constraints(bounding_cs);
}

PPL::Polyhedron::BHRZ03_info::BHRZ03_info(const Polyhedron& x)
  : poly_dim(0), lin_space_dim(0), num_constraints(0), num_points(0),
    num_zero_ray_coord(x.space_dimension(), 0) {
  x.minimize();
  // `x' cannot be an empty polyhedron or become empty after minimization.
  assert(!x.marked_empty());
  x.collect_BHRZ03_info(*this);
  assert(OK());
}

bool
PPL::Polyhedron::BHRZ03_info::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // The dimension of the vector space.
  const dimension_type space_dim = num_zero_ray_coord.size();

  if (poly_dim > space_dim) {
#ifndef NDEBUG
    cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	 << "the polyhedron dimension is greater than the space dimension!"
	 << endl;
#endif
    return false;
  }

  if (lin_space_dim > poly_dim) {
#ifndef NDEBUG
    cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	 << "the lineality space dimension is greater than "
	 << "the polyhedron dimension!"
	 << endl;
#endif
    return false;
  }

  if (num_constraints < space_dim - poly_dim) {
#ifndef NDEBUG
    cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	 << "in a vector space of dimension `n',"
	 << "any polyhedron of dimension `k'" << endl
	 << "should have `n-k' non-redundant constraints at least."
	 << endl
	 << "Here space_dim = " << space_dim << ", "
	 << "poly_dim = " << poly_dim << ", "
	 << "but num_constraints = " << num_constraints << "!"
	 << endl;
#endif
    return false;
  }

  if (num_points == 0) {
#ifndef NDEBUG
    cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	 << "the generator system has no points!"
	 << endl;
#endif
    return false;
  }

  if (lin_space_dim == space_dim) {
    // This was a universe polyhedron.
    if (num_constraints > 0) {
#ifndef NDEBUG
      cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	   << "a universe polyhedron has non-redundant constraints!"
	   << endl;
#endif
      return false;
    }

    if (num_points != 1) {
#ifndef NDEBUG
      cerr << "In the BHRZ03 info about a non-empty polyhedron:" << endl
	   << "a universe polyhedron has more than one non-redundant point!"
	   << endl;
#endif
      return false;
    }
  }

  // All tests passed.
  return true;
}

int
PPL::Polyhedron::BHRZ03_info::compare(const BHRZ03_info& y) const {
  assert(OK() && y.OK());
  if (poly_dim != y.poly_dim)
    return poly_dim > y.poly_dim ? 1 : -1;
  if (lin_space_dim != y.lin_space_dim)
    return lin_space_dim > y.lin_space_dim ? 1 : -1; 
  if (num_constraints != y.num_constraints)
    return num_constraints > y.num_constraints ? 1 : -1;
  if (num_points != y.num_points)
    return num_points > y.num_points ? 1 : -1;

  const dimension_type space_dim = num_zero_ray_coord.size();
  assert(num_zero_ray_coord.size() == y.num_zero_ray_coord.size());
  // Note: iterating upwards, because we have to check first
  // the number of rays having more NON-zero coordinates.
  for (dimension_type i = 0; i < space_dim; i++)
    if (num_zero_ray_coord[i] != y.num_zero_ray_coord[i])
      return num_zero_ray_coord[i] > y.num_zero_ray_coord[i] ? 1 : -1;
  // All components are equal.
  return 0;
}

void
PPL::Polyhedron::collect_BHRZ03_info(BHRZ03_info& info) const {
  assert(!marked_empty() && !has_something_pending()
	 && constraints_are_minimized() && generators_are_minimized());

  // Since the constraint system is minimized, the dimension of
  // the polyhedron is obtained by subtracting the number of
  // equalities from the space dimension.
  info.poly_dim = space_dim - con_sys.num_equalities();
  // Since the generator systems is minimized,
  // the dimension of the lineality space is equal to the number of lines.
  info.lin_space_dim = gen_sys.num_lines();

  // The constraint system is in minimal form ...
  info.num_constraints = con_sys.num_rows();
  // ... but for a correct reasoning we have to disregard
  // the low-level constraints (i.e., the positivity constraint
  // and epsilon bounds).
  // TODO: provide a correct implementation for NNC polyhedra.
  for (dimension_type i = info.num_constraints; i-- > 0; )
    if (con_sys[i].is_trivial_true()) {
      --info.num_constraints;
      break;
    }

  // The generator system is in minimal form.
  const dimension_type gen_sys_num_rows = gen_sys.num_rows();
  info.num_points = 0;
  if (is_necessarily_closed()) {
    // Count the number of points.
    for (dimension_type i = gen_sys_num_rows; i-- > 0; )
      if (gen_sys[i].is_point())
	++info.num_points;
  }
  else {
    // For NNC polyhedra, count the number of closure points.
    for (dimension_type i = gen_sys_num_rows; i-- > 0; )
      if (gen_sys[i].is_closure_point())
	++info.num_points;
  }

  // For each i such that 0 <= i < space_dim,
  // `num_zero_ray_coord[i]' will be the number of rays
  // in `gen_sys' having exactly `i' coordinates equal to 0.
  for (dimension_type i = gen_sys_num_rows; i-- > 0; )
    if (gen_sys[i].is_ray()) {
      const Generator& r = gen_sys[i];
      dimension_type num_zeroes = 0;
      for (dimension_type j = space_dim; j >= 1; j--)
	if (r[j] == 0)
	  ++num_zeroes;
      ++info.num_zero_ray_coord[num_zeroes];
    }
}

bool
PPL::Polyhedron::is_BHRZ03_stabilizing(const Polyhedron& x,
				       const Polyhedron& y) {
  // It is assumed that `y' is included into `x'.
  assert(x.topology() == y.topology());
  assert(x.space_dim == y.space_dim);
  assert(!x.marked_empty() && !x.has_something_pending()
	 && x.constraints_are_minimized() && x.generators_are_minimized());
  assert(!y.marked_empty() && !y.has_something_pending()
	 && y.constraints_are_minimized() && y.generators_are_minimized());

  // If the dimension of `x' is greater than the dimension of `y',
  // the chain is stabilizing.
  // Since the constraint systems are minimized, the dimension of
  // the polyhedra is obtained by subtracting the number of
  // equalities from the space dimension.
  const dimension_type x_dimension =
    x.space_dim - x.con_sys.num_equalities();
  const dimension_type y_dimension =
    y.space_dim - y.con_sys.num_equalities();
  if (x_dimension > y_dimension) {
#if PPL_STATISTICS
    ++statistics->reason.poly_dim;
#endif
    return true;
  }

  // Since `y' is assumed to be included into `x',
  // at this point the two polyhedra must have the same dimension.
  assert(x_dimension == y_dimension);

  // If the dimension of the lineality space of `x' is greater than
  // the dimension of the lineality space of `y', then the chain
  // is stabilizing. Since both generator systems are minimized,
  // the dimension of the lineality space is equal to the number of lines.
  const dimension_type x_num_lines = x.gen_sys.num_lines();
  const dimension_type y_num_lines = y.gen_sys.num_lines();
  if (x_num_lines > y_num_lines) {
#if PPL_STATISTICS
    ++statistics->reason.lin_space_dim;
#endif
    return true;
  }

  // Since `y' is assumed to be included into `x', at this point
  // the lineality space of the two polyhedra must have the same dimension.
  assert (x_num_lines == y_num_lines);

  // If the number of constraints of `x' is smaller than the number
  // of constraints of `y', then the chain is stabilizing. If it is
  // bigger, the chain is not stabilizing. If they are equal, further
  // investigation is needed.

  // NOTE: we have to consider high-level constraints only.
  dimension_type x_con_sys_num_rows = 0;
  for (ConSys::const_iterator i = x.con_sys.begin(),
	 x_cs_end = x.con_sys.end(); i != x_cs_end; ++i)
    ++x_con_sys_num_rows;
  dimension_type y_con_sys_num_rows = 0;
  for (ConSys::const_iterator i = y.con_sys.begin(),
	 y_cs_end = y.con_sys.end(); i != y_cs_end; ++i)
    ++y_con_sys_num_rows;
  if (x_con_sys_num_rows < y_con_sys_num_rows) {
#if PPL_STATISTICS
    ++statistics->reason.num_constraints;
#endif
    return true;
  }
  else if (x_con_sys_num_rows > y_con_sys_num_rows)
    return false;

  const dimension_type x_gen_sys_num_rows = x.gen_sys.num_rows();
  const dimension_type y_gen_sys_num_rows = y.gen_sys.num_rows();
  if (x.is_necessarily_closed()) {
    // If the number of points of `x' is smaller than the number
    // of points of `y', then the chain is stabilizing.
    const dimension_type x_num_points
      = x_gen_sys_num_rows - x_num_lines - x.gen_sys.num_rays();
    const dimension_type y_num_points
      = y_gen_sys_num_rows - y_num_lines - y.gen_sys.num_rays();
    if (x_num_points < y_num_points) {
#if PPL_STATISTICS
      ++statistics->reason.num_points;
#endif
      return true;
    }
    else
      // If the number of points of `y' is smaller than the number of
      // points of `x', then the chain is not stabilizing.
      if (x_num_points > y_num_points)
	return false;
  }
  else {
    // The polyhedra are NNC.
    dimension_type x_num_closure_points = 0;
    for (dimension_type i = x_gen_sys_num_rows; i-- > 0; )
      if (x.gen_sys[i].is_closure_point())
	++x_num_closure_points;
    dimension_type y_num_closure_points = 0;
    for (dimension_type i = y_gen_sys_num_rows; i-- > 0; )
      if (y.gen_sys[i].is_closure_point())
	++y_num_closure_points;
    // If the number of closure points of `x' is smaller than
    // the number of closure points of `y', the chain is stabilizing.
    if (x_num_closure_points < y_num_closure_points) {
#if PPL_STATISTICS
      ++statistics->reason.num_points;
#endif
      return true;
    }
    else
      // If the number of closure points of `y' is smaller than the
      // number of closure points of `x', the chain is not stabilizing.
      if (x_num_closure_points > y_num_closure_points)
	return false;
  }

  // For each i such that 0 <= i < x.space_dim, let x_num_rays[i] be
  // the number of rays in x.gen_sys having exactly `i' coordinates
  // equal to 0.
  std::vector<dimension_type> x_num_rays(x.space_dim, 0);
  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; )
    if (x.gen_sys[i].is_ray()) {
      const Generator& r = x.gen_sys[i];
      dimension_type num_zeroes = 0;
      for (dimension_type j = x.space_dim; j >= 1; j--)
	if (r[j] == 0)
	  ++num_zeroes;
      ++x_num_rays[num_zeroes];
    }
  // The same as above, this time for `y'.
  std::vector<dimension_type> y_num_rays(y.space_dim, 0);
  for (dimension_type i = y_gen_sys_num_rows; i-- > 0; )
    if (y.gen_sys[i].is_ray()) {
      const Generator& r = y.gen_sys[i];
      dimension_type num_zeroes = 0;
      for (dimension_type j = y.space_dim; j >= 1; j--)
	if (r[j] == 0)
	  ++num_zeroes;
      ++y_num_rays[num_zeroes];
    }
  // Compare (lexicographically) the two vectors:
  // if x_num_rays < y_num_rays the chain is stabilizing.
  for (dimension_type i = 0; i < x.space_dim; i++) {
    if (x_num_rays[i] > y_num_rays[i])
      // Not stabilizing.
      break;
    if (x_num_rays[i] < y_num_rays[i]) {
#if PPL_STATISTICS
      ++statistics->reason.zero_coord_rays;
#endif
      return true;
    }
  }

  // The chain is not stabilizing.
  // NOTE: we do NOT check for equality of the two polyhedra here.
  return false;
}


bool
PPL::Polyhedron::BHRZ03_combining_constraints(const Polyhedron& y,
					      const BHRZ03_Certificate& y_cert,
					      const Polyhedron& H79,
					      const ConSys& x_minus_H79_cs) {
  Polyhedron& x = *this;
  // It is assumed that `y <= x <= H79'.
  assert(x.topology() == y.topology()
	 && x.topology() == H79.topology()
	 && x.topology() == x_minus_H79_cs.topology());
  assert(x.space_dim == y.space_dim
	 && x.space_dim == H79.space_dim
	 && x.space_dim == x_minus_H79_cs.space_dimension());
  assert(!x.marked_empty() && !x.has_something_pending()
	 && x.constraints_are_minimized() && x.generators_are_minimized());
  assert(!y.marked_empty() && !y.has_something_pending()
	 && y.constraints_are_minimized() && y.generators_are_minimized());
  assert(!H79.marked_empty() && !H79.has_something_pending()
	 && H79.constraints_are_minimized() && H79.generators_are_minimized());

  // We will choose from `x_minus_H79_cs' many subsets of constraints,
  // that will be collected (one at a time) into `combining_cs'.
  // For each group collected, we compute an average constraint,
  // that will be stored into `new_cs'.

  // There is no point in applying this technique when `x_minus_H79_cs'
  // has one constraint at most (no ``new'' constraint can be computed).
  const dimension_type x_minus_H79_cs_num_rows = x_minus_H79_cs.num_rows();
  if (x_minus_H79_cs_num_rows <= 1)
    return false;

  const Topology tpl = x.topology();
  ConSys combining_cs(tpl);
  ConSys new_cs(tpl);

  // Consider the points that belong to both `x.gen_sys' and `y.gen_sys'.
  // For NNC polyhedra, the role of points is played by closure points.
  const bool closed = x.is_necessarily_closed();
  for (dimension_type i = y.gen_sys.num_rows(); i-- > 0; ) {
    const Generator& g = y.gen_sys[i];
    if ((g.is_point() && closed) || (g.is_closure_point() && !closed)) {
      // If in `H79.con_sys' there is already an inequality constraint
      // saturating this point, then there is no need to produce another
      // constraint.
      bool lies_on_the_boundary_of_H79 = false;
      const ConSys& H79_cs = H79.con_sys;
      for (dimension_type j = H79_cs.num_rows(); j-- > 0; ) {
	const Constraint& c = H79_cs[j];
	if (c.is_inequality() && c * g == 0) {
	  lies_on_the_boundary_of_H79 = true;
	  break;
	}
      }
      if (lies_on_the_boundary_of_H79)
	continue;

      // Consider all the constraints in `x_minus_H79_con_sys'
      // that are saturated by the point `g'.
      combining_cs.clear();
      for (dimension_type j = x_minus_H79_cs_num_rows; j-- > 0; ) {
	const Constraint& c = x_minus_H79_cs[j];
	if (c * g == 0)
	  combining_cs.insert(c);
      }
      // Build a new constraint by combining all the chosen constraints.
      const dimension_type combining_cs_num_rows = combining_cs.num_rows();
      if (combining_cs_num_rows > 0) {
	if (combining_cs_num_rows == 1)
	  // No combination is needed.
	  new_cs.insert(combining_cs[0]);
	else {
	  LinExpression e(0);
	  bool strict_inequality = false;
	  for (dimension_type h = combining_cs_num_rows; h-- > 0; ) {
	    if (combining_cs[h].is_strict_inequality())
	      strict_inequality = true;
	    e += LinExpression(combining_cs[h]);
	  }
	  // Simple normalization is enough, since
	  // `e' will not become an equality constraint.
	  e.normalize();

	  if (!e.all_homogeneous_terms_are_zero())
	    if (strict_inequality)
	      new_cs.insert(e > 0);
	    else
	      new_cs.insert(e >= 0);
	}
      }
    }
  }

  // If none of the collected constraints strictly intersects `H79',
  // then the technique was unsuccessful.
  bool improves_upon_H79 = false;
  const Poly_Con_Relation si = Poly_Con_Relation::strictly_intersects();
  for (dimension_type i = new_cs.num_rows(); i-- > 0; )
    if (H79.relation_with(new_cs[i]) == si) {
      improves_upon_H79 = true;
      break;
    }
  if (!improves_upon_H79)
    return false;

  // The resulting polyhedron is obtained by adding the constraints
  // in `new_cs' to polyhedron `H79'.
  Polyhedron result = H79;
  result.add_recycled_constraints_and_minimize(new_cs);

  // Check for stabilization wrt `y_cert' and improvement over `H79'.
  if (y_cert.is_stabilizing(result) && !result.contains(H79)) {
    // The technique was successful.
#if PPL_STATISTICS
    ++statistics->technique.combining_constraints;
#endif
    std::swap(x, result);
    assert(x.OK(true));
    return true;
  }
  else
    // The technique was unsuccessful.
    return false;
}

bool
PPL::Polyhedron::BHRZ03_evolving_points(const Polyhedron& y,
					const BHRZ03_Certificate& y_cert, 
					const Polyhedron& H79) {
  Polyhedron& x = *this;
  // It is assumed that `y <= x <= H79'.
  assert(x.topology() == y.topology()
	 && x.topology() == H79.topology());
  assert(x.space_dim == y.space_dim
	 && x.space_dim == H79.space_dim);
  assert(!x.marked_empty() && !x.has_something_pending()
	 && x.constraints_are_minimized() && x.generators_are_minimized());
  assert(!y.marked_empty() && !y.has_something_pending()
	 && y.constraints_are_minimized() && y.generators_are_minimized());
  assert(!H79.marked_empty() && !H79.has_something_pending()
	 && H79.constraints_are_minimized() && H79.generators_are_minimized());

  // For each point in `x.gen_sys' that is not in `y',
  // this technique tries to identify a set of rays that:
  //  - are included in polyhedron `H79';
  //  - when added to `y' will subsume the point.
  GenSys candidate_rays;

  const dimension_type x_gen_sys_num_rows = x.gen_sys.num_rows();
  const dimension_type y_gen_sys_num_rows = y.gen_sys.num_rows();
  const bool closed = x.is_necessarily_closed();
  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; ) {
    Generator& g1 = x.gen_sys[i];
    // For C polyhedra, we choose a point of `x.gen_sys'
    // that is not included in `y'.
    // In the case of NNC polyhedra, we can restrict attention to
    // closure points (considering also points will only add redundancy).
    if (((g1.is_point() && closed) || (g1.is_closure_point() && !closed))
	&& y.relation_with(g1) == Poly_Gen_Relation::nothing()) {
      // For each point (resp., closure point) `g2' in `y.gen_sys',
      // where `g1' and `g2' are different,
      // build the candidate ray `g1 - g2'.
      for (dimension_type j = y_gen_sys_num_rows; j-- > 0; ) {
	const Generator& g2 = y.gen_sys[j];
	if ((g2.is_point() && closed)
	    || (g2.is_closure_point() && !closed)) {
	  assert(compare(g1, g2) != 0);
	  Generator ray_from_g2_to_g1 = g1;
	  ray_from_g2_to_g1.linear_combine(g2, 0);
	  candidate_rays.insert(ray_from_g2_to_g1);
	}
      }
    }
  }

  // Be non-intrusive.
  Polyhedron result = x;
  result.add_recycled_generators_and_minimize(candidate_rays);
  result.intersection_assign_and_minimize(H79);

  // Check for stabilization wrt `y_cert' and improvement over `H79'.
  if (y_cert.is_stabilizing(result) && !result.contains(H79)) {
    // The technique was successful.
#if PPL_STATISTICS
    ++statistics->technique.evolving_points;
#endif
    std::swap(x, result);
    assert(x.OK(true));
    return true;
  }
  else
    // The technique was unsuccessful.
    return false;
}

bool
PPL::Polyhedron::BHRZ03_evolving_rays(const Polyhedron& y,
				      const BHRZ03_Certificate& y_cert, 
				      const Polyhedron& H79) {
  Polyhedron& x = *this;
  // It is assumed that `y <= x <= H79'.
  assert(x.topology() == y.topology()
	 && x.topology() == H79.topology());
  assert(x.space_dim == y.space_dim
	 && x.space_dim == H79.space_dim);
  assert(!x.marked_empty() && !x.has_something_pending()
	 && x.constraints_are_minimized() && x.generators_are_minimized());
  assert(!y.marked_empty() && !y.has_something_pending()
	 && y.constraints_are_minimized() && y.generators_are_minimized());
  assert(!H79.marked_empty() && !H79.has_something_pending()
	 && H79.constraints_are_minimized() && H79.generators_are_minimized());

  const dimension_type x_gen_sys_num_rows = x.gen_sys.num_rows();
  const dimension_type y_gen_sys_num_rows = y.gen_sys.num_rows();

  // Candidate rays are kept in a temporary generator system.
  GenSys candidate_rays;
  Integer& tmp_1 = tmp_Integer[0];
  Integer& tmp_2 = tmp_Integer[1];
  for (dimension_type i = x_gen_sys_num_rows; i-- > 0; ) {
    const Generator& x_g = x.gen_sys[i];
    // We choose a ray of `x' that does not belong to `y'.
    if (x_g.is_ray() && y.relation_with(x_g) == Poly_Gen_Relation::nothing()) {
      for (dimension_type j = y_gen_sys_num_rows; j-- > 0; ) {
	const Generator& y_g = y.gen_sys[j];
	if (y_g.is_ray()) {
	  Generator new_ray(x_g);
	  // Modify `new_ray' according to the evolution of `x_g' wrt `y_g'.
	  std::deque<bool> considered(x.space_dim + 1);
	  for (dimension_type k = 1; k < x.space_dim; ++k)
	    if (!considered[k])
	      for (dimension_type h = k + 1; h <= x.space_dim; ++h)
		if (!considered[h]) {
		  tmp_1 = x_g[k] * y_g[h];
		  tmp_2 = x_g[h] * y_g[k];
		  tmp_1 -= tmp_2;
		  const int clockwise
		    = sgn(tmp_1);
		  const int first_or_third_quadrant
		    = sgn(x_g[k])*sgn(x_g[h]);
		  switch (clockwise * first_or_third_quadrant) {
		  case -1:
		    new_ray[k] = 0;
		    considered[k] = true;
		    break;
		  case 1:
		    new_ray[h] = 0;
		    considered[h] = true;
		    break;
		  default:
		    break;
		  }
		}
	  new_ray.normalize();
	  candidate_rays.insert(new_ray);
	}
      }
    }
  }

  // If there are no candidate rays, we cannot obtain stabilization.
  if (candidate_rays.num_rows() == 0)
    return false;

  // Be non-intrusive.
  Polyhedron result = x;
  // Add to `result' the rays in `candidate_rays'
  result.add_recycled_generators_and_minimize(candidate_rays);
  // Intersect with `H79'.
  result.intersection_assign_and_minimize(H79);

  // Check for stabilization wrt `y' and improvement over `H79'.
  if (y_cert.is_stabilizing(result) && !result.contains(H79)) {
    // The technique was successful.
#if PPL_STATISTICS
    ++statistics->technique.evolving_rays;
#endif
    std::swap(x, result);
    assert(x.OK(true));
    return true;
  }
  else
    // The technique was unsuccessful.
    return false;
}

void
PPL::Polyhedron::BHRZ03_widening_assign(const Polyhedron& y, unsigned* tp) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.topology() != y.topology())
    throw_topology_incompatible("BHRZ03_widening_assign(y)", y);
  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("BHRZ03_widening_assign(y)", y);

#ifndef NDEBUG
  {
    // We assume that y is contained in or equal to x.
    const Polyhedron x_copy = x;
    const Polyhedron y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  // If any argument is zero-dimensional or empty,
  // the BHRZ03-widening behaves as the identity function.
  if (x.space_dim == 0 || x.marked_empty() || y.marked_empty()) {
#if PPL_STATISTICS
    ++statistics->reason.zero_dim_or_empty;
    ++statistics->technique.nop;
#endif
    return;
  }

  // `x.con_sys' and `x.gen_sys' should be in minimal form.
  x.minimize();

  // `y.con_sys' and `y.gen_sys' should be in minimal form.
  if (y.is_necessarily_closed()) {
    if (!y.minimize()) {
      // `y' is empty: the result is `x'.
#if PPL_STATISTICS
      ++statistics->reason.zero_dim_or_empty;
      ++statistics->technique.nop;
#endif
      return;
    }
  }
  else {
    // Dealing with a NNC polyhedron.
    // To obtain a correct reasoning when comparing
    // the constraints of `x' with the generators of `y',
    // we enforce the inclusion relation holding between
    // the two NNC polyhedra `x' and `y' (i.e., `y <= x')
    // to also hold for the corresponding eps-representations:
    // this is obtained by intersecting the two eps-representations.
    Polyhedron& yy = const_cast<Polyhedron&>(y);
    if (!yy.intersection_assign_and_minimize(x)) {
      // `y' is empty: the result is `x'.
#if PPL_STATISTICS
      ++statistics->reason.zero_dim_or_empty;
      ++statistics->technique.nop;
#endif
      return;
    }
  }

  // Compute certificate info for polyhedron `y'.
  BHRZ03_Certificate y_cert(y);

  // If the iteration is stabilizing, the resulting polyhedron is `x'.
  // At this point, also check if the two polyhedra are the same
  // (exploiting the knowledge that `y <= x').
  if (y_cert.is_stabilizing(x) || y.contains(x)) {
#if PPL_STATISTICS
    ++statistics->technique.nop;
#endif
    assert(OK());
    return;
  }

  // Here the iteration is not immediately stabilizing.
  // If we are using the widening-with-tokens technique and
  // there are tokens available, use one of them and return `x'.
  if (tp != 0 && *tp > 0) {
#if PPL_STATISTICS
    ++statistics->technique.delay;
#endif
    --(*tp);
    assert(OK());
    return;
  }

  // Copy into `H79_cs' the constraints that are common to `x' and `y',
  // according to the definition of the H79 widening.
  // The other ones are copied into `x_minus_H79_cs'.
  const Topology tpl = x.topology();
  ConSys H79_cs(tpl);
  ConSys x_minus_H79_cs(tpl);
  x.select_H79_constraints(y, H79_cs, x_minus_H79_cs);

  // We cannot have selected all of the rows, since otherwise
  // the iteration should have been immediately stabilizing.
  assert(x_minus_H79_cs.num_rows() > 0);
  // Be careful to obtain the right space dimension
  // (because `H79_cs' may be empty).
  Polyhedron H79(tpl, x.space_dim, UNIVERSE);
  H79.add_recycled_constraints_and_minimize(H79_cs);

  // NOTE: none of the following widening heuristics is intrusive:
  // they will modify `x' only when returning successfully.
  if (x.BHRZ03_combining_constraints(y, y_cert, H79, x_minus_H79_cs))
    return;

  assert(H79.OK() && x.OK() && y.OK());

  if (x.BHRZ03_evolving_points(y, y_cert, H79))
    return;

  assert(H79.OK() && x.OK() && y.OK());

  if (x.BHRZ03_evolving_rays(y, y_cert, H79))
    return;

  assert(H79.OK() && x.OK() && y.OK());

  // No previous technique was successful: fall back to the H79 widening.
#if PPL_STATISTICS
  ++statistics->technique.h79;
#endif
  std::swap(x, H79);
  assert(x.OK(true));

#ifndef NDEBUG
  // The H79 widening is always stabilizing.
  x.minimize();
  assert(y_cert.is_stabilizing(x));
#endif
}

void
PPL::Polyhedron::limited_BHRZ03_extrapolation_assign(const Polyhedron& y,
						     const ConSys& cs,
						     unsigned* tp) {
  Polyhedron& x = *this;
  // Topology compatibility check.
  if (x.is_necessarily_closed()) {
    if (!y.is_necessarily_closed())
      throw_topology_incompatible("limited_BHRZ03_extrapolation_assign(y, cs)",
				  y);
    if (cs.has_strict_inequalities())
      throw_topology_incompatible("limited_BHRZ03_extrapolation_assign(y, cs)",
				  cs);
  }
  else if (y.is_necessarily_closed())
    throw_topology_incompatible("limited_BHRZ03_extrapolation_assign(y, cs)",
				y);

  // Dimension-compatibility check.
  if (x.space_dim != y.space_dim)
    throw_dimension_incompatible("limited_BHRZ03_extrapolation_assign(y, cs)",
				 y);
  // `cs' must be dimension-compatible with the two polyhedra.
  const dimension_type cs_space_dim = cs.space_dimension();
  if (x.space_dim < cs_space_dim)
    throw_dimension_incompatible("limited_BHRZ03_extrapolation_assign(y, cs)",
				 "cs", cs);

#ifndef NDEBUG
  {
    // We assume that y is contained in or equal to x.
    const Polyhedron x_copy = x;
    const Polyhedron y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  if (y.marked_empty())
    return;
  if (x.marked_empty())
    return;

  // The limited BHRZ03-widening between two polyhedra in a
  // zero-dimensional space is a polyhedron in a zero-dimensional
  // space, too.
  if (x.space_dim == 0)
    return;

  if (!y.minimize())
    // We have just discovered that `y' is empty.
    return;

  // Update the generators of `x': these are used to select,
  // from the constraints in `cs', those that must be added
  // to the resulting polyhedron.
  if ((x.has_pending_constraints() && !x.process_pending_constraints())
      || (!x.generators_are_up_to_date() && !x.update_generators()))
    // We have just discovered that `x' is empty.
    return;

  ConSys new_cs;
  // The constraints to be added must be satisfied by all the
  // generators of `x'. We can disregard `y' because `y <= x'.
  const GenSys& x_gs = x.gen_sys;
  // FIXME: why iterating upwards here?
  // FIXME: why repeating the call to cs[i]?
  for (dimension_type i = 0, cs_num_rows = cs.num_rows(); i < cs_num_rows; ++i)
    if (x_gs.satisfied_by_all_generators(cs[i]))
      new_cs.insert(cs[i]);

  x.BHRZ03_widening_assign(y, tp);
  x.add_constraints(new_cs);
  assert(OK());
}

void
PPL::Polyhedron::bounded_BHRZ03_extrapolation_assign(const Polyhedron& y,
						     const ConSys& cs,
						     unsigned* tp) {
  ConSys bounding_cs;
  BW_Box box(bounding_cs);
  shrink_bounding_box(box, ANY);
  limited_BHRZ03_extrapolation_assign(y, cs, tp);
  add_recycled_constraints(bounding_cs);
}
