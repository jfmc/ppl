/* Generator_System class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#include <ppl-config.h>

#include "Generator_System.defs.hh"
#include "Generator_System.inlines.hh"
#include "Constraint.defs.hh"
#include "Scalar_Products.defs.hh"
#include "Scalar_Products.inlines.hh"
#include "assert.hh"
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

bool
PPL::Generator_System::
adjust_topology_and_space_dimension(const Topology new_topology,
				    const dimension_type new_space_dim) {
  PPL_ASSERT(space_dimension() <= new_space_dim);

  const dimension_type old_space_dim = space_dimension();
  const Topology old_topology = sys.topology();
  dimension_type cols_to_be_added = new_space_dim - old_space_dim;

  // Dealing with empty generator systems first.
  if (sys.has_no_rows()) {
    if (sys.num_columns() == 0)
      if (new_topology == NECESSARILY_CLOSED) {
	sys.add_zero_columns(cols_to_be_added + 1);
	sys.set_necessarily_closed();
      }
      else {
	sys.add_zero_columns(cols_to_be_added + 2);
	sys.set_not_necessarily_closed();
      }
    else
      // Here `num_columns() > 0'.
      if (old_topology != new_topology)
	if (new_topology == NECESSARILY_CLOSED) {
	  switch (cols_to_be_added) {
	  case 0:
	    sys.remove_trailing_columns(1);
	    break;
	  case 1:
	    // Nothing to do.
	    break;
	  default:
	    sys.add_zero_columns(cols_to_be_added - 1);
	  }
	  sys.set_necessarily_closed();
	}
	else {
	  // Here old_topology == NECESSARILY_CLOSED
	  //  and new_topology == NOT_NECESSARILY_CLOSED.
	  sys.add_zero_columns(cols_to_be_added + 1);
	  sys.set_not_necessarily_closed();
	}
      else
	// Here topologies agree.
	if (cols_to_be_added > 0)
	  sys.add_zero_columns(cols_to_be_added);
    PPL_ASSERT(OK());
    return true;
  }

  // Here the generator system is not empty.
  if (cols_to_be_added > 0)
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED generator system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain closure points.
	// This check has to be performed under the user viewpoint.
	if (has_closure_points())
	  return false;
	// For a correct implementation, we have to remove those
	// closure points that were matching a point (i.e., those
	// that are in the generator system, but are invisible to
	// the user).
	Generator_System& gs = *this;
	dimension_type num_closure_points = 0;
	dimension_type gs_end = gs.sys.num_rows();
	for (dimension_type i = 0; i < gs_end; ) {
	  // All the closure points seen so far have consecutive
	  // indices starting from `i'.
	  if (num_closure_points > 0)
	    // Let next generator have index `i'.
            gs.sys.swap_rows(i, i + num_closure_points);
	  if (gs[i].is_closure_point()) {
	    ++num_closure_points;
	    --gs_end;
	  }
	  else
	    ++i;
	}
	// We may have identified some closure points.
	if (num_closure_points > 0) {
	  PPL_ASSERT(num_closure_points == sys.num_rows() - gs_end);
	  sys.remove_trailing_rows(num_closure_points);
	}
	// Remove the epsilon column, re-normalize and, after that,
	// add the missing dimensions. This ensures that
	// non-zero epsilon coefficients will be cleared.
	sys.remove_trailing_columns(1);
	sys.set_necessarily_closed();
	sys.normalize();
	sys.add_zero_columns(cols_to_be_added);
      }
      else {
        sys.add_zero_columns(cols_to_be_added);
        convert_into_non_necessarily_closed();
      }
    else {
      // Topologies agree: first add the required zero columns ...
      sys.add_zero_columns(cols_to_be_added);
      // ... and, if needed, move the epsilon coefficients
      // to the new last column.
      if (old_topology == NOT_NECESSARILY_CLOSED)
	sys.swap_columns(old_space_dim + 1, new_space_dim + 1);
    }
  else
    // Here `cols_to_be_added == 0'.
    if (old_topology != new_topology) {
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED generator system
	// can be converted in to a NECESSARILY_CLOSED one
	// only if it does not contain closure points.
	if (has_closure_points())
	  return false;
	// We just remove the column of the epsilon coefficients.
	sys.remove_trailing_columns(1);
	sys.set_necessarily_closed();
      }
      else
        convert_into_non_necessarily_closed();
    }
  // We successfully adjusted dimensions and topology.
  PPL_ASSERT(OK());
  return true;
}

// TODO: would be worth to avoid adding closure points
// that already are in the system of generators?
// To do this efficiently we could sort the system and
// perform insertions keeping its sortedness.
void
PPL::Generator_System::add_corresponding_closure_points() {
  PPL_ASSERT(!sys.is_necessarily_closed());
  // NOTE: we always add (pending) rows at the end of the generator system.
  // Updating `index_first_pending', if needed, is done by the caller.
  Generator_System& gs = *this;
  const dimension_type n_rows = gs.sys.num_rows();
  const dimension_type eps_index = gs.sys.num_columns() - 1;
  for (dimension_type i = n_rows; i-- > 0; ) {
    const Generator& g = gs[i];
    if (g[eps_index] > 0) {
      // `g' is a point: adding the closure point.
      Generator cp = g;
      cp[eps_index] = 0;
      // Enforcing normalization.
      cp.normalize();
      gs.insert_pending_recycled(cp);
    }
  }
  PPL_ASSERT(OK());
}


// TODO: would be worth to avoid adding points
// that already are in the system of generators?
// To do this efficiently we could sort the system and
// perform insertions keeping its sortedness.
void
PPL::Generator_System::add_corresponding_points() {
  PPL_ASSERT(!sys.is_necessarily_closed());
  // NOTE: we always add (pending) rows at the end of the generator system.
  // Updating `index_first_pending', if needed, is done by the caller.
  Generator_System& gs = *this;
  const dimension_type n_rows = gs.sys.num_rows();
  const dimension_type eps_index = gs.sys.num_columns() - 1;
  for (dimension_type i = 0; i < n_rows; i++) {
    const Generator& g = gs[i];
    if (!g.is_line_or_ray() && g[eps_index] == 0) {
      // `g' is a closure point: adding the point.
      // Note: normalization is preserved.
      Generator p = g;
      p[eps_index] = p[0];
      gs.insert_pending_recycled(p);
    }
  }
  PPL_ASSERT(OK());
}

bool
PPL::Generator_System::has_closure_points() const {
  if (sys.is_necessarily_closed())
    return false;
  // Adopt the point of view of the user.
  for (Generator_System::const_iterator i = begin(),
	 this_end = end(); i != this_end; ++i)
    if (i->is_closure_point())
      return true;
  return false;
}

void
PPL::Generator_System::convert_into_non_necessarily_closed() {
  // Padding the matrix with the column
  // corresponding to the epsilon coefficients:
  // all points must have epsilon coordinate equal to 1
  // (i.e., the epsilon coefficient is equal to the divisor);
  // rays and lines must have epsilon coefficient equal to 0.
  // Note: normalization is preserved.
  const dimension_type eps_index = sys.num_columns();
  sys.add_zero_columns(1);
  Generator_System& gs = *this;

  Swapping_Vector<Linear_Row> rows;
  // Release the rows from the linear system, so they can be modified.
  gs.sys.release_rows(rows);

  for (dimension_type i = rows.size(); i-- > 0; ) {
    Linear_Row& row = rows[i];
    Generator& gen = static_cast<Generator&>(row);
    if (!gen.is_line_or_ray())
      gen[eps_index] = gen[0];
  }

  // Put the rows back into the linear system.
  gs.sys.take_ownership_of_rows(rows);

  sys.set_not_necessarily_closed();
}

bool
PPL::Generator_System::has_points() const {
  const Generator_System& gs = *this;
  // Avoiding the repeated tests on topology.
  if (sys.is_necessarily_closed())
    for (dimension_type i = sys.num_rows(); i-- > 0; ) {
      if (!gs[i].is_line_or_ray())
	return true;
    }
  else {
    // !is_necessarily_closed()
    const dimension_type eps_index = gs.sys.num_columns() - 1;
    for (dimension_type i = sys.num_rows(); i-- > 0; )
    if (gs[i][eps_index] != 0)
      return true;
  }
  return false;
}

void
PPL::Generator_System_const_iterator::skip_forward() {
  const Linear_System<Linear_Row>::const_iterator gsp_end = gsp->end();
  if (i != gsp_end) {
    Linear_System<Linear_Row>::const_iterator i_next = i;
    ++i_next;
    if (i_next != gsp_end) {
      const Generator& cp = static_cast<const Generator&>(*i);
      const Generator& p = static_cast<const Generator&>(*i_next);
      if (cp.is_closure_point()
	  && p.is_point()
	  && cp.is_matching_closure_point(p))
	i = i_next;
    }
  }
}

void
PPL::Generator_System::insert(const Generator& g) {
  Generator tmp = g;
  insert_recycled(tmp);
}

void
PPL::Generator_System::insert_pending(const Generator& g) {
  Generator tmp = g;
  insert_pending_recycled(tmp);
}

void
PPL::Generator_System::insert_recycled(Generator& g) {
  // We are sure that the matrix has no pending rows
  // and that the new row is not a pending generator.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  if (sys.topology() == g.topology())
    sys.insert_recycled(g);
  else
    // `*this' and `g' have different topologies.
    if (sys.is_necessarily_closed()) {
      convert_into_non_necessarily_closed();
      // Inserting the new generator.
      sys.insert_recycled(g);
    }
    else {
      // The generator system is NOT necessarily closed:
      // copy the generator, adding the missing dimensions
      // and the epsilon coefficient.
      const dimension_type new_size = 2 + std::max(g.space_dimension(),
						   space_dimension());
      g.resize(new_size);
      // If it was a point, set the epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      // Note: normalization is preserved.
      if (!g.is_line_or_ray())
	g[new_size - 1] = g[0];
      g.set_not_necessarily_closed();
      // Inserting the new generator.
      sys.insert_recycled(g);
    }
  PPL_ASSERT(OK());
}

void
PPL::Generator_System::insert_pending_recycled(Generator& g) {
  if (sys.topology() == g.topology())
    sys.insert_pending_recycled(g);
  else
    // `*this' and `g' have different topologies.
    if (sys.is_necessarily_closed()) {
      convert_into_non_necessarily_closed();

      // Inserting the new generator.
      sys.insert_pending_recycled(g);
    }
    else {
      // The generator system is NOT necessarily closed:
      // copy the generator, adding the missing dimensions
      // and the epsilon coefficient.
      const dimension_type new_size = 2 + std::max(g.space_dimension(),
						   space_dimension());
      g.resize(new_size);
      // If it was a point, set the epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      // Note: normalization is preserved.
      if (!g.is_line_or_ray())
	g[new_size - 1] = g[0];
      g.set_not_necessarily_closed();
      // Inserting the new generator.
      sys.insert_pending_recycled(g);
    }
  PPL_ASSERT(OK());
}

PPL::dimension_type
PPL::Generator_System::num_lines() const {
  // We are sure that this method is applied only to a matrix
  // that does not contain pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  const Generator_System& gs = *this;
  dimension_type n = 0;
  // If sys happens to be sorted, take advantage of the fact
  // that lines are at the top of the system.
  if (sys.is_sorted()) {
    dimension_type nrows = sys.num_rows();
    for (dimension_type i = 0; i < nrows && gs[i].is_line(); ++i)
      ++n;
  }
  else
    for (dimension_type i = sys.num_rows(); i-- > 0 ; )
      if (gs[i].is_line())
	++n;
  return n;
}

PPL::dimension_type
PPL::Generator_System::num_rays() const {
  // We are sure that this method is applied only to a matrix
  // that does not contain pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  const Generator_System& gs = *this;
  dimension_type n = 0;
  // If sys happens to be sorted, take advantage of the fact
  // that rays and points are at the bottom of the system and
  // rays have the inhomogeneous term equal to zero.
  if (sys.is_sorted()) {
    for (dimension_type i = sys.num_rows(); i != 0 && gs[--i].is_ray_or_point(); )
      if (gs[i].is_line_or_ray())
	++n;
  }
  else
    for (dimension_type i = sys.num_rows(); i-- > 0 ; )
      if (gs[i].is_ray())
	++n;
  return n;
}

PPL::Poly_Con_Relation
PPL::Generator_System::relation_with(const Constraint& c) const {
  // Note: this method is not public and it is the responsibility
  // of the caller to actually test for dimension compatibility.
  // We simply assert it.
  PPL_ASSERT(space_dimension() >= c.space_dimension());
  // Number of generators: the case of an empty polyhedron
  // has already been filtered out by the caller.
  const dimension_type n_rows = sys.num_rows();
  PPL_ASSERT(n_rows > 0);
  const Generator_System& gs = *this;

  // `result' will keep the relation holding between the generators
  // we have seen so far and the constraint `c'.
  Poly_Con_Relation result = Poly_Con_Relation::saturates();

  switch (c.type()) {

  case Constraint::EQUALITY:
    {
      // The hyperplane defined by the equality `c' is included
      // in the set of points satisfying `c' (it is the same set!).
      result = result && Poly_Con_Relation::is_included();
      // The following integer variable will hold the scalar product sign
      // of either the first point or the first non-saturating ray we find.
      // If it is equal to 2, then it means that we haven't found such
      // a generator yet.
      int first_point_or_nonsaturating_ray_sign = 2;

      for (dimension_type i = n_rows; i-- > 0; ) {
	const Generator& g = gs[i];
	const int sp_sign = Scalar_Products::sign(c, g);
	// Checking whether the generator saturates the equality.
	// If that is the case, then we have to do something only if
	// the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point()) {
	    if (first_point_or_nonsaturating_ray_sign == 2)
	      // It is the first time that we find a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray_sign = 0;
	    else
	      // We already found a point or a non-saturating ray.
	      if (first_point_or_nonsaturating_ray_sign != 0)
		return Poly_Con_Relation::strictly_intersects();
	  }
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c'
	    // and the points generated by `gs'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray_sign == 2) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray_sign = sp_sign;
	      result = Poly_Con_Relation::is_disjoint();
	    }
	    else
	      // We already found a point or a non-saturating ray.
	      if (sp_sign != first_point_or_nonsaturating_ray_sign)
		return Poly_Con_Relation::strictly_intersects();
	    break;

	  case Generator::POINT:
	  case Generator::CLOSURE_POINT:
	    // NOTE: a non-saturating closure point is treated as
	    // a normal point.
	    if (first_point_or_nonsaturating_ray_sign == 2) {
	      // It is the first time that we find a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray_sign = sp_sign;
	      result = Poly_Con_Relation::is_disjoint();
	    }
	    else
	      // We already found a point or a non-saturating ray.
	      if (sp_sign != first_point_or_nonsaturating_ray_sign)
		return Poly_Con_Relation::strictly_intersects();
	    break;
	  }
      }
    }
    break;

  case Constraint::NONSTRICT_INEQUALITY:
    {
      // The hyperplane implicitly defined by the non-strict inequality `c'
      // is included in the set of points satisfying `c'.
      result = result && Poly_Con_Relation::is_included();
      // The following Boolean variable will be set to `false'
      // as soon as either we find (any) point or we find a
      // non-saturating ray.
      bool first_point_or_nonsaturating_ray = true;

      for (dimension_type i = n_rows; i-- > 0; ) {
	const Generator& g = gs[i];
	const int sp_sign = Scalar_Products::sign(c, g);
	// Checking whether the generator saturates the non-strict
	// inequality. If that is the case, then we have to do something
	// only if the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point()) {
	    if (first_point_or_nonsaturating_ray)
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray = false;
	    else
	      // We already found a point or a non-saturating ray before.
	      if (result == Poly_Con_Relation::is_disjoint())
		// Since g saturates c, we have a strict intersection if
		// none of the generators seen so far are included in `c'.
		return Poly_Con_Relation::strictly_intersects();
	  }
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c' and
	    // the points generated by `gs'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray = false;
	      result = (sp_sign > 0)
		? Poly_Con_Relation::is_included()
		: Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray.
	      if ((sp_sign > 0
		   && result == Poly_Con_Relation::is_disjoint())
		  || (sp_sign < 0
		      && result.implies(Poly_Con_Relation::is_included())))
		// We have a strict intersection if either:
		// - `g' satisfies `c' but none of the generators seen
		//    so far are included in `c'; or
		// - `g' does not satisfy `c' and all the generators
		//    seen so far are included in `c'.
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign > 0)
		// Here all the generators seen so far either saturate
		// or are included in `c'.
		// Since `g' does not saturate `c' ...
		result = Poly_Con_Relation::is_included();
	    }
	    break;

	  case Generator::POINT:
	  case Generator::CLOSURE_POINT:
	    // NOTE: a non-saturating closure point is treated as
	    // a normal point.
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      // - If point `g' saturates `c', then all the generators
	      //   seen so far saturate `c'.
	      // - If point `g' is included (but does not saturate) `c',
	      //   then all the generators seen so far are included in `c'.
	      // - If point `g' does not satisfy `c', then all the
	      //   generators seen so far are disjoint from `c'.
	      first_point_or_nonsaturating_ray = false;
	      if (sp_sign > 0)
		result = Poly_Con_Relation::is_included();
	      else if (sp_sign < 0)
		result = Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && result == Poly_Con_Relation::is_disjoint())
		  || (sp_sign < 0
		      && result.implies(Poly_Con_Relation::is_included())))
		// We have a strict intersection if either:
		// - `g' satisfies or saturates `c' but none of the
		//    generators seen so far are included in `c'; or
		// - `g' does not satisfy `c' and all the generators
		//    seen so far are included in `c'.
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign > 0)
		// Here all the generators seen so far either saturate
		// or are included in `c'.
		// Since `g' does not saturate `c' ...
		result = Poly_Con_Relation::is_included();
	    }
	    break;
	  }
      }
    }
    break;

  case Constraint::STRICT_INEQUALITY:
    {
      // The hyperplane implicitly defined by the strict inequality `c'
      // is disjoint from the set of points satisfying `c'.
      result = result && Poly_Con_Relation::is_disjoint();
      // The following Boolean variable will be set to `false'
      // as soon as either we find (any) point or we find a
      // non-saturating ray.
      bool first_point_or_nonsaturating_ray = true;
      for (dimension_type i = n_rows; i-- > 0; ) {
	const Generator& g = gs[i];
	// Using the reduced scalar product operator to avoid
	// both topology and num_columns mismatches.
	const int sp_sign = Scalar_Products::reduced_sign(c, g);
	// Checking whether the generator saturates the strict inequality.
	// If that is the case, then we have to do something
	// only if the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point()) {
	    if (first_point_or_nonsaturating_ray)
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray = false;
	    else
	      // We already found a point or a non-saturating ray before.
	      if (result == Poly_Con_Relation::is_included())
		return Poly_Con_Relation::strictly_intersects();
	  }
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c' and the points
	    // generated by `gs'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray = false;
	      result = (sp_sign > 0)
		? Poly_Con_Relation::is_included()
		: Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && result.implies(Poly_Con_Relation::is_disjoint()))
		  ||
		  (sp_sign <= 0
		   && result == Poly_Con_Relation::is_included()))
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign < 0)
		// Here all the generators seen so far either saturate
		// or are disjoint from `c'.
		// Since `g' does not saturate `c' ...
		result = Poly_Con_Relation::is_disjoint();
	    }
	    break;

	  case Generator::POINT:
	  case Generator::CLOSURE_POINT:
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      // - If point `g' saturates `c', then all the generators
	      //   seen so far saturate `c'.
	      // - If point `g' is included in (but does not saturate) `c',
	      //   then all the generators seen so far are included in `c'.
	      // - If point `g' strictly violates `c', then all the
	      //   generators seen so far are disjoint from `c'.
	      first_point_or_nonsaturating_ray = false;
	      if (sp_sign > 0)
		result = Poly_Con_Relation::is_included();
	      else if (sp_sign < 0)
		result = Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && result.implies(Poly_Con_Relation::is_disjoint()))
		  ||
		  (sp_sign <= 0
		   && result == Poly_Con_Relation::is_included()))
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign < 0)
		// Here all the generators seen so far either saturate
		// or are disjoint from `c'.
		// Since `g' does not saturate `c' ...
		result = Poly_Con_Relation::is_disjoint();
	    }
	    break;
	  }
      }
    }
    break;
  }
  // We have seen all generators.
  return result;
}


bool
PPL::Generator_System::satisfied_by_all_generators(const Constraint& c) const {
  PPL_ASSERT(c.space_dimension() <= space_dimension());

  // Setting `sps' to the appropriate scalar product sign operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  Topology_Adjusted_Scalar_Product_Sign sps(c);

  const Generator_System& gs = *this;
  switch (c.type()) {
  case Constraint::EQUALITY:
    // Equalities must be saturated by all generators.
    for (dimension_type i = gs.sys.num_rows(); i-- > 0; )
      if (sps(c, gs[i]) != 0)
	return false;
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    // Non-strict inequalities must be saturated by lines and
    // satisfied by all the other generators.
    for (dimension_type i = gs.sys.num_rows(); i-- > 0; ) {
      const Generator& g = gs[i];
      const int sp_sign = sps(c, g);
      if (g.is_line()) {
	if (sp_sign != 0)
	  return false;
      }
      else
	// `g' is a ray, point or closure point.
	if (sp_sign < 0)
	  return false;
    }
    break;
  case Constraint::STRICT_INEQUALITY:
    // Strict inequalities must be saturated by lines,
    // satisfied by all generators, and must not be saturated by points.
    for (dimension_type i = gs.sys.num_rows(); i-- > 0; ) {
      const Generator& g = gs[i];
      const int sp_sign = sps(c, g);
      switch (g.type()) {
      case Generator::POINT:
	if (sp_sign <= 0)
	  return false;
	break;
      case Generator::LINE:
	if (sp_sign != 0)
	  return false;
	break;
      default:
	// `g' is a ray or closure point.
	if (sp_sign < 0)
	  return false;
	break;
      }
    }
    break;
  }
  // If we reach this point, `c' is satisfied by all generators.
  return true;
}


void
PPL::Generator_System
::affine_image(dimension_type v,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator) {
  Generator_System& x = *this;
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the epsilon dimension of NNC polyhedra).
  PPL_ASSERT(v > 0 && v <= x.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= x.space_dimension());
  PPL_ASSERT(denominator > 0);

  const dimension_type n_columns = x.sys.num_columns();
  const dimension_type n_rows = x.sys.num_rows();

  // TODO: Check if it's correct to arrive at this point with some pending
  // rows.
  const dimension_type pending_row_index = sys.first_pending_row();

  // Avoid triggering assertions in release_rows().
  sys.unset_pending_rows();

  Swapping_Vector<Linear_Row> rows;
  // Release the rows from the linear system, so they can be modified.
  x.sys.release_rows(rows);

  // Compute the numerator of the affine transformation and assign it
  // to the column of `*this' indexed by `v'.
  PPL_DIRTY_TEMP_COEFFICIENT(numerator);
  for (dimension_type i = n_rows; i-- > 0; ) {
    Linear_Row& row = rows[i];
    Scalar_Products::assign(numerator, expr.get_linear_row(), row);
    std::swap(numerator, row[v]);
  }

  if (denominator != 1) {
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Linear_Row& row = rows[i];
      for (dimension_type j = n_columns; j-- > 0; )
	if (j != v)
	  row[j] *= denominator;
    }
  }

  // Put the modified rows back into the linear system.
  x.sys.take_ownership_of_rows(rows);

  // Restore the pending row index.
  x.sys.set_index_first_pending_row(pending_row_index);

  // If the mapping is not invertible we may have transformed
  // valid lines and rays into the origin of the space.
  const bool not_invertible = (v > expr.space_dimension() || expr.get_linear_row()[v] == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_rays();

  // Strong normalization also resets the sortedness flag.
  x.sys.strong_normalize();
}

void
PPL::Generator_System::ascii_dump(std::ostream& s) const {
  const Generator_System& x = *this;
  const dimension_type x_num_rows = x.sys.num_rows();
  const dimension_type x_num_columns = x.sys.num_columns();
  s << "topology " << (sys.is_necessarily_closed()
		       ? "NECESSARILY_CLOSED"
		       : "NOT_NECESSARILY_CLOSED")
    << "\n"
    << x_num_rows << " x " << x_num_columns << ' '
    << (x.sys.is_sorted() ? "(sorted)" : "(not_sorted)")
    << "\n"
    << "index_first_pending " << x.sys.first_pending_row()
    << "\n";
  for (dimension_type i = 0; i < x_num_rows; ++i) {
    const Generator& g = x[i];
    for (dimension_type j = 0; j < x_num_columns; ++j)
      s << g[j] << ' ';
    switch (g.type()) {
    case Generator::LINE:
      s << "L";
      break;
    case Generator::RAY:
      s << "R";
      break;
    case Generator::POINT:
      s << "P";
      break;
    case Generator::CLOSURE_POINT:
      s << "C";
      break;
    }
    s << "\n";
  }
}

PPL_OUTPUT_DEFINITIONS(Generator_System)

bool
PPL::Generator_System::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;
  if (str == "NECESSARILY_CLOSED")
    sys.set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    sys.set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> ncols))
      return false;
  sys.resize_no_copy(0, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  sys.set_sorted(str == "(sorted)");
  dimension_type pending_index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> pending_index))
    return false;

  Generator_System& x = *this;
  for (dimension_type i = 0; i < nrows; ++i) {
    Linear_Row row(ncols, Linear_Row::Flags());
    Generator& gen = static_cast<Generator&>(row);
    
    for (dimension_type j = 0; j < x.sys.num_columns(); ++j)
      if (!(s >> row[j]))
	return false;

    if (!(s >> str))
      return false;
    if (str == "L")
      gen.set_is_line();
    else if (str == "R" || str == "P" || str == "C")
      gen.set_is_ray_or_point();
    else
      return false;

    gen.set_topology(sys.topology());

    // Checking for equality of actual and declared types.
    switch (gen.type()) {
    case Generator::LINE:
      if (str != "L")
	return false;
      break;
    case Generator::RAY:
      if (str != "R")
	return false;
      break;
    case Generator::POINT:
      if (str != "P")
	return false;
      break;
    case Generator::CLOSURE_POINT:
      if (str != "C")
	return false;
      break;
    }
    sys.insert_pending_recycled(row);
  }
  sys.set_index_first_pending_row(pending_index);

  // Check invariants.
  PPL_ASSERT(OK());
  return true;
}

void
PPL::Generator_System::remove_invalid_lines_and_rays() {
  // The origin of the vector space cannot be a valid line/ray.
  // NOTE: the following swaps will mix generators without even trying
  // to preserve sortedness: as a matter of fact, it will almost always
  // be the case that the input generator system is NOT sorted.
  Generator_System& gs = *this;
  const dimension_type old_n_rows = gs.sys.num_rows();
  dimension_type n_rows = old_n_rows;
  if (sys.num_pending_rows() == 0) {
    for (dimension_type i = n_rows; i-- > 0; ) {
      const Generator& g = gs[i];
      if (g.is_line_or_ray() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/ray has been found.
	--n_rows;
        gs.sys.swap_rows(i, n_rows);
      }
    }
    sys.set_index_first_pending_row(n_rows);
  }
  else {
    // If the matrix has some pending rows, we can not
    // swap the "normal" rows with the pending rows. So
    // we must put at the end of the "normal" rows
    // the invalid "normal" rows, put them at the end
    // of the matrix, find the invalid rows in the pending
    // part and then erase the invalid rows that now
    // are in the bottom part of the matrix.
    PPL_ASSERT(sys.num_pending_rows() > 0);
    dimension_type first_pending = sys.first_pending_row();
    for (dimension_type i = first_pending; i-- > 0; ) {
      const Generator& g = gs[i];
      if (g.is_line_or_ray() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/ray has been found.
	--first_pending;
	gs.sys.swap_rows(i, first_pending);
      }
    }
    const dimension_type num_invalid_rows
      = sys.first_pending_row() - first_pending;
    sys.set_index_first_pending_row(first_pending);
    for (dimension_type i = 0; i < num_invalid_rows; ++i)
      gs.sys.swap_rows(n_rows - i, first_pending + i);
    n_rows -= num_invalid_rows;
    for (dimension_type i = n_rows; i-- > first_pending; ) {
      const Generator& g = gs[i];
      if (g.is_line_or_ray() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/ray has been found.
	--n_rows;
        gs.sys.swap_rows(i, n_rows);
      }
    }
  }
  gs.sys.remove_trailing_rows(old_n_rows - n_rows);
}

const PPL::Generator_System* PPL::Generator_System::zero_dim_univ_p = 0;

void
PPL::Generator_System::initialize() {
  PPL_ASSERT(zero_dim_univ_p == 0);
  zero_dim_univ_p
    = new Generator_System(Generator::zero_dim_point());
}

void
PPL::Generator_System::finalize() {
  PPL_ASSERT(zero_dim_univ_p != 0);
  delete zero_dim_univ_p;
  zero_dim_univ_p = 0;
}

bool
PPL::Generator_System::OK() const {
  // A Generator_System must be a valid Linear_System; do not check for
  // strong normalization, since this will be done when
  // checking each individual generator.
  if (!sys.OK(false))
    return false;

  // Checking each generator in the system.
  const Generator_System& x = *this;
  for (dimension_type i = sys.num_rows(); i-- > 0; )
    if (!x[i].OK())
      return false;

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Generator_System */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Generator_System& gs) {
  Generator_System::const_iterator i = gs.begin();
  const Generator_System::const_iterator gs_end = gs.end();
  if (i == gs_end)
    return s << "false";
  while (true) {
    s << *i++;
    if (i == gs_end)
      return s;
    s << ", ";
  }
}
