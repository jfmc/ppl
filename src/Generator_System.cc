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

  if (sys.topology() != new_topology) {
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
      for (dimension_type i = 0; i < sys.num_rows(); )
        if (gs[i].is_closure_point())
          sys.remove_row(i, false);
        else
          ++i;
      sys.set_necessarily_closed();
      if (new_space_dim > old_space_dim)
        sys.normalize();
    } else {
      convert_into_non_necessarily_closed();
    }
  }

  sys.set_space_dimension(new_space_dim);
  
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
  const dimension_type eps_index = gs.sys.space_dimension() + 1;
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
  const dimension_type eps_index = sys.space_dimension() + 1;
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
  const dimension_type eps_index = sys.space_dimension() + 1;
  sys.set_not_necessarily_closed();
  Generator_System& gs = *this;

  Swapping_Vector<Generator> rows;
  // Release the rows from the linear system, so they can be modified.
  gs.sys.release_rows(rows);

  for (dimension_type i = rows.size(); i-- > 0; ) {
    Generator& gen = rows[i];
    if (!gen.is_line_or_ray())
      gen[eps_index] = gen[0];
  }

  // Put the rows back into the linear system.
  gs.sys.take_ownership_of_rows(rows);
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
    const dimension_type eps_index = gs.sys.space_dimension() + 1;
    for (dimension_type i = sys.num_rows(); i-- > 0; )
    if (gs[i][eps_index] != 0)
      return true;
  }
  return false;
}

void
PPL::Generator_System_const_iterator::skip_forward() {
  const Linear_System<Generator>::const_iterator gsp_end = gsp->end();
  if (i != gsp_end) {
    Linear_System<Generator>::const_iterator i_next = i;
    ++i_next;
    if (i_next != gsp_end) {
      const Generator& cp = *i;
      const Generator& p = *i_next;
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
      const dimension_type new_space_dim = std::max(g.space_dimension(),
                                                    space_dimension());
      g.set_not_necessarily_closed();
      g.set_space_dimension(new_space_dim);
      // If it was a point, set the epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      // Note: normalization is preserved.
      if (!g.is_line_or_ray())
	g[new_space_dim + 1] = g[0];
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
      const dimension_type new_space_dim = std::max(g.space_dimension(),
                                                    space_dimension());
      g.set_topology(NOT_NECESSARILY_CLOSED);
      g.set_space_dimension(new_space_dim);
      // If it was a point, set the epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      // Note: normalization is preserved.
      if (!g.is_line_or_ray())
	g[new_space_dim + 1] = g[0];
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
	// both topology and space dimension mismatches.
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
  // (which could also cause a mismatch in the number of space dimensions).
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
::affine_image(Variable v,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator) {
  Generator_System& x = *this;
  PPL_ASSERT(v.space_dimension() <= x.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= x.space_dimension());
  PPL_ASSERT(denominator > 0);

  // TODO: Avoid using the number of columns if possible.
  const dimension_type n_columns
    = sys.is_necessarily_closed() ? sys.space_dimension() + 1
                                  : sys.space_dimension() + 2;
  const dimension_type n_rows = x.sys.num_rows();

  // TODO: Check if it's correct to arrive at this point with some pending
  // rows.
  const dimension_type pending_row_index = sys.first_pending_row();

  // Avoid triggering assertions in release_rows().
  sys.unset_pending_rows();

  Swapping_Vector<Generator> rows;
  // Release the rows from the linear system, so they can be modified.
  x.sys.release_rows(rows);

  // Compute the numerator of the affine transformation and assign it
  // to the column of `*this' indexed by `v'.
  PPL_DIRTY_TEMP_COEFFICIENT(numerator);
  for (dimension_type i = n_rows; i-- > 0; ) {
    Generator& row = rows[i];
    Scalar_Products::assign(numerator, expr.get_linear_row(), row);
    std::swap(numerator, row[v.space_dimension()]);
  }

  if (denominator != 1) {
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Generator& row = rows[i];
      for (dimension_type j = n_columns; j-- > 0; )
	if (j != v.space_dimension())
	  row[j] *= denominator;
    }
  }

  // Put the modified rows back into the linear system.
  x.sys.take_ownership_of_rows(rows);

  // Restore the pending row index.
  x.sys.set_index_first_pending_row(pending_row_index);

  // If the mapping is not invertible we may have transformed
  // valid lines and rays into the origin of the space.
  const bool not_invertible = (v.space_dimension() > expr.space_dimension()
                               || expr.coefficient(v) == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_rays();

  // Strong normalization also resets the sortedness flag.
  x.sys.strong_normalize();
}

void
PPL::Generator_System::ascii_dump(std::ostream& s) const {
  const dimension_type num_rows = sys.num_rows();
  const dimension_type space_dims = sys.space_dimension();
  s << "topology " << (sys.is_necessarily_closed()
		       ? "NECESSARILY_CLOSED"
		       : "NOT_NECESSARILY_CLOSED")
    << "\n"
    << num_rows << " x " << space_dims << ' '
    << (sys.is_sorted() ? "(sorted)" : "(not_sorted)")
    << "\n"
    << "index_first_pending " << sys.first_pending_row()
    << "\n";
  for (dimension_type i = 0; i < num_rows; ++i)
    sys[i].ascii_dump(s);
}

PPL_OUTPUT_DEFINITIONS(Generator_System)

bool
PPL::Generator_System::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;

  sys.clear();

  if (str == "NECESSARILY_CLOSED")
    sys.set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    sys.set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type space_dims;
  if (!(s >> nrows))
    return false;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> space_dims))
      return false;

  sys.set_space_dimension(space_dims);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  sys.set_sorted(str == "(sorted)");
  dimension_type pending_index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> pending_index))
    return false;

  for (dimension_type i = 0; i < nrows; ++i) {
    Generator gen;
    gen.ascii_load(s);
    sys.insert_pending_recycled(gen);
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
  
  // Note that num_rows() is *not* constant, because it is decreased by
  // remove_row().
  for (dimension_type i = 0; i < num_rows(); ) {
    const Generator& g = (*this)[i];
    if (g.is_line_or_ray() && g.all_homogeneous_terms_are_zero())
      sys.remove_row(i, false);
    else
      ++i;
  }
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

