/* GenSys class implementation (non-inline functions).
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

#include <config.h>

#include "GenSys.defs.hh"

#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

bool
PPL::GenSys::adjust_topology_and_dimension(Topology new_topology,
					   size_t new_space_dim) {
  assert(space_dimension() <= new_space_dim);

  size_t old_space_dim = space_dimension();
  size_t cols_to_be_added = new_space_dim - old_space_dim;
  Topology old_topology = topology();

  if (cols_to_be_added > 0)
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED generator system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain closure points.
	if (has_closure_points())
	  return false;
	// Remove the \epsilon column and, after that,
	// add the missing dimensions. This ensures that
	// non-zero \epsilon coefficients will be cleared.
	resize_no_copy(num_rows(), old_space_dim + 1);
	set_necessarily_closed();
	add_zero_columns(cols_to_be_added);
      }
      else {
	// A NECESSARILY_CLOSED generator system is converted into
	// a NOT_NECESSARILY_CLOSED one by adding a further column
	// and setting the \epsilon coordinate of all points to 1.
	add_zero_columns(++cols_to_be_added);
	GenSys& gs = *this;
	size_t eps_index = new_space_dim + 1;
	for (size_t i = num_rows(); i-- > 0; )
	  gs[i][eps_index] = gs[i][0];
	set_not_necessarily_closed();
      }
    else {
      // Topologies agree: first add the required zero columns ...
      add_zero_columns(cols_to_be_added);
      // ... and, if needed, move the \epsilon coefficients
      // to the new last column.
      if (old_topology == NOT_NECESSARILY_CLOSED)
	swap_columns(old_space_dim + 1, new_space_dim + 1);
    }
  else
    // Here `cols_to_be_added == 0'.
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED generator system
	// can be converted in to a NECESSARILY_CLOSED one
	// only if it does not contain closure points.
	if (has_closure_points())
	  return false;
	// We just remove the column of the \epsilon coefficients.
	resize_no_copy(num_rows(), old_space_dim + 1);
	set_necessarily_closed();
      }
      else {
	// Add the column of the \epsilon coefficients
	// and set the \epsilon coordinate of all points to 1.
	add_zero_columns(1);
	GenSys& gs = *this;
	size_t eps_index = new_space_dim + 1;
	for (size_t i = num_rows(); i-- > 0; )
	  gs[i][eps_index] = gs[i][0];
	set_not_necessarily_closed();
      }
  // We successfully adjusted dimensions and topology.
  assert(OK());
  return true;
}

// FIXME: would be worth to avoid adding closure points
// that already are in the system of generators ?
// To do this efficiently we could sort the system and
// performing insertions keeping its sortedness.
void
PPL::GenSys::add_corresponding_closure_points() {
  assert(!is_necessarily_closed());
  GenSys& gs = *this;
  size_t n_rows = gs.num_rows();
  size_t eps_index = gs.num_columns() - 1;
  for (size_t i = n_rows; i-- > 0; ) {
    const Generator& g = gs[i];
    if (g[eps_index] > 0) {
      // `g' is a point: adding the closure point.
      Generator cp = g;
      cp[eps_index] = 0;
      gs.add_row(cp);
    }
  }
}


// FIXME: would be worth to avoid adding points
// that already are in the system of generators ?
// To do this efficiently we could sort the system and
// performing insertions keeping its sortedness.
void
PPL::GenSys::add_corresponding_points() {
  assert(!is_necessarily_closed());
  GenSys& gs = *this;
  size_t n_rows = gs.num_rows();
  size_t eps_index = gs.num_columns() - 1;
  for (size_t i = n_rows; i-- > 0; ) {
    const Generator& g = gs[i];
    if (g[0] > 0 && g[eps_index] == 0) {
      // `g' is a closure point: adding the point.
      Generator cp = g;
      cp[eps_index] = cp[0];
      gs.add_row(cp);
    }
  }
}


bool
PPL::GenSys::has_closure_points() const {
  // Avoiding the repeated tests on topology.
  if (is_necessarily_closed())
    return false;
  const GenSys& gs = *this;
  size_t eps_index = gs.num_columns() - 1;
  for (size_t i = num_rows(); i-- > 0; )
    if (gs[i][0] != 0 && gs[i][eps_index] == 0)
      return true;
  return false;
}


bool
PPL::GenSys::has_points() const {
  const GenSys& gs = *this;
  // Avoiding the repeated tests on topology.
  if (is_necessarily_closed())
    for (size_t i = num_rows(); i-- > 0; ) {
      if (gs[i][0] != 0)
	return true;
    }
  else {
    // is_necessarily_closed() == false.
    size_t eps_index = gs.num_columns() - 1;
    for (size_t i = num_rows(); i-- > 0; )
    if (gs[i][eps_index] != 0)
      return true;
  }
  return false;
}


void
PPL::GenSys::insert(const Generator& g) {
  if (topology() == g.topology())
    Matrix::insert(g);
  else 
    // `*this' and `g' have different topologies.
    if (is_necessarily_closed()) {
      // Padding the matrix with the column
      // corresponding to the \epsilon coefficients:
      // all points must have \epsilon coordinate equal to 1
      // (i.e., the \epsilon coefficient is equal to the divisor);
      // rays and lines must have \epsilon coefficient equal to 0.
      size_t eps_index = num_columns();
      add_zero_columns(1);
      GenSys& gs = *this;
      for (size_t i = num_rows(); i-- > 0; ) {
	Generator& gen = gs[i];
	if (gen[0] != 0)
	  gen[eps_index] = gen[0];
      }
      set_not_necessarily_closed();
      // Inserting the new generator.
      Matrix::insert(g);
    }
    else {
      // The generator system is NOT necessarily closed:
      // copy the generator, adding the missing dimensions
      // and the \epsilon coefficient.
      // NOTE: computing `gs_size = num_columns()' would provide
      //       a wrong result if the matrix has no rows.
      size_t gs_size = space_dimension() + 2;
      Generator tmp_g(g, gs_size);
      // If it was a point, set the \epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      if (tmp_g[0] != 0)
	tmp_g[gs_size - 1] = tmp_g[0];
      tmp_g.set_not_necessarily_closed();
      // Inserting the new generator.
      Matrix::insert(tmp_g);
    }
}

size_t
PPL::GenSys::num_lines() const {
  size_t n = 0;
  // If the Matrix happens to be sorted, take advantage of the fact
  // that lines are at the top of the system.
  if (is_sorted()) {
    size_t nrows = num_rows();
    for (size_t i = 0; i < nrows && (*this)[i].is_line(); ++i)
      ++n;
  }
  else
    for (size_t i = num_rows(); i-- > 0 ; )
      if ((*this)[i].is_line())
	++n;
  return n;
}

size_t
PPL::GenSys::num_rays() const {
  size_t n = 0;
  // If the Matrix happens to be sorted, take advantage of the fact
  // that rays and points are at the bottom of the system and
  // rays have the inhomogeneous term equal to zero.
  if (is_sorted()) {
    const GenSys& x = *this;
    for (size_t i = num_rows(); i != 0 && x[--i].is_ray_or_point(); )
      if (x[i][0] == 0)
	++n;
  }
  else
    for (size_t i = num_rows(); i-- > 0 ; ) {
      const Generator& g = (*this)[i];
      if (g.is_ray_or_point() && g[0] == 0)
	++n;
    }
  return n;
}

/*!
  Returns the relations holding between the set of points generated
  by \p *this and the constraint \p c.
*/
PPL::Poly_Con_Relation
PPL::GenSys::relation_with(const Constraint& c) const {
  // Note: this method is not public and it is the responsibility
  // of the caller to actually test for dimension compatibility.
  // We simply assert it.
  assert(space_dimension() >= c.space_dimension());
  // Number of generators: the case of an empty polyhedron
  // has already been filtered out by the caller.
  size_t n_rows = num_rows();
  assert(n_rows > 0);
  const GenSys& gen_sys = *this;

  // `res' will keep the relation holding between the generators
  // we have seen so far and the constraint `c'.
  Poly_Con_Relation res = Poly_Con_Relation::saturates();

  switch (c.type()) {

  case Constraint::EQUALITY:
    {
      // The hyperplane defined by the equality `c' is included
      // in the set of points satisfying `c' (it is the same set!).
      res = res && Poly_Con_Relation::is_included();
      // The following integer variable will hold the scalar product sign
      // of either the first point or the first non-saturating ray we find.
      // If it is equal to 2, then it means that we haven't found such
      // a generator yet.
      int first_point_or_nonsaturating_ray_sign = 2;

      for (size_t i = n_rows; i-- > 0; ) {
	const Generator& g = gen_sys[i];
	int sp_sign = sgn(c * g);
	// Checking whether the generator saturates the equality.
	// If that is the case, then we have to do something only if
	// the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point())
	    if (first_point_or_nonsaturating_ray_sign == 2)
	      // It is the first time that we find a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray_sign = 0;
	    else
	      // We already found a point or a non-saturating ray.
	      if (first_point_or_nonsaturating_ray_sign != 0)
		return Poly_Con_Relation::strictly_intersects();
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c'
	    // and the points generated by `gen_sys'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray_sign == 2) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray_sign = sp_sign;
	      res = Poly_Con_Relation::is_disjoint();
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
	      res = Poly_Con_Relation::is_disjoint();
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
      res = res && Poly_Con_Relation::is_included();
      // The following boolean variable will be set to `false'
      // as soon as either we find (any) point or we find a
      // non-saturating ray.
      bool first_point_or_nonsaturating_ray = true;

      for (size_t i = n_rows; i-- > 0; ) {
	const Generator& g = gen_sys[i];
	int sp_sign = sgn(c * g);
	// Checking whether the generator saturates the non-strict
	// inequality. If that is the case, then we have to do something
	// only if the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point())
	    if (first_point_or_nonsaturating_ray)
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray = false;
	    else
	      // We already found a point or a non-saturating ray before.
	      if (res == Poly_Con_Relation::is_disjoint())
		// Since g saturates c, we have a strict intersection if
		// none of the generators seen so far are included in `c'.
		return Poly_Con_Relation::strictly_intersects();
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c' and
	    // the points generated by `gen_sys'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray = false;
	      res = (sp_sign > 0)
		? Poly_Con_Relation::is_included()
		: Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray.
	      if ((sp_sign > 0
		   && res == Poly_Con_Relation::is_disjoint())
		  || (sp_sign < 0
		      && res.implies(Poly_Con_Relation::is_included())))
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
		res = Poly_Con_Relation::is_included();
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
		res = Poly_Con_Relation::is_included();
	      else if (sp_sign < 0)
		res = Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && res == Poly_Con_Relation::is_disjoint())
		  || (sp_sign < 0
		      && res.implies(Poly_Con_Relation::is_included())))
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
		res = Poly_Con_Relation::is_included();
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
      res = res && Poly_Con_Relation::is_disjoint();
      // The following boolean variable will be set to `false'
      // as soon as either we find (any) point or we find a
      // non-saturating ray.
      bool first_point_or_nonsaturating_ray = true;
      for (size_t i = n_rows; i-- > 0; ) {
	const Generator& g = gen_sys[i];
	// Using the reduced scalar product operator to avoid
	// both topology and num_columns mismatches.
	int sp_sign = sgn(reduced_scalar_product(c, g));
	// Checking whether the generator saturates the strict inequality.
	// If that is the case, then we have to do something
	// only if the generator is a point.
	if (sp_sign == 0) {
	  if (g.is_point())
	    if (first_point_or_nonsaturating_ray)
	      // It is the first time that we have a point and
	      // we have not found a non-saturating ray yet.
	      first_point_or_nonsaturating_ray = false;
	    else
	      // We already found a point or a non-saturating ray before.
	      if (res == Poly_Con_Relation::is_included())
		return Poly_Con_Relation::strictly_intersects();
	}
	else
	  // Here we know that sp_sign != 0.
	  switch (g.type()) {

	  case Generator::LINE:
	    // If a line does not saturate `c', then there is a strict
	    // intersection between the points satisfying `c' and the points
	    // generated by `gen_sys'.
	    return Poly_Con_Relation::strictly_intersects();

	  case Generator::RAY:
	    if (first_point_or_nonsaturating_ray) {
	      // It is the first time that we have a non-saturating ray
	      // and we have not found any point yet.
	      first_point_or_nonsaturating_ray = false;
	      res = (sp_sign > 0)
		? Poly_Con_Relation::is_included()
		: Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && res.implies(Poly_Con_Relation::is_disjoint()))
		  ||
		  (sp_sign <= 0
		   && res == Poly_Con_Relation::is_included()))
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign < 0)
		// Here all the generators seen so far either saturate
		// or are disjoint from `c'.
		// Since `g' does not saturate `c' ...
		res = Poly_Con_Relation::is_disjoint();
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
		res = Poly_Con_Relation::is_included();
	      else if (sp_sign < 0)
		res = Poly_Con_Relation::is_disjoint();
	    }
	    else {
	      // We already found a point or a non-saturating ray before.
	      if ((sp_sign > 0
		   && res.implies(Poly_Con_Relation::is_disjoint()))
		  ||
		  (sp_sign <= 0
		   && res == Poly_Con_Relation::is_included()))
		return Poly_Con_Relation::strictly_intersects();
	      if (sp_sign < 0)
		// Here all the generators seen so far either saturate
		// or are disjoint from `c'.
		// Since `g' does not saturate `c' ...
		res = Poly_Con_Relation::is_disjoint();
	    }
	    break;
	  }
      }
    }
    break;
  }
  // We have seen all generators.
  return res;
}


/*!
  \param v            Index of the column to which the
                      affine transformation is assigned.
  \param expr         The numerator of the affine transformation:
                      \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$.
  \param denominator  The denominator of the affine transformation.

  We want to allow affine transformations (see the Introduction) having
  any rational coefficients. Since the coefficients of the
  constraints are integers we must also provide an integer \p denominator
  that will be used as denominator of the affine transformation.

  The affine transformation assigns to each element of \p v -th
  column the follow expression:
  \f[
    \frac{\sum_{i = 0}^{n - 1} a_i x_i + b}
         {\mathrm{denominator}}.
  \f]

  \p expr is a constant parameter and unaltered by this computation.
*/
void
PPL::GenSys::affine_image(size_t v,
			  const LinExpression& expr,
			  const Integer& denominator) {
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the \epsilon dimension of NNC polyhedra).
  assert(v > 0 && v <= space_dimension());
  assert(expr.space_dimension() <= space_dimension());
  assert(denominator != 0);

  size_t n_columns = num_columns();
  size_t n_rows = num_rows();
  GenSys& x = *this;

  // Compute the numerator of the affine transformation and assign it
  // to the column of `*this' indexed by `v'.
  for (size_t i = n_rows; i-- > 0; ) {
    Generator& row = x[i];
    tmp_Integer[1] = 0;
    for (size_t j = expr.size(); j-- > 0; )
      tmp_Integer[1] += row[j] * expr[j];
    std::swap(tmp_Integer[1], row[v]); 
  }

  if (denominator != 1)
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (size_t i = n_rows; i-- > 0; )
      for (size_t j = n_columns; j-- > 0; )
	if (j != v)
	  x[i][j] *= denominator;

  // If the mapping is not invertible we may have trasformed
  // valid lines and rays into the origin of the space.
  bool not_invertible = (v > expr.space_dimension() || expr[v] == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_rays();

  x.strong_normalize();
}

/*!
  Like <CODE>ConSys::print()</CODE>, this prints the number of rows,
  the number of columns and value of \p sorted, using the
  <CODE>Matrix::print()</CODE> method, then prints the contents of
  all the rows, specifying whether a row represent a line or a point/ray.
*/
void
PPL::GenSys::print(std::ostream& s) const {
  Matrix::print(s);
  const char separator = ' ';
  const GenSys& x = *this;
  s << "topology ";
  if (!x.is_necessarily_closed())
    s << "NOT_";
  s << "NECESSARILY_CLOSED"
    << std::endl;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s << x[i][j] << separator;
    s << separator << separator;
    switch (static_cast<Generator>(x[i]).type()) {
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
    s << std::endl;
  }
}

/*!
  Like <CODE>ConSys::get()</CODE>, this uses <CODE>Matrix::get()</CODE>
  to resize the matrix of generators taking information from \p s,
  then initializes the coefficients of each generator and its type
  (line or ray/point).
*/
void
PPL::GenSys::get(std::istream& s) {
  Matrix::get(s);
  std::string tempstr;
  GenSys& x = *this;
  s >> tempstr;
  assert(tempstr == "topology");
  s >> tempstr;
  if (tempstr == "NECESSARILY_CLOSED")
    x.set_necessarily_closed();
  else {
    assert(tempstr == "NOT_NECESSARILY_CLOSED");
    x.set_not_necessarily_closed();
  }
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s >> x[i][j];
    s >> tempstr;
    if (tempstr == "L")
      x[i].set_is_line();
    else
      x[i].set_is_ray_or_point();
    // Checking for equality of actual and declared types.
    switch (static_cast<Generator>(x[i]).type()) {
    case Generator::LINE:
      if (tempstr == "L")
	continue;
      break;
    case Generator::RAY:
      if (tempstr == "R")
	continue;
      break;
    case Generator::POINT:
      if (tempstr == "P")
	continue;
      break;
    case Generator::CLOSURE_POINT:
      if (tempstr == "C")
	continue;
      break;
    }
    // Reaching this point means that the input was illegal.
    throw std::runtime_error("void PPL::GenSys::get(s)");
  }
  // Checking for well-formedness.
  if (!x.OK())
    throw std::runtime_error("void PPL::GenSys::get(s)");
}


void
PPL::GenSys::remove_invalid_lines_and_rays() {
  // The origin of the vector space cannot be a valid line/ray.
  GenSys& gs = *this;
  size_t n_rows = gs.num_rows();
  for (size_t i = n_rows; i-- > 0; ) {
    Generator& g = gs[i];
    if (g[0] == 0 && g.all_homogeneous_terms_are_zero()) {
      // An invalid line/ray has been found.
      --n_rows;
      std::swap(g, gs[n_rows]);
      gs.set_sorted(false);
    }
  }
  gs.erase_to_end(n_rows);
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this actually represents
  a system of generators. So, \p *this must satisfy some rule:
  -# it must be a valid Matrix;
  -# every row in the matrix must be a valid generator.
*/
bool
PPL::GenSys::OK() const {
  // A GenSys must be a valid Matrix.
  if (!Matrix::OK())
    return false;

  // Checking each generator in the system.
  for (size_t i = num_rows(); i-- > 0; ) {
    const Generator& g = (*this)[i];
    if (!g.OK())
      return false;
  }

  // All checks passed.
  return true;
}
