/* Grid_Generator_System class implementation (non-inline functions).
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

#include <config.h>

#include "Grid_Generator_System.defs.hh"
#include "Grid_Generator_System.inlines.hh"
#include "Scalar_Products.defs.hh"

#include <cassert>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid_Generator_System::insert(const Grid_Generator& g) {

  // This is a copy of Generator_System::affine_image.

  // We are sure that the matrix has no pending rows
  // and that the new row is not a pending generator.
  assert(num_pending_rows() == 0);
  if (topology() == g.topology())
    Linear_System::insert(g);
  else
    // `*this' and `g' have different topologies.
    if (is_necessarily_closed()) {
      // Padding the matrix with the column
      // corresponding to the epsilon coefficients:
      // all points must have epsilon coordinate equal to 1
      // (i.e., the epsilon coefficient is equal to the divisor);
      // rays and lines must have epsilon coefficient equal to 0.
      // Note: normalization is preserved.
      const dimension_type eps_index = num_columns();
      add_zero_columns(1);
      Generator_System& gs = *this;
      for (dimension_type i = num_rows(); i-- > 0; ) {
	Generator& gen = gs[i];
	if (!gen.is_line_or_ray())
	  gen[eps_index] = gen[0];
      }
      set_not_necessarily_closed();
      // Inserting the new generator.
      Linear_System::insert(g);
    }
    else {
      // The generator system is NOT necessarily closed:
      // copy the generator, adding the missing dimensions
      // and the epsilon coefficient.
      const dimension_type new_size = 2 + std::max(g.space_dimension(),
						   space_dimension());
      Generator tmp_g(g, new_size);
      // If it was a point, set the epsilon coordinate to 1
      // (i.e., set the coefficient equal to the divisor).
      // Note: normalization is preserved.
      if (!tmp_g.is_line_or_ray())
	tmp_g[new_size - 1] = tmp_g[0];
      tmp_g.set_not_necessarily_closed();
      // Inserting the new generator.
      Linear_System::insert(tmp_g);
    }
  assert(OK());
}

void
PPL::Grid_Generator_System
::affine_image(dimension_type v,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator,
	       bool grid) {
  // FIX use grid_generator

  // This is mostly a copy of Generator_System::affine_image.

  Generator_System& x = *this;
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the epsilon dimension of NNC polyhedra).
  assert(v > 0 && v <= x.space_dimension());
  assert(expr.space_dimension() <= x.space_dimension());
  assert(denominator > 0);

  const dimension_type n_columns = x.num_columns();
  const dimension_type n_rows = x.num_rows();

  // Compute the numerator of the affine transformation and assign it
  // to the column of `*this' indexed by `v'.
  TEMP_INTEGER(numerator);
  for (dimension_type i = n_rows; i-- > 0; ) {
    Generator& row = x[i];
    Scalar_Products::assign(numerator, expr, row);
    std::swap(numerator, row[v]);
  }

  if (denominator != 1) {
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Generator& row = x[i];
      for (dimension_type j = n_columns; j-- > 0; )
	if (j != v)
	  row[j] *= denominator;
    }
  }

  // If the mapping is not invertible we may have transformed
  // valid lines and rays into the origin of the space.
  const bool not_invertible = (v > expr.space_dimension() || expr[v] == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_rays();

  if (grid)
    return;

  // Strong normalization also resets the sortedness flag.
  x.strong_normalize();
}

bool
PPL::Grid_Generator_System::ascii_load(std::istream& s) {
  // FIX

  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;
  if (str == "NECESSARILY_CLOSED")
    set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
      return false;
  resize_no_copy(nrows, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  set_sorted(str == "(sorted)");
  dimension_type index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> index))
    return false;
  set_index_first_pending_row(index);

  Grid_Generator_System& x = *this;
  for (dimension_type i = 0; i < x.num_rows(); ++i) {
    for (dimension_type j = 0; j < x.num_columns(); ++j)
      if (!(s >> x[i][j]))
	return false;

    if (!(s >> str))
      return false;
    if (str == "L")
      x[i].set_is_line();
    else
      x[i].set_is_ray_or_point();

    // Checking for equality of actual and declared types.
    switch (x[i].type()) {
    case Grid_Generator::LINE:
      if (str == "L")
	continue;
      break;
    case Grid_Generator::PARAMETER:
      if (str == "R")
	continue;
      break;
    case Grid_Generator::POINT:
      if (str == "P")
	continue;
      break;
    }
    // Reaching this point means that the input was illegal.
    return false;
  }

  // Checking for well-formedness.

  assert(OK());
  return true;
}

bool
PPL::Grid_Generator_System::OK() const {
  // A Generator_System and hence a Grid_Generator must be a valid
  // Linear_System; do not check for strong normalization, since this
  // will be done when checking each individual generator.
  if (!Linear_System::OK(false))
    return false;

  // Checking each generator in the system.
  const Generator_System& x = *this;
  for (dimension_type i = num_rows(); i-- > 0; )
    if (!x[i].OK(false))
      return false;

  // All checks passed.
  return true;
}
