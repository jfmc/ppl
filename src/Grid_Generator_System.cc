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
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid_Generator_System::linear_system_insert(const Linear_Row& r) {

  // This is a slightly modified copy of Linear_System::insert.  It is
  // here to force Grid_Generator::OK to be used, which works around
  // the normalization assertions in Linear_System::OK.

  // The added row must be strongly normalized and have the same
  // topology as the system.
  assert(topology() == r.topology());
  // This method is only used when the system has no pending rows.
  assert(num_pending_rows() == 0);

  const dimension_type old_num_rows = num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type r_size = r.size();

  // Resize the system, if necessary.
  if (r_size > old_num_columns) {
    add_zero_columns(r_size - old_num_columns);
    if (!is_necessarily_closed() && old_num_rows != 0)
      // Move the epsilon coefficients to the last column
      // (note: sorting is preserved).
      swap_columns(old_num_columns - 1, r_size - 1);
    Matrix::add_row(r);
  }
  else if (r_size < old_num_columns)
    if (is_necessarily_closed() || old_num_rows == 0)
      Matrix::add_row(Linear_Row(r, old_num_columns, row_capacity));
    else {
      // Create a resized copy of the row (and move the epsilon
      // coefficient to its last position).
      Linear_Row tmp_row(r, old_num_columns, row_capacity);
      std::swap(tmp_row[r_size - 1], tmp_row[old_num_columns - 1]);
      // FIX what will free tmp_row mem if oom in add_row when insert
      //     if being called from a ctor, as in Constraint?
      Matrix::add_row(tmp_row);
    }
  else {
    // Here r_size == old_num_columns.
    Matrix::add_row(r);
  }
  set_index_first_pending_row(num_rows());
  set_sorted(false);

  // The added row was not a pending row.
  assert(num_pending_rows() == 0);
  assert(OK());
}

void
PPL::Grid_Generator_System::recycling_insert(Grid_Generator_System& gs) {
#if 0
  if (space_dim < gs_space_dim)
    throw_dimension_incompatible("recycling_insert(gs)", "gs", gs);
#endif

  const dimension_type old_num_rows = num_rows();
  const dimension_type gs_num_rows = gs.num_rows();
  add_zero_rows(gs_num_rows,
		Linear_Row::Flags(NECESSARILY_CLOSED,
				  Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
  for (dimension_type i = gs_num_rows; i-- > 0; )
    // Swap one coefficient at a time into the newly added rows
    // instead of swapping each entire row.  This ensures that the
    // added rows have the same capacities as the existing rows.
    operator[](old_num_rows + i).coefficient_swap(gs[i]);
}

void
PPL::Grid_Generator_System::insert(const Grid_Generator& g) {

#if 0
  if (g.is_parameter()) {
    // FIX Scale to match system divisor, or divisor of first point
  }
#endif

  // The rest is a copy of Generator_System::insert which calls
  // linear_system_insert instead of Linear_System::insert.

  // FIX much of this is redundant

  // We are sure that the matrix has no pending rows
  // and that the new row is not a pending generator.
  assert(num_pending_rows() == 0);
  if (topology() == g.topology())
    linear_system_insert(g);
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
      linear_system_insert(g);
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
      linear_system_insert(tmp_g);
    }

  set_sorted(false);
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

void
PPL::Grid_Generator_System::ascii_dump() const {
  ascii_dump(std::cerr);
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
      if (!(s >> const_cast<Coefficient&>(x[i][j])))
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
  if (topology() == NOT_NECESSARILY_CLOSED) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator_System is NOT_NECESSARILY_CLOSED"
	      << std::endl;
#endif
    return false;
  }

  if (is_sorted()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator_System is marked as sorted."
	      << std::endl;
#endif
    return false;
  }

  // A Generator_System and hence a Grid_Generator must be a valid
  // Linear_System; do not check for strong normalization, since this
  // will be done when checking each individual generator.
  if (!Linear_System::OK(false))
    return false;

  // Checking each generator in the system.
  const Grid_Generator_System& x = *this;
  for (dimension_type i = num_rows(); i-- > 0; )
    if (!x[i].OK())
      return false;

  // All checks passed.
  return true;
}

void
PPL::Grid_Generator_System
::add_universe_rows_and_columns(dimension_type dims) {
  // FIX add point if sys is empty
  dimension_type col = num_columns();
  add_zero_rows_and_columns(dims, dims,
			    Linear_Row::Flags(NECESSARILY_CLOSED,
					      Linear_Row::LINE_OR_EQUALITY));
  dimension_type rows = num_rows();
  for (dimension_type row = rows - dims; row < rows; ++row, ++col)
    const_cast<Coefficient&>(operator[](row)[col]) = 1;
}

void
PPL::Grid_Generator_System
::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // FIX may need some checks, as in Grid::remove_space_dimensions

  // For each variable to be removed, replace the corresponding column
  // by shifting left the columns to the right that will be kept.
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst_col = tbr->space_dimension();
  dimension_type src_col = dst_col + 1;
  for (++tbr; tbr != tbr_end; ++tbr) {
    dimension_type tbr_col = tbr->space_dimension();
    // Move all columns in between to the left.
    while (src_col < tbr_col)
      // FIXME: consider whether Linear_System must have a swap_columns()
      // method.  If the answer is "no", remove this Matrix:: qualification.
      Matrix::swap_columns(dst_col++, src_col++);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type num_cols = num_columns();
  while (src_col < num_cols)
    // FIXME: consider whether Linear_System must have a swap_columns()
    // method.  If the answer is "no", remove this Matrix:: qualification.
    Matrix::swap_columns(dst_col++, src_col++);

  // The number of remaining columns is `dst_col'.
  Matrix::remove_trailing_columns(num_cols - dst_col);
}

void
PPL::Grid_Generator_System
::remove_higher_space_dimensions(dimension_type new_dimension) {
  // FIX may need some checks, as in Grid::remove_space_dimensions
  Matrix::remove_trailing_columns(space_dimension() - new_dimension);
}
