/* Grid_Generator_System class implementation (non-inline functions).
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
#include "Grid_Generator_System.defs.hh"
#include "Grid_Generator_System.inlines.hh"
#include "Scalar_Products.defs.hh"
#include "Variables_Set.defs.hh"
#include "assert.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid_Generator_System::recycling_insert(Grid_Generator_System& gs) {
  const dimension_type old_num_rows = num_rows();
  const dimension_type gs_num_rows = gs.num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type gs_num_columns = gs.num_columns();
  if (old_num_columns < gs_num_columns) {
    sys.add_zero_columns(gs_num_columns - old_num_columns);
    // Swap the parameter divisor column into the new last column.
    sys.swap_columns(old_num_columns - 1, num_columns() - 1);
  }
  Swapping_Vector<Grid_Generator> rows;
  gs.sys.release_rows(rows);
  for (dimension_type i = 0; i < gs_num_rows; ++i) {
    rows[i].set_topology(NECESSARILY_CLOSED);
    sys.insert_recycled(rows[i]);
  }
  set_index_first_pending_row(old_num_rows + gs_num_rows);
}

void
PPL::Grid_Generator_System::recycling_insert(Grid_Generator& g) {
  sys.insert_recycled(g);
}

void
PPL::Grid_Generator_System::insert(const Grid_Generator& g) {
  const dimension_type g_space_dim = g.space_dimension();

  if (g.is_parameter() && g.all_homogeneous_terms_are_zero()) {
    // There is no need to add the origin as a parameter,
    // as it will be immediately flagged as redundant.
    // However, we still have to adjust space dimension.
    const dimension_type initial_space_dim = space_dimension();
    if (initial_space_dim < g_space_dim) {
      // Adjust the space dimension.
      sys.add_zero_columns(g_space_dim - initial_space_dim);
      // Swap the parameter divisor column into the new last column.
      sys.swap_columns(g_space_dim + 1, initial_space_dim + 1);
      PPL_ASSERT(OK());
    }
    return;
  }

  // Note: we can not call sys.insert(g),
  // because that would check for strong normalization of g.
  PPL_ASSERT(sys.is_necessarily_closed() && sys.topology() == g.topology());
  // This method is only used when the system has no pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);

  const dimension_type old_num_rows = num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type g_size = g.size();

  // Resize the system, if necessary.
  if (g_size > old_num_columns) {
    sys.add_zero_columns(g_size - old_num_columns);
    if (old_num_rows > 0)
      // Swap the existing parameter divisor column into the new
      // last column.
      sys.swap_columns(old_num_columns - 1, g_size - 1);
    sys.insert(g);
  }
  else if (g_size < old_num_columns) {
    // Create a resized copy of the row (and move the parameter
    // divisor coefficient to its last position).
    Grid_Generator tmp_row(g, old_num_columns, old_num_columns);
    std::swap(tmp_row[g_size - 1], tmp_row[old_num_columns - 1]);
    sys.insert_recycled(tmp_row);
  }
  else
    // Here r_size == old_num_columns.
    sys.insert(g);

  set_index_first_pending_row(num_rows());
  set_sorted(false);

  PPL_ASSERT(OK());
}

void
PPL::Grid_Generator_System
::affine_image(dimension_type v,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator) {
  // This is mostly a copy of Generator_System::affine_image.

  Grid_Generator_System& x = *this;
  // `v' is the index of a column corresponding to a "user" variable
  // (i.e., it cannot be the inhomogeneous term).
  PPL_ASSERT(v > 0 && v <= x.sys.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= x.sys.space_dimension());
  PPL_ASSERT(denominator > 0);

  const dimension_type num_columns = x.num_columns();
  const dimension_type num_rows = x.num_rows();

  // Compute the numerator of the affine transformation and assign it
  // to the column of `*this' indexed by `v'.
  PPL_DIRTY_TEMP_COEFFICIENT(numerator);

  const dimension_type pending_row_index = sys.first_pending_row();

  // Avoid triggering assertions in release_rows().
  unset_pending_rows();

  Swapping_Vector<Grid_Generator> rows;
  // Release the rows from the linear system, so they can be modified.
  x.sys.release_rows(rows);
  
  for (dimension_type i = num_rows; i-- > 0; ) {
    Grid_Generator& row = rows[i];
    Scalar_Products::assign(numerator, expr.get_linear_row(), row);
    std::swap(numerator, row[v]);
  }

  if (denominator != 1)
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (dimension_type i = num_rows; i-- > 0; ) {
      Grid_Generator& row = rows[i];
      for (dimension_type j = num_columns; j-- > 0; )
	if (j != v)
	  row[j] *= denominator;
    }

  // Put the modified rows back into the linear system.
  x.sys.take_ownership_of_rows(rows);

  // Restore the number of pending rows.
  x.set_index_first_pending_row(pending_row_index);

  // If the mapping is not invertible we may have transformed valid
  // lines and rays into the origin of the space.
  const bool not_invertible = (v > expr.space_dimension()
                               || expr.get_linear_row()[v] == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_parameters();
}

PPL_OUTPUT_DEFINITIONS(Grid_Generator_System)

void
PPL::Grid_Generator_System::ascii_dump(std::ostream& s) const {
  const dimension_type num_rows = this->num_rows();
  s << num_rows << " x " << num_columns() << '\n';
  for (dimension_type i = 0; i < num_rows; ++i)
    operator[](i).ascii_dump(s);
}

bool
PPL::Grid_Generator_System::ascii_load(std::istream& s) {
  dimension_type num_rows;
  dimension_type num_columns;
  if (!(s >> num_rows))
    return false;
  std::string str;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> num_columns))
      return false;
  resize_no_copy(0, num_columns);

  set_sorted(false);

  for (dimension_type i = 0; i < num_rows; ++i) {
    Grid_Generator tmp;
    if (!tmp.ascii_load(s))
      return false;
    if (i == 0)
      sys.set_topology(tmp.topology());
    sys.insert_recycled(tmp);
  }

  set_index_first_pending_row(num_rows);
  // Check invariants.
  PPL_ASSERT(OK());

  return true;
}

const PPL::Grid_Generator_System*
PPL::Grid_Generator_System::zero_dim_univ_p = 0;

void
PPL::Grid_Generator_System::initialize() {
  PPL_ASSERT(zero_dim_univ_p == 0);
  zero_dim_univ_p
    = new Grid_Generator_System(Grid_Generator::zero_dim_point());
}

void
PPL::Grid_Generator_System::finalize() {
  PPL_ASSERT(zero_dim_univ_p != 0);
  delete zero_dim_univ_p;
  zero_dim_univ_p = 0;
}

bool
PPL::Grid_Generator_System::OK() const {
  if (sys.topology() == NOT_NECESSARILY_CLOSED) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator_System is NOT_NECESSARILY_CLOSED"
	      << std::endl;
#endif
    return false;
  }

  if (sys.is_sorted()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator_System is marked as sorted."
	      << std::endl;
#endif
    return false;
  }

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Grid_Generator_System */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s,
			      const Grid_Generator_System& gs) {
  Grid_Generator_System::const_iterator i = gs.begin();
  const Grid_Generator_System::const_iterator gs_end = gs.end();
  if (i == gs_end)
    return s << "false";
  while (true) {
    s << *i++;
    if (i == gs_end)
      return s;
    s << ", ";
  }
}

void
PPL::Grid_Generator_System
::add_universe_rows_and_columns(dimension_type dims) {
  PPL_ASSERT(num_columns() > 0);
  dimension_type col = num_columns() - 1;
  sys.add_zero_columns(dims);
  // Swap the parameter divisor column into the new last column.
  sys.swap_columns(col, col + dims);

  // Add the new rows and set their diagonal element.
  for (dimension_type i = 0; i < dims; ++i) {
    Grid_Generator tmp(num_columns(),
                       Linear_Row::Flags(NECESSARILY_CLOSED,
                                         Linear_Row::LINE_OR_EQUALITY));
    tmp[col] = 1;
    ++col;
    sys.insert_recycled(tmp);
  }
  unset_pending_rows();
}

void
PPL::Grid_Generator_System
::remove_space_dimensions(const Variables_Set& vars) {
  // Dimension-compatibility assertion.
  PPL_ASSERT(space_dimension() >= vars.space_dimension());

  // The removal of no dimensions from any system is a no-op.  This
  // case also captures the only legal removal of dimensions from a
  // 0-dim system.
  if (vars.empty())
    return;

  // For each variable to be removed, replace the corresponding column
  // by shifting left the columns to the right that will be kept.
  Variables_Set::const_iterator vsi = vars.begin();
  Variables_Set::const_iterator vsi_end = vars.end();
  dimension_type dst_col = *vsi+1;
  dimension_type src_col = dst_col + 1;
  for (++vsi; vsi != vsi_end; ++vsi) {
    const dimension_type vsi_col = *vsi+1;
    // Move all columns in between to the left.
    while (src_col < vsi_col)
      sys.swap_columns(dst_col++, src_col++);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type num_columns = this->num_columns();
  while (src_col < num_columns)
    sys.swap_columns(dst_col++, src_col++);

  // The number of remaining columns is `dst_col'.
  sys.remove_trailing_columns_without_normalizing(num_columns - dst_col);
}

void
PPL::Grid_Generator_System
::set_space_dimension(const dimension_type new_dimension) {
  dimension_type space_dim = space_dimension();

  // The removal of no dimensions from any system is a no-op.  Note
  // that this case also captures the only legal removal of dimensions
  // from a system in a 0-dim space.
  if (new_dimension == space_dim)
    return;

  if (new_dimension < space_dim) {
    // Swap the parameter divisor column into the column that will
    // become the last column.
    sys.swap_columns(new_dimension + 1, space_dim + 1);
    sys.remove_trailing_columns_without_normalizing(space_dim - new_dimension);
  } else {
    sys.add_zero_columns(new_dimension - space_dim);
    // Swap the parameter divisor column into the column that will
    // become the last column.
    sys.swap_columns(new_dimension + 1, space_dim + 1);
  }

  PPL_ASSERT(OK());
}

void
PPL::Grid_Generator_System::remove_invalid_lines_and_parameters() {
  // The origin of the vector space cannot be a valid line/parameter.
  // NOTE: the following swaps will mix grid generators without even trying
  // to preserve sortedness: as a matter of fact, it will almost always
  // be the case that the input generator system is NOT sorted.
  Grid_Generator_System& ggs = *this;
  const dimension_type old_n_rows = ggs.num_rows();
  dimension_type n_rows = old_n_rows;
  if (sys.num_pending_rows() == 0) {
    for (dimension_type i = n_rows; i-- > 0; ) {
      const Grid_Generator& g = ggs[i];
      if (g.is_line_or_parameter() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/parameter has been found.
	--n_rows;
        ggs.sys.swap_rows(i, n_rows);
      }
    }
    set_index_first_pending_row(n_rows);
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
      const Grid_Generator& g = ggs[i];
      if (g.is_line_or_parameter() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/parameter has been found.
	--first_pending;
        ggs.sys.swap_rows(i, first_pending);
      }
    }
    const dimension_type num_invalid_rows
      = sys.first_pending_row() - first_pending;
    set_index_first_pending_row(first_pending);
    for (dimension_type i = 0; i < num_invalid_rows; ++i)
      ggs.sys.swap_rows(n_rows - i, first_pending + i);
    n_rows -= num_invalid_rows;
    for (dimension_type i = n_rows; i-- > first_pending; ) {
      const Grid_Generator& g = ggs[i];
      if (g.is_line_or_parameter() && g.all_homogeneous_terms_are_zero()) {
	// An invalid line/parameter has been found.
	--n_rows;
        ggs.sys.swap_rows(i, n_rows);
      }
    }
  }
  ggs.sys.remove_trailing_rows(old_n_rows - n_rows);
}

bool
PPL::Grid_Generator_System::has_points() const {
  const Grid_Generator_System& ggs = *this;
  for (dimension_type i = num_rows(); i-- > 0; ) {
    if (!ggs[i].is_line_or_parameter())
      return true;
  }
  return false;
}

PPL::dimension_type
PPL::Grid_Generator_System::num_lines() const {
  // We are sure that this method is applied only to a matrix
  // that does not contain pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  const Grid_Generator_System& ggs = *this;
  dimension_type n = 0;
  // If the Linear_System happens to be sorted, take advantage of the fact
  // that lines are at the top of the system.
  if (sys.is_sorted()) {
    dimension_type nrows = num_rows();
    for (dimension_type i = 0; i < nrows && ggs[i].is_line(); ++i)
      ++n;
  }
  else {
    for (dimension_type i = num_rows(); i-- > 0 ; )
      if (ggs[i].is_line())
	++n;
  }
  return n;
}

PPL::dimension_type
PPL::Grid_Generator_System::num_parameters() const {
  // We are sure that this method is applied only to a matrix
  // that does not contain pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  const Grid_Generator_System& ggs = *this;
  dimension_type n = 0;
  // If the Linear_System happens to be sorted, take advantage of the fact
  // that rays and points are at the bottom of the system and
  // rays have the inhomogeneous term equal to zero.
  if (sys.is_sorted()) {
    for (dimension_type i = num_rows();
         i != 0 && ggs[--i].is_parameter_or_point(); )
      if (ggs[i].is_line_or_parameter())
	++n;
  }
  else {
    for (dimension_type i = num_rows(); i-- > 0 ; )
      if (ggs[i].is_parameter())
	++n;
  }
  return n;
}
