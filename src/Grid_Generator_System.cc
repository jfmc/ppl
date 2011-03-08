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
  const dimension_type gs_num_rows = gs.num_rows();

  if (space_dimension() < gs.space_dimension())
    set_space_dimension(gs.space_dimension());
  else
    gs.set_space_dimension(space_dimension());

  Swapping_Vector<Grid_Generator> rows;
  gs.sys.release_rows(rows);
  for (dimension_type i = 0; i < gs_num_rows; ++i)
    sys.insert_recycled(rows[i]);

  unset_pending_rows();
}

void
PPL::Grid_Generator_System::recycling_insert(Grid_Generator& g) {
  sys.insert_recycled(g);
}

void
PPL::Grid_Generator_System::insert(const Grid_Generator& g) {
  if (g.is_parameter() && g.all_homogeneous_terms_are_zero()) {
    // There is no need to add the origin as a parameter,
    // as it will be immediately flagged as redundant.
    // However, we still have to adjust space dimension.
    if (space_dimension() < g.space_dimension())
      set_space_dimension(g.space_dimension());
    return;
  }

  // Note: we can not call sys.insert(g),
  // because that would check for strong normalization of g.
  PPL_ASSERT(sys.is_necessarily_closed() && sys.topology() == g.topology());
  // This method is only used when the system has no pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);

  // Resize the system, if necessary.
  if (g.space_dimension() > space_dimension()) {
    set_space_dimension(g.space_dimension());
    sys.insert(g);
  }
  else if (g.space_dimension() < space_dimension()) {
    // Insert a resized copy of the row.
    Grid_Generator tmp = g;
    tmp.set_space_dimension(space_dimension());
    sys.insert_recycled(tmp);
  }
  else
    // Here r_size == old_num_columns.
    sys.insert(g);

  PPL_ASSERT(OK());
}

void
PPL::Grid_Generator_System
::affine_image(Variable v,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator) {
  // This is mostly a copy of Generator_System::affine_image.

  Grid_Generator_System& x = *this;
  PPL_ASSERT(v.space_dimension() <= x.sys.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= x.sys.space_dimension());
  PPL_ASSERT(denominator > 0);

  const dimension_type num_columns = x.sys.num_columns();
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

  const dimension_type v_space_dim = v.space_dimension();
  
  for (dimension_type i = num_rows; i-- > 0; ) {
    Grid_Generator& row = rows[i];
    Scalar_Products::assign(numerator, expr.get_linear_row(), row);
    std::swap(numerator, row[v_space_dim]);
  }

  if (denominator != 1)
    // Since we want integer elements in the matrix,
    // we multiply by `denominator' all the columns of `*this'
    // having an index different from `v'.
    for (dimension_type i = num_rows; i-- > 0; ) {
      Grid_Generator& row = rows[i];
      for (dimension_type j = num_columns; j-- > 0; )
	if (j != v_space_dim)
	  row[j] *= denominator;
    }

  // Put the modified rows back into the linear system.
  x.sys.take_ownership_of_rows(rows);

  // Restore the number of pending rows.
  x.set_index_first_pending_row(pending_row_index);

  // If the mapping is not invertible we may have transformed valid
  // lines and rays into the origin of the space.
  const bool not_invertible = (v.space_dimension() >= expr.space_dimension()
                               || expr.coefficient(v) == 0);
  if (not_invertible)
    x.remove_invalid_lines_and_parameters();
}

PPL_OUTPUT_DEFINITIONS(Grid_Generator_System)

void
PPL::Grid_Generator_System::ascii_dump(std::ostream& s) const {
  const dimension_type num_rows = this->num_rows();
  s << num_rows << " x " << sys.num_columns() << '\n';
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

  sys.clear();
  if (sys.topology() == NECESSARILY_CLOSED) {
    PPL_ASSERT(num_columns >= 1);
    sys.set_space_dimension(num_columns - 1);
  } else {
    PPL_ASSERT(num_columns >= 2);
    sys.set_space_dimension(num_columns - 2);
  }

  set_sorted(false);

  for (dimension_type i = 0; i < num_rows; ++i) {
    Grid_Generator tmp;
    if (!tmp.ascii_load(s))
      return false;
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
  PPL_ASSERT(sys.num_columns() > 0);
  dimension_type col = sys.num_columns() - 1;

  set_space_dimension(space_dimension() + dims);

  // Add the new rows and set their diagonal element.
  for (dimension_type i = 0; i < dims; ++i) {
    Grid_Generator tmp(sys.num_columns(),
                       Linear_Row::Flags(NECESSARILY_CLOSED,
                                         Linear_Row::LINE_OR_EQUALITY));
    tmp[col] = 1;
    ++col;
    sys.insert_recycled(tmp);
  }
}

void
PPL::Grid_Generator_System
::remove_space_dimensions(const Variables_Set& vars) {
  sys.remove_space_dimensions(vars);
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
  
  // Note that the num_rows() value is *not* constant because remove_row()
  // decreases it.
  for (dimension_type i = 0; i < num_rows(); ) {
    const Grid_Generator& g = (*this)[i];
    if (g.is_line_or_parameter() && g.all_homogeneous_terms_are_zero())
      sys.remove_row(i, false);
    else
      ++i;
  }
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
