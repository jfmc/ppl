/* Grid class implementation: simplify().
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
#include "assert.hh"

#include "Grid.defs.hh"

namespace Parma_Polyhedra_Library {

void
Grid::reduce_line_with_line(Grid_Generator& row, Grid_Generator& pivot,
			    dimension_type column) {
  const Coefficient& pivot_column = pivot[column];
  Coefficient& row_column = row[column];
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);
  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.
  row_column = 0;
  // pivot.size() - 1 is the index for the parameter divisor so we
  // start reducing the line at index pivot.size() - 2.
  for (dimension_type col = pivot.size() - 2;
       col > column;
       --col) {
    Coefficient& row_col = row[col];
    row_col *= reduced_pivot_col;
    sub_mul_assign(row_col, reduced_row_col, pivot[col]);
  }
}

void
Grid::reduce_equality_with_equality(Congruence& row,
				    const Congruence& pivot,
				    const dimension_type column) {
  // Assume two equalities.
  PPL_ASSERT(row.modulus() == 0 && pivot.modulus() == 0);

  const Coefficient& pivot_column = pivot[column];
  Coefficient& row_column = row[column];
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);
  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.
  row_column = 0;
  for (dimension_type col = column; col-- > 0; ) {
    Coefficient& row_col = row[col];
    row_col *= reduced_pivot_col;
    sub_mul_assign(row_col, reduced_row_col, pivot[col]);
  }
}

template <typename R>
void
Grid::reduce_pc_with_pc(R& row, R& pivot,
			const dimension_type column,
			const dimension_type start,
			const dimension_type end) {
  Coefficient& pivot_column = pivot[column];
  Coefficient& row_column = row[column];

  PPL_DIRTY_TEMP_COEFFICIENT(s);
  PPL_DIRTY_TEMP_COEFFICIENT(t);
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcdext_assign(reduced_row_col, s, t, pivot_column, row_column);
  // Now pivot[column] * s + row[column] * t == gcd.

  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  pivot_column = reduced_row_col /* gcd */;
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);

  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.  Afterward, multiply pivot,
  // then add to it a (possibly negative) multiple of row such that
  // the result in pivot[column] is the smallest possible positive
  // integer.
  PPL_ASSERT(pivot.size() > 0);
  PPL_ASSERT(row.size() > 0);
  row_column = 0;
  PPL_DIRTY_TEMP_COEFFICIENT(old_pivot_col);
  for (dimension_type col = start; col < end; ++col) {
    Coefficient& pivot_col = pivot[col];
    old_pivot_col = pivot_col;
    pivot_col *= s;
    Coefficient& row_col = row[col];
    add_mul_assign(pivot_col, t, row_col);
    row_col *= reduced_pivot_col;
    sub_mul_assign(row_col, reduced_row_col, old_pivot_col);
  }
}

void
Grid::reduce_parameter_with_line(Grid_Generator& row,
				 const Grid_Generator& pivot,
				 const dimension_type column,
				 Swapping_Vector<Linear_Row>& rows,
                                 const dimension_type total_num_columns) {
  // Very similar to reduce_congruence_with_equality below.  Any
  // change here may be needed there too.

  const Coefficient& pivot_column = pivot[column];
  Coefficient& row_column = row[column];

  // Subtract one to allow for the parameter divisor column
  const dimension_type num_columns = total_num_columns - 1;

  // If the elements at column in row and pivot are the same, then
  // just subtract pivot from row.
  if (row_column == pivot_column) {
    for (dimension_type col = num_columns; col-- > 0; )
      row[col] -= pivot[col];
    return;
  }

  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);


  // Since we are reducing the system to "strong minimal form",
  // ensure that the multiplier is positive, so that the preceding
  // diagonals (including the divisor) remain positive.  It's safe to
  // swap the signs as row[column] will still come out 0.
  if (reduced_pivot_col < 0) {
    neg_assign(reduced_pivot_col);
    neg_assign(reduced_row_col);
  }

  // Multiply row such that a multiple of pivot can be subtracted from
  // it below to render row[column] zero.  This requires multiplying
  // all other parameters to match.
  for (dimension_type index = rows.size(); index-- > 0; ) {
    Linear_Row& row = rows[index];
    Grid_Generator& gen = static_cast<Grid_Generator&>(row);
    if (gen.is_parameter_or_point())
      for (dimension_type col = num_columns; col-- > 0; )
        gen[col] *= reduced_pivot_col;
  }

  // Subtract from row a multiple of pivot such that the result in
  // row[column] is zero.
  row_column = 0;
  for (dimension_type col = num_columns - 1; col > column; --col)
    sub_mul_assign(row[col], reduced_row_col, pivot[col]);
}

void
Grid::reduce_congruence_with_equality(Congruence& row,
				      const Congruence& pivot,
				      const dimension_type column,
				      Swapping_Vector<Congruence>& sys) {
  // Very similar to reduce_parameter_with_line above.  Any change
  // here may be needed there too.
  PPL_ASSERT(row.modulus() > 0 && pivot.modulus() == 0);

  const Coefficient& pivot_column = pivot[column];
  Coefficient& row_column = row[column];

  // If the elements at `column' in row and pivot are the same, then
  // just subtract `pivot' from `row'.
  if (row_column == pivot_column) {
    row -= pivot;
    return;
  }

  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);
  // Ensure that `reduced_pivot_col' is positive, so that the modulus
  // remains positive when multiplying the proper congruences below.
  // It's safe to swap the signs as row[column] will still come out 0.
  if (reduced_pivot_col < 0) {
    neg_assign(reduced_pivot_col);
    neg_assign(reduced_row_col);
  }
  // Multiply `row', including the modulus, by reduced_pivot_col.  To
  // keep all the moduli the same this requires multiplying all the
  // other proper congruences in the same way.
  for (dimension_type index = sys.size(); index-- > 0; ) {
    Congruence& cg = sys[index];
    if (cg.is_proper_congruence())
      cg.scale(reduced_pivot_col);
  }
  // Subtract from row a multiple of pivot such that the result in
  // row[column] is zero.
  sub_mul_assign(row, reduced_row_col, pivot);
  PPL_ASSERT(row[column] == 0);
}

#ifndef NDEBUG
template <typename M, typename R>
bool
Grid::rows_are_zero(M& system, dimension_type first,
		    dimension_type last, dimension_type row_size) {
  while (first <= last) {
    const R& row = system[first++];
    for (dimension_type col = 0; col < row_size; ++col)
      if (row[col] != 0)
	return false;
  }
  return true;
}
#endif

void
Grid::simplify(Grid_Generator_System& sys, Dimension_Kinds& dim_kinds) {
  PPL_ASSERT(!sys.has_no_rows());
  // For reduce_pc_with_pc.
  PPL_ASSERT(sys.num_columns() > 0);

  // Changes here may also be required in the congruence version
  // below.

  // Subtract one to allow for the parameter divisor column
  const dimension_type num_columns = sys.num_columns() - 1;

  if (dim_kinds.size() != num_columns)
    dim_kinds.resize(num_columns);

  const dimension_type num_rows = sys.num_rows();

  Swapping_Vector<Linear_Row> rows;
  // Release the rows from the linear system, so they can be modified.
  sys.release_rows(rows);

  // For each dimension `dim' move or construct a row into position
  // `pivot_index' such that the row has zero in all elements
  // following column `dim' and a value other than zero in column
  // `dim'.
  dimension_type pivot_index = 0;
  for (dimension_type dim = 0; dim < num_columns; ++dim) {
    // Consider the pivot and following rows.
    dimension_type row_index = pivot_index;

    // Move down over rows which have zero in column `dim'.
    while (row_index < num_rows && rows[row_index][dim] == 0)
      ++row_index;

    if (row_index == num_rows)
      // Element in column `dim' is zero in all rows from the pivot.
      dim_kinds[dim] = GEN_VIRTUAL;
    else {
      if (row_index != pivot_index)
        std::swap(rows[row_index], rows[pivot_index]);
      Linear_Row& pivot_row = rows[pivot_index];
      Grid_Generator& pivot = static_cast<Grid_Generator&>(pivot_row);
      bool pivot_is_line = pivot.is_line();

      // Change the matrix so that the value at `dim' in every row
      // following `pivot_index' is 0, leaving an equivalent grid.
      while (row_index < num_rows - 1) {
	++row_index;

        Linear_Row& lr = rows[row_index];
        Grid_Generator& row = static_cast<Grid_Generator&>(lr);

	if (row[dim] == 0)
	  continue;

	if (row.is_line())
	  if (pivot_is_line)
	    reduce_line_with_line(row, pivot, dim);
	  else {
	    PPL_ASSERT(pivot.is_parameter_or_point());
	    std::swap(row, pivot);
	    pivot_is_line = true;
	    reduce_parameter_with_line(row, pivot, dim, rows, num_columns + 1);
	  }
	else {
	  PPL_ASSERT(row.is_parameter_or_point());
	  if (pivot_is_line)
	    reduce_parameter_with_line(row, pivot, dim, rows, num_columns + 1);
	  else {
	    PPL_ASSERT(pivot.is_parameter_or_point());
	    reduce_pc_with_pc(row, pivot, dim, dim + 1, num_columns);
	  }
	}
      }

      if (pivot_is_line)
	dim_kinds[dim] = LINE;
      else {
	PPL_ASSERT(pivot.is_parameter_or_point());
	dim_kinds[dim] = PARAMETER;
      }

      // Since we are reducing the system to "strong minimal form",
      // ensure that a positive value follows the leading zeros.
      if (pivot[dim] < 0)
        pivot.negate(dim, num_columns);

      // Factor this row out of the preceding rows.
      reduce_reduced<Grid_Generator_System, Grid_Generator>
	(rows, dim, pivot_index, dim, num_columns - 1, dim_kinds);

      ++pivot_index;
    }
  }

  // Clip any zero rows from the end of the matrix.
  if (num_rows > pivot_index) {
#ifndef NDEBUG
    sys.take_ownership_of_rows(rows);
    const bool ret = rows_are_zero<Grid_Generator_System, Grid_Generator>
      (sys,
       // index of first
       pivot_index,
       // index of last
       sys.num_rows() - 1,
       // row size
       sys.num_columns() - 1);
    sys.release_rows(rows);
    PPL_ASSERT(ret == true);
#endif
    rows.resize(pivot_index);
  }

  // Ensure that the parameter divisors are the same as the system
  // divisor.
  const Coefficient& system_divisor = rows[0][0];
  for (dimension_type i = rows.size() - 1, dim = num_columns - 1;
       dim > 0; --dim) {
    Linear_Row& row = rows[i];
    Grid_Generator& g = static_cast<Grid_Generator&>(row);

    switch (dim_kinds[dim]) {
    case PARAMETER:
      g.set_divisor(system_divisor);
    case LINE:
      --i;
    case GEN_VIRTUAL:
      break;
    }
  }

  sys.take_ownership_of_rows(rows);

  PPL_ASSERT(sys.OK());
}

bool
Grid::simplify(Congruence_System& sys, Dimension_Kinds& dim_kinds) {
  PPL_ASSERT(sys.num_columns() > 2);

  // Changes here may also be required in the generator version above.

  // TODO: Consider normalizing the moduli only when congruences are
  //       added to con_sys.
  sys.normalize_moduli();

  const dimension_type num_columns = sys.num_columns() - 1 /* modulus */;

  if (dim_kinds.size() != num_columns)
    dim_kinds.resize(num_columns);

  const dimension_type num_rows = sys.num_rows();

  // For each dimension `dim' move or construct a row into position
  // `pivot_index' such that the row has a value of zero in all
  // elements preceding column `dim' and some other value in column
  // `dim'.
  dimension_type pivot_index = 0;
  for (dimension_type dim = num_columns; dim-- > 0; ) {
    // Consider the pivot and following rows.
    dimension_type row_index = pivot_index;

    // Move down over rows which have zero in column `dim'.
    while (row_index < num_rows && sys[row_index][dim] == 0)
      ++row_index;

    if (row_index == num_rows)
      // Element in column `dim' is zero in all rows from the pivot,
      // or `sys' is empty of rows.
      dim_kinds[dim] = CON_VIRTUAL;
    else {

      Swapping_Vector<Congruence> rows;
      sys.release_rows(rows);

      if (row_index != pivot_index)
	std::swap(rows[row_index], rows[pivot_index]);

      Congruence& pivot = rows[pivot_index];
      bool pivot_is_equality = pivot.is_equality();

      // Change the matrix so that the value at `dim' in every row
      // following `pivot_index' is 0, leaving an equivalent grid.
      while (row_index < num_rows - 1) {
	++row_index;

	Congruence& row = rows[row_index];

	if (row[dim] == 0)
	  continue;

	if (row.is_equality())
	  if (pivot_is_equality)
	    reduce_equality_with_equality(row, pivot, dim);
	  else {
	    PPL_ASSERT(pivot.is_proper_congruence());
	    std::swap(row, pivot);
	    pivot_is_equality = true;
	    reduce_congruence_with_equality(row, pivot, dim, rows);
	  }
	else {
	  PPL_ASSERT(row.is_proper_congruence());
	  if (pivot_is_equality)
	    reduce_congruence_with_equality(row, pivot, dim, rows);
	  else {
	    PPL_ASSERT(pivot.is_proper_congruence());
	    reduce_pc_with_pc(row, pivot, dim, 0, dim);
	  }
	}
      }

      if (pivot_is_equality)
	dim_kinds[dim] = EQUALITY;
      else {
	PPL_ASSERT(pivot.is_proper_congruence());
	dim_kinds[dim] = PROPER_CONGRUENCE;
      }

      // Since we are reducing the system to "strong minimal form",
      // ensure that a positive value follows the leading zeros.
      if (pivot[dim] < 0)
	pivot.negate(0, dim + 1);

      // Factor this row out of the preceding ones.
      reduce_reduced<Congruence_System, Congruence>
	(rows, dim, pivot_index, 0, dim, dim_kinds, false);

      sys.take_ownership_of_rows(rows);

      ++pivot_index;
    }
  } // end for (dimension_type dim = num_columns; dim-- > 0; )

  // For clearer naming.
  dimension_type& reduced_num_rows = pivot_index;

  if (reduced_num_rows > 0) {
    // If the last row is false then make it the equality 1 = 0, and
    // make it the only row.
    Congruence& last_row = sys[reduced_num_rows - 1];
    if (dim_kinds[0] == PROPER_CONGRUENCE) {
      if (last_row.inhomogeneous_term() % last_row.modulus() != 0) {
	// The last row is a false proper congruence.
	last_row.set_is_equality();
	dim_kinds[0] = EQUALITY;
	goto return_empty;
      }
    }
    else if (dim_kinds[0] == EQUALITY) {
      // The last row is a false equality, as all the coefficient terms
      // are zero while the inhomogeneous term (as a result of the
      // reduced form) is some other value.
    return_empty:
      last_row[0] = 1;
      dim_kinds.resize(1);
      std::swap(sys[0], sys[sys.num_rows()-1]);
      sys.remove_trailing_rows(num_rows - 1);

      PPL_ASSERT(sys.OK());
      return true;
    }
  }
  else {
    // Either sys is empty (it defines the universe) or every column
    // before the modulus column contains only zeroes.

    // Set up the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    if (num_rows == 0) {
      Congruence cg(sys.space_dimension());
      cg[num_columns] = 1;
      cg[0] = 1;
      sys.insert_verbatim_recycled(cg);

      PPL_ASSERT(sys.OK());
      return false;
    }
    sys[0][num_columns] = 1;
    // Ensure that, after any zero row clipping below, a single row
    // will remain for the integrality congruence.
    reduced_num_rows = 1;
  }

  // Clip any zero rows from the end of the matrix.
  if (num_rows > 1 && num_rows > reduced_num_rows) {
#ifndef NDEBUG
    const bool ret = rows_are_zero<Congruence_System, Congruence>
      (sys,
       // index of first
       reduced_num_rows,
       // index of last
       num_rows - 1,
       // row size
       num_columns);
    PPL_ASSERT(ret == true);
#endif
    sys.remove_trailing_rows(num_rows - reduced_num_rows);
  }

  PPL_ASSERT(sys.num_rows() == reduced_num_rows);

  // Ensure that the last row is the integrality congruence.
  const dimension_type mod_index = num_columns;
  if (dim_kinds[0] == CON_VIRTUAL) {
    // The last row is virtual, append the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    Congruence new_last_row(sys.space_dimension());
    new_last_row[mod_index] = 1;
    // Try use an existing modulus.
    dimension_type row_index = reduced_num_rows;
    while (row_index-- > 0) {
      Congruence& row = sys[row_index];
      if (row[mod_index] > 0) {
	new_last_row[mod_index] = row[mod_index];
	break;
      }
    }
    new_last_row[0] = new_last_row[mod_index];
    sys.insert_verbatim_recycled(new_last_row);
    // Since we are reducing the system to "strong minimal form",
    // increment the number of reduced rows.
    ++reduced_num_rows;
  }
  else {
    Congruence& last_row = sys[reduced_num_rows - 1];
    last_row[0] = last_row[mod_index];
  }

  Swapping_Vector<Congruence> rows;
  sys.release_rows(rows);

  // Since we are reducing the system to "strong minimal form",
  // factor the modified integrality congruence out of the other rows;
  reduce_reduced<Congruence_System, Congruence>
    (rows, 0, reduced_num_rows - 1, 0, 0, dim_kinds, false);

  sys.take_ownership_of_rows(rows);

  PPL_ASSERT(sys.OK());
  return false;
}

} // namespace Parma_Polyhedra_Library
