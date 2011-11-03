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
  Coefficient_traits::const_reference pivot_column = pivot.expression().get(column);
  Coefficient_traits::const_reference row_column = row.expression().get(column);
  
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);
  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.
  neg_assign(reduced_row_col);
  // pivot.space_dimension() is the index for the parameter divisor so we
  // start reducing the line at index pivot.space_dimension() - 2.
  row.expression().linear_combine(pivot.expression(), reduced_pivot_col, reduced_row_col,
                                  column, pivot.expression().space_dimension());
  PPL_ASSERT(row.expression().get(column) == 0);
}

void
Grid::reduce_equality_with_equality(Congruence& row,
				    const Congruence& pivot,
				    const dimension_type column) {
  // Assume two equalities.
  PPL_ASSERT(row.modulus() == 0 && pivot.modulus() == 0);

  Coefficient_traits::const_reference pivot_column = pivot.expression().get(column);
  Coefficient_traits::const_reference row_column = row.expression().get(column);
  
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  // Use reduced_row_col temporarily to hold the gcd.
  gcd_assign(reduced_row_col, pivot_column, row_column);
  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, reduced_row_col);
  exact_div_assign(reduced_row_col, row_column, reduced_row_col);
  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.
  neg_assign(reduced_row_col);
  row.expression().linear_combine(pivot.expression(), reduced_pivot_col, reduced_row_col,
                                  0, column + 1);
  PPL_ASSERT(row.expression().get(column) == 0);
}

template <typename R>
void
Grid::reduce_pc_with_pc(R& row, R& pivot,
			const dimension_type column,
			const dimension_type start,
			const dimension_type end) {
  PPL_ASSERT(start <= end);
  PPL_ASSERT(start <= column);
  PPL_ASSERT(column < end);

  Linear_Expression& row_e = row.expression();
  Linear_Expression& pivot_e = pivot.expression();
  
  Coefficient_traits::const_reference pivot_column = pivot_e.get(column);
  Coefficient_traits::const_reference row_column = row_e.get(column);

  PPL_DIRTY_TEMP_COEFFICIENT(s);
  PPL_DIRTY_TEMP_COEFFICIENT(t);
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_row_col);
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  gcdext_assign(gcd, s, t, pivot_column, row_column);
  PPL_ASSERT(pivot_e.get(column) * s + row_e.get(column) * t == gcd);

  // Store the reduced ratio between pivot[column] and row[column].
  PPL_DIRTY_TEMP_COEFFICIENT(reduced_pivot_col);
  exact_div_assign(reduced_pivot_col, pivot_column, gcd);
  exact_div_assign(reduced_row_col, row_column, gcd);

  // Multiply row, then subtract from it a multiple of pivot such that
  // the result in row[column] is zero.  Afterward, multiply pivot,
  // then add to it a (possibly negative) multiple of row such that
  // the result in pivot[column] is the smallest possible positive
  // integer.
  Linear_Expression old_pivot_e = pivot_e;
  pivot_e.linear_combine_lax(row_e, s, t, start, end);
  PPL_ASSERT(pivot_e.get(column) == gcd);
  row_e.linear_combine(old_pivot_e, reduced_pivot_col, -reduced_row_col, start, end);
  PPL_ASSERT(row_e.get(column) == 0);
}

void
Grid::reduce_parameter_with_line(Grid_Generator& row,
				 const Grid_Generator& pivot,
				 const dimension_type column,
				 Swapping_Vector<Grid_Generator>& rows,
                                 const dimension_type total_num_columns) {
  // Very similar to reduce_congruence_with_equality below.  Any
  // change here may be needed there too.

  Coefficient_traits::const_reference pivot_column = pivot.expression().get(column);
  Coefficient_traits::const_reference row_column = row.expression().get(column);

  // Subtract one to allow for the parameter divisor column
  const dimension_type num_columns = total_num_columns - 1;

  // If the elements at column in row and pivot are the same, then
  // just subtract pivot from row.
  if (row_column == pivot_column) {
    row.expression().linear_combine(pivot.expression(), 1, -1, 0, num_columns);
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
    Grid_Generator& gen = rows[index];
    if (gen.is_parameter_or_point()) {
      gen.expression() *= reduced_pivot_col;
      Coefficient& divisor = gen.expression()[num_columns];
      exact_div_assign(divisor, divisor, reduced_pivot_col);
    }
  }

  // Subtract from row a multiple of pivot such that the result in
  // row[column] is zero.
  row.expression().linear_combine(pivot.expression(), 1, -reduced_row_col,
                                  column, num_columns);
  PPL_ASSERT(row.expression().get(column) == 0);
}

void
Grid::reduce_congruence_with_equality(Congruence& row,
				      const Congruence& pivot,
				      const dimension_type column,
				      Swapping_Vector<Congruence>& sys) {
  // Very similar to reduce_parameter_with_line above.  Any change
  // here may be needed there too.
  PPL_ASSERT(row.modulus() > 0 && pivot.modulus() == 0);

  Coefficient_traits::const_reference pivot_column = pivot.expression().get(column);
  Coefficient_traits::const_reference row_column = row.expression().get(column);

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
  PPL_ASSERT(row.expression().get(column) == 0);
}

#ifndef NDEBUG
template <typename M, typename R>
bool
Grid::rows_are_zero(M& system, dimension_type first,
		    dimension_type last, dimension_type row_size) {
  while (first <= last) {
    const R& row = system[first++];
    for (dimension_type col = 0; col < row_size; ++col)
      if (row.expression()[col] != 0)
	return false;
  }
  return true;
}
#endif

void
Grid::simplify(Grid_Generator_System& sys, Dimension_Kinds& dim_kinds) {
  PPL_ASSERT(!sys.has_no_rows());

  // Changes here may also be required in the congruence version
  // below.

  // Subtract one to allow for the parameter divisor column
  const dimension_type num_columns = sys.space_dimension() + 1;

  if (dim_kinds.size() != num_columns)
    dim_kinds.resize(num_columns);

  const dimension_type num_rows = sys.num_rows();

  Swapping_Vector<Grid_Generator> rows;
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
    while (row_index < num_rows && rows[row_index].expression().get(dim) == 0)
      ++row_index;

    if (row_index == num_rows)
      // Element in column `dim' is zero in all rows from the pivot.
      dim_kinds[dim] = GEN_VIRTUAL;
    else {
      if (row_index != pivot_index)
        std::swap(rows[row_index], rows[pivot_index]);
      Grid_Generator& pivot = rows[pivot_index];
      bool pivot_is_line = pivot.is_line();

      // Change the matrix so that the value at `dim' in every row
      // following `pivot_index' is 0, leaving an equivalent grid.
      while (row_index < num_rows - 1) {
	++row_index;

        Grid_Generator& row = rows[row_index];

	if (row.expression().get(dim) == 0)
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
	    reduce_pc_with_pc(row, pivot, dim, dim, num_columns);
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
      if (pivot.expression().get(dim) < 0)
        pivot.negate(dim, num_columns);

      // Factor this row out of the preceding rows.
      reduce_reduced<Grid_Generator_System>
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
       sys.space_dimension() + 1);
    sys.release_rows(rows);
    PPL_ASSERT(ret == true);
#endif
    rows.resize(pivot_index);
  }

  // Ensure that the parameter divisors are the same as the system
  // divisor.
  const Coefficient& system_divisor = rows[0].expression().get(0);
  for (dimension_type i = rows.size() - 1, dim = num_columns - 1;
       dim > 0; --dim) {
    switch (dim_kinds[dim]) {
    case PARAMETER:
      rows[i].set_divisor(system_divisor);
    case LINE:
      --i;
    case GEN_VIRTUAL:
      break;
    }
  }

  sys.take_ownership_of_rows(rows);
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
    while (row_index < num_rows && sys[row_index].expression().get(dim) == 0)
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

	if (row.expression().get(dim) == 0)
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
	    reduce_pc_with_pc(row, pivot, dim, 0, dim + 1);
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
      if (pivot.expression().get(dim) < 0)
	pivot.negate(0, dim + 1);

      // Factor this row out of the preceding ones.
      reduce_reduced<Congruence_System>
	(rows, dim, pivot_index, 0, dim, dim_kinds, false);

      sys.take_ownership_of_rows(rows);

      ++pivot_index;
    }
  } // end for (dimension_type dim = num_columns; dim-- > 0; )

  if (pivot_index > 0) {
    // If the last row is false then make it the equality 1 = 0, and
    // make it the only row.

#ifndef NDEBUG
    {
      const bool ret = rows_are_zero<Congruence_System, Congruence>
        (sys,
         // index of first
         pivot_index,
         // index of last
         num_rows - 1,
         // row size
         num_columns);
      PPL_ASSERT(ret == true);
    }
#endif
    sys.remove_trailing_rows(num_rows - pivot_index);

    Congruence last_row;
    sys.release_row(last_row);

    switch (dim_kinds[0]) {

    case PROPER_CONGRUENCE:
      if (last_row.inhomogeneous_term() % last_row.modulus() == 0)
        break;
      
      // The last row is a false proper congruence.
      last_row.set_is_equality();
      dim_kinds[0] = EQUALITY;

      // No break!

    case EQUALITY:
      // The last row is a false equality, as all the coefficient terms
      // are zero while the inhomogeneous term (as a result of the
      // reduced form) is some other value.
      last_row.set_inhomogeneous_term(Coefficient_one());
      dim_kinds.resize(1);
      sys.remove_trailing_rows(sys.num_rows());
      sys.insert_verbatim_recycled(last_row);

      PPL_ASSERT(sys.OK());
      return true;

    default:
      break;
    }

    sys.insert_verbatim_recycled(last_row);
  }
  else {
    // Either sys is empty (it defines the universe) or every column
    // before the modulus column contains only zeroes.

    if (num_rows > 0) {
#ifndef NDEBUG
      const bool ret = rows_are_zero<Congruence_System, Congruence>
        (sys,
         // index of first
         0,
         // index of last
         num_rows - 1,
         // row size
         num_columns);
      PPL_ASSERT(ret == true);
#endif
      // Ensure that a single row will remain for the integrality congruence.
      sys.remove_trailing_rows(num_rows - 1);
    }

    // Set up the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    if (num_rows == 0) {
      Congruence cg(sys.space_dimension());
      cg.set_modulus(Coefficient_one());
      cg.set_inhomogeneous_term(Coefficient_one());
      sys.insert_verbatim_recycled(cg);

      PPL_ASSERT(sys.OK());
      return false;
    }

    Congruence cg;
    sys.release_row(cg);
    PPL_ASSERT(sys.num_rows() == 0);
    cg.set_modulus(Coefficient_one());
    sys.insert_verbatim_recycled(cg);
  }

  // Ensure that the last row is the integrality congruence.
  if (dim_kinds[0] == CON_VIRTUAL) {
    // The last row is virtual, append the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    Congruence new_last_row(sys.space_dimension());
    new_last_row.set_modulus(Coefficient_one());
    // Try use an existing modulus.
    dimension_type row_index = sys.num_rows();
    while (row_index-- > 0) {
      const Congruence& row = sys[row_index];
      if (row.modulus() > 0) {
	new_last_row.set_modulus(row.modulus());
	break;
      }
    }
    new_last_row.set_inhomogeneous_term(new_last_row.modulus());
    sys.insert_verbatim_recycled(new_last_row);
  }
  else {
    Congruence last_row;
    sys.release_row(last_row);
    last_row.set_inhomogeneous_term(last_row.modulus());
    sys.insert_verbatim_recycled(last_row);
  }

  Swapping_Vector<Congruence> rows;
  sys.release_rows(rows);

  // Since we are reducing the system to "strong minimal form",
  // factor the modified integrality congruence out of the other rows;
  reduce_reduced<Congruence_System>
    (rows, 0, rows.size() - 1, 0, 0, dim_kinds, false);

  sys.take_ownership_of_rows(rows);

  PPL_ASSERT(sys.OK());
  return false;
}

} // namespace Parma_Polyhedra_Library
