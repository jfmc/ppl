/* Grid class implementation: simplify().
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "Grid.defs.hh"
#include "Linear_System.defs.hh"
#include "Saturation_Row.defs.hh"
#include "Saturation_Matrix.defs.hh"

// FIX Temporary tracing stream.
#if 1
#include <iostream>
std::ostream& strace = std::cout;
#define strace_dump(sys) sys.ascii_dump(strace)
#else
#include <fstream>
std::ofstream strace;
#define strace_dump(sys)
#endif

namespace Parma_Polyhedra_Library {


inline void
Grid::reduce_line_with_line(Row& pivot, Row& row,
			    dimension_type column) {
  strace << "reduce_line_with_line" << std::endl;
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  Coefficient_traits::const_reference a_p = pivot[column] / gcd;
  Coefficient_traits::const_reference a_r = row[column] / gcd;
  /* Adjust the elements of row.  When `col' is the same as `column'
     then a_r * pivot[col] equals a_p * row[col] (this results from
     a_r and a_p being multiples of the gcd), which means that
     row[col] is set to zero.  All elements that were originally
     zero remain zero, and the rest change.  The equivalence of the
  FIX ~ pivot_0 (coef) pivot_i (inh)
     adjusted matrix can be seen as follows, where ck_n and cj_n are
     coefficients and i_k and i_j are the inhomogeneous terms.

  FIX suddenly parameters and congruences are equations?
  FIX and they're treated equivalently

     1 The equation row is multiplied throughout by a_p, producing

         a_p * ck_0 * x_0  +  a_p * ck_1 * x_1  ... +  a_p * i_k  =  0

     2 The equation pivot is multiplied throughout by a_r and then
       substituted (for the zero) into the equation produced in (1),
       producing

         a_p * ck_0 * x_0  +  a_p * ck_1 * x_1  ... +  a_p * i_k
	     =  a_r * cj_0 * x_0  +  a_r * cj_1 * x_1  ... +  a_r * i_j

       which simplifies to an equation having the new values required
       in each element of pivot:

         a_r * cj_0 * x_0  -  a_p * ck_0 * x_0  +  a_r * cj_1 * x_1  -  a_p * ck_1 * x_1
	     ... +  a_r * i_j  -  a_p * i_k  =  0

         (a_r * cj_0  -  a_p * ck_0) * x_0  +  (a_r * cj_1  -  a_p * ck_1) * x_1 ...
	     +  (a_r * i_j - a_p * i_k)  =  0
  */
  for (dimension_type col = 0; col < pivot.size(); ++col)
    row[col] = (a_r * pivot[col]) - (a_p * row[col]);
}

inline void
Grid::reduce_equality_with_equality(Row& pivot, Row& row,
					 dimension_type column) {
  strace << "reduce_equality_with_equality" << std::endl;
  assert(pivot[0] == 0 && row[0] == 0); // Assume two equalities.
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  Coefficient_traits::const_reference a_p = pivot[column] / gcd;
  Coefficient_traits::const_reference a_r = row[column] / gcd;
  // Adjust the elements of row, as in reduce_line_with_line.
  for (dimension_type col = 0; col < pivot.size() - 1; ++col)
    row[col] = (a_r * pivot[col]) - (a_p * row[col]);
}

void
Grid::reduce_pc_with_pc(Row& row, Row& pivot,
			     dimension_type column,
			     bool parameters){
  strace << "reduce_pc_with_pc" << std::endl;
  // Assume moduli are equal.
  assert(parameters || row[pivot.size()-1] == pivot[pivot.size()-1]);
  TEMP_INTEGER(gcd);
  TEMP_INTEGER(s);
  TEMP_INTEGER(t);
  gcdext_assign(gcd, pivot[column], row[column], s, t);
  // Now pivot[column] * s + row[column] * t == gcd.
  TEMP_INTEGER(a_pivot);
  TEMP_INTEGER(a_row);
  a_pivot = pivot[column] / gcd;
  a_row = row[column] / gcd;
  // Adjust the elements of row, as in reduce_line_with_line above.
  for (dimension_type col = 0;
       col < pivot.size() - (parameters == false) /* modulus */;
       ++col) {
    TEMP_INTEGER(pivot_col);
    TEMP_INTEGER(row_col);
    pivot_col = pivot[col];
    row_col = row[col];
    /* FIX Is this to lower pivot[col] to the value of gcd, and hence
       lower the sizes of the elements?  Could it be done in
       reduce_line_with_line?  (confirm whether) NB for reducing all rows
       with only constant terms to the integrality congruence.

       The equivalence of the adjusted pivot can be seen as with row
       in reduce_line_with_line above, only with a t-multiplied copy of
       row being negated and then substituted into an s-multiplied
       pivot.  */
    pivot[col] = (s * pivot_col) + (t * row_col);
    row[col] = (a_pivot * row_col) - (a_row * pivot_col);
  }
}

void
Grid::reduce_line_with_parameter(Linear_Row& row,
				      Linear_Row& pivot,
				      dimension_type column,
				      Linear_System& sys) {
  // Very similar to the the Congruence version below.  Any change
  // here may be needed there too.
  strace << "reduce_line_with_parameter" << std::endl;
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(a_pivot);
  TEMP_INTEGER(a_row);
  a_pivot = pivot[column] / gcd;
  a_row = row[column] / gcd;
  dimension_type num_cols = sys.num_columns();
  // FIX why?  (pivot is a line, row is a param)
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Linear_Row& row = sys[index];
    if (row.is_ray_or_point_or_inequality())
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= a_pivot;
  }
  /* These are like the adjustments in reduce_line_with_line (row[col]
     has been multiplied by a_pivot already, in the loop above).  */
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= a_row * pivot[col];
}

void
Grid::reduce_equality_with_congruence(Congruence& row,
					   Congruence& pivot,
					   dimension_type column,
					   Congruence_System& sys) {
  // Very similar to the the Linear_Row version above.  Any change
  // here may be needed there too.
  strace << "reduce_equality_with_congruence" << std::endl;
  assert(pivot[pivot.size()-1] == row[pivot.size()-1]);

  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(a_pivot);
  TEMP_INTEGER(a_row);
  a_pivot = pivot[column] / gcd;
  a_row = row[column] / gcd;
  dimension_type num_cols = sys.num_columns() - 1 /* modulus */;
  // FIX factor up every congruence, leaving equalities alone?
  // FIX why?
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Congruence& row = sys[index];
    if (row.is_virtual() == false && row.is_equality() == false)
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= a_pivot;
  }
  /* These are like the adjustments in reduce_line_with_line (row[col]
     has been multiplied by a_pivot already, in the loop above).  */
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= a_row * pivot[col];
}

//! Check for trailing rows containing only zero terms.
/*!
  Return <code>true</code> if all columns contain zero terms in all
  the rows of \p grid that follow row number \p first, else return
  false.  \p row_size gives the number of columns in each row.  Used
  in assertion below.
*/
bool
trailing_rows_are_zero (Matrix& system,
			dimension_type first, dimension_type row_size) {
  dimension_type num_rows = system.num_rows();
  while (first < num_rows) {
    Row& row = system[first++];
    for (dimension_type col = 0; col < row_size; ++col)
      if (row[col] != 0)
	return false;
  }
  return true;
}

bool
Grid::simplify(Generator_System& sys, Saturation_Matrix& sat) {
  strace << "==== simplify (reduce) gs:" << std::endl;
  strace << "sys:" << std::endl;
  strace_dump(sys);

  // FIX use generator iterators?

  // Changes here may also be required in the congruence version
  // below.

  // FIX at least to please assertion in linear_row::is_sorted
  sys.set_sorted(false);

  // For each column col find or construct a row (pivot) in which the
  // value at position `col' is non-zero.
  for (dimension_type col = 0; col < sys.row_size; ++col) {
    strace << "col " << col << std::endl;
    dimension_type num_rows = sys.num_rows();
    strace << "  num_rows " << num_rows << std::endl;
    // Start at the diagonal (col, col).
    dimension_type row_index = col;
    strace << "  row_index " << row_index << std::endl;
    // Move over rows which have zero in column col.
    while (row_index < num_rows && sys[row_index][col] == 0)
      ++row_index;
    if (row_index >= num_rows) {
      // Element col is zero in all rows from the col'th, so create a
      // row at index col with diagonal value 1.
      if (col)
	strace << "  Adding virtual row" << std::endl;
      else
	strace << "  Adding origin" << std::endl;

      sys.add_zero_rows(1, Linear_Row::Flags(NECESSARILY_CLOSED,
					     Linear_Row::LINE_OR_EQUALITY));
      Linear_Row& new_row = sys[num_rows];
      new_row[col] = 1;
      if (col)
	new_row.set_is_virtual();
      else
	new_row.set_is_ray_or_point_or_inequality();
      std::swap(new_row, sys[col]);
    }
    else {
      Linear_Row& pivot = sys[row_index];
      dimension_type pivot_num = row_index;
      // For each row having a value other than 0 at col, change the
      // matrix so that the value at col is 0 (leaving the grid itself
      // equivalent).
      strace << "Reducing all subsequent rows" << std::endl;
      ++row_index;
      while (row_index < num_rows) {
	if (sys[row_index][col] == 0) {
	  ++row_index;
	  continue;
	}

	Linear_Row& row = sys[row_index];
	if (row.is_virtual()) {

#define free_row()							\
	  std::swap(row, sys[--num_rows]);				\
	  sys.Matrix::resize_no_copy(num_rows, sys.num_columns(), Row::Flags()); \
	  continue;		/* Skip ++row_index.  */

	  // Free the virtual row from sys.
	  free_row();
	}
	else if (row.is_line_or_equality()) {
	  if (pivot.is_virtual()) {
	    // Keep the row as the new pivot.
	    std::swap(row, pivot);

	    // Free the old, virtual, pivot from sys.
	    free_row();
	  }
	  else if (pivot.is_line_or_equality())
	    reduce_line_with_line(row, pivot, col);
	  else if (pivot.is_ray_or_point_or_inequality()) {
	    std::swap(row, pivot);
	    reduce_line_with_parameter(row, pivot, col, sys);
	  }
	  else
	    throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (1).");
	}
	else if (row.is_ray_or_point_or_inequality()) {
	  if (pivot.is_virtual()) {
	    // Keep the row as the new pivot.
	    std::swap(row, pivot);
	    // Free the virtual pivot from sys.
	    free_row();
	  }
	  else if (pivot.is_line_or_equality())
	    reduce_line_with_parameter(row, pivot, col, sys);
	  else if (pivot.is_ray_or_point_or_inequality())
	    reduce_pc_with_pc(row, pivot, col);
	  else
	    throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (2).");
	}
	else
	  throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (3).");
	++row_index;
      }
      if (col != pivot_num) {
	strace << "swapping" << std::endl;
	std::swap(sys[col], pivot);
      }
    }
    strace_dump(sys);
  }
  assert(sys.num_rows() >= sys.num_columns());
  /* Clip any zero rows from the end of the matrix.  */
  if (sys.num_rows() > sys.num_columns()) {
    strace << "clipping trailing" << std::endl;
    assert(trailing_rows_are_zero(sys,
				  sys.num_columns(),
				  sys.num_columns()));
    sys.erase_to_end(sys.num_columns());
  }

  sys.set_index_first_pending_row(sys.num_rows());

  // FIX is this psbl after parameterizing?
  //assert(sys.OK());

  sys.set_sorted(false);

  // Only consistent grids exist.
  if (sys[0].is_ray_or_point() == false) {
    strace << "---- simplify (reduce) gs done (empty)." << std::endl;
    return true;
  }

  strace << "---- simplify (reduce) gs done." << std::endl;
  return false;
}

bool
Grid::simplify(Congruence_System& sys, Saturation_Matrix& sat) {
  strace << "======== simplify (reduce) cgs:" << std::endl;
  assert(sys.num_rows());

  // Changes here may also be required in the generator version above.

  // For each column col find or construct a row (pivot) in which the
  // value at position `col' is non-zero.
  dimension_type col = 0;
  while (col < sys.num_columns() - 1 /* modulus */) {
    strace << "col " << col << std::endl;
    dimension_type num_rows = sys.num_rows();
    strace << "  num_rows " << num_rows << std::endl;
    // Start at the diagonal (col, col).
    dimension_type row_num = num_rows - col;
    strace << "  row_num " << row_num << std::endl;
    dimension_type orig_row_num = row_num;
    // Move backward over rows which have zero in the column that is
    // col elements from the right hand side of the matrix.
    dimension_type column = sys.num_columns() - 2 /* modulus, index */ - col;
    while (row_num > 0 && sys[row_num-1][column] == 0)
      strace << ".", --row_num;
    strace << std::endl;
    if (row_num == 0) {
      // Element col is zero in all rows from the col'th, so create a
      // virtual row at index col with diagonal value 1.
      if (orig_row_num)
	strace << "  Adding virtual row" << std::endl;
      else
	strace << "  Adding integrality congruence" << std::endl;
      Row new_row(sys.num_columns(), sys.row_capacity, Row::Flags());
      if (orig_row_num) {
	new_row[column] = 1;
	(static_cast<Congruence&>(new_row)).set_is_virtual();
      }
      else {
	assert(sys.num_columns());
	TEMP_INTEGER(modulus);
	modulus = 1;
	// Try find an existing modulus.
	for (dimension_type r = 0; r < num_rows; ++r) {
	  TEMP_INTEGER(mod);
	  mod = sys[r].modulus();
	  if (mod > 0) {
	    modulus = mod;
	    break;
	  }
	}
	new_row[sys.num_columns() - 1] = modulus;
	new_row[column] = modulus;
      }
      sys.rows.insert(sys.rows.begin() + orig_row_num, new_row);
    }
    else {
      dimension_type& row_index = row_num;  // For clearer naming.
      --row_index;
      dimension_type pivot_num = row_index;
      Congruence& pivot = sys[pivot_num];
      // For each row having a lower index and a value at col other
      // than 0, change the grid representation so that the value at
      // col is 0 (leaving an equivalent grid).
      strace << "  Reducing all preceding rows" << std::endl;
      while (row_index > 0) {
	--row_index;
	strace << "    row_index " << row_index << " ";
	Congruence& row = sys[row_index];
	if (row[column] != 0) {
	  if (row.is_virtual()) {
	    // Free the virtual row from sys.

#undef free_row
#define free_row()						\
	    sys.rows.erase(sys.rows.begin() + row_index);	\
	    strace << "drop" << std::endl;			\
	    --num_rows;

	    free_row();
	  }
	  else if (row.is_equality())
	    if (pivot.is_virtual()) {
	      // Keep the row as the new pivot.
	      std::swap(row, pivot);

	      // Free the old, virtual, pivot from sys.
	      free_row();
	    }
	    else if (pivot.is_equality())
	      reduce_equality_with_equality(row, pivot, column);
	    else {
	      // Pivot is a congruence.
	      std::swap(row, pivot);
	      reduce_equality_with_congruence(row, pivot, column, sys);
	    }
	  else
	    // Row is a congruence.
	    if (pivot.is_virtual()) {
	      // Keep the row as the new pivot.
	      std::swap(row, pivot);
	      // Free the virtual pivot from sys.
	      free_row();
	    }
	    else if (pivot.is_equality())
	      reduce_equality_with_congruence(row, pivot, column, sys);
	    else
	      // Pivot is a congruence.
	      reduce_pc_with_pc(row, pivot, column, false);
	}
      }
      if (orig_row_num != pivot_num) {
	strace << "swapping" << std::endl;
	std::swap(sys[orig_row_num - 1], pivot);
      }
      strace_dump(sys);
    }
    ++col;
  }
  assert(sys.num_rows() >= sys.num_columns() - 1);
  // Clip any zero rows from the end of the matrix.
  if (sys.num_rows() > sys.num_columns() - 1) {
    assert(trailing_rows_are_zero(sys,
				  sys.num_columns(),
				  sys.num_columns() - 1));
    sys.rows.erase(sys.rows.begin(),
		   sys.rows.begin()
		   + (sys.num_rows() - sys.num_columns() + 1));
  }

  assert(sys.OK());

  // Only consistent grids exist.
  TEMP_INTEGER(modulus);
  TEMP_INTEGER(it);
  modulus = sys[0].modulus();
  it = sys[0].inhomogeneous_term();
  assert(modulus >= 0);
  if (modulus > 0) {
    if (it % modulus != 0) {
      strace << "---- simplify (reduce) cgs done (empty)." << std::endl;
      return true;
    }
  }
  else if (it > 0) {
    strace << "---- simplify (reduce) cgs done (empty)." << std::endl;
    return true;
  }

  strace << "---- simplify (reduce) cgs done." << std::endl;
  return false;
}

} // namespace Parma_Polyhedra_Library
