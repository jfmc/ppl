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
#include <cassert>

#include "Grid.defs.hh"
#include "Linear_System.defs.hh"

// FIX Temporary tracing stream.
#if 0
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
Grid::reduce_line_with_line(Row& row, Row& pivot,
			    dimension_type column) {
  strace << "reduce_line_with_line" << std::endl;
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  Coefficient_traits::const_reference pa = pivot[column] / gcd;
  Coefficient_traits::const_reference ra = row[column] / gcd;
  /* Adjust the elements of row.

     ra * pivot[col] == pa * row[col], which means that in the loop
     below row[column] is set to zero.  All elements in row that were
     originally zero remain zero, and the rest are updated.

     The equivalence of the adjusted row can be seen as follows, where
     pn and rn are the pivot and row coefficients and pi and ri are
     the inhomogeneous terms of the pivot and row.

     FIX Is is OK to consider the parameters as equations like this?

     1 The parameter or congruence relation row is multiplied
       throughout by pa, producing

         pa * r0 * x0  +  pa * r1 * x1  ... +  pa * ri  ==  0

     2 The relation pivot is multiplied throughout by ra and then
       substituted for the zero in the updated row relation, resulting
       in

         pa * r0 * x0  +  pa * r1 * x1  ... +  pa * ri
	     ==  ra * p0 * x0  +  ra * p1 * x1  ... +  ra * pi

     3 The row relation is simplified, below, to show that the element
       col will be ra * pivot[col] - pa * row[col], which is zero.

         ra * p0 * x0  -  pa * r0 * x0  +  ra * p1 * x1  -  pa * r1 * x1
	     ... +  ra * pi  -  pa * ri  =  0

         (ra * p0  -  pa * r0) * x0  +  (ra * p1  -  pa * r1) * x1 ...
	     +  (ra * pi - pa * ri)  =  0
  */
  for (dimension_type col = 0; col < pivot.size(); ++col)
    row[col] = (ra * pivot[col]) - (pa * row[col]);
}

inline void
Grid::reduce_equality_with_equality(Congruence& row, Congruence& pivot,
				    dimension_type column) {
  strace << "reduce_equality_with_equality" << std::endl;
  // Assume two equalities.
  assert(row.modulus() == 0 && pivot.modulus() == 0);
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  Coefficient_traits::const_reference pa = pivot[column] / gcd;
  Coefficient_traits::const_reference ra = row[column] / gcd;
  // Adjust the elements of row, as in reduce_line_with_line.
  for (dimension_type col = 0; col < pivot.size() - 1; ++col)
    row[col] = (ra * pivot[col]) - (pa * row[col]);
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
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
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
       in reduce_line_with_line above, only with a s-multiplied copy
       of pivot being negated and then substituted into an
       t-multiplied row.  */
    pivot[col] = (s * pivot_col) + (t * row_col);
    row[col] = (pivot_a * row_col) - (row_a * pivot_col);
  }
}

void
Grid::reduce_parameter_with_line(Linear_Row& row,
				 Linear_Row& pivot,
				 dimension_type column,
				 Linear_System& sys) {
  // Very similar to the the Congruence version below.  Any change
  // here may be needed there too.
  strace << "reduce_parameter_with_line" << std::endl;
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
  dimension_type num_cols = sys.num_columns();
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Linear_Row& row = sys[index];
    if (row.is_ray_or_point_or_inequality())
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= pivot_a;
  }
  // These are like the adjustments in reduce_line_with_line (row[col]
  // has been multiplied by pivot_a already, in the loop above).
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= row_a * pivot[col];
}

void
Grid::reduce_congruence_with_equality(Congruence& row,
				      Congruence& pivot,
				      dimension_type column,
				      Congruence_System& sys) {
  // Very similar to the Linear_Row version above.  Any change here
  // may be needed there too.
  strace << "reduce_congruence_with_equality" << std::endl;
  assert(row.modulus() > 0 && pivot.modulus() == 0);

  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
  dimension_type num_cols = sys.num_columns() - 1 /* modulus */;
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Congruence& row = sys[index];
    if (row.is_proper_congruence())
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= pivot_a;
  }
  /* These are like the adjustments in reduce_line_with_line (row[col]
     has been multiplied by pivot_a already, in the loop above).  */
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= row_a * pivot[col];
}

#ifndef NDEBUG
//! Check for trailing rows containing only zero terms.
/*!
  If all columns contain zero in the rows of \p system from row index
  \p first to row \p last then return <code>true</code>, else return
  <code>false</code>.  \p row_size gives the number of columns in each
  row.  Used in assertion below.
*/
bool
rows_are_zero (Matrix& system, dimension_type first,
	       dimension_type last, dimension_type row_size) {
  while (first <= last) {
    Row& row = system[first++];
    for (dimension_type col = 0; col < row_size; ++col)
      if (row[col] != 0)
	return false;
  }
  return true;
}
#endif

bool
Grid::simplify(Generator_System& sys) {
  strace << "==== simplify (reduce) gs:" << std::endl;
  strace << "sys:" << std::endl;
  strace_dump(sys);

  // FIX use generator iterators?

  // Changes here may also be required in the congruence version
  // below.

  // FIX at least to please assertion in linear_row::is_sorted
  sys.set_sorted(false);

  // For each column `col' find or construct a row (pivot) in which
  // the value at position `col' is non-zero FIX and other zero such
  // that triangular.
  for (dimension_type col = 0; col < sys.row_size; ++col) {
    strace << "col " << col << std::endl;
    dimension_type num_rows = sys.num_rows();
    strace << "  num_rows " << num_rows << std::endl;
    // Start at the diagonal (col, col).
    dimension_type row_index = col;
    strace << "  row_index " << row_index << std::endl;
    // Move down over rows which have zero in column col.
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
      dimension_type pivot_index = row_index;
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
	    reduce_parameter_with_line(row, pivot, col, sys);
	  }
#ifndef NDEBUG
	  else
	    throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (1).");
#endif
	}
	else if (row.is_ray_or_point_or_inequality()) {
	  if (pivot.is_virtual()) {
	    // Keep the row as the new pivot.
	    std::swap(row, pivot);
	    // Free the virtual pivot from sys.
	    free_row();
	  }
	  else if (pivot.is_line_or_equality())
	    reduce_parameter_with_line(row, pivot, col, sys);
	  else if (pivot.is_ray_or_point_or_inequality())
	    reduce_pc_with_pc(row, pivot, col);
#ifndef NDEBUG
	  else
	    throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (2).");
#endif
	}
#ifndef NDEBUG
	else
	  throw std::runtime_error("PPL internal error: Grid simplify: failed to match row type (3).");
#endif
	++row_index;
      }
      if (col != pivot_index) {
	strace << "swapping" << std::endl;
	std::swap(sys[col], sys[pivot_index]);
      }
    }
    strace_dump(sys);
  }
  assert(sys.num_rows() >= sys.num_columns());
  /* Clip any zero rows from the end of the matrix.  */
  if (sys.num_rows() > sys.num_columns()) {
    strace << "clipping trailing" << std::endl;
    assert(rows_are_zero(sys,
			 sys.num_columns(),
			 sys.num_rows() - 1,
			 sys.num_columns()));
    sys.erase_to_end(sys.num_columns());
#if EXTRA_ROW_DEBUG
    // std::vector may have relocated rows, so the row copy constructor may
    // have used a new capacity.
    sys.row_capacity = sys[0].capacity_;
#endif
  }

  sys.unset_pending_rows();

  // FIX is this psbl after parameterizing?
  //assert(sys.OK());

  sys.set_sorted(false);

  // Grids are either consistent or empty.
  if (sys[0].is_ray_or_point() == false) {
    dimension_type row_size = sys.num_columns();
    // Make all rows virtual, to free space.
    // FIX is this worth the time?
    for (dimension_type row = 0; row < sys.num_rows(); ++row) {
      Generator& gen = sys[row];
      for (dimension_type col = 0; col < row_size; ++col)
	gen[col] = 0;
      gen[row] = 1;
      gen.set_is_virtual();
    }
    strace << "---- simplify (reduce) gs done (empty)." << std::endl;
    return true;
  }

  strace << "---- simplify (reduce) gs done." << std::endl;
  return false;
}

bool
Grid::simplify(Congruence_System& sys) {
  strace << "======== simplify (reduce) cgs:" << std::endl;
  strace_dump(sys);
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
      // FIX add to end and swap instead?
      sys.rows.insert(sys.rows.begin() + orig_row_num, new_row);
#if EXTRA_ROW_DEBUG
      // std::vector may have relocated rows, so the row copy constructor
      // may have used a new capacity.
      sys.row_capacity = sys[0].capacity_;
#endif
    }
    else {
      dimension_type& row_index = row_num; // For clearer naming.
      --row_index;
      dimension_type pivot_index = row_index;
      Congruence& pivot = sys[pivot_index];
      // For each row having a lower index and a value at col other
      // than 0, change the grid representation so that the value at
      // col is 0 (leaving an equivalent grid).
      strace << "  Reducing all preceding rows" << std::endl
	     << "    pivot_index " << pivot_index << std::endl;
      while (row_index > 0) {
	--row_index;
	strace << "    row_index " << row_index << std::endl;
	Congruence& row = sys[row_index];
	if (row[column] != 0) {
	  if (row.is_virtual()) {
	    // Free the virtual row from sys.

#if EXTRA_ROW_DEBUG
// std::vector may have relocated rows, so the row copy constructor
// may have used a new capacity.
#define adjust_row_capacity() sys.row_capacity = sys[0].capacity_
#else
#define adjust_row_capacity()
#endif

#undef free_row
#define free_row()						\
	    /* FIX Slow. */					\
	    sys.rows.erase(sys.rows.begin() + row_index);	\
	    adjust_row_capacity();				\
	    strace << "drop" << std::endl;			\
	    --num_rows;						\
	    --orig_row_num;					\
	    --pivot_index;

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
	      reduce_congruence_with_equality(row, pivot, column, sys);
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
	      reduce_congruence_with_equality(row, pivot, column, sys);
	    else
	      // Pivot is a congruence.
	      reduce_pc_with_pc(row, pivot, column, false);
	}
      }
      if (orig_row_num != pivot_index) {
	strace << "swapping row_num " << orig_row_num << " and pivot" << std::endl;
	std::swap(sys[orig_row_num - 1], sys[pivot_index]);
	// FIX why causes error when a virtual row has been dropped
	//std::swap(sys[orig_row_num - 1], pivot);
      }
      strace_dump(sys);
    }
    ++col;
  }

  assert(sys.num_rows() >= sys.num_columns() - 1);
  // Clip any zero rows from the front of the matrix.
  if (sys.num_rows() > sys.num_columns() - 1) {
    assert(rows_are_zero(sys,
			 0,
			 sys.num_rows() - sys.num_columns(),
			 sys.num_columns() - 1));
    sys.rows.erase(sys.rows.begin(),
		   sys.rows.begin()
		   + (sys.num_rows() - sys.num_columns() + 1));
#if EXTRA_ROW_DEBUG
    // std::vector may have relocated rows, so the row copy constructor may
    // have used a new capacity.
    sys.row_capacity = sys[0].capacity_;
    // FIX check other places that do rows.erase (Linear_System...)
#endif
  }

  assert(sys.OK());

  TEMP_INTEGER(modulus);
  Congruence& first_row = sys[0];
  modulus = first_row.modulus();
  // If first row is false then make it the equality 1 = 0.
  if (modulus > 0 && first_row.inhomogeneous_term() % modulus != 0) {
    // The first row is a false congruence.
    first_row[0] = 1;
    first_row.set_is_equality();
    strace << "---- simplify (reduce) cgs done (empty)." << std::endl;
    return true;
  }
  if (modulus == 0) {
    // The first row is a false equality, as all the coefficient terms
    // are zero while the inhomogeneous term holds a value (as a
    // result of the reduced form).
    first_row[0] = 1;
    strace << "---- simplify (reduce) cgs done (empty)." << std::endl;
    return true;
  }
  // Ensure that the first row is the integrality congruence.
  dimension_type last = first_row.size() - 1;
  if (modulus == -1) {
    // The first row is virtual, make it the integrality congruence.
    first_row[last] = 1;
    // Try use an existing modulus.
    dimension_type row = sys.num_rows();
    while (row-- > 1)
      if (sys[row][last] > 0) {
	first_row[last] = sys[row][last];
	break;
      }
  }
  first_row[0] = first_row[last];

  strace << "---- simplify (reduce) cgs done." << std::endl;
  return false;
}

} // namespace Parma_Polyhedra_Library
