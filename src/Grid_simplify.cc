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

namespace PPL = Parma_Polyhedra_Library;

namespace Parma_Polyhedra_Library {

// FIX rename these 3?  reduce_le_[with_]le
// FIX rename args to ~ pivot and row

inline void
PPL::Grid::le_le_reduce(Row& row_j, Row& row_k, dimension_type column,
			dimension_type first_column) {
  std::cout << "le_le_reduce" << std::endl;
  // Assume divisors of given generators equal, or moduli of given
  // congruences zero.
  assert(first_column == 1 ?
	 row_j[0] == row_k[0] :
	 row_j[row_j.size()-1] == 0 && row_k[row_j.size()-1] == 0);
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, row_j[column], row_k[column]);
  // FIX const Coefficient_traits::const_reference?
  const Coefficient& a_j = row_j[column] / gcd;
  const Coefficient& a_k = row_k[column] / gcd;
  /* Adjust the elements of row_k.  When `col' is the same as `column'
     then a_k * row_j[col] equals a_j * row_k[col] (this results from
     a_k and a_j being multiples of the gcd), which means that
     row_k[col] is set to zero.  All elements that were originally
     zero remain zero, and the rest change.  The equivalence of the
     adjusted matrix can be seen as follows, where ck_n and cj_n are
     coefficients and i_k and i_j are the inhomogeneous terms.

     1 row_k is multiplied throughout by a_j, producing
         a_j * ck_0 * x_0  +  a_j * ck_1 * x_1 ... a_j * i_k  =  0

     2 A copy of row_j is multiplied throughout by a_k and then
       substituted (for zero) into row_k, producing

         a_j * ck_0 * x_0  +  a_j * ck_1 * x_1  ... +  a_j * i_k
	     =  a_k * cj_0 * x_0  +  a_k * cj_1 * x_1  ... +  a_k * i_j

       which simplifies to an equation having the new values required
       in each element of row_j:

         a_k * cj_0 * x_0  -  a_j * ck_0 * x_0  +  a_k * cj_1 * x_1  -  a_j * ck_1 * x_1
	     ... +  a_k * i_j  -  a_j * i_k  =  0

         (a_k * cj_0  -  a_j * ck_0) * x_0  +  (a_k * cj_1  -  a_j * ck_1) * x_1 ...
	     +  (a_k * i_j - a_j * i_k)  =  0
  */
  for (dimension_type col = first_column;
       col < row_j.size() - 1 - !first_column /* Skip moduli.  */;
       ++col)
    row_k[col] = (a_k * row_j[col]) - (a_j * row_k[col]);
}

inline void
PPL::Grid::pc_pc_reduce(Row& row_j, Row& row_k,
			dimension_type column, dimension_type first_col){
  std::cout << "pc_pc_reduce" << std::endl;
  // Assume the divisors or moduli of the given generators or,
  // respectively, congruences are the same.
  assert(first_col == 1
	 ? row_j[0] == row_k[0] : row_j[row_j.size()-1] == row_k[row_j.size()-1]);
  // FIX better way than all these TEMP_..'s?
  TEMP_INTEGER(gcd);
  TEMP_INTEGER(s);
  TEMP_INTEGER(t);
  gcdext_assign(gcd, row_j[column], row_k[column], s, t);
  TEMP_INTEGER(a_j);
  TEMP_INTEGER(a_k);
  a_j = row_j[column] / gcd;
  a_k = row_k[column] / gcd;
  // Adjust the elements of row_k, as in le_le_reduce above.
  for (dimension_type col = first_col;
       col < row_j.size() - 1 /* Skip moduli or divisors.  */;
       ++col) {
    TEMP_INTEGER(c_j);
    TEMP_INTEGER(c_k);
    c_j = row_j[col];
    c_k = row_k[col];
    /* FIX Is this to lower row_j[col] to the value of gcd, and hence
       decrease the sizes of the elements?  Could it be done in
       le_le_reduce?  (confirm whether) NB for reducing all rows with
       only constant terms to the integrality congruence.

       The equivalence of the adjusted row_j can be seen as with row_k
       in le_le_reduce above, only with a t-multiplied copy of row_k
       being negated and then substituted into an s-multiplied
       row_j.  */
    row_j[col] = (s * c_j) + (t * c_k);
    row_k[col] = (a_j * c_k) - (a_k * c_j);
  }
}

// FIX l_p_reduce?
void
PPL::Grid::le_pc_reduce(Linear_Row& row_j, Linear_Row& row_k,
			dimension_type column, Linear_System& sys) {
  std::cout << "le_pc_reduce" << std::endl;
  // Very similar to the the Congruence version below.  Any change
  // here may be needed there too.

  // FIX add assert
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, row_j[column], row_k[column]);
  TEMP_INTEGER(a_j);
  TEMP_INTEGER(a_k);
  a_j = row_j[column] / gcd;
  a_k = row_k[column] / gcd;
  // FIX correct to skip divisors?
  dimension_type num_cols = sys.num_columns();
  // FIX why?
  for (dimension_type index = 0; index < sys.num_rows() - 1; ++index) {
    Linear_Row& row = sys[index];
    if (row.is_ray_or_point_or_inequality())
      for (dimension_type col = 1; col < num_cols; ++col)
        row[col] *= a_j;
  }
  /* This is like the adjustments in le_le_reduce and pc_pc_reduce
     (row_k[col] has been multiplied by a_j already, in the loop
     above).  */
  for (dimension_type col = 1; col < num_cols; ++col)
    row_k[col] -= a_k * row_j[col];
}

// FIX e_c_reduce?
void
PPL::Grid::le_pc_reduce(Congruence& row_j, Congruence& row_k,
			dimension_type column, Congruence_System& sys) {
  // Very similar to the the Linear_Row version above.  Any change
  // here may be needed there too.
  // FIX add assert
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, row_j[column], row_k[column]);
  TEMP_INTEGER(a_j);
  TEMP_INTEGER(a_k);
  a_j = row_j[column] / gcd;
  a_k = row_k[column] / gcd;
  // FIX -1 size for congs?
  // FIX skip moduli?
  dimension_type num_cols = sys.num_columns() - 1 /* modulus */;
  // FIX factor up every congruence, leaving equalities alone?
  // FIX why?
  for (dimension_type index = 0; index < sys.num_rows() - 1; ++index) {
    Congruence& row = sys[index];
    if (row.is_equality() == false)
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= a_j;
  }
  /* This is like the adjustments in le_le_reduce and pc_pc_reduce
     (row_k[col] has been multiplied by a_j already, in the loop
     above).  */
  for (dimension_type col = 0; col < num_cols; ++col)
    row_k[col] -= a_k * row_j[col];
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
  while (first < system.num_rows())
    for (dimension_type col = 0; col < row_size; ++col)
      if (system[first][col] != 0)
	return false;
  return true;
}

}

int
PPL::Grid::simplify(Linear_System& sys, Saturation_Matrix& sat) {
  // This method is only applied to a well-formed system `sys'.

  // FIX use simple checks 1,2 dimensions to test
  // FIX should be using generator iterators?

  // FIX congruence virtualness  -ve modulus (?)

  // FIX are all the divisors the same?

  // For each column col we must find or construct a row (pivot) in
  // which the value at position `col' is non-zero.
  for (dimension_type col = 1; col < sys.row_size; ++col) {
    std::cout << col << std::endl;
    dimension_type num_rows = sys.num_rows();
    // Start at the diagonal (col, col).
    // FIX when col - 1 > numb of rows?
    dimension_type row_num = col - 1;
    // Move over rows which have zero in column col.
    // FIX array access assumes row has >= 2 elements
    while (row_num < num_rows && sys[row_num][col] == 0)
      ++row_num;
    if (row_num >= num_rows) {
      // Element col is zero in all rows from the col'th, so create a
      // virtual row at index col with diagonal value 1.
      std::cout << "Adding virtual row" << std::endl;
      /* FIX check */
      sys.add_zero_rows(1, sys[0].flags()); // FIX assumes sys has rows
      Linear_Row& new_row = sys[num_rows];
      new_row[col] = 1;
      // FIX NB is this changing the grid?
      //        how should a generator by marked virtual?
      new_row[sys.row_size - 1] = -1; // Mark the row as virtual.
      // FIX will these use the correct swap? (more below)
      std::swap(new_row, sys[col]);
    }
    else {
      Linear_Row& pivot = sys[row_num];
      // For each row having a value other than 0 at col, change the
      // matrix so that the value at col is 0 (leaving the grid itself
      // equivalent).
      std::cout << "Reducing all subsequent rows" << std::endl;
      ++row_num;
      while (row_num < num_rows) {
	if (sys[row_num][col] != 0) {
	  Linear_Row& row = sys[row_num];
	  if (row.is_line_or_equality()) {
	    if (pivot.is_line_or_equality())
	      le_le_reduce(pivot, row, col, 1);
	    else if (pivot.is_ray_or_point_or_inequality()) {
	      std::swap(row, pivot);
	      le_pc_reduce(pivot, row, col, sys);
	    }
	    else {
	      // Keep the row as the new pivot.
	      std::swap(row, pivot);

#define drop_row()							\
	      std::swap(row, sys[--num_rows]);				\
	      sys.Matrix::resize_no_copy(num_rows, sys.row_size, Row::Flags()); \
	      continue;

	      // Free the old, virtual, pivot from sys.
	      drop_row();
	    }
	  }
	  else if (row.is_ray_or_point_or_inequality()) {
	    if (pivot.is_line_or_equality())
	      le_pc_reduce(pivot, row, col, sys);
	    else if (pivot.is_ray_or_point_or_inequality())
	      pc_pc_reduce(pivot, row, col, 1);
	    else {
	      // Keep the row as the new pivot.
	      std::swap(row, pivot);
	      // Free the virtual pivot from sys.
	      drop_row();
	    }
	  }
	  else {
	    // Free the virtual row from sys.
	    drop_row();
	  }
	}
	++row_num;
      }
      Linear_Row& row = sys[col - 1 /* Account for the divisor. */];
      if (row != pivot) {
	std::cout << "swapping" << std::endl;
	std::swap(row, pivot);
      }
    }
  }
  assert(sys.num_rows() >= sys.num_columns() - 1);
  /* Clip any zero rows from the end of the matrix.  */
  if (sys.num_rows() > sys.num_columns() - 1) {
    assert(trailing_rows_are_zero(sys, sys.num_columns() - 1, sys.num_columns()));
    sys.erase_to_end(sys.num_columns() - 1);
  }

  // FIX is this psbl after parameterizing?
  //assert(sys.OK());

  sys.set_sorted(false);  // FIX

  return 1; // FIX
}

/*!
  \return
  The rank of \p sys.

  \param sys
  The system to simplify: it will be modified;

  \param sat
  The saturation matrix corresponding to \p sys.

*/
int
PPL::Grid::simplify(Congruence_System& sys, Saturation_Matrix& sat) {
  std::cout << "simplify cgs:" << std::endl; // FIX
  sys.ascii_dump(std::cout);

  // This method is only applied to a well-formed system `sys'.

  // Cross any changes to the Generator System version.

  // For each column col we must find or construct a row (pivot) in
  // which the value at position `col' is non-zero.
  dimension_type col = 0;
  while (col < sys.num_columns() - 1 /* modulus */) {
    std::cout << "col " << col << std::endl;
    dimension_type num_rows = sys.num_rows();
    std::cout << "  num_rows " << num_rows << std::endl;
    // Start at the diagonal (col, col).
    // FIX rename row_index
    dimension_type row_num = num_rows - col;
    std::cout << "  row_num " << row_num << std::endl;
    dimension_type orig_row_num = (row_num ? row_num - 1 : 0);
    // Move over rows which have zero in column col.
    // FIX array access assumes row has >= 2 elements
    dimension_type column = sys.num_columns() - 1 /* modulus */ - 1 /* index */ - col;
    while (row_num > 0 && sys[row_num-1][column] == 0)
      std::cout << ".", --row_num;
    std::cout << std::endl;
    if (row_num == 0) {
      // Element col is zero in all rows from the col'th, so create a
      // virtual row at index col with diagonal value 1.
      std::cout << "  Adding virtual row" << std::endl;
      // FIX better way to construct virtual row?
      // FIX assumes a row
      //Congruence new_row(sys[0]);
      Row new_row(sys.num_columns(), sys.row_capacity, Row::Flags() /* FIX? */);
      // FIX zero garaunteed in all cols? depends on coefficient ctor?
      std::cout << "    setting column" << std::endl;
      new_row[column] = 1;
      std::cout << "    marking virtual" << std::endl;
      new_row[sys.num_columns() - 1] = -1; // Mark the row as virtual.
      sys.rows.insert(sys.rows.begin() + orig_row_num, new_row);
    }
    else {
      --row_num;		// Now an index.
      Congruence& pivot = sys[row_num];
      // For each FIX<preceding|higher> row having a value other than
      // 0 at col, change the representation so that the value at col
      // is 0 (leaving an equivalent grid).
      std::cout << "  Reducing all subsequent rows" << std::endl;
      // FIX equiv of while (row_num-- > 0)  as row_num is always +ve
      if (row_num-- > 0)
	while (1) { // FIX do-while?
	  std::cout << "    row_num " << row_num << " ";
	  if (sys[row_num][column] != 0) {
	    Congruence& row = sys[row_num]; // FIX just accessed sys
	    // FIX assumes cols?
	    if (row[sys.num_columns() - 1] == -1) {
	      // Free the virtual row from sys.

#undef drop_row
#define drop_row()					  \
	      sys.rows.erase(sys.rows.begin() + row_num); \
	      std::cout << "drop" << std::endl;
	      --num_rows;

	      drop_row();
	    }
	    else if (row.is_equality()) {
	      if (pivot[sys.num_columns() - 1] == -1) {
		// Virtual pivot.

		// Keep the row as the new pivot.
		std::swap(row, pivot);

		// Free the old, virtual, pivot from sys.
		drop_row();
	      }
	      else if (pivot.is_equality())
		le_le_reduce(pivot, row, column, 0);
	      else {
		// Pivot is ray, point or inequality.
		std::swap(row, pivot);
		le_pc_reduce(pivot, row, column, sys);
	      }
	    }
	    else {
	      // Row is a congruence.
	      if (pivot[sys.num_columns() - 1] == -1) {
		// Virtual pivot.

		// Keep the row as the new pivot.
		std::swap(row, pivot);
		// Free the virtual pivot from sys.
		drop_row();
	      }
	      else if (pivot.is_equality())
		le_pc_reduce(pivot, row, column, sys);
	      else
		// Pivot is a congruence.
		pc_pc_reduce(pivot, row, column, 0);
	    }
	  }
	  if (row_num-- == 0)
	    break;
	}
      Congruence& row = sys[orig_row_num /*FIX column?*/];
      if (row != pivot) {
	std::cout << "swapping" << std::endl;
	std::swap(row, pivot);
      }
      sys.ascii_dump(std::cout);
    }
    ++col;
  }
  assert(sys.num_rows() >= sys.num_columns() - 1);
  /* Clip any zero rows from the end of the matrix.  */
  if (sys.num_rows() > sys.num_columns() - 1) {
    assert(trailing_rows_are_zero(sys, sys.num_columns(), sys.num_columns() - 1));
    sys.rows.erase(sys.rows.begin(), sys.rows.begin() + (sys.num_rows() - sys.num_columns() + 1));
  }

  assert(sys.OK());

  return 1; // FIX
}
