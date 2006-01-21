/* Grid class implementation: simplify().
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include <cassert>

#include "Grid.defs.hh"

namespace Parma_Polyhedra_Library {

#define TRACE(x)
//#define TRACE(x) x

TRACE(using std::endl);
TRACE(using std::cerr);

#ifdef STRONG_REDUCTION
template <typename M, typename R>
void
Grid::reduce_reduced(M& sys, dimension_type dim, dimension_type pivot_index,
		     dimension_type start, dimension_type end,
		     Dimension_Kinds& dim_kinds, bool generators) {
  R& pivot = sys[pivot_index];

  TEMP_INTEGER(pivot_dim);
  pivot_dim = pivot[dim];

  if (pivot_dim == 0)
    return;

  TEMP_INTEGER(pivot_dim_half);
  pivot_dim_half = (pivot_dim + 1) / 2;
  Dimension_Kind row_kind = dim_kinds[dim];
  Dimension_Kind line_or_equality, virtual_kind;
  int jump;
  if (generators) {
    line_or_equality = LINE;
    virtual_kind = GEN_VIRTUAL;
    jump = -1;
  } else {
    line_or_equality = EQUALITY;
    virtual_kind = CON_VIRTUAL;
    jump = 1;
  }

  for (dimension_type row_index = pivot_index, kinds_index = dim + jump;
       row_index-- > 0;
       kinds_index += jump) {
    // Move over any virtual rows.
    while (dim_kinds[kinds_index] == virtual_kind)
      kinds_index += jump;

    if (row_kind == line_or_equality
	|| (row_kind == PARAMETER // a.k.a. CONGRUENCE
	    && dim_kinds[kinds_index] == PARAMETER)) {
      R& row = sys[row_index];

      TEMP_INTEGER(row_dim);
      row_dim = row[dim];
      TEMP_INTEGER(num_rows);
      num_rows = row_dim / pivot_dim;

      // Ensure that after subtracting num_rows * r_dim from row_dim,
      // -pivot_dim_half < row_dim <= pivot_dim_half.  E.g., if pivot[dim] =
      // 9, then after strong reduction -5 < row_dim <= 5.
      Coefficient& row_dim_rem = row_dim;
      row_dim_rem %= pivot_dim;
      if (row_dim_rem < 0) {
	if (row_dim_rem <= -pivot_dim_half)
	  --num_rows;
      }
      else if (row_dim_rem > 0 && row_dim_rem > pivot_dim_half)
	num_rows++;

      // Subtract num_rows copies of pivot from row i.  Only the
      // entries from dim need to be subtracted, as the preceding
      // entries are all zero.
      if (num_rows != 0)
	for (dimension_type col = start; col <= end; ++col)
	  row[col] -= num_rows * pivot[col];
    }
  }
}
#endif // STRONG_REDUCTION

inline void
Grid::reduce_line_with_line(Grid_Generator& row, Grid_Generator& pivot,
			    dimension_type column) {
  TRACE(cerr << "reduce_line_with_line" << endl);
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pa);
  TEMP_INTEGER(ra);
  pa = pivot[column] / gcd;
  ra = row[column] / gcd;
  /* Adjust the elements of row such that row[column] is zero.

     ra * pivot[col] == pa * row[col], which means that in the loop
     below row[column] is set to zero.  All elements in row that were
     originally zero remain zero, and the rest are updated.

     The equivalence of the adjusted row can be seen as follows, where
     pn and rn are the pivot and row coefficients and pi and ri are
     the inhomogeneous terms of the pivot and row.

     FIX Is it OK to consider the parameters as equations like this?

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
  for (dimension_type col = pivot.size() - 2 /* parameter divisor, index */;
       col >= column;
       --col)
    row[col] = (ra * pivot[col]) - (pa * row[col]);
}

inline void
Grid::reduce_equality_with_equality(Congruence& row, Congruence& pivot,
				    dimension_type column) {
  TRACE(cerr << "reduce_equality_with_equality" << endl);
  // Assume two equalities.
  assert(row.modulus() == 0 && pivot.modulus() == 0);

  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pa);
  TEMP_INTEGER(ra);
  pa = pivot[column] / gcd;
  ra = row[column] / gcd;
  // Adjust the elements of row, as in reduce_line_with_line.
  for (dimension_type col = 0; col <= column; ++col)
    row[col] = (ra * pivot[col]) - (pa * row[col]);
}

template <typename R>
void
Grid::reduce_pc_with_pc(R& row, R& pivot,
			dimension_type column,
			dimension_type start,
			dimension_type end) {
  TEMP_INTEGER(gcd);
  TEMP_INTEGER(s);
  TEMP_INTEGER(t);
  gcdext_assign(gcd, pivot[column], row[column], s, t);
  TRACE(cerr << "  gcd " << gcd << ", s " << s << ", t " << t << endl);
  // Now pivot[column] * s + row[column] * t == gcd.
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
  TRACE(cerr << "  pivot_a " << pivot_a << ", row_a " << row_a << endl);
  // Adjust the elements of row and pivot, similarly to
  // reduce_line_with_line above.
  assert(pivot.size() > 0);
  assert(row.size() > 0);
  pivot[column] = gcd;
  row[column] = 0;
  for (dimension_type col = start; col < end; ++col) {
    TEMP_INTEGER(pivot_col);
    TEMP_INTEGER(row_col);
    pivot_col = pivot[col];
    row_col = row[col];
    /* FIX Is this to lower pivot[col] to the value of gcd, and hence
       lower the sizes of the elements?  Could it be done in
       reduce_line_with_line?  (confirm whether) NB for reducing all rows
       with only constant terms to the integrality congruence.

       The equivalence of the adjusted pivot can be seen as with row
       in reduce_line_with_line above, only with an s-multiplied copy
       of pivot being negated and then substituted into a t-multiplied
       row.  */
    pivot[col] = (s * pivot_col) + (t * row_col);
    // FIX up then back down, is there a direct route?
    row[col] = (pivot_a * row_col) - (row_a * pivot_col);
  }
}

void
Grid::reduce_parameter_with_line(Grid_Generator& row,
				 Grid_Generator& pivot,
				 dimension_type column,
				 Grid_Generator_System& sys) {
  // Very similar to reduce_congruence_with_equality below.  Any
  // change here may be needed there too.
  // FIX check if row[column] == pivot[column], as in cong version?
  TRACE(cerr << "reduce_parameter_with_line" << endl);

  dimension_type num_cols = sys.num_columns();

  // If the elements at column in row and pivot are the same, then
  // just subtract pivot from row.
  if (row[column] == pivot[column]) {
    for (dimension_type col = 0; col < num_cols; ++col)
      row[col] -= pivot[col];
    return;
  }

  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
#ifdef STRONG_REDUCTION
  // Ensure that the multiplier is positive, so that the preceding
  // diagonals (including the divisor) remain positive.  It's safe to
  // swap the signs as row[column] will still come out 0.
  if (pivot_a < 0) {
    neg_assign(pivot_a);
    neg_assign(row_a);
  }
#endif
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Grid_Generator& row = sys[index];
    if (row.is_parameter_or_point())
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= pivot_a;
  }
  // Adjust row as in reduce_line_with_line (row[col] has been
  // multiplied by pivot_a already, in the loop above).
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= row_a * pivot[col];
}

void
Grid::reduce_congruence_with_equality(Congruence& row,
				      Congruence& pivot,
				      dimension_type column,
				      Congruence_System& sys) {
  // Very similar to reduce_parameter_with_line above.  Any change
  // here may be needed there too.
  TRACE(cerr << "reduce_congruence_with_equality" << endl);
  assert(row.modulus() > 0 && pivot.modulus() == 0);

  dimension_type num_cols = sys.num_columns();

  // If the elements at `column' in row and pivot are the same, then
  // just subtract `pivot' from `row'.
  if (row[column] == pivot[column]) {
    for (dimension_type col = 0; col < num_cols; ++col)
      row[col] -= pivot[col];
    return;
  }

  TEMP_INTEGER(gcd);
  gcd_assign(gcd, pivot[column], row[column]);
  TEMP_INTEGER(pivot_a);
  TEMP_INTEGER(row_a);
  pivot_a = pivot[column] / gcd;
  row_a = row[column] / gcd;
  // Ensure that `pivot_a' is positive, so that the modulus remains
  // positive when multiplying the proper congruences below.  It's
  // safe to swap the signs as row[column] will still come out 0.
  if (pivot_a < 0) {
    neg_assign(pivot_a);
    neg_assign(row_a);
  }
  // Multiply `row', including the modulus, by pivot_a.  FIX To keep
  // all the moduli the same this requires multiplying all the other
  // proper congruences in the same way.
  for (dimension_type index = 0; index < sys.num_rows(); ++index) {
    Congruence& row = sys[index];
    if (row.is_proper_congruence())
      for (dimension_type col = 0; col < num_cols; ++col)
        row[col] *= pivot_a;
  }
  --num_cols;			// Modulus.
  // Adjust row as in reduce_line_with_line (`row' has been multiplied
  // by pivot_a already, in the loop above).
  for (dimension_type col = 0; col < num_cols; ++col)
    row[col] -= row_a * pivot[col];
}

#ifndef NDEBUG
//! Check for trailing rows containing only zero terms.
/*!
  If all columns contain zero in the rows of \p system from row index
  \p first to row index \p last then return <code>true</code>, else
  return <code>false</code>.  \p row_size gives the number of columns
  in each row.  Used in assertion below.
*/
bool
rows_are_zero(Matrix& system, dimension_type first,
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
Grid::simplify(Grid_Generator_System& sys, Dimension_Kinds& dim_kinds) {
  TRACE(cerr << "==== simplify (reduce) gs:" << endl);
  TRACE(cerr << "sys:" << endl);
  TRACE(sys.ascii_dump(cerr));
  assert(sys.num_rows() > 0);
  assert(sys.num_columns() > 0); // For reduce_pc_with_pc.

  // Changes here may also be required in the congruence version
  // below.

  dimension_type num_cols = sys.num_columns() - 1 /* parameter divisor */;

  if (dim_kinds.size() != num_cols)
    dim_kinds.resize(num_cols);

  dimension_type num_rows = sys.num_rows();
  TRACE(cerr << "  num_rows " << num_rows << endl);

  // For each dimension `dim' move or construct a row into position
  // `pivot_index' such that the row has zero in all elements
  // following column `dim' and a value other than zero in column
  // `dim'.
  dimension_type pivot_index = 0;
  for (dimension_type dim = 0; dim < num_cols; ++dim) {
    TRACE(cerr << "dim " << dim << endl);
    trace_dim_kinds("  ", dim_kinds);

    // Consider the pivot and following rows.
    dimension_type row_index = pivot_index;
    TRACE(cerr << "  row_index " << row_index << endl);

    // Move down over rows which have zero in column `dim'.
    while (row_index < num_rows && sys[row_index][dim] == 0) {
      TRACE(cerr << "  .");
      ++row_index;
    }
    TRACE(cerr << endl);

    if (row_index == num_rows) {
      // Element in column `dim' is zero in all rows from the pivot.
      TRACE(cerr << "  Marking virtual row" << endl);
      dim_kinds[dim] = GEN_VIRTUAL;
    }
    else {
      if (row_index != pivot_index)
	std::swap(sys[row_index], sys[pivot_index]);
      Grid_Generator& pivot = sys[pivot_index];
      bool pivot_is_line = pivot.is_line();

      // Change the matrix so that the value at `dim' in every row
      // following `pivot_index' is 0, leaving an equivalent grid.
      TRACE(cerr << "  Reducing all following rows" << endl);
      while (row_index < num_rows - 1) {
	++row_index;
	TRACE(cerr << "    row_index " << row_index << endl);

	Grid_Generator& row = sys[row_index];

	if (row[dim] == 0)
	  continue;

	if (row.is_line())
	  if (pivot_is_line)
	    reduce_line_with_line(row, pivot, dim);
	  else {
	    assert(pivot.is_parameter_or_point());
	    std::swap(row, pivot);
	    pivot_is_line = true;
	    reduce_parameter_with_line(row, pivot, dim, sys);
	  }
	else {
	  assert(row.is_parameter_or_point());
	  if (pivot_is_line)
	    reduce_parameter_with_line(row, pivot, dim, sys);
	  else {
	    assert(pivot.is_parameter_or_point());
	    reduce_pc_with_pc(row, pivot, dim, dim + 1, num_cols);
	  }
	}
      }

      if (pivot_is_line)
	dim_kinds[dim] = LINE;
      else {
	assert(pivot.is_parameter_or_point());
	dim_kinds[dim] = PARAMETER;
      }

#ifdef STRONG_REDUCTION
      // Ensure a positive follows the leading zeros.
      if (pivot[dim] < 0)
	pivot.negate(dim, num_cols - 1);
      TRACE(cerr << "  rr pivot_index " << pivot_index << endl);
      TRACE(sys.ascii_dump(cerr));
      // Factor this row out of the preceding rows.
      reduce_reduced<Grid_Generator_System, Grid_Generator>
	(sys, dim, pivot_index, dim, num_cols - 1, dim_kinds);
#endif

      ++pivot_index;
    }
    TRACE(sys.ascii_dump(cerr));
  }
  trace_dim_kinds("gs simpl end ", dim_kinds);

  // Either the first row is a point, or H is empty.
  // FIX all callers assume always returns containing points?

  if (sys[0].is_parameter_or_point()) {
    // Clip any zero rows from the end of the matrix.
    if (num_rows > pivot_index) {
      TRACE(cerr << "clipping trailing" << endl);
      assert(rows_are_zero(sys,
			   pivot_index,		// index of first
			   sys.num_rows() - 1,  // index of last
			   sys.num_columns() - 1)); // row size
      sys.erase_to_end(pivot_index);
    }

    sys.unset_pending_rows();

    // Ensure that the parameter divisors are the same as the system
    // divisor.
    TRACE(cerr << "updating param divisors" << endl);
    Coefficient_traits::const_reference system_divisor = sys[0][0];
    for (dimension_type row = 1, dim = 1, num_cols = sys.num_columns() - 1;
	 dim < num_cols;
	 ++dim)
      switch (dim_kinds[dim]) {
      case PARAMETER:
	sys[row].divisor() = system_divisor;
      case LINE:
	++row;
      case GEN_VIRTUAL:
	break;
      }

    assert(sys.OK());

    TRACE(cerr << "---- simplify (reduce) gs done." << endl);
    return false;
  }

  sys.clear();
  sys.set_sorted(false);
  sys.unset_pending_rows();

  assert(sys.OK());
  TRACE(cerr << "---- simplify (reduce) gs done (empty)." << endl);
  return true;
}

bool
Grid::simplify(Congruence_System& sys, Dimension_Kinds& dim_kinds) {
  TRACE(cerr << "======== simplify (reduce) cgs:" << endl);
  TRACE(cerr << "sys:" << endl);
  TRACE(sys.ascii_dump(cerr));
  assert(sys.num_columns() > 2);

  // Changes here may also be required in the generator version above.

  sys.normalize_moduli();

  dimension_type num_cols = sys.num_columns() - 1 /* modulus */;

  if (dim_kinds.size() != num_cols)
    dim_kinds.resize(num_cols);

  dimension_type num_rows = sys.num_rows();
  TRACE(cerr << "  num_rows " << num_rows << endl);

  // For each dimension `dim' move or construct a row into position
  // `pivot_index' such that the row has a value of zero in all
  // elements preceding column `dim' and some other value in column
  // `dim'.
  dimension_type pivot_index = 0;
  for (dimension_type dim = num_cols; dim-- > 0; ) {
    TRACE(cerr << "dim " << dim << endl);
    trace_dim_kinds("  ", dim_kinds);

    // Consider the pivot and following rows.
    dimension_type row_index = pivot_index;
    TRACE(cerr << "  row_index " << row_index << endl);

    // Move down over rows which have zero in column `dim'.
    while (row_index < num_rows && sys[row_index][dim] == 0) {
      TRACE(cerr << "  .");
      ++row_index;
    }
    TRACE(cerr << endl);

    if (row_index == num_rows) {
      // Element in column `dim' is zero in all rows from the pivot,
      // or `sys' is empty of rows.
      TRACE(cerr << "  Marking virtual row" << endl);
      dim_kinds[dim] = CON_VIRTUAL;
    }
    else {
      if (row_index != pivot_index)
	std::swap(sys[row_index], sys[pivot_index]);
      Congruence& pivot = sys[pivot_index];
      bool pivot_is_equality = pivot.is_equality();

      // Change the matrix so that the value at `dim' in every row
      // following `pivot_index' is 0, leaving an equivalent grid.
      TRACE(cerr << "  Reducing all following rows" << endl);
      while (row_index < num_rows - 1) {
	++row_index;
	TRACE(cerr << "    row_index " << row_index << endl);

	Congruence& row = sys[row_index];

	if (row[dim] == 0)
	  continue;

	if (row.is_equality())
	  if (pivot_is_equality)
	    reduce_equality_with_equality(row, pivot, dim);
	  else {
	    assert(pivot.is_proper_congruence());
	    std::swap(row, pivot);
	    pivot_is_equality = true;
	    reduce_congruence_with_equality(row, pivot, dim, sys);
	  }
	else {
	  assert(row.is_proper_congruence());
	  if (pivot_is_equality)
	    reduce_congruence_with_equality(row, pivot, dim, sys);
	  else {
	    assert(pivot.is_proper_congruence());
	    reduce_pc_with_pc(row, pivot, dim, 0, dim);
	  }
	}
      }

      if (pivot_is_equality)
	dim_kinds[dim] = EQUALITY;
      else {
	assert(pivot.is_proper_congruence());
	dim_kinds[dim] = PROPER_CONGRUENCE;
      }

#ifdef STRONG_REDUCTION
      // Ensure a positive follows the leading zeros.
      if (pivot[dim] < 0)
	pivot.negate(0, dim);
      // Factor this row out of the preceding ones.
      reduce_reduced<Congruence_System, Congruence>
	(sys, dim, pivot_index, 0, dim, dim_kinds, false);
#endif
      ++pivot_index;
    }
    TRACE(sys.ascii_dump(cerr));
  }

  dimension_type& reduced_num_rows = pivot_index; // For clearer naming.

  // Clip any zero rows from the end of the matrix.
  if (num_rows > 1 && num_rows > reduced_num_rows) {
    TRACE(cerr << "clipping trailing" << endl);
    assert(rows_are_zero(sys,
			 reduced_num_rows,    // index of first
			 num_rows - 1,	      // index of last
			 num_cols));	      // row size
    sys.erase_to_end(reduced_num_rows);
  }

  assert(sys.num_rows() == reduced_num_rows
	 || (sys.num_rows() == 1 && reduced_num_rows == 0));

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
      std::swap(sys.rows[0], sys.rows.back());
      sys.erase_to_end(1);

      trace_dim_kinds("cgs simpl end ", dim_kinds);
      assert(sys.OK());
      TRACE(cerr << "---- simplify (reduce) cgs done (empty)." << endl);
      return true;
    }
  }
  else if (num_rows > 0) {
    assert(sys.num_rows() == 1);
    // All columns up to the modulus column must have been zero, set
    // up the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    sys[0][num_cols] = 1;
    reduced_num_rows = 1;
  }

  // Ensure that the last row is the integrality congruence.
  dimension_type mod_index = num_cols;
  if (dim_kinds[0] == CON_VIRTUAL) {
    // The last row is virtual, append the integrality congruence.
    dim_kinds[0] = PROPER_CONGRUENCE;
    sys.add_zero_rows(1, Linear_Row::Flags(NECESSARILY_CLOSED,
					   Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
    Congruence& new_last_row = sys[reduced_num_rows];
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
#ifdef STRONG_REDUCTION
    ++reduced_num_rows;
#endif
  }
  else {
    Congruence& last_row = sys[reduced_num_rows - 1];
    last_row[0] = last_row[mod_index];
  }

#ifdef STRONG_REDUCTION
  // Factor the modified integrality congruence out of the other rows.
  reduce_reduced<Congruence_System, Congruence>
    (sys, 0, reduced_num_rows - 1, 0, 0, dim_kinds, false);
#endif

  trace_dim_kinds("cgs simpl end ", dim_kinds);
  assert(sys.OK());
  TRACE(cerr << "---- simplify (reduce) cgs done." << endl);
  return false;
}

#undef TRACE

} // namespace Parma_Polyhedra_Library
