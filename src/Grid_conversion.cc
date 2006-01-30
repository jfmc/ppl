/* Grid class implementation: conversion().
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

#include "Grid.defs.hh"
#include <cstddef>

namespace Parma_Polyhedra_Library {

#define TRACE(x)
//#define TRACE(x) x

TRACE(using std::endl);
TRACE(using std::cerr);

// X 0 0 0  upside down, so  x x x X
// x X 0 0                   x x X 0
// x x X 0                   x X 0 0
// x x x X                   X 0 0 0
//
// Where X is greater than zero and x is an integer.
bool
Grid::lower_triangular(const Congruence_System& sys,
		       const Dimension_Kinds& dim_kinds) {
  dimension_type num_cols = sys.num_columns() - 1;
  dimension_type row = sys.num_rows();

  // Check for easy square failure case.
  if (row > num_cols)
    return false;

  // Check triangularity.
  for (dimension_type dim = 0; dim < num_cols; ++dim) {
    if (dim_kinds[dim] == CON_VIRTUAL)
      continue;
    const Congruence& cg = sys[--row];
    // Check diagonal.
    if (cg[dim] <= 0)
      return false;
    // Check elements following diagonal.
    dimension_type col = dim;
    while (++col < num_cols)
      if (cg[col] != 0)
	return false;
  }

  // Check squareness.
  return row == 0;
}

// X x x x
// 0 X x x
// 0 0 X x
// 0 0 0 X
//
// Where X is greater than zero and x is an integer.
bool
Grid::upper_triangular(const Grid_Generator_System& sys,
		       const Dimension_Kinds& dim_kinds) {
  dimension_type num_cols = sys.space_dimension() + 1;
  dimension_type row = sys.num_generators();

  // Check for easy square fail case.
  if (row > num_cols)
    return false;

  // Check triangularity.
  while (num_cols > 0) {
    --num_cols;
    if (dim_kinds[num_cols] == GEN_VIRTUAL)
      continue;
    const Grid_Generator& gen = sys[--row];
    // Check diagonal.
    if (gen[num_cols] <= 0)
      return false;
    // Check elements preceding diagonal.
    dimension_type col = num_cols;
    while (col-- > 0)
      if (gen[col] != 0)
	return false;
  }

  // Check for squareness.
  return num_cols == row;
}

inline void
Grid::multiply_grid(const Coefficient& multiplier, Grid_Generator& gen,
		    Grid_Generator_System& dest, const dimension_type num_rows,
		    const dimension_type num_dims) {
  if (multiplier == 1)
    return;

  if (gen.is_line())
    // Multiply every element of the line.
    for (dimension_type column = 0; column < num_dims; ++column)
      gen[column] *= multiplier;
  else {
    assert(gen.is_parameter_or_point());
    // Multiply every element of every parameter.
    for (dimension_type index = 0; index < num_rows; ++index) {
      Grid_Generator& generator = dest[index];
      if (generator.is_parameter_or_point())
	for (dimension_type column = 0; column < num_dims; ++column)
	  generator[column] *= multiplier;
    }
  }
}

inline void
Grid::multiply_grid(const Coefficient& multiplier, Congruence& cg,
		    Congruence_System& dest, const dimension_type num_rows,
		    const dimension_type num_dims) {
  if (multiplier == 1)
    return;

  if (cg.is_proper_congruence())
    // Multiply every element of every congruence.
    for (dimension_type index = 0; index < num_rows; ++index) {
      Congruence& congruence = dest[index];
      if (congruence.is_proper_congruence())
	for (dimension_type column = 0; column < num_dims; ++column)
	  congruence[column] *= multiplier;
    }
  else {
    assert(cg.is_equality());
    // Multiply every element of the equality.
    for (dimension_type column = 0; column < num_dims; ++column)
      cg[column] *= multiplier;
  }
}

// TODO: Rename the next two methods to convert and this file
//       Grid_convert.cc (and equivalently for Polyhedron) to use
//       verbs consistently as function and method names.

void
Grid::conversion(Grid_Generator_System& source, Congruence_System& dest,
		 Dimension_Kinds& dim_kinds) {
  TRACE(cerr << "============= convert gs to cgs" << endl);
  TRACE(cerr << "source:" << endl);
  TRACE(source.ascii_dump(cerr));
  TRACE(cerr << "dest:" << endl);
  TRACE(dest.ascii_dump(cerr));
  trace_dim_kinds("gs to cgs ", dim_kinds);

  // Quite similar to the congruence to generator version below.
  // Changes here may be needed there too.

  assert(upper_triangular(source, dim_kinds));

  // Initialise matrix row number counters and compute the LCM of the
  // diagonal entries of the parameters in `source'.
  //
  // The top-down order of the parameter system rows corresponds to
  // the left-right order of the dimensions.
  dimension_type source_num_rows = 0;
  // The congruence system rows have a bottom-up ordering.
  dimension_type dest_num_rows = 0;
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = 1;
  const dimension_type dims = source.space_dimension() + 1;
  for (dimension_type dim = 0; dim < dims; ++dim)
    if (dim_kinds[dim] == GEN_VIRTUAL)
      // Virtual generators map to equalities.
      ++dest_num_rows;
    else {
      if (dim_kinds[dim] == PARAMETER) {
	// Dimension `dim' has a parameter row at `source_num_rows' in
	// `source', so include in `diagonal_lcm' the `dim'th element
	// of that row.
	lcm_assign(diagonal_lcm, diagonal_lcm, source[source_num_rows][dim]);
	// Parameters map to proper congruences.
	++dest_num_rows;
      }
      // Lines map to virtual congruences.
      ++source_num_rows;
    }
  TRACE(cerr << "diagonal_lcm: " << diagonal_lcm << endl);
  TRACE(cerr << "source_num_rows: " << source_num_rows << endl);
  TRACE(cerr << "dest_num_rows: " << dest_num_rows << endl);

  // `source' must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  dest.resize_no_copy(dest_num_rows, dims + 1 /* moduli */);

  // In `dest' initialize row types and elements, including setting
  // the diagonal elements to the inverse ratio of the `source'
  // diagonal elements.
  dimension_type source_index = 0, dest_index = dest_num_rows - 1;
  for (dimension_type dim = 0; dim < dims; ++dim) {
    TRACE(cerr << "init dim " << dim << endl);
    if (dim_kinds[dim] == LINE) {
      TRACE(cerr << "  line" << endl);
      ++source_index;
    }
    else {
      Congruence& cg = dest[dest_index];
      for (dimension_type j = 0; j < dim; j++)
	cg[j] = 0;
      for (dimension_type j = dim + 1; j < dims; j++)
	cg[j] = 0;

      if (dim_kinds[dim] == GEN_VIRTUAL) {
	TRACE(cerr << "  gen_virtual" << endl);
	cg[dims] = 0;		// An equality.
	cg[dim] = 1;
      }
      else {
	assert(dim_kinds[dim] == PARAMETER);
	TRACE(cerr << "  parameter" << endl);
	cg[dims] = 1;		// A proper congruence.
	cg[dim] = diagonal_lcm / source[source_index][dim];
	++source_index;
      }
      --dest_index;
    }
  }

  assert(lower_triangular(dest, dim_kinds));

  TRACE(cerr << "dest after init:" << endl);
  TRACE(dest.ascii_dump(cerr));

  // Convert.
  //
  // `source_index' and `dest_index' hold the positions of pivot rows
  // in `source' and `dest'.  The order of the rows in `dest' is the
  // reverse of the order in `source', so the rows are iterated from
  // last to first (index 0) in `source' and from first to last in
  // `dest'.
  source_index = source_num_rows;
  dest_index = 0;

  for (dimension_type dim = dims; dim-- > 0; ) {
    TRACE(cerr << "dim: " << dim << endl);

    if (dim_kinds[dim] != GEN_VIRTUAL) {
      --source_index;
      TEMP_INTEGER(source_dim);
      source_dim = source[source_index][dim];

      // In the rows in `dest' above `dest_index' divide each element
      // at column `dim' by `source_dim'.
      for (dimension_type row = dest_index; row-- > 0; ) {
	TRACE(cerr << "  row " << row << endl);
	TRACE(dest.ascii_dump(cerr));

	Congruence& cg = dest[row];

	// Multiply the representation of `dest' such that entry `dim'
        // of `g' is a multiple of `source_dim'.  This ensures that
        // the result of the division that follows is a whole number.
	TEMP_INTEGER(multiplier);
	gcd_assign(multiplier, cg[dim], source_dim);
	multiplier = source_dim / multiplier;
	multiply_grid(multiplier, cg, dest, dest_num_rows, dims);

	cg[dim] /= source_dim;
      }
      TRACE(cerr << "dest after dividing grid:" << endl);
      TRACE(dest.ascii_dump(cerr));
    }

    // Invert and transpose the source row at `source_index' into the
    // destination row at `dest_index'.
    //
    // Consider each dimension `dim_prec' that precedes `dim', as the
    // rows in `dest' that follow `dim_index' have zeroes at index
    // `dim'.
    dimension_type tem_source_index = source_index;
    if (dim_kinds[dim] != LINE)
      ++dest_index;
    for (dimension_type dim_prec = dim; dim_prec-- > 0; ) {
      TRACE(cerr << "  dim_prec: " << dim_prec);
      TRACE(cerr << "  dest_index: " << dest_index);
      TRACE(cerr << "  tem_source_index: " << tem_source_index << endl);
      if (dim_kinds[dim_prec] != GEN_VIRTUAL) {
	--tem_source_index;
	TEMP_INTEGER(source_dim);
	source_dim = source[tem_source_index][dim];
	TRACE(cerr << "  rows:" << endl);
	// In order to compute the transpose of the inverse of
	// `source', subtract source[tem_source_index][dim] times the
	// column vector in `dest' at `dim' from the column vector in
	// `dest' at `dim_prec'.
	//
	// I.e., for each row `dest_index' in `dest' that is above the
	// row `dest_index', subtract dest[tem_source_index][dim]
	// times the entry `dim' from the entry at `dim_prec'.
	for (dimension_type row = dest_index; row-- > 0; ) {
	  assert(row < dest_num_rows);
	  TRACE(cerr << "       " << row << endl);
	  Congruence& cg = dest[row];
	  cg[dim_prec] -= source_dim * cg[dim];
	}
      }
    }

    TRACE(cerr << "dest after processing preceding rows:" << endl);
    TRACE(dest.ascii_dump(cerr));
  }
  // Set the modulus in every congruence.
  Coefficient_traits::const_reference modulus = dest[dest_num_rows - 1][0];
  for (dimension_type row = 0; row < dest_num_rows; ++row) {
    Congruence& cg = dest[row];
    if (cg[dims] > 0)
      // `cg' is a proper congruence.
      cg[dims] = modulus;
  }
  TRACE(cerr << "dest after setting moduli:" << endl);
  TRACE(dest.ascii_dump(cerr));

  assert(lower_triangular(dest, dim_kinds));

#ifdef STRONG_REDUCTION
  for (dimension_type dim = dims, i = 0; dim-- > 0; )
    if (dim_kinds[dim] != CON_VIRTUAL)
      // Factor the "diagonal" congruence out of the preceding rows.
      reduce_reduced<Congruence_System, Congruence>
	(dest, dim, i++, 0, dim, dim_kinds, false);
  TRACE(cerr << "dest after strong reduction:" << endl);
  TRACE(dest.ascii_dump(cerr));
#endif

  trace_dim_kinds("gs to cgs end ", dim_kinds);

  TRACE(cerr << "------------------- gs to cgs conversion done." << endl);
}

void
Grid::conversion(Congruence_System& source, Grid_Generator_System& dest,
		 Dimension_Kinds& dim_kinds) {
  TRACE(cerr << "============= convert cgs to gs" << endl);
  TRACE(cerr << "source:" << endl);
  TRACE(source.ascii_dump(cerr));
  TRACE(cerr << "dest:" << endl);
  TRACE(dest.ascii_dump(cerr));
  trace_dim_kinds("cgs to gs ", dim_kinds);

  // Quite similar to the generator to congruence version above.
  // Changes here may be needed there too.

  assert(lower_triangular(source, dim_kinds));

  // Initialise matrix row number counters and compute the LCM of the
  // diagonal entries of the proper congruences in `source'.
  dimension_type source_num_rows = 0, dest_num_rows = 0;
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = 1;
  dimension_type dims = source.num_columns() - 1;
  for (dimension_type dim = dims; dim-- > 0; )
    if (dim_kinds[dim] == CON_VIRTUAL)
      // Virtual congruences map to lines.
      ++dest_num_rows;
    else {
      if (dim_kinds[dim] == PROPER_CONGRUENCE) {
	// Dimension `dim' has a proper congruence row at
	// `source_num_rows' in `source', so include in `diagonal_lcm'
	// the `dim'th element of that row.
	lcm_assign(diagonal_lcm, diagonal_lcm, source[source_num_rows][dim]);
	// Proper congruences map to parameters.
	++dest_num_rows;
      }
      // Equalities map to virtual generators.
      ++source_num_rows;
    }
  TRACE(cerr << "diagonal_lcm: " << diagonal_lcm << endl);
  TRACE(cerr << "source_num_rows: " << source_num_rows << endl);
  TRACE(cerr << "dest_num_rows: " << dest_num_rows << endl);

  // `source' must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  dest.set_index_first_pending_row(dest_num_rows);
  dest.resize_no_copy(dest_num_rows, dims + 1 /* parameter divisor */);

  // In `dest' initialize row types and elements, including setting
  // the diagonal elements to the inverse ratio of the `source'
  // diagonal elements.
  //
  // The top-down order of the congruence system rows corresponds to
  // the right-left order of the dimensions.
  dimension_type source_index = 0;
  // The generator system has a bottom-up ordering.
  dimension_type dest_index = dest_num_rows - 1;
  for (dimension_type dim = dims; dim-- > 0; ) {
    TRACE(cerr << "init dim " << dim << endl);
    if (dim_kinds[dim] == EQUALITY) {
      TRACE(cerr << "  equality" << endl);
      ++source_index;
    }
    else {
      Grid_Generator& g = dest[dest_index];
      for (dimension_type j = 0; j < dim; ++j)
	g[j] = 0;
      for (dimension_type j = dim + 1; j < dims; ++j)
	g[j] = 0;

      if (dim_kinds[dim] == CON_VIRTUAL) {
	TRACE(cerr << "  con_virtual" << endl);
	g.set_is_line();
	g[dim] = 1;
      }
      else {
	assert(dim_kinds[dim] == PROPER_CONGRUENCE);
	TRACE(cerr << "  proper_congruence" << endl);
	g.set_is_parameter_or_point();
	g[dim] = diagonal_lcm / source[source_index][dim];
	++source_index;
      }
      --dest_index;
    }
  }

  assert(upper_triangular(dest, dim_kinds));

  TRACE(cerr << "dest after init:" << endl);
  TRACE(dest.ascii_dump(cerr));

  // Convert.
  //
  // `source_index' and `dest_index' hold the positions of pivot rows
  // in `source' and `dest'.  The order of the rows in `dest' is the
  // reverse of the order in `source', so the rows are iterated from
  // last to first (index 0) in `source' and from first to last in
  // `dest'.
  source_index = source_num_rows;
  dest_index = 0;

  for (dimension_type dim = 0; dim < dims; ++dim) {
    TRACE(cerr << "dim: " << dim << endl);

    if (dim_kinds[dim] != CON_VIRTUAL) {
      --source_index;
      TEMP_INTEGER(source_dim);
      source_dim = source[source_index][dim];

      // In the rows in `dest' above `dest_index' divide each element
      // at column `dim' by `source_dim'.
      for (dimension_type row = dest_index; row-- > 0; ) {
	TRACE(cerr << "  row " << row << endl);
	TRACE(dest.ascii_dump(cerr));

	Grid_Generator& g = dest[row];

	// Multiply the representation of `dest' such that entry `dim'
        // of `g' is a multiple of `source_dim'.  This ensures that
        // the result of the division that follows is a whole number.
	TEMP_INTEGER(red_source_dim);
	gcd_assign(red_source_dim, g[dim], source_dim);
	red_source_dim = source_dim / red_source_dim;
	multiply_grid(red_source_dim, g, dest, dest_num_rows,
		      dims + 1 /* parameter divisor */);

	g[dim] /= source_dim;
      }
      TRACE(cerr << "dest after dividing grid:" << endl);
      TRACE(dest.ascii_dump(cerr));
    }

    // Invert and transpose the source row at `source_index' into the
    // destination row at `dest_index'.
    //
    // Consider each dimension `dim_fol' that follows `dim', as the
    // rows in `dest' that follow row `dest_index' are zero at index
    // `dim'.
    dimension_type tem_source_index = source_index;
    if (dim_kinds[dim] != EQUALITY)
      ++dest_index;
    for (dimension_type dim_fol = dim + 1; dim_fol < dims; ++dim_fol) {
      TRACE(cerr << "  dim_fol: " << dim_fol);
      TRACE(cerr << "  dest_index: " << dest_index);
      TRACE(cerr << "  tem_source_index: " << tem_source_index << endl);
      if (dim_kinds[dim_fol] != CON_VIRTUAL) {
	--tem_source_index;
	TEMP_INTEGER(source_dim);
	source_dim = source[tem_source_index][dim];
	TRACE(cerr << "  rows:" << endl);
	// In order to compute the transpose of the inverse of
	// `source', subtract source[tem_source_index][dim] times the
	// column vector in `dest' at `dim' from the column vector in
	// `dest' at `dim_fol'.
	//
	// I.e., for each row `dest_index' in `dest' that is above the
	// row `dest_index', subtract dest[tem_source_index][dim]
	// times the entry `dim' from the entry at `dim_fol'.
	for (dimension_type row = dest_index; row-- > 0; ) {
	  assert(row < dest_num_rows);
	  TRACE(cerr << "       " << row << endl);
	  Grid_Generator& g = dest[row];
	  g[dim_fol] -= source_dim * g[dim];
	}
      }
    }
    TRACE(cerr << "dest after processing preceding rows:" << endl);
    TRACE(dest.ascii_dump(cerr));
  }

  assert(upper_triangular(dest, dim_kinds));

#ifdef STRONG_REDUCTION
  for (dimension_type dim = 0, i = 0; dim < dims; ++dim)
    if (dim_kinds[dim] != GEN_VIRTUAL)
      // Factor the "diagonal" generator out of the preceding rows.
      reduce_reduced<Grid_Generator_System, Grid_Generator>
	(dest, dim, i++, dim, dims - 1, dim_kinds);
  TRACE(cerr << "dest after strong reduction:" << endl);
  TRACE(dest.ascii_dump(cerr));
#endif

  // Ensure that the parameter divisors are the same as the divisor of
  // the point.
  Coefficient_traits::const_reference system_divisor = dest[0][0];
  for (dimension_type row = 1, dim = 1; dim < dims; ++dim)
    switch (dim_kinds[dim]) {
    case PARAMETER:
      dest[row].divisor() = system_divisor;
    case LINE:
      ++row;
    case GEN_VIRTUAL:
      break;
    }
  TRACE(cerr << "dest after updating param divisors:" << endl);
  TRACE(dest.ascii_dump(cerr));

  trace_dim_kinds("cgs to gs end ", dim_kinds);

  TRACE(cerr << "------------------- cgs to gs conversion done." << endl);
}

#undef TRACE

} // namespace Parma_Polyhedra_Library
