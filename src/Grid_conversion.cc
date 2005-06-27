/* Grid class implementation: conversion().
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

#include "Linear_Row.defs.hh"
#include "Linear_System.defs.hh"
#include "Grid.defs.hh"
#include "globals.defs.hh"
#include <cstddef>

// FIX Temporary tracing stream.
#if 0
#include <iostream>
std::ostream& ctrace = std::cout;
#define ctrace_dump(sys) sys.ascii_dump(ctrace)
#else
#include <fstream>
std::ofstream ctrace;
#define ctrace_dump(sys)
#endif

namespace Parma_Polyhedra_Library {

// X 0 0 0  upside down, so  x x x X
// x X 0 0                   x x X 0
// x x X 0                   x X 0 0
// x x x X                   X 0 0 0
//
// Any of the x's can be zeros.
bool
Grid::lower_triangular(const Congruence_System& sys,
		       const Dimension_Kinds& dim_kinds) {
  dimension_type num_cols = sys.num_columns() - 1;
  dimension_type row = sys.num_rows();

  // Check squareness.
  if (row > num_cols)
    return false;

  // Check triangularity.
  for (dimension_type dim = 0; dim < num_cols; ++dim) {
    if (dim_kinds[dim] == CON_VIRTUAL)
      continue;
    const Congruence& cg = sys[--row];
    // Check diagonal.
    if (cg[dim] == 0)
      return false;
    // Check elements following diagonal.
    dimension_type col = dim;
    while (++col < num_cols)
      if (cg[col] != 0)
	return false;
  }
  assert(row == 0);

  return true;
}

// X x x x
// 0 X x x
// 0 0 X x
// 0 0 0 X
//
// Any of the x's can be zeros.
bool
Grid::upper_triangular(const Generator_System& sys,
		       const Dimension_Kinds& dim_kinds) {
  dimension_type num_cols = sys.num_columns();
  dimension_type row = sys.num_rows();

  // Check squareness.
  if (row > num_cols)
    return false;

  // Check triangularity.
  while (num_cols > 0) {
    num_cols--;
    if (dim_kinds[num_cols] == GEN_VIRTUAL)
      continue;
    const Generator& gen = sys[--row];
    // Check diagonal.
    if (gen[num_cols] == 0)
      return false;
    // Check elements preceding diagonal.
    dimension_type col = num_cols;
    while (col-- > 0)
      if (gen[col] != 0)
	return false;
  }
  assert(num_cols == row);

  return true;
}

inline void
multiply_grid(const Coefficient& multiplier, Linear_Row& gen,
	      Linear_System& dest, const dimension_type num_rows,
	      const dimension_type num_dims) {
  if (multiplier == 1)
    return;

  if (gen.is_line_or_equality())
    // Multiply every element of the equality.
    for (dimension_type column = 0; column < num_dims; ++column)
      gen[column] *= multiplier;
  else {
    assert(gen.is_ray_or_point_or_inequality());
    // Multiply every element of every parameter.
    for (dimension_type index = 0; index < num_rows; ++index) {
      Linear_Row& generator = dest[index];
      if (generator.is_ray_or_point_or_inequality())
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

/* The next two methods should be named convert, and this file
   Grid_convert.cc, to use verbs consistently as function and method
   names.  The same holds for the Polyhedron equivalents.  */

dimension_type
Grid::conversion(Generator_System& source, Congruence_System& dest,
		 Dimension_Kinds& dim_kinds) {
  ctrace << "============= convert gs to cgs" << std::endl
	<< "source:" << std::endl;
  ctrace_dump(source);
  ctrace << "dest:" << std::endl;
  ctrace_dump(dest);
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
  const dimension_type dims = source.num_columns();
  for (dimension_type dim = 0; dim < dims; ++dim)
    if (dim_kinds[dim] == GEN_VIRTUAL)
      // Virtual generators map to equalities.
      ++dest_num_rows;
    else {
      if (dim_kinds[dim] == PARAMETER) {
	// Dimension `dim' has a parameter row at `source_num_rows' in
	// `source', so include in `diagonal_lcm' the `dim'th element
	// of that row.
	lcm_assign(diagonal_lcm, source[source_num_rows][dim]);
	// Parameters map to proper congruences.
	++dest_num_rows;
      }
      // Lines map to virtual congruences.
      ++source_num_rows;
    }
  ctrace << "diagonal_lcm: " << diagonal_lcm << std::endl
	 << "source_num_rows: " << source_num_rows << std::endl
	 << "dest_num_rows: " << dest_num_rows << std::endl;

  // `source' must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  dest.resize_no_copy(dest_num_rows, dims + 1 /* moduli */);

  // Initialize elements and row types in `dest'.
  dimension_type source_index = 0, dest_index = dest_num_rows - 1;
  for (dimension_type dim = 0; dim < dims; ++dim) {
    ctrace << "init dim " << dim << std::endl;
    if (dim_kinds[dim] == LINE) {
      ctrace << "  line" << std::endl;
      ++source_index;
    }
    else {
      Congruence& cg = dest[dest_index];
      for (dimension_type j = 0; j < dim; j++)
	cg[j] = 0;
      for (dimension_type j = dim + 1; j < dims; j++)
	cg[j] = 0;

      if (dim_kinds[dim] == GEN_VIRTUAL) {
	ctrace << "  gen_virtual" << std::endl;
	cg[dims] = 0;		// An equality.
	cg[dim] = 1;
      }
      else {
	assert(dim_kinds[dim] == PARAMETER);
	ctrace << "  parameter" << std::endl;
	cg[dims] = 1;		// A proper congruence.
	// Set the destination diagonal element to have the same size
	// relative to the other diagonals as the corresponding
	// element in the source has to the other source diagonals.
	cg[dim] = diagonal_lcm / source[source_index][dim];
	++source_index;
      }
      --dest_index;
    }
  }

  ctrace << "dest after init:" << std::endl;
  ctrace_dump(dest);

  // Convert.
  //
  // `source_index' and `dest_index' hold positions of pivot rows in
  // `source' and `dest'.  The order of the rows in `dest' is the
  // reverse of the order in `source', so the rows are iterated from
  // top to bottom in `source' and from bottom to top in `dest'.
  source_index = source_num_rows;
  dest_index = 0;

  for (dimension_type dim = dims; dim-- > 0; ) {
    ctrace << "dim: " << dim << std::endl;

    if (dim_kinds[dim] != GEN_VIRTUAL) {
      --source_index;
      TEMP_INTEGER(source_dim);
      source_dim = source[source_index][dim];

      // For each row in `dest' that is above the `dest_index'...
      for (dimension_type row = dest_index; row-- > 0; ) {
	ctrace << "  row " << row << std::endl;
	ctrace_dump(dest);

	Congruence& cg = dest[row];

	// Multiply `dest' by the minimum amount so that integer
        // division of entry `dim' at `row' (FIX by the corresponding
        // entry in `source'?) is exact.
	TEMP_INTEGER(multiplier);
	gcd_assign(multiplier, cg[dim], source_dim);
	// FIX multiplier like reduced source_dim (wrt assoc ele in dest row num row)
	// FIX does it hold the relationship b/w these ele's?
	multiplier = source_dim / multiplier;
	multiply_grid(multiplier, cg, dest, dest_num_rows, dims);

	// FIX Set entry `dim' at `row' in `dest' to the smallest
	// possible integer value such that the corresponding entry in
	// `source' could be made an integer such that `source' and
	// `dest' would then have the same relative sizes that they
	// had before the mult_grid.
	cg[dim] /= source_dim;
      }
      ctrace << "dest after multiplying grid:" << std::endl;
      ctrace_dump(dest);
    }

    // Consider each dimension `dim_prec' that precedes `dim', as the
    // ones that follow have zeros in `dest' below `dest_index'.
    dimension_type tem_source_index = source_index; // FIX name
    if (dim_kinds[dim] != LINE)
      ++dest_index;
    for(dimension_type dim_prec = dim; dim_prec-- > 0; ) {
      ctrace << "  dim_prec: " << dim_prec
	     << "  dest_index: " << dest_index
	     << "  tem_source_index: " << tem_source_index << std::endl;
      if (dim_kinds[dim_prec] != GEN_VIRTUAL) {
	--tem_source_index;
	TEMP_INTEGER(source_dim);
	source_dim = source[tem_source_index][dim];
	ctrace << "  rows:" << std::endl;
	// In order to compute the transpose of the inverse of
	// `source', subtract source[tem_source_index][dim] times the
	// column vector in `dest' at `dim' from the column vector in
	// `dest' at `dim_prec'.
	//
	// I.e., for each row `dest_index' in `dest' that is below the
	// row npiv, subtract dest[tem_source_index][dim] times the
	// entry `dim' from the entry at `dim_prec'.
	for (dimension_type row = dest_index; row-- > 0; ) {
	  assert(row < dest_num_rows);
	  ctrace << "       " << row << std::endl;
	  Congruence& cg = dest[row];
	  cg[dim_prec] -= source_dim * cg[dim];
	}
      }
    }

    ctrace << "dest after processing preceding rows:" << std::endl;
    ctrace_dump(dest);
  }
  // Set the modulus in every congruence.
  TEMP_INTEGER(modulus);
  Congruence& cg = dest[dest_num_rows - 1];
  modulus = cg[0];
  if (modulus < 0) {
    modulus = -modulus;
    cg[0] = modulus;
  }
  for (dimension_type row = 0; row < dest_num_rows; ++row) {
    Congruence& cg = dest[row];
    if (cg[dims] > 0)
      // `cg' is a proper congruence.
      cg[dims] = modulus;
  }
  ctrace << "dest after setting moduli:" << std::endl;
  ctrace_dump(dest);

  trace_dim_kinds("gs to cgs end ", dim_kinds);

  ctrace << "------------------- gs to cgs conversion done." << std::endl;

  return 0; // FIX
}

dimension_type
Grid::conversion(Congruence_System& source, Linear_System& dest,
		 Dimension_Kinds& dim_kinds) {
  ctrace << "============= convert cgs to gs" << std::endl
	 << "source:" << std::endl;
  ctrace_dump(source);
  ctrace << "dest:" << std::endl;
  ctrace_dump(dest);
  trace_dim_kinds("cgs to gs ", dim_kinds);

  // Quite similar to the generator to congruence version above.
  // Changes here may be needed there too.

  assert(lower_triangular(source, dim_kinds));

  // Initialise matrix row number counters and compute the LCM of the
  // diagonal entries of the parameters in `source'.
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
	// Dimension `dim' has a parameter row at `source_num_rows' in
	// `source', so include in `diagonal_lcm' the `dim'th element
	// of that row.
	lcm_assign(diagonal_lcm, source[source_num_rows][dim]);
	// Proper congruences map to parameters.
	++dest_num_rows;
      }
      // Equalities map to virtual generators.
      ++source_num_rows;
    }
  ctrace << "diagonal_lcm: " << diagonal_lcm << std::endl
	 << "source_num_rows: " << source_num_rows << std::endl
	 << "dest_num_rows: " << dest_num_rows << std::endl;

  // `source' must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  dest.set_index_first_pending_row(dest_num_rows);
  dest.resize_no_copy(dest_num_rows, dims);

  // Initialize elements and row types in `dest'.
  //
  // The top-down order of the congruence system rows corresponds to
  // the right-left order of the dimensions.
  dimension_type source_index = 0;
  // The generator system has a bottom-up ordering.
  dimension_type dest_index = dest_num_rows - 1;
  for (dimension_type dim = dims; dim-- > 0;) {
    ctrace << "init dim " << dim << std::endl;
    if (dim_kinds[dim] == EQUALITY) {
      ctrace << "  equality" << std::endl;
      ++source_index;
    }
    else {
      Linear_Row& g = dest[dest_index];
      for (dimension_type j = 0; j < dim; j++)
	g[j] = 0;
      for (dimension_type j = dim + 1; j < dims; j++)
	g[j] = 0;

      if (dim_kinds[dim] == CON_VIRTUAL) {
	ctrace << "  con_virtual" << std::endl;
	g.set_is_line_or_equality();
	g[dim] = 1;
      }
      else {
	assert(dim_kinds[dim] == PROPER_CONGRUENCE);
	ctrace << "  proper_congruence" << std::endl;
	g.set_is_ray_or_point_or_inequality();
	g[dim] = diagonal_lcm / source[source_index][dim];
	++source_index;
      }
      --dest_index;
    }
  }

  ctrace << "dest after init:" << std::endl;
  ctrace_dump(dest);

  // Convert.
  //
  // `source_index' and `dest_index' hold positions of pivot rows in
  // `source' and `dest'.  The order of the rows in `dest' is the
  // reverse of the order in `source', so the rows are iterated from
  // (FIX confirm) top to bottom in `source' and from bottom to top in `dest'.
  source_index = source_num_rows;
  dest_index = 0;

  for (dimension_type dim = 0; dim < dims; ++dim) {
    ctrace << "dim: " << dim << std::endl;

    if (dim_kinds[dim] != CON_VIRTUAL) {
      --source_index;
      TEMP_INTEGER(source_dim);
      source_dim = source[source_index][dim];

      // For each row in `dest' that is above the `dest_index'...
      for (dimension_type row = dest_index; row-- > 0; ) {
	ctrace << "  row " << row << std::endl;
	ctrace_dump(dest);

	Linear_Row& g = dest[row];

	// Multiply `dest' by the minimum amount so that integer
        // division of entry `dim' at `row' (FIX by the corresponding
        // entry in `source'?) is exact.
	TEMP_INTEGER(multiplier);
	gcd_assign(multiplier, g[dim], source_dim);
	// FIX multiplier like reduced source_dim (wrt assoc ele in dest row num row)
	// FIX does it hold the relationship b/w these ele's?
	multiplier = source_dim / multiplier;
	PPL::multiply_grid(multiplier, g, dest, dest_num_rows, dims);

	// FIX Set entry `dim' at `row' in `dest' to the smallest
	// possible integer value such that the corresponding entry in
	// `source' could be made an integer such that `source' and
	// `dest' would then have the same relative sizes that they
	// had before the mult_grid.
	g[dim] /= source_dim;
      }
      ctrace << "dest after multiplying grid:" << std::endl;
      ctrace_dump(dest);
    }

    // Consider each dimension `dim_fol' that precede `dim', as the
    // preceding rows have zeros in `dest' below `dest_index'.
    dimension_type tem_source_index = source_index; // FIX name
    if (dim_kinds[dim] != EQUALITY)
      ++dest_index;
    for(dimension_type dim_fol = dim + 1; dim_fol < dims; ++dim_fol) {
      ctrace << "  dim_fol: " << dim_fol
	     << "  dest_index: " << dest_index
	     << "  tem_source_index: " << tem_source_index << std::endl;
      if (dim_kinds[dim_fol] != CON_VIRTUAL) {
	--tem_source_index;
	TEMP_INTEGER(source_dim);
	source_dim = source[tem_source_index][dim];
	ctrace << "  rows:" << std::endl;
	// In order to compute the transpose of the inverse of
	// `source', subtract source[tem_source_index][dim] times the
	// column vector in `dest' at `dim' from the column vector in
	// `dest' at `dim_fol'.
	//
	// I.e., for each row `dest_index' in `dest' that is below the
	// row npiv, subtract dest[tem_source_index][dim] times the
	// entry `dim' from the entry at `dim_fol'.
	for (dimension_type row = dest_index; row-- > 0; ) {
	  assert(row < dest_num_rows);
	  ctrace << "       " << row << std::endl;
	  Linear_Row& g = dest[row];
	  g[dim_fol] -= source_dim * g[dim];
	}
      }
    }
    ctrace << "dest after processing preceding rows:" << std::endl;
    ctrace_dump(dest);
  }

  trace_dim_kinds("cgs to gs end ", dim_kinds);

  ctrace << "------------------- cgs to gs conversion done." << std::endl;

  return 0; // FIX
}

} // namespace Parma_Polyhedra_Library
