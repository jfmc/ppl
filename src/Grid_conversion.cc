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

#ifndef NDEBUG
// If \p sys is the identity matrix return true, else return false.
bool
identity(Matrix& sys, dimension_type num_cols) {
  dimension_type row_num = sys.num_rows();
  if (row_num != num_cols)
    return false;
  while (row_num--) {
    Row& row = sys[row_num];
    if (row[row_num] != 1)
      return false;
    dimension_type col = 0;
    while (col < row_num)
      if (row[col++] != 0)
	return false;
    while (++col < num_cols)
      if (row[col] != 0)
	return false;
  }
  return true;
}
#endif

// x 0 0 0
// x x 0 0
// x x x 0
// x x x x
bool
Grid::lower_triangular(const Congruence_System& sys) {
  dimension_type num_cols = sys.num_columns() - 1;
  dimension_type rowi = sys.num_rows();
  // Check squareness.
  if (rowi != num_cols)
    return false;

  // Check triangularity.
  --rowi;			// Convert to index.
  // Check diagonal element of last row.
  if (sys[rowi][rowi] == 0)
    return false;
  while (rowi > 0) {
    dimension_type col = rowi--;
    const Congruence& row = sys[rowi];
    // Check elements following diagonal.
    while (col < num_cols)
      if (row[col++] != 0)
	return false;
    // Check diagonal.
    if (row[rowi] == 0)
      return false;
  }

  return true;
}

// x x x x
// 0 x x x
// 0 0 x x
// 0 0 0 x
bool
Grid::upper_triangular(const Generator_System& sys) {
  dimension_type num_cols = sys.num_columns();
  dimension_type rowi = sys.num_rows();
  // Check squareness.
  if (rowi != num_cols) {
    std::cout << "square fail " << rowi << num_cols << std::endl;
    return false;
  }

  // Check triangularity.
  --rowi;			// Convert to index.
  // Check diagonal element of first row.
  if (sys[0][0] == 0) {
    std::cout << "first fail" << std::endl;
    return false;
  }
  while (rowi > 0) {
    const Generator& row = sys[rowi];
    // Check diagonal.
    if (row[rowi] == 0) {
      std::cout << "diag " << rowi << std::endl;
      return false;
    }
    // Check elements preceding diagonal.
    dimension_type col = --rowi;
    do {
      if (row[col] != 0) {
	std::cout << "prec " << rowi << std::endl;
	return false;
      }
    } while (col-- > 0);
  }

  return true;
}

/* The next two methods should be named convert, and this file
   Grid_convert.cc, to use verbs consistently as function and method
   names.  The same holds for the Polyhedron equivalents.  */

dimension_type
Grid::conversion(Congruence_System& source, Linear_System& dest) {
  ctrace << "============= convert cgs to gs" << std::endl
	 << "source:" << std::endl;
  ctrace_dump(source);
  ctrace << "dest:" << std::endl;
  ctrace_dump(dest);

  // Quite similar to the congruence to parameter version below.
  // Changes here may be needed there too.

  assert(dest.num_rows() == source.num_rows());
  assert(dest.num_columns() == source.num_columns() - 1);
  assert(lower_triangular(source));
  assert(identity(dest, dest.num_columns()));

  // Compute the LCM of the diagonals.  Use the LCM instead of the
  // determinant (which is the product of the diagonals) to produce
  // smaller numbers.
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = source[0][0];
  dimension_type num_rows = source.num_rows();
  dimension_type col = num_rows;
  while (--col > 0 && diagonal_lcm != 0)
    lcm_assign(diagonal_lcm, source[col][col]);

  ctrace << "  diagonal_lcm: " << diagonal_lcm << std::endl;

  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  // Initialize diagonal elements and row types in `dest'.
  while (col < num_rows) {
    Congruence& row = source[col];
    if (row.is_virtual())
      dest[col].set_is_line_or_equality();
    else if (row.is_equality())
      dest[col].set_is_virtual();
    else {
      // `row' is a congruence.
      dest[col].set_is_ray_or_point_or_inequality();
      // Set the diagonal element to the equivalent source diagonal,
      // reduced relative to the other source diagonal elements.
      dest[col][col] = diagonal_lcm / source[col][col];
    }
    ++col;
  }

  ctrace << "dest after init:" << std::endl;
  ctrace_dump(dest);

  for (col = 0; col < num_rows; ++col) {
    ctrace << "col: " << col << std::endl;
    /* Change the destination from the source rows that precede row
       `col'.
       FIX
       FIX Matrix is lower triangular so loop counter i starts at col-1
    */
    TEMP_INTEGER(source_diag);
    source_diag = source[col][col];
    ctrace << "  source_diag " << source_diag << std::endl;
    dimension_type row = col;
    while (row > 0) {
      row--;
      ctrace << "  row " << row << std::endl;
      ctrace_dump(dest);

      Linear_Row& gen = dest[row];

      if (gen.is_virtual())
	continue;

      TEMP_INTEGER(multiplier);
      // FIX multiplier like reduced source_diag (wrt assoc ele in dest row num row)
      // FIX does it hold the relationship b/w these ele's?
      gcd_assign(multiplier, gen[col], source_diag);
      multiplier = source_diag / multiplier;
      ctrace << "    multiplier " << multiplier << std::endl;

      /* Multiply the destination grid by the multiplier.  Only
	 multiply parameters, as lines are equivalent under
	 multiplication and the virtual rows just ensure a square
	 matrix.  */
      if (multiplier != 1)
	if (gen.is_virtual() || gen.is_line_or_equality())
	  // Multiply every element of the equality.
	  for (dimension_type column = 0; column < num_rows; ++column)
	    gen[column] *= multiplier;
	else if (gen.is_ray_or_point_or_inequality())
	  // Multiply every element of every parameter.
	  for (dimension_type index = 0; index < num_rows; ++index) {
	    Linear_Row& generator = dest[index];
	    if (generator.is_virtual() || generator.is_line_or_equality())
	      continue;
	    for (dimension_type column = 0; column < num_rows; ++column)
	      generator[column] *= multiplier;
	  }
#ifndef NDEBUG
	else
	  throw std::runtime_error("PPL internal error: Grid conversion: failed to match row type.");
#endif

      gen[col] /= source_diag;
    }
    ctrace << "dest after processing following rows:" << std::endl;
    ctrace_dump(dest);

    /* Change the destination from the source rows that follow the
       current column.

       FIX

       Keep in mind square.
    */
    for (dimension_type index = col + 1; index < num_rows; ++index) {
      ctrace << "  index: " << index << " col: " << col << std::endl;
      TEMP_INTEGER(source_col);
      source_col = source[index][col];
      ctrace << "  rows:" << std::endl;
      // Matrix is upper triangular so row starts at col.
      dimension_type row = col;
      while (1) {
	ctrace << "       " << row << std::endl;
	Linear_Row& dest_row = dest[row];
	if (dest_row.is_virtual() == false)
	  dest_row[index] -= (source_col * dest_row[col]);
	if (row == 0) break;
	row--;
      }
    }
    ctrace << "dest after processing preceding rows:" << std::endl;
    ctrace_dump(dest);
  }

  ctrace << "------------------- cgs to gs conversion done." << std::endl;

  return 0; // FIX
}

dimension_type
Grid::conversion(Generator_System& source, Congruence_System& dest) {
  ctrace << "============= convert gs to cgs" << std::endl
	<< "source:" << std::endl;
  ctrace_dump(source);
  ctrace << "dest:" << std::endl;
  ctrace_dump(dest);

  // Quite similar to the parameter to congruence version above.
  // Changes here may be needed there too.

  assert(dest.num_rows() == source.num_rows());
  assert(dest.num_columns() - 1 == source.num_columns());
  assert(upper_triangular(source));
  assert(identity(dest, dest.num_columns() - 1 /* modulus */));

  // Compute the LCM of the diagonals.  Use the LCM instead of the
  // determinant (which is the product of the diagonals) to produce
  // smaller numbers.
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = source[0][0];
  dimension_type num_rows = source.num_rows();
  dimension_type col = num_rows;
  while (--col > 0 && diagonal_lcm != 0)
    lcm_assign(diagonal_lcm, source[col][col]);

  ctrace << "  diagonal_lcm: " << diagonal_lcm << std::endl;

  // The source matrix must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  // Initialize destination matrix diagonal elements and row types.
  while (col < num_rows) {
    ctrace << "   init col " << col << std::endl;
    Linear_Row& row = source[col];
    if (row.is_virtual()) {
      ctrace << "     virtual" << std::endl;
      dest[col][num_rows] = 0;	// An equality congruence.
    }
    else if (row.is_ray_or_point_or_inequality()) {
      ctrace << "     rpi" << std::endl;
      dest[col][num_rows] = 1;	// A congruence.
      dest[col][col] = diagonal_lcm / source[col][col];
    }
    else if (row.is_line_or_equality())
      dest[col].set_is_virtual();
#ifndef NDEBUG
    else
      throw std::runtime_error("PPL internal error: Grid conversion: failed to match row type.");
#endif
    ++col;
  }

  ctrace << "dest after init:" << std::endl;
  ctrace_dump(dest);

  while (col) {
    col--;
    ctrace << "col: " << col << std::endl;
    /* Change from the source rows that follow the current column.
       FIX
       FIX Matrix is upper triangular so the row starts at col+1
    */
    TEMP_INTEGER(source_diag);
    source_diag = source[col][col];
    for (dimension_type row = col + 1; row < num_rows; ++row) {
      ctrace << "  row " << row << std::endl;
      ctrace_dump(dest);

      Congruence& cg = dest[row];
      if (cg.is_virtual())
	continue;

      TEMP_INTEGER(multiplier);
      // FIX multiplier like reduced source_diag (wrt assoc ele in dest row num row)
      // FIX does it hold the relationship b/w these ele's?
      gcd_assign(multiplier, cg[col], source_diag);
      multiplier = source_diag / multiplier;

      /* Multiply the desination grid by the multiplier.  Only
	 multiply congruences, as equalities are equivalent under
	 multiplication and the virtual rows just ensure a regular
	 matrix.  */
      if (multiplier != 1)
	if (cg.is_congruence())
	  // Multiply every element of every congruence.
	  for (dimension_type index = 0; index < num_rows; ++index) {
	    Congruence& congruence = dest[index];
	    if (congruence.is_congruence())
	      for (dimension_type column = 0; column < num_rows; ++column)
		congruence[column] *= multiplier;
	  }
	else if (cg.is_equality())
	  // Multiply every element of the equality.
	  for (dimension_type column = 0; column < num_rows; ++column)
	    cg[column] *= multiplier;

      cg[col] /= source_diag;
    }
    ctrace << "dest after processing following rows:" << std::endl;
    ctrace_dump(dest);

    /* Change from the source rows that precede the current column.
       FIX

       Keep in mind square.

       For every source col above in every row below the equiv row in
       the dest subtract the product of the source col and the same
       col in the destination from the dest element vertically
       symmetrical in the dest row to that dest element.
    */
    dimension_type index = col;
    while (index) {
      index--;
      ctrace << "  index: " << index << " col: " << col << std::endl;
      TEMP_INTEGER(source_col);
      source_col = source[index][col];
      ctrace << "  rows:" << std::endl;
      for (dimension_type row = col; row < num_rows; ++row) {
	ctrace << "       " << row << std::endl;
	Congruence& cg = dest[row];
	if (cg.is_virtual())
	  continue;
	cg[index] -= (source_col * cg[col]);
      }
    }
    ctrace << "dest after processing preceding rows:" << std::endl;
    ctrace_dump(dest);
  }
  // Set the modulus in every congruence.
  TEMP_INTEGER(modulus);
  modulus = dest[0][0];
  for (dimension_type row = 0; row < num_rows; ++row) {
    Congruence& cg = dest[row];
    if (cg.is_congruence())
      cg[num_rows] = modulus;
  }
  ctrace << "dest after setting moduli:" << std::endl;
  ctrace_dump(dest);

  ctrace << "------------------- gs to cgs conversion done." << std::endl;

  return 0; // FIX
}

} // namespace Parma_Polyhedra_Library
