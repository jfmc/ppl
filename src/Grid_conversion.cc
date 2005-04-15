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
#include "Saturation_Row.defs.hh"
#include "Saturation_Matrix.defs.hh"
#include "Grid.defs.hh"
#include "globals.defs.hh"
#include <cstddef>

namespace PPL = Parma_Polyhedra_Library;

// FIX always include (external in globals.defs.hh?)
// FIX Temporary tracing stream.
#if 1
#include <iostream>
std::ostream& trace = std::cout;
using std::endl;
#else
#include <fstream>
std::ofstream trace;
#endif

/* FIX Name this method convert and name this file Grid_convert.cc, to
   use verbs consistently as function names.  Make the same changes to
   the Polyhedron equivalents. */

/*! FIX
  \return
  The number of lines of the polyhedron or the number of equality
  congruences in the result of conversion.

  \param source
  The system to use to convert \p dest: it may be modified;

  \param start
  The index of \p source row from which conversion begin;

  \param dest
  The result of the conversion;

  \param sat
  The saturation matrix telling us, for each row in \p source, which
  are the rows of \p dest that satisfy but do not saturate it;

  \param num_lines_or_equalities
  The number of rows in the system \p dest that are either lines of
  the polyhedron (when \p dest is a system of generators) or equality
  congruences (when \p dest is a system of congruences).

  For simplicity, all the following comments assume we are converting a
  congruence system \p source to a generator system \p dest;
  the comments for the symmetric case can be obtained by duality.

  If some of the congruences...
*/
// FIXME: I am not sure we do enough here in avoiding unwanted
// common factors.  -- PMH
PPL::dimension_type
PPL::Grid::conversion(Congruence_System& source,
		      // FIX params
		      const dimension_type start, // FIX
		      Linear_System& dest,
		      Saturation_Matrix& sat,
		      dimension_type num_lines_or_equalities) {
  trace << "============= convert cgs to gs" << std::endl
	<< "source:" << std::endl;
  source.ascii_dump(trace);
  trace << "dest:" << std::endl;
  dest.ascii_dump(trace);

  // Quite similar to the Congruence_System to Linear_System version
  // below.  Changes here may be needed there too.

  // FIX assert source,dest square
  // FIX assert(upper_triangular_and_regular(source));

  // Compute the LCM of the diagonals.  Use the LCM instead of the
  // determinant (which is the product of the diagonals) to produce
  // smaller numbers.
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = source[0][0]; // FIX assumes elements
  dimension_type num_rows = source.num_rows();
  dimension_type col = num_rows;
  while (--col > 0 && diagonal_lcm != 0)
    lcm_assign(diagonal_lcm, source[col][col]);

  trace << "  diagonal_lcm: " << diagonal_lcm << std::endl;

  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  // Initialize destination matrix diagonal elements and row types.
  while (col < num_rows) {
    Congruence& row = source[col];
    if (virtual_row(row))
      dest[col].set_is_line_or_equality();
    else if (row.is_equality())
      mark_virtual(dest[col]);
    else {
      // `row' is a congruence.
      dest[col].set_is_ray_or_point_or_inequality();
      // Set the diagonal element to the equivalent source diagonal,
      // reduced relative to the other source diagonal elements.
      dest[col][col] = diagonal_lcm / source[col][col];
    }
    ++col;
  }

  trace << "dest after init:" << std::endl;
  dest.ascii_dump(trace);

  for (col = 0; col < num_rows; ++col) {
    trace << "col: " << col << std::endl;
    /* Change the destination from the source rows that precede row
       `col'.
       FIX
       FIX Matrix is lower triangular so loop counter i starts at col-1
    */
    TEMP_INTEGER(source_diag);
    source_diag = source[col][col];
    trace << "  source_diag " << source_diag << std::endl;
    dimension_type row = col;
    while (row > 0) {
      row--;
      trace << "  row " << row << std::endl;
      dest.ascii_dump(trace);

      Linear_Row& gen = dest[row];

      if (virtual_row(gen))
	continue;

      TEMP_INTEGER(multiplier);
      // FIX multiplier like reduced source_diag (wrt assoc ele in dest row num row)
      // FIX does it hold the relationship b/w these ele's?
      // FIX return the gcd from gcd_assign?
      //multiplier = source_diag / gcd(dest[row][col], source_diag);
      gcd_assign(multiplier, gen[col], source_diag);
      multiplier = source_diag / multiplier;
      trace << "    multiplier " << multiplier << std::endl;

      /* Multiply the desination grid by the multiplier.  FIX Only
	 multiply congruences, as equalities are equivalent under
	 multiplication and the virtual rows just ensure a regular
	 matrix.  */
      if (virtual_row(gen) /*FIX?*/ || gen.is_line_or_equality())
	// Multiply every element of the equality.
	for (dimension_type column = 0; column < num_rows; ++column)
	  gen[column] *= multiplier;
      else if (gen.is_ray_or_point_or_inequality())
	// Multiply every element of every parameter.
	for (dimension_type index = 0; index < num_rows; ++index) {
	  Linear_Row& generator = dest[index];
	  if (virtual_row(generator) || generator.is_line_or_equality())
	    continue;
	  for (dimension_type column = 0; column < num_rows; ++column)
	    generator[column] *= multiplier;
	}
      else
	throw std::runtime_error("PPL internal error: Grid conversion: failed to match row type.");

      gen[col] /= source_diag;
    }
    trace << "dest after processing following rows:" << std::endl;
    dest.ascii_dump(trace);

    /* Change the destination from the source rows that follow the
       current column.

    FIX

       Keep in mind square.

    FIX (for gs to cs)
       For every source col above in every row below the equiv row in
       the dest subtract the product of the source col and the same
       col in the destination from the dest element vertically
       symmetrical in the dest row to that dest element.
    */
    // FIX (gs to cgs) ~~ index is the column/row in the source
    for (dimension_type index = col + 1; index < num_rows; ++index) {
      trace << "  index: " << index << " col: " << col << std::endl;
      TEMP_INTEGER(source_col); // FIX name
      source_col = source[index][col];
      trace << "  rows:" << std::endl;
      // Matrix is upper triangular so row starts at col.
      dimension_type row = col;
      while (1) {
	trace << "       " << row << std::endl;
	Linear_Row& dest_row = dest[row];
	if (virtual_row(dest_row) == false)
	  dest_row[index] -= (source_col * dest_row[col]);
	if (row == 0) break;
	row--;
      }
    }
    trace << "dest after processing preceding rows:" << std::endl;
    dest.ascii_dump(trace);
  }

  trace << "------------------- cgs to gs conversion done." << std::endl;

  return 0; // FIX
}

// FIXME: I am not sure we do enough here in avoiding unwanted
// common factors.  -- PMH
PPL::dimension_type
PPL::Grid::conversion(Generator_System& source,
		      // FIX params
		      const dimension_type start,
		      Congruence_System& dest,
		      Saturation_Matrix& sat,
		      dimension_type num_lines_or_equalities) {
  trace << "============= convert gs to cgs" << std::endl
	<< "source:" << std::endl;
  source.ascii_dump(trace);
  trace << "dest:" << std::endl;
  dest.ascii_dump(trace);

  // Quite similar to the Linear_System to Congruence_System version
  // above.  Changes here may be needed there too.

  // FIX assert source,dest square
  // FIX assert dest an identity matrix
  // FIX assert dest smaller than or same size as source
  // FIX assert(upper_triangular_and_regular(source));

  // Compute the LCM of the diagonals.  Use the LCM instead of the
  // determinant (which is the product of the diagonals) to produce
  // smaller numbers.
  TEMP_INTEGER(diagonal_lcm);
  diagonal_lcm = source[0][0]; // FIX assumes elements
  dimension_type num_rows = source.num_rows();
  dimension_type col = num_rows;
  while (--col > 0 && diagonal_lcm != 0)
    lcm_assign(diagonal_lcm, source[col][col]);

  trace << "  diagonal_lcm: " << diagonal_lcm << std::endl;

  // The source matrix must be regular.
  if (diagonal_lcm == 0)
    throw std::runtime_error("PPL internal error: Grid::conversion: source matrix singular.");

  // Initialize destination matrix diagonal elements and row types.
  while (col < num_rows) {
    trace << "   init col " << col << std::endl;
    Linear_Row& row = source[col];
    if (virtual_row(row)) {
      trace << "     virtual" << std::endl;
      dest[col][num_rows] = 0; // An equality congruence.
    }
    else if (row.is_ray_or_point_or_inequality()) {
      trace << "     rpi" << std::endl;
      dest[col][num_rows] = 1 /* FIX */; // A congruence.
      dest[col][col] = diagonal_lcm / source[col][col];
    }
    else if (row.is_line_or_equality())
      mark_virtual(dest[col]);
    else
      throw std::runtime_error("PPL internal error: Grid conversion: failed to match row type.");
    ++col;
  }

  trace << "dest after init:" << std::endl;
  dest.ascii_dump(trace);

  while (col) {  // FIX why was this --col? (added next line)
    col--;
    trace << "col: " << col << std::endl;
    /* Change from the source rows that follow the current column.
       FIX
       FIX Matrix is upper triangular so the row starts at col+1
    */
    for (dimension_type row = col + 1; row < num_rows; ++row) {
      trace << "  row " << row << std::endl;
      dest.ascii_dump(trace);

      if (virtual_row(dest[row]))
	continue;

      // FIX why inside loop? perhaps source changes?
      TEMP_INTEGER(source_diag);
      source_diag = source[col][col];

      TEMP_INTEGER(multiplier);
      // FIX multiplier like reduced source_diag (wrt assoc ele in dest row num row)
      // FIX does it hold the relationship b/w these ele's?
      // FIX return the gcd from gcd_assign?
      //multiplier = source_diag / gcd(dest[row][col], source_diag);
      gcd_assign(multiplier, dest[row][col], source_diag);
      multiplier = source_diag / multiplier;

      /* Multiply the desination grid by the multiplier.  FIX Only
	 multiply congruences, as equalities are equivalent under
	 multiplication and the virtual rows just ensure a regular
	 matrix.  */
      // FIX move up, use cg twice above and once below
      Congruence& cg = dest[row];
      // FIX skip if mult is 1 // FIX and in other
      if (virtual_row(cg) /*FIX?*/ || cg.is_equality())
	// Multiply every element of the equality.
	for (dimension_type column = 0; column < num_rows; ++column)
	  cg[column] *= multiplier;
      else
	// Multiply every element of every congruence.
	for (dimension_type index = 0; index < num_rows; ++index) {
	  Congruence& congruence = dest[index];
	  if (virtual_row(congruence) || congruence.is_equality())
	    continue;
	  for (dimension_type column = 0; column < num_rows; ++column)
	    congruence[column] *= multiplier;
	}

      // FIX skip if mult is 1 // FIX and in other
      dest[row][col] /= source_diag;
    }
    trace << "dest after processing following rows:" << std::endl;
    dest.ascii_dump(trace);

    /* Change from the source rows that precede the current column.
       FIX

       Keep in mind square.

       For every source col above in every row below the equiv row in
       the dest subtract the product of the source col and the same
       col in the destination from the dest element vertically
       symmetrical in the dest row to that dest element.
    */
    // FIX ~~ index is the column/row in the source
    dimension_type index = col;
    while (index) {
      index--;
      trace << "  index: " << index << " col: " << col << std::endl;
      TEMP_INTEGER(source_col); // FIX name
      source_col = source[index][col];
      trace << "  rows:" << std::endl;
      for (dimension_type row = col; row < num_rows; ++row) {
	trace << "       " << row << std::endl;
	// FIX use temp for dest[row]
	if (virtual_row(dest[row]))
	  continue;
	dest[row][index] -= (source_col * dest[row][col]);
      }
    }
    trace << "dest after processing preceding rows:" << std::endl;
    dest.ascii_dump(trace);
  }
  TEMP_INTEGER(modulus);
  modulus = dest[0][0];
  for (dimension_type row = 0; row < num_rows; ++row) {
    Congruence& cg = dest[row];
    if (virtual_row(cg) || cg.is_equality())
      continue;
    cg[num_rows] = modulus;
  }
  trace << "dest after adding moduli:" << std::endl;
  dest.ascii_dump(trace);

  trace << "------------------- gs to cgs conversion done." << std::endl;

  return 0; // FIX
}
