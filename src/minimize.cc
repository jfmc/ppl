/* Polyhedron class implementation: minimize() and add_and_minimize().
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "SatMatrix.defs.hh"
#include "Polyhedron.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

// FOR DEBUGGING PURPOSES ONLY
#include <iostream>

/*!
  \fn static bool PPL::Polyhedron::minimize(bool con_to_ray,
                                            Matrix& source,
				            Matrix& dest,
				            SatMatrix& sat)

  \param con_to_ray   <CODE>true</CODE> if \p source represents the
                      constraints, <CODE>false</CODE> otherwise.
  \param source       The given matrix.
  \param dest         The matrix to build and minimize.
  \param sat          The saturation matrix.

  \return             <CODE>true</CODE> if the polyhedron is empty,
                      <CODE>false</CODE> otherwise.

  \p dest is not <CODE>const</CODE> because it will be built (and then
  modified) during minimize(). Also, \p sat and \p source are
  not <CODE>const</CODE> because the former will be built during
  \p dest creation and the latter will maybe be sorted and modified by
  <CODE>conversion()</CODE> and <CODE>simplify()</CODE>.

  \p sat has the generators on its columns and the constraints on its rows
  if \p con_to_ray is <CODE>true</CODE>, otherwise it has the generators on
  its rows and the constraints on its columns.

  Given \p source, this function builds (by means of
  <CODE>conversion()</CODE>) \p dest and then simplifies (invoking
  <CODE>simplify()</CODE>) \p source, erasing reduntant rows.
  For the sequel we assume that \p source is the matrix of constraints
  and \p dest is the matrix of generators.
  This will simplify the description of the function; the dual case is
  similar.
*/

bool
PPL::Polyhedron::minimize(bool con_to_ray,
			  Matrix& source, Matrix& dest, SatMatrix& sat) {
  // Sort the source matrix, if necessary.
  // This ensures that all the equalities come before the inequalities
  // (the correctness of simplify() relies on this hypothesis).
  if (!source.is_sorted())
    source.sort_rows();

  // Initialization of the matrix of generators `dest'.
  // The algorithm works incrementally and we haven't seen any
  // constraint yet: as a consequence, `dest' should describe
  // the universe polyhedron of the appropriate dimension.
  // To this end, we initialize it to the identity matrix of dimension
  // `source.num_columns()': the rows represent the lines corresponding
  // to the canonical basis of the vector space.

  // Resizing `dest' to be the appropriate square matrix.
  size_t dest_num_rows = source.num_columns();
  dest.resize_no_copy(dest_num_rows, dest_num_rows);

  // Initializing it to the identity matrix.
  for(size_t i = dest_num_rows; i-- > 0; ) {
    for (size_t j = dest_num_rows; j-- > 0; )
      dest[i][j] = 0;
    dest[i][i] = 1;
    // FIXME: why the first row, having divisor 1, is said to be a line?
    dest[i].set_is_line_or_equality();
  }
  // The identity matrix `dest' is not sorted
  // (see the sorting rules in Row.cc).
  dest.set_sorted(false);

  // Building a saturation matrix and initializing it by setting
  // all of its elements to zero. This matrix will be modified together
  // with `dest' during the conversion.
  // NOTE: since we haven't seen any constraint yet, the relevant
  //       portion of `tmp_sat' is the sub-matrix consisting of
  //       the first 0 rows: thus the relevant portion correctly
  //       characterizes the initial saturation information.
  SatMatrix tmp_sat(dest_num_rows, source.num_rows());

  // By invoking the function conversion(), we populate `dest' with
  // the generators characterizing the polyhedron described by all
  // the constraints in `source'.
  // The `start' parameter is zero (we haven't seen any constraint yet)
  // and the 5th parameter (representing the number of lines in `dest'),
  // by construction, is equal to `dest_num_rows'.
  size_t num_lines_or_equalities = conversion(source, 0,
					      dest, tmp_sat,
					      dest_num_rows);
  // conversion() may have modified the number of rows in `dest'.
  dest_num_rows = dest.num_rows();
  // NOTE: conversion() can only remove inequalities from `source'.
  // Thus, all the equalities still come before the inequalities
  // (the correctness of simplify() relies on this hypothesis).

  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no vertices (because rays
  // and lines need a supporting point).
  size_t first_vertex = num_lines_or_equalities;
  for ( ; first_vertex < dest_num_rows; ++first_vertex)
    // Vertices have a positive divisor.
    if (dest[first_vertex][0] > 0)
      break;

  if (first_vertex == dest_num_rows)
    if (con_to_ray)
      // No vertex has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_ray' is false: `dest' is a matrix of constraints.
      // In this case the condition `first_vertex == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      abort();
  else {
    // A vertex has been found: the polyhedron is not empty.
    // Now invoking simplify() to remove all the redundant constraints
    // from the matrix `source'.
    // Since the saturation matrix `tmp_sat' returned by conversion()
    // is a constraint (sat_c) saturation matrix, we have to transpose it
    // to obtain the generator (sat_g) saturation matrix that needs
    // to be passed to the function simplify().
    sat.transpose_assign(tmp_sat);
    simplify(source, sat);
    return false;
  }
}

/*!
  \fn bool PPL::Polyhedron::add_and_minimize(bool con_to_ray,
                                             Matrix& source1,
                                             Matrix& dest,
                                             SatMatrix& sat,
                                             const Matrix& source2)
					
  \param con_to_ray   <CODE>true</CODE> if \p source1 and \p source2
                      are matrix of constraints, <CODE>false</CODE> otherwise.
  \param source1      The first element of the given DD pair.
  \param dest         The second element of the given DD pair.
  \param sat          The saturation matrix that bind \p source1 to \p dest.
  \param source2      The new system of generators or constraints.

  \return             <CODE>true</CODE> if the obtained polyhedron
                      is empty, <CODE>false</CODE> otherwise.

  \p sat has the rows indexed by rows of \p dest and the columns
  indexed by rows of \p source1 (on entry) and rows of the matrix
  obtained merging \p source1 and \p source2 (on exit).

  Let us suppose we want to add some constraints to a given matrix of
  constraints \p source1. This method, given a minimized double description
  pair (\p source1, \p dest) and a matrix of new constraints \p source2,
  builds a new DD pair such that the matrix of constraints is obtained
  merging \p source1 with \p source2. Given the new \p source1, this
  function modifies (using <CODE>conversion()</CODE>) \p dest according
  to the added constraints and then simplifies \p source1 (invoking
  <CODE>simplify</CODE>), erasing the redundant constraints.

  This method treats also the dual case, i.e., adding new generators to
  a previous matrix of generators. In this case \p source1 contains the
  old generators, \p source2 the new ones and \p dest is the matrix
  of constraints in the given minimized DD pair. Like we did in
  conversion.cc we will describe only the case in which \p source1 (and
  then \p source2) contains constraints and \p dest contains generators.
  \p source1 and \p source2 are assumed to be sorted.

  Since \p source2 contains the constraints (or the generators) that
  will be added to \p source1, it is constant: it will not be modified.
*/
bool
PPL::Polyhedron::add_and_minimize(bool con_to_ray,
				  Matrix& source1,
				  Matrix& dest,
				  SatMatrix& sat,
				  const Matrix& source2) {
  // `source1' and `source2' must have the same number of dimensions
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());
  assert(source1.is_sorted());
  assert(source2.is_sorted());

  size_t old_source1_num_rows = source1.num_rows();
  size_t index = source1.num_rows();
  // `k1' runs through the rows of `source1'.
  size_t k1 = 0;
  // `k2' runs through the rows of `source2'.
  size_t k2 = 0;
  size_t source2_num_rows = source2.num_rows();
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the non-redundant constraints from `source2'
    // without sort.
    int cmp = compare(source1[k1], source2[k2]);
    if (cmp == 0) {
      // If the compared rows are equal, we choose another couple of rows.
      ++k1;
      ++k2;
    }
    else if (cmp < 0)
      // If `source1[k1]' is less than `source2[k2]', we compare
      // `source2[k2]' with another rows of `source1'.
    	++k1;
    else {
      // If `source1[k1]' is greater than `source2[k2]' it means that
      // in `source1' there is not the row `source2[k2]' then we add it
      // to `source1' (after the existing rows): this is because the
      // new merged matrix may be not sorted and also because we have to
      // increment the number of rows of `source1'.
      // Then we compare `source1[k1]' with another rows of `source2'.
      source1.add_row(source2[k2]);
      ++index;
      ++k2;
    }
  }
  // If there exist rows of `source2' that are greater than the last
  // (added) row of `source1', we add these ones at the end of 'source1'
  // and update the number of rows of the matrix.
  while (k2 < source2_num_rows) {
    source1.add_row(source2[k2]);
    ++index;
    ++k2;
  }
  // At this point `source1' has the old rows of `source1' from the
  // one indexed by 0 and the one indexed by `old_source1_num_rows' - 1.
  // The remaining rows (from the `old_source1_num_rows'-th one to the
  // end) are the ones added from `source2', i.e., the rows of `source2'
  // that are different from those in the old 'source1'.

  // source1 is not sorted any more.
  source1.set_sorted(false);

  // Now we have to add to `sat' the same number of rows that we added to
  // `source1'. The elements of these rows are set to zero.
  // New dimensions of `sat' are: `dest.num_rows()' rows and
  // `source1.num_rows()' columns, i.e., the rows of `sat' are indexed by
  // generators and its columns are indexed by constraints.
  size_t dest_num_rows = dest.num_rows();
  SatMatrix tmp_sat(dest_num_rows, source1.num_rows());
  // Copy the old `sat' into the new one.
  for (size_t i = sat.num_rows(); i-- > 0; )
    tmp_sat[i] = sat[i];
  // We can compute the matrix of generators corresponding to the new
  // matrix of constraints: we invoke the function conversion() but
  // this time `start' is the index of the first row added to the
  // old `source1', because generators corresponding to previous
  // constraints are already in `dest'.
  size_t num_lines_or_equalities = conversion(source1, old_source1_num_rows,
					      dest, tmp_sat,
					      dest.num_lines_or_equalities());

  // A non-empty polyhedron must have a constraints representation that
  // provides the positivity constraint and a generators representation
  // that provides at least one vertex (see the function minimize()).
  bool empty_or_illegal = true;
  dest_num_rows = dest.num_rows();
  for (size_t i = num_lines_or_equalities; i < dest_num_rows; ++i)
    if (dest[i][0] > 0) {
      empty_or_illegal = false;
      break;
    }
  if (empty_or_illegal) {
    if (con_to_ray)
      return empty_or_illegal;
    else
      abort();
  }
  else {
    // Since the function conversion() returns a `sat_c' we have to
    // transpose this matrix to obtain the `sat_g' that will be
    // passed to the function simplify().
    sat.transpose_assign(tmp_sat);
    // Deleting the redundant rows of `source1'.
    simplify(source1, sat);
    // We re-obtain the `sat_c'.
    sat.transpose_assign(sat);
  }
  return empty_or_illegal;
}
