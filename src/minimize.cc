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
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

/*!
  \fn static bool PPL::Polyhedron::minimize(bool con_to_gen,
                                            Matrix& source,
				            Matrix& dest,
				            SatMatrix& sat)

  \param con_to_gen   <CODE>true</CODE> if \p source represents the
                      constraints, <CODE>false</CODE> otherwise.
  \param source       The given matrix, which is not empty.
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
  if \p con_to_gen is <CODE>true</CODE>, otherwise it has the generators on
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
PPL::Polyhedron::minimize(bool con_to_gen,
			  Matrix& source, Matrix& dest, SatMatrix& sat) {
  // `source' cannot be empty: even if it is an empty constraint system,
  // representing the universe polyhedron, homogeneization has added
  // the positive constraint. It also cannot be an empty generator system,
  // since this function is always called starting from a non-empty
  // polyhedron. 
  assert(source.num_rows() > 0);

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
  for (size_t i = dest_num_rows; i-- > 0; ) {
    for (size_t j = dest_num_rows; j-- > 0; )
      dest[i][j] = 0;
    dest[i][i] = 1;
    dest[i].set_is_line_or_equality();
  }
  // The identity matrix `dest' is not sorted
  // (see the sorting rules in Row.cc).
  dest.set_sorted(false);

  // NOTE: the matrix `dest', as it is now, it is not a _legal_
  //       system of generators, because in the first row we have
  // a line with a non-zero divisor (which should only happen for
  // points). However, this is NOT a problem, because `source'
  // necessarily contains the positivity constraint (or a combination
  // of it with another constraint) which will restore things as they
  // should be.


  // Building a saturation matrix and initializing it by setting
  // all of its elements to zero. This matrix will be modified together
  // with `dest' during the conversion.
  // NOTE: since we haven't seen any constraint yet, the relevant
  //       portion of `tmp_sat' is the sub-matrix consisting of
  //       the first 0 columns: thus the relevant portion correctly
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
  // the polyhedron is empty if there are no points (because rays
  // and lines need a supporting point).
  size_t first_point = num_lines_or_equalities;
  for ( ; first_point < dest_num_rows; ++first_point)
    // Points have a positive divisor.
    if (dest[first_point][0] > 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_gen' is false: `dest' is a matrix of constraints.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("PPL internal error");
  else {
    // A point has been found: the polyhedron is not empty.
    // Now invoking simplify() to remove all the redundant constraints
    // from the matrix `source'.
    // Since the saturation matrix `tmp_sat' returned by conversion()
    // has rows indexed by generators (the rows of `dest') and columns
    // indexed by constraints (the rows of `source'), we have to
    // transpose it to obtain the saturation matrix needed by simplify().
    sat.transpose_assign(tmp_sat);
    simplify(source, sat);
    return false;
  }
}

/*!
  \fn bool PPL::Polyhedron::add_and_minimize(bool con_to_gen,
                                             Matrix& source1,
                                             Matrix& dest,
                                             SatMatrix& sat,
                                             const Matrix& source2)
					
  \param con_to_gen   <CODE>true</CODE> if \p source1 and \p source2
                      are matrix of constraints, <CODE>false</CODE> otherwise.
  \param source1      The first element of the given DD pair.
  \param dest         The second element of the given DD pair.
  \param sat          The saturation matrix that bind \p source1 to \p dest.
  \param source2      The new system of generators or constraints.

  \return             <CODE>true</CODE> if the obtained polyhedron
                      is empty, <CODE>false</CODE> otherwise.

  On entry, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the rows of \p source1.
  On exit, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the rows of the matrix obtained
  by merging \p source1 and \p source2.

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
PPL::Polyhedron::add_and_minimize(bool con_to_gen,
				  Matrix& source1,
				  Matrix& dest,
				  SatMatrix& sat,
				  const Matrix& source2) {
  // `source1' and `source2' cannot be empty: even if they are empty
  // constraint systems, representing universe polyhedra, homogeneization
  // has added the positive constraint. They also cannot be empty
  // generator systems, since this function is always called starting
  // from a pair of non-empty polyhedra.
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());
  assert(source1.is_sorted());
  assert(source2.is_sorted());

  size_t old_source1_num_rows = source1.num_rows();
  // `k1' and `k2' run through the rows of `source1' and `source2', resp.
  size_t k1 = 0;
  size_t k2 = 0;
  size_t source2_num_rows = source2.num_rows();
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the constraints from `source2'.
    // We exploit the property (here called `initial sortedness')
    // that initially both `source1' and `source2' are sorted and
    // index `k1' only scans the initial rows of `source1', so that
    // it is not influenced by rows appended at the end of `source1'.
    // This allows to avoid the introduction in `source1' of any
    // duplicate constraint (which would be trivially redundant).
    int cmp = compare(source1[k1], source2[k2]);
    if (cmp == 0) {
      // We found the same row: there is no need to add `source2[k2]'.
      ++k2;
      // By initial sortedness, since `k1 < old_source1_num_rows',
      // we can increment index `k1' too.
      ++k1;
    }
    else if (cmp < 0)
      // By initial sortedness, we can increment `k1'.
      ++k1;
    else {
      // Here `cmp > 0'.
      // By initial sortedness, `source2[k2]' cannot be in `source1'.
      // We append it to the end of `source1', without worrying
      // about maintaining the sortedness of `source1' (note however
      // that `initial sortedness' is maintained).
      source1.add_row(source2[k2]);
      // We can increment `k2'.
      ++k2;
    }
  }
  // Have we scanned all the rows in `source2' ?
  if (k2 < source2_num_rows)
    // By initial sortedness, all the rows in `source2' having indexes
    // greater than or equal to `k2' were not in `source1'.
    // We append them at the end of 'source1'.
    for ( ; k2 < source2_num_rows; ++k2)
      source1.add_row(source2[k2]);

  size_t new_source1_num_rows = source1.num_rows();
  if (new_source1_num_rows == old_source1_num_rows)
    // No row was appended to `source1', because all the constraints
    // in `source2' were already in `source1'.
    // There is nothing left to do ...
    return false;

  // FIXME: add_row() already sets correctly the flag `sorted'.
  // ... otherwise, `source1' may have lost his sortedness.
  source1.set_sorted(false);

  // We have to add to `sat' the same number of rows that we added to
  // `source1'. The elements of these rows are set to zero.
  // New dimensions of `sat' are: `dest.num_rows()' rows and
  // `source1.num_rows()' columns, i.e., the rows of `sat' are
  // indexed by generators and its columns are indexed by constraints.
  SatMatrix tmp_sat(dest.num_rows(), source1.num_rows());
  // Copy the old `sat' into the new one.
  for (size_t i = sat.num_rows(); i-- > 0; )
    tmp_sat[i] = sat[i];
  // We compute the matrix of generators corresponding to the new
  // matrix of constraints by invoking the function conversion().
  // The `start' parameter is set to the index of the first constraint
  // we appended to `source1', because generators corresponding
  // to previous constraints are already in `dest'.
  size_t num_lines_or_equalities = conversion(source1, old_source1_num_rows,
					      dest, tmp_sat,
					      dest.num_lines_or_equalities());
  // conversion() may have modified the number of rows in `dest'.
  size_t dest_num_rows = dest.num_rows();

  // NOTE: conversion() can only remove inequalities from `source'.
  // Thus, all the equalities still come before the inequalities
  // (the correctness of simplify() relies on this hypothesis).

  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no points (because rays
  // and lines need a supporting point).
  size_t first_point = num_lines_or_equalities;
  for ( ; first_point < dest_num_rows; ++first_point)
    // Points have a positive divisor.
    if (dest[first_point][0] > 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_gen' is false: `dest' is a matrix of constraints.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("PPL internal error");
  else {
    // A point has been found: the polyhedron is not empty.
    // Now invoking simplify() to remove all the redundant constraints
    // from the matrix `source1'.
    // Since the saturation matrix `tmp_sat' returned by conversion()
    // has rows indexed by generators (the rows of `dest') and columns
    // indexed by constraints (the rows of `source'), we have to
    // transpose it to obtain the saturation matrix needed by simplify().
    sat.transpose_assign(tmp_sat);
    simplify(source1, sat);
    // Transposing back.
    sat.transpose_assign(sat);
    return false;
  }
}

