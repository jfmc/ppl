/* Grid class implementation: minimize() and add_and_minimize().
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
#include "Saturation_Matrix.defs.hh"
#include "Grid.defs.hh"
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

/*!
  \return
  <CODE>true</CODE> if the polyhedron is empty, <CODE>false</CODE>
  otherwise.

  \param con_to_gen
  <CODE>true</CODE> if \p source represents the constraints,
  <CODE>false</CODE> otherwise;

  \param source
  The given system, which is not empty;

  \param dest
  The system to build and reduce;

  \param sat
  The saturation matrix.

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
  <CODE>simplify()</CODE>) \p source, erasing redundant rows.
  For the sequel we assume that \p source is the system of constraints
  and \p dest is the system of generators.
  This will simplify the description of the function; the dual case is
  similar.
*/

bool
PPL::Grid::minimize(bool con_to_gen,
		    Generator_System& source,
		    Congruence_System& dest,
		    Saturation_Matrix& sat) {
  std::cout << "4 param minimize gs to cgs..." << std::endl; // FIX
  std::cout << "source.num_pending_rows(): " << source.num_pending_rows() << std::endl;  // FIX
  std::cout << "source.first_pending_row(): " << source.first_pending_row() << std::endl;  // FIX
  // Topologies have to agree.
  //assert(source.topology() == dest.topology()); // FIX
  // `source' cannot be empty: even if it is an empty constraint system,
  // representing the universe polyhedron, homogenization has added
  // the positive constraint. It also cannot be an empty generator system,
  // since this function is always called starting from a non-empty
  // polyhedron.
  assert(source.num_rows() > 0);

  // Sort the source system, if necessary.
  if (!source.is_sorted())
    source.sort_rows();

  std::cout << "source.num_pending_rows(): " << source.num_pending_rows() << std::endl;  // FIX

  // Initialization of the system of generators `dest'.
  // The algorithm works incrementally and we haven't seen any
  // constraint yet: as a consequence, `dest' should describe
  // the universe polyhedron of the appropriate dimension.
  // To this end, we initialize it to the identity matrix of dimension
  // `source.num_columns()': the rows represent the lines corresponding
  // to the canonical basis of the vector space.

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns();
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  //dest.set_index_first_pending_row(dest_num_rows); // FIX
  dest.resize_no_copy(dest_num_rows, dest_num_rows + 1 /* moduli */);
  // FIX init the moduli?

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Congruence& dest_i = dest[i];
    for (dimension_type j = dest_num_rows; j-- > 0; )
      dest_i[j] = (i == j) ? 1 : 0;
    // Mark dest_i as an equality.
    dest_i[dest_i.size()-1] = 0; // FIX dest_i.set_modulus(0);?
  }
  // The identity matrix `dest' is not sorted (see the sorting rules
  // in Linear_Row.cc).
  //dest.set_sorted(false); // FIX

  // NOTE: the system `dest', as it is now, it is not a _legal_
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
  Saturation_Matrix tmp_sat(dest_num_rows, source.num_rows());

  // FIX move to conversion?
  simplify(source, sat);	// reduce?

  // By invoking the function conversion(), we populate `dest' with
  // the generators characterizing the polyhedron described by all
  // the constraints in `source'.
  // The `start' parameter is zero (we haven't seen any constraint yet)
  // and the 5th parameter (representing the number of lines in `dest'),
  // by construction, is equal to `dest_num_rows'.
  const dimension_type num_lines_or_equalities
    = conversion(source, 0, dest, tmp_sat, dest_num_rows);
  // conversion() may have modified the number of rows in `dest'.
  dest_num_rows = dest.num_rows();

#if 0
  // FIX sort the triangular rows to please the assertions
  // FIX this is likely to be NB, at least for performance
  source.sort_rows();
#endif

  // FIX is there an equiv check for congruences?
#if 0
  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no points
  // (because rays, lines and closure points need a supporting point).
  // Points can be detected by looking at:
  // - the divisor, for necessarily closed polyhedra;
  // - the epsilon coordinate, for NNC polyhedra.
  const dimension_type checking_index
    = dest.is_necessarily_closed()
    ? 0
    : dest.num_columns() - 1;
  dimension_type first_point;
  for (first_point = num_lines_or_equalities /* FIX why? */;
       first_point < dest_num_rows;
       ++first_point)
    if (dest[first_point][checking_index] != 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_gen' is false: `dest' is a system of constraints.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("PPL internal error");
#endif

#if 0
  // A point has been found: the polyhedron is not empty.
  // Now invoking simplify() to remove all the redundant constraints
  // from the system `source'.
  // Since the saturation matrix `tmp_sat' returned by conversion()
  // has rows indexed by generators (the rows of `dest') and columns
  // indexed by constraints (the rows of `source'), we have to
  // transpose it to obtain the saturation matrix needed by simplify().
  sat.transpose_assign(tmp_sat);
  simplify(source, sat);
#endif
  std::cout << "4 param minimize gs to cgs... done." << std::endl;
  return false; // FIX?
}

bool
PPL::Grid::minimize(bool con_to_gen,
		    Congruence_System& source,
		    Linear_System& dest,
		    Saturation_Matrix& sat) {
  // Topologies have to agree.
  //assert(source.topology() == dest.topology()); // FIX
  // `source' cannot be empty: even if it is an empty constraint system,
  // representing the universe polyhedron, homogenization has added
  // the positive constraint. It also cannot be an empty generator system,
  // since this function is always called starting from a non-empty
  // polyhedron.
  // FIX should grid add single cong?
  //assert(source.num_rows() > 0);

  std::cout << "4 param minimize cgs to gs" << std::endl;

#if 0
  // Sort the source system, if necessary.
  if (!source.is_sorted())
    source.sort_rows();
#endif

  // Initialization of the system of generators `dest'.
  // The algorithm works incrementally and we haven't seen any
  // constraint yet: as a consequence, `dest' should describe
  // the universe polyhedron of the appropriate dimension.
  // To this end, we initialize it to the identity matrix of dimension
  // `source.num_columns()': the rows represent the lines corresponding
  // to the canonical basis of the vector space.

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns() - 1 /* modulus */;
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  dest.set_index_first_pending_row(dest_num_rows);
  dest.resize_no_copy(dest_num_rows, dest_num_rows);

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Linear_Row& dest_i = dest[i];
    for (dimension_type j = dest_num_rows; j-- > 0; )
      // FIX comparing i and j every iteration
      dest_i[j] = (i == j) ? 1 : 0;
    dest_i.set_is_line_or_equality();
    dest_i.set_necessarily_closed();
  }
  dest.set_necessarily_closed();
  // The identity matrix `dest' is not sorted (see the sorting rules
  // in Linear_Row.cc).
  dest.set_sorted(false);  // FIX resize_no_copy does this

  // NOTE: the system `dest', as it is now, it is not a _legal_
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
  Saturation_Matrix tmp_sat(dest_num_rows, source.num_rows());

  // FIX move to conversion?
  source.normalize_moduli();	// FIX necessary?
  simplify(source, sat);	// FIX rename reduce?

  // By invoking the function conversion(), we populate `dest' with
  // the generators characterizing the polyhedron described by all
  // the congruences in `source'.
  // The `start' parameter is zero (the next congruence will be the first)
  // and the 5th parameter (representing the number of lines in `dest'),
  // by construction, is `dest_num_rows'.
  const dimension_type num_lines_or_equalities
    = conversion(source, 0, dest, tmp_sat, dest_num_rows);
  // conversion() may have modified the number of rows in `dest'.
  dest_num_rows = dest.num_rows();

  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no points
  // (because rays, lines and closure points need a supporting point).
  // Points can be detected by looking at:
  // - the divisor, for necessarily closed polyhedra;
  // - the epsilon coordinate, for NNC polyhedra.
  assert(dest.is_necessarily_closed());
  const dimension_type checking_index
    = dest.is_necessarily_closed()
    ? 0
    : dest.num_columns() - 1;
  dimension_type first_point;
  for (first_point = num_lines_or_equalities /* FIX why? (more below) */;
       first_point < dest_num_rows;
       ++first_point)
    if (dest[first_point][checking_index] != 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else   // FIX
      // Here `con_to_gen' is false: `dest' is a system of congruences.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the congruences in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("cgs to gs converstion called w/ con_to_gen false");
#if 0
  else {
    // A point has been found: the polyhedron is not empty.
    // Now invoking simplify() to remove all the redundant constraints
    // from the system `source'.
    // Since the saturation matrix `tmp_sat' returned by conversion()
    // has rows indexed by generators (the rows of `dest') and columns
    // indexed by constraints (the rows of `source'), we have to
    // transpose it to obtain the saturation matrix needed by simplify().
    sat.transpose_assign(tmp_sat);
    simplify(source, sat);
    return false;
  }
#endif
  return false; // FIX?
}


/*!
  \return
  <CODE>true</CODE> if the obtained polyhedron is empty,
  <CODE>false</CODE> otherwise.

  \param con_to_gen
  <CODE>true</CODE> if \p source1 and \p source2 are system of
  constraints, <CODE>false</CODE> otherwise;

  \param source1
  The first element of the given DD pair;

  \param dest
  The second element of the given DD pair;

  \param sat
  The saturation matrix that bind \p source1 to \p dest;

  \param source2
  The new system of generators or constraints.

  It is assumed that \p source1 and \p source2 are sorted and have
  no pending rows. It is also assumed that \p dest has no pending rows.
  On entry, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the rows of \p source1.
  On exit, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the rows of the system obtained
  by merging \p source1 and \p source2.

  Let us suppose we want to add some constraints to a given system of
  constraints \p source1. This method, given a reduced double description
  pair (\p source1, \p dest) and a system of new constraints \p source2,
  modifies \p source1 by adding to it the constraints of \p source2 that
  are not in \p source1. Then, by invoking
  <CODE>add_and_minimize(bool, Linear_System&, Linear_System&, Saturation_Matrix&)</CODE>,
  processes the added constraints obtaining a new DD pair.

  This method treats also the dual case, i.e., adding new generators to
  a previous system of generators. In this case \p source1 contains the
  old generators, \p source2 the new ones and \p dest is the system
  of constraints in the given reduced DD pair.

  Since \p source2 contains the constraints (or the generators) that
  will be added to \p source1, it is constant: it will not be modified.
*/
bool
PPL::Grid::add_and_minimize(const bool con_to_gen,
			    Congruence_System& source1,
			    Linear_System& dest,
			    Saturation_Matrix& sat,
			    const Congruence_System& source2) {
  std::cout << "add_and_minimize cgs to gs (5 param)" << std::endl;
#if 0
  // FIX must a cgs hold equiv of equality constraint?
  // `source1' and `source2' cannot be empty.
  assert(source1.num_rows() > 0 && source2.num_rows() > 0);
  // `source1' and `source2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == source2.num_columns());
#else
  assert(source2.num_rows() > 0);
#endif
#if 0
  // `source1' and `source2' are fully sorted.
  assert(source1.is_sorted() && source1.num_pending_rows() == 0);
  assert(source2.is_sorted() && source2.num_pending_rows() == 0);
  assert(dest.num_pending_rows() == 0);
#endif

  bool added = false;

  const dimension_type old_source1_num_rows = source1.num_rows();
  // `k1' and `k2' run through the rows of `source1' and `source2', resp.
  dimension_type k1 = 0;
  dimension_type k2 = 0;
  dimension_type source2_num_rows = source2.num_rows();
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the constraints from `source2', as pending rows.
    // We exploit the property that initially both `source1' and `source2'
    // are sorted and index `k1' only scans the non-pending rows of `source1',
    // so that it is not influenced by the pending rows appended to it.
    // This way no duplicate (i.e., trivially redundant) constraint
    // is introduced in `source1'.
    //const int cmp = compare(source1[k1], source2[k2]);
    const int cmp = source1[k1] == source2[k2]; // FIX
    std::cout << "cmp (";
    source1[k1].ascii_dump(std::cout);
    std::cout << " vs ";
    source2[k2].ascii_dump(std::cout);
    std::cout << ") returned " << cmp << std::endl;
    //if (cmp == 0) {
    if (cmp) {
      // We found the same row: there is no need to add `source2[k2]'.
      ++k2;
      // By sortedness, since `k1 < old_source1_num_rows', we can
      // increment index `k1' too.
      ++k1;
    }
#if 0
    else if (cmp < 0)
      // By sortedness, we can increment `k1'.
      ++k1;
#endif
    else {
      // Here `cmp > 0'. // FIX  == false
      // By sortedness, `source2[k2]' cannot be in `source1'.
      // We add it as a pending row of `source1' (sortedness unaffected).
      //source1.add_pending_row(source2[k2]);
      added = true, source1.add_row(source2[k2]); // FIX
      // We can increment `k2'.
      ++k2;
    }
  }
  // Have we scanned all the rows in `source2'?
  if (k2 < source2_num_rows)
    // By sortedness, all the rows in `source2' having indexes
    // greater than or equal to `k2' were not in `source1'.
    // We add them as pending rows of 'source1' (sortedness not affected).
    for ( ; k2 < source2_num_rows; ++k2)
      //source1.add_pending_row(source2[k2]);
      added = true, source1.add_row(source2[k2]); // FIX

  //if (source1.num_pending_rows() == 0)
  if (added == false)
    // No row was appended to `source1', because all the constraints
    // in `source2' were already in `source1'.
    // There is nothing left to do ...
    return false;

  source1.ascii_dump(std::cout);

  return add_and_minimize(con_to_gen, source1, dest, sat);
}

/*!
  \return
  <CODE>true</CODE> if the obtained polyhedron is empty,
  <CODE>false</CODE> otherwise.

  \param con_to_gen
  <CODE>true</CODE> if \p source is a system of constraints,
  <CODE>false</CODE> otherwise;

  \param source
  The first element of the given DD pair. It also contains the pending
  rows to be processed;

  \param dest
  The second element of the given DD pair. It cannot have pending rows;

  \param sat
  The saturation matrix that bind the upper part of \p source to \p dest.

  On entry, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the non-pending rows of \p source.
  On exit, the rows of \p sat are indexed by the rows of \p dest
  and its columns are indexed by the rows of \p source.

  Let us suppose that \p source is a system of constraints.
  This method assumes that the non-pending part of \p source and
  system \p dest form a double description pair in minimal form and
  will build a new DD pair in minimal form by processing the pending
  constraints in \p source. To this end, it will call
  <CODE>conversion()</CODE>) and <CODE>simplify</CODE>.

  This method treats also the dual case, i.e., processing pending
  generators. In this case \p source contains generators and \p dest
  is the system of constraints corresponding to the non-pending part
  of \p source.
*/
bool
PPL::Grid::add_and_minimize(const bool con_to_gen,
			    Congruence_System& source,
			    Linear_System& dest,
			    Saturation_Matrix& sat) {
#if 0
  assert(source.num_pending_rows() > 0);
  assert(source.num_columns() == dest.num_columns());
  assert(source.is_sorted());
#endif
  std::cout << "add_and_minimize cgs to gs (4 param)" << std::endl;

  // FIX move to conversion?
  source.normalize_moduli();
  simplify(source, sat);	// reduce?

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns() - 1 /* modulus */;
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  dest.set_index_first_pending_row(dest_num_rows);
  dest.resize_no_copy(dest_num_rows, dest_num_rows);

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Linear_Row& dest_i = dest[i];
    for (dimension_type j = dest_num_rows; j-- > 0; )
      // FIX comparing i and j every iteration
      dest_i[j] = (i == j) ? 1 : 0;
    dest_i.set_is_line_or_equality();
    dest_i.set_necessarily_closed();
  }
  dest.set_necessarily_closed();

  // The identity matrix `dest' is not sorted (see the sorting rules
  // in Linear_Row.cc).
  dest.set_sorted(false);  // FIX resize_no_copy does this

  // First, pad the saturation matrix with new columns (of zeroes)
  // to accommodate for the pending rows of `source'.
  sat.resize(dest_num_rows, source.num_rows());

  // Incrementally compute the new system of generators.
  // Parameter `start' is set to the index of the first pending constraint.
  const dimension_type num_lines_or_equalities
    = conversion(source, 0 /* FIX source.first_pending_row() */,
		 dest, sat,
		 dest.num_lines_or_equalities());

  // conversion() may have modified the number of rows in `dest'.
  dest_num_rows = dest.num_rows();

  // FIX will this still be required?
  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no points
  // (because rays, lines and closure points need a supporting point).
  // Points can be detected by looking at:
  // - the divisor, for necessarily closed polyhedra;
  // - the epsilon coordinate, for NNC polyhedra.
  assert(dest.is_necessarily_closed());
  const dimension_type checking_index
    = dest.is_necessarily_closed()
    ? 0
    : dest.num_columns() - 1;
  dimension_type first_point;
  for (first_point = num_lines_or_equalities;
       first_point < dest_num_rows;
       ++first_point)
     if (dest[first_point][checking_index] != 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_gen' is false: `dest' is a system of constraints.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("cgs to gs converstion called w/ con_to_gen false");
#if 0
  else {
    // A point has been found: the polyhedron is not empty.
    // Now invoking `simplify()' to remove all the redundant constraints
    // from the system `source'.
    // Since the saturation matrix `sat' returned by `conversion()'
    // has rows indexed by generators (the rows of `dest') and columns
    // indexed by constraints (the rows of `source'), we have to
    // transpose it to obtain the saturation matrix needed by `simplify()'.
    sat.transpose();
    simplify(source, sat);
    // Transposing back.
    sat.transpose();
    simplify(source, sat);
    return false;
  }
#endif
  return false;
}

bool
PPL::Grid::add_and_minimize(Generator_System& source,
			    Congruence_System& dest,
			    Saturation_Matrix& sat) {
  std::cout << "add_and_minimize gs to cgs (3 param)" << std::endl;
  assert(source.num_pending_rows() > 0);
#if 0
  assert(source.num_columns() == dest.num_columns());
  assert(source.is_sorted());
#endif

  // Resizing `dest' to be the appropriate square matrix.
  dimension_type dest_num_rows = source.num_columns();
#if 0
  // Note that before calling `resize_no_copy()' we must update
  // `index_first_pending'.
  dest.set_index_first_pending_row(dest_num_rows);
#endif
  dest.resize_no_copy(dest_num_rows, dest_num_rows + 1 /* moduli */);
  // FIX init the moduli?

  // Initialize `dest' to the identity matrix.
  for (dimension_type i = dest_num_rows; i-- > 0; ) {
    Congruence& dest_i = dest[i];
    for (dimension_type j = dest_num_rows; j-- > 0; )
      // FIX comparing i and j every iteration
      dest_i[j] = (i == j) ? 1 : 0;
    dest_i.set_is_equality();
  }
#if 0
  // The identity matrix `dest' is not sorted (see the sorting rules
  // in Linear_Row.cc).
  dest.set_sorted(false);  // FIX resize_no_copy does this
#endif

  // First, pad the saturation matrix with new columns (of zeroes)
  // to accommodate for the pending rows of `source'.
  sat.resize(dest.num_rows(), source.num_rows());

  simplify(source, sat);	// FIX reduce?

  // Incrementally compute the new system of generators.
  // Parameter `start' is set to the index of the first pending constraint.
  const dimension_type num_lines_or_equalities
    = conversion(source, source.first_pending_row(),
		 dest, sat,
		 dest.num_equalities());

  // conversion() may have modified the number of rows in `dest'.
  dest_num_rows = dest.num_rows();

#if 0
  // FIX sort the triangular rows to please the assertions
  // FIX this is likely to be NB, at least for performance
  source.sort_rows();
#endif

  // FIX is there an equiv check for congruences?
#if 0
  // Checking if the generators in `dest' represent an empty polyhedron:
  // the polyhedron is empty if there are no points
  // (because rays, lines and closure points need a supporting point).
  // Points can be detected by looking at:
  // - the divisor, for necessarily closed polyhedra;
  // - the epsilon coordinate, for NNC polyhedra.
  const dimension_type checking_index
    = dest.is_necessarily_closed()
    ? 0
    : dest.num_columns() - 1;
  dimension_type first_point;
  for (first_point = num_lines_or_equalities;
       first_point < dest_num_rows;
       ++first_point)
     if (dest[first_point][checking_index] != 0)
      break;

  if (first_point == dest_num_rows)
    if (con_to_gen)
      // No point has been found: the polyhedron is empty.
      return true;
    else
      // Here `con_to_gen' is false: `dest' is a system of constraints.
      // In this case the condition `first_point == dest_num_rows'
      // actually means that all the constraints in `dest' have their
      // inhomogeneous term equal to 0.
      // This is an ILLEGAL situation, because it implies that
      // the constraint system `dest' lacks the positivity constraint
      // and no linear combination of the constraints in `dest'
      // can reintroduce the positivity constraint.
      throw std::runtime_error("PPL internal error");
#endif

#if 0
  else {
    // A point has been found: the polyhedron is not empty.
    // Now invoking `simplify()' to remove all the redundant constraints
    // from the system `source'.
    // Since the saturation matrix `sat' returned by `conversion()'
    // has rows indexed by generators (the rows of `dest') and columns
    // indexed by constraints (the rows of `source'), we have to
    // transpose it to obtain the saturation matrix needed by `simplify()'.
    sat.transpose();
    simplify(source, sat);
    // Transposing back.
    sat.transpose();
    simplify(source, sat);
    return false;
  }
#endif

  return false;
}

bool
PPL::Grid::add_and_minimize(Generator_System& source1,
			    Congruence_System& dest,
			    Saturation_Matrix& sat,
			    const Generator_System& csource2) {
  std::cout << "add_and_minimize gs to cgs (4 param)" << std::endl;

  // `source1' and `csource2' must contain elements.
  assert(source1.num_rows() > 0 && csource2.num_rows() > 0);
  // `source1' and `csource2' must have the same number of columns
  // to be merged.
  assert(source1.num_columns() == csource2.num_columns());
#if 0
  // `source1' and `csource2' must be fully sorted.
  assert(source1.is_sorted() && source1.num_pending_rows() == 0);
  assert(csource2.is_sorted() && csource2.num_pending_rows() == 0);
#endif
  //assert(dest.num_pending_rows() == 0); // FIX

  Linear_System source2 = csource2; // FIX must csource2 be const?
  //parameterize(source2);  // FIX check all callers do this

  const dimension_type old_source1_num_rows = source1.num_rows();
  // `k1' and `k2' run through the rows of `source1' and `source2', resp.
  dimension_type k1 = 0;
  dimension_type k2 = 0;
  dimension_type source2_num_rows = source2.num_rows();
  while (k1 < old_source1_num_rows && k2 < source2_num_rows) {
    // Add to `source1' the generators from `source2', as pending rows.
    // We exploit the property that initially both `source1' and `source2'
    // are sorted and index `k1' only scans the non-pending rows of `source1',
    // so that it is not influenced by the pending rows appended to it.
    // This way no duplicate (i.e., trivially redundant) generators
    // is introduced in `source1'.
    const int cmp = compare(source1[k1], source2[k2]);
    //const int cmp = source1[k1] == source2[k2]; // FIX
    if (cmp == 0) {
      // We found the same row: there is no need to add `source2[k2]'.
      ++k2;
      // By sortedness, since `k1 < old_source1_num_rows', we can
      // increment index `k1' too.
      ++k1;
    }
    else if (cmp < 0)
      // By sortedness, we can increment `k1'.
      ++k1;
    else {
      // Here `cmp > 0'.
      // By sortedness, `source2[k2]' cannot be in `source1'.
      // We add it as a pending row of `source1' (sortedness unaffected).
      source1.add_pending_row(source2[k2]);
      // We can increment `k2'.
      ++k2;
    }
  }
  // Have we scanned all the rows in `source2'?
  if (k2 < source2_num_rows)
    // By sortedness, all the rows in `source2' having indexes
    // greater than or equal to `k2' were not in `source1'.
    // We add them as pending rows of 'source1' (sortedness not affected).
    for ( ; k2 < source2_num_rows; ++k2)
      source1.add_pending_row(source2[k2]);

  if (source1.num_pending_rows() == 0)
    // No row was appended to `source1', because all the constraints
    // in `source2' were already in `source1'.
    // There is nothing left to do ...
    return false;

  return add_and_minimize(source1, dest, sat);
}
