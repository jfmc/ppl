/* Polyhedron class implementation: minimize() and add_and_minimize().
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#define POS_SIMPLEX_TRICK 0

/*!
  \fn static bool PPL::Polyhedron::minimize(bool con_to_gen,
                                            Matrix& source,
				            Matrix& dest,
				            SatMatrix& sat)

  \param con_to_gen   <CODE>true</CODE> if \p source represent the
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
			  Matrix& source, Matrix& dest, SatMatrix& sat,
			  bool pos) {
  // Sort the source matrix, if necessary.
  if (!source.is_sorted())
    source.sort_rows();
  size_t source_num_columns = source.num_columns();

#if !POSITIVE_TRANSFORMATION
  if (pos && !con_to_gen) {
    bool negative = false;
    for (size_t i = source.num_rows(); i-- > 0; )
      for (size_t j = 1; j < source_num_columns; ++j)
	if (source[i][j] < 0) {
	  negative = true;
	  break;
	}
    if (negative)
      return true;

#if POS_SIMPLEX_TRICK
    source.grow(source.num_rows(), 2*source_num_columns);
    for (size_t i = source.num_rows(); i-- > 0; )
      for (size_t j = 0; j < source_num_columns; ++j)
	source[i][source_num_columns + j] = -source[i][j];
    source_num_columns =  2*source_num_columns;
    source.set_sorted(false);
#endif
  }
#endif
#if 0
  std::cout << source << std::endl; 
#endif
  // Since we have to build `dest', we initialize it as the identity
  // matrix, i.e., we assume that generators are lines identified by
  // the vectors of the canonical basis in the space having
  // dimension `source.num_columns()'. We will modified these generators if
  // necessary.

  // `dest' is resized such that it is a square matrix having dimension
  // `source.num_columns()': the most important thing is that generators
  // and constraints have the same number of variables. Rows of `dest'
  // (i.e., constraints or generators based on `con_to_gen')
  // can be added or removed if it is necessary.

  dest.resize_no_copy(source_num_columns, source_num_columns);

  // `dest' is now a square matrix.
  size_t dest_num_rows = source_num_columns;
  for(size_t i = dest_num_rows; i-- > 0; ) {
    for (size_t j = dest_num_rows; j-- > 0; )
      if (j != i)
	dest[i][j] = 0;
    dest[i][i] = 1;
    if (pos
#if !POS_SIMPLEX_TRICK
	&& con_to_gen
#endif
	)
      dest[i].set_is_ray_or_vertex_or_inequality();
    else
      dest[i].set_is_line_or_equality();
  }
#if 0
  std::cout << dest << std::endl;
#endif
  // Since we have built `dest' as the identity matrix, it is not sorted
  // (see the sorting rules in Row.cc).
  dest.set_sorted(false);

  size_t num_positive = 0;
  if (pos 
#if !POS_SIMPLEX_TRICK
      && con_to_gen
#endif
      )
    num_positive = source_num_columns;
  // We need a saturation matrix, too; then we initialize a temporary one.
  // They will be modified together with `dest'.
  // If the polyhedron is not positive or we are computing the system of
  // constraints starting from the system  of generators, the saturation
  // matrix is inizialized setting all the elements to zero.
  // If the polyhedron is positive and `con_to_ray' is true, the saturation
  // matrix is composed by two parts: the upper part considers the behaviour
  // of the row of `dest' with the constraints of positivity of all the
  // variables (that can not be in the system of constraints). 
  SatMatrix tmp_sat(dest_num_rows, num_positive + source.num_rows());
  if (pos 
#if !POS_SIMPLEX_TRICK
      && con_to_gen
#endif
      )
    for (size_t i = 0; i < num_positive; ++i)
      tmp_sat[i].set(i);

   // Since we want to build a new matrix of generators starting from the
  // given matrix of constraints, we invoke the function conversion() with
  // `start' parameter zero and we pass it the initialized matrices
  // `dest' and `tmp_sat'.
  // Note that `dest.num_lines_or_equalities()' is the number of the rows
  // of `dest' (because of our construction) and also the number
  // of columns of `source'.

#if 0
  using std::cout;
  using std::endl;
  cout << "Prima di conversion" << endl;
  cout << "source" << endl << source << endl;
  cout << "dest" << endl << dest << endl;
  cout << "sat" << endl << tmp_sat << endl;
#endif

  size_t num_lines_or_equalities = conversion(source, 0,
					      dest, tmp_sat,
					      dest.num_lines_or_equalities());
#if 0
  using std::cout;
  using std::endl;
  cout << "Dopo conversion" << endl;
  cout << "source" << endl << source << endl;
  cout << "dest" << endl << dest << endl;
  cout << "sat" << endl << tmp_sat << endl;
#endif
#if POS_SIMPLEX_TRICK
  if (pos && !con_to_gen) {
    source_num_columns = source_num_columns / 2;
    for (size_t i = dest.num_rows(); i-- > 0; )
      for (size_t j = 0; j < source_num_columns; ++j)
	dest[i][j] = dest[i][j] - dest[i][source_num_columns + j];
    source.resize_no_copy(source.num_rows(), source_num_columns);
    dest.resize_no_copy(dest.num_rows(), source_num_columns);
#if 0
    std::cout << "source" << std::endl << source << std::endl; 
    std::cout << "dest" << std::endl << dest << std::endl; 
#endif
  }
#endif
  // `empty_or_illegal' will remain set to `true' if there
  // not exists a ray/vertex or an inequality having a positive
  // inhomogeneous term.

  // This means
  //  -# if `dest' represent generators:
  //     there not exists any vertex; in fact (in our representation) a
  //     vertex has 1 in the first position, while a ray has 0.
  //  -# if `dest' represent constraints:
  //     the positivity constraint is not provided; in fact (in our
  //     representation) this constraint is an inequality in which
  //     only the inhomogeneus term is positive and the other term
  //     are equal to zero.

  // conversion() may have modified dest.
  dest_num_rows = dest.num_rows();
  bool empty_or_illegal = true;
  for (size_t i = num_lines_or_equalities; i < dest_num_rows; ++i) {
    if (dest[i][0] > 0) {
      empty_or_illegal = false;
      break;
    }
  }
  if (empty_or_illegal) {
    if (con_to_gen) {
      // In this case `dest' contains generators and we have not found
      // any vertex: the polyhedron is empty (See Poly.cc).
      return empty_or_illegal;
    }
    else
      // In this case `dest' contains constraints but does not contain
      // the positivity constraint. Since the polyhedron is not bounded
      // (see the definition in the Introduction) because the inhomogeneous
      // term is zero in all of them (i.e., all hyper-plane corresponding
      // to the constraints contain the origin), the system of constraints
      // is illegal.
      abort();
  }
  else {
    // Polyhedron is not empty.
    // Since `sat' has generators on its columns
    // while the saturation matrix argument of the function conversion()
    // (i.e., `tmp_sat') has constraints on its columns, we have to
    // transpose the saturation matrix returned by conversion().
    sat.transpose_assign(tmp_sat);

    // Deleting the redundant rows of `source'.
    simplify(source, sat);
    size_t source_num_rows = source.num_rows();
    if (pos)
      // If the polyhedron is positive and we are computing the system of
      // constraints we erase the redundant constraints, the constraints of
      // positivity of all the variables and those that are a linear
      // combination of a constraints of positivity of variables and an
      // equalities.
      if (!con_to_gen) {
#if POS_SIMPLEX_TRICK
	sat.transpose_assign(sat);
	simplify(dest, sat);
	dest_num_rows = dest.num_rows();
	for (size_t i = dest_num_rows; i-- > 0; )
	  if (dest[i].only_a_term_is_positive()) {
	    --dest_num_rows;
	    std::swap(dest[i], dest[dest_num_rows]);
	    std::swap(sat[i], sat[dest_num_rows]);
	  }
	if (dest_num_rows < dest.num_rows()) {
	  dest.erase_to_end(dest_num_rows);
	  sat.rows_erase_to_end(dest_num_rows);
	}
	sat.transpose_assign(sat);
#if 0
	std::cout << "Dopo la nuova simplify" << std::endl;
	std::cout << dest << std::endl;
	std::cout << sat << std::endl;
#endif
#else
#if !POSITIVE_TRANSFORMATION
	source.resize_no_copy(source_num_columns, source_num_columns);
	for(size_t i = source_num_columns; i-- > 0; ) {
	  for (size_t j = source_num_columns; j-- > 0; )
	    if (j != i)
	      source[i][j] = 0;
	  source[i][i] = 1;
	  source[i].set_is_ray_or_vertex_or_inequality();
	}
	source.set_sorted(false);
	SatMatrix tmp(source_num_columns,
		      source_num_columns + dest.num_rows());
	for (size_t i = 0; i < source_num_columns; ++i)
	  tmp[i].set(i);

	conversion(dest, 0, source, tmp, 0);
	std::swap(tmp, sat);
#endif
#endif
      }
#if !POSITIVE_TRANSFORMATION
      else {
	// If the polyhedron is positive and we are computing the system of
	// generators, we must erase the constraints of positivity of
	// variables, that there still are.  
	source_num_rows = source.num_rows();
 	for (size_t i = source.num_lines_or_equalities();
	     i < source_num_rows; )
	  if (source[i].only_a_term_is_positive()) {
	    --source_num_rows;
	    std::swap(source[i], source[source_num_rows]);
	    std::swap(sat[i], sat[source_num_rows]);
	  }
	  else
	    ++i;

	source.erase_to_end(source_num_rows);
	sat.rows_erase_to_end(source_num_rows);

	source.set_sorted(false);
      }
#endif
  }
#if 0
  using std::cout;
  using std::endl;
  cout << "Alla fine di minimize" << endl;
  cout << "source" << endl << source << endl;
  cout << "dest" << endl << dest << endl;
  cout << "sat" << endl << sat << endl;
#endif
  return empty_or_illegal;
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
PPL::Polyhedron::add_and_minimize(bool con_to_gen,
				  Matrix& source1,
				  Matrix& dest,
				  SatMatrix& sat,
				  const Matrix& source2,
				  bool pos) {
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
  size_t num_columns = source1.num_columns();
  size_t dest_num_rows = dest.num_rows();

  if (pos && !con_to_gen) {
    bool negative = false;
    size_t source2_num_columns = source2.num_columns();
    for (size_t i = source2_num_rows; i-- > 0; )
      for (size_t j = 1; j < source2_num_columns; ++j)
	if (source2[i][j] < 0) {
	  negative = true;
	  break;
	}
    if (negative)
      return true;
  }
#if !POS_SIMPLEX_TRICK
  if (pos)
    if (!con_to_gen) {
      dest.grow(dest_num_rows + num_columns, num_columns);
      sat.resize(dest_num_rows + num_columns, old_source1_num_rows);
      for (size_t i = 0; i < num_columns; ++i) {
	dest[dest_num_rows + i][i] = 1;
	dest[dest_num_rows + i].set_is_ray_or_vertex_or_inequality();
	for (size_t j = 0; j < old_source1_num_rows; ++j)
	  if (source1[j][i] != 0)
	    sat[dest_num_rows + i].set(j);
      }
      dest_num_rows += num_columns;
      dest.set_sorted(false);
    }
#endif
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
  size_t source1_num_rows = source1.num_rows();
  
  // At this point `source1' has the old rows of `source1' from the
  // one indexed by 0 and the one indexed by `old_source1_num_rows' - 1.
  // The remaining rows (from the `old_source1_num_rows'-th one to the
  // end) are the ones added from `source2', i.e., the rows of `source2'
  // that are different from those in the old 'source1'.
  
  // source1 is not sorted any more.
  source1.set_sorted(false);
  if (pos && !con_to_gen) {
#if POS_SIMPLEX_TRICK
    // We do not know how to transform the system of constraints
    // that we alreay have. So, we call the function minimize()
    // to obtain the system of constraints starting from `source1'.
    // In this way, we lose the informations of `dest'.
    sat.transpose_assign(sat);
    bool empty_or_illegal = minimize(false, source1, dest, sat, true);
    sat.transpose_assign(sat);
    return empty_or_illegal;
#endif
  }

  // Now we have to add to `sat' the same number of rows that we added to
  // `source1'. The elements of these rows are set to zero.
  // New dimensions of `sat' are: `dest.num_rows()' rows and
  // `source1.num_rows()' columns, i.e., the rows of `sat' are indexed by
  // generators and its columns are indexed by constraints.
  // If the polyhedron is positive the saturation matrix must consider the
  // behaviour of the system of generators that we have with the constraints
  // of positivity of the variables.
  dest_num_rows = dest.num_rows();
  size_t num_positive = 0;

  SatMatrix tmp_sat;
  if (pos) {
    num_positive = source1.num_columns();
    if (con_to_gen) {
      tmp_sat.resize(dest_num_rows, num_positive + source1_num_rows);
      for (size_t i = 0; i < dest_num_rows; ++i) {
	for (size_t j = 0; j < num_positive; ++j)
	  if (dest[i][j] != 0)
	    tmp_sat[i].set(j);
	for (size_t j = 0; j < source1_num_rows; ++j)
	  if (sat[i][j])
	    tmp_sat[i].set(num_positive + j);
      }
    }
    else {
      tmp_sat.resize(dest_num_rows, source1_num_rows);
      
      // Copy the old `sat' into the new one.
      for (size_t i = sat.num_rows(); i-- > 0; )
	tmp_sat[i] = sat[i];
    }
  }
  else {
    // SatMatrix tmp_sat(dest_num_rows, source1_num_rows);
    tmp_sat.resize(dest_num_rows, source1_num_rows);
    // Copy the old `sat' into the new one.
    for (size_t i = sat.num_rows(); i-- > 0; )
      tmp_sat[i] = sat[i];
  }

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
  if (pos && !con_to_gen)
    empty_or_illegal = false;
  else
    for (size_t i = num_lines_or_equalities; i < dest_num_rows; ++i)
      if (dest[i][0] > 0) {
	empty_or_illegal = false;
	break;
      }
  if (empty_or_illegal) {
    if (con_to_gen)
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
    source1_num_rows = source1.num_rows();
    // We erase the columns of the saturation matrix that consider the
    // behaviour of the system of generators with the constraints of positivity
    // of the variables, if there are someone.
    if (sat.num_columns() > dest_num_rows) {
      SatMatrix tmp(source1_num_rows, dest_num_rows);
      for (size_t i = 0; i < source1_num_rows; ++i)
	for (size_t j = 0; j < dest_num_rows; ++j)
	  if (sat[i][num_positive + j])
	    tmp[i].set(j);
      std::swap(tmp,sat);
    }
    // If the polyhedron is positive, we erase the redundant constraints,
    // the constraints of positivity of the variables and those that are a
    // linear combination of constraints of positivity of variable and
    // equalities. Then we re-obtain the `sat_c'.
    if (pos) {
      if (!con_to_gen) {
#if POSITIVE_TRANSFORMATION
	sat.transpose_assign(sat);
	for (size_t i = dest_num_rows; i-- > 0; )
	  if (dest[i].is_ray_or_vertex_or_inequality()
	      && dest[i].only_a_term_is_positive()) {
	    --dest_num_rows;
	    std::swap(dest[i], dest[dest_num_rows]);
	    std::swap(sat[i],sat[dest_num_rows]);
	  }
	if (dest_num_rows < dest.num_rows()) {
	  dest.erase_to_end(dest_num_rows);
	  sat.rows_erase_to_end(dest_num_rows);
	}
#else
	source1.resize_no_copy(num_columns, num_columns);
	for(size_t i = num_columns; i-- > 0; ) {
	  for (size_t j = num_columns; j-- > 0; )
	    if (j != i)
	      source1[i][j] = 0;
	  source1[i][i] = 1;
	  source1[i].set_is_ray_or_vertex_or_inequality();
	}
       	SatMatrix tmp(num_columns, num_columns + dest.num_rows());
	for (size_t i = 0; i < num_columns; ++i)
	  tmp[i].set(i);
	conversion(dest, 0, source1, tmp, 0);
	tmp.transpose_assign(tmp);
	std::swap(tmp, sat);
#endif
      }
      else {
	for (size_t i = source1.num_lines_or_equalities();
	     i < source1_num_rows; ) {
	  if (source1[i].only_a_term_is_positive()) {
	    --source1_num_rows;
	    std::swap(source1[i], source1[source1_num_rows]);
	    std::swap(sat[i], sat[source1_num_rows]);
	  }
	  else
	    ++i;
	}
	
	source1.erase_to_end(source1_num_rows);
	sat.rows_erase_to_end(source1_num_rows);
	sat.transpose_assign(sat);
      }
    }
    else
      sat.transpose_assign(sat);
  }
  return empty_or_illegal;
}

