/* Polyhedron class implementation: simplify().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Polyhedron.defs.hh"
#include "SatMatrix.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  \param mat          The matrix to simplify: it will be modified.
  \param sat          The saturation matrix corresponding to \p mat.

  \return             The rank of \p mat.

  \note On enter, it holds that all the rows of \p mat corresponding
  to equalities (resp., lines) are placed before all the rows corresponding
  to inequalities (resp., rays and points). This partial sortedness
  condition will also hold on exit.

  \p mat may be modified by swapping some of its rows and by possibly
  removing some of them, if they turn out to be redundant.

  If \p mat is a matrix of constraints, then the rows of \p sat are
  indexed by constraints and its columns are indexed by generators;
  otherwise, if \p mat is a matrix of generators, then the rows of
  \p sat are indexed by generators and its columns by constraints.

  Given a matrix of constraints or a matrix of generators, this function
  simplifies it using Gauss' elimination method (to remove redundant
  equalities/lines), deleting redundant inequalities/rays/points and
  making back-substitution.
  The explanation that follows assumes that \p mat is a matrix of
  constraints. For the case when \p mat is a matrix of generators,
  a similar explanation can be obtain by applying duality.

  The explanation relies on the notion of <EM>redundancy</EM>.
  (See the Introduction).

  First we make some observations that can help the reader
  in understanding the function:

  Proposition: An inequality that is saturated by all the generators
  can be transformed into an equality.

  In fact, by combining any number of generators that saturate the
  constraints, we obtain a generator that saturates the constraints too:
  \f[
    \langle \vect{c}, \vect{r}_1 \rangle = 0 \land
    \langle \vect{c}, \vect{r}_2 \rangle = 0
    \Rightarrow
    \langle \vect{c}, (\lambda_1 \vect{r}_1 + \lambda_2 \vect{r}_2) \rangle =
    \lambda_1 \langle \vect{c}, \vect{r}_1 \rangle
    + \lambda_2 \langle \vect{c}, \vect{r}_2 \rangle
    = 0,
  \f]
  where \f$\lambda_1, \lambda_2\f$ can be any real number.
*/
int
PPL::Polyhedron::simplify(Matrix& mat, SatMatrix& sat) {
  dimension_type num_rows = mat.num_rows();
  dimension_type num_columns = mat.num_columns();
  dimension_type num_cols_sat = sat.num_columns();

  // Looking for the first inequality in `mat'.
  dimension_type num_equal_or_line = 0;
  while (num_equal_or_line < num_rows
	 && mat[num_equal_or_line].is_line_or_equality())
    ++num_equal_or_line;

  // `num_saturators[i]' will contain the number of generators
  // that saturate the constraint `mat[i]'.
  static std::vector<dimension_type> num_saturators;
  num_saturators.reserve(num_rows);

  // Computing the number of saturators for each inequality,
  // possibly identifying and swapping those that happen to be
  // equalities (see Proposition above).
  for (dimension_type i = num_equal_or_line; i < num_rows; ++i)
    // FIXME: in the following (commented out) boolean test,
    // the condition `mat[i].is_line_or_equality()' is never met,
    // because on entry the matrix `mat' is partially sorted.
    //   if (mat[i].is_line_or_equality() || sat[i].empty()) {
    // That is the reason why we use the following simpler version.
    if (sat[i].empty()) {
      // The inequality `mat[i]' is saturated by all the generators.
      // Thus, it can be transformed into an equality (see proposition).
      mat[i].set_is_line_or_equality();
      // We also move it just after all the other equalities,
      // so that matrix `mat' keeps its partial sortedness.
      std::swap(mat[i], mat[num_equal_or_line]);
      std::swap(sat[i], sat[num_equal_or_line]);
      std::swap(num_saturators[i], num_saturators[num_equal_or_line]);
      ++num_equal_or_line;
      // `mat' is no longer sorted (but it is partially sorted).
      mat.set_sorted(false);
    }
    else
      // There exists a generator which does not saturate `mat[i]',
      // so that `mat[i]' is indeed an inequality.
      // We store the number of its saturators.
      num_saturators[i] = num_cols_sat - sat[i].count_ones();

  // At this point, all the equalities of `mat' (included those
  // inequalities that we just tranformed in to equalities) have
  // indexes between 0 and `num_equal_or_line' - 1,
  // which is the property needed by the function gauss().
  // We can simplify the system of equalities, obtaining the rank
  // of `mat' as result.
  dimension_type rank = mat.gauss();
  // Now the irredundant equalities of `mat' have indexes from 0
  // to `rank' - 1, whereas the equalities having indexes from `rank'
  // to `num_equal_or_line' - 1 are all redundant.
  // (The inequalities in `mat' have been left untouched.)
  // The rows containing equalities are not sorted.

  if (rank < num_equal_or_line) {
    // We identified some redundant equalities.
    // Moving them at the bottom of `mat':
    // - index `redundant' runs through the redundant equalities
    // - index `erasing' identifies the first row that should
    //   be erased after this loop.
    // Note that we exit the loop either because we have moved all
    // redundant equalities or because we have moved all the
    // inequalities.
    for (dimension_type redundant = rank, erasing = num_rows;
	 redundant < num_equal_or_line && erasing > num_equal_or_line; ) {
      --erasing;
      std::swap(mat[redundant], mat[erasing]);
      std::swap(sat[redundant], sat[erasing]);
      std::swap(num_saturators[redundant], num_saturators[erasing]);
      mat.set_sorted(false);
      ++redundant;
    }
    // Adjusting the value of `num_rows' to the number of meaningful
    // rows of `mat': `num_equal_or_line' - `rank' is the number of
    // redundant equalities moved to the bottom of `mat', which are
    // no longer meaningful.
    num_rows -= num_equal_or_line - rank;
    // Adjusting the value of `num_equal_or_line'.
    num_equal_or_line = rank;
  }


  // Now we use the definition of redundancy (given in the Introduction)
  // to remove redundant inequalities.

  // First we check the saturation rule, which provides a necessary
  // condition for an inequality to be irredundant (i.e., it provides
  // a sufficient condition for identifying redundant inequalities).
  // Let
  //   num_saturators[i] = num_sat_lines[i] + num_sat_rays_or_points[i];
  //   dim_lin_space = num_irred_lines;
  //   dim_ray_space
  //     = dim_vector_space - num_irred_equalities - dim_lin_space
  //     = num_columns - 1 - num_equal_or_line - dim_lin_space;
  //   min_sat_rays_or_points = dim_ray_space.
  //   
  // An inequality saturated by less than `dim_ray_space' _rays/points_
  // is redundant. Thus we have the implication
  //
  //   (num_saturators[i] - num_sat_lines[i] < dim_ray_space)
  //      ==>
  //        redundant(mat[i]).
  //
  // Moreover, since every line saturates all inequalities, we also have
  //     dim_lin_space = num_sat_lines[i]
  // so that we can rewrite the condition above as follows:
  //
  //   (num_saturators[i] < num_columns - num_equal_or_line - 1)
  //      ==>
  //        redundant(mat[i]).
  //
  dimension_type min_saturators = num_columns - num_equal_or_line - 1;
  for (dimension_type i = num_equal_or_line; i < num_rows; ) {
    if (num_saturators[i] < min_saturators) {
      // The inequality `mat[i]' is redundant.
      --num_rows;
      std::swap(mat[i], mat[num_rows]);
      std::swap(sat[i], sat[num_rows]);
      std::swap(num_saturators[i], num_saturators[num_rows]);
      mat.set_sorted(false);
    }
    else
      i++;
  }

  // Now we check the independence rule.
  for (dimension_type i = num_equal_or_line; i < num_rows; ) {
    bool redundant = false;
    // NOTE: in the inner loop, index `j' runs through _all_ the
    // inequalities and we do not test `sat[i] < sat[j]'.
    // Experimentation has shown that this is faster than having
    // `j' only run through the indexes greater than `i' and
    // also doing the test `sat[i] < sat[j]'.
    for (dimension_type j = num_equal_or_line; j < num_rows; ) {
      if (i == j)
	// Want to compare different rows of mat.
	++j;
      else {
	// Let us recall that each generator lies on a facet
	// (see the Introduction) of the polyhedron.
	// Given two constraints `c_1' and `c_2', if there are
	// `m' generators lying on the hyper-plane corresponding
	// to `c_1', the same `m' generators lie on the hyper-plane
	// corresponding to `c_2', too, and there is another one lying
	// on the latter but not on the former, then `c_2' is more
	// restrictive than `c_1', i.e., `c_1' is redundant.
	if (strict_subset(sat[j], sat[i])) {
	  // All the saturators of the inequality `mat[i]' are
	  // saturators of the inequality `mat[j]' too,
	  // and there exists at least one saturator of `mat[j]'
	  // which is not a saturator of `mat[i]'.
	  // It follows that inequality `mat[i]' is redundant.
	  redundant = true;
	  break;
	}
	else if (sat[i] == sat[j]) {
	  // Inequalities `mat[i]' and `mat[j]' are saturated by
	  // the same set of generators. Then we can remove either one
	  // of the two inequalities: we remove `mat[j]'.
	  --num_rows;
	  std::swap(mat[j], mat[num_rows]);
	  std::swap(sat[j], sat[num_rows]);
	  std::swap(num_saturators[j], num_saturators[num_rows]);
	  mat.set_sorted(false);
	}
	else
	  // If we reach this point, then we know that `sat[i] >= sat[j]'
	  // does not hold, so that `mat[i]' is not made redundant by
	  // inequality `mat[j]'.
	  ++j;
      }
    }
    if (redundant) {
      // The inequality `mat[i]' is redundant.
      --num_rows;
      std::swap(mat[i], mat[num_rows]);
      std::swap(sat[i], sat[num_rows]);
      std::swap(num_saturators[i], num_saturators[num_rows]);
      mat.set_sorted(false);
    }
    else
      // The inequality `mat[i]' is not redundant.
      ++i;
  }

  // Here we physically remove the redundant inequalities previously
  // moved to the bottom of `mat' and the corresponding `sat' rows.
  // NOTE: We must update `index_first_pending' of `mat', before calling
  // `erase_to_end'
  mat.set_index_first_pending_row(num_rows);
  mat.erase_to_end(num_rows);
  sat.rows_erase_to_end(num_rows);
  // At this point the first `num_equal_or_line' rows of 'mat'
  // represent the irredundant equalities, while the remaining rows
  // (i.e., those having indexes from `num_equal_or_line' to
  // `num_rows' - 1) represent the irredundant inequalities: here we
  // check if the flag is set (that of the equalities is already set).
  for (dimension_type i = num_equal_or_line; i < num_rows; ++i)
    assert(mat[i].is_ray_or_point_or_inequality());
  // Here we are checking if `mat' and `sat' have the same number of rows,
  // i.e., the new number of rows obtained excluding the rows of redundant
  // inequalities.
  // FIXME: are these assertions meaningful, given that we just invoked
  // erase_to_end(num_rows) on both matrices?
  assert(mat.num_rows() == num_rows);
  assert(sat.num_rows() == num_rows);

  // Finally, since now the sub-matrix (of `mat') of the irredundant
  // equalities is in triangular form, we back substitute (using
  // the function back_substitute()) each variables with the
  // expression obtained considering the equalities starting
  // from the last one.
  mat.back_substitute(num_equal_or_line);
  // The returned value is the number of irredundant equalities i.e.,
  // the rank of the sub-matrix of `mat' containing only equalities.
  // (See the Introduction for definition of lineality space dimension).
  return num_equal_or_line;
}
