/* Poly class implementation: simplify().
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

#include "Polyhedron.defs.hh"
#include "SatMatrix.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

/*!
  \fn static int PPL::Polyhedron::simplify(Matrix& mat,
                                           SatMatrix& sat)
				
  \param mat          The matrix to simplify: it will be modified.
  \param sat          The saturation matrix corresponding to \p mat.

  \return             The rank of \p mat.

  \p mat will be modified swapping some of its rows and maybe removing
  some of them: this is because the argument is not <CODE>const</CODE>.

  If \p mat is a matrix of constraints, the rows of \p sat are indexed
  by constraints and the columns of \p sat are indexed by generators;
  otherwise the rows indicate the generators and the columns
  indicate the constraints.


  Given a matrix of constraints or a matrix of generators, this function
  simplifies it using Gauss' elimination method (to remove redundant
  equalities/lines), deleting redundant inequalities/rays/vertices and
  making back-substitution.
  Just as we did for \p conversion(), we will
  explain it assuming that we are given a matrix of constraints,
  since, because of duality, the case for a matrix of generators is similar.

  To do this, we will use the redundancy definition (See the Introduction).

  First we make some observations that can help the reader
  in understanding the function:


  Proposition: An inequality that is saturated by all the generators
  can be transformed into an equality.

  In fact, any vectors obtained combining generators that saturate
  the constraints will also saturate the constraints:
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
  size_t num_rows = mat.num_rows();
  size_t num_columns = mat.num_columns();
  size_t num_cols_sat = sat.num_columns();

  // Looking for the first inequality in `mat'.
  size_t num_equal_or_line;
  for (num_equal_or_line = 0;
       num_equal_or_line < num_rows;
       ++num_equal_or_line ) {
    if (mat[num_equal_or_line].is_ray_or_vertex_or_inequality())
      break;
  }

  // `num_saturators' will contain, in the i-th position,
  // the number of generators that saturate the i-th constraint.
  static std::vector<size_t> num_saturators;
  num_saturators.reserve(num_rows);
  // We want to group all the equalities in the top rows of `mat':
  // between rows indexed by `0' and `num_equal_or_line' - 1. The
  // remaining rows (from the `num_equal_or_line'-th to the last one)
  // will contain inequalities. Thus we start checking from the
  // `num_equal_or_line'-th row that is the first inequality found.
  for (size_t i = num_equal_or_line; i < num_rows; ++i) {
    if (mat[i].is_line_or_equality() || sat[i].empty()) {
      // Note that an inequality `mat[i]' saturated by all the generators
      // (i.e., such that `sat[i]' contains only zeroes) can be transformed
      // in an equality (see proposition).
      mat[i].set_is_line_or_equality();
      // Moving the found equality (and the corresponding row of `sat')
      // after the ones in the top of `mat'.
      std::swap(mat[i], mat[num_equal_or_line]);
      std::swap(sat[i], sat[num_equal_or_line]);
      std::swap(num_saturators[i], num_saturators[num_equal_or_line]);
      ++num_equal_or_line;
      // After swapping mat is not sorted anymore.
      mat.set_sorted(false);
    }
    else {
      // If `mat[i]' is not an equality or it cannot be transformed into
      // an equality, then we store the number of generators that
      // saturate it in `num_saturators[i]'.
      num_saturators[i] = num_cols_sat - sat[i].count_ones();
    }
  }
  // Now that `mat' is ordered how the function gauss() requires (i.e.,
  // the equalities at the top), we can invoke the elimination method
  // to simplify the system of equalities, obtaining the rank
  // of `mat' as result.
  size_t rank = mat.gauss();
  // Since Gauss' elimination works on the equalities system, the
  // order of inequalities in `mat' is unchanged.
  // Also, now we have irredundant equalities in the first `rank' rows
  // (from 0-th to `rank' - 1-th) of `mat' and the redundant ones from
  // the `rank'-th row to the `num_equal_or_line' - 1 one.
  // The rows containing equalities are not sorted.

  // If the rank of `mat' is `num_equal_or_line' it means that there
  // are not redundant equalities. Otherwise, to remove redundant
  // equalities we simply move corresponding `mat' rows to the
  // bottom of the matrix: we will erase them later.
  if (rank < num_equal_or_line) {
    // The index `j' runs through redundant equalities and `i'
    // indexes the first of the rows of `mat' that will be erased.
    // Since we decrement `i' and increment `j' at every step, we
    // have to check two condition:
    // - `j' has not to cross over the `num_equal_or_line' - 1 -th row,
    // - `i' has to index an inequality row: if it is not the case
    //    it means that the redundant equalities are already at the bottom.
    for (size_t i = num_rows, j = rank;
	 j < num_equal_or_line && i > num_equal_or_line; ) {
      --i;
      std::swap(mat[j], mat[i]);
      std::swap(num_saturators[j], num_saturators[i]);
      std::swap(sat[j], sat[i]);
      // After swapping mat is not sorted anymore.
      mat.set_sorted(false);
      ++j;
    }
    // `num_equal_or_line' - `rank' is the number of redundant
    // equalities moved to the bottom of `mat', below the inequalities.
    // Subtracting it from the number of rows of `mat', we obtain
    // the meaningful number of rows of `mat' (i.e., irredundant
    // equalities plus inequalities).
    num_rows -= num_equal_or_line - rank;
    // At this point the only equalities to consider are the irredundant
    // ones and they are in number of rank.
    num_equal_or_line = rank;
  }
  // Now we use the redundancy definition (given in the Introduction)
  // to remove redundant inequalities.
  for (size_t i = num_equal_or_line; i < num_rows; ) {
    // i runs through the inequalities.
    if (num_saturators[i] < num_columns - num_equal_or_line - 1) {
      // Here we check if the saturation rule holds.
      // To be irredundant, an inequality has to be saturated by at least
      // n rays/vertices, where n is the dimension of the ray space.
      // Because of the dimensionality rule (see in the Introduction),
      // the dimension of the ray space is the dimension of the space
      // minus the number of irredundant lines, minus the number of
      // irredundant equalities. Then an inequality is redundant if
      //   `nb of columns' - 1 - nb of irr. lines - nb of irr. equalities
      // 	     > nb of rays/vertices saturators.
      // Since every line saturates all inequalities (and equalities), the
      // number of the lines that saturate an inequality is just the
      // number of (irredundant lines). Moreover we can write
      //  `nb of columns' - 1 - nb of irr. equalities
      //   > nb of irr. lines + nb of rays/vertices saturators,
      // where nb of irr. lines + nb of rays/vertices saturators
      // is the total number of generators that saturate the inequality,
      // i.e., `num_saturators[i]'. This is because we use the above
      // condition: if the i-th inequality is satisfied, then it is redundant
      // and we can remove it.
      --num_rows;
      std::swap(mat[i], mat[num_rows]);
      std::swap(sat[i], sat[num_rows]);
      std::swap(num_saturators[i], num_saturators[num_rows]);
      mat.set_sorted(false);
    }
    else
      i++;
  }

  // Now we check if the independence rule holds comparing each couple
  // of inequalities.
  for (size_t i = num_equal_or_line; i < num_rows; ) {
    // i run through inequalities.
    bool redundant = false;
    for (size_t j = num_equal_or_line; j < num_rows; ) {
      // j run through inequalities.
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
	if (sat[i] > sat[j]) {
	  // Note that `sat[i]' > `sat[j]' means that the generators
	  // that saturate the `i'-th constraint, saturate the
	  // `j'-th constraint, too, and there is at least one
	  // generator that saturates the `j'-th constraint but not
	  // the `i'-th one, then on the hyper-plane corresponding to
	  // the `j'-th constraint lies one more generator than
	  // the ones lying on the hyper-plane corresponding to the
	  // `i'-th one. It follows that (see comment above)
	  // the `i'-th constraint is redundant.
	  redundant = true;
	  break;
	}
	else if (sat[i] == sat[j]) {
	  // Note that `sat[i]' == `sat[j]' means that the `i'-th
	  // inequality is saturated by the same generators that
	  // saturate the `j'-th one and then we can remove either one of
	  // the two constraints: we decided to remove the `j'-th one.
	  --num_rows;
	  std::swap(mat[j], mat[num_rows]);
	  std::swap(num_saturators[j], num_saturators[num_rows]);
	  std::swap(sat[j], sat[num_rows]);
	  mat.set_sorted(false);
	}
	else
	  // If a couple of `sat' rows is not comparable it means that
	  // the corresponding constraints are satisfied by different
	  // generators and then no one is redundant. Thus we test
	  // another couple of constraints: we compare the `i'-th
	  // inequality with a next one.
	  ++j;
      }
    }
    if (redundant) {
      // This is the case, commented above, in which we can remove
      // the `i'-th inequality as it is redundant. Thus we move it
      // to the bottom of mat.
      --num_rows;
      std::swap(mat[i], mat[num_rows]);
      std::swap(num_saturators[i], num_saturators[num_rows]);
      std::swap(sat[i], sat[num_rows]);
      mat.set_sorted(false);
    }
    else
      // If the i-th inequality is not redundant we compare the next
      // one with all the other inequalities.
      ++i;
  }

  // Here we physically remove the redundant inequalities previously
  // moved to the bottom of `mat' and the corresponding `sat' rows.
  mat.erase_to_end(num_rows);
  sat.rows_erase_to_end(num_rows);
  // At this point the first `num_line_or_equal' rows of 'mat'
  // represent the irredundant equalities, while the remaining rows
  // (i.e., between the `num_line_or_equal' index and
  // `num_rows' - 1 one) represent the irredundant inequalities: here we
  // check if the flag is set (that of the equalities is already set).
  for (size_t i = num_equal_or_line; i < num_rows; ++i)
    assert(mat[i].is_ray_or_vertex_or_inequality());
  // Here we are checking if `mat' and `sat' have the same number of rows,
  // i.e., the new number of rows obtained excluding the rows of redundant
  // inequalities.
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
