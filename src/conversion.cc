/* Polyhedron class implementation: conversion().
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

#include "Matrix.defs.hh"
#include "SatMatrix.defs.hh"
#include "Polyhedron.defs.hh"
#include "globals.hh"
#include <cstddef>

namespace PPL = Parma_Polyhedra_Library;

/*!
  \fn static size_t PPL::Polyhedron::conversion(Matrix& source,
                                                size_t start,
                                                Matrix& dest,
                                                SatMatrix& sat,
			                        size_t num_lines_or_equalities)

  \param source  The matrix to use to convert \p dest: it
                 will be modified.
  \param start   The index of \p source row from which conversion begin.
  \param dest    The result of the conversion.
  \param sat     The matrix that tell us which lines of \p source is
                 saturated (or is only satisfied) by which lines of \p dest.
  \param num_lines_or_equalities
                 The number of lines of the polyhedron or the number of
		 equality constraints in given \p dest matrix.
  \return        The number of lines of the polyhedron or the number of
		 equality constraints in the result of conversion.

  If some of the constraints (or some of the generators if
  we are making conversion from generators to constraints) of \p source
  is redundant, it will be removed. This means that the matrix used
  to convert \p dest could be modified and then it is not constant.

  \p dest is supposed to have lines from index 0 to index
  \p num_lines_or_equalities - 1  and ray/vertex from index
  \p num_lines_or_equalities to the last row.

  \p start parameter is 0 when we have to find generators from a given 
  system of constraints.
  However \p start may not be 0. This happens, for example, when, given
  a system of constraints and the corresponding system of generators, 
  we add some new constraints to the previous system and we want to 
  find the new corresponding system of generators starting from the 
  previous one.

  Note that here the rows of \p sat are indexed by rows of \p dest
  and its columns are indexed by rows of \p source.

  \p num_lines_or_equalities indicates the number of \p dest rows that
  represent the lines of the polyhedron (in conversion from constraints
  to generators) or the number of the equality constraints (in conversion
  from generators to constraints). This parameter will be
  returned at the end of the conversion.

  We know that polyhedra can be represented by both a system of 
  constraints or a system of generators (vertices, rays and lines)
  (see it in the Introduction).
  When we have both descriptions for a polyhedron \f$P\f$
  we have what is called a <EM>double description</EM>
  (or <EM>DD pair</EM>) for \f$P\f$.

  Here, the <EM>representation matrix</EM> refers to the matrix \f$C\f$
  whose rows represent the constraints that characterize \f$P\f$
  and the <EM>generating matrix</EM>, the matrix \f$G\f$ whose rows
  represent the generators of \f$P\f$.
  We say that a pair \f$(C, G)\f$ of (real) matrices is
  a <EM>double description pair</EM> if
  \f[
    C\vect{x} \geq \vect{0}
      \quad\iff\quad
        \exists \vect{\lambda} \geq \vect{0} \mathrel{.} 
	\vect{x} = G\vect{\lambda}.
  \f]

  The term "double description" is quite natural in the sense that
  such a pair contains two different description of the same object.
  In fact, if we refer to the cone representation of a polyhedron \f$P\f$
  and we call \f$C\f$ and \f$G\f$ the matrices of constraints and
  rays respectively, we have
  \f[
    P = \{\, \vect{x} \in \Rset^n \mid C\vect{x} \geq \vect{0}\, \}
      = \{\, \vect{x} \in \Rset^n \mid \vect{x} = G\vect{\lambda}
      \text{ for some } \vect{\lambda} \geq \vect{0}\, \}.
  \f]

  Because of the theorem of Minkowski (see it in the Introduction),
  we can say that, given a \f$m \times n\f$ representation matrix
  \f$C\f$ such that \f$\mathop{\mathrm{rank}}(C) = n = dimension of
  the whole space \f$ for a non-empty polyhedron \f$P\f$,
  it is always possible to find a generating matrix \f$G\f$ for \f$P\f$
  such that \f$(C, G)\f$ is a DD pair.
  Conversely, Weil's theorem ensures that, for each generating matrix
  \f$G\f$, it is possible to find a representation matrix \f$C\f$
  such that \f$(C, G)\f$ is a DD pair.

  For efficiency reasons, our representation of polyhedra makes use
  of a double description.
  We are thus left with two problems:
    -# given \f$C\f$ find \f$G\f$ such that \f$(C, G)\f$ is a DD pair;
    -# given \f$G\f$ find \f$C\f$ such that \f$(C, G)\f$ is a DD pair.

  Using Farkas' lemma we can prove that these two problems are
  computationally equivalent (i.e., linear-time reducible to each other).
  Farkas' lemma establishes a fundamental property of vectors in
  \f$\Rset^n\f$ that, in a sense, captures the essence of duality.
  Consider a matrix \f$A \in \Rset^{m \times n}\f$ and let
  \f$\{ \vect{a}_1, \ldots, \vect{a}_m \}\f$ be its set of row vectors.
  Consider also another vector \f$\vect{c} \in \Rset^n\f$ such that,
  whenever a vector \f$\vect{y} \in \Rset^n\f$ has a non-negative projection
  on the \f$\vect{a}_i\f$'s, it also has a non-negative projection 
  on \f$\vect{c}\f$.
  The lemma states that \f$\vect{c}\f$ has this property if and only if
  it is in the cone generated by the \f$\vect{a}_i\f$'s.
  Formally, the lemma states the equivalence of the two following
  assertions:
    -# \f$
         \forall \vect{y}
           \mathrel{:} (A\vect{y} \geq 0 \implies 
	   \langle \vect{y},\vect{c} \rangle \geq 0)
       \f$;
    -# \f$
         \exists \vect{\lambda} \geq \vect{0}
           \mathrel{.} \vect{c}^\mathrm{T} = \vect{\lambda}^\mathrm{T}A
       \f$.

  With this result we can prove that \f$(C, G)\f$ is a DD pair
  if and only if \f$(G^\mathrm{T}, C^\mathrm{T})\f$ is a DD pair.

  Suppose \f$(C, G)\f$ is a DD pair.
  Thus, for each \f$x\f$ of the appropriate dimension,
  \f$C\vect{x} \geq \vect{0}\f$ if and only if
  \f$\exists \lambda \geq 0 \mathrel{.} \vect{x} = G\vect{\lambda}\f$,
  which is of course equivalent to
  \f$
    \exists \vect{\lambda} \geq \vect{0}
      \mathrel{.} \vect{x}^\mathrm{T} = \vect{\lambda}^\mathrm{T}G^\mathrm{T}
  \f$.

  First, we assume that \f$\vect{z}\f$ is such that 
  \f$G^\mathrm{T}\vect{z} \geq \vect{0}\f$
  and we will show that
  \f$\exists \vect{\mu} \geq \vect{0} \mathrel{.} 
  \vect{z} = C^\mathrm{T}\vect{\mu}\f$.
  Let \f$\vect{x}\f$ be such that \f$C\vect{x} \geq \vect{0}\f$.
  Since \f$(C, G)\f$ is a DD pair, this is equivalent to
  \f$
    \exists \vect{\lambda} \geq \vect{0}
      \mathrel{.} \vect{x}^\mathrm{T} = \vect{\lambda}^\mathrm{T}G^\mathrm{T}
  \f$,
  which, by Farkas' lemma is equivalent to
  \f$
    \forall \vect{y} \mathrel{:} (G^\mathrm{T}\vect{y} \geq \vect{0} \implies
                                 \langle \vect{y}, \vect{x} \rangle \geq 0)
  \f$.
  Taking \f$\vect{y} = \vect{z}\f$ and recalling our assumption that
  \f$G^\mathrm{T}\vect{z} \geq \vect{0}\f$
  we can conclude that \f$\langle \vect{z}, \vect{x} \rangle \geq 0\f$, 
  that is equivalent to \f$\langle \vect{x}, \vect{z} \rangle \geq 0\f$.
  We have thus established that
  \f$
    \forall \vect{x} \mathrel{:} (C\vect{x} \geq \vect{0} \implies 
    \langle \vect{x}, \vect{z} \rangle \geq 0)
  \f$.
  By Farkas' lemma, this is equivalent to
  \f$\exists \vect{\mu} \geq \vect{0} \mathrel{.} 
  \vect{z}^\mathrm{T} = \vect{\mu}^\mathrm{T} C\f$,
  which is equivalent to what we wanted to prove, that is,
  \f$\exists \vect{\mu} \geq \vect{0} \mathrel{.} 
  \vect{z} = C^\mathrm{T}\vect{\mu}\f$.

  In order to prove the reverse implication, the following observation
  turns out to be useful:
  when \f$(C, G)\f$ is a DD pair, \f$CG \geq 0\f$.
  In fact,
  let \f$\vect{e}_j\f$ be the vector whose components are all \f$0\f$ 
  apart from the \f$j\f$-th one, which is \f$1\f$.
  Clearly \f$\vect{e}_j \geq \vect{0}\f$ and, taking 
  \f$\vect{\lambda} = \vect{e}_j\f$ and
  \f$\vect{x} = G\vect{\lambda} = G \vect{e}_j\f$, we have
  \f$C\vect{x} = C(G \vect{e}_j) = (CG)\vect{e}_j \geq \vect{0}\f$,
  since \f$(C, G)\f$ is a DD pair.
  Thus, as \f$(CG)\vect{e}_j\f$ is the \f$j\f$-th column of \f$CG\f$
  and since the choice of \f$j\f$ was arbitrary, \f$CG \geq \vect{0}\f$.

  We now assume that \f$\vect{z}\f$ is such that
  \f$\exists \vect{\mu} \geq \vect{0} \mathrel{.} 
  \vect{z} = C^\mathrm{T}\vect{\mu}\f$
  and we will prove that \f$G^\mathrm{T}\vect{z} \geq \vect{0}\f$.
  By Farkas' lemma, the assumption
  \f$\exists \vect{\mu} \geq \vect{0} \mathrel{.} 
  \vect{z}^\mathrm{T} = \vect{\mu}^\mathrm{T}C\f$,
  is equivalent to
  \f$\forall \vect{y} \mathrel{:} (C\vect{y} \geq \vect{0} 
  \implies \langle \vect{y}, \vect{z} \rangle \geq 0)\f$.
  If we take \f$\vect{y} = G\vect{e}_j\f$ then \f$C\vect{y} 
                 = CG\vect{e}_j \geq 0\f$,
  since \f$CG \geq \vect{0}\f$.
  So
  \f$
    \langle \vect{y}, \vect{z} \rangle
      = (\vect{e}_j^\mathrm{T}G^\mathrm{T}) \vect{z}
      = \vect{e}_j^\mathrm{T}(G^\mathrm{T} \vect{z})
      \geq 0
  \f$,
  that is, the \f$j\f$-th component of \f$G^\mathrm{T}\vect{z}\f$ 
  is non-negative. The arbitrary choice of \f$j\f$ allows us to conclude 
  that \f$G^\mathrm{T}\vect{z} \geq \vect{0}\f$, as required.

  In view of this result, the following exposition assumes, for clarity,
  that the conversion being performed is from constraints to generators.
  Thus, even if the roles of \p source and \p dest can be interchanged,
  in the sequel we assume the \p source matrix will contain the constraints
  that represent the polyhedron and the \p dest matrix will contain
  the generator that generates it. Also, we assume that \p source is sorted.

  There are some observations that are useful to understand this function:

  Observation 1: Let \f$A\f$ be a matrix of constraints that generate
  the polyhedron \f$P\f$ and \f$\vect{c}\f$ a new constraint that must 
  be added. Suppose that there is a line \f$\vect{z}\f$ that does not 
  saturate the constraint \f$\vect{c}\f$. If we combine the old lines 
  and rays that do not saturate \f$\vect{c}\f$ (except \f$\vect{z}\f$) 
  with \f$\vect{z}\f$ such that the new ones saturate \f$\vect{c}\f$, 
  the new lines and rays also saturate the constraints  saturated by 
  the old constraints.

  In fact, if \f$\vect{y}_1\f$ is the old generator that does not saturate
  \f$\vect{c}\f$, \f$\vect{y}_2\f$ is the new one such that
  \f[
    \vect{y}_2 = \lambda \vect{y}_1 + \mu \vect{z}
  \f]
  and \f$\vect{c}_1\f$ is a previous constraint that \f$\vect{y}_1\f$ 
  and \f$\vect{z}\f$ saturates, we can see
  \f[
    \langle \vect{c}_1, \vect{y}_2 \rangle 
    = \langle \vect{c}_1, (\lambda \vect{y}_1 + \mu \vect{z}) \rangle
    = \lambda \langle \vect{c}_1, \vect{y}_1 \rangle 
       + \mu \langle \vect{c}_1, \vect{z} \rangle
       = 0 + \mu \langle \vect{c}_1, \vect{z} \rangle 
       = \mu \langle \vect{c}_1, \vect{z} \rangle
  \f]
  and
  \f[
    \mu \langle \vect{c}_1, \vect{z} \rangle = 0.
  \f]

  Proposition 1: Let \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ be distinct 
  rays of \f$P\f$.
  Then the following statements are equivalent:
  a) \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ are adjacent extreme rays 
     (see definition in theIntroduction);
  b) \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ are extreme rays and the 
     rank of the matrix composed by the constraints saturated by both 
     \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ is equal to 
     \f$d - 2\f$, where \f$d\f$ is the rank of the matrix of constraints.

  In fact, let \f$F\f$ be the system of generators that saturate the
  constraints saturated by both \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$.
  If b) holds, the set \f$F\f$ is 2-dimensional and \f$\vect{r}_1\f$ and
  \f$\vect{r}_2\f$ generate this set. So, every generator
  \f$\vect{x}\f$ of \f$F\f$ can be built as a combination of 
  \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$, i.e.
  \f[
    \vect{x} = \lambda \vect{r}_1 + \mu \vect{r}_2.
  \f]
  This combination is non-negative because there exixt at least a
  constraints \f$c\f$ saturated by \f$\vect{r}_1\f$ and not 
  \f$\vect{r}_2\f$ (or vice versa) (because they are distinct) for which
  \f[
    \langle \vect{c}, \vect{x} \rangle \geq 0
  \f]
  and
  \f[
    \langle \vect{c}, \vect{x} \rangle 
    = \lambda \langle \vect{c}, \vect{r}_1 \rangle
                           (or = \mu \langle \vect{c}, \vect{r}_2 \rangle).
  \f]
  So, there is no other extreme ray in \f$F\f$ and a) holds.
  Otherwise, if b) does not hold, the rank of the matrix generated by
  the constraints saturated by both \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ 
  is equal to \f$d - k\f$, with \p k >= 3, the set \f$F\f$ is 
  \p k -dimensional and at least \p k extreme rays are necessary 
  to generate \f$F\f$.
  So, \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ are not adjacent and 
  a) does not hold.

  Proposition 2: When we build the new system of generators starting a
  matrix \f$A\f$ of constraints of \f$P\f$, if \f$\vect{c}\f$ is the constraint
  to add to \f$A\f$ and all lines of \f$P\f$ saturate \f$\vect{c}\f$,
  the new set of rays is the union of those rays that saturate, of
  those that satisfy and of a set \f$\overline Q\f$ of rays such that
  each of them
  -# lies on the hyper-plane represented by the k-th constraint,
  -# is a positive combination of two adjacent rays \f$\vect{r}_1\f$ and
     \f$\vect{r}_2\f$ such that the first one verifies the constraint and
     the other does not verify it.
  If the adjacency property is not taken in account, the new set of
  rays is not irredundant, in general.

  In fact, if \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ are not adjacent,
  the rank of the matrix composed by the constraints saturated by both 
  \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$ is different from \f$d - 2\f$ 
  (see the previous proposition) or neither \f$\vect{r}_1\f$ nor 
  \f$\vect{r}_2\f$ are extreme rays. Since the new ray \f$\vect{r}\f$ 
  is a combination of \f$\vect{r}_1\f$ and \f$\vect{r}_2\f$,
  it saturates the same constraints saturated by both \f$\vect{r}_1\f$ and
  \f$\vect{r}_2\f$.
  If the rank is less than \f$d - 2\f$, the rank of
  the matrix composed by \f$\vect{c}\f$ (that is saturated by \f$\vect{r}\f$)
  and by the constraints of \f$A\f$ saturated by \f$\vect{r}\f$  is less
  than \f$d - 1\f$. It means that \f$r\f$ is redundant (see in 
  the Introduction).
  If neither \f$\vect{r}_1\f$ nor \f$\vect{r}_2\f$ are extreme rays,
  they belong to a 2-dimensional face containing exactly two extreme rays
  of \f$P\f$.
  These two adjacent rays built a ray equal to \f$\vect{r}\f$ and so 
  \f$\vect{r}\f$ is redundant.
*/
size_t
PPL::Polyhedron::conversion(Matrix& source,
			    size_t start,
			    Matrix& dest,
			    SatMatrix& sat,
			    size_t num_lines_or_equalities) {
  size_t source_num_rows = source.num_rows();
  size_t source_num_columns = source.num_columns();
  size_t dest_num_rows = dest.num_rows();
  size_t dest_num_columns = dest.num_columns();

  // `sat' have the same number of columns of `source' rows and
  // the same number of rows of `dest' rows (because of the choice
  // made for the definition of `sat').
  assert(source_num_rows == sat.num_columns());
  assert(dest_num_rows == sat.num_rows());

  // Making conversion for the sub-matrix whose rows are those
  // of `source' from the one indexed by `start' to the last one.
  for (size_t k = start; k < source_num_rows; ) {

    // Constraints and generators must have the same dimension,
    // otherwise the scalar product below will bomb.
    assert(source_num_columns == dest_num_columns);

    // The i-th element of `scalar_prod' will contain the scalar product
    // of the k-th row of 'source' and the i-th row of `dest'. If this
    // product is 0 it means that the i-th generator saturates the k-th
    // constraint.
    static std::vector<Integer> scalar_prod;
    if (scalar_prod.size() < dest_num_rows)
      scalar_prod.resize(dest_num_rows);
    // `index_non_zero' will indicate the first row of `dest' that does
    // not saturate the k-th constraint: we initialize it to
    // `dest_num_rows' and we will change it if we will find a
    // previous row that does not saturate the constraint.
    size_t index_non_zero = dest_num_rows;
    for (size_t i = 0; i < dest_num_rows; ++i) {
      scalar_prod[i] = source[k] * dest[i];
      if (index_non_zero == dest_num_rows && scalar_prod[i] != 0)
	// We have not already changed the initial value of
	// `index_non_zero' and we found a row that does not saturate
	// the k-th constraint, with index i that is less than dest_num_rows.
	index_non_zero = i;
    }
    // The case treated below is the one when `index_non_zero' is
    // less than `num_lines_or_equalities', i.e., there exists a
    // bidirectional ray (`dest row') that does not saturate the
    // k-th constraint.
    // The other case (described later) that may arise is that all
    // the bidirectional rays represented by `dest' first
    // `num_or_equalities' rows saturate the k-th constraint.

    // Since the `index_non_zero'-th row of `dest' does not
    // saturate the k-th constraint it can not to be a line (see
    // saturation rule in the Introduction); so, if it is placed
    // among the lines we have to move it below, except if it
    // is already placed after all the lines.
    // Note that now the number of `dest' rows that saturate the k-th
    // constraint is one less.

    if (index_non_zero < num_lines_or_equalities) {
      --num_lines_or_equalities;
      if (index_non_zero != num_lines_or_equalities) {
	std::swap(dest[index_non_zero],
		  dest[num_lines_or_equalities]);
	std::swap(scalar_prod[index_non_zero],
		  scalar_prod[num_lines_or_equalities]);
      }
      // The fact that a generator does not saturate the constraint means that
      // that the generator does not lie on the hyper-plane represented by
      // the constraints: it belongs to one of the two half-spaces generated
      // by the hyper-plane. For this reason the generator cannot be a line
      // of the polyhedron.
      // On the other hand, we know (see saturation rule in the Introduction)
      // that every line of a polyhedron must saturate all equalities
      // and inequalities, then the found generator (now swapped with the
      // `num_lines_or_equalities'-th one) can not to be a line and
      // we set it to ray/vertex.
      dest[num_lines_or_equalities].set_is_ray_or_vertex_or_inequality();

      // We have modified dest so it is no more sorted.
      dest.set_sorted(false);

      // The new lineality space is obtained considering a set of new
      // lines that must lie on the hyper-plane represented by
      // the k-th constraint, i.e., the scalar product between the k-th
      // constraint and the i-th generator must be 0. So we have to consider
      // the lines (they are placed in `dest' from the `index_non_zero'-th
      // row and the `num_lines_or_equalities' - 1-th one) that do
      // not saturate the k-th constraint and
      // combine them (in a linear combination) with the
      // `num_lines_or_equalities'-th one such that the new generators
      // saturate the constraint. So, for the observation 1,
      // the new lines saturate the costraints that the old corresponding
      // ones saturate and also the `k'-th constraint.

      for (size_t i = index_non_zero; i < num_lines_or_equalities; ++i) {
	if (scalar_prod[i] != 0) {
	  // The following fragment optimizes the computation of
	  //
	  // Integer scale = scalar_prod[i];
	  // scale.gcd_assign(scalar_prod[num_lines_or_equalities]);
	  // Integer scaled_sp_i = scalar_prod[i] / scale;
	  // Integer scaled_sp_n
	  //   = scalar_prod[num_lines_or_equalities] / scale;
	  // for (size_t c = dest_num_columns; c-- > 0; ) {
	  //   dest[i][c] *= scaled_sp_n;
	  //   dest[i][c] -= scaled_sp_i * dest[num_lines_or_equalities][c];
	  // }
	  tmp_Integer(1).gcd_assign(scalar_prod[i],
				   scalar_prod[num_lines_or_equalities]);
	  tmp_Integer(2)
	    .exact_div_assign(scalar_prod[i],
			      tmp_Integer(1));
	  tmp_Integer(3)
	    .exact_div_assign(scalar_prod[num_lines_or_equalities],
			      tmp_Integer(1));
	  for (size_t c = dest_num_columns; c-- > 0; ) {
	    tmp_Integer(4).mul_assign(tmp_Integer(3),
				     dest[i][c]);
	    tmp_Integer(5).mul_assign(tmp_Integer(2),
				     dest[num_lines_or_equalities][c]);
	    dest[i][c].sub_assign(tmp_Integer(4), tmp_Integer(5));
	  }

	  dest[i].normalize();
	  scalar_prod[i] = 0;
	}
      }
      // Since the `num_lines_or_equalities'-th generator does not lie on
      // the hyper-plane correspondent to the k-th constraint, we have
      // to choose the half-line that belongs to the half-space we
      // are considering. The right one is the one that multiplied by the
      // k-th constraint give a positive result (i.e., the one that
      // satisfy the constraint). If it is not the case, we
      // multiply the `num_lines_or_equalities'-th row of `dest' by -1.
      if (scalar_prod[num_lines_or_equalities] < 0) {
	scalar_prod[num_lines_or_equalities].negate();
	for (size_t j = source_num_columns; j-- > 0; )
	  // source and dest have the same number of columns:
	  // we can use j (running through source's columns)
	  // to change sign to all the elements of the
	  // `num_lines_or_equalities'-th row of dest.
	  dest[num_lines_or_equalities][j].negate();
      }
      // To build the new pointed cone we need the new oriented ray: we
      // have to compute a positive combination of this ray with each of
      // the old extremal ray (they are placed in `dest' from the
      // `num_lines_or_equalities' row to the last one).
      // As we seen for the lineality space, we have to consider the rays
      // that do not lie on the hyper-plane represented by the k-th
      // constraint.

      // In both cases (the new lineality space computation and the
      // new pointed cone computation), for every new generator built, we
      // set to 0 the element of `scalar_prod' that correspond to the
      // scalar product between the generator itself and the k-th constraint
      // because it is what we wanted to happen building the new generators.
      for (size_t i = num_lines_or_equalities + 1; i < dest_num_rows; ++i) {
	if (scalar_prod[i] != 0) {
	  // The following fragment optimizes the computation of
	  //
	  // Integer scale = scalar_prod[i];
	  // scale.gcd_assign(scalar_prod[num_lines_or_equalities]);
	  // Integer scaled_sp_i = scalar_prod[i] / scale;
	  // Integer scaled_sp_n
	  // = scalar_prod[num_lines_or_equalities] / scale;
	  // for (size_t c = dest_num_columns; c-- > 0; ) {
	  //   dest[i][c] *= scaled_sp_n;
	  //   dest[i][c] -= scaled_sp_i * dest[num_lines_or_equalities][c];
	  // }
	  tmp_Integer(1).gcd_assign(scalar_prod[i],
				   scalar_prod[num_lines_or_equalities]);
	  tmp_Integer(2)
	    .exact_div_assign(scalar_prod[i],
			      tmp_Integer(1));
	  tmp_Integer(3)
	    .exact_div_assign(scalar_prod[num_lines_or_equalities],
			      tmp_Integer(1));
	  for (size_t c = dest_num_columns; c-- > 0; ) {
	    tmp_Integer(4).mul_assign(tmp_Integer(3),
				     dest[i][c]);
	    tmp_Integer(5).mul_assign(tmp_Integer(2),
				     dest[num_lines_or_equalities][c]);
	    dest[i][c].sub_assign(tmp_Integer(4), tmp_Integer(5));
	  }

	  dest[i].normalize();
	  scalar_prod[i] = 0;
	}
      }
      // Since the `num_lines_or_equalities' generator is such that its
      // scalar product with the k-th constraint is positive (we have
      // chosen it this way) and it does not saturate the constraint, if
      // the k-th constraint is an inequality we are sure that the
      // generator only verifies the constraint, so we set to 1 the
      // corresponding element of `sat'. This is what we do with the
      // following instruction.
      if (source[k].is_ray_or_vertex_or_inequality())
	sat[num_lines_or_equalities].set(k);
      // If the k-th constraint is an equality, as we have chosen the
      // `num_lines_or_equalities' generator such above, it does not verify
      // the constraint, so it has to be removed from the system of generators.
      else {
	--dest_num_rows;
	std::swap(dest[num_lines_or_equalities],
		  dest[dest_num_rows]);
	std::swap(scalar_prod[dest_num_rows],
		  scalar_prod[num_lines_or_equalities]);
	std::swap(sat[num_lines_or_equalities],
		  sat[dest_num_rows]);
	dest.set_sorted(false);
      }
      ++k;
    }
    // Now we treat the case in which all the lines between
    // `start' and `num_lines_or_equalities' `dest' positions
    // saturate the k-th constraint, i.e., `index_non_zero' is not less than
    // `num_lines_or_equalities'.

    // In this case we want `dest' to be like this:
    // -# from the 0-th row to the `num_lines_or_equalities' - 1-th one
    //    we put the lines of the polyhedron;
    // -# from the `num_lines_or_equalities'-th to the
    //    `lines_or_equal_bound' - 1-th one we put the rays that saturate
    //	  the k-th constraint;
    // -# from the `lines_or_equal_bound'-th to the
    //    `sup_bound' - 1-th one we put the rays that verify
    //    the k-th constraint;
    // -# from the `sup_bound'-th to the
    //    `dest_num_rows' - 1-th one we put the rays that do not verify
    //	  the k-th constraint.
    else {
      size_t lines_or_equal_bound = num_lines_or_equalities;
      size_t sup_bound = num_lines_or_equalities;
      size_t inf_bound = dest_num_rows;
      while (inf_bound > sup_bound) {
	int sp_sign = sgn(scalar_prod[sup_bound]);
	if (sp_sign == 0) {
	  std::swap(dest[sup_bound], dest[lines_or_equal_bound]);
	  std::swap(scalar_prod[sup_bound], scalar_prod[lines_or_equal_bound]);
	  std::swap(sat[sup_bound], sat[lines_or_equal_bound]);
	  ++lines_or_equal_bound;
	  ++sup_bound;
	  dest.set_sorted(false);
	}
	else if (sp_sign < 0) {
	  --inf_bound;
	  std::swap(dest[sup_bound], dest[inf_bound]);
	  std::swap(scalar_prod[sup_bound], scalar_prod[inf_bound]);
	  std::swap(sat[sup_bound], sat[inf_bound]);
	  dest.set_sorted(false);
	}
	else
	  ++sup_bound;
      }

      // When the k-th constraint is an inequality and
      // `sup_bound' is equal to `dest_num_rows', it means that
      // none of the considered generators do not verify the constraint;
      // then we have not decremented `inf_bound' and the while
      // loop terminates when `inf_bound' and `sup_bound' are both equal to
      // `dest_num_rows'.
      // Thus all the generators verify the k-th constraint and it can be
      // removed from the constraint since it is redundant. We thus
      // modify `source': this explains why it is not a constant parameter.
      if (source[k].is_ray_or_vertex_or_inequality()
	  && sup_bound == dest_num_rows) {
	--source_num_rows;
	// Is not necessary to do the swap on the columns of `sat'
	// because at this point the elements of `sat' are
	// all zeroes from the k-th columns to the end.
	std::swap(source[k], source[source_num_rows]);
	source.set_sorted(false);
      }
      else {
	// Now we have to distinguish two cases:
	// -# none of the generators verify the k-th constraint,
	// -# some of the generators verify the k-th constraint and some of
	//    them do not verify it.
	if (sup_bound == num_lines_or_equalities)
	  // If we are building the set of generators starting from
	  // a set of constraints, this situation means that the constraint
	  // represented by the rows of `dest' from the
	  // `num_lines_or_equalities' index to the `dest_num_rows'
	  // one are not satisfied by the k-th generator
	  // in `source' and these constraints have to be removed
	  // from `dest', i.e., we reduce the number of rows
	  // of the dest matrix to `num_lines_or_equalities'.
	  dest_num_rows = num_lines_or_equalities;
	else {
	  // The remaining case is when there are some of the generators
	  // that verify the k-th constraint and some of them that do not
	  // verify it.

	  // In this case we have to build the new pointed cone as the set
	  // union of the rays that saturate the k-th constraint, the rays
	  // that verify it and a set of rays such that each of them
	  // -# lies on the hyper-plane represented by the k-th constraint,
	  // -# is a positive combination of two adjacent rays such that
	  //    the first one verifies the constraint and the other does
	  //    not verify it.
	
	  // The adjacency property is necessary to have an irredundant
	  // set of new rays (see proposition 2).
	  size_t bound = dest_num_rows;

	  // Checking if the i-th generator (that verifies the
	  // k-th constraint) and the j-th generator (that does
	  // not verify it) are adjacent, i.e.,
	  // if does not exist a generator that saturates all the constraints
	  // saturated by both the i-th and the j-th generator.
	  for (size_t i = lines_or_equal_bound; i < sup_bound; ++i)
	    // i runs through the rows of dest containing the
	    // rays that verify the k-th constraint.
	    for(size_t j = sup_bound; j < bound; ++j) {
	      // j runs through the rows of dest containing
	      // the rays that do not verify the k-th constraint.
	      SatRow new_satrow;
	      assert(sat[i].last() < 0 || unsigned(sat[i].last()) < k);
	      assert(sat[j].last() < 0 || unsigned(sat[j].last()) < k);
	      // `new_satrow' is a Boolean row that has 1 in position
	      // where `sat[i]' or `sat[j]' has 1; this new row of
	      // `sat' correspond to a ray that verify all the constraints
	      // verified by both `dest[i]' and `dest[j]'.
	      set_union(sat[i], sat[j], new_satrow);

	      // `num_common_satur' indicates the number of constraints
	      // that are saturated by both `dest[i]' and `dest[j]'.
	      // Note that the set to 1 bits of `sat[i]' and `sat[j]'
	      // have to be less than `k' because we are treating the
	      // `k'-th constraint: we use 'sat.set()' only with k argument.
	      size_t num_common_satur = k - new_satrow.count_ones();
	      if (num_common_satur >=
		  source_num_columns - num_lines_or_equalities - 2) {
		// To check if the new ray is an extremal ray we refer
		// to the definition of a minimal proper face
		// (see comments in Polyhedron.defs.hh); we can say that
		// an extremal ray saturates at least `n' - `t' - 1
		// constraints where `n' is the dimension of the
		// space and `t' is the dimension of the lineality
		// space of the considered cone. Since the first columns
		// of `source' contains the inhomogeneous terms, in
		// our case the space dimension is `source_num_columns - 1'.
		// Also the dimension of the lineality space is
		// `num_lines_or_equalities', then if a ray does not
		// saturate at least
		// `source_num_columns - num_lines_or_equalities - 2'
		// it is not an extremal ray. Otherwise it is an extremal
		// ray and we have to check if it is redundant.
		bool redundant = false;
		for (size_t l = num_lines_or_equalities; l < bound; ++l)
		  // Check if `dest[i]' and `dest[j]' are adjacent (see
		  // definition in the Introduction) otherwise
		  // the new ray is redundant.
		
		  if (l != i && l != j && sat[l] <= new_satrow) {
		    redundant = true;
		    break;
		  }
		if (!redundant) {	
		  // If the new ray obtained as positive combination of
		  // the i-th and the j-th ones is not redundant, we add it
		  // to `dest' and we add the correspondent row to `sat'.
		  if (dest_num_rows == dest.num_rows()) {
		    // Make room for one more row.
		    dest.add_row(Row::RAY_OR_VERTEX_OR_INEQUALITY);
		    sat.add_row(new_satrow);
		  }
		  else
		    sat[dest_num_rows] = new_satrow;

		  Row& new_row = dest[dest_num_rows];
		  // The following fragment optimizes the computation of
		  //
		  // Integer scale = scalar_prod[i];
		  // scale.gcd_assign(scalar_prod[j]);
		  // Integer scaled_sp_i = scalar_prod[i] / scale;
		  // Integer scaled_sp_j = scalar_prod[j] / scale;
		  // for (size_t c = dest_num_columns; c-- > 0; ) {
		  //   new_row[c] = scaled_sp_i * dest[j][c];
		  //   new_row[c] -= scaled_sp_j * dest[i][c];
		  // }
		  tmp_Integer(1).gcd_assign(scalar_prod[i],
					   scalar_prod[j]);
		  tmp_Integer(2).exact_div_assign(scalar_prod[i],
						 tmp_Integer(1));
		  tmp_Integer(3).exact_div_assign(scalar_prod[j],
						 tmp_Integer(1));
		  for (size_t c = dest_num_columns; c-- > 0; ) {
		    tmp_Integer(4).mul_assign(tmp_Integer(2), dest[j][c]);
		    tmp_Integer(5).mul_assign(tmp_Integer(3), dest[i][c]);
		    new_row[c].sub_assign(tmp_Integer(4), tmp_Integer(5));
		  }
		  new_row.normalize();
		  // Since we added a new row to `dest', we have to add
		  // a new element to `scalar_prod' too; we add 0 because
		  // the new ray lies on the hyper-plane represented by
		  // the `k'-th constraint.
		  assert(scalar_prod.size() >= dest_num_rows);
		  if (scalar_prod.size() <= dest_num_rows)
		    scalar_prod.push_back(Integer::zero());
		  else
		    scalar_prod[dest_num_rows] = Integer::zero();
		  ++dest_num_rows;
		}
	      }
	    }
	  // Now we want to substitute the rays that do not verify the
	  // `k'-th constraint with new added rays.
	  size_t j;
	  if (source[k].is_ray_or_vertex_or_inequality()) {
	    //If the `k'-th constraint is an inequality, the `dest' rows
	    // that do not verify it are the ones from the `sup_bound'
	    // index to the `bound' index.
	    j = sup_bound;
	    // All the dest's rows in the index range
	    // [lines_or_equal_bound, sup_bound-1]
	    // satisfy the `k'-th constraint.
	    // We record this fact in the saturation matrix.
            for (size_t l = lines_or_equal_bound; l < sup_bound; ++l)
              sat[l].set(k);
	  }
	  else
	    // If the `k'-th constraint is an equality, the `dest' rows
	    // that do not verify it are the ones from the
	    // `lines_or_equal_bound' index to the `bound' index.
	    j = lines_or_equal_bound;
	  // Since the added rays are the ones, in the `dest' matrix,
	  // from the `bound' index to the `dest_num_rows' index, we
	  // swap these new ray with the ones that do not verify the
	  // `k'-th constraint.
	
	  //`i' runs through the added rays and `j' runs through the rays
	  // that do not verify the `k'-th constraint.
	  size_t i = dest_num_rows;
	  while (j < bound && i > bound) {
	    --i;
	    std::swap(dest[i], dest[j]);
	    std::swap(scalar_prod[i], scalar_prod[j]);
	    std::swap(sat[i], sat[j]);
	    ++j;
	    dest.set_sorted(false);
	  }
	  // Setting the new dimension of `dest':
	  // - if the number of rays that do not verify the 'k'-th constraint
	  //   is less (or equal) than the number of the added rays, we
	  //   set to `i' the number of 'dest' rows because the added rays
	  //   after this index have already been swapped;
	  // - else we set to `j' the number of `dest' rows because
	  //   the rays after this index are the ones that do not verify
	  //   the `k'-th constraint.
	  dest_num_rows = (j == bound) ? i : j;
	}
	// We have to consider next constraint.
	++k;
      }
    }
  }
  // Since we may have deleted some redundant constraint from `source' or
  // some redundant extremal ray from `dest', we have to delete the
  // useless rows of both the matrices.

  if (source_num_rows < source.num_rows())
    source.erase_to_end(source_num_rows);
  if (dest_num_rows < dest.num_rows()) {
    dest.erase_to_end(dest_num_rows);
    sat.rows_erase_to_end(dest_num_rows);
  }
  sat.columns_erase_to_end(source_num_rows);
  return num_lines_or_equalities;
}
