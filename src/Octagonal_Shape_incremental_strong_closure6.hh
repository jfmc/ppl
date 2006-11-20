/* Octagonal_Shape class implementation: incremental strong-closure function.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

template <typename T>
void
Octagonal_Shape<T>
::incremental_strong_closure_assign(const Variable var) const {
  // Enea's code.
  using Implementation::BD_Shapes::min_assign;

  // `var' should be one of the dimensions of the octagon.
  if (var.id() >= space_dim)
    throw_dimension_incompatible("incremental_strong_closure_assign(v)",
				 var.id());

  // Do something only if necessary.
  if (marked_empty() || marked_strongly_closed())
    return;

  // Zero-dimensional octagons are necessarily strongly closed.
  if (space_dim == 0)
    return;

  Octagonal_Shape& x = const_cast<Octagonal_Shape<T>&>(*this);

  // Use these type aliases for short.
  typedef typename OR_Matrix<N>::row_iterator Row_Iterator;
  typedef typename OR_Matrix<N>::row_reference_type Row_Reference;
  // Avoid recomputations.
  const Row_Iterator m_begin = x.matrix.row_begin();
  const Row_Iterator m_end = x.matrix.row_end();

  // Fill the main diagonal with zeros.
  for (Row_Iterator i = m_begin; i != m_end; ++i) {
    assert(is_plus_infinity((*i)[i.index()]));
    assign_r((*i)[i.index()], 0, ROUND_NOT_NEEDED);
  }

  // Using the incremental Floyd-Warshall algorithm.
  // Step 1: Improve all constraints on variable `var'.
  const dimension_type v = 2*var.id();
  const dimension_type cv = v+1;
  Row_Iterator v_iter = m_begin + v;
  Row_Iterator cv_iter = v_iter + 1;
  Row_Reference x_v = *v_iter;
  Row_Reference x_cv = *cv_iter;
  const dimension_type rs_v = v_iter.row_size();
  const dimension_type n_rows = x.matrix.num_rows();
  N sum;
  for (Row_Iterator k_iter = m_begin; k_iter != m_end; ++k_iter) {
    const dimension_type k = k_iter.index();
    const dimension_type ck = coherent_index(k);
    const dimension_type rs_k = k_iter.row_size();
    Row_Reference x_k = *k_iter;
    Row_Reference x_ck = (k%2) ? *(k_iter-1) : *(k_iter+1);

    for (Row_Iterator i_iter = m_begin; i_iter != m_end; ++i_iter) {
      const dimension_type i = i_iter.index();
      const dimension_type ci = coherent_index(i);
      const dimension_type rs_i = i_iter.row_size();
      Row_Reference x_i = *i_iter;
      Row_Reference x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);

      const N& x_i_k = (k < rs_i) ? x_i[k] : x_ck[ci];
      if (!is_plus_infinity(x_i_k)) {
	const N& x_k_v = (v < rs_k) ? x_k[v] : x_cv[ck];
	if (!is_plus_infinity(x_k_v)) {
	  add_assign_r(sum, x_i_k, x_k_v, ROUND_UP);
	  N& x_i_v = (v < rs_i) ? x_i[v] : x_cv[ci];
	  min_assign(x_i_v, sum);
	}
	const N& x_k_cv = (cv < rs_k) ? x_k[cv] : x_v[ck];
	if (!is_plus_infinity(x_k_cv)) {
	  add_assign_r(sum, x_i_k, x_k_cv, ROUND_UP);
	  N& x_i_cv = (cv < rs_i) ? x_i[cv] : x_v[ci];
	  min_assign(x_i_cv, sum);
	}
      }
      const N& x_k_i = (i < rs_k) ? x_k[i] : x_ci[ck];
      if (!is_plus_infinity(x_k_i)) {
	const N& x_v_k = (k < rs_v) ? x_v[k] : x_ck[cv];
	if (!is_plus_infinity(x_v_k)) {
	  N& x_v_i = (i < rs_v) ? x_v[i] : x_ci[cv];
	  add_assign_r(sum, x_v_k, x_k_i, ROUND_UP);
	  min_assign(x_v_i, sum);
	}
	const N& x_cv_k = (k < rs_v) ? x_cv[k] : x_ck[v];
	if (!is_plus_infinity(x_cv_k)) {
	  N& x_cv_i = (i < rs_v) ? x_cv[i] : x_ci[v];
	  add_assign_r(sum, x_cv_k, x_k_i, ROUND_UP);
	  min_assign(x_cv_i, sum);
	}
      }

    }
  }

  // Step 2: improve the other bounds by using the precise bounds
  // for the constraints on `var'.
  for (Row_Iterator i_iter = m_begin; i_iter != m_end; ++i_iter) {
    const dimension_type i = i_iter.index();
    const dimension_type ci = coherent_index(i);
    const dimension_type rs_i = i_iter.row_size();
    Row_Reference x_i = *i_iter;
    Row_Reference x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
    const N& x_i_v = (v < rs_i) ? x_i[v] : x_cv[ci];
    // TODO: see if it is possible to optimize this inner loop
    // by splitting it into several parts, so as to avoid
    // conditional expressions.
    for (dimension_type j = 0; j < n_rows; ++j) {
      const dimension_type cj = coherent_index(j);
      Row_Reference x_cj = *(m_begin+cj);
      N& x_i_j = (j < rs_i) ? x_i[j] : x_cj[ci];
      if (!is_plus_infinity(x_i_v)) {
	const N& x_v_j = (j < rs_v) ? x_v[j] : x_cj[cv];
	if (!is_plus_infinity(x_v_j)) {
	  add_assign_r(sum, x_i_v, x_v_j, ROUND_UP);
	  min_assign(x_i_j, sum);
	}
      }
      const N& x_i_cv = (cv < rs_i) ? x_i[cv] : x_v[ci];
      if (!is_plus_infinity(x_i_cv)) {
	const N& x_cv_j = (j < rs_v) ? x_cv[j] : x_cj[v];
	if (!is_plus_infinity(x_cv_j)) {
	  add_assign_r(sum, x_i_cv, x_cv_j, ROUND_UP);
	  min_assign(x_i_j, sum);
	}
      }
    }
  }

  // Check for emptyness: the octagon is empty if and only if there is a
  // negative value on the main diagonal.
  for (Row_Iterator i = m_begin; i != m_end; ++i) {
    N& x_i_i = (*i)[i.index()];
    if (x_i_i < 0) {
      x.status.set_empty();
      return;
    }
    else {
      // Restore PLUS_INFINITY on the main diagonal.
      assert(x_i_i == 0);
      x_i_i = PLUS_INFINITY;
    }
  }

  // Step 3: we enforce the strong coherence.
  x.strong_coherence_assign();
  // The octagon is not empty and it is now strongly closed.
  x.status.set_strongly_closed();
}
