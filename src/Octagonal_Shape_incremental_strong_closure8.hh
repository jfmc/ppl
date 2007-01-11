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
  // Optimized version unrolling the first loop and using the two auxiliary
  // vectors.
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


  dimension_type count = 0;
  dimension_type min_count = 0;
  dimension_type add_count = 0;

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
  N sum1, sum2;
  Row_Reference x_k, x_ck, x_i, x_ci;

  for (Row_Iterator k_iter = m_begin; k_iter != m_end; k_iter+=2) {
    const dimension_type k = k_iter.index();
    const dimension_type ck = k+1;
    const dimension_type rs_k = k_iter.row_size();
    x_k = *k_iter;
    x_ck = *(k_iter+1);
    const N& x_v_k = (k < rs_v) ? x_v[k] : x_ck[cv];
    const N& x_cv_k = (k < rs_v) ? x_cv[k] : x_ck[v];
    const N& x_v_ck = (ck < rs_v) ? x_v[ck] : x_k[cv];
    const N& x_cv_ck = (ck < rs_v) ? x_cv[ck] : x_k[v];

    // Work on the v-th and cv-th rows.
    for (Row_Iterator i_iter = m_begin; i_iter != v_iter+2; ++i_iter) {
      const dimension_type i = i_iter.index();

      x_i = *i_iter;
      x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
      const N& x_k_i = (i < rs_k) ? x_k[i] : x_ci[ck];
      const N& x_ck_i = (i < rs_k) ? x_ck[i] : x_ci[k];

      N& x_v_i = x_v[i];
      add_assign_r(sum1, x_v_k, x_k_i, ROUND_UP);
      add_assign_r(sum2, x_v_ck, x_ck_i, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_v_i, sum1);

      N& x_cv_i = x_cv[i];
      add_assign_r(sum1, x_cv_k, x_k_i, ROUND_UP);
      add_assign_r(sum2, x_cv_ck, x_ck_i, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_cv_i, sum1);

      min_count+=4;
      add_count+=4;

    }

    for (Row_Iterator i_iter = v_iter+2; i_iter != m_end; ++i_iter) {
      const dimension_type i = i_iter.index();

      x_i = *i_iter;
      x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
      const N& x_k_i = (i < rs_k) ? x_k[i] : x_ci[ck];
      const N& x_ck_i = (i < rs_k) ? x_ck[i] : x_ci[k];

      N& x_v_i = x_ci[cv];
      add_assign_r(sum1, x_v_k, x_k_i, ROUND_UP);
      add_assign_r(sum2, x_v_ck, x_ck_i, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_v_i, sum1);

      N& x_cv_i = x_ci[v];
      add_assign_r(sum1, x_cv_k, x_k_i, ROUND_UP);
      add_assign_r(sum2, x_cv_ck, x_ck_i, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_cv_i, sum1);

      min_count+=4;
      add_count+=4;

    }

    // Work on the v-th and cv-th columns.
    const N& x_k_v = (v < rs_k) ? x_k[v] : x_cv[ck];
    const N& x_ck_v = (v < rs_k) ? x_ck[v] : x_cv[k];
    const N& x_k_cv = (cv < rs_k) ? x_k[cv] : x_v[ck];
    const N& x_ck_cv = (cv < rs_k) ? x_ck[cv] : x_v[k];
    for (Row_Iterator i_iter = m_begin; i_iter != v_iter+2; ++i_iter) {
      const dimension_type i = i_iter.index();
      const dimension_type ci = coherent_index(i);
      const dimension_type rs_i = i_iter.row_size();
      Row_Reference x_i = *i_iter;
      Row_Reference x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
      const N& x_i_k = (k < rs_i) ? x_i[k] : x_ck[ci];
      const N& x_i_ck = (ck < rs_i) ? x_i[ck] : x_k[ci];

      N& x_i_v = x_cv[ci];
      add_assign_r(sum1, x_i_k, x_k_v, ROUND_UP);
      add_assign_r(sum2, x_i_ck, x_ck_v, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_i_v, sum1);

      N& x_i_cv = x_v[ci];
      add_assign_r(sum1, x_i_k, x_k_cv, ROUND_UP);
      add_assign_r(sum2, x_i_ck, x_ck_cv, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_i_cv, sum1);

      min_count+=4;
      add_count+=4;

    }

    for (Row_Iterator i_iter = v_iter+2; i_iter != m_end; ++i_iter) {
      const dimension_type i = i_iter.index();
      const dimension_type ci = coherent_index(i);
      const dimension_type rs_i = i_iter.row_size();
      Row_Reference x_i = *i_iter;
      Row_Reference x_ci = (i%2) ? *(i_iter-1) : *(i_iter+1);
      const N& x_i_k = (k < rs_i) ? x_i[k] : x_ck[ci];
      const N& x_i_ck = (ck < rs_i) ? x_i[ck] : x_k[ci];

      N& x_i_v = x_i[v];
      add_assign_r(sum1, x_i_k, x_k_v, ROUND_UP);
      add_assign_r(sum2, x_i_ck, x_ck_v, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_i_v, sum1);

      N& x_i_cv = x_i[cv];
      add_assign_r(sum1, x_i_k, x_k_cv, ROUND_UP);
      add_assign_r(sum2, x_i_ck, x_ck_cv, ROUND_UP);
      min_assign(sum1, sum2);
      min_assign(x_i_cv, sum1);

      min_count+=4;
      add_count+=4;

    }
  }

  // Step 2: improve the other bounds by using the precise bounds
  // for the constraints on `var'.
  typename OR_Matrix<N>::element_iterator iter_ij;
  std::vector<N> vec_v(n_rows);
  std::vector<N> vec_cv(n_rows);

  // Since the index `j' of the inner loop will go from 0 up to `i',
  // the two nested loops have to be executed twice.
  for (int twice = 0; twice < 2; ++twice) {
    // Re-initialize the element iterator.
    iter_ij = x.matrix.element_begin();
    Row_Iterator x_i_iter = m_begin;

    for (dimension_type i = 0; i <= v; i += 2) {
      const dimension_type ci = i+1;
      // Storing x_v_i == x_ci_cv.
      assign_r(vec_v[i], x_v[i], ROUND_NOT_NEEDED);
      // Storing x_v_ci == x_i_cv.
      assign_r(vec_v[ci], x_v[ci], ROUND_NOT_NEEDED);
      // Storing x_cv_i == x_ci_v.
      assign_r(vec_cv[i], x_cv[i], ROUND_NOT_NEEDED);
      // Storing x_cv_ci == x_i_v.
      assign_r(vec_cv[ci], x_cv[ci], ROUND_NOT_NEEDED);
    }
    x_i_iter = v_iter+2;
    for (dimension_type i = v+2; i < n_rows; i += 2) {
      const dimension_type ci = i+1;
      x_i = *x_i_iter;
      ++x_i_iter;
      x_ci = *x_i_iter;
      ++x_i_iter;
      // Storing x_v_i == x_ci_cv.
      assign_r(vec_v[i], x_ci[cv], ROUND_NOT_NEEDED);
      // Storing x_v_ci == x_i_cv.
      assign_r(vec_v[ci], x_i[cv], ROUND_NOT_NEEDED);
      // Storing x_cv_i == x_ci_v.
      assign_r(vec_cv[i], x_ci[v], ROUND_NOT_NEEDED);
      // Storing x_cv_ci == x_i_v.
      assign_r(vec_cv[ci], x_i[v], ROUND_NOT_NEEDED);
    }

    for (dimension_type i = 0; i < n_rows; ++i) {
      const dimension_type ci = coherent_index(i);
      const N& vec_v_ci = vec_v[ci];
      const N& vec_cv_ci = vec_cv[ci];
      // Unfolding two iterations on `j': this ensures that
      // the loop exit condition `j <= i' is OK.
      for (dimension_type j = 0; j <= i; ) {
	// First iteration:
	// sum1 = x_i_v + x_v_j == x_cv_ci + x_v_j;
	// sum2 = x_i_cv + x_cv_j == x_v_ci + x_cv_j.
	add_assign_r(sum1, vec_cv_ci, vec_v[j], ROUND_UP);
	add_assign_r(sum2, vec_v_ci, vec_cv[j], ROUND_UP);
	min_assign(sum1, sum2);
	min_assign(*iter_ij, sum1);
	// Exiting the first iteration: loop index control.
	++j;
	++iter_ij;
	// Second iteration: ditto.
	add_assign_r(sum1, vec_cv_ci, vec_v[j], ROUND_UP);
	add_assign_r(sum2, vec_v_ci, vec_cv[j], ROUND_UP);
	min_assign(sum1, sum2);
	min_assign(*iter_ij, sum1);
	// Exiting the second iteration: loop index control.
	++j;
	++iter_ij;

	min_count+=4;
	add_count+=4;

      }
    }
  }

  std::cout << "Il numero di minimi e': " << min_count << std::endl;
  std::cout << "Il numero di addizioni e': " << add_count << std::endl;
  count = min_count + add_count;
  std::cout << "Il numero totale di operazioni e': " << count << std::endl;

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
