/* Octagonal_Shape class implementation: strong-closure function.
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
Octagonal_Shape<T>::strong_closure_assign() const {
  // A first implementation of the classical Floyd-Warshall algorithm.
  using Implementation::BD_Shapes::min_assign;

  // Do something only if necessary (zero-dim implies strong closure).
  if (marked_empty() || marked_strongly_closed() || space_dim == 0)
    return;

  // Even though the octagon will not change, its internal representation
  // is going to be modified by the closure algorithm.
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

#if COUNT
  dimension_type count = 0;
  dimension_type min_count = 0;
  dimension_type add_count = 0;
#endif

  // This algorithm is given by two steps: the first one is a simple
  // adaptation of the `shortest-path closure' using the Floyd-Warshall
  // algorithm; the second one is the `strong-coherence' algorithm.
  // It is important to note that after the strong-coherence,
  // the octagon is still shortest-path closed and hence, strongly closed.

  // Recall that, given an index `h', we indicate with `ch' the coherent
  // index, i.e., the index such that:
  //   ch = h + 1, if h is an even number;
  //   ch = h - 1, if h is an odd number.

  // Allocate it here, once and for all.
  N sum1;
  N sum2;

  // Step 1: closure.
  for (Row_Iterator k_iter = m_begin; k_iter != m_end; ++k_iter) {
    Row_Reference x_k = *k_iter;
    dimension_type rs_k = k_iter.row_size();
    dimension_type k = k_iter.index();
    Row_Iterator ck_iter = (k%2) ? k_iter-1 : k_iter+1;
    Row_Reference x_ck = *ck_iter;
    dimension_type ck = ck_iter.index();
    // First case: i < rs_k e rs_k > j.
    // Indicated with `m' the matrix of `*this', in this case
    // the step is given the following way:
    // m_i_j = min(m_i_j,
    //             m_ck_ci + m_k_j,
    //             m_k_ci + m_ck_j).
    for (dimension_type i = 0; i < rs_k; ++i) {
      dimension_type ci = coherent_index(i);
      const N& x_ck_ci = x_ck[ci];
      const N& x_k_ci = x_k[ci];

      Row_Iterator i_iter = m_begin + i;
      Row_Reference x_i = *i_iter;
      dimension_type rs_i = i_iter.row_size();
      for (dimension_type j = 0; j < rs_i; ++j) {
	const N& x_k_j = x_k[j];
	const N& x_ck_j = x_ck[j];

	add_assign_r(sum1, x_ck_ci, x_k_j, ROUND_UP);
	add_assign_r(sum2, x_k_ci, x_ck_j, ROUND_UP);
	min_assign(sum1, sum2);
	min_assign(x_i[j], sum1);

#if COUNT
	min_count+=2;
	add_count+=2;
#endif

      }
    }
    // Second case: i >= rs_k.
    for (Row_Iterator i_iter = m_begin + rs_k; i_iter != m_end; ++i_iter) {
      Row_Reference x_i = *i_iter;
      dimension_type rs_i = i_iter.row_size();
      const N& x_i_k = x_i[k];
      const N& x_i_ck = x_i[ck];

      // And j < rs_k.
      // the step is given the following way:
      // m_i_j = min(m_i_j,
      //             m_i_k + m_k_j,
      //             m_i_ck + m_ck_j).
      for (dimension_type j = 0; j < rs_k; ++j) {
	const N& x_k_j = x_k[j];
	const N& x_ck_j = x_ck[j];

	add_assign_r(sum1, x_i_k, x_k_j, ROUND_UP);
	add_assign_r(sum2, x_i_ck, x_ck_j, ROUND_UP);
	min_assign(sum1, sum2);
	min_assign(x_i[j], sum1);

#if COUNT
	min_count+=2;
	add_count+=2;
#endif

      }

      // And j >= rs_k.
      // the step is given the following way:
      // m_i_j = min(m_i_j,
      //             m_i_k + m_cj_ck,
      //             m_i_ck + m_cj_k).
      for (dimension_type j = rs_k; j < rs_i; ++j) {
	dimension_type cj = coherent_index(j);
	dimension_type ck = coherent_index(k);
	Row_Reference x_cj = *(m_begin+cj);
	const N& x_cj_k = x_cj[k];
	const N& x_cj_ck = x_cj[ck];

	add_assign_r(sum1, x_i_k, x_cj_ck, ROUND_UP);
	add_assign_r(sum2, x_i_ck, x_cj_k, ROUND_UP);
	min_assign(sum1, sum2);
	min_assign(x_i[j], sum1);

#if COUNT
	min_count+=2;
	add_count+=2;
#endif

      }
    }
  }

#if COUNT
  std::cout << "Il numero di minimi e': " << min_count << std::endl;
  std::cout << "Il numero di addizioni e': " << add_count << std::endl;
  count = min_count + add_count;
  std::cout << "Il numero totale di operazioni per la chiusura forte e': "
	    << count << std::endl;
#endif

  // Check for emptyness: the octagon is empty if and only if there is a
  // negative value in the main diagonal.
  for (Row_Iterator i = m_begin; i != m_end; ++i) {
    N& x_i_i = (*i)[i.index()];
    if (x_i_i < 0) {
      x.status.set_empty();
      return;
    }
    else {
      assert(x_i_i == 0);
      // Restore PLUS_INFINITY on the main diagonal.
      x_i_i = PLUS_INFINITY;
    }
  }

  // Step 2: we enforce the strong coherence.
  x.strong_coherence_assign();
  // The octagon is not empty and it is now strongly closed.
  x.status.set_strongly_closed();
}
