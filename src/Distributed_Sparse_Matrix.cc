/* Distributed_Sparse_Matrix class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#include <ppl-config.h>

#include "Distributed_Sparse_Matrix.defs.hh"

#include "Sparse_Matrix.defs.hh"
#include "Dense_Row.defs.hh"

#include <boost/serialization/utility.hpp>
#include <boost/mpi/nonblocking.hpp>
#include <numeric>

namespace mpi = boost::mpi;
namespace PPL = Parma_Polyhedra_Library;

// WARNING: when the following is modified, the operation_code enum must be
// updated accordingly.
const PPL::dimension_type
PPL::Distributed_Sparse_Matrix::num_operation_params[] = {
  0, // QUIT_OPERATION: (none)
  2, // CREATE_MATRIX_OPERATION: id, num_cols
  2, // COPY_MATRIX_OPERATION: source_id, id
  1, // DELETE_MATRIX_OPERATION: id
  3, // GET_ROW_OPERATION: rank, id, local_index
  3, // SET_ROW_OPERATION: rank, id, local_index
  4, // LINEAR_COMBINE_MATRIX_OPERATION: rank, id, local_row_index, col_index
  2, // REMOVE_COLUMN_OPERATION: id, col_index
  2, // REMOVE_TRAILING_COLUMNS_OPERATION: id, n
  2, // ADD_ZERO_COLUMNS_OPERATION: id, n
  2, // CHECK_OPERATION: id, num_columns
  5, // ADD_ZERO_ROWS_OPERATION: id, n, num_columns, flag_bits, old_num_rows
  3, // ADD_ROW_OPERATION: rank, id, global_index
  2, // RESET_COLUMN_OPERATION: id, column_index
  2, // REMOVE_TRAILING_ROWS_OPERATION: id, row_n
  5, // SWAP_ROWS_OPERATION: id, rank1, local_index1, rank2, local_index2
  1, // FILL_MATRIX_OPERATION: id
  1, // COMPARE_WITH_SPARSE_MATRIX_OPERATION: id
  1, // COMPUTE_WORKING_COST_OPERATION: id
  1, // MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION: id
  1, // SET_ARTIFICIAL_INDEXES_FOR_UNFEASIBLE_ROWS_OPERATION: id
  1, // ASCII_DUMP_OPERATION: id
  3, // LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION: id, k_rank, k_local_index
  2, // GET_COLUMN_OPERATION: id, column_index
  1, // GET_SCATTERED_ROW_OPERATION: id
  1, // FLOAT_ENTERING_INDEX_OPERATION: id
  1, // SET_ARTIFICIAL_INDEXES_FOR_NEW_ROWS_OPERATION: id
  4, // SET_BASE_COLUMN_OPERATION: id, rank, row_index, column_index
  1, // SET_BASE_OPERATION: id
  1, // GET_BASE_OPERATION: id
  1, // EXACT_ENTERING_INDEX_OPERATION: id
  2, // EXITING_INDEX_OPERATION: id, entering_index
  4, // REMOVE_ROW_OPERATION: id, rank_i, local_index_i, rank_last
  1, // BASE_VARIABLES_OCCUR_ONCE_OPERATION: id
  2, // GET_EXITING_AND_PIVOT: id, entering_index
  1, // SET_WORKING_COST_OPERATION: id
};

const mpi::communicator*
PPL::Distributed_Sparse_Matrix::comm_ptr = NULL;

int PPL::Distributed_Sparse_Matrix::comm_size = -1;

#define PARAMS_1 op.params[0]
#define PARAMS_2 op.params[0], op.params[1]
#define PARAMS_3 op.params[0], op.params[1], op.params[2]
#define PARAMS_4 op.params[0], op.params[1], op.params[2], op.params[3]
#define PARAMS_5 op.params[0], op.params[1], op.params[2], op.params[3], \
                 op.params[4]

void
PPL::Distributed_Sparse_Matrix
::worker_main_loop(const mpi::communicator& comm1) {
  PPL_ASSERT(comm_ptr == NULL);
  PPL_ASSERT(comm1.rank() != 0);
  comm_ptr = &comm1;
  comm_size = comm().size();

  Worker worker;

  Operation op;

  while (1) {

    mpi::broadcast(comm(), op, 0);

    if (op.code == QUIT_OPERATION) {
      PPL_ASSERT(worker.row_chunks.empty());
      break;
    }

    switch (op.code) {

    case CREATE_MATRIX_OPERATION:
      worker.create_matrix(PARAMS_2);
      break;

    case COPY_MATRIX_OPERATION:
      worker.copy_matrix(PARAMS_2);
      break;

    case DELETE_MATRIX_OPERATION:
      worker.delete_matrix(PARAMS_1);
      break;

    case GET_ROW_OPERATION:
      worker.get_row(PARAMS_3);
      break;

    case SET_ROW_OPERATION:
      worker.set_row(PARAMS_3);
      break;

    case LINEAR_COMBINE_MATRIX_OPERATION:
      worker.linear_combine_matrix(PARAMS_4);
      break;

    case REMOVE_COLUMN_OPERATION:
      worker.remove_column(PARAMS_2);
      break;

    case REMOVE_TRAILING_COLUMNS_OPERATION:
      worker.remove_trailing_columns(PARAMS_2);
      break;

    case ADD_ZERO_COLUMNS_OPERATION:
      worker.add_zero_columns(PARAMS_2);
      break;

    case CHECK_OPERATION:
      worker.check(PARAMS_2);
      break;

    case ADD_ZERO_ROWS_OPERATION:
      worker.add_zero_rows(PARAMS_5);
      break;

    case ADD_ROW_OPERATION:
      worker.add_row(PARAMS_3);
      break;

    case RESET_COLUMN_OPERATION:
      worker.reset_column(PARAMS_2);
      break;

    case REMOVE_TRAILING_ROWS_OPERATION:
      worker.remove_trailing_rows(PARAMS_2);
      break;

    case SWAP_ROWS_OPERATION:
      worker.swap_rows(PARAMS_5);
      break;

    case FILL_MATRIX_OPERATION:
      worker.fill_matrix(PARAMS_1);
      break;

    case COMPARE_WITH_SPARSE_MATRIX_OPERATION:
      worker.compare_with_sparse_matrix(PARAMS_1);
      break;

    case COMPUTE_WORKING_COST_OPERATION:
      worker.compute_working_cost(PARAMS_1);
      break;

    case MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION:
      worker.make_inhomogeneous_terms_nonpositive(PARAMS_1);
      break;

    case SET_ARTIFICIAL_INDEXES_FOR_UNFEASIBLE_ROWS_OPERATION:
      worker.set_artificial_indexes_for_unfeasible_rows(PARAMS_1);
      break;

    case ASCII_DUMP_OPERATION:
      worker.ascii_dump(PARAMS_1);
      break;

    case LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION:
      worker.linear_combine_with_base_rows(PARAMS_3);
      break;

    case GET_COLUMN_OPERATION:
      worker.get_column(PARAMS_2);
      break;

    case GET_SCATTERED_ROW_OPERATION:
      worker.get_scattered_row(PARAMS_1);
      break;

    case FLOAT_ENTERING_INDEX_OPERATION:
      worker.float_entering_index(PARAMS_1);
      break;

    case SET_ARTIFICIAL_INDEXES_FOR_NEW_ROWS_OPERATION:
      worker.set_artificial_indexes_for_new_rows(PARAMS_1);
      break;

    case SET_BASE_COLUMN_OPERATION:
      worker.set_base_column(PARAMS_4);
      break;

    case SET_BASE_OPERATION:
      worker.set_base(PARAMS_1);
      break;

    case GET_BASE_OPERATION:
      worker.get_base(PARAMS_1);
      break;

    case EXACT_ENTERING_INDEX_OPERATION:
      worker.exact_entering_index(PARAMS_1);
      break;

    case EXITING_INDEX_OPERATION:
      worker.exiting_index(PARAMS_2);
      break;

    case REMOVE_ROW_OPERATION:
      worker.remove_row(PARAMS_4);
      break;

    case BASE_VARIABLES_OCCUR_ONCE_OPERATION:
      worker.base_variables_occur_once(PARAMS_1);
      break;

    case GET_EXITING_AND_PIVOT:
      worker.get_exiting_and_pivot(PARAMS_2);
      break;

    case SET_WORKING_COST_OPERATION:
      worker.set_working_cost(PARAMS_1);
      break;

    case QUIT_OPERATION:
      PPL_ASSERT(false);

    default:
      PPL_ASSERT(false);
    }
  }
  comm_ptr = NULL;
}

#undef PARAMS_1
#undef PARAMS_2
#undef PARAMS_3
#undef PARAMS_4
#undef PARAMS_5

PPL::Distributed_Sparse_Matrix&
PPL::Distributed_Sparse_Matrix::operator=(const Sparse_Matrix& matrix) {
  broadcast_operation(DELETE_MATRIX_OPERATION, id);

  init(matrix.num_rows(), matrix.num_columns());

  broadcast_operation(FILL_MATRIX_OPERATION, id);

  std::vector<mpi::request> requests;
  requests.reserve(num_rows() - local_rows.size());

  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = mapping[i].first;
    dimension_type local_i = mapping[i].second;
    if (rank != 0) {
      // FIXME: This cast can be dangerous!
      int tag = static_cast<int>(local_i);
      requests.push_back(comm().isend(rank, tag, matrix[i]));
    }
  }

  // This loop has been splitted from the above so the worker nodes can do
  // work while the root executes its copies.
  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = mapping[i].first;
    dimension_type local_i = mapping[i].second;
    if (rank == 0)
      local_rows[local_i] = matrix[i];
  }

  mpi::wait_all(requests.begin(), requests.end());

  PPL_ASSERT(*this == matrix);
  PPL_ASSERT(OK());

  return *this;
}

bool
PPL::Distributed_Sparse_Matrix
::operator==(const Sparse_Matrix& matrix) const {

  if (num_rows() != matrix.num_rows())
    return false;

  if (num_columns() != matrix.num_columns())
    return false;

  broadcast_operation(COMPARE_WITH_SPARSE_MATRIX_OPERATION, id);

  std::vector<mpi::request> requests;
  requests.reserve(num_rows() - local_rows.size());

  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = mapping[i].first;
    dimension_type local_i = mapping[i].second;
    if (rank != 0) {
      // FIXME: This cast can be dangerous!
      int tag = static_cast<int>(local_i);
      requests.push_back(comm().isend(rank, tag, matrix[i]));
    }
  }

  bool local_result = true;

  // This loop has been splitted from the above so the worker nodes can do
  // work while the root executes its comparisons.
  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = mapping[i].first;
    dimension_type local_i = mapping[i].second;
    if (rank == 0)
      if (!(local_rows[local_i] == matrix[i])) {
        local_result = false;
        break;
      }
  }

  mpi::wait_all(requests.begin(), requests.end());

  bool result;
  mpi::reduce(comm(), local_result, result, std::logical_and<bool>(), 0);

  return result;
}

bool
PPL::Distributed_Sparse_Matrix::OK() const {

  if (comm_size != comm().size()) {
    std::cerr << "Distributed_Sparse_Matrix error: comm_size is wrong." << std::endl;
    return false;
  }

  if (next_rank < 0 || next_rank >= comm_size) {
    std::cerr << "Distributed_Sparse_Matrix error: next_rank is not valid." << std::endl;
    return false;
  }

  // 1. Check that reverse_mapping is the reverse of mapping.

  std::vector<std::vector<dimension_type> >
    correct_reverse_mapping(comm_size);
  dimension_type num_rows = mapping.size();
  const dimension_type unused_index = -(dimension_type)1;
  for (dimension_type i = 0; i < num_rows; i++) {
    int rank = mapping[i].first;
    dimension_type local_index = mapping[i].second;
    if (correct_reverse_mapping[rank].size() <= local_index)
      correct_reverse_mapping[rank].resize(local_index + 1, unused_index);
    correct_reverse_mapping[rank][local_index] = i;
  }

  if (reverse_mapping != correct_reverse_mapping) {
    std::cerr << "Distributed_Sparse_Matrix error: reverse_mapping is not the reverse of mapping." << std::endl;
    return false;
  }

  // 2. Check that the parts of reverse_mapping stored by worker nodes
  // are in sync with reverse_mapping, and that base[] has the correct size.

  broadcast_operation(CHECK_OPERATION, id, num_columns());

  std::vector<dimension_type> root_reverse_mapping;
  mpi::scatter(comm(), reverse_mapping, root_reverse_mapping, 0);

  bool local_result = (root_reverse_mapping.size() == local_rows.size());
  if (!local_result)
    std::cerr << "Distributed_Sparse_Matrix error: row check failed for root node." << std::endl;

  if (local_rows.size() != base.size()) {
    local_result = false;
    std::cerr << "Distributed_Sparse_Matrix error: base[] has wrong size, in root node." << std::endl;
  }

  for (std::vector<Sparse_Row>::const_iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    if (i->size() != num_columns()) {
      std::cerr << "Distributed_Sparse_Matrix error: column check failed for root node." << std::endl;
      local_result = false;
    }

  bool result;
  mpi::reduce(comm(), local_result, result, std::logical_and<bool>(), 0);

  if (!result) {
    std::cerr << "Distributed_Sparse_Matrix error: global row/column check failed." << std::endl;
    return false;
  }

  // 3. Check that the local row indexes for each node are increasing.

  for (int rank = 0; rank < comm_size; ++rank) {
    const std::vector<dimension_type>& vec = reverse_mapping[rank];
    if (!vec.empty()) {
      dimension_type last_local_index = vec[0];
      for (std::vector<dimension_type>::const_iterator
        i = vec.begin() + 1, i_end = vec.end(); i != i_end; ++i) {
        if (*i <= last_local_index) {
          std::cerr << "Distributed_Sparse_Matrix error: local row indexes are not ordered." << std::endl;
          return false;
        }
        last_local_index = *i;
      }
    }
  }

  return true;
}

void
PPL::Distributed_Sparse_Matrix
::map_indexes(const std::vector<dimension_type>& indexes,
              std::vector<std::vector<dimension_type> >& local_indexes) const {
  PPL_ASSERT(local_indexes.empty());
  local_indexes.resize(comm_size);
  for (std::vector<dimension_type>::const_iterator
      i = indexes.begin(), i_end = indexes.end(); i != i_end; ++i) {
    int rank = mapping[*i].first;
    dimension_type local_index = mapping[*i].second;
    local_indexes[rank].push_back(local_index);
  }
}

// This is needed because PPL_DIRTY_TEMP_COEFFICIENT works only inside
// the PPL namespace.
namespace Parma_Polyhedra_Library {

// This is a template to avoid duplicating code.
// Row_T is expected to be either Sparse_Row or Dense_Row.
template <typename Row_T>
void
incremental_linear_combine(Coefficient& scaling, Coefficient& reverse_scaling,
                           Sparse_Row& increase,
                           const Row_T& x, const Sparse_Row& y,
                           dimension_type k) {
  WEIGHT_BEGIN();
  PPL_ASSERT(scaling != 0);
  PPL_ASSERT(reverse_scaling != 0);
  const dimension_type x_size = x.size();
  Coefficient_traits::const_reference x_k = x.get(k);
  Coefficient_traits::const_reference y_k = y.get(k);
  PPL_ASSERT(y_k != 0 && x_k != 0);
  Coefficient coeff1 = y_k * reverse_scaling;
  Coefficient coeff2 = scaling * x_k;

  // Compute increase[i] and new_reverse_scaling such that
  // increase[i] * new_reverse_scaling = increase[i]*coeff1 - y[i]*coeff2, for each i.

  gcd_assign(reverse_scaling, coeff1, coeff2);
  exact_div_assign(coeff1, coeff1, reverse_scaling);
  exact_div_assign(coeff2, coeff2, reverse_scaling);

  neg_assign(coeff2);
  increase.linear_combine(y, coeff1, coeff2);

  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  increase.normalize(gcd);
  reverse_scaling *= gcd;

  scaling *= y_k;

  PPL_ASSERT(scaling != 0);
  PPL_ASSERT(reverse_scaling != 0);
  normalize2(scaling, reverse_scaling, scaling, reverse_scaling);

  PPL_ASSERT(increase[k] * reverse_scaling == - scaling * x[k]);
  PPL_ASSERT(scaling != 0);
  PPL_ASSERT(reverse_scaling != 0);
  WEIGHT_ADD_MUL(83, x_size);
}

void
linear_combine(Sparse_Row& x, const Sparse_Row& y, const dimension_type k) {
  Coefficient x_k = x.get(k);
  Coefficient y_k = y.get(k);
  PPL_ASSERT(y_k != 0 && x_k != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.

  normalize2(x_k, y_k, x_k, y_k);

  neg_assign(x_k);
  x.linear_combine(y, y_k, x_k);

  PPL_ASSERT(x.find(k) == x.end());
  x.normalize();
}

} // namespace Parma_Polyhedra_Library

const PPL::Sparse_Row&
PPL::Distributed_Sparse_Matrix
::linear_combine_matrix__common(int rank, dimension_type local_row_index,
                                dimension_type col_index, int my_rank,
                                std::vector<Sparse_Row>& local_rows) {
  if (rank == my_rank) {
    PPL_ASSERT(local_row_index < local_rows.size());
    Sparse_Row& row = local_rows[local_row_index];
    mpi::broadcast(comm(), row, rank);
    for (dimension_type i = 0; i < local_rows.size(); i++)
      if (i != local_row_index && local_rows[i].get(col_index) != 0)
        linear_combine(local_rows[i], row, col_index);
    return row;
  } else {
    static Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
    for (dimension_type i = 0; i < local_rows.size(); i++)
      if (local_rows[i].get(col_index) != 0)
        linear_combine(local_rows[i], row, col_index);
    return row;
  }
}

void
PPL::Distributed_Sparse_Matrix
::swap_rows__common(int rank1, int rank2,
                    dimension_type local_index1, dimension_type local_index2,
                    int my_rank, std::vector<Sparse_Row>& rows,
                    std::vector<dimension_type>& base) {
  PPL_ASSERT(my_rank == rank1 || my_rank == rank2);
  if (rank1 == rank2) {
    std::swap(rows[local_index1], rows[local_index2]);
    std::swap(base[local_index1], base[local_index2]);
    return;
  }
  if (rank1 > rank2) {
    // These are useful to simplify the code below.
    std::swap(rank1, rank2);
    std::swap(local_index1, local_index2);
  }
  PPL_ASSERT(rank1 < rank2);
  if (rank1 == my_rank) {
    // rank1 < rank2, and this node has rank rank1, so it will do the actual
    // swap.
    Sparse_Row row;
    dimension_type base_i;
    std::vector<mpi::request> requests;
    comm().recv(rank2, 0, row);
    comm().recv(rank2, 0, base_i);
    comm().send(rank2, 0, rows[local_index1]);
    comm().send(rank2, 0, base[local_index1]);
    std::swap(row, rows[local_index1]);
  } else {
    PPL_ASSERT(rank2 == my_rank);
    // rank1 < rank2, and this node has rank rank2, so it won't do the actual
    // swap.
    comm().send(rank1, 0, rows[local_index2]);
    comm().send(rank1, 0, base[local_index2]);
    comm().recv(rank1, 0, rows[local_index2]);
    comm().recv(rank1, 0, base[local_index2]);
  }
}

namespace {

struct compute_working_cost_reducer_functor {

  typedef std::pair<std::pair<PPL::Coefficient,
                              PPL::Coefficient>, PPL::Sparse_Row> pair_type;
  const pair_type&
  operator()(pair_type& x, const pair_type& y) const {
    PPL::Coefficient& x_scaling = x.first.first;
    PPL::Coefficient& x_reverse_scaling = x.first.second;
    PPL::Sparse_Row& x_row = x.second;
    const PPL::Coefficient& y_scaling = y.first.first;
    const PPL::Coefficient& y_reverse_scaling = y.first.second;
    const PPL::Sparse_Row& y_row = y.second;
    PPL_ASSERT(x_scaling != 0);
    PPL_ASSERT(x_reverse_scaling != 0);
    PPL_ASSERT(y_scaling != 0);
    PPL_ASSERT(y_reverse_scaling != 0);

    PPL::Coefficient x_normalized_scaling = x_scaling * y_reverse_scaling;
    PPL::Coefficient y_normalized_scaling = y_scaling * x_reverse_scaling;
    PPL::Coefficient gcd;
    PPL::gcd_assign(gcd, x_normalized_scaling, y_normalized_scaling);
    PPL::exact_div_assign(x_normalized_scaling, x_normalized_scaling, gcd);
    PPL::exact_div_assign(y_normalized_scaling, y_normalized_scaling, gcd);

    x_scaling *= y_scaling;
    x_reverse_scaling = gcd;

    x_row.linear_combine(y_row, y_normalized_scaling, x_normalized_scaling);

    x_row.normalize(gcd);
    x_reverse_scaling *= gcd;

    PPL::normalize2(x_scaling, x_reverse_scaling,
                    x_scaling, x_reverse_scaling);

    // This return is needed because mpi::reduce() requires it.
    // x will be assigned to itself, so the data will not be copied.
    return x;
  }
};

}

namespace boost {
namespace mpi {

template <>
struct is_commutative<compute_working_cost_reducer_functor,
                      std::pair<std::pair<PPL::Coefficient, PPL::Coefficient>,
                                PPL::Dense_Row> >: public mpl::true_ { };

}
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost__common(std::pair<std::pair<Coefficient, Coefficient>,
                                         Sparse_Row>& x,
                               const Sparse_Row& working_cost,
                               const std::vector<dimension_type>& base,
                               const std::vector<Sparse_Row>& local_rows) {
  Coefficient& local_scaling = x.first.first;
  Coefficient& local_reverse_scaling = x.first.second;
  Sparse_Row& local_increase = x.second;

  local_scaling = 1;
  local_reverse_scaling = 1;
  local_increase.resize(working_cost.size());

  Sparse_Row::const_iterator i = working_cost.end();
  Sparse_Row::const_iterator i_end = working_cost.end();
  for (dimension_type local_index = 0;
      local_index < local_rows.size(); ++local_index) {
    i = working_cost.lower_bound(i, base[local_index]);
    if (i != i_end && i.index() == base[local_index] && *i != 0)
      incremental_linear_combine(local_scaling, local_reverse_scaling,
                                 local_increase, working_cost,
                                 local_rows[local_index], base[local_index]);
  }
}

void
PPL::Distributed_Sparse_Matrix
::make_inhomogeneous_terms_nonpositive__common(std::vector<Sparse_Row>& rows) {
  for (dimension_type i = rows.size(); i-- > 0 ; ) {
    Sparse_Row& row = rows[i];
    if (row.get(0) > 0) {
      for (Sparse_Row::iterator
          j = row.begin(), j_end = row.end(); j != j_end; ++j)
        neg_assign(*j);
    }
  }
}

void
PPL::Distributed_Sparse_Matrix
::set_artificial_indexes_for_unfeasible_rows__common(
    const std::pair<dimension_type, std::vector<dimension_type> >&node_data,
    std::vector<Sparse_Row>& rows, std::vector<dimension_type>& base) {

  const std::vector<dimension_type>& indexes = node_data.second;
  dimension_type current_artificial = node_data.first;
  for (std::vector<dimension_type>::const_iterator
      i = indexes.begin(), i_end = indexes.end(); i != i_end; ++i) {
    rows[*i].insert(current_artificial, Coefficient_one());
    base[*i] = current_artificial;
  }
}

void
PPL::Distributed_Sparse_Matrix::linear_combine_with_base_rows__common(
    int k_rank, dimension_type k_local_index,
    int my_rank, std::vector<Sparse_Row>& local_rows,
    const std::vector<dimension_type>& base) {

  Sparse_Row a_local_row;

  Sparse_Row& row_k = (k_rank == my_rank ? local_rows[k_local_index] : a_local_row);

  mpi::broadcast(comm(), row_k, k_rank);

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;
  Coefficient& local_scaling = x.first.first;
  Coefficient& local_reverse_scaling = x.first.second;
  Sparse_Row& local_increase = x.second;

  local_scaling = 1;
  local_reverse_scaling = 1;
  local_increase.resize(row_k.size());

  if (my_rank == k_rank) {
    for (dimension_type i = local_rows.size(); i-- > 0; ) {
      if (i == k_local_index)
        continue;
      if (base[i] == 0)
        continue;
      const Sparse_Row& row = local_rows[i];
      if (row_k.get(base[i]) != 0)
        incremental_linear_combine(local_scaling, local_reverse_scaling,
                                  local_increase, row_k, row, base[i]);
    }
  } else {
    for (dimension_type i = local_rows.size(); i-- > 0; ) {
      if (base[i] == 0)
        continue;
      const Sparse_Row& row = local_rows[i];
      if (row_k.get(base[i]) != 0)
        incremental_linear_combine(local_scaling, local_reverse_scaling,
                                  local_increase, row_k, row, base[i]);
    }
  }

  PPL_ASSERT(local_scaling != 0);
  PPL_ASSERT(local_reverse_scaling != 0);

  if (k_rank != my_rank) {
    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), k_rank);
    return;
  }

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> y;
  mpi::reduce(comm(), x, y, compute_working_cost_reducer_functor(), k_rank);

  Coefficient_traits::const_reference global_scaling = y.first.first;
  Coefficient_traits::const_reference global_reverse_scaling = y.first.second;
  const Sparse_Row& global_increase = y.second;

#ifndef NDEBUG
  {
    PPL_ASSERT(global_scaling != 0);
    PPL_ASSERT(global_reverse_scaling != 0);
    // Check that global_scaling and global_increase are normalized.
    Coefficient normalized_scaling;
    Coefficient normalized_reverse_scaling;
    normalize2(global_scaling, global_reverse_scaling,
               normalized_scaling, normalized_reverse_scaling);
    PPL_ASSERT(normalized_scaling == global_scaling);
    PPL_ASSERT(normalized_reverse_scaling == global_reverse_scaling);
  }
#endif

  // Calculate the global result from global_scaling and global_increase.
  // global_result[i] == global_scaling * row_k[i]
  //                     + global_reverse_scaling * global_increase[i].
  // The global result is stored in row_k to improve performance.

  row_k.linear_combine(global_increase, global_scaling,
                       global_reverse_scaling);

  // TODO: row_k seems to be already normalized, check whether this can be
  // removed or not.
  row_k.normalize();
}

namespace {

// NOTE: the following two `assign' helper functions are needed to
// handle the assignment of a Coefficient to a double in method
//     Distributed_Sparse_Matrix::float_entering_index__common().
// We cannot use assign_r(double, Coefficient, Rounding_Dir) as it would
// lead to a compilation error on those platforms (e.g., ARM) where
// controlled floating point rounding is not available (even if the
// rounding mode would be set to ROUND_IGNORE).

inline void
assign(double& d, const mpz_class& c) {
  d = c.get_d();
}

template <typename T, typename Policy>
inline void
assign(double& d,
       const Parma_Polyhedra_Library::Checked_Number<T, Policy>& c) {
  d = raw_value(c);
}

} // namespace

void
PPL::Distributed_Sparse_Matrix
::float_entering_index__common(std::vector<dimension_type>& candidates,
                               const std::vector<dimension_type>& base,
                               const std::vector<Sparse_Row>& rows,
                               std::vector<double>& results,
                               int my_rank, const Sparse_Row& working_cost) {

  const dimension_type num_rows = rows.size();
  const dimension_type num_columns_minus_1 = working_cost.size() - 1;

  PPL_ASSERT(candidates.empty());
  {
    const int cost_sign = sgn(working_cost.get(num_columns_minus_1));
    PPL_ASSERT(cost_sign != 0);
    Sparse_Row::const_iterator i = working_cost.lower_bound(1);
    // Note that find() is equivalent to linear_combine() when searching the
    // last element.
    Sparse_Row::const_iterator i_end = working_cost.find(num_columns_minus_1);
    for ( ; i != i_end; ++i)
      if (sgn(*i) == cost_sign)
        candidates.push_back(i.index());
  }

  PPL_ASSERT(results.empty());

  if (my_rank == 0)
    results.resize(candidates.size(), 0.0);
  else
    results.resize(candidates.size(), 1.0);

  for (dimension_type i = num_rows; i-- > 0; ) {
    const Sparse_Row& tableau_i = rows[i];
    Coefficient_traits::const_reference tableau_i_base_i
      = tableau_i.get(base[i]);
    double float_tableau_denum;
    assign(float_tableau_denum, tableau_i_base_i);
    Sparse_Row::const_iterator j = tableau_i.begin();
    Sparse_Row::const_iterator j_end = tableau_i.end();
    std::vector<dimension_type>::iterator k1 = candidates.begin();
    std::vector<dimension_type>::iterator k1_end = candidates.end();
    std::vector<double>::iterator k2 = results.begin();
    std::vector<double>::iterator k2_end = results.end();
    while (j != j_end && k1 != k1_end) {
      const dimension_type column = j.index();
      while (k1 != k1_end && column > *k1) {
        ++k1;
        ++k2;
      }
      if (k1 == k1_end)
        break;
      if (*k1 > column) {
        j = tableau_i.lower_bound(j, *k1);
      } else {
        PPL_ASSERT(*k1 == column);
        Coefficient_traits::const_reference tableau_ij = *j;
        WEIGHT_BEGIN();
        if (tableau_ij != 0) {
          PPL_ASSERT(tableau_i_base_i != 0);
          double float_tableau_value;
          assign(float_tableau_value, tableau_ij);
          float_tableau_value /= float_tableau_denum;
          float_tableau_value *= float_tableau_value;
          *k2 += float_tableau_value;
        }
        WEIGHT_ADD_MUL(338, num_rows);
        ++j;
        ++k1;
        ++k2;
      }
    }
  }
}

namespace {

class exact_entering_index_reducer_functor {
public:
  const PPL::Coefficient&
  operator()(PPL::Coefficient& x, const PPL::Coefficient& y) const {
    PPL::lcm_assign(x, x, y);
    return x;
  }
};

} // namespace

namespace boost {
namespace mpi {

template <>
struct is_commutative<exact_entering_index_reducer_functor, PPL::Coefficient>
  : public mpl::true_ { };

}
}

void
PPL::Distributed_Sparse_Matrix
::exact_entering_index__common(std::vector<dimension_type>& columns,
                               std::vector<Coefficient>& challenger_values,
                               const std::vector<Sparse_Row>& rows,
                               const std::vector<dimension_type>& base,
                               Coefficient& squared_lcm_basis,
                               const Sparse_Row& working_cost) {

  {
    const int cost_sign = sgn(working_cost.get(working_cost.size() - 1));
    PPL_ASSERT(cost_sign != 0);

    Sparse_Row::const_iterator i = working_cost.lower_bound(1);
    // Note that find() is equivalent to linear_combine() when searching the
    // last element.
    Sparse_Row::const_iterator i_end
      = working_cost.find(working_cost.size() - 1);
    for ( ; i != i_end; ++i)
      if (sgn(*i) == cost_sign)
        columns.push_back(i.index());
  }

  // The normalization factor for each coefficient in the tableau.
  std::vector<Coefficient> norm_factor(rows.size());
  {
    // Compute the lcm of all the coefficients of variables in base.
    PPL_DIRTY_TEMP_COEFFICIENT(local_lcm_basis);
    local_lcm_basis = 1;
    for (dimension_type i = rows.size(); i-- > 0; )
      lcm_assign(local_lcm_basis, local_lcm_basis, rows[i].get(base[i]));

    PPL_DIRTY_TEMP_COEFFICIENT(global_lcm_basis);
    mpi::all_reduce(comm(), local_lcm_basis, global_lcm_basis,
                    exact_entering_index_reducer_functor());

    // Compute normalization factors for local rows.
    for (dimension_type i = rows.size(); i-- > 0; )
      exact_div_assign(norm_factor[i], global_lcm_basis,
                       rows[i].get(base[i]));

    // Compute the square of `global_lcm_basis', exploiting the fact that
    // `global_lcm_basis' will no longer be needed.
    global_lcm_basis *= global_lcm_basis;
    std::swap(squared_lcm_basis, global_lcm_basis);
  }

  PPL_DIRTY_TEMP_COEFFICIENT(scalar_value);

  const dimension_type columns_size = columns.size();
  challenger_values.resize(columns_size);

  for (dimension_type i = rows.size(); i-- > 0; ) {
    const Sparse_Row& row = rows[i];
    Sparse_Row::const_iterator j = row.begin();
    Sparse_Row::const_iterator j_end = row.end();
    // This will be used to index the columns[] and challenger_values[]
    // vectors.
    dimension_type k = 0;
    while (j != j_end) {
      while (k != columns_size && j.index() > columns[k])
        ++k;
      if (k == columns_size)
        break;
      PPL_ASSERT(j.index() <= columns[k]);
      if (j.index() < columns[k])
        j = row.lower_bound(j, columns[k]);
      else {
        Coefficient_traits::const_reference tableau_ij = *j;
        // FIXME: Check if the test against zero speeds up the sparse version.
        // The test against 0 gives rise to a consistent speed up: see
        // http://www.cs.unipr.it/pipermail/ppl-devel/2009-February/014000.html
        if (tableau_ij != 0) {
          scalar_value = tableau_ij * norm_factor[i];
          add_mul_assign(challenger_values[k], scalar_value, scalar_value);
        }
        ++k;
        ++j;
      }
    }
  }
}

namespace {

struct exiting_index_candidate {
  //! The local row index.
  PPL::dimension_type index;

  //! The rank of the node that sent this candidate.
  int rank;

  //! base[index].
  PPL::dimension_type base_index;

  //! row[0].
  PPL::Coefficient row_0;

  //! row[entering_index].
  PPL::Coefficient row_entering;

  template <typename Archive>
  void serialize(Archive& ar, const unsigned long /* version */) {
    ar & index;
    ar & rank;
    ar & base_index;
    ar & row_0;
    ar & row_entering;
  }
};

} // namespace

// This is needed to make the PPL_DIRTY_TEMP_COEFFICIENT macros work.
namespace Parma_Polyhedra_Library {

class exiting_index_reducer_functor {
public:
  const exiting_index_candidate&
  operator()(exiting_index_candidate& x,
             const exiting_index_candidate& y) const {
    const dimension_type unused_index = -(dimension_type)1;

    if (y.index == unused_index)
      return x;

    if (x.index == unused_index)
      x = y;
    else {
      PPL_DIRTY_TEMP_COEFFICIENT(lcm);
      PPL_DIRTY_TEMP_COEFFICIENT(current_min);
      PPL_DIRTY_TEMP_COEFFICIENT(challenger);
      lcm_assign(lcm, x.row_entering, y.row_entering);
      exact_div_assign(current_min, lcm, x.row_entering);
      current_min *= x.row_0;
      abs_assign(current_min);
      exact_div_assign(challenger, lcm, y.row_entering);
      challenger *= y.row_0;
      abs_assign(challenger);
      current_min -= challenger;
      const int sign = sgn(current_min);
      if (sign > 0
          || (sign == 0 && y.base_index < x.base_index))
        x = y;
    }
    return x;
  }
};

} // namespace Parma_Polyhedra_Library

namespace boost {
namespace mpi {

template <>
struct is_commutative<Parma_Polyhedra_Library::exiting_index_reducer_functor,
                      exiting_index_candidate>
  : public mpl::true_ { };

}
}


namespace Parma_Polyhedra_Library {

// Note that this is not in the Distributed_Sparse_Matrix class, because
// it would have required to declare exiting_index_candidate in the class.
// exiting_index_candidate is used by exiting_index_reducer_functor, so it
// should either have been public or made visible using a friend declaration,
// and both solutions are not good.
void exiting_index__common(exiting_index_candidate& current,
                           const std::vector<Sparse_Row>& local_rows,
                           const std::vector<dimension_type>& base,
                           dimension_type entering_index) {

  const dimension_type unused_index = -(dimension_type)1;

  // The variable exiting the base should be associated to a tableau
  // constraint such that the ratio
  // tableau[i][entering_var_index] / tableau[i][base[i]]
  // is strictly positive and minimal.

  PPL_DIRTY_TEMP_COEFFICIENT(lcm);
  PPL_DIRTY_TEMP_COEFFICIENT(current_min);
  PPL_DIRTY_TEMP_COEFFICIENT(challenger);
  for (dimension_type i = 0; i < local_rows.size(); ++i) {
    const Sparse_Row& t_i = local_rows[i];
    Coefficient_traits::const_reference t_i_0
      = t_i.get(0);
    Coefficient_traits::const_reference t_i_entering
      = t_i.get(entering_index);
    Coefficient_traits::const_reference t_i_base_i
      = t_i.get(base[i]);
    const int num_sign = sgn(t_i_entering);
    if (num_sign != 0 && num_sign == sgn(t_i_base_i)) {
      if (current.index != unused_index) {
        lcm_assign(lcm, current.row_entering, t_i_entering);
        exact_div_assign(current_min, lcm, current.row_entering);
        current_min *= current.row_0;
        abs_assign(current_min);
        exact_div_assign(challenger, lcm, t_i_entering);
        challenger *= t_i_0;
        abs_assign(challenger);
        current_min -= challenger;
        const int sign = sgn(current_min);
        if (sign > 0
            || (sign == 0 && base[i] < current.base_index)) {
          current.index = i;
          current.base_index = base[i];
          current.row_0 = t_i_0;
          current.row_entering = t_i_entering;
        }
      } else {
        // This is the first candidate, no comparisons are needed.
        current.index = i;
        current.base_index = base[i];
        current.row_entering = t_i_entering;
        current.row_0 = t_i_0;
      }
    }
  }
}

} // namespace Parma_Polyhedra_Library

void
PPL::Distributed_Sparse_Matrix
::remove_row__common(const mpi::communicator& comm, int my_rank,
                     int rank_i, dimension_type local_index_i,
                     int rank_last, std::vector<Sparse_Row>& rows,
                     std::vector<dimension_type>& base,
                     std::vector<dimension_type>& reverse_mapping) {
  if (rank_i == my_rank) {
    if (rank_last == my_rank) {
      // This node stores both of the involved rows.
      std::swap(rows[local_index_i], rows.back());
      std::swap(base[local_index_i], base.back());
      rows.pop_back();
      base.pop_back();
      reverse_mapping.pop_back();
    } else {
      // This node stores the row that has to be removed, but not the last.
      comm.recv(rank_last, 0, rows[local_index_i]);
      comm.recv(rank_last, 0, base[local_index_i]);
    }
  } else {
    if (rank_last == my_rank) {
      // This node stores the last row, but not the one that must be removed.
      comm.send(rank_i, 0, rows.back());
      comm.send(rank_i, 0, base.back());
      rows.pop_back();
      base.pop_back();
      reverse_mapping.pop_back();
    } else {
      // Nothing to do, this node doesn't store the involved rows.
    }
  }
}

bool
PPL::Distributed_Sparse_Matrix
::base_variables_occur_once__common(const std::vector<Sparse_Row>& rows,
                                    const std::vector<dimension_type>&
                                        reverse_mapping,
                                    const std::vector<dimension_type>& base) {
  // Needed to sort accesses to tableau_j, improving performance.
  // This is calculated separately by each node, to save bandwidth.
  typedef std::vector<std::pair<dimension_type, dimension_type> >
    pair_vector_t;
  pair_vector_t vars_in_base;
  for (dimension_type i = base.size(); i-- > 0; )
    vars_in_base.push_back(std::make_pair(base[i], i));

  std::sort(vars_in_base.begin(), vars_in_base.end());

  for (dimension_type j = rows.size(); j-- > 0; ) {
    const Sparse_Row& tableau_j = rows[j];
    pair_vector_t::iterator i = vars_in_base.begin();
    pair_vector_t::iterator i_end = vars_in_base.end();
    Sparse_Row::const_iterator itr = tableau_j.begin();
    Sparse_Row::const_iterator itr_end = tableau_j.end();
    for ( ; i != i_end && itr != itr_end; ++i) {
      // tableau[i][base[j]], with i different from j, must be zero.
      if (itr.index() < i->first)
        itr = tableau_j.lower_bound(itr, itr.index());
      if (i->second != reverse_mapping[j]
          && itr.index() == i->first
          && *itr != 0) {
        return false;
      }
    }
  }

  return true;
}

void
PPL::Distributed_Sparse_Matrix::init(dimension_type num_rows1,
                                     dimension_type num_cols1) {

  my_num_columns = num_cols1,
  mapping.resize(num_rows1);
  reverse_mapping.clear();
  reverse_mapping.resize(comm_size);

  id = get_unique_id();

  if (comm_size == 1) {
    local_rows.resize(num_rows1);
    base.resize(num_rows1, 0);
    for (dimension_type i = 0; i < num_rows1; ++i) {
      local_rows[i].resize(num_columns());
      mapping[i].first = 0;
      mapping[i].second = i;
      reverse_mapping[0].push_back(i);
    }
    next_rank = 0;
  } else {
    broadcast_operation(CREATE_MATRIX_OPERATION, id, num_columns());

    dimension_type n = num_rows1/comm_size;
    int k = num_rows1 % comm_size;

    // This will be scattered among nodes.
    // vec[rank] is a pair <num_rows, first_global_index>
    std::vector<std::pair<dimension_type, dimension_type> > vec(comm_size);

    // The first k nodes will store n+1 rows, the others will store k rows.
    // Note that n may be zero, but we need to send those zeroes so the nodes
    // will all return to the main listening loop.

    {
      dimension_type current_global_index = 0;

      for (int rank = 0; rank < k; ++rank) {
        vec[rank].first = n + 1;
        vec[rank].second = current_global_index;
        current_global_index += n + 1;
      }

      for (int rank = k; rank < comm_size; ++rank) {
        vec[rank].first = n;
        vec[rank].second = current_global_index;
        current_global_index += n;
      }
      PPL_ASSERT(current_global_index == num_rows1);
    }

    std::pair<dimension_type, dimension_type> x;
    mpi::scatter(comm(), vec, x, 0);
    dimension_type root_n = x.first;
    // x.second is not used, because the root already stores the full
    // reverse_mapping, so it does not need to store
    // reverse_mapping[0].

    dimension_type current_row = 0;
    // These loops are splitted from the above loops, so all the worker nodes
    // can do work in parallel with the execution of these loops.
    for (int rank = 0; rank < k; ++rank) {
      reverse_mapping[rank].resize(n + 1);
      for (dimension_type j = 0; j < n; ++j) {
        mapping[current_row].first = rank;
        mapping[current_row].second = j;
        reverse_mapping[rank][j] = current_row;
        current_row++;
      }
      mapping[current_row].first = rank;
      mapping[current_row].second = n;
      reverse_mapping[rank][n] = current_row;
      current_row++;
    }

    for (int rank = k; rank < comm_size; ++rank) {
      reverse_mapping[rank].resize(n);
      for (dimension_type j = 0; j < n; ++j) {
        mapping[current_row].first = rank;
        mapping[current_row].second = j;
        reverse_mapping[rank][j] = current_row;
        current_row++;
      }
    }

    PPL_ASSERT(k < comm_size);
    next_rank = k + 1;
    if (next_rank == comm_size)
      next_rank = 0;

    PPL_ASSERT(current_row == num_rows1);

    local_rows.resize(root_n);
    base.resize(root_n, 0);
    for (std::vector<Sparse_Row>::iterator
        i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
      i->resize(num_columns());
  }
  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix::reset_column(dimension_type column_index) {
  broadcast_operation(RESET_COLUMN_OPERATION, id, column_index);

  for (std::vector<Sparse_Row>::iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    i->reset(column_index);

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix::remove_column(dimension_type column_index) {
  broadcast_operation(REMOVE_COLUMN_OPERATION, id, column_index);

  for (std::vector<Sparse_Row>::iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    i->delete_element_and_shift(column_index);

  --my_num_columns;

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::remove_trailing_columns(dimension_type n) {
  PPL_ASSERT(my_num_columns >= n);
  my_num_columns -= n;
  broadcast_operation(REMOVE_TRAILING_COLUMNS_OPERATION, id, my_num_columns);

  for (std::vector<Sparse_Row>::iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    i->resize(my_num_columns);

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::remove_trailing_rows(dimension_type n) {
  PPL_ASSERT(num_rows() >= n);
  dimension_type row_n = num_rows() - n;
  broadcast_operation(REMOVE_TRAILING_ROWS_OPERATION, id, row_n);

  local_rows.resize(row_n);
  base.resize(row_n);

  mapping.resize(row_n);
  for (int rank = 0; rank < comm_size; ++rank)
    while (!reverse_mapping[rank].empty()
           && reverse_mapping[rank].back() >= row_n)
      reverse_mapping[rank].pop_back();

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix::add_zero_columns(dimension_type n) {
  broadcast_operation(ADD_ZERO_COLUMNS_OPERATION, id, n);

  my_num_columns += n;

  for (std::vector<Sparse_Row>::iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    i->resize(my_num_columns);

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::add_zero_rows(dimension_type n, Row_Flags flags) {
  broadcast_operation(ADD_ZERO_ROWS_OPERATION, id, n, num_columns(),
                      flags.get_bits(), num_rows());

  dimension_type k = n / comm_size;
  int remainder = n % comm_size;

  dimension_type root_n = k;
  if (remainder != 0)
    ++root_n;

  next_rank += remainder;
  if (next_rank >= comm_size)
    next_rank -= comm_size;
  PPL_ASSERT(next_rank >= 0 && next_rank < comm_size);

#ifndef NDEBUG
  const dimension_type old_row_n = num_rows();
#endif

  dimension_type row_n = num_rows();

  for (int rank = 0; rank < comm_size; rank++) {
    dimension_type local_row_n = reverse_mapping[rank].size();
    dimension_type local_k = k;
    if (rank < remainder)
      ++local_k;
    for (dimension_type i = 0; i < local_k; i++) {
      reverse_mapping[rank].push_back(row_n);
      ++row_n;
      mapping.push_back(std::make_pair(rank, local_row_n));
      ++local_row_n;
    }
  }

  PPL_ASSERT(row_n == old_row_n + n);

  if (root_n == 0)
    return;

  Sparse_Row row(num_columns(), flags);
  local_rows.resize(local_rows.size() + root_n, row);
  base.resize(base.size() + root_n, 0);

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost(Sparse_Row& working_cost) {
  PPL_ASSERT(working_cost.size() == num_columns());
  broadcast_operation(COMPUTE_WORKING_COST_OPERATION, id);
  mpi::broadcast(comm(), working_cost, 0);

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;
  compute_working_cost__common(x, working_cost, base, local_rows);

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> y;
  mpi::reduce(comm(), x, y, compute_working_cost_reducer_functor(), 0);

  Coefficient_traits::const_reference global_scaling = y.first.first;
  Coefficient_traits::const_reference global_reverse_scaling = y.first.second;
  const Sparse_Row& global_increase = y.second;

#ifndef NDEBUG
  {
    // Check that global_scaling and global_increase are normalized.
    Coefficient normalized_scaling;
    Coefficient normalized_reverse_scaling;
    normalize2(global_scaling, global_reverse_scaling,
               normalized_scaling, normalized_reverse_scaling);
    PPL_ASSERT(normalized_scaling == global_scaling);
    PPL_ASSERT(normalized_reverse_scaling == global_reverse_scaling);
  }
#endif

  // Calculate the global result from global_scaling and global_increase.
  // global_result[i] == global_scaling * working_cost[i]
  //                     + global_reverse_scaling * global_increase[i].
  // The global result is stored in working_cost to improve performance.

  working_cost.linear_combine(global_increase, global_scaling,
                              global_reverse_scaling);

  working_cost.normalize();
}

void
PPL::Distributed_Sparse_Matrix::set_artificial_indexes_for_unfeasible_rows(
    const std::vector<dimension_type>& unfeasible_tableau_rows,
    dimension_type artificial_index) {
  broadcast_operation(SET_ARTIFICIAL_INDEXES_FOR_UNFEASIBLE_ROWS_OPERATION, id);

  // This will be scattered to nodes, so it is indexed by rank.
  // The first element of each pair is the artificial_index for that node, the
  // second element is a vector containing the local indexes of the involved
  // rows.
  std::vector<std::pair<dimension_type, std::vector<dimension_type> > >
    vec(comm_size);

  // 1. Fill vec[i].second, for each i.

  for (std::vector<dimension_type>::const_iterator
      i = unfeasible_tableau_rows.begin(), i_end = unfeasible_tableau_rows.end();
      i != i_end;
      ++i) {
    int rank = mapping[*i].first;
    dimension_type local_index = mapping[*i].second;
    vec[rank].second.push_back(local_index);
  }

  // 2. Fill vec[i].first, for each i.

  for (std::vector<std::pair<dimension_type, std::vector<dimension_type> > >::iterator
      i = vec.begin(), i_end = vec.end(); i != i_end; ++i) {
    i->first = artificial_index;
    artificial_index += i->second.size();
  }

  std::pair<dimension_type, std::vector<dimension_type> > root_data;
  mpi::scatter(comm(), vec, root_data, 0);

  set_artificial_indexes_for_unfeasible_rows__common(root_data, local_rows,
                                                     base);
}

void
PPL::Distributed_Sparse_Matrix::ascii_dump(std::ostream& stream) const {
  broadcast_operation(ASCII_DUMP_OPERATION, id);

  std::vector<std::string> root_output;
  root_output.reserve(local_rows.size());
  for (std::vector<Sparse_Row>::const_iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i) {
    std::ostringstream strstream;
    i->ascii_dump(strstream);
    // TODO: Avoid the string copy.
    root_output.push_back(strstream.str());
  }

  // vec[rank][local_index] will store the ASCII representation of the row
  // with index `local_index', stored at the node with rank `rank'.
  std::vector<std::vector<std::string> > output(comm_size);

  mpi::gather(comm(), root_output, output, 0);

  stream << num_rows() << " x ";
  stream << num_columns() << "\n";
  for (dimension_type i = 0; i < num_rows(); ++i) {
    int rank = mapping[i].first;
    dimension_type local_index = mapping[i].second;
    stream << output[rank][local_index];
  }
}

void
PPL::Distributed_Sparse_Matrix
::get_column(dimension_type column_index,
             std::vector<Coefficient>& results) const {

  PPL_ASSERT(column_index < num_columns());

  broadcast_operation(GET_COLUMN_OPERATION, id, column_index);

  results.resize(num_rows());

  std::vector<mpi::request> requests;
  requests.reserve(num_rows() - local_rows.size());

  // NOTE: this loop skips rank 0.
  for (int rank = 1; rank < comm_size; ++rank) {
    const std::vector<dimension_type>& node_reverse_mapping
      = reverse_mapping[rank];
    for (dimension_type i = node_reverse_mapping.size(); i-- > 0; ) {
      // TODO: This cast can be dangerous!
      int tag = static_cast<int>(i);
      requests.push_back(comm().irecv(rank, tag,
                                      results[node_reverse_mapping[i]]));
    }
  }

  // The root node does its copies while the other nodes send the data.
  const std::vector<dimension_type>& root_reverse_mapping
    = reverse_mapping[0];
  for (dimension_type i = root_reverse_mapping.size(); i-- > 0; )
    results[root_reverse_mapping[i]] = local_rows[i].get(column_index);

  mpi::wait_all(requests.begin(), requests.end());
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix::external_memory_in_bytes() const {
  dimension_type result = 0;
  for (std::vector<Sparse_Row>::const_iterator
       i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    result += i->external_memory_in_bytes();
  result += local_rows.capacity() * sizeof(local_rows[0]);
  result += base.capacity() * sizeof(base[0]);
  result += mapping.capacity() * sizeof(mapping[0]);
  result += reverse_mapping.capacity() * sizeof(reverse_mapping[0]);
  for (std::vector<std::vector<dimension_type> >::const_iterator
       i = reverse_mapping.begin(), i_end = reverse_mapping.end();
       i != i_end; ++i)
    result += i->capacity() * sizeof((*i)[0]);
  return result;
}

void
PPL::Distributed_Sparse_Matrix
::get_scattered_row(const std::vector<dimension_type>& indexes,
                    std::vector<Coefficient>& row) const {
  broadcast_operation(GET_SCATTERED_ROW_OPERATION, id);

  // This will be scattered among nodes.
  // workunits[rank] is a vector of <local_row_index, column_index> pairs.
  // column_index is stored as an int, because it will be used as a tag.
  std::vector<std::vector<std::pair<dimension_type, int> > >
    workunits(comm_size);

  for (dimension_type i = 0; i < num_columns(); ++i) {
    dimension_type global_row_index = indexes[i];
    int rank = mapping[global_row_index].first;
    dimension_type local_row_index = mapping[global_row_index].second;
    // TODO: This cast can be dangerous!
    int column_index = static_cast<int>(i);
    workunits[rank].push_back(std::make_pair(local_row_index, column_index));
  }

  std::vector<std::pair<dimension_type, int> > workunit;
  mpi::scatter(comm(), workunits, workunit, 0);

  std::vector<mpi::request> requests;
  requests.reserve(num_columns() - workunit.size());

  row.resize(num_columns());

  // NOTE: This skips rank 0.
  for (int rank = 1; rank < comm_size; ++rank) {
    for (std::vector<std::pair<dimension_type, int> >::const_iterator
         i = workunits[rank].begin(), i_end = workunits[rank].end();
         i != i_end; ++i)
      requests.push_back(comm().irecv(rank, i->second, row[i->second]));
  }

  // The root node does its copies, while other nodes are sending data.
  for (std::vector<std::pair<dimension_type, int> >::const_iterator
        i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i)
    row[i->second] = local_rows[i->first].get(i->second);

  mpi::wait_all(requests.begin(), requests.end());
}

namespace {

class float_entering_index_reducer_functor {
public:
  const std::vector<double>& operator()(std::vector<double>& x,
                                        const std::vector<double>& y) {
    for (PPL::dimension_type i = x.size(); i-- > 0; )
      x[i] += y[i];
    return x;
  }
};

} // namespace

namespace boost {
namespace mpi {

template <>
struct is_commutative<float_entering_index_reducer_functor,
                      std::vector<double> >
  : public mpl::true_ { };

}
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix
::float_entering_index(const Sparse_Row& working_cost) const {
  broadcast_operation(FLOAT_ENTERING_INDEX_OPERATION, id);

  std::vector<dimension_type> candidates;
  std::vector<double> results;

  float_entering_index__common(candidates, base, local_rows, results, 0,
                               working_cost);

  std::vector<double> global_results;
  mpi::reduce(comm(), results, global_results,
              float_entering_index_reducer_functor(), 0);

  double current_value = 0.0;
  dimension_type entering_index = 0;

  for (dimension_type i = candidates.size(); i-- > 0; ) {
    double challenger_value = sqrt(global_results[i]);
    if (entering_index == 0 || challenger_value > current_value) {
      current_value = challenger_value;
      entering_index = candidates[i];
    }
  }

  return entering_index;
}

void
PPL::Distributed_Sparse_Matrix
::set_artificial_indexes_for_new_rows(dimension_type old_num_rows,
                                      const std::deque<bool>& worked_out_row,
                                      dimension_type artificial_index) {
  broadcast_operation(SET_ARTIFICIAL_INDEXES_FOR_NEW_ROWS_OPERATION, id);

  std::vector<std::vector<std::pair<dimension_type, dimension_type> > >
    workunits(comm_size);

  for (dimension_type i = old_num_rows; i < num_rows(); ++i) {
    if (worked_out_row[i])
      continue;
    int rank = mapping[i].first;
    dimension_type local_row_index = mapping[i].second;
    workunits[rank].push_back(std::make_pair(local_row_index,
                                             artificial_index));
    ++artificial_index;
  }

  std::vector<std::pair<dimension_type, dimension_type> > workunit;
  mpi::scatter(comm(), workunits, workunit, 0);

  for (std::vector<std::pair<dimension_type, dimension_type> >::const_iterator
       i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i) {
    local_rows[i->first].insert(i->second, Coefficient_one());
    base[i->first] = i->second;
  }
}

void
PPL::Distributed_Sparse_Matrix
::set_base(const std::vector<dimension_type>& base1) {
  broadcast_operation(SET_BASE_OPERATION, id);

  PPL_ASSERT(base1.size() == num_rows());

  std::vector<std::vector<dimension_type> > vec(comm_size);

  for (int i = 0; i < comm_size; ++i)
    vec[i].resize(reverse_mapping[i].size());

  for (dimension_type i = num_rows(); i-- > 0; ) {
    int rank = mapping[i].first;
    dimension_type local_row_index = mapping[i].second;
    vec[rank][local_row_index] = base1[i];
  }

  std::vector<dimension_type> new_base;
  mpi::scatter(comm(), vec, new_base, 0);

  base = new_base;
}

void
PPL::Distributed_Sparse_Matrix
::get_base(std::vector<dimension_type>& base1) const {
  broadcast_operation(GET_BASE_OPERATION, id);

  std::vector<std::vector<dimension_type> > vec(comm_size);
  mpi::gather(comm(), base, vec, 0);

  base1.resize(num_rows());

  for (dimension_type i = num_rows(); i-- > 0; ) {
    int rank = mapping[i].first;
    dimension_type local_row_index = mapping[i].second;
    base1[i] = vec[rank][local_row_index];
  }
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix
::exiting_index(dimension_type entering_index) const {
  broadcast_operation(EXITING_INDEX_OPERATION, id, entering_index);

  const dimension_type unused_index = -(dimension_type)1;

  exiting_index_candidate current;
  current.index = unused_index;
  current.rank = 0;

  exiting_index__common(current, local_rows, base, entering_index);

  exiting_index_candidate winner;
  mpi::reduce(comm(), current, winner, exiting_index_reducer_functor(), 0);

  if (winner.index == unused_index)
    return num_rows();

  return reverse_mapping[winner.rank][winner.index];
}

namespace {

class exact_entering_index_reducer_functor2 {
public:
  const std::vector<PPL::Coefficient>&
  operator()(std::vector<PPL::Coefficient>& x,
             const std::vector<PPL::Coefficient>& y) const {
    PPL_ASSERT(x.size() == y.size());
    for (PPL::dimension_type i = x.size(); i-- > 0; )
      x[i] += y[i];
    return x;
  }
};

} // namespace

namespace boost {
namespace mpi {

template <>
struct is_commutative<exact_entering_index_reducer_functor2,
                      std::vector<PPL::Coefficient> >
  : public mpl::true_ { };

}
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix
::exact_entering_index(const Sparse_Row& working_cost) const {
  broadcast_operation(EXACT_ENTERING_INDEX_OPERATION, id);

  // Contains the list of the column candidates.
  std::vector<dimension_type> columns;

  // For each i, challenger_values[i] contains the challenger value for the
  // columns[i] column.
  std::vector<Coefficient> challenger_values;
  PPL_DIRTY_TEMP_COEFFICIENT(squared_lcm_basis);
  exact_entering_index__common(columns, challenger_values, local_rows, base,
                               squared_lcm_basis, working_cost);

  std::vector<Coefficient> global_challenger_values;
  mpi::reduce(comm(), challenger_values, global_challenger_values,
              exact_entering_index_reducer_functor2(), 0);

  PPL_DIRTY_TEMP_COEFFICIENT(challenger_num);
  PPL_DIRTY_TEMP_COEFFICIENT(current_num);
  PPL_DIRTY_TEMP_COEFFICIENT(current_den);
  PPL_DIRTY_TEMP_COEFFICIENT(challenger_value);
  PPL_DIRTY_TEMP_COEFFICIENT(current_value);
  dimension_type entering_index = 0;

  Sparse_Row::const_iterator itr = working_cost.end();
  for (dimension_type k = columns.size(); k-- > 0; ) {
    global_challenger_values[k] += squared_lcm_basis;
    itr = working_cost.lower_bound(itr, columns[k]);
    if (itr == working_cost.end() || itr.index() != columns[k]) {
      PPL_ASSERT(working_cost.get(columns[k]) == 0);
      // Initialization during the first loop.
      if (entering_index == 0) {
        current_num = 0;
        std::swap(current_den, global_challenger_values[k]);
        entering_index = columns[k];
        continue;
      }
      current_value = current_num * global_challenger_values[k];
      // Update the values, if the challenger wins.
      if (0 > sgn(current_num) * sgn(global_challenger_values[k])) {
        current_num = 0;
        std::swap(current_den, global_challenger_values[k]);
        entering_index = columns[k];
      }
    } else {
      Coefficient_traits::const_reference cost_j = *itr;
      // We cannot compute the (exact) square root of abs(\Delta x_j).
      // The workaround is to compute the square of `cost[j]'.
      challenger_num = cost_j * cost_j;
      // Initialization during the first loop.
      if (entering_index == 0) {
        std::swap(current_num, challenger_num);
        std::swap(current_den, global_challenger_values[k]);
        entering_index = columns[k];
        continue;
      }
      PPL_ASSERT(challenger_num * current_den >= 0);
      challenger_value = challenger_num * current_den;
      current_value = current_num * global_challenger_values[k];
      // Update the values, if the challenger wins.
      if (challenger_value > current_value) {
        std::swap(current_num, challenger_num);
        std::swap(current_den, global_challenger_values[k]);
        entering_index = columns[k];
      }
    }
  }

  return entering_index;
}

bool
PPL::Distributed_Sparse_Matrix
::get_exiting_and_pivot(dimension_type entering_index,
                        dimension_type& exiting_var_index,
                        Sparse_Row& working_cost) {

  broadcast_operation(GET_EXITING_AND_PIVOT, id, entering_index);

  // 1. exiting_index(entering_index):

  const dimension_type unused_index = -(dimension_type)1;

  exiting_index_candidate current;
  current.index = unused_index;
  current.rank = 0;

  exiting_index__common(current, local_rows, base, entering_index);

  exiting_index_candidate winner;

  mpi::all_reduce(comm(), current, winner, exiting_index_reducer_functor());

  if (winner.index == unused_index)
    return false;

  // 2. linear_combine_matrix(exiting_var_index, entering_index, tableau_out);

  const Sparse_Row& tableau_out
    = linear_combine_matrix__common(winner.rank, winner.index, entering_index,
                                    0, local_rows);

  // 3. set_base_column(exiting_var_index, entering_index);

  if (winner.rank == 0)
    base[winner.index] = entering_index;

  // 4. Linearly combine the cost function.

  if (working_cost.get(entering_index) != 0)
    linear_combine(working_cost, tableau_out, entering_index);

  // 5. Set the correct value for exiting_var_index.

  dimension_type local_index = winner.index;
  int rank = winner.rank;

  exiting_var_index = reverse_mapping[rank][local_index];

  return true;
}

void
PPL::Distributed_Sparse_Matrix
::set_working_cost(const Sparse_Row& working_cost) {
  broadcast_operation(SET_WORKING_COST_OPERATION, id);

  Sparse_Row& working_cost_ref = const_cast<Sparse_Row&>(working_cost);
  mpi::broadcast(comm(), working_cost_ref, 0);
}


PPL::Distributed_Sparse_Matrix::Worker::Worker()
  : my_rank(comm().rank()) {
}

void
PPL::Distributed_Sparse_Matrix::Worker
::create_matrix(dimension_type id, dimension_type num_cols) {
  PPL_ASSERT(row_chunks.find(id) == row_chunks.end());
  std::pair<dimension_type, dimension_type> x;
  mpi::scatter(comm(), x, 0);
  dimension_type num_rows = x.first;
  dimension_type current_global_index = x.second;
  if (num_rows == 0)
    return;
  Row_Chunk& row_chunk = row_chunks[id];
  row_chunk.base.resize(num_rows, 0);
  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(num_rows);
  for (std::vector<Sparse_Row>::iterator i = rows.begin(), i_end = rows.end();
      i != i_end; ++i)
    i->resize(num_cols);
  std::vector<dimension_type>& reverse_mapping
    = row_chunk.reverse_mapping;
  reverse_mapping.resize(num_rows);
  for (std::vector<dimension_type>::iterator
       i = reverse_mapping.begin(), i_end = reverse_mapping.end();
       i != i_end; ++i) {
    *i = current_global_index;
    ++current_global_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::copy_matrix(dimension_type source_id, dimension_type id) {

  PPL_ASSERT(row_chunks.find(id) == row_chunks.end());
  row_chunks[id] = row_chunks[source_id];
}

void
PPL::Distributed_Sparse_Matrix::Worker::delete_matrix(dimension_type id) {
  // Note that this node may not store data for the specified matrix id,
  // so row_chunks may not contain the `id' key.
  row_chunks.erase(id);
}


void
PPL::Distributed_Sparse_Matrix::Worker::get_row(int rank,
                                                dimension_type id,
                                                dimension_type row_index
                                               ) const {
  if (my_rank != rank)
    return;
  PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
  PPL_ASSERT(row_index < row_chunks.find(id)->second.rows.size());
  comm().send(0, 0, row_chunks.find(id)->second.rows[row_index]);
}

void
PPL::Distributed_Sparse_Matrix::Worker::set_row(int rank, dimension_type id,
                                                dimension_type row_index) {
  if (my_rank != rank)
    return;
  PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
  PPL_ASSERT(row_index < row_chunks[id].rows.size());
  comm().recv(0, 0, row_chunks[id].rows[row_index]);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_matrix(int rank, dimension_type id,
                        dimension_type local_row_index,
                        dimension_type col_index) {
  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  const Sparse_Row& tableau_out
    = linear_combine_matrix__common(rank, local_row_index, col_index, my_rank,
                                    row_chunk.rows);
  (void)tableau_out;

  if (rank == my_rank)
    comm().send(0, 0, row_chunk.rows[local_row_index]);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::reset_column(dimension_type id, dimension_type col_index) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  for (std::vector<Sparse_Row>::iterator
      i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    i->reset(col_index);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_column(dimension_type id, dimension_type col_index) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  for (std::vector<Sparse_Row>::iterator
      i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    i->delete_element_and_shift(col_index);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_columns(dimension_type id, dimension_type col_index) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  for (std::vector<Sparse_Row>::iterator
      i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    i->resize(col_index);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_zero_columns(dimension_type id, dimension_type n) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  for (std::vector<Sparse_Row>::iterator
      i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    i->resize(i->size() + n);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::check(dimension_type id, dimension_type num_columns) const {
  std::vector<dimension_type> correct_reverse_mapping;
  mpi::scatter(comm(), correct_reverse_mapping, 0);

  bool result = true;

  const Row_Chunk& row_chunk = get_row_chunk(id);

  if (correct_reverse_mapping.size() != row_chunk.rows.size()) {
    std::cerr << "Worker node: row check failed" << std::endl;
    result = false;
  }
  if (row_chunk.base.size() != row_chunk.rows.size()) {
    std::cerr << "Worker node: base[] has the wrong size." << std::endl;
    result = false;
  }
  if (correct_reverse_mapping != row_chunk.reverse_mapping) {
    std::cerr << "Worker node: reverse_mapping check failed" << std::endl;
    std::cerr << "Correct reverse_mapping:" << std::endl;
    for (std::vector<dimension_type>::const_iterator
          i = correct_reverse_mapping.begin(),
          i_end = correct_reverse_mapping.end(); i != i_end; ++i)
      std::cerr << *i << ' ';
    std::cerr << std::endl;
    std::cerr << "Local reverse_mapping:" << std::endl;
    for (std::vector<dimension_type>::const_iterator
         i = row_chunk.reverse_mapping.begin(),
         i_end = row_chunk.reverse_mapping.end(); i != i_end; ++i)
      std::cerr << *i << ' ';
    std::cerr << std::endl;
    result = false;
  }

  for (std::vector<Sparse_Row>::const_iterator
      i = row_chunk.rows.begin(), i_end = row_chunk.rows.end();
      i != i_end; ++i)
    if (i->size() != num_columns) {
      std::cerr << "Worker node: column check failed" << std::endl;
      result = false;
    }

  mpi::reduce(comm(), result, std::logical_and<bool>(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_zero_rows(dimension_type id, dimension_type n,
                dimension_type num_columns, dimension_type flag_bits,
                dimension_type old_num_rows) {
  dimension_type my_n = n / comm_size;
  // The first (n % comm_size) nodes will store one more row.
  dimension_type current_global_index;
  int remainder = n % comm_size;
  if (my_rank < remainder) {
    current_global_index = old_num_rows + (my_n + 1) * my_rank;
    ++my_n;
  } else
    current_global_index = old_num_rows + remainder*(my_n + 1)
                           + (my_rank - remainder)*my_n;
  if (my_n == 0)
    return;
  Row_Flags flags(flag_bits);
  Sparse_Row row(num_columns, flags);
  // This may default-construct the Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(rows.size() + my_n, row);

  std::vector<dimension_type>& base = row_chunk.base;
  base.resize(base.size() + my_n, 0);

  std::vector<dimension_type>& reverse_mapping
    = row_chunk.reverse_mapping;
  for (dimension_type i = 0; i < my_n; ++i) {
    reverse_mapping.push_back(current_global_index);
    ++current_global_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker::add_row(int rank,
                                                dimension_type id,
                                                dimension_type global_index) {
  if (rank != my_rank)
    return;
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(rows.size() + 1);

  std::vector<dimension_type>& base = row_chunk.base;
  base.push_back(0);

  row_chunk.reverse_mapping.push_back(global_index);
  Sparse_Row& row = rows.back();
  comm().recv(0, 0, row);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_rows(dimension_type id, dimension_type row_n) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  std::vector<dimension_type>& base = row_chunk.base;
  std::vector<dimension_type>& reverse_mapping = row_chunk.reverse_mapping;

  while (!reverse_mapping.empty() && reverse_mapping.back() >= row_n) {
    reverse_mapping.pop_back();
    rows.pop_back();
    base.pop_back();
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::swap_rows(dimension_type id,
            int rank1, dimension_type local_index1,
            int rank2, dimension_type local_index2) {
  if (rank1 != my_rank && rank2 != my_rank)
    return;

  PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
  Row_Chunk& row_chunk = row_chunks[id];

  swap_rows__common(rank1, rank2, local_index1, local_index2, my_rank,
                    row_chunk.rows, row_chunk.base);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::fill_matrix(dimension_type id) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;

  std::vector<mpi::request> requests;
  requests.reserve(rows.size());
  for (dimension_type i = 0; i < rows.size(); i++) {
    // FIXME: This cast can be dangerous!
    int tag = static_cast<int>(i);
    requests.push_back(comm().irecv(0, tag, rows[i]));
  }
  mpi::wait_all(requests.begin(), requests.end());
}

void
PPL::Distributed_Sparse_Matrix::Worker
::compare_with_sparse_matrix(dimension_type id) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  std::vector<Sparse_Row> received_rows(rows.size());

  std::vector<mpi::request> requests;
  requests.reserve(rows.size());
  for (dimension_type i = 0; i < rows.size(); i++) {
    // FIXME: This cast can be dangerous!
    int tag = static_cast<int>(i);
    requests.push_back(comm().irecv(0, tag, received_rows[i]));
  }
  mpi::wait_all(requests.begin(), requests.end());

  bool result = true;
  for (dimension_type i = 0; i < rows.size(); i++)
    if (rows[i] != received_rows[i]) {
      result = false;
      break;
    }

  mpi::reduce(comm(), result, std::logical_and<bool>(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::compute_working_cost(dimension_type id) {

  Sparse_Row working_cost(0, Row_Flags());
  mpi::broadcast(comm(), working_cost, 0);

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;

  compute_working_cost__common(x, working_cost, row_chunk.base,
                               row_chunk.rows);

  mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::make_inhomogeneous_terms_nonpositive(dimension_type id) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  make_inhomogeneous_terms_nonpositive__common(row_chunk.rows);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_artificial_indexes_for_unfeasible_rows(dimension_type id) {
  std::pair<dimension_type, std::vector<dimension_type> > node_data;
  mpi::scatter(comm(), node_data, 0);

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  set_artificial_indexes_for_unfeasible_rows__common(node_data,
                                                     row_chunk.rows,
                                                     row_chunk.base);
}

void
PPL::Distributed_Sparse_Matrix::Worker::ascii_dump(dimension_type id) const {

  const Row_Chunk& row_chunk = get_row_chunk(id);

  std::vector<std::string> output;
  output.reserve(row_chunk.rows.size());

  for (std::vector<Sparse_Row>::const_iterator
      i = row_chunk.rows.begin(), i_end = row_chunk.rows.end();
      i != i_end; ++i) {
    std::ostringstream strstream;
    i->ascii_dump(strstream);
    // TODO: Avoid the string copy.
    output.push_back(strstream.str());
  }

  mpi::gather(comm(), output, 0);
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Distributed_Sparse_Matrix)

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_with_base_rows(dimension_type id, int k_rank,
                                dimension_type k_local_index) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  linear_combine_with_base_rows__common(k_rank, k_local_index, my_rank,
                                        row_chunk.rows, row_chunk.base);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::get_column(dimension_type id, dimension_type column_index) const {

  const Row_Chunk& row_chunk = get_row_chunk(id);

  const std::vector<Sparse_Row>& rows = row_chunk.rows;

  std::vector<mpi::request> requests;
  requests.reserve(rows.size());

  for (dimension_type i = rows.size(); i-- > 0; ) {
    // TODO: This cast can be dangerous!
    int tag = static_cast<int>(i);
    requests.push_back(comm().isend(0, tag, rows[i].get(column_index)));
  }

  mpi::wait_all(requests.begin(), requests.end());
}

bool
PPL::Distributed_Sparse_Matrix::ascii_load(std::istream& stream) {
  Sparse_Matrix matrix;
  if (!matrix.ascii_load(stream))
    return false;
  // Only a binary representation of the matrix will be sent to nodes, instead
  // of the bloated human-readable ASCII representation.
  *this = matrix;

  return true;
}

void
PPL::Distributed_Sparse_Matrix::Worker
::get_scattered_row(dimension_type id) const {

  // workunit is a vector of <local_row_index, column_index> pairs.
  // column_index is stored as an int, because it is used as a tag.
  std::vector<std::pair<dimension_type, int> > workunit;
  mpi::scatter(comm(), workunit, 0);

  const Row_Chunk& row_chunk = get_row_chunk(id);

  const std::vector<Sparse_Row>& rows = row_chunk.rows;

  std::vector<mpi::request> requests;
  requests.reserve(workunit.size());

  for (std::vector<std::pair<dimension_type, int> >::const_iterator
       i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i)
    requests.push_back(comm().isend(0, i->second,
                                    rows[i->first].get(i->second)));

  mpi::wait_all(requests.begin(), requests.end());
}

void
PPL::Distributed_Sparse_Matrix::Worker
::float_entering_index(dimension_type id) const {

  const Row_Chunk& row_chunk = get_row_chunk(id);
  const Sparse_Row& working_cost = row_chunk.working_cost;

  std::vector<dimension_type> candidates;
  std::vector<double> results;

  float_entering_index__common(candidates, row_chunk.base, row_chunk.rows,
                               results, my_rank, working_cost);

  mpi::reduce(comm(), results, float_entering_index_reducer_functor(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_artificial_indexes_for_new_rows(dimension_type id) {

  std::vector<std::pair<dimension_type, dimension_type> > workunit;
  mpi::scatter(comm(), workunit, 0);

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  std::vector<Sparse_Row>& rows = row_chunk.rows;
  std::vector<dimension_type>& base = row_chunk.base;

  for (std::vector<std::pair<dimension_type, dimension_type> >::const_iterator
       i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i) {
    rows[i->first].insert(i->second, Coefficient_one());
    base[i->first] = i->second;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_base_column(dimension_type id, int rank,
                  dimension_type local_row_index,
                  dimension_type column_index) {
  if (my_rank == rank) {
    PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
    row_chunks[id].base[local_row_index] = column_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker::set_base(dimension_type id) {

  std::vector<dimension_type> new_base;
  mpi::scatter(comm(), new_base, 0);

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  PPL_ASSERT(new_base.size() == row_chunk.base.size());

  row_chunk.base = new_base;
}

void
PPL::Distributed_Sparse_Matrix::Worker::get_base(dimension_type id) const {

  const Row_Chunk& row_chunk = get_row_chunk(id);

  mpi::gather(comm(), row_chunk.base, 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::exact_entering_index(dimension_type id) const {
  // Contains the list of the (column) indexes of challengers.
  std::vector<dimension_type> columns;

  // For each i, challenger_values[i] contains the challenger value for the
  // columns[i] column.
  std::vector<Coefficient> challenger_values;

  PPL_DIRTY_TEMP_COEFFICIENT(squared_lcm_basis);

  const Row_Chunk& row_chunk = get_row_chunk(id);

  exact_entering_index__common(columns, challenger_values, row_chunk.rows,
                               row_chunk.base, squared_lcm_basis,
                               row_chunk.working_cost);

  mpi::reduce(comm(), challenger_values,
              exact_entering_index_reducer_functor2(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::exiting_index(dimension_type id, dimension_type entering_index) const {

  const dimension_type unused_index = -(dimension_type)1;

  exiting_index_candidate current;
  current.index = unused_index;
  current.rank = my_rank;

  const Row_Chunk& row_chunk = get_row_chunk(id);

  exiting_index__common(current, row_chunk.rows, row_chunk.base,
                        entering_index);

  mpi::reduce(comm(), current, exiting_index_reducer_functor(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_row(dimension_type id, int rank_i, dimension_type local_index_i,
             int rank_last) {

  Row_Chunk row_chunk = row_chunks[id];

  remove_row__common(comm(), my_rank, rank_i, local_index_i, rank_last,
                     row_chunk.rows, row_chunk.base,
                     row_chunk.reverse_mapping);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::base_variables_occur_once(dimension_type id) const {

  std::vector<dimension_type> base;
  mpi::broadcast(comm(), base, 0);

  const Row_Chunk& row_chunk = get_row_chunk(id);

  bool local_result
    = base_variables_occur_once__common(row_chunk.rows,
                                        row_chunk.reverse_mapping, base);

  mpi::reduce(comm(), local_result, std::logical_and<bool>(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::get_exiting_and_pivot(dimension_type id, dimension_type entering_index) {

  // This may create a new Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];

  // 1. exiting_index(entering_index):

  const dimension_type unused_index = -(dimension_type)1;

  exiting_index_candidate current;
  current.index = unused_index;
  current.rank = my_rank;

  exiting_index__common(current, row_chunk.rows, row_chunk.base,
                        entering_index);

  exiting_index_candidate winner;

  mpi::all_reduce(comm(), current, winner, exiting_index_reducer_functor());

  if (winner.index == unused_index)
    return;

  // 2. linear_combine_matrix(exiting_var_index, entering_index, tableau_out);

  const Sparse_Row& tableau_out
    = linear_combine_matrix__common(winner.rank, winner.index, entering_index,
                                    my_rank, row_chunk.rows);

  // 3. set_base_column(exiting_var_index, entering_index);

  if (winner.rank == my_rank)
    row_chunk.base[winner.index] = entering_index;

  // 4. Linearly combine the cost function.

  if (row_chunk.working_cost.get(entering_index) != 0)
    linear_combine(row_chunk.working_cost, tableau_out, entering_index);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_working_cost(dimension_type id) {

  // This may create a new Row_Chunk for this id.
  Row_Chunk& row_chunk = row_chunks[id];

  mpi::broadcast(comm(), row_chunk.working_cost, 0);
}

template <typename Archive>
void
PPL::Distributed_Sparse_Matrix::Operation
::serialize(Archive& archive, const unsigned int version) {
  (void)version;
  archive & code;
  dimension_type n = num_operation_params[code];
  for (dimension_type i = 0; i < n; i++)
    archive & params[i];
}
