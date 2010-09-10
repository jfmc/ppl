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

#include <boost/mpi/collectives.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/mpi/nonblocking.hpp>
#include <boost/mpi/operations.hpp>
#include <functional>
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
  3, // ADD_ZERO_ROWS_OPERATION: id, num_columns, flag_bits
  1, // ADD_ROW_OPERATION: rank
  2, // RESET_COLUMN_OPERATION: id, column_index
  1, // REMOVE_TRAILING_ROWS_OPERATION: id
  5, // SWAP_ROWS_OPERATION: id, rank1, local_index1, rank2, local_index2
  1, // FILL_MATRIX_OPERATION: id
  1, // COMPARE_WITH_SPARSE_MATRIX_OPERATION: id
  1, // COMPUTE_WORKING_COST_OPERATION: id
  1, // MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION: id
  1, // SET_ARTIFICIAL_INDEXES_FOR_UNFEASIBLE_ROWS_OPERATION: id
  1, // ASCII_DUMP_OPERATION: id
  3, // LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION: id, k_rank, k_local_index
};

const mpi::communicator*
PPL::Distributed_Sparse_Matrix::comm_ptr = NULL;

int PPL::Distributed_Sparse_Matrix::comm_size = -1;

void
PPL::Distributed_Sparse_Matrix
::init_root(const mpi::communicator& comm1) {
  PPL_ASSERT(comm_ptr == NULL);
  PPL_ASSERT(comm1.rank() == 0);
  comm_ptr = &comm1;
  comm_size = comm().size();
}

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

    if (op.code == QUIT_OPERATION)
      break;

    switch (op.code) {

    case CREATE_MATRIX_OPERATION:
      worker.create_matrix(op.params[0], op.params[1]);
      break;

    case COPY_MATRIX_OPERATION:
      worker.copy_matrix(op.params[0], op.params[1]);
      break;

    case DELETE_MATRIX_OPERATION:
      worker.delete_matrix(op.params[0]);
      break;

    case GET_ROW_OPERATION:
      worker.get_row(op.params[0], op.params[1], op.params[2]);
      break;

    case SET_ROW_OPERATION:
      worker.set_row(op.params[0], op.params[1], op.params[2]);
      break;

    case LINEAR_COMBINE_MATRIX_OPERATION:
      worker.linear_combine_matrix(op.params[0], op.params[1], op.params[2],
                                   op.params[3]);
      break;

    case REMOVE_COLUMN_OPERATION:
      worker.remove_column(op.params[0], op.params[1]);
      break;

    case REMOVE_TRAILING_COLUMNS_OPERATION:
      worker.remove_trailing_columns(op.params[0], op.params[1]);
      break;

    case ADD_ZERO_COLUMNS_OPERATION:
      worker.add_zero_columns(op.params[0], op.params[1]);
      break;

    case CHECK_OPERATION:
      worker.check(op.params[0], op.params[1]);
      break;

    case ADD_ZERO_ROWS_OPERATION:
      worker.add_zero_rows(op.params[0], op.params[1], op.params[2]);
      break;

    case ADD_ROW_OPERATION:
      worker.add_row(op.params[0]);
      break;

    case RESET_COLUMN_OPERATION:
      worker.reset_column(op.params[0], op.params[1]);
      break;

    case REMOVE_TRAILING_ROWS_OPERATION:
      worker.remove_trailing_rows(op.params[0]);
      break;

    case SWAP_ROWS_OPERATION:
      worker.swap_rows(op.params[0], op.params[1], op.params[2], op.params[3],
                       op.params[4]);
      break;

    case FILL_MATRIX_OPERATION:
      worker.fill_matrix(op.params[0]);
      break;

    case COMPARE_WITH_SPARSE_MATRIX_OPERATION:
      worker.compare_with_sparse_matrix(op.params[0]);
      break;

    case COMPUTE_WORKING_COST_OPERATION:
      worker.compute_working_cost(op.params[0]);
      break;

    case MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION:
      worker.make_inhomogeneous_terms_nonpositive(op.params[0]);
      break;

    case SET_ARTIFICIAL_INDEXES_FOR_UNFEASIBLE_ROWS_OPERATION:
      worker.set_artificial_indexes_for_unfeasible_rows(op.params[0]);
      break;

    case ASCII_DUMP_OPERATION:
      worker.ascii_dump(op.params[0]);
      break;

    case LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION:
      worker.linear_combine_with_base_rows(op.params[0], op.params[1],
                                           op.params[2]);
      break;

    case QUIT_OPERATION:
      PPL_ASSERT(false);

    default:
      PPL_ASSERT(false);
    }
  }
  comm_ptr = NULL;
}

void
PPL::Distributed_Sparse_Matrix::quit_workers() {
  broadcast_operation(QUIT_OPERATION);
#ifndef NDEBUG
  comm_ptr = NULL;
#endif
}

PPL::Distributed_Sparse_Matrix::Distributed_Sparse_Matrix()
  : my_num_columns(0), id(get_unique_id()), row_mapping(),
    reverse_row_mapping(comm_size), next_rank(0), local_rows(0) {
  PPL_ASSERT(comm().rank() == 0);
  PPL_ASSERT(OK());
}

PPL::Distributed_Sparse_Matrix
::Distributed_Sparse_Matrix(dimension_type num_rows1,
                            dimension_type num_cols1) {
  PPL_ASSERT(comm().rank() == 0);

  init(num_rows1, num_cols1);
}

PPL::Distributed_Sparse_Matrix
::Distributed_Sparse_Matrix(const Distributed_Sparse_Matrix& matrix)
  : my_num_columns(matrix.my_num_columns),
    row_mapping(matrix.row_mapping),
    reverse_row_mapping(matrix.reverse_row_mapping),
    next_rank(matrix.next_rank),
    local_rows(matrix.local_rows) {

  id = get_unique_id();

  broadcast_operation(COPY_MATRIX_OPERATION, matrix.id, id);

  PPL_ASSERT(OK());
}

PPL::Distributed_Sparse_Matrix&
PPL::Distributed_Sparse_Matrix::operator=(const Sparse_Matrix& matrix) {
  broadcast_operation(DELETE_MATRIX_OPERATION, id);

  init(matrix.num_rows(), matrix.num_columns());

  broadcast_operation(FILL_MATRIX_OPERATION, id);

  std::vector<mpi::request> requests;
  requests.reserve(num_rows() - local_rows.size());

  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = row_mapping[i].first;
    dimension_type local_i = row_mapping[i].second;
    if (rank != 0) {
      // FIXME: This cast can be dangerous!
      int tag = static_cast<int>(local_i);
      requests.push_back(comm().isend(rank, tag, matrix[i]));
    }
  }

  // This loop has been splitted from the above so the worker nodes can do
  // work while the root executes its copies.
  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = row_mapping[i].first;
    dimension_type local_i = row_mapping[i].second;
    if (rank == 0)
      local_rows[local_i] = matrix[i];
  }

  mpi::wait_all(requests.begin(), requests.end());

  PPL_ASSERT(*this == matrix);
  PPL_ASSERT(OK());

  return *this;
}

void
PPL::Distributed_Sparse_Matrix::swap(Distributed_Sparse_Matrix& matrix) {
  std::swap(id, matrix.id);
  std::swap(my_num_columns, matrix.my_num_columns);
  std::swap(row_mapping, matrix.row_mapping);
  std::swap(reverse_row_mapping, matrix.reverse_row_mapping);
  std::swap(next_rank, matrix.next_rank);
  std::swap(local_rows, matrix.local_rows);
  PPL_ASSERT(OK());
  PPL_ASSERT(matrix.OK());
}

bool
PPL::Distributed_Sparse_Matrix
::operator==(const Sparse_Matrix& matrix) const {

  if (num_rows() != matrix.num_rows()) {
    std::cout << "Wrong number of rows" << std::endl;
    return false;
  }
  if (num_columns() != matrix.num_columns()) {
    std::cout << "Wrong number of columns" << std::endl;
    return false;
  }

  broadcast_operation(COMPARE_WITH_SPARSE_MATRIX_OPERATION, id);

  std::vector<mpi::request> requests;
  requests.reserve(num_rows() - local_rows.size());

  for (dimension_type i = 0; i < num_rows(); i++) {
    int rank = row_mapping[i].first;
    dimension_type local_i = row_mapping[i].second;
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
    int rank = row_mapping[i].first;
    dimension_type local_i = row_mapping[i].second;
    if (rank == 0)
      if (!(local_rows[local_i] == matrix[i])) {
        std::cout << "Found mismatch in root node" << std::endl;
        local_result = false;
      }
  }

  mpi::wait_all(requests.begin(), requests.end());

  bool result;
  mpi::reduce(comm(), local_result, result, std::logical_and<bool>(), 0);

  return result;
}

bool
PPL::Distributed_Sparse_Matrix
::operator!=(const Sparse_Matrix& matrix) const {
  return !(*this == matrix);
}

PPL::Distributed_Sparse_Matrix
::~Distributed_Sparse_Matrix() {
  PPL_ASSERT(comm_ptr != NULL);
  broadcast_operation(DELETE_MATRIX_OPERATION, id);

  // The rows stored at the root will be destroyed automatically.
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix::num_rows() const {
  return row_mapping.size();
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix::num_columns() const {
  return my_num_columns;
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

  // 1. Check that reverse_row_mapping is the reverse of row_mapping.

  std::vector<std::vector<dimension_type> >
    correct_reverse_row_mapping(comm_size);
  dimension_type num_rows = row_mapping.size();
  const dimension_type unused_index = -(dimension_type)1;
  for (dimension_type i = 0; i < num_rows; i++) {
    int rank = row_mapping[i].first;
    dimension_type local_index = row_mapping[i].second;
    if (correct_reverse_row_mapping[rank].size() <= local_index)
      correct_reverse_row_mapping[rank].resize(local_index + 1, unused_index);
    correct_reverse_row_mapping[rank][local_index] = i;
  }

  if (reverse_row_mapping != correct_reverse_row_mapping) {
    std::cerr << "Distributed_Sparse_Matrix error: reverse_row_mapping is not the reverse of row_mapping." << std::endl;
    return false;
  }

  // 2. Check that the parts of row_reverse_mapping stored by worker nodes
  // are in sync with reverse_row_mapping.

  broadcast_operation(CHECK_OPERATION, id, num_columns());

  std::vector<dimension_type> root_reverse_mapping;
  mpi::scatter(comm(), reverse_row_mapping, root_reverse_mapping, 0);

  bool local_result = (root_reverse_mapping.size() == local_rows.size());
  if (!local_result)
    std::cerr << "Distributed_Sparse_Matrix error: row check failed for root node." << std::endl;

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
    const std::vector<dimension_type>& vec = reverse_row_mapping[rank];
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
    int rank = row_mapping[*i].first;
    dimension_type local_index = row_mapping[*i].second;
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

void
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
  } else {
    Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
    for (dimension_type i = 0; i < local_rows.size(); i++)
      if (local_rows[i].get(col_index) != 0)
        linear_combine(local_rows[i], row, col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix
::swap_rows__common(int rank1, int rank2,
                    dimension_type local_index1, dimension_type local_index2,
                    int my_rank, std::vector<Sparse_Row>& rows) {
  PPL_ASSERT(my_rank == rank1 || my_rank == rank2);
  if (rank1 == rank2) {
    std::swap(rows[local_index1], rows[local_index2]);
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
    comm().recv(rank2, 0, row);
    comm().send(rank2, 0, rows[local_index1]);
    std::swap(row, rows[local_index1]);
  } else {
    PPL_ASSERT(rank2 == my_rank);
    // rank1 < rank2, and this node has rank rank2, so it won't do the actual
    // swap.
    comm().send(rank1, 0, rows[local_index2]);
    comm().recv(rank1, 0, rows[local_index2]);
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
                      std::pair<PPL::Coefficient, PPL::Dense_Row> >: public mpl::true_ { };

}
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost__common(std::pair<std::pair<Coefficient, Coefficient>,
                                         Sparse_Row>& x,
                               const Dense_Row& working_cost,
                               const std::vector<dimension_type>&
                                 reverse_row_mapping,
                               const std::vector<dimension_type>& base,
                               const std::vector<Sparse_Row>& local_rows) {
  Coefficient& local_scaling = x.first.first;
  Coefficient& local_reverse_scaling = x.first.second;
  Sparse_Row& local_increase = x.second;

  local_scaling = 1;
  local_reverse_scaling = 1;
  local_increase.resize(working_cost.size());

  for (dimension_type local_index = 0;
      local_index < reverse_row_mapping.size(); ++local_index) {
    dimension_type global_index = reverse_row_mapping[local_index];
    const Sparse_Row& row = local_rows[local_index];
    Coefficient_traits::const_reference cost_i = working_cost[base[global_index]];
    if (cost_i != 0)
      incremental_linear_combine(local_scaling, local_reverse_scaling,
                                 local_increase, working_cost,
                                 row, base[global_index]);
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
    std::vector<Sparse_Row>& rows) {

  const std::vector<dimension_type>& indexes = node_data.second;
  dimension_type current_artificial = node_data.first;
  for (std::vector<dimension_type>::const_iterator
      i = indexes.begin(), i_end = indexes.end(); i != i_end; ++i)
    rows[*i].find_create(current_artificial, Coefficient_one());
}

void
PPL::Distributed_Sparse_Matrix::linear_combine_with_base_rows__common(
    int k_rank, dimension_type k_local_index,
    const std::vector<std::pair<dimension_type, dimension_type> >& workunit,
    int my_rank, std::vector<Sparse_Row>& local_rows) {

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

  for (std::vector<std::pair<dimension_type, dimension_type> >::const_iterator
       i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i) {
    dimension_type local_index = i->first;
    dimension_type column_index = i->second;
    const Sparse_Row& row = local_rows[local_index];
    if (row_k.get(column_index) != 0)
      incremental_linear_combine(local_scaling, local_reverse_scaling,
                                 local_increase, row_k, row, column_index);
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
  // global_result[i] == global_scaling * working_cost[i]
  //                     + global_reverse_scaling * global_increase[i].
  // The global result is stored in working_cost to improve performance.

  // TODO: use row_k.combine() instead of these two loops.
  for (Sparse_Row::iterator
       i = row_k.begin(), i_end = row_k.end(); i != i_end; ++i)
    *i *= global_scaling;
  for (Sparse_Row::const_iterator
       i = global_increase.begin(), i_end = global_increase.end();
       i != i_end; ++i)
    row_k[i.index()] += global_reverse_scaling * *i;

  // TODO: row_k seems to be already normalized, check whether this can be
  // removed or not.
  row_k.normalize();
}

void
PPL::Distributed_Sparse_Matrix::init(dimension_type num_rows1,
                                     dimension_type num_cols1) {

  my_num_columns = num_cols1,
  row_mapping.resize(num_rows1);
  reverse_row_mapping.clear();
  reverse_row_mapping.resize(comm_size);

  id = get_unique_id();

  if (comm_size == 1) {
    local_rows.resize(num_rows1);
    for (dimension_type i = 0; i < num_rows1; ++i) {
      local_rows[i].resize(num_columns());
      row_mapping[i].first = 0;
      row_mapping[i].second = i;
      reverse_row_mapping[0].push_back(i);
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
    // reverse_row_mapping, so it does not need to store
    // reverse_row_mapping[0].

    dimension_type current_row = 0;
    // These loops are splitted from the above loops, so all the worker nodes
    // can do work in parallel with the execution of these loops.
    for (int rank = 0; rank < k; ++rank) {
      reverse_row_mapping[rank].resize(n + 1);
      for (dimension_type j = 0; j < n; ++j) {
        row_mapping[current_row].first = rank;
        row_mapping[current_row].second = j;
        reverse_row_mapping[rank][j] = current_row;
        current_row++;
      }
      row_mapping[current_row].first = rank;
      row_mapping[current_row].second = n;
      reverse_row_mapping[rank][n] = current_row;
      current_row++;
    }

    for (int rank = k; rank < comm_size; ++rank) {
      reverse_row_mapping[rank].resize(n);
      for (dimension_type j = 0; j < n; ++j) {
        row_mapping[current_row].first = rank;
        row_mapping[current_row].second = j;
        reverse_row_mapping[rank][j] = current_row;
        current_row++;
      }
    }

    PPL_ASSERT(k < comm_size);
    next_rank = k + 1;
    if (next_rank == comm_size)
      next_rank = 0;

    PPL_ASSERT(current_row == num_rows1);

    local_rows.resize(root_n);
    for (std::vector<Sparse_Row>::iterator
        i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
      i->resize(num_columns());
  }
  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code) {
  PPL_ASSERT(num_operation_params[code] == 0);

  Operation op;
  op.code = code;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                                    dimension_type param0) {
  PPL_ASSERT(num_operation_params[code] == 1);

  Operation op;
  op.code = code;
  op.params[0] = param0;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                                    dimension_type param0,
                                                    dimension_type param1) {
  PPL_ASSERT(num_operation_params[code] == 2);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                                    dimension_type param0,
                                                    dimension_type param1,
                                                    dimension_type param2) {
  PPL_ASSERT(num_operation_params[code] == 3);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;
  op.params[2] = param2;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                                    dimension_type param0,
                                                    dimension_type param1,
                                                    dimension_type param2,
                                                    dimension_type param3) {
  PPL_ASSERT(num_operation_params[code] == 4);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;
  op.params[2] = param2;
  op.params[3] = param3;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                                    dimension_type param0,
                                                    dimension_type param1,
                                                    dimension_type param2,
                                                    dimension_type param3,
                                                    dimension_type param4) {
  PPL_ASSERT(num_operation_params[code] == 5);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;
  op.params[2] = param2;
  op.params[3] = param3;
  op.params[4] = param4;

  mpi::broadcast(comm(), op, 0);
}

void
PPL::Distributed_Sparse_Matrix
::get_row(dimension_type i, Sparse_Row& row) const {
  const std::pair<int, dimension_type>& row_info = row_mapping[i];
  int rank = row_info.first;
  dimension_type local_index = row_info.second;

  PPL_ASSERT(rank < comm_size);

  if (rank == 0) {
    PPL_ASSERT(local_index < local_rows.size());
    row = local_rows[local_index];
  } else {
    broadcast_operation(GET_ROW_OPERATION, rank, id, local_index);

    comm().recv(rank, 0, row);
  }
}

void
PPL::Distributed_Sparse_Matrix
::set_row(dimension_type i, const Sparse_Row& row) {
  std::pair<int, dimension_type>& row_info = row_mapping[i];
  int rank = row_info.first;
  dimension_type local_index = row_info.second;

  PPL_ASSERT(rank < comm_size);

  if (rank == 0) {
    PPL_ASSERT(local_index < local_rows.size());
    local_rows[local_index] = row;
  } else {
    broadcast_operation(SET_ROW_OPERATION, rank, id, local_index);

    comm().send(rank, 0, row);
  }
}

void
PPL::Distributed_Sparse_Matrix
::linear_combine_matrix(dimension_type row_index, dimension_type col_index) {
  std::pair<int, dimension_type>& row_info = row_mapping[row_index];
  int rank = row_info.first;
  dimension_type local_index = row_info.second;

  broadcast_operation(LINEAR_COMBINE_MATRIX_OPERATION, rank, id, local_index,
                      col_index);

  linear_combine_matrix__common(rank, local_index, col_index, 0, local_rows);
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
  broadcast_operation(REMOVE_TRAILING_ROWS_OPERATION, id);

  std::vector<dimension_type> local_sizes(comm_size);
  for (int rank = 0; rank < comm_size; ++rank)
    local_sizes[rank] = reverse_row_mapping[rank].size();
  for (dimension_type row = row_n; row < num_rows(); ++row) {
    int rank = row_mapping[row].first;
    dimension_type local_index = row_mapping[row].second;
    if (local_sizes[rank] > local_index)
      local_sizes[rank] = local_index;
  }

  dimension_type local_size;
  mpi::scatter(comm(), local_sizes, local_size, 0);

  local_rows.resize(local_size);

  row_mapping.resize(row_n);
  for (int rank = 0; rank < comm_size; ++rank)
    reverse_row_mapping[rank].resize(local_sizes[rank]);

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
  broadcast_operation(ADD_ZERO_ROWS_OPERATION, id, num_columns(),
                      flags.get_bits());

  std::vector<std::pair<dimension_type, dimension_type> > vec(comm_size);

  dimension_type k = n / comm_size;
  int remainder = n % comm_size;

  {
    dimension_type current_global_index = num_rows();
    for (int rank = 0; rank < remainder; ++rank) {
      vec[rank].first = k + 1;
      vec[rank].second = current_global_index;
      current_global_index += k + 1;
    }
    for (int rank = remainder; rank < comm_size; ++rank) {
      vec[rank].first = k;
      vec[rank].second = current_global_index;
      current_global_index += k;
    }
    PPL_ASSERT(current_global_index == num_rows() + n);
  }

  next_rank += remainder;
  if (next_rank >= comm_size)
    next_rank -= comm_size;
  PPL_ASSERT(next_rank >= 0 && next_rank < comm_size);

  std::pair<dimension_type, dimension_type> x;
  mpi::scatter(comm(), vec, x, 0);
  dimension_type root_n = x.first;
  // x.second is unused, because the root node already stores the full
  // reverse_row_mapping, so it does not need to separately store
  // reverse_row_mapping[0].

#ifndef NDEBUG
  const dimension_type old_row_n = num_rows();
#endif

  dimension_type row_n = num_rows();

  for (int rank = 0; rank < comm_size; rank++) {
    dimension_type local_row_n = reverse_row_mapping[rank].size();
    for (dimension_type i = 0; i < vec[rank].first; i++) {
      reverse_row_mapping[rank].push_back(row_n);
      ++row_n;
      row_mapping.push_back(std::make_pair(rank, local_row_n));
      ++local_row_n;
    }
  }

  PPL_ASSERT(row_n == old_row_n + n);

  if (root_n == 0)
    return;

  Sparse_Row row(num_columns(), flags);
  local_rows.resize(local_rows.size() + root_n, row);

  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix::add_row(Sparse_Row& row) {
  PPL_ASSERT(row.size() == num_columns());
  int rank = next_rank;
  ++next_rank;
  if (next_rank == comm_size)
    next_rank = 0;
  broadcast_operation(ADD_ROW_OPERATION, rank);
  dimension_type local_index = reverse_row_mapping[rank].size();
  dimension_type global_index = num_rows();
  reverse_row_mapping[rank].push_back(global_index);
  row_mapping.push_back(std::make_pair(rank, local_index));
  if (rank == 0) {
    local_rows.resize(local_rows.size() + 1);
    std::swap(local_rows.back(), row);
  } else {
    comm().send(rank, 0, std::make_pair(id, global_index));
    comm().send(rank, 0, row);
  }
  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::swap_rows(dimension_type row_index1, dimension_type row_index2) {
  int rank1 = row_mapping[row_index1].first;
  dimension_type local_index1 = row_mapping[row_index1].second;
  int rank2 = row_mapping[row_index2].first;
  dimension_type local_index2 = row_mapping[row_index2].second;
  if (rank1 == 0 && rank2 == 0) {
    std::swap(local_rows[local_index1], local_rows[local_index2]);
    return;
  }
  broadcast_operation(SWAP_ROWS_OPERATION, id, rank1, local_index1, rank2,
                      local_index2);
  swap_rows__common(rank1, rank2, local_index1, local_index2, 0, local_rows);
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost(Dense_Row& working_cost,
                       const std::vector<dimension_type>& base) {
  PPL_ASSERT(working_cost.size() == num_columns());
  PPL_ASSERT(base.size() == num_rows());
  broadcast_operation(COMPUTE_WORKING_COST_OPERATION, id);
  mpi::broadcast(comm(), working_cost, 0);

  // base will not be modified.
  // This const cast is needed because mpi::broadcast takes a non-const
  // reference.
  std::vector<dimension_type>& base_ref
    = const_cast<std::vector<dimension_type>&>(base);
  mpi::broadcast(comm(), base_ref, 0);

  std::vector<dimension_type> root_reverse_row_mapping;
  mpi::scatter(comm(), reverse_row_mapping, root_reverse_row_mapping, 0);

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;
  compute_working_cost__common(x, working_cost, root_reverse_row_mapping,
                               base, local_rows);

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

  for (dimension_type i = 0; i < working_cost.size(); ++i)
    working_cost[i] *= global_scaling;
  for (Sparse_Row::const_iterator
       i = global_increase.begin(), i_end = global_increase.end();
       i != i_end; ++i)
    working_cost[i.index()] += global_reverse_scaling * *i;

#ifndef NDEBUG
  // Check that the working_cost values that correspond to variables in base
  // are zero.
  for (dimension_type i = 0; i < num_rows(); ++i)
    PPL_ASSERT(working_cost[base[i]] == 0);
#endif

  working_cost.normalize();
}

void
PPL::Distributed_Sparse_Matrix::make_inhomogeneous_terms_nonpositive() {
  broadcast_operation(MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION, id);

  make_inhomogeneous_terms_nonpositive__common(local_rows);
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
    int rank = row_mapping[*i].first;
    dimension_type local_index = row_mapping[*i].second;
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

  set_artificial_indexes_for_unfeasible_rows__common(root_data, local_rows);
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
    int rank = row_mapping[i].first;
    dimension_type local_index = row_mapping[i].second;
    stream << output[rank][local_index];
  }
}

void
PPL::Distributed_Sparse_Matrix
::linear_combine_with_base_rows(const std::vector<dimension_type>& base,
                                dimension_type k) {
  int k_rank = row_mapping[k].first;
  dimension_type k_local_index = row_mapping[k].second;

  broadcast_operation(LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION, id, k_rank,
                      k_local_index);

  // This vector will be scattered to nodes.
  // vec[rank] contains the <local_index, column> pairs relevant to that node.
  std::vector<std::vector<std::pair<dimension_type, dimension_type> > >
    vec(comm_size);

  for (dimension_type i = base.size(); i-- > 0; )
    if (i != k && base[i] != 0) {
      int rank = row_mapping[i].first;
      dimension_type local_index = row_mapping[i].second;
      vec[rank].push_back(std::make_pair(local_index, base[i]));
    }

  std::vector<std::pair<dimension_type, dimension_type> > workunit;
  mpi::scatter(comm(), vec, workunit, 0);

  linear_combine_with_base_rows__common(k_rank, k_local_index, workunit, 0,
                                        local_rows);
}

PPL::dimension_type
PPL::Distributed_Sparse_Matrix::get_unique_id() {
  static dimension_type next_id = 0;
  return next_id++;
}

const mpi::communicator&
PPL::Distributed_Sparse_Matrix::comm() {
  PPL_ASSERT(comm_ptr != 0);
  return *comm_ptr;
}

namespace std {

void
swap(PPL::Distributed_Sparse_Matrix& x, PPL::Distributed_Sparse_Matrix& y) {
  x.swap(y);
}

} // namespace std


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
  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(num_rows);
  for (std::vector<Sparse_Row>::iterator i = rows.begin(), i_end = rows.end();
      i != i_end; ++i)
    i->resize(num_cols);
  std::vector<dimension_type>& reverse_row_mapping
    = row_chunk.reverse_row_mapping;
  reverse_row_mapping.resize(num_rows);
  for (std::vector<dimension_type>::iterator
       i = reverse_row_mapping.begin(), i_end = reverse_row_mapping.end();
       i != i_end; ++i) {
    *i = current_global_index;
    ++current_global_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker::copy_matrix(dimension_type source_id,
                                               dimension_type id) {
  PPL_ASSERT(row_chunks.find(id) == row_chunks.end());
  row_chunks_itr_type itr = row_chunks.find(source_id);
  if (itr == row_chunks.end())
    // This node doesn't store data for the specified matrix, nothing to do.
    return;
  row_chunks[id] = itr->second;
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
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end()) {
    PPL_ASSERT(my_rank != rank);
    // Partecipate in the broadcast operation, then return.
    Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
  } else {
    linear_combine_matrix__common(rank, local_row_index, col_index, my_rank,
                                  itr->second.rows);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::reset_column(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    std::vector<Sparse_Row>& rows = itr->second.rows;
    for (std::vector<Sparse_Row>::iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
      i->reset(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_column(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    std::vector<Sparse_Row>& rows = itr->second.rows;
    for (std::vector<Sparse_Row>::iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
      i->delete_element_and_shift(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_columns(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    std::vector<Sparse_Row>& rows = itr->second.rows;
    for (std::vector<Sparse_Row>::iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
      i->resize(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_zero_columns(dimension_type id, dimension_type n) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    std::vector<Sparse_Row>& rows = itr->second.rows;
    for (std::vector<Sparse_Row>::iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
      i->resize(i->size() + n);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::check(dimension_type id, dimension_type num_columns) const {
  std::vector<dimension_type> correct_reverse_row_mapping;
  mpi::scatter(comm(), correct_reverse_row_mapping, 0);

  row_chunks_const_itr_type itr = row_chunks.find(id);
  bool result = true;
  if (itr == row_chunks.end()) {
    if (correct_reverse_row_mapping.size() != 0) {
      std::cerr << "Worker node: row check failed" << std::endl;
      result = false;
    }
  } else {
    if (correct_reverse_row_mapping.size() != itr->second.rows.size()) {
      std::cerr << "Worker node: row check failed" << std::endl;
      result = false;
    }
    if (correct_reverse_row_mapping != itr->second.reverse_row_mapping) {
      std::cerr << "Worker node: reverse_row_mapping check failed" << std::endl;
      std::cerr << "Correct reverse_row_mapping:" << std::endl;
      for (std::vector<dimension_type>::const_iterator
           i = correct_reverse_row_mapping.begin(),
           i_end = correct_reverse_row_mapping.end(); i != i_end; ++i)
        std::cerr << *i << ' ';
      std::cerr << std::endl;
      std::cerr << "Local reverse_row_mapping:" << std::endl;
      for (std::vector<dimension_type>::const_iterator
           i = itr->second.reverse_row_mapping.begin(),
           i_end = itr->second.reverse_row_mapping.end(); i != i_end; ++i)
        std::cerr << *i << ' ';
      std::cerr << std::endl;
      result = false;
    }
  }
  if (itr != row_chunks.end()) {
    const std::vector<Sparse_Row>& rows = itr->second.rows;
    for (std::vector<Sparse_Row>::const_iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
      if (i->size() != num_columns) {
        std::cerr << "Worker node: column check failed" << std::endl;
        result = false;
      }
  }
  mpi::reduce(comm(), result, std::logical_and<bool>(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_zero_rows(dimension_type id, dimension_type num_columns,
                dimension_type flag_bits) {
  std::pair<dimension_type, dimension_type> x;
  mpi::scatter(comm(), x, 0);
  dimension_type n = x.first;
  dimension_type current_global_index = x.second;
  if (n == 0)
    return;
  Row_Flags flags(flag_bits);
  Sparse_Row row(num_columns, flags);
  // This may default-construct the Row_Chunk.
  Row_Chunk& row_chunk = row_chunks[id];
  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(rows.size() + n, row);
  std::vector<dimension_type>& reverse_row_mapping
    = row_chunk.reverse_row_mapping;
  for (dimension_type i = 0; i < n; ++i) {
    reverse_row_mapping.push_back(current_global_index);
    ++current_global_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker::add_row(int rank) {
  if (rank != my_rank)
    return;
  std::pair<dimension_type, dimension_type> x;
  comm().recv(0, 0, x);
  dimension_type id = x.first;
  dimension_type global_index = x.second;
  Row_Chunk& row_chunk = row_chunks[id];
  std::vector<Sparse_Row>& rows = row_chunk.rows;
  rows.resize(rows.size() + 1);
  row_chunk.reverse_row_mapping.push_back(global_index);
  Sparse_Row& row = rows.back();
  comm().recv(0, 0, row);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_rows(dimension_type id) {
  dimension_type local_size;
  mpi::scatter(comm(), local_size, 0);
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    PPL_ASSERT(itr->second.rows.size() >= local_size);
    Row_Chunk& row_chunk = itr->second;
    std::vector<Sparse_Row>& rows = row_chunk.rows;
    rows.resize(local_size);
    std::vector<dimension_type>& reverse_row_mapping
      = row_chunk.reverse_row_mapping;
    reverse_row_mapping.resize(local_size);
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
  swap_rows__common(rank1, rank2, local_index1, local_index2, my_rank,
                    row_chunks[id].rows);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::fill_matrix(dimension_type id) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;
  std::vector<Sparse_Row>& rows = itr->second.rows;

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
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end()) {
    mpi::reduce(comm(), true, std::logical_and<bool>(), 0);
    return;
  }
  std::vector<Sparse_Row>& rows = itr->second.rows;
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
      std::cout << "Found mismatch in worker node" << std::endl;
      result = false;
      break;
    }

  mpi::reduce(comm(), result, std::logical_and<bool>(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::compute_working_cost(dimension_type id) {

  Dense_Row working_cost(0, Row_Flags());
  mpi::broadcast(comm(), working_cost, 0);

  std::vector<dimension_type> base;
  mpi::broadcast(comm(), base, 0);

  std::vector<dimension_type> reverse_row_mapping;
  mpi::scatter(comm(), reverse_row_mapping, 0);

  row_chunks_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {

    // Construct a dummy result object
    std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row>
      x(std::pair<Coefficient, Coefficient>(1, 1), Sparse_Row(working_cost.size()));

    // And use it in the reduce operation.
    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);

  } else {
    std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;

    compute_working_cost__common(x, working_cost, reverse_row_mapping, base,
                                 itr->second.rows);

    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::make_inhomogeneous_terms_nonpositive(dimension_type id) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;
  make_inhomogeneous_terms_nonpositive__common(itr->second.rows);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_artificial_indexes_for_unfeasible_rows(dimension_type id) {
  std::pair<dimension_type, std::vector<dimension_type> > node_data;
  mpi::scatter(comm(), node_data, 0);

  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;

  set_artificial_indexes_for_unfeasible_rows__common(node_data,
                                                     itr->second.rows);
}

void
PPL::Distributed_Sparse_Matrix::Worker::ascii_dump(dimension_type id) const {

  std::vector<std::string> output;

  row_chunks_const_itr_type itr = row_chunks.find(id);

  if (itr != row_chunks.end()) {
    const std::vector<Sparse_Row>& rows = itr->second.rows;
    output.reserve(rows.size());
    for (std::vector<Sparse_Row>::const_iterator
        i = rows.begin(), i_end = rows.end(); i != i_end; ++i) {
      std::ostringstream strstream;
      i->ascii_dump(strstream);
      // TODO: Avoid the string copy.
      output.push_back(strstream.str());
    }
  }

  mpi::gather(comm(), output, 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_with_base_rows(dimension_type id, int k_rank,
                                dimension_type k_local_index) {

  std::vector<std::pair<dimension_type, dimension_type> > workunit;
  mpi::scatter(comm(), workunit, 0);

  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end()) {
    // Nothing to do, partecipate in the collective operations to keep in sync
    // with other nodes.
    Sparse_Row row;
    mpi::broadcast(comm(), row, k_rank);
    std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;
    x.first.first = 1;
    x.first.second = 1;
    x.second.resize(row.size());
    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), k_rank);
    return;
  }

  linear_combine_with_base_rows__common(k_rank, k_local_index, workunit,
                                        my_rank, itr->second.rows);
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
