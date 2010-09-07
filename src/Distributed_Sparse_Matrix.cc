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
  1, // GET_ROW_OPERATION: rank
  1, // SET_ROW_OPERATION: rank
  3, // LINEAR_COMBINE_OPERATION: rank, id, col_index
  2, // REMOVE_COLUMN_OPERATION: id, col_index
  2, // REMOVE_TRAILING_COLUMNS_OPERATION: id, n
  2, // ADD_ZERO_COLUMNS_OPERATION: id, n
  2, // CHECK_OPERATION: id, num_columns
  3, // ADD_ZERO_ROWS_OPERATION: id, num_columns, flag_bits
  1, // ADD_ROW_OPERATION: rank
  3, // LINEAR_COMBINE_SOME_OPERATION: id, rank, column_index
  2, // LINEAR_COMBINE_WITH_OPERATION: id, column_index
  2, // RESET_COLUMN_OPERATION: id, column_index
  1, // REMOVE_TRAILING_ROWS_OPERATION: id
  5, // SWAP_ROWS_OPERATION: id, rank1, local_index1, rank2, local_index2
  1, // FILL_MATRIX_OPERATION: id
  1, // COMPARE_WITH_SPARSE_MATRIX_OPERATION: id
  1, // COMPUTE_WORKING_COST_OPERATION: id
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
      worker.get_row(op.params[0]);
      break;

    case SET_ROW_OPERATION:
      worker.set_row(op.params[0]);
      break;

    case LINEAR_COMBINE_OPERATION:
      worker.linear_combine_matrix(op.params[0], op.params[1], op.params[2]);
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

    case LINEAR_COMBINE_SOME_OPERATION:
      worker.linear_combine_some(op.params[0], op.params[1], op.params[2]);
      break;

    case LINEAR_COMBINE_WITH_OPERATION:
      worker.linear_combine_with(op.params[0], op.params[1]);
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

  // 2. Check that the worker nodes are in sync with row_mapping.

  broadcast_operation(CHECK_OPERATION, id, num_columns());

  std::vector<dimension_type> vec(comm_size);
  for (int rank = 0; rank < comm_size; ++rank)
    vec[rank] = reverse_row_mapping[rank].size();

  dimension_type root_n;
  mpi::scatter(comm(), vec, root_n, 0);

  bool local_result = (root_n == local_rows.size());
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

    std::vector<dimension_type> vec(comm_size);

    // The first k nodes will store n+1 rows, the others will store k rows.
    // Note that n may be zero, but we need to send those zeroes so the nodes
    // will all return to the main listening loop.

    dimension_type current_row = 0;

    for (int rank = 0; rank < k; ++rank)
      vec[rank] = n + 1;

    for (int rank = k; rank < comm_size; ++rank)
      vec[rank] = n;

    dimension_type root_n;
    mpi::scatter(comm(), vec, root_n, 0);

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

namespace {

class linear_combine_helper1 {

public:
  inline
  linear_combine_helper1(const PPL::Coefficient& normalized_y_k1)
    : normalized_y_k(normalized_y_k1) {
  }

  inline void
  operator()(PPL::Coefficient& x) const {
    x *= normalized_y_k;
  }

private:
  PPL::Coefficient normalized_y_k;
};

class linear_combine_helper2 {

public:
  inline
  linear_combine_helper2(const PPL::Coefficient& normalized_x_k1,
                         const PPL::Coefficient& normalized_y_k1)
    : normalized_x_k(normalized_x_k1), normalized_y_k(normalized_y_k1) {
  }

  inline void
  operator()(PPL::Coefficient& x, const PPL::Coefficient& y) const {
    x *= normalized_y_k;
    PPL::sub_mul_assign(x, y, normalized_x_k);
  }

private:
  PPL::Coefficient normalized_x_k;
  PPL::Coefficient normalized_y_k;
};

class linear_combine_helper3 {

public:
  inline
  linear_combine_helper3(const PPL::Coefficient& normalized_x_k1)
    : normalized_x_k(normalized_x_k1) {
  }

  inline void
  operator()(PPL::Coefficient& x, const PPL::Coefficient& y) const {
    x = y;
    x *= normalized_x_k;
    PPL::neg_assign(x);
  }

private:
  PPL::Coefficient normalized_x_k;
};

} // namespace

// This is needed because PPL_DIRTY_TEMP_COEFFICIENT works only inside
// the PPL namespace.
namespace Parma_Polyhedra_Library {

void
linear_combine(Dense_Row& x, const Sparse_Row& y,
               const dimension_type k,
               Coefficient& normalized_y_k, Coefficient& gcd) {
  WEIGHT_BEGIN();
  const dimension_type x_size = x.size();
  Coefficient_traits::const_reference x_k = x.get(k);
  Coefficient_traits::const_reference y_k = y.get(k);
  PPL_ASSERT(y_k != 0 && x_k != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_k);
  normalize2(x_k, y_k, normalized_x_k, normalized_y_k);
  Sparse_Row::const_iterator j = y.begin();
  Sparse_Row::const_iterator j_end = y.end();
  dimension_type i;
  for (i = 0; j != j_end; ++i) {
    PPL_ASSERT(i < x_size);
    PPL_ASSERT(j.index() >= i);
    if (i != k) {
      Coefficient& x_i = x[i];
      x_i *= normalized_y_k;
      if (j.index() == i) {
        Coefficient_traits::const_reference y_i = *j;
        // FIXME: check if adding "if (j->second != 0)" speeds this up.
        sub_mul_assign(x_i, y_i, normalized_x_k);
        ++j;
      }
    } else
      if (j.index() == k)
        ++j;
  }
  PPL_ASSERT(j == j_end);
  for ( ; i < x_size; ++i)
    if (i != k) {
      Coefficient& x_i = x[i];
      x_i *= normalized_y_k;
    }
  x[k] = 0;
  x.normalize(gcd);
  WEIGHT_ADD_MUL(83, x_size);
}

void
linear_combine(Sparse_Row& x, const Sparse_Row& y, const dimension_type k) {
  const Coefficient& x_k = x.get(k);
  const Coefficient& y_k = y.get(k);
  PPL_ASSERT(y_k != 0 && x_k != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_k);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_y_k);
  normalize2(x_k, y_k, normalized_x_k, normalized_y_k);

  x.combine(y,
            linear_combine_helper1(normalized_y_k),
            linear_combine_helper2(normalized_x_k, normalized_y_k),
            linear_combine_helper3(normalized_x_k));

  x.reset(k);
  x.normalize();
}

} // namespace Parma_Polyhedra_Library

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
    broadcast_operation(GET_ROW_OPERATION, rank);

    std::pair<dimension_type, dimension_type> y(id, local_index);
    comm().send(rank, 0, y);
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
    broadcast_operation(SET_ROW_OPERATION, rank);

    std::pair<dimension_type, dimension_type> y(id, local_index);
    comm().send(rank, 0, y);
    comm().send(rank, 0, row);
  }
}

void
PPL::Distributed_Sparse_Matrix
::linear_combine_matrix(dimension_type row_index, dimension_type col_index) {
  std::pair<int, dimension_type>& row_info = row_mapping[row_index];
  int rank = row_info.first;
  dimension_type local_index = row_info.second;

  broadcast_operation(LINEAR_COMBINE_OPERATION, rank, id, col_index);

  if (rank == 0) {
    PPL_ASSERT(local_index < local_rows.size());
    Sparse_Row& row = local_rows[local_index];
    mpi::broadcast(comm(), row, 0);
    for (dimension_type i = 0; i < local_rows.size(); i++)
      if (i != local_index && local_rows[i].get(col_index) != 0)
        linear_combine(local_rows[i], row, col_index);
  } else {
    Sparse_Row row;
    comm().send(rank, 0, local_index);
    mpi::broadcast(comm(), row, rank);
    for (dimension_type i = 0; i < local_rows.size(); i++)
      if (local_rows[i].get(col_index) != 0)
        linear_combine(local_rows[i], row, col_index);
  }
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

  std::vector<dimension_type> vec(comm_size, n / comm_size);

  dimension_type remainder = n % comm_size;
  for (dimension_type i = 0; i < remainder; i++) {
    ++vec[next_rank];
    ++next_rank;
    if (next_rank == comm_size)
      next_rank = 0;
  }

  dimension_type root_n;
  mpi::scatter(comm(), vec, root_n, 0);

#ifndef NDEBUG
  const dimension_type old_row_n = num_rows();
#endif

  dimension_type row_n = num_rows();

  for (int rank = 0; rank < comm_size; rank++) {
    dimension_type local_row_n = reverse_row_mapping[rank].size();
    for (dimension_type i = 0; i < vec[rank]; i++) {
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
  reverse_row_mapping[rank].push_back(num_rows());
  row_mapping.push_back(std::make_pair(rank, local_index));
  if (rank == 0) {
    local_rows.resize(local_rows.size() + 1);
    std::swap(local_rows.back(), row);
  } else {
    comm().send(rank, 0, id);
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
  if (rank1 != 0 && rank2 != 0)
    return;
  PPL_ASSERT(rank1 != rank2);
  if (rank1 != 0) {
    // These swaps are useful to simplify the code below.
    std::swap(rank1, rank2);
    std::swap(local_index1, local_index2);
  }
  PPL_ASSERT(rank1 == 0);
  PPL_ASSERT(rank2 != 0);
  // rank1 < rank2, and this node has rank rank1, so it will do the actual
  // swap.
  Sparse_Row row;
  comm().recv(rank2, 0, row);
  comm().send(rank2, 0, local_rows[local_index1]);
  std::swap(row, local_rows[local_index1]);
}

void
PPL::Distributed_Sparse_Matrix
::linear_combine_some(const std::vector<dimension_type>& row_indexes,
                      dimension_type row_i, dimension_type col_i) {
#ifndef NDEBUG
  for (std::vector<dimension_type>::const_iterator
      i = row_indexes.begin(), i_end = row_indexes.end(); i != i_end; ++i)
    PPL_ASSERT(*i != row_i);
#endif
  int rank = row_mapping[row_i].first;
  dimension_type local_index = row_mapping[row_i].second;
  broadcast_operation(LINEAR_COMBINE_SOME_OPERATION, id, rank, col_i);
  std::vector<std::vector<dimension_type> > vec;
  std::vector<dimension_type> root_indexes;
  map_indexes(row_indexes, vec);
  mpi::scatter(comm(), vec, root_indexes, 0);
  if (rank == 0) {
    Sparse_Row& row = local_rows[local_index];
    mpi::broadcast(comm(), row, 0);
    for (std::vector<dimension_type>::const_iterator
        i = root_indexes.begin(), i_end = root_indexes.end(); i != i_end; ++i) {
      PPL_ASSERT(*i < local_rows.size());
      linear_combine(local_rows[*i], row, col_i);
    }
  } else {
    comm().send(rank, 0, local_index);
    Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
    for (std::vector<dimension_type>::const_iterator
        i = root_indexes.begin(), i_end = root_indexes.end(); i != i_end; ++i) {
      PPL_ASSERT(*i < local_rows.size());
      linear_combine(local_rows[*i], row, col_i);
    }
  }
}

void
PPL::Distributed_Sparse_Matrix
::linear_combine_with(const Sparse_Row& row, dimension_type column_index) {
  broadcast_operation(LINEAR_COMBINE_WITH_OPERATION, id, column_index);
  // The row will not be modified. The const_cast is needed because
  // mpi::broadcast takes a non-const reference.
  Sparse_Row& row_ref = const_cast<Sparse_Row&>(row);
  mpi::broadcast(comm(), row_ref, 0);
  for (std::vector<Sparse_Row>::iterator
      i = local_rows.begin(), i_end = local_rows.end(); i != i_end; ++i)
    linear_combine(*i, row, column_index);
}

namespace {

struct compute_working_cost_reducer_functor {
  typedef std::pair<std::pair<PPL::Coefficient,
                              PPL::Coefficient>, PPL::Dense_Row> pair_type;
  pair_type
  operator()(const pair_type& x, const pair_type& y) const {
    pair_type result(x);
    const PPL::Coefficient& x_scaling = x.first.first;
    const PPL::Coefficient& x_reverse_scaling = x.first.second;
    const PPL::Coefficient& y_scaling = y.first.first;
    const PPL::Coefficient& y_reverse_scaling = y.first.second;
    const PPL::Dense_Row& y_row = y.second;
    PPL::Coefficient& scaling = result.first.first;
    PPL::Coefficient& reverse_scaling = result.first.second;
    PPL::Dense_Row& row = result.second;

    PPL::Coefficient x_normalized_scaling = x_scaling * y_reverse_scaling;
    PPL::Coefficient y_normalized_scaling = y_scaling * x_reverse_scaling;
    PPL::Coefficient gcd;
    PPL::gcd_assign(gcd, x_normalized_scaling, y_normalized_scaling);
    PPL::exact_div_assign(x_normalized_scaling, x_normalized_scaling, gcd);
    PPL::exact_div_assign(y_normalized_scaling, y_normalized_scaling, gcd);

    scaling *= y_scaling;
    reverse_scaling *= y_reverse_scaling;
    reverse_scaling *= gcd;
    PPL::normalize2(scaling, reverse_scaling, scaling, reverse_scaling);

    PPL::dimension_type n = x.second.size();
    PPL::Dense_Row tmp(n, PPL::Row_Flags());
    for (PPL::dimension_type i = 0; i < n; i++) {
      row[i] *= y_normalized_scaling;
      row[i] += x_normalized_scaling * y_row[i];
    }
    // TODO: Check if the copy can be avoided.
    return result;
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

  std::pair<std::pair<Coefficient, Coefficient>, Dense_Row>
    x(std::pair<Coefficient, Coefficient>(1, 1), working_cost);

  Coefficient& local_scaling = x.first.first;
  Coefficient& local_reverse_scaling = x.first.second;
  Dense_Row& local_result = x.second;
  PPL_DIRTY_TEMP_COEFFICIENT(scaling);
  PPL_DIRTY_TEMP_COEFFICIENT(reverse_scaling);
  for (dimension_type local_index = 0;
      local_index < root_reverse_row_mapping.size(); ++local_index) {
    dimension_type global_index = root_reverse_row_mapping[local_index];
    const Sparse_Row& row = local_rows[local_index];
    Coefficient_traits::const_reference cost_i = local_result[base[global_index]];
    if (cost_i != 0) {
      linear_combine(local_result, row, base[global_index], scaling,
                     reverse_scaling);
      local_scaling *= scaling;
      local_reverse_scaling *= reverse_scaling;
    }
  }
  normalize2(local_scaling, local_reverse_scaling,
             local_scaling, local_reverse_scaling);
  // Calculate the local increase such that, for each i:
  // local_result[i] == local_scaling * working_cost[i]
  //                    + local_reverse_scaling * local_increase[i].
  // Local increase is stored in local_result to improve performance.
  for (dimension_type i = 0; i < local_result.size(); ++i) {
    local_result[i] -= local_scaling * working_cost[i];
    PPL_ASSERT(local_result[i] % local_reverse_scaling == 0);
    exact_div_assign(local_result[i], local_result[i], local_reverse_scaling);
  }

  std::pair<std::pair<Coefficient, Coefficient>, Dense_Row> y;
  mpi::reduce(comm(), x, y, compute_working_cost_reducer_functor(), 0);


  PPL_ASSERT(comm_size > 1 || x == y);

  Coefficient_traits::const_reference global_scaling = y.first.first;
  Coefficient_traits::const_reference global_reverse_scaling = y.first.second;
  const Dense_Row& global_increase = y.second;

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
  PPL_ASSERT(working_cost.size() == global_increase.size());
  PPL_ASSERT(working_cost.size() == local_result.size());
  for (dimension_type i = 0; i < local_result.size(); ++i) {
    working_cost[i] *= global_scaling;
    working_cost[i] += global_reverse_scaling * global_increase[i];
  }

  // Reset the working_cost values that correspond to variables in base.
  for (dimension_type i = 0; i < num_rows(); ++i) {
    PPL_ASSERT(base[i] < working_cost.size());
    working_cost[base[i]] = 0;
  }

  working_cost.normalize();
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
  dimension_type num_rows;
  mpi::scatter(comm(), num_rows, 0);
  if (num_rows == 0)
    return;
  std::vector<Sparse_Row>& rows = row_chunks[id];
  rows.resize(num_rows);
  for (std::vector<Sparse_Row>::iterator i = rows.begin(), i_end = rows.end();
      i != i_end; ++i)
    i->resize(num_cols);
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
PPL::Distributed_Sparse_Matrix::Worker::get_row(int rank) const {
  if (my_rank != rank)
    return;
  std::pair<dimension_type, dimension_type> x;
  comm().recv(0, 0, x);
  dimension_type id = x.first;
  dimension_type row_index = x.second;
  PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
  PPL_ASSERT(row_index < row_chunks.find(id)->second.size());
  comm().send(0, 0, row_chunks.find(id)->second[row_index]);
}

void
PPL::Distributed_Sparse_Matrix::Worker::set_row(int rank) {
  if (my_rank != rank)
    return;
  std::pair<dimension_type, dimension_type> x;
  comm().recv(0, 0, x);
  dimension_type id = x.first;
  dimension_type row_index = x.second;
  // If row_chunks did not contain the `id' key, the assertion will fail,
  // because default-constructed rows have size 0.
  PPL_ASSERT(row_index < row_chunks[id].size());
  comm().recv(0, 0, row_chunks[id][row_index]);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_matrix(int rank, dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end()) {
    PPL_ASSERT(my_rank != rank);
    // Partecipate in the broadcast operation, then return.
    Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
  } else {
    std::vector<Sparse_Row>& rows = itr->second;
    if (my_rank == rank) {
      dimension_type local_index;
      comm().recv(0, 0, local_index);
      Sparse_Row& row = rows[local_index];
      mpi::broadcast(comm(), row, rank);
      for (dimension_type i = 0; i < rows.size(); i++)
        if (i != local_index && rows[i].get(col_index) != 0)
          linear_combine(rows[i], row, col_index);
    } else {
      Sparse_Row row;
      mpi::broadcast(comm(), row, rank);
      for (std::vector<Sparse_Row>::iterator
          i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
        if (i->get(col_index) != 0)
          linear_combine(*i, row, col_index);
    }
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::reset_column(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    for (std::vector<Sparse_Row>::iterator
        i = itr->second.begin(), i_end = itr->second.end(); i != i_end; ++i)
      i->reset(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_column(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    for (std::vector<Sparse_Row>::iterator
        i = itr->second.begin(), i_end = itr->second.end(); i != i_end; ++i)
      i->delete_element_and_shift(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_columns(dimension_type id, dimension_type col_index) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    for (std::vector<Sparse_Row>::iterator
        i = itr->second.begin(), i_end = itr->second.end(); i != i_end; ++i)
      i->resize(col_index);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_zero_columns(dimension_type id, dimension_type n) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    for (std::vector<Sparse_Row>::iterator
        i = itr->second.begin(), i_end = itr->second.end(); i != i_end; ++i)
      i->resize(i->size() + n);
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::check(dimension_type id, dimension_type num_columns) const {
  dimension_type n;
  mpi::scatter(comm(), n, 0);

  row_chunks_const_itr_type itr = row_chunks.find(id);
  bool result;
  if (itr == row_chunks.end())
    result = (n == 0);
  else
    result = (n == itr->second.size());
  if (!result)
    std::cerr << "Worker node: row check failed" << std::endl;
  if (itr != row_chunks.end()) {
    for (std::vector<Sparse_Row>::const_iterator
        i = itr->second.begin(), i_end = itr->second.end(); i != i_end; ++i)
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
  dimension_type n;
  mpi::scatter(comm(), n, 0);
  if (n == 0)
    return;
  Row_Flags flags(flag_bits);
  Sparse_Row row(num_columns, flags);
  // This may default-construct the vector.
  std::vector<Sparse_Row>& rows = row_chunks[id];
  rows.resize(rows.size() + n, row);
}

void
PPL::Distributed_Sparse_Matrix::Worker::add_row(int rank) {
  if (rank != my_rank)
    return;
  dimension_type id;
  comm().recv(0, 0, id);
  std::vector<Sparse_Row>& rows = row_chunks[id];
  rows.resize(rows.size() + 1);
  Sparse_Row& row = rows.back();
  comm().recv(0, 0, row);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_some(dimension_type id, int rank,
                      dimension_type column_index) {
  std::vector<dimension_type> local_indexes;
  mpi::scatter(comm(), local_indexes, 0);
  std::vector<Sparse_Row>& rows = row_chunks[id];
  if (my_rank == rank) {
    dimension_type local_index;
    comm().recv(0, 0, local_index);
    PPL_ASSERT(local_index < rows.size());
    Sparse_Row& row = rows[local_index];
    mpi::broadcast(comm(), row, rank);
    PPL_ASSERT(row.get(column_index) != 0);
    for (std::vector<dimension_type>::const_iterator
        i = local_indexes.begin(), i_end = local_indexes.end(); i != i_end; ++i) {
      PPL_ASSERT(*i < rows.size());
      linear_combine(rows[*i], row, column_index);
    }
  } else {
    Sparse_Row row;
    mpi::broadcast(comm(), row, rank);
    PPL_ASSERT(row.get(column_index) != 0);
    for (std::vector<dimension_type>::const_iterator
        i = local_indexes.begin(), i_end = local_indexes.end(); i != i_end; ++i) {
      PPL_ASSERT(*i < rows.size());
      linear_combine(rows[*i], row, column_index);
    }
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_with(dimension_type id, dimension_type column_index) {
  Sparse_Row row;
  mpi::broadcast(comm(), row, 0);
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;
  std::vector<Sparse_Row>& rows = itr->second;
  for (std::vector<Sparse_Row>::iterator
      i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    linear_combine(*i, row, column_index);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_trailing_rows(dimension_type id) {
  dimension_type local_size;
  mpi::scatter(comm(), local_size, 0);
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr != row_chunks.end()) {
    PPL_ASSERT(itr->second.size() >= local_size);
    itr->second.resize(local_size);
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
  std::vector<Sparse_Row>& rows = row_chunks[id];
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

void
PPL::Distributed_Sparse_Matrix::Worker
::fill_matrix(dimension_type id) {
  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;
  std::vector<Sparse_Row>& rows = itr->second;

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
  std::vector<Sparse_Row>& rows = itr->second;
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
    std::pair<std::pair<Coefficient, Coefficient>, Dense_Row>
      x(std::pair<Coefficient, Coefficient>(1, 1), Dense_Row());
    x.second.construct(working_cost.size(), Row_Flags());

    // And use it in the reduce operation.
    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);

  } else {
    std::vector<Sparse_Row>& rows = itr->second;
    PPL_ASSERT(reverse_row_mapping.size() == rows.size());

    std::pair<std::pair<Coefficient, Coefficient>, Dense_Row>
      x(std::pair<Coefficient, Coefficient>(1, 1), working_cost);

    Coefficient& local_scaling = x.first.first;
    Coefficient& local_reverse_scaling = x.first.second;
    Dense_Row& local_result = x.second;
    PPL_DIRTY_TEMP_COEFFICIENT(scaling);
    PPL_DIRTY_TEMP_COEFFICIENT(reverse_scaling);
    for (dimension_type local_index = 0;
        local_index < reverse_row_mapping.size(); ++local_index) {
      dimension_type global_index = reverse_row_mapping[local_index];
      const Sparse_Row& row = rows[local_index];
      Coefficient_traits::const_reference cost_i = local_result[base[global_index]];
      if (cost_i != 0) {
        linear_combine(local_result, row, base[global_index], scaling,
                      reverse_scaling);
        local_scaling *= scaling;
        local_reverse_scaling *= reverse_scaling;
      }
    }
    normalize2(local_scaling, local_reverse_scaling,
              local_scaling, local_reverse_scaling);
    // Calculate the local increase such that, for each i:
    // local_result[i] == local_scaling * working_cost[i]
    //                    + local_reverse_scaling * local_increase[i].
    // Local increase is stored in local_result to improve performance.
    for (dimension_type i = 0; i < local_result.size(); ++i) {
      local_result[i] -= local_scaling * working_cost[i];
      PPL_ASSERT(local_result[i] % local_reverse_scaling == 0);
      exact_div_assign(local_result[i], local_result[i], local_reverse_scaling);
    }

    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);
  }
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
