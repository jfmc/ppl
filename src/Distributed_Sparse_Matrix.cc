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
  4, // ADD_ROW_INTO_BASE_OPERATION: id, rank, row_index, column_index
  3, // REMOVE_ROW_FROM_BASE_OPERATION: id, rank, row_index
  1, // SET_BASE_OPERATION: id
  1, // GET_BASE_OPERATION: id
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
      worker.add_zero_rows(op.params[0], op.params[1], op.params[2],
                           op.params[3], op.params[4]);
      break;

    case ADD_ROW_OPERATION:
      worker.add_row(op.params[0], op.params[1], op.params[2]);
      break;

    case RESET_COLUMN_OPERATION:
      worker.reset_column(op.params[0], op.params[1]);
      break;

    case REMOVE_TRAILING_ROWS_OPERATION:
      worker.remove_trailing_rows(op.params[0], op.params[1]);
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

    case GET_COLUMN_OPERATION:
      worker.get_column(op.params[0], op.params[1]);
      break;

    case GET_SCATTERED_ROW_OPERATION:
      worker.get_scattered_row(op.params[0]);
      break;

    case FLOAT_ENTERING_INDEX_OPERATION:
      worker.float_entering_index(op.params[0]);
      break;

    case SET_ARTIFICIAL_INDEXES_FOR_NEW_ROWS_OPERATION:
      worker.set_artificial_indexes_for_new_rows(op.params[0]);
      break;

    case ADD_ROW_INTO_BASE_OPERATION:
      worker.add_row_into_base(op.params[0], op.params[1], op.params[2],
                               op.params[3]);
      break;

    case REMOVE_ROW_FROM_BASE_OPERATION:
      worker.remove_row_from_base(op.params[0], op.params[1], op.params[2]);
      break;

    case SET_BASE_OPERATION:
      worker.set_base(op.params[0]);
      break;

    case GET_BASE_OPERATION:
      worker.get_base(op.params[0]);
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
  : my_num_columns(0), id(get_unique_id()), mapping(),
    reverse_mapping(comm_size), next_rank(0), local_rows(0), base(0) {
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
    mapping(matrix.mapping),
    reverse_mapping(matrix.reverse_mapping),
    next_rank(matrix.next_rank),
    local_rows(matrix.local_rows),
    base(matrix.base) {

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

void
PPL::Distributed_Sparse_Matrix::swap(Distributed_Sparse_Matrix& matrix) {
  std::swap(id, matrix.id);
  std::swap(my_num_columns, matrix.my_num_columns);
  std::swap(mapping, matrix.mapping);
  std::swap(reverse_mapping, matrix.reverse_mapping);
  std::swap(next_rank, matrix.next_rank);
  std::swap(local_rows, matrix.local_rows);
  std::swap(base, matrix.base);
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
        std::cout << "Found mismatch in root node, at row " << i << std::endl;
        std::cout << "LHS: ";
        local_rows[local_i].ascii_dump(std::cout);
        std::cout << "RHS: ";
        matrix[i].ascii_dump(std::cout);
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
  return mapping.size();
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
                      std::pair<PPL::Coefficient, PPL::Dense_Row> >: public mpl::true_ { };

}
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost__common(std::pair<std::pair<Coefficient, Coefficient>,
                                         Sparse_Row>& x,
                               const Dense_Row& working_cost,
                               const std::vector<dimension_type>&
                                 reverse_mapping,
                               const std::vector<dimension_type>& base,
                               const std::vector<Sparse_Row>& local_rows) {
  Coefficient& local_scaling = x.first.first;
  Coefficient& local_reverse_scaling = x.first.second;
  Sparse_Row& local_increase = x.second;

  local_scaling = 1;
  local_reverse_scaling = 1;
  local_increase.resize(working_cost.size());

  for (dimension_type local_index = 0;
      local_index < reverse_mapping.size(); ++local_index) {
    Coefficient_traits::const_reference cost_i = working_cost[base[local_index]];
    if (cost_i != 0)
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
    rows[*i].find_create(current_artificial, Coefficient_one());
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
::float_entering_index__common(const std::vector<bool>& candidates,
                               const std::vector<dimension_type>& base,
                               const std::vector<Sparse_Row>& rows,
                               std::vector<double>& results) {
  const dimension_type num_rows = rows.size();
  const dimension_type num_columns_minus_1 = candidates.size();

  for (dimension_type i = num_rows; i-- > 0; ) {
    const Sparse_Row& row_i = rows[i];
    Coefficient_traits::const_reference tableau_i_base_i
      = row_i.get(base[i]);
    double float_tableau_denum;
    assign(float_tableau_denum, tableau_i_base_i);
    for (Sparse_Row::const_iterator
         j = row_i.begin(), j_end = row_i.end(); j != j_end; ++j) {
      if (j.index() >= num_columns_minus_1)
        break;
      if (!candidates[j.index()])
        continue;
      Coefficient_traits::const_reference tableau_ij = *j;
      WEIGHT_BEGIN();
      if (tableau_ij != 0) {
        PPL_ASSERT(tableau_i_base_i != 0);
        double float_tableau_value;
        assign(float_tableau_value, tableau_ij);
        float_tableau_value /= float_tableau_denum;
        float_tableau_value *= float_tableau_value;
        results[j.index()] += float_tableau_value;
      }
      WEIGHT_ADD_MUL(338, num_rows);
    }
  }
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
  const std::pair<int, dimension_type>& row_info = mapping[i];
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
  std::pair<int, dimension_type>& row_info = mapping[i];
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
  std::pair<int, dimension_type>& row_info = mapping[row_index];
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
PPL::Distributed_Sparse_Matrix::add_row(Sparse_Row& row) {
  PPL_ASSERT(row.size() == num_columns());
  int rank = next_rank;
  ++next_rank;
  if (next_rank == comm_size)
    next_rank = 0;
  dimension_type local_index = reverse_mapping[rank].size();
  dimension_type global_index = num_rows();
  reverse_mapping[rank].push_back(global_index);
  mapping.push_back(std::make_pair(rank, local_index));
  if (rank == 0) {
    local_rows.resize(local_rows.size() + 1);
    base.push_back(0);
    std::swap(local_rows.back(), row);
  } else {
    broadcast_operation(ADD_ROW_OPERATION, rank, id, global_index);
    comm().send(rank, 0, row);
  }
  PPL_ASSERT(OK());
}

void
PPL::Distributed_Sparse_Matrix
::swap_rows(dimension_type row_index1, dimension_type row_index2) {
  int rank1 = mapping[row_index1].first;
  dimension_type local_index1 = mapping[row_index1].second;
  int rank2 = mapping[row_index2].first;
  dimension_type local_index2 = mapping[row_index2].second;
  if (rank1 == 0 && rank2 == 0) {
    std::swap(local_rows[local_index1], local_rows[local_index2]);
    std::swap(base[local_index1], base[local_index2]);
    return;
  }
  broadcast_operation(SWAP_ROWS_OPERATION, id, rank1, local_index1, rank2,
                      local_index2);
  swap_rows__common(rank1, rank2, local_index1, local_index2, 0, local_rows,
                    base);
}

void
PPL::Distributed_Sparse_Matrix
::compute_working_cost(Dense_Row& working_cost) {
  PPL_ASSERT(working_cost.size() == num_columns());
  broadcast_operation(COMPUTE_WORKING_COST_OPERATION, id);
  mpi::broadcast(comm(), working_cost, 0);

  std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;
  compute_working_cost__common(x, working_cost, reverse_mapping[0],
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
::linear_combine_with_base_rows(dimension_type k) {
  int k_rank = mapping[k].first;
  dimension_type k_local_index = mapping[k].second;

  broadcast_operation(LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION, id, k_rank,
                      k_local_index);

  linear_combine_with_base_rows__common(k_rank, k_local_index, 0, local_rows,
                                        base);
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

PPL::dimension_type
PPL::Distributed_Sparse_Matrix
::float_entering_index(const Dense_Row& working_cost) const {
  broadcast_operation(FLOAT_ENTERING_INDEX_OPERATION, id);

  const dimension_type num_columns_minus_1 = num_columns() - 1;

  const int cost_sign = sgn(working_cost[working_cost.size() - 1]);

  // When candidate[i] is true, i is one of the column candidates.
  std::vector<bool> candidates(num_columns_minus_1);

  for (dimension_type column = 1; column < num_columns_minus_1; ++column)
    candidates[column] = (sgn(working_cost[column]) == cost_sign);

  mpi::broadcast(comm(), candidates, 0);

  std::vector<double> results(num_columns_minus_1, 1.0);

  float_entering_index__common(candidates, base, local_rows, results);

  std::vector<double> global_results;
  mpi::reduce(comm(), results, global_results,
              float_entering_index_reducer_functor(), 0);

  double current_value = 0.0;
  dimension_type entering_index = 0;

  for (dimension_type i = num_columns_minus_1; i-- > 1; )
    if (candidates[i]) {
      double challenger_value = sqrt(results[i]);
      if (entering_index == 0 || challenger_value > current_value) {
        current_value = challenger_value;
        entering_index = i;
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
    local_rows[i->first].find_create(i->second, Coefficient_one());
    base[i->first] = i->second;
  }
}

void
PPL::Distributed_Sparse_Matrix
::add_row_into_base(dimension_type row_index, dimension_type column_index) {
  PPL_ASSERT(column_index != 0);
  int rank = mapping[row_index].first;
  dimension_type local_row_index = mapping[row_index].second;

  if (rank == 0) {
    base[local_row_index] = column_index;
    return;
  }

  broadcast_operation(ADD_ROW_INTO_BASE_OPERATION, id, rank, local_row_index,
                      column_index);
}

void
PPL::Distributed_Sparse_Matrix
::remove_row_from_base(dimension_type row_index) {
  int rank = mapping[row_index].first;
  dimension_type local_row_index = mapping[row_index].second;

  if (rank == 0) {
    base[local_row_index] = 0;
    return;
  }

  broadcast_operation(REMOVE_ROW_FROM_BASE_OPERATION, id, rank, row_index);
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
  std::vector<dimension_type> correct_reverse_mapping;
  mpi::scatter(comm(), correct_reverse_mapping, 0);

  row_chunks_const_itr_type itr = row_chunks.find(id);
  bool result = true;
  if (itr == row_chunks.end()) {
    if (correct_reverse_mapping.size() != 0) {
      std::cerr << "Worker node: no rows found for this id." << std::endl;
      result = false;
    }
  } else {
    if (correct_reverse_mapping.size() != itr->second.rows.size()) {
      std::cerr << "Worker node: row check failed" << std::endl;
      result = false;
    }
    if (itr->second.base.size() != itr->second.rows.size()) {
      std::cerr << "Worker node: base[] has the wrong size." << std::endl;
      result = false;
    }
    if (correct_reverse_mapping != itr->second.reverse_mapping) {
      std::cerr << "Worker node: reverse_mapping check failed" << std::endl;
      std::cerr << "Correct reverse_mapping:" << std::endl;
      for (std::vector<dimension_type>::const_iterator
           i = correct_reverse_mapping.begin(),
           i_end = correct_reverse_mapping.end(); i != i_end; ++i)
        std::cerr << *i << ' ';
      std::cerr << std::endl;
      std::cerr << "Local reverse_mapping:" << std::endl;
      for (std::vector<dimension_type>::const_iterator
           i = itr->second.reverse_mapping.begin(),
           i_end = itr->second.reverse_mapping.end(); i != i_end; ++i)
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

  row_chunks_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;

  Row_Chunk& row_chunk = itr->second;
  std::vector<Sparse_Row>& rows = row_chunk.rows;
  std::vector<dimension_type>& base = row_chunk.base;
  std::vector<dimension_type>& reverse_mapping
    = row_chunk.reverse_mapping;

  while (!reverse_mapping.empty()
          && reverse_mapping.back() >= row_n) {
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
      std::cout << "Found mismatch in root node, at row "
                << itr->second.reverse_mapping[i] << std::endl;
      std::cout << "LHS: ";
      rows[i].ascii_dump(std::cout);
      std::cout << "RHS: ";
      received_rows[i].ascii_dump(std::cout);
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

  row_chunks_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {

    // Construct a dummy result object
    std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row>
      x(std::pair<Coefficient, Coefficient>(1, 1), Sparse_Row(working_cost.size()));

    // And use it in the reduce operation.
    mpi::reduce(comm(), x, compute_working_cost_reducer_functor(), 0);

  } else {
    Row_Chunk& row_chunk = itr->second;

    std::pair<std::pair<Coefficient, Coefficient>, Sparse_Row> x;

    compute_working_cost__common(x, working_cost,
                                 row_chunk.reverse_mapping, row_chunk.base,
                                 row_chunk.rows);

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
                                                     itr->second.rows,
                                                     itr->second.base);
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

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Distributed_Sparse_Matrix)

void
PPL::Distributed_Sparse_Matrix::Worker
::linear_combine_with_base_rows(dimension_type id, int k_rank,
                                dimension_type k_local_index) {

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

  linear_combine_with_base_rows__common(k_rank, k_local_index, my_rank,
                                        itr->second.rows, itr->second.base);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::get_column(dimension_type id, dimension_type column_index) const {

  row_chunks_const_itr_type itr = row_chunks.find(id);
  if (itr == row_chunks.end())
    return;

  const std::vector<Sparse_Row>& rows = itr->second.rows;

  std::vector<mpi::request> requests;
  requests.reserve(rows.size());

  for (dimension_type i = rows.size(); i-- > 0; ) {
    // TODO: This cast can be dangerous!
    int tag = static_cast<int>(i);
    requests.push_back(comm().isend(0, tag, rows[i].get(column_index)));
  }

  mpi::wait_all(requests.begin(), requests.end());
}

void
PPL::Distributed_Sparse_Matrix::Worker
::get_scattered_row(dimension_type id) const {

  // workunit is a vector of <local_row_index, column_index> pairs.
  // column_index is stored as an int, because it is used as a tag.
  std::vector<std::pair<dimension_type, int> > workunit;
  mpi::scatter(comm(), workunit, 0);

  row_chunks_const_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {
    PPL_ASSERT(workunit.empty());
    return;
  }

  const std::vector<Sparse_Row>& rows = itr->second.rows;

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

  std::vector<bool> candidates;
  mpi::broadcast(comm(), candidates, 0);

  row_chunks_const_itr_type itr = row_chunks.find(id);

  std::vector<double> results(candidates.size(), 0.0);

  if (itr != row_chunks.end()) {
    float_entering_index__common(candidates, itr->second.base,
                                 itr->second.rows, results);
  }

  mpi::reduce(comm(), results, float_entering_index_reducer_functor(), 0);
}

void
PPL::Distributed_Sparse_Matrix::Worker
::set_artificial_indexes_for_new_rows(dimension_type id) {

  std::vector<std::pair<dimension_type, dimension_type> > workunit;
  mpi::scatter(comm(), workunit, 0);

  row_chunks_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {
    PPL_ASSERT(workunit.empty());
    return;
  }

  std::vector<Sparse_Row>& rows = itr->second.rows;
  std::vector<dimension_type>& base = itr->second.base;

  for (std::vector<std::pair<dimension_type, dimension_type> >::const_iterator
       i = workunit.begin(), i_end = workunit.end(); i != i_end; ++i) {
    rows[i->first].find_create(i->second, Coefficient_one());
    base[i->first] = i->second;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::add_row_into_base(dimension_type id, int rank,
                    dimension_type local_row_index,
                    dimension_type column_index) {
  if (my_rank == rank) {
    PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
    row_chunks[id].base[local_row_index] = column_index;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker
::remove_row_from_base(dimension_type id, int rank,
                       dimension_type local_row_index) {
  if (my_rank == rank) {
    PPL_ASSERT(row_chunks.find(id) != row_chunks.end());
    row_chunks[id].base[local_row_index] = 0;
  }
}

void
PPL::Distributed_Sparse_Matrix::Worker::set_base(dimension_type id) {

  std::vector<dimension_type> new_base;
  mpi::scatter(comm(), new_base, 0);

  row_chunks_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {
    PPL_ASSERT(new_base.empty());
    return;
  }

  PPL_ASSERT(new_base.size() == itr->second.base.size());

  itr->second.base = new_base;
}

void
PPL::Distributed_Sparse_Matrix::Worker::get_base(dimension_type id) const {

  row_chunks_const_itr_type itr = row_chunks.find(id);

  if (itr == row_chunks.end()) {
    std::vector<dimension_type> fake_base;
    mpi::gather(comm(), fake_base, 0);
  } else
    mpi::gather(comm(), itr->second.base, 0);
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
