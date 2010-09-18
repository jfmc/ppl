/* Distributed_Sparse_Matrix class implementation: inline functions.
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

#ifndef PPL_Distributed_Sparse_Matrix_inlines_hh
#define PPL_Distributed_Sparse_Matrix_inlines_hh 1

// TODO: Remove this line. It was added to please KDevelop4.
#include "Distributed_Sparse_Matrix.defs.hh"

#include <boost/mpi/collectives.hpp>
#include <boost/mpi/operations.hpp>
#include <functional>

namespace Parma_Polyhedra_Library {

inline void
Distributed_Sparse_Matrix
::init_root(const boost::mpi::communicator& comm1) {
  PPL_ASSERT(comm_ptr == NULL);
  PPL_ASSERT(comm1.rank() == 0);
  comm_ptr = &comm1;
  comm_size = comm().size();
}

inline void
Distributed_Sparse_Matrix::quit_workers() {
  broadcast_operation(QUIT_OPERATION);
#ifndef NDEBUG
  comm_ptr = NULL;
#endif
}

inline
Distributed_Sparse_Matrix::Distributed_Sparse_Matrix()
  : my_num_columns(0), id(get_unique_id()), mapping(),
    reverse_mapping(comm_size), next_rank(0), local_rows(0), base(0) {
  PPL_ASSERT(comm().rank() == 0);
  PPL_ASSERT(OK());
}

inline
Distributed_Sparse_Matrix
::Distributed_Sparse_Matrix(dimension_type num_rows1,
                            dimension_type num_cols1) {
  PPL_ASSERT(comm().rank() == 0);

  init(num_rows1, num_cols1);
}

inline
Distributed_Sparse_Matrix
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

inline void
Distributed_Sparse_Matrix::swap(Distributed_Sparse_Matrix& matrix) {
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

inline bool
Distributed_Sparse_Matrix::operator!=(const Sparse_Matrix& matrix) const {
  return !(*this == matrix);
}

inline
Distributed_Sparse_Matrix::~Distributed_Sparse_Matrix() {
  PPL_ASSERT(comm_ptr != NULL);
  broadcast_operation(DELETE_MATRIX_OPERATION, id);

  // The rows stored at the root will be destroyed automatically.
}

inline dimension_type
Distributed_Sparse_Matrix::num_rows() const {
  return mapping.size();
}

inline dimension_type
Distributed_Sparse_Matrix::num_columns() const {
  return my_num_columns;
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code) {
  PPL_ASSERT(num_operation_params[code] == 0);

  Operation op;
  op.code = code;

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                               dimension_type param0) {
  PPL_ASSERT(num_operation_params[code] == 1);

  Operation op;
  op.code = code;
  op.params[0] = param0;

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                               dimension_type param0,
                                               dimension_type param1) {
  PPL_ASSERT(num_operation_params[code] == 2);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
                                               dimension_type param0,
                                               dimension_type param1,
                                               dimension_type param2) {
  PPL_ASSERT(num_operation_params[code] == 3);

  Operation op;
  op.code = code;
  op.params[0] = param0;
  op.params[1] = param1;
  op.params[2] = param2;

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
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

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::broadcast_operation(operation_code code,
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

  boost::mpi::broadcast(comm(), op, 0);
}

inline void
Distributed_Sparse_Matrix::get_row(dimension_type i, Sparse_Row& row) const {
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

inline void
Distributed_Sparse_Matrix::set_row(dimension_type i, const Sparse_Row& row) {
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

inline void
Distributed_Sparse_Matrix
::linear_combine_matrix(dimension_type row_index, dimension_type col_index,
                        Sparse_Row& combined_row) {
  std::pair<int, dimension_type>& row_info = mapping[row_index];
  int rank = row_info.first;
  dimension_type local_index = row_info.second;

  broadcast_operation(LINEAR_COMBINE_MATRIX_OPERATION, rank, id, local_index,
                      col_index);

  linear_combine_matrix__common(rank, local_index, col_index, 0, local_rows);

  if (rank == 0)
    combined_row = local_rows[local_index];
  else
    comm().recv(rank, 0, combined_row);
}

inline void
Distributed_Sparse_Matrix::add_row(Sparse_Row& row) {
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

inline void
Distributed_Sparse_Matrix
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

inline void
Distributed_Sparse_Matrix::make_inhomogeneous_terms_nonpositive() {
  broadcast_operation(MAKE_INHOMOGENEOUS_TERMS_NONPOSITIVE_OPERATION, id);

  make_inhomogeneous_terms_nonpositive__common(local_rows);
}

inline void
Distributed_Sparse_Matrix
::linear_combine_with_base_rows(dimension_type k) {
  int k_rank = mapping[k].first;
  dimension_type k_local_index = mapping[k].second;

  broadcast_operation(LINEAR_COMBINE_WITH_BASE_ROWS_OPERATION, id, k_rank,
                      k_local_index);

  linear_combine_with_base_rows__common(k_rank, k_local_index, 0, local_rows,
                                        base);
}

inline void
Distributed_Sparse_Matrix
::set_base_column(dimension_type row_index, dimension_type column_index) {
  int rank = mapping[row_index].first;
  dimension_type local_row_index = mapping[row_index].second;

  if (rank == 0) {
    base[local_row_index] = column_index;
    return;
  }

  broadcast_operation(SET_BASE_COLUMN_OPERATION, id, rank, local_row_index,
                      column_index);
}

inline void
Distributed_Sparse_Matrix::remove_row(dimension_type i) {
  // This guarantees that num_rows() != 0, too.
  PPL_ASSERT(i < num_rows());

  int rank_i = mapping[i].first;
  dimension_type local_index_i = mapping[i].second;

  int rank_last = mapping.back().first;
  dimension_type local_index_last = mapping.back().second;

  mapping.pop_back();
  // This check is needed, because if rank_last == 0, this is done in
  // remove_row__common().
  if (rank_last != 0)
    reverse_mapping[rank_last].pop_back();

  if (rank_i == 0 && rank_last == 0) {
    // Lucky case, no broadcasts needed.
    if (local_index_i != local_index_last) {
      std::swap(local_rows[local_index_i], local_rows[local_index_last]);
      std::swap(base[local_index_i], base[local_index_last]);
    }
    local_rows.pop_back();
    base.pop_back();
    reverse_mapping[0].pop_back();
    return;
  }

  broadcast_operation(REMOVE_ROW_OPERATION, id, rank_i, local_index_i,
                      rank_last, local_index_last);

  remove_row__common(comm(), 0, rank_i, local_index_i, rank_last, local_rows,
                     base, reverse_mapping[0]);

  PPL_ASSERT(OK());
}

inline bool
Distributed_Sparse_Matrix
::base_variables_occur_once(const std::vector<dimension_type>& base1) const {
  broadcast_operation(BASE_VARIABLES_OCCUR_ONCE_OPERATION, id);

  std::vector<dimension_type>& base_ref
    = const_cast<std::vector<dimension_type>&>(base1);
  boost::mpi::broadcast(comm(), base_ref, 0);

  bool local_result = base_variables_occur_once__common(local_rows,
                                                        reverse_mapping[0],
                                                        base1);

  bool global_result;
  boost::mpi::reduce(comm(), local_result, global_result, std::logical_and<bool>(),
              0);

  return global_result;
}

inline dimension_type
Distributed_Sparse_Matrix::get_unique_id() {
  static dimension_type next_id = 0;
  return next_id++;
}

inline const boost::mpi::communicator&
Distributed_Sparse_Matrix::comm() {
  PPL_ASSERT(comm_ptr != 0);
  return *comm_ptr;
}


inline const Distributed_Sparse_Matrix::Worker::Row_Chunk&
Distributed_Sparse_Matrix::Worker
::get_row_chunk(dimension_type id) const {
  static const Row_Chunk empty_row_chunk;

  std::tr1::unordered_map<dimension_type, Row_Chunk>::const_iterator itr
    = row_chunks.find(id);

  if (itr == row_chunks.end())
    return empty_row_chunk;
  else
    return itr->second;
}

} // namespace Parma_Polyhedra_Library

namespace std {

inline void
swap(Parma_Polyhedra_Library::Distributed_Sparse_Matrix& x,
     Parma_Polyhedra_Library::Distributed_Sparse_Matrix& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Distributed_Sparse_Matrix_inlines_hh)
