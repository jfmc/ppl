/* Distributed_Sparse_Matrix class declaration.
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

#ifndef PPL_Distributed_Sparse_Matrix_defs_hh
#define PPL_Distributed_Sparse_Matrix_defs_hh 1

#include "Distributed_Sparse_Matrix.types.hh"

#include <boost/mpi/communicator.hpp>
#include <tr1/unordered_map>

#include "Sparse_Row.defs.hh"
#include "Sparse_Matrix.types.hh"
#include "Coefficient.defs.hh"

class Parma_Polyhedra_Library::Distributed_Sparse_Matrix {
public:
  //! This must be called before creating any Distributed_Sparse_Matrix
  //! objects.
  /*!
    comm.rank() must be 0.
  */
  static void init_root(const boost::mpi::communicator& comm);

  //! This must be called before creating any Distributed_Sparse_Matrix
  //! objects.
  /*!
    This method returns only when quit_workers() is called at the root node.
    After that, no more matrices can be created.
    comm.rank() must not be 0.
  */
  static void worker_main_loop(const boost::mpi::communicator& comm);

  //! This must be called by the root node after all matrices have been
  //! destroyed, just before process termination.
  static void quit_workers();

  //! Constructs an empty Distributed_Sparse_Matrix.
  Distributed_Sparse_Matrix();

  Distributed_Sparse_Matrix(dimension_type num_rows,
                            dimension_type num_cols);

  Distributed_Sparse_Matrix(const Distributed_Sparse_Matrix& matrix);

  Distributed_Sparse_Matrix& operator=(const Sparse_Matrix& matrix);

  void swap(Distributed_Sparse_Matrix& matrix);

  bool operator==(const Sparse_Matrix& matrix) const;
  bool operator!=(const Sparse_Matrix& matrix) const;

  ~Distributed_Sparse_Matrix();

  dimension_type num_rows() const;
  dimension_type num_columns() const;

  void get_row(dimension_type i, Sparse_Row& row) const;
  void set_row(dimension_type i, const Sparse_Row& row);

  void linear_combine_matrix(dimension_type row_i, dimension_type col_i);
  //! Linear combines the row whose indexes are in row_indexes with the
  //! row_i-th row.
  /*!
    row_indexes must not contain duplicates nor row_i.
  */
  void linear_combine_some(const std::vector<dimension_type>& row_indexes,
                           dimension_type row_i, dimension_type col_i);

  void linear_combine_with(const Sparse_Row& row, dimension_type column_index);

  void reset_column(dimension_type column_index);
  void remove_column(dimension_type column_index);
  void remove_trailing_columns(dimension_type n);

  void remove_trailing_rows(dimension_type n);

  void add_zero_columns(dimension_type n);
  void add_zero_rows(dimension_type n, Row_Flags flags = Row_Flags());

  //! Adds \p row at the end of to the matrix.
  //! The value of \p row after the call is unspecified.
  void add_row(Sparse_Row& row);

  void swap_rows(dimension_type row_index1, dimension_type row_index2);

  bool OK() const;

private:
  void init(dimension_type num_rows, dimension_type num_columns);

  static dimension_type get_unique_id();
  static const boost::mpi::communicator& comm();

  //! Maps global row indexes into local row indexes, grouping them by rank.
  /*!
    For each global row index indexes[i], stored at rank `rank' and with local
    row index `local_index', local_index will be an element of
    local_indexes[rank].

    local_indexes is used as an
    std::map< dimension_type, std::set< dimension_type > >.

    When this method is called, local_indexes must be empty.
    When this method returns, local_indexes will have size comm.size().
  */
  void map_indexes(const std::vector<dimension_type>& indexes,
                   std::vector<std::vector<dimension_type> >& local_indexes)
                   const;

  class Worker {
  public:
    Worker();

    void create_matrix(dimension_type id, dimension_type num_cols);
    void copy_matrix(dimension_type source_id, dimension_type id);
    void delete_matrix(dimension_type id);
    void get_row(int rank) const;
    void set_row(int rank);
    void linear_combine_matrix(int rank, dimension_type id,
                               dimension_type col_index);
    void reset_column(dimension_type id, dimension_type column_index);
    void remove_column(dimension_type id, dimension_type col_index);
    void remove_trailing_columns(dimension_type id, dimension_type col_index);
    void remove_trailing_rows(dimension_type id);
    void add_zero_columns(dimension_type id, dimension_type n);
    void check(dimension_type id, dimension_type num_columns) const;
    void add_zero_rows(dimension_type id, dimension_type num_columns,
                       dimension_type flag_bits);
    void add_row(int rank);
    void linear_combine_some(dimension_type id, int rank,
                             dimension_type column_index);
    void linear_combine_with(dimension_type id, dimension_type column_index);
    void swap_rows(dimension_type id, int rank1, dimension_type local_index1,
                   int rank2, dimension_type local_index2);
    void fill_matrix(dimension_type id);
    void compare_with_sparse_matrix(dimension_type id);

  private:
    // Every node has an associated Node_Data, including the root node.
    // This declaration refers to worker nodes only.
    std::tr1::unordered_map<dimension_type, std::vector<Sparse_Row> > row_chunks;
    typedef std::tr1::unordered_map<dimension_type,
                                    std::vector<Sparse_Row> >::iterator
            row_chunks_itr_type;
    typedef std::tr1::unordered_map<dimension_type,
                                    std::vector<Sparse_Row> >::const_iterator
            row_chunks_const_itr_type;
    const int my_rank;
  };

  // WARNING: when the following enum is modified, num_operation_params[]
  // must be updated accordingly.
  enum operation_code {
    //! Parameters: (none).
    QUIT_OPERATION,
    //! Parameters: id, num_cols.
    CREATE_MATRIX_OPERATION,
    //! Parameters: source_id, id.
    COPY_MATRIX_OPERATION,
    //! Parameters: id.
    DELETE_MATRIX_OPERATION,
    //! Parameters: rank.
    GET_ROW_OPERATION,
    //! Parameters: rank.
    SET_ROW_OPERATION,
    //! Parameters: rank, id, col_index.
    LINEAR_COMBINE_OPERATION,
    //! Parameters: id, col_index
    REMOVE_COLUMN_OPERATION,
    //! Parameters: id, col_index
    REMOVE_TRAILING_COLUMNS_OPERATION,
    //! Parameters: id, n
    ADD_ZERO_COLUMNS_OPERATION,
    //! Parameters: id, num_columns
    CHECK_OPERATION,
    //! Parameters: id, num_columns, flag_bits
    ADD_ZERO_ROWS_OPERATION,
    //! Parameters: rank
    ADD_ROW_OPERATION,
    //! Parameters: id, rank, column_index
    LINEAR_COMBINE_SOME_OPERATION,
    //! Parameters: id, column_index
    LINEAR_COMBINE_WITH_OPERATION,
    //! Parameters: id, column_index
    RESET_COLUMN_OPERATION,
    //! Parameters: id
    REMOVE_TRAILING_ROWS_OPERATION,
    //! Parameters: id, rank1, local_index1, rank2, local_index2
    SWAP_ROWS_OPERATION,
    //! Parameters: id
    FILL_MATRIX_OPERATION,
    //! Parameters: id
    COMPARE_WITH_SPARSE_MATRIX_OPERATION,
  };

  // This associates to each operation code the number of dimension_type
  // parameters that will be broadcasted together with the operation code.
  static const dimension_type num_operation_params[];

  static void broadcast_operation(operation_code code);
  static void broadcast_operation(operation_code code, dimension_type param0);
  static void broadcast_operation(operation_code code, dimension_type param0,
                                  dimension_type param1);
  static void broadcast_operation(operation_code code, dimension_type param0,
                                  dimension_type param1, dimension_type param2);
  static void broadcast_operation(operation_code code, dimension_type param0,
                                  dimension_type param1, dimension_type param2,
                                  dimension_type param3);
  static void broadcast_operation(operation_code code, dimension_type param0,
                                  dimension_type param1, dimension_type param2,
                                  dimension_type param3, dimension_type param4);

  struct Operation {
    template <typename Archive>
    void serialize(Archive& archive, const unsigned int version);

    operation_code code;
    dimension_type params[5];
  };

  static const boost::mpi::communicator* comm_ptr;
  static int comm_size;

  dimension_type my_num_columns;
  dimension_type id;

  //! Maps the rows to the rank that stores them and its local index.
  /*!
    For each row index \p i, row_mapping[i] is a pair < rank, local_index >.
    row_mapping.size() is the total number of rows.
  */
  std::vector<std::pair<int, dimension_type> > row_mapping;

  //! Maps the local row indexes to the matrix's row indexes.
  /*!
    reverse_row_mapping[rank][local_index] is the global row index.
    reverse_row_mapping[rank].size() is the number of rows stored in that node.

    This could be computed from row_mapping, but it is stored to increase
    performance.
  */
  std::vector<std::vector<dimension_type> > reverse_row_mapping;

  //! The rank that will be used to insert the next row.
  //! This cycles through all ranks, to distribute rows evenly among nodes.
  int next_rank;

  //! The rows that are stored at the root.
  std::vector<Sparse_Row> local_rows;
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::Distributed_Sparse_Matrix */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Distributed_Sparse_Matrix& x,
          Parma_Polyhedra_Library::Distributed_Sparse_Matrix& y);

} // namespace std

#endif // !defined(PPL_Distributed_Sparse_Matrix_defs_hh)
