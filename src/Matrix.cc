/* Matrix class implementation (non-inline functions).
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Integer.defs.hh"
#include "Row.defs.hh"
#include "globals.hh"
#include "SatMatrix.defs.hh"
#include <algorithm>
#include <iostream>
#include <string>
#include <deque>

namespace PPL = Parma_Polyhedra_Library;

PPL::dimension_type
PPL::Matrix::num_lines_or_equalities() const {
  assert(num_pending_rows() == 0);
  dimension_type n = 0;
  for (dimension_type i = num_rows(); i != 0; )
    if (rows[--i].is_line_or_equality())
      ++n;
  return n;
}


PPL::Matrix::Matrix(Topology topol,
		    dimension_type n_rows, dimension_type n_columns)
  : rows(n_rows),
    row_topology(topol),
    row_size(n_columns),
    row_capacity(compute_capacity(n_columns)),
    index_first_pending(n_rows),
    sorted(true) {
  // Build the appropriate row type.
  Row::Type row_type(topol, Row::RAY_OR_POINT_OR_INEQUALITY);
  // Construct in direct order: will destroy in reverse order.
  for (dimension_type i = 0; i < n_rows; ++i)
    rows[i].construct(row_type, n_columns, row_capacity);
}

PPL::Matrix::Matrix(const Matrix& y)
  : rows(y.rows),
    row_topology(y.row_topology),
    row_size(y.row_size),
    row_capacity(compute_capacity(y.row_size)),
    index_first_pending(y.index_first_pending),
    sorted(y.sorted) {
}

PPL::Matrix::Matrix(Matrix& y, dimension_type first_stolen)
  : rows(y.num_rows() - first_stolen),
    row_topology(y.row_topology),
    row_size(y.row_size),
    row_capacity(y.row_capacity),
    index_first_pending(rows.size()),
    sorted(false) {
  assert(first_stolen < y.num_rows());
  // Steal the rows from `y', starting from `first_stolen'.
  for (dimension_type i = num_rows(); i-- > 0; )
    std::swap(rows[i], y.rows[first_stolen + i]);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
  // Erase from `y' the rows just swapped in from `*this'.
  y.erase_to_end(first_stolen);
  // Adjust the index of the first pending row, if needed.
  if (y.first_pending_row() > first_stolen)
    y.set_index_first_pending_row(first_stolen);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(y.OK(false));
}

PPL::Matrix&
PPL::Matrix::operator=(const Matrix& y) {
  // Without the following guard against auto-assignments we would
  // recompute the row capacity based on row size, possibly without
  // actually increasing the capacity of the rows.  This would lead to
  // an inconsistent state.
  if (this != &y) {
    // The following assignment may do nothing on auto-assignments...
    rows = y.rows;
    row_topology = y.row_topology;
    row_size = y.row_size;
    // ... hence the following assignment must not be done on
    // auto-assignments.
    row_capacity = compute_capacity(y.row_size);
    index_first_pending = y.index_first_pending;
    sorted = y.sorted;
  }
  return *this;
}

void
PPL::Matrix::set_rows_topology() {
  if (is_necessarily_closed())
    for (dimension_type i = num_rows(); i-- > 0; )
      rows[i].set_necessarily_closed();
  else
    for (dimension_type i = num_rows(); i-- > 0; )
      rows[i].set_not_necessarily_closed();
}


void
PPL::Matrix::grow(dimension_type new_n_rows, dimension_type new_n_columns) {
  dimension_type old_n_rows = rows.size();

  assert(new_n_rows >= old_n_rows);
  assert(new_n_columns >= row_size);

  // Note that, if we have `new_n_rows == old_n_rows', the matrix
  // will keep its sortedness.
  // This is obvious if `new_n_columns == row_size'.
  // If `new_n_columns > row_size', then sortedness is maintained
  // because trailing zeroes will be added to all rows.
  bool was_sorted = is_sorted();

  if (new_n_rows > old_n_rows) {
    if (new_n_columns <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_n_rows) {
	// Reallocation will take place.
	std::vector<Row> new_rows;
	new_rows.reserve(compute_capacity(new_n_rows));
	new_rows.insert(new_rows.end(), new_n_rows, Row());
	// Construct the new rows.
	Row::Type row_type(row_topology, Row::RAY_OR_POINT_OR_INEQUALITY);
	dimension_type i = new_n_rows;
	while (i-- > old_n_rows)
	  new_rows[i].construct(row_type, new_n_columns, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation will NOT take place.
	rows.insert(rows.end(), new_n_rows - old_n_rows, Row());
	Row::Type row_type(row_topology, Row::RAY_OR_POINT_OR_INEQUALITY);
	for (dimension_type i = new_n_rows; i-- > old_n_rows; )
	  rows[i].construct(row_type, new_n_columns, row_capacity);
      }
    }
    else {
      // We cannot even recycle the old rows.
      Matrix new_matrix(row_topology);
      new_matrix.rows.reserve(compute_capacity(new_n_rows));
      new_matrix.rows.insert(new_matrix.rows.end(), new_n_rows, Row());
      // Construct the new rows.
      new_matrix.row_size = new_n_columns;
      new_matrix.row_capacity = compute_capacity(new_n_columns);
      dimension_type i = new_n_rows;
      Row::Type row_type(row_topology, Row::RAY_OR_POINT_OR_INEQUALITY);
      while (i-- > old_n_rows)
	new_matrix.rows[i].construct(row_type,
				     new_matrix.row_size,
				     new_matrix.row_capacity);
      // Copy the old rows.
      ++i;
      while (i-- > 0) {
	Row new_row(rows[i],
		    new_matrix.row_size,
		    new_matrix.row_capacity);
	std::swap(new_matrix.rows[i], new_row);
      }
      // We have the same number of pending rows as before.
      new_matrix.set_index_first_pending_row(index_first_pending);
      // Rows have been added: see if the matrix is known to be sorted.
      new_matrix.set_sorted(old_n_rows == 0
			    || (was_sorted
				&& (new_matrix[old_n_rows-1]
				    <= new_matrix[old_n_rows])));
      // Put the new vector into place.
      swap(new_matrix);
      return;
    }
  }
  // Here we have the right number of rows.
  if (new_n_columns > row_size) {
    // We need more columns.
    if (new_n_columns <= row_capacity)
      // But we have enough capacity: we resize existing rows.
      for (dimension_type i = old_n_rows; i-- > 0; )
	rows[i].grow_no_copy(new_n_columns);
    else {
      // Capacity exhausted: we must reallocate the rows and
      // make sure all the rows have the same capacity.
      dimension_type new_row_capacity = compute_capacity(new_n_columns);
      for (dimension_type i = old_n_rows; i-- > 0; ) {
	Row new_row(rows[i], new_n_columns, new_row_capacity);
	std::swap(rows[i], new_row);
      }
      row_capacity = new_row_capacity;
    }
    // Rows have grown or shrunk.
    row_size = new_n_columns;
  }
  // If rows have been added, we should check if we are still sorted.
  if (old_n_rows == 0)
    // The matrix was empty: now it is sorted.
    set_sorted(true);
  else if (new_n_rows > old_n_rows)
    // Rows were added.
    if (was_sorted)
      set_sorted((*this)[old_n_rows-1] <= (*this)[old_n_rows]);
  // If no rows was added the matrix keeps its sortedness.
}

void
PPL::Matrix::resize_no_copy(dimension_type new_n_rows,
			    dimension_type new_n_columns) {
  dimension_type old_n_rows = rows.size();
  // Note that, if we have `new_n_rows <= old_n_rows' and
  // `new_n_columns >= row_size', the matrix will keep its sortedness.
  // This is obvious if `new_n_columns == row_size'.
  // If `new_n_columns > row_size', then sortedness is maintained
  // because trailing zeroes will be added to all rows.
  if (new_n_rows > old_n_rows) {
    if (new_n_columns <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_n_rows) {
	// Reallocation (of vector `rows') will take place.
	std::vector<Row> new_rows;
	new_rows.reserve(compute_capacity(new_n_rows));
	new_rows.insert(new_rows.end(), new_n_rows, Row());
	// Construct the new rows (be careful: each new row must have
	// the same capacity as each one of the old rows).
	Row::Type row_type(row_topology, Row::LINE_OR_EQUALITY);
	dimension_type i = new_n_rows;
	while (i-- > old_n_rows)
	  new_rows[i].construct(row_type, new_n_columns, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation (of vector `rows') will NOT take place.
	rows.insert(rows.end(), new_n_rows - old_n_rows, Row());
	Row::Type row_type(row_topology, Row::LINE_OR_EQUALITY);
	// Be careful: each new row must have
	// the same capacity as each one of the old rows.
	for (dimension_type i = new_n_rows; i-- > old_n_rows; )
	  rows[i].construct(row_type, new_n_columns, row_capacity);
      }
      // Even though `*this' may happen to keep its sortedness,
      // we believe that checking such a property is not worth the effort.
      // Moreover, it is very likely that the matrix will be overwritten
      // as soon as we return.
      set_sorted(false);
    }
    else {
      // We cannot even recycle the old rows: allocate a new matrix and swap.
      Matrix new_matrix(row_topology, new_n_rows, new_n_columns);
      swap(new_matrix);
      return;
    }
  }
  else if (new_n_rows < old_n_rows) {
    // Drop some rows.
    rows.erase(rows.begin() + new_n_rows, rows.end());
    old_n_rows = new_n_rows;
  }
  // Here we have the right number of rows.
  if (new_n_columns != row_size) {
    if (new_n_columns < row_size) {
      // Shrink the existing rows.
      for (dimension_type i = old_n_rows; i-- > 0; )
	rows[i].shrink(new_n_columns);
      // Ditto.
      set_sorted(false);
    }
    else
      // We need more columns.
      if (new_n_columns <= row_capacity)
	// But we have enough capacity: we resize existing rows.
	for (dimension_type i = old_n_rows; i-- > 0; )
	  rows[i].grow_no_copy(new_n_columns);
      else {
	// Capacity exhausted: we must reallocate the rows and
	// make sure all the rows have the same capacity.
	dimension_type new_row_capacity = compute_capacity(new_n_columns);
	Row::Type row_type(row_topology, Row::LINE_OR_EQUALITY);
	for (dimension_type i = old_n_rows; i-- > 0; ) {
	  Row new_row(row_type, new_n_columns, new_row_capacity);
	  std::swap(rows[i], new_row);
	}
	row_capacity = new_row_capacity;
      }
    // Rows have grown or shrunk.
    row_size = new_n_columns;
  }
}

void
PPL::Matrix::ascii_dump(std::ostream& s) const {
  using std::endl;

  const Matrix& x = *this;
  const char separator = ' ';
  s << "topology" << separator
    << (x.is_necessarily_closed() ? "" : "NOT_")
    << "NECESSARILY_CLOSED"
    << endl;
  s << x.num_rows() << separator << 'x' << separator
    << x.num_columns() << separator
    << (x.sorted ? "(sorted)" : "(not_sorted)")
    << endl
    << "index_first_pending " << x.first_pending_row()
    << endl;
}

bool
PPL::Matrix::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;
  if (str == "NECESSARILY_CLOSED")
    set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
      return false;
  resize_no_copy(nrows, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  set_sorted(str == "(sorted)");
  dimension_type index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> index))
    return false;
  set_index_first_pending_row(index);  
  // Check for well-formedness.
  assert(OK(true));
  return true;
}

void
PPL::Matrix::merge_rows_assign(const Matrix& y) {
  assert(row_size >= y.row_size);
  assert(check_sorted() && y.check_sorted());
  // We can use this method only when the matrices do not
  // contain any pending rows.
  assert(num_pending_rows() == 0 && y.num_pending_rows() == 0);

  Matrix& x = *this;

  // A temporary vector of rows...
  std::vector<Row> tmp;
  // ... with enough capacity not to require any reallocations.
  tmp.reserve(compute_capacity(x.num_rows() + y.num_rows()));

  std::vector<Row>::iterator xi = x.rows.begin();
  std::vector<Row>::iterator xend = x.rows.end();
  std::vector<Row>::const_iterator yi = y.rows.begin();
  std::vector<Row>::const_iterator yend = y.rows.end();

  while (xi != xend && yi != yend) {
    int comp = compare(*xi, *yi);
    if (comp <= 0) {
      // Elements that can be taken from `x' are actually _stolen_ from `x'
      std::swap(*xi++, *tmp.insert(tmp.end(), Row()));
      if (comp == 0)
	// A duplicate element.
	++yi;
    }
    else {
      // (comp > 0)
      Row copy(*yi++, row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Row()));
    }
  }
  // Insert what is left.
  if (xi != xend)
    while (xi != xend)
      std::swap(*xi++, *tmp.insert(tmp.end(), Row()));
  else
    while (yi != yend) {
      Row copy(*yi++, row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Row()));
    }

  // We get the result vector and let the old one be destroyed.
  std::swap(tmp, rows);
  // There are no pending rows.
  unset_pending_rows();
  assert(check_sorted());
}

void
PPL::Matrix::add_pending_rows(const Matrix& y) {
  Matrix& x = *this;
  assert(x.row_size >= y.row_size);

  dimension_type x_n_rows = x.num_rows();
  dimension_type y_n_rows = y.num_rows();
  // Grow to the required size without changing sortedness.
  bool was_sorted = sorted;
  grow(x_n_rows + y_n_rows, x.row_size);
  sorted = was_sorted;

  // Copy the rows of `y', forcing size and capacity.
  for (dimension_type i = y_n_rows; i-- > 0; ) {
    Row copy(y[i], x.row_size, x.row_capacity);
    std::swap(copy, x[x_n_rows+i]);
  }
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::add_rows(const Matrix& y) {
  assert(num_pending_rows() == 0);

  // Adding no rows is a no-op.
  if (y.num_rows() == 0)
    return;

  // Check if sortedness is preserved.
  if (is_sorted())
    if (y.is_sorted() && y.num_pending_rows() == 0) {
    dimension_type n_rows = num_rows();
    if (n_rows > 0)
      set_sorted((*this)[n_rows-1] <= y[0]);
  }

  // Add the rows of `y' as if they were pending.
  add_pending_rows(y);
  // There are no pending_rows.
  unset_pending_rows();

  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::sort_rows() {
  dimension_type num_pending = num_pending_rows();
  // We sort the non-pending rows only.
  sort_rows(0, first_pending_row());
  set_index_first_pending_row(num_rows() - num_pending);
  sorted = true;
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::sort_rows(dimension_type start, dimension_type end) {
  assert(start <= end && end <= num_rows());
  // We cannot mix pending and non-pending rows.
  assert(start >= first_pending_row() || end <= first_pending_row());
  Matrix& x = *this;
  // Sorting one or no rows is a no-op.
  if (start >= end - 1)
    return;
  dimension_type old_end = end;
  Row x_i;
  for (dimension_type i = start + 1; i < end; ) {
    x_i.assign(x[i]);
    dimension_type j;
    int cmp = 1;
    for (j = i; j > start; --j) {
      cmp = compare(x[j-1], x_i);
      if (cmp <= 0)
	break;
      x[j].assign(x[j-1]);
    }
    if (cmp == 0) {
      for ( ; j < i; ++j)
	x[j].assign(x[j+1]);
      x[i].assign(x_i);
      --end;
      std::swap(x[i], x[end]);
    }
    else {
      x[j].assign(x_i);
      ++i;
    }
  }
  Row null;
  x_i.assign(null);
  // The rows that we must erase are before `old_end'.
  rows.erase(rows.begin() + end, rows.begin() + old_end);
  // NOTE: we cannot check for well-formedness of the matrix here,
  // because the caller still has to update `index_first_pending'.
}

void
PPL::Matrix::sort_pending_and_remove_duplicates() {
  assert(num_pending_rows() > 0);
  assert(is_sorted());
  Matrix& x = *this;

  // The non-pending part of the matrix is already sorted.
  // Now sorting the pending part..
  dimension_type first_pending = x.first_pending_row(); 
  x.sort_rows(first_pending, x.num_rows());
  // Recompute the number of rows, because we may have removed
  // some rows occurring more than once in the pending part.
  dimension_type num_rows = x.num_rows();

  dimension_type k1 = 0;
  dimension_type k2 = first_pending;
  dimension_type num_duplicates = 0;
  // In order to erase them, put at the end of the matrix
  // those pending rows that also occur in the non-pending part.
  while (k1 < first_pending && k2 < num_rows) {
    int cmp = compare(x[k1], x[k2]);
    if (cmp == 0) {
      // We found the same row.
      ++num_duplicates;
      --num_rows;
      // By initial sortedness, we can increment index `k1'.
      ++k1;
      // Do not increment `k2'; instead, swap there the next pending row.
      if (k2 < num_rows)
	std::swap(x[k2], x[k2 + num_duplicates]);
    }
    else if (cmp < 0)
      // By initial sortedness, we can increment `k1'.
      ++k1;
    else {
      // Here `cmp > 0'.
      // Increment `k2' and, if we already found any duplicate,
      // swap the next pending row in position `k2'.
      ++k2;
      if (num_duplicates > 0 && k2 < num_rows)
	std::swap(x[k2], x[k2 + num_duplicates]);
    }
  }
  // If needed, swap any duplicates found past the pending rows
  // that has not been considered yet; then erase the duplicates.
  if (num_duplicates > 0) {
    if (k2 < num_rows)
      for (++k2; k2 < num_rows; ++k2)
	std::swap(x[k2], x[k2 + num_duplicates]);
    x.erase_to_end(num_rows);
  }
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::add_row(const Row& row) {
  // The added row must be strongly normalized and have
  // the same number of elements of the existing rows of the matrix.
  assert(row.check_strong_normalized());
  assert(row.size() == row_size);
  // This method is only used when the matrix has no pending rows.
  assert(num_pending_rows() == 0);
  bool was_sorted = is_sorted();
  dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(row, row_capacity);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new rows into place.
    std::swap(rows, new_rows);
  }
  else {
    // Reallocation will NOT take place.
    // Inserts a new empty row at the end,
    // then substitutes it with a copy of the given row.
    Row tmp(row, row_capacity);
    std::swap(*rows.insert(rows.end(), Row()), tmp);
  }

  //  We update `index_first_pending', because it must
  // equal to `num_rows()'.
  set_index_first_pending_row(num_rows());

  if (was_sorted) {
    dimension_type nrows = num_rows();
    // The added row may have caused the matrix to be not sorted anymore.
    if (nrows > 1) {
      // If the matrix is not empty and the inserted row
      // is the greatest one, the matrix is set to be sorted.
      // If it is not the greatest one then the matrix is no longer sorted.
      Matrix& x = *this;
      set_sorted(x[nrows-2] <= x[nrows-1]);
    }
    else
      // A matrix having only one row is sorted.
      set_sorted(true);
  }
  // The added row was not a pending row.
  assert(num_pending_rows() == 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::add_pending_row(const Row& row) {
  // The added row must be strongly normalized and have
  // the same number of elements of the existing rows of the matrix.
  assert(row.check_strong_normalized());
  assert(row.size() == row_size);

  dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(row, row_capacity);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new rows into place.
    std::swap(rows, new_rows);
  }
  else {
    // Reallocation will NOT take place.
    // Inserts a new empty row at the end,
    // then substitutes it with a copy of the given row.
    Row tmp(row, row_capacity);
    std::swap(*rows.insert(rows.end(), Row()), tmp);
  }

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::insert(const Row& row) {
  // The added row must be strongly normalized and have
  // the same topology of the matrix.
  assert(row.check_strong_normalized());
  assert(topology() == row.topology());
  // This method is only used when the matrix has no pending rows.
  assert(num_pending_rows() == 0);

  dimension_type old_num_rows = num_rows();

  // Resize the matrix, if necessary.
  if (row.size() > row_size) {
    if (is_necessarily_closed() || old_num_rows == 0)
      grow(old_num_rows, row.size());
    else {
      // After resizing, move the epsilon coefficients to
      // the last column (note: sorting is preserved).
      dimension_type old_eps_index = row_size - 1;
      grow(old_num_rows, row.size());
      swap_columns(old_eps_index, row_size - 1);
    }
    add_row(row);
  }
  else if (row.size() < row_size)
    if (is_necessarily_closed() || old_num_rows == 0)
      add_row(Row(row, row_size, row_capacity));
    else {
      // Create a resized copy of the row (and move the epsilon
      // coefficient to its last position).
      Row tmp_row = Row(row, row_size, row_capacity);
      std::swap(tmp_row[row.size() - 1], tmp_row[row_size - 1]);
      add_row(tmp_row);
    }
  else
    // Here row.size() == row_size.
    add_row(row);

  // The added row was not a pending row.
  assert(num_pending_rows() == 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::insert_pending(const Row& row) {
  // The added row must be strongly normalized and have
  // the same topology of the matrix.
  assert(row.check_strong_normalized());
  assert(topology() == row.topology());

  dimension_type old_num_rows = num_rows();

  // Resize the matrix, if necessary.
  if (row.size() > row_size) {
    if (is_necessarily_closed() || old_num_rows == 0)
      grow(old_num_rows, row.size());
    else {
      // After resizing, move the epsilon coefficients to
      // the last column (note: sorting is preserved).
      dimension_type old_eps_index = row_size - 1;
      grow(old_num_rows, row.size());
      swap_columns(old_eps_index, row_size - 1);
    }
    add_pending_row(row);
  }
  else if (row.size() < row_size)
    if (is_necessarily_closed() || old_num_rows == 0)
      add_pending_row(Row(row, row_size, row_capacity));
    else {
      // Create a resized copy of the row (and move the epsilon
      // coefficient to its last position).
      Row tmp_row = Row(row, row_size, row_capacity);
      std::swap(tmp_row[row.size() - 1], tmp_row[row_size - 1]);
      add_pending_row(tmp_row);
    }
  else
    // Here row.size() == row_size.
    add_pending_row(row);

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Matrix::add_pending_row(Row::Type type) {
  dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(type, row_size, row_capacity);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new vector into place.
    std::swap(rows, new_rows);
  }
  else
    // Reallocation will NOT take place.
    // Insert a new empty row at the end,
    // then construct it assigning it the given type.
    rows.insert(rows.end(), Row())->construct(type, row_size, row_capacity);

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
}

void
PPL::Matrix::swap_columns(dimension_type i,  dimension_type j) {
  assert(i != j && i < num_columns() && j < num_columns());
  for (dimension_type k = num_rows(); k-- > 0; )
    std::swap(rows[k][i], rows[k][j]);
}

void
PPL::Matrix::normalize() {
  // We normalize also the pending rows.
  for (dimension_type i = num_rows(); i-- > 0; )
    rows[i].normalize();
  set_sorted(false);
}

void
PPL::Matrix::strong_normalize() {
  // We strongly normalize also the pending rows.
  for (dimension_type i = num_rows(); i-- > 0; )
    rows[i].strong_normalize();
  set_sorted(false);
}

/*! \relates Parma_Polyhedra_Library::Matrix */
bool
PPL::operator==(const Matrix& x, const Matrix& y) {
  if (x.num_columns() != y.num_columns())
    return false;
  dimension_type x_num_rows = x.num_rows();
  if (x_num_rows != y.num_rows())
    return false;
  if (x.first_pending_row() != y.first_pending_row())
    return false;
  for (dimension_type i = x_num_rows; i-- > 0; )
    if (compare(x[i], y[i]) != 0)
      return false;
  return true;
}

void
PPL::Matrix::sort_and_remove_with_sat(SatMatrix& sat) {
  Matrix& x = *this;
  // We can only sort the non-pending part of the matrix.
  dimension_type num_kept_rows = x.first_pending_row();
  assert(num_kept_rows == sat.num_rows());
  if (num_kept_rows <= 1) {
    set_sorted(true);
    return;
  }
  for (dimension_type i = 0; i < num_kept_rows - 1; ++i) {
    for (dimension_type j = num_kept_rows - 1 ; j > i ; --j) {
      int cmp = compare(x[j], x[j - 1]);
      if (cmp == 0) {
	// If the compared rows are equals, we move the one with
	// the greatest index (and the corresponding row of
	// the saturation matrix) to the bottom of the matrix.
	// Now the number of row is one less.
	--num_kept_rows;
	std::swap(x[j], x[num_kept_rows]);
	std::swap(sat[j], sat[num_kept_rows]);
      }
      else if (cmp < 0) {
	// If `x[j]' is less than `x[j - 1]' we swap the two rows
	// and the corresponding rows of the saturation matrix.
	std::swap(x[j], x[j - 1]);
	std::swap(sat[j], sat[j - 1]);
      }
    }
  }

  if (num_pending_rows() > 0) {
    // In this case, we must put the rows to erase after the 
    // pending rows.
    dimension_type num_rows_to_erase = x.first_pending_row() - num_kept_rows;
    dimension_type n_rows = num_rows() - 1;
    for (dimension_type i = 0; i < num_rows_to_erase; ++i)
      std::swap(x[num_kept_rows + i], x[n_rows - i]);
  }
  // Erasing the duplicated rows...
  x.erase_to_end(num_kept_rows + num_pending_rows());
  x.set_index_first_pending_row(num_kept_rows);
  // ... and the corresponding rows of the saturation matrix.
  sat.rows_erase_to_end(num_kept_rows);
  assert(check_sorted());
  // Now the matrix is sorted.
  x.set_sorted(true);
}

void
PPL::Matrix::gram_shmidt() {
  assert(num_pending_rows() == 0);

  // The first part of this algorithm is is an adaptation of the one
  // proposed in a 1996 TR by Erlingsson, Kaltofen, and Musser
  // "Generic Gram-Shmidt Orthogonalization by Exact Division".

  // It is assumed that the lines/equalities come first in the matrix,
  // which contains no redundant row.

  dimension_type rank = num_lines_or_equalities();
  if (rank == 0)
    return;

#if 0
  std::cout << "+++ Before Gram-Shmidt +++" << std::endl;
  ascii_dump(std::cout);
#endif

  static std::vector< std::vector<Integer> > mu;
  mu.reserve(compute_capacity(rank));
  for (dimension_type i = mu.size(); i < rank; i++) {
    std::vector<Integer> mu_i(i+1);
    mu.push_back(mu_i);
  }

  // Compute the scalar products `rows[i]*rows[j]',
  // for all 0 <= j <= i < rank, storing them into `mu[i][j]'.
  for (dimension_type i = rank; i-- > 0; ) {
    Row& row_i = rows[i];
    std::vector<Integer>& mu_i = mu[i];
    for (dimension_type j = i+1; j-- > 0; )
      mu_i[j] = row_i * rows[j];
  }

  dimension_type n_columns = num_columns();

  // Start from the second line/equality of the matrix.
  for (dimension_type i = 1; i < rank; i++) {
    Row& row_i = rows[i];
    std::vector<Integer>& mu_i = mu[i];
    
    // Finish computing `mu[i][j]', for all j <= i.
    for (dimension_type j = 0; j <= i; j++) {
      const std::vector<Integer>& mu_j = mu[j];
      if (j > 0)
	mu_i[j] *= mu[j-1][j-1];
      tmp_Integer[0] = 0;
      for (dimension_type h = 0; h < j; h++) {
        tmp_Integer[0] *= mu[h][h];
	tmp_Integer[1] = mu_i[h] * mu_j[h];
	tmp_Integer[0] += tmp_Integer[1];
	if (h > 0)
	  exact_div_assign(tmp_Integer[0], mu[h-1][h-1]);
      }
      mu_i[j] -= tmp_Integer[0];
    }

    // Let the `i'-th line become orthogonal wrt the `j'-th line,
    // for all 0 <= j < i.
    for (dimension_type j = 0; j < i; j++) {
      const Row& row_j = rows[j];
      const Integer& mu_ij = mu_i[j];
      const Integer& mu_jj = mu[j][j];
      for (dimension_type k = n_columns; k-- > 0; ) {
        row_i[k] *= mu_jj;
        tmp_Integer[0] = mu_ij * row_j[k];
        row_i[k] -= tmp_Integer[0];
	if (j > 0)
	  exact_div_assign(row_i[k], mu[j-1][j-1]);
      }
    }
  }
  
  // Normalize the coefficients of the orthogonal base found.
  for (dimension_type i = rank; i-- > 0; )
    rows[i].strong_normalize();

#if 0
  std::cout << "+++ After Gram-Shmidt on the base +++" << std::endl;
  ascii_dump(std::cout);
#endif

#ifndef NDEBUG
  // Check that the new base is indeed orthogonal.
  for (dimension_type i = rank; i-- > 0; ) {
    const Row& row_i = rows[i];
    for (dimension_type j = i; j-- > 0; )
      if (row_i * rows[j] != 0) {
	std::cout << "Not an orthogonal base" << std::endl;
	std::cout << "i = " << i << ", j = " << j << std::endl;
	std::cout << "After Gram-Shmidt on the base" << std::endl;
	ascii_dump(std::cout);
	assert(false);
      }
  }
#endif

  // Let denominator = <v_0, v_0> * ... * <v_{rank-1}, v_{rank-1}>
  // be the product of the squared norms of the orthogonal base.
  // Define d[j] = denominator / <v_j, v_j>.
  // Then, the formula to be computed, for each vector w which is not
  // in the orthogonal base, is the following:
  //
  // w' = denominator * w - \sum_{j=0}^{rank-1} (d[j] * <w, v_j> * v_j)
  //
  // factors[j] will contain d[j] * <w, v_j>.

  static std::vector<Integer> d;
  static std::vector<Integer> factors;
  d.reserve(compute_capacity(rank));
  factors.reserve(compute_capacity(rank));
  if (d.size() < rank) {
    dimension_type growth = rank - d.size();
    d.insert(d.end(), growth, 0);
    factors.insert(factors.end(), growth, 0);
  }

  // Computing all the factors d[0], ..., d[rank-1], and the denominator.
  Integer denominator = 1;
  for (dimension_type i = rank; i-- > 0; ) {
    const Row& row_i = rows[i];
    d[i] = row_i * row_i;
    denominator *= d[i];
  }
  for (dimension_type i = rank; i-- > 0; )
    exact_div_assign(d[i], denominator, d[i]);

  // Orthogonalize the rows that are not lines/equalities.
  dimension_type n_rows = num_rows();
  for (dimension_type i = rank; i < n_rows; i++) {
    Row& w = rows[i];
    // Compute `factors' according to `w'.
    for (dimension_type j = rank; j-- > 0; ) {
      factors[j] = w * rows[j];
      factors[j] *= d[j];
    }
    for (dimension_type k = n_columns; k-- > 0; )
      w[k] *= denominator;
    for (dimension_type j = rank; j-- > 0; ) {
      Row& v_j = rows[j];
      for (dimension_type k = n_columns; k-- > 0; )
        w[k] -= factors[j] * v_j[k];
    }
    assert(w.is_ray_or_point_or_inequality());
    w.normalize();

#if 0
  std::cout << "+++ After Gram-Shmidt on the whole matrix +++" << std::endl;
  ascii_dump(std::cout);
#endif

#ifndef NDEBUG
    // Check that w is indeed orthogonal wrt all the vectors in the base.
    for (dimension_type h = rank; h-- > 0; )
      if (w * rows[h] != 0) {
	std::cout << "Not orthogonal" << std::endl;
	std::cout << "i = " << i << ", h = " << h << std::endl;
	std::cout << "After Gram-Shmidt on the whole matrix" << std::endl;
	ascii_dump(std::cout);
	assert(false);
      }
#endif
  }
  // Matrix may be no longer sorted (unless it has one line/equality
  // and at most one ray/point/inequality).
  if (rank > 1 || n_rows > rank + 1)
    set_sorted(false);
  // A well-formed matrix has to be returned.
  assert(OK(true));
}

PPL::dimension_type
PPL::Matrix::gauss() {
  // This method is only applied to a well-formed matrix
  // having no pending rows.
  assert(OK(true));
  assert(num_pending_rows() == 0);

  dimension_type rank = 0;
  // Will keep track of the variations on the matrix of equalities.
  bool changed = false;
  dimension_type n_columns =  num_columns();
  dimension_type n_lines_or_equalities = num_lines_or_equalities();
  for (dimension_type j = n_columns; j-- > 0; ) {
    for (dimension_type i = rank; i < n_lines_or_equalities; ++i) {
      // Looking for the first non-zero coefficient (the pivot)
      // in the j-th column, starting from the last column.
      if (rows[i][j] != 0) {
	// We want the pivot to be placed on the secondary diagonal,
	// if it is not the case, we swap the row containing it
	// with the one indexed by rank (that can be a previous one
	// or the same: in this case we do not swap).
	if (i > rank) {
	  std::swap(rows[i], rows[rank]);
	  // After swapping the matrix is no longer sorted.
	  changed = true;
	}
	// Linear combining the row containing the pivot with
	// all the ones that follow it such that all the elements
	// on the j-th column (of these rows) become 0.
	for (dimension_type k = i + 1; k < n_lines_or_equalities; ++k) {
	  if (rows[k][j] != 0) {
	    rows[k].linear_combine(rows[rank], j);
	    changed = true;
	  }
	}
	// Have to consider the rows following the rank-th one
	// because until that one are already triangularized.
	++rank;
	break;
      }
    }
  }
  if (changed)
    set_sorted(false);
  // A well-formed matrix is returned.
  assert(OK(true));
  return rank;
}

void
PPL::Matrix::back_substitute(dimension_type rank) {
  // This method is only applied to a well-formed matrix
  // having no pending rows.
  assert(OK(true));
  assert(num_pending_rows() == 0);
  // The matrix describes a non-empty polyhedron and thus it always
  // contains a row which is not a line/equality (corresponding to
  // a vertex or to a low-level constraint).
  assert(num_rows() > rank);

  dimension_type nrows = num_rows();
  // Trying to keep sortedness.
  bool was_sorted = is_sorted();
  // This deque of booleans will be used to flag those rows that,
  // before exiting, need to be re-checked for sortedness.
  std::deque<bool> check_for_sortedness;
  if (was_sorted)
    check_for_sortedness.insert(check_for_sortedness.end(), nrows, false); 

  for (dimension_type k = rank; k-- > 0; ) {
    // For each row, starting from the rank-th one,
    // looks for the last non-zero element.
    // j will be the index of such a element.
    Row& row_k = rows[k];
    dimension_type j = num_columns() - 1;
    while (j != 0 && row_k[j] == 0)
      --j;

    // Go through the equalities above `row_k'.
    for (dimension_type i = k; i-- > 0; ) {
      Row& row_i = rows[i];
      assert(row_i.is_line_or_equality());
      if (row_i[j] != 0) {
	// Combine linearly `row_i' with `row_k'
	// so that `row_i[j]' becomes zero.
	row_i.linear_combine(row_k, j);
	if (was_sorted) {
	  // Trying to keep sortedness: remember which rows
	  // have to be re-checked for sortedness at the end.
	  if (i > 0)
	    check_for_sortedness[i-1] = true;
	  check_for_sortedness[i] = true;
	}
      }
    }

    // Due to strong normalization during previous iterations,
    // the pivot coefficient `row_k[j]' may now be negative.
    // Since an inequality (or ray or point) cannot be multiplied
    // by a negative factor, the coefficient of the pivot must be
    // forced to be positive.
    bool have_to_negate = (row_k[j] < 0);
    if (have_to_negate)
      for (dimension_type h = num_columns(); h-- > 0; )
	PPL::negate(row_k[h]);
    // Note: we do not mark index `k' in `check_for_sortedness',
    // because we will later negate back the row.
    
    // Go through all the inequalities of the matrix.
    for (dimension_type i = rank; i < nrows; ++i) {
      Row& row_i = rows[i];
      if (row_i[j] != 0) {
	// Combine linearly the `row_i' with `row_k'
	// so that `row_i[j]' becomes zero.
	row_i.linear_combine(row_k, j);
	if (was_sorted) {
	  // Trying to keep sortedness: remember which rows
	  // have to be re-checked for sortedness at the end.
	  if (i > rank)
	    check_for_sortedness[i-1] = true;
	  check_for_sortedness[i] = true;
	}
      }
    }

    if (have_to_negate)
      // Negate `row_k' to restore strong-normalization.
      for (dimension_type h = num_columns(); h-- > 0; )
	PPL::negate(row_k[h]);
  }

  // Trying to keep sortedness.
  for (dimension_type i = 0, iend = nrows-1; was_sorted && i < iend; ++i)
    if (check_for_sortedness[i])
      // Have to check sortedness of `mat[i]' wrt `mat[i+1]'.
      was_sorted = (rows[i] <= rows[i+1]);
  // Set the sortedness flag.
  set_sorted(was_sorted);

  // A well-formed matrix is returned.
  assert(OK(true));
}


void
PPL::Matrix::add_rows_and_columns(dimension_type n) {
  assert(n > 0);
  bool was_sorted = is_sorted();
  dimension_type old_n_rows = num_rows();
  dimension_type old_n_columns = num_columns();
  grow(old_n_rows + n, old_n_columns + n);
  Matrix& x = *this;
  // The old matrix is moved to the bottom.
  for (dimension_type i = old_n_rows; i-- > 0; )
    std::swap(x[i], x[i + n]);
  for (dimension_type i = n, c = old_n_columns; i-- > 0; ) {
    // The top right-hand sub-matrix (i.e., the matrix made
    // of new rows and columns) is set to the specular image
    // of the identity matrix.
    Row& r = x[i];
    r[c++] = 1;
    r.set_is_line_or_equality();
    // Note: `r' is strongly normalized.
  }
  // If the old matrix was empty, the last row added is either
  // a positivity constraint or a point.
  if (old_n_columns == 0) {
    x[n-1].set_is_ray_or_point_or_inequality();
    // Since ray, points and inequalities come after lines
    // and equalities, this case implies the matrix is sorted.
    set_sorted(true);
  }  
  else if (was_sorted)
    set_sorted(x[n-1] <= x[n]);

  // A well-formed matrix has to be returned.
  assert(OK(true));
}


bool
PPL::Matrix::check_sorted() const {
  const Matrix& x = *this;
  for (dimension_type i = first_pending_row(); i-- > 1; )
    if (x[i] < x[i-1])
      return false;
  return true;
}


bool
PPL::Matrix::OK(bool check_strong_normalized) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // `index_first_pending' must be less then or equal to `num_rows()'.
  if (first_pending_row() > num_rows()) {
#ifndef NDEBUG
      cerr << "Matrix has a negative number of pending rows!"
	   << endl;
#endif
    return false;
  }

  // An empty matrix must have num_columns() == 0.
  if (num_rows() == 0)
    if (num_columns() == 0)
      // An empty matrix is OK.
      return true;
    else {
#ifndef NDEBUG
      cerr << "Matrix has no rows but num_columns() is nonzero!"
	   << endl;
#endif
      return false;
    }

  // A non-empty matrix will contain constraints or generators; in
  // both cases it must have at least one column for the inhomogeneous
  // term and, if it is non-necessarily closed, another one
  // for the epsilon coefficient.
  dimension_type min_cols = is_necessarily_closed() ? 1 : 2;
  if (num_columns() < min_cols) {
#ifndef NDEBUG
    cerr << "Matrix has fewer columns than the minimum "
	 << "allowed by its topology:"
	 << endl
	 << "num_columns is " << num_columns()
	 << ", minimum is " << min_cols
	 << endl;
#endif
    return false;
  }

  const Matrix& x = *this;
  dimension_type n_rows = num_rows();
  for (dimension_type i = 0; i < n_rows; ++i) {
    if (!x[i].OK(row_size, row_capacity))
      return false;
    // Checking for topology mismatches.
    if (x.topology() != x[i].topology()) {
#ifndef NDEBUG
      cerr << "Topology mismatch between the matrix "
	   << "and one of its rows!"
	   << endl;
#endif
      return false;
    }
  }

  if (check_strong_normalized) {
    // Check for strong normalization of rows.
    // Note: normalization cannot be checked inside the Row::OK() method,
    // because a Row object may also implement a LinExpression object,
    // which in general cannot be (strongly) normalized.
    Matrix tmp = x;
    tmp.strong_normalize();
    if (x != tmp) {
#ifndef NDEBUG
      cerr << "Matrix rows are not strongly normalized!"
	   << endl;
#endif
      return false;
    }
  }

  if (sorted && !check_sorted()) {
#ifndef NDEBUG
    cerr << "The matrix declares itself to be sorted but it is not!"
	 << endl;
#endif
    return false;
  }

  // All checks passed.
  return true;
}
