/* SatMatrix class implementation (non-inline functions).
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "SatMatrix.defs.hh"

#include "globals.defs.hh"
#include <iostream>
#include <string>

#include "swapping_sort.icc"

namespace PPL = Parma_Polyhedra_Library;

PPL::SatMatrix&
PPL::SatMatrix::operator=(const SatMatrix& y){
  rows = y.rows;
  row_size = y.row_size;
  assert(OK());
  return *this;
}

void
PPL::SatMatrix::sort_rows() {
  typedef std::vector<SatRow>::iterator Iter;
  // Sorting without removing duplicates.
  Iter first = rows.begin();
  Iter last = rows.end();
  swapping_sort(first, last, SatRow_Less_Than());
  // Moving all the duplicate elements at the end of the vector.
  Iter new_last = swapping_unique(first, last);
  // Removing duplicates.
  rows.erase(new_last, last);
  assert(OK());
}

void
PPL::SatMatrix::add_row(const SatRow& row) {
  const dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<SatRow> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size, max_num_rows()));
    new_rows.insert(new_rows.end(), new_rows_size, SatRow());
    // Put the new row in place.
    dimension_type i = new_rows_size-1;
    new_rows[i] = row;
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new rows into place.
    std::swap(rows, new_rows);
  }
  else
    // Reallocation will NOT take place: append a new empty row.
    rows.push_back(row);
  assert(OK());
}

void
PPL::SatMatrix::transpose() {
  const SatMatrix& x = *this;
  const dimension_type nrows = num_rows();
  const dimension_type ncols = num_columns();
  SatMatrix tmp(ncols, nrows);
  for (dimension_type i = nrows; i-- > 0; )
    for (int j = x[i].last(); j >= 0; j = x[i].prev(j))
      tmp[j].set(i);
  swap(tmp);
  assert(OK());
}

void
PPL::SatMatrix::transpose_assign(const SatMatrix& y) {
  const dimension_type y_nrows = y.num_rows();
  const dimension_type y_ncols = y.num_columns();
  SatMatrix tmp(y_ncols, y_nrows);
  for (dimension_type i = y_nrows; i-- > 0; )
    for (int j = y[i].last(); j >= 0; j = y[i].prev(j))
      tmp[j].set(i);
  swap(tmp);
  assert(OK());
}

void
PPL::SatMatrix::resize(dimension_type new_n_rows,
		       dimension_type new_n_columns) {
  assert(OK());
  const dimension_type old_num_rows = num_rows();
  if (new_n_columns < row_size) {
    const dimension_type num_preserved_rows
      = std::min(old_num_rows, new_n_rows);
    SatMatrix& x = *this;
    for (dimension_type i = num_preserved_rows; i-- > 0; )
      x[i].clear_from(new_n_columns);
  }
  row_size = new_n_columns;
  if (new_n_rows > old_num_rows) {
    if (rows.capacity() < new_n_rows) {
      // Reallocation will take place.
      std::vector<SatRow> new_rows;
      new_rows.reserve(compute_capacity(new_n_rows, max_num_rows()));
      new_rows.insert(new_rows.end(), new_n_rows, SatRow());
      // Steal the old rows.
      for (dimension_type i = old_num_rows; i-- > 0; )
	new_rows[i].swap(rows[i]);
      // Put the new vector into place.
      std::swap(rows, new_rows);
    }
    else
      // Reallocation will NOT take place.
      rows.insert(rows.end(), new_n_rows - old_num_rows, SatRow());
  }
  else if (new_n_rows < old_num_rows)
    // Drop some rows.
    rows.erase(rows.begin() + new_n_rows, rows.end());

  assert(OK());
}

void
PPL::SatMatrix::ascii_dump(std::ostream& s) const {
  using std::endl;

  const SatMatrix& x = *this;
  const char separator = ' ';
  s << num_rows() << separator << 'x' << separator
    << num_columns() << endl;
  for (dimension_type i = 0; i < num_rows(); ++i) {
    for (dimension_type j = 0; j < num_columns(); ++j)
      s << x[i][j] << separator;
    s << endl;
  }
}

bool
PPL::SatMatrix::ascii_load(std::istream& s) {
  SatMatrix& x = *this;
  dimension_type nrows;
  dimension_type ncols;
  std::string str;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
    return false;
  resize(nrows, ncols);

  for (dimension_type i = 0; i < num_rows(); ++i)
    for (dimension_type j = 0; j < num_columns(); ++j) {
      int bit;
      if (!(s >> bit))
	return false;
      if (bit)
	x[i].set(j);
      else
	x[i].clear(j);
    }
  // Check for well-formedness.
  assert(OK());
  return true;
}

PPL::memory_size_type
PPL::SatMatrix::external_memory_in_bytes() const {
  memory_size_type n = rows.capacity() * sizeof(Row);
  for (dimension_type i = num_rows(); i-- > 0; )
    n += rows[i].external_memory_in_bytes();
  return n;
}

bool
PPL::SatMatrix::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  const SatMatrix& x = *this;
  for (dimension_type i = num_rows(); i-- > 1; ) {
    const SatRow& row = x[i];
    if (!row.OK())
      return false;
    else if (row.last() >= 0 && unsigned(row.last()) >= row_size) {
#ifndef NDEBUG
      cerr << "SatMatrix[" << i << "] is a SatRow with too many bits!"
	   << endl
	   << "(row_size == " << row_size
	   << ", row.last() == " << row.last() << ")"
	   << endl;
#endif
      return false;
    }
  }
  return true;
}

#ifndef NDEBUG
bool
PPL::SatMatrix::check_sorted() const {
  const SatMatrix& x = *this;
  for (dimension_type i = num_rows(); i-- > 1; )
    if (compare(x[i-1], x[i]) > 0)
      return false;
  return true;
}
#endif
