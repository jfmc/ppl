/* SatMatrix class implementation (non-inline functions).
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "maxmin.hh"
#include "globals.hh"
#include <iostream>
#include <algorithm>
#include <string>

namespace PPL = Parma_Polyhedra_Library;

PPL::SatMatrix&
PPL::SatMatrix::operator =(const SatMatrix& y){
  rows = y.rows;
  row_size = y.row_size;
  assert(OK());
  return *this;
}

/*!
  Sorts the rows and removes duplicates.
*/
void
PPL::SatMatrix::sort_rows() {
  // Sorting without removing duplicates.
  std::sort(rows.begin(), rows.end(), RowCompare());
  // Moving all the duplicate elements at the end of the vector.
  std::vector<SatRow>::iterator first_duplicate
    = std::unique(rows.begin(), rows.end());
  // Removing duplicates.
  rows.erase(first_duplicate, rows.end());
  assert(OK());
}

/*!
  Adds the given row.
*/
void
PPL::SatMatrix::add_row(const SatRow& row) {
  size_t new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<SatRow> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, SatRow());
    // Put the new row in place.
    size_t i = new_rows_size-1;
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

/*!
  Transpose the matrix.
*/
void
PPL::SatMatrix::transpose() {
  const SatMatrix& x = *this;
  size_t nrows = num_rows();
  size_t ncols = num_columns();
  SatMatrix tmp(ncols, nrows);
  for (size_t i = nrows; i-- > 0; )
    for (int j = x[i].last(); j >= 0; j = x[i].prev(j))
      tmp[j].set(i);
  swap(tmp);
  assert(OK());
}

/*!
  Make \p *this a transposed copy of \p y.
*/
void
PPL::SatMatrix::transpose_assign(const SatMatrix& y) {
  size_t y_nrows = y.num_rows();
  size_t y_ncols = y.num_columns();
  SatMatrix tmp(y_ncols, y_nrows);
  for (size_t i = y_nrows; i-- > 0; )
    for (int j = y[i].last(); j >= 0; j = y[i].prev(j))
      tmp[j].set(i);
  swap(tmp);
  assert(OK());
}

/*!
  Clears all the rows.
*/
void
PPL::SatMatrix::clear() {
  SatMatrix& x = *this;
  for (size_t i = num_rows(); i-- > 0; )
    x[i].clear();
  assert(x.OK());
}

/*!
  Resizes the matrix copying the old contents.
*/
void
PPL::SatMatrix::resize(size_t new_num_rows, size_t new_num_columns) {
  size_t old_num_rows = num_rows();
  size_t num_preserved_rows = min(old_num_rows, new_num_rows);
  if (new_num_columns < row_size) {
    SatMatrix& x = *this;
    for (size_t i = num_preserved_rows; i-- > 0; )
      x[i].clear_from(new_num_columns);
  }
  row_size = new_num_columns;
  if (new_num_rows > old_num_rows) {
    if (rows.capacity() < new_num_rows) {
      // Reallocation will take place.
      std::vector<SatRow> new_rows;
      new_rows.reserve(compute_capacity(new_num_rows));
      new_rows.insert(new_rows.end(), new_num_rows, SatRow());
      // Steal the old rows.
      for (size_t i = old_num_rows; i-- > 0; )
	new_rows[i].swap(rows[i]);
      // Put the new vector into place.
      std::swap(rows, new_rows);
    }
    else
      // Reallocation will NOT take place.
      rows.insert(rows.end(), new_num_rows - old_num_rows, SatRow());
  }
  assert(OK());
}

/*!
  Returns <CODE>true</CODE> if and only if \p x is identical
  to \p y.
*/
bool
PPL::operator ==(const SatMatrix& x, const SatMatrix& y) {
  assert(x.OK());
  assert(y.OK());
  if (x.num_columns() != y.num_columns())
    return false;
  size_t x_num_rows = x.num_rows();
  if (x_num_rows != y.num_rows())
    return false;
  for (size_t i = x_num_rows; i-- > 0; )
    if (compare(x[i], y[i]) != 0)
      return false;
  return true;
}


/*!
  \param row   The row that will be searched in the matrix.

  \return      <CODE>true</CODE> if \p row belongs
               to \p *this, false otherwise.

  Given a sorted saturation matrix (this ensures better efficiency), tells
  whether it contains a given row.
*/
bool
PPL::SatMatrix::sorted_contains(const SatRow& row) const {
  assert(check_sorted());
  const SatMatrix& x = *this;
  for (size_t i = num_rows(); i-- > 0; ) {
    int comp = compare(x[i], row);
    if (comp == 0)
      return true;
    else if (comp < 0)
      return false;
  }
  return false;
}

/*!
  Raw output function.
*/
std::ostream&
PPL::operator <<(std::ostream& s, const SatMatrix& x) {
  using std::endl;

  const char separator = ' ';
  s << x.num_rows() << separator << 'x' << separator
    << x.num_columns() << endl;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s << x[i][j] << separator;
    s << endl;
  }
  return s;
}

/*!
  Raw input function.
*/
std::istream&
PPL::operator >>(std::istream& s, SatMatrix& x) {
  size_t nrows;
  size_t ncols;
  std::string tempstr;
  s >> nrows
    >> tempstr
    >> ncols;
  x.resize(nrows, ncols);
  for (size_t i = 0; i < x.num_rows(); ++i)
    for (size_t j = 0; j < x.num_columns(); ++j) {
      int bit;
      s >> bit;
      if (bit)
	x[i].set(j);
      else
	x[i].clear(j);
    }
  assert(x.OK());
  return s;
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this actually represents
  a saturation matrix.
*/
bool
PPL::SatMatrix::OK() const {
  using std::endl;
  using std::cerr;

  const SatMatrix& x = *this;
  for (size_t i = num_rows(); i-- > 1; ) {
    const SatRow& r = x[i];
    if (!r.OK())
      return false;
    else if (r.last() >= 0 && unsigned(r.last()) >= row_size) {
      cerr << "SatMatrix has a SatRow with too many bits!"
	   << endl;
      return false;
    }
  }
  return true;
}

#ifndef NDEBUG
/*!
  Checks whether the matrix is sorted.
  It does NOT remove duplicates.
*/
bool
PPL::SatMatrix::check_sorted() const {
  const SatMatrix& x = *this;
  for (size_t i = num_rows(); i-- > 1; )
    if (compare(x[i-1], x[i]) > 0)
      return false;
  return true;
}
#endif
