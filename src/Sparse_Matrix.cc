/* Sparse_Matrix class implementation (non-inline functions).
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

#include "Sparse_Matrix.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Sparse_Matrix::remove_column(dimension_type i) {
  for (dimension_type j = rows.size(); j-- > 0; )
    rows[j].delete_element_and_shift(i);
  --num_columns_;
  PPL_ASSERT(OK());
}

void
PPL::Sparse_Matrix
::permute_columns(const std::vector<dimension_type>& cycles) {
  PPL_DIRTY_TEMP_COEFFICIENT(tmp);
  const dimension_type n = cycles.size();
  PPL_ASSERT(cycles[n - 1] == 0);
  for (dimension_type k = num_rows(); k-- > 0; ) {
    Sparse_Row& rows_k = (*this)[k];
    for (dimension_type i = 0, j = 0; i < n; i = ++j) {
      // Make `j' be the index of the next cycle terminator.
      while (cycles[j] != 0)
        ++j;
      // Cycles of length less than 2 are not allowed.
      PPL_ASSERT(j - i >= 2);
      if (j - i == 2)
        // For cycles of length 2 no temporary is needed, just a swap.
        rows_k.swap(cycles[i], cycles[i + 1]);
      else {
        // Longer cycles need a temporary.
        tmp = rows_k.get(cycles[j - 1]);
        for (dimension_type l = (j - 1); l > i; --l)
          rows_k.swap(cycles[l-1], cycles[l]);
        if (tmp == 0)
          rows_k.reset(cycles[i]);
        else
          std::swap(tmp, rows_k[cycles[i]]);
      }
    }
  }
}

void
PPL::Sparse_Matrix::add_zero_columns(dimension_type n, dimension_type i) {
  for (dimension_type j = rows.size(); j-- > 0; )
    rows[j].add_zeroes_and_shift(n, i);
  num_columns_ += n;
  PPL_ASSERT(OK());
}

void
PPL::Sparse_Matrix::resize(dimension_type num_rows,
                           dimension_type num_columns,
                           Flags row_flags) {
  const dimension_type old_num_rows = rows.size();
  rows.resize(num_rows);
  if (old_num_rows < num_rows) {
    for (dimension_type i = old_num_rows; i < num_rows; ++i) {
      rows[i].flags() = row_flags;
      rows[i].resize(num_columns);
    }
    if (num_columns_ != num_columns) {
      num_columns_ = num_columns;
      for (dimension_type i = 0; i < old_num_rows; ++i)
        rows[i].resize(num_columns);
    }
  } else
    if (num_columns_ != num_columns) {
      num_columns_ = num_columns;
      for (dimension_type i = 0; i < num_rows; ++i)
        rows[i].resize(num_columns);
    }
  PPL_ASSERT(OK());
}

void
PPL::Sparse_Matrix::ascii_dump(std::ostream& s) const {
  s << num_rows() << " x ";
  s << num_columns() << "\n";
  for (const_iterator i = begin(), i_end = end(); i !=i_end; ++i)
    i->ascii_dump(s);
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Sparse_Matrix)

bool
PPL::Sparse_Matrix::ascii_load(std::istream& s) {
  std::string str;
  dimension_type new_num_rows;
  dimension_type new_num_cols;
  if (!(s >> new_num_rows))
    return false;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> new_num_cols))
    return false;

  for (iterator i = rows.begin(), i_end = rows.end(); i != i_end; ++i)
    i->clear();

  resize(new_num_rows, new_num_cols);

  for (dimension_type row = 0; row < new_num_rows; ++row)
    if (!rows[row].ascii_load(s))
      return false;

  // Check invariants.
  PPL_ASSERT(OK());
  return true;
}

PPL::memory_size_type
PPL::Sparse_Matrix::external_memory_in_bytes() const {
  // Estimate the size of vector.
  memory_size_type n = rows.capacity() * sizeof(Unlimited_Sparse_Row);
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    n += i->external_memory_in_bytes();
  return n;
}

bool
PPL::Sparse_Matrix::OK() const {
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    if (i->size() != num_columns_)
      return false;
  return true;
}
