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
PPL::Sparse_Matrix::resize(dimension_type num_rows,
                           dimension_type num_columns) {
  typedef std::vector<Unlimited_Sparse_Row>::iterator rows_itr_type;
  rows.resize(num_rows);
  if (num_columns < num_columns_) {
    for (rows_itr_type i=rows.begin(),i_end=rows.end(); i!=i_end; ++i)
      i->reset_after(num_columns);
  }
  num_columns_ = num_columns;
  PPL_ASSERT(OK());
}

void
PPL::Sparse_Matrix::ascii_dump(std::ostream& s) const {
  s << num_rows() << " x ";
  s << num_columns() << "\n";
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i)
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

  resize(0);
  resize(new_num_rows, new_num_cols);

  for (dimension_type row = 0; row < new_num_rows; ++row)
    if (!rows[row].ascii_load(s))
      return false;

  // Check invariants.
  PPL_ASSERT(OK());
  return true;
}

bool
PPL::Sparse_Matrix::OK() const {
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i) {
    if (!i->OK())
      return false;
    if (i->begin() != i->end()) {
      Unlimited_Sparse_Row::const_iterator itr = i->begin();
      --itr;
      if (itr->first >= num_columns_)
        return false;
    }
  }
  return true;
}
