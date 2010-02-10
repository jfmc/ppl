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

PPL::Sparse_Matrix::Sparse_Matrix(dimension_type n)
  : rows(n), num_columns_(n) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Matrix::Sparse_Matrix(dimension_type num_rows,
                                  dimension_type num_columns)
  : rows(num_rows), num_columns_(num_columns) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Matrix::iterator
PPL::Sparse_Matrix::begin() {
  return iterator(rows.begin(),num_columns());
}

PPL::Sparse_Matrix::iterator
PPL::Sparse_Matrix::end() {
  return iterator(rows.end(),num_columns());
}

PPL::Sparse_Matrix::const_iterator
PPL::Sparse_Matrix::begin() const {
  return rows.begin();
}

PPL::Sparse_Matrix::const_iterator
PPL::Sparse_Matrix::end() const {
  return rows.end();
}

PPL::Sparse_Matrix_Row
PPL::Sparse_Matrix::operator[](dimension_type i) {
  PPL_ASSERT(i < rows.size());
  return Sparse_Matrix_Row(rows[i],num_columns());
}

const PPL::Unlimited_Sparse_Row&
PPL::Sparse_Matrix::operator[](dimension_type i) const {
  PPL_ASSERT(i < rows.size());
  return rows[i];
}

PPL::dimension_type
PPL::Sparse_Matrix::num_rows() const {
  return rows.size();
}

PPL::dimension_type
PPL::Sparse_Matrix::num_columns() const {
  return num_columns_;
}

void
PPL::Sparse_Matrix::resize(dimension_type n) {
  resize(n,n);
}

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

void
PPL::Sparse_Matrix::add_zero_rows(const dimension_type n) {
  resize(num_rows()+n,num_columns());
}

void
PPL::Sparse_Matrix::add_zero_columns(const dimension_type n) {
  resize(num_rows(),num_columns()+n);
}

void
PPL::Sparse_Matrix::add_zero_rows_and_columns(const dimension_type n,
                                              const dimension_type m) {
  resize(num_rows()+n,num_columns()+m);
}

void
PPL::Sparse_Matrix::remove_trailing_columns(const dimension_type n) {
  PPL_ASSERT(n <= num_columns());
  resize(num_rows(),num_columns()-n);
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
    Sparse_Row row(*i,num_columns_);
    if (static_cast<Unlimited_Sparse_Row>(row) != *i)
      return false;
  }
  return true;
}


PPL::Sparse_Matrix::iterator::iterator(const iterator& x)
  : itr(x.itr), size_(x.size_) {
}

PPL::Sparse_Matrix_Row
PPL::Sparse_Matrix::iterator::operator*() {
  return Sparse_Matrix_Row(*itr,size_);
}

PPL::Sparse_Matrix::iterator&
PPL::Sparse_Matrix::iterator::operator++() {
  ++itr;
  return *this;
}

PPL::Sparse_Matrix::iterator
PPL::Sparse_Matrix::iterator::operator++(int) {
  iterator x(*this);
  ++(*this);
  return x;
}

PPL::Sparse_Matrix::iterator::iterator(
  std::vector<Unlimited_Sparse_Row>::iterator i,
  const dimension_type size)
  : itr(i), size_(size) {
}


PPL::Sparse_Matrix_Row::Sparse_Matrix_Row(Unlimited_Sparse_Row& row,
                                          const dimension_type size)
  : row_(row), size_(size) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::reset(iterator i) {
  iterator res = row_.reset(i);
  PPL_ASSERT(OK());
  return res;
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::reset(iterator first,iterator last) {
  iterator res = row_.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::begin() {
  return row_.begin();
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::end() {
  return row_.end();
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::begin() const {
  return row_.begin();
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::end() const {
  return row_.end();
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::find(const dimension_type c) {
  return row_.find(c);
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::lower_bound(const dimension_type c) {
  return row_.lower_bound(c);
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::upper_bound(const dimension_type c) {
  return row_.upper_bound(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::find(const dimension_type c) const {
  return row_.find(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::lower_bound(const dimension_type c) const {
  return row_.lower_bound(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::upper_bound(const dimension_type c) const {
  return row_.upper_bound(c);
}

PPL::Sparse_Matrix_Row::operator const Unlimited_Sparse_Row&() const {
  return row_;
}

PPL::Sparse_Matrix_Row::operator Sparse_Row() const {
  return Sparse_Row(row_,size_);
}

bool
PPL::Sparse_Matrix_Row::OK() const {
  Sparse_Row row(row_,size_);
  if (static_cast<Unlimited_Sparse_Row>(row) != row_)
    return false;
  return true;
}
