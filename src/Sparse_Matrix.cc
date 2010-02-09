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
  : rows(n), width_(n) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Matrix::Sparse_Matrix(dimension_type height, dimension_type width)
  : rows(height), width_(width) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Matrix::iterator
PPL::Sparse_Matrix::begin() {
  return iterator(rows.begin(),width_);
}

PPL::Sparse_Matrix::iterator
PPL::Sparse_Matrix::end() {
  return iterator(rows.end(),width_);
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
  return Sparse_Matrix_Row(rows[i],width_);
}

const PPL::Unlimited_Sparse_Row&
PPL::Sparse_Matrix::operator[](dimension_type i) const {
  PPL_ASSERT(i < rows.size());
  return rows[i];
}

bool
PPL::Sparse_Matrix::OK() const {
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i) {
    Sparse_Row row(*i,width_);
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
  const dimension_type &size)
  : itr(i), size_(size) {
}


PPL::Sparse_Matrix_Row::Sparse_Matrix_Row(Unlimited_Sparse_Row& row,
                                          const dimension_type& size)
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
PPL::Sparse_Matrix_Row::find(const key_type &c) {
  return row_.find(c);
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::lower_bound(const key_type &c) {
  return row_.lower_bound(c);
}

PPL::Sparse_Matrix_Row::iterator
PPL::Sparse_Matrix_Row::upper_bound(const key_type &c) {
  return row_.upper_bound(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::find(const key_type &c) const {
  return row_.find(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::lower_bound(const key_type &c) const {
  return row_.lower_bound(c);
}

PPL::Sparse_Matrix_Row::const_iterator
PPL::Sparse_Matrix_Row::upper_bound(const key_type &c) const {
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
