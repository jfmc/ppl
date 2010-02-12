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

void
PPL::Sparse_Matrix::permute_columns(const std::vector<dimension_type>&
                                    cycles) {
  PPL_DIRTY_TEMP_COEFFICIENT(tmp);
  const dimension_type n = cycles.size();
  PPL_ASSERT(cycles[n - 1] == 0);
  for (dimension_type k = num_rows(); k-- > 0; ) {
    Sparse_Matrix_Row rows_k = (*this)[k];
    for (dimension_type i = 0, j = 0; i < n; i = ++j) {
      // Make `j' be the index of the next cycle terminator.
      while (cycles[j] != 0)
        ++j;
      // Cycles of length less than 2 are not allowed.
      PPL_ASSERT(j - i >= 2);
      if (j - i == 2)
        // For cycles of length 2 no temporary is needed, just a swap.
        std::swap(rows_k[cycles[i]], rows_k[cycles[i+1]]);
      else {
        // Longer cycles need a temporary.
        std::swap(rows_k[cycles[j-1]], tmp);
        for (dimension_type l = j-1; l > i; --l)
          std::swap(rows_k[cycles[l-1]], rows_k[cycles[l]]);
        std::swap(tmp, rows_k[cycles[i]]);
      }
    }
  }
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

void
PPL::Sparse_Matrix::erase_to_end(dimension_type first_to_erase) {
  resize(first_to_erase,num_columns());
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

void
PPL::Sparse_Matrix_Row::swap(Sparse_Matrix_Row x) {
  PPL_ASSERT(size_ == x.size_);
  row_.swap(x.row_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

PPL::dimension_type
PPL::Sparse_Matrix_Row::size() const {
  return size_;
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

PPL::Coefficient&
PPL::Sparse_Matrix_Row::operator[](const dimension_type i) {
  PPL_ASSERT(i < size_);
  return row_[i];
}

const PPL::Coefficient&
PPL::Sparse_Matrix_Row::operator[](const dimension_type i) const {
  return get(i);
}

const PPL::Coefficient&
PPL::Sparse_Matrix_Row::get(const dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row_.get(i);
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

void
PPL::Sparse_Matrix_Row::normalize() {
  // Compute the GCD of all the coefficients.
  const_iterator i=begin();
  const const_iterator i_end=end();
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  for ( ; i!=i_end; ++i) {
    const Coefficient& x_i = i->second;
    if (const int x_i_sign = sgn(x_i)) {
      // FIXME: can this be optimized further?
      gcd = x_i;
      if (x_i_sign < 0)
        neg_assign(gcd);
      goto compute_gcd;
    }
  }
  // We reach this point only if all the coefficients were zero.
  return;

 compute_gcd:
  if (gcd == 1)
    return;
  for (++i; i!=i_end; ++i) {
    const Coefficient& x_i = i->second;
    if (x_i != 0) {
      // Note: we use the ternary version instead of a more concise
      // gcd_assign(gcd, x_i) to take advantage of the fact that
      // `gcd' will decrease very rapidly (see D. Knuth, The Art of
      // Computer Programming, second edition, Section 4.5.2,
      // Algorithm C, and the discussion following it).  Our
      // implementation of gcd_assign(x, y, z) for checked numbers is
      // optimized for the case where `z' is smaller than `y', so that
      // on checked numbers we gain.  On the other hand, for the
      // implementation of gcd_assign(x, y, z) on GMP's unbounded
      // integers we cannot make any assumption, so here we draw.
      // Overall, we win.
      gcd_assign(gcd, x_i, gcd);
      if (gcd == 1)
        return;
    }
  }
  // Divide the coefficients by the GCD.
  for (iterator j=begin(), j_end=end(); j!=j_end; ++j) {
    Coefficient& x_j = j->second;
    exact_div_assign(x_j, x_j, gcd);
  }
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
  if (!row_.OK())
    return false;
  Unlimited_Sparse_Row row1(row_);
  row1.reset_after(size_);
  return (row_ == row1);
}
