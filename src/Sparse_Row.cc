/* Sparse_Row class implementation (non-inline functions).
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

#include "Sparse_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::Sparse_Row::Sparse_Row(size_type n,const Unlimited_Sparse_Row &x)
  : row(x), size_(n) {
  row.reset_after(n);
  PPL_ASSERT(OK());
}

PPL::Sparse_Row::Sparse_Row(const std::vector<Coefficient>& v)
  : row(v), size_(v.size()) {
  PPL_ASSERT(OK());
}

void
PPL::Sparse_Row::resize(size_type n) {
  if (n < size_)
    reset(lower_bound(n),lower_bound(size_));
  size_ = n;
  PPL_ASSERT(OK());
}

PPL::Sparse_Row::size_type
PPL::Sparse_Row::size() const {
  return size_;
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::reset(iterator i) {
  iterator res = row.reset(i);
  PPL_ASSERT(OK());
  return res;
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::reset(iterator first,iterator last) {
  iterator res = row.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::begin() {
  return row.begin();
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::end() {
  return row.end();
}

PPL::Sparse_Row::const_iterator
PPL::Sparse_Row::begin() const {
  return row.begin();
}

PPL::Sparse_Row::const_iterator
PPL::Sparse_Row::end() const {
  return row.end();
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::find(const key_type &k) {
  return row.find(k);
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::lower_bound(const key_type &k) {
  return row.lower_bound(k);
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::upper_bound(const key_type &k) {
  return row.upper_bound(k);
}

PPL::Sparse_Row::const_iterator
PPL::Sparse_Row::find(const key_type &k) const {
  return row.find(k);
}

PPL::Sparse_Row::const_iterator
PPL::Sparse_Row::lower_bound(const key_type &k) const {
  return row.lower_bound(k);
}

PPL::Sparse_Row::const_iterator
PPL::Sparse_Row::upper_bound(const key_type &k) const {
  return row.upper_bound(k);
}

PPL::Sparse_Row::operator const PPL::Unlimited_Sparse_Row &() const {
  return row;
}

bool
PPL::Sparse_Row::OK() const {
  if (!row.OK())
    return false;
  Unlimited_Sparse_Row row1(row);
  row1.reset_after(size_);
  return (row == row1);
}
