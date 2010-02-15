/* Sparse_Row class implementation: inline functions.
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

#ifndef PPL_Sparse_Row_inlines_hh
#define PPL_Sparse_Row_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Sparse_Row::Sparse_Row(const dimension_type n)
  : row(), size_(n) {
  PPL_ASSERT(OK());
}

inline
Sparse_Row::Sparse_Row(const Unlimited_Sparse_Row &x,
                            const dimension_type n)
  : row(x), size_(n) {
  row.reset_after(n);
  PPL_ASSERT(OK());
}

inline
Sparse_Row::Sparse_Row(const std::vector<Coefficient>& v)
  : row(v), size_(v.size()) {
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::swap(Sparse_Row& x) {
  row.swap(x.row);
  std::swap(size_,x.size_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Sparse_Row::construct(const dimension_type sz) {
  resize(sz);
}

inline void
Sparse_Row::construct(const dimension_type sz,
                           const dimension_type capacity) {
  (void)capacity;
  resize(sz);
}

inline void
Sparse_Row::resize(const dimension_type n) {
  if (n < size_)
    reset(lower_bound(n),end());
  size_ = n;
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::shrink(const dimension_type n) {
  resize(n);
}

inline dimension_type
Sparse_Row::size() const {
  return size_;
}

inline Sparse_Row::iterator
Sparse_Row::reset(iterator i) {
  iterator res = row.reset(i);
  PPL_ASSERT(OK());
  return res;
}

inline Sparse_Row::iterator
Sparse_Row::reset(iterator first,iterator last) {
  iterator res = row.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

inline Coefficient&
Sparse_Row::operator[](const dimension_type i) {
  PPL_ASSERT(i < size_);
  return row[i];
}

inline const Coefficient&
Sparse_Row::operator[](const dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Row::get(const dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row.get(i);
}

inline Sparse_Row::iterator
Sparse_Row::begin() {
  return row.begin();
}

inline Sparse_Row::iterator
Sparse_Row::end() {
  return row.end();
}

inline Sparse_Row::const_iterator
Sparse_Row::begin() const {
  return row.begin();
}

inline Sparse_Row::const_iterator
Sparse_Row::end() const {
  return row.end();
}

inline Sparse_Row::iterator
Sparse_Row::find(const dimension_type k) {
  return row.find(k);
}

inline Sparse_Row::iterator
Sparse_Row::lower_bound(const dimension_type k) {
  return row.lower_bound(k);
}

inline Sparse_Row::iterator
Sparse_Row::upper_bound(const dimension_type k) {
  return row.upper_bound(k);
}

inline Sparse_Row::const_iterator
Sparse_Row::find(const dimension_type k) const {
  return row.find(k);
}

inline Sparse_Row::const_iterator
Sparse_Row::lower_bound(const dimension_type k) const {
  return row.lower_bound(k);
}

inline Sparse_Row::const_iterator
Sparse_Row::upper_bound(const dimension_type k) const {
  return row.upper_bound(k);
}

inline
Sparse_Row::operator const Unlimited_Sparse_Row &() const {
  return row;
}

inline void
Sparse_Row::ascii_dump(std::ostream& s) const {
  s << "size " << size_ << ' ';
  row.ascii_dump(s);
}

inline bool
Sparse_Row::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str!="size")
    return false;
  if (!(s >> size_))
    return false;
  if (!row.ascii_load(s))
    return false;
  PPL_ASSERT(OK());
  return true;
}

inline bool
Sparse_Row::OK() const {
  if (!row.OK())
    return false;
  if (row.begin() == row.end())
    return true;
  Unlimited_Sparse_Row::const_iterator itr=row.end();
  --itr;
  return (itr->first < size_);
}

} // namespace Parma_Polyhedra_Library


namespace std {

inline void
swap(Parma_Polyhedra_Library::Sparse_Row& x,
     Parma_Polyhedra_Library::Sparse_Row& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Sparse_Row_inlines_hh)
