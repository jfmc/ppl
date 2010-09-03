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

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Sparse_Row::Sparse_Row(dimension_type n, Flags flags)
  : size_(n), flags_(flags) {
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::construct(dimension_type sz) {
  resize(sz);
}

inline void
Sparse_Row::construct(dimension_type sz, dimension_type /* capacity */) {
  resize(sz);
}

inline void
Sparse_Row::swap(Sparse_Row& x) {
  tree.swap(x.tree);
  std::swap(size_, x.size_);
  std::swap(flags_, x.flags_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline dimension_type
Sparse_Row::size() const {
  return size_;
}

inline void
Sparse_Row::resize(dimension_type n) {
  if (n < size_)
    reset_after(n);
  size_ = n;
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::shrink(dimension_type n) {
  resize(n);
}

inline void
Sparse_Row::delete_element_and_shift(dimension_type i) {
  PPL_ASSERT(i < size_);
  tree.erase_element_and_shift_left(i);
  --size_;
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::add_zeroes_and_shift(dimension_type n, dimension_type i) {
  PPL_ASSERT(i <= size_);
  tree.increase_keys_from(i, n);
  size_ += n;
  PPL_ASSERT(OK());
}

inline Sparse_Row::iterator
Sparse_Row::begin() {
  return tree.begin();
}

inline const Sparse_Row::iterator&
Sparse_Row::end() {
  return tree.end();
}

inline Sparse_Row::const_iterator
Sparse_Row::begin() const {
  return tree.cbegin();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::end() const {
  return tree.cend();
}

inline Sparse_Row::const_iterator
Sparse_Row::cbegin() const {
  return tree.cbegin();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::cend() const {
  return tree.cend();
}

inline const Sparse_Row::Flags&
Sparse_Row::flags() const {
  return flags_;
}

inline Sparse_Row::Flags&
Sparse_Row::flags() {
  return flags_;
}

inline void
Sparse_Row::clear() {
  tree.clear();
}

inline Coefficient&
Sparse_Row::operator[](dimension_type i) {
  PPL_ASSERT(i < size_);
  iterator itr = find_create(i);
  return itr->second;
}

inline const Coefficient&
Sparse_Row::operator[](dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Row::get(dimension_type i) const {
  PPL_ASSERT(i < size_);
  if (tree.empty())
    return Coefficient_zero();
  const_iterator itr = find(i);
  if (itr != end())
    return itr->second;
  else
    return Coefficient_zero();
}

inline Sparse_Row::iterator
Sparse_Row::find(dimension_type i) {
  iterator itr = tree.bisect(i);

  if (itr != end() && itr->first == i)
    return itr;

  return end();
}

inline Sparse_Row::iterator
Sparse_Row::find(iterator hint, dimension_type i) {
  iterator itr = tree.bisect_near(hint, i);

  if (itr != end() && itr->first == i)
    return itr;

  return end();
}

inline Sparse_Row::const_iterator
Sparse_Row::find(dimension_type i) const {
  const_iterator itr = tree.bisect(i);

  if (itr != end() && itr->first == i)
    return itr;

  return end();
}

inline Sparse_Row::const_iterator
Sparse_Row::find(const_iterator hint, dimension_type i) const {
  const_iterator itr = tree.bisect_near(hint, i);

  if (itr != end() && itr->first == i)
    return itr;

  return end();
}

inline Sparse_Row::iterator
Sparse_Row::lower_bound(dimension_type i) {
  iterator itr = tree.bisect(i);

  if (itr == end())
    return end();

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Sparse_Row::iterator
Sparse_Row::lower_bound(iterator hint, dimension_type i) {

  iterator itr = tree.bisect_near(hint, i);

  if (itr == end())
    return end();

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Sparse_Row::const_iterator
Sparse_Row::lower_bound(dimension_type i) const {
  const_iterator itr = tree.bisect(i);

  if (itr == end())
    return end();

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Sparse_Row::const_iterator
Sparse_Row::lower_bound(const_iterator hint, dimension_type i) const {
  const_iterator itr = tree.bisect_near(hint, i);

  if (itr == end())
    return end();

  if (itr->first < i)
    ++itr;

  PPL_ASSERT(itr == end() || itr->first >= i);

  return itr;
}

inline Sparse_Row::iterator
Sparse_Row::find_create(dimension_type i, const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return tree.insert(i, x);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(iterator itr, dimension_type i,
                        const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return tree.insert(itr, i, x);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(dimension_type i) {
  PPL_ASSERT(i < size_);
  return tree.insert(i);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(iterator itr, dimension_type i) {
  PPL_ASSERT(i < size_);
  return tree.insert(itr, i);
}

inline void
Sparse_Row::swap(iterator i, iterator j) {
  PPL_ASSERT(i != end());
  PPL_ASSERT(j != end());
  std::swap(i->second, j->second);
  PPL_ASSERT(OK());
}

inline Sparse_Row::iterator
Sparse_Row::reset(iterator i) {
  iterator res = tree.erase(i);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Row::reset(dimension_type i) {
  tree.erase(i);
  PPL_ASSERT(OK());
}

inline memory_size_type
Sparse_Row::external_memory_in_bytes() const {
  return tree.external_memory_in_bytes();
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
