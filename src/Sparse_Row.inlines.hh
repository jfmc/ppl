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

// FIXME: Remove this.
// It's needed only to please KDevelop4.
#include "Sparse_Row.defs.hh"

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Sparse_Row::Sparse_Row(dimension_type n)
  : row(), size_(n) {
  PPL_ASSERT(OK());
}

inline
Sparse_Row::Sparse_Row(const Unlimited_Sparse_Row &x, dimension_type n)
  : row(x), size_(n) {
  row.reset_after(n);
  PPL_ASSERT(OK());
}

inline
Sparse_Row::Sparse_Row(const std::vector<Coefficient>& v)
  : row(v), size_(v.size()) {
  PPL_ASSERT(OK());
}

inline
Sparse_Row::Sparse_Row(const Sparse_Row_Reference& x)
  : row(static_cast<const Unlimited_Sparse_Row&>(x)), size_(x.size()) {
  PPL_ASSERT(OK());
}

inline Sparse_Row&
Sparse_Row::operator=(const Unlimited_Sparse_Row& x) {
  row = x;
  PPL_ASSERT(OK());
  return *this;
}

inline Sparse_Row&
Sparse_Row::operator=(const Sparse_Row_Reference& x) {
  Sparse_Row_Reference(row, size_) = x;
  return *this;
}

inline void
Sparse_Row::clear() {
  row.clear();
}

inline void
Sparse_Row::swap(Sparse_Row& x) {
  row.swap(x.row);
  std::swap(size_, x.size_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Sparse_Row::swap(Sparse_Row_Reference x) {
  std::swap(x, *this);
}

inline void
Sparse_Row::swap(dimension_type i, dimension_type j) {
  row.swap(i, j);
  assert(OK());
}

inline void
Sparse_Row::swap(iterator i, iterator j) {
  row.swap(i, j);
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
Sparse_Row::reset(iterator first, iterator last) {
  iterator res = row.reset(first, last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Row::reset_after(dimension_type i) {
  PPL_ASSERT(i < size_);
  row.reset_after(i);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row::normalize() {
  row.normalize();
  PPL_ASSERT(OK());
}

inline Coefficient&
Sparse_Row::operator[](dimension_type i) {
  PPL_ASSERT(i < size_);
  return row[i];
}

inline void
Sparse_Row::assign(dimension_type i, const Coefficient& x) {
  PPL_ASSERT(i < size_);
  row.assign(i,x);
}

inline void
Sparse_Row::assign_if_nonzero(dimension_type i, const Coefficient& x) {
  PPL_ASSERT(i < size_);
  row.assign_if_nonzero(i,x);
}

inline const Coefficient&
Sparse_Row::operator[](dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Row::get(dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row.get(i);
}

inline const Sparse_Row::iterator&
Sparse_Row::before_begin() {
  return row.before_begin();
}

inline Sparse_Row::iterator
Sparse_Row::begin() {
  return row.begin();
}

inline const Sparse_Row::iterator&
Sparse_Row::end() {
  return row.end();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::before_begin() const {
  return row.before_cbegin();
}

inline Sparse_Row::const_iterator
Sparse_Row::begin() const {
  return row.cbegin();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::end() const {
  return row.cend();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::before_cbegin() const {
  return row.before_cbegin();
}

inline Sparse_Row::const_iterator
Sparse_Row::cbegin() const {
  return row.cbegin();
}

inline const Sparse_Row::const_iterator&
Sparse_Row::cend() const {
  return row.cend();
}

inline Sparse_Row::iterator
Sparse_Row::find(dimension_type k) {
  return row.find(k);
}

inline Sparse_Row::iterator
Sparse_Row::lower_bound(dimension_type k) {
  return row.lower_bound(k);
}

inline Sparse_Row::const_iterator
Sparse_Row::find(dimension_type k) const {
  return row.find(k);
}

inline Sparse_Row::const_iterator
Sparse_Row::lower_bound(dimension_type k) const {
  return row.lower_bound(k);
}

inline Sparse_Row::iterator
Sparse_Row::find(iterator itr, dimension_type i) {
  return row.find(itr, i);
}

inline Sparse_Row::iterator
Sparse_Row::lower_bound(iterator itr, dimension_type i) {
  return row.lower_bound(itr, i);
}

inline Sparse_Row::const_iterator
Sparse_Row::find(const_iterator itr, dimension_type i) const {
  return row.find(itr, i);
}

inline Sparse_Row::const_iterator
Sparse_Row::lower_bound(const_iterator itr, dimension_type i) const {
  return row.lower_bound(itr, i);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(dimension_type i, const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(i ,x);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(dimension_type i) {
  PPL_ASSERT(i < size_);
  return row.find_create(i);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(iterator itr, dimension_type i,
                        const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(itr, i, x);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(iterator itr, dimension_type i) {
  PPL_ASSERT(i < size_);
  return row.find_create(itr, i);
}

inline
Sparse_Row::operator Sparse_Row_Reference() {
  return Sparse_Row_Reference(row, size_);
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


inline
Sparse_Row_Reference::Sparse_Row_Reference(Unlimited_Sparse_Row& row1,
                                           dimension_type size)
  : row(row1), size_(size) {
  PPL_ASSERT(OK());
}

inline Sparse_Row_Reference&
Sparse_Row_Reference::operator=(const Unlimited_Sparse_Row& x) {
  row = x;
  PPL_ASSERT(OK());
  return *this;
}

inline Sparse_Row_Reference&
Sparse_Row_Reference::operator=(const Sparse_Row_Reference& x) {
  PPL_ASSERT(size_ == x.size_);
  row = x.row;
  PPL_ASSERT(OK());
  return *this;
}

inline Sparse_Row_Reference&
Sparse_Row_Reference::operator=(const Sparse_Row& x) {
  PPL_ASSERT(size() == x.size());
  row = static_cast<const Unlimited_Sparse_Row&>(x);
  PPL_ASSERT(OK());
  return *this;
}

inline void
Sparse_Row_Reference::clear() {
  row.clear();
}

inline void
Sparse_Row_Reference::swap(Sparse_Row_Reference x) {
  PPL_ASSERT(size_ == x.size_);
  row.swap(x.row);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Sparse_Row_Reference::swap(Sparse_Row& x) {
  std::swap(*this, x);
}

inline void
Sparse_Row_Reference::swap(dimension_type i, dimension_type j) {
  row.swap(i, j);
  assert(OK());
}

inline void
Sparse_Row_Reference::swap(iterator i, iterator j) {
  row.swap(i, j);
  PPL_ASSERT(OK());
}

inline dimension_type
Sparse_Row_Reference::size() const {
  return size_;
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::reset(iterator i) {
  PPL_ASSERT(i != end());
  iterator res = row.reset(i);
  PPL_ASSERT(OK());
  return res;
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::reset(iterator first, iterator last) {
  iterator res = row.reset(first, last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Row_Reference::reset(dimension_type i) {
  PPL_ASSERT(i < size_);
  row.reset(i);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row_Reference::reset(dimension_type first, dimension_type last) {
  PPL_ASSERT(last <= size_);
  row.reset(first, last);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row_Reference::reset_after(dimension_type i) {
  PPL_ASSERT(i < size_);
  row.reset_after(i);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row_Reference::normalize() {
  row.normalize();
  PPL_ASSERT(OK());
}

inline Coefficient&
Sparse_Row_Reference::operator[](dimension_type i) {
  PPL_ASSERT(i < size_);
  return row[i];
}

inline void
Sparse_Row_Reference::assign(dimension_type i, const Coefficient& x) {
  PPL_ASSERT(i < size_);
  row.assign(i,x);
}

inline void
Sparse_Row_Reference::assign_if_nonzero(dimension_type i,
                                        const Coefficient& x) {
  PPL_ASSERT(i < size_);
  row.assign_if_nonzero(i,x);
}

inline const Coefficient&
Sparse_Row_Reference::operator[](dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Row_Reference::get(dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row.get(i);
}

inline const Sparse_Row_Reference::iterator&
Sparse_Row_Reference::before_begin() {
  return row.before_begin();
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::begin() {
  return row.begin();
}

inline const Sparse_Row_Reference::iterator&
Sparse_Row_Reference::end() {
  return row.end();
}

inline const Sparse_Row_Reference::const_iterator&
Sparse_Row_Reference::before_begin() const {
  return row.before_cbegin();
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::begin() const {
  return row.cbegin();
}

inline const Sparse_Row_Reference::const_iterator&
Sparse_Row_Reference::end() const {
  return row.cend();
}

inline const Sparse_Row_Reference::const_iterator&
Sparse_Row_Reference::before_cbegin() const {
  return row.before_cbegin();
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::cbegin() const {
  return row.cbegin();
}

inline const Sparse_Row_Reference::const_iterator&
Sparse_Row_Reference::cend() const {
  return row.cend();
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find(dimension_type i) {
  return row.find(i);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::lower_bound(dimension_type i) {
  return row.lower_bound(i);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::find(dimension_type i) const {
  return row.find(i);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::lower_bound(dimension_type i) const {
  return row.lower_bound(i);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find(iterator itr, dimension_type i) {
  return row.find(itr, i);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::lower_bound(iterator itr, dimension_type i) {
  return row.lower_bound(itr, i);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::find(const_iterator itr, dimension_type i) const {
  return row.find(itr, i);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::lower_bound(const_iterator itr,
                                  dimension_type i) const {
  return row.lower_bound(itr, i);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(dimension_type i) {
  PPL_ASSERT(i < size_);
  return row.find_create(i);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(dimension_type i,
                                  const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(i, x);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(iterator itr, dimension_type i,
                                  const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(itr, i, x);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(iterator itr, dimension_type i) {
  PPL_ASSERT(i < size_);
  return row.find_create(itr, i);
}

inline
Sparse_Row_Reference::operator const Unlimited_Sparse_Row&() const {
  return row;
}


template <typename Func>
inline void
Sparse_Row_Reference::for_each_nonzero(const Func& func,
                                       dimension_type /* n */) {
  std::for_each(begin(), end(), apply_to_data(func));
}

template <typename Func>
inline void
Sparse_Row_Reference::for_each_nonzero(const Func& func,
                                       dimension_type /* n */) const {
  std::for_each(begin(), end(), apply_to_data(func));
}

template <typename Func>
inline
Sparse_Row_Reference::applier_to_data<Func>::applier_to_data(const Func& func)
  : f(func) {
}

template <typename Func>
inline void
Sparse_Row_Reference::applier_to_data<Func>
::operator()(std::pair<dimension_type, Coefficient&> x) const {
  f(x.second);
}

template <typename Func>
inline Sparse_Row_Reference::applier_to_data<Func>
Sparse_Row_Reference::apply_to_data(const Func& func) {
  return applier_to_data<Func>(func);
}

} // namespace Parma_Polyhedra_Library


namespace std {

inline void
swap(Parma_Polyhedra_Library::Sparse_Row& x,
     Parma_Polyhedra_Library::Sparse_Row& y) {
  x.swap(y);
}

inline void
swap(Parma_Polyhedra_Library::Sparse_Row_Reference x,
     Parma_Polyhedra_Library::Sparse_Row_Reference y) {
  x.swap(y);
}

inline void
swap(Parma_Polyhedra_Library::Sparse_Row_Reference x,
     Parma_Polyhedra_Library::Sparse_Row& y) {
  if (y.size() == 0)
    y.resize(x.size());
  PPL_ASSERT(x.size() == y.size());
  x.row.swap(y.row);
  PPL_ASSERT(x.OK());
  PPL_ASSERT(y.OK());
}

inline void
swap(Parma_Polyhedra_Library::Sparse_Row& x,
     Parma_Polyhedra_Library::Sparse_Row_Reference y) {
  y.swap(x);
}

} // namespace std

#endif // !defined(PPL_Sparse_Row_inlines_hh)
