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

inline
Sparse_Row::Sparse_Row(const Sparse_Row_Reference& x)
  : row(static_cast<const Unlimited_Sparse_Row&>(x)), size_(x.size()) {
  PPL_ASSERT(OK());
}

inline Sparse_Row&
Sparse_Row::operator=(const Sparse_Row_Reference& x) {
  Sparse_Row_Reference(row,size_) = x;
  return *this;
}

inline void
Sparse_Row::swap(Sparse_Row& x) {
  row.swap(x.row);
  std::swap(size_,x.size_);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Sparse_Row::swap(Sparse_Row_Reference x) {
  std::swap(Sparse_Row_Reference(row,size_),x);
}

inline void
Sparse_Row::swap(dimension_type i, dimension_type j) {
  row.swap(i,j);
  assert(OK());
}

inline void
Sparse_Row::swap(iterator i, iterator j) {
  row.swap(i,j);
  PPL_ASSERT(OK());
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

inline Sparse_Row::dangerous_iterator
Sparse_Row::reset(dangerous_iterator i) {
  dangerous_iterator res = row.reset(i);
  PPL_ASSERT(OK());
  return res;
}

inline Sparse_Row::dangerous_iterator
Sparse_Row::reset(dangerous_iterator first,dangerous_iterator last) {
  dangerous_iterator res = row.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Row::normalize() {
  row.normalize();
  PPL_ASSERT(OK());
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

inline void
Sparse_Row::get2(const dimension_type c1,const dimension_type c2,
                 const Coefficient*& p1,const Coefficient*& p2) const {
  PPL_ASSERT(c1 < size_);
  PPL_ASSERT(c2 < size_);
  return row.get2(c1,c2,p1,p2);
}

inline Sparse_Row::dangerous_iterator
Sparse_Row::begin() {
  return row.begin();
}

inline Sparse_Row::dangerous_iterator
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

inline Sparse_Row::dangerous_iterator
Sparse_Row::find(const dimension_type k) {
  return row.find(k);
}

inline Sparse_Row::dangerous_iterator
Sparse_Row::lower_bound(const dimension_type k) {
  return row.lower_bound(k);
}

inline Sparse_Row::dangerous_iterator
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

inline Sparse_Row::iterator
Sparse_Row::find_create(const dimension_type i,const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(i,x);
}

inline Sparse_Row::iterator
Sparse_Row::find_create(const dimension_type i,const Coefficient& x,
                        iterator itr) {
  PPL_ASSERT(i < size_);
  return row.find_create(i,x,itr);
}

inline
Sparse_Row::operator Sparse_Row_Reference() {
  return Sparse_Row_Reference(row,size_);
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
  Unlimited_Sparse_Row::const_iterator i=row.begin();
  Unlimited_Sparse_Row::const_iterator i_end=row.end();
  Unlimited_Sparse_Row::const_iterator next=i;
  ++next;
  while (next != i_end)
    ++i,++next;
  return (i->first < size_);
}


inline
Sparse_Row_Reference::Sparse_Row_Reference(Unlimited_Sparse_Row& row1,
                                     const dimension_type size)
  : row(row1), size_(size) {
  PPL_ASSERT(OK());
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
Sparse_Row_Reference::swap(Sparse_Row_Reference x) {
  PPL_ASSERT(size_ == x.size_);
  row.swap(x.row);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

inline void
Sparse_Row_Reference::swap(dimension_type i, dimension_type j) {
  row.swap(i,j);
  assert(OK());
}

inline void
Sparse_Row_Reference::swap(iterator i, iterator j) {
  row.swap(i,j);
  PPL_ASSERT(OK());
}

inline dimension_type
Sparse_Row_Reference::size() const {
  return size_;
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::reset(dangerous_iterator i) {
  PPL_ASSERT(i != end());
  dangerous_iterator res = row.reset(i);
  PPL_ASSERT(OK());
  return res;
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::reset(dangerous_iterator first,dangerous_iterator last) {
  dangerous_iterator res = row.reset(first,last);
  PPL_ASSERT(OK());
  return res;
}

inline void
Sparse_Row_Reference::reset(const dimension_type i) {
  PPL_ASSERT(i < size_);
  row.reset(i);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row_Reference::reset(const dimension_type first,
                         const dimension_type last) {
  PPL_ASSERT(last <= size_);
  row.reset(first,last);
  PPL_ASSERT(OK());
}

inline void
Sparse_Row_Reference::normalize() {
  row.normalize();
  PPL_ASSERT(OK());
}

inline Coefficient&
Sparse_Row_Reference::operator[](const dimension_type i) {
  PPL_ASSERT(i < size_);
  return row[i];
}

inline const Coefficient&
Sparse_Row_Reference::operator[](const dimension_type i) const {
  return get(i);
}

inline const Coefficient&
Sparse_Row_Reference::get(const dimension_type i) const {
  PPL_ASSERT(i < size_);
  return row.get(i);
}

inline void
Sparse_Row_Reference::get2(const dimension_type c1,const dimension_type c2,
                        const Coefficient*& p1,const Coefficient*& p2) const {
  PPL_ASSERT(c1 < size_);
  PPL_ASSERT(c2 < size_);
  return row.get2(c1,c2,p1,p2);
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::begin() {
  return row.begin();
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::end() {
  return row.end();
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::begin() const {
  return row.begin();
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::end() const {
  return row.end();
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::find(const dimension_type c) {
  return row.find(c);
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::lower_bound(const dimension_type c) {
  return row.lower_bound(c);
}

inline Sparse_Row_Reference::dangerous_iterator
Sparse_Row_Reference::upper_bound(const dimension_type c) {
  return row.upper_bound(c);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::find(const dimension_type c) const {
  return row.find(c);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::lower_bound(const dimension_type c) const {
  return row.lower_bound(c);
}

inline Sparse_Row_Reference::const_iterator
Sparse_Row_Reference::upper_bound(const dimension_type c) const {
  return row.upper_bound(c);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(const dimension_type i,const Coefficient& x) {
  PPL_ASSERT(i < size_);
  return row.find_create(i,x);
}

inline Sparse_Row_Reference::iterator
Sparse_Row_Reference::find_create(const dimension_type i,const Coefficient& x,
                               iterator itr) {
  PPL_ASSERT(i < size_);
  return row.find_create(i,x,itr);
}

inline
Sparse_Row_Reference::operator const Unlimited_Sparse_Row&() const {
  return row;
}

inline bool
Sparse_Row_Reference::OK() const {
  if (!row.OK())
    return false;
  if (row.begin() == row.end())
    return true;
  Unlimited_Sparse_Row::const_iterator i=row.begin();
  Unlimited_Sparse_Row::const_iterator i_end=row.end();
  Unlimited_Sparse_Row::const_iterator next=i;
  ++next;
  while (next != i_end)
    ++i,++next;
  return (i->first < size_);
}


template <typename Func>
inline void
Sparse_Row_Reference::for_each_nonzero(const Func& func,const dimension_type n) {
  (void)n;
  std::for_each(begin(),end(),apply_to_data(func));
}

template <typename Func>
inline void
Sparse_Row_Reference::for_each_nonzero(const Func& func,const dimension_type n)
  const {
  (void)n;
  std::for_each(begin(),end(),apply_to_data(func));
}

template <typename Func>
inline
Sparse_Row_Reference::applier_to_data<Func>::applier_to_data(const Func& func)
  : f(func) {
}

template <typename Func>
inline void
Sparse_Row_Reference::applier_to_data<Func>::operator()(
  std::pair<dimension_type,Coefficient>& x) const {
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
  x.swap(y);
}

inline void
swap(Parma_Polyhedra_Library::Sparse_Row& x,
     Parma_Polyhedra_Library::Sparse_Row_Reference y) {
  y.swap(x);
}

} // namespace std

#endif // !defined(PPL_Sparse_Row_inlines_hh)
