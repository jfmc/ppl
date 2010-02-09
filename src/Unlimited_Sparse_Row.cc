/* Unlimited_Sparse_Row class implementation (non-inline functions).
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

#include "Unlimited_Sparse_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::Unlimited_Sparse_Row::Unlimited_Sparse_Row()
  : data() {
  PPL_ASSERT(OK());
}

PPL::Unlimited_Sparse_Row::Unlimited_Sparse_Row(const std::vector<data_type>&
                                                v)
  : data() {
  typedef std::vector<Coefficient>::size_type vec_size_type;

  for (vec_size_type i=0,n=v.size(); i<n; ++i)
    if (v[i] != 0)
      data.push_back(std::make_pair(i,v[i]));
  PPL_ASSERT(OK());
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::reset(iterator i) {
  iterator res = iterator(data.erase(i.itr));
  PPL_ASSERT(OK());
  return res;
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::reset(iterator first,iterator last) {
  iterator res = iterator(data.erase(first.itr,last.itr));
  PPL_ASSERT(OK());
  return res;
}

void
PPL::Unlimited_Sparse_Row::reset_after(key_type i) {
  data.erase(lower_bound(i).itr,data.end());
  PPL_ASSERT(OK());
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::begin() {
  return iterator(data.begin());
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::end() {
  return iterator(data.end());
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::begin() const {
  return data.begin();
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::end() const {
  return data.end();
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::find(const key_type &k) {
  return std::find_if(begin(),end(),std::bind2nd(
    value_key_compare(std::equal_to<key_type>()),k));
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::lower_bound(const key_type &k) {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<key_type>()));
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::upper_bound(const key_type &k) {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<key_type>()));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::find(const key_type &k) const {
  return std::find_if(begin(),end(),std::bind2nd(
    value_key_compare(std::equal_to<key_type>()),k));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::lower_bound(const key_type &k) const {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<key_type>()));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::upper_bound(const key_type &k) const {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<key_type>()));
}

bool
PPL::Unlimited_Sparse_Row::operator==(const Unlimited_Sparse_Row &x) const {
  const_iterator i = begin();
  const_iterator j = x.begin();
  const_iterator i_end = end();
  const_iterator j_end = x.end();

  while (i != i_end && j != j_end) {
    if (i->first == j->first) {
      if (i->second != j->second)
        return false;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        if (i->second != 0)
          return false;
        i++;
      } else {
        // i->first > j->first
        if (j->second != 0)
          return false;
        j++;
      }
  }
  return true;
}

bool
PPL::Unlimited_Sparse_Row::operator!=(const Unlimited_Sparse_Row &x) const {
  return !((*this) == x);
}

bool
PPL::Unlimited_Sparse_Row::OK() const {
  if (begin() == end())
    return true;
  const_iterator previous = begin();
  const_iterator i = begin();
  ++i;
  const_iterator i_end = end();
  for (; i != i_end; ++i,++previous)
    if (previous->first >= i->first)
      return false;
  return true;
}


PPL::Unlimited_Sparse_Row::iterator::iterator()
  : itr() {
}

std::pair<const PPL::Unlimited_Sparse_Row::key_type,
          PPL::Unlimited_Sparse_Row::data_type&>
PPL::Unlimited_Sparse_Row::iterator::operator*() {
  return std::pair<const Unlimited_Sparse_Row::key_type,
                   Unlimited_Sparse_Row::data_type&>(itr->first,itr->second);
}

PPL::Unlimited_Sparse_Row::iterator&
PPL::Unlimited_Sparse_Row::iterator::operator++() {
  ++itr;
  return *this;
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::iterator::operator++(int) {
  iterator x(*this);
  ++(*this);
  return x;
}

PPL::Unlimited_Sparse_Row::iterator&
PPL::Unlimited_Sparse_Row::iterator::operator--() {
  --itr;
  return *this;
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::iterator::operator--(int) {
  iterator x(*this);
  --(*this);
  return x;
}

bool
PPL::Unlimited_Sparse_Row::iterator::operator==(const iterator &x) const {
  return itr == x.itr;
}

bool
PPL::Unlimited_Sparse_Row::iterator::operator!=(const iterator &x) const {
  return !((*this) == x);
}

PPL::Unlimited_Sparse_Row::iterator::operator
  Unlimited_Sparse_Row::const_iterator() {
  return static_cast<Unlimited_Sparse_Row::const_iterator>(itr);
}

PPL::Unlimited_Sparse_Row::iterator::iterator(list_t::iterator i)
  : itr(i) {
}
