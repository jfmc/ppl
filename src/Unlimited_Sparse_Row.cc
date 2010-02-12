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

const PPL::Coefficient
PPL::Unlimited_Sparse_Row::zero = 0;

PPL::Unlimited_Sparse_Row::Unlimited_Sparse_Row()
  : data() {
  PPL_ASSERT(OK());
}

PPL::Unlimited_Sparse_Row::Unlimited_Sparse_Row(const
                                                std::vector<Coefficient>& v)
  : data() {
  typedef std::vector<Coefficient>::size_type vec_size_type;

  for (vec_size_type i=0,n=v.size(); i<n; ++i)
    if (v[i] != 0)
      data.push_back(std::make_pair(i,v[i]));
  PPL_ASSERT(OK());
}

void
PPL::Unlimited_Sparse_Row::swap(Unlimited_Sparse_Row& x) {
  data.swap(x.data);
  PPL_ASSERT(OK());
  PPL_ASSERT(x.OK());
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::reset(iterator i) {
  iterator res = data.erase(i);
  PPL_ASSERT(OK());
  return res;
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::reset(iterator first,iterator last) {
  iterator res = data.erase(first,last);
  PPL_ASSERT(OK());
  return res;
}

void
PPL::Unlimited_Sparse_Row::reset_after(dimension_type i) {
  data.erase(lower_bound(i),end());
  PPL_ASSERT(OK());
}

PPL::Coefficient&
PPL::Unlimited_Sparse_Row::operator[](const dimension_type i) {
  iterator itr = lower_bound(i);
  if (itr != end())
    if (itr->first == i)
      return itr->second;

  data.insert(itr,std::make_pair(i,Coefficient(0)));
  --itr;
  return itr->second;
}

const PPL::Coefficient&
PPL::Unlimited_Sparse_Row::operator[](const dimension_type i) const {
  return get(i);
}

const PPL::Coefficient&
PPL::Unlimited_Sparse_Row::get(const dimension_type i) const {
  const_iterator itr = find(i);
  if (itr == end())
    return Unlimited_Sparse_Row::zero;
  else
    return itr->second;
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::begin() {
  return data.begin();
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::end() {
  return data.end();
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
PPL::Unlimited_Sparse_Row::find(const dimension_type k) {
  return std::find_if(begin(),end(),std::bind2nd(
    value_key_compare(std::equal_to<dimension_type>()),k));
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::lower_bound(const dimension_type k) {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

PPL::Unlimited_Sparse_Row::iterator
PPL::Unlimited_Sparse_Row::upper_bound(const dimension_type k) {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<dimension_type>()));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::find(const dimension_type k) const {
  return std::find_if(begin(),end(),std::bind2nd(
    value_key_compare(std::equal_to<dimension_type>()),k));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::lower_bound(const dimension_type k) const {
  return std::lower_bound(begin(),end(),k,
                          value_key_compare(std::less<dimension_type>()));
}

PPL::Unlimited_Sparse_Row::const_iterator
PPL::Unlimited_Sparse_Row::upper_bound(const dimension_type k) const {
  return std::upper_bound(begin(),end(),k,
                          key_value_compare(std::less<dimension_type>()));
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

void
PPL::Unlimited_Sparse_Row::ascii_dump(std::ostream& s) const {
  dimension_type n_elements=0;
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i)
    if (i->second != 0)
      ++n_elements;
  s << "elements " << n_elements << ' ';
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i)
    if (i->second != 0)
      s << "[ " << i->first << " ]= " << i->second << ' ';
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Unlimited_Sparse_Row)

bool
PPL::Unlimited_Sparse_Row::ascii_load(std::istream& s) {
  reset_after(0);
  std::string str;
  dimension_type n_elements;
  dimension_type current_key;
  Coefficient current_data;

  if (!(s >> str) || str!="elements")
    return false;

  if (!(s >> n_elements))
    return false;

  for (dimension_type i = 0; i < n_elements; ++i) {
    if (!(s >> str) || str!="[")
      return false;
    if (!(s >> current_key))
      return false;
    if (!(s >> str) || str!="]=")
      return false;
    if (!(s >> current_data))
      return false;
    data.push_back(std::make_pair(current_key,current_data));
  }
  PPL_ASSERT(OK());
  return true;
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

void
std::swap(PPL::Unlimited_Sparse_Row& x, PPL::Unlimited_Sparse_Row& y) {
  x.swap(y);
}
