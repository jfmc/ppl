/* Unlimited_Sparse_Row_Custom_Slist_Backend class implementation
   (non-inline functions).
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

#include "Unlimited_Sparse_Row_Custom_Slist_Backend.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Unlimited_Sparse_Row_Custom_Slist_Backend::clear() {
  while (!empty())
    pop_front();
  PPL_ASSERT(OK());
}

PPL::Unlimited_Sparse_Row_Custom_Slist_Backend&
PPL::Unlimited_Sparse_Row_Custom_Slist_Backend::operator=(const This& x) {
  if (this == &x)
    return *this;
  clear();
  for (const_iterator i=x.begin(),i_end=x.end(); i!=i_end; ++i)
    push_back(*i);
  PPL_ASSERT(OK());
  PPL_ASSERT(*this == x);
  return *this;
}

bool
PPL::Unlimited_Sparse_Row_Custom_Slist_Backend::operator==(const This& x)
  const {
  const_iterator i = begin();
  const_iterator i_end = end();
  const_iterator j = x.begin();
  const_iterator j_end = x.end();
  for ( ; i!=i_end && j!=j_end; ++i,++j)
    if (*i != *j)
      return false;
  return (i == i_end) && (j == j_end);
}

PPL::memory_size_type
PPL::Unlimited_Sparse_Row_Custom_Slist_Backend::external_memory_in_bytes()
  const {
  dimension_type count=0;
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i)
    ++count;
  return count*sizeof(list_elem);
}

bool
PPL::Unlimited_Sparse_Row_Custom_Slist_Backend::OK() const {
  if (last == 0)
    return false;
  if (*last != 0)
    return false;
  list_elem* const* real_last = &first;
  while (*real_last != 0)
    real_last = &((*real_last)->next);
  return (last == real_last);
}
