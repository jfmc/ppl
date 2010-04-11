/* Unlimited_Sparse_Row_Over_CO_Tree class implementation
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

#include "Unlimited_Sparse_Row_Over_CO_Tree.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Unlimited_Sparse_Row_Over_CO_Tree
::ascii_dump(std::ostream& s) const {
  dimension_type n_elements=0;
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    ++n_elements;
  s << "elements " << n_elements << ' ';
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    s << "[ " << i->first << " ]= " << i->second << ' ';
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Unlimited_Sparse_Row_Over_CO_Tree)

bool
PPL::Unlimited_Sparse_Row_Over_CO_Tree
::ascii_load(std::istream& s) {
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
    tree.insert(current_key, current_data);
  }
  PPL_ASSERT(OK());
  return true;
}
