/* Unlimited_Sparse_Row_Std_List_Backend class implementation
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

#include "Unlimited_Sparse_Row_Std_List_Backend.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::memory_size_type
PPL::Unlimited_Sparse_Row_Std_List_Backend::external_memory_in_bytes()
  const {
  memory_size_type total=0;
  for (const_iterator i=begin(),i_end=end(); i!=i_end; ++i) {
    // Pointers to the previous and next element.
    total += 2*sizeof(void*);
    // Index
    total += sizeof(dimension_type);
    // Size of the Coefficient.
    total += PPL::total_memory_in_bytes((*i).second);
  }
  // Assume implementation of std::list uses one additional element
  // at the beginning and at the end of the list.
  total += 2*(2*sizeof(void*)+sizeof(value_type));
  return total;
}
