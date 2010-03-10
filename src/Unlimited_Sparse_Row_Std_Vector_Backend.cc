/* Unlimited_Sparse_Row_Std_Vector_Backend class implementation
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

#include "Unlimited_Sparse_Row_Std_Vector_Backend.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::memory_size_type
PPL::Unlimited_Sparse_Row_Std_Vector_Backend::external_memory_in_bytes()
  const {
  return 2*capacity();
}

void
PPL::Unlimited_Sparse_Row_Std_Vector_Backend::
  swap_vector_chunks(Base& A,dimension_type i,
                     dimension_type n,dimension_type m) {
  PPL_ASSERT((i+n+m) <= A.size());
  dimension_type k;
  Base::iterator itr = A.begin() + i;
  Base::iterator itr2 = itr + n;
  while (n==0 || m==0) {
    if (n <= m) {
      for (k=n; k>0; --k,++itr,++itr2)
        std::swap(*itr,*itr2);
      m -= n;
    } else {
      for (k=m; k>0; --k,++itr,++itr2)
        std::swap(*itr,*itr2);
      n -= m;
      itr2 = itr + n;
    }
  }
}
