/* Declarations of global objects.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef _globals_hh
#define _globals_hh 1

#include "Integer.types.hh"

namespace Parma_Polyhedra_Library {

inline Integer& tmp_Integer(size_t n) {
  static Integer tmp[6];
  return tmp[n];
}

//! Speculative allocation function.
/*!
  \param requested_size   The number of elements we need.

  \return                 The actual capacity to be allocated.

  Computes a capacity given a requested size.
  Allows for speculative allocation aimed at reducing the number of
  reallocations.
*/
inline size_t
compute_capacity(size_t requested_size) {
  return 2*(requested_size + 1);
}

} // namespace Parma_Polyhedra_Library

#endif
