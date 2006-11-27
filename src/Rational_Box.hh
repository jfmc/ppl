/* Rational_Box class declaration and implementation.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Rational_Box_hh
#define PPL_Rational_Box_hh 1

#include "Box.defs.hh"

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A box with rational boundaries.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

namespace Parma_Polyhedra_Library {

struct Rational_Box_Interval_Info_Policy {
  const_bool_nodef(store_special, true);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, false);
  const_bool_nodef(may_be_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
  const_bool_nodef(infinity_is_open, false);
};

typedef Interval_No_Restrictions<Interval_Info_Bitset<unsigned int, Rational_Box_Interval_Info_Policy> > Rational_Box_Interval_Info;

typedef Box<Interval<mpq_class, Rational_Box_Interval_Info> > Rational_Box;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Rational_Box_hh)
