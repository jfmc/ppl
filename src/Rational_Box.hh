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

namespace Implementation {

struct Rational_Box_Interval_Info_Policy {
  static const bool store_unbounded = true;
  static const bool store_open = true;
  static const bool store_integer = false;
  static const bool store_empty = true;
  static const bool store_singleton = true;
  static const unsigned int next_bit = 0;
  static const bool handle_infinity = false;
  static const bool check_inexact = false;
  static const bool check_empty_args = false;
  static const bool check_integer_args = false;
};

typedef Interval_Info_Bitset<unsigned int, Rational_Box_Interval_Info_Policy>
Rational_Box_Interval_Info;

typedef Box<Interval<mpq_class, Rational_Box_Interval_Info> > Rational_Box;

} // namespace Implementation

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Rational_Box_hh)
