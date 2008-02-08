/* Declarations for the Box instantiations offered by the foreign interfaces.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_interfaced_boxes_hh
#define PPL_interfaced_boxes_hh 1

namespace Parma_Polyhedra_Library {

struct Z_Box_Interval_Info_Policy {
  const_bool_nodef(store_special, true);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef
Interval_Restriction_None<Interval_Info_Bitset
                          <unsigned int,
                           Z_Box_Interval_Info_Policy> >
Z_Box_Interval_Info;

typedef Box<Interval<mpz_class, Z_Box_Interval_Info> > Z_Box;

struct Floating_Point_Box_Interval_Info_Policy {
  const_bool_nodef(store_special, false);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef
Interval_Restriction_None<Interval_Info_Bitset
                          <unsigned int,
                           Floating_Point_Box_Interval_Info_Policy> >
Floating_Point_Box_Interval_Info;

typedef
Box<Interval<float, Floating_Point_Box_Interval_Info> >
Float_Box;

typedef Box<Interval<double, Floating_Point_Box_Interval_Info> >
Double_Box;

typedef Box<Interval<long double, Floating_Point_Box_Interval_Info> >
Long_Double_Box;

/*
int8_t      Int8_Box
int16_t     etc
int32_t
int64_t
uint8_t     UInt8_Box or Uint8_Box
uint16_t    etc
uint32_t
uint64_t
*/

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_interfaced_boxes_hh)
