/* Result info for checked functions
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Result_Info_defs_hh
#define PPL_Result_Info_defs_hh 1

namespace Parma_Polyhedra_Library {

/* This is a kind of bit mask (with logical OR semantic), with special
   values for underflow and overflow */ 
enum Result_Info {
  V_EQ = 1,
  V_LT = 2,
  V_GT = 4,
  V_LE = V_EQ | V_LT,
  V_GE = V_EQ | V_GT,
  V_NE = V_LT | V_GT,
  V_APPROX = V_LT | V_EQ | V_GT,
  V_NAN = 0,
  V_NEG_OVERFLOW = 8,
  V_POS_OVERFLOW = 9
};

} // namespace std
 
#endif // !defined(PPL_Result_Info_defs_hh)
