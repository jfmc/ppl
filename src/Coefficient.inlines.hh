/* Coefficient class implementation: inline functions.
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

#ifndef PPL_Coefficient_inlines_hh
#define PPL_Coefficient_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline const Coefficient&
Coefficient_zero() {
  static Coefficient z(0);
  return z;
}

inline const Coefficient&
Coefficient_one() {
  static Coefficient o(1);
  return o;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Coefficient_inlines_hh)
