/* Implementation of global objects: inline functions.
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

#ifndef PPL_globals_inlines_hh
#define PPL_globals_inlines_hh 1

#include "Integer.defs.hh"

namespace Parma_Polyhedra_Library {

inline void
normalize2(const Integer& x, const Integer& y, Integer& nx, Integer& ny) {
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, x, y);
  exact_div_assign(nx, x, gcd);
  exact_div_assign(ny, y, gcd);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_globals_inlines_hh)
