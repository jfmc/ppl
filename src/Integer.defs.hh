/* Integer class declaration.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Integer_defs_hh
#define _Integer_defs_hh 1

#include "Integer.types.hh"

namespace Parma_Polyhedra_Library {

  inline void negate(Integer& x);
  inline void gcd_assign(Integer& x, const Integer& y);
  inline void gcd_assign(Integer& x, const Integer& y, const Integer& z);
  inline void exact_div_assign(Integer& x, const Integer& y);
  inline void exact_div_assign(Integer& x, const Integer& y, const Integer& z);
  inline int cmp(const Integer& x, const Integer& y);
  inline const Integer& Integer_zero();
  inline const Integer& Integer_one();

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::Integer& x,
	  Parma_Polyhedra_Library::Integer& y);

} // namespace std

#include "Integer.inlines.hh"

#endif
