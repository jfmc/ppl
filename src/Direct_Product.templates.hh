/* Direct_Product class implementation: inline functions.
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

#ifndef PPL_Direct_Product_templates_hh
#define PPL_Direct_Product_templates_hh 1

#include "Interval.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Grid_Generator_System.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::ascii_load(std::istream& s) {
  std::string str;
  return ((s >> str) && str == "Domain"
	  && (s >> str) && str == "1:"
	  && d1.ascii_load(s)
	  && (s >> str) && str == "Domain"
	  && (s >> str) && str == "2:"
	  && d2.ascii_load(s));
}

// FIXME: Move to Open_Product.templates.hh when name of Open_Product
//        decided.

template <typename D1, typename D2>
bool
empty_check_reduce(D1& d1, D2& d2) {
  if (d2.is_empty()) {
    if (d1.is_empty())
      return false;
    D1 new_d1(d1.space_dimension(), EMPTY);
    std::swap(d1, new_d1);
    return true;
  }
  if (d1.is_empty()) {
    D2 new_d2(d2.space_dimension(), EMPTY);
    std::swap(d2, new_d2);
    return true;
  }
  return false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Direct_Product_templates_hh)
