/* Result supporting functions implementation: inline functions.
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

#ifndef PPL_Result_inlines_hh
#define PPL_Result_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

inline Result
type(Result r) {
  return static_cast<Result>(r & V_TYPE_MASK);
}

inline bool
is_special(Result r) {
  return type(r) != V_NORMAL;
}

inline Result
sign(Result r) {
  switch (r) {
  case V_LT:
  case V_EQ:
  case V_GT:
  case V_UNKNOWN:
    return r;
  case V_MINUS_INFINITY:
    return V_LT;
  case V_PLUS_INFINITY:
    return V_GT;
  default:
    assert(false);
    return V_UNKNOWN;
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Result_inlines_hh)
