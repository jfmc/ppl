/* Variable class implementation: inline functions.
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

#ifndef _Variable_inlines_hh
#define _Variable_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Variable::Variable(unsigned int i)
  : varid(i) {
}

inline unsigned int
Variable::id() const {
  return varid;
}

/*! \relates Variable */
inline bool
operator<(const Variable& v, const Variable& w) {
  return v.id() < w.id();
}

} // namespace Parma_Polyhedra_Library

#endif // _Variable_inlines_hh
