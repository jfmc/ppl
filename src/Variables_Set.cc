/* Variables_Set class implementation (non-inline functions).
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

#include <config.h>
#include "Variables_Set.defs.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

PPL::Variables_Set::Variables_Set(const Variable& v, const Variable& w)
  : Base() {
  for (dimension_type d = v.id(), last = w.id(); d <= last; ++d)
    insert(d);
}

bool
PPL::Variables_Set::OK() const {
  for (const_iterator i = begin(), set_end = end(); i != set_end; ++i)
    if (!Variable(*i).OK())
      return false;
  return true;
}

/*! \relates Parma_Polyhedra_Library::Variables_Set */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Variables_Set& vs) {
  s << '{';
  for (Variables_Set::const_iterator i = vs.begin(),
	 vs_end = vs.end(); i != vs_end; ) {
    s << ' ' << Variable(*i++);
    if (i != vs_end)
      s << ',';
  }
  s << " }";
  return s;
}
