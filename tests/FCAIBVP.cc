/* Implementation of class FCAIBVP (non-inline functions).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <ppl-config.h>
#include "FCAIBVP.defs.hh"
#include <stdexcept>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const FCAIBVP& x) {
  s << "{";
  for (FCAIBVP::Set::const_iterator i = x.set.begin(),
	 x_end = x.set.end(); i != x_end; ) {
    const Variable& v = Variable(*i++);
#if 0 // Old compilers may not understand the following.
    using IO_Operators::operator<<;
    s << v;
#else
    Parma_Polyhedra_Library::IO_Operators::operator<<(s, v);
#endif
    if (i != x_end)
      s << ", ";
  }
  s << "}";
  return s;
}
