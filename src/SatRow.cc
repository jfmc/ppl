/* SatRow class implementation (non-inline functions).
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>

#include "SatRow.defs.hh"

#if OUTLINE
#include "SatRow.inlines.hh"
#endif

#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

std::ostream&
PPL::operator <<(std::ostream& s, const SatRow& r) {
  for (int i = 0, last = r.last(); i <= last; ++i)
    s << (r[i] ? "1" : "0") << " ";
  s << "0 ...";
  return s;
}


#ifndef NDEBUG
bool
PPL::SatRow::OK() const {
  using std::endl;
  using std::cerr;

  if (!vec.OK()) {
    cerr << "Corrupted SatRow!"
	 << endl;
    return false;
  }
  return true;
}
#endif
