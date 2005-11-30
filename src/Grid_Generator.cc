/* Grid_Generator class implementation (non-inline functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Grid_Generator.defs.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

/*! \relates Parma_Polyhedra_Library::Grid_Generator */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s,
			      const Grid_Generator::Type& t) {
  const char* n = 0;
  switch (t) {
  case Grid_Generator::LINE:
    n = "LINE";
    break;
  case Grid_Generator::PARAMETER:
    n = "PARAMETER";
    break;
  case Generator::POINT:
    n = "POINT";
    break;
  }
  s << n;
  return s;
}
