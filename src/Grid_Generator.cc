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

bool
PPL::Grid_Generator::OK() const {
  const Grid_Generator& g = *this;

  if (!is_necessarily_closed()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator should be necessarily closed." << std::endl;
#endif
    return false;
  }

  // Topology consistency check.
  if (size() < 1) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator has fewer coefficients than the minimum "
	      << "allowed:" << std::endl
	      << "size is " << size() << ", minimum is 1." << std::endl;
#endif
    return false;
  }

  switch (g.type()) {
  case Grid_Generator::LINE:
    // Intentionally fall through.
  case Grid_Generator::PARAMETER:
    if (g[0] != 0) {
#ifndef NDEBUG
      std::cerr << "Lines and parameters must have a zero inhomogeneous term!"
		<< std::endl;
#endif
      return false;
    }
    break;

  case Grid_Generator::POINT:
    if (g[0] <= 0) {
#ifndef NDEBUG
      std::cerr << "Points must have a positive divisor!"
		<< std::endl;
#endif
      return false;
    }
    break;

  }

  // All tests passed.
  return true;
}
