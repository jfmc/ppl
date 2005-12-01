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

void
PPL::Grid_Generator::coefficient_swap(Grid_Generator& y) {
  // Swap one coefficient at a time into *this instead of swapping the
  // entire row.  This ensures that the row keeps the same capacity.
  if (y.is_line())
    set_is_line();
  else
    set_is_ray_or_point();
  for (dimension_type j = size(); j-- > 0; )
    std::swap(operator[](j), y[j]);
}

bool
PPL::Grid_Generator::is_equal_to(const Grid_Generator& y) const {
  if (type() != y.type())
    return false;
  for (dimension_type col = size(); col-- > 0; )
    if (Generator::operator[](col) != y.Generator::operator[](col))
      return false;
  return true;
}

void
PPL::Grid_Generator::multiply(Coefficient_traits::const_reference mult,
			      Coefficient_traits::const_reference div) {
  if (is_parameter_or_point()) {
    TEMP_INTEGER(factor);
    if (is_point()) {
      factor = mult / divisor();
      const_cast<Coefficient&>(divisor()) = mult;
    }
    else {
      if (div == 0)
	return;
      factor = mult / div;
    }
    assert(factor > 0);
    if (factor > 1) {
      dimension_type num_cols = size();
      for (dimension_type col = 1; col < num_cols; ++col)
	Generator::operator[](col) *= factor;
    }
  }
}

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
  if (!is_necessarily_closed()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator Generator should be necessarily closed."
	      << std::endl;
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

  switch (type()) {
  case Grid_Generator::LINE:
    // Intentionally fall through.
  case Grid_Generator::PARAMETER:
    if (divisor() != 0) {
#ifndef NDEBUG
      std::cerr << "Lines and parameters must have a zero inhomogeneous term!"
		<< std::endl;
#endif
      return false;
    }
    break;

  case Grid_Generator::POINT:
    if (divisor() <= 0) {
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
