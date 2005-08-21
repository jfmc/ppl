/* Implementation of class PFunction (non-inline functions).
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

#include "globals.defs.hh"
#include "Variable.defs.hh"
#include "PFunction.hh"

#include <stdexcept>
#include <iostream>

void
PFunction::insert(dim_t x, dim_t y) {
 std::pair<Map::iterator, bool> stat = map.insert(Map::value_type(x, y));
 if (!stat.second)
   throw std::runtime_error("PFunction::insert(x, y) called"
			    " with `x' already in domain");
 if (y > max)
   max = y;
}

PFunction::dim_t
PFunction::max_in_codomain() const {
  if (has_empty_codomain())
    throw std::runtime_error("PFunction::max_in_codomain() called"
			     " when has_empty_codomain()");
  return max;
}

bool
PFunction::maps(dim_t x, dim_t& y) const {
  if (has_empty_codomain())
    throw std::runtime_error("PFunction::maps() called"
			     " when has_empty_codomain()");
  Map::const_iterator i = map.find(x);
  if (i != map.end()) {
    y = (*i).second;
    return true;
  }
  else
    return false;
}

void
PFunction::print(std::ostream& s) const {
  using namespace Parma_Polyhedra_Library;
  using namespace IO_Operators;

  if (has_empty_codomain())
    s << "empty" << std::endl;
  else
    for (Map::const_iterator i = map.begin(),
	   map_end = map.end(); i != map_end; ++i)
      s << Variable((*i).first) << " --> "
	<< Variable((*i).second)
	<< std::endl;
}
