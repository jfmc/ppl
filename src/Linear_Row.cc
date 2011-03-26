/* Linear_Row class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#include "Linear_Row.defs.hh"
#include "Coefficient.defs.hh"
#include <algorithm>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

bool
PPL::Linear_Row::is_zero() const {
  const Linear_Row& x = *this;
  for (dimension_type i = x.get_row().size(); i-- > 0; )
    if (x.get_row()[i] != 0)
      return false;
  return true;
}

bool
PPL::Linear_Row::all_homogeneous_terms_are_zero() const {
  const Linear_Row& x = *this;
  for (dimension_type i = x.get_row().size(); --i > 0; )
    if (x.get_row()[i] != 0)
      return false;
  return true;
}

void
PPL::Linear_Row::ascii_dump(std::ostream& s) const {
  const Dense_Row& x = get_row();
  const dimension_type x_size = x.size();
  s << "size " << x_size << " ";
  for (dimension_type i = 0; i < x_size; ++i)
    s << x[i] << '\n';
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Linear_Row)

bool
PPL::Linear_Row::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  Dense_Row& x = get_row();
  const dimension_type old_size = x.size();
  if (new_size < old_size)
    x.shrink(new_size);
  else if (new_size > old_size) {
    Dense_Row y(new_size);
    x.swap(y);
  }

  for (dimension_type col = 0; col < new_size; ++col)
    if (!(s >> x[col]))
      return false;

  return true;
}

bool
PPL::Linear_Row::OK() const {
  return get_row().OK();
}

bool
PPL::Linear_Row::OK(const dimension_type row_size) const {
  return get_row().OK(row_size);
}
