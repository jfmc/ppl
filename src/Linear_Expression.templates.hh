/* Linear_Expression class implementation: non-inline template functions.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Linear_Expression_templates_hh
#define PPL_Linear_Expression_templates_hh 1

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
template <typename OStream>
OStream&
operator<<(OStream& s, const Linear_Expression& e) {
  const dimension_type num_variables = e.space_dimension();
  TEMP_INTEGER(ev);
  bool first = true;
  for (dimension_type v = 0; v < num_variables; ++v) {
    ev = e[v+1];
    if (ev != 0) {
      if (!first) {
	if (ev > 0)
	  s << " + ";
	else {
	  s << " - ";
	  neg_assign(ev);
	}
      }
      else
	first = false;
      if (ev == -1)
	s << "-";
      else if (ev != 1)
	s << ev << "*";
      s << Variable(v);
    }
  }
  // Inhomogeneous term.
  TEMP_INTEGER(it);
  it = e[0];
  if (it != 0) {
    if (!first) {
      if (it > 0)
	s << " + ";
      else {
	s << " - ";
	neg_assign(it);
      }
    }
    else
      first = false;
    s << it;
  }

  if (first)
    // The null linear expression.
    s << Coefficient_zero();
  return s;
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Linear_Expression_templates_hh)
