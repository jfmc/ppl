/* Generator class implementation (non-inline functions).
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

#include "Generator.defs.hh"

#include "Variable.defs.hh"
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Generator::throw_zero_denominator_vertex() {
  throw std::invalid_argument("Generator PPL::vertex(e, d): d == 0");
}

void
PPL::Generator::throw_zero_dim_ray() {
  throw std::invalid_argument("Generator PPL::ray(e): dim(e) == 0");
}

void
PPL::Generator::throw_zero_dim_line() {
  throw std::invalid_argument("Generator PPL::line(e): dim(e) == 0");
}

std::ostream&
PPL::operator <<(std::ostream& s, const Generator& g) {
  bool vertex_with_divisor = false;
  bool extra_parentheses = false;
  int num_variables = g.size()-1;
  if (g.is_line())
    s << "l(";
  else if (g[0] == 0)
    s << "r(";
  else {
    s << "v(";
    if (g[0] != 1) {
      vertex_with_divisor = true;
      int num_non_zero_coefficients = 0;
      for (int v = 0; v < num_variables; ++v)
	if (g[v+1] != 0)
	  if (++num_non_zero_coefficients > 1) {
	    extra_parentheses = true;
	    s << "(";
	    break;
	  }
    }
  }
  bool first = true;
  for (int v = 0; v < num_variables; ++v) {
    Integer gv = g[v+1];
    if (gv != 0) {
      if (!first) {
	if (gv > 0)
	  s << " + ";
	else {
	  s << " - ";
	  gv.negate();
	}
      }
      else
	first = false;
      if (gv == -1)
	s << "-";
      else if (gv != 1)
	s << gv << "*";
      s << PPL::Variable(v);
    }
  }
  if (first)
    // A vertex in the origin.
    s << Integer::zero;
  if (extra_parentheses)
    s << ")";
  if (vertex_with_divisor)
    s << "/" << g[0];
  s << ")";
  return s;
}
