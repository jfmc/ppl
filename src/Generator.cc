/* Generator class implementation (non-inline functions).
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include <sstream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Generator::throw_dimension_incompatible(const char* method,
					     PPL::Variable v) const {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "this->space_dimension() == " << this->space_dimension()
    << ", v.id() == " << v.id();
  throw std::invalid_argument(s.str());
}

PPL::Generator
PPL::vertex(const LinExpression& e, const Integer& d) {
  if (d == 0)
    throw std::invalid_argument("Generator PPL::vertex(e, d): d == 0");
  LinExpression ec = e;
  Generator g(ec);
  g[0] = d;

  // If the denominator is negative, we multiply the vertex for
  // -1, because we want that the denominator is always positive.
  if (d < 0)
    for (size_t i = g.size(); i-- > 0; )
      negate(g[i]);

  g.set_is_ray_or_vertex();
  return g;
}

PPL::Generator
PPL::ray(const LinExpression& e) {
  // The origin of the space cannot be a ray.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::ray(e): the origin cannot be a ray");

  LinExpression ec = e;
  Generator g(ec);
  g[0] = 0;
  g.set_is_ray_or_vertex();
  return g;
}

PPL::Generator
PPL::line(const LinExpression& e) {
  // The origin of the space cannot be a line.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::line(e): the origin cannot be a line");

  LinExpression ec = e;
  Generator g(ec);
  g[0] = 0;
  g.set_is_line();
  return g;
}

std::ostream&
PPL::operator<<(std::ostream& s, const Generator& g) {
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
	  negate(gv);
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
    s << 0;
  if (extra_parentheses)
    s << ")";
  if (vertex_with_divisor)
    s << "/" << g[0];
  s << ")";
  return s;
}

bool
PPL::Generator::OK() const {
  using std::endl;
  using std::cerr;

  const Generator& g = *this;
  bool ray_or_line = false;
  // Looking for vertices.
  if (g.is_ray_or_vertex()) {
    // A vertex is legal only if its inhomogeneous term
    // is strictly positive.
    if (g[0] < 0) {
      cerr << "Vertices cannot have a negative inhomogeneous term!"
	   << endl;
      return false;
    }
    else if (g[0] == 0)
      // Since rays and lines have a zero inhomogeneous term,
      // we found a ray.
      ray_or_line = true;
  }
  else if (g[0] != 0) {
    cerr << "Lines must have a zero inhomogeneous term!"
	 << endl;
    return false;
  }
  else
    // We found a line.
    ray_or_line = true;

  if (ray_or_line && g.all_homogeneous_terms_are_zero()) {
    // By definition, the origin of the space cannot be a ray or a line.
    cerr << "The origin of the vector space cannot be a ";
    if (g.is_line())
      cerr << "line.";
    else
      cerr << "ray.";
    cerr << endl;
    return false;
  }
  return true;
}
