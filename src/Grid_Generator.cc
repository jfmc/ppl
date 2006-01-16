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
#include <sstream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid_Generator::throw_invalid_argument(const char* method,
					    const char* reason) const {
  std::ostringstream s;
  s << "PPL::Grid_Generator::" << method << ":" << std::endl
    << reason << ".";
  throw std::invalid_argument(s.str());
}

PPL::Grid_Generator
PPL::Grid_Generator::parameter(const Linear_Expression& e,
			       Coefficient_traits::const_reference d) {
  if (d == 0)
    throw std::invalid_argument("PPL::parameter(e, d):\n"
				"d == 0.");
  Linear_Expression ec(e,
		       e.space_dimension() + 2 /* parameter divisor */);
  Generator g(ec, Generator::RAY, NECESSARILY_CLOSED);
  g[0] = 0;
  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(g);
  gg.divisor() = d;

  // If the divisor is negative, negate it and all the coefficients of
  // the parameter.  This ensures that divisors are always positive.
  if (d < 0)
    for (dimension_type i = gg.size(); i-- > 0; )
      neg_assign(gg[i]);

  return gg;
}

PPL::Grid_Generator
PPL::Grid_Generator::point(const Linear_Expression& e,
			   Coefficient_traits::const_reference d) {
  if (d == 0)
    throw std::invalid_argument("PPL::grid_point(e, d):\n"
				"d == 0.");
  Linear_Expression ec(e,
		       e.space_dimension() + 2 /* parameter divisor */);
  Generator g(ec, Generator::POINT, NECESSARILY_CLOSED);
  g[0] = d;
  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(g);

  // If the divisor is negative, negate it and all the coefficients of
  // the parameter.  This ensures that divisors are always positive.
  if (d < 0)
    for (dimension_type i = gg.size(); i-- > 0; )
      neg_assign(gg[i]);

  // Enforce normalization.
  gg.normalize();
  return gg;
}

PPL::Grid_Generator
PPL::Grid_Generator::line(const Linear_Expression& e) {
  // The origin of the space cannot be a line.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::grid_line(e):\n"
				"e == 0, but the origin cannot be a line.");

  Linear_Expression ec(e,
		       e.space_dimension() + 2 /* parameter divisor */);
  Generator g(ec, Generator::LINE, NECESSARILY_CLOSED);
  g[0] = 0;
  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(g);

  // Enforce normalization.
  gg.strong_normalize();
  return gg;
}

void
PPL::Grid_Generator::coefficient_swap(Grid_Generator& y) {
  // Swap one coefficient at a time into *this.  Doing this instead of
  // swapping the entire row ensures that the row keeps the same
  // capacity.
  if (y.is_line())
    set_is_line();
  else
    set_is_ray_or_point();
  for (dimension_type j = size(); j-- > 0; )
    std::swap(operator[](j), y[j]);
}

bool
PPL::Grid_Generator::is_equivalent_to(const Grid_Generator& y) const {
  const Grid_Generator& x = *this;
  dimension_type x_space_dim = x.space_dimension();
  if (x_space_dim != y.space_dimension())
    return false;

  const Type x_type = x.type();
  if (x_type != y.type())
    return false;

  Grid_Generator tem(*this);
  Grid_Generator tem_y(y);
  dimension_type& last = x_space_dim;
  ++last;
  if (x_type == POINT || x_type == LINE) {
    tem[last] = 0;
    tem_y[last] = 0;
  }
  // Normalize the copies, including the divisor column.
  tem.Row::normalize();
  tem_y.Row::normalize();
  // Check for equality.
  while (last-- > 0)
    if (tem[last] != tem_y[last])
      return false;
  return true;
}

bool
PPL::Grid_Generator::is_equal_to(const Grid_Generator& y) const {
  if (type() != y.type())
    return false;
  for (dimension_type col = (is_parameter() ? size() : size() - 1);
       col-- > 0; )
    if (Generator::operator[](col) != y.Generator::operator[](col))
      return false;
  return true;
}

bool
PPL::Grid_Generator::all_homogeneous_terms_are_zero() const {
  for (dimension_type i = size() - 1 /* parameter divisor */; --i > 0; )
    if (operator[](i) != 0)
      return false;
  return true;
}

void
PPL::Grid_Generator::scale_to_divisor(Coefficient_traits::const_reference d) {
  if (is_parameter_or_point()) {
    if (d == 0)
      throw std::invalid_argument("PPL::Grid_Generator::scale_to_divisor(d):\n"
				  "d == 0.");

    TEMP_INTEGER(factor);
    factor = d / divisor();
    divisor() = d;
    assert(factor > 0);
    if (factor > 1)
      for (dimension_type col = size() - 2; col >= 1; --col)
	Generator::operator[](col) *= factor;
  }
}

/*! \relates Parma_Polyhedra_Library::Grid_Generator */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Grid_Generator& g) {
  bool need_divisor = false;
  bool extra_parentheses = false;
  const int num_variables = g.space_dimension();
  Grid_Generator::Type t = g.type();
  switch (t) {
  case Grid_Generator::LINE:
    s << "l(";
    break;
  case Grid_Generator::PARAMETER:
    s << "r(";
    if (g[num_variables + 1] == 1)
      break;
    goto any_point;
  case Grid_Generator::POINT:
    s << "p(";
    if (g[0] > 1) {
    any_point:
      need_divisor = true;
      int num_non_zero_coefficients = 0;
      for (int v = 0; v < num_variables; ++v)
	if (g[v+1] != 0)
	  if (++num_non_zero_coefficients > 1) {
	    extra_parentheses = true;
	    s << "(";
	    break;
	  }
    }
    break;
  }

  bool first = true;
  for (int v = 0; v < num_variables; ++v) {
    Coefficient gv = g[v+1];
    if (gv != 0) {
      if (!first) {
	if (gv > 0)
	  s << " + ";
	else {
	  s << " - ";
	  neg_assign(gv);
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
    // A generator in the origin.
    s << 0;
  if (extra_parentheses)
    s << ")";
  if (need_divisor)
    s << "/" << g.divisor();
  s << ")";
  return s;
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
    if (operator[](0) != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of lines must be zero!"
		<< std::endl;
#endif
      return false;
    }
    break;

  case Grid_Generator::PARAMETER:
    if (operator[](0) != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of parameters must be zero!"
		<< std::endl;
#endif
      return false;
    }
    // Fall through.

  case Grid_Generator::POINT:
    if (divisor() <= 0) {
#ifndef NDEBUG
      std::cerr << "Points and parameters must have positive divisors!"
		<< std::endl;
#endif
      return false;
    }
    break;

  }

  // All tests passed.
  return true;
}

PPL_OUTPUT_DEFINITIONS(Grid_Generator);
