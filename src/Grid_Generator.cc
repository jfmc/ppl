/* Grid_Generator class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Grid_Generator.defs.hh"
#include <iostream>
#include <sstream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Grid_Generator::throw_dimension_incompatible(const char* method,
                                                  const char* name_var,
                                                  const Variable v) const {
  std::ostringstream s;
  s << "PPL::Grid_Generator::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << name_var << ".space_dimension() == " << v.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

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
  // Add 2 to space dimension to allow for parameter divisor column.
  const dimension_type gg_size = 2 + e.space_dimension();
  Linear_Expression ec(e, gg_size);

  ec[0] = 0;
  ec[gg_size - 1] = d;

  // If the divisor is negative, negate it and all the coefficients of
  // the parameter, so as to satisfy the invariant.
  if (d < 0)
    for (dimension_type i = gg_size; i-- > 0; )
      neg_assign(ec[i]);

  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(ec, PARAMETER);

  // NOTE: normalize() must *not* be called here, because this is a parameter,
  // and it would change the represented parameter.
  return gg;
}

PPL::Grid_Generator
PPL::Grid_Generator::grid_point(const Linear_Expression& e,
				Coefficient_traits::const_reference d) {
  if (d == 0)
    throw std::invalid_argument("PPL::grid_point(e, d):\n"
				"d == 0.");
  // Add 2 to space dimension to allow for parameter divisor column.
  Linear_Expression ec(e, 2 + e.space_dimension());
  ec[0] = d;

  // If the divisor is negative, negate it and all the coefficients of
  // the point, so as to satisfy the invariant.
  if (d < 0)
    for (dimension_type i = ec.size(); i-- > 0; )
      neg_assign(ec[i]);

  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(ec, POINT);

  // Enforce normalization.
  gg.normalize();
  return gg;
}

PPL::Grid_Generator
PPL::Grid_Generator::grid_line(const Linear_Expression& e) {
  // The origin of the space cannot be a line.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::grid_line(e):\n"
				"e == 0, but the origin cannot be a line.");

  // Add 2 to space dimension to allow for parameter divisor column.
  Linear_Expression ec(e, 2 + e.space_dimension());
  ec[0] = 0;
  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(ec, LINE);

  // Enforce normalization.
  gg.strong_normalize();
  return gg;
}

void
PPL::Grid_Generator::coefficient_swap(Grid_Generator& y) {
  Grid_Generator& x = *this;
  // Swap one coefficient at a time between x and y.
  // Doing this instead of swapping the entire row ensures that
  // the row keeps the same capacity.
  if (y.is_line())
    x.set_is_line();
  else
    x.set_is_parameter_or_point();
  PPL_ASSERT(x.size() > 0);
  PPL_ASSERT(y.size() > 0);
  dimension_type x_sz = x.size() - 1;
  dimension_type y_sz = y.size() - 1;
  // Swap parameter divisors.
  std::swap(x[x_sz], y[y_sz]);
  // Swap other coefficients.
  for (dimension_type j = (x_sz > y_sz ? y_sz : x_sz); j-- > 0; )
    std::swap(x[j], y[j]);
}

void
PPL::Grid_Generator::ascii_dump(std::ostream& s) const {
  const Grid_Generator& x = *this;
  const dimension_type x_size = x.size();
  s << "size " << x_size << " ";
  for (dimension_type i = 0; i < x_size; ++i)
    s << x[i] << ' ';
  switch (x.type()) {
  case LINE:
    s << "L";
    break;
  case PARAMETER:
    s << "Q";
    break;
  case POINT:
    s << "P";
    break;
  }
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS(Grid_Generator)

bool
PPL::Grid_Generator::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  Dense_Row& x = *this;
  const dimension_type old_size = x.size();
  if (new_size < old_size)
    x.shrink(new_size);
  else if (new_size > old_size) {
    Dense_Row y(new_size, Dense_Row::Flags());
    x.swap(y);
  }

  for (dimension_type col = 0; col < new_size; ++col)
    if (!(s >> x[col]))
      return false;

  if (!(s >> str))
    return false;
  if (str == "L")
    set_is_line();
  else if (str == "P" || str == "Q")
    set_is_parameter_or_point();
  else
    return false;

  return true;
}

void
PPL::Grid_Generator::set_is_parameter() {
  if (is_line())
    set_is_parameter_or_point();
  else if (!is_line_or_parameter()) {
    // The grid generator is a point.
    Grid_Generator& x = *this;
    x[x.size() - 1] = x[0];
    x[0] = 0;
  }
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

  Grid_Generator tmp_x = *this;
  Grid_Generator tmp_y = y;
  dimension_type& last = x_space_dim;
  ++last;
  if (x_type == POINT || x_type == LINE) {
    tmp_x[last] = 0;
    tmp_y[last] = 0;
  }
  // Normalize the copies, including the divisor column.
  tmp_x.Dense_Row::normalize();
  tmp_y.Dense_Row::normalize();
  // Check for equality.
  while (last-- > 0)
    if (tmp_x[last] != tmp_y[last])
      return false;
  return true;
}

bool
PPL::Grid_Generator::is_equal_to(const Grid_Generator& y) const {
  const Grid_Generator& x = *this;
  if (x.type() != y.type())
    return false;
  for (dimension_type col = (x.is_parameter() ? x.size() : x.size() - 1);
       col-- > 0; )
    if (x[col] != y[col])
      return false;
  return true;
}

bool
PPL::Grid_Generator::all_homogeneous_terms_are_zero() const {
  const Grid_Generator& x = *this;
  // Start at size() - 2 to avoid the extra grid generator column.
  // Also avoid the point divisor column (0).
  for (dimension_type i = x.size() - 2; i > 0; --i)
    if (x[i] != 0)
      return false;
  return true;
}

void
PPL::Grid_Generator::scale_to_divisor(Coefficient_traits::const_reference d) {
  PPL_ASSERT(d != 0);
  Grid_Generator& x = *this;
  if (x.is_line())
    return;

  PPL_DIRTY_TEMP_COEFFICIENT(factor);
  exact_div_assign(factor, d, x.divisor());
  x.set_divisor(d);
  PPL_ASSERT(factor > 0);
  if (factor > 1) {
    for (dimension_type i = x.size() - 2; i > 0; --i)
      x[i] *= factor;
  }
}

const PPL::Grid_Generator* PPL::Grid_Generator::zero_dim_point_p = 0;

void
PPL::Grid_Generator::initialize() {
  PPL_ASSERT(zero_dim_point_p == 0);
  zero_dim_point_p = new Grid_Generator(grid_point());
}

void
PPL::Grid_Generator::finalize() {
  PPL_ASSERT(zero_dim_point_p != 0);
  delete zero_dim_point_p;
  zero_dim_point_p = 0;
}

/*! \relates Parma_Polyhedra_Library::Grid_Generator */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Grid_Generator& g) {
  bool need_divisor = false;
  bool extra_parentheses = false;
  const dimension_type num_variables = g.space_dimension();
  Grid_Generator::Type t = g.type();
  switch (t) {
  case Grid_Generator::LINE:
    s << "l(";
    break;
  case Grid_Generator::PARAMETER:
    s << "q(";
    if (g[num_variables + 1] == 1)
      break;
    goto any_point;
  case Grid_Generator::POINT:
    s << "p(";
    if (g[0] > 1) {
    any_point:
      need_divisor = true;
      dimension_type num_non_zero_coefficients = 0;
      for (dimension_type v = 0; v < num_variables; ++v)
	if (g[v+1] != 0)
	  if (++num_non_zero_coefficients > 1) {
	    extra_parentheses = true;
	    s << "(";
	    break;
	  }
    }
    break;
  }

  PPL_DIRTY_TEMP_COEFFICIENT(gv);
  bool first = true;
  for (dimension_type v = 0; v < num_variables; ++v) {
    gv = g[v+1];
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
    // A grid generator in the origin.
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
  case Grid_Generator::POINT:
    n = "POINT";
    break;
  }
  s << n;
  return s;
}

bool
PPL::Grid_Generator::OK() const {
  // Check the underlying Linear_Row object.
  if (!Linear_Row::OK())
    return false;

  // NOTE: do not check for normalization, as it does not hold.
  const Grid_Generator& x = *this;

  if (!x.is_necessarily_closed()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator should be necessarily closed.\n";
#endif
    return false;
  }

  if (x.size() < 2) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator has fewer coefficients than the minimum "
	      << "allowed:\nsize is " << x.size()
              << ", minimum is 2.\n";
#endif
    return false;
  }

  switch (x.type()) {
  case Grid_Generator::LINE:
    if (x[0] != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of lines must be zero!\n";
#endif
      return false;
    }
    break;

  case Grid_Generator::PARAMETER:
    if (x[0] != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of parameters must be zero!\n";
#endif
      return false;
    }
    if (x.divisor() <= 0) {
#ifndef NDEBUG
      std::cerr << "Parameters must have positive divisors!\n";
#endif
      return false;
    }
    break;

  case Grid_Generator::POINT:
    if (x[0] <= 0) {
#ifndef NDEBUG
      std::cerr << "Points must have positive divisors!\n";
#endif
      return false;
    }
    if (x[size() - 1] != 0) {
#ifndef NDEBUG
      std::cerr << "Points must have a zero parameter divisor!\n";
#endif
      return false;
    }
    break;

  } // switch (x.type())

  // All tests passed.
  return true;
}
