/* Grid_Generator class implementation (non-inline functions).
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

#include "Grid_Generator.defs.hh"

#include "Variables_Set.defs.hh"
#include "math_utilities.defs.hh"

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
    neg_assign(ec);

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
    neg_assign(ec);

  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(ec, POINT);

  // Enforce normalization.
  gg.expr.normalize();
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
PPL::Grid_Generator::swap_space_dimensions(Variable v1, Variable v2) {
  PPL_ASSERT(v1.space_dimension() <= space_dimension());
  PPL_ASSERT(v2.space_dimension() <= space_dimension());
  expr.swap_space_dimensions(v1, v2);
  // *this is still normalized but it may not be strongly normalized.
  if (!is_parameter())
    sign_normalize();
  PPL_ASSERT(OK());
}

bool
PPL::Grid_Generator::remove_space_dimensions(const Variables_Set& vars) {
  PPL_ASSERT(vars.space_dimension() <= space_dimension());
  
  expr.remove_space_dimensions(vars);

  PPL_ASSERT(OK());
  return true;
}

void
PPL::Grid_Generator
::permute_space_dimensions(const std::vector<Variable>& cycle) {
  if (cycle.size() < 2)
    // No-op. No need to call sign_normalize().
    return;

  expr.permute_space_dimensions(cycle);

  // *this is still normalized but may be not strongly normalized: sign
  // normalization is necessary.
  // Sign-normalizing a parameter changes its meaning, so do nothing for
  // parameters.
  if (!is_parameter())
    sign_normalize();
  PPL_ASSERT(OK());
}

void
PPL::Grid_Generator::ascii_dump(std::ostream& s) const {
  expr.ascii_dump(s);
  s << ' ';
  switch (type()) {
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

  if (!expr.ascii_load(s))
    return false;

  std::string str;

  if (!(s >> str))
    return false;
  if (str == "L")
    set_is_line();
  else if (str == "P" || str == "Q")
    set_is_parameter_or_point();
  else
    return false;

  PPL_ASSERT(OK());
  return true;
}

void
PPL::Grid_Generator::set_is_parameter() {
  if (is_line())
    set_is_parameter_or_point();
  else if (!is_line_or_parameter()) {
    // The grid generator is a point.
    expr[expr.space_dimension()] = expr[0];
    expr[0] = 0;
  }
}

void
PPL::Grid_Generator::linear_combine(const Grid_Generator& y,
                                    Variable v) {
  expr.linear_combine(y.expr, v);
  strong_normalize();
}

void
PPL::Grid_Generator::linear_combine_inhomogeneous(const Grid_Generator& y) {
  expr.linear_combine_inhomogeneous(y.expr);
  strong_normalize();
}

/*! \relates Parma_Polyhedra_Library::Grid_Generator */
int
PPL::compare(const Grid_Generator& x, const Grid_Generator& y) {
  const bool x_is_line_or_equality = x.is_line_or_equality();
  const bool y_is_line_or_equality = y.is_line_or_equality();
  if (x_is_line_or_equality != y_is_line_or_equality)
    // Equalities (lines) precede inequalities (ray/point).
    return y_is_line_or_equality ? 2 : -2;

  return compare(x.expression(), y.expression());
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
  dimension_type last = x_space_dim + 1;
  if (x_type == POINT || x_type == LINE) {
    tmp_x.expr[last] = 0;
    tmp_y.expr[last] = 0;
  }
  // Normalize the copies, including the divisor column.
  tmp_x.expr.normalize();
  tmp_y.expr.normalize();
  // Check for equality.
  return tmp_x.is_equal_to(tmp_y);
}

bool
PPL::Grid_Generator::is_equal_to(const Grid_Generator& y) const {
  return expr.is_equal_to(y.expr) && kind_ == y.kind_;
}

bool
PPL::Grid_Generator::all_homogeneous_terms_are_zero() const {
  // This does not check neither the first nor the last coefficient.
  return expr.all_zeroes(1, expr.space_dimension());
}

void
PPL::Grid_Generator::scale_to_divisor(Coefficient_traits::const_reference d) {
  PPL_ASSERT(d != 0);
  if (is_line())
    return;

  PPL_DIRTY_TEMP_COEFFICIENT(factor);
  exact_div_assign(factor, d, divisor());
  set_divisor(d);
  PPL_ASSERT(factor > 0);
  if (factor > 1) {
    expr *= factor;
    // Undo the multipication for the first and last coefficient.
    Coefficient& first = expr[0];
    exact_div_assign(first, first, factor);
    Coefficient& last = expr[expr.space_dimension()];
    exact_div_assign(last, last, factor);
  }
}

void
PPL::Grid_Generator::sign_normalize() {
  if (is_line_or_equality())
    expr.sign_normalize();
}

bool
PPL::Grid_Generator::check_strong_normalized() const {
  Grid_Generator tmp = *this;
  tmp.strong_normalize();
  return compare(*this, tmp) == 0;
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
    if (g.expr.coefficient(Variable(num_variables)) == 1)
      break;
    goto any_point;
  case Grid_Generator::POINT:
    s << "p(";
    if (g.expr.inhomogeneous_term() > 1) {
    any_point:
      need_divisor = true;
      dimension_type num_non_zero_coefficients = 0;
      for (dimension_type v = 0; v < num_variables; ++v)
	if (g.expr.coefficient(Variable(v)) != 0)
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
    gv = g.expr.coefficient(Variable(v));
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
  // NOTE: do not check for normalization, as it does not hold.
  const Grid_Generator& x = *this;

  if (!x.is_necessarily_closed()) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator should be necessarily closed.\n";
#endif
    return false;
  }

  if (x.expr.space_dimension() < 1) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator has fewer coefficients than the minimum "
	      << "allowed:\nspace dimension is " << x.expr.space_dimension()
              << ", minimum is 1.\n";
#endif
    return false;
  }

  switch (x.type()) {
  case Grid_Generator::LINE:
    if (x.expr.inhomogeneous_term() != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of lines must be zero!\n";
#endif
      return false;
    }
    break;

  case Grid_Generator::PARAMETER:
    if (x.expr.inhomogeneous_term() != 0) {
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
    if (x.expr.inhomogeneous_term() <= 0) {
#ifndef NDEBUG
      std::cerr << "Points must have positive divisors!\n";
#endif
      return false;
    }
    if (x.expr.coefficient(Variable(space_dimension())) != 0) {
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
