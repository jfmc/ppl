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

  ec.get_row()[0] = 0;
  ec.get_row()[gg_size - 1] = d;

  // If the divisor is negative, negate it and all the coefficients of
  // the parameter, so as to satisfy the invariant.
  if (d < 0)
    for (dimension_type i = gg_size; i-- > 0; )
      neg_assign(ec.get_row()[i]);

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
  ec.get_row()[0] = d;

  // If the divisor is negative, negate it and all the coefficients of
  // the point, so as to satisfy the invariant.
  if (d < 0)
    for (dimension_type i = ec.get_row().size() - 1; i-- > 0; )
      neg_assign(ec.get_row()[i]);

  // Using this constructor saves reallocation when creating the
  // coefficients.
  Grid_Generator gg(ec, POINT);

  // Enforce normalization.
  gg.expr.get_row().normalize();
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
  ec.get_row()[0] = 0;
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
  swap(v1.space_dimension(), v2.space_dimension());
  // *this is still normalized but it may not be strongly normalized.
  if (!is_parameter())
    sign_normalize();
  PPL_ASSERT(OK());
}

bool
PPL::Grid_Generator::remove_space_dimensions(const Variables_Set& vars) {
  PPL_ASSERT(vars.space_dimension() <= space_dimension());
  // For each variable to be removed, replace the corresponding coefficient
  // by shifting left the coefficient to the right that will be kept.
  Variables_Set::const_iterator vsi = vars.begin();
  Variables_Set::const_iterator vsi_end = vars.end();
  dimension_type dst_col = *vsi+1;
  dimension_type src_col = dst_col + 1;
  for (++vsi; vsi != vsi_end; ++vsi) {
    const dimension_type vsi_col = *vsi+1;
    // Move all columns in between to the left.
    while (src_col < vsi_col)
      std::swap(expr.get_row()[dst_col++], expr.get_row()[src_col++]);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type sz = expr.get_row().size();
  while (src_col < sz)
    std::swap(expr.get_row()[dst_col++], expr.get_row()[src_col++]);

  // The number of remaining coefficients is `dst_col'.
  expr.get_row().resize(dst_col);

  PPL_ASSERT(OK());
  return true;
}

void
PPL::Grid_Generator
::permute_space_dimensions(const std::vector<Variable>& cycle) {
  const dimension_type n = cycle.size();
  if (n < 2)
    // No-op. No need to call sign_normalize().
    return;

  if (n == 2) {
    expr.get_row().swap(cycle[0].space_dimension(), cycle[1].space_dimension());
  } else {
    PPL_DIRTY_TEMP_COEFFICIENT(tmp);
    tmp = expr.get_row()[cycle.back().space_dimension()];
    for (dimension_type i = n - 1; i-- > 0; )
      expr.get_row().swap(cycle[i + 1].space_dimension(),
                     cycle[i].space_dimension());
    if (tmp == 0)
      expr.get_row().reset(cycle[0].space_dimension());
    else
      std::swap(tmp, expr.get_row()[cycle[0].space_dimension()]);
  }
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
  const dimension_type x_size = expr.get_row().size();
  s << "size " << x_size << " ";
  for (dimension_type i = 0; i < x_size; ++i)
    s << expr.get_row()[i] << ' ';
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
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  Dense_Row& x = expr.get_row();
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
    expr.get_row()[expr.get_row().size() - 1] = expr.get_row()[0];
    expr.get_row()[0] = 0;
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

  // Compare all the coefficients of the row starting from position 1.
  const dimension_type xsz = x.expression().get_row().size();
  const dimension_type ysz = y.expression().get_row().size();
  const dimension_type min_sz = std::min(xsz, ysz);
  dimension_type i;
  for (i = 1; i < min_sz; ++i)
    if (const int comp = cmp(x.expression().get_row()[i],
                             y.expression().get_row()[i]))
      // There is at least a different coefficient.
      return (comp > 0) ? 2 : -2;

  // Handle the case where `x' and `y' are of different size.
  if (xsz != ysz) {
    for( ; i < xsz; ++i)
      if (const int sign = sgn(x.expression().get_row()[i]))
        return (sign > 0) ? 2 : -2;
    for( ; i < ysz; ++i)
      if (const int sign = sgn(y.expression().get_row()[i]))
        return (sign < 0) ? 2 : -2;
  }

  // If all the coefficients in `x' equal all the coefficients in `y'
  // (starting from position 1) we compare coefficients in position 0,
  // i.e., inhomogeneous terms.
  if (const int comp = cmp(x.expression().get_row()[0],
                           y.expression().get_row()[0]))
    return (comp > 0) ? 1 : -1;

  // `x' and `y' are equal.
  return 0;
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
    tmp_x.expr.get_row()[last] = 0;
    tmp_y.expr.get_row()[last] = 0;
  }
  // Normalize the copies, including the divisor column.
  tmp_x.expr.get_row().normalize();
  tmp_y.expr.get_row().normalize();
  // Check for equality.
  while (last-- > 0)
    if (tmp_x.expr.get_row()[last] != tmp_y.expr.get_row()[last])
      return false;
  return true;
}

bool
PPL::Grid_Generator::is_equal_to(const Grid_Generator& y) const {
  return expr.is_equal_to(y.expr) && kind_ == y.kind_;
}

bool
PPL::Grid_Generator::all_homogeneous_terms_are_zero() const {
  // Start at size() - 2 to avoid the extra grid generator column.
  // Also avoid the point divisor column (0).
  for (dimension_type i = expr.get_row().size() - 2; i > 0; --i)
    if (expr.get_row()[i] != 0)
      return false;
  return true;
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
    for (dimension_type i = expr.get_row().size() - 2; i > 0; --i)
      expr.get_row()[i] *= factor;
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
    if (g.expr.get_row()[num_variables + 1] == 1)
      break;
    goto any_point;
  case Grid_Generator::POINT:
    s << "p(";
    if (g.expr.get_row()[0] > 1) {
    any_point:
      need_divisor = true;
      dimension_type num_non_zero_coefficients = 0;
      for (dimension_type v = 0; v < num_variables; ++v)
	if (g.expr.get_row()[v+1] != 0)
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
    gv = g.expr.get_row()[v+1];
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

  if (x.expr.get_row().size() < 2) {
#ifndef NDEBUG
    std::cerr << "Grid_Generator has fewer coefficients than the minimum "
	      << "allowed:\nsize is " << x.expr.get_row().size()
              << ", minimum is 2.\n";
#endif
    return false;
  }

  switch (x.type()) {
  case Grid_Generator::LINE:
    if (x.expr.get_row()[0] != 0) {
#ifndef NDEBUG
      std::cerr << "Inhomogeneous terms of lines must be zero!\n";
#endif
      return false;
    }
    break;

  case Grid_Generator::PARAMETER:
    if (x.expr.get_row()[0] != 0) {
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
    if (x.expr.get_row()[0] <= 0) {
#ifndef NDEBUG
      std::cerr << "Points must have positive divisors!\n";
#endif
      return false;
    }
    if (x.expr.get_row()[expr.get_row().size() - 1] != 0) {
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
