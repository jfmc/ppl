/* Generator class implementation (non-inline functions).
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

#include "Generator.defs.hh"

#include "Variable.defs.hh"
#include "Variables_Set.defs.hh"
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Generator::throw_dimension_incompatible(const char* method,
					     const char* name_var,
					     const Variable v) const {
  std::ostringstream s;
  s << "PPL::Generator::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << name_var << ".space_dimension() == " << v.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

void
PPL::Generator::throw_invalid_argument(const char* method,
				       const char* reason) const {
  std::ostringstream s;
  s << "PPL::Generator::" << method << ":" << std::endl
    << reason << ".";
  throw std::invalid_argument(s.str());
}

PPL::Generator
PPL::Generator::point(const Linear_Expression& e,
		      Coefficient_traits::const_reference d) {
  if (d == 0)
    throw std::invalid_argument("PPL::point(e, d):\n"
				"d == 0.");
  Linear_Expression ec = e;
  ec.get_row()[0] = d;
  Generator g(ec, Generator::POINT, NECESSARILY_CLOSED);

  // If the divisor is negative, we negate it as well as
  // all the coefficients of the point, because we want to preserve
  // the invariant: the divisor of a point is strictly positive.
  if (d < 0)
    for (dimension_type i = g.get_row().size(); i-- > 0; )
      neg_assign(g.get_row()[i]);

  // Enforce normalization.
  g.get_row().normalize();
  return g;
}

PPL::Generator
PPL::Generator::closure_point(const Linear_Expression& e,
			      Coefficient_traits::const_reference d) {
  if (d == 0)
    throw std::invalid_argument("PPL::closure_point(e, d):\n"
				"d == 0.");
  // Adding the epsilon dimension with coefficient 0.
  Linear_Expression ec = 0 * Variable(e.space_dimension());
  ec += e;
  // A closure point is indeed a point in the higher dimension space.
  Generator g = point(ec, d);
  // Fix the topology.
  // TODO: Avoid the mark_as_*() methods if possible.
  g.mark_as_not_necessarily_closed();
  // Enforce normalization.
  g.get_row().normalize();
  return g;
}

PPL::Generator
PPL::Generator::ray(const Linear_Expression& e) {
  // The origin of the space cannot be a ray.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::ray(e):\n"
				"e == 0, but the origin cannot be a ray.");

  Linear_Expression ec = e;
  ec.get_row()[0] = 0;
  Generator g(ec, Generator::RAY, NECESSARILY_CLOSED);

  return g;
}

PPL::Generator
PPL::Generator::line(const Linear_Expression& e) {
  // The origin of the space cannot be a line.
  if (e.all_homogeneous_terms_are_zero())
    throw std::invalid_argument("PPL::line(e):\n"
				"e == 0, but the origin cannot be a line.");

  Linear_Expression ec = e;
  ec.get_row()[0] = 0;
  Generator g(ec, Generator::LINE, NECESSARILY_CLOSED);

  return g;
}

void
PPL::Generator::swap_space_dimensions(Variable v1, Variable v2) {
  PPL_ASSERT(v1.space_dimension() <= space_dimension());
  PPL_ASSERT(v2.space_dimension() <= space_dimension());
  swap(v1.space_dimension(), v2.space_dimension());
  // *this is still normalized but it may not be strongly normalized.
  sign_normalize();
  PPL_ASSERT(OK());
}

bool
PPL::Generator::remove_space_dimensions(const Variables_Set& vars) {
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
      std::swap(get_row()[dst_col++], get_row()[src_col++]);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type sz = get_row().size();
  while (src_col < sz)
    std::swap(get_row()[dst_col++], get_row()[src_col++]);

  // The number of remaining coefficients is `dst_col'.
  get_row().resize(dst_col);

  if (is_line_or_ray() && all_homogeneous_terms_are_zero()) {
    // Become a point.
    set_is_ray_or_point();
    get_row()[0] = 1;
    if (is_not_necessarily_closed())
      get_row()[get_row().size() - 1] = 1;

    PPL_ASSERT(OK());
    return false;
  } else {
    strong_normalize();
    PPL_ASSERT(OK());
    return true;
  }
}

void
PPL::Generator
::permute_space_dimensions(const std::vector<Variable>& cycle) {
  const dimension_type n = cycle.size();
  if (n < 2)
    // No-op. No need to call sign_normalize().
    return;

  if (n == 2) {
    get_row().swap(cycle[0].space_dimension(), cycle[1].space_dimension());
  } else {
    PPL_DIRTY_TEMP_COEFFICIENT(tmp);
    tmp = get_row()[cycle.back().space_dimension()];
    for (dimension_type i = n - 1; i-- > 0; )
      get_row().swap(cycle[i + 1].space_dimension(),
                     cycle[i].space_dimension());
    if (tmp == 0)
      get_row().reset(cycle[0].space_dimension());
    else
      std::swap(tmp, get_row()[cycle[0].space_dimension()]);
  }
  // *this is still normalized but may be not strongly normalized: sign
  // normalization is necessary.
  sign_normalize();
  PPL_ASSERT(OK());
}

void
PPL::Generator::linear_combine(const Generator& y,
                               const dimension_type k) {
  Generator& x = *this;
  // We can combine only vector of the same dimension.
  PPL_ASSERT(x.get_row().size() == y.get_row().size());
  PPL_ASSERT(y.get_row()[k] != 0 && x.get_row()[k] != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_k);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_y_k);
  normalize2(x.get_row()[k], y.get_row()[k], normalized_x_k, normalized_y_k);
  for (dimension_type i = get_row().size(); i-- > 0; )
    if (i != k) {
      Coefficient& x_i = x.get_row()[i];
      x_i *= normalized_y_k;
      sub_mul_assign(x_i, y.get_row()[i], normalized_x_k);
    }
  x.get_row()[k] = 0;
  x.strong_normalize();
}

/*! \relates Parma_Polyhedra_Library::Generator */
int
PPL::compare(const Generator& x, const Generator& y) {
  const bool x_is_line_or_equality = x.is_line_or_equality();
  const bool y_is_line_or_equality = y.is_line_or_equality();
  if (x_is_line_or_equality != y_is_line_or_equality)
    // Equalities (lines) precede inequalities (ray/point).
    return y_is_line_or_equality ? 2 : -2;

  // Compare all the coefficients of the row starting from position 1.
  const dimension_type xsz = x.get_row().size();
  const dimension_type ysz = y.get_row().size();
  const dimension_type min_sz = std::min(xsz, ysz);
  dimension_type i;
  for (i = 1; i < min_sz; ++i)
    if (const int comp = cmp(x.get_row()[i], y.get_row()[i]))
      // There is at least a different coefficient.
      return (comp > 0) ? 2 : -2;

  // Handle the case where `x' and `y' are of different size.
  if (xsz != ysz) {
    for( ; i < xsz; ++i)
      if (const int sign = sgn(x.get_row()[i]))
        return (sign > 0) ? 2 : -2;
    for( ; i < ysz; ++i)
      if (const int sign = sgn(y.get_row()[i]))
        return (sign < 0) ? 2 : -2;
  }

  // If all the coefficients in `x' equal all the coefficients in `y'
  // (starting from position 1) we compare coefficients in position 0,
  // i.e., inhomogeneous terms.
  if (const int comp = cmp(x.get_row()[0], y.get_row()[0]))
    return (comp > 0) ? 1 : -1;

  // `x' and `y' are equal.
  return 0;
}

bool
PPL::Generator::is_equivalent_to(const Generator& y) const {
  const Generator& x = *this;
  const dimension_type x_space_dim = x.space_dimension();
  if (x_space_dim != y.space_dimension())
    return false;

  const Type x_type = x.type();
  if (x_type != y.type())
    return false;

  if (x_type == POINT
      && !(x.is_necessarily_closed() && y.is_necessarily_closed())) {
    // Due to the presence of epsilon-coefficients, syntactically
    // different points may actually encode the same generator.
    // First, drop the epsilon-coefficient ...
    Linear_Expression x_expr(x);
    Linear_Expression y_expr(y);
    // ... second, re-normalize ...
    x_expr.get_row().normalize();
    y_expr.get_row().normalize();
    // ... and finally check for syntactic equality.
    for (dimension_type i = x_space_dim + 1; i-- > 0; )
      if (x_expr.get_row()[i] != y_expr.get_row()[i])
	return false;
    return true;
  }

  // Here the epsilon-coefficient, if present, is zero.
  // It is sufficient to check for syntactic equality.
  for (dimension_type i = x_space_dim + 1; i-- > 0; )
    if (x.get_row()[i] != y.get_row()[i])
      return false;
  return true;
}

bool
PPL::Generator::is_equal_to(const Generator& y) const {
  return static_cast<const Linear_Expression&>(*this)
         .is_equal_to(static_cast<const Linear_Expression&>(y))
         && kind_ == y.kind_ && topology_ == y.topology_;
}

void
PPL::Generator::sign_normalize() {
  if (is_line_or_equality()) {
    Generator& x = *this;
    const dimension_type sz = x.get_row().size();
    // `first_non_zero' indicates the index of the first
    // coefficient of the row different from zero, disregarding
    // the very first coefficient (inhomogeneous term / divisor).
    dimension_type first_non_zero;
    for (first_non_zero = 1; first_non_zero < sz; ++first_non_zero)
      if (x.get_row()[first_non_zero] != 0)
        break;
    if (first_non_zero < sz)
      // If the first non-zero coefficient of the row is negative,
      // we negate the entire row.
      if (x.get_row()[first_non_zero] < 0) {
        for (dimension_type j = first_non_zero; j < sz; ++j)
          neg_assign(x.get_row()[j]);
        // Also negate the first coefficient.
        neg_assign(x.get_row()[0]);
      }
  }
}

bool
PPL::Generator::check_strong_normalized() const {
  Generator tmp = *this;
  tmp.strong_normalize();
  return compare(*this, tmp) == 0;
}

const PPL::Generator* PPL::Generator::zero_dim_point_p = 0;
const PPL::Generator* PPL::Generator::zero_dim_closure_point_p = 0;

void
PPL::Generator::initialize() {
  PPL_ASSERT(zero_dim_point_p == 0);
  zero_dim_point_p
    = new Generator(point());

  PPL_ASSERT(zero_dim_closure_point_p == 0);
  zero_dim_closure_point_p
    = new Generator(closure_point());
}

void
PPL::Generator::finalize() {
  PPL_ASSERT(zero_dim_point_p != 0);
  delete zero_dim_point_p;
  zero_dim_point_p = 0;

  PPL_ASSERT(zero_dim_closure_point_p != 0);
  delete zero_dim_closure_point_p;
  zero_dim_closure_point_p = 0;
}

/*! \relates Parma_Polyhedra_Library::Generator */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Generator& g) {
  bool needed_divisor = false;
  bool extra_parentheses = false;
  const dimension_type num_variables = g.space_dimension();
  Generator::Type t = g.type();
  switch (t) {
  case Generator::LINE:
    s << "l(";
    break;
  case Generator::RAY:
    s << "r(";
    break;
  case Generator::POINT:
    s << "p(";
    goto any_point;
  case Generator::CLOSURE_POINT:
    s << "c(";
  any_point:
    if (g.get_row()[0] != 1) {
      needed_divisor = true;
      dimension_type num_non_zero_coefficients = 0;
      for (dimension_type v = 0; v < num_variables; ++v)
	if (g.get_row()[v+1] != 0)
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
    gv = g.get_row()[v+1];
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
    // A point or closure point in the origin.
    s << 0;
  if (extra_parentheses)
    s << ")";
  if (needed_divisor)
    s << "/" << g.get_row()[0];
  s << ")";
  return s;
}

/*! \relates Parma_Polyhedra_Library::Generator */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Generator::Type& t) {
  const char* n = 0;
  switch (t) {
  case Generator::LINE:
    n = "LINE";
    break;
  case Generator::RAY:
    n = "RAY";
    break;
  case Generator::POINT:
    n = "POINT";
    break;
  case Generator::CLOSURE_POINT:
    n = "CLOSURE_POINT";
    break;
  }
  s << n;
  return s;
}

bool
PPL::Generator::is_matching_closure_point(const Generator& p) const {
  PPL_ASSERT(topology() == p.topology()
	 && space_dimension() == p.space_dimension()
	 && type() == CLOSURE_POINT
	 && p.type() == POINT);
  const Generator& cp = *this;
  if (cp.get_row()[0] == p.get_row()[0]) {
    // Divisors are equal: we can simply compare coefficients
    // (disregarding the epsilon coefficient).
    for (dimension_type i = cp.get_row().size() - 2; i > 0; --i)
      if (cp.get_row()[i] != p.get_row()[i])
	return false;
    return true;
  }
  else {
    // Divisors are different: divide them by their GCD
    // to simplify the following computation.
    PPL_DIRTY_TEMP_COEFFICIENT(gcd);
    gcd_assign(gcd, cp.get_row()[0], p.get_row()[0]);
    const bool rel_prime = (gcd == 1);
    PPL_DIRTY_TEMP_COEFFICIENT(cp_0_scaled);
    PPL_DIRTY_TEMP_COEFFICIENT(p_0_scaled);
    if (!rel_prime) {
      exact_div_assign(cp_0_scaled, cp.get_row()[0], gcd);
      exact_div_assign(p_0_scaled, p.get_row()[0], gcd);
    }
    const Coefficient& cp_div = rel_prime ? cp.get_row()[0] : cp_0_scaled;
    const Coefficient& p_div = rel_prime ? p.get_row()[0] : p_0_scaled;
    PPL_DIRTY_TEMP_COEFFICIENT(prod1);
    PPL_DIRTY_TEMP_COEFFICIENT(prod2);
    for (dimension_type i = cp.get_row().size() - 2; i > 0; --i) {
      prod1 = cp.get_row()[i] * p_div;
      prod2 = p.get_row()[i] * cp_div;
      if (prod1 != prod2)
	return false;
    }
    return true;
  }
}

PPL_OUTPUT_DEFINITIONS(Generator)

namespace {

// These are the keywords that indicate the individual assertions.
const char* rpi_valid = "RPI_V";
const char* is_rpi = "RPI";
const char* nnc_valid = "NNC_V";
const char* is_nnc = "NNC";
const char* bit_names[] = {rpi_valid, is_rpi, nnc_valid, is_nnc};

} // namespace

bool
PPL::Generator::OK() const {
  // Check the underlying Linear_Row object.
  if (!Linear_Row::OK())
    return false;

  // Topology consistency check.
  const dimension_type min_size = is_necessarily_closed() ? 1 : 2;
  if (get_row().size() < min_size) {
#ifndef NDEBUG
    std::cerr << "Generator has fewer coefficients than the minimum "
	      << "allowed by its topology:"
	      << std::endl
	      << "size is " << get_row().size()
	      << ", minimum is " << min_size << "."
	      << std::endl;
#endif
    return false;
  }

  // Normalization check.
  const Generator& g = *this;
  Generator tmp = g;
  tmp.strong_normalize();
  if (tmp != g) {
#ifndef NDEBUG
    std::cerr << "Generators should be strongly normalized!"
	      << std::endl;
#endif
    return false;
  }

  switch (g.type()) {
  case LINE:
    // Intentionally fall through.
  case RAY:
    if (g.get_row()[0] != 0) {
#ifndef NDEBUG
      std::cerr << "Lines must have a zero inhomogeneous term!"
		<< std::endl;
#endif
      return false;
    }
    if (!g.is_necessarily_closed() && g.get_row()[get_row().size() - 1] != 0) {
#ifndef NDEBUG
      std::cerr << "Lines and rays must have a zero coefficient "
		<< "for the epsilon dimension!"
		<< std::endl;
#endif
      return false;
    }
    // The following test is correct, since we already checked
    // that the epsilon coordinate is zero.
    if (g.all_homogeneous_terms_are_zero()) {
#ifndef NDEBUG
      std::cerr << "The origin of the vector space cannot be a line or a ray!"
		<< std::endl;
#endif
      return false;
    }
    break;

  case POINT:
    if (g.get_row()[0] <= 0) {
#ifndef NDEBUG
      std::cerr << "Points must have a positive divisor!"
		<< std::endl;
#endif
      return false;
    }
    if (!g.is_necessarily_closed())
      if (g.get_row()[get_row().size() - 1] <= 0) {
#ifndef NDEBUG
	std::cerr << "In the NNC topology, points must have epsilon > 0"
		  << std::endl;
#endif
	return false;
      }
    break;

  case CLOSURE_POINT:
    if (g.get_row()[0] <= 0) {
#ifndef NDEBUG
      std::cerr << "Closure points must have a positive divisor!"
		<< std::endl;
#endif
      return false;
    }
    break;
  }

  // All tests passed.
  return true;
}
