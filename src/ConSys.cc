/* ConSys class implementation (non-inline functions).
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

#include "ConSys.defs.hh"

#include "Generator.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

size_t
PPL::ConSys::num_inequalities() const {
  int n = 0;
  // If the Matrix happens to be sorted, take advantage of the fact
  // that inequalities are at the bottom of the system.
  if (is_sorted())
    for (size_t i = num_rows(); i != 0 && (*this)[--i].is_inequality(); )
      ++n;
  else
    for (size_t i = num_rows(); i-- > 0 ; )
      if ((*this)[i].is_inequality())
	++n;
  return n;
}

size_t
PPL::ConSys::num_equalities() const {
  return num_rows() - num_inequalities();
}

void
PPL::ConSys::const_iterator::skip_forward() {
  Matrix::const_iterator csp_end = csp->end();
  while (i != csp_end && (*this)->is_trivial())
    ++i;
}

/*!
  Returns <CODE>true</CODE> if the given generator \p g satisfies
  all the constraints in \p *this system.
*/
bool
PPL::ConSys::satisfies_all_constraints(const Generator& g) const {
  size_t n_rows = num_rows();
  size_t space_dim = space_dimension();
  size_t g_space_dim = g.space_dimension();
  assert(g_space_dim <= space_dim);
  bool g_is_ray_or_vertex = g.is_ray_or_vertex();
  for (size_t i = n_rows; i-- > 0; ) {
    const Constraint& c = (*this)[i];
    // Compute the sign of the scalar product.
    int sp_sign;
    if (g_space_dim < space_dim)
      sp_sign = sgn(projected_scalar_prod(g, c));
    else
      sp_sign = sgn(c * g);
    if (g_is_ray_or_vertex && c.is_inequality()) {
      // A ray satisfies an inequality if its scalar product
      // with such a constraint is positive.
      if (sp_sign < 0)
	return false;
    }
    else if (sp_sign != 0)
      // Equalities are saturated by all rays/vertices and lines.
      // Lines saturate all equalities.
      return false;
  }
  // All constraints are saturated by g.
  return true;
}

/*!
  \param v            Index of the column to which the
                      affine transformation is substituted.
  \param expr         The numerator of the affine transformation:
                      \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$
  \param denominator  The denominator of the affine transformation.

  We want to allow affine transformations (see the Introduction) having
  any rational coefficients. Since the coefficients of the
  constraints are integers we must also provide an integer \p denominator
  that will be used as denominator of the affine transformation.

  The affine transformation substitutes the matrix of constraints
  by a new matrix whose elements \f${a'}_{ij}\f$ are built from
  the old one \f$a_{ij}\f$ as follows:
  \f[
    {a'}_{ij} =
    \begin{cases}
    a_{ij} * \text{denominator} + a_{i\text{v}} * \text{expr}[j]
    \quad \text{for } j \neq \text{v}; \\
    \text{expr}[\text{v}] * a_{i\text{v}}
    \quad \text{for } j = \text{v}.
    \end{cases}
  \f]

  \p expr is a constant parameter and unaltered by this computation
*/
void
PPL::ConSys::substitute_variable(size_t v,
				 const LinExpression& expr,
				 const Integer& denominator) {
  ConSys& x = *this;
  size_t num_columns = x.num_columns();
  size_t num_rows = x.num_rows();

  assert(v != 0);
  assert(num_columns = expr.size());
  assert(denominator != 0);
  assert(v < num_columns);

  // Building the new matrix of constraints.
  for (size_t i = 0; i < num_rows; ++i) {
    Constraint& row = x[i];
    if (row[v] != 0) {
      Integer tmp = row[v];
      row[v] *= expr[v];
      for (size_t j = 0; j < num_columns; ++j)
	if (j != v) {
	  row[j] *= denominator;
	  row[j] += tmp * expr[j];
	}
    }
  }
  x.normalize();
}

/*!
  Raw write function: prints the number of rows,
  the number of columns and the value of \p sorted invoking the
  <CODE>Matrix::print()</CODE> method, then prints the contents of
  all the rows, specifying whether a row is an equality or an inequality.
*/
void
PPL::ConSys::print(std::ostream& s) const {
  Matrix::print(s);
  const char separator = ' ';
  const ConSys& x = *this;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s << x[i][j] << separator;
    s << separator << separator
      << (x[i].is_equality() ? "=" : ">=")
      << std::endl;
  }
}

/*!
  Raw read function: resizes the matrix of constraints using number of
  rows and number of columns read from \p s, then initializes the
  coefficients of each constraint and its type (equality or inequality)
  reading the contents from \p s.
*/
void
PPL::ConSys::get(std::istream& s) {
  Matrix::get(s);
  std::string tempstr;
  ConSys& x = *this;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s >> x[i][j];
    s >> tempstr;
    if (tempstr == "=")
      x[i].set_is_equality();
    else if (tempstr == ">=")
      x[i].set_is_inequality();
    else
      throw std::runtime_error("void PPL::ConSys::get(s)");
  }
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this actually represents
  a system of constraints. So \p *this must have:
  -# at least a column for the inhomogeneus term and one for a variable;
  -# at least a row.
*/
bool
PPL::ConSys::OK() const {
  using std::endl;
  using std::cerr;

  // ConSys must have at least two columns: one for the inhomogeneous
  // terms and one for the coefficients of at least one variable.
  if (!Matrix::OK()) {
    cerr << "A ConSys must have at least two columns!"
	 << endl;
    return false;
  }

  if (num_rows() == 0) {
    // A valid constraint system must have at least one constraint.
    // In fact, the constraint representation of a non-universe
    // polyhedron has, by definition, at least one constraint,
    // while a constraint system denoting a universe polyhedron must have,
    // in our representation, at least one positivity constraint.
    cerr << "A ConSys must not have zero rows!"
	 << endl;
    return false;
  }

  return true;
}
