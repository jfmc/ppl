/* ConSys class implementation (non-inline functions).
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

#include "ConSys.defs.hh"

#include "Generator.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;


bool
PPL::ConSys::adjust_topology_and_dimension(Topology new_topology,
					   size_t new_space_dim) {
  assert(space_dimension() <= new_space_dim);

  size_t old_space_dim = space_dimension();
  size_t cols_to_be_added = new_space_dim - old_space_dim;
  Topology old_topology = topology();

  if (cols_to_be_added > 0)
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED constraint system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain strict inequalities.
	if (has_strict_inequalities())
	  return false;
	// Since there were no strict inequalities,
	// all \epsilon coefficients are equal to zero
	// and we do not need to clear them:
	// we just decrement the number of columns to be added.
	if (--cols_to_be_added > 0)
	  add_zero_columns(cols_to_be_added);
	set_necessarily_closed();
      }
      else {
	// A NECESSARILY_CLOSED constraint system is converted to
	// a NOT_NECESSARILY_CLOSED one by adding a further column
	// of zeros for the \epsilon coefficients.
	add_zero_columns(++cols_to_be_added);
	set_not_necessarily_closed();
      }
    else {
      // Topologies agree: first add the required zero columns ...
      add_zero_columns(cols_to_be_added);
      // ... and, if needed, move the \epsilon coefficients
      // to the new last column.
      if (old_topology == NOT_NECESSARILY_CLOSED)
	swap_columns(old_space_dim + 1, new_space_dim + 1);
    }
  else
    // Here `cols_to_be_added == 0'.
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED constraint system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain strict inequalities.
	if (has_strict_inequalities())
	  return false;
	// We just remove the column of the \epsilon coefficients.
	resize_no_copy(num_rows(), old_space_dim + 1);
	set_necessarily_closed();
      }
      else {
	// We just add the column of the \epsilon coefficients.
	add_zero_columns(1);
	set_not_necessarily_closed();
      }
  // We successfully adjusted dimensions and topology.
  assert(OK());
  return true;
}

bool
PPL::ConSys::has_strict_inequalities() const {
  if (is_necessarily_closed())
    return false;
  const ConSys& cs = *this;
  size_t eps_index = cs.num_columns() - 1;
  for (size_t i = num_rows(); i-- > 0; )
    // Optimized type checking: we already know the topology;
    // also, equalities have the \epsilon coefficient equal to zero.
    // NOTE : the constraint eps_leq_one should not be considered
    //        a strict inequality.
    if (cs[i][eps_index] < 0 && !cs[i].is_trivial_true())
      return true;
  return false;
}


void
PPL::ConSys::insert(const Constraint& c) {
  if (topology() == c.topology())
    Matrix::insert(c);
  else
    // `*this' and `c' have different topologies.
    if (is_necessarily_closed()) {
      // Padding the matrix with a columns of zeros
      // corresponding to the \epsilon coefficients.
      add_zero_columns(1);
      set_not_necessarily_closed();
      Matrix::insert(c);
    }
    else {
      // Copying the constraint adding the missing dimensions
      // and the \epsilon coefficient.
      Constraint tmp_c(c, num_columns());
      tmp_c.set_not_necessarily_closed();
      Matrix::insert(tmp_c);
    }
}

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
  while (i != csp_end && (*this)->is_trivial_true())
    ++i;
}

/*!
  Returns <CODE>true</CODE> if the given generator \p g satisfies
  all the constraints in \p *this system.
*/
bool
PPL::ConSys::satisfies_all_constraints(const Generator& g) const {
  assert(g.space_dimension() <= space_dimension());

  // Setting `sp_fp' to the appropriate scalar product operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  const Integer& (*sp_fp)(const Row&, const Row&);
  if (g.is_necessarily_closed())
    sp_fp = PPL::operator*;
  else
    sp_fp = PPL::operator^;

  const ConSys& cs = *this;
  if (cs.is_necessarily_closed())
    for (size_t i = cs.num_rows(); i-- > 0; ) {
      const Constraint& c = cs[i];
      int sp_sign = sgn(sp_fp(g, c));
      if (c.is_inequality()) {
	// `c' is a non-strict inequality.
	if (sp_sign < 0)
	  return false;
      }
      else
	// `c' is an equality.
	if (sp_sign != 0)
	  return false;
    }
  else
    // `cs' is NON-necessarily closed.
    if (g.is_point())
      // Generator `g' is a point: have to perform the special test
      // when dealing with a strict inequality.
      for (size_t i = cs.num_rows(); i-- > 0; ) {
	const Constraint& c = cs[i];
	int sp_sign = sgn(sp_fp(g, c));
	switch (c.type()) {
	case Constraint::EQUALITY:
	  if (sp_sign != 0)
	    return false;
	  break;
	case Constraint::NONSTRICT_INEQUALITY:
	  if (sp_sign < 0)
	    return false;
	  break;
	case Constraint::STRICT_INEQUALITY:
	  if (sp_sign <= 0)
	    return false;
	  break;
	}
      }
    else
      // Generator `g' is a line, ray or closure point.
      for (size_t i = cs.num_rows(); i-- > 0; ) {
	const Constraint& c = cs[i];
	int sp_sign = sgn(sp_fp(g, c));
	if (c.is_inequality()) {
	  // Constraint `c' is either a strict or a non-strict inequality.
	  if (sp_sign < 0)
	    return false;
	}
	else
	  // Constraint `c' is an equality.
	  if (sp_sign != 0)
	    return false;
      }
  // `g' satisfies all constraints.
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
    a_{ij} * \mathrm{denominator} + a_{iv} * \mathrm{expr}[j]
    \quad \text{for } j \neq v; \\
    \mathrm{expr}[v] * a_{iv}
    \quad \text{for } j = v.
    \end{cases}
  \f]

  \p expr is a constant parameter and unaltered by this computation.
*/
void
PPL::ConSys::affine_preimage(size_t v,
			     const LinExpression& expr,
			     const Integer& denominator) {
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the \epsilon dimension of NNC polyhedra).
  assert(v > 0 && v <= space_dimension());
  assert(expr.space_dimension() <= space_dimension());
  assert(denominator != 0);

  size_t n_columns = num_columns();
  size_t n_rows = num_rows();
  size_t expr_size = expr.size();
  bool not_invertible = (v >= expr_size || expr[v] == 0);
  ConSys& x = *this;

  if (denominator != 1)
    for (size_t i = n_rows; i-- > 0; ) {
      Constraint& row = x[i];
      Integer& row_v = row[v];
      if (row_v != 0) {
	for (size_t j = n_columns; j-- > 0; )
	  if (j != v) {
	    row[j] *= denominator;
	    if (j < expr_size)
	      row[j] += row_v * expr[j];
	  }
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr[v];
      }
    }
  else
    // Here `denominator' == 1: optimized computation
    // only considering columns having indexes < expr_size.
    for (size_t i = n_rows; i-- > 0; ) {
      Constraint& row = x[i];
      Integer& row_v = row[v];
      if (row_v != 0) {
	for (size_t j = expr_size; j-- > 0; )
	  if (j != v)
	    row[j] += row_v * expr[j];
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr[v];
      }
    }
  x.strong_normalize();
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
  s << "topology ";
  if (!x.is_necessarily_closed())
    s << "NOT_";
  s << "NECESSARILY_CLOSED"
    << std::endl;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s << x[i][j] << separator;
    s << separator << separator;
    switch (static_cast<Constraint>(x[i]).type()) {
    case Constraint::EQUALITY:
      s << "=";
      break;
    case Constraint::NONSTRICT_INEQUALITY:
      s << ">=";
      break;
    case Constraint::STRICT_INEQUALITY:
      s << ">";
      break;
    }
    s << std::endl;
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
  s >> tempstr;
  assert(tempstr == "topology");
  s >> tempstr;
  if (tempstr == "NECESSARILY_CLOSED")
    x.set_necessarily_closed();
  else {
    assert(tempstr == "NOT_NECESSARILY_CLOSED");
    x.set_not_necessarily_closed();
  }
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s >> x[i][j];
    s >> tempstr;
    if (tempstr == "=")
      x[i].set_is_equality();
    else
      x[i].set_is_inequality();
    // Checking for equality of actual and declared types.
    switch (static_cast<Constraint>(x[i]).type()) {
    case Constraint::EQUALITY:
      if (tempstr == "=")
	continue;
      break;
    case Constraint::NONSTRICT_INEQUALITY:
      if (tempstr == ">=")
	continue;
      break;
    case Constraint::STRICT_INEQUALITY:
      if (tempstr == ">")
	continue;
      break;
    }
    // Reaching this point means that the input was illegal.
    throw std::runtime_error("void PPL::ConSys::get(s)");
  }
  // Checking for well-formedness.
  if (!x.OK())
    throw std::runtime_error("void PPL::ConSys::get(s)");
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this is a valid Matrix.
  No other checks can be performed here, since any valid Row object
  in the matrix is also a valid Constraint object.
*/
bool
PPL::ConSys::OK() const {
  return Matrix::OK();
}
