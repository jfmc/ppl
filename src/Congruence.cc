/* Congruence class implementation (non-inline functions).
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

#include "Congruence.defs.hh"

#include "Variable.defs.hh"
#include "assert.hh"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace PPL = Parma_Polyhedra_Library;

PPL::Congruence::Congruence(const Constraint& c)
  : row(c.is_equality()
	? c
	: (throw_invalid_argument("Congruence(c)",
				  "constraint c must be an equality."),
	   c),
	c.space_dimension() + 2,
	compute_capacity(c.space_dimension() + 2, Dense_Row::max_size())) {
  row[row.size() - 1] = 0;
}

PPL::Congruence::Congruence(const Constraint& c,
                            dimension_type new_space_dimension)
  : row(c.is_equality()
        ? c
        : (throw_invalid_argument("Congruence(c)",
                                  "constraint c must be an equality."),
           c),
        new_space_dimension + 2,
        new_space_dimension + 2) {
  set_modulus(Coefficient_zero());
}

PPL::Congruence::Congruence(const Constraint& c,
                            dimension_type sz, dimension_type capacity)
  : row(c.is_equality()
        ? c
        : (throw_invalid_argument("Congruence(c)",
                                  "constraint c must be an equality."),
           c),
        sz,
        capacity) {
  PPL_ASSERT(sz > 1);
  row[sz - 1] = 0;
}

void
PPL::Congruence::sign_normalize() {
  const dimension_type sz = row.size() - 1;
  // `first_non_zero' indicates the index of the first
  // coefficient of the row different from zero, disregarding
  // the very first coefficient (inhomogeneous term).
  dimension_type first_non_zero;
  for (first_non_zero = 1; first_non_zero < sz; ++first_non_zero)
    if (row[first_non_zero] != 0)
      break;
  if (first_non_zero < sz)
    // If the first non-zero coefficient of the row is negative,
    // negate all the coefficients and the inhomogeneous term.
    if (row[first_non_zero] < 0) {
      for (dimension_type j = first_non_zero; j < sz; ++j)
	neg_assign(row[j]);
      // Also negate the inhomogeneous term.
      neg_assign(row[0]);
    }
}

void
PPL::Congruence::normalize() {
  sign_normalize();

  dimension_type sz = row.size();
  if (sz == 0)
    return;

  const Coefficient& mod = modulus();
  if (mod == 0)
    return;

  Coefficient& row_0 = row[0];
  // Factor the modulus out of the inhomogeneous term.
  row_0 %= mod;
  if (row_0 < 0)
    // Make inhomogeneous term positive.
    row_0 += mod;
  return;
}

void
PPL::Congruence::strong_normalize() {
  normalize();
  row.normalize();
}

void
PPL::Congruence::scale(Coefficient_traits::const_reference factor) {
  for (dimension_type i = row.size(); i-- > 0; )
    row[i] *= factor;
}

void
PPL::Congruence
::affine_preimage(dimension_type v, const Linear_Expression& expr,
                  Coefficient_traits::const_reference denominator) {
  const dimension_type expr_size = expr.get_row().size();

  Coefficient& row_v = row[v];

  if (row_v == 0)
    return;
  
  if (denominator == 1) {
    // Optimized computation only considering columns having indexes <
    // expr_size.
    for (dimension_type j = expr_size; j-- > 0; )
      if (j != v)
        // row[j] = row[j] + row_v * expr[j]
        add_mul_assign(row[j], row_v, expr.get_row()[j]);

  } else {
    for (dimension_type j = row.size(); j-- > 0; )
      if (j != v) {
        Coefficient& row_j = row[j];
        row_j *= denominator;
        if (j < expr_size)
          add_mul_assign(row_j, row_v, expr.get_row()[j]);
      }
  }

  if (v >= expr_size || expr.get_row()[v] == 0)
    // Not invertible
    row_v = 0;
  else
    row_v *= expr.get_row()[v];
}

PPL::Congruence
PPL::Congruence::create(const Linear_Expression& e1,
			const Linear_Expression& e2) {
  // Ensure that diff is created with capacity for the modulus.
  dimension_type dim, e1_dim, e2_dim;
  e1_dim = e1.space_dimension();
  e2_dim = e2.space_dimension();
  if (e1_dim > e2_dim)
    dim = e1_dim;
  else
    dim = e2_dim;
  Linear_Expression diff(e1_dim > e2_dim ? e1 : e2,
			 dim + 2);
  diff -= (e1_dim > e2_dim ? e2 : e1);
  Congruence cg(diff, 1);
  return cg;
}

void
PPL::Congruence::throw_invalid_argument(const char* method,
					const char* message) const {
  std::ostringstream s;
  s << "PPL::Congruence::" << method << ":" << std::endl
    << message;
  throw std::invalid_argument(s.str());
}

void
PPL::Congruence::throw_dimension_incompatible(const char* method,
					      const char* v_name,
					      const Variable v) const {
  std::ostringstream s;
  s << "this->space_dimension() == " << space_dimension() << ", "
    << v_name << ".space_dimension() == " << v.space_dimension() << ".";
  std::string str = s.str();
  throw_invalid_argument(method, str.c_str());
}

/*! \relates Parma_Polyhedra_Library::Congruence */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Congruence& c) {
  const dimension_type num_variables = c.space_dimension();
  PPL_DIRTY_TEMP_COEFFICIENT(cv);
  bool first = true;
  for (dimension_type v = 0; v < num_variables; ++v) {
    cv = c.coefficient(Variable(v));
    if (cv != 0) {
      if (!first) {
	if (cv > 0)
	  s << " + ";
	else {
	  s << " - ";
	  neg_assign(cv);
	}
      }
      else
	first = false;
      if (cv == -1)
	s << "-";
      else if (cv != 1)
	s << cv << "*";
      s << PPL::Variable(v);
    }
  }
  if (first)
    s << Coefficient_zero();
  s << " = " << -c.inhomogeneous_term();
  if (c.is_proper_congruence())
    s << " (mod " << c.modulus() << ")";
  return s;
}

bool
PPL::Congruence::is_tautological() const {
  if ((is_equality() && inhomogeneous_term() == 0)
      || (is_proper_congruence()
	  && (inhomogeneous_term() % modulus() == 0))) {
    for (unsigned i = space_dimension(); i > 0; --i)
      if (row[i] != 0)
	return false;
    return true;
  }
  return false;
}

bool
PPL::Congruence::is_inconsistent() const {
  if (inhomogeneous_term() == 0
      || (is_proper_congruence()
	  && ((inhomogeneous_term() % modulus()) == 0)))
    return false;
  for (unsigned i = space_dimension(); i > 0; --i)
    if (row[i] != 0)
      return false;
  return true;
}

void
PPL::Congruence::ascii_dump(std::ostream& s) const {
  const dimension_type row_size = row.size();
  s << "size " << row_size << " ";
  if (row_size > 0) {
    for (dimension_type i = 0; i < row_size - 1; ++i)
      s << row[i] << ' ';
    s << "m " << row[row_size - 1];
  }
  s << std::endl;
}

PPL_OUTPUT_DEFINITIONS(Congruence)

bool
PPL::Congruence::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  const dimension_type old_size = row.size();
  if (new_size < old_size)
    row.shrink(new_size);
  else if (new_size > old_size) {
    Dense_Row y(new_size);
    std::swap(row, y);
  }

  if (new_size > 0) {
    for (dimension_type col = 0; col < new_size - 1; ++col)
      if (!(s >> row[col]))
	return false;
    if (!(s >> str) || str != "m")
      return false;
    if (!(s >> row[new_size-1]))
      return false;
  }
  return true;
}

bool
PPL::Congruence::OK() const {
  // A Congruence must be a valid Dense_Row.
  if (!row.OK())
    return false;

  // Modulus check.
  if (modulus() < 0) {
#ifndef NDEBUG
    std::cerr << "Congruence has a negative modulus " << modulus() << "."
	      << std::endl;
#endif
    return false;
  }

  // All tests passed.
  return true;
}

PPL::Congruence&
PPL::operator+=(Congruence& c1, const Congruence& c2) {
  PPL_ASSERT(c1.is_proper_congruence() || c2.is_proper_congruence());
  if (!c1.is_proper_congruence())
    c1.set_modulus(c2.modulus());
  if (c1.space_dimension() < c2.space_dimension())
    c1.set_space_dimension(c2.space_dimension());
  for (dimension_type i = c2.space_dimension() + 1; i-- > 0; )
    c1.row[i] += c2.row[i];
  return c1;
}

PPL::Congruence&
PPL::operator-=(Congruence& c1, const Congruence& c2) {
  PPL_ASSERT(c1.is_proper_congruence() || c2.is_proper_congruence());
  if (!c1.is_proper_congruence())
    c1.set_modulus(c2.modulus());
  if (c1.space_dimension() < c2.space_dimension())
    c1.set_space_dimension(c2.space_dimension());
  for (dimension_type i = c2.space_dimension() + 1; i-- > 0; )
    c1.row[i] -= c2.row[i];
  return c1;
}

void
PPL::add_mul_assign(Congruence& c1,
                    Coefficient_traits::const_reference factor,
                    const Congruence& c2) {
  PPL_ASSERT(c1.is_proper_congruence() || c2.is_proper_congruence());
  if (!c1.is_proper_congruence())
    c1.set_modulus(c2.modulus());
  if (c1.space_dimension() < c2.space_dimension())
    c1.set_space_dimension(c2.space_dimension());
  for (dimension_type i = c2.space_dimension() + 1; i-- > 0; )
    add_mul_assign(c1.row[i], factor, c2.row[i]);
}

void
PPL::sub_mul_assign(Congruence& c1,
                    Coefficient_traits::const_reference factor,
                    const Congruence& c2) {
  PPL_ASSERT(c1.is_proper_congruence() || c2.is_proper_congruence());
  if (!c1.is_proper_congruence())
    c1.set_modulus(c2.modulus());
  if (c1.space_dimension() < c2.space_dimension())
    c1.set_space_dimension(c2.space_dimension());
  for (dimension_type i = c2.space_dimension() + 1; i-- > 0; )
    sub_mul_assign(c1.row[i], factor, c2.row[i]);
}


const PPL::Congruence* PPL::Congruence::zero_dim_false_p = 0;
const PPL::Congruence* PPL::Congruence::zero_dim_integrality_p = 0;

void
PPL::Congruence::initialize() {
  PPL_ASSERT(zero_dim_false_p == 0);
  zero_dim_false_p
    = new Congruence((Linear_Expression::zero() %= Coefficient(-1)) / 0);

  PPL_ASSERT(zero_dim_integrality_p == 0);
  zero_dim_integrality_p
    = new Congruence(Linear_Expression::zero() %= Coefficient(-1));
}

void
PPL::Congruence::finalize() {
  PPL_ASSERT(zero_dim_false_p != 0);
  delete zero_dim_false_p;
  zero_dim_false_p = 0;

  PPL_ASSERT(zero_dim_integrality_p != 0);
  delete zero_dim_integrality_p;
  zero_dim_integrality_p = 0;
}
