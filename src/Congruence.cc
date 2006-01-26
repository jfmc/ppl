/* Congruence class implementation (non-inline functions).
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Congruence.defs.hh"

#include "Variable.defs.hh"
#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace PPL = Parma_Polyhedra_Library;

PPL::Congruence::Congruence(const Constraint& c)
  : Row(c.is_equality()
	? c
	: (throw_invalid_argument("Congruence(c)",
				  "constraint c must be an equality."),
	   c),
	c.space_dimension() + 2,
	compute_capacity(c.space_dimension() + 2, Row::max_size())) {

  (*this)[size()-1] = 0;
}

PPL::Congruence::Congruence(const Constraint& c,
			    dimension_type sz, dimension_type capacity)
  : Row(c.is_equality()
	? c
	: (throw_invalid_argument("Congruence(c)",
				  "constraint c must be an equality."),
	   c),
	sz,
	capacity) {

  (*this)[sz-1] = 0;
}

void
PPL::Congruence::sign_normalize() {
  Row& x = *this;
  const dimension_type sz = x.size() - 1;
  // `first_non_zero' indicates the index of the first
  // coefficient of the row different from zero, disregarding
  // the very first coefficient (inhomogeneous term).
  dimension_type first_non_zero;
  for (first_non_zero = 1; first_non_zero < sz; ++first_non_zero)
    if (x[first_non_zero] != 0)
      break;
  if (first_non_zero < sz)
    // If the first non-zero coefficient of the row is negative,
    // negate all the coefficients and the inhomogeneous term.
    if (x[first_non_zero] < 0) {
      for (dimension_type j = first_non_zero; j < sz; ++j)
	neg_assign(x[j]);
      // Also negate the inhomogeneous term.
      neg_assign(x[0]);
    }
}

void
PPL::Congruence::normalize() {
  sign_normalize();

  dimension_type sz = size();
  if (sz == 0)
    return;

  Coefficient_traits::const_reference mod = modulus();
  if (mod == 0)
    return;

  Coefficient& row_0 = (*this)[0];
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
  Row::normalize();
}

/*! \relates Parma_Polyhedra_Library::Congruence */
PPL::Congruence
PPL::operator%=(const Linear_Expression& e1, const Linear_Expression& e2) {
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
  Congruence cg(diff, 1, false);
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
  throw_invalid_argument(method, s.str().c_str());
}

/*! \relates Parma_Polyhedra_Library::Congruence */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Congruence& c) {
  const int num_variables = c.space_dimension();
  bool first = true;
  for (int v = 0; v < num_variables; ++v) {
    Coefficient cv = c.coefficient(Variable(v));
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
PPL::Congruence::is_trivial_true() const {
  if ((is_equality() && inhomogeneous_term() == 0)
      || (is_proper_congruence()
	  && (inhomogeneous_term() % modulus() == 0))) {
    for (unsigned i = 1; i <= space_dimension(); i++)
      if ((*this)[i] != 0)
	return false;
    return true;
  }
  return false;
}

bool
PPL::Congruence::is_trivial_false() const {
  if (inhomogeneous_term() == 0
      || (is_proper_congruence()
	  && ((inhomogeneous_term() % modulus()) == 0)))
    return false;
  for (unsigned i = 1; i <= space_dimension(); i++)
    if ((*this)[i] != 0)
      return false;
  return true;
}

void
PPL::Congruence::ascii_dump(std::ostream& s) const {
  const Row& x = *this;
  dimension_type x_size = x.size();
  for (dimension_type i = 0; i < x_size - 1; ++i)
    s << x[i] << ' ';
  if (x_size)
    s << "m " << x[x_size - 1];
  s << std::endl;
}

PPL_OUTPUT_DEFINITIONS(Congruence);

bool
PPL::Congruence::ascii_load(std::istream& s) {
  std::string str;
  Congruence& x = *this;
  dimension_type col = 0;
  while (col < x.size() - 1)
    if (!(s >> x[col]))
      return false;
    else
      col++;

  if (!(s >> str) || str.compare("m"))
    return false;

  if (!(s >> x[col]))
    return false;

  return true;
}

bool
PPL::Congruence::OK() const {
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
