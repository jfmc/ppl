/* Constraint class implementation (non-inline functions).
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

#include "Constraint.defs.hh"

#include "Variable.defs.hh"
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Constraint::throw_dimension_incompatible(const char* method,
					      Variable v) const {
  std::ostringstream s;
  s << method << ":" << std::endl
    << "this->space_dimension() == " << this->space_dimension()
    << ", v.id() == " << v.id();
  throw std::invalid_argument(s.str());
}

PPL::Constraint
PPL::operator>>(const Constraint& y, unsigned int offset) {
  size_t y_size = y.size();
  Constraint x(y.Row::type(), y_size+offset);
  x[0] = y[0];
  for (size_t i = 1; i < y_size; ++i)
    x[i+offset] = y[i];
  return x;
}

PPL::Constraint
PPL::Constraint::construct_epsilon_geq_zero() {
  LinExpression e = Variable(0);
  Constraint c = Constraint(e);
  c.set_not_necessarily_closed();
  c.set_is_ray_or_point_or_inequality();
  return c;
}

bool
PPL::Constraint::is_trivial_true() const {
  assert(size() > 0);
  const Constraint& x = *this;
  if (x.all_homogeneous_terms_are_zero())
    if (is_equality())
      return x[0] == 0;
    else
      // Non-strict inequality constraint.
      return x[0] >= 0;
  else
    // There is a non-zero homogeneous coefficient.
    if (is_necessarily_closed())
      return false;
    else {
      // The constraint is NOT necessarily closed.
      size_t eps_index = size() - 1;
      int eps_sign = sgn(x[eps_index]);
      if (eps_sign > 0)
	// We have found the constraint epsilon >= 0.
	return true;
      if (eps_sign == 0)
	// One of the `true' dimensions has a non-zero coefficient.
	return false;
      else {
	// Here the epsilon coefficient is negative: strict inequality.
	if (x[0] <= 0)
	  // A strict inequality such as `lhs - k > 0',
	  // where k is a non negative integer, cannot be trivially true.
	  return false;
	// Checking for another non-zero coefficient.
	for (size_t i = eps_index; --i > 0; )
	  if (x[i] != 0)
	    return false;
	// We have the inequality `k > 0',
	// where k is a positive integer. 
	return true;
      }
    }
}


bool
PPL::Constraint::is_trivial_false() const {
  assert(size() > 0);
  const Constraint& x = *this;
  if (x.all_homogeneous_terms_are_zero())
    // The inhomogeneous term is the only non-zero coefficient.
    if (is_equality())
      return (x[0] != 0);
    else
      // Non-strict inequality constraint.
      return (x[0] < 0);
  else
    // There is a non-zero homogeneous coefficient.
    if (is_necessarily_closed())
      return false;
    else {
      // The constraint is NOT necessarily closed.
      size_t eps_index = size() - 1;
      if (x[eps_index] >= 0)
	// If positive, we have found the constraint epsilon >= 0.
	// If zero, one of the `true' dimensions has a non-zero coefficient.
	// In both cases, it is not trivially false.
	return false;
      else {
	// Here the epsilon coefficient is negative: strict inequality.
	if (x[0] > 0)
	  // A strict inequality such as `lhs + k > 0',
	  // where k is a positive integer, cannot be trivially false.
	  return false;
	// Checking for another non-zero coefficient.
	for (size_t i = eps_index; --i > 0; )
	  if (x[i] != 0)
	    return false;
	// We have the inequality `k > 0',
	// where k is zero or a negative integer. 
	return true;
      }
    }
}


/*!
  \relates Parma_Polyhedra_Library::Constraint
*/
std::ostream&
PPL::operator<<(std::ostream& s, const Constraint& c) {
  int num_variables = c.space_dimension();
  bool first = true;
  for (int v = 0; v < num_variables; ++v) {
    Integer cv = c.coefficient(Variable(v));
    if (cv != 0) {
      if (!first) {
	if (cv > 0)
	  s << " + ";
	else {
	  s << " - ";
	  negate(cv);
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
    s << "0";
  const char* relation_symbol = 0;
  switch (c.type()) {
  case Constraint::EQUALITY:
    relation_symbol = " = ";
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    relation_symbol = " >= ";
    break;
  case Constraint::STRICT_INEQUALITY:
    relation_symbol = " > ";
    break;
  }
  s << relation_symbol << -c.inhomogeneous_term();
  return s;
}


bool
PPL::Constraint::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // A constraint has to be normalized.
  Constraint tmp = *this;
#if EXTRA_NORMALIZATION
  tmp.strongly_normalize();
#else
  tmp.normalize();
#endif
  if (tmp != *this) {
#ifndef NDEBUG
    cerr << "Constraints should be ";
#if EXTRA_NORMALIZATION
    cerr << "strongly ";
#endif
    cerr << "normalized!"
	 << endl;
#endif
    return false;
  }

  // All tests passed.
  return true;
}
