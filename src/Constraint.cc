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

// CHECK ME.
PPL::Constraint
PPL::operator>>(const Constraint& y, unsigned int offset) {
  size_t y_size = y.size();
  Constraint x(y.Row::type(), y_size+offset);
  x[0] = y[0];
  for (size_t i = 1; i < y_size; ++i)
    x[i+offset] = y[i];
  return x;
}

// CHECK ME.
bool
PPL::Constraint::is_trivial_true() const {
  assert(size() > 0);
  const Constraint& x = *this;
  if (!x.all_homogeneous_terms_are_zero())
    return false;
  else if (is_equality())
    return (x[0] == 0);
  else
    // Inequality constraint.
    return (x[0] >= 0);
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
      if (x[eps_index] == 0)
	// One of the `true' dimensions has a non-zero coefficient.
	return false;
      else {
	assert(x[eps_index] < 0);
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


/*! \relates Parma_Polyhedra_Library::Constraint */
std::ostream&
PPL::operator<<(std::ostream& s, const Constraint& c) {
  int num_variables = c.space_dimension();
  bool first = true;
  for (int v = 0; v < num_variables; ++v) {
    Integer cv = c[v+1];
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
  switch (c.type()) {
  case Constraint::EQUALITY:
    s << " = ";
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    s << " >= ";
    break;
  case Constraint::STRICT_INEQUALITY:
    s << " > ";
    break;
  }
  s << -c[0];
  return s;
}
