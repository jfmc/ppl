/* Congruence class implementation (non-inline functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Congruence.defs.hh"

#include "Variable.defs.hh"
#include <cassert>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Congruence::sign_normalize() {
  Row& x = *this;
  const dimension_type sz = x.size() - 1;
  // `first_non_zero' indicates the index of the first
  // coefficient of the row different from zero, disregarding
  // the very first coefficient (inhomogeneous term / divisor).
  dimension_type first_non_zero;
  for (first_non_zero = 1; first_non_zero < sz; ++first_non_zero)
    if (x[first_non_zero] != 0)
      break;
  if (first_non_zero < sz)
    // If the first non-zero coefficient of the row is negative,
    // negate all the coefficients and the inhomogeneous term.
    if (x[first_non_zero] < 0) {
      for (dimension_type j = first_non_zero; j < sz; ++j)
	negate(x[j]);
      // Also negate the inhomogeneous term.
      negate(x[0]);
    }
}

void
PPL::Congruence::normalize() {
  sign_normalize();

  dimension_type sz = size();
  if (sz == 0)
    return;

  TEMP_INTEGER(mod);
  mod = modulus();
  if (mod == 0)
    return;

  Congruence& row = (*this);
  for (dimension_type col = 1; col < sz - 1 /* modulus */; ++col) {
    if (row[col] == 0)
      continue;
    // Factor the modulus out of the inhomogeneous term.
    if ((row[0] %= mod) < 0)
      // Make the inhomogeneous term positive.
      row[0] += mod;
    return;
  }

  // All the coefficients are zero.  For such a row the reduction and
  // conversion algorithms require a value in the inhomogeneous term,
  // at least for the first row in the congruence system.
  row[0] = mod;
}

void
PPL::Congruence::strong_normalize() {
  normalize();

  if (modulus() == 0)
    return;

  Row& x = *this;
  // Compute the GCD of all the coefficients into gcd.
  TEMP_INTEGER(gcd);
  gcd = 0;
  const dimension_type sz = size();
  for (dimension_type i = sz; i-- > 0; ) {
    const Coefficient& x_i = x[i];
    if (x_i != 0)
      gcd_assign(gcd, x_i);
  }
  if (gcd > 1)
    // Divide the coefficients by the GCD.
    for (dimension_type i = sz; i-- > 0; )
      exact_div_assign(x[i], gcd);
}

/*! \relates Parma_Polyhedra_Library::Congruence */
PPL::Congruence
PPL::operator%=(const Linear_Expression& e1, const Linear_Expression& e2) {
  // Ensure that diff is created with capacity for the modulo.
  dimension_type dim, e1_dim, e2_dim;
  e1_dim = e1.space_dimension();
  e2_dim = e2.space_dimension();
  if (e1_dim > e2_dim)
    dim = e1_dim;
  else
    dim = e2_dim;
  Linear_Expression diff(e1_dim > e2_dim ? e1 : e2,
			 dim + 1,
			 dim + 2);
  diff -= (e1_dim > e2_dim ? e2 : e1);
  Congruence cg(diff, 1);
  return cg;
}

void
PPL::Congruence::throw_dimension_incompatible(const char* method,
					      const char* v_name,
					      const Variable v) const {
  std::ostringstream s;
  s << "PPL::Congruence::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << v_name << ".space_dimension() == " << v.space_dimension() << ".";
  throw std::invalid_argument(s.str());
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
  s << " == " << -c.inhomogeneous_term()
    << " (mod " << c.modulus() << ")";
  return s;
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

#if 0
  if (!(s >> str))
    return false;
  if (str == "=")
    x.set_is_equality();
  else
    x.set_is_inequality();
#endif

  return true;
}

bool
PPL::Congruence::OK() const {
#if 0 // FIX -1 currently used for virtual row.
  // Modulus check.
  if (modulus() < 0) {
#ifndef NDEBUG
    std::cerr << "Congruence has a negative modulus " << modulus() << "."
	      << std::endl;
#endif
    return false;
  }
#endif

  // All tests passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Congruence */
void
PPL::scalar_product_assign(Coefficient& z,
			   const Linear_Row& x, const Congruence& y) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size() - 1);
  z = 0;
  for (dimension_type i = x.size(); i-- > 0; )
    // The following line optimizes the computation of z += x[i] *
    // y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Congruence */
void
PPL::reduced_scalar_product_assign(Coefficient& z,
				   const Linear_Row& x, const Congruence& y) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  assert(x.size() <= y.size());
  z = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; )
    // The following line optimizes z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Congruence */
void
PPL::scalar_product_assign(Coefficient& z,
			   const Linear_Row& x, const Congruence& y,
			   const Linear_Row& ref) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size() - 1);
  z = 0;
  for (dimension_type i = x.size(); i-- > 0; ) {
    // z += (ref[i] + x[i]) * y[i].
    TEMP_INTEGER(ele);
    ele = ref[i] + x[i];
    // The following line optimizes z += ele * y[i].
    add_mul_assign(z, ele, y[i]);
  }
}

/*! \relates Parma_Polyhedra_Library::Congruence */
void
PPL::reduced_scalar_product_assign(Coefficient& z,
				   const Linear_Row& x, const Congruence& y,
				   const Linear_Row& ref) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  assert(x.size() <= y.size());
  z = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; ) {
    // z += (x[i] + ref[i]) * y[i].
    TEMP_INTEGER(ele);
    ele = x[i] + ref[i];
    // The following line optimizes the computation
    // of z += ele * y[i].
    add_mul_assign(z, ele, y[i]);
  }
}

/*! \relates Parma_Polyhedra_Library::Congruence */
void
PPL::homogeneous_scalar_product_assign(Coefficient& z,
				       const Linear_Row& x, const Congruence& y) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size() - 1);
  z = 0;
  // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = x.size(); --i > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}
