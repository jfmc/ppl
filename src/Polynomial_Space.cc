/* Polynomial_Space class implementation: non-inline functions.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "Polynomial_Space.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::next_graded_lex_term(dimension_type num_dimensions, Term& t) {

  dimension_type i;
  for (i = 0;
       i < num_dimensions - 1 && t.exponent(Variable(i)) == 0;
       ++i) ;

  exponent_type aux = t.exponent(Variable(i));

  if (i < num_dimensions - 1) {
    div_pow_assign(t, Variable(i), aux);
    mul_pow_assign(t, Variable(0), aux - 1);
    t *= Variable(i+1);
  }
  else {
    Variable var(num_dimensions - 1);
    exponent_type exp = t.exponent(var);

    if (exp > 0)
      div_pow_assign(t, var, exp);

    mul_pow_assign(t, Variable(0), aux + 1);
  }
}

PPL::Polynomial
PPL::move_to_higher_space_dimensions(const Polynomial& p,
				     dimension_type offset) {
  Polynomial result;
  for (Polynomial::const_iterator i = p.begin(),
	 i_end = p.end(); i != i_end; ++i) {
    const Monomial& m = *i;
    const Term& t = m.term();
    Term new_t;
    // Moving each of the monomials 'offset' dimensions.
    for (dimension_type j = t.space_dimension(); j-- > 0; ) {
      exponent_type exp = t.exponent(Variable(j));
      // TO DO: could this be made more efficient with a
      // 'move_to_higher_space_dimensions' for Terms?
      new_t *= pow(Variable(j + offset), exp);
    }
    result += m.coefficient() * new_t;
  }
  return result;
}

PPL::Polynomial
PPL::substitute(const Polynomial& p, Variable x, const Polynomial& q) {

  // Accumulates the result of the substitution.
  Polynomial result;
  for (Polynomial::const_iterator i = p.begin(),
	 i_end = p.end(); i != i_end; ++i) {
    const Monomial& m = *i;
    const Term& t = m.term();
    exponent_type x_exp = t.exponent(x);
    if (x_exp == 0)
      // If the variable `x' is not in monomial `m', just add the
      // monomial to `result'.
      result += m;
    else {
      // Substituting `x' by `q' in the monomial `m' and adding the
      // obtained monomial to `result'.
      Term new_t;
      for (dimension_type j = t.space_dimension(); j-- > 0; ) {
	exponent_type exp = t.exponent(Variable(j));
	if (x != Variable(j) && exp > 0)
	  new_t *= pow(Variable(j), exp);
      }
      Polynomial new_q = pow(q, x_exp);
      result += m.coefficient() * new_t * new_q;
    }
  }
  return result;
}
