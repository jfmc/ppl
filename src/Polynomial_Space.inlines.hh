/* Polynomial_Space class implementation: inline functions.
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

#ifndef PPL_Polynomial_Space_inlines_hh
#define PPL_Polynomial_Space_inlines_hh 1

#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include "Polynomial_Constraint_System.defs.hh"
#include "Polynomial_Constraint.defs.hh"
#include "Polynomial.defs.hh"
#include "Polynomial.inlines.hh"
#include "Term.defs.hh"
#include <cassert>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <algorithm>
#include <list>

namespace Parma_Polyhedra_Library {

template <degree_type db>
inline dimension_type
Polynomial_Space<db>::max_space_dimension() {
  return Polynomial::max_space_dimension();
}

template <degree_type db>
inline
Polynomial_Space<db>::Polynomial_Space(const Polynomial_Space& y)
  : polynomials(y.polynomials),
    in_canonical_form(y.in_canonical_form),
    dim(y.dim) {

  assert(OK());
}

template <degree_type db>
template <degree_type eb>
inline
Polynomial_Space<db>::Polynomial_Space(const Polynomial_Space<eb>& y)
  : polynomials(y.polynomials),
    in_canonical_form(y.in_canonical_form),
    dim(y.dim) {

  // Existentially quantifying over the terms of degree > `db'.
  if (eb > db)
    existentially_quantify(db);

  // Closing by products up to the new degree bound `db'.
  else if (eb < db)
    close_by_products();

  assert(OK());
}

template <degree_type db>
inline void
Polynomial_Space<db>::add_constraint(const Constraint& c) {
  // Dimension-compatibility check:
  // the dimension of `c' cannot be greater than that of *this.
  if (space_dimension() < c.space_dimension()) {
    throw_dimension_incompatible("add_constraint(c)", "c", c);
  }

  // CHECK ME: is ignoring the right behavior?
  if (c.type() != Constraint::EQUALITY) {
    assert(OK());
    return;
  }

  // Avoiding adding the zero polynomial.
  if (c.is_tautological()) {
    assert(OK());
    return;
  }

  Polynomial aux = Polynomial(Linear_Expression(c));
  aux.primitive_form_assign();
  polynomials.push_back(aux);

  // The list of polynomials cannot be empty.
  in_canonical_form = false;
  close_by_products();

  assert(OK());
}

template <degree_type db>
inline void
Polynomial_Space<db>::
add_polynomial_constraint(const Polynomial_Constraint& c) {
  // Dimension-compatibility check:
  // the dimension of `c' cannot be greater than that of *this.
  if (space_dimension() < c.space_dimension())
    throw_dimension_incompatible("add_polynomial_constraint(c)", "c", c);

  if (c.relation() != Polynomial_Constraint::EQUALITY)
    // CHECK ME: is ignoring the right behavior?
    return;

  // Avoiding adding the zero polynomial.
  const Polynomial& p = c.polynomial();
  if (p.is_equal_to(Polynomial::zero()))
    return;
  // Avoiding adding a polynomial whose degree exceeds `db'.
  if (p.degree() > db)
    return;

  polynomials.push_back(p);

  // The list of polynomials cannot be empty.
  in_canonical_form = false;
  close_by_products();

  assert(OK());
}

template <degree_type db>
inline Polynomial_Space<db>&
Polynomial_Space<db>::operator=(const Polynomial_Space& y) {
  polynomials = y.polynomials;
  in_canonical_form = y.in_canonical_form;
  dim = y.dim;
  return *this;
}

template <degree_type db>
inline void
Polynomial_Space<db>::swap(Polynomial_Space& y) {
  std::swap(polynomials, y.polynomials);
  std::swap(in_canonical_form, y.in_canonical_form);
  std::swap(dim, y.dim);
}

template <degree_type db>
inline dimension_type
Polynomial_Space<db>::space_dimension() const {
  return dim;
}

template <degree_type db>
inline bool
Polynomial_Space<db>::is_bottom() const {
  canonicalize();
  // If there are no constraints, it is the top.
  if (polynomials.empty())
    return false;
  // If 1 == 0 is a constraint, then the space is the bottom.
  // Otherwise, we return `false'.
  // Note: when in canonical form, it suffices to check the last
  // polynomial.
  return polynomials.rbegin()->degree() == 0;
}

template <degree_type db>
inline bool
Polynomial_Space<db>::is_top() const {
  canonicalize();
  // Space is top iff it has no non-trivial polynomials.
  return polynomials.empty();
}

template <degree_type db>
inline bool
operator!=(const Polynomial_Space<db>& x, const Polynomial_Space<db>& y) {
  return !(x == y);
}

// CHECK ME: would it be more efficient not to return a term, but
// take a reference as an argument?
inline Term
end_graded_lex_term(const degree_type db) {
  // The first term of degree `db' + 1 is A^(`db' + 1).
  return pow(Variable(0), db + 1);
}

template <degree_type db>
inline void
Polynomial_Space<db>::join_assign(const Polynomial_Space& y) {

  // If both arguments are in canonical form, then we call adapted
  // Karr's algorithm. Otherwise, we call the other algorithm, which
  // does not require the arguments to be in canonical form, so saving
  // some computation time.

  if (in_canonical_form && y.in_canonical_form) 
    join_assign_karr(y);
  else
    join_assign_canonical_not_required(y);
}

template <degree_type db>
inline void
Polynomial_Space<db>::upper_bound_assign(const Polynomial_Space& y) {
  join_assign(y);
}

template <degree_type db>
template <degree_type eb>
inline void
Polynomial_Space<db>::difference_assign(const Polynomial_Space<eb>& y) {
  polynomial_space_difference_assign(y);
}

template <degree_type db>
inline void
Polynomial_Space<db>::add_space_dimensions_and_embed(const dimension_type m) {
  // The space dimension of the resulting polynomial space should not
  // overflow the maximum allowed space dimension.
  if (m > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("add_space_dimensions_and_embed(m)",
				   "adding m new space dimensions exceeds "
				   "the maximum allowed space dimension");

  // Adding no dimensions is a no-op.
  if (m == 0)
    return;
  dim += m;
  close_by_products();
  assert(OK());
}

template <degree_type db>
inline void
Polynomial_Space<db>::affine_preimage(const Variable var,
				      const Linear_Expression& expr,
				      Coefficient_traits::const_reference
				      denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the polynomial space.
  const dimension_type w = var.id() + 1;
  if (w > space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  // Computing the degree of each polynomial in the space with
  // respect to the variable `var' (stored in the list `degrees'),
  // and also the maximum of these degrees (stored in `max_deg').
  exponent_type max_deg;
  std::list<exponent_type> degrees;
  compute_degrees(var, max_deg, degrees);

  // If `var' does not occur, nothing to do. This avoids computing
  // the numerator of the inverse map unnecessarily.
  if (max_deg == 0) {
    assert(OK());
    return;
  }
  // Transforming the numerator of the map into a polynomial.
  Polynomial num(expr);

  // Substituting the occurrences of `var' by `num'/`den'.
  substitute_assign(var, num, denominator, max_deg, degrees);

  assert(OK());
}

template <degree_type db>
inline void
Polynomial_Space<db>::polynomial_preimage(const Variable var,
					  const Polynomial& expr,
					  Coefficient_traits::const_reference
					  denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("polynomial_preimage(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("polynomial_preimage(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the polynomial space.
  const dimension_type w = var.id() + 1;
  if (w > space_dim)
    throw_dimension_incompatible("polynomial_preimage(v, e, d)", var.id());

  // Computing the degree of each polynomial in the space with
  // respect to the variable `var' (stored in the list `degrees'),
  // and also the maximum of these degrees (stored in `max_deg').
  exponent_type max_deg;
  std::list<exponent_type> degrees;
  compute_degrees(var, max_deg, degrees);

  // If `var' does not occur, nothing to do. This avoids computing
  // the numerator of the inverse map unnecessarily.
  if (max_deg == 0) {
    assert(OK());
    return;
  }
  // Substituting the occurrences of `var' by `num'/`den'.
  substitute_assign(var, expr, denominator, max_deg, degrees);

  assert(OK());
}

template <degree_type db>
inline void
Polynomial_Space<db>::widening_assign(const Polynomial_Space& y,
				      unsigned* tp) {
  // no widening is required, as there cannot be infinite ascending
  // chains
  join_assign(y);

  assert(OK());
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Polynomial_Space */
template <Parma_Polyhedra_Library::degree_type db>
inline void
swap(Parma_Polyhedra_Library::Polynomial_Space<db>& x,
     Parma_Polyhedra_Library::Polynomial_Space<db>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polynomial_Space_inlines_hh)
