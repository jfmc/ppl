/* Polynomial_Space class implementation: non-inline template functions.
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

#ifndef PPL_Polynomial_Space_templates_hh
#define PPL_Polynomial_Space_templates_hh 1

#include "Constraint.defs.hh"
#include "Generator.defs.hh"
#include "Variables_Set.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <degree_type db>
Polynomial_Space<db>::Polynomial_Space(const dimension_type num_dimensions,
				       const Degenerate_Space kind)
  : polynomials(),
    in_canonical_form(true),
    dim(num_dimensions <= max_space_dimension()
	? num_dimensions
	: (throw_space_dimension_overflow("Polynomial_Space<db>(n, k)",
					  "n exceeds the maximum "
					  "allowed space dimension"), 0)) {
  // If `kind == TOP', the result is the polynomial space {0},
  // so that there is nothing left to do.
  // If `kind == BOTTOM', the result is the inconsistent polynomial space
  // containing Polynomial(1); by degree-bounded product closure, this
  // generates all terms of degree <= `db'.
  if (kind == BOTTOM) {
    if (num_dimensions == 0)
      polynomials.push_front(Polynomial(1));
    else
      // CHECK ME: what are the pros of insisting on product closure
      // even when the polynomial space is inconsistent?
      for (Term iter, iter_end = end_graded_lex_term(db);
	   iter != iter_end; next_graded_lex_term(dim, iter))
	// Pushing terms at the *front* gives the right order.
	polynomials.push_front(Polynomial(iter));
  }
  // In either case, `polynomials' is in canonical form.
  assert(OK());
}

template <degree_type db>
Polynomial_Space<db>::Polynomial_Space(dimension_type num_dimensions,
				       const Constraint_System& cs)
  : polynomials(),
    in_canonical_form(true),
    dim(num_dimensions <= max_space_dimension()
	? num_dimensions
	: (throw_space_dimension_overflow("Polynomial_Space<db>(n, cs)",
					  "n exceeds the maximum "
					  "allowed space dimension"), 0)) {
  add_constraints(cs);
  assert(OK());
}

template <degree_type db>
Polynomial_Space<db>::Polynomial_Space(dimension_type num_dimensions,
				       const Polynomial_Constraint_System& pcs)
  : polynomials(),
    in_canonical_form(true),
    dim(num_dimensions <= max_space_dimension()
	? num_dimensions
	: (throw_space_dimension_overflow("Polynomial_Space<db>(n, pcs)",
					  "n exceeds the maximum "
					  "allowed space dimension"), 0)) {
  add_polynomial_constraints(pcs);
  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::
add_constraints(const Constraint_System& cs) {
  // Dimension-compatibility check:
  // the dimension of `c' cannot be greater than that of `*this'.
  if (space_dimension() < cs.space_dimension())
    throw_dimension_incompatible("add_constraints(cs)", "cs", cs);

  for (Constraint_System::const_iterator i = cs.begin(),
	 i_end = cs.end(); i != i_end; ++i) {

    if (i->type() != Constraint::EQUALITY)
      // CHECK ME: is ignoring the right behavior?
      continue;

    // Avoiding adding the zero polynomial.
    if (!i->is_tautological()) {
      // Transforming left-hand sides of constraints into polynomials.
      Polynomial aux = Polynomial(Linear_Expression(*i));
      // NOTE: the linear expression extracted from a Constraint
      // is already normalized. Hence, primitive form is obtained by
      // simply computing its sign-normalization.
      aux.sign_normalize();
      assert(aux.check_primitive_form());
      polynomials.push_back(aux);
    }
  }
  // Adding all polynomials and then closing minimizes the number of
  // closures to 1.
  in_canonical_form = polynomials.empty();
  close_by_products();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::
add_polynomial_constraints(const Polynomial_Constraint_System& cs) {
  // Dimension-compatibility check:
  // the dimension of `c' cannot be greater than that of `*this'.
  if (space_dimension() < cs.space_dimension())
    throw_dimension_incompatible("add_polynomial_constraints(cs)", "cs", cs);

  for (Polynomial_Constraint_System::const_iterator i = cs.begin(),
	 i_end = cs.end(); i != i_end; ++i) {

    if (i->relation() != Polynomial_Constraint::EQUALITY)
      // CHECK ME: is ignoring the right behavior?
      continue;

    // Avoiding adding the zero polynomial or a polynomial whose
    // degree exceeds `db'.
    const Polynomial& p = i->polynomial();
    // TODO: could comparisons with 0 be made more efficient?
    if (!p.is_equal_to(Polynomial::zero()) && p.degree() <= db)
      polynomials.push_back(p);
  }
  // Adding all polynomials and then closing minimizes the number of
  // closures to 1.
  in_canonical_form = polynomials.empty();
  close_by_products();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::concatenate_assign(const Polynomial_Space& y) {

  // Moving polynomials to be concatenated a number of
  // `space_dimension()' dimensions and adding them.
  for (C_Iterator i = y.polynomials.begin(), i_end = y.polynomials.end(); i != i_end; ++i)
    polynomials.push_back(move_to_higher_space_dimensions(*i, dim));

  in_canonical_form = polynomials.empty();
  dim += y.space_dimension();
  close_by_products();

  assert(OK());
}

template <degree_type db>
bool
Polynomial_Space<db>::entails(const Polynomial_Space& y) const {

  const Polynomial_Space& x = *this;

  // Dimension-compatibility check:
  if (y.space_dimension() != x.space_dimension())
    throw_dimension_incompatible("entails(y)", y);

  // By duality, we have to check if `y' is included in `x' as a set.

  // We need to canonicalize `x'.
  x.canonicalize();

  bool x_entails_y = true;
  for (C_Iterator i = y.polynomials.begin(),
	 i_end = y.polynomials.end(); i != i_end && x_entails_y; ++i) {
    // Cancelling polynomials in `*this' using the polynomials in `x' as pivots.

    // A copy has to be created for checking if cancellation is possible.
    Polynomial p_i = *i;

    for (C_Iterator j = x.polynomials.begin(), j_end = x.polynomials.end(); j != j_end; ++j) {

      // Using leading term of polynomial in `x' for pivoting.
      const Term& term = j->rbegin()->term();

      // TODO: a possible change (improvement?) is to check if
      // `p_i->rbegin()->term()' > `j->rbegin()->term()'; if this is
      // true, as `x' is in canonical form, `p_i' cannot be
      // cancelled and we can return false;

      const Coefficient& coef_i = p_i.coefficient(term);

      // CHECK ME: is there a more efficient way to compare
      // coefficients to 0?
      if (coef_i != Coefficient_zero()) {
	const Coefficient& coef_j = j->rbegin()->coefficient();
	p_i.linear_combine(*j, term, coef_i, coef_j);
      }
    }
    // CHECK ME: could comparisons with polynomial 0 be made more
    // efficient?
    if (!p_i.is_equal_to(Polynomial::zero()))
      x_entails_y = false;
  }
  return x_entails_y;
}

template <degree_type db>
void
Polynomial_Space<db>::join_assign_karr(const Polynomial_Space& yy) {
  Polynomial_Space& x = *this;
  const dimension_type x_space_dim = x.space_dimension();
  
  // Dimension-compatibility check.
  if (x_space_dim != yy.space_dimension())
    throw_dimension_incompatible("join_assign(yy)", yy);
  
  // Based on the algorithm for computing the hull of affine spaces by
  // M. Karr: "Affine Relationships Among Variables of a Program".

  x.canonicalize();
  yy.canonicalize();

  // CHECK ME: is this copy necessary?
  // Enric: I would say it is...
  Polynomial_Space<db> y(yy);

  Iterator x_i   = x.polynomials.begin();
  Iterator x_end = x.polynomials.end();
  Iterator y_i   = y.polynomials.begin();
  Iterator y_end = y.polynomials.end();
  TEMP_INTEGER(divisor);
  TEMP_INTEGER(x_factor);
  TEMP_INTEGER(y_factor);
  while (x_i != x_end && y_i != y_end) {
    const Term& x_i_term = x_i->rbegin()->term();
    const Term& y_i_term = y_i->rbegin()->term();
    if (x_i_term == y_i_term) {
      // Case 1: both matrices have pivoting rows at the column under
      // consideration.

      // Multiplying the pivots so that the columns coincide.
      const Coefficient& x_i_coeff = x_i->rbegin()->coefficient();
      const Coefficient& y_i_coeff = y_i->rbegin()->coefficient();
      gcd_assign(divisor, x_i_coeff, y_i_coeff);
      exact_div_assign(x_factor, y_i_coeff, divisor);
      exact_div_assign(y_factor, x_i_coeff, divisor);
      *x_i *= x_factor;
      *y_i *= y_factor;
      ++x_i;
      ++y_i;
    }
    else if (Term::Compare()(x_i_term, y_i_term)) {
      // Case 2a: matrix `y' has pivoting row at the column under
      // consideration, but not `x'.

      // Using the pivot row in `y' so as to make the columns coincide.
      const Coefficient& y_i_coeff = y_i->rbegin()->coefficient();
      for (Iterator x_j = x.polynomials.begin(),
	     y_j = y.polynomials.begin(); y_j != y_i; ++x_j, ++y_j) {
	const Coefficient& x_j_y_i_term_coeff = x_j->coefficient(y_i_term);
	if (x_j_y_i_term_coeff != Coefficient_zero()) {
	  gcd_assign(divisor, y_i_coeff, x_j_y_i_term_coeff);
	  exact_div_assign(x_factor, y_i_coeff, divisor);
	  exact_div_assign(y_factor, x_j_y_i_term_coeff, divisor);

	  // TO DO: this could be made more efficient with a
	  // method for computing linear combinations.
	  *y_j *= x_factor;
	  *y_j += y_factor * (*y_i);
	  *x_j *= x_factor;
	}
      }
      // As we erase the pivot, we restore the new end.
      y_i   = y.polynomials.erase(y_i);
      y_end = y.polynomials.end();
    }
    else {
      // Case 2b: matrix `x' has pivoting row at the column under
      // consideration, but not `y'.

      // Using the pivot row in `x' so as to make the columns coincide.
      const Coefficient& x_i_coeff = x_i->rbegin()->coefficient();
      for (Iterator x_j = x.polynomials.begin(),
	     y_j = y.polynomials.begin(); x_j != x_i; ++x_j, ++y_j) {
	const Coefficient& y_j_x_i_term_coeff = y_j->coefficient(x_i_term);
	if (y_j_x_i_term_coeff != Coefficient_zero()) {
	  gcd_assign(divisor, x_i_coeff, y_j_x_i_term_coeff);
	  exact_div_assign(x_factor, x_i_coeff, divisor);
	  exact_div_assign(y_factor, y_j_x_i_term_coeff, divisor);
	  *x_j *= x_factor;
	  *x_j += y_factor * (*x_i);
	  *y_j *= x_factor;
	}
      }
      // As we erase the pivot, we restore the new end.
      x_i   = x.polynomials.erase(x_i);
      x_end = x.polynomials.end();
    }
  }

  while(y_i != y_end) {
    // Case 2a: matrix `y' has pivoting row at the column under
    // consideration, but not `x'.

    // Using the pivot row in `y' so as to make the columns coincide.
    const Term& y_i_term = y_i->rbegin()->term();
    const Coefficient& y_i_coeff = y_i->rbegin()->coefficient();
    for (Iterator x_j = x.polynomials.begin(),
	   y_j = y.polynomials.begin(); y_j != y_i; ++x_j, ++y_j) {
      const Coefficient& x_j_y_i_term_coeff = x_j->coefficient(y_i_term);
      if (x_j_y_i_term_coeff != Coefficient_zero()) {
	gcd_assign(divisor, y_i_coeff, x_j_y_i_term_coeff);
	exact_div_assign(x_factor, y_i_coeff, divisor);
	exact_div_assign(y_factor, x_j_y_i_term_coeff, divisor);
	*y_j *= x_factor;
	*y_j += y_factor * (*y_i);
	*x_j *= x_factor;
      }
    }
    // As we erase the pivot, we restore the new end.
    y_i   = y.polynomials.erase(y_i);
    y_end = y.polynomials.end();
  }

  while(x_i != x_end) {
    // Case 2b: matrix `x' has pivoting row at the column under
    // consideration, but not `y'.

    // Using the pivot row in `x' so as to make the columns coincide.
    const Term& x_i_term = x_i->rbegin()->term();
    const Coefficient& x_i_coeff = x_i->rbegin()->coefficient();
    for (Iterator x_j = x.polynomials.begin(),
	   y_j = y.polynomials.begin(); x_j != x_i; ++x_j, ++y_j) {
      const Coefficient& y_j_x_i_term_coeff = y_j->coefficient(x_i_term);
      if (y_j_x_i_term_coeff != Coefficient_zero()) {
	gcd_assign(divisor, x_i_coeff, y_j_x_i_term_coeff);
	exact_div_assign(x_factor, x_i_coeff, divisor);
	exact_div_assign(y_factor, y_j_x_i_term_coeff, divisor);
	*x_j *= x_factor;
	*x_j += y_factor * (*x_i);
	*y_j *= x_factor;
      }
    }
    // As we erase the pivot, we restore the new end.
    x_i = x.polynomials.erase(x_i);
    x_end = x.polynomials.end();
  }

  // Case 3: none of the matrices has a pivot row at the column under
  // consideration.
  R_Iterator x_k = x.polynomials.rbegin();
  R_Iterator y_k = y.polynomials.rbegin();
  R_Iterator x_rend = x.polynomials.rend();
  R_Iterator y_rend = y.polynomials.rend();
  while (x_k != x_rend) {
    // TO DO: this could be improved by having a method that, given
    // two polynomials `p' and `q', returned a term `t' and `coeff_p'
    // == `p.coefficient(t)' and `coeff_q' == `q.coefficient(t)' such
    // that `coeff_p' != `coeff_q'. This would avoid the copy of the
    // temporary polynomial `aux'.
    Polynomial aux = (*y_k) - (*x_k);
    if (!aux.is_equal_to(Polynomial::zero())) {
      const Term& t = aux.rbegin()->term();
      const Coefficient& diff_i = aux.rbegin()->coefficient();

      assert(diff_i == y_k->coefficient(t) - x_k->coefficient(t));

      R_Iterator x_j = x_k;
      R_Iterator y_j = y_k;
      for (++x_j, ++y_j; x_j != x_rend; ++x_j, ++y_j) {
	const Coefficient& diff_j = y_j->coefficient(t) - x_j->coefficient(t);
	if (diff_j != Coefficient_zero()) {
	  gcd_assign(divisor, diff_i, diff_j);
	  exact_div_assign(x_factor, diff_i, divisor);
	  exact_div_assign(y_factor, diff_j, divisor);
	  *x_j *= x_factor;
	  *x_j -= y_factor * (*x_k);
	  *y_j *= x_factor;
	  *y_j -= y_factor * (*y_k);
	}
      }
      // As we erase the pivot, we restore the new end.
      // NOTE: incrementing the reverse iterators as they are `off-by-one'.
      ++x_k;
      x_k = R_Iterator(x.polynomials.erase(x_k.base()));
      x_rend = x.polynomials.rend();
      ++y_k;
      y_k = R_Iterator(y.polynomials.erase(y_k.base()));
      y_rend = y.polynomials.rend();
    }
    else {
      ++x_k;
      ++y_k;
    }
  }
  // The matrix is guaranteed to be in echelon form.
  // Putting polynomials in `x' in primitive form.
  for (x_k = x.polynomials.rbegin(); x_k != x_rend; ++x_k)
    x_k->primitive_form_assign();

  assert(OK());
}


template <degree_type db>
void
Polynomial_Space<db>::join_assign_canonical_not_required(const Polynomial_Space& y) {
  Polynomial_Space& x = *this;
  
  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    throw_dimension_incompatible("join_assign(yy)", y);
  
  // Computing intersection of vector spaces by multiplying
  // polynomials by a new fresh variable `t' and then existentially
  // eliminating it.

  // Expanding the dimension of `x' to allow for a new variable.
  Variable t(x.dim);
  ++x.dim;

  // Since multiplying by * `t' is cheaper than multiplying by
  // (1-`t'), we do the first with the largest sequence of
  // polynomials.
 
  if (x.polynomials.size() > y.polynomials.size()) {

    // Multiplying by `t'.
    for (Iterator i = x.polynomials.begin(),
	   i_end = x.polynomials.end(); i != i_end; ++i)
      *i *= t;

    // Multiplying by 1 - `t'.
    for (C_Iterator i = y.polynomials.begin(),
	   i_end = y.polynomials.end(); i != i_end; ++i)
      // TO DO: Which is more efficient: `push_back' or `push_front'?
      x.polynomials.push_front((*i) - (*i) * t);
  }
  else {

    // Multiplying by 1 - `t'.
    for (Iterator i = x.polynomials.begin(),
	   i_end = x.polynomials.end(); i != i_end; ++i)
      (*i) -= (*i) * t;

    // Multiplying by `t'.
    for (C_Iterator i = y.polynomials.begin(),
	   i_end = y.polynomials.end(); i != i_end; ++i)
      x.polynomials.push_front((*i) * t);
  }

  // Existentially quantifying all terms involving the auxiliary
  // variable `t'.
  for (Term iter, iter_end = end_graded_lex_term(db);
       iter != iter_end; next_graded_lex_term(x.dim - 1, iter))
    existentially_quantify(t * iter);

  // Restoring the dimension.
  --x.dim;

  assert(OK());
}


template <degree_type db>
template <degree_type eb>
void
Polynomial_Space<db>::
polynomial_space_difference_assign(const Polynomial_Space<eb>& y) {

  const dimension_type space_dim = space_dimension();
  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("polynomial_space_difference_assign(y)", y);

  // If \p *this is zero-dimensional, nothing to do.
  if (space_dim == 0)
    return;

  // Computing the difference for each of the polynomials in `y' and
  // taking the join of the resulting polynomial spaces.
  Polynomial_Space<db> result(space_dim, BOTTOM);
  for (C_Iterator i = y.polynomials.begin(), i_end = y.polynomials.end(); i != i_end; ++i) {

    degree_type deg = i->degree();
    // If `db' < `deg', the final result is *this: so return.
    if (db < deg) {
      assert(OK());
      return;
    }
    // TO DO: would it be better to have an initial check that `eb' <=
    // `db', and return if the check failed?


    // Computing the polynomial space
    // space({t * (*i) | t is a term , deg(t) <= db - deg}).
    Polynomial_Space<db> product(space_dim);
    product.in_canonical_form = false;
    for (Term iter, iter_end = end_graded_lex_term(db - deg);
	 iter != iter_end; next_graded_lex_term(space_dim, iter))
      product.polynomials.push_front(iter * (*i));

    // Computing the polynomials p * (`*i') belonging to `*this'.
    product.join_assign(*this);

    // If only the zero polynomial is obtained, the final result is
    // *this: so return.
    Iterator j     = product.polynomials.begin();
    Iterator j_end = product.polynomials.end();
    if (j == j_end) {
      assert(OK());
      return;
    }
    // Inserting the new polynomials to a copy of `*this' to obtain
    // the quotient.
    Polynomial_Space<db> quotient(*this);
    quotient.in_canonical_form = false;
    for (; j != j_end; ++j)
      quotient.polynomials.push_front(exact_div_assign(*j, *i));

    // Taking the join with previous results.
    result.join_assign(quotient);
  }
  *this = result;
  close_by_products();
  assert(OK());
}


template <degree_type db>
void
Polynomial_Space<db>::
add_space_dimensions_and_project(const dimension_type m) {
  // The space dimension of the resulting polynomial space should not
  // overflow the maximum allowed space dimension.
  if (m > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("add_space_dimensions_and_project(m)",
				   "adding m new space dimensions exceeds "
				   "the maximum allowed space dimension");

  // Adding no dimensions is a no-op.
  if (m == 0) {
    assert(OK());
    return;
  }

  // Adding the equations `Variable(dim + i) == 0', for 0 <= i < m.
  for (dimension_type i = 0; i < m; ++i)
    polynomials.push_back(Polynomial(Variable(dim + i)));

  in_canonical_form = false;
  dim += m;
  close_by_products();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::
remove_higher_space_dimensions(const dimension_type new_dim) {
  // Dimension-compatibility check: the variable having
  // maximum index is the one occurring last in the set.
  if (new_dim > space_dimension())
    throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				 new_dim);

  // The removal of no dimensions from a PC is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a zero-dim space PC.
  if (new_dim == space_dimension()) {
    assert(OK());
    return;
  }

  // Existentially quantifying and removing the space dimensions (no
  // renaming needed).
  for (; dim > new_dim; --dim)
    existentially_quantify_all(Variable(dim - 1));

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::
remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any PS is a no-op.
  // Note that this case also captures the only legal removal of
  // space dimensions from a PS in a 0-dim space.
  const dimension_type sz = to_be_removed.size();
  if (sz == 0) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having
  // maximum cardinality is the one occurring last in the set.
  const dimension_type max_dim_to_be_removed = *to_be_removed.rbegin();
  const dimension_type old_space_dim = space_dimension();
  if (max_dim_to_be_removed >= old_space_dim)
    throw_dimension_incompatible("remove_space_dimensions(vs)",
				 max_dim_to_be_removed);

  // If `db' == 0, there is no variable to be removed.
  if (db == 0) {
    assert(OK());
    return;
  }

  // If the space is zero-dimensional, there is no variable to be
  // removed.
  if (dim == 0) {
    assert(OK());
    return;
  }

  // TO DO: could this be made more efficient?

  // First existentially quantify ...
  // There are two alternatives for the existential quantification:

  // a) For each variable in `to_be_removed', existentially quantify
  //    all terms that have positive exponent in the variable. This
  //    explores size(to_be_removed) * combinatorial_number(dim + db - 1,
  //    dim) terms.

  // b) For each term of degree <= db, check if it has positive
  //    exponent in any of the variables in `to_be_removed'. This explores
  //    combinatorial_number(dim + db, dim) = (dim + db) / (db - 1) *
  //    combinatorial_number(dim + db - 1, dim) terms.

  // So, if size(to_be_removed) < (dim + db) / (db - 1), we do a).
  // Otherwise, we do b) (assuming db > 1; if db == 1, we do a)).

  if ((db - 1) * sz < dim + db)
    for (Variables_Set::iterator i = to_be_removed.begin(),
	   i_end = to_be_removed.end(); i != i_end; ++i)
      existentially_quantify_all(Variable(*i));
  else
    for (Term iter, iter_end = end_graded_lex_term(db);
	 iter != iter_end; next_graded_lex_term(dim, iter)) {
      Variables_Set::iterator i     = to_be_removed.begin();
      Variables_Set::iterator i_end = to_be_removed.end();
      for (; i != i_end && iter.exponent(Variable(*i)) == 0; ++i)
	;
      if (i != i_end)
	existentially_quantify(iter);
    }

  // ... then actually remove the space dimensions.
  for (Iterator p_i = polynomials.begin(),
	 p_end = polynomials.end(); p_i != p_end; ++p_i)
    p_i->shift_space_dimensions(to_be_removed);
  // Update the space dimension.
  dim -= sz;

  assert(OK());

}

template <degree_type db>
template <typename PartialFunction>
void
Polynomial_Space<db>::map_space_dimensions(const PartialFunction& pfunc) {
  // If the polynomial space is 0-dimensional, nothing to do.
  if (dim == 0) {
    assert(OK());
    return;
  }

  // If the partial function has empty codomain,
  // all variables have to be eliminated.
  if (pfunc.has_empty_codomain()) {
    Polynomial_Space<db> temp(0, (is_bottom() ? BOTTOM : TOP));
    swap(temp);
    assert(OK());
    return;
  }

  // Let `aux_func' : {0, ..., m-1} --> {0, ..., m-1} encode the
  // permutation obtained from `pfunc' after eliminating those
  // variables having no image (the unmapped variables).
  const dimension_type new_space_dimension = pfunc.max_in_codomain() + 1;
  std::vector<dimension_type> aux_func(new_space_dimension);
  Variables_Set unmapped_variables;
  for (dimension_type i = 0, j = 0; i < dim; ++i) {
    dimension_type pfunc_i;
    if (pfunc.maps(i, pfunc_i))
      aux_func[j++] = pfunc_i;
    else
      unmapped_variables.insert(Variable(i));
  }

  // Remove the unmapped variables.
  remove_space_dimensions(unmapped_variables);

  // Permute remaining variables according to `aux_func'.
  for (Iterator i = polynomials.begin(), i_end = polynomials.end(); i != i_end; ++i) {
    Polynomial& p = *i;
    p.permute_space_dimensions(aux_func);
    // NOTE: the polynomial is still normalized.
    p.sign_normalize();
    assert(p.check_primitive_form());
  }

  in_canonical_form = polynomials.empty();
  dim = new_space_dimension;
  canonicalize();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::meet_assign(const Polynomial_Space& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("meet_assign(y)", y);

  // Adding the polynomials.
  for (C_Iterator i = y.polynomials.begin(), i_end = y.polynomials.end();
       i != i_end; ++i)
    polynomials.push_back(*i);

  in_canonical_form = false;
  close_by_products();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::affine_image(const Variable var,
				   const Linear_Expression& expr,
				   Coefficient_traits::const_reference
				   denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("affine_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the polynomial space.
  const dimension_type w = var.id() + 1;
  if (w > space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());

  // Checking if the affine map is invertible.
  const Coefficient& new_den = expr.coefficient(var);
  if (new_den != 0) {

    // The map is invertible. The image is obtained by computing the
    // inverse map and then substituting `var' according to this map.

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
    // Transforming the numerator of the inverse map into a
    // polynomial.
    Polynomial new_num = -Polynomial(expr);
    new_num += (denominator + new_den) * var;

    // Substituting the occurrences of `var' by `num'/`den'.
    substitute_assign(var, new_num, new_den, max_deg, degrees);
  }
  else {
    // Transforming the linear map `var' := `expr' / `denominator' into the
    // polynomial:
    //               - `denominator' * `var' + `expr'[`var' <- VAR]
    //
    // where VAR is an extra fresh variable, to be eliminated later.

    Variable new_var(dim);
    Polynomial poly(expr);
    // TO DO: could this be made more efficient? `new_den' * `var' is
    // added and then subtracted.
    poly -= (denominator + new_den) * var;
    poly += new_den * new_var;

    // Substituting in the polynomials of the space `var' <- VAR.
    for (Iterator i = polynomials.begin(), i_end = polynomials.end(); i != i_end; ++i) {
      Polynomial& p_i = *i;
      // TO DO: could this be made more efficient by a method of the
      // kind substitute_assign? Besides, the fact that `new_var' is a
      // fresh single variable in not exploited in the call to
      // `substitute'.
      p_i = substitute(p_i, var, Polynomial(new_var));
    }
    // Adding the new polynomial.
    poly.primitive_form_assign();
    polynomials.push_back(poly);

    // Increase the dimension of the space.
    ++dim;

    in_canonical_form = polynomials.empty();
    close_by_products();

    // Existentially quantify the auxiliary variable `new_var'.
    existentially_quantify_all(new_var);

    // Restore original dimension.
    --dim;
  }
  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::polynomial_image(const Variable var,
				       const Polynomial& expr,
				       Coefficient_traits::const_reference
				       denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("polynomial_image(v, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("polynomial_image(v, e, d)", "e", expr);

  // `var' should be one of the dimensions of the polynomial space.
  const dimension_type w = var.id() + 1;
  if (w > space_dim)
    throw_dimension_incompatible("polynomial_image(v, e, d)", var.id());

  // Checking if the polynomial map is invertible, i.e., if it is of the form:
  // `expr' = coefficient(`var') * `var' + polynomial independent of `var', with
  // coefficient(`var') != 0.
  Coefficient new_den = 0;
  bool invertible = true;
  for (Polynomial::const_iterator j = expr.begin(), j_end = expr.end();
       j != j_end && invertible; ++j) {
    exponent_type exp = j->term().exponent(var);
    if (exp > 1)
      // There is a non-linear term with exponent(`var') > 1: not invertible.
      invertible = false;
    else if (exp == 1 && j->degree() > 1)
      // There is a non-linear term with exponent(`var') == 1: not invertible.
      invertible = false;
    else if (exp == 1)
      // Linear term `var': storing the coefficient.
      new_den = j->coefficient();
  }
  invertible = invertible && new_den != 0;
  if (invertible) {

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
    // Transforming the numerator of the inverse map into a
    // polynomial.
    Polynomial new_num = -expr;
    new_num += (denominator + new_den) * var;

    // Substituting the occurrences of `var' by `num'/`den'.
    substitute_assign(var, new_num, new_den, max_deg, degrees);
  }
  else {
    // Transforming the linear map `var' := `expr' / `denominator' into the
    // polynomial:
    //               - `denominator' * `var' + `expr'[`var' <- VAR]
    //
    // where VAR is an extra fresh variable, to be eliminated later.

    Variable new_var(dim);
    // TO DO: could this be made more efficient? The fact that
    // `new_var' is a fresh single variable in not exploited in the
    // call to `substitute'.
    Polynomial poly = substitute(expr, var, Polynomial(new_var));
    poly -= var * denominator;

    // Substituting in the polynomials of the space `var' <- VAR.
    for (Iterator i = polynomials.begin(), i_end = polynomials.end(); i != i_end; ++i) {
      Polynomial& p_i = *i;
      // TO DO: could this be made more efficient by a method of the
      // kind substitute_assign? Besides, the fact that `new_var' is a
      // fresh single variable in not exploited in the call to
      // `substitute'.
      p_i = substitute(p_i, var, Polynomial(new_var));
    }
    // Adding the new polynomial.
    poly.primitive_form_assign();
    polynomials.push_back(poly);

    // Increase the dimension of the space.
    ++dim;

    in_canonical_form = polynomials.empty();
    close_by_products();

    // Existentially quantify the auxiliary variable `new_var'.
    existentially_quantify_all(new_var);

    // Restore original dimension.
    --dim;
  }
  assert(OK());
}

template <degree_type db>
Polynomial_Constraint_System
Polynomial_Space<db>::polynomial_constraints() const {
  Polynomial_Constraint_System cs;
  for (C_Iterator i = polynomials.begin(), i_end = polynomials.end();
       i != i_end; ++i)
    cs.insert((*i) == 0);

  return cs;
}

/*! \relates Polynomial_Space */
template <degree_type db>
inline bool
operator==(const Polynomial_Space<db>& x,
	   const Polynomial_Space<db>& y) {
  // We need both arguments in canonical form.
  x.canonicalize();
  y.canonicalize();
  if (x.polynomials.size() != y.polynomials.size())
    return false;
  for (typename Polynomial_Space<db>::C_Iterator i = x.polynomials.begin(),
	 j = y.polynomials.begin(), i_end = x.polynomials.end(); i != i_end; ++i, ++j)
    // Here we use that the primitive reduced row-echelon form is a
    // canonical form.
    if (!i->is_equal_to(*j))
      return false;

  return true;
}

/*! \relates Parma_Polyhedra_Library::Polynomial_Space */
template <degree_type db>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Polynomial_Space<db>& ps) {
  if (ps.is_top())
    s << "true";
  else {
    typedef typename Polynomial_Space<db>::C_Iterator C_Iterator;
    C_Iterator i     = ps.polynomials.begin();
    C_Iterator next  = ps.polynomials.begin();
    C_Iterator i_end = ps.polynomials.end();

    // Case of a single polynomial.
    if (++next == i_end)
      s << *i << " == 0";
    else {
      // Tracking next polynomial to handle the last one properly.
      for (; next!= i_end; ++i, ++next)
	s << *i << " == 0, ";
      // Last polynomial.
      s << *i << " == 0";
    }
  }
  return s;
}

template <degree_type db>
bool
Polynomial_Space<db>::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  /*
                                               / dim + db \
   The list of polynomials should have size <= |          | ,
                                               \    db    /

   which is the number of terms of degree at most 'db'
   in `dim' variables.

              / dim + db \    (dim + db) * ... * (dim + 1)
   Using that |          | = -----------------------------
              \    db    /                 db!
  */

  // Computing (dim + db) * ... * (dim + 1)
  unsigned long int bound = 1;
  for (degree_type i = 1; i <= db; ++i)
    bound *= (unsigned long int) (dim + i);
  // Dividing into db! .
  for (degree_type i = 1; i <= db; ++i)
    bound /= (unsigned long int)(i);
  if (polynomials.size() > bound) {
#ifndef NDEBUG
      cerr << "Polynomial_Space of degree " << db
	   << " and having dimension " << dim
	   << " cannot have more than "
	   << bound << " polynomials" << endl;
#endif
	goto bomb;
      }

    // All the polynomials should ...
    for (C_Iterator i = polynomials.begin(),
	   i_end = polynomials.end(); i != i_end; ++i) {
      const Polynomial& p = *i;

      // ... have a compatible space dimension, ...
      if (p.space_dimension() > dim) {
#ifndef NDEBUG
	cerr << "Polynomial_Space of dimension " << dim
	     << " contains a polynomial having dimension "
	     << p.space_dimension() << endl;
#endif
	goto bomb;
      }

      // ... be non-null, ...
      if (p.is_equal_to(Polynomial::zero())) {
#ifndef NDEBUG
	cerr << "Polynomial_Space contains a zero polynomial" << endl;
#endif
	goto bomb;
      }

      // ... be in primitive form.
      if (!p.check_primitive_form()) {
#ifndef NDEBUG
	cerr << "Polynomial_Space contains a polynomial"
	     << " not in primitive form" << endl;
#endif
	goto bomb;
      }
    }

    // The top polynomial space is in canonical form.
    if (polynomials.empty() && !in_canonical_form) {
#ifndef NDEBUG
      cerr << "A top Polynomial_Space has to be in canonical form"
	   << endl;
#endif
      goto bomb;
    }

    if (in_canonical_form) {
      for (C_Iterator i = polynomials.begin(),
	     i_end = polynomials.end(); i != i_end; ++i) {
	const Term& leading_term = i->rbegin()->term();
	C_Iterator next_i = i;
	++next_i;
	if (next_i != i_end) {
	  const Term& next_leading_term = next_i->rbegin()->term();
	  if (Term::Compare()(leading_term, next_leading_term)) {
#ifndef NDEBUG
	    cerr << "Polynomial_Space in canonical form has "
		 << "an unordered list of polynomials"
		 << endl;
#endif
	    goto bomb;
	  }
	}
	for (C_Iterator j = polynomials.begin(); j != i_end; ++j)
	  if (i != j
	      && j->coefficient(leading_term) != Coefficient_zero()) {
#ifndef NDEBUG
	    cerr << "Polynomial_Space in canonical form is not "
		 << "in echelon form"
		 << endl;
#endif
	    goto bomb;
	  }
      }
    }

    // All checks passed.
    return true;

 bomb:
#ifndef NDEBUG
    cerr << "Here is the guilty Polynomial_Space:" << endl;
    
    using namespace IO_Operators;
    //cerr << *this << endl;
    ascii_dump(cerr);
#endif
    return false;
}

// Exception throwers.

template <degree_type db>
template <degree_type eb>
void
Polynomial_Space<db>
::throw_dimension_incompatible(const char* method,
			       const Polynomial_Space<eb>& y) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>
::throw_dimension_incompatible(const char* method,
			       dimension_type required_dim) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::throw_dimension_incompatible(const char* method,
						   const char* c_name,
						   const Constraint& c) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension() << ", "
    << c_name << "->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>
::throw_dimension_incompatible(const char* method,
			       const char* c_name,
			       const Polynomial_Constraint& c) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension() << ", "
    << c_name << "->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>
::throw_dimension_incompatible(const char* method,
			       const char* c_name,
			       const Constraint_System& cs) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension() << ", "
    << c_name << "->space_dimension == " << cs.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>
::throw_dimension_incompatible(const char* method,
			       const char* c_name,
			       const Polynomial_Constraint_System& cs) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension() << ", "
    << c_name << "->space_dimension == " << cs.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::throw_dimension_incompatible(const char* method,
						   const Generator& g) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", g->space_dimension == " << g.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::
throw_constraint_incompatible(const char* method) {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "the constraint is incompatible.";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::
throw_dimension_incompatible(const char* method,
			     const char* name_row,
			     const Linear_Expression& y) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::
throw_dimension_incompatible(const char* method,
			     const char* name_row,
			     const Polynomial& p) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n"
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << p.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::throw_generic(const char* method,
				    const char* reason) {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n" << reason << ".";
  throw std::invalid_argument(s.str());
}

template <degree_type db>
void
Polynomial_Space<db>::throw_space_dimension_overflow(const char* method,
						     const char* reason) {
  std::ostringstream s;
  s << "PPL::Polynomial_Space<" << db << ">::"
    << method << ":\n" << reason << ".";
  throw std::length_error(s.str());
}

// Auxiliary private methods of the class.

template <degree_type db>
void
Polynomial_Space<db>::canonicalize() const {

  typedef Polynomial::const_reverse_iterator Poly_const_rev_iterator;

  // If already in canonical form, there is nothing to do.
  if (in_canonical_form) {
    assert(OK());
    return;
  }

  Polynomial_Space& x = const_cast<Polynomial_Space&>(*this);
  std::list<Polynomial>& x_polynomials = x.polynomials;

  // Looking for a pivot for Gaussian elimination.
  for (Iterator x_i = x_polynomials.begin(), x_end = x_polynomials.end()
	 ; x_i != x_end; ) {

    if (x_i->is_equal_to(Polynomial::zero())) {
      // Found a zero polynomial: removing it.
      x_i   = x_polynomials.erase(x_i);
      x_end = x_polynomials.end();
    }
    else {
      // Computing greatest leading monomial among all remaining
      // polynomials to be processed.
      Iterator x_greatest = x_i;
      Poly_const_rev_iterator x_greatest_monomial = x_i->rbegin();

      Iterator x_j = x_i;
      ++x_j;
      while (x_j != x_end) {
	Poly_const_rev_iterator x_j_first = x_j->rbegin();
	Poly_const_rev_iterator x_j_last = x_j->rend();
	if (x_j_first != x_j_last) {
	  // Found a non-zero polynomial: compare leading term with
	  // current greatest term.
	  if (Term::Compare()
	      (x_greatest_monomial->term(), x_j_first->term())) {
	    x_greatest_monomial = x_j_first;
	    x_greatest = x_j;
	  }
	  ++x_j;
	}
	else {
	  // Found a zero polynomial: removing it.
	  x_j   = x_polynomials.erase(x_j);
	  x_end = x_polynomials.end();
	}
      }
      // Swapping polynomials if necessary.
      if (x_i != x_greatest)
	std::iter_swap(x_i, x_greatest);

      // Applying Gaussian elimination to rows below.
      const Term& x_i_term = x_i->rbegin()->term();
      const Coefficient& x_i_coeff = x_i->rbegin()->coefficient();
      x_j = x_i;
      ++x_j;
      while (x_j != x_end) {
	if (!x_j->is_equal_to(Polynomial::zero())) {
	  // Found a non-zero polynomial: pivoting.
	  const Term& x_j_term = x_j->rbegin()->term();
	  if (x_j_term == x_i_term) {
	    const Coefficient& x_j_coeff = x_j->rbegin()->coefficient();
	    x_j->linear_combine(*x_i, x_i_term, x_j_coeff, x_i_coeff);
	  }
	  ++x_j;
	}
	else {
	  // Found a zero polynomial: removing it.
	  x_j   = x_polynomials.erase(x_j);
	  x_end = x_polynomials.end();
	}
      }
      // Applying Gaussian elimination to rows above as well.
      x_j = x_polynomials.begin();
      while (x_j != x_i) {
	// Polynomials above cannot be zero.

	// TODO: could the search for this coefficient be made more
	// efficient by bearing in mind that we are looking up for
	// terms which are decreasing in the term ordering?
	const Coefficient& x_j_coeff = x_j->coefficient(x_i_term);
	if (x_j_coeff != Coefficient_zero())
	  x_j->linear_combine(*x_i, x_i_term, x_j_coeff, x_i_coeff);
	++x_j;
      }
      // Moving forward.
      ++x_i;
    }
  }
  x.in_canonical_form = true;

  assert(x.OK());
}

template <degree_type db>
void Polynomial_Space<db>::existentially_quantify(degree_type deg) {

  canonicalize();

  // The order of polynomials in polynomial spaces in canonical form
  // allows us to existentially quantify by removing the first
  // polynomials exceeding `deg'.
  for (Iterator p = polynomials.begin(), p_end = polynomials.end();
       p != p_end && p->degree() > deg; p = polynomials.erase(p))
    ;

  // If we obtained the top, we are in canonical form.
  in_canonical_form = polynomials.empty();

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::existentially_quantify(const Term& t) {
  Iterator p_i   = polynomials.begin();
  Iterator p_end = polynomials.end();
  // Find the pivot, if any exists.
  while (p_i != p_end && p_i->coefficient(t) == Coefficient_zero())
    ++p_i;

  if (p_i != p_end) {
    // Pivot found.
    const Polynomial& pivot = *p_i;
    const Coefficient& pivot_coeff = pivot.coefficient(t);
    // Eliminate the term from the other polynomials:
    // we only need to check the polynomials below the pivot.
    Iterator q_i = p_i;
    ++q_i;
    while(q_i != p_end) {
      Polynomial& q = *q_i;
      const Coefficient& q_coeff = q.coefficient(t);
      if (q_coeff != Coefficient_zero()) {
	q.linear_combine(pivot, t, pivot_coeff);
	if (q.is_equal_to(Polynomial::zero())) {
	  q_i   = polynomials.erase(q_i);
	  p_end = polynomials.end();
	}
	else
	  ++q_i;
      }
      else
	++q_i;
    }
    // Eliminate the pivoting polynomial.
    polynomials.erase(p_i);

    // If we obtained the top, we are in canonical form.
    in_canonical_form = polynomials.empty();
  }
  //  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::close_by_products() {

  canonicalize();

  // Existentially quantifying all those terms of degree > `db', just
  // in case any of the polynomials does not satisfy the degree bound
  // of the polynomial space.
  existentially_quantify(db);

  // If `db' == 0, the space is either {0} or the field IK:
  // nothing else to do.
  if (db == 0) {
    assert(OK());
    return;
  }
  // The product closure of a polynomial space S of degree `db' is
  // computed as the fixpoint of the sequence:
  // S[0]     := S
  // S[i + 1] := S[i] U { x * p | x in Vars(S), p in S[i], deg(p) < db}.
  Polynomial_Space<db> old;
  while(old != *this) {
    old = *this;

    Polynomial_Space<db> trunc(*this);
    trunc.existentially_quantify(db - 1);

    for (Iterator p = trunc.polynomials.begin(), p_end = trunc.polynomials.end();
	 p != p_end; ++p)
      for (dimension_type i = 0; i < dim; ++i)
	polynomials.push_back( Variable(i) * (*p));

    in_canonical_form = false;
    canonicalize();
  }
  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::existentially_quantify_all(const Variable v) {

  // If \p *this is zero-dimensional, nothing to do.
  if (dim == 0)
    return;

  // If \p *this is has degree bound `db' = 0, nothing to do.
  if (db == 0)
    return;

  // The set of all terms of degree at most `db' having a positive
  // exponent in variable `v' can be obtained by multiplying the terms
  // of degree `db' - 1 by `v'.
  for (Term iter, iter_end = end_graded_lex_term(db - 1);
       iter != iter_end; next_graded_lex_term(dim, iter))
    existentially_quantify(v * iter);

  assert(OK());
}

template <degree_type db>
void
Polynomial_Space<db>::
compute_degrees(const Variable var,
		exponent_type& max_deg,
		std::list<exponent_type>& degrees) const {

  // The list for the degrees must be initially empty.
  assert(degrees.empty());

  max_deg = 0;
  for (C_Iterator i = polynomials.begin(), i_end = polynomials.end(); i != i_end; ++i) {
    // `deg' == degree of current polynomial in variable `var'.
    // Scanning all terms and looking up the exponent of the variable.
    exponent_type deg = 0;
    for (Polynomial::const_iterator j = i->begin(), j_end = i->end();
	 j != j_end; ++j) {
      const exponent_type exp = j->term().exponent(var);
      if (exp > deg)
	deg = exp;
    }
    if (deg > max_deg)
      // Updating the maximum degree in `var' up to now.
      max_deg = deg;

    // Storing the degree in `var' of the current polynomial.
    degrees.push_back(deg);
  }
}

template <degree_type db>
void
Polynomial_Space<db>::
substitute_assign(const Variable var,
		  const Polynomial& num,
		  Coefficient_traits::const_reference den,
		  const exponent_type& max_deg,
		  std::list<exponent_type>& degrees) {

  // Computing vector of powers of `num' and `den', from
  // 0 to `max_deg', for the substitution.
  std::vector<Polynomial>  num_pow(max_deg + 1);
  std::vector<Coefficient> den_pow(max_deg + 1);

  // TO DO: the first positions of these vectors store '1', so they
  // could be avoided. Is it worth to shift the vectors one position
  // and keep adding/subtracting 1 to acces the elements in the
  // vectors?
  num_pow[0] = Polynomial(1);
  den_pow[0] = 1;
  for (exponent_type d = 1; d <= max_deg; ++d) {
    num_pow[d] = num * num_pow[d-1];
    den_pow[d] = den * den_pow[d-1];
  }
  // TO DO: is there any way of avoiding this computation every time
  // we call this method with the same affine map?

  // For each polynomial `p', substituting the occurrences of `var' by
  // (`num'/`den') in the polynomial `p' * pow(`den',
  // `p'.degree(`var')). This avoids the use of rational numbers.
  Iterator i = polynomials.begin(), i_end = polynomials.end();
  std::list<exponent_type>::iterator k = degrees.begin();
  while (i != i_end) {
    const exponent_type& deg = *k;
    // deg == degree(`var').
    if (deg == 0) {
      // If degree(`var') == 0, nothing to do.
      ++i;
      ++k;
    }
    else {
      Polynomial& p = *i;
      // `result' accumulates the result of the substitution.
      Polynomial result;
      for (Polynomial::const_iterator j = p.begin(),
	     j_end = p.end(); j != j_end; ++j) {

	// Substituting in each monomial `m' by decomposing `m' =
	// `m'.coefficient() * `new_t' * pow(`var', `var_exp').
	const Monomial& m = *j;
	const Term&     t = m.term();
	// TO DO: it would be quicker to have a method for terms
	// which, given a term `t' and a variable `v', returned
	// `t'/`t'.exponent(`v').
	Term new_t(t);
	exponent_type var_exp = t.exponent(var);
	new_t = div_pow_assign(new_t, var, var_exp);
	result += den_pow[deg - var_exp] * m.coefficient()
	  * new_t * num_pow[var_exp];
      }
      result.primitive_form_assign();
      std::swap(p, result);
      if (!p.is_equal_to(Polynomial::zero())) {
	++i;
	++k;
      }
      else {
	// The result of the substitution is 0: removing it.
	i     = polynomials.erase(i);
	i_end = polynomials.end();
	k     = degrees.erase(k);
      }
    }
  }
  in_canonical_form = polynomials.empty();
  close_by_products();
}

template <degree_type db>
void
Polynomial_Space<db>::
substitute_invertible_assign(const Variable var,
			     const Polynomial& num,
			     Coefficient_traits::const_reference den,
			     const exponent_type& max_deg,
			     const std::list<exponent_type>& degrees) {

  // Computing vector of powers of `num' and `den', from
  // 0 to `max_deg', for the substitution.
  std::vector<Polynomial>  num_pow(max_deg + 1);
  std::vector<Coefficient> den_pow(max_deg + 1);

  // TO DO: the first positions of these vectors store '1', so they
  // could be avoided. Is it worth to shift the vectors one position
  // and keep adding/subtracting 1 to acces the elements in the
  // vectors?
  num_pow[0] = Polynomial(1);
  den_pow[0] = 1;
  for (exponent_type d = 1; d <= max_deg; ++d) {
    num_pow[d] = num * num_pow[d-1];
    den_pow[d] = den * den_pow[d-1];
  }
  // TO DO: is there any way of avoiding this computation every time
  // we call this method with the same affine map?

  // For each polynomial `p', substituting the occurrences of `var' by
  // (`num'/`den') in the polynomial `p' * pow(`den',
  // `p'.degree(`var')). This avoids the use of rational numbers.
  Iterator i = polynomials.begin(), i_end = polynomials.end();
  std::list<exponent_type>::const_iterator k = degrees.begin();
  while (i != i_end) {
    const exponent_type& deg = *k;
    // If degree(`var') == 0, nothing to do.
    if (deg > 0) {
      Polynomial& p = *i;
      // `result' accumulates the result of the substitution.
      Polynomial result;
      for (Polynomial::const_iterator j = p.begin(),
	     j_end = p.end(); j != j_end; ++j) {

	// Substituting in each monomial `m' by decomposing `m' =
	// `m'.coefficient() * `new_t' * pow(`var', `var_exp').
	const Monomial& m = *j;
	const Term&     t = m.term();
	// TO DO: it would be quicker to have a method for terms
	// which, given a term `t' and a variable `v', returned
	// `t'/`t'.exponent(`v').
	Term new_t(t);
	exponent_type var_exp = t.exponent(var);
	new_t = div_pow_assign(new_t, var, var_exp);
	result += den_pow[deg - var_exp] * m.coefficient()
	  * new_t * num_pow[var_exp];
      }
      result.primitive_form_assign();
      std::swap(p, result);
    }
    // The result of the substitution cannot 0, as the
    // substitution is invertible.
    ++i;
    ++k;
  }
  in_canonical_form = polynomials.empty();
  // It is not necessary to close by products, as the substitution
  // is invertible.
}

template <degree_type db>
void
Polynomial_Space<db>::ascii_dump(std::ostream& s) const {
  s << "degree_bound " << db << "\n";
  s << "space_dim " << dim << "\n";
  s << "polynomials "
    << polynomials.size() << " " << "("
    << (in_canonical_form ? "" : "not_")
    << "in_canonical_form)\n";
  for (Polynomial_Sequence::const_iterator i = polynomials.begin(),
	 i_end = polynomials.end(); i != i_end; ++i) {
    i->ascii_dump(s);
    s << "\n";
  }
}

template <degree_type db>
void
Polynomial_Space<db>::ascii_dump() const {
  ascii_dump(std::cerr);
}

template <degree_type db>
void
Polynomial_Space<db>::print() const {
  using namespace IO_Operators;
  std::cerr << *this;
}

template <degree_type db>
bool
Polynomial_Space<db>::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "degree_bound")
    return false;

  degree_type degree_bound;
  if (!(s >> degree_bound) || degree_bound != db)
    return false;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> dim))
    return false;

  if (!(s >> str) || str != "polynomials")
    return false;

  dimension_type sz;
  if (!(s >> sz))
    return false;

  if (!(s >> str)
      || (str != "(in_canonical_form)" && str != "(not_in_canonical_form)"))
    return false;
  in_canonical_form = (str == "(in_canonical_form)");

  polynomials.clear();
  Polynomial p;
  for (dimension_type i = 0; i < sz; ++i) {
    if (!p.ascii_load(s))
      return false;
    polynomials.push_back(p);
  }

  assert(OK());
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Polynomial_Space_templates_hh)
