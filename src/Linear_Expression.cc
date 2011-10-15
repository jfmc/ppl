/* Linear_Expression class implementation (non-inline functions).
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

#include "Linear_Expression.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

int
PPL::compare(const Linear_Expression& x, const Linear_Expression& y) {
  return x.impl->compare(*y.impl);
}

PPL::Linear_Expression::Linear_Expression(const Constraint& c)
  : impl(new Linear_Expression_Impl(c)) {
}

PPL::Linear_Expression::Linear_Expression(const Generator& g)
  : impl(new Linear_Expression_Impl(g)) {
}

PPL::Linear_Expression::Linear_Expression(const Grid_Generator& g)
  : impl(new Linear_Expression_Impl(g)) {
}

const PPL::Linear_Expression* PPL::Linear_Expression::zero_p = 0;

void
PPL::Linear_Expression::initialize() {
  PPL_ASSERT(zero_p == 0);
  zero_p = new Linear_Expression(Coefficient_zero());
}

void
PPL::Linear_Expression::finalize() {
  PPL_ASSERT(zero_p != 0);
  delete zero_p;
  zero_p = 0;
}

PPL::Linear_Expression::Linear_Expression(const Congruence& cg)
  : impl(new Linear_Expression_Impl(cg)) {
}

PPL::Linear_Expression::Linear_Expression(const Variable v)
  : impl(new Linear_Expression_Impl(v)) {
}

bool
PPL::Linear_Expression::is_equal_to(const Linear_Expression& x) const {
  return impl->is_equal_to(*x.impl);
}

void
PPL::Linear_Expression::remove_space_dimensions(const Variables_Set& vars) {
  impl->remove_space_dimensions(vars);
}

void
PPL::Linear_Expression::permute_space_dimensions(const std::vector<Variable>& cycle) {
  impl->permute_space_dimensions(cycle);
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator+(const Linear_Expression& e1, const Linear_Expression& e2) {
  if (e1.space_dimension() >= e2.space_dimension()) {
    Linear_Expression e = e1;
    e += e2;
    return e;
  } else {
    Linear_Expression e = e2;
    e += e1;
    return e;
  }
}

/*! \relates Linear_Expression */
PPL::Linear_Expression
PPL::operator+(const Variable v, const Linear_Expression& e) {
  return e + v;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator+(Coefficient_traits::const_reference n,
	       const Linear_Expression& e) {
  return e + n;
}

/*! \relates Linear_Expression */
PPL::Linear_Expression
PPL::operator+(const Variable v, const Variable w) {
  const dimension_type v_space_dim = v.space_dimension();
  const dimension_type w_space_dim = w.space_dimension();
  const dimension_type space_dim = std::max(v_space_dim, w_space_dim);
  if (space_dim > Linear_Expression::max_space_dimension())
    throw std::length_error("Linear_Expression "
                            "PPL::operator+(v, w):\n"
                            "v or w exceed the maximum allowed "
                            "space dimension.");
  if (v_space_dim >= w_space_dim) {
    Linear_Expression e(v);
    e += w;
    return e;
  } else {
    Linear_Expression e(w);
    e += v;
    return e;
  }
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator-(const Linear_Expression& e) {
  Linear_Expression r(e);
  neg_assign(r);
  return r;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator-(const Linear_Expression& e1, const Linear_Expression& e2) {
  if (e1.space_dimension() >= e2.space_dimension()) {
    Linear_Expression e = e1;
    e -= e2;
    return e;
  } else {
    Linear_Expression e = e2;
    neg_assign(e);
    e += e1;
    return e;
  }
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator-(const Variable v, const Linear_Expression& e) {
  Linear_Expression result(e, std::max(v.space_dimension(), e.space_dimension()) + 1);
  result.negate(0, e.space_dimension() + 1);
  result += v;
  return result;
}

/*! \relates Linear_Expression */
PPL::Linear_Expression
PPL::operator-(const Linear_Expression& e, const Variable v) {
  Linear_Expression result(e, std::max(v.space_dimension(), e.space_dimension()) + 1);
  result -= v;
  return result;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator-(Coefficient_traits::const_reference n,
	       const Linear_Expression& e) {
  Linear_Expression result(e);
  neg_assign(result);
  result += n;
  return result;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression
PPL::operator*(Coefficient_traits::const_reference n,
	       const Linear_Expression& e) {
  return e * n;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::operator+=(Linear_Expression& e1, const Linear_Expression& e2) {
  *e1.impl += *e2.impl;
  return e1;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::operator+=(Linear_Expression& e, const Variable v) {
  *e.impl += v;
  return e;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::operator-=(Linear_Expression& e1, const Linear_Expression& e2) {
  *e1.impl -= *e2.impl;
  return e1;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::operator-=(Linear_Expression& e, const Variable v) {
  *e.impl -= v;
  return e;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::operator*=(Linear_Expression& e, Coefficient_traits::const_reference n) {
  *e.impl *= n;
  return e;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
void
PPL::neg_assign(Linear_Expression& e) {
  e.impl->negate();
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::add_mul_assign(Linear_Expression& e,
                    Coefficient_traits::const_reference n,
                    const Variable v) {
  e.impl->add_mul_assign(n, v);
  return e;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
PPL::Linear_Expression&
PPL::sub_mul_assign(Linear_Expression& e,
                    Coefficient_traits::const_reference n,
                    const Variable v) {
  e.impl->sub_mul_assign(n, v);
  return e;
}

PPL::Linear_Expression&
PPL::sub_mul_assign(Linear_Expression& x, Coefficient_traits::const_reference n,
                    const Linear_Expression& y, dimension_type start, dimension_type end) {
  x.impl->sub_mul_assign(n, *y.impl, start, end);
  return x;
}

void
PPL::add_mul_assign(Linear_Expression& e1,
                    Coefficient_traits::const_reference factor,
                    const Linear_Expression& e2) {
  e1.impl->add_mul_assign(factor, *e2.impl);
}

void
PPL::sub_mul_assign(Linear_Expression& e1,
                    Coefficient_traits::const_reference factor,
                    const Linear_Expression& e2) {
  e1.impl->sub_mul_assign(factor, *e2.impl);
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Linear_Expression& e) {
  *e.impl << s;
  return s;
}

PPL::Coefficient&
PPL::Linear_Expression::operator[](dimension_type i) {
  return (*impl)[i];
}

const PPL::Coefficient&
PPL::Linear_Expression::operator[](dimension_type i) const {
  return (*impl)[i];
}

const PPL::Coefficient&
PPL::Linear_Expression::get(dimension_type i) const {
  return impl->get(i);
}

bool
PPL::Linear_Expression::all_zeroes(dimension_type start, dimension_type end) const {
  return impl->all_zeroes(start, end);
}

PPL::Coefficient
PPL::Linear_Expression::gcd(dimension_type start, dimension_type end) const {
  return impl->gcd(start, end);
}

void
PPL::Linear_Expression
::exact_div_assign(Coefficient_traits::const_reference c,
                   dimension_type start, dimension_type end) {
  impl->exact_div_assign(c, start, end);
}

void
PPL::Linear_Expression::linear_combine(const Linear_Expression& y,
                                       Coefficient_traits::const_reference c1,
                                       Coefficient_traits::const_reference c2,
                                       dimension_type start,
                                       dimension_type end) {
  impl->linear_combine(*y.impl, c1, c2, start, end);
}

void
PPL::Linear_Expression::sign_normalize() {
  impl->sign_normalize();
}

void
PPL::Linear_Expression::negate(dimension_type first, dimension_type last) {
  impl->negate(first, last);
}

bool
PPL::Linear_Expression::all_zeroes(const Variables_Set& vars) const {
  return impl->all_zeroes(vars);
}

void
PPL::Linear_Expression
::modify_according_to_evolution(const Linear_Expression& x,
                                const Linear_Expression& y) {
  impl->modify_according_to_evolution(*x.impl, *y.impl);
}

PPL::dimension_type
PPL::Linear_Expression::last_nonzero() const {
  return impl->last_nonzero();
}

PPL_OUTPUT_DEFINITIONS(Linear_Expression)
