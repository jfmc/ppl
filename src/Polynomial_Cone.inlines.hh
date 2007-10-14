/* Polynomial_Cone class implementation: inline functions.
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

#ifndef PPL_Polynomial_Cone_inlines_hh
#define PPL_Polynomial_Cone_inlines_hh 1

#include "C_Polyhedron.defs.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "Polynomial_Constraint_System.defs.hh"
#include <cassert>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename PH, unsigned db>
inline dimension_type
Polynomial_Cone<PH, db>::max_space_dimension() {
  // FIXME!!!
  abort();
  return C_Polyhedron::max_space_dimension();
}

template <typename PH, unsigned db>
inline
Polynomial_Cone<PH, db>::Polynomial_Cone(const dimension_type num_dimensions,
					 const Degenerate_Element kind)
  : ph() {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
inline
Polynomial_Cone<PH, db>::Polynomial_Cone(const Polynomial_Cone& y)
  : ph() {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
template <typename QH, unsigned eb>
inline
Polynomial_Cone<PH, db>::Polynomial_Cone(const Polynomial_Cone<QH, eb>& y)
  : ph() {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::add_constraints(const Constraint_System& cs) {
  for (Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i)
    add_constraint(*i);
  assert(OK());
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>
::add_polynomial_constraints(const Polynomial_Constraint_System& cs) {
  for (Polynomial_Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i)
    add_polynomial_constraint(*i);
  assert(OK());
}

template <typename PH, unsigned db>
inline
Polynomial_Cone<PH, db>::Polynomial_Cone(const Constraint_System& cs)
  : ph() {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
inline dimension_type
Polynomial_Cone<PH, db>::affine_dimension() const {
  abort();
  return 0;
}

template <typename PH, unsigned db>
inline Polynomial_Cone<PH, db>&
Polynomial_Cone<PH, db>::operator=(const Polynomial_Cone& y) {
  ph = y.ph;
  abort();
  return *this;
}

template <typename PH, unsigned db>
inline
Polynomial_Cone<PH, db>::~Polynomial_Cone() {
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::swap(Polynomial_Cone& y) {
  std::swap(ph, y.ph);
  abort();
}

template <typename PH, unsigned db>
inline dimension_type
Polynomial_Cone<PH, db>::space_dimension() const {
  abort();
  return 0;
}

template <typename PH, unsigned db>
inline bool
Polynomial_Cone<PH, db>::is_empty() const {
  abort();
  return false;
}

template <typename PH, unsigned db>
inline bool
operator==(const Polynomial_Cone<PH, db>& x,
	   const Polynomial_Cone<PH, db>& y) {
  abort();
  return false;
}

template <typename PH, unsigned db>
inline bool
operator!=(const Polynomial_Cone<PH, db>& x, const Polynomial_Cone<PH, db>& y) {
  return !(x == y);
}

template <typename PH, unsigned db>
inline bool
Polynomial_Cone<PH, db>::strictly_contains(const Polynomial_Cone& y) const {
  abort();
  return false;
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::upper_bound_assign(const Polynomial_Cone& y) {
  abort();
}

template <typename PH, unsigned db>
inline bool
Polynomial_Cone<PH, db>::polynomial_cone_hull_assign_if_exact(const Polynomial_Cone&) {
  abort();
  return false;
}

template <typename PH, unsigned db>
inline bool
Polynomial_Cone<PH, db>::upper_bound_assign_if_exact(const Polynomial_Cone& y) {
  return polynomial_cone_hull_assign_if_exact(y);
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::difference_assign(const Polynomial_Cone& y) {
  polynomial_cone_difference_assign(y);
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::remove_higher_space_dimensions(const dimension_type new_dim) {
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

  abort();
  assert(OK());
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::time_elapse_assign(const Polynomial_Cone& y) {
  // Dimension-compatibility check.
  if (space_dimension() != y.space_dimension())
    throw_dimension_incompatible("time_elapse_assign(y)", y);
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
inline void
Polynomial_Cone<PH, db>::ascii_dump(std::ostream& s) const {
  abort();
}

template <typename PH, unsigned db>
inline bool
Polynomial_Cone<PH, db>::ascii_load(std::istream& s) {
  abort();
  return false;
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Polynomial_Cone */
template <typename PH, unsigned db>
inline void
swap(Parma_Polyhedra_Library::Polynomial_Cone<PH, db>& x,
     Parma_Polyhedra_Library::Polynomial_Cone<PH, db>& y) {
  x.swap(y);
}

} // namespace std

// From here onwards, there should be no inline methods/functions,
// but only non-inline member/function templates.

namespace Parma_Polyhedra_Library {

template <typename PH, unsigned db>
Polynomial_Cone<PH, db>::Polynomial_Cone(const Generator_System& gs)
  : ph() {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::add_constraint(const Constraint& c) {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::concatenate_assign(const Polynomial_Cone& y) {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
bool
Polynomial_Cone<PH, db>::contains(const Polynomial_Cone& y) const {
  abort();
  return false;
}

template <typename PH, unsigned db>
bool
Polynomial_Cone<PH, db>::is_universe() const {
  abort();
  return false;
}

#if 0
template <typename PH, unsigned db>
Poly_Con_Relation
Polynomial_Cone<PH, db>::relation_with(const Constraint& c) const {
  abort();
}

template <typename PH, unsigned db>
Poly_Gen_Relation
Polynomial_Cone<PH, db>::relation_with(const Generator& g) const {
  abort();
}
#endif

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::polynomial_cone_hull_assign(const Polynomial_Cone& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("polynomial_cone_hull_assign(y)", y);

  abort();
  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::polynomial_cone_difference_assign(const Polynomial_Cone& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("bds_difference_assign(y)", y);

  abort();
  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::add_space_dimensions_and_embed(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  const dimension_type space_dim = space_dimension();
  const dimension_type new_space_dim = space_dim + m;

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::add_space_dimensions_and_project(const dimension_type m) {
  // Adding no dimensions is a no-op.
  if (m == 0)
    return;

  const dimension_type space_dim = space_dimension();

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any PC is a no-op.
  // Note that this case also captures the only legal removal of
  // space dimensions from a PC in a 0-dim space.
  if (to_be_removed.empty()) {
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

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
template <typename PartialFunction>
void
Polynomial_Cone<PH, db>::map_space_dimensions(const PartialFunction& pfunc) {
  const dimension_type space_dim = space_dimension();
  // TODO: this implementation is just an executable specification.
  if (space_dim == 0)
    return;

  if (pfunc.has_empty_codomain()) {
    // All dimensions vanish: the BDS becomes zero_dimensional.
    remove_higher_space_dimensions(0);
    assert(OK());
    return;
  }

  const dimension_type new_space_dim = pfunc.max_in_codomain() + 1;

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::intersection_assign(const Polynomial_Cone& y) {
  const dimension_type space_dim = space_dimension();

  // Dimension-compatibility check.
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("intersection_assign(y)", y);

#if 0
  // If one of the two systems of bounded differences is empty,
  // the intersection is empty.
  if (marked_empty())
    return;
  if (y.marked_empty()) {
    set_empty();
    return;
  }
#endif

  // If both systems of bounded differences are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (space_dim == 0)
    return;

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::widening_assign(const Polynomial_Cone& y,
					 unsigned* tp) {
  abort();
  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::limited_extrapolation_assign(const Polynomial_Cone& y,
						      const Polynomial_Constraint_System& pcs,
						      unsigned* /*tp*/) {
  // Dimension-compatibility check.
  const dimension_type space_dim = space_dimension();
  if (space_dim != y.space_dimension())
    throw_dimension_incompatible("limited_extrapolation_assign(y, cs)",
				 y);

  // `cs' must be dimension-compatible with the two systems
  // of bounded differences.
  const dimension_type pcs_space_dim = pcs.space_dimension();
  if (space_dim < pcs_space_dim)
    throw_constraint_incompatible("limited_extrapolation_assign(y, cs)");

  // Strict inequalities not allowed.
  if (pcs.has_strict_inequalities())
    throw_constraint_incompatible("limited_extrapolation_assign(y, cs)");

  // The limited extrapolation between two polynomial cones
  // in a zero-dimensional space is a polynomial cone
  // in a zero-dimensional space, too.
  if (space_dim == 0)
    return;

#ifndef NDEBUG
  {
    // We assume that `y' is contained in or equal to `*this'.
    const Polynomial_Cone x_copy = *this;
    const Polynomial_Cone y_copy = y;
    assert(x_copy.contains(y_copy));
  }
#endif

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::affine_image(const Variable var,
			  const Linear_Expression& expr,
			  Coefficient_traits::const_reference denominator) {
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

  // `var' should be one of the dimensions of the shape.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("affine_image(v, e, d)", var.id());

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::affine_preimage(const Variable var,
			     const Linear_Expression& expr,
			     Coefficient_traits::const_reference denominator) {
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

  // `var' should be one of the dimensions of
  // the systems of bounded differences.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("affine_preimage(v, e, d)", var.id());

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::generalized_affine_image(const Variable var,
				      const Relation_Symbol relsym,
				      const Linear_Expression& expr,
				      Coefficient_traits::const_reference
				      denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_image(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 "e", expr);

  // `var' should be one of the dimensions of the PC.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("generalized_affine_image(v, r, e, d)",
				 var.id());

  // The relation symbol cannot be a strict relation symbol.
  if (relsym == LESS_THAN || relsym == GREATER_THAN)
    throw_generic("generalized_affine_image(v, r, e, d)",
  		  "r is a strict relation symbol and "
  		  "*this is a Polynomial_Cone");

  if (relsym == EQUAL) {
    // The relation symbol is "==":
    // this is just an affine image computation.
    affine_image(var, expr, denominator);
    assert(OK());
    return;
  }

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::generalized_affine_image(const Linear_Expression& lhs,
						  const Relation_Symbol relsym,
						  const Linear_Expression& rhs) {
  // Dimension-compatibility checks.
  // The dimension of `lhs' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type lhs_space_dim = lhs.space_dimension();
  if (space_dim < lhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e1", lhs);

  // The dimension of `rhs' should not be greater than the dimension
  // of `*this'.
  const dimension_type rhs_space_dim = rhs.space_dimension();
  if (space_dim < rhs_space_dim)
    throw_dimension_incompatible("generalized_affine_image(e1, r, e2)",
				 "e2", rhs);

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::generalized_affine_preimage(const Variable var,
						     const Relation_Symbol relsym,
						     const Linear_Expression& expr,
						     Coefficient_traits::const_reference
						     denominator) {
  // The denominator cannot be zero.
  if (denominator == 0)
    throw_generic("generalized_affine_preimage(v, r, e, d)", "d == 0");

  // Dimension-compatibility checks.
  // The dimension of `expr' should not be greater than the dimension
  // of `*this'.
  const dimension_type space_dim = space_dimension();
  const dimension_type expr_space_dim = expr.space_dimension();
  if (space_dim < expr_space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 "e", expr);

  // `var' should be one of the dimensions of the BDS.
  const dimension_type v = var.id() + 1;
  if (v > space_dim)
    throw_dimension_incompatible("generalized_affine_preimage(v, r, e, d)",
				 var.id());

  abort();

  assert(OK());
}

template <typename PH, unsigned db>
Polynomial_Constraint_System
Polynomial_Cone<PH, db>::polynomial_constraints() const {
  Polynomial_Constraint_System pcs;
  const dimension_type space_dim = space_dimension();

  abort();

  return pcs;
}

/*! \relates Parma_Polyhedra_Library::Polynomial_Cone */
template <typename PH, unsigned db>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Polynomial_Cone<PH, db>& c) {
  typedef typename Polynomial_Cone<PH, db>::coefficient_type N;
  if (c.is_universe())
    s << "true";
  else {
    abort();
  }
  return s;
}

template <typename PH, unsigned db>
bool
Polynomial_Cone<PH, db>::OK() const {
  // Check whether the representation polyhedron is well-formed.
  if (!ph.OK())
    return false;

#if 0
  // Check whether the status information is legal.
  if (!status.OK())
    return false;
#endif

  abort();

  // All checks passed.
  return true;
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_dimension_incompatible(const char* method,
					  const Polynomial_Cone& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", y->space_dimension() == " << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_dimension_incompatible(const char* method,
					  dimension_type required_dim) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", required dimension == " << required_dim << ".";
  throw std::invalid_argument(s.str());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_dimension_incompatible(const char* method,
					  const Constraint& c) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", c->space_dimension == " << c.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_dimension_incompatible(const char* method,
					  const Generator& g) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", g->space_dimension == " << g.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_constraint_incompatible(const char* method) {
  std::ostringstream s;
  s << "PPL::Polynomial_Cone::" << method << ":" << std::endl
    << "the constraint is incompatible.";
  throw std::invalid_argument(s.str());
}

template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_dimension_incompatible(const char* method,
					  const char* name_row,
					  const Linear_Expression& y) const {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension()
    << ", " << name_row << "->space_dimension() == "
    << y.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}


template <typename PH, unsigned db>
void
Polynomial_Cone<PH, db>::throw_generic(const char* method, const char* reason) {
  std::ostringstream s;
  s << "PPL::";
  s << "Polynomial_Cone::" << method << ":" << std::endl
    << reason;
  throw std::invalid_argument(s.str());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Polynomial_Cone_inlines_hh)
