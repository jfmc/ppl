/* Direct_Product class implementation: inline functions.
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

#ifndef PPL_Direct_Product_inlines_hh
#define PPL_Direct_Product_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::max_space_dimension() {
  return std::min(D1::max_space_dimension(), D2::max_space_dimension());
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(dimension_type num_dimensions,
				       const Degenerate_Element kind)
  : d1(num_dimensions, kind), d2(num_dimensions, kind) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Congruence_System& ccgs)
  : d1(ccgs), d2(ccgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Congruence_System& cgs)
  : d1(const_cast<const Congruence_System&>(cgs)), d2(cgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Constraint_System& ccs)
  : d1(ccs), d2(ccs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Constraint_System& cs)
  : d1(const_cast<const Constraint_System&>(cs)), d2(cs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Grid_Generator_System& gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Grid_Generator_System& gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Generator_System& gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Generator_System& gs) {
}

template <typename D1, typename D2>
template <typename Box>
inline
Direct_Product<D1, D2>::Direct_Product(const Box& box,
				       From_Bounding_Box dummy)
  : d1(box, dummy), d2(box, dummy) {
}

template <typename D1, typename D2>
template <typename Box>
inline
Direct_Product<D1, D2>::Direct_Product(const Box& box,
				       From_Covering_Box dummy) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Direct_Product& y)
  : d1(y.d1), d2(y.d2) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::~Direct_Product() {
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::external_memory_in_bytes() const {
  return d1.external_memory_in_bytes() + d2.external_memory_in_bytes();
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::space_dimension() const {
  assert(d1.space_dimension() == d2.space_dimension());
  return d1.space_dimension();
}

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::affine_dimension() const {
  const dimension_type d1_dim = d1.affine_dimension();
  const dimension_type d2_dim = d2.affine_dimension();
  return std::min(d1_dim, d2_dim);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::intersection_assign(const Direct_Product& y) {
  d1.intersection_assign(y.d1);
  d2.intersection_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::difference_assign(const Direct_Product& y) {
  d1.difference_assign(y.d1);
  d2.difference_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::upper_bound_assign(const Direct_Product& y) {
  d1.upper_bound_assign(y.d1);
  d2.upper_bound_assign(y.d2);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::upper_bound_assign_if_exact(const Direct_Product& y) {
  return d1.upper_bound_assign_if_exact(y.d1)
    || d2.upper_bound_assign_if_exact(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::affine_image(Variable var,
	       const Linear_Expression& expr,
	       Coefficient_traits::const_reference denominator) {
  d1.affine_image(var, expr, denominator);
  d2.affine_image(var, expr, denominator);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::affine_preimage(Variable var,
		  const Linear_Expression& expr,
		  Coefficient_traits::const_reference denominator) {
  d1.affine_preimage(var, expr, denominator);
  d2.affine_preimage(var, expr, denominator);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_image(Variable var,
			   const Relation_Symbol relsym,
			   const Linear_Expression& expr,
			   Coefficient_traits::const_reference denominator) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_image(Variable var,
			   const Linear_Expression& expr,
			   Coefficient_traits::const_reference denominator,
			   Coefficient_traits::const_reference modulus) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(Variable var,
			      const Relation_Symbol relsym,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(Variable var,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator,
			      Coefficient_traits::const_reference modulus) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_image(const Linear_Expression& lhs,
			   const Relation_Symbol relsym,
			   const Linear_Expression& rhs) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_image(const Linear_Expression& lhs,
			   const Linear_Expression& rhs,
			   Coefficient_traits::const_reference modulus) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(const Linear_Expression& lhs,
			      const Relation_Symbol relsym,
			      const Linear_Expression& rhs) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(const Linear_Expression& lhs,
			      const Linear_Expression& rhs,
			      Coefficient_traits::const_reference modulus) {
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::time_elapse_assign(const Direct_Product& y) {
  d1.time_elapse_assign(y.d1);
  d2.time_elapse_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::topological_closure_assign() {
  d1.topological_closure_assign();
  d2.topological_closure_assign();
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::swap(Direct_Product& y) {
  std::swap(d1, y.d1);
  std::swap(d2, y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_constraint(const Constraint& c) {
  d1.add_constraint(c);
  d2.add_constraint(c);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_congruence(const Congruence& cg) {
  d1.add_congruence(cg);
  d2.add_congruence(cg);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_grid_generator(const Grid_Generator& g) {
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::add_grid_generator_and_minimize(const Grid_Generator& g) {
  return false;
}

template <typename D1, typename D2>
inline Direct_Product<D1, D2>&
Direct_Product<D1, D2>::operator=(const Direct_Product& y) {
  d1 = y.d1;
  d2 = y.d2;
  return *this;
}

template <typename D1, typename D2>
inline const D1&
Direct_Product<D1, D2>::domain1() const {
  return d1;
}

template <typename D1, typename D2>
inline const D2&
Direct_Product<D1, D2>::domain2() const {
  return d2;
}

template <typename D1, typename D2>
inline const Congruence_System&
Direct_Product<D1, D2>::congruences() const {
  // FIX return ref to universe of correct dim?
  return Congruence_System::zero_dim_empty();
}

template <typename D1, typename D2>
inline const Congruence_System&
Direct_Product<D1, D2>::minimized_congruences() const {
  // FIX return ref to universe of correct dim?
  return Congruence_System::zero_dim_empty();
}

template <typename D1, typename D2>
inline const Constraint_System&
Direct_Product<D1, D2>::constraints() const {
  // FIX return ref to universe of correct dim?
  return Constraint_System::zero_dim_empty();
}

template <typename D1, typename D2>
inline const Constraint_System&
Direct_Product<D1, D2>::minimized_constraints() const {
  // FIX return ref to universe of correct dim?
  return Constraint_System::zero_dim_empty();
}

template <typename D1, typename D2>
inline const Generator_System&
Direct_Product<D1, D2>::generators() const {
  // FIX return ref to correct dim?
  return Generator_System::zero_dim_univ();
}

template <typename D1, typename D2>
inline const Generator_System&
Direct_Product<D1, D2>::minimized_generators() const {
  // FIX return ref to correct dim?
  return Generator_System::zero_dim_univ();
}

template <typename D1, typename D2>
inline const Grid_Generator_System&
Direct_Product<D1, D2>::grid_generators() const {
  // FIX return ref to correct dim?
  return Grid_Generator_System::zero_dim_univ();
}

template <typename D1, typename D2>
inline const Grid_Generator_System&
Direct_Product<D1, D2>::minimized_grid_generators() const {
  // FIX return ref to correct dim?
  return Grid_Generator_System::zero_dim_univ();
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_empty() const {
  return d1.is_empty() || d2.is_empty();
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_universe() const {
  return d1.is_universe() && d2.is_universe();
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_topologically_closed() const {
  return d1.is_topologically_closed() && d2.is_topologically_closed();
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_disjoint_from(const Direct_Product& y) const {
  return d1.is_disjoint_from(y.d1) && d2.is_disjoint_from(y.d2);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_discrete() const {
  // Assume wholeness.  Must be specialized for Grid.
  return false;
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_bounded() const {
  return d1.is_bounded() && d2.is_bounded();
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_space_dimensions_and_embed(dimension_type m) {
  d1.add_space_dimensions_and_embed(m);
  d2.add_space_dimensions_and_embed(m);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_space_dimensions_and_project(dimension_type m) {
  d1.add_space_dimensions_and_project(m);
  d2.add_space_dimensions_and_project(m);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::concatenate_assign(const Direct_Product& y) {
  d1.concatenate_assign(y.d1);
  d2.concatenate_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  d1.remove_space_dimensions(to_be_removed);
  d2.remove_space_dimensions(to_be_removed);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::remove_higher_space_dimensions(dimension_type new_dimension) {
  d1.remove_higher_space_dimensions(new_dimension);
  d2.remove_higher_space_dimensions(new_dimension);
}

template <typename D1, typename D2>
template <typename Partial_Function>
inline void
Direct_Product<D1, D2>::map_space_dimensions(const Partial_Function& pfunc) {
  d1.map_space_dimensions(pfunc);
  d2.map_space_dimensions(pfunc);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::expand_space_dimension(Variable var, dimension_type m) {
  d1.expand_space_dimension(var, m);
  d2.expand_space_dimension(var, m);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::fold_space_dimensions(const Variables_Set& to_be_folded,
					      Variable var) {
  d1.fold_space_dimensions(to_be_folded, var);
  d2.fold_space_dimensions(to_be_folded, var);
}

PPL_OUTPUT_2_PARAM_TEMPLATE_DEFINITIONS(D1, D2, Direct_Product)

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::ascii_dump(std::ostream& s) const {
  s << "Domain 1:\n";
  d1.ascii_dump(s);
  s << "Domain 2:\n";
  d2.ascii_dump(s);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::OK() const {
  return d1.OK() && d2.OK();
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline bool
operator==(const Direct_Product<D1, D2>& x,
	   const Direct_Product<D1, D2>& y) {
  return x.d1 == y.d1 && x.d2 == y.d2;
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline bool
operator!=(const Direct_Product<D1, D2>& x,
	   const Direct_Product<D1, D2>& y) {
  return !(x == y);
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline std::ostream&
IO_Operators::operator<<(std::ostream& s, const Direct_Product<D1, D2>& dp) {
  return s << "Domain 1:\n"
	   << dp.d1
	   << "Domain 2:\n"
	   << dp.d2;
}

// FIXME: move to dedicated file once name decided

template <typename D1, typename D2>
inline
Open_Product<D1, D2>::Open_Product(dimension_type num_dimensions,
				   const Degenerate_Element kind)
  : Direct_Product<D1, D2>(num_dimensions, kind) {
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::is_empty() const {
  // FIX intersection.is_empty()
  const Open_Product& op = *this;
  return op.d1.is_empty() || op.d2.is_empty();
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::is_topologically_closed() const {
  // FIX intersection.is_topologically_closed()
  const Open_Product& op = *this;
  return op.d1.is_topologically_closed() && op.d2.is_topologically_closed();
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::is_disjoint_from(const Open_Product& y) const {
  // FIX intersection.is_disjoint_from(y.intersection)
  const Open_Product& op = *this;
  return op.d1.is_disjoint_from(y.d1) && op.d2.is_disjoint_from(y.d2);
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::is_bounded() const {
  // FIX intersection.is_bounded()
  const Open_Product& op = *this;
  return op.d1.is_bounded() && op.d2.is_bounded();
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::is_discrete() const {
  // FIX intersection.is_discrete()
  const Open_Product& op = *this;
  return op.d1.is_discrete() && op.d2.is_discrete();
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::reduce_domain1_with_domain2() {
  return false;
}

template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::reduce_domain2_with_domain1() {
  return false;
}

} // namespace Parma_Polyhedra_Library

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline void
std::swap(Parma_Polyhedra_Library::Direct_Product<D1, D2>& x,
	  Parma_Polyhedra_Library::Direct_Product<D1, D2>& y) {
  x.swap(y);
}

#endif // !defined(PPL_Direct_Product_inlines_hh)
