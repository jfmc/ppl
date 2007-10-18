/* Direct_Product class implementation: inline functions.
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

#ifndef PPL_Direct_Product_inlines_hh
#define PPL_Direct_Product_inlines_hh 1

#include "compiler.hh"

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
Direct_Product<D1, D2>::Direct_Product(const Grid_Generator_System& gs)
  : d1(gs), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Grid_Generator_System& gs)
  : d1(gs), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Generator_System& gs)
  : d1(gs), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Generator_System& gs) {
}

template <typename D1, typename D2>
template <typename Interval>
inline
Direct_Product<D1, D2>::Direct_Product(const Box<Interval>& box)
  : d1(box), d2(box) {
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
  d1.generalized_affine_image(var, relsym, expr, denominator);
  d2.generalized_affine_image(var, relsym, expr, denominator);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(Variable var,
			      const Relation_Symbol relsym,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator) {
  d1.generalized_affine_preimage(var, relsym, expr, denominator);
  d2.generalized_affine_preimage(var, relsym, expr, denominator);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_image(const Linear_Expression& lhs,
			   const Relation_Symbol relsym,
			   const Linear_Expression& rhs) {
  d1.generalized_affine_image(lhs, relsym, rhs);
  d2.generalized_affine_image(lhs, relsym, rhs);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::generalized_affine_preimage(const Linear_Expression& lhs,
			      const Relation_Symbol relsym,
			      const Linear_Expression& rhs) {
  d1.generalized_affine_preimage(lhs, relsym, rhs);
  d2.generalized_affine_preimage(lhs, relsym, rhs);
}


template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::bounded_affine_image(Variable var,
			       const Linear_Expression& lb_expr,
			       const Linear_Expression& ub_expr,
			       Coefficient_traits::const_reference denominator) {
  d1.bounded_affine_image(var, lb_expr, ub_expr, denominator);
  d2.bounded_affine_image(var, lb_expr, ub_expr, denominator);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::bounded_affine_preimage(Variable var,
			       const Linear_Expression& lb_expr,
			       const Linear_Expression& ub_expr,
			       Coefficient_traits::const_reference denominator) {
  d1.bounded_affine_preimage(var, lb_expr, ub_expr, denominator);
  d2.bounded_affine_preimage(var, lb_expr, ub_expr, denominator);
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
Direct_Product<D1, D2>::add_generator(const Generator& g) {
  d1.add_generator(g);
  d2.add_generator(g);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::add_generator_and_minimize(const Generator& g) {
  bool empty = d1.add_generator_and_minimize(g);
  return d2.add_generator_and_minimize(g) || empty;
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_grid_generator(const Grid_Generator& g) {
  d1.add_grid_generator(g);
  d2.add_grid_generator(g);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>
::add_grid_generator_and_minimize(const Grid_Generator& g) {
  bool empty = d1.add_grid_generator_and_minimize(g);
  return d2.add_grid_generator_and_minimize(g) || empty;
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_constraints(const Constraint_System& cs) {
  d1.add_constraints(cs);
  d2.add_constraints(cs);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_recycled_constraints(Constraint_System& cs) {
  d1.add_recycled_constraints(cs);
  d2.add_recycled_constraints(cs);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>
::add_constraints_and_minimize(const Constraint_System& cs) {
  bool empty = d1.add_constraints_and_minimize(cs);
  return d2.add_constraints_and_minimize(cs) || empty;
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>
::add_recycled_constraints_and_minimize(Constraint_System& cs) {
  bool empty = d1.add_recycled_constraints_and_minimize(cs);
  return d2.add_recycled_constraints_and_minimize(cs) || empty;
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_congruences(const Congruence_System& cgs) {
  d1.add_congruences(cgs);
  d2.add_congruences(cgs);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_generators(const Generator_System& gs) {
  d1.add_generators(gs);
  d2.add_generators(gs);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::add_grid_generators(const Grid_Generator_System& gs) {
  d1.add_grid_generators(gs);
  d2.add_grid_generators(gs);
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
inline Constraint_System
Direct_Product<D1, D2>::constraints() const {
  Constraint_System cs = d2.constraints();
  const Constraint_System& cs1 = d1.constraints();
  for (Constraint_System::const_iterator i = cs1.begin(),
	 cs_end = cs1.end(); i != cs_end; ++i)
    cs.insert(*i);
  return cs;
}

template <typename D1, typename D2>
inline Constraint_System
Direct_Product<D1, D2>::minimized_constraints() const {
  Constraint_System cs = d2.constraints();
  const Constraint_System& cs1 = d1.constraints();
  for (Constraint_System::const_iterator i = cs1.begin(),
	 cs_end = cs1.end(); i != cs_end; ++i)
    cs.insert(*i);
  if (cs.has_strict_inequalities()) {
    C_Polyhedron ph(cs);
    return ph.minimized_constraints();
  }
  else {
    NNC_Polyhedron ph(cs);
    return ph.minimized_constraints();
  }
}

template <typename D1, typename D2>
inline Congruence_System
Direct_Product<D1, D2>::congruences() const {
  Congruence_System cgs = d2.congruences();
  const Constraint_System& cgs1 = d1.congruences();
  for (Congruence_System::const_iterator i = cgs1.begin(),
	 cgs_end = cgs1.end(); i != cgs_end; ++i)
    cgs.insert(*i);
  return cgs;
}

template <typename D1, typename D2>
inline Congruence_System
Direct_Product<D1, D2>::minimized_congruences() const {
  Congruence_System cgs = d2.congruences();
  const Constraint_System& cgs1 = d1.congruences();
  for (Congruence_System::const_iterator i = cgs1.begin(),
	 cgs_end = cgs1.end(); i != cgs_end; ++i)
    cgs.insert(*i);
  Grid gr(cgs);
  return gr.minimized_congruences();
}

template <typename D1, typename D2>
inline Generator_System
Direct_Product<D1, D2>::generators() const {
  return d1.generators();
}

template <typename D1, typename D2>
inline Generator_System
Direct_Product<D1, D2>::minimized_generators() const {
  return d1.minimized_generators();
}

template <typename D1, typename D2>
inline Grid_Generator_System
Direct_Product<D1, D2>::grid_generators() const {
  return d1.grid_generators();
}

template <typename D1, typename D2>
inline Grid_Generator_System
Direct_Product<D1, D2>::minimized_grid_generators() const {
  return d1.minimized_grid_generators();
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
  return d1.is_disjoint_from(y.d1) || d2.is_disjoint_from(y.d2);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_discrete() const {
  return d1.is_discrete() || d2.is_discrete();
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::is_bounded() const {
  return d1.is_bounded() || d2.is_bounded();
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::bounds_from_above(
                                        const Linear_Expression& expr) const {
  return d1.bounds_from_above(expr) || d2.bounds_from_above(expr);
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::bounds_from_below(
                                        const Linear_Expression& expr) const {
  return d1.bounds_from_below(expr) || d2.bounds_from_below(expr);
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::maximize(const Linear_Expression& expr,
				   Coefficient& sup_n,
                                   Coefficient& sup_d,
                                   bool& maximum) const {
  if (is_empty())
    return false;
  Coefficient sup1_n;
  Coefficient sup1_d;
  Coefficient sup2_n;
  Coefficient sup2_d;
  bool maximum1;
  bool maximum2;
  bool r1 = d1.maximize(expr, sup1_n, sup1_d, maximum1);
  bool r2 = d2.maximize(expr, sup2_n, sup2_d, maximum2);
  /* if neither is bounded from above, return false */
  if (!r1 && !r2)
    return false;
  /* if only d2 is bounded from above, then use the values for d2 */
  if (!r1) {
    sup_n = sup2_n;
    sup_d = sup2_d;
    maximum = maximum2;
    return true;
  }
  /* if only d1 is bounded from above, then use the values for d1 */
  if (!r2) {
    sup_n = sup1_n;
    sup_d = sup1_d;
    maximum = maximum1;
    return true;
  }
  /* if both d1 and d2 are bounded from above, then use the minimum values */
  if (sup2_d * sup1_n >= sup1_d * sup2_n) {
    sup_n = sup1_n;
    sup_d = sup1_d;
    maximum = maximum1;
  }
  else {
    sup_n = sup2_n;
    sup_d = sup2_d;
    maximum = maximum2;
  }
  return true;
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::minimize(const Linear_Expression& expr,
				   Coefficient& inf_n,
                                   Coefficient& inf_d,
                                   bool& minimum) const {
  if (is_empty())
    return false;
  Coefficient inf1_n;
  Coefficient inf1_d;
  Coefficient inf2_n;
  Coefficient inf2_d;
  bool minimum1;
  bool minimum2;
  bool r1 = d1.minimize(expr, inf1_n, inf1_d, minimum1);
  bool r2 = d2.minimize(expr, inf2_n, inf2_d, minimum2);
  /* if neither is bounded from below, return false */
  if (!r1 && !r2)
    return false;
  /* if only d2 is bounded from below, then use the values for d2 */
  if (!r1) {
    inf_n = inf2_n;
    inf_d = inf2_d;
    minimum = minimum2;
    return true;
  }
  /* if only d1 is bounded from below, then use the values for d1 */
  if (!r2) {
    inf_n = inf1_n;
    inf_d = inf1_d;
    minimum = minimum1;
    return true;
  }
  /* if both d1 and d2 are bounded from below, then use the minimum values */
  if (inf2_d * inf1_n <= inf1_d * inf2_n) {
    inf_n = inf1_n;
    inf_d = inf1_d;
    minimum = minimum1;
  }
  else {
    inf_n = inf2_n;
    inf_d = inf2_d;
    minimum = minimum2;
  }
  return true;
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::maximize(const Linear_Expression& expr,
				   Coefficient& sup_n,
                                   Coefficient& sup_d,
                                   bool& maximum,
                                   Generator& pnt) const {
  if (is_empty())
    return false;
  Coefficient sup1_n;
  Coefficient sup1_d;
  Coefficient sup2_n;
  Coefficient sup2_d;
  bool maximum1;
  bool maximum2;
  Generator pnt1(point());
  Generator pnt2(point());
  bool r1 = d1.maximize(expr, sup1_n, sup1_d, maximum1, pnt1);
  bool r2 = d2.maximize(expr, sup2_n, sup2_d, maximum2, pnt2);
  /* if neither is bounded from above, return false */
  if (!r1 && !r2)
    return false;
  /* if only d2 is bounded from above, then use the values for d2 */
  if (!r1) {
    sup_n = sup2_n;
    sup_d = sup2_d;
    maximum = maximum2;
    pnt = pnt2;
    return true;
  }
  /* if only d1 is bounded from above, then use the values for d1 */
  if (!r2) {
    sup_n = sup1_n;
    sup_d = sup1_d;
    maximum = maximum1;
    pnt = pnt1;
    return true;
  }
  /* if both d1 and d2 are bounded from above, then use the minimum values */
  if (sup2_d * sup1_n >= sup1_d * sup2_n) {
    sup_n = sup1_n;
    sup_d = sup1_d;
    maximum = maximum1;
    pnt = pnt1;
  }
  else {
    sup_n = sup2_n;
    sup_d = sup2_d;
    maximum = maximum2;
    pnt = pnt2;
  }
  return true;
 }

template <typename D1, typename D2>
inline bool
  Direct_Product<D1, D2>::minimize(const Linear_Expression& expr,
				   Coefficient& inf_n,
                                   Coefficient& inf_d,
                                   bool& minimum,
                                   Generator& pnt) const {
  if (is_empty())
    return false;
  Coefficient inf1_n;
  Coefficient inf1_d;
  Coefficient inf2_n;
  Coefficient inf2_d;
  bool minimum1;
  bool minimum2;
  Generator pnt1(point());
  Generator pnt2(point());
  bool r1 = d1.minimize(expr, inf1_n, inf1_d, minimum1, pnt1);
  bool r2 = d2.minimize(expr, inf2_n, inf2_d, minimum2, pnt2);
  /* if neither is bounded from below, return false */
  if (!r1 && !r2)
    return false;
  /* if only d2 is bounded from below, then use the values for d2 */
  if (!r1) {
    inf_n = inf2_n;
    inf_d = inf2_d;
    minimum = minimum2;
    pnt = pnt2;
    return true;
  }
  /* if only d1 is bounded from below, then use the values for d1 */
  if (!r2) {
    inf_n = inf1_n;
    inf_d = inf1_d;
    minimum = minimum1;
    pnt = pnt1;
    return true;
  }
  /* if both d1 and d2 are bounded from below, then use the minimum values */
  if (inf2_d * inf1_n <= inf1_d * inf2_n) {
    inf_n = inf1_n;
    inf_d = inf1_d;
    minimum = minimum1;
    pnt = pnt1;
  }
  else {
    inf_n = inf2_n;
    inf_d = inf2_d;
    minimum = minimum2;
    pnt = pnt2;
  }
  return true;
 }

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::widening_assign(const Direct_Product& y,
					unsigned* tp) {
  d1.widening_assign(y.d1, tp);
  d2.widening_assign(y.d2, tp);
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
Direct_Product<D1, D2>
::remove_space_dimensions(const Variables_Set& to_be_removed) {
  d1.remove_space_dimensions(to_be_removed);
  d2.remove_space_dimensions(to_be_removed);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::remove_higher_space_dimensions(dimension_type new_dimension) {
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
Direct_Product<D1, D2>
::expand_space_dimension(Variable var, dimension_type m) {
  d1.expand_space_dimension(var, m);
  d2.expand_space_dimension(var, m);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>
::fold_space_dimensions(const Variables_Set& to_be_folded,
			Variable var) {
  d1.fold_space_dimensions(to_be_folded, var);
  d2.fold_space_dimensions(to_be_folded, var);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::contains(const Direct_Product& y) const {
  return d1.contains(y.d1) && d2.contains(y.d2);
}

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::strictly_contains(const Direct_Product& y) const {
  return d1.strictly_contains(y.d1) && d2.strictly_contains(y.d2);
}

// FIXME: Improve this name.
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

// FIXME: Move to dedicated file when name for Open_Product is
//        decided.

template <typename D1, typename D2>
inline bool propagate_constraints_reduce(D1& d1, D2& d2) {
  d1.add_constraints(d2.constraints());
  d2.add_constraints(d1.constraints());
  return true;
}

template <typename D1, typename D2>
inline bool standard_reduce (D1& d1, D2& d2) {
  return empty_check_reduce(d1, d2)
    || propagate_constraints_reduce(d1, d2);
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(dimension_type num_dimensions,
				      const Degenerate_Element kind)
  : Direct_Product<D1, D2>(num_dimensions, kind) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(const Congruence_System& ccgs)
  : Direct_Product<D1, D2>(ccgs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(Congruence_System& cgs)
  : Direct_Product<D1, D2>(cgs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(const Constraint_System& ccs)
  : Direct_Product<D1, D2>(ccs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(Constraint_System& cs)
  : Direct_Product<D1, D2>(cs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(const Grid_Generator_System& gs)
  : Direct_Product<D1, D2>(gs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(Grid_Generator_System& gs)
  : Direct_Product<D1, D2>(gs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(const Generator_System& gs)
  : Direct_Product<D1, D2>(gs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(Generator_System& gs)
  : Direct_Product<D1, D2>(gs) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
template <typename Interval>
inline
Open_Product<D1, D2, R>::Open_Product(const Box<Interval>& box)
  : Direct_Product<D1, D2>(box) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::Open_Product(const Open_Product& y)
  : Direct_Product<D1, D2>(y) {
  clear_reduced_flag();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline
Open_Product<D1, D2, R>::~Open_Product() {
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Open_Product<D1, D2, R>&
Open_Product<D1, D2, R>::operator=(const Open_Product& y) {
  return Direct_Product<D1, D2>::operator=(y);
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Constraint_System
Open_Product<D1, D2, R>::constraints() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.constraints();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Constraint_System
Open_Product<D1, D2, R>::minimized_constraints() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.minimized_constraints();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Congruence_System
Open_Product<D1, D2, R>::congruences() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.congruences();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Congruence_System
Open_Product<D1, D2, R>::minimized_congruences() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.minimized_congruences();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Generator_System
Open_Product<D1, D2, R>::generators() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.generators();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Generator_System
Open_Product<D1, D2, R>::minimized_generators() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.minimized_generators();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Grid_Generator_System
Open_Product<D1, D2, R>::grid_generators() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.grid_generators();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline Grid_Generator_System
Open_Product<D1, D2, R>::minimized_grid_generators() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.minimized_grid_generators();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_empty() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.is_empty() || op.d2.is_empty();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_universe() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.is_universe() && op.d2.is_universe();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_topologically_closed() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.is_topologically_closed() && op.d2.is_topologically_closed();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_disjoint_from(const Open_Product& y) const {
  const Open_Product& op = *this;
  Open_Product copy = op;
  copy.intersection_assign(y);
  copy.reduce();
  return copy.is_empty();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_bounded() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.is_bounded() || op.d2.is_bounded();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_discrete() const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.is_discrete() || op.d2.is_discrete();
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::contains(const Open_Product& y) const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.contains(y.d1) && op.d2.contains(y.d2);
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::strictly_contains(const Open_Product& y) const {
  const Open_Product& op = *this;
  op.reduce();
  return op.d1.strictly_contains(y.d1) && op.d2.strictly_contains(y.d2);
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::reduce() const {
  Open_Product& op = const_cast<Open_Product&>(*this);
  if (op.is_reduced())
    return false;
  bool modified = R(op.d1, op.d2);
  set_reduced_flag();
  return modified;
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline bool
Open_Product<D1, D2, R>::is_reduced() const {
  return reduced;
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline void
Open_Product<D1, D2, R>::clear_reduced_flag() const {
  const_cast<Open_Product&>(*this).reduced = false;
}

template <typename D1, typename D2, bool R(D1&, D2&)>
inline void
Open_Product<D1, D2, R>::set_reduced_flag() const {
  const_cast<Open_Product&>(*this).reduced = true;
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
