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

#ifndef PPL_Direct_Product_templates_hh
#define PPL_Direct_Product_templates_hh 1

#include "Interval.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Grid_Generator_System.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

// FIX Direct_Product.cc for full specializations

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::ascii_load(std::istream& s) {
  std::string str;
  return ((s >> str) && str == "Domain"
	  && (s >> str) && str == "1:"
	  && d1.ascii_load(s)
	  && (s >> str) && str == "Domain"
	  && (s >> str) && str == "2:"
	  && d2.ascii_load(s));
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(const Grid_Generator_System& gs)
  : d2(gs) {
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(Grid_Generator_System& gs)
  : d2(gs) {
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(const Generator_System& gs)
  : d1(gs) {
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(Generator_System& gs)
  : d1(gs) {
}

template <>
template <typename Box>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(const Box& box,
						     From_Covering_Box dummy)
  : d2(box, dummy) {
  // FIXME: create ph from covering box
}

template <>
inline const Constraint_System&
Direct_Product<NNC_Polyhedron, Grid>::constraints() const {
  const_cast<NNC_Polyhedron&>(d1).add_congruences(d2.minimized_congruences());
  return d1.constraints();
}

template <>
inline const Constraint_System&
Direct_Product<NNC_Polyhedron, Grid>::minimized_constraints() const {
  const_cast<NNC_Polyhedron&>(d1).add_congruences(d2.minimized_congruences());
  return d1.minimized_constraints();
}

template <>
inline const Congruence_System&
Direct_Product<NNC_Polyhedron, Grid>::congruences() const {
  const_cast<Grid&>(d2).add_constraints(d1.minimized_constraints());
  return d2.congruences();
}

template <>
inline const Congruence_System&
Direct_Product<NNC_Polyhedron, Grid>::minimized_congruences() const {
  const_cast<Grid&>(d2).add_constraints(d1.minimized_constraints());
  return d2.minimized_congruences();
}

template <>
inline const Generator_System&
Direct_Product<NNC_Polyhedron, Grid>::generators() const {
  const_cast<NNC_Polyhedron&>(d1).add_congruences(d2.minimized_congruences());
  return d1.generators();
}

template <>
inline const Generator_System&
Direct_Product<NNC_Polyhedron, Grid>::minimized_generators() const {
  const_cast<NNC_Polyhedron&>(d1).add_congruences(d2.minimized_congruences());
  return d1.minimized_generators();
}

template <>
inline const Grid_Generator_System&
Direct_Product<NNC_Polyhedron, Grid>::grid_generators() const {
  const_cast<Grid&>(d2).add_constraints(d1.minimized_constraints());
  return d2.grid_generators();
}

template <>
inline const Grid_Generator_System&
Direct_Product<NNC_Polyhedron, Grid>::minimized_grid_generators() const {
  const_cast<Grid&>(d2).add_constraints(d1.minimized_constraints());
  return d2.minimized_grid_generators();
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>::add_congruence(const Congruence& cg) {
  d1.add_congruence(cg);
  d2.add_congruence(cg);
}

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline void								\
  Direct_Product<class1, class2>::add_generator(const Generator& g) {	\
    d1.add_generator(g);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<class2, class1>::add_generator(const Generator& g) {	\
    d2.add_generator(g);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline bool								\
  Direct_Product<class1, class2>::add_generator_and_minimize(const Generator& g) { \
    return d1.add_generator_and_minimize(g);				\
  }									\
  template <>								\
  inline bool								\
  Direct_Product<class2, class1>::add_generator_and_minimize(const Generator& g) { \
    return d2.add_generator_and_minimize(g);				\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline void								\
  Direct_Product<class1, class2>::add_grid_generator(const Grid_Generator& g) { \
    d2.add_grid_generator(g);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<class2, class1>::add_grid_generator(const Grid_Generator& g) { \
    d1.add_grid_generator(g);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline bool								\
  Direct_Product<class1, class2>::add_grid_generator_and_minimize(const Grid_Generator& g) { \
    return d2.add_grid_generator_and_minimize(g);			\
  }									\
  template <>								\
  inline bool								\
  Direct_Product<class2, class1>::add_grid_generator_and_minimize(const Grid_Generator& g) { \
    return d1.add_grid_generator_and_minimize(g);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>::add_congruences(const Congruence_System& cgs) { \
    d1.add_congruences(cgs);						\
    d2.add_congruences(cgs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>::add_congruences(const Congruence_System& cgs) { \
    d1.add_congruences(cgs);						\
    d2.add_congruences(cgs);						\
  }									\

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>::add_generators(const Generator_System& gs) { \
    d1.add_generators(gs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>::add_generators(const Generator_System& gs) { \
    d2.add_generators(gs);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>::add_grid_generators(const Grid_Generator_System& gs) { \
    d2.add_grid_generators(gs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>::add_grid_generators(const Grid_Generator_System& gs) { \
    d1.add_grid_generators(gs);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_image(Variable var,
			   const Relation_Symbol relsym,
			   const Linear_Expression& expr,
			   Coefficient_traits::const_reference denominator) {
  d1.generalized_affine_image(var, relsym, expr, denominator);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_image(Variable var,
			   const Linear_Expression& expr,
			   Coefficient_traits::const_reference denominator,
			   Coefficient_traits::const_reference modulus) {
  d2.generalized_affine_image(var, expr, denominator, modulus);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_preimage(Variable var,
			      const Relation_Symbol relsym,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator) {
  d1.generalized_affine_preimage(var, relsym, expr, denominator);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_preimage(Variable var,
			      const Linear_Expression& expr,
			      Coefficient_traits::const_reference denominator,
			      Coefficient_traits::const_reference modulus) {
  d2.generalized_affine_preimage(var, expr, denominator, modulus);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_image(const Linear_Expression& lhs,
			   const Relation_Symbol relsym,
			   const Linear_Expression& rhs) {
  d1.generalized_affine_image(lhs, relsym, rhs);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_image(const Linear_Expression& lhs,
			   const Linear_Expression& rhs,
			   Coefficient_traits::const_reference modulus) {
  d2.generalized_affine_image(lhs, rhs, modulus);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_preimage(const Linear_Expression& lhs,
			      const Relation_Symbol relsym,
			      const Linear_Expression& rhs) {
  d1.generalized_affine_preimage(lhs, relsym, rhs);
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>
::generalized_affine_preimage(const Linear_Expression& lhs,
			      const Linear_Expression& rhs,
			      Coefficient_traits::const_reference modulus) {
  d2.generalized_affine_preimage(lhs, rhs, modulus);
}

template <>
inline bool
Direct_Product<NNC_Polyhedron, Grid>::is_discrete() const {
  const Direct_Product& op = *this;
  return op.d1.affine_dimension() == 0 || op.d2.is_discrete();
}

// FIXME: move to dedicated file once name decided

template <>
inline bool
Open_Product<NNC_Polyhedron, Grid>::is_discrete() const {
  const Open_Product& op = *this;
  return op.d1.affine_dimension() == 0 || op.d2.is_discrete();
}

template <typename D1, typename D2>
bool
Open_Product<D1, D2>::empty_reduce_d1_with_d2() {
  D1& d1 = this->d1;
  D2& d2 = this->d2;
  d2.minimized_congruences();
  if (d2.is_empty()) {
    if (d1.is_empty())
      return false;
    d1.add_constraint(Constraint::zero_dim_false());
    return true;
  }
  return false;
}

template <typename D1, typename D2>
bool
Open_Product<D1, D2>::empty_reduce_d2_with_d1() {
  D1& d1 = this->d1;
  D2& d2 = this->d2;
  d1.minimized_constraints();
  if (d1.is_empty()) {
    if (d2.is_empty())
      return false;
    d2.add_congruence(Congruence::zero_dim_false());
    return true;
  }
  return true;
}

template <>
inline bool
Open_Product<NNC_Polyhedron, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}

template <>
inline bool
Open_Product<C_Polyhedron, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}

#if 0
template <>
inline bool
Open_Product<BD_Shape<T>, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}
#endif

template <>
inline bool
Open_Product<NNC_Polyhedron, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}

template <>
inline bool
Open_Product<C_Polyhedron, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}

// FIXME: error: no member function 'reduce_domain2_with_domain1' declared in 'Parma_Polyhedra_Library::Open_Product<Parma_Polyhedra_Library::BD_Shape<T>, Parma_Polyhedra_Library::Grid>'
#if 0
template <>
template <typename T>
inline bool
Open_Product<BD_Shape<T>, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}
#endif

#if 0
template <typename D1, typename D2>
inline bool
Open_Product<D1, D2>::reduce_ph_with_gr() {
  // Skeleton attempt at simple reduction.

  // Reduce ph d1 with gr d2 by moving ph c's to nearest grid point
  // (any grid point, inside or outside the ph).

  // Always either ==, >= or >.
  // FIX include >
  // for each axis

  // FIX using dim 1 (A)

  //    flatten points to axis

  D2 d2_copy = d2;
  //d2_copy.remove_higher_space_dimensions(1);
  // FIX need to leave single space dim

  //    for each relational c in ph

  if (d1.has_pending_constraints() && !d1.process_pending_constraints())
    // d1 found empty.
    return false;

  TEMP_INTEGER(temp);
  bool modified = false;
  Constraint_System& cs = const_cast<Constraint_System&>(d1.constraints());
  for (Constraint_System::const_iterator i = cs.begin(),
         cs_end = cs.end(); i != cs_end; ++i) {
    const Constraint& c = *i;

    c.ascii_dump();

  //       if the constraint affects the axis

    if (c.is_equality() || c.coefficient(Variable(0)) == 0)
      continue;

  //          incr/decr const term to nearest grid point (depending on direction of relation)

    // FIX assume >=

    std::cout << "adjust c" << std::endl;

    // FIX include c divisors

    // FIX will signs of cterms be the same?

    Constraint& writable_c = const_cast<Constraint&>(c);
    Congruence_System& cgs = const_cast<Congruence_System&>(d2_copy.minimized_congruences());
    std::cout << "   using cg:" << std::endl;
    cgs.begin()->ascii_dump();
    // FIX include cg cterm <> 0

    // Substitute into cg the FIX extreme value of the single
    // dimension of c.
    temp = (- writable_c[0] *
	    (cgs.begin()->coefficient(Variable(0))))
      + (cgs.begin()->inhomogeneous_term());
    // Find the distance to the next module closest to the origin.
    temp %= (cgs.begin()->modulus() * writable_c[1]);
    writable_c[0] -= temp;
    if (temp < 0)
      // Raise to next module.
      writable_c[0] -= cgs.begin()->modulus();

    writable_c.strong_normalize();

    std::cout << "c after" << std::endl;
    c.ascii_dump();

    // FIX now need to adjust the entire cs? clear minimal form?
    d1.clear_constraints_minimized();
    d1.clear_generators_up_to_date();
    modified = true;
  }

  return modified;
}
#endif

template <>
inline bool
Open_Product<NNC_Polyhedron, Grid>::reduce() {
  bool modified = reduce_domain1_with_domain2();
  if (reduce_domain2_with_domain1()) {
    modified = true;
    while (reduce_domain1_with_domain2() && reduce_domain1_with_domain2());
  }
#if 0
  if (reduce_ph_with_gr()) {
    modified = true;
    while (reduce_ph_with_gr() && reduce_ph_with_gr());
  }
#endif
  return modified;
}

template <typename D1, typename D2>
bool
Open_Product<D1, D2>::reduce() {
  bool modified = reduce_domain1_with_domain2();
  if (reduce_domain2_with_domain1()) {
    modified = true;
    while (reduce_domain1_with_domain2() && reduce_domain1_with_domain2());
  }
  return modified;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Direct_Product_templates_hh)
