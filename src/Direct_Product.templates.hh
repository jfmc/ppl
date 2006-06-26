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

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline								\
  Direct_Product<ph_class, gr_class>					\
  ::Direct_Product(const Grid_Generator_System& gs)			\
    : d2(gs) {								\
  }									\
  template <>								\
  inline								\
  Direct_Product<gr_class, ph_class>					\
  ::Direct_Product(const Grid_Generator_System& gs)			\
    : d1(gs) {								\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline								\
  Direct_Product<ph_class, gr_class>					\
  ::Direct_Product(Grid_Generator_System& gs)				\
    : d2(gs) {								\
  }									\
  template <>								\
  inline								\
  Direct_Product<gr_class, ph_class>					\
  ::Direct_Product(Grid_Generator_System& gs)				\
    : d1(gs) {								\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline								\
  Direct_Product<ph_class, gr_class>					\
  ::Direct_Product(const Generator_System& gs)				\
    : d1(gs) {								\
  }									\
  template <>								\
  inline								\
  Direct_Product<gr_class, ph_class>					\
  ::Direct_Product(const Generator_System& gs)				\
    : d2(gs) {								\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline								\
  Direct_Product<ph_class, gr_class>::Direct_Product(Generator_System& gs) \
    : d1(gs) {								\
  }									\
  template <>								\
  inline								\
  Direct_Product<gr_class, ph_class>::Direct_Product(Generator_System& gs) \
    : d2(gs) {								\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  template <typename Box>						\
  inline								\
  Direct_Product<ph_class, gr_class>					\
  ::Direct_Product(const Box& box, From_Covering_Box dummy)		\
    : d1(box.space_dimension()), d2(box, dummy) {			\
  }									\
  template <>								\
  template <typename Box>						\
  inline								\
  Direct_Product<gr_class, ph_class>					\
  ::Direct_Product(const Box& box, From_Covering_Box dummy)		\
    : d1(box, dummy), d2(box.space_dimension()) {			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

// FIXME: These return the constraints from only one of the
//        components.

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Constraint_System&					\
  Direct_Product<ph_class, gr_class>::constraints() const {		\
    return d1.constraints();						\
  }									\
  template <>								\
  inline const Constraint_System&					\
  Direct_Product<gr_class, ph_class>::constraints() const {		\
    return d2.constraints();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Constraint_System&					\
  Direct_Product<ph_class, gr_class>::minimized_constraints() const {	\
    return d1.minimized_constraints();					\
  }									\
  template <>								\
  inline const Constraint_System&					\
  Direct_Product<gr_class, ph_class>::minimized_constraints() const {	\
    return d2.minimized_constraints();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Congruence_System&					\
  Direct_Product<ph_class, gr_class>::congruences() const {		\
    return d2.congruences();						\
  }									\
  template <>								\
  inline const Congruence_System&					\
  Direct_Product<gr_class, ph_class>::congruences() const {		\
    return d1.congruences();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Congruence_System&					\
  Direct_Product<ph_class, gr_class>::minimized_congruences() const {	\
    return d2.minimized_congruences();					\
  }									\
  template <>								\
  inline const Congruence_System&					\
  Direct_Product<gr_class, ph_class>::minimized_congruences() const {	\
    return d1.minimized_congruences();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Generator_System&					\
  Direct_Product<ph_class, gr_class>::generators() const {		\
    return d1.generators();						\
  }									\
  template <>								\
  inline const Generator_System&					\
  Direct_Product<gr_class, ph_class>::generators() const {		\
    return d2.generators();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Generator_System&					\
  Direct_Product<ph_class, gr_class>::minimized_generators() const {	\
    return d1.minimized_generators();					\
  }									\
  template <>								\
  inline const Generator_System&					\
  Direct_Product<gr_class, ph_class>::minimized_generators() const {	\
    return d2.minimized_generators();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Grid_Generator_System&					\
  Direct_Product<ph_class, gr_class>::grid_generators() const {		\
    return d2.grid_generators();					\
  }									\
  template <>								\
  inline const Grid_Generator_System&					\
  Direct_Product<gr_class, ph_class>::grid_generators() const {		\
    return d1.grid_generators();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Grid_Generator_System&					\
  Direct_Product<ph_class, gr_class>::minimized_grid_generators() const { \
    return d2.minimized_grid_generators();				\
  }									\
  template <>								\
  inline const Grid_Generator_System&					\
  Direct_Product<gr_class, ph_class>::minimized_grid_generators() const { \
    return d1.minimized_grid_generators();				\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline void								\
  Direct_Product<class1, class2>::add_congruence(const Congruence& cg) { \
    d1.add_congruence(cg);						\
    d2.add_congruence(cg);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<class2, class1>::add_congruence(const Congruence& cg) { \
    d1.add_congruence(cg);						\
    d2.add_congruence(cg);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

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
//SPECIALIZE(BD_Shape<T>, Grid);  // FIX
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline bool								\
  Direct_Product<class1, class2>					\
  ::add_generator_and_minimize(const Generator& g) {			\
    return d1.add_generator_and_minimize(g);				\
  }									\
  template <>								\
  inline bool								\
  Direct_Product<class2, class1>					\
  ::add_generator_and_minimize(const Generator& g) {			\
    return d2.add_generator_and_minimize(g);				\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline void								\
  Direct_Product<class1, class2>					\
  ::add_grid_generator(const Grid_Generator& g) {			\
    d2.add_grid_generator(g);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<class2, class1>					\
  ::add_grid_generator(const Grid_Generator& g) {			\
    d1.add_grid_generator(g);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline bool								\
  Direct_Product<class1, class2>					\
  ::add_grid_generator_and_minimize(const Grid_Generator& g) {		\
    return d2.add_grid_generator_and_minimize(g);			\
  }									\
  template <>								\
  inline bool								\
  Direct_Product<class2, class1>					\
  ::add_grid_generator_and_minimize(const Grid_Generator& g) {		\
    return d1.add_grid_generator_and_minimize(g);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(class1, class2)					\
  template <>								\
  inline void								\
  Direct_Product<class1, class2>					\
  ::add_congruences(const Congruence_System& cgs) {			\
    d1.add_congruences(cgs);						\
    d2.add_congruences(cgs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<class2, class1>					\
  ::add_congruences(const Congruence_System& cgs) {			\
    d1.add_congruences(cgs);						\
    d2.add_congruences(cgs);						\
  }									\

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::add_generators(const Generator_System& gs) {			\
    d1.add_generators(gs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::add_generators(const Generator_System& gs) {			\
    d2.add_generators(gs);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::add_grid_generators(const Grid_Generator_System& gs) {		\
    d2.add_grid_generators(gs);						\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::add_grid_generators(const Grid_Generator_System& gs) {		\
    d1.add_grid_generators(gs);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_image(Variable var,				\
			     const Relation_Symbol relsym,		\
			     const Linear_Expression& expr,		\
			     Coefficient_traits::const_reference denominator) { \
    d1.generalized_affine_image(var, relsym, expr, denominator);	\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_image(Variable var,				\
			     const Relation_Symbol relsym,		\
			     const Linear_Expression& expr,		\
			     Coefficient_traits::const_reference denominator) { \
    d2.generalized_affine_image(var, relsym, expr, denominator);	\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_image(Variable var,				\
			     const Linear_Expression& expr,		\
			     Coefficient_traits::const_reference denominator, \
			     Coefficient_traits::const_reference modulus) { \
    d2.generalized_affine_image(var, expr, denominator, modulus);	\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_image(Variable var,				\
			     const Linear_Expression& expr,		\
			     Coefficient_traits::const_reference denominator, \
			     Coefficient_traits::const_reference modulus) { \
    d1.generalized_affine_image(var, expr, denominator, modulus);	\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_preimage(Variable var,				\
				const Relation_Symbol relsym,		\
				const Linear_Expression& expr,		\
				Coefficient_traits::const_reference denominator) { \
    d1.generalized_affine_preimage(var, relsym, expr, denominator);	\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_preimage(Variable var,				\
				const Relation_Symbol relsym,		\
				const Linear_Expression& expr,		\
				Coefficient_traits::const_reference denominator) { \
    d2.generalized_affine_preimage(var, relsym, expr, denominator);	\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_preimage(Variable var,				\
				const Linear_Expression& expr,		\
				Coefficient_traits::const_reference denominator, \
				Coefficient_traits::const_reference modulus) { \
    d2.generalized_affine_preimage(var, expr, denominator, modulus);	\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_preimage(Variable var,				\
				const Linear_Expression& expr,		\
				Coefficient_traits::const_reference denominator, \
				Coefficient_traits::const_reference modulus) { \
    d1.generalized_affine_preimage(var, expr, denominator, modulus);	\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_image(const Linear_Expression& lhs,		\
			     const Relation_Symbol relsym,		\
			     const Linear_Expression& rhs) {		\
    d1.generalized_affine_image(lhs, relsym, rhs);			\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_image(const Linear_Expression& lhs,		\
			     const Relation_Symbol relsym,		\
			     const Linear_Expression& rhs) {		\
    d2.generalized_affine_image(lhs, relsym, rhs);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_image(const Linear_Expression& lhs,		\
			     const Linear_Expression& rhs,		\
			     Coefficient_traits::const_reference modulus) { \
    d2.generalized_affine_image(lhs, rhs, modulus);			\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_image(const Linear_Expression& lhs,		\
			     const Linear_Expression& rhs,		\
			     Coefficient_traits::const_reference modulus) { \
    d1.generalized_affine_image(lhs, rhs, modulus);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_preimage(const Linear_Expression& lhs,		\
				const Relation_Symbol relsym,		\
				const Linear_Expression& rhs) {		\
    d1.generalized_affine_preimage(lhs, relsym, rhs);			\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_preimage(const Linear_Expression& lhs,		\
				const Relation_Symbol relsym,		\
				const Linear_Expression& rhs) {		\
    d2.generalized_affine_preimage(lhs, relsym, rhs);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline void								\
  Direct_Product<ph_class, gr_class>					\
  ::generalized_affine_preimage(const Linear_Expression& lhs,		\
				const Linear_Expression& rhs,		\
				Coefficient_traits::const_reference modulus) { \
    d2.generalized_affine_preimage(lhs, rhs, modulus);			\
  }									\
  template <>								\
  inline void								\
  Direct_Product<gr_class, ph_class>					\
  ::generalized_affine_preimage(const Linear_Expression& lhs,		\
				const Linear_Expression& rhs,		\
				Coefficient_traits::const_reference modulus) { \
    d1.generalized_affine_preimage(lhs, rhs, modulus);			\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline bool								\
  Direct_Product<ph_class, gr_class>::is_discrete() const {		\
    const Direct_Product& op = *this;					\
    return op.d1.affine_dimension() == 0 || op.d2.is_discrete();	\
  }									\
  template <>								\
  inline bool								\
  Direct_Product<gr_class, ph_class>::is_discrete() const {		\
    const Direct_Product& op = *this;					\
    return op.d2.affine_dimension() == 0 || op.d1.is_discrete();	\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

// FIXME: Is there a sensible interpretation of covering box for
//        polyhedron?  Perhaps this should just return false if the
//        intersection is a polyhedron.
#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  template <typename Box>						\
  inline void								\
  Direct_Product<ph_class, gr_class>::get_covering_box(Box& box) const { \
    d2.get_covering_box(box);						\
  }									\
  template <>								\
  template <typename Box>						\
  inline void								\
  Direct_Product<gr_class, ph_class>::get_covering_box(Box& box) const { \
    d1.get_covering_box(box);						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

// FIXME: move to dedicated file once name decided

#define SPECIALIZE(ph_class, gr_class)					\
  template <bool R(ph_class&, gr_class&)>				\
  struct Open_Product_is_discrete<ph_class, gr_class, R> {		\
      static inline bool						\
      function(const Open_Product<ph_class, gr_class, R>& op) {		\
	op.reduce();							\
	return op.d1.affine_dimension() == 0 || op.d2.is_discrete();	\
      }									\
  };									\
  template <bool R(gr_class&, ph_class&)>				\
  struct Open_Product_is_discrete<gr_class, ph_class, R> {		\
      static inline bool						\
      function(const Open_Product<gr_class, ph_class, R>& op) {		\
	op.reduce();							\
	return op.d2.affine_dimension() == 0 || op.d1.is_discrete();	\
      }									\
  };

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Constraint_System&					\
  Open_Product<ph_class, gr_class>::constraints() const {		\
    reduce();								\
    return d1.constraints();						\
  }									\
  template <>								\
  inline const Constraint_System&					\
  Open_Product<gr_class, ph_class>::constraints() const {		\
    reduce();								\
    return d2.constraints();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Constraint_System&					\
  Open_Product<ph_class, gr_class>::minimized_constraints() const {	\
    reduce();								\
    return d1.minimized_constraints();					\
  }									\
  template <>								\
  inline const Constraint_System&					\
  Open_Product<gr_class, ph_class>::minimized_constraints() const {	\
    reduce();								\
    return d2.minimized_constraints();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Congruence_System&					\
  Open_Product<ph_class, gr_class>::congruences() const {		\
    reduce();								\
    return d2.congruences();						\
  }									\
  template <>								\
  inline const Congruence_System&					\
  Open_Product<gr_class, ph_class>::congruences() const {		\
    reduce();								\
    return d1.congruences();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Congruence_System&					\
  Open_Product<ph_class, gr_class>::minimized_congruences() const {	\
    reduce();								\
    return d2.minimized_congruences();					\
  }									\
  template <>								\
  inline const Congruence_System&					\
  Open_Product<gr_class, ph_class>::minimized_congruences() const {	\
    reduce();								\
    return d1.minimized_congruences();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Generator_System&					\
  Open_Product<ph_class, gr_class>::generators() const {		\
    reduce();								\
    return d1.generators();						\
  }									\
  template <>								\
  inline const Generator_System&					\
  Open_Product<gr_class, ph_class>::generators() const {		\
    reduce();								\
    return d2.generators();						\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Generator_System&					\
  Open_Product<ph_class, gr_class>::minimized_generators() const {	\
    reduce();								\
    return d1.minimized_generators();					\
  }									\
  template <>								\
  inline const Generator_System&					\
  Open_Product<gr_class, ph_class>::minimized_generators() const {	\
    reduce();								\
    return d2.minimized_generators();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Grid_Generator_System&					\
  Open_Product<ph_class, gr_class>::grid_generators() const {		\
    reduce();								\
    return d2.grid_generators();					\
  }									\
  template <>								\
  inline const Grid_Generator_System&					\
  Open_Product<gr_class, ph_class>::grid_generators() const {		\
    reduce();								\
    return d1.grid_generators();					\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline const Grid_Generator_System&					\
  Open_Product<ph_class, gr_class>::minimized_grid_generators() const { \
    reduce();								\
    return d2.minimized_grid_generators();				\
  }									\
  template <>								\
  inline const Grid_Generator_System&					\
  Open_Product<gr_class, ph_class>::minimized_grid_generators() const { \
    reduce();								\
    return d1.minimized_grid_generators();				\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

template <typename D1, typename D2>
bool
empty_check_reduce(D1& d1, D2& d2) {
  if (d2.is_empty()) {
    if (d1.is_empty())
      return false;
    d1.add_constraint(Constraint::zero_dim_false());
    return true;
  }
  if (d1.is_empty()) {
    d2.add_constraint(Constraint::zero_dim_false());
    return true;
  }
  return false;
}

#define SPECIALIZE(ph_class, gr_class)					\
  template <>								\
  inline bool								\
  propagate_constraints_reduce(ph_class& d1, gr_class& d2) {		\
    d1.add_congruences(d2.congruences());				\
    d2.add_congruences(d1.constraints());				\
    return true;							\
  }									\
									\
  template <>								\
  inline bool								\
  propagate_constraints_reduce(gr_class& d1, ph_class& d2) {		\
    d1.add_congruences(d2.constraints());				\
    d2.add_congruences(d1.congruences());				\
    return true;							\
  }

SPECIALIZE(NNC_Polyhedron, Grid);
SPECIALIZE(C_Polyhedron, Grid);
#undef SPECIALIZE

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Direct_Product_templates_hh)
