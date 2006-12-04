/* Implementation of the OCaml interface.
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

#include "ppl.hh"

// OCaml include files.
#define CAML_NAME_SPACE
extern "C" {
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/alloc.h"
}

#include <stdexcept>
#include <sstream>
#include <cstdio>
#include <cerrno>
#include <climits>
#include <iostream>
#include <algorithm>


using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#define CATCH_ALL							\
  catch(std::bad_alloc&) {						\
    caml_raise_out_of_memory();						\
  }									\
  catch(std::invalid_argument& e) {					\
    caml_invalid_argument(const_cast<char*>(e.what()));			\
  }									\
  catch(std::overflow_error& e) {					\
    caml_raise_with_string(*caml_named_value("PPL_arithmetic_overflow"), \
			   (const_cast<char*>(e.what())));		\
  }									\
  catch(std::runtime_error& e) {					\
    caml_raise_with_string(*caml_named_value("PPL_internal_error"),	\
			   (const_cast<char*>(e.what())));		\
  }									\
  catch(std::exception& e) {						\
    caml_raise_with_string(*caml_named_value("PPL_unknown_standard_exception"), \
			   (const_cast<char*>(e.what())));		\
  }									\
  catch(...) {								\
    caml_raise_constant(*caml_named_value("PPL_unexpected_error"));	\
  }


// Function for the management of mpz_t integers.
extern "C" struct custom_operations _mlgmp_custom_z;

static inline mpz_t* mpz_val(value val) {
  return ((mpz_t*) (Data_custom_val(val)));
}

static inline value alloc_mpz(void) {
  return caml_alloc_custom(&_mlgmp_custom_z, sizeof(mpz_t), 0, 1);
}

Variable
build_ppl_Variable(value caml_var) {
  return Variable(Long_val(caml_var));
}

// FIXME: This should be placed in a more convenient place inside
// the library.
//! Reinterpret an mpz_t as mpz_class.
mpz_class&
reinterpret_mpz_class(mpz_t n) {
  return reinterpret_cast<mpz_class&>(*n);
}

value
build_caml_coefficient(const Coefficient& ppl_coeff) {
  value ml_coeff = alloc_mpz();
  mpz_init(*mpz_val(ml_coeff));
  assign_r(reinterpret_mpz_class(*mpz_val(ml_coeff)), ppl_coeff,
 	   ROUND_NOT_NEEDED);
  return ml_coeff;
}

Coefficient
build_ppl_Coefficient(value coeff) {
   mpz_class z((__mpz_struct*) Data_custom_val(coeff));
   return Coefficient(z);
}

Linear_Expression
build_ppl_Linear_Expression(value e) {
  switch (Tag_val(e)) {
  case 0:
    // Variable
    return Variable(Long_val(Field(e, 0)));
  case 1: {
    // Coefficient
    mpz_class z((__mpz_struct*) Data_custom_val(Field(e, 0)));
    return Linear_Expression(Coefficient(z));
  }
  case 2:
    // Unary_Plus
    return build_ppl_Linear_Expression(Field(e, 0));
  case 3:
    // Unary_Minus
    return -build_ppl_Linear_Expression(Field(e, 0));
  case 4:
    // Plus
    return build_ppl_Linear_Expression(Field(e, 0))
      + build_ppl_Linear_Expression(Field(e, 1));
  case 5:
    // Minus
    return build_ppl_Linear_Expression(Field(e, 0))
      - build_ppl_Linear_Expression(Field(e, 1));
  case 6: {
    // Times
    mpz_class z((__mpz_struct*) Data_custom_val(Field(e, 0)));
    return Coefficient(z) * build_ppl_Linear_Expression(Field(e, 1));
  }
  default:
    caml_invalid_argument("Error building PPL::Linear_Expression");
  }
}

Relation_Symbol
build_ppl_relsym(value caml_relsym) {
  switch (Int_val(caml_relsym)) {
  case 0: {
    return LESS_THAN;
  }
  case 1: {
    return LESS_THAN_OR_EQUAL;
  }
 case 2: {
   return EQUAL;
 }
  case 3: {
    return GREATER_THAN_OR_EQUAL;
  }
  case 4: {
    return GREATER_THAN;
  }
 default:
    ;
  }
  // We should not be here!
  throw std::runtime_error("PPL Caml interface internal error");
  }

Constraint
build_ppl_Constraint(value c) {
  value e1 = Field(c, 0);
  value e2 = Field(c, 1);
  switch (Tag_val(c)) {
  case 0:
    // Less_Than
    return build_ppl_Linear_Expression(e1) < build_ppl_Linear_Expression(e2);
  case 1:
    // Less_Than_Or_Equal
    return build_ppl_Linear_Expression(e1) <= build_ppl_Linear_Expression(e2);
  case 2:
    // Equal
    return build_ppl_Linear_Expression(e1) == build_ppl_Linear_Expression(e2);
  case 3:
    // Greater_Than
    return build_ppl_Linear_Expression(e1) > build_ppl_Linear_Expression(e2);
  case 4:
    // Greater_Than_Or_Equal
    return build_ppl_Linear_Expression(e1) >= build_ppl_Linear_Expression(e2);
  default:
    caml_invalid_argument("Error building PPL::Constraint");
  }
}


template <typename R>
CAMLprim value
get_inhomogeneous_term(const R& r) {
  Coefficient coeff = -r.inhomogeneous_term();
  value coeff_term = caml_alloc(1,1);
  Field(coeff_term, 0) = build_caml_coefficient(coeff);
  return coeff_term;
}

// Takes from constraints, generators... the embedded linear
// expression.
template <typename R>
CAMLprim value
get_linear_expression(const R& r) {
  dimension_type space_dimension = r.space_dimension();
  dimension_type varid = 0;
  Coefficient coeff;
  while (varid < space_dimension
	 && (coeff = r.coefficient(Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    value zero_term = caml_alloc(1,1);
    value zero_mpz = alloc_mpz();
    mpz_init_set_ui(*mpz_val(zero_mpz), 0);
    Field(zero_term, 0) = zero_mpz;
    return zero_term;
  }
  else {
    value term1 = caml_alloc(2,6);
    Coefficient ppl_coeff = r.coefficient(Variable(varid));
    Field(term1, 0) = build_caml_coefficient(ppl_coeff);
    value ml_le_var1 = caml_alloc(1,0);
    Field(ml_le_var1, 0) = Val_int(varid);
    Field(term1, 1) = ml_le_var1;
    while (true) {
      ++varid;
      value sum;
      while (varid < space_dimension
	     && (coeff = r.coefficient(Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	return term1;
      else {
	sum = caml_alloc(2,4);
	value term2 = caml_alloc(2,6);
	Coefficient ppl_coeff = r.coefficient(Variable(varid));
	Field(term2, 0) = build_caml_coefficient(ppl_coeff);
	value ml_le_var2 = caml_alloc(1,0);
	Field(ml_le_var2, 0) = Val_int(varid);
	Field(term2, 1) = ml_le_var2;
	Field(sum, 0) = term1;
	Field(sum, 1) = term2;
	term1 = sum;
      }
    }
  }
}

value
build_caml_generator(const Generator& ppl_generator) {
  switch (ppl_generator.type()) {
  case Generator::LINE: {
    // Store the linear expression. (1,0) stands for
    // allocate one block (the linear expression) with Tag 0 (a line here).
    value caml_generator = caml_alloc(1,0);
    Field(caml_generator, 0) = get_linear_expression(ppl_generator);
    return caml_generator;
    break;
  }
  case Generator::RAY: {
    value caml_generator = caml_alloc(1,1);
    Field(caml_generator, 0) = get_linear_expression(ppl_generator);
    return caml_generator;
    break;
  }
  case Generator::POINT:  {
    // Allocates two blocks (the linear expression and the divisor)
    // of tag 2 (Point).
    value caml_generator = caml_alloc(2,2);
    Field(caml_generator, 0) = get_linear_expression(ppl_generator);
    const Coefficient& divisor = ppl_generator.divisor();
    Field(caml_generator, 1) = build_caml_coefficient(divisor);
    return caml_generator;
    break;
  }
  case Generator::CLOSURE_POINT:  {
    value caml_generator = caml_alloc(2,3);
    Field(caml_generator, 0) = get_linear_expression(ppl_generator);
    const Coefficient& divisor = ppl_generator.divisor();
    Field(caml_generator, 1) =  build_caml_coefficient(divisor);
    return caml_generator;
    break;
  }
  default:
    throw std::runtime_error("PPL OCaml interface internal error");
  }
}


value
build_caml_constraint(const Constraint& ppl_constraint) {
  switch (ppl_constraint.type()) {
  case Constraint::EQUALITY: {
    value caml_constraint = caml_alloc(2,2);
    Field(caml_constraint, 0) = get_linear_expression(ppl_constraint);
    Field(caml_constraint, 1) = get_inhomogeneous_term(ppl_constraint);
    return caml_constraint;
    break;
  }
  case Constraint::STRICT_INEQUALITY: {
    value caml_constraint = caml_alloc(2,3);
    Field(caml_constraint, 0) = get_linear_expression(ppl_constraint);
    Field(caml_constraint, 1) = get_inhomogeneous_term(ppl_constraint);
    return caml_constraint;
    break;
  }
  case Constraint::NONSTRICT_INEQUALITY:  {
    value caml_constraint = caml_alloc(2,4);
    Field(caml_constraint, 0) = get_linear_expression(ppl_constraint);
    Field(caml_constraint, 1) = get_inhomogeneous_term(ppl_constraint);
    return caml_constraint;
    break;
  }
  default:
    throw std::runtime_error("PPL OCaml interface internal error");
  }
}

value
build_caml_congruence(const Congruence& ppl_congruence) {
    value caml_congruence = caml_alloc(3,0);
    Field(caml_congruence, 0) = get_linear_expression(ppl_congruence);
    Field(caml_congruence, 1) = get_inhomogeneous_term(ppl_congruence);
    const Coefficient& modulus = ppl_congruence.modulus();
    Field(caml_congruence, 2) = build_caml_coefficient(modulus);
    return caml_congruence;
}

value
build_caml_congruence_system(const Congruence_System& ppl_cgs) {
  // This code builds a list of constraints starting from bottom to
  // top. A list on OCaml must be built like a sequence of Cons and Tail.
  // The first element is the Nil list (the Val_int(0)).
  value caml_cgs_tail = Val_int(0);
  bool tail_built = false;
  for (Congruence_System::const_iterator v_begin = ppl_cgs.begin(),
  	 v_end = ppl_cgs.end(); v_begin != v_end; ++v_begin) {
    if (!tail_built) {
      tail_built = true;
      caml_cgs_tail = caml_alloc_tuple(2);
      Field(caml_cgs_tail, 1) = Val_int(0);
    }
    Field(caml_cgs_tail, 0) = build_caml_congruence(*v_begin);
    Congruence_System::const_iterator itr = v_begin;
    ++itr;
    // If we have a `next` element, make space to store it in the next
    // cycle.
    if (itr != v_end) {
      value new_tail = caml_alloc_tuple(2);
      Field(new_tail, 1) = caml_cgs_tail;
      caml_cgs_tail = new_tail;
    }
  }
  return caml_cgs_tail;
}

value
build_caml_constraint_system(const Constraint_System& ppl_cs) {
  // This code builds a list of constraints starting from bottom to
  // top. A list on OCaml must be built like a sequence of Cons and Tail.
  // The first element is the Nil list (the Val_int(0)).
  value caml_cs_tail = Val_int(0);
  bool tail_built = false;
  for (Constraint_System::const_iterator v_begin = ppl_cs.begin(),
  	 v_end = ppl_cs.end(); v_begin != v_end; ++v_begin) {
    if (!tail_built) {
      tail_built = true;
      caml_cs_tail = caml_alloc_tuple(2);
      Field(caml_cs_tail, 1) = Val_int(0);
    }
    Field(caml_cs_tail, 0) = build_caml_constraint(*v_begin);
    Constraint_System::const_iterator itr = v_begin;
    ++itr;
    // If we have a `next` element, make space to store it in the next
    // cycle.
    if (itr != v_end) {
      value new_tail = caml_alloc_tuple(2);
      Field(new_tail, 1) = caml_cs_tail;
      caml_cs_tail = new_tail;
    }
  }
  return caml_cs_tail;
}

value
build_caml_generator_system(const Generator_System& ppl_gs) {
  value caml_gs_tail = Val_int(0);
  bool tail_built = false;
  for (Generator_System::const_iterator v_begin = ppl_gs.begin(),
  	 v_end = ppl_gs.end(); v_begin != v_end; ++v_begin) {
    if (!tail_built) {
      tail_built = true;
      caml_gs_tail = caml_alloc_tuple(2);
      Field(caml_gs_tail, 1) = Val_int(0);
    }
    Field(caml_gs_tail, 0) = build_caml_generator(*v_begin);
    Generator_System::const_iterator itr = v_begin;
    ++itr;
    if (itr != v_end) {
      value new_tail = caml_alloc_tuple(2);
      Field(new_tail, 1) = caml_gs_tail;
      caml_gs_tail = new_tail;
    }
  }
  return caml_gs_tail;
}

Congruence
build_ppl_Congruence(value c) {
  value e1 = Field(c, 0);
  value e2 = Field(c, 1);
  mpz_class z((__mpz_struct*) Data_custom_val(Field(c, 2)));
  Linear_Expression lhs = build_ppl_Linear_Expression(e1);
  Linear_Expression rhs = build_ppl_Linear_Expression(e2);
  return ((lhs %= rhs) / z);
}

Generator
build_ppl_Generator(value g) {
  switch (Tag_val(g)) {
  case 0:
    // Line
    return Generator::line(build_ppl_Linear_Expression(Field(g, 0)));
  case 1:
    // Ray
    return Generator::ray(build_ppl_Linear_Expression(Field(g, 0)));
  case 2: {
    // Point
    mpz_class z((__mpz_struct*) Data_custom_val(Field(g, 1)));
    return Generator::point(build_ppl_Linear_Expression(Field(g, 0)),
			    Coefficient(z));
  }
  case 3: {
    // Closure_point
    mpz_class z((__mpz_struct*) Data_custom_val(Field(g, 1)));
    return Generator::closure_point(build_ppl_Linear_Expression(Field(g, 0)),
				    Coefficient(z));
  }
  default:
    caml_invalid_argument("Error building PPL::Constraint");
  }
}

Constraint_System
build_ppl_Constraint_System(value cl) {
  Constraint_System cs;
  while (cl != Val_int(0)) {
    cs.insert(build_ppl_Constraint(Field(cl, 0)));
    cl = Field(cl, 1);
  }
  return cs;
}

Generator_System
build_ppl_Generator_System(value gl) {
  Generator_System gs;
  while (gl != Val_int(0)) {
    gs.insert(build_ppl_Generator(Field(gl, 0)));
    gl = Field(gl, 1);
  }
  return gs;
}

Congruence_System
build_ppl_Congruence_System(value cgl) {
  Congruence_System cgs;
  while (cgl != Val_int(0)) {
    cgs.insert(build_ppl_Congruence(Field(cgl, 0)));
    cgl = Field(cgl, 1);
  }
  return cgs;
}

//! Give access to the embedded Polyhedron* in \p v.
inline Polyhedron*&
p_Polyhedron_val(value v) {
  return *reinterpret_cast<Polyhedron**>(Data_custom_val(v));
}

void
custom_Polyhedron_finalize(value v) {
  std::cerr << "About to delete a polyhedron " << *p_Polyhedron_val(v)
	    << std::endl;
  delete p_Polyhedron_val(v);
}

static struct custom_operations Polyhedron_custom_operations = {
  "it.unipr.cs.ppl" "." PPL_VERSION "." "Polyhedron",
  custom_Polyhedron_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

inline value
val_p_Polyhedron(const Polyhedron& ph) {
  value v = caml_alloc_custom(&Polyhedron_custom_operations,
			      sizeof(Polyhedron*), 0, 1);
  p_Polyhedron_val(v) = const_cast<Polyhedron*>(&ph);
  return(v);
}

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_space_dimension(value d) try {
  CAMLparam1(d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(dd)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_constraint_system(value cl) try {
  CAMLparam1(cl);
  Constraint_System cs = build_ppl_Constraint_System(cl);
  Generator_System gs;
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(cs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_generator_system(value gl) try {
  CAMLparam1(gl);
  Generator_System gs = build_ppl_Generator_System(gl);
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(gs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_congruence_system(value gl) try {
  CAMLparam1(gl);
  Congruence_System gs = build_ppl_Congruence_System(gl);
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(gs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_space_dimension(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  dimension_type d = pph.space_dimension();
  if (d > INT_MAX)
    abort();
  CAMLreturn(Val_int(d));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_affine_dimension(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  dimension_type d = pph.affine_dimension();
  if (d > INT_MAX)
    abort();
  CAMLreturn(Val_int(d));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_empty(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_empty()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_universe(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_universe()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_contains_integer_point(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.contains_integer_point()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_topologically_closed(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_topologically_closed()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_bounded(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_bounded()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounds_from_below(value ph, value le) try {
  CAMLparam2(ph, le);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Linear_Expression ple = build_ppl_Linear_Expression(le);
  CAMLreturn(Val_bool(pph.bounds_from_below(ple)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounds_from_above(value ph, value le) try {
  CAMLparam2(ph, le);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Linear_Expression ple = build_ppl_Linear_Expression(le);
  CAMLreturn(Val_bool(pph.bounds_from_above(ple)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_constraint(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint pc = build_ppl_Constraint(c);
  pph.add_constraint(pc);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_constraint_and_minimize(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint pc = build_ppl_Constraint(c);
  CAMLreturn(Val_bool(pph.add_constraint_and_minimize(pc)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_constraints(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint_System pcs = build_ppl_Constraint_System(cs);
  pph.add_constraints(pcs);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_constraints_and_minimize(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint_System pcs = build_ppl_Constraint_System(cs);
  CAMLreturn(Val_bool(pph.add_constraints_and_minimize(pcs)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_generator(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator pc = build_ppl_Generator(c);
  pph.add_generator(pc);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_generator_and_minimize(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator pc = build_ppl_Generator(c);
  CAMLreturn(Val_bool(pph.add_generator_and_minimize(pc)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_generators(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator_System pcs = build_ppl_Generator_System(cs);
  pph.add_generators(pcs);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_generators_and_minimize(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator_System pcs = build_ppl_Generator_System(cs);
  CAMLreturn(Val_bool(pph.add_generators_and_minimize(pcs)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_congruences(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Congruence_System pc = build_ppl_Congruence_System(c);
  pph.add_congruences(pc);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_disjoint_from(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.is_disjoint_from(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_contains(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.contains(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_strictly_contains(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.strictly_contains(pph2)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_intersection_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.intersection_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_intersection_assign_and_minimize(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.intersection_assign_and_minimize(pph2)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_poly_hull_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.poly_hull_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_poly_hull_assign_and_minimize(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.poly_hull_assign_and_minimize(pph2)));
}
CATCH_ALL


extern "C"
void
ppl_Polyhedron_upper_bound_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.upper_bound_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_poly_difference_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.poly_difference_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_difference_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.difference_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_time_elapse_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.time_elapse_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_concatenate_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.concatenate_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_add_space_dimensions_and_embed(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.add_space_dimensions_and_embed(dd);
  CAMLreturn0;
							     }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_add_space_dimensions_and_project(value ph,
							       value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.add_space_dimensions_and_project(dd);
  CAMLreturn0;
							       }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_remove_higher_space_dimensions(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.remove_higher_space_dimensions(dd);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_expand_space_dimension(value ph,
						     value var_index,
						     value m) try {
  CAMLparam3(ph, var_index, m);
  int c_var_index = Int_val(var_index);
  if (c_var_index < 0)
    abort();
  int c_m = Int_val(m);
  if (c_m < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.expand_space_dimension(build_ppl_Variable(c_var_index), c_m);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_constraints(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_constraint_system(pph.constraints()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_minimized_constraints(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_constraint_system(pph.minimized_constraints()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_generators(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_generator_system(pph.generators()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_minimized_generators(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_generator_system(pph.minimized_generators()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_congruences(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_congruence_system(pph.congruences()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_minimized_congruences(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_congruence_system(pph.minimized_congruences()));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_bounded_affine_image(value ph, value var, value lb_expr,
				    value ub_expr, value coeff) try {
  CAMLparam5(ph, var, lb_expr, ub_expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.bounded_affine_image(build_ppl_Variable(Val_int(var)),
			   build_ppl_Linear_Expression(lb_expr),
 			   build_ppl_Linear_Expression(ub_expr),
 			   build_ppl_Coefficient(coeff));
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_bounded_affine_preimage(value ph, value var, value lb_expr,
				       value ub_expr, value coeff) try {
  CAMLparam5(ph, var, lb_expr, ub_expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.bounded_affine_preimage(build_ppl_Variable(Val_int(var)),
			      build_ppl_Linear_Expression(lb_expr),
			      build_ppl_Linear_Expression(ub_expr),
			      build_ppl_Coefficient(coeff));
  CAMLreturn0;
				    }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_affine_image(value ph, value var, value expr,
			    value coeff) try {
  CAMLparam4(ph, var, expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  build_ppl_Linear_Expression(expr);
  pph.affine_image(build_ppl_Variable(var),
		   build_ppl_Linear_Expression(expr),
		   build_ppl_Coefficient(coeff));
  CAMLreturn0;
			    }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_affine_preimage(value ph, value var, value expr,
			       value coeff) try {
  CAMLparam4(ph, var, expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.affine_preimage(build_ppl_Variable(var),
		      build_ppl_Linear_Expression(expr),
		      build_ppl_Coefficient(coeff));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_image1(value ph, value le1, value rel_sym,
					 value le2) try {
  CAMLparam4(ph, le1, rel_sym, le2);
  build_ppl_relsym(rel_sym);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_image(build_ppl_Linear_Expression(le1),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le2));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_image2(value ph, value int_val,
					 value rel_sym,
					 value le, value caml_coeff) try {
  CAMLparam5(ph, int_val, rel_sym, le, caml_coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_image(build_ppl_Variable(int_val),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le),
			       build_ppl_Coefficient(caml_coeff));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_preimage1(value ph, value le1, value rel_sym,
					    value le2) try {
  CAMLparam4(ph, le1, rel_sym, le2);
  build_ppl_relsym(rel_sym);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_preimage(build_ppl_Linear_Expression(le1),
				  build_ppl_relsym(rel_sym),
				  build_ppl_Linear_Expression(le2));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_preimage2(value ph, value int_val,
					 value rel_sym,
					 value le, value caml_coeff) try {
  CAMLparam5(ph, int_val, rel_sym, le, caml_coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_preimage(build_ppl_Variable(int_val),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le),
			       build_ppl_Coefficient(caml_coeff));
  CAMLreturn0;
 }
CATCH_ALL


extern "C"
CAMLprim value ppl_Polyhedron_BHRZ03_widening_assign(value ph1, value ph2,
						     value integer) try {
  CAMLparam3(ph1, ph2, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.BHRZ03_widening_assign(pph2, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.limited_BHRZ03_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(value ph1, value ph2,
				    value caml_cs,
				    value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.bounded_BHRZ03_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value ppl_Polyhedron_H79_widening_assign(value ph1, value ph2,
						  value integer) try {
  CAMLparam3(ph1, ph2, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.H79_widening_assign(pph2, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_limited_H79_extrapolation_assign(value ph1, value ph2,
						value caml_cs,
						value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.limited_H79_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounded_H79_extrapolation_assign(value ph1, value ph2,
						value caml_cs,
						value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.bounded_H79_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_maximize(value ph, value caml_le) try {
  CAMLparam2(ph, caml_le);
  Coefficient num, den = 0;
  bool is_supremum = false;
  Generator g = point();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  bool ppl_return_value = pph.maximize(build_ppl_Linear_Expression(caml_le),
				      num, den, is_supremum, g);
  value caml_return_value = caml_alloc(5,0);
  Field(caml_return_value, 0) = Val_bool(ppl_return_value);
  Field(caml_return_value, 1) = build_caml_coefficient(num);
  Field(caml_return_value, 2) = build_caml_coefficient(den);
  Field(caml_return_value, 3) = Val_bool(is_supremum);
  Field(caml_return_value, 4) = build_caml_generator(g);
  CAMLreturn(caml_return_value);
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_minimize(value ph, value caml_le) try {
  CAMLparam2(ph, caml_le);
  Coefficient num, den = 0;
  bool is_supremum = false;
  Generator g = point();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  bool ppl_return_value = pph.minimize(build_ppl_Linear_Expression(caml_le),
				      num, den, is_supremum, g);
  value caml_return_value = caml_alloc(5,0);
  Field(caml_return_value, 0) = Val_bool(ppl_return_value);
  Field(caml_return_value, 1) = build_caml_coefficient(num);
  Field(caml_return_value, 2) = build_caml_coefficient(den);
  Field(caml_return_value, 3) = Val_bool(is_supremum);
  Field(caml_return_value, 4) = build_caml_generator(g);
  CAMLreturn(caml_return_value);
}
CATCH_ALL


extern "C"
CAMLprim value
ppl_Polyhedron_OK(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Bool_val(pph.OK()));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_swap(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.swap(pph2);
  CAMLreturn0;
}
CATCH_ALL


extern "C"
CAMLprim void
test_linear_expression(value ocaml_le) {
  CAMLparam1(ocaml_le);
  Linear_Expression cxx_le = build_ppl_Linear_Expression(ocaml_le);
  std::cout << cxx_le << std::endl;
  CAMLreturn0;
}

extern "C"
CAMLprim void
test_linear_constraint(value ocaml_c) {
  CAMLparam1(ocaml_c);
  Constraint cxx_c = build_ppl_Constraint(ocaml_c);
  std::cout << cxx_c << std::endl;
  CAMLreturn0;
}

extern "C"
CAMLprim void
test_constraint_system(value cl) {
  CAMLparam1(cl);
  Constraint_System cs = build_ppl_Constraint_System(cl);
  std::cout << cs << std::endl;
  CAMLreturn0;
}

extern "C"
CAMLprim void
test_linear_generator(value ocaml_g) {
  CAMLparam1(ocaml_g);
  Generator cxx_g = build_ppl_Generator(ocaml_g);
  std::cout << cxx_g << std::endl;
  CAMLreturn0;
}

extern "C"
CAMLprim void
test_generator_system(value gl) {
  CAMLparam1(gl);
  Generator_System gs = build_ppl_Generator_System(gl);
  std::cout << gs << std::endl;
  CAMLreturn0;
}

#if 0
void
ppl_error_out_of_memory() {
  caml_raise_out_of_memory();
}

void
ppl_error_invalid_argument() {
  caml_raise_constant(*caml_named_value("invalid_argument"));
}

void
ppl_arithmetic_overflow() {
  caml_raise_constant(*caml_named_value("arithmetic_overflow"));
}

void
ppl_stdio_error() {
  caml_raise_constant(*caml_named_value("stdio_error"));
}

void
ppl_internal_error() {
  caml_raise_constant(*caml_named_value("internal_error"));
}

void
ppl_unknow_standard_exception() {
  caml_raise_constant(*caml_named_value("standard_exception"));
}

void
ppl_error_unexpected_error() {
  caml_raise_constant(*caml_named_value("unexpected_error"));
}
#endif
