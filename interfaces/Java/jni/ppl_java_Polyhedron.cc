/* PPL Java interface Polyhedron routines implementation.
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

#include "ppl_java_Polyhedron.h"
#include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1empty
(JNIEnv* env, jobject j_polyhedron) {
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c  = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_empty();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1universe
(JNIEnv* env, jobject j_polyhedron) {
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_universe();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_equals
(JNIEnv* env , jobject j_this_polyhedron, jobject j_polyhedron ) {
  jlong ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(ptr);
  ptr = get_ptr(env, j_polyhedron);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(ptr);
  return (*this_polyhedron)==(*polyhedron);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1topologically_1closed
(JNIEnv* env, jobject j_polyhedron){
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_topologically_closed();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1bounded
(JNIEnv* env, jobject j_polyhedron){
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_bounded();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1disjoint_1from
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->is_disjoint_from(*polyhedron);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_bounds_1from_1above
(JNIEnv* env, jobject j_this_polyhedron, jobject java_le) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_polyhedron->bounds_from_above(le);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_bounds_1from_1below
(JNIEnv* env, jobject j_this_polyhedron, jobject java_le) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_polyhedron->bounds_from_below(le);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_contains
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->contains(*polyhedron);

}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_strictly_1contains
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->strictly_contains(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1constraint
(JNIEnv* env, jobject j_this_polyhedron, jobject j_constraint) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Constraint c = build_ppl_constraint(env, j_constraint);
  this_polyhedron->add_constraint(c);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1constraint_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_constraint) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Constraint c = build_ppl_constraint(env, j_constraint);
  return this_polyhedron->add_constraint_and_minimize(c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1generator
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generator) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator g = build_ppl_generator(env, j_generator);
  this_polyhedron->add_generator(g);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1generator_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generator) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator g = build_ppl_generator(env, j_generator);
  return this_polyhedron->add_generator_and_minimize(g);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1constraints
(JNIEnv* env, jobject j_this_polyhedron, jobject j_constraints) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Constraint_System cs = build_ppl_constraint_system(env, j_constraints);
  this_polyhedron->add_constraints(cs);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1constraints_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_constraints) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Constraint_System cs = build_ppl_constraint_system(env, j_constraints);
  return this_polyhedron->add_constraints_and_minimize(cs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1generators
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generators) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator_System gs = build_ppl_generator_system(env, j_generators);
  this_polyhedron->add_generators(gs);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1generators_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generators) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator_System gs = build_ppl_generator_system(env, j_generators);
  return this_polyhedron->add_generators_and_minimize(gs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_intersection_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->intersection_assign(*polyhedron);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_intersection_1assign_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->intersection_assign_and_minimize(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_poly_1hull_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->poly_hull_assign(*polyhedron);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_poly_1hull_1assign_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->poly_hull_assign_and_minimize(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_poly_1difference_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->poly_difference_assign(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_difference_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->difference_assign(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_topological_1closure_1assign
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  this_polyhedron->topological_closure_assign();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_contains_1integer_1point
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return this_polyhedron->contains_integer_point();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1discrete
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return this_polyhedron->is_discrete();
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_time_1elapse_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->time_elapse_assign(*polyhedron);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_affine_1image
(JNIEnv* env, jobject j_this_polyhedron, jobject j_var, jobject j_le,
 jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  Coefficient c = build_ppl_coeff(env, j_coeff);
  this_polyhedron->affine_image(v, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_affine_1preimage
(JNIEnv* env, jobject j_this_polyhedron, jobject j_var, jobject j_le,
 jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  Coefficient c = build_ppl_coeff(env, j_coeff);
  this_polyhedron->affine_preimage(v, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1image__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Variable v = build_ppl_variable(env, j_variable);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 Coefficient c = build_ppl_coeff(env, j_coeff);
 this_polyhedron->generalized_affine_image(v, relsym, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1preimage__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Variable v = build_ppl_variable(env, j_variable);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 Coefficient c = build_ppl_coeff(env, j_coeff);
 this_polyhedron->generalized_affine_preimage(v, relsym, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1image__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 this_polyhedron->generalized_affine_image(lhs, relsym, rhs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1preimage__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 this_polyhedron->generalized_affine_preimage(lhs, relsym, rhs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1affine_1image
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  Coefficient c = build_ppl_coeff(env, j_coeff);
  this_polyhedron->bounded_affine_image(v, lhs, rhs, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1affine_1preimage
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  Coefficient c = build_ppl_coeff(env, j_coeff);
  this_polyhedron->bounded_affine_preimage(v, lhs, rhs, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1space_1dimensions_1and_1embed
(JNIEnv* env, jobject j_this_polyhedron, jlong dim) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 this_polyhedron->add_space_dimensions_and_embed(dim);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1space_1dimensions_1and_1project
(JNIEnv* env, jobject j_this_polyhedron, jlong dim) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  this_polyhedron->add_space_dimensions_and_project(dim);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_concatenate_1assign
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  this_polyhedron->poly_difference_assign(*polyhedron);
}


JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_remove_1space_1dimensions
(JNIEnv* env, jobject j_this_polyhedron, jobject j_v_set) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variables_Set v_set = build_ppl_variables_set(env, j_v_set);
  this_polyhedron->remove_space_dimensions(v_set);
}


JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_remove_1higher_1space_1dimensions
(JNIEnv* env, jobject j_this_polyhedron, jlong dim) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  this_polyhedron->remove_higher_space_dimensions(dim);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_expand_1space_1dimension
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jlong dim) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_variable);
  this_polyhedron->expand_space_dimension(v, dim);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_fold_1space_1dimensions
(JNIEnv* env, jobject j_this_polyhedron, jobject j_v_set, jobject j_var) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Variables_Set v_set = build_ppl_variables_set(env, j_v_set);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = build_ppl_variable(env, j_var);
  this_polyhedron->fold_space_dimensions(v_set, v);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_maximize__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2
(JNIEnv* env, jobject j_this_polyhedron , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Coefficient coeff_num = build_ppl_coeff(env, j_coeff_num);
  Coefficient coeff_den = build_ppl_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  if(this_polyhedron->maximize(le, coeff_num, coeff_den, b_value)) {
    jobject j_coeff_num_result = build_java_coeff(env, coeff_num);
    jobject j_coeff_den_result = build_java_coeff(env, coeff_den);
    set_coefficient(env, j_coeff_num, j_coeff_num_result);
    set_coefficient(env, j_coeff_den, j_coeff_den_result);
    jobject j_boolean = bool_to_j_boolean(env, b_value);
    set_by_reference(env, j_ref_boolean, j_boolean);
    return true;
  }
  return false;
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_maximize__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2Lppl_1java_Generator_2
(JNIEnv* env, jobject j_this_polyhedron , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean,
 jobject j_generator) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Coefficient coeff_num = build_ppl_coeff(env, j_coeff_num);
  Coefficient coeff_den = build_ppl_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  Generator g = point();
  if(this_polyhedron->maximize(le, coeff_num, coeff_den, b_value, g)) {
    jobject j_coeff_num_result = build_java_coeff(env, coeff_num);
    jobject j_coeff_den_result = build_java_coeff(env, coeff_den);
    jobject j_generator_result = build_java_generator(env, g);
    set_coefficient(env, j_coeff_num, j_coeff_num_result);
    set_coefficient(env, j_coeff_den, j_coeff_den_result);
    jobject j_boolean = bool_to_j_boolean(env, b_value);
    set_by_reference(env, j_ref_boolean, j_boolean);
    set_generator(env, j_generator, j_generator_result);
    return true;
  }
  return false;
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_minimize__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2
(JNIEnv* env, jobject j_this_polyhedron , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Coefficient coeff_num = build_ppl_coeff(env, j_coeff_num);
  Coefficient coeff_den = build_ppl_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  if(this_polyhedron->minimize(le, coeff_num, coeff_den, b_value)) {
    jobject j_coeff_num_result = build_java_coeff(env, coeff_num);
    jobject j_coeff_den_result = build_java_coeff(env, coeff_den);
    set_coefficient(env, j_coeff_num, j_coeff_num_result);
    set_coefficient(env, j_coeff_den, j_coeff_den_result);
    jobject j_boolean = bool_to_j_boolean(env, b_value);
    set_by_reference(env, j_ref_boolean, j_boolean);
    return true;
  }
  return false;
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_minimize__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2Lppl_1java_Generator_2
(JNIEnv* env, jobject j_this_polyhedron , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean,
 jobject j_generator) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Coefficient coeff_num = build_ppl_coeff(env, j_coeff_num);
  Coefficient coeff_den = build_ppl_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  Generator g = point();
  if(this_polyhedron->minimize(le, coeff_num, coeff_den, b_value, g)) {
    jobject j_coeff_num_result = build_java_coeff(env, coeff_num);
    jobject j_coeff_den_result = build_java_coeff(env, coeff_den);
    jobject j_generator_result = build_java_generator(env, g);
    set_coefficient(env, j_coeff_num, j_coeff_num_result);
    set_coefficient(env, j_coeff_den, j_coeff_den_result);
    jobject j_boolean = bool_to_j_boolean(env, b_value);
    set_by_reference(env, j_ref_boolean, j_boolean);
    set_generator(env, j_generator, j_generator_result);
    return true;
  }
  return false;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_BHRZ03_1widening_1assign
(JNIEnv *, jobject, jobject, jobject) {
    std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_limited_1BHRZ03_1extrapolation_1assign
    (JNIEnv *, jobject, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1BHRZ03_1extrapolation_1assign
(JNIEnv *, jobject, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_H79_1widening_1assign
(JNIEnv *, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_widening_1assign
(JNIEnv *, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_limited_1H79_1extrapolation_1assign
(JNIEnv *, jobject, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1H79_1extrapolation_1assign
(JNIEnv *, jobject, jobject, jobject, jobject) {
  std::cerr << "implement me" << std::endl;
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1grid_1generator
(JNIEnv* env, jobject j_this_polyhedron, jobject j_grid_generator) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Grid_Generator gg = build_ppl_grid_generator(env, j_grid_generator);
  this_polyhedron->add_grid_generator(gg);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1grid_1generator_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_grid_generator) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Grid_Generator gg = build_ppl_grid_generator(env, j_grid_generator);
  return this_polyhedron->add_grid_generator_and_minimize(gg);
}


JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1congruence
(JNIEnv* env , jobject j_this_polyhedron, jobject j_congruence) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Congruence cg = build_ppl_congruence(env, j_congruence);
  this_polyhedron->add_congruence(cg);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_add_1congruences
(JNIEnv* env, jobject j_this_polyhedron, jobject j_congruences) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Congruence_System cgs = build_ppl_congruence_system(env, j_congruences);
  this_polyhedron->add_congruences(cgs);
}

JNIEXPORT jlong JNICALL Java_ppl_1java_Polyhedron_space_1dimension
(JNIEnv* env, jobject j_this_polyhedron) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 return this_polyhedron->space_dimension();
}

JNIEXPORT jlong JNICALL Java_ppl_1java_Polyhedron_affine_1dimension
(JNIEnv* env, jobject j_this_polyhedron) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 return this_polyhedron->affine_dimension();
}


JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_constraints
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_constraint_system(env, this_polyhedron->constraints());
}

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_minimized_1constraints
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_constraint_system(env,
				   this_polyhedron->minimized_constraints());
}

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_generators
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_generator_system(env, this_polyhedron->generators());
}

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_minimized_1generators
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_generator_system(env,
				  this_polyhedron->minimized_generators());
}

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_congruences
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_congruence_system(env, this_polyhedron->congruences());
}


JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_minimized_1congruences
(JNIEnv* env, jobject j_this_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  return build_java_congruence_system(env,
				   this_polyhedron->minimized_congruences());
}


JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_grid_1generators
  (JNIEnv *, jobject);

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_minimized_1grid_1generators
  (JNIEnv *, jobject);

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_relation_1with__Lppl_1java_Constraint_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_constraint) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Constraint c = build_ppl_constraint(env, j_constraint);
  Poly_Con_Relation pcr = this_polyhedron->relation_with(c);
  return build_ppl_poly_con_relation(env, pcr);
}

JNIEXPORT jobject JNICALL Java_ppl_1java_Polyhedron_relation_1with__Lppl_1java_Generator_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generator) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator g = build_ppl_generator(env, j_generator);
  Poly_Gen_Relation pgr = this_polyhedron->relation_with(g);
  return build_java_poly_gen_relation(env, pgr);
}
