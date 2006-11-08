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
  Generator g = build_generator(env, j_generator);
  this_polyhedron->add_generator(g);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_add_1generator_1and_1minimize
(JNIEnv* env, jobject j_this_polyhedron, jobject j_generator) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Generator g = build_generator(env, j_generator);
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
  Variable v = j_variable_to_ppl_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
  this_polyhedron->affine_image(v, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_affine_1preimage
(JNIEnv* env, jobject j_this_polyhedron, jobject j_var, jobject j_le,
 jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = j_variable_to_ppl_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
  this_polyhedron->affine_preimage(v, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1image__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Variable v = j_variable_to_ppl_variable(env, j_variable);
 Relation_Symbol relsym = j_relsym_to_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
 this_polyhedron->generalized_affine_image(v, relsym, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1preimage__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Variable v = j_variable_to_ppl_variable(env, j_variable);
 Relation_Symbol relsym = j_relsym_to_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
 this_polyhedron->generalized_affine_preimage(v, relsym, le, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1image__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = j_relsym_to_ppl_relsym(env, j_relsym);
 this_polyhedron->generalized_affine_image(lhs, relsym, rhs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_generalized_1affine_1preimage__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_polyhedron, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
 jlong this_ptr = get_ptr(env, j_this_polyhedron);
 Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = j_relsym_to_ppl_relsym(env, j_relsym);
 this_polyhedron->generalized_affine_preimage(lhs, relsym, rhs);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1affine_1image
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = j_variable_to_ppl_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
  this_polyhedron->bounded_affine_image(v, lhs, rhs, c);
}

JNIEXPORT void JNICALL Java_ppl_1java_Polyhedron_bounded_1affine_1preimage
(JNIEnv* env, jobject j_this_polyhedron, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Variable v = j_variable_to_ppl_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  Coefficient c = j_coeff_to_ppl_coeff(env, j_coeff);
  this_polyhedron->bounded_affine_preimage(v, lhs, rhs, c);
}
