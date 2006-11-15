/* PPL Java interface C_Polyhedron routines implementation.
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

#include "ppl_java_C_Polyhedron.h"
#include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_build_1cpp_1object__Lppl_1java_Constraint_1System_2
(JNIEnv* env, jobject j_c_polyhedron, jobject j_iterable) {
  jclass j_c_polyhedron_class = env->GetObjectClass(j_c_polyhedron);
  Constraint_System cs = build_ppl_constraint_system(env, j_iterable);
  C_Polyhedron* c_ptr = new C_Polyhedron(cs);
  jfieldID pointer_field = env->GetFieldID(j_c_polyhedron_class, "ptr", "J");
  env->SetLongField(j_c_polyhedron, pointer_field, (long long) c_ptr);
}

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_build_1cpp_1object__JLppl_1java_Degenerate_1Element_2
(JNIEnv* env, jobject j_c_polyhedron, jlong j_dim,
 jobject j_degenerate_element) {
  jclass degenerate_element_class
    = env->FindClass("ppl_java/Degenerate_Element");
  jmethodID degenerate_element_ordinal_id
    = env->GetMethodID(degenerate_element_class, "ordinal", "()I");
  jint j_degenerate_element_int
    = env->CallIntMethod(j_degenerate_element, degenerate_element_ordinal_id);

  jclass j_c_polyhedron_class = env->GetObjectClass(j_c_polyhedron);
  C_Polyhedron* c_ptr;
  switch (j_degenerate_element_int) {
  case 0:
    c_ptr = new C_Polyhedron(j_dim, UNIVERSE);
  case 1:
    c_ptr = new C_Polyhedron(j_dim, EMPTY);
  default:
    throw std::runtime_error("PPL Java interface internal error");
  }
  jfieldID pointer_field = env->GetFieldID(j_c_polyhedron_class, "ptr", "J");
  env->SetLongField(j_c_polyhedron, pointer_field, (long long) c_ptr);
}

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_build_1cpp_1object__Lppl_1java_Generator_1System_2
(JNIEnv* env, jobject j_c_polyhedron, jobject j_iterable) {
  jclass j_c_polyhedron_class = env->GetObjectClass(j_c_polyhedron);
  Generator_System gs = build_ppl_generator_system(env, j_iterable);
  C_Polyhedron* c_ptr = new C_Polyhedron(gs);
  jfieldID pointer_field = env->GetFieldID(j_c_polyhedron_class, "ptr", "J");
  env->SetLongField(j_c_polyhedron, pointer_field, (long long) c_ptr);
}

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_build_1cpp_1object__Lppl_1java_Grid_1Generator_1System_2
(JNIEnv* env, jobject  j_c_polyhedron, jobject j_ggs) {
  jclass j_c_polyhedron_class = env->GetObjectClass(j_c_polyhedron);
  Grid_Generator_System gs = build_ppl_grid_generator_system(env, j_ggs);
  C_Polyhedron* c_ptr = new C_Polyhedron(gs);
  jfieldID pointer_field = env->GetFieldID(j_c_polyhedron_class, "ptr", "J");
  env->SetLongField(j_c_polyhedron, pointer_field, (long long) c_ptr);
}

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_build_1cpp_1object__Lppl_1java_Congruence_1System_2
(JNIEnv* env, jobject  j_c_polyhedron, jobject j_cgs) {
  jclass j_c_polyhedron_class = env->GetObjectClass(j_c_polyhedron);
  Congruence_System cgs = build_ppl_congruence_system(env, j_cgs);
  C_Polyhedron* c_ptr = new C_Polyhedron(cgs);
  jfieldID pointer_field = env->GetFieldID(j_c_polyhedron_class, "ptr", "J");
  env->SetLongField(j_c_polyhedron, pointer_field, (long long) c_ptr);
}

JNIEXPORT void JNICALL Java_ppl_1java_C_1Polyhedron_finalize
(JNIEnv* env, jobject j_c_polyhedron) {
  jlong this_ptr = get_ptr(env, j_c_polyhedron);
  Polyhedron* str  = reinterpret_cast<C_Polyhedron*>(this_ptr);
  delete str;
}
