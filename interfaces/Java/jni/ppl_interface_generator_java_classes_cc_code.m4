dnl  -*- C++ -*-
m4_divert(-1)

This m4 file contains the code for generating ppl_java_<CLASS_NAME>.cc

Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ .

FIXME: Find a way to avoid having these dummy macros.
No code is needed for these procedure schemas in the Java interface
as the tokens argument for widening and extrapolation is optional.

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_widening_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code', `')

Define here as empty any known schematic method macros for which
the definition is not yet implemented.

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
`dnl
#include "parma_polyhedra_library_@CLASS@_Iterator.h"
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_1Iterator_equals
(JNIEnv* env, jobject j_this_it, jobject j_it) {
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_this_itr_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(get_ptr(env, j_this_it));
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(get_ptr(env, j_it));
return *@LTOPOLOGY@@LCLASS@_itr_ptr == *@LTOPOLOGY@@LCLASS@_this_itr_ptr;
}
')

m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_@BEGINEND@_1iterator
  (JNIEnv* env, jobject j_this_powerset) {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_powerset));
jclass j_it_class = env->FindClass("parma_polyhedra_library/@TOPOLOGY@@CLASS@_Iterator");
jmethodID j_it_ctr_id = env->GetMethodID(j_it_class, "<init>", "()V");
jobject j_it = env->NewObject(j_it_class, j_it_ctr_id);
@TOPOLOGY@@CPP_CLASS@::iterator* ppl_it = new @TOPOLOGY@@CPP_CLASS@::iterator(this_@LCLASS@->@BEGINEND@());
set_ptr(env, j_it, ppl_it);
return j_it;
}
')

m4_define(`ppl_@CLASS@_delete_iterator_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_delete_1iterator
(JNIEnv* env) {
jclass j_it_class = env->FindClass("parma_polyhedra_library/@CLASS@_Iterator");
 if (!is_java_marked(env, j_it_class))
  delete j_it_class;
}

')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code',
`dnl
JNIEXPORT void JNICALL
   Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_1Iterator_@ALT_INCDEC@
(JNIEnv* env, jobject j_it) {
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(get_ptr(env, j_it));
@CPPX_INCDEC@(*@LTOPOLOGY@@LCLASS@_itr_ptr);
}
')

m4_define(`m4_increment_extra_op_name', `next')
m4_define(`m4_decrement_extra_op_name', `prev')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_1Iterator_get_1disjunct
(JNIEnv* env, jobject j_it) {
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(get_ptr(env, j_it));
jclass j_class = env->FindClass("parma_polyhedra_library/@CLASSTOPOLOGY@@DISJUNCT@");
jmethodID j_ctr_id = env->GetMethodID(j_class, "<init>", "()V");
jobject j_obj = env->NewObject(j_class, j_ctr_id);
set_ptr(env, j_obj,  &((*@LTOPOLOGY@@LCLASS@_itr_ptr)->element()), true);
return j_obj;
}
')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_drop_1disjunct
(JNIEnv* env, jobject j_pps, jobject j_it) {
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(get_ptr(env, j_it));
 @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@@LCLASS@_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_pps));
@LTOPOLOGY@@LCLASS@_ptr->drop_disjunct(*@LTOPOLOGY@@LCLASS@_itr_ptr);
}
')


m4_define(`ppl_@CLASS@_add_disjunct_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_add_1disjunct
(JNIEnv* env, jobject j_pps, jobject j_disj) {
 @TOPOLOGY@@CPP_CLASS@* pps_ptr
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_pps));
 @CLASSTOPOLOGY@@CPP_DISJUNCT@* disj_ptr
 = reinterpret_cast<@CLASSTOPOLOGY@@CPP_DISJUNCT@*>(get_ptr(env, j_disj));
pps_ptr->add_disjunct(*disj_ptr);
}
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__JLparma_1polyhedra_1library_Degenerate_1Element_2
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@, jlong j_dim,
 jobject j_degenerate_element) {
  dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(j_dim);
  jclass degenerate_element_class
    = env->FindClass("parma_polyhedra_library/Degenerate_Element");
  jmethodID degenerate_element_ordinal_id
    = env->GetMethodID(degenerate_element_class, "ordinal", "()I");
  jint j_degenerate_element_int
    = env->CallIntMethod(j_degenerate_element, degenerate_element_ordinal_id);
  @TOPOLOGY@@CPP_CLASS@* c_ptr;
  switch (j_degenerate_element_int) {
  case 0:
    c_ptr = new @TOPOLOGY@@CPP_CLASS@(ppl_dim, UNIVERSE);
    break;
 case 1:
    c_ptr = new @TOPOLOGY@@CPP_CLASS@(ppl_dim, EMPTY);
    break;
  default:
    throw std::runtime_error("PPL Java interface internal error");
  }
 set_ptr(env, j_@LTOPOLOGY@@LCLASS@, c_ptr);
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__Lparma_1polyhedra_1library_@1FRIEND@_2
(JNIEnv* env, jobject  j_this_@LTOPOLOGY@@LCLASS@, jobject j_@LFRIEND@)
{
 @CPPX_FRIEND@* @LFRIEND@_ptr
 = reinterpret_cast<@CPPX_FRIEND@*>(get_ptr(env, j_@LFRIEND@));
 @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@_this_@LCLASS@_ptr = new @TOPOLOGY@@CPP_CLASS@(*@LFRIEND@_ptr);
 set_ptr(env, j_this_@LTOPOLOGY@@LCLASS@, @LTOPOLOGY@_this_@LCLASS@_ptr);
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__Lparma_1polyhedra_1library_@1FRIEND@_2Lparma_1polyhedra_1library_Complexity_1Class_2
(JNIEnv* env, jobject j_this_@LTOPOLOGY@@LCLASS@,
              jobject j_@LFRIEND@,
              jobject j_complexity)
{
  @CPPX_FRIEND@* @LFRIEND@_ptr
  = reinterpret_cast<@CPPX_FRIEND@*>(get_ptr(env, j_@LFRIEND@));
  jclass complexity_class
    = env->FindClass("parma_polyhedra_library/Complexity_Class");
  jmethodID complexity_ordinal_id
    = env->GetMethodID(complexity_class, "ordinal", "()I");
  jint j_complexity_int
    = env->CallIntMethod(j_complexity, complexity_ordinal_id);
  @TOPOLOGY@@CPP_CLASS@* this_@LTOPOLOGY@_@LCLASS@_ptr;
  switch (j_complexity_int) {
  case 0:
    this_@LTOPOLOGY@_@LCLASS@_ptr = new @TOPOLOGY@@CPP_CLASS@(*@LFRIEND@_ptr, POLYNOMIAL_COMPLEXITY);
    break;
  case 1:
    this_@LTOPOLOGY@_@LCLASS@_ptr = new @TOPOLOGY@@CPP_CLASS@(*@LFRIEND@_ptr, SIMPLEX_COMPLEXITY);
    break;
  case 2:
    this_@LTOPOLOGY@_@LCLASS@_ptr = new @TOPOLOGY@@CPP_CLASS@(*@LFRIEND@_ptr, ANY_COMPLEXITY);
    break;
  default:
    throw std::runtime_error("PPL Java interface internal error");
  }
  set_ptr(env, j_this_@LTOPOLOGY@@LCLASS@, this_@LTOPOLOGY@_@LCLASS@_ptr);
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__Lparma_1polyhedra_1library_@1UBUILD_REPRESENT@_1System_2
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@, jobject j_iterable) {
  @UBUILD_REPRESENT@_System cs = build_cxx_@BUILD_REPRESENT@_system(env, j_iterable);

  @TOPOLOGY@@CPP_CLASS@* c_ptr = new @TOPOLOGY@@CPP_CLASS@(cs@RECYCLE@);
  set_ptr(env, j_@LTOPOLOGY@@LCLASS@, c_ptr);
}

')

m4_define(`ppl_@CLASS@_@UB_EXACT@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_@1UB_EXACT@
(JNIEnv* env, jobject j_this_@LTOPOLOGY@@LCLASS@, jobject j_@LTOPOLOGY@@LCLASS@) {
  @TOPOLOGY@@CPP_CLASS@* this_@LTOPOLOGY@@LCLASS@
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_this_@LTOPOLOGY@@LCLASS@));
  @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@@LCLASS@
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_@LTOPOLOGY@@LCLASS@));
  return this_@LTOPOLOGY@@LCLASS@->@UB_EXACT@(*@LTOPOLOGY@@LCLASS@);
}

')

m4_define(`ppl_delete_@CLASS@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_finalize
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@) {
  @TOPOLOGY@@CPP_CLASS@* str
 = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_@LTOPOLOGY@@LCLASS@));
 if (!is_java_marked(env, j_@LTOPOLOGY@@LCLASS@))
  delete str;
}

')

m4_define(`ppl_free_@CLASS@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_free
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@) {
  @TOPOLOGY@@CPP_CLASS@* str  =
       reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_@LTOPOLOGY@@LCLASS@));
 if (!is_java_marked(env, j_@LTOPOLOGY@@LCLASS@)) {
  	delete str;
        void* null_ptr = 0;
  	set_ptr(env, j_@LTOPOLOGY@@LCLASS@, null_ptr);
   }
}

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_swap
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    this_@LCLASS@->swap(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
JNIEXPORT jlong JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1DIMENSION@
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 return this_@LCLASS@->@DIMENSION@();
  }
  CATCH_ALL;
  return 0;
}

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1GET_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  return build_java_@GET_REPRESENT@_system(env, this_@LCLASS@->@GET_REPRESENT@s());
  }
  CATCH_ALL;
  jobject null = 0;
  return null;
}

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1CLASS@_minimized_1@1GET_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  return build_java_@GET_REPRESENT@_system(env,
				   this_@LCLASS@->minimized_@GET_REPRESENT@s());
  }
  CATCH_ALL;
  jobject null = 0;
  return null;
}

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1CLASS@_relation_1with__Lparma_1polyhedra_1library_@1URELATION_REPRESENT@_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@RELATION_REPRESENT@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @URELATION_REPRESENT@ c = build_cxx_@RELATION_REPRESENT@(env, j_@RELATION_REPRESENT@);
  Poly_@UALT_RELATION_REPRESENT@_Relation pcr = this_@LCLASS@->relation_with(c);
  return build_java_poly_@ALT_RELATION_REPRESENT@_relation(env, pcr);
  }
  CATCH_ALL;
  jobject null = 0;
  return null;
}

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1HAS_PROPERTY@
(JNIEnv* env, jobject j_@LCLASS@) {
  try {
  @CPP_CLASS@* c
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
  return c->@HAS_PROPERTY@();
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1SIMPLIFY@
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  this_@LCLASS@->@SIMPLIFY@();
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_unconstrain_1space_1dimension
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_var) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variable v = build_cxx_variable(env, j_var);
  this_@LCLASS@->unconstrain(v);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_unconstrain_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_v_set) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variables_Set v_set = build_cxx_variables_set(env, j_v_set);
  this_@LCLASS@->unconstrain(v_set);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_bounds_1from_1@1ABOVEBELOW@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject java_le) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_@LCLASS@->bounds_from_@ABOVEBELOW@(le);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1MAXMIN@__Lparma_1polyhedra_1library_Linear_1Expression_2Lparma_1polyhedra_1library_Coefficient_2Lparma_1polyhedra_1library_Coefficient_2Lparma_1polyhedra_1library_By_1Reference_2
(JNIEnv* env, jobject j_this_@LCLASS@ , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  TEMP_INTEGER(coeff_num);
  TEMP_INTEGER(coeff_den);
  coeff_num = build_cxx_coeff(env, j_coeff_num);
  coeff_den = build_cxx_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  if(this_@LCLASS@->@MAXMIN@(le, coeff_num, coeff_den, b_value)) {
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
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1MAXMIN@__Lparma_1polyhedra_1library_Linear_1Expression_2Lparma_1polyhedra_1library_Coefficient_2Lparma_1polyhedra_1library_Coefficient_2Lparma_1polyhedra_1library_By_1Reference_2Lparma_1polyhedra_1library_Generator_2
(JNIEnv* env, jobject j_this_@LCLASS@ , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean,
 jobject j_generator) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  TEMP_INTEGER(coeff_num);
  TEMP_INTEGER(coeff_den);
  coeff_num = build_cxx_coeff(env, j_coeff_num);
  coeff_den = build_cxx_coeff(env, j_coeff_den);
  Linear_Expression le = build_linear_expression(env, j_le);
  bool b_value;
  Generator g = point();
  if(this_@LCLASS@->@MAXMIN@(le, coeff_num, coeff_den, b_value, g)) {
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
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1COMPARISON@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
  return this_@LCLASS@->@COMPARISON@(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_equals
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_@LCLASS@ ) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
  return (*this_@LCLASS@)==(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_hashcode_code',
`dnl
JNIEXPORT jint JNICALL Java_parma_1polyhedra_1library_@1CLASS@_hashCode
(JNIEnv* env , jobject j_this_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  return (*this_@LCLASS@).hash_code();
  }
  CATCH_ALL;
  return 0;
}

')


m4_define(`ppl_@CLASS@_OK_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_OK
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    return this_@LCLASS@->OK();
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_add_1@1ADD_REPRESENT@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @UADD_REPRESENT@ c = build_cxx_@ADD_REPRESENT@(env, j_@ADD_REPRESENT@);
    this_@LCLASS@->add_@ADD_REPRESENT@(c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_refine_1with_1@1REFINE_REPRESENT@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@REFINE_REPRESENT@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @UREFINE_REPRESENT@ c = build_cxx_@REFINE_REPRESENT@(env, j_@REFINE_REPRESENT@);
    this_@LCLASS@->refine_with_@REFINE_REPRESENT@(c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_add_1@1ADD_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@s) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @UADD_REPRESENT@_System cs = build_cxx_@ADD_REPRESENT@_system(env, j_@ADD_REPRESENT@s);
  this_@LCLASS@->add_@ADD_REPRESENT@s(cs);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_refine_1with_1@1REFINE_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@REFINE_REPRESENT@s) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @UREFINE_REPRESENT@_System cs = build_cxx_@REFINE_REPRESENT@_system(env, j_@REFINE_REPRESENT@s);
  this_@LCLASS@->refine_with_@REFINE_REPRESENT@s(cs);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1BINOP@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
  this_@LCLASS@->@BINOP@(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_simplify_1using_1context_1assign
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
  return this_@LCLASS@->simplify_using_context_assign(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1AFFIMAGE@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_var, jobject j_le,
 jobject j_coeff) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variable v = build_cxx_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  TEMP_INTEGER(c);
  c = build_cxx_coeff(env, j_coeff);
  this_@LCLASS@->@AFFIMAGE@(v, le, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_generalized_1@1AFFIMAGE@__Lparma_1polyhedra_1library_Linear_1Expression_2Lparma_1polyhedra_1library_Relation_1Symbol_2Lparma_1polyhedra_1library_Linear_1Expression_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
  try {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_cxx_relsym(env, j_relsym);
 this_@LCLASS@->generalized_@AFFIMAGE@(lhs, relsym, rhs);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_generalized_1@1AFFIMAGE@__Lparma_1polyhedra_1library_Variable_2Lparma_1polyhedra_1library_Relation_1Symbol_2Lparma_1polyhedra_1library_Linear_1Expression_2Lparma_1polyhedra_1library_Coefficient_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
  try {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 Variable v = build_cxx_variable(env, j_variable);
 Relation_Symbol relsym = build_cxx_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 TEMP_INTEGER(c);
 c = build_cxx_coeff(env, j_coeff);
 this_@LCLASS@->generalized_@AFFIMAGE@(v, relsym, le, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_generalized_1@1AFFIMAGE@_1lhs_1rhs_1with_1congruence
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs, jobject j_modulus) {
  try {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_cxx_relsym(env, j_relsym);
 TEMP_INTEGER(modulus);
 modulus = build_cxx_coeff(env, j_modulus);
 this_@LCLASS@->generalized_@AFFIMAGE@(lhs, relsym, rhs, modulus);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@CLASS@_generalized_1@1AFFIMAGE@_1with_1congruence
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff, jobject j_modulus) {
  try {
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 Variable v = build_cxx_variable(env, j_variable);
 Relation_Symbol relsym = build_cxx_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 TEMP_INTEGER(c);
 TEMP_INTEGER(modulus);
 c = build_cxx_coeff(env, j_coeff);
 modulus = build_cxx_coeff(env, j_modulus);
 this_@LCLASS@->generalized_@AFFIMAGE@(v, relsym, le, c, modulus);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_bounded_1@1AFFIMAGE@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variable v = build_cxx_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  TEMP_INTEGER(c);
  c = build_cxx_coeff(env, j_coeff);
  this_@LCLASS@->bounded_@AFFIMAGE@(v, lhs, rhs, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1WIDEN@_1widening_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@,
 jobject j_by_ref_int) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    if (is_null(env, j_by_ref_int))
      this_@LCLASS@->@WIDEN@_widening_assign(*@LCLASS@);
    else {
      jobject j_integer = get_by_reference(env, j_by_ref_int);
      unsigned int tokens =
        jtype_to_unsigned<unsigned int>(j_integer_to_j_int(env, j_integer));
      this_@LCLASS@->@WIDEN@_widening_assign(*@LCLASS@, &tokens);
      j_integer = j_int_to_j_integer(env, tokens);
      set_by_reference(env, j_by_ref_int, j_integer);
    }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_widening_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_widening_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@,
 jobject j_by_ref_int) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    if (is_null(env, j_by_ref_int))
      this_@LCLASS@->widening_assign(*@LCLASS@);
    else {
      jobject j_integer = get_by_reference(env, j_by_ref_int);
      unsigned int tokens =
        jtype_to_unsigned<unsigned int>(j_integer_to_j_int(env, j_integer));
      this_@LCLASS@->widening_assign(*@LCLASS@, &tokens);
      j_integer = j_int_to_j_integer(env, tokens);
      set_by_reference(env, j_by_ref_int, j_integer);
    }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1EXTRAPOLATION@_1extrapolation_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@,
 jobject j_by_ref_int) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    if (is_null(env, j_by_ref_int))
      this_@LCLASS@->@EXTRAPOLATION@_extrapolation_assign(*@LCLASS@);
    else {
      jobject j_integer = get_by_reference(env, j_by_ref_int);
      unsigned int tokens =
        jtype_to_unsigned<unsigned int>(j_integer_to_j_int(env, j_integer));
      this_@LCLASS@->@EXTRAPOLATION@_extrapolation_assign(*@LCLASS@, &tokens);
      j_integer = j_int_to_j_integer(env, tokens);
      set_by_reference(env, j_by_ref_int, j_integer);
    }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1EXTRAPOLATION@_1narrowing_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    this_@LCLASS@->@EXTRAPOLATION@_narrowing_assign(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1LIMITEDBOUNDED@_1@1WIDENEXPN@_1extrapolation_1assign
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_@LCLASS@, jobject j_cs,
 jobject j_by_ref_int) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    @UCONSTRAINER@_System cs = build_cxx_@CONSTRAINER@_system(env, j_cs);
    if (is_null(env, j_by_ref_int))
      this_@LCLASS@->@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(*@LCLASS@, cs);
    else {
      jobject j_integer = get_by_reference(env, j_by_ref_int);
      unsigned int tokens =
        jtype_to_unsigned<unsigned int>(j_integer_to_j_int(env, j_integer));
      this_@LCLASS@->@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(*@LCLASS@, cs,
                                                                       &tokens);
      j_integer = j_int_to_j_integer(env, tokens);
      set_by_reference(env, j_by_ref_int, j_integer);
    }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_BGP99_1@1DISJUNCT_WIDEN@_1extrapolation_1assign
  (JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@, jobject j_disjuncts) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
   unsigned int disjuncts =
        jtype_to_unsigned<unsigned int>(j_integer_to_j_int(env, j_disjuncts));
   this_@LCLASS@->BGP99_extrapolation_assign
     (*@LCLASS@,
      widen_fun_ref(&@CLASSTOPOLOGY@@CPP_DISJUNCT@::@DISJUNCT_WIDEN@_widening_assign),
      disjuncts);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_BHZ03_1@1ALT_DISJUNCT_WIDEN@_1@1DISJUNCT_WIDEN@_1widening_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@) {
  try {
    @CPP_CLASS@* this_@LCLASS@
      = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    @CPP_CLASS@* @LCLASS@
      = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_@LCLASS@));
    this_@LCLASS@->BHZ03_widening_assign<@ALT_DISJUNCT_WIDEN@_Certificate>
      (*@LCLASS@,
       widen_fun_ref(
         &@CLASSTOPOLOGY@@CPP_DISJUNCT@::@DISJUNCT_WIDEN@_widening_assign));
  }
  CATCH_ALL;
}

')


m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_add_1space_1dimensions_1@1EMBEDPROJECT@
(JNIEnv* env, jobject j_this_@LCLASS@, jlong dim) {
  try {
dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
 @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
 this_@LCLASS@->add_space_dimensions_@EMBEDPROJECT@(ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_remove_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_v_set) {
  try {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variables_Set v_set = build_cxx_variables_set(env, j_v_set);
  this_@LCLASS@->remove_space_dimensions(v_set);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_remove_1higher_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jlong dim) {
  try {
    dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    this_@LCLASS@->remove_higher_space_dimensions(ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_expand_1space_1dimension
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jlong dim) {
  try {
    dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    Variable v = build_cxx_variable(env, j_variable);
    this_@LCLASS@->expand_space_dimension(v, ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_fold_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_v_set, jobject j_var) {
  try {
  Variables_Set v_set = build_cxx_variables_set(env, j_v_set);
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Variable v = build_cxx_variable(env, j_var);
  this_@LCLASS@->fold_space_dimensions(v_set, v);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_parma_1polyhedra_1library_@1CLASS@_map_1space_1dimensions
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_p_func) {
  try {
  @CPP_CLASS@* this_@LCLASS@
    = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  Partial_Function ppl_pfunc(j_p_func, env);
  this_@LCLASS@->map_space_dimensions(ppl_pfunc);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_string_code',
`dnl
JNIEXPORT jstring JNICALL Java_parma_1polyhedra_1library_@1CLASS@_toString
(JNIEnv* env , jobject j_this_@LCLASS@) {
  try {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  std::ostringstream s;
  s << *this_@LCLASS@;
  std::string str = s.str();
  return env->NewStringUTF(str.c_str());
  }
  CATCH_ALL;
  return 0;
}
')

m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
`JNIEXPORT jlong JNICALL Java_parma_1polyhedra_1library_@1CLASS@_@1MEMBYTES@
(JNIEnv* env, jobject j_pps) {
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_pps));
  return this_@LCLASS@->@MEMBYTES@();
}

')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
JNIEXPORT jboolean JNICALL Java_parma_1polyhedra_1library_@1CLASS@_constrains
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_var) {
  try {
    @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
    Variable v = build_cxx_variable(env, j_var);
    return this_@LCLASS@->constrains(v);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_ascii_dump_code',
`dnl
JNIEXPORT jstring JNICALL Java_parma_1polyhedra_1library_@1CLASS@_ascii_1dump
(JNIEnv* env , jobject j_this_@LCLASS@) {
  try {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  @CPP_CLASS@* this_@LCLASS@
 = reinterpret_cast<@CPP_CLASS@*>(get_ptr(env, j_this_@LCLASS@));
  std::ostringstream s;
  this_@LCLASS@->ascii_dump(s);
  std::string str = s.str();
  return env->NewStringUTF(str.c_str());
  }
  CATCH_ALL;
  return 0;
}
')

m4_define(`ppl_@CLASS@_linear_@PARTITION@_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1TOPOLOGY@@1CLASS@_linear_1@PARTITION@
(JNIEnv* env, jclass ppl_class, jobject j_p, jobject j_q) {
  try {
    // Suppress warnings concerning "ppl_class" not used.
    ppl_class = 0;
    @TOPOLOGY@@CPP_CLASS@* ph
      = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_p));
    @TOPOLOGY@@CPP_CLASS@* qh
      = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(get_ptr(env, j_q));
    std::pair<@TOPOLOGY@@CPP_CLASS@@COMMA@
              Pointset_Powerset<NNC_Polyhedron> >
      r = linear_partition(*ph, *qh);

    @TOPOLOGY@@CPP_CLASS@* r1
      = new @TOPOLOGY@@CPP_CLASS@(0, EMPTY);
    Pointset_Powerset<NNC_Polyhedron>*
      r2 = new Pointset_Powerset<NNC_Polyhedron>(0, EMPTY);
    r1->swap(r.first);
    r2->swap(r.second);

    jclass j_pair_class = env->FindClass("parma_polyhedra_library/Pair");
    jmethodID j_ctr_id_pair = env->GetMethodID(j_pair_class, "<init>", "()V");
    jobject j_pair_obj = env->NewObject(j_pair_class, j_ctr_id_pair);

    jclass j_class_r1 = env->FindClass("parma_polyhedra_library/@TOPOLOGY@@CLASS@");
    jmethodID j_ctr_id_r1 = env->GetMethodID(j_class_r1, "<init>", "()V");
    jobject j_obj_r1 = env->NewObject(j_class_r1, j_ctr_id_r1);
    set_ptr(env, j_obj_r1, r1);

    jclass j_class_r2
      = env->FindClass("parma_polyhedra_library/Pointset_Powerset_NNC_Polyhedron");
    jmethodID j_ctr_id_r2 = env->GetMethodID(j_class_r2, "<init>", "()V");
    jobject j_obj_r2 = env->NewObject(j_class_r2, j_ctr_id_r2);
    set_ptr(env, j_obj_r2, r2);
    set_pair_element(env, j_pair_obj, 0, j_obj_r1);
    set_pair_element(env, j_pair_obj, 1, j_obj_r2);
    return j_pair_obj;
  }
  CATCH_ALL;
  return 0;
}

')

m4_define(`ppl_@CLASS@_approximate_@PARTITION@_code',
`dnl
JNIEXPORT jobject JNICALL Java_parma_1polyhedra_1library_@1CLASS@_approximate_1@PARTITION@
(JNIEnv* env, jclass ppl_class, jobject j_p_@LCLASS@, jobject j_q_@LCLASS@,
 jobject j_ref_finite_bool) {
  try {
    // Suppress warnings concerning "ppl_class" not used.
    ppl_class = 0;
    @CPP_CLASS@* ph
      = reinterpret_cast<@CPP_CLASS@*>
          (get_ptr(env, j_p_@LCLASS@));
    @CPP_CLASS@* qh
      = reinterpret_cast<@CPP_CLASS@*>
          (get_ptr(env, j_q_@LCLASS@));
    bool b_finite_val;
    std::pair<@CPP_CLASS@@COMMA@ Pointset_Powerset<Grid> >
      r = approximate_partition(*ph, *qh, b_finite_val);

    @CPP_CLASS@* r1 = new @CPP_CLASS@(0, EMPTY);
    Pointset_Powerset<Grid>* r2 = new Pointset_Powerset<Grid>(0, EMPTY);
    r1->swap(r.first);
    r2->swap(r.second);

    jclass j_pair_class = env->FindClass("parma_polyhedra_library/Pair");
    jmethodID j_ctr_id_pair = env->GetMethodID(j_pair_class, "<init>", "()V");
    jobject j_pair_obj = env->NewObject(j_pair_class, j_ctr_id_pair);

    jclass j_class_r1
      = env->FindClass("parma_polyhedra_library/@CLASS@");
    jmethodID j_ctr_id_r1 = env->GetMethodID(j_class_r1, "<init>", "()V");
    jobject j_obj_r1 = env->NewObject(j_class_r1, j_ctr_id_r1);
    set_ptr(env, j_obj_r1, r1);

    jclass j_class_r2
      = env->FindClass("parma_polyhedra_library/Pointset_Powerset_Grid");
    jmethodID j_ctr_id_r2 = env->GetMethodID(j_class_r2, "<init>", "()V");
    jobject j_obj_r2 = env->NewObject(j_class_r2, j_ctr_id_r2);
    set_ptr(env, j_obj_r2, r2);
    set_pair_element(env, j_pair_obj, 0, j_obj_r1);
    set_pair_element(env, j_pair_obj, 1, j_obj_r2);
    jobject j_finite_bool = bool_to_j_boolean(env, b_finite_val);
    set_by_reference(env, j_ref_finite_bool, j_finite_bool);
    return j_pair_obj;
  }
  CATCH_ALL;
  return 0;
}

')
