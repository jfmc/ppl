dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

m4_define(`ppl_@CLASS@_begin_iterator_code',
`dnl
`#'include "ppl_java_@CLASS@_Iterator.h"
JNIEXPORT jobject JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_begin_1iterator
  (JNIEnv* env, jobject j_this_powerset) {
 jlong powerset_ptr = get_ptr(env, j_this_powerset);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(powerset_ptr);
jclass j_it_class = env->FindClass("ppl_java/@TOPOLOGY@@CLASS@_Iterator");
jmethodID j_it_ctr_id = env->GetMethodID(j_it_class, "<init>", "()V");
jobject j_it = env->NewObject(j_it_class, j_it_ctr_id);
@TOPOLOGY@@CPP_CLASS@::iterator* ppl_it = new @TOPOLOGY@@CPP_CLASS@::iterator(this_@LCLASS@->begin());
set_ptr(env, j_it, (long long) ppl_it);
return j_it;
}
')

m4_define(`ppl_@CLASS@_end_iterator_code',
`dnl
JNIEXPORT jobject JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_end_1iterator
  (JNIEnv* env, jobject j_this_powerset) {
 jlong powerset_ptr = get_ptr(env, j_this_powerset);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(powerset_ptr);
jclass j_it_class = env->FindClass("ppl_java/@TOPOLOGY@@CLASS@_Iterator");
jmethodID j_it_ctr_id = env->GetMethodID(j_it_class, "<init>", "()V");
jobject j_it = env->NewObject(j_it_class, j_it_ctr_id);
@TOPOLOGY@@CPP_CLASS@::iterator* ppl_it = new @TOPOLOGY@@CPP_CLASS@::iterator(this_@LCLASS@->end());
set_ptr(env, j_it, (long long) ppl_it);
return j_it;
}
')

m4_define(`ppl_@CLASS@_increment_iterator_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_1Iterator_next
(JNIEnv* env, jobject j_it) {
 jlong ptr = get_ptr(env, j_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
++(*@LTOPOLOGY@@LCLASS@_itr_ptr);
}
')

m4_define(`ppl_@CLASS@_decrement_iterator_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_1Iterator_prev
(JNIEnv* env, jobject j_it) {
 jlong ptr = get_ptr(env, j_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
--(*@LTOPOLOGY@@LCLASS@_itr_ptr);
}
')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`dnl
JNIEXPORT jobject JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_1Iterator_get_1disjunct
(JNIEnv* env, jobject j_it) {
 jlong ptr = get_ptr(env, j_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
jclass j_class = env->FindClass("ppl_java/@ALT_CPP_DISJUNCT@");
jmethodID j_ctr_id = env->GetMethodID(j_class, "<init>", "()V");
jobject j_obj = env->NewObject(j_class, j_ctr_id);
  set_ptr(env, j_obj, (long long) &((*@LTOPOLOGY@@LCLASS@_itr_ptr)->element()));set_is_a_reference(env, j_obj, true);
return j_obj;
}
')
m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',`
dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_1Iterator_equals
(JNIEnv* env, jobject j_this_it, jobject j_it) {
jlong ptr = get_ptr(env, j_this_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_this_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
ptr = get_ptr(env, j_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
return *@LTOPOLOGY@@LCLASS@_itr_ptr == *@LTOPOLOGY@@LCLASS@_this_itr_ptr;
}
')

m4_define(`ppl_@CLASS@_size_code',
`JNIEXPORT jlong JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_size
(JNIEnv* env, jobject j_pps) {
  jlong this_ptr = get_ptr(env, j_pps);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  return this_@LCLASS@->size();
}

')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_drop_1disjunct
(JNIEnv* env, jobject j_pps, jobject j_it) {
 jlong ptr = get_ptr(env, j_it);
 @TOPOLOGY@@CPP_CLASS@::iterator* @LTOPOLOGY@@LCLASS@_itr_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@::iterator*>(ptr);
 ptr = get_ptr(env, j_pps);
 @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@@LCLASS@_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(ptr);
@LTOPOLOGY@@LCLASS@_ptr->drop_disjunct(*@LTOPOLOGY@@LCLASS@_itr_ptr);
}
')


m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__JLppl_1java_Degenerate_1Element_2
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@, jlong j_dim,
 jobject j_degenerate_element) {
  dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(j_dim);
  jclass degenerate_element_class
    = env->FindClass("ppl_java/Degenerate_Element");
  jmethodID degenerate_element_ordinal_id
    = env->GetMethodID(degenerate_element_class, "ordinal", "()I");
  jint j_degenerate_element_int
    = env->CallIntMethod(j_degenerate_element, degenerate_element_ordinal_id);

  jclass j_@LTOPOLOGY@@LCLASS@_class = env->GetObjectClass(j_@LTOPOLOGY@@LCLASS@);
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
  jfieldID pointer_field = env->GetFieldID(j_@LTOPOLOGY@@LCLASS@_class, "ptr", "J");
  env->SetLongField(j_@LTOPOLOGY@@LCLASS@, pointer_field, (long long) c_ptr);
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__Lppl_1java_@1TOPOLOGY@@1CLASS@_2
(JNIEnv* env, jobject  j_this_@LTOPOLOGY@@LCLASS@, jobject j_@LTOPOLOGY@@LCLASS@)
{
 jclass j_@LTOPOLOGY@@LCLASS@_class = env->GetObjectClass(j_@LTOPOLOGY@@LCLASS@);
 jlong ptr = get_ptr(env, j_@LTOPOLOGY@@LCLASS@);
 @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@@LCLASS@_ptr = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(ptr);
 @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@_this_@LCLASS@_ptr = new @TOPOLOGY@@CPP_CLASS@(*@LTOPOLOGY@@LCLASS@_ptr);
jfieldID pointer_field = env->GetFieldID(j_@LTOPOLOGY@@LCLASS@_class, "ptr", "J");
env->SetLongField(j_this_@LTOPOLOGY@@LCLASS@, pointer_field,
		   (long long) @LTOPOLOGY@_this_@LCLASS@_ptr);

}

')


m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_build_1cpp_1object__Lppl_1java_@UBUILD_REPRESENT@_1System_2
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@, jobject j_iterable) {
  jclass j_@LTOPOLOGY@@LCLASS@_class = env->GetObjectClass(j_@LTOPOLOGY@@LCLASS@);
  @UBUILD_REPRESENT@_System cs = build_ppl_@BUILD_REPRESENT@_system(env, j_iterable);
  @TOPOLOGY@@CPP_CLASS@* c_ptr = new @TOPOLOGY@@CPP_CLASS@(cs@RECYCLE@);
  jfieldID pointer_field = env->GetFieldID(j_@LTOPOLOGY@@LCLASS@_class, "ptr", "J");
  env->SetLongField(j_@LTOPOLOGY@@LCLASS@, pointer_field, (long long) c_ptr);
}

')

m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_@1UB_EXACT@
(JNIEnv* env, jobject j_this_@LTOPOLOGY@@LCLASS@, jobject j_@LTOPOLOGY@@LCLASS@) {
  jlong ptr = get_ptr(env, j_this_@LTOPOLOGY@@LCLASS@);
  @TOPOLOGY@@CPP_CLASS@* this_@LTOPOLOGY@@LCLASS@ = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(ptr);
  ptr = get_ptr(env, j_@LTOPOLOGY@@LCLASS@);
  @TOPOLOGY@@CPP_CLASS@* @LTOPOLOGY@@LCLASS@ = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(ptr);
  return this_@LTOPOLOGY@@LCLASS@->@UB_EXACT@(*@LTOPOLOGY@@LCLASS@);
}

')

m4_define(`ppl_delete_@CLASS@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1TOPOLOGY@@1CLASS@_finalize
(JNIEnv* env, jobject j_@LTOPOLOGY@@LCLASS@) {
  jlong this_ptr = get_ptr(env, j_@LTOPOLOGY@@LCLASS@);
  @TOPOLOGY@@CPP_CLASS@* str  = reinterpret_cast<@TOPOLOGY@@CPP_CLASS@*>(this_ptr);
 if (!is_a_reference(env, j_@LTOPOLOGY@@LCLASS@))
  delete str;
}

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_swap
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
    jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
    @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    this_ptr = get_ptr(env, j_@LCLASS@);
    @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    this_@LCLASS@->swap(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
JNIEXPORT jlong JNICALL Java_ppl_1java_@1CLASS@_@1DIMENSION@
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 return this_@LCLASS@->@DIMENSION@();
  }
  CATCH_ALL;
  return 0;
}

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
JNIEXPORT jobject JNICALL Java_ppl_1java_@1CLASS@_@1GET_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  return build_java_@GET_REPRESENT@_system(env, this_@LCLASS@->@GET_REPRESENT@s());
  }
  CATCH_ALL;
  jobject null = 0;
  return null;
}

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
JNIEXPORT jobject JNICALL Java_ppl_1java_@1CLASS@_minimized_1@GET_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
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
JNIEXPORT jobject JNICALL Java_ppl_1java_@1CLASS@_relation_1with__Lppl_1java_@1URELATION_REPRESENT@_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@RELATION_REPRESENT@) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @URELATION_REPRESENT@ c = build_ppl_@RELATION_REPRESENT@(env, j_@RELATION_REPRESENT@);
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
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_@1HAS_PROPERTY@
(JNIEnv* env, jobject j_@LCLASS@) {
  try {
  jlong ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* c = reinterpret_cast<@CPP_CLASS@*>(ptr);
  return c->@HAS_PROPERTY@();
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1SIMPLIFY@
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  this_@LCLASS@->@SIMPLIFY@();
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_bounds_1from_1@1ABOVEBELOW@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject java_le) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_@LCLASS@->bounds_from_@ABOVEBELOW@(le);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_@1MAXMIN@__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2
(JNIEnv* env, jobject j_this_@LCLASS@ , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  TEMP_INTEGER(coeff_num);
  TEMP_INTEGER(coeff_den);
  coeff_num = build_ppl_coeff(env, j_coeff_num);
  coeff_den = build_ppl_coeff(env, j_coeff_den);
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
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_@1MAXMIN@__Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2Lppl_1java_By_1Reference_2Lppl_1java_Generator_2
(JNIEnv* env, jobject j_this_@LCLASS@ , jobject j_le,
 jobject j_coeff_num, jobject j_coeff_den, jobject j_ref_boolean,
 jobject j_generator) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  TEMP_INTEGER(coeff_num);
  TEMP_INTEGER(coeff_den);
  coeff_num = build_ppl_coeff(env, j_coeff_num);
  coeff_den = build_ppl_coeff(env, j_coeff_den);
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
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_@1COMPARISON@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  jlong @LCLASS@_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(@LCLASS@_ptr);
  return this_@LCLASS@->@COMPARISON@(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_equals
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_@LCLASS@ ) {
  try {
  jlong ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(ptr);
  ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(ptr);
  return (*this_@LCLASS@)==(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_hashcode_code',
`dnl
JNIEXPORT jint JNICALL Java_ppl_1java_@1CLASS@_hashCode
(JNIEnv* env , jobject j_this_@LCLASS@) {
  try {
  jlong ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(ptr);
  return (*this_@LCLASS@).hash_code();
  }
  CATCH_ALL;
  return 0;
}

')


m4_define(`ppl_@CLASS@_OK_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_OK
(JNIEnv* env, jobject j_this_@LCLASS@) {
  try {
    jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
    @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    return this_@LCLASS@->OK();
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_add_1@1ADD_REPRESENT@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@) {
  try {
    jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
    @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    @UADD_REPRESENT@ c = build_ppl_@ADD_REPRESENT@(env, j_@ADD_REPRESENT@);
    this_@LCLASS@->add_@ADD_REPRESENT@(c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_add_1@1ADD_REPRESENT@_1and_1minimize
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@) {
  try {
    jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
    @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    @UADD_REPRESENT@ c = build_ppl_@ADD_REPRESENT@(env, j_@ADD_REPRESENT@);
    return this_@LCLASS@->add_@ADD_REPRESENT@_and_minimize(c);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_add_1@1ADD_REPRESENT@s
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@s) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @UADD_REPRESENT@_System cs = build_ppl_@ADD_REPRESENT@_system(env, j_@ADD_REPRESENT@s);
  this_@LCLASS@->add_@ADD_REPRESENT@s(cs);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_add_1@1ADD_REPRESENT@s_1and_1minimize
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@ADD_REPRESENT@s) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @UADD_REPRESENT@_System cs = build_ppl_@ADD_REPRESENT@_system(env, j_@ADD_REPRESENT@s);
  return this_@LCLASS@->add_@ADD_REPRESENT@s_and_minimize(cs);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1BINOP@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  jlong @LCLASS@_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(@LCLASS@_ptr);
  this_@LCLASS@->@BINOP@(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_@1BINMINOP@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  jlong @LCLASS@_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(@LCLASS@_ptr);
  return this_@LCLASS@->@BINMINOP@(*@LCLASS@);
  }
  CATCH_ALL;
  return false;
}

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1AFFIMAGE@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_var, jobject j_le,
 jobject j_coeff) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Variable v = build_ppl_variable(env, j_var);
  Linear_Expression le = build_linear_expression(env, j_le);
  TEMP_INTEGER(c);
  c = build_ppl_coeff(env, j_coeff);
  this_@LCLASS@->@AFFIMAGE@(v, le, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_generalized_1@1AFFIMAGE@__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 this_@LCLASS@->generalized_@AFFIMAGE@(lhs, relsym, rhs);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_generalized_1@1AFFIMAGE@__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 Variable v = build_ppl_variable(env, j_variable);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 TEMP_INTEGER(c);
 c = build_ppl_coeff(env, j_coeff);
 this_@LCLASS@->generalized_@AFFIMAGE@(v, relsym, le, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_1Grid_generalized_1@1AFFIMAGE@__Lppl_1java_Linear_1Expression_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_le_lhs, jobject j_relsym,
 jobject j_le_rhs, jobject j_modulus) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
 Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 TEMP_INTEGER(modulus);
 modulus = build_ppl_coeff(env, j_modulus);
 this_@LCLASS@->generalized_@AFFIMAGE@(lhs, relsym, rhs, modulus);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_Grid_generalized_1@1AFFIMAGE@__Lppl_1java_Variable_2Lppl_1java_Relation_1Symbol_2Lppl_1java_Linear_1Expression_2Lppl_1java_Coefficient_2Lppl_1java_Coefficient_2
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_relsym,
 jobject j_le , jobject j_coeff, jobject j_modulus) {
  try {
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 Variable v = build_ppl_variable(env, j_variable);
 Relation_Symbol relsym = build_ppl_relsym(env, j_relsym);
 Linear_Expression le = build_linear_expression(env, j_le);
 TEMP_INTEGER(c);
 TEMP_INTEGER(modulus);
 c = build_ppl_coeff(env, j_coeff);
 modulus = build_ppl_coeff(env, j_modulus);
 this_@LCLASS@->generalized_@AFFIMAGE@(v, relsym, le, c, modulus);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_bounded_1@1AFFIMAGE@
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jobject j_le_lhs, jobject j_le_rhs, jobject j_coeff) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Variable v = build_ppl_variable(env, j_variable);
  Linear_Expression lhs = build_linear_expression(env, j_le_lhs);
  Linear_Expression rhs = build_linear_expression(env, j_le_rhs);
  TEMP_INTEGER(c);
  c = build_ppl_coeff(env, j_coeff);
  this_@LCLASS@->bounded_@AFFIMAGE@(v, lhs, rhs, c);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1WIDEN@_1widening_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@,
 jobject j_by_ref_int) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  this_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  if (is_null(env, j_by_ref_int))
    this_@LCLASS@->@WIDEN@_widening_assign(*@LCLASS@);
  else {
    jobject j_integer = get_by_reference(env, j_by_ref_int);
    unsigned int tokens =
	 jtype_to_unsigned<unsigned int> (j_integer_to_j_int(env,
					   j_integer));
    this_@LCLASS@->@WIDEN@_widening_assign(*@LCLASS@, &tokens);
    j_integer = j_int_to_j_integer(env, tokens);
    set_by_reference(env, j_by_ref_int, j_integer);
   }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1EXTRAPOLATION@_1narrowing_1assign
(JNIEnv* env , jobject j_this_@LCLASS@ , jobject j_@LCLASS@) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  this_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    this_@LCLASS@->@EXTRAPOLATION@_narrowing_assign(*@LCLASS@);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_@1LIMITEDBOUNDED@_1@1WIDENEXPN@_1extrapolation_1assign
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_cs, jobject j_@LCLASS@,
 jobject j_by_ref_int) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  this_ptr = get_ptr(env, j_@LCLASS@);
  @CPP_CLASS@* @LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  @UCONSTRAINER@_System cs = build_ppl_@CONSTRAINER@_system(env, j_cs);
  if (is_null(env, j_by_ref_int))
    this_@LCLASS@->@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(*@LCLASS@, cs);
  else {
    jobject j_integer = get_by_reference(env, j_by_ref_int);
    unsigned int tokens =
	 jtype_to_unsigned<unsigned int> (j_integer_to_j_int(env,
					   j_integer));
    this_@LCLASS@->@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(*@LCLASS@, cs,
							 &tokens);
    j_integer = j_int_to_j_integer(env, tokens);
    set_by_reference(env, j_by_ref_int, j_integer);
   }
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_add_1space_1dimensions_1@1EMBEDPROJECT@
(JNIEnv* env, jobject j_this_@LCLASS@, jlong dim) {
  try {
dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
 jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
 @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
 this_@LCLASS@->add_space_dimensions_@EMBEDPROJECT@(ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_remove_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_v_set) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Variables_Set v_set = build_ppl_variables_set(env, j_v_set);
  this_@LCLASS@->remove_space_dimensions(v_set);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_remove_1higher_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jlong dim) {
  try {
  dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  this_@LCLASS@->remove_higher_space_dimensions(ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_expand_1space_1dimension
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_variable, jlong dim) {
  try {
  dimension_type ppl_dim = jtype_to_unsigned<dimension_type>(dim);
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Variable v = build_ppl_variable(env, j_variable);
  this_@LCLASS@->expand_space_dimension(v, ppl_dim);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_fold_1space_1dimensions
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_v_set, jobject j_var) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  Variables_Set v_set = build_ppl_variables_set(env, j_v_set);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  Variable v = build_ppl_variable(env, j_var);
  this_@LCLASS@->fold_space_dimensions(v_set, v);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
JNIEXPORT void JNICALL Java_ppl_1java_@1CLASS@_map_1space_1dimensions
(JNIEnv* env , jobject j_this_@LCLASS@, jobject j_p_func) {
  try {
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  PFunc ppl_pfunc = PFunc(j_p_func, env);
  this_@LCLASS@->map_space_dimensions(ppl_pfunc);
  }
  CATCH_ALL;
}

')

m4_define(`ppl_@CLASS@_string_code',
`dnl
JNIEXPORT jstring JNICALL Java_ppl_1java_@1CLASS@_toString
(JNIEnv* env , jobject j_this_@LCLASS@) {
  try {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  std::ostringstream s;
  s << *this_@LCLASS@;
  return env->NewStringUTF(s.str().c_str());
  }
  CATCH_ALL;
  return 0;
}
')

m4_define(`ppl_@CLASS@_total_memory_in_bytes_code',
`JNIEXPORT jlong JNICALL Java_ppl_1java_@1CLASS@_total_1memory_1in_1bytes
(JNIEnv* env, jobject j_pps) {
  jlong this_ptr = get_ptr(env, j_pps);
  @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
  return this_@LCLASS@->total_memory_in_bytes();
}

')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
JNIEXPORT jboolean JNICALL Java_ppl_1java_@1CLASS@_constrains
(JNIEnv* env, jobject j_this_@LCLASS@, jobject j_var) {
  try {
    jlong this_ptr = get_ptr(env, j_this_@LCLASS@);
    @CPP_CLASS@* this_@LCLASS@ = reinterpret_cast<@CPP_CLASS@*>(this_ptr);
    Variable v = build_ppl_variable(env, j_var);
    return this_@LCLASS@->constrains(v);
  }
  CATCH_ALL;
  return false;
}

')

