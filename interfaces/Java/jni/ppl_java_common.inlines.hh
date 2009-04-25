/* Domain-independent part of the Java interface: inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_ppl_java_common_inlines_hh
#define PPL_ppl_java_common_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Java {

template <typename U, typename V>
U
jtype_to_unsigned(const V& value) {
  if (value < 0)
    throw std::invalid_argument("not an unsigned integer.");

  if (sizeof(U) < sizeof(V)) {
    if (value > static_cast<V>(std::numeric_limits<U>::max()))
      throw std::invalid_argument("unsigned integer out of range.");
  }

  return value;
}

template <typename T>
void
set_ptr(JNIEnv* env, jobject ppl_object,
	const T* address, bool to_be_marked) {
  const T* ptr = (to_be_marked ? mark(address) : address);
  jlong pointer_value = reinterpret_cast<jlong>(ptr);
  assert(reinterpret_cast<const T*>(pointer_value) == ptr);
  env->SetLongField(ppl_object, cached_FMIDs.PPL_Object_ptr_ID, pointer_value);
}

inline void
set_coefficient(JNIEnv* env, jobject dst, jobject src) {
  jobject src_bi
    = env->GetObjectField(src, cached_FMIDs.Coefficient_value_ID);
  env->SetObjectField(dst, cached_FMIDs.Coefficient_value_ID, src_bi);
}

inline void
set_by_reference(JNIEnv* env, jobject by_ref_dst, jobject src) {
  env->SetObjectField(by_ref_dst,
                      cached_FMIDs.By_Reference_obj_ID,
                      src);
}

inline jobject
get_by_reference(JNIEnv* env, jobject by_reference) {
  return env->GetObjectField(by_reference, cached_FMIDs.By_Reference_obj_ID);
}

template <typename R>
jobject
build_linear_expression(JNIEnv* env, const R& r) {
  jobject j_ret;
  PPL_DIRTY_TEMP_COEFFICIENT(coefficient);
  dimension_type varid = 0;
  dimension_type space_dimension = r.space_dimension();
  while (varid < space_dimension
 	 && (coefficient = r.coefficient(Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    jclass j_le_coeff_class;
    PPL_JNI_FIND_CLASS(j_le_coeff_class, env, Linear_Expression_Coefficient,
                       "parma_polyhedra_library/Linear_Expression_Coefficient");
    jobject j_coefficient_zero = build_java_coeff(env, Coefficient(0));
    j_ret = env->NewObject(j_le_coeff_class,
                           cached_FMIDs.Linear_Expression_Coefficient_init_ID,
                           j_coefficient_zero);
    CHECK_RESULT_THROW(env, j_ret);
  }
  else {
    jclass j_le_times_class;
    PPL_JNI_FIND_CLASS(j_le_times_class, env, Linear_Expression_Times,
                       "parma_polyhedra_library/Linear_Expression_Times");
    jmethodID coeff_var_init_ID
      = cached_FMIDs.Linear_Expression_Times_init_from_coeff_var_ID;
    jobject j_coefficient = build_java_coeff(env, coefficient);
    jobject j_variable = build_java_variable(env, Variable(varid));
    jobject j_coeff_var = env->NewObject(j_le_times_class,
                                         coeff_var_init_ID,
                                         j_coefficient, j_variable);
    CHECK_EXCEPTION_THROW(env);
    j_ret = j_coeff_var;
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
 	j_coefficient = build_java_coeff(env, coefficient);
        j_variable = build_java_variable(env, Variable(varid));
        j_coeff_var = env->NewObject(j_le_times_class,
                                     coeff_var_init_ID,
                                     j_coefficient, j_variable);
	CHECK_EXCEPTION_THROW(env);
        j_ret = env->CallObjectMethod(j_ret,
                                      cached_FMIDs.Linear_Expression_sum_ID,
                                      j_coeff_var);
	CHECK_EXCEPTION_THROW(env);
      }
    }
  }
  return j_ret;
}

inline
Partial_Function::Partial_Function(jobject j_p_func, JNIEnv* env)
  : j_p_func(j_p_func),
    env(env) {
}

inline bool
Partial_Function::has_empty_codomain() const {
  jclass j_partial_function_class
    = env->FindClass("parma_polyhedra_library/Partial_Function");
  CHECK_RESULT_ASSERT(env, j_partial_function_class);
  jmethodID j_has_empty_codomain_id
    = env->GetMethodID(j_partial_function_class, "has_empty_codomain", "()Z");
  CHECK_RESULT_ASSERT(env, j_has_empty_codomain_id);
  bool ret = env->CallBooleanMethod(j_p_func, j_has_empty_codomain_id);
  CHECK_EXCEPTION_THROW(env);
  return ret;
}

inline dimension_type
Partial_Function::max_in_codomain() const {
  jclass j_partial_function_class
    = env->FindClass("parma_polyhedra_library/Partial_Function");
  CHECK_RESULT_ASSERT(env, j_partial_function_class);
  jmethodID j_max_in_codomain_id
    = env->GetMethodID(j_partial_function_class, "max_in_codomain", "()J");
  CHECK_RESULT_ASSERT(env, j_max_in_codomain_id);
  jlong value = env->CallLongMethod(j_p_func, j_max_in_codomain_id);
  CHECK_EXCEPTION_THROW(env);
  return jtype_to_unsigned<dimension_type>(value);
}

inline bool
Partial_Function::maps(dimension_type i, dimension_type& j) const {
  jclass j_partial_function_class
    = env->FindClass("parma_polyhedra_library/Partial_Function");
  CHECK_RESULT_ASSERT(env, j_partial_function_class);
  jclass j_by_reference_class;
  PPL_JNI_FIND_CLASS(j_by_reference_class, env, By_Reference,
                     "parma_polyhedra_library/By_Reference");
  jobject coeff = j_long_to_j_long_class(env, 0);
  jobject new_by_ref = env->NewObject(j_by_reference_class,
                                      cached_FMIDs.By_Reference_init_ID,
                                      coeff);
  CHECK_RESULT_THROW(env, new_by_ref);
  jmethodID j_maps_id
    = env->GetMethodID(j_partial_function_class, "maps",
                       "(Ljava/lang/Long;Lparma_polyhedra_library/By_Reference;)Z");
  CHECK_RESULT_ASSERT(env, j_maps_id);
  jboolean b = env->CallBooleanMethod(j_p_func, j_maps_id,
				      j_long_to_j_long_class(env, i),
				      new_by_ref);
  CHECK_EXCEPTION_THROW(env);
  if (b) {
    jobject long_value = get_by_reference(env, new_by_ref);
    j = jtype_to_unsigned<dimension_type>(j_long_class_to_j_long(env,
                                                                 long_value));
    return true;
  }
  return false;
}

} // namespace Java

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_ppl_java_common_inlines_hh)
