/* PPL Java interface common routines implementation.
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

#include "ppl_java_common.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Java {

// Define field/method ID cache.
Java_FMID_Cache cached_FMIDs;

void
handle_exception(JNIEnv* env, const std::overflow_error& e) {
  jclass newExcCls
    = env->FindClass("parma_polyhedra_library/Overflow_Error_Exception");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::invalid_argument& e) {
  jclass newExcCls
    = env->FindClass("parma_polyhedra_library/Invalid_Argument_Exception");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::logic_error& e) {
  jclass newExcCls
    = env->FindClass("parma_polyhedra_library/Logic_Error_Exception");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::length_error& e) {
  jclass newExcCls
    = env->FindClass("parma_polyhedra_library/Length_Error_Exception");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::domain_error& e) {
  jclass newExcCls
    = env->FindClass("parma_polyhedra_library/Domain_Error_Exception");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::bad_alloc&) {
  jclass newExcCls
    = env->FindClass("java/lang/RuntimeException");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, "Out of memory");
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env, const std::exception& e) {
  jclass newExcCls = env->FindClass("java/lang/RuntimeException");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, e.what());
  CHECK_RESULT_ABORT(env, ret);
}

void
handle_exception(JNIEnv* env) {
  jclass newExcCls = env->FindClass("java/lang/RuntimeException");
  CHECK_RESULT_ASSERT(env, newExcCls);
  jint ret = env->ThrowNew(newExcCls, "PPL bug: unknown exception raised");
  CHECK_RESULT_ABORT(env, ret);
}

jobject
build_java_poly_gen_relation(JNIEnv* env, Poly_Gen_Relation& r) {
  jint j_value = 0;
  while (r != Poly_Gen_Relation::nothing()) {
    if (r.implies(Poly_Gen_Relation::subsumes())) {
      j_value += 1;
      r = r - Poly_Gen_Relation::subsumes();
    }
  }
  jclass j_poly_gen_relation_class
    = env->FindClass("parma_polyhedra_library/Poly_Gen_Relation");
  CHECK_RESULT_ASSERT(env, j_poly_gen_relation_class);
  jobject ret = env->NewObject(j_poly_gen_relation_class,
			       cached_FMIDs.Poly_Gen_Relation_init_ID,
                               j_value);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

jobject
build_java_poly_con_relation(JNIEnv* env, Poly_Con_Relation& r) {
  jint j_value = 0;
  while (r != Poly_Con_Relation::nothing()) {
    if (r.implies(Poly_Con_Relation::is_disjoint())) {
      j_value += 1;
      r = r - Poly_Con_Relation::is_disjoint();
    }
    else if (r.implies(Poly_Con_Relation::strictly_intersects())) {
      j_value += 2;
      r = r - Poly_Con_Relation::strictly_intersects();
    }
    else if (r.implies(Poly_Con_Relation::is_included())) {
      j_value += 4;
      r = r - Poly_Con_Relation::is_included();
    }
    else if (r.implies(Poly_Con_Relation::saturates())) {
      j_value += 8;
      r = r - Poly_Con_Relation::saturates();
    }
  }
  jclass j_poly_con_relation_class
    = env->FindClass("parma_polyhedra_library/Poly_Con_Relation");
  CHECK_RESULT_ASSERT(env, j_poly_con_relation_class);
  jobject ret = env->NewObject(j_poly_con_relation_class,
			       cached_FMIDs.Poly_Con_Relation_init_ID,
                               j_value);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}


Congruence
build_cxx_congruence(JNIEnv* env, jobject j_congruence) {
  jobject j_modulus
    = env->GetObjectField(j_congruence, cached_FMIDs.Congruence_modulus_ID);
  jobject j_lhs
    = env->GetObjectField(j_congruence, cached_FMIDs.Congruence_lhs_ID);
  jobject j_rhs
    = env->GetObjectField(j_congruence, cached_FMIDs.Congruence_rhs_ID);
  PPL_DIRTY_TEMP_COEFFICIENT(ppl_modulus);
  ppl_modulus = build_cxx_coeff(env, j_modulus);
  Linear_Expression lhs = build_cxx_linear_expression(env, j_lhs);
  Linear_Expression rhs = build_cxx_linear_expression(env, j_rhs);
  return (lhs %= rhs) / ppl_modulus;
}

jobject
bool_to_j_boolean(JNIEnv* env, const bool value) {
  // FIXME: make sure class Coefficient is loaded.
  jclass Boolean_class = env->FindClass("java/lang/Boolean");
  CHECK_RESULT_ASSERT(env, Boolean_class);
  jobject ret = env->CallStaticObjectMethod(Boolean_class,
					    cached_FMIDs.Boolean_valueOf_ID,
					    value);
  CHECK_EXCEPTION_ASSERT(env);
  return ret;
}

jobject
j_long_to_j_long_class(JNIEnv* env, jlong jlong_value) {
  // FIXME: make sure class Coefficient is loaded.
  jclass Long_class = env->FindClass("java/lang/Long");
  CHECK_RESULT_ASSERT(env, Long_class);
  jobject ret = env->CallStaticObjectMethod(Long_class,
					    cached_FMIDs.Long_valueOf_ID,
					    jlong_value);
  CHECK_EXCEPTION_THROW(env);
  return ret;
}

jlong
j_long_class_to_j_long(JNIEnv* env, jobject j_long) {
  // FIXME: make sure class Coefficient is loaded.
  jlong ret = env->CallLongMethod(j_long, cached_FMIDs.Long_longValue_ID);
  CHECK_EXCEPTION_ASSERT(env);
  return ret;
}

jobject
j_int_to_j_integer(JNIEnv* env, jint jint_value) {
  // FIXME: make sure class Coefficient is loaded.
  jclass Integer_class = env->FindClass("java/lang/Integer");
  CHECK_RESULT_ASSERT(env, Integer_class);
  jobject ret = env->CallStaticObjectMethod(Integer_class,
					    cached_FMIDs.Integer_valueOf_ID,
					    jint_value);
  CHECK_EXCEPTION_THROW(env);
  return ret;
}

jint
j_integer_to_j_int(JNIEnv* env, jobject j_integer) {
  // FIXME: make sure class Coefficient is loaded.
  jint ret = env->CallIntMethod(j_integer, cached_FMIDs.Integer_intValue_ID);
  CHECK_EXCEPTION_ASSERT(env);
  return ret;
}

Variables_Set
build_cxx_variables_set(JNIEnv* env, jobject j_v_set) {
  jclass variables_set_class = env->GetObjectClass(j_v_set);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  CHECK_RESULT_ASSERT(env, iterator_java_class);
  Variables_Set v_set;
  jmethodID iterator_method_id = env->GetMethodID(variables_set_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  CHECK_RESULT_ASSERT(env, iterator_method_id);
  jobject j_iterator = env->CallObjectMethod(j_v_set, iterator_method_id);
  CHECK_EXCEPTION_THROW(env);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  CHECK_RESULT_ASSERT(env, has_next_method_id);
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  CHECK_EXCEPTION_ASSERT(env);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");
  CHECK_RESULT_ASSERT(env, next_method_id);

  while (has_next_value) {
    jobject j_variable = env->CallObjectMethod(j_iterator,
					       next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
    v_set.insert(build_cxx_variable(env, j_variable));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
  }
  return v_set;
}

jobject
build_java_variables_set(JNIEnv* env, const Variables_Set& v_set) {
  jclass j_vs_class = env->FindClass("parma_polyhedra_library/Variables_Set");
  CHECK_RESULT_ASSERT(env, j_vs_class);
  jmethodID j_vs_ctr_id = env->GetMethodID(j_vs_class, "<init>", "()V");
  CHECK_RESULT_ASSERT(env, j_vs_ctr_id);
  jmethodID j_vs_add_id = env->GetMethodID(j_vs_class, "add",
					   "(Ljava/lang/Object;)Z");
  CHECK_RESULT_ASSERT(env, j_vs_add_id);
  jobject j_vs = env->NewObject(j_vs_class, j_vs_ctr_id);
  CHECK_RESULT_THROW(env, j_vs);
  for (Variables_Set::const_iterator v_begin = v_set.begin(),
	 v_end = v_set.end(); v_begin != v_end; ++v_begin) {
    Variable var(*v_begin);
    jobject j_variable = build_java_variable(env, var);
    env->CallBooleanMethod(j_vs, j_vs_add_id, j_variable);
    CHECK_EXCEPTION_THROW(env);
  }
  return j_vs;
}

Variable
build_cxx_variable(JNIEnv* env, jobject j_var) {
  return Variable(env->GetIntField(j_var, cached_FMIDs.Variable_varid_ID));
}

jobject
build_java_variable(JNIEnv* env, const Variable& var) {
  jclass variable_class = env->FindClass("parma_polyhedra_library/Variable");
  CHECK_RESULT_ASSERT(env, variable_class);
  jobject ret = env->NewObject(variable_class,
                               cached_FMIDs.Variable_init_ID,
			       var.id());
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

Relation_Symbol
build_cxx_relsym(JNIEnv* env, jobject j_relsym) {
  jint rel_sym
    = env->CallIntMethod(j_relsym, cached_FMIDs.Relation_Symbol_ordinal_ID);
  CHECK_EXCEPTION_ASSERT(env);
  switch (rel_sym) {
  case 0:
    return LESS_THAN;
  case 1:
    return LESS_OR_EQUAL;
  case 2:
    return EQUAL;
  case 3:
    return GREATER_OR_EQUAL;
  case 4:
    return GREATER_THAN;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

Optimization_Mode
build_cxx_optimization_mode(JNIEnv* env, jobject j_opt_mode) {
  jint opt_mode
    = env->CallIntMethod(j_opt_mode, cached_FMIDs.Optimization_Mode_ordinal_ID);
  CHECK_EXCEPTION_ASSERT(env);
  switch (opt_mode) {
  case 0:
    return MINIMIZATION;
  case 1:
    return MAXIMIZATION;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

jobject
build_java_mip_status(JNIEnv* env, const MIP_Problem_Status& mip_status) {
  jclass j_mip_problem_status_class
    = env->FindClass("parma_polyhedra_library/MIP_Problem_Status");
  CHECK_RESULT_ASSERT(env, j_mip_problem_status_class);
  jfieldID fID;
  switch (mip_status) {
  case UNFEASIBLE_MIP_PROBLEM:
    fID = cached_FMIDs.MIP_Problem_Status_UNFEASIBLE_MIP_PROBLEM_ID;
    break;
  case UNBOUNDED_MIP_PROBLEM:
    fID = cached_FMIDs.MIP_Problem_Status_UNBOUNDED_MIP_PROBLEM_ID;
    break;
  case OPTIMIZED_MIP_PROBLEM:
    fID = cached_FMIDs.MIP_Problem_Status_OPTIMIZED_MIP_PROBLEM_ID;
    break;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  return env->GetStaticObjectField(j_mip_problem_status_class, fID);
}

jobject
build_java_optimization_mode(JNIEnv* env, const Optimization_Mode& opt_mode) {
  jclass j_optimization_mode_class
    = env->FindClass("parma_polyhedra_library/Optimization_Mode");
  CHECK_RESULT_ASSERT(env, j_optimization_mode_class);
  jfieldID fID;
  switch (opt_mode) {
  case MINIMIZATION:
    fID = cached_FMIDs.Optimization_Mode_MINIMIZATION_ID;
    break;
  case MAXIMIZATION:
    fID = cached_FMIDs.Optimization_Mode_MAXIMIZATION_ID;
    break;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  return env->GetStaticObjectField(j_optimization_mode_class, fID);
}

MIP_Problem::Control_Parameter_Name
build_cxx_control_parameter_name(JNIEnv* env, jobject j_cp_name) {
  jclass cp_name_class = env->GetObjectClass(j_cp_name);
  CHECK_RESULT_ASSERT(env, cp_name_class);
  jmethodID cp_name_ordinal_id
    = env->GetMethodID(cp_name_class, "ordinal", "()I");
  CHECK_RESULT_ASSERT(env, cp_name_ordinal_id);
  jint cp_name = env->CallIntMethod(j_cp_name, cp_name_ordinal_id);
  CHECK_EXCEPTION_ASSERT(env);
  switch (cp_name) {
  case 0:
    return MIP_Problem::PRICING;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

jobject
build_java_control_parameter_name
(JNIEnv* env,const MIP_Problem::Control_Parameter_Name& cp_name) {
  jclass j_cp_name_class
    = env->FindClass("parma_polyhedra_library/Control_Parameter_Name");
  CHECK_RESULT_ASSERT(env, j_cp_name_class);
  jfieldID cp_name_pricing_get_id
    = env->GetStaticFieldID(j_cp_name_class, "PRICING",
			    "Lparma_polyhedra_library/Control_Parameter_Name;");
  CHECK_RESULT_ASSERT(env, cp_name_pricing_get_id);
  switch (cp_name) {
  case MIP_Problem::PRICING:
    return env->GetStaticObjectField(j_cp_name_class,
				     cp_name_pricing_get_id);
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

MIP_Problem::Control_Parameter_Value
build_cxx_control_parameter_value(JNIEnv* env, jobject j_cp_value) {
  jclass cp_value_class = env->GetObjectClass(j_cp_value);
  CHECK_RESULT_ASSERT(env, cp_value_class);
  jmethodID cp_value_ordinal_id
    = env->GetMethodID(cp_value_class, "ordinal", "()I");
  CHECK_RESULT_ASSERT(env, cp_value_ordinal_id);
  jint cp_value = env->CallIntMethod(j_cp_value, cp_value_ordinal_id);
  CHECK_EXCEPTION_ASSERT(env);
  switch (cp_value) {
  case 0:
    return MIP_Problem::PRICING_STEEPEST_EDGE_FLOAT;
  case 1:
    return MIP_Problem::PRICING_STEEPEST_EDGE_EXACT;
  case 2:
    return MIP_Problem::PRICING_TEXTBOOK;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

jobject
build_java_control_parameter_value
(JNIEnv* env, const MIP_Problem::Control_Parameter_Value& cp_value) {
  jclass j_cp_value_class
    = env->FindClass("parma_polyhedra_library/Control_Parameter_Value");
  CHECK_RESULT_ASSERT(env, j_cp_value_class);
  const char* field_name;
  switch (cp_value) {
  case MIP_Problem::PRICING_STEEPEST_EDGE_FLOAT:
    field_name = "PRICING_STEEPEST_EDGE_FLOAT";
    break;
  case MIP_Problem::PRICING_STEEPEST_EDGE_EXACT:
    field_name = "PRICING_STEEPEST_EDGE_EXACT";
    break;
  case MIP_Problem::PRICING_TEXTBOOK:
    field_name = "PRICING_TEXTBOOK";
    break;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  jfieldID fID = env->GetStaticFieldID(j_cp_value_class, field_name,
                                       "Lparma_polyhedra_library/Control_Parameter_Value;");
  CHECK_RESULT_ASSERT(env, fID);
  return env->GetStaticObjectField(j_cp_value_class, fID);
}

Coefficient
build_cxx_coeff(JNIEnv* env, jobject j_coeff) {
  jstring bi_string
    = (jstring) env->CallObjectMethod(j_coeff,
                                      cached_FMIDs.Coefficient_toString_ID);
  CHECK_EXCEPTION_THROW(env);
  const char *nativeString = env->GetStringUTFChars(bi_string, 0);
  CHECK_RESULT_THROW(env, nativeString);
  PPL_DIRTY_TEMP_COEFFICIENT(ppl_coeff);
  ppl_coeff = Coefficient(nativeString);
  env->ReleaseStringUTFChars(bi_string, nativeString);
  return ppl_coeff;
}

jobject
build_java_coeff(JNIEnv* env, const Coefficient& ppl_coeff) {
  std::ostringstream s;
  s << ppl_coeff;
  std::string str = s.str();
  jstring coeff_string = env->NewStringUTF(str.c_str());
  CHECK_RESULT_THROW(env, coeff_string);
  jclass j_coeff_class =
    env->FindClass("parma_polyhedra_library/Coefficient");
  CHECK_RESULT_ASSERT(env, j_coeff_class);
  jobject ret = env->NewObject(j_coeff_class,
                               cached_FMIDs.Coefficient_init_from_String_ID,
                               coeff_string);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

Constraint
build_cxx_constraint(JNIEnv* env, jobject j_constraint) {
  jobject lhs_value
    = env->GetObjectField(j_constraint, cached_FMIDs.Constraint_lhs_ID);
  jobject rhs_value
    = env->GetObjectField(j_constraint, cached_FMIDs.Constraint_rhs_ID);
  jobject kind
    = env->GetObjectField(j_constraint, cached_FMIDs.Constraint_kind_ID);
  Linear_Expression first_le = build_cxx_linear_expression(env, lhs_value);
  Linear_Expression second_le = build_cxx_linear_expression(env, rhs_value);
  jint rel_sym
    = env->CallIntMethod(kind, cached_FMIDs.Relation_Symbol_ordinal_ID);
  CHECK_EXCEPTION_ASSERT(env);
  switch (rel_sym) {
  case 0:
    return Constraint(first_le < second_le);
  case 1:
    return Constraint(first_le <= second_le);
  case 2:
    return Constraint(first_le == second_le);
  case 3:
    return Constraint(first_le >= second_le);
  case 4:
    return Constraint(first_le > second_le);
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

Linear_Expression
build_cxx_linear_expression(JNIEnv* env, jobject j_le) {
  jclass le_sum_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Sum");
  CHECK_RESULT_ASSERT(env, le_sum_class);
  jclass le_diff_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Difference");
  CHECK_RESULT_ASSERT(env, le_diff_class);
  jclass le_times_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Times");
  CHECK_RESULT_ASSERT(env, le_times_class);
  jclass le_uminus_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Unary_Minus");
  CHECK_RESULT_ASSERT(env, le_uminus_class);
  jclass le_coeff_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Coefficient");
  CHECK_RESULT_ASSERT(env, le_coeff_class);
  jclass le_var_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Variable");
  CHECK_RESULT_ASSERT(env, le_var_class);

  jclass current_class = env->GetObjectClass(j_le);
  // Variable
  if (env->IsAssignableFrom(current_class, le_var_class)) {
    jint var_id
      = env->CallIntMethod(j_le,
                           cached_FMIDs.Linear_Expression_Variable_var_id_ID);
    return Linear_Expression(Variable(var_id));
  }
  // Coefficient
  if (env->IsAssignableFrom(current_class, le_coeff_class)) {
    jfieldID fID = cached_FMIDs.Linear_Expression_Coefficient_coeff_ID;
    jobject ppl_coeff = env->GetObjectField(j_le, fID);
    return Linear_Expression(build_cxx_coeff(env, ppl_coeff));
  }
  // Sum
  if (env->IsAssignableFrom(current_class, le_sum_class)) {
    jobject l_value
      = env->GetObjectField(j_le, cached_FMIDs.Linear_Expression_Sum_lhs_ID);
    jobject r_value
      = env->GetObjectField(j_le, cached_FMIDs.Linear_Expression_Sum_rhs_ID);
    return (build_cxx_linear_expression(env, l_value)
	    + build_cxx_linear_expression(env, r_value));
  }
  // Difference
  if (env->IsAssignableFrom(current_class, le_diff_class)) {
    jobject l_value
      = env->GetObjectField(j_le,
                            cached_FMIDs.Linear_Expression_Difference_lhs_ID);
    jobject r_value
      = env->GetObjectField(j_le,
                            cached_FMIDs.Linear_Expression_Difference_rhs_ID);
    return (build_cxx_linear_expression(env, l_value)
	    - build_cxx_linear_expression(env, r_value));
  }
  // Times
  if (env->IsAssignableFrom(current_class, le_times_class)) {
    jobject le_coeff_value
      = env->GetObjectField(j_le,
                            cached_FMIDs.Linear_Expression_Times_lhs_ID);
    jobject ppl_coeff
      = env->GetObjectField(le_coeff_value,
                            cached_FMIDs.Linear_Expression_Coefficient_coeff_ID);
    jobject le_value
      = env->GetObjectField(j_le,
                            cached_FMIDs.Linear_Expression_Times_rhs_ID);
    return (build_cxx_coeff(env, ppl_coeff)
	    * build_cxx_linear_expression(env, le_value));
  }
  // Unary_Minus
  if (env->IsAssignableFrom(current_class, le_uminus_class)) {
    jobject le_value
      = env->GetObjectField(j_le,
                            cached_FMIDs.Linear_Expression_Unary_Minus_arg_ID);
    return (-build_cxx_linear_expression(env, le_value));
  }
  assert(false);
  throw std::runtime_error("PPL Java interface internal error");
}

Generator
build_cxx_generator(JNIEnv* env, jobject j_generator) {
  jobject j_le
    = env->GetObjectField(j_generator, cached_FMIDs.Generator_le_ID);
  jobject generator_type
    = env->GetObjectField(j_generator, cached_FMIDs.Generator_gt_ID);
  jint generator_type_ordinal
    = env->CallIntMethod(generator_type,
                         cached_FMIDs.Generator_Type_ordinal_ID);
  CHECK_EXCEPTION_ASSERT(env);
  switch (generator_type_ordinal) {
  case 0:
    return line(build_cxx_linear_expression(env, j_le));
  case 1:
    return ray(build_cxx_linear_expression(env, j_le));
  case 2:
    {
      jobject j_div
        = env->GetObjectField(j_generator, cached_FMIDs.Generator_div_ID);
      return point(build_cxx_linear_expression(env, j_le),
                   build_cxx_coeff(env, j_div));
    }
  case 3:
    {
      jobject j_div
        = env->GetObjectField(j_generator, cached_FMIDs.Generator_div_ID);
      return closure_point(build_cxx_linear_expression(env, j_le),
                           build_cxx_coeff(env, j_div));
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

Grid_Generator
build_cxx_grid_generator(JNIEnv* env, jobject j_grid_generator) {
  jobject j_le
    = env->GetObjectField(j_grid_generator, cached_FMIDs.Grid_Generator_le_ID);
  jobject grid_generator_type
    = env->GetObjectField(j_grid_generator, cached_FMIDs.Grid_Generator_gt_ID);
  jint grid_generator_type_ordinal
    = env->CallIntMethod(grid_generator_type,
                         cached_FMIDs.Grid_Generator_Type_ordinal_ID);
  CHECK_EXCEPTION_ASSERT(env);
  switch (grid_generator_type_ordinal) {
  case 0:
    return grid_line(build_cxx_linear_expression(env, j_le));
  case 1:
    {
      jobject j_div = env->GetObjectField(j_grid_generator,
                                          cached_FMIDs.Grid_Generator_div_ID);
      return parameter(build_cxx_linear_expression(env, j_le),
                       build_cxx_coeff(env, j_div));
    }
  case 2:
    {
      jobject j_div = env->GetObjectField(j_grid_generator,
                                          cached_FMIDs.Grid_Generator_div_ID);
      return grid_point(build_cxx_linear_expression(env, j_le),
                        build_cxx_coeff(env, j_div));
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
}

void*
get_ptr(JNIEnv* env, jobject ppl_object) {
  jlong pointer_value
    = env->GetLongField(ppl_object, cached_FMIDs.PPL_Object_ptr_ID);
  void* ptr = reinterpret_cast<void*>(pointer_value);
  assert(reinterpret_cast<jlong>(ptr) == pointer_value);
  return unmark(ptr);
}

bool
is_java_marked(JNIEnv* env, jobject ppl_object) {
  jlong pointer_value
    = env->GetLongField(ppl_object, cached_FMIDs.PPL_Object_ptr_ID);
  const void* ptr = reinterpret_cast<const void*>(pointer_value);
  assert(reinterpret_cast<jlong>(ptr) == pointer_value);
  return marked(ptr);
}

Grid_Generator_System
build_cxx_grid_generator_system(JNIEnv* env, jobject j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  CHECK_RESULT_ASSERT(env, iterator_java_class);
  Grid_Generator_System ggs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  CHECK_RESULT_ASSERT(env, iterator_method_id);
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  CHECK_EXCEPTION_THROW(env);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  CHECK_RESULT_ASSERT(env, has_next_method_id);
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  CHECK_EXCEPTION_ASSERT(env);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");
  CHECK_RESULT_ASSERT(env, next_method_id);

  while (has_next_value) {
    jobject j_grid_generator = env->CallObjectMethod(j_iterator,
                                                     next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
    ggs.insert(build_cxx_grid_generator(env, j_grid_generator));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
  }
  return ggs;
}

Constraint_System
build_cxx_constraint_system(JNIEnv* env, jobject j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  CHECK_RESULT_ASSERT(env, iterator_java_class);
  Constraint_System cs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  CHECK_RESULT_ASSERT(env, iterator_method_id);
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  CHECK_EXCEPTION_THROW(env);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  CHECK_RESULT_ASSERT(env, has_next_method_id);
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  CHECK_EXCEPTION_ASSERT(env);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");
  CHECK_RESULT_ASSERT(env, next_method_id);

  while (has_next_value) {
    jobject j_constraint = env->CallObjectMethod(j_iterator,
						 next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
    cs.insert(build_cxx_constraint(env, j_constraint));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
  }
  return cs;
}

Generator_System
build_cxx_generator_system(JNIEnv* env, jobject j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  CHECK_RESULT_ASSERT(env, iterator_java_class);
  Generator_System gs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  CHECK_RESULT_ASSERT(env, iterator_method_id);
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  CHECK_EXCEPTION_THROW(env);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  CHECK_RESULT_ASSERT(env, has_next_method_id);
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  CHECK_EXCEPTION_ASSERT(env);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");
  CHECK_RESULT_ASSERT(env, next_method_id);

  while (has_next_value) {
    jobject j_constraint = env->CallObjectMethod(j_iterator,
						 next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
    gs.insert(build_cxx_generator(env, j_constraint));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
  }
  return gs;
}

Congruence_System
build_cxx_congruence_system(JNIEnv* env, jobject j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  CHECK_RESULT_ASSERT(env, iterator_java_class);
  Congruence_System cgs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  CHECK_RESULT_ASSERT(env, iterator_method_id);
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  CHECK_EXCEPTION_THROW(env);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  CHECK_RESULT_ASSERT(env, has_next_method_id);
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  CHECK_EXCEPTION_ASSERT(env);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");
  CHECK_RESULT_ASSERT(env, next_method_id);

  while (has_next_value) {
    jobject j_congruence = env->CallObjectMethod(j_iterator,
						 next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
    cgs.insert(build_cxx_congruence(env, j_congruence));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
    CHECK_EXCEPTION_ASSERT(env);
  }
  return cgs;
}

jobject
build_java_linear_expression_coefficient(JNIEnv* env, const Coefficient& c) {
  jclass j_le_coeff_class
    = env->FindClass("parma_polyhedra_library/Linear_Expression_Coefficient");
  CHECK_RESULT_ASSERT(env, j_le_coeff_class);
  jobject j_coeff =  build_java_coeff(env, c);
  jobject ret
    = env->NewObject(j_le_coeff_class,
                     cached_FMIDs.Linear_Expression_Coefficient_init_ID,
                     j_coeff);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

void
set_generator(JNIEnv* env, jobject& to_be_set, jobject gen) {
  jclass j_generator_class
    = env->FindClass("parma_polyhedra_library/Generator");
  CHECK_RESULT_ASSERT(env, j_generator_class);
  jmethodID j_coeff_set_id
    = env->GetMethodID(j_generator_class, "set",
                       "(Lparma_polyhedra_library/Generator;)V");
  CHECK_RESULT_ASSERT(env, j_coeff_set_id);
  env->CallVoidMethod(to_be_set, j_coeff_set_id, gen);
  CHECK_EXCEPTION_ASSERT(env); // CHECK ME: an exception is better here?
}

void
set_coefficient(JNIEnv* env, jobject& to_be_set, jobject c) {
  jclass j_coeff_class
    = env->FindClass("parma_polyhedra_library/Coefficient");
  CHECK_RESULT_ASSERT(env, j_coeff_class);
  jmethodID j_coeff_set_id
    = env->GetMethodID(j_coeff_class, "set",
                       "(Lparma_polyhedra_library/Coefficient;)V");
  CHECK_RESULT_ASSERT(env, j_coeff_set_id);
  env->CallVoidMethod(to_be_set, j_coeff_set_id, c);
  CHECK_EXCEPTION_ASSERT(env);
}

void
set_by_reference(JNIEnv* env, jobject& by_ref_to_be_set, jobject to_insert) {
  env->SetObjectField(by_ref_to_be_set,
                      cached_FMIDs.By_Reference_obj_ID,
                      to_insert);
}

jobject
get_by_reference(JNIEnv* env, jobject by_reference) {
  return env->GetObjectField(by_reference, cached_FMIDs.By_Reference_obj_ID);
}

void
set_pair_element(JNIEnv* env, jobject& pair_to_be_set,
                 int arg, jobject to_insert) {
  jclass pair_class = env->FindClass("parma_polyhedra_library/Pair");
  CHECK_RESULT_ASSERT(env, pair_class);
  switch (arg) {
  case 0:
    {
      jfieldID obj_field_id = env->GetFieldID(pair_class,
                                              "first",
                                              "Ljava/lang/Object;");
      CHECK_RESULT_ASSERT(env, obj_field_id);
      env->SetObjectField(pair_to_be_set, obj_field_id, to_insert);
      return;
    }
  case 1:
    {
      jfieldID obj_field_id = env->GetFieldID(pair_class,
                                              "second",
                                              "Ljava/lang/Object;");
      CHECK_RESULT_ASSERT(env, obj_field_id);
      env->SetObjectField(pair_to_be_set, obj_field_id, to_insert);
      return;
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error: pair value"
			     " not allowed");
  }
}


jobject
get_pair_element(JNIEnv* env, int arg, jobject j_pair) {
  jclass pair_class = env->FindClass("parma_polyhedra_library/Pair");
  CHECK_RESULT_ASSERT(env, pair_class);
  switch (arg) {
  case 0:
    {
      jfieldID obj_field_id = env->GetFieldID(pair_class,
                                              "first",
                                              "Ljava/lang/Object;");
      CHECK_RESULT_ASSERT(env, obj_field_id);
      return env->GetObjectField(j_pair, obj_field_id);
    }
  case 1:
    {
      jfieldID obj_field_id = env->GetFieldID(pair_class,
                                              "second",
                                              "Ljava/lang/Object;");
      CHECK_RESULT_ASSERT(env, obj_field_id);
      return env->GetObjectField(j_pair, obj_field_id);
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error: pair value"
			     " not allowed");
  }
}

jobject
build_java_constraint(JNIEnv* env, const Constraint& c) {
  jobject lhs = build_linear_expression(env, c);
  jobject rhs
    = build_java_linear_expression_coefficient(env, -c.inhomogeneous_term());
  jfieldID fID;
  switch (c.type()) {
  case Constraint::EQUALITY:
    fID = cached_FMIDs.Relation_Symbol_EQUAL_ID;
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    fID = cached_FMIDs.Relation_Symbol_GREATER_OR_EQUAL_ID;
    break;
  case Constraint::STRICT_INEQUALITY:
    fID = cached_FMIDs.Relation_Symbol_GREATER_THAN_ID;
    break;
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  jclass j_rel_sym_class
    = env->FindClass("parma_polyhedra_library/Relation_Symbol");
  CHECK_RESULT_ASSERT(env, j_rel_sym_class);
  jobject relation = env->GetStaticObjectField(j_rel_sym_class, fID);
  jclass j_constraint_class
    = env->FindClass("parma_polyhedra_library/Constraint");
  CHECK_RESULT_ASSERT(env, j_constraint_class);
  jobject ret = env->NewObject(j_constraint_class,
                               cached_FMIDs.Constraint_init_ID,
			       lhs, relation, rhs);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

jobject
build_java_congruence(JNIEnv* env, const Congruence& cg) {
  jobject j_modulus = build_java_coeff(env, cg.modulus());
  jobject j_lhs = build_linear_expression(env, cg);
  jobject j_rhs
    = build_java_linear_expression_coefficient(env, -cg.inhomogeneous_term());
  jclass j_congruence_class
    = env->FindClass("parma_polyhedra_library/Congruence");
  CHECK_RESULT_ASSERT(env, j_congruence_class);
  jobject ret = env->NewObject(j_congruence_class,
                               cached_FMIDs.Congruence_init_ID,
			       j_lhs, j_rhs, j_modulus);
  CHECK_RESULT_THROW(env, ret);
  return ret;
}

jobject
build_java_generator(JNIEnv* env, const Generator& g) {
  jclass j_generator_class
    = env->FindClass("parma_polyhedra_library/Generator");
  CHECK_RESULT_ASSERT(env, j_generator_class);
  jmethodID line_ctr_id
    = env->GetStaticMethodID(j_generator_class,
                             "line",
                             "(Lparma_polyhedra_library/Linear_Expression;)"
                             "Lparma_polyhedra_library/Generator;");
  CHECK_RESULT_ASSERT(env, line_ctr_id);
  jmethodID ray_ctr_id
    = env->GetStaticMethodID(j_generator_class,
                             "ray",
                             "(Lparma_polyhedra_library/Linear_Expression;)"
                             "Lparma_polyhedra_library/Generator;");
  CHECK_RESULT_ASSERT(env, ray_ctr_id);
  jmethodID point_ctr_id
    = env->GetStaticMethodID(j_generator_class,
                             "point",
                             "(Lparma_polyhedra_library/Linear_Expression;"
                             "Lparma_polyhedra_library/Coefficient;)"
                             "Lparma_polyhedra_library/Generator;");
  CHECK_RESULT_ASSERT(env, point_ctr_id);
  jmethodID closure_point_ctr_id
    = env->GetStaticMethodID(j_generator_class,
                             "closure_point",
                             "(Lparma_polyhedra_library/Linear_Expression;"
                             "Lparma_polyhedra_library/Coefficient;)"
                             "Lparma_polyhedra_library/Generator;");
  CHECK_RESULT_ASSERT(env, closure_point_ctr_id);
  jobject j_g_le = build_linear_expression(env, g);
  jobject jcoeff = build_java_coeff(env, Coefficient(1));
  jobject ret;
  switch (g.type()) {
  case Generator::LINE:
    ret = env->CallStaticObjectMethod(j_generator_class,
				      line_ctr_id, j_g_le);
    break;
  case Generator::RAY:
    ret = env->CallStaticObjectMethod(j_generator_class,
				      ray_ctr_id, j_g_le);
    break;
  case Generator::POINT:
    {
      const Coefficient& divisor = g.divisor();
      j_g_le = build_linear_expression(env, g);
      jcoeff = build_java_coeff(env, divisor);
      ret = env->CallStaticObjectMethod(j_generator_class,
					point_ctr_id, j_g_le, jcoeff);
      break;
    }
  case Generator::CLOSURE_POINT:
    {
      const Coefficient& divisor = g.divisor();
      j_g_le = build_linear_expression(env, g);
      jcoeff = build_java_coeff(env, divisor);
      ret = env->CallStaticObjectMethod(j_generator_class,
					closure_point_ctr_id, j_g_le, jcoeff);
      break;
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  CHECK_EXCEPTION_THROW(env);
  return ret;
}

jobject
build_java_grid_generator(JNIEnv* env, const Grid_Generator& g) {
  jclass j_grid_generator_class
    = env->FindClass("parma_polyhedra_library/Grid_Generator");
  CHECK_RESULT_ASSERT(env, j_grid_generator_class);
  jmethodID line_ctr_id
    = env->GetStaticMethodID(j_grid_generator_class,
                             "grid_line",
                             "(Lparma_polyhedra_library/Linear_Expression;)"
                             "Lparma_polyhedra_library/Grid_Generator;");
  CHECK_RESULT_ASSERT(env, line_ctr_id);
  jmethodID parameter_ctr_id
    = env->GetStaticMethodID(j_grid_generator_class,
                             "parameter",
                             "(Lparma_polyhedra_library/Linear_Expression;"
                             "Lparma_polyhedra_library/Coefficient;)"
                             "Lparma_polyhedra_library/Grid_Generator;");
  CHECK_RESULT_ASSERT(env, parameter_ctr_id);
  jmethodID point_ctr_id
    = env->GetStaticMethodID(j_grid_generator_class,
                             "grid_point",
                             "(Lparma_polyhedra_library/Linear_Expression;"
                             "Lparma_polyhedra_library/Coefficient;)"
                             "Lparma_polyhedra_library/Grid_Generator;");
  CHECK_RESULT_ASSERT(env, point_ctr_id);
  jobject j_g_le = build_linear_expression(env, g);
  jobject jcoeff = build_java_coeff(env, Coefficient(1));
  jobject ret;
  switch (g.type()) {
  case Grid_Generator::LINE:
    ret = env->CallStaticObjectMethod(j_grid_generator_class,
				      line_ctr_id, j_g_le);
    break;
  case Grid_Generator::PARAMETER:
    {
      const Coefficient& divisor = g.divisor();
      j_g_le = build_linear_expression(env, g);
      jcoeff = build_java_coeff(env, divisor);
      ret = env->CallStaticObjectMethod(j_grid_generator_class,
					parameter_ctr_id, j_g_le, jcoeff);
      break;
    }
  case Grid_Generator::POINT:
    {
      const Coefficient& divisor = g.divisor();
      j_g_le = build_linear_expression(env, g);
      jcoeff = build_java_coeff(env, divisor);
      ret = env->CallStaticObjectMethod(j_grid_generator_class,
					point_ctr_id, j_g_le, jcoeff);
      break;
    }
  default:
    assert(false);
    throw std::runtime_error("PPL Java interface internal error");
  }
  CHECK_EXCEPTION_THROW(env);
  return ret;
}

jobject
build_java_constraint_system(JNIEnv* env, const Constraint_System& cs) {
  jclass j_cs_class
    = env->FindClass("parma_polyhedra_library/Constraint_System");
  CHECK_RESULT_ASSERT(env, j_cs_class);
  jobject j_cs = env->NewObject(j_cs_class,
                                cached_FMIDs.Constraint_System_init_ID);
  CHECK_RESULT_THROW(env, j_cs);
  for (Constraint_System::const_iterator v_begin = cs.begin(),
 	 v_end = cs.end(); v_begin != v_end; ++v_begin) {
    jobject j_constraint = build_java_constraint(env, *v_begin);
    env->CallBooleanMethod(j_cs,
                           cached_FMIDs.Constraint_System_add_ID,
                           j_constraint);
    CHECK_EXCEPTION_THROW(env);
  }
  return j_cs;
}

jobject
build_java_generator_system(JNIEnv* env, const Generator_System& gs) {
  jclass j_gs_class
    = env->FindClass("parma_polyhedra_library/Generator_System");
  CHECK_RESULT_ASSERT(env, j_gs_class);
  jobject j_gs = env->NewObject(j_gs_class,
                                cached_FMIDs.Generator_System_init_ID);
  CHECK_RESULT_THROW(env, j_gs);
  for (Generator_System::const_iterator v_begin = gs.begin(),
 	 v_end = gs.end(); v_begin != v_end; ++v_begin) {
    jobject j_generator = build_java_generator(env, *v_begin);
    env->CallBooleanMethod(j_gs,
                           cached_FMIDs.Generator_System_add_ID,
                           j_generator);
    CHECK_EXCEPTION_THROW(env);
  }
  return j_gs;
}

jobject
build_java_grid_generator_system(JNIEnv* env,
				 const Grid_Generator_System& gs) {
  jclass j_gs_class
    = env->FindClass("parma_polyhedra_library/Grid_Generator_System");
  CHECK_RESULT_ASSERT(env, j_gs_class);
  jobject j_gs = env->NewObject(j_gs_class,
                                cached_FMIDs.Grid_Generator_System_init_ID);
  CHECK_RESULT_THROW(env, j_gs);
  for (Grid_Generator_System::const_iterator v_begin = gs.begin(),
 	 v_end = gs.end(); v_begin != v_end; ++v_begin) {
    jobject j_generator = build_java_grid_generator(env, *v_begin);
    env->CallBooleanMethod(j_gs,
                           cached_FMIDs.Grid_Generator_System_add_ID,
                           j_generator);
    CHECK_EXCEPTION_THROW(env);
  }
  return j_gs;
}

jobject
build_java_congruence_system(JNIEnv* env, const Congruence_System& cgs) {
  jclass j_cgs_class
    = env->FindClass("parma_polyhedra_library/Congruence_System");
  CHECK_RESULT_ASSERT(env, j_cgs_class);
  jobject j_cgs = env->NewObject(j_cgs_class,
                                 cached_FMIDs.Congruence_System_init_ID);
  CHECK_RESULT_THROW(env, j_cgs);
  for (Congruence_System::const_iterator v_begin = cgs.begin(),
  	 v_end = cgs.end(); v_begin != v_end; ++v_begin) {
    jobject j_congruence = build_java_congruence(env,*v_begin);
    env->CallBooleanMethod(j_cgs,
                           cached_FMIDs.Congruence_System_add_ID,
                           j_congruence);
    CHECK_EXCEPTION_THROW(env);
  }
  return j_cgs;
}

} // namespace Java

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library
