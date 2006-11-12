/* PPL Java interface common routines implementation.
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

#include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;

jobject
ppl_poly_gen_relation_to_j_poly_gen_relation(JNIEnv* env,
					     Poly_Gen_Relation& r) {
  jclass j_poly_gen_relation_class
    = env->FindClass("ppl_java/Poly_Gen_Relation");
  jmethodID j_poly_gen_relation_ctr_id
    = env->GetMethodID(j_poly_gen_relation_class, "<init>", "(I)V");

  jint j_value = 0;
  while (r != Poly_Gen_Relation::nothing()) {
    if (r.implies(Poly_Gen_Relation::subsumes())) {
      j_value += 1;
      r = r - Poly_Gen_Relation::subsumes();
    }
  }
  return env->NewObject(j_poly_gen_relation_class,
			j_poly_gen_relation_ctr_id, j_value);
}

jobject
ppl_poly_con_relation_to_j_poly_con_relation(JNIEnv* env,
					     Poly_Con_Relation& r) {
  jclass j_poly_con_relation_class
    = env->FindClass("ppl_java/Poly_Con_Relation");
  jmethodID j_poly_con_relation_ctr_id
    = env->GetMethodID(j_poly_con_relation_class, "<init>", "(I)V");

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
  return env->NewObject(j_poly_con_relation_class,
			j_poly_con_relation_ctr_id, j_value);
}


Parma_Polyhedra_Library::Congruence
build_ppl_congruence(JNIEnv* env, const jobject& j_congruence) {
  jclass congruence_class
    = env->FindClass("ppl_java/Congruence");
  jfieldID modulus_field_id = env->GetFieldID(congruence_class,
					      "modulus",
					      "Lppl_java/Coefficient;");
  jobject j_modulus = env->GetObjectField(j_congruence, modulus_field_id);
  Coefficient ppl_modulus = j_coeff_to_ppl_coeff(env, j_modulus);
  jfieldID lhs_field_id
      = env->GetFieldID(congruence_class,
 			"lhs",
 			"Lppl_java/Linear_Expression;");
    jfieldID rhs_field_id
      = env->GetFieldID(congruence_class,
 			"rhs",
 			"Lppl_java/Linear_Expression;");
    jobject j_lhs = env->GetObjectField(j_congruence, lhs_field_id);
    jobject j_rhs = env->GetObjectField(j_congruence, rhs_field_id);
    Linear_Expression lhs = build_linear_expression(env, j_lhs);
    Linear_Expression rhs = build_linear_expression(env, j_rhs);
    return (lhs %= rhs) / ppl_modulus;
}

// Converts a C++ bool to a Java boolean.

// jobject
// bool_to_j_boolean(JNIEnv* env,
// 		  const bool bool_value) {
//  jclass boolean_java_class = env->FindClass("java/lang/Boolean");
//  jmethodID getboolean_method_id = env->GetStaticMethodID(boolean_java_class,
// 							 "valueOf", "(Z)java/lang/Boolean;");
//  return env->CallStaticObjectMethod(boolean_java_class,
// 				    getboolean_method_id,
// 				    bool_value);
//}

// bool
// j_boolean_to_bool(JNIEnv* env,
// 		  const jobject& j_boolean) {
//   jclass boolean_class = env->GetObjectClass(j_boolean);
//   jmethodID booleanvalue_method_id = env->GetMethodID(boolean_class,
// 						      "booleanValue",
// 						      "()Z");
//   return env->CallBooleanMethod(j_boolean, booleanvalue_method_id);

// }

Variables_Set
j_variables_set_to_ppl_variables_set(JNIEnv* env,
				     const jobject& j_v_set) {
  jclass variables_set_class = env->GetObjectClass(j_v_set);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  Variables_Set v_set;
  jmethodID iterator_method_id = env->GetMethodID(variables_set_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  jobject j_iterator = env->CallObjectMethod(j_v_set, iterator_method_id);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");

  while (has_next_value) {
    jobject j_variable = env->CallObjectMethod(j_iterator,
					       next_method_id);
    v_set.insert(j_variable_to_ppl_variable(env, j_variable));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
  }
  return v_set;
}

Variable
j_variable_to_ppl_variable(JNIEnv* env, const jobject& j_var) {
  jclass j_variable_class = env->FindClass("ppl_java/Variable");
  jfieldID varid_field_id = env->GetFieldID(j_variable_class,
					  "varid",
					  "I");
  // Retrieve the value.
  return Variable(env->GetIntField(j_var, varid_field_id));
}

Relation_Symbol
j_relsym_to_ppl_relsym(JNIEnv* env, const jobject& j_relsym) {

  jclass rel_sym_class = env->FindClass("ppl_java/Relation_Symbol");
  jmethodID rel_sym_ordinal_id = env->GetMethodID(rel_sym_class, "ordinal",
						  "()I");
  jint rel_sym = env->CallIntMethod(j_relsym, rel_sym_ordinal_id);
  switch (rel_sym) {
  case 0: {
    if (rel_sym == 0)
      return LESS_THAN;
  }
  case 1: {
    if (rel_sym == 1)
      return LESS_THAN_OR_EQUAL;
  }
 case 2: {
    if (rel_sym == 2)
      return EQUAL;
 }
  case 3: {
    if (rel_sym == 3)
      return GREATER_THAN_OR_EQUAL;
  }
  case 4: {
    if (rel_sym == 4)
      return GREATER_THAN;
  }
  default:
    ;
  }
  jclass newExcCls = env->FindClass("javax/management/RuntimeErrorException");
  env->ThrowNew(newExcCls, "ppl.java: \n runtime error");
  // We should not be here!
  throw std::runtime_error("PPL Java interface internal error");
}

Coefficient
j_coeff_to_ppl_coeff(JNIEnv* env, const jobject& j_coeff) {
  jclass j_coeff_class = env->GetObjectClass(j_coeff);
  jfieldID fid = env->GetFieldID(j_coeff_class, "value",
				 "Ljava/math/BigInteger;");
  jobject bi = env->GetObjectField(j_coeff, fid);
  jclass big_integer_class = env->GetObjectClass(bi);
  jmethodID bi_to_string = env->GetMethodID(big_integer_class, "toString",
					    "()Ljava/lang/String;");
  jstring bi_string = (jstring) env->CallObjectMethod(bi, bi_to_string);
  const char *nativeString = env->GetStringUTFChars(bi_string, 0);
  Coefficient ppl_coeff = Coefficient(nativeString);
  env->ReleaseStringUTFChars(bi_string, nativeString);
  return ppl_coeff;
}

// Converts a PPL coefficient to a Java coefficient.
jobject
ppl_coeff_to_j_coeff(JNIEnv* env, const Coefficient& ppl_coeff) {
  std::ostringstream s;
  s << ppl_coeff;
  jclass j_coefficient_class = env->FindClass("ppl_java/Coefficient");
  jmethodID j_coefficient_ctr_id = env->GetMethodID(j_coefficient_class,
						    "<init>",
						    "(Ljava/lang/String;)V");
  jstring coeff_string = env->NewStringUTF(s.str().c_str());
  return env->NewObject(j_coefficient_class, j_coefficient_ctr_id,
			coeff_string);
}

Constraint
build_ppl_constraint(JNIEnv* env, const jobject& j_constraint) {
  jclass constraint_class = env->FindClass("ppl_java/Constraint");
  jclass rel_sym_class = env->FindClass("ppl_java/Relation_Symbol");
  jfieldID lhs_field_id = env->GetFieldID(constraint_class, "lhs",
					  "Lppl_java/Linear_Expression;");
  jfieldID rhs_field_id = env->GetFieldID(constraint_class, "rhs",
					  "Lppl_java/Linear_Expression;");
  jobject lhs_value = env->GetObjectField(j_constraint, lhs_field_id);
  jobject rhs_value = env->GetObjectField(j_constraint, rhs_field_id);
  Linear_Expression first_le = build_linear_expression(env, lhs_value);
  Linear_Expression second_le = build_linear_expression(env, rhs_value);
  jfieldID kind_field_id = env->GetFieldID(constraint_class, "kind",
					   "Lppl_java/Relation_Symbol;");
  jobject kind = env->GetObjectField(j_constraint, kind_field_id);
  jmethodID rel_sym_ordinal_id = env->GetMethodID(rel_sym_class, "ordinal",
						  "()I");
  jint rel_sym = env->CallIntMethod(kind, rel_sym_ordinal_id);
  switch (rel_sym) {
  case 0:
    return Constraint(first_le < second_le);
  case 1:
    return Constraint(first_le <= second_le);
  case 2:
    return Constraint(first_le == second_le);
  case 3:
  if (rel_sym == 3)
    return Constraint(first_le >= second_le);
  case 4:
    return Constraint(first_le > second_le);
  default:
    ;
  }
  jclass newExcCls = env->FindClass("javax/management/RuntimeErrorException");
  env->ThrowNew(newExcCls, "ppl.java: \n runtime error");
  // We should not be here!
  throw std::runtime_error("PPL Java interface internal error");
}


Linear_Expression
build_linear_expression(JNIEnv* env, const jobject& j_le) {
  jclass le_sum_class = env->FindClass("ppl_java/Linear_Expression_Sum");
  jclass le_difference_class
    = env->FindClass("ppl_java/Linear_Expression_Difference");
  jclass le_times_class
    = env->FindClass("ppl_java/Linear_Expression_Times");
  jclass le_unary_minus_class
    = env->FindClass("ppl_java/Linear_Expression_Unary_Minus");
  jclass j_coeff_le_class
    = env->FindClass("ppl_java/Linear_Expression_Coefficient");
  jclass j_variable_le_class
    = env->FindClass("ppl_java/Linear_Expression_Variable");
  jclass j_variable_class = env->FindClass("ppl_java/Variable");

  jclass current_class = env->GetObjectClass(j_le);
  // Variable
  if (env->IsAssignableFrom(j_variable_le_class, current_class)) {
    jfieldID arg_field_id = env->GetFieldID(j_variable_le_class,
					    "arg",
					    "Lppl_java/Variable;");
    jobject var = env->GetObjectField(j_le, arg_field_id);
    jfieldID varid_field_id = env->GetFieldID(j_variable_class,
					      "varid",
					      "I");

    // Retrieve the value.
    jint varid = env->GetIntField(var, varid_field_id);
    return Linear_Expression(Variable(varid));
  }
  // Coefficient
  if (env->IsAssignableFrom(j_coeff_le_class, current_class)) {
    jfieldID coeff_field_id = env->GetFieldID(j_coeff_le_class,
					      "coeff",
					      "Lppl_java/Coefficient;");
    jobject ppl_coeff = env->GetObjectField(j_le, coeff_field_id);

    return Linear_Expression(j_coeff_to_ppl_coeff(env, ppl_coeff));
  }
  // Sum
  if (env->IsAssignableFrom(le_sum_class, current_class)) {
    jfieldID l_field_id = env->GetFieldID(current_class, "lhs",
					  "Lppl_java/Linear_Expression;");
    jfieldID r_field_id = env->GetFieldID(current_class, "rhs",
					  "Lppl_java/Linear_Expression;");
    jobject l_value = env->GetObjectField(j_le, l_field_id);
    jobject r_value = env->GetObjectField(j_le, r_field_id);
    return (build_linear_expression(env, l_value)
	    + build_linear_expression(env, r_value));
  }
  // Difference
  if (env->IsAssignableFrom(current_class, le_difference_class)) {
    jfieldID l_field_id = env->GetFieldID(current_class, "lhs",
					  "Lppl_java/Linear_Expression;");
    jfieldID r_field_id = env->GetFieldID(current_class, "rhs",
					  "Lppl_java/Linear_Expression;");
    jobject l_value = env->GetObjectField(j_le, l_field_id);
    jobject r_value = env->GetObjectField(j_le, r_field_id);
    return (build_linear_expression(env, l_value)
	    - build_linear_expression(env, r_value));
  }
  // Times
  if (env->IsAssignableFrom(le_times_class, current_class)) {
    jfieldID le_field_id = env->GetFieldID(current_class, "rhs",
					   "Lppl_java/Linear_Expression;");
    jfieldID le_coeff_field_id
      = env->GetFieldID(current_class, "lhs",
			"Lppl_java/Linear_Expression_Coefficient;");
    jobject le_value = env->GetObjectField(j_le, le_field_id);
    jobject le_coeff_value = env->GetObjectField(j_le, le_coeff_field_id);
    jfieldID coeff_field_id = env->GetFieldID(j_coeff_le_class,
					      "coeff",
					      "Lppl_java/Coefficient;");
    jobject ppl_coeff = env->GetObjectField(le_coeff_value, coeff_field_id);
    return (j_coeff_to_ppl_coeff(env, ppl_coeff)
	    * build_linear_expression(env, le_value));
  }
  // Unary_Minus
  if (env->IsAssignableFrom(current_class, le_unary_minus_class)) {
    jfieldID le_field_id = env->GetFieldID(current_class, "arg",
					   "Lppl_java/Linear_Expression;");
    jobject le_value = env->GetObjectField(j_le, le_field_id);
    return (-build_linear_expression(env, le_value));
  }
  // We should not be here!
  throw std::runtime_error("PPL Java interface internal error");
}

Generator
build_generator(JNIEnv* env, const jobject& j_generator) {
  jclass generator_class = env->FindClass("ppl_java/Generator");
  jclass generator_type_class = env->FindClass("ppl_java/Generator_Type");

  jfieldID j_le_field = env->GetFieldID(generator_class, "le",
					"Lppl_java/Linear_Expression;");
  jobject j_le = env->GetObjectField(j_generator, j_le_field);
  jfieldID j_coeff_field = env->GetFieldID(generator_class, "den",
					   "Lppl_java/Coefficient;");
  jobject j_coeff = env->GetObjectField(j_generator, j_coeff_field);

  jfieldID generator_type_field = env->GetFieldID(generator_class, "gt",
						  "Lppl_java/Generator_Type;");
  jobject generator_type = env->GetObjectField(j_generator,
					       generator_type_field);
  jmethodID generator_type_ordinal_id = env->GetMethodID(generator_type_class,
							 "ordinal",
							 "()I");
  jint generator_type_ordinal = env->CallIntMethod(generator_type,
						   generator_type_ordinal_id);
  switch (generator_type_ordinal) {
  case 0:
    return line(build_linear_expression(env, j_le));
  case 1:
    return ray(build_linear_expression(env, j_le));
  case 2:
    return point(build_linear_expression(env, j_le),
		 j_coeff_to_ppl_coeff(env, j_coeff));
  case 3:
    return closure_point(build_linear_expression(env, j_le),
			 j_coeff_to_ppl_coeff(env, j_coeff));
  default:
    ;
  }
  jclass newExcCls = env->FindClass("javax/management/RuntimeErrorException");
  env->ThrowNew(newExcCls, "ppl.java: \n runtime error");
  // We should not be here!
  throw std::runtime_error("PPL Java interface internal error");
}

jlong
get_ptr(JNIEnv* env, const jobject& ppl_object) {
  jclass ppl_object_class = env->GetObjectClass(ppl_object);
  jfieldID pointer_field = env->GetFieldID(ppl_object_class, "ptr","J");
  return  env->GetLongField(ppl_object, pointer_field);
}

Constraint_System
build_ppl_constraint_system(JNIEnv* env, const jobject& j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  Constraint_System cs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");

  while (has_next_value) {
    jobject j_constraint = env->CallObjectMethod(j_iterator,
						 next_method_id);
    cs.insert(build_ppl_constraint(env, j_constraint));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
  }
 return cs;
}

Generator_System
build_ppl_generator_system(JNIEnv* env, const jobject& j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  Generator_System gs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");

  while (has_next_value) {
    jobject j_constraint = env->CallObjectMethod(j_iterator,
						 next_method_id);
    gs.insert(build_generator(env, j_constraint));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
  }
  return gs;
}

Congruence_System
build_ppl_congruence_system(JNIEnv* env, const jobject& j_iterable) {
  jclass j_iterable_class = env->GetObjectClass(j_iterable);
  jclass iterator_java_class = env->FindClass("java/util/Iterator");
  Congruence_System cgs;
  jmethodID iterator_method_id = env->GetMethodID(j_iterable_class,
						  "iterator",
 						  "()Ljava/util/Iterator;");
  jobject j_iterator = env->CallObjectMethod(j_iterable, iterator_method_id);
  jmethodID has_next_method_id = env->GetMethodID(iterator_java_class,
  						  "hasNext",
  						  "()Z");
  jboolean has_next_value = env->CallBooleanMethod(j_iterator,
						   has_next_method_id);
  jmethodID next_method_id = env->GetMethodID(iterator_java_class,
					      "next",
					      "()Ljava/lang/Object;");

  while (has_next_value) {
    jobject j_congruence = env->CallObjectMethod(j_iterator,
						 next_method_id);
    cgs.insert(build_ppl_congruence(env, j_congruence));
    has_next_value = env->CallBooleanMethod(j_iterator,
					    has_next_method_id);
  }
  return cgs;
}

jobject
get_le_inhomogeneous_term(JNIEnv* env, const Coefficient& c) {
  jclass j_le_coeff_class
    = env->FindClass("ppl_java/Linear_Expression_Coefficient");
  jobject j_coeff =  ppl_coeff_to_j_coeff(env, c);
  jmethodID j_le_coeff_ctr_id
    = env->GetMethodID(j_le_coeff_class,
		       "<init>",
		       "(Lppl_java/Coefficient;)V");
  return env->NewObject(j_le_coeff_class, j_le_coeff_ctr_id,
			j_coeff);
}

jobject
build_j_constraint(JNIEnv* env, const Constraint& c) {
  jclass j_constraint_class = env->FindClass("ppl_java/Constraint");
  jclass j_rel_sym_class = env->FindClass("ppl_java/Relation_Symbol");
  jmethodID j_constraint_ctr_id
    = env->GetMethodID(j_constraint_class,
		       "<init>",
		       "(Lppl_java/Linear_Expression;"
		       "Lppl_java/Linear_Expression;"
		       "Lppl_java/Relation_Symbol;)V");
  jfieldID rel_sym_eq_get_id
    = env->GetStaticFieldID(j_rel_sym_class,
			    "EQUAL",
			    "Lppl_java/Relation_Symbol;");
   jfieldID rel_sym_gt_get_id
     = env->GetStaticFieldID(j_rel_sym_class,
 			    "GREATER_THAN",
 			    "Lppl_java/Relation_Symbol;");
   jfieldID rel_sym_gtoeq_get_id
     = env->GetStaticFieldID(j_rel_sym_class,
 			    "GREATER_THAN_OR_EQUAL",
 			    "Lppl_java/Relation_Symbol;");
   jobject lhs = get_linear_expression(env, c);
   jobject rhs = get_le_inhomogeneous_term(env, -c.inhomogeneous_term());
   jobject relation;
   switch (c.type()) {
  case Constraint::EQUALITY: {
    relation = env->GetStaticObjectField(j_rel_sym_class, rel_sym_eq_get_id);
    break;
  }
  case Constraint::NONSTRICT_INEQUALITY:
    relation = env->GetStaticObjectField(j_rel_sym_class, rel_sym_gtoeq_get_id);
    break;
  case Constraint::STRICT_INEQUALITY:
    relation = env->GetStaticObjectField(j_rel_sym_class, rel_sym_gt_get_id);
    break;
  default:
      throw std::runtime_error("PPL Java interface internal error");
  }
   return env->NewObject(j_constraint_class,j_constraint_ctr_id,
			 lhs, rhs,
			 relation);
}

jobject
build_j_congruence(JNIEnv* env, const Congruence& cg) {
  jclass j_congruence_class = env->FindClass("ppl_java/Congruence");
  jmethodID j_congruence_ctr_id
    = env->GetMethodID(j_congruence_class,
		       "<init>",
		       "(Lppl_java/Linear_Expression;"
		       "Lppl_java/Linear_Expression;"
		       "Lppl_java/Coefficient;)V");

  jobject j_modulus = ppl_coeff_to_j_coeff(env, cg.modulus());
  jobject lhs = get_linear_expression(env, cg);
  jobject rhs = get_le_inhomogeneous_term(env, -cg.inhomogeneous_term());
  return env->NewObject(j_congruence_class, j_congruence_ctr_id,
			lhs, rhs,
			j_modulus);

}

jobject
build_j_generator(JNIEnv* env, const Generator& g) {
  jclass j_generator_class = env->FindClass("ppl_java/Generator");
  jclass j_gen_type_class = env->FindClass("ppl_java/Generator_Type");
  jmethodID line_ctr_id =
    env->GetStaticMethodID(j_generator_class,
			   "line",
			   "(Lppl_java/Linear_Expression;)"
			   "Lppl_java/Generator;");
  jmethodID ray_ctr_id =
    env->GetStaticMethodID(j_generator_class,
			   "ray",
			   "(Lppl_java/Linear_Expression;)"
			   "Lppl_java/Generator;");
 jmethodID point_ctr_id =
    env->GetStaticMethodID(j_generator_class,
			   "point",
			   "(Lppl_java/Linear_Expression;"
			   "Lppl_java/Coefficient;)"
			   "Lppl_java/Generator;");
 jmethodID closure_point_ctr_id =
   env->GetStaticMethodID(j_generator_class,
			  "closure_point",
			  "(Lppl_java/Linear_Expression;"
			  "Lppl_java/Coefficient;)"
			  "Lppl_java/Generator;");

 jfieldID gen_type_line_get_id
    = env->GetStaticFieldID(j_gen_type_class,
			    "LINE",
			    "Lppl_java/Generator_Type;");
   jfieldID gen_type_ray_get_id
     = env->GetStaticFieldID(j_gen_type_class,
 			    "RAY",
 			    "Lppl_java/Generator_Type;");
   jfieldID gen_type_point_get_id
     = env->GetStaticFieldID(j_gen_type_class,
 			    "POINT",
 			    "Lppl_java/Generator_Type;");
   jfieldID gen_type_closure_point_get_id
     = env->GetStaticFieldID(j_gen_type_class,
			     "CLOSURE_POINT",
			     "Lppl_java/Generator_Type;");
   jobject j_g_type;
   jobject j_g_le = get_linear_expression(env, g);
   jobject jcoeff = ppl_coeff_to_j_coeff(env, Coefficient(1));
  switch (g.type()) {
  case Generator::LINE:
    j_g_type
      = env->GetStaticObjectField(j_gen_type_class, gen_type_line_get_id);
    return env->CallStaticObjectMethod(j_generator_class,
				       line_ctr_id, j_g_le);
    break;
  case Generator::RAY:
    j_g_type
      = env->GetStaticObjectField(j_gen_type_class, gen_type_ray_get_id);
    return env->CallStaticObjectMethod(j_generator_class,
				       ray_ctr_id, j_g_le);
    break;
  case Generator::POINT:
    {
      j_g_type
	= env->GetStaticObjectField(j_gen_type_class, gen_type_point_get_id);
      const Coefficient& divisor = g.divisor();
      j_g_le = get_linear_expression(env, g);
      jcoeff = ppl_coeff_to_j_coeff(env, divisor);
      return env->CallStaticObjectMethod(j_generator_class,
					 point_ctr_id, j_g_le, jcoeff);
    }
  case Generator::CLOSURE_POINT:
    {
      j_g_type = env->GetStaticObjectField(j_gen_type_class,
					  gen_type_closure_point_get_id);
      const Coefficient& divisor = g.divisor();
      j_g_le = get_linear_expression(env, g);
      jcoeff = ppl_coeff_to_j_coeff(env, divisor);
      return env->CallStaticObjectMethod(j_generator_class,
					 closure_point_ctr_id, j_g_le, jcoeff);
    }
  default:
    throw std::runtime_error("PPL Java interface internal error");
  }
}

jobject
build_j_constraint_system(JNIEnv* env, const Constraint_System& cs) {
  jclass j_cs_class = env->FindClass("ppl_java/Constraint_System");
  jmethodID j_cs_ctr_id = env->GetMethodID(j_cs_class, "<init>", "()V");
  jmethodID j_cs_add_id = env->GetMethodID(j_cs_class, "add",
					   "(Ljava/lang/Object;)Z");
   jobject j_cs = env->NewObject(j_cs_class, j_cs_ctr_id);
   for (Constraint_System::const_iterator v_begin = cs.begin(),
 	 v_end = cs.end(); v_begin != v_end; ++v_begin) {
     jobject j_constraint = build_j_constraint(env, *v_begin);
     env->CallBooleanMethod(j_cs, j_cs_add_id, j_constraint);
   }
   return j_cs;
}

jobject
build_j_generator_system(JNIEnv* env, const Generator_System& gs) {
  jclass j_gs_class = env->FindClass("ppl_java/Generator_System");
  jmethodID j_gs_ctr_id = env->GetMethodID(j_gs_class, "<init>", "()V");
  jmethodID j_gs_add_id = env->GetMethodID(j_gs_class, "add",
					   "(Ljava/lang/Object;)Z");
   jobject j_gs = env->NewObject(j_gs_class, j_gs_ctr_id);
   for (Generator_System::const_iterator v_begin = gs.begin(),
 	 v_end = gs.end(); v_begin != v_end; ++v_begin) {
     jobject j_generator = build_j_generator(env, *v_begin);
     env->CallBooleanMethod(j_gs, j_gs_add_id, j_generator);
   }
   return j_gs;
}

jobject
build_j_congruence_system(JNIEnv* env, const Congruence_System& cgs) {
   jclass j_cgs_class = env->FindClass("ppl_java/Congruence_System");
   jmethodID j_cgs_ctr_id = env->GetMethodID(j_cgs_class, "<init>", "()V");
   jmethodID j_cgs_add_id = env->GetMethodID(j_cgs_class, "add",
 					   "(Ljava/lang/Object;)Z");
    jobject j_cgs = env->NewObject(j_cgs_class, j_cgs_ctr_id);
    for (Congruence_System::const_iterator v_begin = cgs.begin(),
  	 v_end = cgs.end(); v_begin != v_end; ++v_begin) {
      jobject j_congruence = build_j_congruence(env,*v_begin);
      env->CallBooleanMethod(j_cgs, j_cgs_add_id, j_congruence);
    }
    return j_cgs;
}
