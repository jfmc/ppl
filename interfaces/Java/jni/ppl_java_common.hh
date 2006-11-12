/* PPL Java interface common routines declaration.
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

#include <jni.h>
#include <ppl.hh>

using namespace Parma_Polyhedra_Library;

// // Converts a C++ bool to a Java boolean.
// jobject
// bool_to_j_boolean(JNIEnv* env,
// 		  const bool bool_value);

// // Converts a Java boolean set to a C++ bool.
// bool
// j_boolean_to_bool(JNIEnv* env,
// 		  const jobject& j_boolean);



// Converts a PPL Poly_Con_Relation to a Java Poly_Con_Relation.
jobject
ppl_poly_con_relation_to_j_poly_con_relation(JNIEnv* env,
					     Poly_Con_Relation& pcr);

// Converts a Java variables set to a PPL variables set.
Parma_Polyhedra_Library::Variables_Set
j_variables_set_to_ppl_variables_set(JNIEnv* env,
				     const jobject& variables_set);

// Converts a Java relation symbol to a PPL relation_symbol.
Parma_Polyhedra_Library::Relation_Symbol
j_relsym_to_ppl_relsym(JNIEnv* env, const jobject& j_relsym);

// Converts a Java variable to a PPL variable.
Parma_Polyhedra_Library::Variable
j_variable_to_ppl_variable(JNIEnv* env, const jobject& j_var);

// Converts a Java coefficient to a PPL coefficient.
Parma_Polyhedra_Library::Coefficient
j_coeff_to_ppl_coeff(JNIEnv* env, const jobject& j_coeff);

// Converts a PPL coefficient to a Java coefficient.
jobject
ppl_coeff_to_j_coeff(JNIEnv* env,
		     const Parma_Polyhedra_Library::Coefficient& ppl_coeff);

// Builds a PPL constraint from a Java constraint.
Parma_Polyhedra_Library::Constraint
build_ppl_constraint(JNIEnv* env, const jobject& j_constraint);

// Builds a PPL linear expression from a Java linear expression.
Parma_Polyhedra_Library::Linear_Expression
build_linear_expression(JNIEnv* env, const jobject& j_le);

// Builds a PPL congruence from a Java congruence.
Parma_Polyhedra_Library::Congruence
build_ppl_congruence(JNIEnv* env, const jobject& j_cg);

// Builds a PPL genearator from a Java generator.
Parma_Polyhedra_Library::Generator
build_generator(JNIEnv* env, const jobject& j_g);

// Get a pointer to the underlined C++ object from a Java object.
jlong
get_ptr(JNIEnv* env, const jobject& ppl_object);

// Builds a PPL constraint system from a Java constraint system.
Parma_Polyhedra_Library::Constraint_System
build_ppl_constraint_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL generator system from a Java generator system.
Parma_Polyhedra_Library::Generator_System
build_ppl_generator_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL congruence system from a Java congruence system.
Parma_Polyhedra_Library::Congruence_System
build_ppl_congruence_system(JNIEnv* env, const jobject& j_iterable);

// Builds a Java constraint from a PPL constraint.
jobject
build_j_constraint(JNIEnv* env, const Constraint& c);

// Builds a Java congruence from a PPL congruence.
jobject
build_j_congruence(JNIEnv* env, const Congruence& cg);

// Builds a Java generator from a PPL generator.
jobject
build_j_generator(JNIEnv* env, const Generator& cg);

// Builds a Java constraint system from a PPL constraint system.
jobject
build_j_constraint_system(JNIEnv* env, const Constraint_System& cs);

// Builds a Java generator system from a PPL generator system.
jobject
build_j_generator_system(JNIEnv* env, const Generator_System& gs);

// Builds a Java congrunce system from a PPL congruence system.
jobject
build_j_congruence_system(JNIEnv* env, const Congruence_System& cgs);

// Utility routine to take the inhomogeneous term from a constraint or a
// congruence.
jobject
get_le_inhomogeneous_term(JNIEnv* env, const Coefficient& c);

// Builds the Java linear expression starting from a congruence,
// a constraint or a generator.
// FIXME: left in the header file to allow g++ to build template code
// properly.
template <typename R>
jobject
get_linear_expression(JNIEnv* env, const R& r) {
  jclass j_le_coeff_class
    = env->FindClass("ppl_java/Linear_Expression_Coefficient");
  jclass j_le_class
    = env->FindClass("ppl_java/Linear_Expression");
  jclass j_le_variable_class
    = env->FindClass("ppl_java/Linear_Expression_Variable");
  jclass j_variable_class
    = env->FindClass("ppl_java/Variable");
  Coefficient coefficient;
  dimension_type varid = 0;
  dimension_type space_dimension = r.space_dimension();
  jobject j_le_term;
  jmethodID j_variable_ctr_id
    = env->GetMethodID(j_variable_class,
		       "<init>",
		       "(I)V");
  jmethodID j_le_variable_ctr_id
    = env->GetMethodID(j_le_variable_class,
		       "<init>",
		       "(Lppl_java/Variable;)V");

  jmethodID j_le_times_id
    = env->GetMethodID(j_le_class,
		       "times",
		       "(Lppl_java/Coefficient;)Lppl_java/Linear_Expression;");

  while (varid < space_dimension
 	 && (coefficient = r.coefficient(Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    jobject j_coefficient_zero = ppl_coeff_to_j_coeff(env, Coefficient(0));
    jmethodID j_le_coeff_ctr_id
      = env->GetMethodID(j_le_coeff_class, "<init>",
			 "(Lppl_java/Coefficient;)V");
    return env->NewObject(j_le_coeff_class, j_le_coeff_ctr_id,
			  j_coefficient_zero);
  }
  else {
    jobject j_coefficient = ppl_coeff_to_j_coeff(env, coefficient);
    jobject j_variable = env->NewObject(j_variable_class, j_variable_ctr_id,
					 varid);
    jobject j_le_variable = env->NewObject(j_le_variable_class,
					   j_le_variable_ctr_id,
					   j_variable);
    j_le_term =  env->CallObjectMethod(j_le_variable,
					j_le_times_id, j_coefficient);
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
 	jobject j_coefficient = ppl_coeff_to_j_coeff(env, coefficient);
 	jobject j_variable = env->NewObject(j_variable_class,
  					     j_variable_ctr_id,
  					     varid);
  	jobject j_le_variable = env->NewObject(j_le_variable_class,
  						j_le_variable_ctr_id,
  						j_variable);
 	jobject j_le_term2 = env->CallObjectMethod(j_le_variable,
						     j_le_times_id,
						     j_coefficient);
  	jmethodID j_le_sum_id
  	  = env->GetMethodID(j_le_class,
  			     "sum",
  			     "(Lppl_java/Linear_Expression;)"
			     "Lppl_java/Linear_Expression;");
 	j_le_term = env->CallObjectMethod(j_le_term, j_le_sum_id, j_le_term2);
      }
    }
  }
  return j_le_term;
}
